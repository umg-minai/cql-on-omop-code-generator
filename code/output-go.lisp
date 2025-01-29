(cl:in-package #:model-info-generator)

(defmethod emit ((element data-model)
                 (format  (eql :go-project))
                 (target  pathname))
  (let* ((directory (uiop:ensure-directory-pathname target))
         (model     (merge-pathnames #P"model/" directory)))
    (emit element :schema  directory)
    (emit element :go model)))

;;; Utilities

(defun emitting-block (continuation stream)
  (format stream "{~%")
  (pprint-logical-block (stream (list continuation) :per-line-prefix "	")
    (funcall continuation stream))
  (format stream "~&}~%"))

(defmacro with-block ((stream) &body body)
  `(emitting-block (lambda (,stream) ,@body) ,stream))

(defun emitting-functionoid (continuation stream name parameters result-type)
  (format stream "~A(~{~{~A ~A~}~^, ~}) ~A "
          name parameters result-type)
  (with-block (stream) (funcall continuation stream)))

(defun emitting-function (continuation stream name parameters result-type)
  (format stream "func ")
  (emitting-functionoid continuation stream name parameters result-type))

(defmacro with-function ((stream name parameters result-type)
                         &body body)
  `(emitting-function (lambda (,stream) ,@body)
                      ,stream ,name  ,parameters ,result-type))

(defun emitting-method (continuation stream
                        name instance-variable type parameters result-type)
  (format stream "func (~A ~A) " instance-variable type)
  (emitting-functionoid continuation stream name parameters result-type))

(defmacro with-method ((stream name instance-variable type parameters result-type)
                       &body body)
  `(emitting-method
    (lambda (,stream) ,@body)
    ,stream ,name ,instance-variable ,type ,parameters ,result-type))

(defun emitting-struct (continuation stream name)
  (format stream "type ~A struct " name)
  (with-block (stream) (funcall continuation stream)))

(defmacro with-struct ((stream name) &body body)
  `(emitting-struct (lambda (,stream) ,@body) ,stream ,name))

(defun emit-field (stream name type &optional annotation)
  (format stream "~A~@[ ~A~]~@[ `~A`~]" name type annotation))

;;;

(defun go-type<-omop-type (omop-type)
  (cond ((string= omop-type "date")                 "Date")
        ((string= omop-type "datetime")             "DateTime")
        ((or (string= omop-type "integer")
             (string= omop-type "Integer"))         "int64")
        ((string= omop-type "float")                "float32")
        ((a:starts-with-subseq "varchar" omop-type) "string")
        (t                                          omop-type)))

(defmethod emit ((element table)
                 (format  (eql :go))
                 (target  stream))
  (let* ((name       (name element))
         (class-name (translate-class-name name))
         (columns    (columns element))
         (relations  (remove-if-not #'foreign-key columns)))
    (format target "package model;~2%")
    (when relations
      (format target "import \"context\"~2%"))
    (format target "import \"github.com/uptrace/bun\"~2%")

    (with-struct (target class-name)
      (emit-field target "bun.BaseModel" nil (format nil "bun:\"table:cds_cdm.~A\"" name))
      (map nil (a:rcurry #'emit format target) columns)

      #+TODO (when (string= name "concept")
               (flet ((emit-relation (name forward-join-column inverse-join-column)
                        (let ((method-name (string-capitalize name :end 1)))
                          (format target "@ManyToMany(targetEntity = ~A.class, fetch = FetchType.LAZY)~@
                        @JoinTable(name=\"concept_ancestor\", schema=\"cds_cdm\",~@
                        ~2@TjoinColumns = {~@
                        ~2@T~2@T@JoinColumn(name=\"~A\")~@
                        ~2@T},~@
                        ~2@TinverseJoinColumns = {~@
                        ~2@T~2@T@JoinColumn(name=\"~A\")~@
                        ~2@T}~@
                        )~@
                        private List<Concept> ~A;~@
                        ~@
                        public List<Concept> get~A() {~@
                        ~2@Treturn this.~A;~@
                        }~2%"
                                  class-name
                                  forward-join-column inverse-join-column
                                  name
                                  method-name name))))
                 (emit-relation "ancestors"   "descendant_concept_id" "ancestor_concept_id")
                 (emit-relation "descendants" "ancestor_concept_id"   "descendant_concept_id")))

      (map nil (lambda (element)
                 (let* ((name                (name element))
                        (base-name           (without-id name))
                        (field-name          (translate-class-name base-name))
                        (foreign-key         (foreign-key element))
                        (foreign-table       (table foreign-key))
                        (foreign-table-name  (name foreign-table))
                        (foreign-column-name (name (column foreign-key)))
                        (data-type           (format nil "*~A" (translate-class-name foreign-table-name)))
                        (annotation          (format nil "bun:\"rel:has-one,join:~A=~A\""
                                                     name foreign-column-name)))
                   (format target "~2%")
                   (emit-field target field-name data-type annotation)))
           relations))

    (map nil (lambda (element)
               (let* ((name           (name element))
                      (base-name      (without-id name))
                      (method-name    (format nil "Ensure~A" (translate-class-name base-name)))
                      (field-name     (translate-class-name base-name))
                      (foreign-key    (foreign-key element))
                      (foreign-table  (table foreign-key))
                      (foreign-table-name  (name foreign-table))
                      (foreign-column-name (name (column foreign-key)))
                      (data-type      (translate-class-name foreign-table-name)))
                 (format target "~%")
                 (with-method (target method-name "o" (format nil "*~A" class-name) '(("connection" "*bun.DB")) (format nil "*~A" data-type))
                   (format target "if (o.~A == (*~A)(nil)) "
                           field-name data-type)
                   (with-block (target)
                     (format target "v := &~A{}~@
                                       error := connection.NewSelect().~@
                                       ~8@TModel(v).~@
                                       ~8@T//Where(\"~A.~A = ?\", o.GenderConceptId).~@
                                       ~8@TScan(context.Background())~@
                                       if error != nil {~@
                                       ~8@Tpanic(error)~@
                                       }~@
                                       o.~A = v"
                             data-type
                             foreign-table-name foreign-column-name
                             field-name))
                   (format target "return o.~A" field-name))))
         relations)))

(defun without-id (name)
  (let ((index (search "_id" name)))
    (concatenate 'string
                 (subseq name 0 index)
                 (subseq name (+ index 3)))))

(defmethod emit ((element column)
                 (format  (eql :go))
                 (target  stream))
  (unless (member (data-type element) '("date" "datetime") :test #'string=)
    (format target "~2%")
    (emit (make-field element) format target)
    #+TODO (a:when-let ((foreign-key (foreign-key element)))
      (let* ((name           (name element))
             (base-name      (without-id name))
             (method-name    (translate-class-name base-name))
             (field-name     (string-downcase method-name :end 1))
             (foreign-table  (table foreign-key))
             (foreign-table-name (name foreign-table))
                                        ; (foreign-column (column foreign-key))
             (data-type      (translate-class-name foreign-table-name)))
        (format target "@ManyToOne(targetEntity = ~A.class, fetch = FetchType.LAZY)~@
                          @JoinColumn(name = \"~A\")~@
                          private ~A ~A;~2%"
                ;; , table = \"~A\", referencedColumnName = \"~A\"
                data-type
                name ;; foreign-table-name (name foreign-column)
                data-type field-name)
        (format target "public Optional<~A> get~A() {~@
                          ~2@Treturn Optional.of(this.~A);~@
                          }~%"
                data-type method-name field-name)))))

(defstruct (field (:constructor make-field (column))) column)
(defmethod emit ((element field)
                 (format  (eql :go))
                 (target  stream))
  (let* ((column      (field-column element))
         (name        (name column))
         (field-name  (translate-class-name name))
         (data-type   (go-type<-omop-type (data-type column)))
         (primary?    (primary-key? column))
         (required?   (required? column))
         (annotation  (format nil "bun:\"~A~:[~;,pk~]~:[~;,notnull~]\""
                              name primary? required?)))
    (emit-field target field-name data-type annotation)))

(defstruct (getter (:constructor make-getter (column))) column)
(defmethod emit ((element getter)
                 (format  (eql :go))
                 (target  stream))
  (let* ((column       (getter-column element))
         (name         (name column))
         (method-name  (translate-class-name name))
         (field-name   (string-downcase method-name :end 1))
         (data-type   (java-type<-omop-type (data-type column))))
    (if (required? column)
        (format target "public ~A get~A() {~@
                        ~2@Treturn this.~A;~@
                        }~%"
                data-type method-name field-name)
        (format target "public Optional<~A> get~A() {~@
                        ~2@Treturn Optional.of(this.~A);~@
                        }~%"
                data-type method-name field-name))))
