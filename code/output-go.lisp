(cl:in-package #:model-info-generator)

(defmethod emit ((element data-model)
                 (format  (eql :go-project))
                 (target  pathname))
  (let* ((directory (uiop:ensure-directory-pathname target))
         (model     (merge-pathnames #P"model/" directory)))
    (emit element :schema  directory)
    (emit element :go model)))

;;; Utilities

(defun write-or-call (string-or-continuation stream)
  (etypecase string-or-continuation
    (string
     (write-string string-or-continuation stream))
    ((or symbol function)
     (funcall string-or-continuation stream))))

(defun emitting-block (continuation stream newline?)
  (format stream "{~%")
  (pprint-logical-block (stream (list continuation) :per-line-prefix "	")
    (funcall continuation stream))
  (format stream "~&}~:[~;~%~]" newline?))

(defmacro with-block ((&optional (newline? t)) &body body)
  `(emitting-block (lambda (stream) ,@body) stream ,newline?))

(defun emitting-if (test then else stream)
  (write-string "if " stream)
  (write-or-call test stream)
  (write-string " " stream)
  (with-block ((null else))
    (write-or-call then stream))
  (when else
    (write-string " else " stream)
    (with-block (stream)
    (write-or-call else stream))))

(defmacro with-if (test then &optional else)
  `(emitting-if ,test ,then ,else stream))

(defun emitting-for (continuation stream variables container)
  (write-string "for " stream)
  (loop :for variable :in variables
        :for first? = t :then nil
        :unless first? :do (write-string ", " stream)
        :do (write-or-call variable stream))
  (write-string " := range " stream)
  (write-or-call container stream)
  (with-block (stream)
    (write-or-call continuation stream)))

(defmacro with-for ((&rest variables) container &body body)
  `(emitting-for (lambda (stream) ,@body) stream '(,@variables) ,container))

(defun emitting-functionoid (continuation stream name parameters result-type newline?)
  (format stream "~@[~A~](~{~{~A ~A~}~^, ~}) ~A "
          name parameters result-type)
  (with-block (newline?) (funcall continuation stream)))

(defun emitting-function (continuation stream name parameters result-type newline?)
  (format stream "func ")
  (emitting-functionoid continuation stream name parameters result-type newline?))

(defmacro with-function ((name parameters result-type &optional (newline? 't)) &body body)
  `(emitting-function
    (lambda (stream) ,@body) stream ,name ,parameters ,result-type ,newline?))

(defun emitting-method (continuation stream
                        name instance-variable type parameters result-type
                        newline?)
  (format stream "func (~A ~A) " instance-variable type)
  (emitting-functionoid continuation stream name parameters result-type newline?))

(defmacro with-method ((name instance-variable type parameters result-type
                        &optional (newline? 't))
                       &body body)
  `(emitting-method
    (lambda (stream) ,@body)
    stream ,name ,instance-variable ,type ,parameters ,result-type ,newline?))

(defun emitting-struct (continuation stream name)
  (format stream "type ~A struct " name)
  (with-block (stream) (funcall continuation stream)))

(defmacro with-struct (name &body body)
  `(emitting-struct (lambda (stream) ,@body) stream ,name))

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

(defmethod emit ((element data-model)
                 (format  (eql :go))
                 (target  pathname))
  (call-next-method)
  (a:with-output-to-file (stream (merge-pathnames "meta.go" target)
                                 :if-does-not-exist :create
                                 :if-exists         :supersede)
    (format stream "package model~@
                    ~@
                    import \"reflect\"~@
                    import \"context\"~@
                    ~@
                    import \"github.com/uptrace/bun\"~2%")

    (format stream "type TypeInfo struct")
    (with-block (stream)
      (format stream "Type      reflect.Type~@
                      Retriever func (*bun.DB) []any"))

    (format stream "~%var ConceptCache map[int64]*Concept = map[int64]*Concept{}~2%")

    (with-function ("EnsureConcept" '(("context" "context.Context") ("connection" "*bun.DB") ("id" "int64")) "*Concept")
      (format stream "result, found := ConceptCache[id]~@:_")
      (with-if "found"
        "return result"
        (lambda (stream)
          (format stream "result = &Concept{}~@
                        error := connection.NewSelect().~@
                        ~8@TModel(result).~@
                        ~8@TWhere(\"concept.concept_id = ?\", id).~@
                        ~8@TScan(context)~@:_")
          (with-if "error != nil"
            "panic(error)")
        (format stream "ConceptCache[id] = result~@
                        return result")))

      #++(with-block (stream)
        (format stream "return result"))
      #++(format stream "else")
      #++(with-block (stream)
        (format stream "result = &Concept{}~@
                        error := connection.NewSelect().~@
                        ~8@TModel(result).~@
                        ~8@TWhere(\"concept.concept_id = ?\", id).~@
                        ~8@TScan(context)~@
                        if error != nil")
        (with-block (stream)
          (format stream "panic(error)"))
        (format stream "ConceptCache[id] = result~@
                        return result")))
    (format stream "~%")

    (format stream "var TypeInfos map[string]TypeInfo = map[string]TypeInfo") ; reflect.Type
    (with-block (stream)
      (maphash
       (lambda (name element)
         (declare (ignore name))
         (when (output? element)
           (let ((name (translate-class-name (name element))))
             (format stream "\"~A\": TypeInfo" name)
             (with-block (nil)
               (format stream "Type:      reflect.TypeFor[~A](),~@
                               Retriever: "
                       name)
               (with-function (nil '(("connection" "*bun.DB")) "[]any" nil)
                 (format stream "var result []*~A;~@
                                 error := connection.NewSelect().~@
                                 ~8@TModel(&result).~@
                                 ~8@TScan(context.Background())~@:_"
                         name)
                 (with-if "error != nil" "panic(error)")
                 (format stream "finalResult := make([]any, len(result))~@:_")
                 (with-for ("i") "result"
                   (write-string "finalResult[i] = result[i]" stream))
                 (write-string "return finalResult" stream))
               (format stream ","))
             (format stream ",~%"))))
       (tables element)))))

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

    (let ((stream target))
      (with-struct class-name
                   (emit-field stream "bun.BaseModel" nil (format nil "bun:\"table:cds_cdm.~A\"" name))
        (map nil (a:rcurry #'emit format stream) columns)

        (map nil (lambda (element)
                   (let* ((name                (name element))
                          (base-name           (without-id name))
                          (field-name          (translate-column-name base-name))
                          (foreign-key         (foreign-key element))
                          (foreign-table       (table foreign-key))
                          (foreign-table-name  (name foreign-table))
                          (foreign-column-name (name (column foreign-key)))
                          (data-type           (format nil "*~A" (translate-class-name foreign-table-name)))
                          (annotation          (format nil "bun:\"rel:has-one,join:~A=~A\""
                                                       name foreign-column-name)))
                     (format stream "~2%")
                     (emit-field stream field-name data-type annotation)))
             relations)

        (when (string= name "concept")
          (format stream "~2%")
          (emit-field stream "ancestors"   "[]*Concept")
          (format stream "~2%")
          (emit-field stream "descendants" "[]*Concept")))

      (map nil (lambda (element)
                 (let* ((name           (name element))
                        (base-name      (without-id name))
                        (method-name    (format nil "Ensure~A" (translate-class-name base-name)))
                        (field-name     (translate-column-name base-name))
                        (foreign-key    (foreign-key element))
                        (foreign-table  (table foreign-key))
                        (foreign-table-name  (name foreign-table))
                        (foreign-column-name (name (column foreign-key)))
                        (data-type      (translate-class-name foreign-table-name)))
                   (format stream "~%")
                   (with-method (method-name "o" (format nil "*~A" class-name) '(("connection" "*bun.DB")) (format nil "*~A" data-type))
                     (with-if (lambda (stream)
                                (format stream "o.~A == (*~A)(nil)"
                                        field-name data-type))
                       (lambda (stream)
                         (if (equal foreign-table-name "concept")
                             (format stream "v := EnsureConcept(context.Background(), connection, o.~A)~@
                                           o.~A = v"
                                     (translate-class-name name)
                                     field-name)
                             (progn
                               (format stream "v := &~A{}~@
                                               error := connection.NewSelect().~@
                                               ~8@TModel(v).~@
                                               ~8@TWhere(\"~A.~A = ?\", o.~A).~@
                                               ~8@TScan(context.Background())~@:_"
                                       data-type
                                       foreign-table-name foreign-column-name
                                       (translate-class-name name))
                               (with-if "error != nil"
                                 "panic(error)")
                               (format stream "o.~A = v" field-name)))))
                     (format stream "return o.~A" field-name))))
           relations)

      #|
      func (o *Concept) EnsureAncestors(connection *bun.DB) []*Concept { ;
      if (o.ancestors == nil) {         ;
      v := &[]*Concept{}                ;
      error := connection.NewSelect().  ;
      Model(v).                         ;
      Join("JOIN cds_cdm.concept_ancestor ON concept_ancestor.ancestor_concept_id = concept.concept_id"). ;
      Where("concept_ancestor.descendant_concept_id = ?", o.ConceptId). ;
      Scan(context.Background())        ;
      if error != nil {                 ;
      panic(error)                      ;
      }                                 ;
      o.ancestors = *v                  ;
      }                                 ;
      return o.ancestors                ;
      }                                 ;
                                        ;
      |#
      (when (string= name "concept")
        (flet ((emit-method (field-name forward-join-column inverse-join-column)
                 (let ((method-name (format nil "Ensure~A" (string-upcase field-name :end 1))))
                   (format stream "~%")
                   (with-method (method-name "o" (format nil "*~A" class-name) '(("connection" "*bun.DB")) "[]*Concept")
                     (with-if (lambda (stream)
                                (format stream "o.~A == nil" field-name))
                       (lambda (stream)
                         (format stream "v := []*Concept{}~@
                                         error := connection.NewSelect().~@
                                         ~8@TModel(v).~@
                                         ~8@TJoin(\"JOIN cds_cdm.concept_ancestors ON concept_ancestor.~A = concept.concept_id\").~@
                                         ~8@TWhere(\"concept_ancestor.~A = ?\", o.ConceptId).~@
                                         ~8@TScan(context.Background())~@:_ "
                                 inverse-join-column forward-join-column)
                         (with-if "error != nil" "panic(error)")
                         (format stream "o.~A = v" field-name)))
                     (format stream "return o.~A" field-name)))))
          (emit-method "ancestors"   "descendant_concept_id" "ancestor_concept_id")
          (emit-method "descendants" "ancestor_concept_id"   "descendant_concept_id"))))))

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
