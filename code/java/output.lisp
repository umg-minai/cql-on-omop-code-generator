(cl:in-package #:model-info-generator.java)

;;; Names

(defmethod mi::filename<-omop-table ((format     (eql :java))
                                     (omop-table string))
  (remove #\_ (string-capitalize omop-table)))

(defmethod mi::cql-type<-omop-table ((format     (eql :java))
                                     (omop-table string))
  (remove #\_ (string-capitalize omop-table)))
(defun translate-class-name (omop-name)
  (remove #\_ (string-capitalize omop-name)))

(defmethod mi::cql-element<-omop-column ((format      (eql :java))
                                         (omop-column string))
  (string-downcase (remove #\_ (string-capitalize omop-column)) :end 1))
(defun translate-column-name (omop-name)
  (string-downcase (remove #\_ (string-capitalize omop-name)) :end 1))

(defun java-type<-omop-type (omop-type)
  (cond ((string= omop-type "date")                 "ZonedDateTime")
        ((string= omop-type "datetime")             "ZonedDateTime")
        ((or (string= omop-type "integer")
             (string= omop-type "Integer"))         "Integer")
        ((string= omop-type "float")                "Float")
        ((a:starts-with-subseq "varchar" omop-type) "String")
        (t                                          omop-type)))

(defun without-id (name)
  (let ((index (search "_id" name)))
    (concatenate 'string
                 (subseq name 0 index)
                 (subseq name (+ index 3)))))

;;;

(defmethod mi:emit ((element mi:data-model)
                    (format  (eql :java-project))
                    (target  pathname))
  (let* ((directory          (uiop:ensure-directory-pathname target))
         (resource-directory (merge-pathnames #P"src/main/resources/org/example/" directory))
         (version            (remove #\. (mi:version element)))
         (version-directory  (make-pathname :directory (list :relative version)))
         (model-directory    (reduce #'merge-pathnames
                                     (list version-directory
                                           #P"src/main/java/OMOP/"
                                           directory)
                                     :from-end t)))
    (ensure-directories-exist resource-directory)
    (let ((schema-format (make-instance 'mi::schema-format :associated-format :java)))
      (mi:emit element schema-format resource-directory))
    (mi:emit element :helpers resource-directory)
    (ensure-directories-exist model-directory) ; TODO: who should do this?
    (mi:emit element :java model-directory)))

;;;

;;; Emit a Register class which provides a static method for
;;; registering all classes of one OMOP version in a provided
;;; MappingInfo instance.
(defmethod mi:emit :after ((element mi:data-model)
                           (format  (eql :java))
                           (target  pathname))
  (assert (uiop:directory-pathname-p target))
  (let ((pathname (merge-pathnames "Register.java" target)))
    (a:with-output-to-file (stream pathname :if-exists :supersede)
      (let ((version (mi:version element)))
        (j:emitting (stream)
          (j:out "package OMOP.~A;~2%" (remove #\. version))
          (j:out "import OMOP.MappingInfo;~2%")
          (j:class "Register" ()
            (j:method ("register" '(("mappingInfo" "MappingInfo")) "void"
                       :modifiers '("public" "static"))
              (a:maphash-values
               (lambda (table)
                 (when (mi:primary-key table)
                   (j:out "mappingInfo.registerDataTypeInfo(\"~A\", new ~:*~AInfo());~@:_"
                          (mi::cql-type<-omop-table
                           format (mi:name table)))))
               (mi:tables element)))))))))

;;; Emit a CLASSInfo class for model class CLASS which has additional
;;; information about CLASS such as the names of joinable fields for
;;; code-based query restrictions.
(defmethod mi:emit ((element mi:table) (format (eql :java)) (target pathname))
  (call-next-method)
  (mi:emit (make-data-type-info element) format target))

(defmethod mi:emit ((element mi:table) (format (eql :java)) (target stream))
  (let* ((meta-model (mi:parent element))
         (name       (mi:name element))
         (class-name (mi::cql-type<-omop-table format name))
         (columns    (mi:columns element))
         (concept?   (string= name "concept")))
    (j:emitting (target)
      (j:out "package OMOP.~A;~2%" (remove #\. (mi:version meta-model)))
      (j:out "import java.time.ZonedDateTime;~@
              import java.util.List;~@
              import java.util.Optional;~@
              import jakarta.persistence.Entity;~@
              import jakarta.persistence.FetchType;~@
              import jakarta.persistence.Id;~@
              import jakarta.persistence.JoinColumn;~@
              import jakarta.persistence.JoinTable;~@
              import jakarta.persistence.ManyToOne;~@
              import jakarta.persistence.ManyToMany;~@
              import jakarta.persistence.Table;~@
              import jakarta.persistence.Column;~@
              import org.opencds.cqf.cql.engine.runtime.DateTime;~@
              import org.opencds.cqf.cql.engine.runtime.Date;~2%")

      (j:out "@Entity~@
              @Table(name = \"~A\", schema = \"cds_cdm\")~@
              public class ~A {~@
              ~@
              "
             name class-name)

      (pprint-logical-block (target (list element) :per-line-prefix "  ")
        (j:emitting (target)
          (map nil (a:rcurry #'mi:emit format target) columns)
          (j:out "~%")
          (a:when-let ((id (find-if #'mi:primary-key? columns)))

            (j:out "@Override~%")
            (j:method ("toString" '() "String")
              (j:out "final var result = new StringBuilder();~@:_~
                    result.append(\"~A{id=\").append(this.~A);~@:_"
                     class-name (mi::cql-element<-omop-column
                                 format (mi:name id)))
              (when concept?
                (j:out "result.append(\", name=')~@:_~
                      ~2@T.append(this.getConceptName().get())~@:_~
                      ~2@T.append(\"'\");"))
              (a:when-let ((concept (mi::canonical-concept-column element)))
                (j:out "this.get~A().ifPresent(concept -> {~@:_~
                      ~2@Tresult.append(\", concept '\")~@:_~
                      ~2@T.append(concept.getConceptName().get())~@:_~
                      ~2@T.append(\"'\");~@:_~
                      });~@:_"
                       (mi::cql-type<-omop-table
                        format (without-id (mi:name concept)))))
              (j:out "result.append(\"}\");~@:_~
                    return result.toString();")))

          (when concept?
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
              (emit-relation "descendants" "ancestor_concept_id"   "descendant_concept_id"))))))

    (format target "~%}~%")))

(defmethod mi:emit ((element mi:column) (format (eql :java)) (target stream))
  (mi:emit (make-field element) format target)
  (format target "~%")
  (let ((getter (cond ((string= (mi:data-type element) "datetime")
                       (make-getter
                        element
                        :type       "DateTime"
                        :conversion (lambda (value)
                                      (format nil "new DateTime(~A.toOffsetDateTime())"
                                              value))))
                      ((string= (mi:data-type element) "date")
                       (make-getter
                        element
                        :type       "Date"
                        :conversion (lambda (value)
                                      (format nil "new Date(~A.toLocalDate())"
                                              value))))
                      (t
                       (make-getter element)))))
   (mi:emit getter format target))
  (format target "~%")
  (a:when-let ((foreign-key (mi:foreign-key element)))
    (let* ((name           (mi:name element))
           (base-name      (without-id name))
           (method-name    (mi::cql-element<-omop-column
                            format base-name))
           (field-name     (string-downcase method-name :end 1))
           (foreign-table  (mi:table foreign-key))
           (foreign-table-name (mi:name foreign-table))
                                        ; (foreign-column (column foreign-key))
           (data-type      (mi::cql-type<-omop-table
                            format foreign-table-name)))
      (format target "@ManyToOne(targetEntity = ~A.class, fetch = FetchType.LAZY)~@
                      @JoinColumn(name = \"~A\")~@
                      private ~A ~A;~2%"
              ;; , table = \"~A\", referencedColumnName = \"~A\"
              data-type
              name ;; foreign-table-name (name foreign-column)
              data-type field-name)
      (format target "public Optional<~A> get~A() {~@
                      ~2@Treturn Optional.ofNullable(this.~A);~@
                      }~%"
              data-type method-name field-name))))

(defstruct (field (:constructor make-field (column))) column)
(defmethod mi:emit ((element field) (format (eql :java)) (target stream))
  (let* ((column      (field-column element))
         (name        (mi:name column))
         (method-name (mi::cql-element<-omop-column format name))
         (field-name  (string-downcase method-name :end 1))
         (data-type   (java-type<-omop-type (mi:data-type column)))
         (required?   (mi:required? column)))
    (when (mi:primary-key? column)
      (format target "@Id~%"))
    (format target "@Column(name = \"~A\", insertable = false, updatable = false~:[~;, nullable = false~])~@
                    private ~A ~A;~%"
            name required? data-type field-name)))

(defstruct (getter
            (:constructor
                make-getter (column
                             &key (type       (java-type<-omop-type
                                               (mi:data-type column)))
                                  (conversion 'identity))))
  column type conversion)
(defmethod mi:emit ((element getter) (format (eql :java)) (target stream))
  (let* ((column       (getter-column element))
         (return-type  (getter-type element))
         (conversion   (getter-conversion element))
         (name         (mi:name column))
         (method-name  (mi::cql-element<-omop-column format name))
         (field-name   (string-downcase method-name :end 1))
         (field-access (format nil "this.~A" field-name)))
    (if (mi:required? column)
        (format target "public ~A get~A() {~@
                        ~2@Treturn ~A;~@
                        }~%"
                return-type method-name
                (funcall conversion field-access))
        ;; TODO: use ofNullable
        (format target "public Optional<~A> get~A() {~@
                        ~2@Tif (this.~A != null) {~@
                        ~4@Treturn Optional.of(~A);~@
                        ~2@T} else {~@
                        ~4@Treturn Optional.empty();~@
                        ~2@T}~@
                        }~%"
                return-type method-name field-name
                (funcall conversion field-access)))))

;;;

(defstruct (data-type-info
            (:constructor make-data-type-info (table))
            (:predicate nil)
            (:copier nil))
  (table (error "required") :read-only t))

(defmethod mi:emit ((element data-type-info)
                    (format  (eql :java))
                    (target  pathname))
  (let* ((table      (data-type-info-table element))
         (name       (mi:name table))
         (class-name (mi::cql-type<-omop-table format name))
         (info-class-name (format nil "~AInfo" class-name))
         (filename        (merge-pathnames
                           (make-pathname :name info-class-name
                                          :type "java")
                           target)))
    (a:with-output-to-file (stream filename :if-exists :supersede)
      (mi:emit element format stream))))

(defmethod mi:emit ((element data-type-info)
                    (format  (eql :java))
                    (target  stream))
  (let* ((table      (data-type-info-table element))
         (name       (mi:name table))
         (columns    (mi:columns table))
         (class-name (mi::cql-type<-omop-table format name))
         (info-class-name (format nil "~AInfo" class-name)))
    (j:emitting (target)
      (j:out "package OMOP.~A;~2%" "v54" ; TODO: (mi:version (mi:parent table))
             )
      (j:out "import OMOP.DataTypeInfo;~2%")

      (j:class info-class-name ((:implements "DataTypeInfo"))
        (j:method ("getClazz" '() "Class<?>")
          (j:out "return ~A.class;" class-name))

        (j:method ("contextPath" '(("contextName" "String")) "String")
          (if (find "person_id" columns :test #'equal :key #'mi:name)
              (j:if "contextName.equals(\"Patient\")"
                (lambda () (j:out "return \"~A\";" "person"))
                "return null;")
              (j:out "return null;")))

        (j:method ("columnForContext"
                   '(("contextPath" "String")
                     ("contextValue" "Object"))
                   "String")
          (if (find "person_id" columns :test #'equal :key #'mi:name)
              (j:if "contextPath.equals(\"person\") && (contextValue instanceof Person)"
                "return \"personId\";"
                "return null;")
              (j:out "return null;")))

        (j:method ("isJoinableCodePath" '(("codePath" "String")) "boolean")
          (j:out "return ~@<~:[false~;~:*~{codePath.equals(\"~A\")~^ ~@:_|| ~}~]~@:>;"
                 (loop :for column :in (mi:columns table)
                       :for name   =   (mi:name column)
                       :when (and (a:ends-with-subseq "concept_id" name) ; TODO: make a function
                                  (equal (mi:data-type column) "integer"))
                         :collect (mi::cql-element<-omop-column
                                   format (without-id name)))))))))
