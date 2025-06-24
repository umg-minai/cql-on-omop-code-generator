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
(defun translate-column-name (omop-name) ; TODO(jmoringe): remove
  (string-downcase (remove #\_ (string-capitalize omop-name)) :end 1))

(defun field-name<-omop-column (omop-column)
  (mi::cql-element<-omop-column :java omop-column))

(defun java-type<-omop-type (omop-type)
  (cond ((string= omop-type "date")                 "ZonedDateTime")
        ((string= omop-type "datetime")             "ZonedDateTime")
        ((or (string= omop-type "integer")
             (string= omop-type "Integer"))         "Integer")
        ((string= omop-type "bigint")               "Long")
        ((string= omop-type "float")                "BigDecimal")
        ((string= omop-type "text")                 "String")
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
         (code-package       '("de" "umg" "minai" "cqlonomop"))
         (resource-directory (reduce #'merge-pathnames
                                     (list (format nil "~{~A/~}" code-package)
                                           #P"src/main/resources/"
                                           directory)
                                     :from-end t)))
    (ensure-directories-exist resource-directory)
    (let ((schema-format (make-instance 'mi::schema-format :associated-format :java)))
      (mi:emit element schema-format resource-directory))
    (mi:emit element :helpers resource-directory)
    (mi:emit element :java (merge-pathnames #P"src/main/java/" directory))

    (format *trace-output* ";; Output in ~S~%" directory)))

;;; Emit a Register class which provides a static method for
;;; registering all classes of one OMOP version in a provided
;;; MappingInfo instance.
(defmethod mi:emit :after ((element mi:data-model)
                           (format  (eql :java))
                           (target  pathname))
  (assert (uiop:directory-pathname-p target))
  (let ((version    (mi:version element))
        (class-name "Register"))
    (j:with-output-to-java-file (target (list "OMOP" version) class-name)
      (j:out "import OMOP.MappingInfo;~2%")
      (j:class (class-name)
        (j:method ("register" '(("mappingInfo" "MappingInfo")) "void"
                   :modifiers '("public" "static"))
          (mapc
           (lambda (table)
             (when (mi:primary-key table)
               (j:out "mappingInfo.registerDataTypeInfo(\"~A\", new ~:*~AInfo());~@:_"
                      (mi::cql-type<-omop-table
                       format (mi:name table)))))
           (mi:sorted-elements
            (a:hash-table-values (mi:tables element)))))))))

;;; Emit a CLASSInfo class for model class CLASS which has additional
;;; information about CLASS such as the names of joinable fields for
;;; code-based query restrictions.
(defmethod mi:emit ((element mi:table) (format (eql :java)) (target pathname))
  ;; TODO: (assert (uiop:pathname-directory-p target))
  (let* ((version    (mi:version (mi:parent element)))
         (name       (mi:name element))
         (class-name (mi::cql-type<-omop-table format name)))
    (j:with-output-to-java-file (target (list "OMOP" version) class-name)
      (mi:emit element format j::*stream*)))
  (mi:emit (make-data-type-info element) format target))

(defmethod mi:emit ((element mi:table) (format (eql :java)) (target stream))
  (let* ((name       (mi:name element))
         (class-name (mi::cql-type<-omop-table format name))
         (columns    (mi:columns element))
         (concept?   (string= name "concept")))
    (j:out "import java.math.BigDecimal;~@
            import java.time.ZonedDateTime;~@
            import java.util.List;~@
            import java.util.Objects;~@
            import java.util.Optional;~@
            import java.util.Set;~@
            import jakarta.persistence.*;~@
            import org.opencds.cqf.cql.engine.runtime.DateTime;~@
            import org.opencds.cqf.cql.engine.runtime.Date;~2%")

    (j:annotations (("Entity")
                    ("Table" (format nil "name = \"~A\"" name)
                             (format nil "schema = \"cds_cdm\""))) ; TODO: don't hard-code
      (j:class (class-name)
        ;; If applicable, generate CompoundKey inner class and
        ;; compoundKey field.
        (a:when-let ((compound-key (mi:compound-key element)))
          (let ((class-name (mi:emit compound-key format target)))
            (j:annotation ("EmbeddedId")
              (j:out "private ~A compoundId;~@:_~@:_" class-name))))
        ;; Columns
        (mapc (a:rcurry #'mi:emit format target)
              (mi:sorted-elements columns))
        ;; Generate extra relationships if any.
        (mapc (a:rcurry #'mi:emit format target)
              (mi:sorted-elements (mi:extra-relations element)))
        ;; If possible, generate a toString method.
        (j:annotation ("Override")
          (j:method ("toString" '() "String")
            (j:out "final var result = new StringBuilder();~@:_")
            (flet ((add (format-control &rest format-arguments)
                     (apply #'j:out "result.append(~@?);~@:_"
                            format-control format-arguments)))
              (add "\"~A{\""  class-name)
              (a:when-let ((id (find-if #'mi:primary-key? columns)))
                (add "\"id=\"")
                (add "this.~A" (mi::cql-element<-omop-column
                                format (mi:name id))))
              (cond (concept?
                     (add ", name='\"")
                     (add "this.getConceptName().get()")
                     (add "\"'\""))
                    (t
                     (a:when-let ((concept (mi::canonical-concept-column element)))
                       (let ((name (mi::cql-type<-omop-table
                                    format (without-id (mi:name concept)))))
                         (cond ((mi:required? concept)
                                (j:block (nil)
                                  (add "\", concept='\"")
                                  (add "this.get~A().getConceptName()" name)
                                  (add "\"'\"")))
                               (t
                                (j:out "this.get~A().ifPresent(concept -> " name)
                                (j:block (nil)
                                  (add "\", concept='\"")
                                  (add "concept.getConceptName()")
                                  (add "\"'\""))
                                (j:out ");~@:_")))))))
              (add "\"}\""))
            (j:out "return result.toString();")))))))

(defmethod mi:emit ((element mi:column) (format (eql :java)) (target stream))
  (let ((compound-key (mi:compound-key element)))
    (unless compound-key
      (mi:emit (make-field element) format target)
      (format target "~%"))
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

    (a:when-let ((foreign-key (mi:foreign-key element)))
      (let* ((name           (mi:name element))
             (base-name      (without-id name))
             (field-name     (field-name<-omop-column base-name))
             (method-name    (string-capitalize field-name :end 1))
             (foreign-table  (mi:table foreign-key))
             (foreign-table-name (mi:name foreign-table))
                                        ; (foreign-column (column foreign-key))
             (data-type      (mi::cql-type<-omop-table
                              format foreign-table-name)))
        (j:emitting (target)
          (j:annotations (("ManyToOne" :|targetEntity| (format nil "~A.class" data-type)
                                       :fetch          "FetchType.LAZY")
                          ("JoinColumn" :name (format nil "\"~A\"" name)))
            (when compound-key
              (let ((field-name (field-name<-omop-column name)))
                (j:annotation ("MapsId" (format nil "\"~A\"" field-name)))))
            (j:out "private ~A ~A;~2%" data-type field-name))

          (j:method ((format nil "get~A" method-name)
                     ()
                     (if (mi:required? element)
                         data-type
                         (format nil "Optional<~A>" data-type)))
            (if (mi:required? element)
                (j:out "return this.~A;" field-name)
                (j:out "return Optional.ofNullable(this.~A);" field-name))))))))

(defmethod mi:emit ((element mi:compound-key)
                    (format  (eql :java))
                    (target  stream))
  (let ((name    "CompoundId")
        (columns (sort (copy-list (mi:columns element)) #'string<
                       :key #'mi:name)))
    (j:annotation ("Embeddable")
      (j:class (name () ("private" "static"))
        (mapc (lambda (column)
                (mi:emit (make-field column) format target)
                (j:out "~@:_"))
              columns)
        ;; equals method
        (j:annotation ("Override")
          (j:method ("equals" '(("other" "Object")) "boolean")
            (j:if "this == other"
                  "return true;"
                  (lambda ()
                    (j:if (lambda ()
                            (j:out "other instanceof ~A otherInstance"
                                   name))
                          (lambda ()
                            (j:out "return ")
                            (pprint-logical-block (j::*stream* (list element)
                                                               :prefix "("
                                                               :suffix ");")
                              (j:out "other.getClass() == this.getClass()")
                              (mapc (lambda (column)
                                      (let ((field-name (field-name<-omop-column
                                                         (mi:name column))))
                                        (j:out "~@:_&& Objects.equals(this.~A, otherInstance.~:*~A)"
                                               field-name)))
                                    columns)))
                          "return false;")))))
        ;; hashCode method
        (j:annotation ("Override")
          (j:method ("hashCode" '() "int" :newline? nil)
            (j:out "return Objects.hash")
            (pprint-logical-block (j::*stream* nil
                                               :prefix "("
                                               :suffix ");")
              (mapc (let ((first? t))
                      (lambda (column)
                        (if first?
                            (setf first? nil)
                            (j:out ", "))
                        (let ((field-name (field-name<-omop-column
                                           (mi:name column))))
                          (j:out "this.~A" field-name))))
                    (mi:columns element)))))
        ))
    (j:out "~@:_")
    name))

(defmethod mi:emit ((element mi:extra-relation)
                    (format  (eql :java))
                    (target  stream))
  (let* ((name                 (mi:name element))
         (foreign-table        (mi:table element))
         (table-name           (mi:name foreign-table))
         (class-name           (mi::cql-type<-omop-table
                                format (mi:name (mi:target-table element))))
         (join-columns         (mi:join-columns element))
         (inverse-join-columns (mi:inverse-join-columns element)))
    (labels ((join-column (column)
               (let ((name (mi:name column)))
                 (j:annotation ("JoinColumn" :name       (format nil "\"~A\"" name)
                                             :insertable "false"
                                             :updatable  "false"))))
             (join-columns (columns)
               (j:block (nil)
                 ;; TODO(jmoringe): idiomatic commas
                 (mapc (let ((first? t))
                         (lambda (join-column)
                           (if first?
                               (setf first? nil)
                               (j:out ", "))
                           (join-column join-column)))
                       (mi:sorted-elements columns)))))
     (j:annotations
         (("ManyToMany" :|targetEntity| (format nil "~A.class" class-name)
                        :fetch          "FetchType.LAZY")
          ("JoinTable" :schema
                       "\"cds_cdm\"" ; TODO(jmoringe: don't hard-code
                       :name
                       (format nil "\"~A\"" table-name)
                       :|joinColumns|
                       (lambda ()
                         (join-columns join-columns))
                       :|inverseJoinColumns|
                       (lambda ()
                         (join-columns inverse-join-columns))))
       (j:out "private Set<~A> ~A;~@:_~@:_" class-name name)
       (j:method ((format nil "get~A" (string-capitalize name :end 1))
                  ()
                  (format nil "Set<~A>" class-name))
         (j:out "return this.~A;" name))))))

(defstruct (field (:constructor make-field (column))) column)
(defmethod mi:emit ((element field) (format (eql :java)) (target stream))
  (let* ((column      (field-column element))
         (name        (mi:name column))
         (method-name (mi::cql-element<-omop-column format name))
         (field-name  (string-downcase method-name :end 1))
         (data-type   (java-type<-omop-type (mi:data-type column)))
         (required?   (mi:required? column)))
    (when (mi:primary-key? column)
      (j:annotation ("Id")))
    (j:annotation ("Column" :name       (format nil "\"~A\"" name)
                            :insertable "false"
                            :updatable  "false"
                            :nullable   (if required? "false" "true"))
     (j:out "private ~A ~A;~%" data-type field-name))))

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
         (base-name    (mi::cql-element<-omop-column format name))
         (method-name  (string-capitalize base-name :end 1))
         (field-name   base-name ; (string-downcase method-name :end 1)
                       )
         (field-access (if (mi:compound-key column)
                           (format nil "this.compoundId.~A" field-name)
                           (format nil "this.~A" field-name))))
    (j:method ((format nil "get~A" method-name)
               ()
               (if (mi:required? column)
                   return-type
                   (format nil "Optional<~A>" return-type)))
      (if (mi:required? column)
          (j:out "return ~A;" (funcall conversion field-access))
          (j:if (format nil "~A != null" field-access)
                (lambda ()
                  (j:out "return Optional.of(~A);"
                         (funcall conversion field-access)))
                "return Optional.empty();")))))

;;;

(defstruct (data-type-info
            (:constructor make-data-type-info (table))
            (:predicate nil)
            (:copier nil))
  (table (error "required") :read-only t))

(defmethod mi:emit ((element data-type-info)
                    (format  (eql :java))
                    (target  pathname))
  (let* ((table           (data-type-info-table element))
         (version         (mi:version (mi:parent table)))
         (name            (mi:name table))
         (class-name      (mi::cql-type<-omop-table format name))
         (info-class-name (format nil "~AInfo" class-name)))
    (j:with-output-to-java-file (target (list "OMOP" version) info-class-name)
      (mi:emit element format j::*stream*))))

(defmethod mi:emit ((element data-type-info)
                    (format  (eql :java))
                    (target  stream))
  (let* ((table      (data-type-info-table element))
         (name       (mi:name table))
         (columns    (mi:columns table))
         (class-name (mi::cql-type<-omop-table format name))
         (info-class-name (format nil "~AInfo" class-name)))
    (j:out "import OMOP.DataTypeInfo;~2%")

    (j:class (info-class-name ((:implements "DataTypeInfo")))
      (j:method ("getClazz" '() "Class<?>")
        (j:out "return ~A.class;" class-name))

      (j:method ("contextPath" '(("contextName" "String")) "String")
        (if (find "person_id" columns :test #'equal :key #'mi:name)
            (j:if "contextName.equals(\"Patient\")"
                  (lambda () (j:out "return \"~A\";" "person"))
                  "return null;")
            (j:out "return null;")))

      (j:method ("infoForContext"
                 '(("contextPath" "String")
                   ("contextValue" "Object"))
                 "ContextInfo")
        (if (find "person_id" columns :test #'equal :key #'mi:name)
            (j:if "contextPath.equals(\"person\") && (contextValue instanceof Person person)"
                  "return new ContextInfo(\"personId\", person.getPersonId());"
                  "return null;")
            (j:out "return null;")))

      (j:method ("isJoinableCodePath" '(("codePath" "String")) "boolean")
        (j:out "return ~@<~:[false~;~:*~{codePath.equals(\"~A\")~^ ~@:_|| ~}~]~@:>;"
               (loop :for column :in (mi:columns table)
                     :for name   =   (mi:name column)
                     :when (and (a:ends-with-subseq "concept_id" name) ; TODO: make a function
                                (equal (mi:data-type column) "integer"))
                       :collect (mi::cql-element<-omop-column
                                 format (without-id name))))))))
