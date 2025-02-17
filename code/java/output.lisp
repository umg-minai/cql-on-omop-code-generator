(cl:in-package #:model-info-generator.java)

(defun translate-class-name (omop-name)
  (remove #\_ (string-capitalize omop-name)))

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

(defmethod mi:emit ((element mi:table) (format (eql :java)) (target stream))
  (let* ((name       (mi:name element))
         (class-name (translate-class-name name))
         (columns    (mi:columns element)))
    (format target "package OMOP;~@
                  ~@
                  import java.time.ZonedDateTime;~@
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
                  import org.opencds.cqf.cql.engine.runtime.Date;~@
                  ~@
                  @Entity~@
                  @Table(name = \"~A\", schema = \"cds_cdm\")~@
                  public class ~A {~@
                  ~@
                  "
            name class-name)
    (pprint-logical-block (target (list element) :per-line-prefix "  ")
      (map nil (a:rcurry #'mi:emit format target) columns)
      (a:when-let ((id (find-if #'mi:primary-key? columns)))
        (format target "@Override~@
                        public String toString() {~@
                        ~2@Treturn \"~A{id=\" + this.~A + \"}\";~@
                        }~2%"
                class-name (translate-column-name (mi:name id))))

      (when (string= name "concept")
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
          (emit-relation "descendants" "ancestor_concept_id"   "descendant_concept_id"))))

    (format target "~%}~%")))

(defun without-id (name)
  (let ((index (search "_id" name)))
    (concatenate 'string
                 (subseq name 0 index)
                 (subseq name (+ index 3)))))

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
           (method-name    (translate-class-name base-name))
           (field-name     (string-downcase method-name :end 1))
           (foreign-table  (mi:table foreign-key))
           (foreign-table-name (mi:name foreign-table))
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
              data-type method-name field-name))))

(defstruct (field (:constructor make-field (column))) column)
(defmethod mi:emit ((element field) (format (eql :java)) (target stream))
  (let* ((column      (field-column element))
         (name        (mi:name column))
         (method-name (translate-class-name name))
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
         (method-name  (translate-class-name name))
         (field-name   (string-downcase method-name :end 1))
         (field-access (format nil "this.~A" field-name)))
    (if (mi:required? column)
        (format target "public ~A get~A() {~@
                        ~2@Treturn ~A;~@
                        }~%"
                return-type method-name
                (funcall conversion field-access))
        (format target "public Optional<~A> get~A() {~@
                        ~2@Tif (this.~A != null) {~@
                        ~4@Treturn Optional.of(~A);~@
                        ~2@T} else {~@
                        ~4@Treturn Optional.empty();~@
                        ~2@T}~@
                        }~%"
                return-type method-name field-name
                (funcall conversion field-access)))))
