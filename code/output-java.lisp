(cl:in-package #:model-info-generator)

(defun java-type<-omop-type (omop-type)
  (cond ((string= omop-type "date")                 "Date")
        ((string= omop-type "datetime")             "DateTime")
        ((or (string= omop-type "integer")
             (string= omop-type "Integer"))         "Integer")
        ((string= omop-type "float")                "Float")
        ((a:starts-with-subseq "varchar" omop-type) "String")
        (t                                          omop-type)))

(defmethod emit ((element table) (format (eql :java)) (target stream))
  (let* ((name       (name element))
         (class-name (translate-class-name name))
         (columns    (columns element)))
    (format target "package OMOP;~@
                  ~@
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
                  ~@
                  @Entity~@
                  @Table(name = \"~A\", schema = \"cds_cdm\")~@
                  public class ~A {~@
                  ~@
                  "
            name class-name)
    (pprint-logical-block (target (list element) :per-line-prefix "  ")
      (map nil (a:rcurry #'emit target) columns)
      (a:when-let ((id (find-if #'primary-key? columns)))
        (format target "@Override~@
                        public String toString() {~@
                        ~2@Treturn \"~A{id=\" + this.~A + \"}\";~@
                        }~2%"
                class-name (translate-column-name (name id))))

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

(defmethod emit ((element column) (format (eql :java)) (target stream))
  (unless (member (data-type element) '("date" "datetime") :test #'string=)
    (emit (make-field element) format target)
    (format target "~%")
    (emit (make-getter element) format target)
    (format target "~%")
    (a:when-let ((foreign-key (foreign-key element)))
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
(defmethod emit ((element field) (format (eql :java)) (target stream))
  (let* ((column      (field-column element))
         (name        (name column))
         (method-name (translate-class-name name))
         (field-name  (string-downcase method-name :end 1))
         (data-type   (java-type<-omop-type (data-type column)))
         (required?   (required? column)))
    (when (primary-key? column)
        (format target "@Id~%"))
    (format target "@Column(name = \"~A\", insertable = false, updatable = false~:[~;, nullable = false~])~@
                    private ~A ~A;~%"
            name required? data-type field-name)))

(defstruct (getter (:constructor make-getter (column))) column)
(defmethod emit ((element getter) (format (eql :java)) (target stream))
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
