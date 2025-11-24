(cl:in-package #:model-info-generator)

(defmethod remove-cohort ((element data-model))
  ;; The cohort table has no primary key, no foreign keys and
  ;; date-based rules for the relation between subjects and cohorts. I
  ;; don't think there is a way to handle this via Hibernate.
  (a:when-let ((cohort-table (find-table "cohort" element)))
    (let ((tables (tables element)))
      (a:maphash-values
       (lambda (table)
         (a:when-let ((column (find-if (lambda (foreign-key)
                                         (and foreign-key
                                              (eq (table foreign-key)
                                                  cohort-table)))
                                       (columns table) :key #'foreign-key)))
           (setf (foreign-key column) nil)))
       tables)
      (remhash "cohort" tables)))
  element)
