(cl:in-package #:model-info-generator)

(defmethod remove-cohort ((element data-model))
  ;; The cohort table has no primary key, no foreign keys and
  ;; date-based rules for the relation between subjects and cohorts. I
  ;; don't think there is a way to handle this via Hibernate.
  (remhash "cohort" (tables element))
  element)
