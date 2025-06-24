(cl:in-package #:model-info-generator)

(defmethod manual-compound-keys ((element data-model))
  (a:maphash-values #'manual-compound-keys (tables element))
  element)

(defmethod manual-compound-keys ((element table))
  (flet ((add-compound-key (&rest column-names)
           (let ((columns (mapcar (a:rcurry #'find-column element)
                                  column-names)))
             (add-compound-key element columns))))
    (let ((name (name element)))
      (a:switch (name :test #'string=)
        ("cdm_source"
         (add-compound-key "cdm_source_name" "cdm_version"))
        ("death"
         ;; Since in the OMOP CDM, a person can have "up to one" death
         ;; record, the person_id column uniquely (and most naturally)
         ;; identifies the death record.
         (add-compound-key "person_id"))
        ("drug_strength"
         ;; TODO(jmoringe): explain why
         (add-compound-key "drug_concept_id" "ingredient_concept_id"))))))
