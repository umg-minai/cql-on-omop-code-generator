(cl:in-package #:model-info-generator)

(defmethod add-extra-relations ((element data-model))
  (flet ((add (name parent table join-columns &key inverse-join-columns target-table)
           (let ((relation (apply #'make-instance 'extra-relation
                                  :name                 name
                                  :parent               parent
                                  :table                table
                                  :join-columns         (a:ensure-list join-columns)
                                  (append
                                   (when inverse-join-columns
                                     (list :inverse-join-columns
                                           (a:ensure-list inverse-join-columns)))
                                   (when target-table
                                     (list :target-table target-table))))))
             (push relation (extra-relations parent)))))
    (let* (;; Tables
           (concept              (find-table "concept"              element))
           (concept-ancestors    (find-table "concept_ancestor"     element))
           (concept-relationship (find-table "concept_relationship" element))
           ;; Columns
           (concept1             (find-column "concept_id_1"          concept-relationship))
           (concept2             (find-column "concept_id_2"          concept-relationship))
           (ancestor             (find-column "ancestor_concept_id"   concept-ancestors))
           (descendant           (find-column "descendant_concept_id" concept-ancestors)))
      (add "ancestors" concept concept-ancestors descendant
           :inverse-join-columns ancestor :target-table concept)
      (add "descendants" concept concept-ancestors ancestor
           :inverse-join-columns descendant :target-table concept)

      (add "relationships" concept concept-relationship concept1)
      ;; TODO(jmoringe): this needs something like "distinct" since
      ;; there can be multiple relations from concept1 to concept2
      ;; which currently leads to duplication in the result list
      ;; Example: ((singleton from ([Concept: Code{code: '1301065', system: 'https://fhir-terminology.ohdsi.org'}])).relatedConcepts) c
      ;;            where c.conceptClassId = 'Substance'
      (add "relatedConcepts" concept concept-relationship concept1
           :inverse-join-columns concept2 :target-table concept)
      ;; TODO(jmoringe): synonyms
      ))
  element)
