(cl:in-package #:model-info-generator)

(defmethod add-compound-keys ((element data-model))
  (a:maphash-values (lambda (table)
                      (when (and (null (primary-key table))
                                 (null (compound-key table)))
                        (add-compound-keys table)))
                    (tables element))
  element)

(defmethod add-compound-keys ((element table))
  (let ((columns (remove-if-not #'foreign-key (columns element))))
    (cond ((string= (name element) "drug_strength")
           (setf columns (remove-if-not
                          (lambda (column)
                            (member (name column)
                                    '("drug_concept_id"
                                      "ingredient_concept_id")
                                    :test #'string=))
                          columns))))
    (let ((key (make-instance 'compound-key :parent  element
                                            :columns columns)))
      (loop :for column :in columns
            :do (setf (compound-key column) key))
      (setf (compound-key element) key)))
  element)
