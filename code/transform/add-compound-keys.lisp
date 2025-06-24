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
    (add-compound-key element columns))
  element)
