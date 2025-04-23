(cl:in-package #:model-info-generator)

(defmethod add-compound-keys ((element data-model))
  (a:maphash-values (lambda (table)
                      (when (null (primary-key table))
                        (add-compound-keys table)))
                    (tables element))
  element)

(defmethod add-compound-keys ((element table))
  (let* ((columns (remove-if-not #'foreign-key (columns element)))
         (key     (make-instance 'compound-key :parent  element
                                               :columns columns)))
    (loop :for column :in columns
          :do (setf (compound-key column) key))
    (setf (compound-key element) key))
  element)
