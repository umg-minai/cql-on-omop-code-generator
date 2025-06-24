(cl:in-package #:model-info-generator)

(defmethod cdm-source-keys ((element data-model))
  (a:when-let ((table (find-table "cdm_source" element)))
    (cdm-source-keys table))
  element)

(defmethod cdm-source-keys ((element table))
  (let* ((columns (mapcar (a:rcurry #'find-column element)
                          '("cdm_source_name" "cdm_version")))
         (key     (make-instance 'compound-key :columns columns)))
    (setf (compound-key element) key)))
