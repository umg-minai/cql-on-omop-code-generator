(cl:in-package #:model-info-generator)

(defmethod changes-for-mimic ((element data-model))
  (let* ((version     (version element))
         (new-version (format nil "~A.MIMIC" version)))
    (reinitialize-instance element :version new-version))
  (a:maphash-values #'changes-for-mimic (tables element))
  element)

(defmethod changes-for-mimic ((element table))
  (mapc #'changes-for-mimic (columns element)))

(defmethod changes-for-mimic ((element column))
  (let ((data-type (data-type element)))
    (cond ((string= data-type "integer")
           (reinitialize-instance element :data-type "bigint"))
          ((a:starts-with-subseq "varchar" data-type)
           (reinitialize-instance element :data-type "text")))))
