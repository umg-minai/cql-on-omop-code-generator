(cl:in-package #:model-info-generator)

(defmethod emit ((element hash-table) (format t) (target pathname))
  (let ((directory (uiop:ensure-directory-pathname target)))
    (a:maphash-values
     (lambda (element)
       (when (output? element)
         (let* ((name       (name element))
                (class-name (translate-class-name name))
                (type       (string-downcase format))
                (pathname   (make-pathname :name class-name
                                           :type type)))
           (emit element format (merge-pathnames pathname directory)))))
     element)))

(defmethod emit ((element table) (format t) (target pathname))
  (a:with-output-to-file (stream target :if-exists :supersede)
    (emit element format stream)))
