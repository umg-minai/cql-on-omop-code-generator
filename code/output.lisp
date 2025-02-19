(cl:in-package #:model-info-generator)

;;; HACK(jmoringe): should not be an `:around' method
(defmethod emit :around ((element data-model) (format t) (target pathname))
  (assert (uiop:directory-pathname-p target))
  (let* ((version          (remove #\. (version element)))
         (versioned-target (merge-pathnames
                            (make-pathname :directory `(:relative ,version))
                            target)))
    (call-next-method element format versioned-target)))

(defmethod emit ((element data-model) (format t) (target pathname))
  (assert (uiop:directory-pathname-p target))
  (ensure-directories-exist target)
  (a:maphash-values
   (lambda (element)
     (when (output? element)
       (let* ((name       (name element))
              (class-name (translate-class-name name))
              (type       (string-downcase format))
              (filename   (make-pathname :name class-name
                                         :type type))
              (pathname   (merge-pathnames filename target)))
         (emit element format pathname))))
   (tables element)))

(defmethod emit ((element table) (format t) (target pathname))
  (a:with-output-to-file (stream target :if-exists :supersede)
    (emit element format stream)))
