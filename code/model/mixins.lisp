(cl:in-package #:model-info-generator)

(defclass named-mixin ()
  ((%name :initarg #1=:name
          :type    string
          :reader  name))
  (:default-initargs
   #1# (a:required-argument #1#)))

(defmethod pi:print-items append ((object named-mixin))
  `((:name "~S" ,(name object))))

(defun sorted-elements (elements &key (key #'name))
  (sort (copy-list elements) #'string< :key key))

(defclass version-mixin ()
  ((%version :initarg  #1=:version
             :type     string
             :reader   version))
  (:default-initargs
   #1# (a:required-argument #1#)))

(defmethod pi:print-items append ((object version-mixin))
  `(((:version (:after :name)) " ~S" ,(version object))))

(defclass parented-mixin ()
  ((%parent :initarg  :parent
            :accessor parent
            :initform nil)))

(defclass description-mixin ()
  ((%description :initarg  :description
                 :type     (or null string cons)
                 :reader   %description
                 :initform nil)))

(defmethod description ((element description-mixin) &optional (which t))
  (let ((description (%description element)))
    (cond ((typep description '(or null string))
           description)
          ((typep which '(or (eql t) list))
           (block outer
             (with-output-to-string (stream)
               (loop :with any-match? = nil
                     :for (key value) :on description :by #'cddr
                     :when (or (eq which t) (member key which))
                       :do (if any-match?
                               (format stream "~2%")
                               (setf any-match? t))
                           (write-string value stream)
                     :finally (when (not any-match?)
                                (return-from outer nil))))))
          (t
           (getf description which)))))
