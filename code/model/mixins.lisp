(cl:in-package #:model-info-generator)

(defclass named-mixin ()
  ((%name :initarg #1=:name
          :type    string
          :reader  name))
  (:default-initargs
   #1# (a:required-argument #1#)))

(defmethod pi:print-items append ((object named-mixin))
  `((:name "~S" ,(name object))))

(defun sorted-elements (elements)
  (sort (copy-list elements) #'string< :key #'name))

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
                 :type     (or null string)
                 :reader   description
                 :initform nil)))
