(cl:in-package #:model-info-generator)

;;; Mixins

(defclass named-mixin ()
  ((%name :initarg #1=:name
          :type    string
          :reader  name))
  (:default-initargs
   #1# (a:required-argument #1#)))

(defmethod pi:print-items append ((object named-mixin))
  `((:name "~S" ,(name object))))

(defclass version-mixin ()
  ((%version :initarg  #1=:version
             :type     string
             :reader   version))
  (:default-initargs
   #1# (a:required-argument #1#)))

(defmethod pi:print-items append ((object version-mixin))
  `(((:version (:after :name)) " ~S" ,(version object))))

(defclass description-mixin ()
  ((%description :initarg  :description
                 :type     (or null string)
                 :reader   description
                 :initform nil)))

;;; `data-model'

(defclass data-model (pi:print-items-mixin named-mixin version-mixin)
  ((%tables :reader   tables
            :initform (make-hash-table :test #'equal))))

(defmethod find-table ((name string) (data-model data-model))
  (gethash name (tables data-model)))

(defmethod (setf find-table) ((new-value  t)
                              (name       string)
                              (data-model data-model))
  (setf (gethash name (tables data-model)) new-value))

;;; `table'

(defclass table (pi:print-items-mixin
                 description-mixin
                 named-mixin)
  ((%columns :reader   %columns
             :initform (make-hash-table :test #'equal))))

(defmethod columns ((table table))
  (a:hash-table-values (%columns table)))

(defmethod find-column ((name string) (table table))
  (gethash name (%columns table)))

(defmethod (setf find-column) ((new-value t) (name string) (table table))
  (setf (gethash name (%columns table)) new-value))

;;; `column'

(defclass column (pi:print-items-mixin
                  description-mixin
                  named-mixin)
  ((%table        :initarg  :table
                  :reader   table)
   (%required?    :initarg  :required?
                  :reader   required?)
   (%data-type    :initarg  :data-type
                  :reader   data-type)
   (%primary-key? :initarg  :primary-key?
                  :reader   primary-key?)
   (%forgein-key  :initarg  :foreign-key
                  :reader   foreign-key
                  :initform nil)))

(defmethod pi:print-items append ((object column))
  `(((:type (:after :name)) ":~A" ,(data-type object))))

(defclass foreign-key (pi:print-items-mixin)
  ((%table  :initarg :table
            :reader  table)
   (%column :initarg :column
            :reader  column)))

(defmethod pi:print-items append ((object foreign-key))
  (let ((table-name  (name (table object)))
        (column-name (name (column object))))
    `(((:table-name)                       "~A"  ,table-name)
      ((:column-name (:after :table-name)) ".~A" ,column-name))))

;;; TODO: does not belong here
(defmethod output? ((element table))
  (not (member (name element) '("concept_ancestors"
                                "concept_relationship")
               :test #'string=)))
