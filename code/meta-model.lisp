(cl:in-package #:model-info-generator)

;;; Model

(defclass named-mixin ()
  ((%name :initarg :name
          :type    string
          :reader  name))
  (:default-initargs
   :name (a:required-argument :name)))

(defmethod pi:print-items append ((object named-mixin))
  `((:name "~S" ,(name object))))

(defclass description-mixin ()
  ((%description :initarg  :description
                 :type     (or null string)
                 :reader   description
                 :initform nil)))

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

(defvar *tables* (make-hash-table :test #'equal))

(defun find-table (name)
  (gethash name *tables*))

(defun (setf find-table) (table name)
  (setf (gethash name *tables*) table))

;;; TODO: does not belong here
(defmethod output? ((element table))
  (not (member (name element) '("concept_ancestors"
                                "concept_relationship")
               :test #'string=)))
