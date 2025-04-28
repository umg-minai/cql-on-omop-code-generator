(cl:in-package #:model-info-generator)

;;; `data-model'

(defclass data-model (pi:print-items-mixin named-mixin version-mixin)
  ((%tables      :reader   tables
                 :initform (make-hash-table :test #'equal))
   (%conversions :type     list
                 :accessor conversions
                 :initform '())))

(defmethod pi:print-items append ((object data-model))
  (let ((conversion-count (length (conversions object))))
    `(((:conversion-count (:after :version)) " ~D conversion~:P" ,conversion-count))))

(defmethod find-table ((name string) (data-model data-model))
  (gethash name (tables data-model)))

(defmethod (setf find-table) ((new-value  t)
                              (name       string)
                              (data-model data-model))
  (setf (parent new-value)                 data-model
        (gethash name (tables data-model)) new-value))

;;; `table'

(defclass table (pi:print-items-mixin
                 description-mixin
                 parented-mixin
                 named-mixin)
  ((%columns         :reader   %columns
                     :initform (make-hash-table :test #'equal))
   (%compound-key    :initarg  :compound-key
                     :accessor compound-key
                     :initform nil)
   (%extra-relations :initarg  :extra-relations
                     :type     list
                     :accessor extra-relations
                     :initform '())))

(defmethod columns ((table table))
  (a:hash-table-values (%columns table)))

(defmethod find-column ((name string) (table table))
  (gethash name (%columns table)))

(defmethod (setf find-column) ((new-value t) (name string) (table table))
  (setf (parent new-value)              table
        (gethash name (%columns table)) new-value))

(defmethod primary-key ((table table))
  (find-if #'primary-key? (a:hash-table-values (%columns table))))

;;; `column'

(defclass column (pi:print-items-mixin
                  description-mixin
                  parented-mixin
                  named-mixin)
  ((%parent       :reader   table)
   (%required?    :initarg  :required?
                  :reader   required?)
   (%data-type    :initarg  :data-type
                  :reader   data-type)
   (%primary-key? :initarg  :primary-key?
                  :reader   primary-key?)
   (%forgein-key  :initarg  :foreign-key
                  :reader   foreign-key
                  :initform nil)
   (%compound-key :initarg  :compound-key
                  :accessor compound-key
                  :initform nil)))

(defmethod pi:print-items append ((object column))
  `(((:type (:after :name)) ":~A" ,(data-type object))))

;;; `foreign-key'

(defclass foreign-key (pi:print-items-mixin
                       parented-mixin)
  ((%table  :initarg :table
            :reader  table)
   (%column :initarg :column ; TODO: just stores parent again
            :reader  column)))

(defmethod pi:print-items append ((object foreign-key))
  (let ((table-name  (name (table object)))
        (column-name (name (column object))))
    `(((:table-name)                       "~A"  ,table-name)
      ((:column-name (:after :table-name)) ".~A" ,column-name))))

;;; `compound-key'

(defclass compound-key (pi:print-items-mixin
                        parented-mixin)
  ((%columns :initarg :columns
             :type    list
             :reader  columns)))

(defmethod pi:print-items append ((object compound-key))
  (let ((table-name   (name (parent object)))
        (column-names (mapcar #'name (columns object))))
    `(((:table-name)                        "~A"           ,table-name)
      ((:column-names (:after :table-name)) ".<~{~A~^ ~}>" ,column-names))))

;;; `extra-relation'

(defclass extra-relation (pi:print-items-mixin
                          parented-mixin
                          named-mixin)
  ((%table                :initarg  :table ; foreign table
                          :reader   table)
   (%join-columns         :initarg  :join-columns ; column in the foreign table to join on
                          :reader   join-columns)
   (%inverse-join-columns :initarg  :inverse-join-columns
                          :reader   inverse-join-columns)
   (%target-table         :reader   target-table)))

(defmethod initialize-instance :after ((instance extra-relation)
                                       &key table
                                            inverse-join-columns
                                            (target-table table))
  (unless inverse-join-columns
    (setf (slot-value instance '%inverse-join-columns)
          (a:if-let ((primary-key (primary-key table)))
            (list primary-key)
            (columns (compound-key table)))))
  (setf (slot-value instance '%target-table) target-table))

;;; TODO: does not belong here
(defmethod output? ((element table))
  (not (member (name element) '("concept_ancestors")
               :test #'string=)))
