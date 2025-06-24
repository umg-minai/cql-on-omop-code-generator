(cl:in-package #:model-info-generator)

(defclass conversion (pi:print-items-mixin)
  ((%from-table    :initarg :from-table
                   :reader  from-table)
   (%to-type       :initarg :to-type
                   :reader  to-type)
   (%function-name :initarg :function-name
                   :reader  function-name)))

(defmethod pi:print-items append ((object conversion))
  (let ((from (name (from-table object))))
   `((:from                           "~A"     ,from)
     ((:to            (:after :from)) " → ~A"  ,(to-type object))
     ((:function-name (:after :to))  " via ~A" ,(function-name object)))))

(defclass to-code-conversion (conversion)
  ((%column :initarg :column
            :reader  column))
  (:default-initargs
   :to-type       "System.Code"
   :function-name "ToCode"))

(defclass to-concept-conversion (conversion)
  ()
  (:default-initargs
   :to-type       "System.Concept"
   :function-name "ToConcept"))

(defclass list-to-concept-conversion (to-concept-conversion) ())

(defclass to-quantity-conversion (conversion)
  ((%value-column :initarg :value-column
                  :reader  value-column)
   (%unit-column  :initarg :unit-column
                  :reader  unit-column))
  (:default-initargs
   :to-type       "System.Quantity"
   :function-name "ToQuantity"))

(defclass to-interval-conversion (conversion)
  ((%start-column :initarg :start-column
                  :reader  start-column)
   (%end-column   :initarg :end-column
                  :reader  end-column))
  (:default-initargs
   :to-type       "System.Interval"
   :function-name "ToInterval"))
