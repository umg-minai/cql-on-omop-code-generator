(cl:in-package #:model-info-generator.cql)

(defun emit-quantity (value unit)
  (c:instance "System.Quantity" :value value :unit unit))

(defun emit-code (code system)
  (c:instance "System.Code" :code code :system system))

(defun emit-concept (codes)
  (c:instance "System.Concept" :codes codes))
(defmethod mi:emit ((element mi:data-model)
                    (format  (eql :helpers))
                    (target  pathname))
  (let* ((directory (uiop:ensure-directory-pathname target))
         (name      (mi:name element))
         (version   (mi:version element))
         (base-name (format nil "OMOPHelpers~A" version))
         (filename  (make-pathname :name base-name :type "cql")))
    (c:with-output-to-cql-file (directory "OMOPHelpers"
                                          :filename          filename
                                          :library-version   "1.0"
                                          :generation-source (format nil "a description of the OMOP CMD ~A"
                                                                     version))
      (c:out "using ~A version '~A'~2%" name version)
      (let ((by-target (make-hash-table :test #'equal)))
        (mapc (lambda (conversion)
                (let ((key (mi::to-type conversion)))
                  (push conversion (gethash key by-target '()))))
              (mi::conversions element))
        (loop :with sorted = (sort (a:hash-table-alist by-target) #'string<
                                   :key #'car)
              :for (to-type . conversions) :in sorted
              :do (c:section "Conversion to ~A" to-type)
                  (mapc (a:rcurry #'mi:emit format c::*stream*)
                        (mi:sorted-elements
                         conversions :key (a:compose #'mi:name #'mi::from-table))))))))

(defmethod mi::from-type ((element mi::conversion))
  (format nil "OMOP.~A"
          (remove #\_ (string-capitalize (mi:name (mi::from-table element))))))

(defmethod mi:emit :around ((element mi::conversion)
                            (format  (eql :helpers))
                            (target  stream))
  (c:function ((mi::function-name element)
               `(("OMOPObject" ,(mi::from-type element))))
    (call-next-method element format target))
  (pprint-newline :mandatory target))

(defmethod mi:emit ((element mi::to-code-conversion)
                    (format  (eql :helpers))
                    (target  stream))
  (let* ((name             (mi:name (mi:column element)))
         (code-property    (string-downcase (remove #\_ (string-capitalize name)) :end 1))
         (concept-property (unless (equal (mi:name (mi::from-table element)) "concept")
                             (string-downcase (remove #\_ (string-capitalize (mi::without-id name))) :end 1))))
    (emit-code (lambda () (c:out "ToString(OMOPObject.~A)" code-property))
               (lambda ()
                 (c:out "'https://fhir-terminology.ohdsi.org' //OMOPObject~@[.~A~].vocabularyId"
                        concept-property))
                #++ :display #++ (lambda ()
                                   (c:out "OMOPObject~@[.~A~].conceptName"
                                          concept-property)))))

(defmethod mi:emit ((element mi::to-concept-conversion)
                    (format  (eql :helpers))
                    (target  stream))
  (emit-concept "{ ToCode(OMOPObject) }"))

(defmethod mi::from-type ((element mi::list-to-concept-conversion))
  (format nil "List<~A>" (call-next-method)))

(defmethod mi:emit ((element mi::list-to-concept-conversion)
                    (format  (eql :helpers))
                    (target  stream))
  (emit-concept "(OMOPObject) o return all ToCode(o)"))

(defmethod mi:emit ((element mi::to-quantity-conversion)
                    (format  (eql :helpers))
                    (target  stream))
  (let ((value-attribute (string-downcase (remove #\_ (string-capitalize (mi:name (mi::value-column element)))) :end 1))
        (unit-attribute  (string-downcase (remove #\_ (string-capitalize (mi::without-id (mi:name (mi::unit-column element))))) :end 1)))
    (c:if (lambda ()
            (c:or (lambda () (c:out "OMOPObject.~A is null" value-attribute))
                  (lambda () (c:out "OMOPObject.~A is null" unit-attribute))))
          "null"
          (lambda ()
            (emit-quantity (lambda ()
                             (c:out "OMOPObject.~A" value-attribute))
                           (lambda ()
                             (c:out "OMOPObject.~A.conceptCode"
                                    unit-attribute)))))))

(defmethod mi:emit ((element mi::drug-strength-to-quantity-conversion)
                    (format  (eql :helpers))
                    (target  stream))
  (c:cond ((lambda () (c:and "OMOPObject.amountValue is not null"
                             "OMOPObject.amountUnitConcept is not null"))
           (lambda ()
             (c:comment "Use amount{Value,UnitConcept} if available.")
             (emit-quantity "OMOPObject.amountValue"
                            "OMOPObject.amountUnitConcept.conceptCode")))
          ((lambda () (c:and "OMOPObject.numeratorValue is not null"
                             "OMOPObject.numeratorUnitConcept is not null"))
           (lambda ()
             (c:comment "If amount is not available, try ~
                         numerator{Value,UnitConcept} and optionally ~
                         denominator{Value,UnitConcept}")
             (c:let (("numerator" (lambda ()
                                    (emit-quantity
                                     "OMOPObject.numeratorValue"
                                     "OMOPObject.numeratorUnitConcept.conceptCode"))))
               (lambda ()
                 (c:cond ("OMOPObject.denominatorValue is null"
                          (lambda ()
                            (c:comment "If there is no denominatorValue, just ~
                                        use the quantity computed from the ~
                                        numerator.")
                            (c:out "numerator")))
                         ("OMOPObject.denominatorUnitConcept is not null"
                          (lambda ()
                            (c:out "numerator / ")
                            (emit-quantity
                             "OMOPObject.denominatorValue"
                             "OMOPObject.denominatorUnitConcept.conceptCode")))
                         (t
                          (lambda ()
                            (c:comment "If there is denominatorValue but no ~
                                        denominatorUnitConcept, we can't ~
                                        compute a valid Quantity")
                            (c:out "null"))))))))
          (t
           (lambda ()
             (c:comment "If there is neither amount{Value,UnitConcept} nor ~
                         numerator{Value,UnitConcept}, we can't compute a ~
                         valid Quantity.")
             (c:out "null")))))

(defmethod mi:emit ((element mi::drug-exposure-to-quantity-conversion)
                    (format  (eql :helpers))
                    (target  stream))
  (c:let (("drugCode"         "ToCode(OMOPObject)")
          (lambda ()
            (c:comment "Parenthesis around the retrieve expression are ~
                        required due to a quirk of the CQL grammar.")
            (c:comment "Also note that the singleton from construct makes this ~
                        function error if there is more than DrugStrength ~
                        associated with the code."))
          ("strength"         "singleton from ([DrugStrength: drugConcept ~ drugCode])")
          ("strengthQuantity" "ToQuantity(strength)"))
    (lambda ()
      (c:if (lambda () (c:and "strengthQuantity is not null"
                              "OMOPObject.quantity is not null"))
            "OMOPObject.quantity * strengthQuantity"
            "null"))))

(defmethod mi:emit ((element mi::to-interval-conversion)
                    (format  (eql :helpers))
                    (target  stream))
  (c:out "Interval[OMOPObject.~A, OMOPObject.~A]"
         (string-downcase (remove #\_ (string-capitalize (mi:name (mi::start-column element)))) :end 1)
         (string-downcase (remove #\_ (string-capitalize (mi:name (mi::end-column element)))) :end 1)))

(defmethod mi:emit ((element mi::to-time-conversion)
                    (format  (eql :helpers))
                    (target  stream))
  (c:out "OMOPObject.~A"
         (string-downcase (remove #\_ (string-capitalize (mi:name (mi:column element)))) :end 1)))
