(cl:in-package #:model-info-generator.cql)

(defmethod mi:emit ((element mi:data-model)
                    (format  (eql :helpers))
                    (target  pathname))
  (let* ((directory (uiop:ensure-directory-pathname target))
         (name      (mi:name element))
         (version   (mi:version element))
         (base-name (format nil "OMOPHelpers~A" version))
         (filename  (make-pathname :name base-name :type "cql"))
         (pathname  (merge-pathnames filename directory)))
    (a:with-output-to-file (stream pathname :if-exists :supersede)
      (format stream "// This file has been generated from a description of the OMOP CMD ~A - do not edit~@
                      ~@
                      library OMOPHelpers version '1.0'~@
                      ~@
                      using ~A version '~A'~2%"
              version name version)
      (let ((by-target (make-hash-table :test #'equal)))
        (mapc (lambda (conversion)
                (let ((key (mi::to-type conversion)))
                  (push conversion (gethash key by-target '()))))
              (mi::conversions element))
        (loop :with sorted = (sort (a:hash-table-alist by-target) #'string<
                                   :key #'car)
              :for (to-type . conversions) :in sorted
              :do (format stream "// Conversion to ~A~2%" to-type)
                  (mapc (a:rcurry #'mi:emit format stream)
                        (mi:sorted-elements
                         conversions :key (a:compose #'mi:name #'mi::from-table))))))))

(defmethod mi::from-type ((element mi::conversion))
  (format nil "OMOP.~A"
          (remove #\_ (string-capitalize (mi:name (mi::from-table element))))))

(defmethod mi:emit :around ((element mi::conversion)
                            (format  (eql :helpers))
                            (target  stream))
  (format target "define function ~A(OMOPObject ~A):~%  "
          (mi::function-name element) (mi::from-type element))
  (pprint-logical-block (target (list element))
    (call-next-method element format target))
  (format target "~%"))

(defmethod mi:emit ((element mi::to-code-conversion)
                    (format  (eql :helpers))
                    (target  stream))
  (let ((name (mi:name (mi:column element))))
    (format target "System.Code{~@:_~
                    ~2@Tcode:    ToString(OMOPObject.~A),~@:_~
                    ~2@Tsystem:  'https://fhir-terminology.ohdsi.org' //OMOPObject~@[.~A~].vocabularyId,~@:_~
                    //~2@Tdisplay: OMOPObject~:*~@[.~A~].conceptName~@:_~
                    }~@:_"
            (string-downcase (remove #\_ (string-capitalize name)) :end 1)
            (unless (equal (mi:name (mi::from-table element)) "concept")
              (string-downcase (remove #\_ (string-capitalize (mi::without-id name))) :end 1)))))

(defmethod mi:emit ((element mi::to-concept-conversion)
                    (format  (eql :helpers))
                    (target  stream))
  (format target "System.Concept{~@:_~
                  ~2@Tcodes: { ToCode(OMOPObject) }~@:_~
                  }~@:_"))

(defmethod mi::from-type ((element mi::list-to-concept-conversion))
  (format nil "List<~A>" (call-next-method)))

(defmethod mi:emit ((element mi::list-to-concept-conversion)
                    (format  (eql :helpers))
                    (target  stream))
  (format target "System.Concept{~@:_~
                  ~2@Tcodes: (OMOPObject) o return all ToCode(o)~@:_~
                  }~@:_"))

(defmethod mi:emit ((element mi::to-quantity-conversion)
                    (format  (eql :helpers))
                    (target  stream))
  (format target "if OMOPObject.~A is null or OMOPObject.~A is null then~@:_~
                  ~2@Tnull~@:_~
                  else~@:_~
                  ~2@TSystem.Quantity{~@:_~
                  ~4@Tvalue: OMOPObject.~2:*~A,~@:_~
                  ~4@Tunit:  OMOPObject.~A.conceptCode~@:_~
                  ~2@T}~@:_"
          (string-downcase (remove #\_ (string-capitalize (mi:name (mi::value-column element)))) :end 1)
          (string-downcase (remove #\_ (string-capitalize (mi::without-id (mi:name (mi::unit-column element))))) :end 1)))

(defmethod mi:emit ((element mi::drug-strength-to-quantity-conversion)
                    (format  (eql :helpers))
                    (target  stream))
  (format
   target
   "case~@:_~
    ~2@T// Try amount{Value,UnitConcept}~@:_~
    ~2@Twhen OMOPObject.amountValue is not null and OMOPObject.amountUnitConcept is not null then~@:_~
    ~2@T  System.Quantity{~@:_~
    ~2@T    value: OMOPObject.amountValue,~@:_~
    ~2@T    unit:  OMOPObject.amountUnitConcept.conceptCode~@:_~
    ~2@T  }~@:_~
    ~2@T// If amount is not available, try numerator{Value,UnitConcept} and optionally denominator{Value,UnitConcept}~@:_~
    ~2@Twhen OMOPObject.numeratorValue is not null and OMOPObject.numeratorUnitConcept is not null then~@:_~
    ~2@T  (1) _~@:_~
    ~2@T    let numerator: System.Quantity{~@:_~
    ~2@T                     value: OMOPObject.numeratorValue,~@:_~
    ~2@T                     unit:  OMOPObject.numeratorUnitConcept.conceptCode~@:_~
    ~2@T                   }~@:_~
    ~2@T      return case~@:_~
    ~2@T               // If there is no denominatorValue, just use the quantity computed from the numerator~@:_~
    ~2@T               when OMOPObject.denominatorValue is null then~@:_~
    ~2@T                 numerator~@:_~
    ~2@T               // If there are both denominator{Value,UnitConcept}, compute the fraction numerator/denominator.~@:_~
    ~2@T               when OMOPObject.denominatorUnitConcept is not null then~@:_~
    ~2@T                 numerator / System.Quantity{~@:_~
    ~2@T                               value: OMOPObject.denominatorValue,~@:_~
    ~2@T                               unit:  OMOPObject.denominatorUnitConcept.conceptCode~@:_~
    ~2@T                             }~@:_~
    ~2@T               // If there is denominatorValue but no denominatorUnitConcept, we can't compute a valid Quantity~@:_~
    ~2@T               else~@:_~
    ~2@T                 null~@:_~
    ~2@T             end~@:_~
    ~2@T// If there is neither amount{Value,UnitConcept} nor numerator{Value,UnitConcept}, we can't compute a valid Quantity~@:_~
    ~2@Telse~@:_~
    ~2@T  null~@:_~
    end~@:_~"))

(defmethod mi:emit ((element mi::drug-exposure-to-quantity-conversion)
                    (format  (eql :helpers))
                    (target  stream))
  (format
   target
   "(1) _~@:_~
    ~2@Tlet drugCode:         ToCode(OMOPObject),~@:_~
    ~2@T    // Parenthesis around the retrieve expression are required due to a quirk of~@:_~
    ~2@T    // the CQL grammar.~@:_~
    ~2@T    // Also note that the singleton from construct makes this function error if~@:_~
    ~2@T    // there is more than DrugStrength associated with the code.~@:_~
    ~2@T    strength:         singleton from ([DrugStrength: drugConcept ~~ drugCode]),~@:_~
    ~2@T    strengthQuantity: ToQuantity(strength)~@:_~
      ~4@Treturn if strengthQuantity is not null and OMOPObject.quantity is not null then~@:_~
        ~6@TOMOPObject.quantity * strengthQuantity~@:_~
      ~4@Telse~@:_~
        ~6@Tnull~@:_"))

(defmethod mi:emit ((element mi::to-interval-conversion)
                    (format  (eql :helpers))
                    (target  stream))
  (format target "Interval[OMOPObject.~A, OMOPObject.~A]~@:_"
          (string-downcase (remove #\_ (string-capitalize (mi:name (mi::start-column element)))) :end 1)
          (string-downcase (remove #\_ (string-capitalize (mi:name (mi::end-column element)))) :end 1)))

(defmethod mi:emit ((element mi::to-time-conversion)
                    (format  (eql :helpers))
                    (target  stream))
  (format target "OMOPObject.~A~@:_"
          (string-downcase (remove #\_ (string-capitalize (mi:name (mi:column element)))) :end 1)))
