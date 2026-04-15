(cl:in-package #:model-info-generator)

(defmethod emit ((element data-model)
                 (format  (eql :helpers))
                 (target  pathname))
  (let* ((directory (uiop:ensure-directory-pathname target))
         (name      (name element))
         (version   (version element))
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
                (let ((key (to-type conversion)))
                  (push conversion (gethash key by-target '()))))
              (conversions element))
        (loop :with sorted = (sort (a:hash-table-alist by-target) #'string<
                                   :key #'car)
              :for (to-type . conversions) :in sorted
              :do (format stream "// Conversion to ~A~2%" to-type)
                  (mapc (a:rcurry #'emit format stream)
                        (sorted-elements
                         conversions :key (a:compose #'name #'from-table))))))))

(defmethod from-type ((element conversion))
  (format nil "OMOP.~A"
          (remove #\_ (string-capitalize (name (from-table element))))))

(defmethod emit :around ((element conversion)
                         (format  (eql :helpers))
                         (target  stream))
  (format target "define function ~A(OMOPObject ~A):~%  "
          (function-name element) (from-type element))
  (pprint-logical-block (target (list element))
    (call-next-method element format target))
  (format target "~%"))

(defmethod emit ((element to-code-conversion)
                 (format  (eql :helpers))
                 (target  stream))
  (let ((name (name (column element))))
    (format target "System.Code{~@:_~
                    ~2@Tcode:    ToString(OMOPObject.~A),~@:_~
                    ~2@Tsystem:  'https://fhir-terminology.ohdsi.org' //OMOPObject~@[.~A~].vocabularyId,~@:_~
                    //~2@Tdisplay: OMOPObject~:*~@[.~A~].conceptName~@:_~
                    }~@:_"
            (string-downcase (remove #\_ (string-capitalize name)) :end 1)
            (unless (equal (name (from-table element)) "concept")
              (string-downcase (remove #\_ (string-capitalize (without-id name))) :end 1)))))

(defmethod emit ((element to-concept-conversion)
                 (format  (eql :helpers))
                 (target  stream))
  (format target "System.Concept{~@:_~
                  ~2@Tcodes: { ToCode(OMOPObject) }~@:_~
                  }~@:_"))

(defmethod from-type ((element list-to-concept-conversion))
  (format nil "List<~A>" (call-next-method)))

(defmethod emit ((element list-to-concept-conversion)
                 (format  (eql :helpers))
                 (target  stream))
  (format target "System.Concept{~@:_~
                  ~2@Tcodes: (OMOPObject) o return all ToCode(o)~@:_~
                  }~@:_"))

(defmethod emit ((element to-quantity-conversion)
                 (format  (eql :helpers))
                 (target  stream))

  (format target "if OMOPObject.~A is null or OMOPObject.~A is null then~@:_~
                  ~2@Tnull~@:_~
                  else~@:_~
                  ~2@TSystem.Quantity{~@:_~
                  ~4@Tvalue: OMOPObject.~2:*~A,~@:_~
                  ~4@Tunit:  OMOPObject.~A.conceptCode~@:_~
                  ~2@T}~@:_"
          (string-downcase (remove #\_ (string-capitalize (name (value-column element)))) :end 1)
          (string-downcase (remove #\_ (string-capitalize (without-id (name (unit-column element))))) :end 1)))

(defmethod emit ((element drug-strength-to-quantity-conversion)
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

(defmethod emit ((element drug-exposure-to-quantity-conversion)
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

(defmethod emit ((element to-interval-conversion)
                 (format  (eql :helpers))
                 (target  stream))
  (format target "Interval[OMOPObject.~A, OMOPObject.~A]~@:_"
          (string-downcase (remove #\_ (string-capitalize (name (start-column element)))) :end 1)
          (string-downcase (remove #\_ (string-capitalize (name (end-column element)))) :end 1)))

(defmethod emit ((element to-time-conversion)
                 (format  (eql :helpers))
                 (target  stream))
  (format target "OMOPObject.~A~@:_"
          (string-downcase (remove #\_ (string-capitalize (name (column element)))) :end 1)))
