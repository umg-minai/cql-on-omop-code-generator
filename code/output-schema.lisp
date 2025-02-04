(cl:in-package #:model-info-generator)

(defmethod emit ((element data-model)
                 (format  (eql :schema))
                 (target  pathname))
  (let* ((directory (uiop:ensure-directory-pathname target))
         (name      (name element))
         (version   (version element))
         (filename  (format nil "~A~A" name version))
         (filename  (make-pathname :name filename :type "xml"))
         (pathname  (merge-pathnames filename directory)))
    (a:with-output-to-file (stream pathname :if-exists :supersede) ;TODO: version
      (cxml:with-xml-output (cxml:make-character-stream-sink stream :indentation 4)
        (cxml:with-element* ("ns4" "modelInfo")
          (cxml:attribute "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance")
          (cxml:attribute "xmlns:ns4" "urn:hl7-org:elm-modelinfo:r1")
          (cxml:attribute "name"      name)
          (cxml:attribute "version"   version)
          (cxml:attribute "url"       (format nil "urn:ohdsi:omop-types:r~A" version))
          ;; TODO: targetQualifier?
          (mapc (a:compose (lambda (element)
                             (when (output? element)
                               (emit element format target)))
                           #'cdr)
                (sort (a:hash-table-alist (tables element))
                      #'string< :key #'car))

          ;; Emit descriptions of available CQL contexts.
          (flet ((emit-context (name key-element namespace type)
                   (cxml:with-element* ("ns4" "contextInfo")
                     (cxml:attribute "name"       name)
                     (cxml:attribute "keyElement" key-element)
                     ;; TODO birthDateElement
                     (cxml:with-element* ("ns4" "contextType")
                       (cxml:attribute "namespace" namespace)
                       (cxml:attribute "name"      type)))))
            (emit-context "Practitioner" "id" name "Person")
            (emit-context "Patient"      "id" name "Person"))

          ;; Emit conversion infos.
          (flet ((emit-conversion (from to function)
                   (cxml:with-element* ("ns4" "conversionInfo")
                     (cxml:attribute "fromType"     from)
                     (cxml:attribute "toType"       to)
                     (cxml:attribute "functionName" function))))
            (emit-conversion "OMOP.Concept" "System.Concept" "OMOPHelpers.ToConcept")))))))

(defmethod emit ((element table) (format (eql :schema)) (target t))
  (cxml:with-element* ("ns4" "typeInfo")
    (cxml:attribute "xsi:type"    "ns4:ClassInfo")
    (cxml:attribute "namespace"   "OMOP")
    (cxml:attribute "name"        (translate-class-name (name element)))
    (cxml:attribute "baseType"    "System.Any")
    (cxml:attribute "retrievable" "true")
    (a:when-let ((description (description element)))
      (cxml:attribute "description" description))

    ;; Primary code path for condition occurrence
    (when (string= (name element) "condition_occurrence")
      (let ((code (find-column "condition_concept_id" element)))
        (assert code)
        (cxml:attribute "primaryCodePath" (translate-column-name
                                           (without-id
                                               (name code))))))

    (when (string= (name element) "concept")
      (flet ((emit-relation (name)
               (cxml:with-element* ("ns4" "element")
                 (cxml:attribute "name" name)
                 (cxml:with-element* ("ns4" "elementTypeSpecifier")
                   (cxml:attribute "elementType" "OMOP.Concept")
                   (cxml:attribute "xsi:type"    "ns4:ListTypeSpecifier")))))
        (emit-relation "Ancestors")
        (emit-relation "Descendants")))

    (map nil (a:rcurry #'emit format target) (columns element))))

(defun translate-class-name (omop-name)
  (remove #\_ (string-capitalize omop-name)))

(defmethod emit ((element column) (format (eql :schema)) (target t))
  (let ((name (name element)))
    (cxml:with-element* ("ns4" "element")
      (cxml:attribute "name" (translate-class-name name))
      (cxml:attribute "type" (translate-type (data-type element))))

    ;; If there is a foreign key, emit a property for accessing the
    ;; other end of the relation.
    (a:when-let ((foreign-key (foreign-key element)))
      (let ((name      (without-id name))
            (data-type (format nil "OMOP.~A" ; TODO: do this properly
                               (translate-class-name (name (table foreign-key))))))
        (cxml:with-element* ("ns4" "element")
          (cxml:attribute "name" (translate-class-name name))
          (cxml:attribute "type" data-type))))))

(defun translate-column-name (omop-name)
  (string-downcase (remove #\_ (string-capitalize omop-name)) :end 1))

(defun translate-type (omop-type)
  (cond ((string= omop-type "date")                 "System.Date")
        ((string= omop-type "datetime")             "System.DateTime")
        ((or (string= omop-type "integer")
             (string= omop-type "Integer"))         "System.Integer")
        ((string= omop-type "float")                "System.Decimal")
        ((a:starts-with-subseq "varchar" omop-type) "System.String")
        (t                                          omop-type)))

;; TODO
#|
<conversionInfo functionName="FHIRHelpers.ToString" fromType="FHIR.ContractStatus" toType="System.String"/>
   <conversionInfo functionName="FHIRHelpers.ToString" fromType="FHIR.CodeSystemHierarchyMeaning" toType="System.String"/>
   <conversionInfo functionName="FHIRHelpers.ToString" fromType="FHIR.VisionBase" toType="System.String"/>
   <conversionInfo functionName="FHIRHelpers.ToString" fromType="FHIR.BundleType" toType="System.String"/>
   <conversionInfo functionName="FHIRHelpers.ToString" fromType="FHIR.DocumentConfidentiality" toType="System.String"/>
   <contextInfo name="Practitioner" keyElement="id">
      <contextType namespace="FHIR" name="Practitioner"/>
   </contextInfo>
   <contextInfo name="Device" keyElement="id">
      <contextType namespace="FHIR" name="Device"/>
   </contextInfo>
   <contextInfo name="Patient" keyElement="id" birthDateElement="birthDate.value">
      <contextType namespace="FHIR" name="Patient"/>
   </contextInfo>
|#
