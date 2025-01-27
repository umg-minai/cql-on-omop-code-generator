(cl:in-package #:model-info-generator)

(defun write-schema (version)
  (let* ((directory #P"~/code/cql/cql-on-omop/src/main/resources/org/example/")
         (filename  (make-pathname :name (format nil "OMOPv~A" version)
                                   :type "xml"))
         (pathname  (merge-pathnames filename directory)))
    (a:with-output-to-file (stream pathname :if-exists :supersede) ;TODO: version
      (cxml:with-xml-output (cxml:make-character-stream-sink stream :indentation 4)
        (cxml:with-element* ("ns4" "modelInfo")
          (cxml:attribute "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance")
          (cxml:attribute "xmlns:ns4" "urn:hl7-org:elm-modelinfo:r1")
          (cxml:attribute "name"      "OMOP")
          (cxml:attribute "version"   (format nil "v~A" version))
          (cxml:attribute "url"       (format nil "urn:ohdsi:omop-types:r~A" version))
          ;; TODO: targetQualifier?
          (mapc (a:compose (lambda (element)
                             (when (output? element)
                               (write-element element)))
                           #'cdr)
                (sort (a:hash-table-alist *tables*) #'string< :key #'car)))))))

(defmethod write-element ((element table))
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
        (emit-relation "ancestors")
        (emit-relation "descendants")))

    (map nil #'write-element (columns element))))

(defun translate-class-name (omop-name)
  (remove #\_ (string-capitalize omop-name)))

(defmethod write-element ((element column))
  (let ((name (name element)))
    (cxml:with-element* ("ns4" "element")
      (cxml:attribute "name" (translate-column-name name))
      (cxml:attribute "type" (translate-type (data-type element))))

    (a:when-let ((foreign-key (foreign-key element)))
      (let ((name      (without-id name))
            (data-type (translate-class-name (name (table foreign-key)))))
        (cxml:with-element* ("ns4" "element")
          (cxml:attribute "name" (translate-column-name name))
          (cxml:attribute "type" (translate-type data-type)))))))

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
