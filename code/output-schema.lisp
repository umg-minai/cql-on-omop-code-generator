(cl:in-package #:model-info-generator)

;;; Utilities

(defun without-id (name)
  (let ((index (search "_id" name)))
    (concatenate 'string
                 (subseq name 0 index)
                 (subseq name (+ index 3)))))

;;;

(defclass schema-format ()
  ((%associated-format :initarg  :associated-format
                       :reader   associated-format)
   (%descriptions?     :initarg  :descriptions?
                       :reader   descriptions?
                       :initform t)))

(defmethod cql-type<-omop-table ((format    schema-format)
                                 (omop-table string))
  (cql-type<-omop-table (associated-format format) omop-table)
  #+no (remove #\_ (string-capitalize omop-table)))

(defmethod cql-element<-omop-column ((format      schema-format)
                                     (omop-column string))
  (cql-element<-omop-column (associated-format format) omop-column)
  #+no (string-downcase (remove #\_ (string-capitalize omop-column)) :end 1))

(defmethod cql-type<-omop-type ((format    schema-format)
                                (omop-type string))
  (cond ((string= omop-type "date")                 "System.Date")
        ((string= omop-type "datetime")             "System.DateTime")
        ((or (string= omop-type "integer")
             (string= omop-type "Integer"))         "System.Integer")
        ((string= omop-type "float")                "System.Decimal")
        ((a:starts-with-subseq "varchar" omop-type) "System.String")
        (t                                          omop-type)))

(defmethod emit ((element data-model)
                 (format  schema-format)
                 (target  pathname))
  (let* ((directory (uiop:ensure-directory-pathname target))
         (name      (name element))
         (version   (version element))
         (filename  (format nil "~A~A" name version))
         (filename  (make-pathname :name filename :type "xml"))
         (pathname  (merge-pathnames filename directory)))
    (a:with-output-to-file (stream pathname :if-exists :supersede)
      (cxml:with-xml-output (cxml:make-character-stream-sink stream :indentation 4)
        (cxml:with-element* ("ns4" "modelInfo")
          (cxml:attribute "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance")
          (cxml:attribute "xmlns:ns4" "urn:hl7-org:elm-modelinfo:r1")
          (cxml:attribute "name"      name)
          (cxml:attribute "version"   version)
          (cxml:attribute "url"       (format nil "urn:ohdsi:omop-types:~A" version))
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
                     (when (equal name "Patient")
                       (cxml:attribute "birthDateElement" "birthDatetime"))
                     (cxml:with-element* ("ns4" "contextType")
                       (cxml:attribute "namespace" namespace)
                       (cxml:attribute "name"      type)))))
            (emit-context "Practitioner" "id" name "Person")
            (emit-context "Patient"      "id" name "Person"))

          ;; Emit conversion infos.
          (mapc (lambda (conversion)
                  ;; TODO(jmoringe): Conversion to Interval doesn't
                  ;; work properly at the moment. Skip for now.
                  (unless (typep conversion 'to-interval-conversion)
                    (emit conversion format target)))
                (sorted-elements (conversions element)
                                 :key (lambda (conversion)
                                        (concatenate 'string
                                                     (name (from-table conversion))
                                                     (to-type conversion))))))))))

(defun maybe-emit-description-elements (element format)
  (when (descriptions? format)
    (a:when-let ((string (description element '(:description :user-guidance))))
      (cxml:attribute "definition" string))
    (a:when-let ((string (description element :etl-conventions)))
      (cxml:attribute "comment" string))))

(defmethod emit ((element table) (format schema-format) (target t))
  (cxml:with-element* ("ns4" "typeInfo")
    (cxml:attribute "xsi:type"    "ns4:ClassInfo")
    (cxml:attribute "namespace"   "OMOP")
    (cxml:attribute "name"        (cql-type<-omop-table
                                   format (name element)))
    (cxml:attribute "baseType"    "System.Any")
    (cxml:attribute "retrievable" "true")
    (maybe-emit-description-elements element format)
    ;; Primary code path for condition occurrence
    (a:when-let ((concept-column (canonical-concept-column element)))
      (let ((path (cql-element<-omop-column
                   format (without-id (name concept-column)))))
        (cxml:attribute "primaryCodePath" path)))
    ;; Add elements for columns and additional relations.
    (mapc (a:rcurry #'emit format target)
          (sorted-elements
           (append (columns element) (extra-relations element))))))

(defmethod emit ((element column) (format schema-format) (target t))
  (let ((name      (name element))
        (data-type (data-type element)))
    (cxml:with-element* ("ns4" "element")
      (cxml:attribute "name" (cql-element<-omop-column format name))
      (cxml:attribute "type" (cql-type<-omop-type format data-type))
      (maybe-emit-description-elements element format))

    ;; If there is a foreign key, emit a property for accessing the
    ;; other end of the relation.
    (a:when-let ((foreign-key (foreign-key element)))
      (let* ((name      (without-id name))
             (type      (cql-type<-omop-table
                         format (name (table foreign-key))))
             (data-type type #++ (format nil "OMOP.~A" ; TODO: do this properly
                                type)))
        (cxml:with-element* ("ns4" "element")
          (cxml:attribute "name" (cql-element<-omop-column
                                  format name))
          (cxml:attribute "type" data-type))))))

(defmethod emit ((element extra-relation)
                 (format  schema-format)
                 (target  t))
  (let ((name (name element))
        (type (cql-type<-omop-table
               format (name (target-table element)))))
    (cxml:with-element* ("ns4" "element")
      (cxml:attribute "name" name)
      (cxml:with-element* ("ns4" "elementTypeSpecifier")
        (cxml:attribute "elementType" type)
        (cxml:attribute "xsi:type"    "ns4:ListTypeSpecifier")))))

;;; Conversion

(defmethod emit ((element conversion) (format schema-format) (target t))
  (cxml:with-element* ("ns4" "conversionInfo")
    (cxml:attribute "fromType"     (format nil "OMOP.~A"
                                           (cql-type<-omop-table
                                            format (name (from-table element)))))
    (cxml:attribute "toType"       (to-type element))
    (cxml:attribute "functionName" (format nil "OMOPHelpers.~A"
                                           (function-name element)))))
