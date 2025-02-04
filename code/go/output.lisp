(cl:in-package #:model-info-generator.go)

;;; Utilities

(defun without-id (name)
  (let ((index (search "_id" name)))
    (concatenate 'string
                 (subseq name 0 index)
                 (subseq name (+ index 3)))))

(defun go-type<-omop-type (omop-type)
  (cond ((string= omop-type "date")                 "Date")
        ((string= omop-type "datetime")             "DateTime")
        ((or (string= omop-type "integer")
             (string= omop-type "Integer"))         "int64")
        ((string= omop-type "float")                "float32")
        ((a:starts-with-subseq "varchar" omop-type) "string")
        (t                                          omop-type)))

(defun translate-class-name (omop-name)
  (remove #\_ (string-capitalize omop-name)))

(defun translate-column-name (omop-name)
  (string-downcase (remove #\_ (string-capitalize omop-name)) :end 1))

;;;

(defmethod mi:emit ((element mi:data-model)
                    (format  (eql :go-project))
                    (target  pathname))
  (let* ((directory (uiop:ensure-directory-pathname target))
         (model     (merge-pathnames #P"model/" directory)))
    (mi:emit element :schema  directory)
    (mi:emit element :go model)))

;;;

(defmethod mi:emit ((element mi:data-model)
                    (format  (eql :go))
                    (target  pathname))
  (call-next-method)
  (a:with-output-to-file (stream (merge-pathnames "meta.go" target)
                                 :if-does-not-exist :create
                                 :if-exists         :supersede)
    (g:emitting (stream)
      (g:out "package model~@
              ~@
              import \"reflect\"~@
              import \"context\"~@
              ~@
              import \"github.com/uptrace/bun\"~2%")

      (g:struct "TypeInfo"
        (g:out "Type      reflect.Type~@
              Retriever func (*bun.DB) []any"))

      (g:out "~%var ConceptCache map[int64]*Concept = map[int64]*Concept{}~2%")

      (g:func ("EnsureConcept" '(("context" "context.Context") ("connection" "*bun.DB") ("id" "int64")) "*Concept")
        (g:out "result, found := ConceptCache[id]~@:_")
        (g:if "found"
              "return result"
              (lambda ()
                (g:out "result = &Concept{}~@
                  error := connection.NewSelect().~@
                  ~8@TModel(result).~@
                  ~8@TWhere(\"concept.concept_id = ?\", id).~@
                  ~8@TScan(context)~@:_")
                (g:if "error != nil" "panic(error)")
                (g:out "ConceptCache[id] = result~@
                  return result"))))
      (g:out "~%")

      (g:out "var TypeInfos map[string]TypeInfo = map[string]TypeInfo")
      (g:block ()
        (maphash
         (lambda (name element)
           (declare (ignore name))
           (when (mi:output? element)
             (let ((name (translate-class-name (mi:name element))))
               (g:out "\"~A\": TypeInfo" name)
               (g:block (nil)
                 (g:out "Type:      reflect.TypeFor[~A](),~@
                       Retriever: "
                        name)
                 (g:func (nil '(("connection" "*bun.DB")) "[]any" nil)
                   (g:out "var result []*~A;~@
                         error := connection.NewSelect().~@
                         ~8@TModel(&result).~@
                         ~8@TScan(context.Background())~@:_"
                          name)
                   (g:if "error != nil" "panic(error)")
                   (g:out "finalResult := make([]any, len(result))~@:_")
                   (g:for ("i") "result"
                     (g:out "finalResult[i] = result[i]"))
                   (g:out "return finalResult" stream))
                 (g:out ","))
               (g:out ",~%"))))
         (mi:tables element))))))

(defmethod mi:emit ((element mi:table)
                    (format  (eql :go))
                    (target  stream))
  (g:emitting (target)
    (let* ((name       (mi:name element))
           (class-name (translate-class-name name))
           (columns    (mi:columns element))
           (relations  (remove-if-not #'mi:foreign-key columns)))
      (g:out "package model;~2%")
      (when relations
        (g:out "import \"context\"~2%"))
      (g:out "import \"github.com/uptrace/bun\"~2%")

      (g:struct class-name
        (g:emit-field "bun.BaseModel" nil (format nil "bun:\"table:cds_cdm.~A\"" name))
        (map nil (a:rcurry #'mi:emit format g::*stream*) columns)

        (map nil (lambda (element)
                   (let* ((name                (mi:name element))
                          (base-name           (without-id name))
                          (field-name          (translate-column-name base-name))
                          (foreign-key         (mi:foreign-key element))
                          (foreign-table       (mi:table foreign-key))
                          (foreign-table-name  (mi:name foreign-table))
                          #++ (foreign-column-name (mi:name (mi:column foreign-key)))
                          (data-type           (format nil "*~A" (translate-class-name foreign-table-name)))
                          #++ (annotation          (format nil "bun:\"rel:has-one,join:~A=~A\""
                                                       name foreign-column-name)))
                     (g:out "~2%")
                     (g:emit-field field-name data-type #++ annotation)))
             relations)

        (when (string= name "concept")
          (g:out "~2%")
          (g:emit-field "ancestors"   "[]*Concept")
          (g:out "~2%")
          (g:emit-field "descendants" "[]*Concept")))

      (map nil (lambda (element)
                 (let* ((name           (mi:name element))
                        (base-name      (without-id name))
                        (method-name    (format nil "Ensure~A" (translate-class-name base-name)))
                        (field-name     (translate-column-name base-name))
                        (foreign-key    (mi:foreign-key element))
                        (foreign-table  (mi:table foreign-key))
                        (foreign-table-name  (mi:name foreign-table))
                        (foreign-column-name (mi:name (mi:column foreign-key)))
                        (data-type      (translate-class-name foreign-table-name)))
                   (g:out "~%")
                   (g:method (method-name "o" (format nil "*~A" class-name) '(("connection" "*bun.DB")) (format nil "*~A" data-type))
                     (g:if (lambda () (g:out "o.~A == nil" field-name))
                       (lambda ()
                         (if (equal foreign-table-name "concept")
                             (g:out "v := EnsureConcept(context.Background(), connection, o.~A)~@
                                     o.~A = v"
                                    (translate-class-name name)
                                    field-name)
                             (progn
                               (g:out "v := &~A{}~@
                                       error := connection.NewSelect().~@
                                       ~8@TModel(v).~@
                                       ~8@TWhere(\"~A.~A = ?\", o.~A).~@
                                       ~8@TScan(context.Background())~@:_"
                                      data-type
                                      foreign-table-name foreign-column-name
                                      (translate-class-name name))
                               (g:if "error != nil" "panic(error)")
                               (g:out "o.~A = v" field-name)))))
                     (g:out "return o.~A" field-name))))
           relations)

      (when (string= name "concept")
        (flet ((emit-method (field-name forward-join-column inverse-join-column)
                 (let ((method-name (format nil "Ensure~A" (string-upcase field-name :end 1))))
                   (g:out "~%")
                   (g:method (method-name "o" (format nil "*~A" class-name) '(("connection" "*bun.DB")) "[]*Concept")
                     (g:if (lambda () (g:out "o.~A == nil" field-name))
                       (lambda ()
                         (g:out "v := &[]*Concept{}~@
                                 error := connection.NewSelect().~@
                                 ~8@TModel(v).~@
                                 ~8@TJoin(\"JOIN cds_cdm.concept_ancestor ON concept_ancestor.~A = concept.concept_id\").~@
                                 ~8@TWhere(\"concept_ancestor.~A = ?\", o.ConceptId).~@
                                 ~8@TScan(context.Background())~@:_ "
                                inverse-join-column forward-join-column)
                             (g:if "error != nil" "panic(error)")
                             (g:out "o.~A = *v" field-name)))
                     (g:out "return o.~A" field-name)))))
          (emit-method "ancestors"   "descendant_concept_id" "ancestor_concept_id")
          (emit-method "descendants" "ancestor_concept_id"   "descendant_concept_id"))))))

(defmethod mi:emit ((element mi:column)
                    (format  (eql :go))
                    (target  stream))
  (g:emitting (target)
    (unless (member (mi:data-type element) '("date" "datetime") :test #'string=)
      (g:out "~2%")
      (let* ((name       (mi:name element))
             (field-name (translate-class-name name))
             (data-type  (go-type<-omop-type (mi:data-type element)))
             (primary?   (mi:primary-key? element))
             (required?  (mi:required? element))
             (annotation (format nil "bun:\"~A~:[~;,pk~]~:[~;,notnull~]\""
                                 name primary? required?)))
        (g:emit-field field-name data-type annotation)))))
