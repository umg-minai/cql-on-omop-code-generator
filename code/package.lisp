(cl:defpackage #:model-info-generator
  (:use
   #:cl)

  (:local-nicknames
   (#:a  #:alexandria)
   (#:pi #:utilities.print-items))

  ;; Model
  (:export
   #:name

   #:sorted-elements

   #:version

   #:parent

   #:data-model
   #:tables

   #:table
   #:columns
   #:find-column
   #:primary-key
   #:compound-key
   #:extra-relations

   #:column
   #:data-type
   #:primary-key?
   #:required?
   #:foreign-key

   #:foreign-key
   #:table
   #:column

   #:compound-key
   #:columns

   #:extra-relation
   #:table
   #:join-columns
   #:inverse-join-columns
   #:target-table)

  ;; Emit protocol
  (:export
   #:emit
   #:output?)

  ;; Interface
  (:export
   #:omop-cdm
   #:*default-transforms*
   #:generate-code)

  ;; MIMIC
  (:export
   #:changes-for-mimic
   #:generate-code-for-mimic
   #:generate-schema-changes-for-mimic))
