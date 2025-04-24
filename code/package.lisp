(cl:defpackage #:model-info-generator
  (:use
   #:cl)

  (:local-nicknames
   (#:a  #:alexandria)
   (#:pi #:utilities.print-items))

  (:export
   #:name

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

  (:export
   #:emit
   #:output?))
