(cl:in-package #:model-info-generator)

(defun do-it (version)
  (load-tables version)
  (load-fields version)

  (write-schema version)
  (emit *tables* #P"~/code/cql/cql-on-omop/src/main/java/OMOP/"))
