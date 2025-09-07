(defsystem "model-info-generator"
  :description "Generates CQL model info and model implementation for the OMOP CDM"
  :license "Apache 2.0" ; see COPYING

  :depends-on  ("alexandria"
                "utilities.print-items"

                "cxml")

  :components  ((:module     "code"
                 :serial     t
                 :components ((:file "package")
                              (:file "protocol")
                              (:module     "model"
                               :serial     t
                               :components ((:file "mixins")
                                            (:file "omop")
                                            (:file "cql")))
                              (:file "predicates")
                              ;; Input
                              (:file "csv")
                              (:file "input")
                              ;; Output
                              (:file "output")
                              (:file "output-schema")
                              (:file "output-helpers")
                              ;; Interface
                              (:file "interface")))

                (:module     "transform"
                 :pathname   "code/transform"
                 :depends-on ("code")
                 :components ((:file "add-conversions")
                              (:file "add-compound-keys")
                              (:file "add-extra-relations")
                              (:file "remove-cohort")
                              (:file "manual-compound-keys")))


                (:module     "java"
                 :pathname   "code/java"
                 :depends-on ("code")
                 :serial     t
                 :components ((:file "syntax")
                              (:file "package")
                              (:file "output")))

                (:module     "go"
                 :pathname   "code/go"
                 :depends-on ("code")
                 :serial     t
                 :components ((:file "syntax")
                              (:file "package")
                              (:file "output")))))

(defsystem "model-info-generator/mimic"
  :description "Generate schemas and code specifically for MIMIC data"

  :depends-on  ("model-info-generator")

  :components  ((:module     "code"
                 :components ((:file "interface-mimic")))

                (:module     "transform"
                 :pathname   "code/transform"
                 :components ((:file "changes-for-mimic")))

                (:module     "sql"
                 :pathname   "code/sql"
                 :components ((:file "mimic-schema-changes")))))
