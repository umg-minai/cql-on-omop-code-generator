(defsystem "model-info-generator"
  :description "Generates CQL model info and model implementation for the OMOP CDM"

  :depends-on  ("alexandria"
                "utilities.print-items"

                "vellum-csv"

                "cxml")

  :components  ((:module     "code"
                 :serial     t
                 :components ((:file "package")
                              (:file "protocol")
                              (:file "meta-model")
                              (:file "predicates")
                              (:file "input")
                              ;; Output
                              (:file "output")
                              (:file "output-schema")
                              ;; Interface
                              (:file "interface")))

                (:module     "java"
                 :pathname   "code/java"
                 :serial     t
                 :components ((:file "syntax")
                              (:file "package")
                              (:file "output")))

                (:module     "go"
                 :pathname   "code/go"
                 :serial     t
                 :components ((:file "syntax")
                              (:file "package")
                              (:file "output")))))
