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
                              (:file "input")
                              ;; Output
                              (:file "output")
                              (:file "output-schema")
                              (:file "output-java")
                              (:file "output-go")
                              ;; Interface
                              (:file "interface")))))
