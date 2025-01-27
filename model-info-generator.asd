(defsystem "model-info-generator"
  :description "Generates CQL model info and model implementation for the OMOP CDM"

  :depends-on  ("alexandria"
                "utilities.print-items"

                "vellum-csv"

                "cxml")

  :components  ((:module     "code"
                 :serial     t
                 :components ((:file "package")
                              (:file "meta-model")
                              (:file "input")
                              (:file "output-xml")
                              (:file "output-java")
                              (:file "interface")))))
