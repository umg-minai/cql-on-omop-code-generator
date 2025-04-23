(defsystem "model-info-generator"
  :description "Generates CQL model info and model implementation for the OMOP CDM"

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
                 :components ((:file "add-conversions")))


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
