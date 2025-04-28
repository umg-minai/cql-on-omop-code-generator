(cl:in-package #:model-info-generator)

(defun do-it (version format target)
  (let ((data-model (reduce #'funcall '(add-conversions
                                        add-extra-relations
                                        add-compound-keys)
                            :initial-value (load-data-model version)
                            :from-end      t)))
    (emit data-model format target)
    data-model))

;; (do-it "v5.4" :go-project #P"~/code/cql/cql-on-omop-in-go/")
