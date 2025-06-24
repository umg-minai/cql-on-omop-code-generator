(cl:in-package #:model-info-generator)

(defun prepare-model (version)
  (reduce (lambda (transform model)
            (format *trace-output* ";; Applying ~S~%" transform)
            (funcall transform model))
          '(add-conversions
            add-extra-relations
            add-compound-keys
            manual-compound-keys
            remove-cohort)
          :initial-value (load-data-model version)
          :from-end      t))

(defun do-it (version format target)
  (let ((data-model (prepare-model version)))
    (emit data-model format target)
    data-model))

;; (do-it "v5.4" :go-project #P"~/code/cql/cql-on-omop-in-go/")
