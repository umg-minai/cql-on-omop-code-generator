(cl:in-package #:model-info-generator)

(defmethod prepare-model ((model-designator omop-cdm))
  (reduce (lambda (transform model)
            (format *trace-output* ";; Applying ~S~%" transform)
            (funcall transform model))
          '(add-conversions
            add-extra-relations
            add-compound-keys
            manual-compound-keys
            remove-cohort)
          :initial-value (load-data-model model-designator)
          :from-end      t))

(defun generate-code (model format target)
  "Generate code for VERSION of OMOP CDM with FORMAT into TARGET.

MODEL must be a `omop-cdm' instance which designates a source
directory and version of the OMOP CDM. Currently, \"v5.3\" and
\"v5.4\" are the only supported versions.

FORMAT selects what should be generated.  Should be either
`:go-project' or `:java-project'.  Other values will be accepted but
those are for internal use at the moment.

TARGET must be a string or pathname which designates the directory
into which the generated code should be written.

Example:

  (generate-code (omop-cdm \"/tmp/commondatamodel\" \"v5.4\")
                 :java-project
                 \"~/code/cql/cql-on-omop-in-java/\")"
  (let ((data-model (prepare-model model))
        (target     (pathname target)))
    (emit data-model format target)
    data-model))
