(cl:in-package #:model-info-generator)

(defvar *default-transforms* '(add-conversions
                               add-extra-relations
                               add-compound-keys
                               manual-compound-keys
                               remove-cohort))

(defmethod prepare-model ((model-designator omop-cdm)
                          &key (transforms *default-transforms*))
  (reduce (lambda (transform model)
            (format *trace-output* ";; Applying ~S~%" transform)
            (funcall transform model))
          transforms
          :initial-value (load-data-model model-designator)
          :from-end      t))

(defun generate-code (model format target &key (transforms *default-transforms*))
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
  (let ((data-model (prepare-model model :transforms transforms))
        (target     (pathname target)))
    (emit data-model format target)
    data-model))
