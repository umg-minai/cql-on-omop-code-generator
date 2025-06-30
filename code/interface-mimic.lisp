(cl:in-package #:model-info-generator)

;;; Load OMOP CDM version specified by MODEL, perform adaptation for
;;; MIMIC, then emit FORMAT into the destination specified by TARGET.
(defun generate-code-for-mimic (model format target)
  (generate-code model format target
                 :transforms (append *default-transforms*
                                     '(changes-for-mimic))))

;;; Load OMOP CDM version specified by MODEL, then emit SQL code into
;;; the destination specified by TARGET for changing the schema of a
;;; database from vanilla OMOP CDM to OMOP CDM with adaptations for
;;; MIMIC data.
(defun generate-schema-changes-for-mimic (model target)
  (generate-code model (mimic-schema-changes) target))
