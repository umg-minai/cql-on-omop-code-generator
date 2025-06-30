(cl:in-package #:model-info-generator)

(defclass mimic-schema-changes ()
  ((%schema :initarg :schema
            :reader  schema)))

(defun mimic-schema-changes (&key (schema "cds_cdm"))
  (make-instance 'mimic-schema-changes :schema schema))

(defmethod emit ((data-model data-model)
                 (format     mimic-schema-changes)
                 (target     pathname))
  (if (uiop:directory-pathname-p target)
      (let* ((version  (version data-model))
             (name     (format nil "schema-changes-for-mimic-on-omop-~A"
                               version))
             (filename (make-pathname :name name :type "sql" :defaults target)))
        (emit data-model format filename))
      (progn
        (ensure-directories-exist target)
        (a:with-output-to-file (stream target :if-exists :supersede)
          (emit data-model format stream)))))

(defmethod emit ((data-model data-model)
                 (format     mimic-schema-changes)
                 (target     stream))
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time))
    (format target "-- Changes for OMOP CDM version ~A to support MIMIC data~@
                    -- Generated ~4D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D~%"
            (version data-model) year month day hour minute second))
  (mapc (a:rcurry #'emit format target)
        (sort (a:hash-table-values (tables data-model)) #'string< :key #'name)))

(defmethod emit ((element table)
                 (format  mimic-schema-changes)
                 (target  stream))
  (mapc (a:rcurry #'emit format target)
        (sort (copy-list (columns element)) #'string< :key #'name)))

(defmethod emit ((element column)
                 (format  mimic-schema-changes)
                 (target  stream))
  (let* ((schema     (schema format))
         (table      (parent element))
         (table-name (name table))
         (name       (name element))
         (data-type  (data-type element)))
    (flet ((alter-column (change)
             (format target "ALTER TABLE ~A.~A ALTER COLUMN \"~A\" ~A;~%"
                     schema table-name name change)))
      (when (and (string= data-type "integer")
                 (not (search "concept_id" name)))
        (alter-column "TYPE bigint"))
      (when (a:starts-with-subseq "varchar" data-type)
        (alter-column "TYPE text"))
      ;; MIMIC uses concepts from the CPT4 set which don't have a
      ;; concept_name.
      (when (and (string= table-name "concept")
                 (string= name "concept_name"))
        (alter-column "DROP NOT NULL"))
      (when (and (string= table-name "cdm_source")
                 (member name '("cdm_holder"
                                "cdm_release_date"
                                "cdm_source_abbreviation"
                                "cdm_source_name"
                                "cdm_version_concept_id"
                                "source_release_date")
                         :test #'string=)
                 (required? element))
        (alter-column "DROP NOT NULL")))))
