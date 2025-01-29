(cl:in-package #:model-info-generator)

;;; Utilities

(defun read-csv (filename)
  (vellum-csv::csv-to-list (a:read-file-into-string filename) #\, #\" #\"))

;;; Loaders

(defun load-data-model (version)
  (let ((data-model (make-instance 'data-model :name    "OMOP" ; "OMOP CDM"
                                               :version version)))
    (load-tables data-model version)
    (load-fields data-model version)
    data-model))

(defun load-tables (data-model version)
  (let* ((directory "~/code/omop/commondatamodel/inst/csv/")
         (filename  (format nil "OMOP_CDM~A_Table_Level.csv" version))
         (pathname  (merge-pathnames filename directory)))
    (destructuring-bind (header &rest tables) (read-csv pathname)
      (print header)
      (mapcar (lambda (table)
                (destructuring-bind
                    (name schema required? concept-prefix
                     measure-person-completeness validation
                     description &rest rest)
                    table
                  (declare (ignore schema required? concept-prefix
                                   measure-person-completeness
                                   validation rest))
                  (let ((description
                          (when (and description
                                     (not (string= description "NA")))
                            description)))
                    (setf (find-table name data-model)
                          (make-instance 'table
                                         :name        name
                                         :description description)))))
              tables))))

(defun load-fields (data-model version)
  (let* ((directory "~/code/omop/commondatamodel/inst/csv/")
         (filename  (format nil "OMOP_CDM~A_Field_Level.csv" version))
         (pathname  (merge-pathnames filename directory)))
    (destructuring-bind (header &rest fields) (read-csv pathname)
      (print header)
      (flet ((parse-field (field)
               (destructuring-bind (table-name field-name required? data-type
                                    user-guidance etl-conventions
                                    primary-key?
                                    foreign-key? foreign-table-name foreign-field-name
                                    &rest rest)
                   field
                 (declare (ignore rest))
                 (let* ((table       (find-table table-name data-model))
                        (foreign-key (when (string= foreign-key? "Yes")
                                       (cons (string-downcase foreign-table-name)
                                             (string-downcase foreign-field-name))))
                        (description (format nil "~A~2%~A"
                                             user-guidance etl-conventions))
                        (column      (make-instance 'column
                                                    :name         field-name
                                                    :description  description
                                                    :table        table
                                                    :required?    (string= required? "yes")
                                                    :data-type    data-type
                                                    :primary-key? (string= primary-key? "Yes")
                                                    :foreign-key  foreign-key)))
                   (setf (find-column field-name table) column))))
             (link-column (column)
               (a:when-let ((foreign-key (foreign-key column)))
                 (check-type foreign-key cons)
                 (destructuring-bind (table-name . column-name) foreign-key
                   (let* ((foreign-table  (find-table table-name data-model))
                          (foreign-column (find-column column-name foreign-table))
                          (foreign-key    (make-instance 'foreign-key
                                                         :table  foreign-table
                                                         :column foreign-column)))
                     (reinitialize-instance column :foreign-key foreign-key))))))
        (mapc #'link-column (mapcar #'parse-field fields))))))
