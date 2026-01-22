(cl:in-package #:model-info-generator)

(defmethod make-conversions ((table table))
  (let ((columns (columns table)))
    (labels ((find-column (&rest predicates)
               (loop :for predicate :in predicates
                     :for match     =   (find-if predicate columns :key #'name)
                     :when match
                       :do (return match)))
             (maybe-to-interval (suffix)
               (flet ((find-matching-column (suffix)
                        (find-column
                         (lambda (name)
                           (and (a:ends-with-subseq suffix name)
                                (let ((base (subseq name 0 (- (length name) (length suffix)))))
                                  (not (or (a:ends-with-subseq "start" base)
                                           (a:ends-with-subseq "end"   base)))))))))
                 (let ((neutral-suffix (format nil "_~A" suffix))
                       (start-suffix   (format nil "_start_~A" suffix))
                       (end-suffix     (format nil "_end_~A"   suffix))
                       start end)
                   (append
                    (when (and (setf start (or (find-matching-column start-suffix)
                                               (find-matching-column neutral-suffix)))
                               (setf end   (find-matching-column end-suffix)))
                      (multiple-value-bind (to-type function-name)
                          (a:eswitch (suffix :test #'string=)
                            ("datetime" (values "Interval<System.DateTime>" "ToDateTimeInterval"))
                            ("date"     (values "Interval<System.Date>"     "ToDateInterval")))
                        (list (make-instance 'to-interval-conversion
                                             :from-table    table
                                             :start-column  start
                                             :end-column    end
                                             :to-type       to-type
                                             :function-name function-name))))
                    (when start
                      (multiple-value-bind (to-type function-name)
                          (a:eswitch (suffix :test #'string=)
                            ("datetime" (values "System.DateTime" "ToDateTime"))
                            ("date"     (values "System.Date"     "ToDate")))
                        (list (make-instance 'to-time-conversion
                                             :from-table    table
                                             :column        start
                                             :to-type       to-type
                                             :function-name function-name)))))))))
      (append (a:when-let ((column (canonical-concept-column table)))
                (list (make-instance 'to-concept-conversion :from-table table)
                      (make-instance 'to-code-conversion :from-table table
                                                         :column     column)))
              (a:when-let ((value (find-column
                                   (lambda (name)
                                     (search "value_as_number" name))))
                           (unit  (find-column
                                   (a:curry #'search "unit_concept"))))
                (list (make-instance 'to-quantity-conversion
                                     :from-table   table
                                     :value-column value
                                     :unit-column  unit)))
              (maybe-to-interval "datetime")
              (maybe-to-interval "date")))))

(defmethod add-conversions ((data-model data-model))
  (let* ((tables        (a:hash-table-values (tables data-model)))
         (output-tables (remove-if-not #'output? tables)))
    (push (make-instance 'list-to-concept-conversion
                         :from-table (find-table "concept" data-model))
          (conversions data-model))
    (a:appendf (conversions data-model)
               (a:mappend #'make-conversions output-tables)))
  data-model)
