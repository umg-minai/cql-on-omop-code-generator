(cl:in-package #:model-info-generator)

(defmethod add-conversions ((data-model data-model))
  (flet ((maybe-to-concept (table)
           (a:when-let ((column (canonical-concept-column table)))
             (push (make-instance 'to-concept-conversion :from-table table)
                   (conversions data-model))
             (push (make-instance 'to-code-conversion
                                  :from-table table
                                  :column     column)
                   (conversions data-model))))
         (maybe-to-quantity (table)
           (a:when-let ((value (find-if (lambda (name)
                                          (search "value_as_number" name))
                                        (columns table) :key #'name))
                        (unit  (find-if (lambda (name)
                                          (search "unit_concept" name))
                                        (columns table) :key #'name)))
             (push (make-instance 'to-quantity-conversion
                                  :from-table   table
                                  :value-column value
                                  :unit-column  unit)
                   (conversions data-model))))
         (maybe-to-interval (table suffix)
           (flet ((find-matching-column (suffix)
                    (find-if (lambda (name)
                               (and (a:ends-with-subseq suffix name)
                                    (let ((base (subseq name 0 (- (length name) (length suffix)))))
                                      (not (or (a:ends-with-subseq "start" base)
                                               (a:ends-with-subseq "end"   base))))))
                             (columns table) :key #'name)))
             (let ((neutral-suffix (format nil "_~A" suffix))
                   (start-suffix   (format nil "_start_~A" suffix))
                   (end-suffix     (format nil "_end_~A"   suffix)))
               (a:when-let ((start (or (find-matching-column start-suffix)
                                       (find-matching-column neutral-suffix)))
                            (end   (find-matching-column end-suffix)))
                 (push (make-instance 'to-interval-conversion
                                      :from-table   table
                                      :start-column start
                                      :end-column   end)
                       (conversions data-model)))))))
    (let* ((tables        (a:hash-table-values (tables data-model)))
           (output-tables (remove-if-not #'output? tables)))
      (mapc #'maybe-to-concept output-tables)
      (mapc #'maybe-to-quantity output-tables)
      (mapc (a:rcurry #'maybe-to-interval "datetime") output-tables)
      (mapc (a:rcurry #'maybe-to-interval "date") output-tables)))
  data-model)
