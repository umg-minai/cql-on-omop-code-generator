;;;; Based on the MIT style licensed "A very simple CSV Reader"
;;;; by Gilbert Baumann

(cl:in-package #:model-info-generator)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +cr+ #.#\Return)

  (defconstant +lf+ #.#\Linefeed)

  (a:define-constant +crlf+ #.(format nil "~A~A" #\Return #\Linefeed)
    :test 'equal))

(defun char-space-p (separator c)
  "Is character C some kind of white space?
NB: this only handles a tiny subset of whitespace characters,
even if restricted to ASCII. However, it's rather portable,
and is what the creativyst document specifies.
Be careful to not skip a separator, as it could be e.g. a tab!"
  (declare (type (or null character) c)
           (type character separator))
  (and c
       (or (eql c #\Space) (eql c #\Tab))
       (not (eql c separator))))

(defconstant +buffer-size+ 4096)

(defun read-csv (stream row-callback column-callback
                 separator quote escape)
  (check-type separator character)
  (check-type quote     character)
  (check-type escape    character)
  (let* ((minimum-room    +buffer-size+)
         (row-callback    (a:ensure-function row-callback))
         (column-callback (a:ensure-function column-callback))
         (buffer          (make-array (* 2 minimum-room)
                                      :element-type 'character))
         (columns-counter 0)
         (start           0)            ; start of our current field
         (fptr            0)            ; fill pointer of buffer
         (p               -1)           ; reading pointer
         (c               #\Space))     ; lookahead
    (labels ((underflow ()
               (replace buffer buffer :start2 start :end2 p)
               (let ((length (length buffer)))
                 (when (<= (- length p) minimum-room)
                   (setf buffer (adjust-array buffer (+ length minimum-room))
                         minimum-room (* 2 minimum-room))))
               (decf p start)
               (setf start 0
                     fptr  (read-sequence buffer stream :start p)))
             (report-result (start end &optional (value buffer))
               (funcall column-callback value start end)
               (incf columns-counter))
             (new-line-p ()
               (member c `(#\Newline ,+cr+ ,+lf+ ,+crlf+)))
             (report-row ()
               (unless (zerop columns-counter)
                 (funcall row-callback))
               (setf columns-counter 0))
             (consume ()
               (when c
                 (incf p)
                 (when (= p fptr) (underflow)))
               (setf c (if (= p fptr) nil (char buffer p))))
             (consume-whitespace ()
               (loop :while (char-space-p separator c)
                     :do (consume)
                         (setf start p)))
             (read-field ()
               (consume-whitespace)
               (cond ((new-line-p) nil)
                     ((eql quote c)
                      (consume)
                      (read-quote-field))
                     (t
                      (setf start p)
                      (loop :with count = 0
                            :until (or (null c)
                                       (eql c separator)
                                       (new-line-p))
                            :unless (char-space-p separator c)
                              :do (setf count  (- p start -1))
                            :do (consume)
                            :finally (let ((end (+ count start)))
                                       (report-result start end))))))
             (read-quote-field (&optional (end p))
               (setf start p)
               (loop :for escaped = nil
                     :until (null c)
                     :when (and (not escaped) (eql c escape))
                       :do (consume)
                           (setf escaped t)
                     :do (if (eql quote escape)
                             (if escaped
                                 (cond ((eql c quote)
                                        (consume)
                                        (setf end p))
                                       (t
                                        (loop-finish)))
                                 (cond ((eql c quote)
                                        (consume)
                                        (setf end p)
                                        (loop-finish))
                                       (t
                                        (consume)
                                        (setf end p))))
                             (cond (escaped
                                    (consume)
                                    (setf end p))
                                   ((eql c quote)
                                    (consume)
                                    (setf end p)
                                    (loop-finish))
                                   (t
                                    (consume)
                                    (setf end p)))))
               (report-result start end)
               (consume-whitespace)
               (assert (or (null c) (new-line-p) (eql separator c))))
             (read-row ()
               (loop :do (read-field)
                         (when (not (eql c separator))
                           (loop-finish))
                         (consume))
               (assert (or (null c) (new-line-p)))
               (consume)))
      (consume)
      (loop :until (null c)
            :do (read-row)
                (report-row)))))

(defun csv-to-list (string separator quote escape)
  (let ((result (list))
        (row (list)))
    (with-input-from-string (stream string)
      (read-csv stream
                (lambda ()
                  (push (nreverse row) result)
                  (setf row (list)))
                (lambda (value start end)
                  (push (subseq value start end) row))
                separator
                quote
                escape))
    (nreverse result)))
