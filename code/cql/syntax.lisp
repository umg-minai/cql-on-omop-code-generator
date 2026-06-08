(cl:defpackage #:model-info-generator.cql.syntax
  (:use
   #:cl)

  (:shadow
   #:block
   #:let
   #:if
   #:cond
   #:or
   #:and
   #:function)

  (:export
   #:emitting
   #:out)

  (:export
   #:comment
   #:section
   #:literal
   #:instance
   #:block
   #:let
   #:if
   #:cond
   #:emit-or
   #:or
   #:emit-and
   #:and
   #:emit-plus
   #:plus
   #:function)

  (:export
   #:call-with-output-to-cql-file
   #:with-output-to-cql-file))

(cl:in-package #:model-info-generator.cql.syntax)

(defvar *stream*)

(defmacro emitting ((stream) &body body)
  `(cl:let ((*stream* ,stream)) ,@body))

(defun out (format-control &rest format-arguments)
  (apply #'format *stream* format-control format-arguments))

(defun coerce-to-indent (thing)
  (typecase thing
    (string  thing)
    (integer (make-string thing :initial-element #\Space))))

(defun write-or-call (string-or-continuation)
  (etypecase string-or-continuation
    (string
     (write-string string-or-continuation *stream*))
    ((cl:or symbol cl:function)
     (funcall string-or-continuation))))

(defun %comment (prefix per-line-prefix suffix format-control &rest format-arguments)
  (cl:let ((stream *stream*))
    (when prefix
      (format stream "~A~@:_" prefix))
    (pprint-logical-block (stream nil :per-line-prefix per-line-prefix)
      (cl:let ((string (apply #'format nil format-control format-arguments)))
        (loop :for previous = 0 :then (1+ index)
              :for index    = (position-if
                               (lambda (character)
                                 (member character '(#\Space #\Newline #\Tab)))
                               string :start previous)
              :do (write-string string stream :start previous
                                              :end   index)
                  (cl:cond ((null index))
                           ((eql (aref string index) #\Newline)
                            (pprint-newline :mandatory stream))
                           (t
                            (write-char #\Space stream)
                            (pprint-newline :fill stream)))
              :when (null index)
                :do (loop-finish))))
    (when suffix
      (format stream "~@:_~A" suffix))
    (pprint-newline :mandatory stream)))

(defun comment (format-control &rest format-arguments)
  (apply #'%comment nil "// " nil format-control format-arguments))

(defun section (format-control &rest format-arguments)
  (apply #'comment format-control format-arguments)
  (out "~@:_"))

(defun emitting-block (continuation newline?)
  (pprint-logical-block (*stream* (list continuation))
    (write-or-call continuation))
  (when newline?
    (out "~@:_")))

(defmacro block ((&optional (newline? t)) &body body)
  `(emitting-block (lambda () ,@body) ,newline?))

(defun emitting-indented-block (continuation &key (initial-newline? t)
                                                  (final-newline?   t)
                                                  (indent           2))
  (cl:if initial-newline?
         (out "~@:_")
         (out "~:_"))
  (cl:let ((prefix (coerce-to-indent indent)))
    (pprint-logical-block (*stream* (list continuation) :per-line-prefix prefix)
      (write-or-call continuation)))
  (when final-newline?
    (out "~@:_")))

(defmacro indented-block ((&optional (newline? t) (initial-newline? t))
                          &body body)
  `(emitting-indented-block (lambda () ,@body)
                            :final-newline?   ,newline?
                            :initial-newline? ,initial-newline?))

(defun literal (value)
  (etypecase value
    (string           (lambda () (out "'~A'" value))) ; TODO(moringenj): escape '
    ((signed-byte 32) (lambda () (out "~D" value))) ; TODO(moringenj): check actual range
    (signed-byte      (lambda () (out "~DL" value)))))

(defun instance (type &rest initargs)
  (assert (zerop (mod (length initargs) 2)))
  (write-or-call type)
  (out "{")
  (indented-block (nil nil)
    (loop :for (name value) :on initargs :by #'cddr
          :for first?       =   t :then nil
          :do (unless first?
                (out ", ~:_"))
              (write-or-call (cl:if (keywordp name) (string-downcase name) name))
              (out ": ")
              (pprint-logical-block (*stream* nil)
                (write-or-call value))))
  (out "~:_}"))

(defun emitting-let (bindings body)
  (out "(1) _")
  (indented-block (nil)
    (out "let ")
    (pprint-logical-block (*stream* bindings)
      (loop :for (binding next) :on bindings
            :if (typep binding '(cl:or string cl:function))
              :do (write-or-call binding)
            :else
              :do (destructuring-bind (name value) binding
                    (write-or-call name)
                    (out ": ")
                    (pprint-logical-block (*stream* (list value))
                      (write-or-call value))
                    (when next
                      (out ",~@:_")))))
    (indented-block (nil)
      (out "return all ")
      (pprint-logical-block (*stream* (list body))
        (write-or-call body)))))

(defmacro let ((&rest bindings) body)
  `(emitting-let
    (list ,@(loop :for binding :in bindings
                  :if (typep binding '(cons string))
                    :collect (destructuring-bind (name value) binding
                               `(list ,name ,value))
                  :else
                    :collect binding))
    ,body))

(defun emitting-if (test then else)
  (pprint-logical-block (*stream* nil)
    (out "if ")
    (write-or-call test)
    (out " then" )
    (indented-block ((not (null else)))
      (write-or-call then))
    (when else
      (out "else")
      (indented-block (nil)
        (write-or-call else)))))

(defmacro if (test then &optional else)
  `(emitting-if ,test ,then ,else))

(defun emitting-cond (clauses)
  (out "case")
  (indented-block (nil)
    (loop :for ((test body) next) :on clauses
          :do (cl:if (member test '(t otherwise))
                     (out "else")
                     (progn
                       (out "when ")
                       (write-or-call test)
                       (out " then" )))
              (indented-block ((not (null next)))
                (write-or-call body))))
  (out "~@:_end"))

(defmacro cond (&rest clauses)
  (cl:let ((count (length clauses)))
    (cl:cond ((member count '(0 1))
              (error "~@<Need at least two clauses for now.~@:>"))
             ((cl:and (= count 2)
                      (member (first (second clauses)) '(t otherwise)))
              (destructuring-bind ((test1 body1) (test2 body2)) clauses
                (declare (ignore test2))
                `(if ,test1 ,body1 ,body2)))
             (t
              `(emitting-cond
                (list ,@(loop :for (test body) :in clauses
                              :collect `(list ,test ,body))))))))

(defun emit-operator (symbol empty-value &rest expressions)
  (case (length expressions)
    (0 (out empty-value))
    (1 (write-or-call (first expressions)))
    (t (cl:let ((stream *stream*))
         (pprint-logical-block (stream expressions :prefix "(" :suffix ")")
           (loop :for (expression next) :on expressions
                 :do (out "(")
                     (write-or-call expression)
                     (out ")")
                 :when next
                   :do (out " ~:_~A " symbol)))))))

(defun emit-or (&rest expressions)
  (apply #'emit-operator "or" "false" expressions))

(defmacro or (&rest expressions)
  `(emit-or ,@expressions))

(defun emit-and (&rest expressions)
  (apply #'emit-operator "and" "true" expressions))

(defmacro and (&rest expressions)
  `(emit-and ,@expressions))

(defun emit-plus (first-expression &rest expressions)
  (apply #'emit-operator "+" nil first-expression expressions))

(defmacro plus (&rest expressions)
  `(emit-plus ,@expressions))

(defun emitting-function (continuation name parameters newline?)
  (out "define function ~A(~{~{~A ~A~}~^, ~}):" name parameters)
  (indented-block (newline?) (write-or-call continuation)))

(defmacro function ((name parameters &key (newline? 't)) &body body)
  `(emitting-function (lambda () ,@body) ,name ,parameters ,newline?))

#+unused (defun emit-field (name type &optional annotation)
           (out "~A~@[ ~A~]~@[ `~A`~]" name type annotation))

;;;

(defun call-with-output-to-cql-file
    (continuation base-directory library-name
     &key (filename (make-pathname :name library-name :type "cql"))
          library-version
          generation-source)
  (cl:let ((filename (merge-pathnames filename base-directory)))
    (ensure-directories-exist filename)
    (alexandria:with-output-to-file (stream filename :if-exists :supersede)
      (pprint-logical-block (stream nil)
        (emitting (stream)
          (comment "This file has been generated~:[~; from ~:*~A~] - do not edit"
                   generation-source)
          (out "library \"~A\"~@[ version '~A'~]~@:_~@:_"
               library-name library-version)
          (funcall continuation))))))

(defmacro with-output-to-cql-file
    ((base-directory library-name
      &rest args &key filename library-version generation-source)
     &body body)
  (declare (ignore filename library-version generation-source))
  `(call-with-output-to-cql-file
    (lambda () ,@body) ,base-directory ,library-name ,@args))
