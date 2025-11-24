(cl:defpackage #:model-info-generator.java.syntax
  (:use
   #:cl)

  (:shadow
   #:block
   #:if
   #:class
   #:method)

  (:export
   #:emitting
   #:out)

  (:export
   #:block
   #:if
   #+unused #:for
   #+unused #:func
   #:class
   #:method
   #+unused #:emit-field

   #:annotation
   #:annotations)

  (:export
   #:call-with-output-to-java-file
   #:with-output-to-java-file))

(cl:in-package #:model-info-generator.java.syntax)

(defvar *stream*)

(defmacro emitting ((stream) &body body)
  `(let ((*stream* ,stream)) ,@body))

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
    ((or symbol cl:function)
     (funcall string-or-continuation))))

(defun emitting-block (continuation newline? &key (indent 4))
  (out "{~@:_")
  (let ((prefix (coerce-to-indent indent)))
   (pprint-logical-block (*stream* (list continuation) :per-line-prefix prefix)
     (write-or-call continuation)))
  (out "~@:_}~:[~;~%~]" newline?))

(defmacro block ((&optional (newline? t)) &body body)
  `(emitting-block (lambda () ,@body) ,newline?))

(defun emitting-if (test then else)
  (out "if (")
  (write-or-call test)
  (out ") " )
  (block ((null else))
    (write-or-call then))
  (when else
    (out " else ")
    (block (nil)
      (write-or-call else))))

(defmacro if (test then &optional else)
  `(emitting-if ,test ,then ,else))

(defun emitting-class (continuation name modifiers superclasses)
  (out "~{~A ~}class ~A~{ ~{~(~A~) ~A~}~^,~} "
       modifiers name superclasses)
  (block ()
    (out "~@:_")
    (funcall continuation)))

(defmacro class ((name &optional ((&rest superclasses) '())
                                 ((&rest modifiers)    '("public")))
                 &body body)
  `(emitting-class
    (lambda () ,@body)
    ,name
    (list ,@modifiers)
    (list ,@(loop :for (relation name) :in superclasses
                  :collect `(list ,relation ,name)))))

(defun emitting-method (continuation name parameters result-type modifiers newline?)
  (out "~{~A ~}~A ~A(~{~{final ~*~A ~2:*~A~*~}~^, ~}) "
       modifiers result-type name parameters)
  (block (newline?) (write-or-call continuation))
  (out "~@:_"))

(defmacro method ((name parameters result-type
                   &key (newline? 't) (modifiers ''("public")))
                  &body body)
  `(emitting-method
    (lambda () ,@body) ,name ,parameters ,result-type ,modifiers ,newline?))

#+unused (defun emit-field (name type &optional annotation)
           (out "~A~@[ ~A~]~@[ `~A`~]" name type annotation))

(defun emitting-annotation (continuation name &rest arguments)
  (out "@~A" name)
  (when arguments
    (pprint-logical-block (*stream* arguments :prefix "(" :suffix ")")
      (loop :with first? = t
            :for argument = (pprint-pop)
            :do (cond ((keywordp argument)
                       (unless first? (out ", ~:_"))
                       (out "~A = "
                            (let ((name (symbol-name argument)))
                              (cl:if (every #'upper-case-p name)
                                     (string-downcase name)
                                     name)))
                       (let ((value (pprint-pop)))
                         (write-or-call value))
                       (setf first? nil))
                      ((not (null argument))
                       (unless first? (out ", ~:_"))
                       (out "~A" argument)
                       (setf first? nil)))
                (pprint-exit-if-list-exhausted))))
  (out "~@:_")
  (funcall continuation))

(defmacro annotation ((name &rest arguments) &body body)
  `(emitting-annotation (lambda () ,@body) ,name ,@arguments))

(defmacro annotations ((&rest annotations) &body body)
  (cl:if (null annotations)
         `(progn ,@body)
         `(annotation ,(first annotations)
            (annotations ,(rest annotations) ,@body))))

;;;

(defun call-with-output-to-java-file (continuation
                                      base-directory
                                      package class-name)
  (let* ((package  (mapcar (alexandria:curry #'remove #\.) package))
         (filename (merge-pathnames
                    (make-pathname :name      class-name
                                   :type      "java"
                                   :directory `(:relative ,@package))
                    base-directory)))
    (ensure-directories-exist filename)
    (alexandria:with-output-to-file (stream filename :if-exists :supersede)
      (pprint-logical-block (stream (list class-name))
        (emitting (stream)
          (out "package ~{~A~^.~};~@:_~@:_" package)
          (funcall continuation))))))

(defmacro with-output-to-java-file ((base-directory package class-name)
                                    &body body)
  `(call-with-output-to-java-file
    (lambda () ,@body) ,base-directory ,package ,class-name))
