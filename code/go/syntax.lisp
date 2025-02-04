(cl:defpackage #:model-info-generator.go.syntax
  (:use
   #:cl)

  (:shadow
   #:block
   #:if
   #:method)

  (:export
   #:emitting
   #:out)

  (:export
   #:block
   #:if
   #:for
   #:func
   #:method
   #:struct
   #:emit-field))

(cl:in-package #:model-info-generator.go.syntax)

(defvar *stream*)

(defmacro emitting ((stream) &body body)
  `(let ((*stream* ,stream)) ,@body))

(defun out (format-control &rest format-arguments)
  (apply #'format *stream* format-control format-arguments))

(defun write-or-call (string-or-continuation)
  (etypecase string-or-continuation
    (string
     (write-string string-or-continuation *stream*))
    ((or symbol cl:function)
     (funcall string-or-continuation))))

(defun emitting-block (continuation newline?)
  (out "{~%")
  (pprint-logical-block (*stream* (list continuation) :per-line-prefix "	")
    (write-or-call continuation))
  (out "~&}~:[~;~%~]" newline?))

(defmacro block ((&optional (newline? t)) &body body)
  `(emitting-block (lambda () ,@body) ,newline?))

(defun emitting-if (test then else)
  (out "if ")
  (write-or-call test)
  (out " " )
  (block ((null else))
    (write-or-call then))
  (when else
    (out " else ")
    (block ()
      (write-or-call else))))

(defmacro if (test then &optional else)
  `(emitting-if ,test ,then ,else))

(defun emitting-for (continuation variables container)
  (out "for ")
  (loop :for variable :in variables
        :for first? = t :then nil
        :unless first? :do (out ", ")
        :do (write-or-call variable))
  (out " := range ")
  (write-or-call container)
  (block ()
    (write-or-call continuation)))

(defmacro for ((&rest variables) container &body body)
  `(emitting-for (lambda () ,@body) '(,@variables) ,container))

(defun emitting-functionoid (continuation name parameters result-type newline?)
  (out "~@[~A~](~{~{~A ~A~}~^, ~}) ~A " name parameters result-type)
  (block (newline?) (write-or-call continuation)))

(defun emitting-function (continuation name parameters result-type newline?)
  (out "func ")
  (emitting-functionoid continuation name parameters result-type newline?))

(defmacro func ((name parameters result-type &optional (newline? 't)) &body body)
  `(emitting-function
    (lambda () ,@body) ,name ,parameters ,result-type ,newline?))

(defun emitting-method (continuation
                        name instance-variable type parameters result-type
                        newline?)
  (out "func (~A ~A) " instance-variable type)
  (emitting-functionoid continuation name parameters result-type newline?))

(defmacro method ((name instance-variable type parameters result-type
                   &optional (newline? 't))
                  &body body)
  `(emitting-method
    (lambda () ,@body)
    ,name ,instance-variable ,type ,parameters ,result-type ,newline?))

(defun emitting-struct (continuation name)
  (out "type ~A struct " name)
  (block () (funcall continuation)))

(defmacro struct (name &body body)
  `(emitting-struct (lambda () ,@body) ,name))

(defun emit-field (name type &optional annotation)
  (out "~A~@[ ~A~]~@[ `~A`~]" name type annotation))
