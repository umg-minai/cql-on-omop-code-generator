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
   #+unused #:emit-field))

(cl:in-package #:model-info-generator.java.syntax)

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
  (pprint-logical-block (*stream* (list continuation) :per-line-prefix "    ")
    (write-or-call continuation))
  (out "~&}~:[~;~%~]" newline?))

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
    (block ()
      (write-or-call else))))

(defmacro if (test then &optional else)
  `(emitting-if ,test ,then ,else))

#+unused (defun emitting-for (continuation variables container)
  (out "for ")
  (loop :for variable :in variables
        :for first? = t :then nil
        :unless first? :do (out ", ")
        :do (write-or-call variable))
  (out " := range ")
  (write-or-call container)
  (block ()
    (write-or-call continuation)))

#+unused (defmacro for ((&rest variables) container &body body)
  `(emitting-for (lambda () ,@body) '(,@variables) ,container))

(defun emitting-class (continuation name superclasses)
  (out "public class ~A ~{~{~(~A~) ~A~}~^, ~} " name superclasses)
  (block () (funcall continuation)))

(defmacro class (name (&rest superclasses) &body body)
  `(emitting-class (lambda () ,@body) ,name '(,@superclasses)))

(defun emitting-method (continuation name parameters result-type modifiers newline?)
  (out "~{~A ~}~A ~A(~{~{final ~*~A ~2:*~A~*~}~^, ~}) "
       modifiers result-type name parameters)
  (block (newline?) (write-or-call continuation)))

(defmacro method ((name parameters result-type
                   &key (newline? 't) (modifiers ''("public")))
                  &body body)
  `(emitting-method
    (lambda () ,@body) ,name ,parameters ,result-type ,modifiers ,newline?))

#+unused (defun emit-field (name type &optional annotation)
           (out "~A~@[ ~A~]~@[ `~A`~]" name type annotation))
