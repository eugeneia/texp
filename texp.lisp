;;;; DSL for outputting TeX expressions using S-expressions.

(defpackage texp
  (:documentation
   "DSL for outputting TeX expressions using S-expressions.")
  (:use :cl
	:named-readtables)
  (:export :*escape-table*
	   :escape
	   :$
	   :br
	   :[]
	   :{}
	   :tex
	   :deftex
	   :syntax))

(in-package :texp)

(defparameter *escape-table*
  '((#\\ . "\\backslash")
    (#\{ . "?")
    (#\} . "?")
    (#\$ . "\\$")
    (#\& . "\\&")
    (#\# . "\\#")
    (#\^ . "?")
    (#\_ . "\\_")
    (#\% . "\\%")
    (#\~ . "?")
    (#\" . "?")
    (#\| . "?")
    (#\< . "?")
    (#\> . "?"))
  "TeX special characters.")

(defun escape-p (character)
  "Predicate to test if CHARACTER needs to be escaped."
  (not (null (assoc character *escape-table*))))

(defun escape-char (character)
  "Return escaped string for CHARACTER."
  (cdr (assoc character *escape-table*)))

(defun escape (string)
  "Escape STRING as defined by *ESCAPE-TABLE*."
  (with-output-to-string (out)
    (loop for start = 0 then (1+ pos)
       for pos = (position-if #'escape-p string :start start)
       do (write-string string out :start start :end pos)
       when pos do (write-string (escape-char (char string pos)) out)
       while pos)))

(defun symbol-identifier (symbol)
  "Return identifier string for SYMBOL."
  (string-downcase (symbol-name symbol)))

(defun compile-interpolation (expression)
  "Interpolate EXPRESSIONS into the TEX macro."
  `(let ((result ,expression))
     (when (stringp result) (write-string result))))

(defun compile-break ()
  "Print double newline."
  `(format t "~%~%"))

(defun compile-parenthized (open close expressions)
  "Compile EXPRESSIONS parenthized by OPEN and CLOSE."
  `(progn (write-char ,open)
	  ,@(compile-expressions expressions)
	  (write-char ,close)
	  (values)))

(defun compile-with-braces (expressions)
  "Compile EXPRESSIONS parenthized by braces."
  (compile-parenthized #\{ #\} expressions))

(defun compile-with-brackets (expressions)
  "Compile EXPRESSIONS parenthized by brackets."
  (compile-parenthized #\[ #\] expressions))

(defun compile-macro-call (name &rest arguments)
  "Compile call to macro with NAME and ARGUMENTS."
  `(progn (format t "\\~a" ,(symbol-identifier name))
	  ,@(compile-expressions arguments)
	  (values)))

(defun compile-expression (expression)
  "Compile TeX EXPRESSION."
  (etypecase expression
    (string `(write-string ,expression))
    (symbol `(write-string ,(symbol-identifier expression)))
    (list (case (car expression)
	    ($         (apply #'compile-interpolation (cdr expression)))
	    (br        (apply #'compile-break (cdr expression)))
	    ({}        (compile-with-braces (cdr expression)))
	    ([]        (compile-with-brackets (cdr expression)))
	    (otherwise (apply #'compile-macro-call expression))))))

(defun compile-expressions (expressions)
  "Compile TeX EXPRESSIONS."
  (loop for expression in expressions
     collect (compile-expression expression)))

(defmacro tex (&rest expressions)
  "Print compiled TeX EXPRESSIONS."
  `(progn
     ,@(compile-expressions expressions)
     (values)))

(defun make-parameter-pointer-map (parameters)
  "Make a map of PARAMETERS to TeX macro parameter pointers of the form
'#<N>'."
  (let ((n (length parameters)))
    (when (> n 9)
      (error "TeX macros support up to nine parameters only."))
    (loop for i from 1 to n
          for parameter in parameters
       collect `(,parameter ,(format nil "#~a" i)))))

(defmacro deftex (name parameters parameter-format &body body)
  "Define a TeX macro with NAME, PARAMETERS, PARAMETER-FORMAT and BODY."
  `(let ,(make-parameter-pointer-map parameters)
     (tex (def (,name ,parameter-format) ({} ,@body))
	  (br))))
