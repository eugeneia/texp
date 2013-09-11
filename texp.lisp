;;;; DSL for outputting TeX expressions using S-expressions.

(defpackage texp
  (:documentation
   "DSL for outputting _TeX_ expressions using s-expressions.")
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
    (#\{ . "{\\tt\\char`{\\/}")
    (#\} . "{\\tt\\char`}\\/}")
    (#\$ . "\\$")
    (#\& . "\\&")
    (#\# . "\\#")
    (#\^ . "{\\tt\\char`^\\/}")
    (#\_ . "\\_")
    (#\% . "\\%")
    (#\~ . "{\\tt\\char`~\\/}")
    (#\" . "{\\tt\\char`\"\\/}")
    (#\| . "{\\tt\\char`|\\/}")
    (#\< . "{\\tt\\char`<\\/}")
    (#\> . "{\\tt\\char`>\\/}"))
  "_TeX_ special characters to quoted equivalent mapping.")

(defun escape-p (character)
  "Predicate to test if CHARACTER needs to be escaped."
  (not (null (assoc character *escape-table*))))

(defun escape-char (character)
  "Return escaped string for CHARACTER."
  (cdr (assoc character *escape-table*)))

(defun escape (string)
  "Escape STRING as defined by *ESCAPE-TABLE*. E.g. quote _TeX_ special
characters."
  (with-output-to-string (out)
    (loop for start = 0 then (1+ pos)
       for pos = (position-if #'escape-p string :start start)
       do (write-string string out :start start :end pos)
       when pos do (write-string (escape-char (char string pos)) out)
       while pos)))

(defparameter $ nil
  "Special form symbol for interpolation.")

(defparameter BR nil
  "Special form symbol printing paragraph seperators.")

(defparameter [] nil
  "Special form symbol for wrapping expressions with brackets.")

(defparameter {} nil
  "Special form symbol for wrapping expressions with curly braces.")

(defparameter syntax nil
  "Readtable for braces and brackets syntax.")

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

(defun compile-parenthesized (open close expressions)
  "Compile EXPRESSIONS parenthesized by OPEN and CLOSE."
  `(progn (write-char ,open)
	  ,@(compile-expressions expressions)
	  (write-char ,close)
	  (values)))

(defun compile-with-braces (expressions)
  "Compile EXPRESSIONS parenthesized by braces."
  (compile-parenthesized #\{ #\} expressions))

(defun compile-with-brackets (expressions)
  "Compile EXPRESSIONS parenthesized by brackets."
  (compile-parenthesized #\[ #\] expressions))

(defun compile-macro-call (name &rest arguments)
  "Compile call to macro with NAME and ARGUMENTS."
  `(progn (format t "\\~a " ,(etypecase name
                              (symbol (symbol-identifier name))
                              (string name)))
	  ,@(compile-expressions arguments)
	  (values)))

(defun compile-expression (expression)
  "Compile _TeX_ EXPRESSION."
  (etypecase expression
    (string `(write-string ,expression))
    (symbol `(write-string ,(symbol-identifier expression)))
    (number `(write-string ,(format nil "~a" expression)))
    (list (case (car expression)
	    ($         (apply #'compile-interpolation (cdr expression)))
	    (br        (apply #'compile-break (cdr expression)))
	    ({}        (compile-with-braces (cdr expression)))
	    ([]        (compile-with-brackets (cdr expression)))
	    (otherwise (apply #'compile-macro-call expression))))))

(defun compile-expressions (expressions)
  "Compile _TeX_ EXPRESSIONS."
  (loop for expression in expressions
     collect (compile-expression expression)))

(defmacro tex (&rest expressions)
  "Print compiled _TeX_ EXPRESSIONS. Strings and numbers are printed as
is and symbols are printed in lower case. The {(BR)} special form is
converted to two newlines (e.g. paragraph seperator).  The
{($ FORM)} special form will evaluate FORM and print its result if it is
a string (e.g. interpolation). Other compound forms (e.g.
{(SYMBOL-OR-STRING &rest EXPRESSIONS)}) are converted to
{\\\\SYMBOL-OR-STRING EXPRESSIONS} while EXPRESSIONS will be procecessed
recursively. The special forms {([] &rest EXPRESSIONS)} and
{(\\{\\} &rest EXPRESSIONS)} print EXPRESSIONS recursively too but wrap
them with bracktes or curly braces. If the readtable TEXP:SYNTAX is used
then the latter forms can be abbreviated using {[&rest EXPRESSIONS]}
and {\\{&rest EXPRESSIONS\\}}."
  `(progn ,@(compile-expressions expressions)
     (values)))

(defun make-parameter-string (n)
  "Make parameter string for N parameters."
  (subseq "#1#2#3#4#5#6#7#8#9" 0 (* n 2)))

(defun compile-parameters (parameters)
  "Returns a map of PARAMETERS to _TeX_ macro parameter pointers of the form
'#<N>' and a parameter string for _TeX_'s \\\\def."
  (let ((n (length parameters)))
    (when (> n 9)
      (error "TeX macros support up to nine parameters only."))
    (values
     (loop for i from 1 to n
	   for parameter in parameters
	collect `(,parameter ,(format nil "#~a" i)))
     (make-parameter-string n))))

(defmacro deftex (name parameters &body body)
  "Define a _TeX_ macro with NAME, PARAMETERS and BODY. NAME must be a
symbol or a string. If it is a symbol it will be printed in lower case.
PARAMETERS must be a list of symbols which will be bound to _TeX_ parameter
identifiers inside BODY. The expressions in BODY will be printed as if by
TEX and the interpolation form can be used to reference PARAMETERS."
  (multiple-value-bind (pointer-map parameter-string)
      (compile-parameters parameters)
    `(let ,pointer-map
       (tex (def (,name ,parameter-string) ({} ,@body))
	    (br)))))
