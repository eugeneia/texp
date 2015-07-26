;;;; DSL for outputting TeX expressions using S-expressions.

(defpackage texp
  (:documentation
   "DSL for outputting _TeX_ expressions using S-expressions.")
  (:use :cl
	:named-readtables)
  (:export :*escape-table*
	   :escape
	   :$
	   :br
           :\\
	   :[]
	   :{}
	   :tex
	   :deftex
	   :syntax))

(in-package :texp)

(defparameter *escape-table*
  '((#\& . "\\&")
    (#\% . "\\%")
    (#\$ . "\\$")
    (#\# . "\\#")
    (#\_ . "\\_")
    (#\{ . "\\{")
    (#\} . "\\}")
    (#\~ . "{\\textasciitilde}")
    (#\^ . "{\\textasciicircum}")
    (#\\ . "{\\textbackslash}"))
  "*Description:*

   _Alist_ mapping _TeX_ special characters to quoted equivalent.")

(defun escape-p (character)
  "Predicate to test if CHARACTER needs to be escaped."
  (not (null (assoc character *escape-table*))))

(defun escape-char (character)
  "Return escaped string for CHARACTER."
  (cdr (assoc character *escape-table*)))

(defun escape (string)
  "*Arguments and Values:*

   _string_—a _string_.

   *Description:*

   {escape} encodes _string_ as defined by {*escape-table*}. E.g. it
   quotes _TeX_ special characters."
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

(defun compile-newline ()
  "Print explicit newline."
  `(format t "\\\\~%"))

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
            (\\        (apply #'compile-newline (cdr expression)))
	    ({}        (compile-with-braces (cdr expression)))
	    ([]        (compile-with-brackets (cdr expression)))
	    (otherwise (apply #'compile-macro-call expression))))))

(defun compile-expressions (expressions)
  "Compile _TeX_ EXPRESSIONS."
  (loop for expression in expressions
     collect (compile-expression expression)))

(defmacro tex (&rest expressions)
  "_expressions_::= ↓_expression_*

   _expression_::= ↓_break-clause_ | ↓_interpolation-clause_ |
                   ↓_brackets-clause_ | ↓_braces-clause_ | ↓_tex-clause |
                   _literal_

   _break-clause_::= {(br)}

   _interpolation-clause_::= {($} _form_ {)}

   _brackets-clause_::= {([]} _expressions_ {)}

   _braces-clause_::= {({\}} _expressions_ {)}

   _tex-clause::= {(} _tex-macro_ _expressions_ {)}

   *Arguments and Values:*

   _literal_—a _string_, _symbol_ or _number_.

   _form_—a _form_.

   _tex-macro_—a _symbol_ or a _string_.

   *Description:*

   {tex} compiles _TeX expressions_ to {*standard-output*}.

   A _Literal_ is printed as follows:

   + a _string_ is printed as it is
   + a _symbol_ is printed in lower case
   + a _number_ is printed _readably_

   A _tex-clause_ prints a _TeX_ macro call of _tex-macro_ followed by
   _expressions_.

   A _break-clause_ prints two newlines (e.g. a paragraph separator).

   An _interpolation-clause_ evaluates _form_ and prints its result if
   the result is a _string_.

   _Bracket-clause_ and _braces-clause_ print _expressions_ surrounded by
   brackets ({[}, {]}) and braces ({{}, {\}}) respectively. If the
   _readtable_ {texp:syntax} is used then _bracket-clause_ and
   _braces-clause_ can be written as {[} _expression_* {]} and {{}
   _expression_* {\}}."
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

(defmacro deftex (name parameters &body forms)
  "_parameters_::= {(}_var_*{)}

   *Arguments and Values:*

   _name_—a _symbol_ or _string_.

   _forms_—_forms_.

   _var_—a _symbol_.

   *Description:*

   {deftex} prints the definition of a _TeX_ macro with _name_ that
   expands to _forms_ to {*standard-output*}. If _name_ is a _symbol_ it
   will be printed in lower case. _Forms_ are evaluated as if by {tex}
   with each _var_ in _parameters_ bound to a numeric _TeX_ parameter
   identifier.

   *Examples:*

   #code#
   (deftex hello (name) \"Hello \" ($ name))
   ▷ \\def \\hello #1{Hello #1}
   #"
  (multiple-value-bind (pointer-map parameter-string)
      (compile-parameters parameters)
    `(let ,pointer-map
       (tex (def (,name ,parameter-string) ({} ,@forms))
	    (br)))))
