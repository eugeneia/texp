;;;; Define readtable for brace and bracket syntax.

(in-package :texp)

(defun parenthized-list-reader (type close)
  "Return read macro function for parenthized list terminated by CLOSE.
It will be consed with TYPE."
  (lambda (stream char)
    (declare (ignore char))
    (cons type (read-delimited-list close stream t))))

(defreadtable syntax
  (:merge :standard)
  (:macro-char #\[ (parenthized-list-reader '[] #\]))
  (:macro-char #\{ (parenthized-list-reader '{} #\}))
  (:macro-char #\] (get-macro-character #\)))
  (:macro-char #\} (get-macro-character #\))))

