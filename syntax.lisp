;;;; Define readtable for brace and bracket syntax.

(in-package :texp)

(defun parenthesized-list-reader (type close)
  "Return read macro function for parenthesized list terminated by CLOSE.
It will be consed with TYPE."
  (lambda (stream char)
    (declare (ignore char))
    (cons type (read-delimited-list close stream t))))

(defreadtable syntax
  (:merge :standard)
  (:macro-char #\[ (parenthesized-list-reader '[] #\]))
  (:macro-char #\{ (parenthesized-list-reader '{} #\}))
  (:macro-char #\] (get-macro-character #\)))
  (:macro-char #\} (get-macro-character #\))))

