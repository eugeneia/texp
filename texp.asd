;;;; System definition for TEXP.

(defpackage texp-asd
  (:documentation
   "System definition for TEXP.")
  (:use :cl :asdf))

(in-package :texp-asd)

(defsystem texp
  :description
  "DSL for outputting TeX expressions using S-expressions."
  :components ((:file  "texp")
	       (:file "syntax" :depends-on ("texp")))
  :depends-on ("named-readtables"))
