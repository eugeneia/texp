;;;; System definition for TEXP.

(defsystem texp
  :description
  "DSL for outputting TeX expressions using S-expressions."
  :author "Max Rottenkolber <max@mr.gy>" 
  :license "GNU Affero General Public License"
  :components ((:file "texp")
	       (:file "syntax" :depends-on ("texp")))
  :depends-on ("named-readtables"))
