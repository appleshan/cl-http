(in-package :www-utils)

(defshowable-procedure-type
  define-generic
  :definition-function mit-defgeneric:defgeneric
  :doc-string "Defines a CLOS generic function for the WWW Lisp System."
  :other-btrees '(*www-procedures*))