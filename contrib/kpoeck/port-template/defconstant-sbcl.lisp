(in-package :www-utils)

(defmacro defconstant-sbcl (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defshowable-procedure-type
    define-constant
    :definition-function #-no defparameter #+no defconstant-sbcl
  :doc-string "Defines a constant in the Communications Linker System."
  :other-btrees '(*www-procedures*))