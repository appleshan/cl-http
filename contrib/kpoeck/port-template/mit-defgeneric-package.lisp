(defpackage mit-defgeneric
  (:use :common-lisp)
  (:shadow "DEFGENERIC")
  (:export "DEFGENERIC")
  )


#|
(:shadowing-import-from :mit-defgeneric "DEFGENERIC")
|#