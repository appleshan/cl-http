(defpackage mit-values
  (:use :common-lisp)
  (:shadow "VALUES")
  (:export "VALUES")
  )

(pushnew :mit-values *features*)