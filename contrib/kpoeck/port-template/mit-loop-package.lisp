(defpackage mit-loop
  (:use :common-lisp)
  (:shadow "LOOP" "LOOP-FINISH")
  (:export "LOOP" "LOOP-FINISH")
  )

(pushnew :mit-loop *features*)

#|
(:shadowing-import-from :mit-loop "LOOP" "LOOP-FINISH")
|#