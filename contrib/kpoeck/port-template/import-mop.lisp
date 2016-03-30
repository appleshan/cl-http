(in-package :cl-user)

(defun fix-symbols-clos ()
  (let ((clos-package #+lispworks (find-package :clos)
                      #+allegro (find-package :mop)
                      #+clozure-common-lisp (find-package :openmcl-mop)
                      #+sbcl (find-package :sb-mop)
                      #+clisp (find-package :mop)
                      #+ecl (find-package :clos)
		      #+cmu (find-package :mop)
                      #-(or lispworks allegro clozure-common-lisp sbcl clisp ecl cmu)
                      (error "Need to define mop package")))
    (dolist (symbol '("CLASS-DIRECT-SUPERCLASSES"
		      "CLASS-SLOTS"
		      "GENERIC-FUNCTION-METHODS"
		      "METHOD-SPECIALIZERS"
		      "SLOT-DEFINITION-NAME"))
      (import (find-symbol symbol clos-package) (find-package :www-utils))
      (export (find-symbol symbol (find-package :www-utils))(find-package :www-utils))))
  )

(eval-when (:load-toplevel :execute)
  (fix-symbols-clos)
  )