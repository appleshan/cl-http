;;;(in-package :cl-user)
;;;
;;;(eval-when (:compile-toplevel :execute :load-toplevel)
;;;  (shadowing-import 'mop:method-specializers (find-package :www-utils))
;;;  (shadowing-import 'mop:generic-function-methods (find-package :www-utils))
;;;  )

;;;(in-package :http)
(in-package :www-utils)

(defun method-specializers (object)
  #+allegro
  (mop:method-specializers object)
  #+clozure-common-lisp
  (openmcl-mop:method-specializers object)
  #+sbcl
  (sb-mop:method-specializers object)
  #+clisp
  (mop:method-specializers object)
  #+ecl
  (clos:method-specializers object)
  #+lispworks
  (hcl:method-specializers object)
  )

(defun generic-function-methods (object)
  #+allegro
  (mop:generic-function-methods object)
  #+clozure-common-lisp
  (openmcl-mop:generic-function-methods object)
  #+sbcl
  (sb-mop:generic-function-methods object)
  #+clisp
  (mop:generic-function-methods object)
  #+ecl
  (clos:generic-function-methods object)
  #+lispworks
  (hcl:generic-function-methods object)
  )
