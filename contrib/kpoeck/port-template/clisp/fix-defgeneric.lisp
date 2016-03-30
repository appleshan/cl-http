
(in-package :clos)

;; Checks a generic-function declspecs list.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ext:without-package-lock 
   ()
   (defun check-gf-declspecs (declspecs keyword errfunc)
     (unless (proper-list-p declspecs)
       (funcall errfunc (TEXT "The ~S argument should be a proper list, not ~S")
                keyword declspecs))
     (dolist (declspec declspecs)
       (unless t ;;;(and (consp declspec) (eq (first declspec) 'OPTIMIZE))
         (funcall errfunc (TEXT "In the ~S argument, only ~S declarations are permitted, not ~S")
                  keyword 'optimize declspec))))
   )
  )