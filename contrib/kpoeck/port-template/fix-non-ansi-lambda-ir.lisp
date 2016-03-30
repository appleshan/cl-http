(in-package :cl-user)

#+(or :clisp :ecl)
(defun fix-it-ir ()
  (let ((defgeneric (find-symbol "DEFGENERIC" (find-package :mit-defgeneric))))
    (shadowing-import defgeneric (find-package :lambda-ir)))
  (let ((loop (find-symbol "LOOP" (find-package :mit-loop)))
        (loop-finish (find-symbol "LOOP-FINISH" (find-package :mit-loop)))
        (package (find-package :lambda-ir)))
    (shadowing-import loop package)
    (shadowing-import loop-finish package)
    )
  )

#+mit-values
(defun fix-it-ir ()
  (let (
        (values (find-symbol "VALUES" (find-package :mit-values))))
    (shadowing-import values (find-package :lambda-ir)))
  )

(eval-when 
    (:load-toplevel :execute)  
  (fix-it-ir)
  )              