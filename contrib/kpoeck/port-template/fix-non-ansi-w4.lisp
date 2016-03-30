(in-package :cl-user)

(defun fix-it-walker ()
  (let ((loop (find-symbol "LOOP" (find-package :mit-loop)))
        (loop-finish (find-symbol "LOOP-FINISH" (find-package :mit-loop)))
        (package (find-package :w4)))
    (shadowing-import loop package)
    (shadowing-import loop-finish package)
    )
  )

(eval-when 
    (:load-toplevel :execute)  
  (fix-it-walker)
  )              