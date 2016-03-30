(in-package :cl-user)

(defun fix-it-values-w4 ()
  (shadowing-import (find-symbol "VALUES" (find-package :mit-values)) (find-package :w4)))

(eval-when 
    (:load-toplevel :execute)  
  (fix-it-values-w4)
  )              