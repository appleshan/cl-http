(in-package :cl-user)

(defun fix-it-values-w5 ()
  (shadowing-import (find-symbol "VALUES" (find-package :mit-values)) (find-package :WWW-PRESENT)))

(eval-when 
    (:load-toplevel :execute)  
  (fix-it-values-w5)
  )              