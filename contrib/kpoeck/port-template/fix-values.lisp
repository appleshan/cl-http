(in-package :cl-user)

(defun fix-it-values ()
  (let ((values (find-symbol "VALUES" (find-package :mit-values))))
    (dolist (package (list 
                      (find-package :http)
                      (find-package :url)
                      (find-package :netscape1.1)
                      (find-package :www-utils)
                      (find-package :md5)
                      (find-package :sha)
                      (find-package :html2)
                      (find-package :html3.2)
                      (find-package :html4.0)
                      )
                     )
      (shadowing-import values package))
    )
  )

(eval-when 
    (:load-toplevel :execute)  
  (fix-it-values)
  )              