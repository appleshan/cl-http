(in-package :http)

;;; The following is loaded before the setf for local-context is defined 
;;; simply reload to avoid the warning

(defun (setf local-context) (new-context)
  (set-local-context new-context))