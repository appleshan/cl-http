(in-package :cl-user)

;;; Logical Pathnames in corman are broken
;;; Rudimentary substitute

(defun simple-translate-logical-pathname (pathname)
  (if (pathnamep pathname)
      pathname
    (let ((translation nil))
      (dolist (tupel
               (list (list "http:port;" "c:/cygwin/home/Karsten/cl-http-svn/contrib/kpoeck/port-template/")
                     (list "http:" "c:/cygwin/home/Karsten/cl-http-svn/")))
        (let ((pattern (first tupel))
              (path (second tupel)))
          (when (search pattern pathname)
            (setq translation (merge-pathnames 
                               (concatenate 'string 
                                 path
                                 (substitute #\/ #\; (subseq pathname (length pattern))))
                               (make-pathname :type "lisp")))
            (return))))
        translation)))



#|
(setf (logical-pathname-translations "http")
  `(("http;*.*"          ,(nicify-pathname (http-pathname "*.*")))
    ("port;**;*.*"       ,(nicify-pathname (http-pathname "porting/**/*.*")))
    ("logs;**;*.*"       ,(nicify-pathname (http-pathname "log/**/*.*")))
    ("pw;**;*.*"         ,(nicify-pathname (http-pathname "log/pw/**/*.*")))
    ("root;**;*.*"       ,(nicify-pathname (rooted-pathname "**/*.*")))
    ("**;*.*"            ,(nicify-pathname (http-pathname "**/*.*")))))


(simple-translate-logical-pathname 
  "http:port;declaration")

|#