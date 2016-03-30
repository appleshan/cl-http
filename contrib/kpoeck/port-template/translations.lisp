(in-package :cl-user)

(defun cl-http-directory (directory)
  (let ((dir #+cormanlisp directory 
             #-cormanlisp (translate-logical-pathname directory)))
    (make-pathname :device (pathname-device dir)
                   :host (pathname-host dir)
                   :directory (butlast (pathname-directory dir) 3))))

(defparameter *http-this-file-true* *load-truename*)
(defparameter *http-directory* (cl-http-directory *http-this-file-true*))

#+ecl
(defun http-pathname (pathname)
  (let ((dir-spec (parse-namestring pathname)))
    (make-pathname
     :device (pathname-device *http-directory*)
     :directory (append (pathname-directory *http-directory*) (cdr (pathname-directory dir-spec)))
     :name "*"
     :type "*")))
  
#-ecl
(defun http-pathname (pathname)
  (let ((dir-spec (parse-namestring pathname)))
    (make-pathname :directory (append (pathname-directory *http-directory*) (cdr (pathname-directory dir-spec)))
                   :defaults *http-directory*)))

(defun rooted-pathname (pathname)
  (let ((dir-spec (parse-namestring pathname)))
    (make-pathname :directory `(:absolute ,@(cdr (pathname-directory dir-spec)))
                   :defaults *http-directory*)))

(defun nicify-pathname (foo)
    #+cormanlisp (namestring foo)
    #-cormanlisp foo)

(defun set-logical ()
  (setf (logical-pathname-translations "http")
        `(("http;*.*"          ,(nicify-pathname (http-pathname "*.*")))
          ("port;**;*.*"       ,(nicify-pathname (http-pathname "contrib/kpoeck/port-template/**/*.*")))
          ("logs;**;*.*"       ,(nicify-pathname (http-pathname "log/**/*.*")))
          ("pw;**;*.*"         ,(nicify-pathname (http-pathname "log/pw/**/*.*")))
          ("root;**;*.*"       ,(nicify-pathname (rooted-pathname "**/*.*")))
          ("**;*.*"            ,(nicify-pathname (http-pathname "**/*.*")))))
  )