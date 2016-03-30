;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: common-lisp-user -*-
;;;
;;; Copyright John C. Mallery,  1994-1995, 2001
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;; LOGICAL PATHNAME HOST FOR HTTP

(defun cl-http-directory (directory)
  (let ((dir (translate-logical-pathname directory)))
    (make-pathname :device (pathname-device dir)
		   :directory (butlast (pathname-directory dir) 2))))

(defparameter *http-directory* (cl-http-directory *load-pathname*))

(defun http-pathname (pathname)
  (let ((dir-spec (parse-namestring pathname)))
    (make-pathname :directory (append (pathname-directory *http-directory*) (cdr (pathname-directory dir-spec)))
		   :defaults *http-directory*)))

(defun rooted-pathname (pathname)
  (let ((dir-spec (parse-namestring pathname)))
    (make-pathname :directory `(:absolute ,@(cdr (pathname-directory dir-spec)))
		   :defaults *http-directory*)))

(setf (logical-pathname-translations "http")
  `(("http;*.*"          ,(http-pathname "*.*"))
    ("port;**;*.*"       ,(http-pathname "acl/jkf/**/*.*"))
    ("acl501;**;*.*"     ,(http-pathname "acl/jkf/**/*.*"))
    ("logs;**;*.*"       ,(http-pathname "log/**/*.*"))
    ("pw;**;*.*"         ,(http-pathname "log/pw/**/*.*"))
    ("root;**;*.*"       ,(rooted-pathname "**/*.*"))
    ("**;*.*"            ,(http-pathname "**/*.*"))))
