(in-package :cl-user)

(defvar *cl-http-directory*
  (ext:resolve-pathname "./../" *load-truename*))

(unless (ignore-errors (logical-pathname-translations "http"))
  (setf (logical-pathname-translations "http")
    `(("**;*.*.*" ,(namestring
		    (merge-pathnames "**/*.*.~*~" *cl-http-directory*)))))
  (load "http:scl;translations.lisp"))

(asdf:defsystem cl-http
  :description "The Common Lisp HTTP Sever"
  :pathname "http:"
  :serial t
  :depends-on (cl-http-server
	       cl-http-client
	       cl-http-proxy
	       cl-http-w4-web-walker
	       cl-http-examples
	       cl-http-w4-web-walker-demo
	       cl-http-html-parser
	       ))

