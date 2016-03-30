;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10 -*-

(in-package :HTTP)

;;;------------------------------------------------------------------- 
;;;
;;;  ACL EXPORTS
;;;

(export-url #u"/cl-http/sources/acl/jkf/-read-me-.text"
            :text-file
            :pathname (pathname "http:acl;jkf;-read-me-.text")
            :expiration `(:interval ,(* 15. 60.))
            :keywords '(:cl-http :documentation :allegro)
            :documentation
	    "README file for this alpha version of CL-HTTP for ACL UNIX.")

(export-url #u"/cl-http/sources/acl/jkf/examples/"
            :lisp-directory
            :pathname (pathname "http:acl;jkf;examples;*.lisp")
            :expiration `(:interval ,(* 15. 60.))
            :keywords '(:cl-http :documentation :allegro)
            :documentation
	    "Example Lisp files showing configuration of the server and export of URLs.")

