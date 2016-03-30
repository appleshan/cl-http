;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10 -*-

(in-package :http)

;;; Make minimum CL-HTTP export for the Scieneer Common Lisp for UNIX.  This
;;; file must be loaded after the default configuration file
;;; 

;;;------------------------------------------------------------------- 
;;;
;;;  The Scieneer CL EXPORTS
;;;

(export-url #u"/cl-http/sources/scl/read-me.text"
            :text-file
            :pathname (pathname "http:scl;read-me.text")
            :expiration `(:interval ,(* 15. 60.))
	    :keywords '(:cl-http :documentation :scl)
            :documentation "README file for CL-HTTP for the Scieneer CL under UNIX.")

;;; Patch and bug files.
(export-url #u"/cl-http/sources/scl/cl-http-patches.text"
            :text-file
            :pathname (pathname "http:scl;cl-http-patches.text")
            :expiration `(:interval ,(* 15. 60.))
            :keywords '(:cl-http :documentation :scl))

