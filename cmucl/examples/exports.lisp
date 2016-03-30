;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10 -*-

(in-package :HTTP)

;;; Make minimum CL-HTTP export for CMU Common Lisp for UNIX.  This
;;; file must be loaded after the default configuration file
;;; 

;;;------------------------------------------------------------------- 
;;;
;;;  CMU CL EXPORTS
;;;

(export-url #u"/cl-http/sources/cmucl/read-me.text"
            :text-file
            :pathname (pathname "http:cmucl;read-me.text")
            :expiration `(:interval ,(* 15. 60.))
            :public t
	    :keywords '(:cl-http :documentation :cmu-cl)
            :documentation "README file for this alpha version of CL-HTTP for CMU CL under UNIX.")


#||

; patch file is no longer needed, RJ 2006-08-05

(export-url #u"/cl-http/sources/cmucl/cl-http-patches.text"
            :text-file
            :pathname (pathname "http:cmucl;cl-http-patches.text")
            :expiration `(:interval ,(* 15. 60.))
            :public t
	    :keywords '(:cl-http :documentation :cmu-cl)
            :documentation "Patch file file for this version of CL-HTTP for CMU CL under UNIX.")

||#
