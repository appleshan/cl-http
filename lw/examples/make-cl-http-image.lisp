;;;   -*- Mode: LISP; Package: cl-user; BASE: 10; SYNTAX: ansi-common-lisp -*-
;;;
;;; (C) Copyright, 2003, 2005-2006, John C. Mallery
;;;     All Rights Reserved.
;;;------------------------------------------------------------------- 
;;;
;;; BUILD LISP IMAGE FOR Cl-HTTP SYSTEM
;;; 
;;; This file builds and saves a CL-HTTP IMAGE for LispWorks
;;;
;;; Please report any successes, difficulties, or modifications of this file to
;;; bug-lw-cl-http@nospam.cl-http.org for inclusion in future releases.
;;; 
;;; Running CL-HTTP out of a saved image reduces start up time and can improve locality of reference.
;;; Once you have made an image, you can start up CL-HTTP using a shell command like:
;;;
;;; /LispWorks/LispWorks-4-4-6/LispWorks-4-4-6-cl-http-70-214-svn79.app/Contents/MacOS/LispWorks-4-4-6-cl-http-70-214 -cl-http-init-script ~/cl-http-org/cl-http-init.lisp &
;;;
;;; The argument to -cl-http-init-script is a file that may load the application but certainly loads
;;; the configuration file followed by the exports and then enables http service on the desired ports.

;; The following commands launches LW, loads cl-http, and saves the image.
#|

# To make a COCOA image,  use the following shell commands:
cd /Common-Lisp/OSX/LispWorks/LispWorks-4-4-6/LispWorks.app/Contents/MacOS/

# Use this file as the init file
./lispworks-4-4-6-darwin -siteinit - -init /cl-http/cl-http-org/make-cl-http-image.lisp

# To make an X windows image, use the following shell commands:
cd /Common-Lisp/OSX/LispWorks-4-4-6/

# Use this file as the init file
./lispworks-4-4-6-darwin-motif -siteinit - -init /cl-http/cl-http-org/make-cl-http-image.lisp

|#

(in-package :cl-user)

;;;------------------------------------------------------------------- 
;;;
;;; CONFIGURE LISPWORKS
;;; 

#-:cocoa
(require "clim")			;load Common Lisp Interface Manager
#-:cocoa
(require "clim-demo")			;load CLIM demos, including lisp listener

;(require "odbc")			;load Common SQL

(load-all-patches)			;update lispworks components

;; *init-file-name* normally bound to "~/.lispworks"
;(setq *init-file-name* "~/.cl-http")

;;;------------------------------------------------------------------- 
;;;
;;;
;;; 

;; set the options as desired
(defparameter *cl-http-options* '((:compile . t)
				  (:debug-info . t)
				  (:proxy . t)
				  (:w4 . t)
				  (:clim-monitor . nil)
				  (:examples . nil)
				  (:mail-archive . t)
				  (:lambda-ir . t)
				  (:configure-and-start . nil)))

;; Edit the values for the first 5 local variables
(let* ((cl-http-version "cl-http-70-214-svn") ;image name
       (svn-version 122) ;subversion commit number or nil
       (start-file (pathname "/cl-http/cl-http-svn/lw/start.lisp")) ;start file location
       (init-file (merge-pathnames "cl-http-init.lisp" (current-pathname))) ;default init to use when starting this image
       (image-dir (pathname "/Common-Lisp/OSX/LispWorks/LispWorks-4-4-5/")) ;target directory for image
       (app-file (merge-pathnames (format nil "~A-~A-~A~:[~;~:*~A~]" (lisp-implementation-type) 
                                          (substitute #\- #\. (lisp-implementation-version)) cl-http-version svn-version) image-dir))
       (comment (format nil "LispWorks ~A, CL-HTTP ~A~:[~; SVN ~:*~D~]" (lisp-implementation-version)
                        (subseq cl-http-version (position-if #'digit-char-p cl-http-version)) svn-version)))

  ;; keep the development environment in the world
  ;; load cl-http from the production version directory

  (load start-file)

  ;; Default CL-HTTP initialization script
  (setq *cl-http-init-script* init-file)

  ;; enable multi-procssing on LW initialization
  (push (list "cl-http init" nil (intern "LW-COLD-ENABLE-HTTP-SERVICE-MP" :www-utils)) mp:*initial-processes*)

  #+:cocoa
  (compile-file-if-needed (sys:example-file "configuration/macos-application-bundle") :load t)

  (save-image 
   #+:cocoa
   (write-macos-application-bundle (make-pathname :type "app" :defaults app-file))
   #-:cocoa 
   app-file
   :remarks comment)

  (quit))
