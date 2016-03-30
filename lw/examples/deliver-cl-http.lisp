;;;   -*- Mode: LISP; Package: cl-user; BASE: 10; SYNTAX: ansi-common-lisp -*-
;;;
;;; (C) Copyright, 2003, 2006, John C. Mallery
;;;     All Rights Reserved.
;;;------------------------------------------------------------------- 
;;;
;;; DELIVER AN APPLICATION WITH CL-HTTP
;;; 
;;; This file delivers a LispWorks application for CL-HTTP.
;;;
;;; Please report any successes, difficulties, or modifications of this file to
;;; bug-lw-cl-http@nospam.cl-http.org for inclusion in future releases.
;;;

;;; The following command launches LW, loads cl-http, and deliers the application.

#|
/LispWorks/LispWorks-4-4-6/LispWorks-4-4-6.app/Contents/MacOS/LispWorks-4-4-6-darwin -siteinit - -init ~/deliver-cl-http.lisp

|#

(in-package :cl-user)

;;;------------------------------------------------------------------- 
;;;
;;; Configure LispWorks
;;; 

#-:cocoa
(require "clim")			;load Common Lisp Interface Manager
#-:cocoa
(require "clim-demo")			;load CLIM demos, including lisp listener

;(require "odbc")			;load Common SQL

(load-all-patches)			;update lispworks components

;;;------------------------------------------------------------------- 
;;;
;;;
;;; 

;; Set the options as desired
(defparameter *cl-http-options* '((:compile . t)
				  (:debug-info . nil)
				  (:proxy . t)
				  (:w4 . nil)
				  (:clim-monitor . nil)
				  (:examples . nil)
				  (:mail-archive . nil)
				  (:lambda-ir . nil)
				  (:configure-and-start . nil)))

;; Edit the values for the first 5 local variables
(let* ((cl-http-version "cl-http-70-214-svn") ;image name
       (svn-version 122) ;subversion commit number or nil
       (start-file (pathname "/cl-http/cl-http-svn/lw/start.lisp")) ;start file location
       (init-file "http:lw;examples;cl-http-init.lisp") ;default init to use when starting this image
       (image-dir (pathname "/Common-Lisp/OSX/LispWorks/LispWorks-4-4-5/")) ;target directory for image
       (app-file (merge-pathnames (format nil "~A-~A-~A~:[~;~:*~A~]" (lisp-implementation-type) 
                                          (substitute #\- #\. (lisp-implementation-version)) cl-http-version svn-version) image-dir)))

  ;; Load cl-http from the production version directory
  (load start-file)

  ;; Default initialization
  (setq *cl-http-init-script* init-file)

  ;; Enable multi-procssing on LW initialization
  (push (list "cl-http init" nil (intern "LW-COLD-ENABLE-HTTP-SERVICE-MP" :www-utils)) mp:*initial-processes*)

  #+:cocoa
  (compile-file-if-needed (sys:example-file "configuration/macos-application-bundle") :load t)

  (deliver (intern "LW-COLD-ENABLE-HTTP-SERVICE-MP" :www-utils)
           #+:cocoa    ;;"/Applications/LispWorks/Othello.app"
           (write-macos-application-bundle (make-pathname :type "app" :defaults app-file))
           #-:cocoa
           app-file
           0
           :multiprocessing t
           :interface :capi
           :keep-conditions :all
           :keep-debug-mode :keep-packages
           :keep-load-function :full
           :keep-documentation nil
           :keep-lexer t
           :keep-eval t
           :keep-fasl-dump t)

  (quit))
