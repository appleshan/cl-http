;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: common-lisp-user -*-
;;;
;;; Copyright 1994-2001, 2003, John C. Mallery.
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;;  ENVIRONMENT DEFINITION FOR MCL CL-HTTP
;;;

;; If you want to just start up the server using the standard demo configuration,
;; you should load http:mcl;mcl-start-server.lisp instead.

;; If you load this file and evaluate the form
;; (http-user:enable-http-service), you can access the URL
;; http://your.host.domain.name/ and peruse the server documentation.

(in-package :cl-user) 

(declaim (special *cl-http-server-version*))

;; lispm major.minor MAC major.minor.rev
(setq *cl-http-server-version* '(70 216 4 1 0)) 

;;;------------------------------------------------------------------- 
;;;
;;; LOAD SYSTEM DEFINITION FACILITY 
;;;

;; Load the logical pathname translations
(load (merge-pathnames "translations"  ccl:*loading-file-source-file*) :verbose t) 

;; allow the values declaration
(define-declaration values nil)

;; allow the arglist declaration
(define-declaration arglist nil)

;; Set compiler switches etc
(load "http:mcl;mcl-configuration" :verbose t)

;; Load the appropriate defsystem interface
(eval-when (:execute :compile-toplevel :load-toplevel)
   ;;  #-(and CLIM Defsystem)
   (unless (find-package :micro-defsystem)
      (load "http:mcl;server;micro-defsystem" :verbose t))
   ;;   #+(and CLIM Defsystem)
   ;;  (unless (find-package :micro-clim-defsystem)
   ;; (load "http:mcl;server;micro-clim-defsystem" :verbose t))
   )

;;;------------------------------------------------------------------- 
;;;
;;;  CL-HTTP SYSTEM CONFIGURATION
;;;

;; This advises the system how to load various CL-HTTP components.
;; The keyword should be identical with the system name, except that it is a keyword.
;; The pathname is typically the system definition, and will load the system.
(note-system-definitions
  (:cl-http "http:mcl;server;sysdcl")		;  Server definition
  (:cl-http-client-substrate "http:mcl;client;sysdcl-substrate")	; Client Substrate
  (:cl-http-proxy "http:mcl;proxy;sysdcl")	; Proxy server (includes client substrate)
  (:cl-http-client "http:mcl;client;sysdcl")	; S-Exp Browser
  #+CLIM(:http-ui "http:clim;ui;sysdcl.lisp")	; CLIM Server Interface
  (:w4-web-walker "http:mcl;w4;sysdcl")		; W4 Constraint-Guided Web Walker
  (:lambda-ir "http:mcl;lambda-ir;sysdcl")	; Hybrid Retrieval System
  (:html-parser "http:mcl;html-parser;sysdcl")	; HTML Parser 
  (:cl-http-examples "http:mcl;examples;sysdcl")	; Server Examples
  (:w4-web-walker-demo "http:mcl;w4;sysdcl-examples")
  (:cgi-support "http:mcl;contrib;tnorderhaug;cgi;cgi-sysdcl")	; CGI Support
  (:db-auth "http:contrib;pmeurer;dbauth;sysdcl"))	; Database Authentication

(setq *standard-systems* '(:cl-http :cl-http-proxy :cl-http-client :w4-web-walker #+CLIM :http-ui))

;; Load the standard CL-HTTP components
(load-standard-systems)


