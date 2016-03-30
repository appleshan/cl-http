;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: common-lisp-user -*-
;;;
;;; (C) Copyright 2000, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;; Copyright 1994-97, Christopher R Vincent & John C. Mallery.
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;;  SYSTEM DEFINITION CL-HTTP PROXY SERVICE
;;;

(in-package :cl-user)

;; This requires that the server be loaded first as there is considerable shared code.
;; options are :load :compile :eval-load :compile-load-always, :compile-load

(define-system (cl-http-proxy)
	       ()
  "http:mcl;proxy;patches"                      ; Patches to MCL 4.1
  ;; client layer
  "http:mcl;client;sysdcl-substrate"            ; Client substrate
  ;; Proxy layer
  "http:proxy;uid"				;Persistent Unique IDs using the file system (move into CL-HTTP)
  "http:proxy;package"				;Symbols and exports
  "http:proxy;variables"			;Variables
  "http:proxy;class"				;Class Definitions
  "http:proxy;utils"
  "http:proxy;database"				;Database operations
  "http:proxy;cache"				;Cache operations
  "http:proxy;resource"				;Resource operations
  "http:proxy;representation"			;Representation operations
  "http:proxy;proxy-cache"			;Proxy caching operations
  "http:proxy;proxy"				;Proxy Server
  "http:proxy;documentation"
  "http:mcl;proxy;mcl")				; MCL specific code
