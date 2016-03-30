;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: common-lisp-user -*-
;;;
;;; Copyright 1994-2001, 2006, John C. Mallery.
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;;  SYSTEM DEFINITION FOR CL-HTTP CLIENT SUBSTRATE
;;;

(in-package :cl-user)

;; This requires that the server be loaded first as there is considerable shared code.
;; options are :load :compile :eval-load :compile-load-always, :compile-load

(define-system (cl-http-client-substrate)
	       (:compile-load)			; required because define-system does not implement embedded systems
  "http:mcl;client;passwords"			; non-echoing editable text dialog item
  ;; client code.
  "http:client;variables"                       ; Client variables
  "http:mcl;client;mcl"                         ; Platform support for client
  "http:mcl;client;tcp-stream"                   ;TCP support for FTP client
  "http:ftp;package"                            ; FTP Package
  "http:ftp;ftp"                                ;FTP client
  "http:client;connection"                      ; Persistent connections
  "http:client;proxy"				; Upstream Proxies
  "http:client;authentication"			; Client authentication
  "http:client;client")                         ; Portable client code
