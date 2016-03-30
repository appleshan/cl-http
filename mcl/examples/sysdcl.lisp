;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: common-lisp-user -*-
;;;
;;; Copyright 2005-2006, John C. Mallery.
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;;  EXAMPLES SYSTEM DEFINITION FOR MCL CL-HTTP
;;;

(in-package :cl-user)

(define-system (cl-http-base-examples)
	       (:compile-load)
  "http:examples;documentation"        ; Web apropos
  "http:examples;find-url"             ; Find url
  "http:examples;examples"             ; Frame, and various examples
  "http:examples;access-control"       ; web-based access control
  "http:examples;lispdoc"              ; reference manual for cl-http
  #+W3P "http:w3p;documentation"       ; W3P self documentation
  "http:examples;log-window"           ; Server push log window for Netscape & Mozilla
  #+VRML1.0 "http:examples;vrml;vrml"          ; VRML examples
  "http:examples;twistdown-tree;twistdown"     ; Java twist-down menu tree
  "http:examples;listener"             ; Web Lisp Listener
  "http:examples;slides")              ; Simple Web slide show generator

