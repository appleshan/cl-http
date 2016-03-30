;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-user -*-

;;; Copyright John C. Mallery, 2006.
;;; All rights reserved.

;;;------------------------------------------------------------------- 
;;;
;;; BACKWARD-COMPATIBLE LOADING AND EXPORT OF THE EXAMPLE WEB SITE
;;;

;;; Ports should define an example system and compile it and then
;;; export the urls after loading the configuration file. 1/8/2006 -- JCMa

(in-package :http-user)

;; load the self documentation utilities
(load "http:examples;documentation" :verbose t)
;; load the authentication interface
(load "http:examples;access-control" :verbose t)
;; load the self documentation utilities for W3P
#+W3P
(load "http:w3p;documentation" :verbose t)
;; Load the remote log window
(load "http:examples;log-window" :verbose t)

;; Load the VRML examples
(when (member :vrml1.0 *features*)
  (load "http:examples;vrml;vrml" :verbose t))

;; Load Java directory example.
(load "http:examples;twistdown-tree;twistdown" :verbose t)

;; Load LispDoc facility
(load "http:examples;lispdoc" :verbose t)

;; load the examples code
(load "http:examples;examples" :verbose t)

;; Export the example site
(load "http:examples;example-exports.lisp" :verbose t)
