;;;
;;; **********************************************************************
;;; This code was written by Douglas T. Crosher and has been placed in
;;; the Public domain, and is provided 'as is'.
;;;
;;; **********************************************************************
;;;
;;; Example initialisation file for CL-HTTP to be loaded into a dumped core.
;;;

(in-package :cl-user)

(load "http:examples;configuration")
(asdf:oos 'asdf:load-op :cl-http-examples)

#+w4
(asdf:oos 'asdf:load-op :cl-http-w4-web-walker-demo)

(gc :full t)

(http:enable-http-service)
