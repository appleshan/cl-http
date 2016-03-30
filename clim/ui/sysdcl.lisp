;;;   -*- Mode: LISP; Package: cl-user; BASE: 10; SYNTAX: ansi-common-lisp; -*-
;;;
;;; (C) Copyright 2000, Rainer Joswig, joswig@corporate-world.lisp.de
;;;     All Rights Reserved.
;;;

(in-package :cl-user)

;;;------------------------------------------------------------------- 
;;;
;;; SYSTEM DEFINITION
;;;

#+Genera
(sct:defsystem http-ui
    (:pretty-name "CL-HTTP CLIM User Interface"
     :default-pathname "http:clim;ui;"
     :journal-directory "http:lispm;clim;ui;patch;"
     :initial-status :experimental
     :patchable t
     :source-category :basic)
  (:module pointers
   ("sys:site;http-ui.system")
   (:type :lisp-example))
  (:serial
    "package" 
    "more-preferences"
    "presentation-types"
    "window-log"
    "http-ui"))

#+MCL
(define-system (http-ui)
               (:compile-load)
  "http:clim;ui;package"
  "http:clim;ui;more-preferences"
  "http:clim;ui;presentation-types"
  "http:clim;ui;window-log"
  "http:clim;ui;http-ui")

;;; ----------------------------------------------------------------
;;; End Of File
