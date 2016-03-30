;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: cl-user; -*-

(in-package "CL-USER")

;;; simplest of tests, to load the parse and parse a document

#+(and mcl (not CL-HTTP))
(load "entwicklung@paz:sourceServer:lisp:xml:define-system.lisp")
#+(and (not mcl) (not cl-http))
(load "d:\\Source\\Lisp\\Xml\\define-system.lisp")

;; minimum system
#+mcl
(register-system-definition :xparser "entwicklung@paz:sourceServer:lisp:xml:sysdcl.lisp")
#-mcl
(register-system-definition :xparser "d:\\Source\\Lisp\\Xml\\sysdcl.lisp")

;; utils
; (execute-system-operations :xutil '(:compile))

;; xml parser
;(execute-system-operations :xqdm '(:load))
;(execute-system-operations :xqdm '(:compile))
;(execute-system-operations :xparser '(:load))
;(execute-system-operations :xparser '(:compile))
(execute-system-operations :xparser '(:compile :load))


;; extended to include xml paths
(register-system-definition :xpath "entwicklung@paz:sourceServer:lisp:xml:sysdcl.lisp")
;(execute-system-operations :xpath '(:load))
;(execute-system-operations :xpath '(:compile))
(execute-system-operations :xpath '(:compile :load))


;; extended to include xml query
(register-system-definition :xquery "entwicklung@paz:sourceServer:lisp:xml:sysdcl.lisp")
;(execute-system-operations :xquery '(:load))
;(execute-system-operations :xquery '(:compile))
(execute-system-operations :xquery '(:compile :load))


;; _really_ simple tests

;; w/o namespaces
(xmlp:document-parser "<test attr='1234'>asdf</test>")

;; with namespaces
(xmlp:document-parser "<ns:test attr='1234' xmlns:ns='ns1'>asdf</ns:test>")

;; for a simple document
(time (xmlp:document-parser "file://xml/tests/xml/channel.xml"))

;;(inspect *)
:EOF
