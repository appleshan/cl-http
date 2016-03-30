;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10; Lowercase: Yes -*-

;;; CL-HTTP Documentation written with Symbolics Concordia
;;; Copyright Rainer Joswig, 2000, joswig@corporate-world.lisp.de
;;; Copyright diverse documentation string writers of CL-HTTP modules


(defsystem CL-HTTP-Doc
    (:pretty-name "CL-HTTP Documentation"
     :default-pathname "http:lispm;docs;"
     :patchable t
     :advertised-in nil
     :default-module-type :sage
     ;; .SAB files are considered to be product files by the distribution dumper
     :distribute-sources t
     :distribute-binaries t
     :source-category :basic
     )

  (:module cl-http-documentation ("cl-http-1" "server" "logging" "proxy"))
  )

#||
; Not yet. Need to define the right toplevel node.

(sage::register-book "Symbolics Concordia"
		     :document-type 'sage::3symanual
		     :mnemonic ""
		     :doc# "999839"
		     :Releaseversion "Genera 8.0"
		     :authorgroup "Documentation Group"
		     :symcopy t
		     :mitcopy nil
		     )
||#
