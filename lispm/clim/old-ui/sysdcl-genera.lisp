;;;   -*- Mode: LISP; Package: cl-user; BASE: 10; SYNTAX: ansi-common-lisp; Default-character-style: (:FIX :ROMAN :NORMAL);-*-
;;;
;;; (C) Copyright 1994-1995, John C. Mallery.
;;;     All Rights Reserved.
;;;

D,#TD1PsT[Begin using 006 escapes](1 0 (NIL 0) (NIL :ITALIC NIL) "CPTFONTI")
;;;------------------------------------------------------------------- 
;;;
;;; CLIM INTERFACE TO CL-HTTP
;;;

0#+clim-2
(sct:defsystem cl-http-clim                             
    (:pretty-name "CL-HTTP Server Interface"
     :default-pathname "HTTP:CLIM;OLD-UI;"
     :journal-directory "HTTP:LISPM;CLIM;OLD-UI;PATCH;"
     :initial-status :experimental
     :patchable t
     :source-category :basic)
  (:module pointers
   ("SYS:SITE;CL-HTTP-CLIM.SYSTEM")
   (:type :lisp-example))
  (:module cl-http ("CL-HTTP") (:type :system))
  (:serial
    "HTTP:CLIM;OLD-UI;PACKAGE"  ; CLIM Interface
    "HTTP:CLIM;OLD-UI;INTERFACE"))
