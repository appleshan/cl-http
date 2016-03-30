;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: future-common-lisp-user; Base: 10 -*-

;;; (C) Copyright 2000, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;; (C) Copyright 1997, Christopher R. Vincent
;;;     All Rights Reserved.
;;;

#+Genera
(unless (fs:get-logical-pathname-host "HTTP" t)
  (fs:make-logical-pathname-host "HTTP"))

(sct:defsystem http-proxy
    (:pretty-name "HTTP Proxy Server"
     :default-pathname "http:proxy;"
     :journal-directory "http:lispm;proxy;patch;"
     :initial-status :experimental
     :patchable t
     :source-category :basic)
  (:module pointers
   ("sys:site;http-proxy.system")
   (:type :lisp-example))
  (:module cl-http
   (cl-http)
   (:type :system))
  (:module http-client-substrate
   (http-client-substrate)
   (:type :system))
  (:serial
    cl-http					;Server
    http-client-substrate			;Client Substrate
    "http:proxy;uid"				;Persistent Unique IDs using the file system (move into CL-HTTP)
    "http:proxy;package"			;Symbols and exports
    "http:proxy;variables"			;Variables
    "http:proxy;class"				;Class Definitions
    "http:proxy;utils"
    "http:proxy;database"			;Database operations
    "http:proxy;cache"				;Cache operations
    "http:proxy;resource"			;Resource operations
    "http:proxy;representation"			;Representation operations
    "http:proxy;proxy-cache"			;Proxy caching operations
    "http:proxy;proxy"				;Proxy Server
    "http:proxy;documentation"))
