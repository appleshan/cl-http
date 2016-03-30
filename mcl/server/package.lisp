;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: cl-user;-*-

;;; (C) Copyright 1994-2001, 2005-2006 John C. Mallery.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; PACKAGE DEFINITIONS
;;;
;;;
;;;------------------------------------------------------------------- 

(in-package :cl-user)

; Advise the lisp environment that we have MAC-CL-HTTP loaded.
(pushnew :mac-cl-http *features*)
(pushnew :mcl-cl-http *features*)

#+:ccl-3 ;; Advise MAC-CL-HTTP  that the Lisp Environment is multithreaded
(pushnew :multi-threaded *features*)

(pushnew :http-chunking *features*) 


(mapc #'(lambda (x)
          (shadowing-import x :www-utils)
          (export x :www-utils))
      (list (intern "DEFRESOURCE" :resources)
            (intern "CLEAR-RESOURCE" :resources)
            (intern "ALLOCATE-RESOURCE" :resources)
            (intern "DEALLOCATE-RESOURCE" :resources)
            (intern "USING-RESOURCE" :resources)
            (intern "MAP-RESOURCE" :resources)
            (intern "GENERIC-FUNCTION-METHODS" :ccl)
            (intern "METHOD-SPECIALIZERS" :ccl)
            (intern "PROCESS-ACTIVE-P" :ccl)
            (intern "PROCESS-DISABLE" :ccl)
            (intern "PROCESS-ENABLE" :ccl)
            (intern "PROCESS-KILL" :ccl)
            (intern "PROCESS-PRIORITY" :ccl)
            (intern "PROCESS-PRESET" :ccl)
            (intern "PROCESS-RESET" :ccl)
            (intern "PROCESS-RUN-FUNCTION" :ccl)
            (intern "PROCESS-WAIT" :ccl)
            (intern "PROCESS-WHOSTATE" :ccl)
            (intern "*CURRENT-PROCESS*" :ccl)
            (intern "CREATE-TIMER-CALL" :ccl)
            (intern "CLEAR-TIMER" :ccl)
            (intern "RESET-TIMER-ABSOLUTE" :ccl)
            (intern "RESET-TIMER-RELATIVE" :ccl)
            (intern "WITH-TIMEOUT" :ccl)
            (intern "CHUNK-TRANSFER-ENCODING-MODE" :ccl)
            (intern "NOTE-FIRST-CHUNK" :ccl)
            (intern "NOTE-LAST-CHUNK" :ccl)
            (intern "CHUNK-TRANSFER-DECODING-MODE" :ccl)
            (intern "CHUNK-TRANSFER-DECODING-MODE-END" :ccl) 
            (intern "CHUNK-TRANSFER-CONTENT-LENGTH" :ccl)
            (intern "CHUNK-TRANSFER-CONTENT-LENGTH-HEADER" :ccl)
            (intern "END-OF-CHUNK-TRANSFER-DECODING" :ccl)
            (intern "CLASS-DIRECT-SUPERCLASSES" :ccl)
            (intern "CLASS-SLOTS" :ccl)
            (intern "SLOT-DEFINITION-NAME" :ccl)))

;; a series of network conditions that we would like to be able to within
;; portable code.
(mapc #'(lambda (sym)
          (let ((sym (intern sym :ccl)))
            (import sym :www-utils)
            (export sym :www-utils)))
      '("BAD-CONNECTION-STATE"
        "CONNECTION-CLOSED"
        "CONNECTION-ERROR"
        "CONNECTION-LOST"
        "CONNECTION-REFUSED"
        "DOMAIN-RESOLVER-ERROR"
        "HOST-NOT-RESPONDING"
        "HOST-STOPPED-RESPONDING"
        "LOCAL-NETWORK-ERROR"
        "NETWORK-ERROR"
        "NETWORK-PARSE-ERROR"
        "NETWORK-RESOURCES-EXHAUSTED"
        "PROTOCOL-TIMEOUT"
        "REMOTE-NETWORK-ERROR"
        "UNKNOWN-ADDRESS"
        "UNKNOWN-HOST-NAME"))

