;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-
;;;
;;; AUTHENTICATION DATA -- Mon, 07 Jan 2013 17:34:12 GMT
;;;

(in-package :http)

;;;-------------------------------------------------------------------
;;;
;;; Server Realm
;;;

(HTTP:INTERN-REALM "SERVER"
                   :IF-DOES-NOT-EXIST
                   :CREATE
                   :SCHEME
                   :DIGEST)
(HTTP:INTERN-GROUP "SERVER" "WEBMASTERS" :IF-DOES-NOT-EXIST :CREATE)
(HTTP:INTERN-USER "SERVER"
                  "Webmaster"
                  :IF-DOES-NOT-EXIST
                  :CREATE
                  :EMAIL-ADDRESS
                  "@localhost"
                  :PERSONAL-NAME
                  "Webmaster"
                  :GROUPS
                  '("WEBMASTERS"))
(HTTP:INTERN-ACCESS-CONTROL
  "SERVER"
  "Webmasters"
  :CAPABILITY-ALIST
  '((:DEFAULT :WEBMASTERS))
  :IF-DOES-NOT-EXIST
  :CREATE)

;;; End Complete Authentication Data Save
;;; Begin Incremental Authentication Data Saves

