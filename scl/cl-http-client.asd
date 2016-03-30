
(in-package :cl-user)

(asdf:defsystem cl-http-client
  :pathname "http:"
  :depends-on (cl-http-server)
  :serial t
  :components
  ((:module client-variables
    :pathname "http:client;"
    :components
    ;; Client variables
    ((:file "variables")))
   ;;
   (:module scl-client
    :pathname "http:scl;client;"
    :components
    ;; Platform support for client
    ((:file "unix")
     ;; FTP socket support.
     #+nil
     (:file "tcp-stream")))
   ;;
   ;; FTP client.
   (:module ftp-client
    :pathname "http:ftp;"
    :components
    ;; Client variables
    ((:file "package")
     (:file "ftp")))
   ;;
   (:module client
    :serial t
    :components
    ((:file "connection")		; Persistent connections
     (:file "proxy")			; Upstream Proxies
     (:file "authentication")		; Client authentication
     (:file "client")			; Portable client code
     (:file "sexp-browser")		; S-Expression browser application
     (:file "flogger")			; CL-HTTP flogger
     ))))
