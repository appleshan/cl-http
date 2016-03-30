(in-package :cl-user)

(asdf:defsystem cl-http-examples
  :pathname "http:examples;"
  :depends-on (cl-http-server)
  :serial t
  :components
  ((:file "documentation")
   (:file "find-url")
   (:file "access-control")
   (:file "lispdoc")		; reference manual for cl-http
   #+nil (:file "http:w3p;documentation")
   (:file "log-window")
   (:module vrml
    :components
    ((:file "vrml")	; best to compile this
     ))
   (:module twistdown-tree
    :components
    ((:file "twistdown")
     ))
   (:file "examples")
   (:file "listener")
   (:file "slides")
   ;;
   #+nil (:file "mail-archive")
   ;; Requires Lambda-Vista.
   #+nil (:file "mail-archive-index")
   #+nil (:file "mcf095;mcf")
   #+nil (:file "exports")		; server example exports
   (:file "configuration")	; server configuration file
   (:file "example-exports")	; server example exports
   ))

