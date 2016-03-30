(in-package :cl-user)

(asdf:defsystem cl-http-proxy
  :pathname "http:proxy;"
  :depends-on (cl-http-client)
  :serial t
  :components
  (;; Proxy layer
   (:file "uid")		; Persistent Unique IDs using the file system (move into CL-HTTP)
   (:file "package")		; Symbols and exports
   (:file "variables")		; Variables
   (:file "class")		; Class Definitions
   (:file "utils")
   (:file "database")		; Database operations
   (:file "cache")		; Cache operations
   (:file "resource")		; Resource operations
   (:file "representation")	; Representation operations
   (:file "proxy-cache")	; Proxy caching operations
   (:file "proxy")		; Proxy Server
   (:file "documentation")))
