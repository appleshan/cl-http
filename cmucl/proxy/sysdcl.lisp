;;;------------------------------------------------------------------- 
;;;
;;;  SYSTEM DEFINITION CL-HTTP PROXY SERVICE
;;;

(in-package :cl-user)

(mk:defsystem cl-http-proxy
    :source-pathname "HTTP:"
    :components
    (;; Proxy layer
     "proxy;uid"		; Persistent Unique IDs using the file system (move into CL-HTTP)
     "proxy;package"		;Symbols and exports
     "proxy;variables"		;Variables
     "proxy;class"		;Class Definitions
     "proxy;utils"
     "proxy;database"		;Database operations
     "proxy;cache"		;Cache operations
     "proxy;resource"		;Resource operations
     "proxy;representation"	;Representation operations
     "proxy;proxy-cache"	;Proxy caching operations
     "proxy;proxy"		;Proxy Server
     "proxy;documentation"))
