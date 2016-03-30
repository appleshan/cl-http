(in-package :cl-user)

(asdf:defsystem cl-http-w4-web-walker
  :pathname "http:"
  :depends-on (cl-http-client)
  :serial t
  :components
  ((:module client
    :components
    ((:file "w4-client")	; W4 client support methods
     ))
   (:module scl
    :pathname "http:scl;client;"
    :components   
    ((:file "w4-client")	; SCL W4 client support methods
     ))
   (:module w4
    :serial t
    :components   
    ((:file "package")		; Package Definition
     (:file "variables")	; Variables
     (:file "utils")		; Utility functions and macros
     (:file "class")		; Class definitions and Print methods
     (:file "walker")		; Main Walker code
     (:file "constraints")	; Constraint Definitions
     (:file "actions")		; Action definitions
     (:file "activity")		; Activity definitions
     ))))

