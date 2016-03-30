(asdf:defsystem cl-http-w4-web-walker-demo
  :pathname "http:"
  :serial t
  :depends-on (cl-http-w4-web-walker)
  :components
  ((:module examples
   :components   
   ((:file "configuration")    ; Standard configuration
    ))
   (:module w4-examples
    :pathname "http:w4;examples;"
    :serial t
    :components   
    ((:file "trace")		; Standard examples
     (:file "search")		; Salton style search example.
     (:file "web-archive")	; Web Whacker
     (:file "exports")		; Basic examples
     ))))
