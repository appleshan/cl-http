(in-package :cl-user)

;;; Load the Scieneer CL http and SSL libraries.
(require :http-library)

;;; Rename the HTTP package to SCL-HTTP.
(unless (find-package :scl-http)
  (rename-package :http :scl-http))

;;; lispm major.minor SCL major.minor
(defparameter *cl-http-server-version* '(70 214 0 4 4))

(defvar *cl-http-directory*
  (ext:resolve-pathname "./../" *load-truename*))

(unless (ignore-errors (logical-pathname-translations "http"))
  (setf (logical-pathname-translations "http")
    `(("**;*.*.*" ,(namestring
		    (merge-pathnames "**/*.*.~*~" *cl-http-directory*)))))
  (load "http:scl;translations.lisp"))

(pushnew :cl-http *features*)
(pushnew :cl-http-ssl *features*)
(pushnew :ANSI-documentation *features*)

(setf c:*suppress-values-declaration* t)

(declaim (inline write-char))

(asdf:defsystem cl-http-server
  :description "The Common Lisp HTTP Sever"
  :pathname "http:"
  :serial t
  :components
  ((:module resources
    :pathname "http:mcl;server;"
    :components
    ((:file "resources")	; Portable resource package.
     ))

   (:module server-package
    :pathname "http:server;"
    :components
    ((:file "package")		; HTTP Packages
     ))
   (:module scl-server-package
    :pathname "http:scl;server;"
    :components
    ((:file "package")    ; SCL package changes.
     ))
   (:module smtp-package
    :pathname "http:smtp;"
    :components
    ((:file "package")		; SMTP Package.
     ))

   (:module w3p-package
    :pathname "http:w3p;"
    :components
    ((:file "package")		; Setup the www-present package for precom.
     ))

   (:module server-preliminary
    :pathname "http:server;"
    :serial t
    :components
    ((:file "preliminary")		; Showable procedures
     (:file "variables")))		; Variables and constants

   (:module www-utils
    :pathname "http:mcl;server;"
    :components
    ((:file "www-utils")	; Portable utilities.
     ))
   
   (:module scl-server
    :pathname "http:scl;server;"
    :serial t
    :components
    ((:file "tcp-stream")
     (:file "ftp")
     (:file "unix")))

   (:module server
    :pathname "http:server;"
    :serial t
    :components
    ((:file "base64-encoding")			; base 64 utility
     (:file "md5")				; MD5 utilitity
     (:file "sha")		      ; SHA Digests based on Internet FIPS 180
     (:file "task-queue")
     (:file "class")
     (:file "url-class")
     (:file "http-conditions")			; HTTP conditions
     (:file "plist")				; property list mixin for CLOS
     (:file "utils")				; Server utility functions
     (:file "tokenizer")			; Simple tokenizer
     (:file "headers")	  		        ; Header hacking, including MIME
     (:file "host")				; Host objects and operations
     (:file "url")				; URL implementation
     (:file "html2")				; HTML authoring functions
     (:file "netscape-1-1")			; Netscape 1.1 html generation
     (:file "vrml-1-0")			; VRML 1.0 Generation
     (:file "image-maps")			; Image maps
     (:file "netscape-2-0")			; Netscape 2.0 HTML generation
     (:file "netscape-3-0")			; Netscape 3.0 HTML generation
     (:file "html-3-2")			; HTML 3.2 Generation	
     (:file "html4")				; HTML 4.0 Generation
     (:file "xhtml1")				; XHTML 1.0 Generation
     (:file "scripts")				; Client-Side Scripts
     (:file "rss-2-0")				; RSS 2.0 Generation
     (:file "netscape-4-0")			; Netscape 4.0 HTML generation
     (:file "shtml")				; Server-Parsed HTML
     ))

   (:module w3p
    :pathname "http:w3p;"
    :serial t
    :components
    ((:file "package")
     (:file "class")
     (:file "w3p-system")
     (:file "functions")
     (:file "standard-types")
     (:file "standard-methods")
     (:file "html-definitions")
     (:file "documentation")
     ))

   (:module server2
    :pathname "http:server;"
    :serial t
    :components
    ((:file "report")			; Reporting for HTTP Conditions
     (:file "log")			; Logging 
     (:file "authentication")		; User authentication
     (:file "data-cache")		; Data caching
     (:file "server")			; HTTP Server
     (:file "cgi")			; Common Gateway Interface
     (:file "preferences")	        ; Server preference system
     (:file "web-configuration")      ; Server Configuration via the Web
     ))

   (:module server-patach
    :pathname "http:scl;server;"
    :components
    ((:file "server-patch")))  ; Patches

   (:module tcp-interface
    :pathname "http:scl;server;"
    :components
    ((:file "tcp-interface")))

   (:module smtp
    :pathname "http:smtp;"
    :serial t
    :components
    ((:file "smtp")			; Simple SMTP mailer
     (:file "mail")))			; Interfaces for sending email
   ))
