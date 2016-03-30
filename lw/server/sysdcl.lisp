;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: cl-user; Base: 10 -*-

(in-package "CL-USER")

;; Based on lispm/server/sysdcl.lisp
(sct-defsystem 
 cl-http                          
 (:pretty-name "HTTP Server"
  :default-pathname "HTTP:SERVER;"
  :journal-directory "HTTP:LISPM;SERVER;PATCH;"
  :initial-status :experimental
  :patchable t
  :source-category :basic)
 (:module pointers
  ("SYS:SITE;HTTP.TRANSLATIONS"
   "SYS:SITE;CL-HTTP.SYSTEM"
   "SYS:SITE;W3P.SYSTEM"
   "HTTP:LISPM;HTTP.TRANSLATIONS")
  (:type :lisp-example))
 #+Genera
 (:module showable-procedures
  ("SHOWABLE-PROCEDURES")                      ; Utility for finding procedures
  (:type :system))
 #+Genera
 (:module lispm
  ("HTTP:LISPM;SERVER;TCP-LISPM-STREAM"        ; Modal ASCII or binary TCP streams
   "HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE" ; Timeout TCP close in 5 seconds (Genera 8.3)
   "HTTP:LISPM;SERVER;LISPM"))                 ; LispM Interface functions
 (:module w3p
  ("W3P")                                      ; W3P Presentation System
  (:type :system))
 (:serial
  #+Genera showable-procedures
  #-LispWorks CLIM-SYS                        ; Port your own CLIM-SYS
  #-Genera "HTTP:mcl;server;resources"        ; portable resource package     
  "PACKAGE"                                   ; HTTP Packages
  #+LispWorks
  "HTTP:lw;server;package"                    ; LispWorks specific package changes
  #-LispWorks3.2
  "http:smtp;package"
  "PRELIMINARY"                               ; Showable procedures
  "VARIABLES"                                 ; Variables and constants
  #+Genera lispm                              ; Load Lisp Machine specific code
  #+LispWorks3.2
  "http:lw;server;tcp-stream-3"               ; LispWorks 3.2.2 specific
  #-LispWorks3.2
  "http:lw;server;tcp-stream"                 ; Modern LispWorks version
  #-Genera "http:mcl;server;www-utils"        ; Some portable utils are there
  #+(or LispWorks3.2 LispWorks4.0 LispWorks4.1 LispWorks4.2 LispWorks4.3)
  "http:lw;server;time-and-author"            ; Modern LispWorks file properties
  #+LispWorks
  "http:lw;server;unix"                       ; add-ons for UNIX-like ports
  #+(and LispWorks4.4 (not CL-HTTP-SSL-CLIENT))
  "http:lw;server;sslfli.lisp"                ; SSL FFI Interface for LispWorks 4.4
  #+CL-HTTP-SSL
  "http:lw;server;ssl.lisp"                   ; SSL support
  "base64-encoding"                           ; Base 64 utility RFC-1113 and RFC-1341.
  "MD5"                                       ; MD5 Digests based on RFC 1321
  #+LispWorks
  "http:mcl;server;md5"                       ; Fast, cons-free MD5 specialization code
  "SHA"                                       ; SHA Digests based on Internet FIPS 180
  "TASK-QUEUE"                                ; Thread-Safe Queuing Facility
  "CLASS"                                     ; Server Class Definitions
  "URL-CLASS"                                 ; URL Class Definitions
  "HTTP-CONDITIONS"                           ; HTTP conditions
  "PLIST"                                     ; Property list mixin for CLOS
  "UTILS"                                     ; Server utility functions
  "TOKENIZER"                                 ; Simple tokenizer
  #+CL-HTTP-X509
  "http:lw;server;x509"                       ; X.509 certificates
  "URL"                                       ; URL implementation
  "HEADERS"                                   ; Header hacking, including MIME
  "HOST"                                      ; Host objects and operations
  "HTML2"                                     ; HTML 2.0 Generation
  "NETSCAPE-1-1"                              ; Netscape 1.1 HTML Generation
  "VRML-1-0"                                  ; VRML 1.0 Generation
  "IMAGE-MAPS"                                ; Client-Side Image Maps
  "NETSCAPE-2-0"                              ; Netscape 2.0 HTML Generation
  "NETSCAPE-3-0"                              ; Netscape 3.0 HTML Generation
  "HTML-3-2"                                  ; HTML 3.2 Generation
  "HTML4"				      ; HTML 4.0 Generation
  "XHTML1"                                    ; XHTML 1.0 Generation
  "SCRIPTS"                                   ; Client-Side Scripts
  "RSS-2-0"				      ; RSS 2.0 Generation
  "NETSCAPE-4-0"                              ; Netscape 4.0 HTML Generation
  "SHTML"				      ; Server-Parsed HTML
  w3p                                         ; W3P Presentation System
  "REPORT"				      ; Reporting for HTTP Conditions
  "LOG"                                       ; Logging
  "AUTHENTICATION"                            ; User authentication
  "DATA-CACHE"                                ; Data caching
  "SERVER"                                    ; HTTP Server
  "CGI"                                       ; Common Gateway Interface
  "PREFERENCES"                               ; Configuration Preference Facility
  "WEB-CONFIGURATION"                         ; Server Configuration via the Web
  #+LispWorks "HTTP:lw;server;tcp-interface"
  #-LispWorks3.2 "http:smtp;smtp"               ; Simple SMTP mailer
  #-LispWorks3.2 "http:smtp;mail"               ; Interfaces for sending email
  ))

#+LispWorks
(defsystem configure-and-export
  (:default-pathname "http:examples;"
   :default-type :lisp-file)
  :members (("configuration" :source-only t) ; server configuration file
            ("example-exports" :source-only t)) ; server example exports
  :rules ((:in-order-to :load :all
           (:requires (:load :previous)))))

(sct-defsystem cl-http-base-examples
    (:pretty-name "HTTP Examples"
     :default-pathname "http:examples;"
     :journal-directory "http:lispm;examples;patch;"
     :initial-status :experimental
     :patchable t
     :source-category :basic)
  (:module configure-and-export
    ("configure-and-export")
    (:type :system))
  (:serial
   "documentation"
   "find-url"
   "access-control"
   "lispdoc" ; reference manual for cl-http
   "http:w3p;documentation"
   "log-window"
   #+VRML1.0 "http:examples;vrml;vrml"
   "http:examples;twistdown-tree;twistdown"
   "examples" ;main examples
   "listener"
   "slides"
   #+LispWorks configure-and-export))


;; Based on w3p/sysdcl.lisp
(sct-defsystem w3p
    (:pretty-name "W3 Presentation System"
     :default-pathname "HTTP:W3P;"
     :journal-directory "HTTP:LISPM;W3P;PATCH;"
     :initial-status :experimental
     :patchable t
     :source-category :basic)
  (:module documentation
   ("SYS:SITE;W3P.SYSTEM"
    "HTTP:W3P;DOCUMENTATION")
   (:type :lisp-example))
  (:serial
    "HTTP:W3P;PACKAGE"
    "HTTP:W3P;CLASS"
    "HTTP:W3P;W3P-SYSTEM"
    "HTTP:W3P;FUNCTIONS"
    "HTTP:W3P;STANDARD-TYPES"
    "HTTP:W3P;STANDARD-METHODS"
    "HTTP:W3P;HTML-DEFINITIONS"
    ;; Recompiling defs referenced here undoes indentation changes made
    ;; here so load last (and should reload whenever referenced defs are
    ;; recompiled).  With modules, could say (:uses-definitions-from
    ;; w3p-system).
    #+Genera
    "HTTP:W3P;ZWEI-INDENTATION"))

;; Based on lispm/lambda-ir/sysdcl.lisp
(sct-defsystem lambda-ir
    (:pretty-name "Lambda Information Retrieval System"
     :default-pathname "http:lambda-ir;"
     :journal-directory "http:lispm;lambda-ir;patch;"
     :initial-status :experimental
     :patchable t
     :source-category :basic)
  (:module pointers
   ("sys:site;lambda-ir.system")
   (:type :lisp-example))
  (:serial
    "package"
    "ir-utils"
    "variables"
    "class"
    "bit-vectors"
    "data-structures"
    "computations"
    "ir-base"
    "contexts"
    "constraints"
    "bin-dump-utils"
    "http:lambda-ir;examples;lambdavista"
    "http:lambda-ir;examples;stemming"
    "http:lambda-ir;examples;lambdavista-exports"))

(sct-defsystem mail-archive
    (:pretty-name "Mail Archive Server"
     :default-pathname "http:examples;")
  (:serial
   "mail-archive"))

(sct-defsystem lambda-ir-and-mail-archive-index
    (:pretty-name "Mail Archive Server"
     :default-pathname "http:examples;")
  (:module MAIL-ARCHIVE ("MAIL-ARCHIVE") (:type :system))
  (:module LAMBDA-IR ("LAMBDA-IR") (:type :system))
  (:serial
   LAMBDA-IR
   MAIL-ARCHIVE
   "mail-archive-index"))


;; Based on lispm/proxy/sysdcl.lisp
(sct-defsystem http-proxy
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


#+CLIM
(sct-defsystem http-ui
    (:pretty-name "CL-HTTP CLIM User Interface"
     :default-pathname "http:clim;ui;"
     :journal-directory "http:lispm;clim;ui;patch;"
     :initial-status :experimental
     :patchable t
     :source-category :basic)
  (:module pointers
   ("sys:site;http-ui.system")
   (:type :lisp-example))
  (:serial
    "package" 
    "more-preferences"
    "presentation-types"
    "window-log"
    "http-ui"))