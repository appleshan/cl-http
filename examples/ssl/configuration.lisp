;;; -*- Mode: LISP; Package: HTTP; Syntax: ANSI-Common-Lisp -*-

;;; Copyright 2006, John C. Mallery.
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;; CONFIGURATION FILE FOR CL-HTTP 
;;;
(in-package :http)

#-SSL-Client-Certificates
(define-https-service
 :port 8443
 :certificate #p"http:examples;ssl;xcert.pem"
 :private-key #p"http:examples;ssl;xkey.pem"
 :password "123456"
 ;; LW 4.4.6 loses parsing logical pathnames with "_" in the file names. -- JCMa 3/25/2006
 :parameters (merge-pathnames "dh_param_512.pem" (translated-pathname "http:examples;ssl;"))
 :ciphers :all
 :ssl-version :ssl-default
 :enable-service-p t)

#+SSL-Client-Certificates
(define-https-service
 :port 8443
 :certificate #p"http:pw;ssl;www-cl-http-org.crt"
 :private-key #p"http:pw;ssl;www-cl-http-org.key"
 :password :none
 :parameters #p"http:pw;ssl;dhparam2048.pem"
 :certificate-authorities #p"http:pw;ssl;ca.crt" ;require client certificates issued by this certificate authority
 :verify :always ;Always verify the client certificates
 :verify-depth 1 ;only go one deep because we're using self-signed certificates here (normal depth is 9)
 :ciphers :all
 :ssl-version :ssl-default
 :enable-service-p t)

;; (undefine-service-on-port 8443)

;; the following variables can be set to change the global defaults
;(setq *standard-protocol* :https)
;(setq *standard-http-port* 8443))

(export-url #u("/cl-http/" :port 8443 :protocol :https)
            :directory
            :recursive-p t
            :pathname "http:www;cl-http;"
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation))

(export-url #u("favicon.ico" :port 8443 :protocol :https)
            :ico-image
            :pathname #p"http:www;cl-http;icons;lambda.ico"
            :public t :max-age #.(* 60 60 24) ;recache every day
            :keywords '(:cl-http :demo)
            :documentation "The Website URL icon.")

(export-url #u("/cl-http/headers.html" :port 8443 :protocol :https)
            :computed
            :response-function #'http-user::compute-headers-page
            :expiration '(:no-expiration-header)
            :public t
            :no-cache t
            :language :en
            :keywords '(:cl-http :demo)
            :documentation "Shows the headers sent by the client to the server.")

;(disable-service-on-port 8443)

(enable-service-on-port 8443)

#|
#+SSL-Client-Certificates
(define-client-certificates ("JCMA")
  ("/Users/jcma/Desktop/jcma-cl-http-org-cert.pem"
   "/Users/jcma/Desktop/jcma-cl-http-org-private-key-clear.pem"))

(define-client-certificates ("JCMA5" :password :prompt)
  ("/Users/jcma/Desktop/jcma-cl-http-org-cert.pem"
   "/Users/jcma/Desktop/jcma-cl-http-org-private-key.pem"))

(define-client-ssl-control "JCMA5"
                           :set-default :client
                           :ssl-version :ssl-default
                           :certificates "JCMA5"
                           :ciphers :all
                           :verify :always
                           :verify-depth 1)
|#
;;;------------------------------------------------------------------- 
;;;
;;; CONFIGURATION FOR SSL PROXY
;;;

;; Not very useful until the SSL pass through is implemented

(define-https-service
 :port 8080
 :certificate #p"http:examples;ssl;xcert.pem"
 :private-key #p"http:examples;ssl;xkey.pem"
 :password "123456"
 ;; LW 4.4.6 loses parsing logical pathnames with "_" in the file names. -- JCMa 3/25/2006
 :parameters (merge-pathnames "dh_param_512.pem" (translated-pathname "http:examples;ssl;"))
 :ciphers :all)

(set-standard-http-proxy-port 8080)

; (disable-proxy-service)

(enable-proxy-service 8080)

;;;------------------------------------------------------------------- 
;;;
;;; CONFIGURATION FOR SSL REVERSE PROXY
;;;

;; The reverse SSL proxy will fetch files from www.cl-http.org and serve
;; them up as if they came from the local host

;; After evaluating this example, try accessing:
;;   http://localhost:8083/cl-http/sources/common-lisp/
;;   http://localhost:8083/cl-http/frame.html
;;   http://localhost:8083/cl-http/headers.html

(define-https-service
 :port 8083
 :certificate #p"http:examples;ssl;xcert.pem"
 :private-key #p"http:examples;ssl;xkey.pem"
 :password "123456"
 ;; LW 4.4.6 loses parsing logical pathnames with "_" in the file names. -- JCMa 3/25/2006
 :parameters (merge-pathnames "dh_param_512.pem" (translated-pathname "http:examples;ssl;"))
 :ciphers :all)

;; Relativize URLs so they will be remapped internally
(setq url:*relativize-urls* t)

;; Turn off caching for testing
(setq *proxy-caching-p* nil)

;; Specify the remapping from the local host on port 8003 to the demo site on port 8000
(define-url-context-remappings
 (:https #.(local-host-domain-name) 8083 "www.cl-http.org" 8000 :http))

;; Turn off caching for testing
(setq *proxy-caching-p* nil)

(export-url #u("/" :port 8083 :protocol :https)
            :reverse-proxy
	    :proxy-forwardings '(("www.cl-http.org" 8000 :http))
            :keywords '(:cl-http :demo :proxy :ssl)
            :documentation "Reverse HTTPS proxy an entire site.")

; (disable-proxy-service 8083)

(enable-proxy-service 8083)

;;;------------------------------------------------------------------- 
;;;
;;; CONFIGURATION FOR CLIENT CERTIFICATES WITH CERTIFICATE REALM
;;;

(define-https-service
 :port 8445
 :certificate #p"http:pw;ssl;www-cl-http-org.crt"
 :private-key #p"http:pw;ssl;www-cl-http-org.key"
 :password :none
 :parameters #p"http:pw;ssl;dhparam2048.pem"
 :certificate-authorities #p"http:pw;ssl;ca.crt" ;require client certificates issued by this certificate authority
 :verify :always ;Always verify the client certificates
 :verify-depth 1 ;only go one deep because we're using self-signed certificates here (normal depth is 9)
 :ciphers :all
 :ssl-version :ssl-default
 :enable-service-p t)

;; (undefine-service-on-port 8445)

;;; May be needed depending on your configuration
;;(add-virtual-host-nick-name "10.0.2.4" 8445 (local-port-context 8445 :https))

;;; Define the certificate realm, a group and an access control.
(define-certificate-realm
 cert-realm
 ()
 :|Webmaster for CL-HTTP.org|) ;provide the issuing authority

(add-group :cert-realm :cert-group)

(add-user "JCMa" :cert-realm
          :personal-name "John C. Mallery" ;person name must agree with X.509 Common Name
          :email-address "jcma@nospam.cl-http.org" ;email address must agree with X.509 1st email address
          :groups '(:cert-group))

(add-access-control-group :cert-capabilities
                          :cert-realm
                          :capabilities '((:default :cert-group)))
                          
;; Export some URLs                        

(export-url #u("/cl-http/" :port 8445 :protocol :https)
            :directory
            :recursive-p t
            :pathname "http:www;cl-http;"
            :authentication-realm :cert-realm
            :capabilities :cert-capabilities
            :language :en
            :keywords '(:cl-http :documentation))

(export-url #u("favicon.ico" :port 8445 :protocol :https)
            :ico-image
            :pathname #p"http:www;cl-http;icons;lambda.ico"
             :authentication-realm :cert-realm
            :capabilities :cert-capabilities
            :keywords '(:cl-http :demo)
            :documentation "The Website URL icon.")

(export-url #u("/cl-http/headers.html" :port 8445 :protocol :https)
            :computed
            :response-function #'http-user::compute-headers-page
            :expiration '(:no-expiration-header)
            :authentication-realm :cert-realm
            :capabilities :cert-capabilities
            :no-cache t
            :language :en
            :keywords '(:cl-http :demo)
            :documentation "Shows the headers sent by the client to the server.")

;(disable-service-on-port 8445)

(enable-service-on-port 8445)

;;;------------------------------------------------------------------- 
;;;
;;; CONFIGURATION FOR HTTP REVERSE PROXY
;;;

;; The reverse proxy will fetch files from www.cl-http.org and serve
;; them up as if they came from the local host

;; After evaluating this example, try accessing:
;;   http://localhost:8003/cl-http/sources/common-lisp/
;;   http://localhost:8083/cl-http/frame.html
;;   http://localhost:8003/cl-http/headers.html

#|
;; Relativize URLs so they will be remapped internally
(setq url:*relativize-urls* t)

;; Specify the remapping from the local host on port 8003 to the demo site on port 8000
(define-url-context-remappings
 (:http #.(local-host-domain-name) 8003 "www.cl-http.org" 8000 :http)
 (:http #.(local-host-ip-address) 8003 "www.cl-http.org" 8000 :http)
#+UNIX
 (:http "127.0.0.1" 8003 "www.cl-http.org" 8000 :http))

#+UNIX ;; make sure the loopback address maps to the local host address
(add-virtual-host-nick-name "127.0.0.1" 8003 (local-port-context 8003 :http))

;; Turn off caching for testing
(setq *proxy-caching-p* nil)

;; This is a reverse proxy example without SSL
(export-url #u("/" :port 8003)
            :reverse-proxy
	    :proxy-forwardings '(("www.cl-http.org" 8000 :http))
            :keywords '(:cl-http :demo :proxy)
            :documentation "Reverse proxy an entire site.")

; (disable-proxy-service 8003)

(enable-proxy-service 8003)
|#
