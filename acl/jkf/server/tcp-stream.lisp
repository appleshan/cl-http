;; tcp stream subset

(defpackage "IPC"
  (:use) (:export "FRANZ-HOSTNAME" 
		  "GETDOMAINNAME" 
		  "INTERNET-ADDRESS" 
		  "INTERNET-ADDRESS-DOMAIN" 
		  "PROTOCOL-ERROR" 
		  "TCP-CLIENT-ALIVE" 
		  "TCP-CLIENT-STREAM" 
		  "TCP-SERVER-STREAM" 
		  "STREAM-READ"))


(in-package "IPC")

(define-condition unknown-host-name (simple-error)
  ((address :initform 0 :initarg :address))
  (:default-initargs :format-control "host name not found for address ~D."))


(define-condition www-utils::host-not-responding (excl::system-socket-error)
  ())

(define-condition www-utils::connection-closed (excl::system-socket-error)
  ())

(define-condition www-utils::connection-error (excl::system-socket-error)
  ())

(define-condition www-utils::connection-lost (excl::system-socket-error)
  ())


(define-condition www-utils::protocol-timeout (error) 
  ; a necessary placeholder but we don't signal it yet
  ())

(define-condition www-utils::remote-network-error (error) 
  ; a necessary placeholder but we don't signal it yet
  ())

(define-condition www-utils::local-network-error (error) 
  ; a necessary placeholder but we don't signal it yet
  ())

(define-condition http::http-host-stopped-responding (error) 
  ; a necessary placeholder but we don't signal it yet
  ())

(define-condition www-utils::host-stopped-responding (error) 
  ; a necessary placeholder but we don't signal it yet
  ())

(define-condition www-utils::network-error-mixin () 
  ; a necessary placeholder but we don't signal it yet
  ())


(define-condition www-utils::connection-refused (error)
  ;; in acl this isn't a specific condition but is instead a socket
  ;; error with a specific code
  ())

(define-condition http::directory-not-found (error)
  ;; used in cl-http but won't be signalled by acl
  ())
