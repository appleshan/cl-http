;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: www-utils -*-
;;;
;;; (C) Copyright 2006, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; SSL FOREIGN FUNCTION CALLS FOR LISPWORKS 4.4
;;;

(in-package :www-utils)

;; http://www.openssl.org/docs/ssl/SSL_CTX_set_verify.html
(fli:define-foreign-function 
    (comm::ssl-ctx-set-verify-depth "SSL_CTX_set_verify_depth" :source)
    ((ctx :pointer)
     (depth :int))
  :result-type :void
  :language :ansi-c)

;; http://www.openssl.org/docs/ssl/SSL_CTX_set_verify.html
(fli:define-foreign-function 
    (comm::ssl-ctx-set-verify "SSL_CTX_set_verify" :source)
    ((ctx :pointer)
     (mode :int)
     (callback :pointer))
  :result-type :void
  :language :ansi-c)

(defconstant ssl_verify_none #x00)
(defconstant ssl_verify_peer #x01)
(defconstant ssl_verify_fail_if_no_peer_cert #x02)
(defconstant ssl_verify_client_once #x04)

(defun comm::set-verification-mode (ctx ssl-side mode &optional callback)
  "Sets the verification mode for CTX according to SSL-SIDE and MODE.

When SSL-SIDE is :SERVER, MODE can be:

     :NEVER - The server will not send a client certificate request to
     the client, so the client will not send a certificate.

     :ALWAYS - The server sends a client certificate request to the
     client. The certificate returned (if any) is checked. If the
     verification process fails, the TLS/SSL handshake is immediately
     terminated with an alert message containing the reason for the
     verification failure.

     :ONCE - Same as :ALWAYS except the a client certificate is
     checked only on the initial TLS/SSL handshake, and not again in
     case of renegotiation.

When SSL-SIDE is :CLIENT, MODE can be:

     :NEVER - If not using an anonymous cipher, the server will send a
     certificate which will be checked by the client. The handshake
     will be continued regardless of the verification result.

     :ALWAYS - The server certificate is verified. If the verification
     process fails, the TLS/SSL handshake is immediately terminated
     with an alert message containing the reason for the verification
     failure. If no server certificate is sent, because an anonymous
     cipher is used, verification is ignored.

CALLBACK - is an optional C function that is called to perform verification."
  (declare (ignore callback))
  (flet ((server-mode-translate (mode)
           (ecase mode
             (:never ssl_verify_none)
             (:always 
              (logior ssl_verify_peer ssl_verify_fail_if_no_peer_cert))
             (:once
              (logior ssl_verify_peer ssl_verify_client_once))))
         (client-mode-translate (mode)
           (ecase mode
             (:never ssl_verify_none)
             (:always ssl_verify_peer))))
    (declare (inline server-mode-translate client-mode-translate))
    (comm::ssl-ctx-set-verify ctx 
                               (ecase ssl-side
                                 (:server (server-mode-translate mode))
                                 (:client (client-mode-translate mode)))
                               nil)))

(defconstant ssl_max_ssl_session_id_length 32)

(fli:define-foreign-function 
    (comm::ssl-ctx-set-session-id-contex "SSL_CTX_set_session_id_context" :source)
    ((ctx :pointer)
     (session-id :pointer)
     (length :int))
  :result-type :int
  :language :ansi-c)

(defun comm::set-session-id-context (ctx session-id)
"SESSION-ID must be a string of length less/equal 32 (8-bits) characters (SSL_MAX_SSL_SESSION_ID_LENGTH). 
It sets the session ID of the SSL-POINTER or SSL-CTX-POINTER to the string."
  (declare (values success-p))
  (let ((session-id-string (etypecase session-id
                             (lw:8-bit-string session-id)
                             (t (coerce session-id 'lw:8-bit-string)))))
    (declare (dynamic-extent session-id-string))
    (fli:with-foreign-string (ptr size c-byte-count :null-terminated-p t :allow-null nil) session-id-string
      (declare (ignore size))
      (unless (> c-byte-count ssl_max_ssl_session_id_length)
        (when (eq 1 (comm::ssl-ctx-set-session-id-contex ctx ptr c-byte-count))
          (return-from comm::set-session-id-context t))))
    ;; Report lossage
    (error "SESSION-ID, ~S, is not less than ~D characters." session-id ssl_max_ssl_session_id_length)))

;; Return codes documented in http://www.openssl.org/docs/apps/verify.html
;; X509_V_OK is 0 and means successful verification or no certificate presented
(fli:define-foreign-function
    (comm::ssl-get-verify-result "SSL_get_verify_result" :source)
    ((ssl :pointer))
  :result-type :long
  :language :ansi-c)

(fli:define-foreign-function
    (comm::ssl-get-peer-certificate "SSL_get_peer_certificate" :source)
    ((ssl :pointer))
  :result-type :pointer
  :language :ansi-c)
