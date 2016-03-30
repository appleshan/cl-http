;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http; -*-
;;;
;;; (C) Copyright 2006, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; SSL CLIENT CERTIFICATES
;;;

(in-package :http)

;;;------------------------------------------------------------------- 
;;;
;;; PROMPTING FOR SSL PASSWORDS
;;;

#+CAPI
(capi:define-interface gui-ask-user-ssl-certificate-key-pw-dialog ()
  ()
  (:panes
   (certificate-label capi:title-pane)
   (certificate-pane capi:title-pane)
   (password-pane capi:password-pane)
   (confirmation-pane capi:password-pane))
  (:layouts
   (main-layout capi:grid-layout
                '(certificate-label certificate-pane 
                  "Password:" password-pane
                  "Confirm:" confirmation-pane)
                :columns 2))
  (:default-initargs
   :layout 'main-layout))

#+CAPI
(defmethod initialize-instance :after ((self gui-ask-user-ssl-certificate-key-pw-dialog) &key label certificate password)
  (with-slots (certificate-label certificate-pane password-pane confirmation-pane) self
    (setf (capi:title-pane-text certificate-label) label)
    (setf (capi:title-pane-text certificate-pane) certificate)
    (when password
      (setf (capi:text-input-pane-text password-pane) password
            (capi:text-input-pane-text confirmation-pane) password))))

#+CAPI
(defun gui-ask-user-ssl-certificate-key-pw-dialog-password (self)
  (with-slots (password-pane confirmation-pane) self
    (let ((password (capi:text-input-pane-text password-pane))
          (confirmation (capi:text-input-pane-text confirmation-pane)))
      (when (equal password confirmation)
        password))))

(defconstant *gui-ask-user-client-ssl-certificate-menu-title* "SSL Client: Provide Certificate Password")

(defconstant *gui-ask-user-proxy-ssl-certificate-menu-title* "SSL Proxy Client: Provide Certificate Password")

#+CAPI
(defun gui-ask-user-ssl-certificate-key-pw (label certificate pwd proxy-p)
  (loop with dialog = (make-instance 'gui-ask-user-ssl-certificate-key-pw-dialog
                                     :title (if proxy-p
                                                *gui-ask-user-proxy-ssl-certificate-menu-title*
                                              *gui-ask-user-client-ssl-certificate-menu-title*)
                                     :label label
                                     :certificate certificate :password pwd)
        while (capi:popup-confirmer dialog nil)
        do (let ((pwd (gui-ask-user-ssl-certificate-key-pw-dialog-password dialog)))
             (when pwd
               (return-from gui-ask-user-ssl-certificate-key-pw (values pwd nil))))
        finally (return (values nil t))))

(defun ask-user-ssl-certificate-key-pw (stream label certificate pwd proxy-p)
  #+CAPI
  (when (capi:screens)
    (return-from ask-user-ssl-certificate-key-pw
      (gui-ask-user-ssl-certificate-key-pw label certificate pwd proxy-p)))
  (let (npwd)
    (fresh-line stream)
    (write-string (if proxy-p *gui-ask-user-proxy-ssl-certificate-menu-title* *gui-ask-user-client-ssl-certificate-menu-title*) stream)
    (format stream "~&~A ~A" label certificate)
    (if pwd
        (format stream "~&Password (default: ~a): " (make-string (length pwd) :initial-element #\*) pwd)
      (format stream "~&Password: "))
    (setq npwd (read-line stream nil nil))
    (if (equal npwd "")
        (setq npwd pwd))
    (values (or npwd pwd) (not npwd))))

(defun get-ssl-certificate-private-key-password (label certificate &optional proxy-p (stream *query-io*))
  (declare (values password abort-p))
  (ask-user-ssl-certificate-key-pw stream label certificate nil proxy-p))

;;;------------------------------------------------------------------- 
;;;
;;; CERTIFICATE SETS
;;;

(defclass certificate-set
          ()
  ((name :initarg :name :reader certificate-set-name)
   (certificate-specs :initform nil :initarg :certificate-specs :accessor certificate-set-certificate-specs :type list)
   (certificate-and-private-key-pointers :initform nil :initarg certificate-and-private-key-pointers
                                         :reader certificate-and-private-key-pointers))
  (:documentation "A set of certificated and associates private keys."))

(defclass client-certificates 
          (certificate-set)
  ()
  (:documentation "X.509 certificates for client authentication over SSL."))

(defmethod print-object ((certificate-set certificate-set) stream)
  (with-slots (name) certificate-set
    (print-unreadable-object (certificate-set stream :type t :identity t)
      (when name
        (write-string name stream)))))

(defparameter *certificate-set-class* 'client-certificates
  "The default class to use when creating certificate sets.")

(defvar *certificate-set-table* nil)

(declaim (inline certificate-set-table))

(defun certificate-set-table ()
  (or *certificate-set-table*
      (setq *certificate-set-table* (make-hash-table :test #'equalp))))

(defmethod register ((certificate-set certificate-set))
  (with-slots (name) certificate-set
    (setf (gethash name (certificate-set-table)) certificate-set)
    certificate-set))

(defmethod unregister ((certificate-set certificate-set))
  (with-slots (name) certificate-set
    (remhash name (certificate-set-table))))

(defun clear-certificate-sets ()
  "Definitializes all certificate sets and clears the certificate set table."
  (flet ((deinitialize (key val)
           (declare (ignore key))
           (deinitialize-certificate-set val)))
    (when *certificate-set-table*
      (maphash #'deinitialize *certificate-set-table*)
      (clrhash *certificate-set-table*))))

(defun find-certificate-set (name &optional (error-p t))
  (cond ((etypecase name
           (string (gethash name (certificate-set-table)))
           (symbol (gethash (symbol-name name) (certificate-set-table)))
           (certificate-set 
            (gethash (certificate-set-name name) (certificate-set-table)))))
        (error-p
         (error "The certificate set named, ~S, was not found." name))
        (t nil)))

(defun intern-certificate-set (name &key (if-does-not-exist :error) (class *certificate-set-class*))
  (declare (values certificate-set newly-created-p))
  (let ((certificate-set (find-certificate-set name nil)))
    (cond (certificate-set certificate-set)
          (t (ecase if-does-not-exist
               (:soft nil)
               (:error (error "The certificate set named, ~S, was not found." name))
               (:create
                (setq certificate-set (make-instance class :name (string name)))
                (register certificate-set)
                (values certificate-set t)))))))

(defmethod initialize-certificate-set ((certificate-set certificate-set) &optional password)
  (macrolet ((with-password ((binding password) &body body)
               `(let ((,binding (etypecase ,password
                                  (string (coerce ,password 'simple-base-string))
                                  (keyword
                                   (ecase ,password
                                     (:prompt :prompt)
                                     (:none ""))))))
                
                  ,@body))
             (with-parsed-entry ((entry file password) &body body)
               `(let (,file ,password)
                  (etypecase ,entry
                    (atom (psetq ,file ,entry ,password nil))
                    (cons (destructuring-bind (file &optional pwd) ,entry
                            (psetq ,file file ,password pwd))))
                  ,@body)))
    (labels ((%read-password (prompt filename stream)
               (let ((pwd (get-ssl-certificate-private-key-password prompt filename nil stream)))
                 (unless (< (length pwd) 1000)
                   (error "SSL Certificate Error: Password too long. The maximum length for OpenSSL passwords is 999."))
                 pwd))
             (get-master-password (password)
               (etypecase password
                 (null :none)
                 (string password)
                 (keyword
                  (ecase password
                    (:prompt (%read-password "Master Password:" "All certificate & private key files" *query-io*))
                    (:none :none)))))
             (ssl-client-filename (file)
               (ssl-filename file (user-homedir-pathname) "SSL Client Certificate Error"))
             (read-certificate (pathname password)
               (let ((filename (ssl-client-filename pathname)))
                 (with-password (pwd password)
                  (case pwd
                    (:prompt
                     (let ((prompt "Certificate:"))
                       (flet ((read-password (type filename)
                                (declare (ignore type))
                                (%read-password prompt filename *query-io*)))
                         (comm::read-certificate-file filename :callback #'read-password))))
                    (t (comm::read-certificate-file filename :pass-phrase pwd))))))
             (read-private-key (pathname password)
               (let ((filename (ssl-client-filename pathname)))
                 (with-password (pwd password)
                  (case pwd
                    (:prompt
                     (let ((prompt "Private Key:"))
                       (flet ((read-password (type filename)
                                (declare (ignore type))
                                (%read-password prompt filename *query-io*)))
                         (comm::read-private-key-file filename :callback #'read-password))))
                    (t (comm::read-private-key-file filename :pass-phrase pwd)))))))
      (with-slots (certificate-specs certificate-and-private-key-pointers) certificate-set
        (loop with passwd = (get-master-password password)
              for (certificte-spec private-key-spec) in certificate-specs
              for certificates = (with-parsed-entry
                                  (certificte-spec certificate-file certificate-password)
                                  (read-certificate certificate-file (or certificate-password passwd)))
              for private-keys = (with-parsed-entry
                                  (private-key-spec private-key-file private-key-password)
                                  (read-private-key private-key-file (or private-key-password passwd)))
              when (= (length certificates) (length private-keys))
              nconc (mapcar #'list certificates private-keys) into cert-set
              else do (error "SSL Certificate Error: Cannot initialization the certificate set, ~S, due to unmatched certificates and private keys in~
                              ~&Certificate File: ~A~&Private Key File: ~A" certificate-set certificte-spec private-key-spec)
              finally (setq certificate-and-private-key-pointers cert-set))
        certificate-set))))

(defmethod deinitialize-certificate-set ((certificate-set certificate-set))
  (with-slots (certificate-and-private-key-pointers) certificate-set
    (when certificate-and-private-key-pointers
      (loop for (certificate-ptr private-key-ptr) in certificate-and-private-key-pointers
            unless (fli:null-pointer-p certificate-ptr)
            do (comm::x509-free certificate-ptr)
            unless (fli:null-pointer-p private-key-ptr)
            do (comm::evp-pkey-free private-key-ptr)
            finally (setq certificate-and-private-key-pointers nil))
      certificate-set)))

;; Always deallocate C structures when resetting the certificates
(defmethod (setf certificate-set-certificate-specs) :after (certificate-specs (certificate-set certificate-set))
  (declare (ignore certificate-specs))
  (deinitialize-certificate-set certificate-set))

(defun %define-client-certificates (set-name master-password set-certificate-specs)
  (declare (values certificate-set newly-created-p))
  (multiple-value-bind (client-certificates newly-created-p)
      (intern-certificate-set set-name :if-does-not-exist :create)
    (setf (certificate-set-certificate-specs client-certificates) set-certificate-specs)
    (initialize-certificate-set client-certificates master-password)
    (values client-certificates newly-created-p)))

(defmacro define-client-certificates ((name &key password) &body certificate-specs)
  "Defines a certificate set for use with the CL-HTTP client.

When an SSL server requests client certificates, the matching certificte to send will be selected by
the Cl-HTTP client from the set provided here. This client certificate set is also used by the W4 Web Walker.

NAME is the name for this collection of certificates.

PASSWORD is the default password to use when no more specific password is supplied.

CERTIFICATE-SPECS are pairs of (CERTIFICATE-FILE PRIVATE-KEY-FILE) where

     CERTIFICATE-FILE is either a fully-specified PEM encoded certificate file 
     or a cons of (CERTIFICATE-FILE PASSWORD)

     PRIVATE-KEY-FILE is either a fully-specified PEM encoded privated key file associted with the certificate file
     or a cons of (PRIVATE-KEY-FILE PASSWORD)

When a certificate file or private key file is encrypted, the password for decrypting it must be
supplied using the extended syntax.

PASSWORD can be: a string containg the password or one of the keywords :NONE or :PROMPT. For
OpenSSL, a password string cannot exceed 999 characters. When the keyword :NONE is supplied, the
file must be unencrypted. When the keyword :PROMPT is supplied, the user is prompted for the
password."
  `(%define-client-certificates ',name ',password ',certificate-specs))

#+ignore
(defmethod select-certificate-to-present ((certificate-set certificate-set) case ssl-pointer)
  (declare (ignore case))
  (flet ((choose-certificate (ssl-pointer certificate-list) ;; Pop up menu when interactive.
           (declare (ignore ssl-pointer))
           ;; return the first match for now
           (values (caar certificate-list) (cadar certificate-list))))
    (let* ((certificates (certificate-and-private-key-pointers certificate-set))
           (matches (comm::select-certificates-matching-ca-list ssl-pointer certificates)))
      (cond (matches
             (if (cdr matches)
                 (choose-certificate ssl-pointer matches)
               (values (caar matches) (cadar matches))))
            (t (values nil nil))))))

;; No consing
(defmethod select-certificate-to-present ((certificate-set certificate-set) case ssl-pointer)
  (declare (ignore case))
  (let ((certificates (certificate-and-private-key-pointers certificate-set)))
    (comm::lookup-in-certificate-key-pairs ssl-pointer certificates)))

;;;------------------------------------------------------------------- 
;;;
;;; SSL CONTROLS
;;;

(defclass ssl-control
         ()
  ((ctx :initarg :ssl-ctx ::reader ssl-ctx)
   (name :initarg :name :reader ssl-control-name)
   (version :initarg :version :accessor ssl-version :type keyword)
   (ciphers :initarg :ciphers :accessor ssl-ciphers :type string)
   (verify-mode :initarg :verify-mode :accessor ssl-verify-mode :type keyword)
   (verify-depth :initarg :verify-depth :accessor ssl-verify-depth :type fixnum))
  (:documentation "SSL control for SSL tunnels over TCP."))

(defclass client-ssl-control
          (ssl-control)
  ((certificate-set :initform nil :initarg :certificate-set :accessor ssl-certificate-set))
  (:documentation "SSL control for client SSL tunnels over TCP."))

(defmethod print-object ((ssl-control ssl-control) stream)
  (with-slots (name version) ssl-control
    (print-unreadable-object (ssl-control stream :type t :identity t)
      (format stream "~A (~:(~A~))" name version)
      ssl-control)))

(defparameter *ssl-control-class* 'client-ssl-control
  "The default class to use when creating SSL controls.")

(defvar *ssl-control-table* nil)

(declaim (inline ssl-control-table))

(defun ssl-control-table ()
  (or *ssl-control-table*
      (setq *ssl-control-table* (make-hash-table :test #'equalp))))

(defmethod register ((ssl-control ssl-control))
  (with-slots (name) ssl-control
    (setf (gethash name (ssl-control-table)) ssl-control)
    ssl-control))

(defmethod unregister ((ssl-control ssl-control))
  (with-slots (name) ssl-control
    (remhash name (ssl-control-table))))

(defun clear-client-ssl-controls ()
  "Definitializes all client SSL controls and clears the SSL control table."
  (flet ((deinitialize (key val)
           (declare (ignore key))
           (ssl-ctx-deinitialize val)))
    (when *certificate-set-table*
      (maphash #'deinitialize *ssl-control-table*)
      (clrhash *ssl-control-table*))))

(defun find-ssl-control (name &optional (error-p t))
  (cond ((etypecase name
           (string (gethash name (ssl-control-table)))
           (symbol (gethash (symbol-name name) (ssl-control-table)))
           (ssl-control 
            (gethash (ssl-control-name name) (ssl-control-table)))))
        (error-p
         (error "The client SSL control named, ~S, was not found." name))
        (t nil)))

(defun intern-ssl-control (name &key (if-does-not-exist :error) (class *ssl-control-class*))
  (declare (values ssl-control newly-created-p))
  (let ((ssl-control (find-ssl-control name nil)))
    (cond (ssl-control ssl-control)
          (t (ecase if-does-not-exist
               (:soft nil)
               (:error (error "The client SSL control named, ~S, was not found." name))
               (:create
                (setq ssl-control (make-instance class :name (string name)))
                (register ssl-control)
                (values ssl-control t)))))))

(defun unintern-ssl-control (name)
  "Uninterns the SSL control named, NAME."
  (let ((ssl-control (find-ssl-control name nil)))
    (when ssl-control
      (ssl-ctx-deinitialize ssl-control)
      (unregister ssl-control))))

(defmethod ssl-ctx-initialize ((ssl-control client-ssl-control))
  (with-slots (ctx version certificate-set ciphers verify-mode verify-depth) ssl-control
    (flet ((select-certificate (ssl-pointer)
             (select-certificate-to-present certificate-set :non-interactive ssl-pointer)))
      (when (slot-boundp ssl-control 'ctx)
        (ssl-ctx-deinitialize ssl-control))
      ;; Make a new CTX and initialize from slots
      (setq ctx (comm:make-ssl-ctx :ssl-ctx version :ssl-side :client))
      (when certificate-set
        (comm::set-ssl-ctx-cert-cb ctx #'select-certificate))
      (comm:set-cipher-list ctx ciphers)
      (comm::set-verification-mode ctx :client verify-mode)
      (comm::ssl-ctx-set-verify-depth ctx verify-depth)
      ssl-control)))

(defmethod ssl-ctx-deinitialize ((ssl-control client-ssl-control))
  (macrolet ((deallocate-slots (ssl-control &body clauses)
               `(cond-every
                 ,.(loop for (slot . clause) in clauses
                         collect `((slot-boundp ,ssl-control ',slot) ,@clause (slot-makunbound ,ssl-control ',slot)))))
             (maybe-free-foreign-object (slot)
               `(cond ((not (fli:pointerp ,slot))
                       (cerror "Skip freeing OpenSSL pointer" "~A is not a C pointer." ',slot))
                      ((fli:null-pointer-p ,slot)) ;no action required
                      (t (fli:free-foreign-object ,slot))))) ;free OpenSSL structure
    (with-slots (ctx) ssl-control
      (deallocate-slots
       ssl-control
       (ctx ; Free memory in FIFO order or crashes result - CTX first
        (comm:destroy-ssl-ctx ctx)))
      ssl-control)))

(defvar *client-ssl-control* nil
  "Holds the standard client SSL control object.")

(defvar *proxy-ssl-control* nil
  "Holds the standard proxy client SSL control object.")

(defmacro with-ssl-control ((ssl-control &key (case :client)) &body body)
  "Binds the default SSL control for CASE to SSL-CONTROL within the scope of BODY.
CASE can be either :CLIENT or :PROXY."
  `(let ((,(ecase case (:client '*client-ssl-control*) (:proxy '*proxy-ssl-control*))
          ,ssl-control))
     ,@body))


(defgeneric set-standard-ssl-control (ssl-control case)
  (:documentation "Sets SSL-CONTROL to be the standard SSL control according to CASE,
which can be either :CLIENT or :PROXY."))

(defmethod set-standard-ssl-control ((ssl-control client-ssl-control) case)
  (ecase case
    (:client
     (setq *client-ssl-control* ssl-control))
    (:proxy
     (setq *proxy-ssl-control* ssl-control))))

(defmethod set-standard-ssl-control ((ssl-control (eql :none)) case)
  (ecase case
    (:client
     (setq *client-ssl-control* nil))
    (:proxy
     (setq *proxy-ssl-control* nil))))

(defun %define-client-ssl-control (name &key (set-default :client) (ssl-version :ssl-default) (ciphers :all)
                                        (verify :always) (verify-depth 9) certificates certificate-password)
  (check-type verify (member :never :always))
  (check-type verify-depth (fixnum 1 32))
  (multiple-value-bind (ssl-control newly-created-p)
      (intern-ssl-control name :if-does-not-exist :create)
    (setf (ssl-version ssl-control) (www-utils::%convert-ssl-versions-for-lw ssl-version)
          (ssl-ciphers ssl-control) (ssl-cipher-string ciphers)
          (ssl-verify-mode ssl-control) verify
          (ssl-verify-depth ssl-control) verify-depth)
    (etypecase certificates
      (null) ; No client certificates
      ((or string symbol) ; Named certificate set
       (setf (ssl-certificate-set ssl-control) (intern-certificate-set certificates :if-does-not-exist :error)))
      (certificate-set ; Certificate set itself
       (setf (ssl-certificate-set ssl-control) certificates))
      (cons ; List specification for a certificate set to be given the same name as the SSL control
       (setf (ssl-certificate-set ssl-control) (%define-client-certificates certificate-password name certificates))))
    ;; Initializat the SSL CTX
    (ssl-ctx-initialize ssl-control)
    ;; Set the default
    (set-standard-ssl-control ssl-control set-default)
    ;; Return some values
    (values ssl-control newly-created-p)))

(defmacro define-client-ssl-control (name &key (set-default :client) (ssl-version :ssl-default) 
                                          (ciphers :all) (verify :always) (verify-depth 9) certificates certificate-password)
  "Defines a client SSL control named NAME.

SET-DEFAULT        - A keyword indicating wether this SSL control should be used as the default control for 
                     outbound SSL connections by the client or proxy. The following values are possible:

                       :CLIENT        -  Use as default for all client SSL connections.
                       :PROXY         -  Use as default for all outbound proxy SSL connections.
                       :NONE          -  Do not make this SSL control a default.

SSL-VERSION        - A keyword that controls the version of SSL/TLS used for outgoing connections. The options are:

                       :TLS-1       - Use only TLS version 1
                       :SSL-2-OR-3  - Use only SSL version 2 or 3 
                       :SSL-3       - Use only SSL version 3
                       :SSL-2       - Use only SSL version 2
                       :SSL-DEFAULT - Use the current default SSL versions (2 or 3)


CIPHERS               - A keyword or string denoting OpenSSL cipher suites to use with SSL on PORT.
                        The following values are available:

                          :ALL           - all ciphers suites offering at least some encryption.
                          :HIGH          - ciphers with key lengths greater than 128 bits.
                          :MEDIUM        - ciphers using 128 bit encryption.
                          :LOW           - ciphers using 64 or 56 bit encryption algorithms but excluding export cipher suites.
                          :56-BIT-EXPORT - ciphers using 56 bit export encryption algorithms.
                          :EXPORT        - ciphers using export encryption algorithms, including both 40 and 56 bits algorithms.
                          :40-BIT-EXPORT - ciphers using 40 bit export encryption algorithms.

                        Users desiring fine-grained control may provide an OpenSSL cipher string. 
                        These are described here: http://www.openssl.org/docs/apps/ciphers.html
   
VERIFY                - A keyword that controls server certificate verification by the SSL client.

                          :NEVER  - The client does not check the server certificate.

                          :ALWAYS - The client always checks the server certificate request to the client. If the verification
                                    process fails, the TLS/SSL handshake is immediately terminated with an alert message 
                                    containing the reason for the verification failure.

VERIFY-DEPTH           - An integer between 1 and 32 specifiying maximum certificate chain length for authentication of client 
                         or peer certificates.

CERTIFICATES          - A string denoting a client certificate set, or a list of certificate file specs in the form
                        (CERTIFICATE-FILE PRIVATE-KEY-FILE) where,

                           CERTIFICATE-FILE is either a fully-specified PEM encoded certificate file or a cons 
                           of (CERTIFICATE-FILE PASSWORD).

                           PRIVATE-KEY-FILE is either a fully-specified PEM encoded privated key file associted with the
                           certificate file or a cons of (PRIVATE-KEY-FILE PASSWORD).

                         When a certificate file or private key file is encrypted, the password for decrypting it must be 
                         supplied using the extended syntax.

                         PASSWORD can be: a string containg the password or one of the keywords :NONE or :PROMPT. For
                         OpenSSL, a password string cannot exceed 999 characters. When the keyword :NONE is supplied, the
                         file must be unencrypted. When the keyword :PROMPT is supplied, the user is prompted for the password.

CERTIFICATE-PASSWORD  - The default password to use when no more specific password is supplied for certificate or private key files."
  `(%define-client-ssl-control ',name :set-default ',set-default :ssl-version ',ssl-version :ciphers ',ciphers
                               :verify ',verify :verify-depth ',verify-depth 
                               :certificates ',certificates :certificate-password ',certificate-password))

(defun set-default-ssl-control (case &key (ssl-version :ssl-default) (ciphers :all) (verify :always) (verify-depth 9))
  "Sets the default SSL control for CASE according to its arguments.
CASE can be either :client or :proxy."
  (%define-client-ssl-control (ecase case
                                (:client "default-client-ssl-control")
                                (:proxy "default-proxy-ssl-control"))
                              :set-default :client
                              :ssl-version ssl-version
                              :ciphers ciphers
                              :verify verify
                              :verify-depth verify-depth))

(export '(define-client-certificates define-client-ssl-control with-ssl-control unintern-ssl-control intern-ssl-control set-default-ssl-control)
        :http)

(defun clear-client-ssl-structures ()
  (set-standard-ssl-control :none :client)
  (set-standard-ssl-control :none :proxy)
  (clear-client-ssl-controls)
  (clear-certificate-sets))

(add-initialization "CLear Client SSL" '(clear-client-ssl-structures) '(:normal) '*shutdown-initialization-list*)
