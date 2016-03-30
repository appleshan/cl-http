;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http; -*-
;;;
;;; (C) Copyright 2006, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; X.509 CERTIFICATES
;;;
;;; http://www.ietf.org/rfc/rfc2459.txt
;;; http://www.openssl.org/docs/crypto/x509.html

(in-package :http)

(defclass certificate () ())

(defclass x509-certificate
          (certificate property-list-mixin)
  ((x509-pointer :initform nil :initarg :x509-pointer :reader %%x509-pointer)
   (must-deallocate-p :initform nil :initarg :must-deallocate-p :reader x509-must-deallocate-p))
  (:documentation "An X.509 certificate object."))

(defmethod print-object ((certificate x509-certificate) stream)
  (print-unreadable-object (certificate stream :type t :identity t)
    (when (x509-pointer-valid-p certificate)
      (let ((common-name (x509-common-name certificate))
            (issuer-common-name (x509-issuer-common-name certificate)))
        (cond-every
         (common-name
          (format stream "~A" common-name))
         (issuer-common-name
          (format stream " (~A)" issuer-common-name)))))
    certificate))

;;;------------------------------------------------------------------- 
;;;
;;; X.509 UTILITIES
;;;

(declaim (inline %x509-pointer-valid-p))

(defun %x509-pointer-valid-p (x509-pointer)
  (and (comm::x509-pointer-p x509-pointer)
       (not (fli:null-pointer-p x509-pointer))))

(defmethod x509-pointer-valid-p ((x509-certificate x509-certificate))
  "Returns non-null when X509-POINTER is valid."
  (with-slots (x509-pointer) x509-certificate
    (%x509-pointer-valid-p x509-pointer)))

(defun %x509-pointer (x509-certificate)
  (let ((ptr (%%x509-pointer x509-certificate)))
    (if (%x509-pointer-valid-p ptr)
        ptr
      (error "Invalid X.509 pointer in ~S." x509-certificate))))

(defmethod x509-pointer ((x509-certificate x509-certificate))
  "Returns a valid x509-pointer or, when the pointer is invalid, signals an error."
  (declare (values x509-pointer))
  (%x509-pointer x509-certificate))

(defun allocate-x509-certificate (x509-pointer &optional must-deallocate-p)
  "Allocates an X.509 certificate object for X509-POINTER."
  (cond ((%x509-pointer-valid-p x509-pointer)
         (make-instance 'x509-certificate :x509-pointer x509-pointer :must-deallocate-p must-deallocate-p))
        (t (error "Invalid X.509 pointer ~S." x509-pointer))))

(defmethod deinitialize-x509-certificate ((x509-certificate x509-certificate))
  "Deinitializes an X.509 certificate object."
  (macrolet ((maybe-free-foreign-object (slot)
               `(cond ((not (fli:pointerp ,slot))
                       (cerror "Skip freeing X.509 certificate pointer" "~A is not a C pointer." ',slot))
                      ((fli:null-pointer-p ,slot)) ;no action required
                      (t (fli:free-foreign-object ,slot)))))
    (with-slots (must-deallocate-p x509-pointer) x509-certificate
      (when must-deallocate-p
        (maybe-free-foreign-object x509-pointer))
      (setq x509-pointer nil
            must-deallocate-p nil)
      x509-certificate)))

(defmethod deallocate-x509-certificate ((x509-certificate x509-certificate))
  "Deallocates an X.509 certificate object."
  (deinitialize-x509-certificate x509-certificate))

;;;------------------------------------------------------------------- 
;;;
;;; VALUE CACHING FOR X.509 CERTIFICATES
;;;

;; Record X509 value cache keys
(eval-when (:compile-toplevel :execute)
(defvar *x509-cache-keys* nil))

(eval-when (:load-toplevel)
(defvar *x509-cache-keys*))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun %note-x509-cache-key (key)
  "Remembers KEY used to cache values on X509 PLISTs."
  (flet ((coerce-key (key)
           (etypecase key
             (keyword key)
             (cons
              (destructuring-bind (fctn . args) key
                (ecase fctn
                  (quote (car args))))))))
    (pushnew (coerce-key key) *x509-cache-keys*)))

(defmacro remember-x509-cache-keys ()
 `(setq *x509-cache-keys* ',(sort *x509-cache-keys*  #'string< :key #'symbol-name))))

(defmethod clear-x509-cached-values ((x509-certificate x509-certificate))
  "Clears any cached values for x509-certificate."
  (loop with plist = (property-list x509-certificate)
        for ptr = plist then (cddr ptr)
        while ptr
        do (let ((entry (cddr ptr)))
             (when (member (first entry) *x509-cache-keys*)
               (setf (cddr ptr) (cddr entry))))
        finally (return (if (member (first plist) *x509-cache-keys*)
                            (setf (property-list x509-certificate) (cddr plist))
                          plist))))

(define-macro with-x509-value-cached ((x509-certificate key &key (recompute-p nil recompute-supplied-p)) &body value-form)
  "Caches the value returned by value-form on X509's property list under the indicator KEY.
When RECOMPUTE-P is non-null, the value is recomputed and recached.
The returned values are VALUE and RETRIEVE-FROM-CACHE-P."
  (declare (values retrieved-from-cache-p))
  (let ((form `(let ((val (getf plist ,key :+not-found+)))
                 (case val
                   (:+not-found+ 
                    (setf (getf plist ,key) (progn  . ,value-form)))
                   (t (values val t))))))
    (%note-x509-cache-key key) ;remember the cache keys
    (cond (recompute-supplied-p
           `(with-slots (plist) ,x509-certificate
              (cond (,recompute-p
                     (setf (getf plist ,key) (progn . ,value-form)))
                    (t ,form))))
          (t `(with-slots (plist) ,x509-certificate ,form)))))

;;;------------------------------------------------------------------- 
;;;
;;; X.509 DISTRINGUISHED NAMES
;;;

(defclass x509-distinguished-name
          ()
  ((common-name :initarg :common-name :accessor %x509-common-name :type string)
   (organizational-unit-name :initarg :organizational-unit-name :accessor %x509-organizational-unit-name :type string)
   (organization-name :initarg :organization-name :accessor %x509-organization-name :type string)
   (country-name :initarg :country-name :accessor %x509-country-name :type string)
   (name-string :initform nil :accessor %x509-name-string :type (or null string))
   (certificate :initform nil :accessor %x509-certificate :type (or null x509-certificate)))
  (:documentation "A globally unique distinguished name for a certificate holder."))

(defmethod print-object ((distinguished-name x509-distinguished-name) stream)
  (with-slots (common-name organizational-unit-name organization-name country-name) distinguished-name
    (print-unreadable-object (distinguished-name stream :type t :identity t)
      (when (and common-name organizational-unit-name organization-name country-name)
        (write-distinguished-name-string distinguished-name stream)))
    distinguished-name))

(defparameter *x509-distinguished-name-table-size* 10
  "The size for the distinguished name table.")

(defvar *x509-distinguished-name-table* nil)

(defun x509-distinguished-name-table ()
  (or *x509-distinguished-name-table*
      (setq *x509-distinguished-name-table* (make-hash-table :test #'equal :size *x509-distinguished-name-table-size*))))

(defmacro x509-distinguished-name-hash-key (common-name organizational-unit-name organization-name country-name)
  `(list ,common-name ,organizational-unit-name ,organization-name ,country-name))

(defun get-x509-distinguished-name (common-name organizational-unit-name organization-name country-name)
  (let ((key (x509-distinguished-name-hash-key common-name organizational-unit-name organization-name country-name)))
    (declare (dynamic-extent key))
    (gethash key (x509-distinguished-name-table))))

(defun intern-x509-distinguished-name (common-name organizational-unit-name organization-name country-name &key (if-does-not-exist :error))
  "Interns distinguished-name according to the arguments: COMMON-NAME, ORGANIZATIONAL-UNIT-NAME, ORGANIZATION-NAME, COUNTRY-NAME.
IF-DOES-NOT-EXIST can be any of: 

          :SOFT       - Return NIL if the object is not found.
          :UNINTERNED - Create the object if none is found but do not intern it.
          :CREATE     - Create the object if none exists
          :ERROR      - Signal an error if the object is not found."
  (declare (values x509-distinguished-name newly-created-p))
  (flet ((create-x509-distinguished-name (common-name organizational-unit-name organization-name country-name)
           (make-instance 'x509-distinguished-name
                          :common-name common-name
                          :organizational-unit-name organizational-unit-name 
                          :organization-name organization-name
                          :country-name country-name)))
    (or (get-x509-distinguished-name common-name organizational-unit-name organization-name country-name)
        (ecase if-does-not-exist
          (:soft nil)
          (:uninterned
           (values (create-x509-distinguished-name common-name organizational-unit-name organization-name country-name)
                   :uninterned))
          (:create
           (let ((key (x509-distinguished-name-hash-key common-name organizational-unit-name organization-name country-name))
                 (dn (create-x509-distinguished-name common-name organizational-unit-name organization-name country-name)))
             (setf (gethash key (x509-distinguished-name-table)) dn)
             (values dn t)))
          (:error
           (error "No distinguished name found corresponding to: ~A/~A/~A/~A" country-name organization-name organizational-unit-name common-name))))))

(defmethod unintern-x509-distinguished-name ((distinguished-name x509-distinguished-name))
  "Uninterns distinguished-name."
  (with-slots (common-name organizational-unit-name organization-name country-name) distinguished-name
    (let ((key (x509-distinguished-name-hash-key common-name organizational-unit-name organization-name country-name)))
      (declare (dynamic-extent key))
      (remhash key (x509-distinguished-name-table)))))

(defmethod x509-distinguished-name-string ((distinguished-name x509-distinguished-name) &optional recompute-p)
  (with-slots (name-string common-name organizational-unit-name organization-name country-name) distinguished-name
    (cond ((and (not recompute-p) name-string))
          (t (setq name-string (concatenate 'string country-name "/" organization-name "/" organizational-unit-name "/" common-name))))))

(defmethod write-distinguished-name-string ((distinguished-name x509-distinguished-name) &optional (stream *standard-output*) (escape t))
  "Writes the Distinguished Name-String for DISTINGUISHED-NAME on STREAM."
  (flet ((write-dn (distinguished-name stream)
           (with-slots (name-string common-name organizational-unit-name organization-name country-name) distinguished-name
             (cond (name-string
                    (write-string name-string stream))
                   (t (write-string country-name stream)
                      (write-char #\/ stream)
                      (write-string organization-name stream)
                      (write-char #\/ stream)
                      (write-string organizational-unit-name stream)
                      (write-char #\/ stream)
                      (write-string common-name stream))))))
    (cond (escape
           (write-char #\" stream)
           (write-dn distinguished-name stream)
           (write-char #\" stream))
          (t (write-dn distinguished-name stream)))
    distinguished-name))

;;;------------------------------------------------------------------- 
;;;
;;; X.509 CERTIFACTE OPERATIONS
;;;

(defmethod x509-netscape-certificate-type ((x509-certificate x509-certificate) &optional recache-p)
  "Returns a list of keywords representing the Netscape type of X509-CERTIFICATE.  
Returned list will contain zero or more of the follwoing keywords:

   :SSL-CLIENT, :SSL-SERVER, :SMIME, :OBJSIGN, :SSL-CA, :SMIME-CA, :OBJSIGN-CA

The keyword list is based on decoding the information in the `Netscape Cert Type' 
extension (NID 71))."
  (declare (values keywords))
  (with-x509-value-cached (x509-certificate :x509-netscape-certificate-type :recompute-p recache-p)
    (comm::x509-netscape-certificate-type (%x509-pointer x509-certificate))))

(defmethod x509-extended-key-usage ((x509-certificate x509-certificate) &optional recache-p)
  "Returns a list of keywords representing the extended usage of X509-CERTIFICATE. The returned list
will contain zero or more of the follwoing keywords:

  :SSL-CLIENT, :SSL-SERVER, :SMIME, :CODE-SIGN, :SGC, :OCSP-SIGN, :TIMESTAMP, :DVCS

The list is based on decoding the information in the `X509v3 Extended Key Usage' extension (NID 126))"
  (declare (values keywords))
  (with-x509-value-cached (x509-certificate :x509-extended-key-usage :recompute-p recache-p)
    (comm::x509-extended-key-usage (%x509-pointer x509-certificate))))

(defmethod x509-key-usage ((x509-certificate x509-certificate) &optional recache-p)
  "Returns a list of keywords representing the usage of X509-CERTIFICATE.  
The returned list will contain zero or more of the following keywords:

  :DIGITAL-SIGNATURE, :NON-REPUDIATION, :KEY-ENCIPHERMENT, :DATA-ENCIPHERMENT,
  :KEY-AGREEMENT, :KEY-CERT-SIGN, :CRL-SIGN, :ENCIPHER-ONLY, :DECIPER-ONLY

The list is based on decoding the information in the `X509v3 Key Usage' extension (NID 83)."
  (declare (values keywords))
  (with-x509-value-cached (x509-certificate :x509-key-usage :recompute-p recache-p)
    (comm::x509-key-usage (%x509-pointer x509-certificate))))

(defmethod x509-purpose-valid-p ((x509-certificate x509-certificate) purpose &optional certificate-authority-required-p)
  "Checks the validity of x509-certificate for a given purpose, PURPOSE, assuming CERTIFICATE-AUTHORITY-REQUIRED-P.
PURPOSE can be either be a integer denoting a defined purpose in OpenSSL, or one of the following
keywords:

   :SSL-CLIENT, :SSL-SERVER, :NS-SSL-SERVER, :SMIME-SIGN, :SMIME-ENCRYPT
   :CRL-SIGN, :ANY, :OCSP-HELPER

If PURPOSE specifies an invalid purpose, an error is signalled.

CERTIFICATE-AUTHORITY-REQUIRED-P specifies wheher or not x509-certificate must have a certificate authority.

The returned values are one of:
 
             T - X509-CERTIFICATE is valid for PURPOSE. 
           NIL - X509-CERTIFICATE is not valid for PURPOSE. 
   :ACCEPTABLE - X509-CERTIFICATE is valid for PURPOSE, but it is not really well-formed."
  (declare (values validity))
  (comm::x509-purpose-valid-p (%x509-pointer x509-certificate) purpose certificate-authority-required-p))

(defmethod x509-path-length ((x509-certificate x509-certificate) &optional recache-p)
  "Returns the pathlen of X509-CERTIFICATE."
  (declare (values path-length))
  (with-x509-value-cached (x509-certificate :x509-path-len :recompute-p recache-p)
    (comm::x509-path-len (%x509-pointer x509-certificate))))

(defmethod x509-sha-digest ((x509-certificate x509-certificate) &optional recache-p)
  "Returns an SHA1 digest of X509-CERTIFICATE as a vector."
  (declare (values sha-vector))
  (with-x509-value-cached (x509-certificate :x509-sha-digest :recompute-p recache-p)
    (comm::x509-sha-digest (%x509-pointer x509-certificate))))

(defmethod x509-signature-algorithm ((x509-certificate x509-certificate) &optional recache-p)
  "Returns the string denoting the signature algorithm for X509-CERTIFICATE as a string."
  (declare (values signature-string))
  (with-x509-value-cached (x509-certificate :x509-signature-algorithm :recompute-p recache-p)
    (comm::x509-signature-algorithm (%x509-pointer x509-certificate))))

;; comm::X509-SIGNATURE-ALGORITHM-NID

(defmethod x509-signature ((x509-certificate x509-certificate) &optional recache-p)
  "Returns the signature for X509-CERTIFICATE as an array of (unisged-byte 8)."
  (declare (values signature-string))
  (with-x509-value-cached (x509-certificate :x509-signature :recompute-p recache-p)
    (comm::x509-signature-value (%x509-pointer x509-certificate))))

(defmethod x509-signature-bits ((x509-certificate x509-certificate) &optional recache-p)
  "Returns the number of bits used in the signature for X509-CERTIFICATE."
  (declare (values integer))
  (with-x509-value-cached (x509-certificate :x509-signature-bits :recompute-p recache-p)
    (* 8 (the fixnum (length (x509-signature x509-certificate recache-p))))))

(defmethod x509-public-key-algorithm ((x509-certificate x509-certificate) &optional recache-p)
  "Returns the algorithm of the public key for X509-CERTIFICATE as a string."
  (declare (values algorithm-string))
  (with-x509-value-cached (x509-certificate :x509-pubkey-algorithm :recompute-p recache-p)
    (comm::x509-pubkey-algorithm (%x509-pointer x509-certificate))))

(defmethod x509-public-key ((x509-certificate x509-certificate) &optional recache-p)
  "Returns the public key for X509-CERTIFICATE as vector of (unsiged-byte 8)."
  (declare (values vector))
  (with-x509-value-cached (x509-certificate :x509-pubkey-key :recompute-p recache-p)
    (comm::x509-pubkey-key (%x509-pointer x509-certificate))))

(defmethod x509-public-key-bits ((x509-certificate x509-certificate) &optional recache-p)
  "Returns the number of bits used in the public key for X509-CERTIFICATE."
  (declare (values vector))
  (with-x509-value-cached (x509-certificate :x509-public-key-bits :recompute-p recache-p)
    (* 8 (the fixnum (length (x509-public-key x509-certificate))))))

;;X509-PUBKEY-ALGORITHM-NID

(defmethod x509-certificate-authority-p ((x509-certificate x509-certificate) &optional recache-p)
  "Returns non-null if X509-CERTIFICATE has a certificate authority."
  (declare (values boolean))
  (with-x509-value-cached (x509-certificate :x509-ca :recompute-p recache-p)
    (comm::x509-ca (%x509-pointer x509-certificate))))

(defmethod x509-version ((x509-certificate x509-certificate) &optional recache-p)
  "Return the X509 version of X509-CERTIFICATE as an integer."
  (declare (values version-number))
  (with-x509-value-cached (x509-certificate :x509-version :recompute-p recache-p)
    (comm::x509-version (%x509-pointer x509-certificate))))

(defmethod x509-serial-number ((x509-certificate x509-certificate) &optional recache-p)
  "Return the serial number of X509-CERTIFICATE as an integer."
  (declare (values serial-number))
  (with-x509-value-cached (x509-certificate :x509-serial-number :recompute-p recache-p)
    (comm::x509-serial-number (%x509-pointer x509-certificate))))

(defmethod x509-email-addresses ((x509-certificate x509-certificate) &optional recache-p)
  "Returns a list of email addresses associated with the subject of X509-CERTIFICATE."
  (declare (values signature-string))
  (with-x509-value-cached (x509-certificate :x509-get-email :recompute-p recache-p)
    (comm::x509-get-email (%x509-pointer x509-certificate))))

(defun %generalized-time-to-universal-time-gmt (generalized-time)
  (let ((universal-time (comm::generalized-time-universal-time generalized-time))
        (gmt-offset (comm::generalized-time-gmtoffset generalized-time)))
    (declare (integer universal-time))
    (cond ((member gmt-offset '(:gmt nil))
           universal-time)
          (t (+ (the integer gmt-offset) universal-time)))))

(defmethod x509-issue-time ((x509-certificate x509-certificate) &optional recache-p)
  "Return the issue time of X509-CERTIFICATE as universal time."
  (declare (values universal-time))
  (with-x509-value-cached (x509-certificate :x509-issue-time :recompute-p recache-p)
    (%generalized-time-to-universal-time-gmt (comm::x509-starting-time (%x509-pointer x509-certificate)))))

(defmethod x509-expiration-time ((x509-certificate x509-certificate) &optional recache-p)
  "Return the expiration time of X509-CERTIFICATE as universal time."
  (declare (values universal-time))
  (with-x509-value-cached (x509-certificate :x509-expiration-time :recompute-p recache-p)
    (%generalized-time-to-universal-time-gmt (comm::x509-expiry-time (%x509-pointer x509-certificate)))))

(defmethod x509-subject-name-alist ((x509-certificate x509-certificate) &optional recache-p)
  "Return the subject name entires for X509-CERTIFICATE as an alist keyed by field string."
  (declare (values alist))
  (with-x509-value-cached (x509-certificate :x509-subject-name-alist :recompute-p recache-p)
    (let ((subject-name-pointer (comm::x509-get-subject-name (%x509-pointer x509-certificate))))
      (comm::x509-name-get-all-values subject-name-pointer))))

(defmethod x509-subject-nid-alist ((x509-certificate x509-certificate) &optional recache-p)
  "Return the subject name entires for X509-CERTIFICATE as an alist key by NID."
  (declare (values alist))
  (with-x509-value-cached (x509-certificate :x509-subject-nid-alist :recompute-p recache-p)
    (let ((subject-name-pointer (comm::x509-get-subject-name (%x509-pointer x509-certificate))))
      (comm::x509-name-get-all-values-nids subject-name-pointer))))  

(defmethod x509-issuer-name-alist ((x509-certificate x509-certificate) &optional recache-p)
  "Return the issuer name entries for X509-CERTIFICATE as an alist keyed by field string."
  (declare (values alist))
  (with-x509-value-cached (x509-certificate :x509-issuer-name-alist :recompute-p recache-p)
    (let ((issuer-name-pointer (comm::x509-get-issuer-name (%x509-pointer x509-certificate))))
      (comm::x509-name-get-all-values issuer-name-pointer))))

(defmethod x509-issuer-nid-alist ((x509-certificate x509-certificate) &optional recache-p)
  "Return the issuer name entries for X509-CERTIFICATE as an alist keyed by NID."
  (declare (values alist))
  (with-x509-value-cached (x509-certificate :x509-issuer-nid-alist :recompute-p recache-p)
    (let ((issuer-name-pointer (comm::x509-get-issuer-name (%x509-pointer x509-certificate))))
      (comm::x509-name-get-all-values-nids issuer-name-pointer))))

(defun %x509-get-subject-name-field-string (x509-pointer nid)
  (let ((subject-name-pointer (comm::x509-get-subject-name x509-pointer)))
    (comm::x509-name-field-string subject-name-pointer nid)))

(defmethod x509-email-address ((x509-certificate x509-certificate) &optional recache-p)
  "Returns the Email Address for X509-CERTIFICATE"
  (declare (values common-name-string))
  (with-x509-value-cached (x509-certificate :x509-email-address :recompute-p recache-p)
    (%x509-get-subject-name-field-string (%x509-pointer x509-certificate) #.(comm::convert-to-nid "emailAddress"))))

(defmethod x509-common-name ((x509-certificate x509-certificate) &optional recache-p)
  "Returns the Common Name for X509-CERTIFICATE"
  (declare (values common-name-string))
  (with-x509-value-cached (x509-certificate :x509-common-name :recompute-p recache-p)
    (%x509-get-subject-name-field-string (%x509-pointer x509-certificate) #.(comm::convert-to-nid "commonName"))))

(defmethod x509-organizational-unit-name ((x509-certificate x509-certificate) &optional recache-p)
  "Returns the Organizational Unit Name for X509-CERTIFICATE"
  (declare (values common-name-string))
  (with-x509-value-cached (x509-certificate :x509-organizational-unit-name :recompute-p recache-p)
    (%x509-get-subject-name-field-string (%x509-pointer x509-certificate) #.(comm::convert-to-nid "organizationalUnitName"))))

(defmethod x509-organization-name ((x509-certificate x509-certificate) &optional recache-p)
  "Returns the Organization Name for X509-CERTIFICATE"
  (declare (values common-name-string))
  (with-x509-value-cached (x509-certificate :x509-organization-name :recompute-p recache-p)
    (%x509-get-subject-name-field-string (%x509-pointer x509-certificate) #.(comm::convert-to-nid "organizationName"))))

(defmethod x509-locality-name ((x509-certificate x509-certificate) &optional recache-p)
  "Returns the Locality Name for X509-CERTIFICATE"
  (declare (values common-name-string))
  (with-x509-value-cached (x509-certificate :x509-locality-name :recompute-p recache-p)
    (%x509-get-subject-name-field-string (%x509-pointer x509-certificate) #.(comm::convert-to-nid "localityName"))))

(defmethod x509-state-or-province-name ((x509-certificate x509-certificate) &optional recache-p)
  "Returns the State or Province Name for X509-CERTIFICATE"
  (declare (values common-name-string))
  (with-x509-value-cached (x509-certificate :x509-state-or-province-name :recompute-p recache-p)
    (%x509-get-subject-name-field-string (%x509-pointer x509-certificate) #.(comm::convert-to-nid "stateOrProvinceName"))))

(defmethod x509-country-name ((x509-certificate x509-certificate) &optional recache-p)
  "Returns the Country Name for X509-CERTIFICATE"
  (declare (values common-name-string))
  (with-x509-value-cached (x509-certificate :x509-country-name :recompute-p recache-p)
    (%x509-get-subject-name-field-string (%x509-pointer x509-certificate) #.(comm::convert-to-nid "countryName"))))

(defmethod x509-distinguished-name ((x509-certificate x509-certificate) &optional recache-p)
  "Returns the issuer Distinguished Name for X509-CERTIFICATE"
  (declare (values x509-distinguished-name))
  (with-x509-value-cached (x509-certificate :x509-distinguished-name :recompute-p recache-p)
    (intern-x509-distinguished-name (x509-common-name x509-certificate)
                                    (x509-organizational-unit-name x509-certificate)
                                    (x509-organization-name x509-certificate)
                                    (x509-country-name x509-certificate)
                                    :if-does-not-exist :uninterned)))

(defmethod x509-distinguished-name-string ((x509-certificate x509-certificate) &optional recache-p)
  "Returns the Distinguished Name-String for X509-CERTIFICATE"
  (x509-distinguished-name-string (x509-distinguished-name x509-certificate recache-p) recache-p))

(defmethod write-distinguished-name-string ((x509-certificate x509-certificate) &optional  (stream *standard-output*) (escape t))
  "Writes the Distinguished Name-String for X509-CERTIFICATE on STREAM."
  (write-distinguished-name-string (x509-distinguished-name x509-certificate) stream escape))

;;;------------------------------------------------------------------- 
;;;
;;; X.509 ISSUER CERTIFACTE OPERATIONS
;;;

(defun %x509-get-issuer-name-field-string (x509-pointer nid)
  (let ((issuer-name-pointer (comm::x509-get-issuer-name x509-pointer)))
    (comm::x509-name-field-string issuer-name-pointer nid)))

(defmethod x509-issuer-email-address ((x509-certificate x509-certificate) &optional recache-p)
  "Returns the issuer Email Address for X509-CERTIFICATE"
  (declare (values common-name-string))
  (with-x509-value-cached (x509-certificate :x509-issuer-email-address :recompute-p recache-p)
    (%x509-get-issuer-name-field-string (%x509-pointer x509-certificate) #.(comm::convert-to-nid "emailAddress"))))

(defmethod x509-issuer-common-name ((x509-certificate x509-certificate) &optional recache-p)
  "Returns the issuer Common Name for X509-CERTIFICATE"
  (declare (values common-name-string))
  (with-x509-value-cached (x509-certificate :x509-issuer-common-name :recompute-p recache-p)
    (%x509-get-issuer-name-field-string (%x509-pointer x509-certificate) #.(comm::convert-to-nid "commonName"))))

(defmethod x509-issuer-organizational-unit-name ((x509-certificate x509-certificate) &optional recache-p)
  "Returns the issuer Organizational Unit Name for X509-CERTIFICATE"
  (declare (values common-name-string))
  (with-x509-value-cached (x509-certificate :x509-issuer-organizational-unit-name :recompute-p recache-p)
    (%x509-get-issuer-name-field-string (%x509-pointer x509-certificate) #.(comm::convert-to-nid "organizationalUnitName"))))

(defmethod x509-issuer-organization-name ((x509-certificate x509-certificate) &optional recache-p)
  "Returns the issuer Organization Name for X509-CERTIFICATE"
  (declare (values common-name-string))
  (with-x509-value-cached (x509-certificate :x509-issuer-organization-name :recompute-p recache-p)
    (%x509-get-issuer-name-field-string (%x509-pointer x509-certificate) #.(comm::convert-to-nid "organizationName"))))

(defmethod x509-issuer-locality-name ((x509-certificate x509-certificate) &optional recache-p)
  "Returns the issuer Locality Name for X509-CERTIFICATE"
  (declare (values common-name-string))
  (with-x509-value-cached (x509-certificate :x509-issuer-locality-name :recompute-p recache-p)
    (%x509-get-issuer-name-field-string (%x509-pointer x509-certificate) #.(comm::convert-to-nid "localityName"))))

(defmethod x509-issuer-state-or-province-name ((x509-certificate x509-certificate) &optional recache-p)
  "Returns the issuer State or Province Name for X509-CERTIFICATE"
  (declare (values common-name-string))
  (with-x509-value-cached (x509-certificate :x509-issuer-state-or-province-name :recompute-p recache-p)
    (%x509-get-issuer-name-field-string (%x509-pointer x509-certificate) #.(comm::convert-to-nid "stateOrProvinceName"))))

(defmethod x509-issuer-country-name ((x509-certificate x509-certificate) &optional recache-p)
  "Returns the issuer Country Name for X509-CERTIFICATE"
  (declare (values common-name-string))
  (with-x509-value-cached (x509-certificate :x509-issuer-country-name :recompute-p recache-p)
    (%x509-get-issuer-name-field-string (%x509-pointer x509-certificate) #.(comm::convert-to-nid "countryName"))))

(defmethod x509-issuer-distinguished-name ((x509-certificate x509-certificate) &optional recache-p)
  "Returns the issuer Distinguished Name for X509-CERTIFICATE"
  (declare (values common-name-string))
  (with-x509-value-cached (x509-certificate :x509-issuer-distinguished-name :recompute-p recache-p)
    (intern-x509-distinguished-name (x509-issuer-common-name x509-certificate)
                                    (x509-issuer-organizational-unit-name x509-certificate)
                                    (x509-issuer-organization-name x509-certificate)
                                    (x509-issuer-country-name x509-certificate)
                                    :if-does-not-exist :uninterned)))

(defmethod x509-issuer-distinguished-name-string ((x509-certificate x509-certificate) &optional recache-p)
  "Returns the issuer Distinguished Name-String for X509-CERTIFICATE"
   (x509-distinguished-name-string (x509-issuer-distinguished-name x509-certificate recache-p) recache-p))

(defmethod write-issuer-distinguished-name-string ((x509-certificate x509-certificate) &optional  (stream *standard-output*) (escape t))
  "Writes the issuer Distinguished Name-String for X509-CERTIFICATE on STREAM."
  (write-distinguished-name-string (x509-issuer-distinguished-name x509-certificate) stream escape))

;; This must be last form in the file following all calls to WITH-VALUE-CACHED
;; Remember the cache keys for when we load the file.
(eval-when (:load-toplevel)
  (remember-x509-cache-keys))

;;;------------------------------------------------------------------- 
;;;
;;; X.509 HIGHER LEVEL OPERATIONS
;;;

(defun read-certificate-file (pathname &key password (error-p t) (default #P"http:pw;ssl;") (error-title "Error Reading Certificates"))
  "Reads certificates from PATHNAME merged against DEFAULT and returns a list of X.509 objects.
If PATHNAME is encrypted, PASSWORD must be supplied.  All X.509 certificate objects created with
this need to be deallocated with DEINITIALIZE-X509-CERTIFICATE"
  (loop for x509-pointer in (comm:read-certificate-file (ssl-filename pathname default error-title)
                                                        :pass-phrase password :errorp error-p)
        collect (allocate-x509-certificate x509-pointer t)))
