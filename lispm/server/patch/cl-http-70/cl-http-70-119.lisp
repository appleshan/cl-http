;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Patch-File: T -*-
;;; Patch file for CL-HTTP version 70.119
;;; Reason: DEFINE-HEADER :PROXY-AUTHORIZATION:  new header.
;;; DEFINE-HEADER :PROXY-AUTHENTICATE:  new
;;; DEFINE-CONDITION HTTP::RECOVERABLE-UNAUTHORIZED-PROXY-ACCESS:  new.
;;; DEFINE-CONDITION HTTP::PROXY-ACCESS-WITH-STALE-NONCE:  new.
;;; DEFINE-CONDITION HTTP::RECOVERABLE-UNAUTHORIZED-ACCESS:  -
;;; DEFINE-CONDITION HTTP::ACCESS-WITH-STALE-NONCE:  -
;;; DEFINE-CONDITION HTTP::RECOVERABLE-UNAUTHORIZED-CLIENT-ACCESS:  -
;;; DEFINE-CONDITION HTTP::CLIENT-ACCESS-WITH-STALE-NONCE:  -
;;; DEFINE-CONDITION HTTP::RECOVERABLE-UNAUTHORIZED-PROXY-ACCESS:  -
;;; DEFINE-CONDITION HTTP::PROXY-ACCESS-WITH-STALE-NONCE:  -
;;; Remove function (CLOS:METHOD HTTP::REPORT-STATUS (HTTP::RECOVERABLE-UNAUTHORIZED-CLIENT-ACCESS T)): undefine.
;;; Function (CLOS:METHOD HTTP::REPORT-STATUS (HTTP::RECOVERABLE-UNAUTHORIZED-ACCESS T)):  move to higher class.
;;; Function HTTP::PROXY-AUTHENTICATE-HEADER-SPEC:  New
;;; Function (CLOS:METHOD HTTP::PROXY-AUTHENTICATE-HEADER-SPEC (HTTP::BASIC-REALM-MIXIN SYMBOL)):  -
;;; Function (CLOS:METHOD HTTP::PROXY-AUTHENTICATE-HEADER-SPEC (HTTP::DIGEST-REALM-MIXIN SYMBOL)):  -
;;; Function (CLOS:METHOD HTTP::AUTHENTICATION-HEADER-SPEC (HTTP::RECOVERABLE-UNAUTHORIZED-PROXY-ACCESS)):  -
;;; Function (CLOS:METHOD HTTP::AUTHENTICATION-HEADER-SPEC (HTTP::PROXY-ACCESS-WITH-STALE-NONCE)):  -
;;; Remove function HTTP::AUTHENTICATE-USER-P: undefine.
;;; Function HTTP::AUTHENTICATE-USER-P:  add case argument.
;;; Function (CLOS:METHOD HTTP::AUTHENTICATE-USER-P (HTTP::BASIC-USER-MIXIN T T T)):  ditto.
;;; Function (CLOS:METHOD HTTP::AUTHENTICATE-USER-P (HTTP::DIGEST-USER-MIXIN T T T)):  ditto.
;;; Remove function HTTP::%AUTHENTICATE-USER: undefine.
;;; Function HTTP::%AUTHENTICATE-USER:  add case arg.
;;; Function (CLOS:METHOD HTTP::%AUTHENTICATE-USER (HTTP::BASIC-REALM-MIXIN T T T)):  ditto.
;;; Function (CLOS:METHOD HTTP::%AUTHENTICATE-USER (HTTP::DIGEST-REALM-MIXIN T T T)):  ditto.
;;; Function HTTP::AUTHENTICATE-USER:  add case arg.
;;; Function HTTP::WITH-AUTHENTICATION-ACCESS-CONTROL:  pass down :client-access value for case arg.
;;; Written by JCMa, 4/10/01 14:18:01
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.118,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.26, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, HTTP Proxy Server 6.18,
;;; HTTP Client Substrate 4.9, Statice Server 466.2, HTTP Client 50.6,
;;; Image Substrate 440.4, Essential Image Substrate 433.0, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x976 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number -2141189585,
;;; Domain Fixes (from CML:MAILER;DOMAIN-FIXES.LISP.33),
;;; Don't force in the mail-x host (from CML:MAILER;MAILBOX-FORMAT.LISP.24),
;;; Make Mailer More Robust (from CML:MAILER;MAILER-FIXES.LISP.15),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add CLIM presentation and text style format directives. (from FV:SCLC;FORMAT.LISP.20),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; Make update schema work on set-value attributes with accessor names (from CML:LISPM;STATICE-SET-VALUED-UPDATE.LISP.1),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.107),
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.7),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;HEADERS.LISP.489"
  "HTTP:SERVER;HTTP-CONDITIONS.LISP.188"
  "HTTP:SERVER;AUTHENTICATION.LISP.150"
  "HTTP:SERVER;REPORT.LISP.184"
  "HTTP:SERVER;AUTHENTICATION.LISP.151")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.489")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :proxy-authorization                   ;RFC 2069
               (:authentication-header :request)
  :print-string "Proxy-Authorization"
  :parse-function 'parse-authorization-header
  :print-function 'print-authorization-header)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.489")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :proxy-authenticate                ;RFC 2069
               (:authentication-header :response)
  :print-string "Proxy-Authenticate"
  :parse-function 'parse-www-authenticate-header
  :print-function 'print-www-authenticate-header)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.188")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition recoverable-unauthorized-access () ())


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.188")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition access-with-stale-nonce () ())


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.188")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition recoverable-unauthorized-client-access
                  (recoverable-unauthorized-access unauthorized-client-access)
  ((authentication-method :initarg :authentication-method :reader http-authentication-method)
   (authentication-realm :initarg :authentication-realm :reader http-authentication-realm)
   (status-code :initform 401)
   (reason :initform "Unauthorized")))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.188")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition client-access-with-stale-nonce (access-with-stale-nonce recoverable-unauthorized-client-access) ())


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.188")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition recoverable-unauthorized-proxy-access
                  (recoverable-unauthorized-access unauthorized-proxy-access)
  ((authentication-method :initarg :authentication-method :reader http-authentication-method)
   (authentication-realm :initarg :authentication-realm :reader http-authentication-realm)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.188")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition proxy-access-with-stale-nonce (access-with-stale-nonce recoverable-unauthorized-proxy-access) ())

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;REPORT.LISP.184")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(SCL:FUNDEFINE '(METHOD REPORT-STATUS (RECOVERABLE-UNAUTHORIZED-CLIENT-ACCESS T)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;REPORT.LISP.184")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(defmethod report-status ((condition recoverable-unauthorized-access) stream)
  (let* ((more-headers (authentication-header-spec condition))
         (server *server*)
         (method (or (server-method server) (http-method condition))))
    (declare (dynamic-extent more-headers))
    (case method
      (:head ;; head redirects never send a body
        (report-status-line condition stream)
       (report-http-headers condition stream t more-headers nil))
      (t (cond ((client-http-version-meets-p server :http/1.1)
                (%with-chunked-transfer-encoding
                  (stream)
                  (progn (report-status-line condition stream)
                         (report-http-headers condition stream nil more-headers))
                  (report-status-message condition stream)))
               (t (case method                  ;close connection for 1.0 puts
                    (:put (setf (server-close-connection-p server) t)))
                  (report-status-line condition stream)
                  (report-http-headers condition stream t more-headers)
                  (report-status-message condition stream)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;AUTHENTICATION.LISP.150")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod proxy-authenticate-header-spec ((realm basic-realm-mixin) (method symbol) &rest args)
  (declare (ignore args))
  `(:proxy-authenticate (,method ,(realm-name realm))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;AUTHENTICATION.LISP.150")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod proxy-authenticate-header-spec ((realm digest-realm-mixin) (method symbol) &rest args)
  `(:proxy-authenticate (,method ,(realm-name realm) :algorithm ,(digest-realm-algorithm realm) ,.args)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;AUTHENTICATION.LISP.150")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(define-generic proxy-authenticate-header-spec (realm authentication-method &rest keyword-value-pairs)
  (:documentation "Returns a PROXY-AUTHENTICATE plist suitable for writing the header over HTTP."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;REPORT.LISP.184")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(defmethod authentication-header-spec ((condition recoverable-unauthorized-proxy-access))
  (proxy-authenticate-header-spec (http-authentication-realm condition) (http-authentication-method condition)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;REPORT.LISP.184")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(defmethod authentication-header-spec ((condition proxy-access-with-stale-nonce))
  (proxy-authenticate-header-spec (http-authentication-realm condition) (http-authentication-method condition) :stale t))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;AUTHENTICATION.LISP.151")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(SCL:FUNDEFINE 'AUTHENTICATE-USER-P)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;AUTHENTICATION.LISP.151")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod authenticate-user-p ((user basic-user-mixin) digest method case)
  (declare (values user-or-null)
           (ignore method case))
  (if (md5:md5-with-temporary-digest-string
	(equalp (md5:md5-digest-hexadecimal-string digest) (user-password-digest user)))
      user
      nil))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;AUTHENTICATION.LISP.151")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod authenticate-user-p ((user digest-user-mixin) authorization http-method case)
  (destructuring-bind (&key nonce opaque response uri &allow-other-keys) (cdr authorization)
    (cond 
      ((accept-nonce-value-p nonce opaque)
       ;; use the method and url from the http request rather than from the authorization header
       (let* ((realm (user-realm user))
              (digest-fctn (digest-realm-digest-function realm))
              (uri-method (concatenate 'string (symbol-name http-method) ":" (server-relative-url-string *server*)))
              (uri-method-digest (funcall digest-fctn uri-method))
              (user-nonce-uri-method (concatenate 'string (user-password-digest user) ":" nonce ":" uri-method-digest))
              (user-nonce-uri-method-digest (funcall digest-fctn user-nonce-uri-method)))
         (declare (dynamic-extent uri-method uri-method-digest user-nonce-uri-method user-nonce-uri-method-digest))
         (if (equalp response user-nonce-uri-method-digest)
             user
             nil)))
      (t (let ((realm (user-realm user)))
           (error (ecase case
		    (:client-access 'client-access-with-stale-nonce)
		    (:proxy-access 'proxy-access-with-stale-nonce))
		  :method http-method :url uri
                  :authentication-method  (realm-scheme realm)
                  :authentication-realm realm))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;AUTHENTICATION.LISP.151")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(SCL:FUNDEFINE '%AUTHENTICATE-USER)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;AUTHENTICATION.LISP.151")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defgeneric %authenticate-user (realm authorization method case)
  (declare (values user-object-or-null))
  (:documentation "Verify a user in a realm and returns the user object or null."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;AUTHENTICATION.LISP.151")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod %authenticate-user ((realm basic-realm-mixin) authorization method case)
  "Checks the basic authentication authorization and returns the user object if valid."
  (declare (values user-object-or-null))
  (when authorization
    (destructuring-bind (authentication-method cookie &rest args) authorization
      (declare (ignore args))
      (case authentication-method
        (:basic
          (let* ((decoded-authorization (base64:base64-decode-vector cookie :decoded-byte-type *standard-character-type*))
                 (username (subseq decoded-authorization 0 (position #\: decoded-authorization :test #'char-equal)))
                 (user (%realm-get-user realm username)))
            (declare (dynamic-extent decoded-authorization username))
            (when user
              (authenticate-user-p user decoded-authorization method case))))
        (t nil)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;AUTHENTICATION.LISP.151")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod %authenticate-user ((realm digest-realm-mixin) authorization http-method case)
  "Checks the digest authentication authorization and returns the user object if valid."
  (declare (values user-object-or-null))
  (and authorization
       (destructuring-bind (authentication-method . plist) authorization
         (case authentication-method
           (:digest
             (let* ((username (getf plist :username))
                    (user (%realm-get-user realm username)))
               (and user
                    (authenticate-user-p user authorization http-method case))))
           (t nil)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;AUTHENTICATION.LISP.151")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(define-generic authenticate-user-p (user digest http-method case)
  (declare (values user-object-or-null))
  (:documentation "Returns the user object if the DIGEST matches the USER for HTTP-METHOD, otherwise returns NIL.
CASE is either :CLIENT-ACCESS or :PROXY-ACCESS"))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;AUTHENTICATION.LISP.151")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun authenticate-user (realm authorization &optional http-method (case :client-access))
  "Authenticate user in a REALM based on authorization.
Returns an authenicated user object or NIL if authentication not possible.
CASE is either :CLIENT-ACCESS or :PROXY-ACCESS"
  (declare (values user-object-or-null))
  (let ((realm (intern-realm realm :if-does-not-exist :soft)))
    (and realm
         (%authenticate-user realm authorization http-method case))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;AUTHENTICATION.LISP.151")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(define-macro with-authentication-access-control ((url method authorization realm &key rejection-form
                                                       require-capabilities) &body body)
  "Executes REJECTION-FORM whenever AUTHORIZATION does not qualify for CAPABILITIES under REALM,
Otherwise executes BODY. If REQUIRE-CAPABILITIES is non-null, all accesses are
rejected whenever CAPABILITIES is null"
  `(cond (,realm
          (handler-case
            (let ((user (authenticate-user ,realm ,authorization ,method :client-access)))
              (cond ((and user
                          (let ((capabilities (url:capabilities ,url)))
                            ,(if require-capabilities
                                 `(and capabilities (allow-user-access-p capabilities user ,method))
                                 `(or (null capabilities) (allow-user-access-p capabilities user ,method)))))
                     (setf (server-user-object *server*) user)
                     ,@body)
                    (t ,rejection-form)))
            (unknown-authentication-method () ,rejection-form)))
         (t ,@body)))

