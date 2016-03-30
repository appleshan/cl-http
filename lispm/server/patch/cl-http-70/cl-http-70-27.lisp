;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.27
;;; Reason: This patch divides secure-subnets into read subnets and write subnets and
;;; implements the distinction for both local and global URL access control.
;;; SECURE-SUBNETS is defined as the union of read-subnets and write-subnets.
;;; WRITE-SUBNETS are uses to explicitly specify the subnets that can PUT and
;;; DELETE resources. SECURE-SUBNETS continues to be used for HEAD, GET, and POST.
;;; The macro DEFINE-SECURE-SUBNETS is now obsolete and should be replaced by
;;; calls to DEFINE-READ-SUBNETS or DEFINE-WRITE-SUBNETS. EXPORT-URL now handles
;;; the arguments :READ-SUBNETS and :WRITE-SUBNETS :SECURE-SUBNETS is obsolete and
;;; defaults to :READ-SUBNETS.
;;; 
;;; Variable HTTP::*STANDARD-LOG-DIRECTORY*:  set to the correct logical pathname.
;;; Function DEFINE-SECURE-SUBNET-TYPE: New abstraction for defining secure networks.
;;; Define-Secure-Subnet-Type URL:SECURE-SUBNETS:  use new definition method.
;;; Define-Secure-Subnet-Type HTTP::PROXY-SUBNETS:  use new definition method.
;;; Define-Secure-Subnet-Type HTTP::WRITE-SUBNETS:  new secure subnet.
;;; Define-Secure-Subnet-Type HTTP::DISALLOWED-SUBNETS:  update.
;;; Function HTTP:DEFINE-SECURE-SUBNETS:  Obsolete: Make it call PARSE-READ-SUBNETS for backward compatibility.
;;; CLOS class URL:SECURE-SUBNETS-MIXIN:  new slot write-subnets.
;;; Remove function URL:INITIALIZE-SECURE-SUBNETS: undefine.
;;; Function URL:INITIALIZE-SECURE-SUBNETS:  add write-subnets argument.
;;; Function (CLOS:METHOD URL:INITIALIZE-SECURE-SUBNETS (URL:SECURE-SUBNETS-MIXIN T T)):  implement.
;;; Function (CLOS:METHOD URL:TRUSTED-HOST-P (URL:SECURE-SUBNETS-MIXIN T)):  update.
;;; Function (CLOS:METHOD HTTP::INHERIT-EXPORT-PARAMETERS (URL:SECURE-SUBNETS-MIXIN URL:SECURE-SUBNETS-MIXIN) PROGN):  update.
;;; Function (CLOS:METHOD HTTP:EXPORT-URL (URL:SECURE-SUBNETS-MIXIN T) :BEFORE):  update.
;;; Function HTTP::ACCEPT-REMOTE-WRITE-METHOD-P:  revise security policies.
;;; Variable HTTP:*ACCEPT-WRITE-METHODS*:  document security policies.
;;; Remove function URL:TRUSTED-HOST-P: undefine.
;;; Function URL:TRUSTED-HOST-P:  add ACCESS-TYPE argument.
;;; Function (CLOS:METHOD HTTP::INVOKE-SERVER-METHOD (HTTP::BASIC-SERVER-MIXIN (EQL :DELETE) SYMBOL)):  use the write subnets.
;;; Function HTTP::%PUT-NEW-RESOURCE:  ditto.
;;; Function (CLOS:METHOD HTTP::INVOKE-SERVER-METHOD (HTTP::BASIC-SERVER-MIXIN (EQL :PUT) SYMBOL)):  ditto.
;;; Written by JCMa, 3/13/00 12:26:09
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.26,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, HTTP Proxy Server 4.5,
;;; HTTP Client Substrate 3.2, Jcma 41, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.11, DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x994 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number -2141189585,
;;; Domain Fixes (from CML:MAILER;DOMAIN-FIXES.LISP.33),
;;; Don't force in the mail-x host (from CML:MAILER;MAILBOX-FORMAT.LISP.24),
;;; Make Mailer More Robust (from CML:MAILER;MAILER-FIXES.LISP.15),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add CLIM presentation and text style format directives. (from FV:SCLC;FORMAT.LISP.20),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; Make update schema work on set-value attributes with accessor names (from CML:LISPM;STATICE-SET-VALUED-UPDATE.LISP.1),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.107),
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;VARIABLES.LISP.177"
  "HTTP:SERVER;UTILS.LISP.424"
  "HTTP:SERVER;PACKAGE.LISP.435"
  "HTTP:SERVER;UTILS.LISP.428"
  "HTTP:SERVER;URL.LISP.376"
  "HTTP:SERVER;UTILS.LISP.427"
  "HTTP:SERVER;VARIABLES.LISP.180"
  "HTTP:SERVER;URL.LISP.377"
  "HTTP:SERVER;URL-CLASS.LISP.14"
  "HTTP:SERVER;SERVER.LISP.816"
  "HTTP:SERVER;SERVER.LISP.815"
  "HTTP:SERVER;SERVER.LISP.817")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;VARIABLES.LISP.177")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp;-*-")

(define-parameter *standard-log-directory* "http:logs;"
                  "The standard directory of the server's log file.")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;PACKAGE.LISP.435")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: CL-USER; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (:compile-toplevel :execute :load-toplevel)
(export (intern "WRITE-SUBNETS" :url) :url)
(export (intern "DEFINE-WRITE-SUBNETS" :http) :http)

(export (intern "READ-SUBNETS" :url) :url)
(export (intern "DEFINE-READ-SUBNETS" :http) :http)
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL-CLASS.LISP.14")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: URL; Base: 10 -*-")

(defclass secure-subnets-mixin
          ()
    ((secure-subnets :initform nil :accessor secure-subnets)	;merge of read and write subnets
     (read-subnets :initform nil :accessor read-subnets)	;read subnets
     (write-subnets :initform nil :accessor write-subnets))	;write subnets
  (:documentation "A mixin that provide for subnet security on access to URLs."))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.424")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(eval-when (:load-toplevel :execute :compile-toplevel)
(define-macro define-secure-subnet-type (name documentation &key secure-subnets-component)
  "Defines a new secure subnet type.
Provides the function parse-<name> and the macro define-<name>."
  (let* ((variable (symbolize (concatenate 'string "*" (string name) "*") :http))
	 (parse-function-name (symbolize (format nil "PARSE-~A" name) :http))
	 (define-macro-name (symbolize (format nil "DEFINE-~A" name) :http))
	 (code `(setq ,variable (www-utils:parse-internet-addresses subnets))))
    `(progn (defparameter ,variable nil ,(string-capitalize documentation :start 0 :end 1))
	    (defun ,parse-function-name (subnets)
	      ,(format nil "Binds ~S to the parsed IP addresses in SUBNETS." variable)
	      ,(if secure-subnets-component
		   `(prog1 ,code
			   (update-secure-subnets))
		   code))
	    (defmacro ,define-macro-name (&body subnets)
	      ,(format nil "Define ~A" documentation)
	      `(,',parse-function-name ',subnets))))))

(declaim (special *read-subnets* *write-subnets*))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.424")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(let ((old-value *secure-subnets*))
(define-secure-subnet-type
  read-subnets
  "the list of trusted subnets for IP-subnet level access control."
  :secure-subnets-component t)
(set (intern "*READ-SUBNETS*" :http) old-value))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.424")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(define-secure-subnet-type
  write-subnets
  "the subnets trusted for HTTP write methods by the HTTP server on any port."
  :secure-subnets-component t)

(defun update-secure-subnets ()
  (setq *secure-subnets* (union *read-subnets* *write-subnets*)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.424")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(let ((old-value *proxy-subnets*))
(define-secure-subnet-type
  proxy-subnets
  "the subnets trusted by the HTTP server on any port.
These subnets override the secure subnets define by define-secure-subnets.
However, if proxy subnets are not provided, they default to the secure subnets.")
(setq *proxy-subnets* old-value))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.424")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(let ((old-value *disallowed-subnets*))
(define-secure-subnet-type
  disallowed-subnets
  "the list of subnets that are denied access globally.")
(setq *disallowed-subnets* old-value))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.428")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmacro define-secure-subnets (&body subnets)
  "OBSOLETE MACRO: Use DEFINE-READ-SUBNETS and DEFINE-WRITE-SUBNETS instead."
  `(parse-read-subnets ',subnets))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.376")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(SCL:FUNDEFINE 'INITIALIZE-SECURE-SUBNETS)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.376")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

;;;------------------------------------------------------------------- 
;;;
;;; ACCESS CONTROL
;;;

(define-generic initialize-secure-subnets (url read-subnets write-subnets)
  (:documentation "Initializes READ-SUBNETS as secure for read access to URL.
and WRITE-SUBNETS as secure for writing to URL."))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.427")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(define accept-remote-write-method-p (url)
  "Returns non-null if side-effecting methods like PUT and DELETE are accepted from parsed-ip-address."
  (ecase *accept-write-methods*
    (:none nil)
    (:local-host
      (let ((secure-subnets (list (local-host-parsed-ip-address))))
        (declare (dynamic-extent secure-subnets))
        (www-utils:ip-host-trusted-p (server-address *server*) secure-subnets)))
    ;; Only authenticated users on trusted subnets
    (:authenticated-users-on-write-subnets
     (and (or (url::write-subnets url) *write-subnets*)
	  (url:capabilities url)))
    ;; Anyone on a subnet trusted to write to this URL
    (:write-subnets
     (or (url::write-subnets url) *write-subnets*))
    ;; Authenticated users with the correct capabilities 
    (:authenticated-users
      (url:capabilities url))
    ;; Either authenticated users or trusted subnets
    (:access-controlled
      (or (url:capabilities url)
	  (url::write-subnets url)
	  *write-subnets*))
    ;; Anyone who can access the system
    (:remote-host t)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;VARIABLES.LISP.180")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp;-*-")

(define-parameter *accept-write-methods* :access-controlled
                  "Controls the security policy for side-effecting methods such
as PUT or DELETE.  Each security policy imposes minimum the requirements to invoke these methods.
Policies are implemented globally by setting this variable to the appropriate keyword.
The security level of each policy is indicated in increasing level of insecurity after each keyword,
assuming that the authentication method is BASIC. Use of more effective authentication methods
could mean that authentication would be more meaningful than subnet security.
The following security policies are available:

     :ACCESS-CONTROLLED (5) -- Requires URLs to restrict users via either user
     authentication or subnet security.

     :AUTHENTICATED-USERS (6) -- Requires URLs to restrict users only via user
     authentication.

     :AUTHENTICATED-USERS-ON-WRITE-SUBNETS (3) -- Requires URLs to restrict users
     via both user authentication and write subnet security.

     :LOCAL-HOST (2) -- Requires users to be on the local host running the server.

     :NONE (1) -- No users can invoke side-effecting methods.

     :REMOTE-HOST -- Does requires URLs to control access, but respects any
     global or URL level access controls.

     :WRITE-SUBNETS (4) -- Requires URLs to restrict access to hosts trusted for write operations.")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.816")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod export-url :before ((url url:secure-subnets-mixin) export-type &rest args)
  (declare (ignore export-type))
  ;; SECURE-SUBNETS is an obsolete keyword that is superseded by READ-SUBNETS 3/13/2000 -- JCMa.
  (destructuring-bind (&key read-subnets write-subnets secure-subnets &allow-other-keys) args
    (url:initialize-secure-subnets url (or read-subnets secure-subnets) write-subnets)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.377")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod initialize-secure-subnets ((url secure-subnets-mixin) read-subnets write-subnets)
  (macrolet ((initialize-subnet-slot (url slot-accessor init-value)
	       `(etypecase ,init-value
		  (null (setf (,slot-accessor ,url) nil))
		  (cons (setf (,slot-accessor ,url) (www-utils:parse-internet-addresses ,init-value))))))
    (initialize-subnet-slot url write-subnets write-subnets)
    (initialize-subnet-slot url read-subnets read-subnets)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.377")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(SCL:FUNDEFINE 'TRUSTED-HOST-P)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.377")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define-generic trusted-host-p (url addresss &optional access-type)
  (:documentation "Returns non-null if ADDRESSS is trusted to access URL in ACCESS-TYPE mode.
ACCESS-TYPE can be any of :read, :write, or :read-write. It defaults to :READ."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.377")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod trusted-host-p ((url secure-subnets-mixin) addresss &optional (access-type :read))
  (ecase access-type
    (:read
      (or (www-utils:ip-host-trusted-p addresss (write-subnets url))
	  (www-utils:ip-host-trusted-p addresss (secure-subnets url))))
    ((:write :read-write)
     (www-utils:ip-host-trusted-p addresss (write-subnets url)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.816")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod inherit-export-parameters progn ((url url:secure-subnets-mixin) (parent url:secure-subnets-mixin))
  (setf (url::read-subnets url) (url::read-subnets parent)
	(url::write-subnets url) (url::write-subnets parent)
	(url::secure-subnets url) (url::secure-subnets parent)))

(defmethod inherit-export-parameters progn ((url url:authentication-mixin) (parent url:authentication-mixin))
  (setf (url:authentication-realm url) (url:authentication-realm parent)
        (url:capabilities url) (url:capabilities parent)))

(defmethod inherit-export-parameters progn ((url url:expiration-mixin) (parent url:expiration-mixin))
  (setf (url:expiration-function url) (url:expiration-function parent)
        (url:max-age-function url) (url:max-age-function parent)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.815")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod invoke-server-method ((server basic-server-mixin) (method (eql :delete)) (http-version symbol)
                                 &aux translation-method)
  (macrolet ((with-delete-response ((server stream) &body body)
               `(multiple-value-bind (url status)
                    (progn . ,body)
                  ;; this could send back a 200 if there was a message to be returned 
                  ;; it would need to force output because
                  ;; provide service won't handle it 6/11/95 -- JCMa.
                  (ecase status
                    (:deleted 
                      (setf (server-status ,server) 204.)
                      (report-status-no-content ,stream))
                    (:accepted
                      (setf (server-status ,server) 202.)
                      (report-status-accepted,stream)))
                  ;; write some headers as the close of transaction
                  (write-headers* ,stream :date (server-request-time *server*)
                                  :location (url:name-string url)
                                  :server *server-version*))))
    (with-slots (address stream url-string) server
      (multiple-value-bind (url)
          (url:intern-url url-string :if-does-not-exist :soft)
        (cond ((and url (setq translation-method (translation-method url)))
               (setf (server-url server) url)
               (with-access-control (url method server (or (url::write-subnets url) *write-subnets*)
                                         :write-method-p t)
                 (case translation-method
                   ((:redirect :temporary-redirect)     ; redirect when there is forwarding.
                    (handle-url-standard-redirection
                      url (eql :temporary-redirect translation-method) :delete))
                   (t (with-delete-response (server stream)
                                            (delete-document url stream))))))
              (t (error 'document-not-found :url url-string :method :delete)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.815")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun %put-new-resource (server stream url-string &aux (length (length url-string)))
  (labels ((url-inferior-directory (parent 1st-delim)
             (loop with start = (incf 1st-delim)
                   while (< start length)
                   for delim = (position #\/ url-string :start start :end length :test #'eql)
                   while delim
                   collect (subseq url-string start delim) into inf-path
                   do (setq start (1+ (the fixnum delim)))
                   finally (return (let ((path (translated-pathname parent)))
                                     (make-pathname :host (pathname-host path)
                                                    :device (pathname-device path)
                                                    :directory `(,@(pathname-directory path) ,.inf-path))))))
           (url-inferior-pathname (parent 1st-delim last-delim directory-level)
             (let ((name-and-extension (subseq url-string (1+ (the fixnum last-delim)) length)))
               (declare (dynamic-extent name-and-extension))
               (merge-pathnames name-and-extension (case directory-level
                                                     (0 (translated-pathname parent))
                                                     (t (url-inferior-directory parent 1st-delim))))))
           (put-inferior (parent 1st-delim last-delim directory-level)
             (let ((pathname (url-inferior-pathname parent 1st-delim last-delim directory-level)))
               (unless (or (zerop directory-level) (probe-directory pathname))
                 (create-directories-recursively pathname))
               (multiple-value-bind (url)
                   (intern-url (merge-url url-string (local-context)) :if-does-not-exist :create)
                 (inherit-export-parameters url parent)
                 (setf (url:translated-pathname url) pathname
                       (url:translation-method url) (export-type-for-pathname-type (pathname-type pathname) t))
                 (put-document url stream t nil)))))
    (multiple-value-bind (parent p-export-type export-type directory-level 1st-delim last-delim)
        (most-specific-exported-parent-url url-string -1 length)
      (unless parent (error 'document-not-found :url url-string :method :put))
      (with-access-control (parent :put server (or (url::write-subnets parent) *write-subnets*)
                                   :deny-subnets *disallowed-subnets* :write-method-p t)
        (cond ((not (and export-type ;;object with export type matched to directory export type.
                         (directory-type-exports-pathname-export-type-p p-export-type export-type)))
               (error 'method-not-allowed :method :put :url url-string))
              ((directory-export-type-p p-export-type)  ;single level directory export.
               (case directory-level
                 (0 (put-inferior parent 1st-delim last-delim 0))
                 (t (error 'document-not-found :url url-string :method :put))))
              ((hierarchical-directory-export-type-p p-export-type)
               (case directory-level
                 (0 (put-inferior parent 1st-delim last-delim 0))
                 ;; need to handle the export of newly created intervening directory levels.  6/11/95 -- JCMa.
                 (t (put-inferior parent 1st-delim last-delim directory-level))))
              (t (error 'method-not-allowed :method :put :url url-string)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.815")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod invoke-server-method ((server basic-server-mixin) (method (eql :put)) (http-version symbol)
                                 &aux url translation-method)
  (with-slots (address stream url-string) server
    (cond ;; resource already exists, so put a new version.
      ((and (setq url (url:intern-url url-string :if-does-not-exist :soft))
            (setq translation-method (translation-method url)))
       (setf (server-url server) url)
       (with-access-control (url method server (or (url::write-subnets url) *write-subnets*)
                                 :deny-subnets *disallowed-subnets*
                                 :write-method-p t)
         (case translation-method
           ((:redirect :temporary-redirect)     ; redirect when there is forwarding.
            (handle-url-standard-redirection
              url (eql translation-method :temporary-redirect) :put))
           (t (put-document url stream nil *check-document-versions*)))))
      (t (%put-new-resource server stream url-string)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.817")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(setf (documentation 'export-url 'function)

      "HTTP:EXPORT-URL is the primary method that exports URLS to make them
accessible via the http server. URL is either a string or an interned URL to
be exported.  EXPORT-TYPE is the method to use in exporting URL.


I. Basic Export Types: These involve making the contents of a file accessible via
a URL. These types require URLs that are object (i.e., have a name and extension).

        :HTML-FILE (&key pathname)
        :TEXT-FILE (&key pathname)
        :RTF-FILE (&key pathname)

        :GIF-IMAGE (&key pathname)
        :JPEG-IMAGE (&key pathname)
        :X-BITMAP-IMAGE (&key pathname)
        :PICT-IMAGE (&key pathname)
        :TIFF-IMAGE (&key pathname)

        :BASIC-AUDIO (&key pathname)
        :AIFF-AUDIO (&key pathname)
        :WAV-AUDIO (&key pathname)

        :MPEG-VIDEO (&key pathname)
        :QUICKTIME-VIDEO (&key pathname)

        :VRML-WORLD (&key pathname)

        :DIRECTORY (&key pathname immediate-export recache recursive-p)
        :HTML-DIRECTORY (&key pathname immediate-export recache recursive-p)
        :TEXT-DIRECTORY (&key pathname immediate-export recache recursive-p)
        :LISP-DIRECTORY (&key pathname immediate-export recache recursive-p)
        :IMAGE-DIRECTORY (&key pathname immediate-export recache recursive-p)
        :AUDIO-DIRECTORY (&key pathname immediate-export recache recursive-p)
        :VIDEO-DIRECTORY (&key pathname immediate-export recache recursive-p)
        :WORLD-DIRECTORY (&key pathname immediate-export recache recursive-p)
        :APPLICATION-DIRECTORY (&key pathname immediate-export recache recursive-p)

:DIRECTORY exports all files whose resource type is known.
Others export a specific content type, and ignore other file types.
When recursive-p is non-null, all subdirectories are also exported.
Otherwise, subdirectories are ignored.

When *AUTO-EXPORT* is non-null, new files are automatically exported when they
are scoped by one of these directory export types. Auto-export occurs on
demand for the GET and HEAD methods. If *AUTO-EXPORT* is :ON-DEMAND,
files are exported when they are first requested rather than at the
time the directory is exported. When exporting a directory, a non-null
argument to :IMMEDIATE-EXPORT overrides lazy directory exporting. In general,
on-demand directory exports trade faster server start up for a slightly slower
first access to a file URL within the directory. When :RECACHE is non-null,
a directory export updates the export parameters for every inferior. This
parameter forces traversal of the entire structure, like a non-null
:IMMEDIATE-EXPORT would.

A directory-style list in HTML is the default content returned for the
get method on a URL directory path. This behavior is customized by
providing a response function via the :DIRECTORY-WRITER keyword.  This
response function is called with the arguments (URL STREAM) and must
return non-null when it handles the response. If it declines to handle
the response, it may return NIL, and the standard method will list the
directory as usual. HTTP:WRITE-INDEXED-DIRECTORY-LISTING is a directory
writer that will serve the contents of an index.html file found in
the directory. Other computed returns are possible.

Note that the presence in file or directory names of escaped characters 
(see *ESCAPED-CHARACTERS*) will lead to inconsistent results, and possibly
errors. Space and question mark are examples.

        :PDF-FILE  (&key pathname)
        :POSTSCRIPT-FILE (&key pathname)

        :BINHEX-FILE (&key pathname)
        :STUFFIT-FILE  (&key pathname)
        :COMPRESSED-FILE (&key pathname)
        :MAC-BINARY-FILE (&key pathname)

        :WORD-FILE  (&key pathname)
        :POWER-POINT-FILE (&key pathname)
        :EXCEL-FILE  (&key pathname)

The following export types support inline plug-ins on the client side.
Plug-ins can be referenced using NS2.0:WITH-EMBEDDED-SCRIPT

        Inline speech synthesis is available using a macintalk plug-in
        from http://www.mvpsolutions.com/PlugInSite/Talker.html

        :TALK-AUDIO (&key pathname)

The Java language provides applets that execute on the client.  This kind of
mobile code is supported with the following export types and the HMTL
generation macro WITH-JAVA-APPLET.
        
        :JAVA-FILE (&key pathname)
        :JAVA-BINARY (&key pathname)
        :JAVA-SCRIPT-FILE (&key pathname)

:JAVA-FILE exports the source code whereas :JAVA-BINARY provides the byte
compiled binary to the client running the applet. :JAVA-SCRIPT-FILE exports
the browser scripting language, JavaScript, which is simpler and easier to use
than Java itself.

II. Redirect Export Types: These export types inform the client to
look elsewhere for a URL.  They work for the GET and HEAD operations.
The exported URL can be either an HTTP object or an HTTP path.

        :REDIRECT (&key alternate-urls pathname)
        :TEMPORARY-REDIRECT (&key alternate-urls pathname)

Alternatively, a computed response may call REDIRECT-REQUEST to issue a
redirect rather than serving content itself.

III. Search Export Types: these involve performing searches using the search
index or map search facilities in HTTP. Search URLs must end with ? so that
the system can composed the right combination of classes. In all cases, the
response function must compute the answer and return HTML over the http stream
to the client.

General Purpose Searches

        :SEARCH (&key response-function search-parser search-database)

        This exports a URL that performs searches by calling RESPONSE-FUNCTION with
        the arguments URL and STREAM. The search database and search parameters are
        cached on the URL and accessible via URL:SEARCH-DATABASE and URL:SEARCH-KEYS.

        The optional export argument SEARCH-PARSER is the parser that obtains URL:SEARCH-KEYS
        from the suffix of a search URL. Several parsers are predefined:

                URL:STANDARD-PARSE-SEARCH-INFO: This standard parser for
                search URLs. It tests whether the search info encodes form or
                list data and calls the appropriate of the next two parsers.

                URL:PARSE-SEARCH-INFO: This normal parser for search URLs
                produces a list of search parameters using + as the delimiter.

                URL:PARSE-SEARCH-INFO-AS-QUERY-ALIST: This parser for URL
                encoded form values returns an alist just like posting a form
                would. This parser should be used when an HTML form is
                returned to a search URL.  However, this method of returning
                form values is limited to 1024 characters total in the URL,
                and therefore, it's use in new code is deprecated.

        Users may provide other specialized parsers. They should accept
        the arguments (url-string start end) and need not located the ?
        suffix delimiter.

        The export argument, SEARCH-WRITER, allows a URL to specialize how the
        parsed presentation on URL:SEARCH-KEYS is written. Several writers are
        predefined.

                URL:STANDARD-WRITE-SEARCH-INFO: This standard writer for
                search URLs. It tests whether the search info encodes form or
                list data and calls the appropriate of the next two writers.

                URL:WRITE-SEARCH-INFO: This normal writer for search URLs
                produces a list of search parameters using + as the delimiter.

                URL:WRITE-SEARCH-INFO-AS-QUERY-ALIST: This writer for URL
                encoded form values that prints alist values as name value pairs
                using the urlencoding syntax.

        The export argument, HEADER-FUNCTION, allows a search URL to specialize
        how it responds to the HEAD method based on runtime knowledge.
        HEADER-FUNCTION is called with the argument URL  and returns values
        used to determine respoonse to HEAD. The returned values are:
        (CONTENT-TYPE CONTENT-LENGTH LAST-MODIFICATION VERSION CHARSET PUBLIC ALLOW)
        
                CONTENT-TYPE (required) is a keyword or content-type. Default
                is HTML.

                CONTENT-LENGTH (optional) is the length in bytes of the entity
                body. Default is none.

                LAST-MODIFICATION (optional) is a universal time indicating
                when the entity was last changed. Default is now.
 
                VERSION (optional) is a number or string that distinguishes
                different versions of the entity. Default is none.

                CHARSET (optional) is a keyword indicating the character set
                of the entity. Default is :ISO-8859-1.

                PUBLIC (optional) is a boolean indicating whether the resource
                is available to the public. Default is none.

                ALLOW (optional) is a list of HTTP method keywords available
                on the resource. Default is none.

Image Searches

   Image Maps

        :IMAGE-MAP (&key pathname export-type map-format map-pathname
                         search-parser search-writer header-function)

        This exports the image in PATHNAME as IMAGE-EXPORT-TYPE and
        establishes a response function based on the image map in MAP-PATHNAME
        whose MAP-FORMAT is either :CERN or :NCSA. EXPORT-TYPE is the
        appropriate image search export type (see below).

   Direct Searches

        These provide direct control over the response function for image searches.

        :GIF-IMAGE (&key pathname response-function search-database
                         search-parser search-writer header-function)
        :JPEG-IMAGE (&key pathname response-function search-database
                          search-parser search-writer header-function)
        :PNG-IMAGE (&key pathname response-function search-database
                         search-parser search-writer header-function)
        :X-BITMAP-IMAGE (&key pathname response-function search-database
                              search-parser search-writer header-function)
        :PICT-IMAGE (&key pathname response-function search-database)
                          search-parser search-writer header-function)
        :TIFF-IMAGE (&key pathname response-function search-database
                          search-parser search-writer header-function)

        These export types allow the client's user to click on images and
        invoke a response from the server.  These URL are both objects and
        searchable.  When they are requested without the ? suffix, the
        contents of their associate image file is returned.  When the ? suffix
        appears, their RESPONSE-FUNCTION is called on the arguments URL and
        STREAM. See the macro HTTP:WITH-IMAGE-COORDINATES automatically binds
        the X and Y coordinates.

IV. Computed Export Types: These compute responses returned to clients.

        :COMPUTED (&key response-function header-function) RESPONSE-FUNCTION
        is called with the arguments URL and STREAM and is responsible for
        returning HTML to the client. :COMPUTED has an optional pathname so
        that the computation may reference a file, if necessary.
        HEADER-FUNCTION is documented section III.
        
        :HTML-FORM (&key response-function pathname durable-form-values
        server) :HTML-FORM returns the contents of PATHNAME when it is
        requested via GET.  When there is a POST, its RESPONSE-FUNCTION is
        called with URL, STREAM, and FORM-ALIST.  FORM-ALIST is an alist of
        (QUERY RAW-VALUE) for all the queries in the form that the client
        returns. QUERY is a keyword.  When a client returns multiple
        raw-values for a QUERY, these are aggregated into a list of the values
        associated with the query in a single, order-preserving entry.
        DURABLE-FORM-VALUES is a boolean and controls whether query values
	are new, copied strings or indirect arrays pointing into a volitile
	form data buffer. High-volume applications or forms large query values
	can gain efficiency by using durable form values, all operations on
	these values must be completed within the context of the form's
	response function.

        :COMPUTED-FORM (&key form-function response-function header-function
        durable-form-values server) :COMPUTED-FORM is a cross between
        :COMPUTED and :HTML-FORM that provides FORM-FUNCTION to generate html
        just like :COMPUTED and RESPONSE-FUNCTION to handle the post method
        when form values are returned. FORM-FUNCTION is called with the
        arguments URL and STREAM and is responsible for returning HTML to the
        client. response-function takes the same arguments as :HTML-FORM.

        :SCRIPT (&key script header-function) Script is a client-side script
        defined with NS2.0:DEFINE-SCRIPT. These scripts may merely deliver a
        static copy of the client-side script, or they may perform a
        computation that emits an appropriate script.

        :SHTML-FILE (&key pathname header-function) This is a computed
        response that is inserted in a static HTML file containing
        server-parsed HTML. When an SHTML element is found by the server, it
        inserts the result of a computation in place of the SHTML element.
        SHTML elements are delimted by <!--# and --> and look like:

                <!--#include file=\"insert.text\"-->

        INCLUDE is an SHTML operation that requires a FILE. For security
        reasons, FILE must be in the same directory as the STHML file. Access
        may be controlled by providing the optional SUBNETS parameter, which
        is a comma-separated string of IP addresses without whitespace.

         <!--#include file=\"insert.text\" subnets=\"128.52.0.0,18.21.0.0\"-->
        
        EVAL is a more general SHTML operation that requires an ACTION
        parameter. ACTION is a string denoting an SHTML action. Specific
        parameters may be required by individual actions. Here, DATE-TIME
        is the operation and FORMAT is a parameter.

              <!--#eval action=\"date-time\" format=\"iso\"-->

        Predefined actions are documented by HTTP:SHOW-SHTML-ACTIONS. New
        SHTML actions are defined with HTTP:DEFINE-SHTML-ACTION. Files with
        the extention SHTML are autoexported by HTML directory export types.

V. Export Parameters and Issues

1. Portable Exports: The #u reader macro merges a partial URL specification
against the default for the local host. Use this when you be able to load the
same exports file on different hosts. The #u reader macro has an extended syntax
that allows you to overview the default host and port specified by
the server configuration. The syntax is 

     #u(url-string :host host-string :port port-number)

URL-STRING is a relative URI. HOST-STRING is the domain name or
IP string for the host. PORT-NUMBER is an integer.


2. Security: 

  A. Subnet Security: Secure subnets are a list of IP addresses, where 0 is a
  wildcard. For example, 128.52.0.0 matches all the subnets at the AI lab.
  The follow export arguments allow control over which clients can perform
  HTTP methods such as GET, HEAD, POST, :OPTIONS, or :TRACE (read access)
  versus PUT or DELETE (write access).

     :READ-SUBNETS allows read access to be specified at the level of
     URLs as they are exported.

     :WRITE-SUBNETS allows write access to be specified at the level of
     URLs as they are exported.

     DEFINE-READ-SUBNETS restricts read access globally to the server.

     DEFINE-WRITE-SUBNETS restricts write access globally to the server.

  Write access presumes read access, and consequently, IP addresses from the
  write subnets need not be included in the read subnets. To select the global
  authentication policy for write methods, HTTP:*ACCEPT-WRITE-METHODS*.

  B. User Authentication: URL authentication can be specified using the
  following export arguments.

     :AUTHENTICATION-REALM a realm obtainable via INTERN-REALM.
     These can be created with ADD-REALM.

     :CAPABILITIES a URL access control obtainable via INTERN-ACCESS-CONTROL.
     These can be created with ADD-ACCESS-CONTROL-GROUP.

  See also: ADD-GROUP, ADD-USER, and SAVE-AUTHENTICATION-DATA.

3. Expiration: The expiration time for a url is issued as an EXPIRES header so
that proxy servers can determine when they need to refresh their cache.

Expiration is controlled by providing the :EXPIRATION when exporting any URL.
If expiration is not provided, the default is no expiration date.

The :EXPIRATION keyword takes one argument, which is either keyword or a list
of (keyword &rest arguments).

     Arguments                       Meaning

 :NO-EXPIRATION-HEADER        --  No EXPIREs header is issued.
 :NEVER                       --  EXPIRES header indicates one year from now.
 (:INTERVAL <universal-time>) --  EXPIRES header indicates now + <universal-time>.
 (:TIME <universal-time>)     --  EXPIRES header indicates an <universal-time>.
 (:FUNCTION <function-spec>)  --  EXPIRES header indicates universal time computed by
                                  applying <function-spec> to URL.  <function-spec>
                                  should return a universal for use in the EXPIRES header
                                  or NIL, in which case no EXPIRES header is issued.

4. Character Sets: The :CHARACTER-SET keyword allows any URLs whose content
type is TEXT (e.g., text/plain, text/html) to be exported with character sets
other than the HTTP default :ISO-8859-1, or subsets. The valid character sets
for HTTP are:  :US-ASCII, :ISO-8859-1, :ISO-8859-2, :ISO-8859-3, :ISO-8859-4,
:ISO-8859-5, :ISO-8859-6, :ISO-8859-7, :ISO-8859-8, :ISO-8859-9, :ISO-2022-JP,
:ISO-2022-JP, :ISO-2022-KR, :UNICODE-1-1, :UNICODE-2-2-UTF-7,
:UNICODE-2-2-UTF-7.  Whenever TEXT content types use a character set other
than :ISO-8859-1, the HTTP requires explicit specification via this export
keyword.

6. Languages:  The :LANGUAGE keyword may be supplied for any exported
URL. The value is a sequence of keywords denoting the language(s) in which the
resource is written. HTTP 1.1 defines these keywords in section 3.10 to
conform with RFC 1766. They can be a two-letter ISO 639 language abbreviation,
optionally with a two-letter ISO 3166 country code as a subtag.

7. Documentation: Keywords and a descriptive string can be attached to URLs at
export time.  For directory exports, note that these arguments apply to ALL
URLs exported during the directory walk.

     :KEYWORDS                  A list of keywords.
     :DOCUMENTATION             A string describing the URL.


8. Virtual Hosts: HTTP 1.1 requires a virtual hosting facility,
which this server implements. You can define a virtual host (or
vanity name) that will be served by the physical server from
the same IP address. Any URIs served by a virtual host must be
exported by specifying the absolute URI, including host and port
number.  The #u reader macro may be useful here.  The following
operators are available for use with virtual hosts:

     ADD-VIRTUAL-HOST: Adds a virtual host on a virtual port and
                       and makes URLs served by that host available
                       to HTTP 1.1 or greater clients.

     REMOVE-VIRTUAL-HOST: Makes the virtual host unavailable, but does
                          does not remove any URLs it exports.

9. New static export types for data stored in files can be defined with
DEFINE-URL-EXPORT-TYPE.

10. HTTP 1.1 Cache Control: The keywords below may be supplied when exporting
any URL in order to statically control how downstream proxies and caches
handle the content associated with a URL.

        :PUBLIC -- If the value is T, the entire message is cachable by any
        cache even if it would not normally be cached.

        :PRIVATE -- If the value is T, the entire message is intended for a
        single user and must not be cached by a shared cache. If the value is
        a list of keywords denoting specific header, then only these headers
        should be considered private.

        :NO-CACHE -- If the value is T, the entire message must not be cached
        by any cache along the request chain.  If the value is a list of
        keywords denoting specific headers, then only these headers should be
        discarded before caching.

        :NO-STORE -- If the value is T, the entire message must not be stored
        in any non-volatile storage by any cache along the request chain.

        :MAX-AGE -- The value is the number of seconds for which the response
        is valid, after which it should be revalidated.  This directive
        overrides the expiration header, thus allowing HTTP 1.1 server to
        provide a longer expiration time to HTTP 1.1 servers and proxies.
        This defaults to a value derived from the expiration time.

        :MUST-REVALIDATE -- If the value is T, caches and proxies must not
        serve the resource without revalidating it once it becomes stale, even
        if configured to operate with state data. Servers should send this
        directive if and only if failure to revalidate will result in
        incorrect operation. Recipients must not take any automated action
        that violates this directive.

        :PROXY-REVALIDATE -- If the value is T, this directive is the same as
        :MUST-REVALIDATE except that it applies only to proxies and not
        non-shared user caches. It can be used on response to an authenticated
        request to permit the user's cache to store and later return the
        response without needing to revalidate it.

        :NO-TRANSFORM -- If the value is T, caches and proxies must not change
        the body of the message or change headers describing the content.

11. Property List: The PROPERTY-LIST keyword adds a property list of
alternating keywords and value to a URL.  These properties can be read
and set via GET-VALUE.")
