;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.59
;;; Reason: CLOS class HTTP::PROXY-SERVER-MIXIN:  new iv proxy-request-p.
;;; Function HTTP::LOG-ENTRY-P:  new predicate.
;;; Function (CLOS:METHOD HTTP::LOG-ENTRY-P (HTTP::FILE-LOGGING-MIXIN HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Function (CLOS:METHOD HTTP::LOG-ENTRY-P (HTTP::DYNAMIC-LOGGIN-MIXIN HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Function (CLOS:METHOD HTTP::LOG-SERVER-ACCESS (HTTP::FILE-LOGGING-MIXIN HTTP::SERVER-LOGGING-MIXIN) AND):  update.
;;; Function (CLOS:METHOD HTTP::LOG-SERVER-ACCESS (HTTP::DYNAMIC-LOGGIN-MIXIN HTTP::SERVER-LOGGING-MIXIN) AND):  ditto.
;;; Function (CLOS:METHOD HTTP::RESET-TRANSACTION-STATE (HTTP::BASIC-SERVER-MIXIN)):  reset server-proxy-request-p on each transaction.
;;; Function HTTP::%EXECUTE-REQUEST:  set server-proxy-request-p as appropriate.
;;; CLOS class HTTP::EXCLUSIVE-PROXY-LOG-MIXIN:  -
;;; CLOS class HTTP::EXCLUSIVE-SERVER-LOG-MIXIN:  -
;;; Function (CLOS:METHOD HTTP::LOG-ENTRY-P (HTTP::EXCLUSIVE-SERVER-LOG-MIXIN HTTP::PROXY-SERVER-MIXIN)):  -
;;; CLOS class HTTP::SERVER-COMMON-FILE-LOG:  new log class.
;;; Function (CLOS:METHOD HTTP::PRINT-ENTITY-TAG-HEADER (CONS T)):  provide methods for set of etag values.
;;; Function HTTP::%DEFINE-HEADER-KEYWORD:  -
;;; Function HTTP::DEFINE-HEADER-KEYWORDS:  -
;;; DEFINE-PREFERENCE-TYPE :SECURITY-PREFERENCES:  replace  :secure-subnets with :read-subnets and :write-subnets.
;;; Function NS3.0::%WRITE-MAKE-FONT-ARGS:  add poin-size and weight.
;;; Function NS3.0:WITH-FONT:  ditto.
;;; Function NETSCAPE4.0:%WRITE-MAKE-FONT-ARGS:  -
;;; Function NETSCAPE4.0:WITH-FONT:  -
;;; Function HTTP::PARSE-SET-COOKIE-HEADER:  handle date errors in expires field.
;;; DEFINE-HEADER :SET-COOKIE:  update.
;;; Variable URL::*ESCAPE-URLS*:  new
;;; Function URL::%UNESCAPE-URL:  respect *escape-urls*.
;;; Function URL::%CANONICALIZE-HOST-PREFIXED-URL:  update.
;;; Function (CLOS:METHOD URL:CANONICALIZE-URL ((EQL :FILE) T)):  update
;;; Function (CLOS:METHOD URL:CANONICALIZE-URL ((EQL :FTP) T)):  update
;;; Function (CLOS:METHOD URL:CANONICALIZE-URL ((EQL :GOPHER) T)):  update
;;; Function (CLOS:METHOD URL:CANONICALIZE-URL ((EQL :HTTP) T)):  update
;;; Function (CLOS:METHOD URL:CANONICALIZE-URL ((EQL :HTTPS) T)):  update
;;; Function (CLOS:METHOD URL:CANONICALIZE-URL ((EQL :MAILTO) T)):  update
;;; Function (CLOS:METHOD URL:CANONICALIZE-URL ((EQL :WAIS) T)):  update
;;; Function URL:SEARCH-URL-FORM-ENCODING-P:  if an unescaped = sign is seen, assume that url form encoding.
;;; Written by JCMa, 8/09/00 17:08:02
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.58,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42, HTTP Proxy Server 5.18,
;;; HTTP Client Substrate 3.14, Ivory Revision 5, VLM Debugger 329,
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
  "HTTP:SERVER;CLASS.LISP.31"
  "HTTP:SERVER;SERVER.LISP.839"
  "HTTP:SERVER;SERVER.LISP.840"
  "HTTP:SERVER;LOG.LISP.183"
  "HTTP:SERVER;SERVER.LISP.841"
  "HTTP:SERVER;CLASS.LISP.32"
  "HTTP:SERVER;HEADERS.LISP.463"
  "HTTP:SERVER;HEADERS.LISP.464"
  "HTTP:SERVER;PREFERENCES.LISP.37"
  "HTTP:SERVER;NETSCAPE-4-0.LISP.15"
  "HTTP:SERVER;HEADERS.LISP.465"
  "HTTP:SERVER;URL.LISP.403"
  "HTTP:SERVER;URL.LISP.404")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.31")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defclass proxy-server-mixin
	  ()
    ((proxy-request-p :initform nil :initarg :proxy-request-p :accessor server-proxy-request-p))
  (:documentation "Provides proxy service capabilities."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.839")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(define-generic log-entry-p (log agent)
  (:documentation "Returns non-null when LOG should write a log entry for the current state of AGENT.
Methods are combined with AND method combination so that NIL is returned if any method returns NIL.")
  (:method-combination and))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.839")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod log-server-access and ((log file-logging-mixin) (server server-logging-mixin))
  (when (log-entry-p log server)
    (write-log-entry-to-file log server))
  t)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.839")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod log-server-access and ((log dynamic-loggin-mixin) (server server-logging-mixin))
  (when (log-entry-p log server)
    (dynamically-log-transaction server log))
  t)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.840")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod reset-transaction-state ((server basic-server-mixin))
  (let ((stream (server-stream server))
        (process (server-process server)))
    ;; reset resourced items
    (setf (fill-pointer (%server-request server)) 0
	  (fill-pointer (server-url-buffer server)) 0)
    (clear-header-set (server-headers server) nil)
    ;; reset instance variables
    (setf (server-request-copy server) nil
	  (server-method server) nil
          (server-url-string server) nil
          (server-url server) nil
          (property-list server) nil
          (server-status server) 200
	  (server-form-data server) nil
	  (server-close-connection-p server) nil
	  (server-persistent-connection-p server) nil
          (www-utils:bytes-transmitted stream) 0
          (www-utils:bytes-received stream) 0
          (server-request-time server) nil
          (server-timeout server) *server-timeout*
          (server-life-time server) *server-life-time*
          (server-start-time server) (get-internal-real-time)
          (server-process-start-time server) (www-utils:process-run-time process)
	  (server-proxy-request-p server) nil)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.840")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun %execute-request (server request stream)
  (declare (values persistent-connection-p)
	   (optimize (speed 3)))
  (flet ((invoke-local-service (server url-string method http-version)
           (multiple-value-bind (request-context bind-context-p)
               (request-local-context url-string http-version *headers*)
             (setf (server-url-string server) (%merge-url url-string request-context t))
             (if bind-context-p
                 (with-virtual-host-local-context (request-context)
                   (invoke-server-method server method http-version))
                 (invoke-server-method server method http-version)))))
    (declare (inline invoke-local-service))
    ;; parse the request
    (multiple-value-bind (method url-string http-version)
	(parse-request request 0 (length request) (server-url-buffer server))
      (unless (and method url-string http-version)
	(error 'bad-syntax-provided :method method :url url-string
	       :format-string "Bad HTTP Request: ~S"
	       :format-args (list request)))
      (unless (member http-version '(:http/1.1 :http/1.0 :http/0.9))
	(error 'http-version-not-supported :url url-string
	       :format-string "The server does not support ~A."
	       :format-args (list http-version)))
      (setf (server-method server) method
	    (server-http-version server) http-version
	    (server-url-string server) url-string
	    (server-status server) 200)		;anything other than 200 must reset the status value.
      (without-connection-overflows (url-string)
	(let ((*headers* (resourced-read-headers (server-headers server) stream)))
	  (cond ;; scheme prefixed URL Proxy path
	    ((url:scheme-prefixed-url-p url-string)
	     (setf (server-proxy-request-p server) t
		   (server-url-string server) url-string)
	     (let ((url:*escape-search-urls* nil)	;pass through escaping
		   (url:*search-url-signal-bad-form-encoding* nil)	;pass through broken URL- form encoded URLs.
		   (uri (intern-url url-string :if-does-not-exist *proxy-intern-url-status*)))
	       (cond  ;; Actually a local reference, start over as 
		 ((url:local-url-p uri)
		  (invoke-local-service server url-string method http-version))
		 (*proxy-service*
		  (setf (server-url server) uri)
		  (invoke-proxy-service server uri method http-version))
		 (t (error 'access-forbidden 
			   :format-string "HTTP Proxy service is currently unavailable on ~A (~D)." 
			   :format-args (list (local-host-domain-name) (server-host-local-port server))
			   :method method :url uri)))))
	    ;; Standard path, call the primary server method.
	    (t (invoke-local-service server url-string method http-version))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.32")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defclass exclusive-proxy-log-mixin () ()
  (:documentation "Mixin that ensures that only proxy requests are logged."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.32")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defclass exclusive-server-log-mixin () ()
  (:documentation "Mixin that ensures that only server requests are logged."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.183")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

;; Predicate that decides when we have a normal server log
(defmethod log-entry-p ((log exclusive-server-log-mixin) (server proxy-server-mixin))
  (not (server-proxy-request-p server)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.841")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod log-entry-p and ((log file-logging-mixin) (server server-logging-mixin))
  (log-file-logging log))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.841")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod log-entry-p and ((log dynamic-loggin-mixin) (server server-logging-mixin))
  (log-dynamic-logging log))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.32")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defclass server-common-file-log
          (exclusive-server-log-mixin common-file-log
	    )
    ((log-file-name :initform "Common-Server-Log" :initarg :log-file-name :allocation :class))
  (:documentation "This log class records accesses according to the Common File Format without logging proxy requests."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.32")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defclass server-extended-common-file-log
          (exclusive-proxy-log-mixin extended-common-file-log)
    ((log-file-name :initform "Ext-Common-Server-Log" :initarg :log-file-name :allocation :class))
  (:documentation "This log class records accesses according to the Common File Format without logging proxy requests.
It also records the referrer field and the user agent when they are present.
This will cons more than common-file-log because it must copy two header values."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.463")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod print-entity-tag-header ((entity-tags cons) stream)
  (loop for (etag . more) = entity-tags then more
	while etag
	do (print-entity-tag-header etag stream)
	when more
	  do (fast-format stream ", ")))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.464")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (:compile-toplevel :execute :load-toplevel)

(defun %define-header-keyword (item)
  (let ((keyword (tokenize-header-keyword item)))
    (setf (get keyword 'keyword-header-value) item)
    keyword))

(define-macro define-header-keywords (&rest values)
  `(dolist (item ',values)
     (%define-header-keyword item)))
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.464")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header-keywords "bytes" "none")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.464")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header-keywords "realm" "nonce" "username" "uri" "response" "digest" "algorithm" "opaque"
			"basic" "digest")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.464")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header-keywords "nextnonce" "digest")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.464")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header-keywords "no-cache" "no-store" "max-age" "max-stale" "min-fresh" "only-if-cached" "public" 
                        "private" "no-transform" "must-revalidate" "proxy-revalidate")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.464")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header-keywords "close" "Keep-Alive")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.464")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header-keywords "100-continue")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.464")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header-keywords "no-cache")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.464")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header-keywords  "delete" "get" "head" "options" "post" "put" "trace")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.464")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header-keywords "chunked" "identity" "deflate")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.464")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header-keywords "realm" "domain" "nonce" "opaque" "stale" "algorithm")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.464")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header-keywords "md5" "sha")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;PREFERENCES.LISP.37")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(define-preference-type :security-preferences
                        :name "Security"
                        :display-string "Security & Authentication"
                        :inferiors (:accept-write-methods :authentication-data-pathname :read-subnets :write-subnets)
                        :description "Parameters related to user authentication, access control, and security.")
;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;NETSCAPE-4-0.LISP.15")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: (netscape4.0 :use (future-common-lisp ns3.0 www-utils url)); BASE: 10; Syntax: ansi-common-lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun %write-make-font-args (stream color face point-size size weight)
  (cond-every
    (color
      (%write-command-key-arg stream "COLOR" (color-mapping color t)))
    (face
      (typecase face
        (atom (%write-command-key-arg stream "FACE" face))
        (cons
          (%write-command-key-arg stream "FACE" (car face))
          (loop for item in (cdr face)
                do (write-char #\, stream)
                   (write (etypecase item
                            (keyword (symbol-name item))
                            (string item))
                          :escape t :stream stream)))))    
    (point-size
      (%write-command-key-arg stream "POINT-SIZE" point-size t))
    (size
      (unless (< 0 size 8)
        (error "SIZE, ~S, is not an integer from 1 through 7." size))
      (%write-command-key-arg stream "SIZE" size t))
    (weight
      (unless (< 99 weight 901)
        (error "WEIGHT, ~S, is not an integer from 100 through 900." weight))
      (%write-command-key-arg stream "WEIGHT" weight t))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;NETSCAPE-4-0.LISP.15")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: (netscape4.0 :use (future-common-lisp ns3.0 www-utils url)); BASE: 10; Syntax: ansi-common-lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (:compile-toplevel :execute :load-toplevel)
  (shadow (intern "WITH-FONT" :netscape4.0) :netscape4.0)
  (export (intern "WITH-FONT" :netscape4.0) :netscape4.0))

(define-macro with-font ((&key color face point-size size weight (stream '*output-stream*)) &body body)
  "Declares the current font size to be SIZE or POINT-SIZE, the font face to be
FACE, the weight or boldness to be WEIGHT, and the color to be COLOR within BODY."
  `(%with-environment ("FONT" :fresh-line nil :stream ,stream)
                      (%write-make-font-args ,stream ,color ,face ,point-size ,size ,weight)
     ,@body))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.465")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun parse-set-cookie-header (string &optional (start 0) (end (length string)))
  (flet ((parse-parameters (string start end)
           (when (< start end)
             (loop for s = start then (1+ e2)
                   while (< s end)
                   for s1 = (position-if-not* #'white-space-char-p string :start s :end end)
                   while s1
                   for e1 = (char-position #\= string s1 end)
                   for e2 = (or (char-position #\; string (or e1 s1) end) end)
                   for keyword = (%intern-header-keyword-value string s1 (or e1 e2))
                   for value = (ecase keyword
				 ;; errors parsing dates mean expire the
				 ;; cookie similar to what happens in HTTP,
				 ;; but there is no guidance in RFC 2109
				 ;; 8/10/2000 -- JCMa.
                                 (:expires (parse-expires-header string (1+ e1) e2))
                                 ((:domain :path) (subseq string (1+ e1) e2))
                                 (:secure t))
                   collect keyword
                   collect value))))
    (declare (inline parse-parameters))
    (with-string-trim-bounds (*white-space-chars* string start end)
      (let* ((e1 (char-position #\= string start end))
	     (e2 (or (char-position #\; string e1 end) end)))
	`(,(%intern-header-keyword-value string start e1)
	  ,(subseq string (1+ e1) e2)
	  ,.(parse-parameters string (1+ e2) end))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.465")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :set-cookie (:header :response)
  :print-string "Set-Cookie"
  :parse-function 'parse-set-cookie-header
  :print-function 'print-set-cookie-header)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.403")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define-parameter *escape-urls* t
                  "Controls whether all URLs is escaped on intern and on generation.") 


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.403")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defun %unescape-url (url-string start end destructive-p)
  (declare (fixnum start end)
	   (values canonical-url new-start new-end))
  (cond (*escape-urls*
	 (http::with-bad-escaping-resignalled (url-string :start start :end end
							  :reason "Bad Escaping: Ill-escaped URL")
	   (cond (destructive-p
		  (multiple-value-bind (unescaped-string new-end)
		      (http::nstring-unescape-special-chars url-string start end t #\space t)
		    (values unescaped-string start new-end)))
		 (t (multiple-value-bind (unescaped-string chars-unescaped-p new-url-string-p)
			(string-unescape-special-chars url-string start end t)
		      (declare (ignore chars-unescaped-p))
		      (unless new-url-string-p
			(setq unescaped-string (subseq url-string start end)))
		      (values unescaped-string 0 (length unescaped-string)))))))
	(destructive-p
	 (values url-string start end))
	(t (values (subseq url-string start end) (- end start)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.403")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defun %canonicalize-host-prefixed-url (prefix prefix-length url-string start end &optional destructive-p)
  (declare (values canonical-url new-string-p)
	   (fixnum start end))
  (flet ((downcase-char (string idx)
	   (setf (aref string idx) (char-downcase (aref string idx)))))
    (declaim (inline downcase-char))
    ;; Ensure that URLs are in canonical form when doing parses or interns.
    (multiple-value-bind (canonical-url start end)
	(%unescape-url url-string start end destructive-p)
      (let ((scheme-prefixed-p (string-equal prefix canonical-url :start1 0 :end1 prefix-length :start2 start :end2 (min prefix-length end)))
	    host-delim)
	(with-fast-array-references ((canonical-url canonical-url string))
	  ;; downcase host
	  (loop with slash-number fixnum = (if scheme-prefixed-p 0 2)
		for idx fixnum upfrom start below end
		for char = (aref canonical-url idx)
		do (case char
		     (#\/ 
		      (when (= 3 (incf slash-number))
			(setq host-delim idx)
			(return)))
		     (t (unless (lower-case-p char)
			  (setf (aref canonical-url idx) (char-downcase char))))))
	  ;; downcase any escapes
	  (when host-delim 
	    (loop for idx fixnum upfrom (1+ (the fixnum host-delim)) below end
		  for char = (aref canonical-url idx)
		  do (case char
		       (#\%
			(let ((idx2 (+ 2 idx)))
			  (when (< end idx2)
			    (error 'bad-escaping :url (subseq url-string start end)))
			  (downcase-char canonical-url (1+ idx))
			  (downcase-char canonical-url idx2)
			  (incf idx 2))))))
	  (values canonical-url (not destructive-p)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.403")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod canonicalize-url ((scheme (eql :file))  url-string &optional (start 0) (end (length url-string)) destructive-p)
  (multiple-value-bind (canonical-url)
      (%unescape-url url-string start end destructive-p)
    canonical-url))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.403")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod canonicalize-url ((scheme (eql :ftp)) url-string &optional (start 0) (end (length url-string)) destructive-p)
  (%canonicalize-host-prefixed-url "ftp" 3 url-string start end destructive-p))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.403")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod canonicalize-url ((scheme (eql :gopher)) url-string &optional (start 0) (end (length url-string)) destructive-p)
  (%canonicalize-host-prefixed-url "gopher" 6 url-string start end destructive-p)) 


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.403")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod canonicalize-url ((scheme (eql :http)) url-string &optional (start 0) (end (length url-string)) destructive-p)
  (%canonicalize-host-prefixed-url "http" 4 url-string start end destructive-p))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.403")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod canonicalize-url ((scheme (eql :https)) url-string &optional (start 0) (end (length url-string)) destructive-p)
  (%canonicalize-host-prefixed-url "https" 4 url-string start end destructive-p))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.403")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod canonicalize-url ((scheme (eql :mailto))  url-string &optional (start 0) (end (length url-string)) destructive-p)
  (declare (fixnum start end)) 
  (multiple-value-bind (canonical-url start end) 
      (%unescape-url url-string start end destructive-p)
    (let* ((prefix-end (+ 6 (the fixnum start)))
	   (at-sign (and (< prefix-end end) (char-position #\@ canonical-url prefix-end end))))
      (when at-sign
        (nstring-downcase canonical-url :start start :end prefix-end)
        (nstring-downcase canonical-url :start at-sign :end end))
      canonical-url))) 


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.403")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod canonicalize-url ((scheme (eql :wais)) url-string &optional (start 0) (end (length url-string)) destructive-p)
  (%canonicalize-host-prefixed-url "wais" 4 url-string start end) destructive-p)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.404")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defun search-url-form-encoding-p (string &optional (start 0) (end (length string)))
  "Returns non-null if STRING contains an HTTP form encoding."
  (with-fast-array-references ((string string string))
    (loop with equal-sign-p and amp
          for idx upfrom start below end
          for char = (aref string idx)
          do (case char
               (#\& (if equal-sign-p
                        (return t)
                        (setq amp t)))
               (#\= (if amp
                        (return t)
                        (setq equal-sign-p t)))
               (#\+ (return nil)))
          finally (return equal-sign-p))))

