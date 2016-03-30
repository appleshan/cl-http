;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.32
;;; Reason: Variable HTTP::*PROXY-INTERN-URL-STATUS*:  new variable to allow URL metering to work for proxy metering.
;;; Function HTTP::%EXECUTE-REQUEST:  respect *proxy-intern-url-status*.
;;; Function HTTP:ENABLE-URL-METERING:  allow metering of proxy requests via *proxy-intern-url-status*.
;;; Function HTTP:DISABLE-URL-METERING:  turn off URL creation in *proxy-intern-url-status*.
;;; Function HTTP::%SHTML-INSERT-FILE:  add no-error-p arg.
;;; DEFINE-SHTML-ACTION HTTP::INSERT-FILE-CONDITIONAL:  new conditional action.
;;; DEFINE-SHTML-ACTION HTTP::INSERT-FILE:  make erroring on missing file part of the contract.
;;; Written by JCMa, 3/21/00 00:31:28
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.31,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, HTTP Proxy Server 4.5,
;;; HTTP Client Substrate 3.4, Jcma 41, HTTP Client 49.6, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, Ivory Revision 5, VLM Debugger 329,
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
  "HTTP:SERVER;VARIABLES.LISP.184"
  "HTTP:SERVER;SERVER.LISP.834"
  "HTTP:SERVER;LOG.LISP.181"
  "HTTP:SERVER;SHTML.LISP.24")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;VARIABLES.LISP.184")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp;-*-")

(defparameter *proxy-intern-url-status* :uninterned
  "Controls how URLs are created when the proxy encounters unknown URLs.
The values can be :UNINTERNED or :CREATE.")

(eval-when (:execute :compile-toplevel :load-toplevel)
  (export (intern "GC-URL-TABLE" :url) :url))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.834")
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
	     (setf (server-url-string server) url-string)
	     (multiple-value-bind (uri)
		 (intern-url url-string :if-does-not-exist *proxy-intern-url-status*)
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
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.181")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

;;;------------------------------------------------------------------- 
;;;
;;; METERING LOGS
;;;

(define enable-url-metering (&key (port *standard-http-port*) (class 'basic-url-metering))
  "Enable URL metering on port with metering class, CLASS."
  (multiple-value-bind (log newly-created-p)
      (intern-access-log "URL-Metering-Log"
                         :port port
                         :if-does-not-exist :create
                         :class class)
    (setq *proxy-intern-url-status* :create)
    (unless newly-created-p
      (add-access-log log port))
    (values log newly-created-p)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.181")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(define disable-url-metering (&key (port *standard-http-port*) (class 'basic-url-metering))
  "Disables URL metering on PORT for metering class, CLASS.
If metering proxy activity, call URL:GC-URL-TABLE to clean up remote URL 
after extracting metering results ."
  (let ((log (intern-access-log "URL-Metering-Log" :port port
                                :if-does-not-exist :soft
                                :class class)))
    (setq *proxy-intern-url-status*  :uninterned)
    (when log
      (remove-access-log log port))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SHTML.LISP.24")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defun %shtml-insert-file (file url stream &optional no-error-p)
  "Returns non-null if FILE exists and is successfully copied to stream."
  (declare (values success-p))
  (let* ((default (translated-pathname url))
         (path (pathname file))
         (pathname (make-pathname :name (pathname-name path) :type (pathname-type path) :version :newest :defaults default)))
    (cond (no-error-p
	   (with-open-file (file pathname :direction :input :if-does-not-exist nil)
	     (if file
		 (stream-copy-until-eof file stream :text)
		 (return-from %shtml-insert-file nil))))
	  (t (with-open-file (file pathname :direction :input :if-does-not-exist :error)
	       (stream-copy-until-eof file stream :text))))
    t))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SHTML.LISP.24")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(define-shtml-action insert-file-conditional (file subnets alternate-text)
                     (:output-mode :text :documentation "Inserts the contents of FILE, which must be a text/* file
in the same directory as the HTML file containing the tag. 
Unlike <INSERT-FILE>, this reports ALTERNATE-TEXT when FILE is not 
found. SUBNETS is an optional comma separated string of IP addresses.")
  (when (or (null subnets)
	    (ip-host-trusted-p (server-address *server*) (template-secure-subnets url file subnets)))
    (unless (%shtml-insert-file file url stream t)
      (if alternate-text
	  (write-string alternate-text stream)
	  (fast-format stream "The file, ~A.~A, was not found. Please advise the Webmaster."
		       (pathname-name file) (pathname-type file))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SHTML.LISP.24")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(define-shtml-action insert-file (file subnets)
                     (:output-mode :text :documentation "Inserts the contents of FILE, which must be a text/* file
in the same directory as the HTML file containing the tag.
An error is signalled when FILE is not found.
SUBNETS is an optional comma separated string of IP addresses.")
  (when (or (null subnets)
	    (ip-host-trusted-p (server-address *server*) (template-secure-subnets url file subnets)))
    (%shtml-insert-file file url stream)))

