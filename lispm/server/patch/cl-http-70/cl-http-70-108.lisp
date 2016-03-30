;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.108
;;; Reason: Add proxy condition and fix local uri optimization for proxy
;;; to get the port right and log both accesses correctly.
;;; 
;;; DEFINE-CONDITION HTTP::PROXY-ACCESS-FORBIDDEN:  new.
;;; Function (CLOS:METHOD HTTP:REPORT-STATUS-MESSAGE (HTTP::REPORTABLE-CONDITION T)):  -
;;; DEFINE-CONDITION HTTP::REPORTABLE-CONDITION:  add reporter slot.
;;; Function WWW-UTILS:SERVER-MAIL-ADDRESS:  -
;;; Function HTTP::WRITE-SERVER-MAIL-ADDRESS:  new
;;; Function HTTP::REPORT-PROXY-ACCESS-FORBIDDEN:  -
;;; Function URL::DIRECTORY-CONTEXT-STRING:  new name for old local-context-string.
;;; Function (CLOS:METHOD URL::DIRECTORY-CONTEXT-STRING (URL:HTTP-URL)):  -
;;; Function URL:LOCAL-CONTEXT-STRING:  -
;;; Function (CLOS:METHOD URL:LOCAL-CONTEXT-STRING (URL:HTTP-URL)):  -
;;; Function HTTP::REQUEST-LOCAL-CONTEXT:  remodularize
;;; Function HTTP::%EXECUTE-REQUEST:  fix port confusion when proxying and serving URLs.
;;; Function (CLOS:METHOD HTTP::WRITE-RFC-931-LOGNAME (HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Function HTTP::%EXECUTE-REQUEST:  -
;;; Function (CLOS:METHOD HTTP:REMOVE-ACCESS-LOG (HTTP::PROCESS-QUEUED-LOGGING-MIXIN T) :AFTER):  fix bug.
;;; Function HTTP:DISABLE-HTTP-SERVICE:  -
;;; Function HTTP:ENABLE-HTTP-SERVICE:  -
;;; Function WWW-UTILS::ALLOW-HTTP-SERVICE-ON-PORT:  -
;;; Remove function WWW-UTILS::ENABLE-HTTP-SERVICE: -
;;; Remove function WWW-UTILS::DISABLE-HTTP-SERVICE: -
;;; Function HTTP::WITH-LOCAL-PORT-CONTEXT-IF:  -
;;; Written by JCMa, 12/18/00 17:47:41
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.107,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 54.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.0,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Jcma 42, HTTP Proxy Server 6.12,
;;; HTTP Client Substrate 4.6, Statice Server 466.2, CL-HTTP Documentation 3.0,
;;; Experimental CL-HTTP CLIM User Interface 1.0, HTTP Client 50.1,
;;; Image Substrate 440.4, Essential Image Substrate 433.0,
;;; W4 Constraint-Guide Web Walker 45.8, W4 Examples 15.0, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
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
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.7),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).

;;; Patch file for CL-HTTP version 70.108
;;; Written by JCMa, 12/19/00 00:18:41
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.108,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 54.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.0,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Jcma 42, HTTP Proxy Server 6.13,
;;; HTTP Client Substrate 4.6, Statice Server 466.2, CL-HTTP Documentation 3.0,
;;; Experimental CL-HTTP CLIM User Interface 1.0, HTTP Client 50.2,
;;; Image Substrate 440.4, Essential Image Substrate 433.0,
;;; W4 Constraint-Guide Web Walker 45.9, W4 Examples 15.0, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
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
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.7),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).


(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;HTTP-CONDITIONS.LISP.186"
  "HTTP:SERVER;UTILS.LISP.474"
  "HTTP:SERVER;REPORT.LISP.180"
  "HTTP:SERVER;REPORT.LISP.181"
  "HTTP:SERVER;REPORT.LISP.182"
  "HTTP:SERVER;URL.LISP.425"
  "HTTP:SERVER;SERVER.LISP.883"
  "HTTP:SERVER;LOG.LISP.214"
  "HTTP:LISPM;SERVER;LISPM.LISP.485"
  "HTTP:LISPM;SERVER;LISPM.LISP.486"
  "HTTP:SERVER;UTILS.LISP.476"
  "HTTP:SERVER;SERVER.LISP.884")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.186")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition reportable-condition
                  (http-condition)
  ((url :initform nil :initarg :url :reader http-url)
   (method :initform nil :initarg :method :reader http-method)
   (status-code :reader status-code)
   (reason :initform nil :initarg :reason :reader http-reason)
   (version :initform *http-version* :initarg :version :reader http-version)
   (headers :initform nil :initarg :headers :reader http-transaction-headers)
   (format-string :initform nil :initarg :format-string :reader format-string)
   (format-args :initform nil :initarg :format-args :reader format-args)
   (reporter :initform nil :initarg :reporter :reader http-reporter))
  (:report report-status-message))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.186")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition proxy-access-forbidden
                  (access-forbidden)
  ((reason :initform "Proxy Access Forbidden")
   (reporter :initform 'report-proxy-access-forbidden)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.474")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(define server-mail-address (&optional recompute-p)
  "Returns the mail address for the server maintainer."
  (cond ((and (not recompute-p) *server-mail-address*))
        (t (setq *server-mail-address* (concatenate 'string *server-maintainer* "@" *default-mailer-host*)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.474")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(define write-server-mail-address (stream)
  "Writes the mail address for the server maintainer on STREAM."
  (fast-format stream "mailto:~A" (server-mail-address)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;REPORT.LISP.180")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(defun %report-status-message (url reason stream &optional reporter)
  (with-html-document (:declare-dtd-version-p t :stream stream)
    (with-document-preamble (:stream stream)
      (declare-title reason :stream stream))
    (with-standard-document-body (:stream stream)
      (with-section-heading (reason :stream stream)
        (horizontal-line :stream stream)
        (with-paragraph (:stream stream)
          (cond (reporter
		 (with-verbatim-text (:fresh-line nil :stream stream)
		   (etypecase reporter
		     (string (html:write-string-quoting-specials reporter stream))
		     ((or symbol function) (funcall reporter url stream)))))
                (url (fast-format stream "~A for URI ~A"
				  reason (typecase url
					   (url (url:name-string url))
					   (t url))))
                (t (fast-format stream "~&HTTP Status Code ~D not properly reported.~I~
                                        ~&Please advise the server maintainer at: ~I"
				(server-status *server*) (break-line :stream stream))
		   (note-anchor (server-mail-address) :reference #'write-server-mail-address :stream stream))
		))
        (horizontal-line :stream stream)
        (cl-http-signature stream)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;REPORT.LISP.181")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(defun report-proxy-access-forbidden (url stream)
  (declare (ignore url))
  (let ((server *server*))
    (if server
	(fast-format stream "Proxy Access via ~A on port ~D is not available for your host at ~A."
		     (server-local-host-domain-name server) (SERVER-HOST-LOCAL-PORT server) (server-host-ip-address server))
	(fast-format stream "Proxy Access is not available for your host."))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;REPORT.LISP.182")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(defmethod report-status-message ((condition reportable-condition) stream &optional format-string format-args)
  (let ((reason (or (http-reason condition) (get-string-for-status-code (status-code condition))))
	(url (http-url condition)))
    (unless format-string
      (setq format-string (format-string condition)))
    (unless format-args
      (setq format-args (format-args condition)))
    (cond (format-string
	   (let ((report (apply #'format nil format-string format-args)))
	     (declare (dynamic-extent report))
	     (%report-status-message url reason stream report)))
	  (t (%report-status-message url reason stream (http-reporter condition))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.425")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define-generic directory-context-string (url &optional recompute-p)
  (:documentation "Returns a string consisting of the scheme, host, and path."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.425")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod directory-context-string ((url url:http-url) &optional recompute-p)
  (with-value-cached (url :directory-context-string :recompute-p recompute-p)
    (with-output-to-string (stream)
      (write-scheme-prefix url stream)
      (write-host-port-string url stream)
      (write-path url stream))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.425")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define-generic local-context-string (url &optional recompute-p)
  (:documentation "Returns the local context string consisting of the scheme, host and port."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.425")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod local-context-string ((url url:http-url) &optional recompute-p)
  (with-value-cached (url :local-context-string :recompute-p recompute-p)
    (with-output-to-string (stream)
      (write-scheme-prefix url stream)
      (write-host-port-string url stream))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.425")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(mapc #'(lambda (x) (export (intern x :url) :url)) '("ROOT-CONTEXT-STRING" "DIRECTORY-CONTEXT-STRING"))
;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.883")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

;;;------------------------------------------------------------------- 
;;;
;;; LOGGING ACCESSES
;;;

;; RFC931 logname (ftp://ds.internic.net/rfc/rfc931.txt)
;; local-port,foreign-port:response-type:operating-system:userid 

;; local-port,foreign-port:error:user-not-known

(defmethod write-rfc-931-logname ((server server-logging-mixin) &optional (log-stream *standard-output*))
  (with-slots (stream rfc-931-response-type rfc-931-response) server
    (let ((local-port *standard-http-port*)
          (foreign-port (www-utils:foreign-port stream))
          (response-type (or (server-rfc-931-response-type server) "ERROR"))
          (response (or (server-rfc-931-response server) "USER-NOT-KNOWN")))
      (write foreign-port :stream log-stream :base 10. :escape nil)
      (write-char #\, log-stream)
      (write local-port :stream log-stream :base 10. :escape nil)
      (write-char #\: log-stream)
      (write-string response-type log-stream)
      (write-char #\: log-stream)
      (write-string response log-stream)))) 

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.214")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

;; Make sure these clear the task queue
(defmethod remove-access-log :after ((log process-queued-logging-mixin) port)
  (declare (ignore port))
  (tq:clear-task-queue log))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.485")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defun disallow-http-service-on-port (port)
  (check-type port integer)
  (let ((protocol-map (assoc port tcp::*tcp-protocol-alist*)))
    (cond ((and protocol-map (eq (cdr protocol-map) :http))
	   (setf tcp::*tcp-protocol-alist* (delete protocol-map tcp::*tcp-protocol-alist*)))
	  (t nil))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.485")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defun allow-http-service-on-port (port)
  (check-type port integer)
  (flet ((add-http-to-port (port)		;required because tcp:add-tcp-port-for-protocol 
	   (nconc tcp::*tcp-protocol-alist* (list (cons port :http)))	;allows only one-to-one mapping of ports to protocols
	   ;; put them all on one page
	   (setf tcp::*tcp-protocol-alist* (copy-tree tcp::*tcp-protocol-alist*))))
    (let ((protocol (tcp:tcp-port-protocol-name port nil)))
      (case protocol
	((nil) (add-http-to-port port))
	(:http nil)
	(t (error "The protocol, ~S, is already using the port, ~D." protocol port))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.485")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defun ensure-http-protocol-on-port (port)
  "Ensures that the HTTP protocol is available on PORT."
  (allow-http-service-on-port port))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.485")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(define http:enable-http-service (&key (on-ports http:*standard-http-port*) listeners)
  "Top-level method for starting up HTTP servers."
  (declare (ignore listeners))
  (let ((ports (etypecase on-ports
		 (atom (list on-ports))
		 (cons on-ports))))
    (dolist (port ports)
      (www-utils:ensure-http-protocol-on-port port))
    (unless (neti:service-enabled-p :http)
      (sys:enable-services '(:http) nil))
    on-ports))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.485")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(define http:disable-http-service (&key (on-ports http:*standard-http-port*))
  "Top-level method for shutting down HTTP servers."
  (let ((ports (etypecase on-ports
		 (atom (list on-ports))
		 (cons on-ports))))
    (dolist (port ports)
      (disallow-http-service-on-port port))
    (when (neti:service-enabled-p :http)
      (sys:disable-services '(:http) nil))
    on-ports))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.485")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(SCL:FUNDEFINE 'ENABLE-HTTP-SERVICE)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.485")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(SCL:FUNDEFINE 'DISABLE-HTTP-SERVICE)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.486")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(define http:http-service-enabled-p (&optional (on-ports '(80)))
  "Returns the ports on which HTTP services is enabled or NIL if HTTP is enabled on no ports."
  (flet ((http-available-on-port-p (port)
	   (eq :http (cdr (assoc port tcp::*tcp-protocol-alist*)))))
    (declare (inline http-available-on-port-p))
    (and (etypecase on-ports
	   (atom (http-available-on-port-p on-ports))
	   (cons (loop for port in on-ports
		       thereis (http-available-on-port-p port))))
	 (neti:service-enabled-p :http))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.476")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmacro with-local-port-context-if ((cond port) &body body)
  "When COND evaluates non-null, this conditionally binds the local context
according to PORT within the scope of BODY."
  `(cond (,cond
	  (with-local-port-context (,port) . ,body))
	 (t . ,body)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.884")
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
                 (invoke-server-method server method http-version))))
	 (proxy-intern-url (url-string)
	   (let ((url:*escape-search-urls* nil)	;pass through escaping
		 (url:*search-url-signal-bad-form-encoding* nil))	;pass through broken URL- form encoded URLs.
	     (intern-url url-string :if-does-not-exist *proxy-intern-url-status*))))
    (declare (inline invoke-local-service proxy-intern-url))
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
	     (let ((uri (proxy-intern-url url-string)))  
	       (cond  ;; Actually a local reference, start over as 
		 ((url:local-url-p uri)
		  (let* ((local-port (host-port uri))
			 (same-port-p (eql local-port *standard-http-port*)))
		    (cond ((or same-port-p (http:http-service-enabled-p local-port))
			   (unwind-protect
			       (with-local-port-context-if ((not same-port-p) local-port)
				 (invoke-local-service server url-string method http-version)
				 ;; log the local HTTP service now, and normally log proxy access
				 ;; But don't handle signals here for now 12/19/2000 -- JCMa.
				 (incf (server-requests-completed server))	;increment completed requests
				 (log-access server))	;request is not turned into a relative URI
			     (setf (server-proxy-request-p server) t)))
			  (t (setf (server-proxy-request-p server) t)
			     (error 'service-unavailable :method method :url uri
				    :format-string "HTTP Service is currently unavailable on port ~D" (list local-port))))))
		 ((proxy-service-enabled-p (server-host-local-port server))
		  (setf (server-url server) uri
			(server-proxy-request-p server) t)
		  (invoke-proxy-service server uri method http-version))
		 (t (error 'access-forbidden 
			   :format-string "HTTP Proxy service is currently unavailable on ~A (~D)." 
			   :format-args (list (local-host-domain-name) (server-host-local-port server))
			   :method method :url uri)))))
	    ;; Standard path, call the primary server method.
	    (t (invoke-local-service server url-string method http-version))))))))

