;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.83
;;; Reason: Function URL::DEFAULT-URL-STRING:  new.
;;; Function URL:PARSE-SEARCH-INFO-AS-QUERY-ALIST:  pass in :url-string based on (default-url-string).
;;; Function HTTP::%EXECUTE-REQUEST:  make sure specials are bound when interning URLs for proxy.
;;; Function (CLOS:METHOD HTTP:LOG-ENTRY-WRITER (HTTP::EXTENDED-COMMON-FILE-FORMAT-MIXIN HTTP::SERVER-LOGGING-MIXIN)):  
;;; Spell referer wrong so we are sure to capture the designated header in the log.
;;; Function HTTP::LOG-ENTRY-P:  moved to log.lisp
;;; Function (CLOS:METHOD HTTP::LOG-ENTRY-P (HTTP::FILE-LOGGING-MIXIN HTTP::SERVER-LOGGING-MIXIN) AND):  -
;;; Function (CLOS:METHOD HTTP::LOG-ENTRY-P (HTTP::DYNAMIC-LOGGIN-MIXIN HTTP::SERVER-LOGGING-MIXIN) AND):  -
;;; Written by JCMa, 10/10/00 14:58:40
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.83,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, HTTP Proxy Server 5.34,
;;; HTTP Client Substrate 3.26, Jcma 42, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Documentation Patches 1.0,
;;; Genera 8 5 Clim Patches 1.0, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
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
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.6),
;;; Cname level patch (from W:>reti>cname-level-patch.lisp.5).

;;; Patch file for CL-HTTP version 70.83
;;; Written by JCMa, 10/10/00 19:48:23
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.82,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Documentation Patches 1.0,
;;; Genera 8 5 Clim Patches 1.0, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, HTTP Proxy Server 5.31,
;;; HTTP Client Substrate 3.26, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
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
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.6).




(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;URL.LISP.416"
  "HTTP:SERVER;SERVER.LISP.874"
  "HTTP:SERVER;SERVER.LISP.875"
  "HTTP:SERVER;LOG.LISP.196")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.416")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

;;;------------------------------------------------------------------- 
;;;
;;; TOP LEVEL CONDITIONS
;;;

(defun default-url-string ()
  "Returns a heuristic value for url string.
Used by parsing errors that do not directly know the full URL."
  (and http:*server* (copy-seq (http::server-url-string http:*server*))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.416")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defun parse-search-info-as-query-alist (string &optional (start 0) (end (length string)))
  "Parses the search component of a search URL according to URL encoding rules
for use with form submission via the GET method."
  (declare (values search-alist))
  (flet ((handle-bad-form-encoding (string start end)
	   (if *search-url-signal-bad-form-encoding*
	       (error 'search-url-bad-form-encoding :search-info (subseq string start end)
		      :url-string (default-url-string))
	       (return-from parse-search-info-as-query-alist (parse-search-info string start end)))))
    (declare (inline handle-bad-form-encoding))
    (unless (= start end)
      (with-fast-array-references ((string string string))
	(loop for s1 = start then (1+ (the fixnum e2))
	      while (< s1 end)
	      for e1 = (or (char-position #\= string s1 end)
			   (handle-bad-form-encoding string start end))
	      for s2 = (1+ (the fixnum e1))
	      for e2 = (or (char-position #\& string s2 end) end)
	      for keyword = (http::%tokenize-form-query-keyword string s1 e1)
	      for value = (unless (= s2 e2)
			    (string-unescape-special-chars string s2 e2))
	      collect `(,keyword ,value))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.874")
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
		  (invoke-local-service server url-string method http-version))
		 ((proxy-service-enabled-p (server-host-local-port server))
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
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.875")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod log-entry-writer ((log extended-common-file-format-mixin) (server server-logging-mixin))
  (let* ((host-name (host-log-name server))
	 (user-name (%server-user-qualified-name server))
	 (request (server-request server t))	; don't lose when transaction reset
	 (request-time (server-request-time server))
	 (status (server-status server))
	 (bytes (server-bytes-transmitted server))	;total bytes (not number of bytes in a document)
	 (header-set (server-headers server))
	 user-agent-val referer-val
	 (log-times-in-gmt-p (log-times-in-gmt-p log)))
    (when header-set				;may be null when client drops connection (408)
      (with-header-values (user-agent referer) (server-headers server)
	(setq user-agent-val user-agent
	      referer-val referer)))
    (flet ((write-log-entry (log-stream)
	     (%write-extended-common-logfile-entry
	       host-name request request-time status bytes user-name user-agent-val referer-val log-times-in-gmt-p log-stream #\tab)
	     ;; Trailing CR makes this consistently parsable.
	     (terpri log-stream)))
      #'write-log-entry)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.196")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(define-generic log-entry-p (log agent)
  (:documentation "Returns non-null when LOG should write a log entry for the current state of AGENT.
Methods are combined with AND method combination so that NIL is returned if any method returns NIL.")
  (:method-combination and))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.196")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod log-entry-p and ((log file-logging-mixin) (server server-logging-mixin))
  (log-file-logging log))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.196")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod log-entry-p and ((log dynamic-loggin-mixin) (server server-logging-mixin))
  (log-dynamic-logging log))

