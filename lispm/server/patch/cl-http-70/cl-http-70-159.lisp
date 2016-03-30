;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.159
;;; Reason: Function URL:NUMERIC-HOSTNAME-P:  faster.
;;; Function URL::CHECK-HOST-STRING:  faster.
;;; Function URL::GET-HOST-PORT-INFO:  faster.
;;; Function HTTP::%EXECUTE-REQUEST:  fix signalling arg bug.
;;; Written by JCMa, 2/25/03 18:44:38
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5-inc1.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.6,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.158,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.4, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.5, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.40, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Documentation Patches 1.0,
;;; Genera 8 5 Clim Patches 1.3, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Jcma 44, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.18, DEC OSF/1 V4.0 (Rev. 110),
;;; 1580x1128 24-bit TRUE-COLOR X Screen FUJI:2.0 with 224 Genera fonts (The Olivetti & Oracle Research Laboratory R3323),
;;; Machine serial number -2141194968,
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;URL.LISP.429"
  "HTTP:SERVER;SERVER.LISP.920")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.429")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defun numeric-hostname-p (hostname &optional (start 0) (end (length hostname)))
  (flet ((numeric-host-char-p (char)
	   (or (digit-char-p char)
	       (char= char #\.))))
    (declare (inline numeric-host-char-p))
    (with-fast-array-references ((hostname hostname string))
      (loop for idx upfrom start below end
	    for char = (aref hostname idx)
	    unless (numeric-host-char-p char)
	      do (return nil)
	    finally (return t)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.429")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defun check-host-string (host url-string start end &aux (len (length host)))
  (declare (values check-host-string numeric-p)
	   (optimize (speed 3)))
  (flet ((bad-chars-in-host-p (host start end)
	   (with-fast-array-references ((host host string))
	     (loop with bad-chars = *bad-internet-hostname-characters*
		   for idx upfrom start below end
		   for char = (aref host idx)
		   when (member char bad-chars :test #'char-equal)
		     do (return t)
		   finally (return nil)))))
    (declare (inline bad-chars-in-host-p))
    (cond ((numeric-hostname-p host 0 len)
	   (values host t))
	  ((bad-chars-in-host-p host 0 len)
	   (error 'bad-host-name-specification :host host
		  :url-string (maybe-trim-url-string url-string start end)))
	  ((char-position #\. host 0 len t)     ; qualified domain name
	   (values host nil))
	  (t (cond (*qualify-unqualified-domain-names*
		    (values (http:qualify-domain-name host) nil))
		   #+(or Allegro CCL-5.0 LispWorks)
		   ((equalp host "localhost") (values host nil))
		   (t (error 'bad-host-name-specification :host host
			     :url-string (maybe-trim-url-string url-string start end))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.429")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defun get-host-port-info (url-string &optional (start 0) end no-delimiter-search-p &aux s e)
  "When no-delimiter-search-p is non-null, parse assumes START and END delimit host-port specification."
  (declare (values host-string port-number start-index)
	   (fixnum start)
	   (optimize (speed 3)))
  (cond
    ((if no-delimiter-search-p
         ;; pregiven, so use them
         (and (setq s start)
	      (setq e end))
         ;; find hostport delimiters
         (and (setq s (string-search= "//" url-string 0 2 start end))
	      (setq e (or (char-position #\/ url-string (setq s (the fixnum (+ 2 s))) end) end))))
     (multiple-value-bind (port host-end)
	 (get-port-info url-string s e)
       (let ((host (subseq url-string s host-end)))
	 ;; return values
	 ;; check the hostname for syntactic well-formedness and qualify unqualified domain names.
	 (values (check-host-string host url-string start end)
		 port (the fixnum (1+ e))))))
    (t (error 'no-host-found :url-string (maybe-trim-url-string url-string start end)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.920")
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
	     (intern-url url-string :if-does-not-exist *proxy-intern-url-status*)))
	 (nrelativize-url (string)
	   (url::with-relative-name-string-indices (string 0 (length string))
	     (let ((new-length (the fixnum (- end start))))
	       (copy-vector-portion string start end string 0 new-length)	;works because we copy down
	       (setf (fill-pointer string) new-length))
	     string)))
    (declare (inline invoke-local-service proxy-intern-url nrelativize-url))
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
				 (invoke-local-service server (nrelativize-url url-string) method http-version)
				 ;; log the local HTTP service now, and normally log proxy access
				 ;; But don't handle signals here for now 12/19/2000 -- JCMa.
				 (incf (server-requests-completed server))	;increment completed requests
				 (log-access server))	;request is not turned into a relative URI
			     (setf (server-proxy-request-p server) t)))
			  (t (setf (server-proxy-request-p server) t)
			     (error 'service-unavailable :method method :url uri
				    :format-string "HTTP Service is currently unavailable on port ~D" 
				    :format-args (list local-port))))))
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

