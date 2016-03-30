;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for W4 version 45.8
;;; Reason: Provide resource body caching.
;;; 
;;; Function (CLOS:METHOD HTTP::CHUNKED-INPUT-CAPTURE (T T T)):  use resources.
;;; Function HTTP::%GET-URL-HEADERS-AND-BODY:  use resources.
;;; Function (CLOS:METHOD W4::CLEAR-WALKER-CACHE (URL:URL)):  -
;;; Function HTTP::WITHOUT-WEB-WALKER-CACHE-RESOURCE-EXCEPTIONAL-LEAKS:  -
;;; Function (CLOS:METHOD HTTP::CHUNKED-INPUT-CAPTURE (T T T)):  -
;;; Function HTTP::%GET-URL-HEADERS-AND-BODY:  -
;;; Function (CLOS:METHOD W4::FETCH-RESOURCE-CONTENT (W4::ACTIVITY URL:HTTP-URL T T)):  -
;;; Variable HTTP::*DEFAULT-WEB-WALKER-CACHE-ARRAY-SIZE*:  new.
;;; Function HTTP::MATCH-WEB-WALKER-CACHE-ARRAY-P:  -
;;; Written by JCMa, 12/16/00 17:49:20
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.104,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42,
;;; Experimental Genera 8 5 Patches 1.0, Genera 8 5 System Patches 1.21,
;;; Genera 8 5 Mailer Patches 1.1, Genera 8 5 Joshua Patches 1.0,
;;; Genera 8 5 Statice Runtime Patches 1.0, Genera 8 5 Statice Patches 1.0,
;;; Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.0,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, HTTP Proxy Server 6.11,
;;; HTTP Client Substrate 4.4, Statice Server 466.2, HTTP Client 50.1,
;;; Image Substrate 440.4, Essential Image Substrate 433.0,
;;; W4 Constraint-Guide Web Walker 45.7, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
;;; 1024x718 24-bit TRUE-COLOR X Screen JCMA-ISDN:0.0 with 224 Genera fonts (eXodusPowerPC 7.0  (c) 1998 White Pine Software,
;;; Inc. R6300),
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
  "HTTP:CLIENT;W4-CLIENT.LISP.167"
  "HTTP:CLIENT;W4-CLIENT.LISP.168"
  "HTTP:W4;WALKER.LISP.103"
  "HTTP:CLIENT;W4-CLIENT.LISP.169"
  "HTTP:W4;WALKER.LISP.104"
  "HTTP:CLIENT;W4-CLIENT.LISP.170"
  "HTTP:CLIENT;W4-CLIENT.LISP.171"
  "HTTP:CLIENT;W4-CLIENT.LISP.172"
  "HTTP:CLIENT;W4-CLIENT.LISP.174")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;W4-CLIENT.LISP.167")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(define clear-web-walker-cache-array-resource ()
  (clear-resource 'web-walker-cache-array))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;W4-CLIENT.LISP.168")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun deallocate-web-walker-cache-array (array)
  (setf (fill-pointer array) 0)
  (deallocate-resource 'web-walker-cache-array array))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;W4-CLIENT.LISP.168")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;; this must allocate and deallocate rapidly
(defresource web-walker-cache-array (element-type size)
  :constructor make-web-walker-cache-array
  :matcher match-web-walker-cache-array-p)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:W4;WALKER.LISP.103")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: w4; -*-")

(defmethod clear-walker-cache ((url url))
  ;; deallocate any retained headers
  (multiple-value-bind (headers found-p)
      (get-value url :headers)
    (when (and found-p headers)
      (deallocate-resource 'http::header-set headers)))
  (multiple-value-bind (array found-p)
      (get-value url :content)
    (when (and found-p array)
      (http::deallocate-web-walker-cache-array array)))
  ;;clear URL cache.
  (dolist (key *cache-indicators*)
    (remove-value url key))
  url)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;W4-CLIENT.LISP.169")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmacro without-web-walker-cache-resource-exceptional-leaks ((var) &body body)
  "Intercepts exceptional conditions and deallocates the resource bound to VAR."
  `(let ((,var nil))
     (flet ((maybe-deallocate-cache-resource (condition)
	      (declare (ignore condition))
	      (when ,var
		(deallocate-web-walker-cache-array ,var)
		(setq ,var nil))))
       (declare (dynamic-extent #'maybe-deallocate-cache-resource))
       (handler-bind
	 ((condition #'maybe-deallocate-cache-resource))
	 ,@body))
     ,var))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:W4;WALKER.LISP.104")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: w4; -*-")

(defmethod fetch-resource-content ((activity activity) (url http-url) headers report-stream)
  (declare (values content headers status-code redirection retrieved-from-cache-p))
  (macrolet ((maybe-dellocate-content-array (content)
	       `(when ,content
		  (http::deallocate-web-walker-cache-array ,content)
		  (setq ,content nil))))
    (let ((http::*standard-proxy* (activity-proxy activity))
	  (outgoing-headers `(,@*standard-get-robot-headers* ,@(robot-headers activity) ,.headers)))
      (declare (dynamic-extent outgoing-headers))
      (handler-case-if (not *debug-walker*) 
	 (multiple-value-bind (body headers status-code redirection http-version)
	     (http:get-url-headers-and-body url outgoing-headers report-stream)
	   (cond ((null status-code)
		  (setf (get-value url :content-status-code) 500)
		  (maybe-dellocate-content-array body)
		  (abort-activity-on-resource))
		 ((< 199 status-code 300)
		  (setf (get-value url :content) body
			(get-value url :headers) headers
			(get-value url :content-status-code) status-code
			(get-value url :http-version) http-version)
		  (when redirection
		    (setf (get-value url :redirection) redirection)))
		 (t (setf (get-value url :content-status-code) status-code)
		    (maybe-dellocate-content-array body)
		    (abort-activity-on-resource)))
	   ;; return values
	   (values body headers status-code redirection nil http-version))
	(host-not-responding (err)
			     (record-url-note activity url :error-getting-content (report-string err))
			     (values nil nil 504))
	(http::reportable-condition
	  (cond)
	  (record-url-note activity url :http-condition-getting-content (report-string cond))
	  (values nil nil (http::status-code cond)))
	(url::url-condition
	  (cond)
	  (record-url-note activity url :url-condition-getting-headers (report-string cond))
	  (values nil 400))
	(error (err)
	       (record-url-note activity url :error-getting-content (report-string err))
	       (values nil nil 500))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;W4-CLIENT.LISP.170")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod chunked-input-capture (stream copy-mode headers)
  (declare (values vector))
  (without-web-walker-cache-resource-exceptional-leaks (vector)
    (with-chunked-transfer-decoding (stream :headers headers)
      (ecase copy-mode
	((:text :crlf)
	 (setq vector (allocate-web-walker-cache-array *standard-character-type*))
	 (crlf-stream-copy-into-string stream nil 0 vector))
	(:binary
	  (with-binary-stream (stream :input)
	    (setq vector (allocate-web-walker-cache-array '(unsigned-byte 8)))
	    (binary-stream-copy-into-8-bit-array stream nil 0 vector)))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;W4-CLIENT.LISP.171")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun %get-url-headers-and-body (url headers report-stream authorization)
  (flet ((standard-capture (stream copy-mode length)
	   (without-web-walker-cache-resource-exceptional-leaks (vector)
	     (ecase copy-mode
	       ((:text :crlf)
		(setq vector (allocate-web-walker-cache-array *standard-character-type* length))
		(crlf-stream-copy-into-string stream length))
	       (:binary
		 (with-binary-stream (stream :input)
		   (setq vector (allocate-web-walker-cache-array '(unsigned-byte 8) length))
		   (binary-stream-copy-into-8-bit-array stream length)))))))
    (declare (inline standard-capture))
    (handling-redirects (url)
      (with-http-request (url :get 
			      :request-headers (compute-standard-request-headers
						 url :authorization authorization :header-plist headers
						 :user-agent (if (getf headers :user-agent) nil *server-version*)))
	(let ((status (client-status client))
	      (http-version (client-connection-version client))
	      (response-headers (client-response-headers client))
	      response-body redirection)
	  (case status
	    ((200 205 206)
	     (let* ((content-type (get-header :content-type response-headers))
		    (copy-mode (mime-content-type-copy-mode content-type))
		    (content-length (get-header :content-length response-headers)))
	       (setq response-body (cond ((or content-length (member http-version '(:http/1.0 :http/0.9)))
					  (standard-capture remote-stream copy-mode content-length))
					 (t (let ((transfer-encoding (get-header :transfer-encoding response-headers)))
					      (case transfer-encoding
						(:chunked (chunked-input-capture remote-stream copy-mode response-headers))
						((nil)
						 (error 'bad-syntax-provided :url url :method :get
							 :format-string "No content length header was provided."))
						(t (error 'server-not-implemented :close-connection t :url url :method :get
							  :format-string "The HTTP transfer decoding, ~A, is not implemented."
							  :format-args (list transfer-encoding))))))))))
	    ((201 202 203 204))
	    ((300 402 403 405 406 407 415))
	    ((301 302)
	     (let ((alternate-urls (mapcar #'url:intern-url (ensure-list (or (get-header :location response-headers) 
									     (get-header :content-location response-headers))))))
	       (flush-input-entity remote-stream response-headers http-version)
	       (push alternate-urls redirection)
	       (signal (ecase status
			 (301 'document-moved-permanently)
			 (302 'document-moved-temporarily))
		       :new-urls alternate-urls :version http-version)))
	    ;; do something about authentication -- JCMa 12/10/1996.
	    (401 (destructuring-bind (&optional authentication-method . realm) (get-header :WWW-Authenticate response-headers)
		   (declare (ignore authentication-method realm))
		   nil))
	    (404
	      (when *debug-client*
		(fresh-line report-stream)
		(%write-common-logfile-entry (host-string url) (concatenate 'string (url:name-string url) " GET")
					     status 0 "-" *log-times-in-gmt* report-stream)))
	    ((nil) (setq status 408))		; didn't return a status code
	    ((408 411 414 500 501 502 503 504 505))
	    (t (client-signal-http-code url status :get :headers response-headers :reason (client-reason client) :version http-version)))
	  (values response-body (durable-response-headers client) status redirection http-version))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;W4-CLIENT.LISP.172")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defconstant *web-walker-cache-array-size-hysterisis* .10
  "The maximum fraction of space that can be wasted when allocating a cache array.")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;W4-CLIENT.LISP.172")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun match-web-walker-cache-array-p (resource array element-type size)
  (declare (ignore resource))
  (and (equal element-type (array-element-type array))
       (let ((array-size (array-total-size array)))
	 (declare (fixnum size array-size))
	 (and (<= size array-size)		;fit inside array?
	      ;; within hysterisis -- maintain resource distribution & frequency
	      (< (float (/ size array-size)) *web-walker-cache-array-size-hysterisis*)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;W4-CLIENT.LISP.172")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defparameter *web-walker-cache-default-array-size* 1000
  "The default size the cache arrays when nospecific size is known.")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;W4-CLIENT.LISP.172")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun allocate-web-walker-cache-array (element-type &optional size)
  (allocate-resource 'web-walker-cache-array element-type (or size *web-walker-cache-default-array-size*)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;W4-CLIENT.LISP.174")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;;;------------------------------------------------------------------- 
;;;
;;; WEB WALKER RESOURCES
;;;

(defun make-web-walker-cache-array (resource element-type size)
  (declare (ignore resource))
  (make-array size :element-type element-type :adjustable t :fill-pointer 0))

