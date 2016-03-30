;;; -*- Default-character-style: (:FIX :ROMAN :SMALL); Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for HTTP-PROXY version 6.17
;;; Reason: Clean up proxy header management.
;;; 
;;; Convert Cache-control and pragma no-cache as appropriate accross HTTP 1.1 and
;;; HTTP 1.0 protocol levels.  Careful about what request headers are passed
;;; through to the server and about which request headers are stored in the cache.
;;; 
;;; Function HTTP::PROXY-RESPOND-WITH-REMOTE-ACCESS:  -
;;; Function HTTP::PRAGMA-NO-CACHE-P:  new abstraction.
;;; Function HTTP::PROXY-REVALIDATE-CACHE-FOR-CLIENT-P:  use it.
;;; Function HTTP::%WRITE-PROXY-HTTP-1-0-REQUEST-HEADERS:  upgrade pragma header for 1.1 upstream servers.
;;; Function (CLOS:METHOD HTTP::WRITE-PROXY-REQUEST-HEADERS (HTTP::PROXY-SERVER-MIXIN T T T)):  down grade no-cache for http 1.0 servers.
;;; Variable HTTP::*CONDITIONAL-REQUEST-HEADERS*:  new.
;;; Function (CLOS:METHOD HTTP::PROXY-REVALIDATE-REPRESENTATION (HTTP::REPRESENTATION)):  -
;;; Function (CLOS:METHOD HTTP::WRITE-PROXY-REQUEST-HEADERS (HTTP::PROXY-SERVER-MIXIN T T T)):  -
;;; Variable HTTP::*REQUEST-SPECIFIC-REQUEST-HEADERS*:  -
;;; Variable HTTP::*USER-SPECIFIC-REQUEST-HEADERS*:  -
;;; Function HTTP::PROXY-CACHEABLE-REQUEST-HEADER-PLIST:  don't cache superfluous request headers.
;;; Variable HTTP::*PROXY-CACHEABLE-REQUEST-HEADERS*:  Variable allows overriding default caching of request headers.
;;; Function (CLOS:METHOD HTTP::REPRESENTATION-NOTE-TRANSACTION (HTTP::REPRESENTATION T T T)):  smarter request header caching.
;;; Written by JCMa, 3/08/01 19:09:06
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.113,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.25, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.1,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Jcma 42, HTTP Proxy Server 6.16,
;;; HTTP Client Substrate 4.9, Statice Server 466.2,
;;; W4 Constraint-Guide Web Walker 45.10, HTTP Client 50.3, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, Ivory Revision 5, VLM Debugger 329,
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
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.7),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1),
;;; Ansi common lisp as synonym patch (from W:>reti>ansi-common-lisp-as-synonym-patch.lisp.9),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).

;;; Patch file for HTTP-PROXY version 6.17
;;; Written by JCMa, 3/10/01 19:56:13
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.115,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.26, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.1,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Jcma 42, HTTP Proxy Server 6.18,
;;; HTTP Client Substrate 4.9, Statice Server 466.2,
;;; W4 Constraint-Guide Web Walker 45.10, HTTP Client 50.4, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, Ivory Revision 5, VLM Debugger 329,
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
;;; Ansi common lisp as synonym patch (from W:>reti>ansi-common-lisp-as-synonym-patch.lisp.9),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).





(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL)
  (SCT:REQUIRE-PATCH-LEVEL-FOR-PATCH '(CL-HTTP 70. 114.)))


(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:PROXY;VARIABLES.LISP.29"
  "HTTP:PROXY;UTILS.LISP.70"
  "HTTP:PROXY;VARIABLES.LISP.27"
  "HTTP:PROXY;UTILS.LISP.72"
  "HTTP:PROXY;REPRESENTATION.LISP.73"
  "HTTP:PROXY;UTILS.LISP.73"
  "HTTP:PROXY;PROXY-CACHE.LISP.91")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;VARIABLES.LISP.29")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (:load-toplevel :compile-toplevel :execute)

(defconstant *conditional-request-headers* '(:if-match :if-modified-since :if-none-match :if-range :if-unmodified-since)
  "Headers involving some kind of condition response from an origin server.")

;; MS IE Extension headers: (:ua-os :ua-cpu)
(defconstant *request-specific-request-headers* '(:cache-control :date :expect :extension :host :max-forwards :pragma :referer :user-agent :via :ua-os :ua-cpu)
  "Headers associated with a specific request by a user-agent that are not cacheable.")

(defconstant *user-specific-request-headers* '(:authorization :cookie :from)
  "Headers associated with a specific user that involve privacy considerations.")

)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;REPRESENTATION.LISP.73")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun pragma-no-cache-p (request-headers)
  "Returns non-null if the pragma header contains the no-cache directive."
  (multiple-value-bind (directive found-p) 
      (get-header :pragma request-headers)
    (and found-p
	 (etypecase directive
	   (atom (eq :no-cache directive))
	   (cons (member :no-cache directive))))))

(declaim (inline pragma-no-cache-p))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;REPRESENTATION.LISP.73")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define proxy-revalidate-cache-for-client-p (representation client-version &optional (time (get-universal-time)))
  "Returns non-null if a CACHE-CONTROL header contains the no cache directive."
  (or (case client-version
	((:http/0.9 :http/1.0)
	 #+ignore (proxy-trace "~&PROXY-REVALIDATE: Expiration: ~\\time\\ Current: ~\\time\\"
			       (representation-expiration-time representation) time)
	 (< (representation-expiration-time representation) time))	; Standard freshness check.
	;; equivalent to previous netscape lossage for http 1.1
	(t (multiple-value-bind (directive found-p)
	       (get-header :cache-control)
	     (if found-p
		 ;; check cache control parameters
		 (cond ;; forced revalidation by client. :NO-CACHE can force a partial revalidation but we don't implement it.
		   ((getf directive :no-cache) t)
		   ;; Cache expired?
		   ((< (representation-expiration-time representation) time)
		    (let ((max-stale (getf directive :max-stale)))
		      ;; Cache cannot be any staler than MAX-STALE.
		      (if (and max-stale (< (- time (representation-expiration-time representation)) max-stale))
			  nil
			  t)))
		   ;; Cache must be more recent than MAX-AGE
		   ((let ((max-age (getf directive :max-age)))
		      (and max-age
			   (or (zerop max-age)
			       (> max-age (- time (representation-verified-date representation))))))
		    t)
		   ;; Cache must retain at least as much freshness as MIN-FRESH.
		   ((let ((min-fresh (getf directive :min-fresh)))
		      (and min-fresh
			   (< min-fresh (- (representation-expiration-time representation) time))))
		    t)
		   (t nil))
		 ;; Standard freshness check.
		 (< (representation-expiration-time representation) time)))))
      ;; HTTP 1.1 clients can send pragma too.
      (pragma-no-cache-p *headers*)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;UTILS.LISP.70")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: HTTP; Base: 10 -*-")

(defun proxy-header-plist (header-set)
  "Converts HEADER-SET into a property list of (keyword value).
Removes connection level headers"
  (%with-header-set-index (header-set)
    index					;ignore
    (with-fast-array-references ((headers headers vector))
      (loop with key
	    for idx fixnum upfrom 0 below (fill-pointer headers)
	    for header = (aref headers idx)
	    unless (or (%header-suppress-p header)
		       (member (setq key (header-keyword header)) *hop-by-hop-headers*))
	      collect key
	      and collect (header-value header)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;VARIABLES.LISP.27")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defparameter *proxy-cacheable-request-headers* '(:authorization :cookie)
  "Headers that are cacheable even though they have some request or user specificity.
This parameter is adjustable to control the desired level of privacy and security.")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;VARIABLES.LISP.27")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(export (intern "*PROXY-CACHEABLE-REQUEST-HEADERS*" :http) :http)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;UTILS.LISP.72")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: HTTP; Base: 10 -*-")

(defun proxy-cacheable-request-header-plist (header-set &optional include-headers exclude-headers )
  "Converts HEADER-SET into a property list of (keyword value).
Removes all connection-level, request-specific and user-specific headers.
EXCLUDE-HEADERS is a list of additional headers to suppress.
INCLUDE-HEADERS is a list of headers not to suppress, which overrides any other suppression mode."
  (%with-header-set-index (header-set)
    index					;ignore
    (with-fast-array-references ((headers headers vector))
      (loop for idx fixnum upfrom 0 below (fill-pointer headers)
	    for header = (aref headers idx)
	    for header-keyword = (header-keyword header)
	    when (or (member header-keyword include-headers)
		     (not (or (%header-suppress-p header)
			      (member exclude-headers exclude-headers)
			      (member header-keyword '#.`(,@*hop-by-hop-headers*
							  ,@*request-specific-request-headers*
							  ,@*conditional-request-headers*
							  ,@*user-specific-request-headers*)))))
	      collect header-keyword
	      and collect (header-value header)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;REPRESENTATION.LISP.73")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod representation-note-transaction ((representation representation) http-status http-version verified-date
					    &optional request-headers)
  (check-type verified-date integer)
  (flet ((must-revalidate-p (http-version request-headers response-headers)
	   (cond ((or (getf response-headers :set-cookie)	;always revalidate for cookies
		      (get-header :cookie request-headers))
		  (return-from must-revalidate-p t))
		 (t (with-header-values (authorization) request-headers
		      (unless (member http-version '(:http/0.9 :http/1.0))
			(multiple-value-bind (directive found-p)
			    (getf response-headers :cache-control)
			  (when found-p
			    (loop for (key value) on directive by #'cddr
				  when (and (member key '(:must-revalidate :proxy-revalidate)) value)
				    do (return-from must-revalidate-p t))
			    (when authorization	;HTTP 1.1 spec 14.8
			      (let ((s-maxage (getf directive :s-maxage)))
				(return-from must-revalidate-p
				  (cond (s-maxage
					 (if (zerop s-maxage) t nil))
					((or (getf directive :public)
					     (not (getf directive :must-revalidate :not-found)))
					 nil)
					(t t))))))))
		      ;; revalidate any requests with AUTHORIATION, cookie
		      (not (null authorization)))))))
    (declare (inline must-revalidate-p))
    (let ((response-headers (representation-response-headers representation)))
      (when request-headers
	(setf (representation-request-headers representation) (proxy-cacheable-request-header-plist request-headers *proxy-cacheable-request-headers*)
	      ;; assume that revalidation doesn't change when no request headers.
	      (representation-must-revalidate-p representation) (must-revalidate-p http-version request-headers response-headers)))
      (setf (representation-verified-date representation) verified-date
	    (representation-etag representation) (getf response-headers :etag)
	    (representation-last-modification representation) (getf response-headers :last-modified)
	    (representation-http-version representation) http-version)
      (case http-status
	(304 ;; don't set status to 304, but be smart when the first mention is a 304
	  (unless (slot-boundp representation 'http-status)
	    (setf (representation-http-status representation) 200)))
	(t (setf (representation-http-status representation) http-status))))
    ;; Reset expiration times
    (representation-compute-expiration-time-from-headers representation)
    representation))				;return the representation

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;UTILS.LISP.73")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: HTTP; Base: 10 -*-")

;; Should check (server-http-version server) to perform appropriate modifications.
(defun %write-proxy-http-1-0-request-headers (server request-headers request-http-version stream header-plist)
  (declare (ignore server request-http-version))
  (flet ((insert-cache-control-p (client request-headers)
	   (and client
		(pragma-no-cache-p request-headers)
		(http-version-less-p :http/1.0 (client-request-version client))
		(multiple-value-bind (directives found-p)
		    (get-header :cache-control request-headers)
		  (and found-p (not (member :no-cache directives)))))))
    (declare (inline insert-cache-control-p))
    (let* ((new-via (compute-via-header request-headers))
	   (modification-plist `(,.(when (insert-cache-control-p *client* request-headers)	;insert HTTP 1.1 cache control as necessary 3/8/2001 -- JCMa.
				     (list :cache-control (cons :no-cache (get-header :cache-control request-headers))))
				 :via ,new-via
				 ,.header-plist)))
      (declare (dynamic-extent new-via modification-plist))
      (write-modified-headers request-headers stream modification-plist *hop-by-hop-headers* nil nil))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;UTILS.LISP.73")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: HTTP; Base: 10 -*-")

(defmethod write-proxy-request-headers ((server proxy-server-mixin) request-headers request-http-version stream &optional header-plist)
  (declare (ignore request-http-version))
  (flet ((insert-pragma-no-cache-p (client request-headers)
	   (and client
		(http-version-less-p (client-request-version client) :http/1.1)
		(multiple-value-bind (directives found-p)
		    (get-header :cache-control request-headers)
		  (and found-p
		       (member :no-cache directives)))
		(not (pragma-no-cache-p request-headers)))))
    (declare (inline insert-pragma-no-cache-p))
    (let* ((method (server-method server))
	   (new-via (compute-via-header request-headers))
	   (modification-plist `(,.(when (member method '(:post :put))
				     (list :expect '(:100-continue t)))
				 ,.(when (insert-pragma-no-cache-p *client* request-headers)
				     (list :pragma :no-cache))
				 :via ,new-via
				 ,.header-plist)))
      (declare (dynamic-extent new-via modification-plist))
      (write-modified-headers request-headers stream modification-plist *hop-by-hop-headers* nil))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY-CACHE.LISP.91")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod proxy-revalidate-representation ((representation representation) &optional (revalidate-time (get-universal-time)))
  (let ((url (intern-url (uri-string representation) :if-does-not-exist :uninterned))
	(request-headers (representation-request-headers representation)))
    (flet ((%proxy-write-request-headers (stream method http-version)
	     (declare (ignore method http-version))
	     (let* ((origin-server-date (and (representation-valid-p representation) (representation-origin-server-date representation)))
		    (modification-plist `(,.(when origin-server-date
					      `(:if-modified-since ,origin-server-date))
					  :host (,(host-string url) ,(host-port url))
					  :user-agent ,*server-version*)))
	       (declare (dynamic-extent modification-plist))
	       (write-modified-headers request-headers stream modification-plist))))
      (declare (dynamic-extent #'%proxy-write-request-headers))
      (proxy-trace "~&;--------------------~&;Revalidate: ~A" (name-string url))
      (with-http-request  (url :get :request-headers #'%proxy-write-request-headers)
	(let* ((client-status (client-status client))
	       (client-http-version (client-request-version client))
	       (response-headers (client-response-headers client)))
	  (when *trace-proxy*
	    (format *trace-output* "~&;Protocol: ~A~&;Status: ~D~&;Response Headers:~&"
		    client-http-version client-status)
	    (write-header-buffer response-headers *trace-output*))
	  (cond
	    ((member client-status *cacheable-response-status-codes*)
	     ;; uncacheable client requests should never get into the cache. 5/12/2000 -- JCMa.
	     (cond ((proxy-cacheable-server-response-p client-status http-version response-headers)
		    ;; if someone else gets this write lock first we just refetch 5/5/2000 -- JCMa.
		    (with-lock-held ((cache-object-lock representation) :write "Cache Entity")	
		      (proxy-trace "~&;Caching data for ~S" representation)
		      (setf (representation-valid-p representation) nil
			    (representation-entity-invalid-p representation) t)
		      (handling-optional-entity-for-status-code
			(client-status response-headers
				       :clean-up-form (%representation-clean-up-incomplete-cache-of-entity representation response-headers))
			(with-binary-stream (remote-stream :input)
			  (with-entity-data-stream (entity-stream representation :output)
			    (with-transfer-decoding* (remote-stream url http-version :headers response-headers :copy-mode :binary)
			      (stream-copy-until-eof remote-stream entity-stream :binary)))
			  (setf (representation-entity-invalid-p representation) (not (representation-entity-handle-valid-p representation)))))
		      ;; update date after successful cache.
		      (setf (representation-response-headers representation) (proxy-header-plist response-headers))	;set response headers
		      (representation-note-transaction representation client-status http-version revalidate-time)
		      (setf (representation-last-update representation) revalidate-time)
		      ;; If new cache entry, set the last reference to the creation date
		      (unless (representation-last-reference representation)
			(setf (representation-last-reference representation) (cache-object-creation-date representation)))
		      (setf (representation-unsaved-metadata representation) revalidate-time)
		      ;; the last form must set the validity of representation
		      (setf (representation-valid-p representation) t))
		    ;; Arrange for persistent storage of new metadata outside of write lock
		    (note-metadata-update representation revalidate-time))
		   (t (proxy-trace "~&;Revalidate: Ignoring and decaching uncacheable entity for status ~S response." client-status)
		      (with-transfer-decoding* (remote-stream url http-version :headers *headers*)
			(advance-input-buffer remote-stream (get-header :content-length response-headers)))
		      (unintern-representation representation))))	;remove uncacheable representation
	    ((eql client-status 304)
	     (proxy-trace "~&;Revalidate: Caching metadata for status ~S response." client-status)
	     (let (verified-date)
	       (with-lock-held ((cache-object-lock representation) :write "Cache Metadata")
		 ;; Bail out if more recent verification
		 (unless (and (setq verified-date (representation-verified-date representation))
			      (> revalidate-time verified-date))
		   (return-from proxy-revalidate-representation))
		 (setf (representation-valid-p representation) nil)
		 (representation-update-response-headers representation response-headers)	;update response headers
		 (representation-note-transaction representation client-status client-http-version revalidate-time)
		 (setf (representation-unsaved-metadata representation) revalidate-time)
		 (setf (representation-valid-p representation) t))
	       (note-metadata-update representation revalidate-time)))	;arrange for persistent storage of new metadata
	    (t (when (response-status-code-implies-entity-p client-status)
		 (with-transfer-decoding* (remote-stream url http-version :headers *headers*)
		   (advance-input-buffer remote-stream (get-header :content-length response-headers))))
	       (case client-status
		 ((401 402 403 404)
		  (proxy-trace "~&;Revalidate: Ignoring metadata for status ~S response." client-status))
		 ((400)
		  (proxy-trace "~&;Revalidate: Broken request syntax ~S response.~&URL: ~S"
			       client-status (name-string url)))
		 (t (client-signal-http-code url client-status :get :headers response-headers
					     :reason (client-reason client) :version client-http-version))))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY-CACHE.LISP.91")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

; Abstract this further when incorporating additional HTTP/1.1 functionality.
(defun proxy-respond-with-remote-access (server res rep request-http-version)
  (let* ((request-url (server-url server))
	 (request-headers (server-headers server))
	 (request-stream (server-stream server)))
    (flet ((%write-request-headers (stream method http-version)
	     (declare (ignore method))
	     ;; No conditional GET if inconsistent representation 5/5/2000 -- JCMa.
	     (let ((additional-headers (when (and rep
						  (representation-valid-p rep)
						  (not (representation-entity-invalid-p rep)))
					 (proxy-trace "~&;Sending conditional request to origin server for ~S." rep)
					 `(:if-modified-since ,(representation-origin-server-date rep)))))
	       (declare (dynamic-extent additional-headers))
	       (with-suppressed-headers (*conditional-request-headers*)
		 (write-proxy-request-headers server request-headers http-version stream additional-headers)))))
      (declare (dynamic-extent #'%write-request-headers))
      (with-http-request (request-url :get :request-headers #'%write-request-headers)
	(let* ((client-status (client-status client))
	       (client-http-version (client-request-version client))
	       (client-response-headers (client-response-headers client)))
	  (when *trace-proxy*
	    (format *trace-output* "~&;Origin Server ~A Status: ~D" client-http-version client-status)
	    (format *trace-output* "~&;Origin Server Headers:~&")
	    (write-header-buffer client-response-headers *trace-output*))
	  (cond ((member client-status *cacheable-response-status-codes*)	;206 requires more general handling of range requests
		 (let* ((response-content-length (get-header :content-length client-response-headers))
			(transfer-encoding (proxy-relay-transfer-encoding response-content-length request-http-version))
			(additional-headers (case transfer-encoding
					      (:chunked '(:transfer-encoding :chunked))))
			cacheable-request-p)
		   (declare (dynamic-extent additional-headers))
		   (send-status-line request-stream (proxy-maybe-downgrade-status-code client-status http-version) (client-reason client))
		   (write-proxy-response-headers server client-response-headers client-http-version response-content-length
						 request-stream additional-headers)
		   (cond ((and (setq cacheable-request-p (proxy-cacheable-client-request-p http-version request-headers))
			       (proxy-cacheable-server-response-p client-status http-version client-response-headers))
			  (proxy-respond-while-caching-entity res request-url request-headers client-response-headers
							      client-status client-http-version remote-stream request-stream transfer-encoding))
			 (t (handling-optional-entity-for-status-code
			      (client-status client-response-headers :clean-up-form (setf (server-close-connection-p server) t))
			      (with-binary-stream (request-stream :output)
				(with-binary-stream (remote-stream :input)
				  (with-transfer-encoding (request-stream transfer-encoding)
				    (with-transfer-decoding* (remote-stream request-url client-http-version
									    :headers client-response-headers :copy-mode :binary)
				      (stream-copy-until-eof remote-stream request-stream :binary))))))
			    ;; Decache representation if it has become uncacheable according to the origin server
			    (when (and rep cacheable-request-p)	;implies response not cacheable
			      (unintern-representation rep))))))
		((eql client-status 304)
		 (cond (rep
			(proxy-trace "~&;Serving cached data ~S" rep)
			(block update-metadata
			  (let ((current-access-time (server-request-time server))
				verified-date)
			    (with-lock-held ((cache-object-lock rep) :write "Cache Metadata")
			      ;; Bail out if more recent verification
			      (when (and (setq verified-date (representation-verified-date rep))
					 (> verified-date current-access-time))
				(return-from update-metadata))
			      (setf (representation-valid-p rep) nil)
			      (representation-update-response-headers rep client-response-headers)	;update response headers
			      (representation-note-transaction rep client-status client-http-version current-access-time request-headers)
			      (setf (representation-last-reference rep) current-access-time
				    (representation-unsaved-metadata rep) current-access-time)
			      (setf (representation-valid-p rep) t))
			    (note-metadata-update rep current-access-time)))	;arrange for persistent storage of new metadata
			;; Respond with coherent data
			(with-lock-held ((cache-object-lock rep) :read "Read Metadata")
			  (proxy-respond-handling-conditional-get server rep request-http-version))
			(server-note-proxy-cache-hit server))
		       (t (send-status-line request-stream (proxy-maybe-downgrade-status-code client-status http-version) (client-reason client))
			  (write-proxy-response-headers server client-response-headers client-http-version t request-stream)
			  (proxy-trace "~&;Relaying headers for status ~S response." client-status)
			  (when (and (proxy-cacheable-client-request-p http-version request-headers)
				     (proxy-cacheable-server-response-p client-status http-version client-response-headers))
			    (let* ((resource (intern-resource server (name-string (server-url server)) :if-does-not-exist :create))
				   (representation (intern-representation resource request-headers :if-does-not-exist :create)))
			      (proxy-trace "~&;Caching headers for status ~S response." client-status)
			      (proxy-cache-headers-only-response representation client-response-headers 200 client-http-version request-headers
								 server (server-request-time server)))))))
		((response-status-code-requires-no-entity-p client-status)
		 (send-status-line request-stream (proxy-maybe-downgrade-status-code client-status http-version) (client-reason client))
		 (write-proxy-response-headers server client-response-headers client-http-version t request-stream)
		 (proxy-trace "~&;Relaying headers for status ~S response." client-status))
		(t (proxy-trace "~&;Relaying entity for status ~S response." client-status)
		   (send-status-line request-stream (proxy-maybe-downgrade-status-code client-status http-version) (client-reason client))
		   (let* ((response-content-length (get-header :content-length client-response-headers))
			  (transfer-encoding (proxy-relay-transfer-encoding response-content-length request-http-version))
			  (additional-headers (case transfer-encoding
						(:chunked '(:transfer-encoding :chunked)))))
		     (declare (dynamic-extent additional-headers))
		     (write-proxy-response-headers server client-response-headers client-http-version response-content-length
						   request-stream additional-headers)
		     (handling-optional-entity-for-status-code
		       (client-status client-response-headers :clean-up-form (setf (server-close-connection-p server) t))
		       (with-binary-stream (request-stream :output)
			 (with-binary-stream (remote-stream :input)
			   (with-transfer-encoding (request-stream transfer-encoding)
			     (with-transfer-decoding* (remote-stream request-url client-http-version :headers client-response-headers :copy-mode :binary)
			       (stream-copy-until-eof remote-stream request-stream :binary))))))))))))))

