;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for HTTP-PROXY version 6.33
;;; Reason: Function HTTP::PROXY-CACHE-HEADERS-ONLY-RESPONSE:  pass status into representation-update-response-headers
;;; Function HTTP::PROXY-RESPOND-WITH-REMOTE-ACCESS:  ditto
;;; Function (CLOS:METHOD HTTP::PROXY-REVALIDATE-REPRESENTATION (HTTP::REPRESENTATION)):  ditto
;;; Function HTTP::CACHEABLE-RESPONSE-HEADER-P:  add status argument.
;;; Remove function HTTP::REPRESENTATION-UPDATE-RESPONSE-HEADERS: undefine
;;; Function HTTP::REPRESENTATION-UPDATE-RESPONSE-HEADERS:  add status argument.
;;; Function (CLOS:METHOD HTTP::REPRESENTATION-UPDATE-RESPONSE-HEADERS (HTTP::REPRESENTATION HTTP::HEADER-SET T)):  don't update content length or type on conditional gets.
;;; Written by JCMa, 10/07/03 13:07:06
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5-inc1.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.173,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.4, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.5, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.41, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Documentation Patches 1.0,
;;; Genera 8 5 Clim Patches 1.3, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; Jcma 44, HTTP Proxy Server 6.32, HTTP Client Substrate 4.22, HTTP Client 51.4,
;;; Image Substrate 440.4, Essential Image Substrate 433.0, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.18, DEC OSF/1 V4.0 (Rev. 110),
;;; 1560x1120 24-bit TRUE-COLOR X Screen FUJI:1.0 with 224 Genera fonts (The Olivetti & Oracle Research Laboratory R3323),
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
  "HTTP:PROXY;REPRESENTATION.LISP.78"
  "HTTP:PROXY;REPRESENTATION.LISP.80"
  "HTTP:PROXY;PROXY-CACHE.LISP.95")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;REPRESENTATION.LISP.78")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

#+Genera
(eval-when (compile eval load)
(SCL:FUNDEFINE 'REPRESENTATION-UPDATE-RESPONSE-HEADERS))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;REPRESENTATION.LISP.78")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defgeneric representation-update-response-headers (representation response-headers status)
  (:documentation "Merges any missing headers from old-headers into new-headers when recording server response headers for the proxy cache."))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;REPRESENTATION.LISP.80")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod representation-update-response-headers ((representation representation) (response-headers header-set) status)
  (let ((old-headers (representation-response-headers representation))
	(exclude-headers (case status ;;Don't update key entity headers on conditional get for robustness against broken proxies or servers
			   (304 '#.(list* :content-length :content-type *hop-by-hop-headers*))
			   (t nil))))
    (flet ((update-header1 (header header-object)
	     (when (cacheable-response-header-p header)
	       (setf (getf old-headers header) (header-value header-object))))
	   (update-header2 (header header-object)
	     (unless (member header exclude-headers)
	       (setf (getf old-headers header) (header-value header-object)))))
      (declare (dynamic-extent #'update-header1 #'update-header2))
      (map-headers (if exclude-headers #'update-header2 #'update-header1) response-headers)
      (setf (representation-response-headers representation) old-headers))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY-CACHE.LISP.95")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun proxy-cache-headers-only-response (representation response-headers status http-version request-headers server current-access-time)
  (let (force-revalidation-p verified-date)
    (with-lock-held ((cache-object-lock representation) :write "Cache Metadata")
      ;; Bail out if more recent verification
      (when (and (setq verified-date (representation-verified-date representation))
		 (> verified-date current-access-time))
	(server-note-proxy-cache-hit server)
	(return-from proxy-cache-headers-only-response))
      ;; Update metadata
      (setf (representation-valid-p representation) nil)
      ;; must be first while previous metadata remains in place
      (unless (representation-entity-invalid-p representation)
	(if (metadata-validates-representation-entity-p representation response-headers http-version)
	    (server-note-proxy-cache-hit server)	;we just revalidated a cache entry
	    (setq force-revalidation-p t)))
      ;; Update metadata
      (representation-update-response-headers representation response-headers status)	;update response headers
      (representation-note-transaction representation status http-version current-access-time request-headers)
      ;; note transaction sets revalidation based on different criteria
      (when force-revalidation-p		;newly cached headers do not correspond to the cached entity body
	(setf (representation-entity-invalid-p representation) t))
      (setf (representation-last-reference representation) current-access-time
	    (representation-unsaved-metadata representation) current-access-time)
      (setf (representation-valid-p representation) t))
    (note-metadata-update representation current-access-time)))	;arrange for persistent storage of new metadata


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY-CACHE.LISP.95")
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
			      (representation-update-response-headers rep client-response-headers client-status)	;update response headers
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


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY-CACHE.LISP.95")
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
		      (setf (representation-response-headers representation) (proxy-cacheable-response-header-plist response-headers))	;set response headers
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
		 (representation-update-response-headers representation response-headers client-status)	;update response headers
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
