;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Patch-File: T -*-
;;; Patch file for HTTP-PROXY version 6.11
;;; Reason: Function HTTP::PROXY-CACHE-HEADERS-ONLY-RESPONSE:  -
;;; Function HTTP::PROXY-RESPOND-WITH-REMOTE-HEAD-ACCESS:  use proxy-cache-headers-only-response.
;;; Function HTTP::PROXY-RESPOND-WITH-REMOTE-ACCESS:  cache 304 responses when the resource is not already cached.
;;; Written by JCMa, 12/13/00 15:37:13
;;; while running on FUJI-3 from FUJI:/usr/lib/symbolics/Inc-CL-HTTP-70-90-LMFS.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, LMFS 442.1, Color 427.1,
;;; Graphics Support 431.0, Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Color System Documentation 10.0, SGD Book Design 10.0, Images 431.2,
;;; Image Substrate 440.4, CLIM 72.0, Genera CLIM 72.0, CLX CLIM 72.0,
;;; PostScript CLIM 72.0, CLIM Demo 72.0, CLIM Documentation 72.0,
;;; Statice Runtime 466.1, Statice 466.0, Statice Browser 466.0,
;;; Statice Server 466.2, Statice Documentation 426.0, Metering 444.0,
;;; Metering Substrate 444.1, Symbolics Concordia 444.0, Graphic Editor 440.0,
;;; Graphic Editing 441.0, Bitmap Editor 441.0, Graphic Editing Documentation 432.0,
;;; Postscript 436.0, Concordia Documentation 432.0, Joshua 237.4,
;;; Joshua Documentation 216.0, Joshua Metering 206.0, Jericho 237.0, C 440.0,
;;; Lexer Runtime 438.0, Lexer Package 438.0, Minimal Lexer Runtime 439.0,
;;; Lalr 1 434.0, Context Free Grammar 439.0, Context Free Grammar Package 439.0,
;;; C Runtime 438.0, Compiler Tools Package 434.0, Compiler Tools Runtime 434.0,
;;; C Packages 436.0, Syntax Editor Runtime 434.0, C Library Headers 434,
;;; Compiler Tools Development 435.0, Compiler Tools Debugger 434.0,
;;; C Documentation 426.0, Syntax Editor Support 434.0, LL-1 support system 438.0,
;;; Pascal 433.0, Pascal Runtime 434.0, Pascal Package 434.0, Pascal Doc 427.0,
;;; Fortran 434.0, Fortran Runtime 434.0, Fortran Package 434.0, Fortran Doc 427.0,
;;; HTTP Proxy Server 6.10, HTTP Server 70.97, Showable Procedures 36.3,
;;; Binary Tree 34.0, W3 Presentation System 8.1, HTTP Client Substrate 4.3,
;;; HTTP Client 50.1, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Metering Patches 1.0, Genera 8 5 Joshua Patches 1.0,
;;; Genera 8 5 Jericho Patches 1.0, Genera 8 5 Joshua Doc Patches 1.0,
;;; Genera 8 5 Joshua Metering Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.0,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clx Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Clim Demo Patches 1.0, Genera 8 5 Color Patches 1.1,
;;; Genera 8 5 Images Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Genera 8 5 Concordia Patches 1.0,
;;; Genera 8 5 Concordia Doc Patches 1.0, Genera 8 5 C Patches 1.0,
;;; Genera 8 5 Pascal Patches 1.0, Genera 8 5 Fortran Patches 1.0, Mailer 438.0,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.16,
;;; DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x976 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number -2141189522,
;;; Vlm lmfs patch (from W:>Reti>vlm-lmfs-patch.lisp.12),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.7),
;;; Pht debugging patch (from W:>Reti>pht-debugging-patch.lisp.4),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.6).

(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:PROXY;REPRESENTATION.LISP.68"
  "HTTP:PROXY;PROXY-CACHE.LISP.85"
  "HTTP:PROXY;REPRESENTATION.LISP.69"
  "HTTP:PROXY;PROXY-CACHE.LISP.86")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;REPRESENTATION.LISP.68")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun proxy-cacheable-server-response-p (http-status http-version headers)
  "Returns non-null when the origin server HEADERS for HTTP-VERSION indicate that the response is cacheable by a proxy.
Assumes HTTP-STATUS is one of *cacheable-response-status-codes*"
  (unless (member http-version '(:http/0.9 :http/1.0))
    (multiple-value-bind (directive found-p)
	(get-header :cache-control headers)
      (when found-p
	;; Ignore :PUBLIC t because the default is to cache.
	(loop for (key value) on directive by #'cddr
	      do (case key
		   ((:private :no-cache :no-store)
		    (when value (return-from proxy-cacheable-server-response-p nil)))))
	;; Max age directives take precedence over expires
	(let ((max-age (or (getf directive :s-maxage)	;S-MAXAGE overrides MAXAGE
			   (getf directive :maxage))))
	  (declare (fixnum max-age))
	  (when max-age
	    (return-from proxy-cacheable-server-response-p (< 0 *proxy-cache-minimum-expiration-time* max-age)))))))
  ;; work with the the expires and last-modified headers
  (with-header-values (date expires last-modified) headers
    ;; if the date header is missing, we could use local proxy time instead,
    ;; but we might just assume that the origin server is a loser, and not
    ;; trust the rest of there headers. If this turns out to bust the cache
    ;; too much, one can always back off the strategy. 5/23/2000 -- JCMa.
    (cond (expires
	   ;; http 1.1 spec 14.9.3 backward compatibility feature for http 1.0
	   (if (zerop expires)
	       nil
	       (and date (< 0 *proxy-cache-minimum-expiration-time* (- (the integer expires) (the integer date))))))
	  ;; Don't cache status codes whose default is no-cache
	  ((member http-status *response-status-codes-defaultedly-not-cached*) nil)
	  ;; this heuristic expiration time is suggested by the http 1.1 spec in 13.2.4 5/4/2000 -- JCMa.
	  (last-modified
	   (and date (< 0 *proxy-cache-minimum-expiration-time* (floor (- (the integer date) (the integer last-modified)) 10))))
	  (t t))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY-CACHE.LISP.85")
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
	       (write-proxy-request-headers server request-headers http-version stream additional-headers))))
      (declare (dynamic-extent #'%write-request-headers))
      (when *trace-proxy*
	(format *trace-output* "~&;~2TProxy Request Headers:~&")
	(%write-request-headers *trace-output* (server-method server) request-http-version))
      (with-http-request (request-url :get :request-headers #'%write-request-headers)
	(let* ((client-status (client-status client))
	       (client-http-version (client-connection-version client))
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


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY-CACHE.LISP.86")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun proxy-respond-with-remote-head-access (server res rep request-http-version)
  (let* ((request-url (server-url server))
	 (request-headers (server-headers server))
	 (request-stream (server-stream server)))
    (flet ((%write-request-headers (stream method http-version)
	     (declare (ignore method))
	     (write-proxy-request-headers server request-headers http-version stream)))
      (declare (dynamic-extent #'%write-request-headers))
      (when *trace-proxy*
	(format *trace-output* "~&;~2TProxy Request Headers:~&")
	(%write-request-headers *trace-output* (server-method server) request-http-version))
      (with-http-request (request-url :head :request-headers #'%write-request-headers)
	(let* ((client-status (client-status client))
	       (client-http-version (client-connection-version client))
	       (client-response-headers (client-response-headers client)))
	  (when *trace-proxy*
	    (format *trace-output* "~&;Origin Server ~A Status: ~D" client-http-version client-status)
	    (format *trace-output* "~&;Origin Server Headers:~&")
	    (write-header-buffer client-response-headers *trace-output*))
	  (cond ((member client-status *cacheable-response-status-codes*)
		 (send-status-line request-stream (proxy-maybe-downgrade-status-code client-status request-http-version) (client-reason client))
		 (write-proxy-response-headers server client-response-headers client-http-version t request-stream)
		 (when (or (proxy-cacheable-client-request-p http-version request-headers) res)	;if we have it, update it
		   (cond ((proxy-cacheable-server-response-p client-status http-version client-response-headers) 
			  (let ((representation (or rep
						    (intern-representation (intern-resource server (name-string (server-url server))
											    :if-does-not-exist :create)
									   request-headers :if-does-not-exist :create))))
			    (proxy-cache-headers-only-response representation client-response-headers client-status client-http-version request-headers
							       server (server-request-time server))))
			 (rep (unintern-representation rep)))))	;decache when server says no-longer cacheable
		(t (send-status-line request-stream (proxy-maybe-downgrade-status-code client-status request-http-version) (client-reason client))
		   (write-proxy-response-headers server client-response-headers client-http-version t request-stream))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;REPRESENTATION.LISP.69")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod metadata-validates-representation-entity-p ((representation representation) (headers header-set) http-version)
  (macrolet ((test-header-value (header-keyword comparator cached-headers authoritative-p)
	       `(multiple-value-bind (value found-p)
		    (get-header ,header-keyword headers)
		  (when found-p
		    (let ((stored-value (getf ,cached-headers ,header-keyword :not-found)))
		      (unless (eq stored-value :not-found)
			,(if authoritative-p
			     `(return-from metadata-validates-representation-entity-p (,comparator value stored-value))
			     `(unless (,comparator value stored-value)
				(return-from metadata-validates-representation-entity-p nil)))))))))
    (let ((response-headers (representation-response-headers representation)))
      (when response-headers
	(case http-version
	  ((:http/1.0 :http/0.9)
	   (test-header-value :content-length equal response-headers nil)	;heuristic
	   (test-header-value :last-modified equal response-headers nil)	;heuristic
	   (test-header-value :content-md5 equalp response-headers t)	;authoritative
	   t)
	  (t (test-header-value :etag entity-tag-equal response-headers t)	;authoritative
	     (test-header-value :content-length equal response-headers nil)	;heuristic
	     (test-header-value :last-modified equal response-headers nil)	;heuristic
	     (test-header-value :content-md5 equalp response-headers t)	;authoritative
	     t))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY-CACHE.LISP.86")
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
      (representation-update-response-headers representation response-headers)	;update response headers
      (representation-note-transaction representation status http-version current-access-time request-headers)
      ;; note transaction sets revalidation based on different criteria
      (when force-revalidation-p		;newly cached headers do not correspond to the cached entity body
	(setf (representation-entity-invalid-p representation) t))
      (setf (representation-last-reference representation) current-access-time
	    (representation-unsaved-metadata representation) current-access-time)
      (setf (representation-valid-p representation) t))
    (note-metadata-update representation current-access-time)))	;arrange for persistent storage of new metadata

