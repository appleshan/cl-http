;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for HTTP-PROXY version 6.18
;;; Reason: Improve header caching
;;; 
;;; Function HTTP::PROXY-CACHEABLE-REQUEST-HEADER-PLIST:  speed up.
;;; Function HTTP::PROXY-CACHEABLE-RESPONSE-HEADER-PLIST:  -
;;; Function HTTP::PROXY-RESPOND-WHILE-CACHING-ENTITY:  use proxy-cacheable-response-header-plist
;;; Function (CLOS:METHOD HTTP::PROXY-REVALIDATE-REPRESENTATION (HTTP::REPRESENTATION)):  ditto.
;;; Remove function HTTP::PROXY-HEADER-PLIST: undefine.
;;; Function HTTP::CACHEABLE-RESPONSE-HEADER-P:  -
;;; Function HTTP::CACHEABLE-REQUEST-HEADER-P:  -
;;; Function (CLOS:METHOD HTTP::REPRESENTATION-UPDATE-RESPONSE-HEADERS (HTTP::REPRESENTATION HTTP::HEADER-SET)):  only update cacheable response headers.
;;; 
;;; 
;;; Function HTTP::CACHE-OBJECT-LAST-REFERENCE-DATE:  -
;;; Function (CLOS:METHOD HTTP::CACHE-OBJECT-LAST-REFERENCE-DATE (HTTP::ENTITY-TRANSACTION-MIXIN)):  -
;;; Function (CLOS:METHOD HTTP::CACHE-OBJECT-LAST-REFERENCE-DATE (HTTP::RESOURCE)):  -
;;; Function (CLOS:METHOD HTTP::RESPOND-TO-SHOW-RESOURCE (URL:HTTP-SEARCH T)):  
;;; lose infinitely slow sorting of resources and display in reference order.
;;; Function (CLOS:METHOD HTTP::RESPOND-TO-SHOW-REPRESENTATION (URL:HTTP-SEARCH T)):  fix bugs.
;;; Function HTTP:EXPORT-PROXY-INTERFACES:  export using correct search parser/writer.
;;; Function (CLOS:METHOD HTTP::SWEEP-CACHE-REPRESENTATIONS-FOR-GARBAGE (HTTP::PROXY-CACHE T)):  grab the resource lock before trying for the table lock.
;;; Function (CLOS:METHOD HTTP::SWEEP-CACHE-REPRESENTATIONS-FOR-GARBAGE (HTTP::PROXY-CACHE T)):  -
;;; Function (CLOS:METHOD HTTP::UNINTERN-RESOURCE (HTTP::RESOURCE)):  locking bug.
;;; Function (CLOS:METHOD HTTP::REMOVE-CACHE-OBJECT (HTTP::PROXY-CACHE HTTP::RESOURCE)):  ditto.
;;; Function (CLOS:METHOD HTTP::UNINTERN-REPRESENTATION (HTTP::REPRESENTATION)):  ditto.
;;; Written by JCMa, 3/09/01 20:00:50
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.114,
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
;;; Genera 8 5 Lock Simple Patches 1.0, Jcma 42, HTTP Proxy Server 6.17,
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

(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:PROXY;DOCUMENTATION.LISP.22"
  "HTTP:PROXY;PROXY-CACHE.LISP.92"
  "HTTP:PROXY;REPRESENTATION.LISP.74"
  "HTTP:PROXY;UTILS.LISP.74"
  "HTTP:PROXY;CACHE.LISP.154"
  "HTTP:PROXY;RESOURCE.LISP.21")


;========================
(sct:begin-patch-section)
(sct:patch-section-source-file "HTTP:PROXY;DOCUMENTATION.LISP.22")
(sct:patch-section-attributes
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod respond-to-show-representation ((url url:http-search) stream)
  (macrolet ((with-html-response (&body body)
               `(with-successful-response (stream :html :expires (get-universal-time)
                                                  :content-location url)
                  (let ((title "Proxy Server Cached Representation"))
                    (html:with-html-document (:stream stream)
                      (html:with-document-preamble (:stream stream)
                        (html:declare-base-reference url :stream stream)
                        (html:declare-title title :stream stream))
                      (html:with-document-body (:stream stream :background :white :foreground :black)
                        (html:with-section-heading (title :level 2 :stream stream)
                          ,@body)))))))
    (destructuring-bind (uri cache-id &optional type) (url:search-keys url)
      (handler-case 
        (let* ((resource (w3p:accept-from-string 'resource uri))
               (id (w3p:accept-from-string 'integer cache-id))
               (representation (when resource (get-representation resource id nil))))
          (cond ((and type (string-equal type "entity"))
                 (proxy-respond-with-representation *server* representation (server-http-version *server*)))
                (t (with-html-response
                     (cond (representation
                            (w3p:present representation '((representation) :verbose-p t) 
                                         :view w3p:+html-view+
                                         :stream stream))  
                           (t (fast-format stream "Cached representation ~A for resource ~A not found." cache-id uri)))
                     (write-back-to-resources stream)
                     (html:horizontal-line :stream stream)
                     (cl-http-signature stream)))))
        (w3p:input-not-of-required-type 
	  () (with-html-response
	       (fast-format stream "Cached representation not found.")
	       (write-back-to-resources stream)
	       (html:horizontal-line :stream stream)
	       (cl-http-signature stream)))))))


;========================
(sct:begin-patch-section)
(sct:patch-section-source-file "HTTP:PROXY;DOCUMENTATION.LISP.22")
(sct:patch-section-attributes
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(define export-proxy-interfaces (&rest export-args &key (context (local-context)) &allow-other-keys)
  "Exports the proxy interfaces.
CONTEXT is the host and port on which to export the interfaces.
EXPORT-ARGS are any valid arguments to export URL, such as security parameters."
  (declare (dynamic-extent export-args))
  (with-local-context (context)
    (apply #'export-url #u"/cl-http/proxy/resource?"
	   :search
	   :response-function #'respond-to-show-resource
	   :expiration '(:no-expiration-header)
	   :keywords '(:cl-http :proxy :documentation)
	   :documentation "Describes proxy constraints."
	   export-args)
    (apply #'export-url #u"/cl-http/proxy/representation?"
	   :search
	   :response-function #'respond-to-show-representation
	   :search-parser #'parse-search-info
	   :search-writer #'write-search-info
	   :expiration '(:no-expiration-header)
	   :keywords '(:cl-http :proxy :documentation)
	   :documentation "Describes proxy predicates."
	   export-args)))

;========================
(sct:begin-patch-section)
(sct:patch-section-source-file "HTTP:PROXY;CACHE.LISP.154")
(sct:patch-section-attributes
  "-*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defgeneric cache-object-last-reference-date (cache-object)
  (declare (values universal-time))
  (:documentation "Returns the universal time when CACHE-OBJECT was last referenced."))


;========================
(sct:begin-patch-section)
(sct:patch-section-source-file "HTTP:PROXY;REPRESENTATION.LISP.74")
(sct:patch-section-attributes
  "-*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod cache-object-last-reference-date ((representation entity-transaction-mixin))
  (or (representation-last-reference representation)
      (cache-object-creation-date representation)))


;========================
(sct:begin-patch-section)
(sct:patch-section-source-file "HTTP:PROXY;RESOURCE.LISP.21")
(sct:patch-section-attributes
  "-*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod cache-object-last-reference-date ((resource resource))
  (or (loop for rep in (resource-representations resource)
	    maximize (cache-object-last-reference-date rep))
      (cache-object-creation-date resource)))


;========================
(sct:begin-patch-section)
(sct:patch-section-source-file "HTTP:PROXY;DOCUMENTATION.LISP.22")
(sct:patch-section-attributes
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod respond-to-show-resource ((url url:http-search) stream)
  (with-slots (url:search-keys) url
    (let ((title (format nil "Resources Caches by Proxy Server (~A)" (local-port-context *standard-proxy-port*))))
      (with-successful-response (stream :html :expires (server-request-time *server*) :content-location url)
	(html:with-html-document (:stream stream)
	  (html:with-document-preamble (:stream stream)
	    (html:declare-base-reference url :stream stream)
	    (html:declare-title title :stream stream))
	  (html:with-document-body (:stream stream :background :white :foreground :black)
	    (html:with-section-heading (title :level 2 :stream stream)
	      (html:horizontal-line :size 3 :stream stream)
              (cond (url:search-keys
                     (handler-case 
                       (let ((resource (w3p:accept-from-string 'resource (car url:search-keys))))
                         (if resource
			     (w3p:present resource '((resource) :verbose t) :view w3p:+html-view+ :stream stream)
			     (fast-format stream "Cached resource ~A not found." (car url:search-keys))))
                       (w3p:input-not-of-required-type () (fast-format stream "Cached resource not found.")))
                     (write-back-to-resources stream))
                    (t (flet ((write-resource (resource)
				(html:enumerating-item (stream)
				  (fast-format stream "[~I] ~I"
					       (write-standard-time (cache-object-last-reference-date resource) stream)
					       (w3p:present resource '((resource) :verbose-p nil) :view w3p:+html-view+ :stream stream)))))
			 (declare (dynamic-extent #'write-resource))
			 (let ((proxy-cache *proxy-cache*))
			   (html:with-paragraph (:stream stream)
			     (fast-format stream "~I ~D bytes" 
					  (html:with-rendition (:bold :stream stream)
					    (fast-format stream "Total Cache Size:"))
					  (cache-size proxy-cache)))
			   (html:with-paragraph (:stream stream)
			     (fast-format stream "~D resources currently cached are displayed in increasing recency of reference."
					  (resources-count proxy-cache)))
			   (html:horizontal-line :stream stream)
			   (html:with-paragraph (:stream stream)
			     (html:with-font (:size -1 :relative-size-p t :stream stream)
			       (html:with-enumeration (stream :enumerate)
				 (cache-recency-map-resources proxy-cache #'write-resource)))))))))
	    (html:horizontal-line :size 3 :stream stream)
	    (cl-http-signature stream)))))))

;========================
(sct:begin-patch-section)
(sct:patch-section-source-file "HTTP:PROXY;PROXY-CACHE.LISP.92")
(sct:patch-section-attributes
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun proxy-respond-while-caching-entity (resource url request-headers response-headers http-status http-version
						    remote-stream request-stream transfer-encoding)
  (flet ((handle-eof-error-on-optional-entity (representation response-headers)
	   (setf (server-close-connection-p *server*) t)	;close connection to maintain syncrhonization
	   (%representation-clean-up-incomplete-cache-of-entity representation response-headers)))
    (let* ((server *server*)
	   (resource (or resource (intern-resource server (name-string url) :if-does-not-exist :create)))
	   (current-access-time (server-request-time server)))
      (multiple-value-bind (representation)
	  (intern-representation resource request-headers :if-does-not-exist :create)
	;; if someone else gets this write lock first we just refetch 5/5/2000 -- JCMa.
	(with-lock-held ((cache-object-lock representation) :write "Cache Entity")
	  (proxy-trace "~&;Caching data for ~S" representation)
	  (setf (representation-valid-p representation) nil
		(representation-entity-invalid-p representation) t)
	  (handling-optional-entity-for-status-code
	    (http-status response-headers :clean-up-form (handle-eof-error-on-optional-entity representation response-headers))
	    (with-binary-stream (request-stream :output)
	      (with-binary-stream (remote-stream :input)
		(with-transfer-encoding (request-stream transfer-encoding)
		  (with-entity-data-stream (entity-stream representation :output)
		    (let ((broadcast (make-broadcast-stream request-stream entity-stream)))
		      (declare (dynamic-extent broadcast))
		      (with-transfer-decoding* (remote-stream url http-version :headers response-headers :copy-mode :binary)
			(stream-copy-until-eof remote-stream broadcast :binary)))))
		(setf (representation-entity-invalid-p representation) (not (representation-entity-handle-valid-p representation))))))
	  ;; update date after successful cache.
	  (setf (representation-response-headers representation) (proxy-cacheable-response-header-plist response-headers))	;set response headers
	  (representation-note-transaction representation http-status http-version current-access-time request-headers)
	  (setf (representation-last-reference representation) current-access-time
		(representation-last-update representation) current-access-time
		(representation-unsaved-metadata representation) current-access-time)
	  ;; the last form must set the validity of representation
	  (setf (representation-valid-p representation) t))
	;; Arrange for persistent storage of new metadata outside of write lock
	(note-metadata-update representation current-access-time)))))


;========================
(sct:begin-patch-section)
(sct:patch-section-source-file "HTTP:PROXY;PROXY-CACHE.LISP.92")
(sct:patch-section-attributes
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
(sct:begin-patch-section)
(sct:patch-section-source-file "HTTP:PROXY;UTILS.LISP.74")
(sct:patch-section-attributes
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: HTTP; Base: 10 -*-")

(defun cacheable-response-header-p (header-keyword)
  "Returns non-null if HEADER-KEYWORD denotes a cacheable response header."
  (not (member header-keyword *hop-by-hop-headers*)))

(declaim (inline cacheable-response-header-p))

;========================
(sct:begin-patch-section)
(sct:patch-section-source-file "HTTP:PROXY;UTILS.LISP.74")
(sct:patch-section-attributes
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: HTTP; Base: 10 -*-")

(defun proxy-cacheable-response-header-plist (header-set &optional include-headers exclude-headers)
  "Converts HEADER-SET into a property list of (keyword value).
Removes connection level headers"
  (flet ((collect-p (header-keyword header include-headers exclude-headers)
	   (and (cacheable-response-header-p header-keyword)
		(or (member header-keyword include-headers)
		    (not (member exclude-headers exclude-headers))
		    (not (%header-suppress-p header))))))
    (declare (inline collect-p))
    (%with-header-set-index (header-set)
      (with-fast-array-references ((headers headers vector)
				   (index index vector))
	(loop for idx fixnum upfrom 0 below (fill-pointer headers)
	      for header = (aref headers idx)
	      for header-keyword = (aref index idx)
	      when (collect-p header-keyword header include-headers exclude-headers)
		collect header-keyword
		and collect (header-value header))))))


;========================
(sct:begin-patch-section)
(sct:patch-section-source-file "HTTP:PROXY;UTILS.LISP.74")
(sct:patch-section-attributes
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: HTTP; Base: 10 -*-")

(defun cacheable-request-header-p (header-keyword)
  "Returns non-null if HEADER-KEYWORD denotes a cacheable request header."
  (not (member header-keyword '#.`(,@*hop-by-hop-headers*
				   ,@*request-specific-request-headers*
				   ,@*conditional-request-headers*))))

(declaim (inline cacheable-request-header-p))

;========================
(sct:begin-patch-section)
(sct:patch-section-source-file "HTTP:PROXY;UTILS.LISP.74")
(sct:patch-section-attributes
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: HTTP; Base: 10 -*-")

(defun proxy-cacheable-request-header-plist (header-set &optional include-headers exclude-headers )
  "Converts HEADER-SET into a property list of (keyword value).
Removes all connection-level, request-specific and user-specific headers.
EXCLUDE-HEADERS is a list of additional headers to suppress.
INCLUDE-HEADERS is a list of headers not to suppress, which overrides any other suppression mode."
  (flet ((collect-p (header-keyword header include-headers exclude-headers)
	   (and (cacheable-request-header-p header-keyword)
		(or (member header-keyword include-headers)
		    (not (or (member exclude-headers *user-specific-request-headers*)
			     (member exclude-headers exclude-headers)
			     (%header-suppress-p header)))))))
    (declare (inline collect-p))
    (%with-header-set-index (header-set)
      (with-fast-array-references ((headers headers vector)
				   (index index vector))
	(loop for idx fixnum upfrom 0 below (fill-pointer headers)
	      for header = (aref headers idx)
	      for header-keyword = (aref index idx)
	      when (collect-p header-keyword header include-headers exclude-headers)
		collect header-keyword
		and collect (header-value header))))))


;========================
(sct:begin-patch-section)
(sct:patch-section-source-file "HTTP:PROXY;REPRESENTATION.LISP.74")
(sct:patch-section-attributes
  "-*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod representation-update-response-headers ((representation representation) (response-headers header-set))
  (let ((old-headers (representation-response-headers representation))) 
    (flet ((update-header (header header-object)
	     (when (cacheable-response-header-p header)
	       (setf (getf old-headers header) (header-value header-object)))))
      (declare (dynamic-extent #'update-header))
      (map-headers #'update-header response-headers)
      (setf (representation-response-headers representation) old-headers))))

;========================
(sct:begin-patch-section)
(sct:patch-section-source-file "HTTP:PROXY;UTILS.LISP.74")
(sct:patch-section-attributes
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: HTTP; Base: 10 -*-")

(scl:fundefine 'proxy-header-plist)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;CACHE.LISP.154")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod sweep-cache-representations-for-garbage ((cache proxy-cache) gc-predicate &key (enumeration :oldest-reference)
						    reclaim-bytes reclaim-objects
						    &aux (bytes-reclaimed 0) (representations-decached 0) (resources-decached 0) fctn
						    resource-deleter)
  (declare (integer bytes-reclaimed representations-decached resources-decached))
  (labels ((delete-resource-without-lock (cache resource)
	     (%unintern-resource cache resource))
	   (delete-resource-with-lock (cache resource)
	     (with-lock-held ((cache-resource-table-lock cache) :write "Cache")
	       (with-lock-held ((cache-object-lock resource) :write "Resource")
		 (%unintern-resource cache resource))))
	   (conditional-gc (resource)
	     (loop with representations-decached-p
		   for representation in (resource-representations resource)
		   do (when (funcall gc-predicate representation)
			(let ((size (cache-object-size representation)))
			  (remove-representation resource representation)
			  (incf representations-decached)
			  (incf bytes-reclaimed size)
			  (setq representations-decached-p t)))
		   finally (unless (resource-representations resource)
			     (with-lock-held ((cache-object-lock resource) :write "Resource")
			       (unless (resource-representations resource)
				 (funcall resource-deleter cache resource)
				 (incf resources-decached))))
			   (return representations-decached-p)))
	   (bounded-gc (resource)
	     (when (conditional-gc resource)
	       (when (or (and reclaim-bytes (>= bytes-reclaimed reclaim-bytes))
			 (and reclaim-objects (= resources-decached reclaim-objects)))
		 (return-from sweep-cache-representations-for-garbage (values representations-decached resources-decached bytes-reclaimed)))))
	   (map-variant (uri resource)
	     (declare (ignore uri))
	     (funcall fctn resource)))
    (declare (dynamic-extent #'conditional-gc #'bounded-gc #'map-variant))
    ;; preamble
    (cond ((or reclaim-bytes reclaim-objects)
	   (unless (or (and reclaim-bytes (< 0 reclaim-bytes))
		       (and reclaim-objects (< 0 reclaim-objects)))
	     (return-from sweep-cache-representations-for-garbage (values 0 0 0)))
	   (setq fctn #'bounded-gc))
	  (t (setq fctn #'conditional-gc)))
    ;; Apply the sweep method
    (ecase enumeration
      (:oldest-reference
	(setq resource-deleter #'delete-resource-with-lock)
	(cache-recency-map-resources cache fctn))
      (:random
	(setq resource-deleter #'delete-resource-without-lock)
	(map-resources cache #'map-variant :write)))
    ;; report the results
    (values representations-decached resources-decached bytes-reclaimed)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;RESOURCE.LISP.21")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod unintern-resource ((resource resource))
  "Uninterns the cached resource, RESOURCE, from its CACHE."
  (let ((cache (cache-object-cache resource)))
    (with-lock-held ((cache-resource-table-lock cache) :write "Cache")
      (with-lock-held ((cache-object-lock resource) :write "Resource")
	(%unintern-resource cache resource)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;RESOURCE.LISP.21")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod remove-cache-object ((cache proxy-cache) (resource resource))
  (with-lock-held ((cache-object-lock resource) :write "Resource")
    (loop for representation in (resource-representations resource)
	  doing (with-lock-held ((cache-object-lock representation) :write "Representation")
		  (%representation-delete representation))))
  (unintern-resource resource))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;RESOURCE.LISP.21")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod unintern-representation ((representation representation))
  (let* ((resource (representation-resource representation))
	 (resource-lock (cache-object-lock resource)))
    (unless (with-lock-held ((cache-object-lock resource) :write "Resource")
	      (prog1 (%remove-representation resource representation)
		     (decache-object-size resource)))
      ;; Carefully grab locks in correct order
      (let ((cache (cache-object-cache resource)))
	(with-lock-held ((cache-resource-table-lock cache) :write "Cache")
	  (with-lock-held (resource-lock :write "Resource")
	    (unless (resource-representations resource)
	      (%unintern-resource cache resource))))))))

