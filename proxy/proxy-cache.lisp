;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-

;;; (C) Copyright John C. Mallery, 1998-2000, 2003, 2006.
;;;     All Rights Reserved.
;;;
;;; (C) Copyright Christopher R. Vincent, 1996-1997.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; PROXY-CACHE HTTP OPERATIONS
;;; 
(in-package :http)

(define check-only-if-cached-precondition (server &optional (headers *headers*))
  (with-header-values (cache-control) headers
    (and cache-control
	 (getf cache-control :only-if-cached)
	 (error 'gateway-timeout :url (server-url-string server) :method (server-method server)))))

(declaim (inline proxy-maybe-downgrade-status-code))

(defun proxy-maybe-downgrade-status-code (code http-version)
  (if (http-version-less-p http-version :http/1.1)
      (case code
	(307 302)
	(303 302)
	(t code))
      code))

(defun proxy-send-status-line (stream code http-version)
  (send-status-line stream
		    (proxy-maybe-downgrade-status-code code http-version)
		    #.(loop for code in *cacheable-response-status-codes*
			    collect `(,code ,(get-string-for-status-code code)) into cases
			    finally (return `(ecase code ,.cases)))))

(defun proxy-note-pure-cache-hit (representation server)
  (let ((current-access-time (server-request-time server)))
    (with-lock-held ((cache-object-lock representation) :write "Cache Hit") 
      (when (> current-access-time (representation-last-reference representation))
	(setf (representation-last-reference representation) current-access-time
	      (representation-unsaved-metadata representation) current-access-time)))
    (server-note-proxy-cache-hit server)))

(defun proxy-respond-with-representation (server rep request-http-version)
  (proxy-trace "~&;Responding with ~S" rep)
  (let* ((stream (server-stream server))
	 (response-headers (representation-response-headers rep)))
    (proxy-send-status-line stream (representation-http-status rep) request-http-version)
    (write-proxy-response-headers server response-headers request-http-version t stream)
    (write-representation-entity rep stream)))

(defun proxy-respond-with-meta-data (server rep request-http-version content-length-zero-p)
  (proxy-trace "~&;Responding with meta-data for ~S" rep)
  (let* ((stream (server-stream server))
	 (response-headers (representation-response-headers rep))
	 modified-headers)
    (when content-length-zero-p
      (let ((content-length (getf response-headers :content-length)))
	(cond ((null content-length)
	       (setq modified-headers '(:content-length 0)))
	      ((eql content-length 0))
	      (t (error "Non-Zero Content-Length, ~D, while serving metadata only for ~S." content-length rep)))))
    (proxy-send-status-line stream (representation-http-status rep) request-http-version)
    (write-proxy-response-headers server response-headers request-http-version t stream nil modified-headers)))

(defun proxy-respond-for-conditional-get (server rep request-http-version)
  (proxy-trace "~&;Responding with Not Modified on ~S" rep)
  (let* ((request-stream (server-stream server))
	 (response-headers (representation-response-headers rep)))
    (send-status-line request-stream 304 "Not Modified")
    (write-proxy-response-headers server response-headers request-http-version t request-stream)))

(defun proxy-respond-handling-conditional-get (server representation request-http-version)
  (declare (optimize (speed 3)))
  (let ((last-modification (or (representation-last-modification representation)
			       (representation-verified-date representation))))
    (unless (http-version-less-p request-http-version :http/1.1)
      (check-if-unmodified-since-precondition last-modification (server-method server) (server-headers server)))
    (cond ;; Check the upstream resource
          ((if-modified-since-p last-modification (server-headers server))
           (proxy-respond-for-conditional-get server representation request-http-version))
          ;; Return meta data for resources with optional entities according to the status code
          ((representation-entity-invalid-p representation)
           (let ((http-status (representation-http-status representation)))
             (unless (response-status-code-implies-optional-entity-p http-status)
               (error 'server-internal-error
                      :format-string "Attempt to send meta-data only for a response code requiring an entity~
                                  ~&Status Code: ~D~&Representation: ~S"
                      :format-args (list http-status representation)
                      :url (server-url-string server) :method (server-method server)))
             (proxy-respond-with-meta-data server representation request-http-version t)))
          ;; Return cached entity
          (t (proxy-respond-with-representation server representation request-http-version)))))

(defgeneric %representation-clean-up-incomplete-cache-of-entity (representation response-headers)
  (:documentation "Cleans up the entity handle and storage when an entity is incompletely cached."))

(defmethod %representation-clean-up-incomplete-cache-of-entity ((representation representation) (response-headers header-set))
  (let ((entity-handle (representation-entity-handle representation)))
    (if entity-handle
	(handle-object-delete entity-handle)
	(setf (representation-entity-size representation) 0))
    (decache-object-size representation))
  (suppress-header response-headers :content-length))

(defun proxy-respond-while-caching-entity (resource url request-headers response-headers http-status http-version
						    remote-stream request-stream transfer-encoding)
  (declare (optimize (speed 3)))
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
			(stream-copy-until-eof remote-stream broadcast :binary)
			(close broadcast)))))
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

; Abstract this further when incorporating additional HTTP/1.1 functionality.
(defun proxy-respond-with-remote-access (server res rep request-http-version)
  (declare (optimize (speed 3)))
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

(defun proxy-respond-with-remote-head-access (server res rep request-http-version)
  (let* ((request-url (server-url server))
	 (request-headers (server-headers server))
	 (request-stream (server-stream server)))
    (flet ((write-the-request-headers (stream method http-version)
	     (declare (ignore method))
	     (write-proxy-request-headers server request-headers http-version stream)))
      (declare (dynamic-extent #'write-the-request-headers))
      (when *trace-proxy*
	(format *trace-output* "~&;~2TProxy Request Headers:~&")
	(write-the-request-headers *trace-output* (server-method server) request-http-version))
      (with-http-request (request-url :head :request-headers #'write-the-request-headers)
	(let* ((client-status (client-status client))
	       (client-http-version (client-request-version client))
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

(define-generic invoke-proxy-cache-service (server method request-http-version)
  (declare (optimize (speed 3)))
  (:documentation "Service A proxy request with caching operations."))

;; HTTP 1.1 cache-control implemented except for the fields on the directives
;; :private & :no-cache and the :no-transform directive.   2/4/99 -- JCMa.
(defmethod invoke-proxy-cache-service ((server proxy-server-mixin) (method (eql :get)) request-http-version)
  (declare (optimize (speed 3)))
  (when *trace-proxy*
    (format *trace-output*"~&--------------------------------------------------------------------------------")
    (format *trace-output*  "~&;Proxying a ~A ~A request for ~S." request-http-version method (server-url-string server))
    (format *trace-output* "~&;~2TClient Request Headers:~&")
    (write-header-buffer (server-headers server) *trace-output*))
  (with-slots (url-string headers) server
    (let* ((resource (intern-resource server url-string :if-does-not-exist :soft))
           (representation (and resource (intern-representation resource headers :if-does-not-exist :soft))))
      (cond (representation
	     (proxy-trace "~&;Matching representation found for ~S" representation)
	     (block serve-from-cache
	       (with-lock-held ((cache-object-lock representation) :read "Write Entity")
		 (cond ((and (representation-valid-p representation) 	;no valid entity available in cache
			     (or (not (representation-entity-invalid-p representation))
				 (response-status-code-implies-optional-entity-p (representation-http-status representation)))
			     (not (or (representation-must-revalidate-p representation)
				      (proxy-revalidate-cache-for-client-p representation request-http-version (server-request-time server)))))
			(proxy-trace "~&;Serving cached data ~S" representation)
			(proxy-respond-handling-conditional-get server representation request-http-version))
		       (t (proxy-trace "~&;Revalidating representation for ~S because ~:[expiration or client~;server~] requires it."
				       representation (representation-must-revalidate-p representation))
			  (return-from serve-from-cache))))
	       (proxy-note-pure-cache-hit representation server)	;uses a write lock
	       (return-from invoke-proxy-cache-service)))
	    (t (proxy-trace "~&;No matching representation found].")))
      (unless (http-version-less-p request-http-version :http/1.1)
	(check-only-if-cached-precondition server (server-headers server)))
      (proxy-respond-with-remote-access server resource representation request-http-version))))

(defmethod invoke-proxy-cache-service ((server proxy-server-mixin) (method (eql :head)) request-http-version)
  (declare (optimize (speed 3)))
  (with-slots (url-string headers) server
    (flet ((proxy-respond-with-cached-meta-data (server representation request-http-version)
	     (let* ((request-stream (server-stream server))
		    (response-headers (representation-response-headers representation)))
	       (proxy-send-status-line request-stream 203 request-http-version)
	       (write-proxy-response-headers server response-headers request-http-version t request-stream))))
      (when *trace-proxy*
	(format *trace-output*"~&--------------------------------------------------------------------------------")
	(format *trace-output*  "~&;Proxying a ~A ~A request for ~S." request-http-version method (server-url-string server))
	(format *trace-output* "~&;~2TClient Request Headers:~&")
	(write-header-buffer (server-headers server) *trace-output*))
      (let* ((resource (intern-resource server url-string :if-does-not-exist :soft))
	     (representation (and resource (intern-representation resource headers :if-does-not-exist :soft))))
	(cond (representation
	       (proxy-trace "~&;Matching representation found for ~S" representation)
	       (block serve-from-cache
		 (with-lock-held ((cache-object-lock representation) :read "Read MetaData")
		   (cond ((and (representation-valid-p representation) 	;no valid entity available in cache
			       (not (or (representation-must-revalidate-p representation)
					(proxy-revalidate-cache-for-client-p representation request-http-version (server-request-time server)))))
			  (proxy-trace "~&;Serving cached metadata for ~S" representation)
			  (proxy-respond-with-cached-meta-data server representation request-http-version))
			 (t (proxy-trace "~&;Revalidating representation for ~S because ~:[expiration or client~;server~] requires it."
					 representation (representation-must-revalidate-p representation))
			    (return-from serve-from-cache))))
		 (proxy-note-pure-cache-hit representation server)	;uses a write lock
		 (return-from invoke-proxy-cache-service)))
	      (t (proxy-trace "~&;No matching representation found].")))
	(unless (http-version-less-p request-http-version :http/1.1)
	  (check-only-if-cached-precondition server (server-headers server)))
	(proxy-respond-with-remote-head-access server resource representation request-http-version)))))

(defgeneric proxy-revalidate-representation (representation &optional revalidate-time)
  (:documentation "Revalidates REPRESENTATION with the origin server."))

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

(defgeneric proxy-cache-ensure-valid-cache-entry (proxy-cache string-or-url &key request-headers cache-time)
  (:documentation "Ensures that STRING-OR-URL is cached and valid in the PROXY-CACHE.
If REQUEST-HEADERS are known, they should be supplied for correct vary processing
CACHE-TIME is the current time when the cache request is invoked."))

(defmethod proxy-cache-ensure-valid-cache-entry (proxy-cache (resource resource) &key request-headers (cache-time (get-universal-time)))
  (declare (ignore proxy-cache))
  (proxy-revalidate-representation (intern-representation resource request-headers :if-does-not-exist :create) cache-time))

(defmethod proxy-cache-ensure-valid-cache-entry ((proxy-cache proxy-cache) (url-string string) &key request-headers (cache-time (get-universal-time)))
  (proxy-cache-ensure-valid-cache-entry proxy-cache (intern-resource proxy-cache url-string :if-does-not-exist :create)
				     :request-headers request-headers :cache-time cache-time))

(defmethod proxy-cache-ensure-valid-cache-entry (proxy-cache (url url) &key request-headers (cache-time (get-universal-time)))
  (proxy-cache-ensure-valid-cache-entry proxy-cache (intern-resource proxy-cache url :if-does-not-exist :create)
				     :request-headers request-headers :cache-time cache-time))
