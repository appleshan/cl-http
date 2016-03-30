;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-
;;;
;;; (C) Copyright 1998-2001, 2003, 2006, John C. Mallery, All Rights Reserved.
;;;
;;; (C) Copyright 1996-1997, Christopher R. Vincent & John C. Mallery
;;;     All Rights Reserved.
;;
;;;
;;;------------------------------------------------------------------- 
;;;
;;; CL-HTTP PROXY SERVER
;;;

(in-package :http)

;;;------------------------------------------------------------------- 
;;;
;;; SIMPLE PROXY RELAY
;;; 

(declaim (inline proxy-relay-transfer-encoding))

;; obsolete 11/16/99 -- JCMa.
(defun proxy-relay-transfer-encoding (content-length http-version)
  (if (or content-length (member http-version '(:http/0.9 :http/1.0))) :fixed-length :chunked))

(defun get-transfer-encoding-header-plist (content-length http-version)
  (declare (values header-plist transfer-encoding))
  (if (or content-length (member http-version '(:http/0.9 :http/1.0)))
      (values nil :fixed-length)
      (values '(:transfer-encoding :chunked) :chunked)))

(defun proxy-relay-simple-request (server method request-http-version)
  (declare (optimize (speed 3)))
  (flet ((write-the-request-headers (stream method http-version)
	   (declare (ignore method))
	   (write-proxy-request-headers server (server-headers server) http-version stream)))
    (declare (dynamic-extent #'write-the-request-headers))
    (let ((request-url (server-url server))
	  (request-stream (server-stream server)))
      (when *trace-proxy*
	(format *trace-output*  "~&;Proxying on port ~D  a ~A ~A request for ~S." *standard-http-port* request-http-version method (name-string request-url))
	(format *trace-output* "~&;~2TClient Request Headers:")
	(write-header-buffer (server-headers server) *trace-output*))
      (with-http-request (request-url method :request-headers #'write-the-request-headers)
	(let* ((client-status (client-status client))
	       (client-http-version (client-request-version client))
	       (client-response-headers (client-response-headers client)))
	  (send-status-line request-stream (proxy-maybe-downgrade-status-code client-status request-http-version) (client-reason client))
	  (cond ((response-status-code-implies-entity-p client-status)
		 (let ((content-length (get-header :content-length client-response-headers)))
		   (multiple-value-bind (additional-headers transfer-encoding)
		       (get-transfer-encoding-header-plist content-length request-http-version)
		     (write-proxy-response-headers server client-response-headers client-http-version content-length request-stream additional-headers)
		     ;; Send the entity body
		     (handling-optional-entity-for-status-code
		       (client-status client-response-headers :clean-up-form (setf (server-close-connection-p server) t))
		       (with-binary-stream (request-stream :output)
			 (with-binary-stream (remote-stream :input)
			   (with-transfer-encoding (request-stream transfer-encoding)
			     (with-transfer-decoding* (remote-stream request-url http-version :headers client-response-headers :copy-mode :binary)
			       (stream-copy-until-eof remote-stream request-stream :binary)))))))))
		(t (write-proxy-response-headers server client-response-headers client-http-version t request-stream))))))))

(defun proxy-relay-head-request (server request-http-version)
  (flet ((write-request-headers (stream method http-version)
				(declare (ignore method))
				(write-proxy-request-headers server (server-headers server) http-version stream)))
    (declare (dynamic-extent #'write-request-headers))
    (let ((request-url (server-url server))
	  (request-stream (server-stream server)))
      (when *trace-proxy*
	(format *trace-output*  "~&;Proxying on port ~D a ~A ~A request for ~S." *standard-http-port* request-http-version :head (name-string request-url))
	(format *trace-output* "~&;~2TClient Request Headers:")
	(write-header-buffer (server-headers server) *trace-output*))
      (with-http-request (request-url :head :request-headers #'write-request-headers)
	remote-stream				;ignore
	(send-status-line request-stream (proxy-maybe-downgrade-status-code (client-status client) request-http-version) (client-reason client))
	(write-proxy-response-headers server (client-response-headers client) request-http-version t request-stream)))))

(defun proxy-relay-request-with-entity (server method request-http-version)
  (let* ((request-url (server-url server))
	 (request-headers (server-headers server))
	 (request-content-length (get-header :content-length request-headers))
	 (request-stream (server-stream server)))
    (flet ((send-data (from-stream to-stream url headers content-length from-http-version to-http-version)
	     ;; Send 100 code to continue as appropriate
	     (case from-http-version
	       ((:http/1.0 :http/0.9))
	       (t (report-status-continue from-stream)
		  (send-cr-line-feed from-stream) 
		  (force-output from-stream)))
	     ;; transfer the body
	     (with-binary-stream (from-stream :input)
	       (with-binary-stream (to-stream :output)
		 (with-transfer-encoding (to-stream (proxy-relay-transfer-encoding content-length to-http-version))
		   (with-transfer-decoding* (from-stream url from-http-version :headers headers :copy-mode :binary)
		     (stream-copy-until-eof from-stream to-stream :binary))))))
	   (write-the-request-headers (stream method http-version)
	     (declare (ignore method))
	     (let ((additional-headers (get-transfer-encoding-header-plist (get-header :content-length request-headers) http-version)))
	       (write-proxy-request-headers server request-headers http-version stream additional-headers))))
      (declare (dynamic-extent #'write-the-request-headers))
;      (let-if (member method '(:post :put))
;	      ((*client-http-version* (http-version-max :http/1.0 (server-http-version server))
;				      #+ignore :http/1.0 ))	;kludge to deal with broken HTTP 1.1 servers   9/30/2000 -- JCMa.
      (when *trace-proxy*
	(format *trace-output*"~&--------------------------------------------------------------------------------")
	(format *trace-output*  "~&;Proxying on port ~D a ~A ~A request for ~S." *standard-http-port* request-http-version method (server-url-string server))
	(format *trace-output* "~&;~2TClient Request Headers:~&")
	(write-header-buffer (server-headers server) *trace-output*))
      (with-http-request
	(request-url method 
		     :request-headers #'write-the-request-headers
		     :request-body (send-data request-stream remote-stream request-url request-headers 
					      request-content-length request-http-version http-version)) 
	(let* ((client-status (client-status client))
	       (client-http-version (client-connection-version client))
	       (client-response-headers (client-response-headers client)))
	  (when *trace-proxy*
	    (format *trace-output* "~&;Origin Server ~A Status: ~D" client-http-version client-status)
	    (format *trace-output* "~&;Origin Server Headers:~&")
	    (write-header-buffer client-response-headers *trace-output*))
	  (send-status-line request-stream (proxy-maybe-downgrade-status-code client-status request-http-version) (client-reason client))
	  (cond ((response-status-code-requires-no-entity-p client-status)
		 (write-proxy-response-headers server client-response-headers client-http-version t request-stream))
		(t (let ((content-length (get-header :content-length client-response-headers)))
		     (multiple-value-bind (additional-headers transfer-encoding)
			 (get-transfer-encoding-header-plist content-length request-http-version)
		       (write-proxy-response-headers server client-response-headers client-http-version content-length request-stream additional-headers)
		       (handling-optional-entity-for-status-code
			 (client-status client-response-headers :clean-up-form (setf (server-close-connection-p server) t))
			 (with-binary-stream (request-stream :output)
			   (with-binary-stream (remote-stream :input)
			     (with-transfer-encoding (request-stream transfer-encoding)
			       (with-transfer-decoding* (remote-stream request-url http-version :headers client-response-headers :copy-mode :binary)
				 (stream-copy-until-eof remote-stream request-stream :binary)))))))))))))))

;;;------------------------------------------------------------------- 
;;;
;;; PROXY METHODS 
;;; 

(defgeneric signal-unauthorized-proxy-access (realm url method)
  (:documentation "Signals unathorized proxy access according to REALM."))

(defmethod signal-unauthorized-proxy-access ((realm realm-http-authentication-mixin) url method)
  (error 'recoverable-unauthorized-proxy-access :method method :url url 
         :authentication-realm realm :authentication-method (realm-scheme realm)))

(defmethod signal-unauthorized-proxy-access ((realm realm-certificate-authentication-mixin) url method)
  (error 'proxy-access-forbidden :method method :url url))

(defmethod invoke-proxy-service :around ((server proxy-server-mixin) url method http-version)
  (declare (optimize (speed 3)))
  (flet ((provide-proxy-service (server url method http-version &aux (catch-error-p (not *debug-proxy*)))
	   (flet ((ensure-live-upstream-connection (condition)
		    (declare (ignore condition))
		    (abort-if-connection-inactive *server*)
		    nil))
	     (handler-case-if 
		 catch-error-p
                 ;; Nasty signalling ensues if the client has dropped the connection, 
                 ;; so intercept errors here and abort the connection if the client is gone. -- JCMa 5/24/1999.
                 (handler-bind-if catch-error-p
                     ((condition #'ensure-live-upstream-connection))
                   ;; Set the life time for the connection 
                   (setf (server-life-time server) *proxy-server-life-time*)
                   ;; call the primary method
                   (call-next-method server url method http-version))
	       (unauthorized-proxy-access (err) (error 'proxy-invalid-authentication-credentials :method method :url url
						       :proxy (get-client-proxy url) :realm (http-authentication-realm err)))
	       (unknown-host-name (err) (error 'proxy-unresolvable-domain-name :format-string (report-string err)
					       :method method :url url))
	       (protocol-timeout (err) (error 'proxy-connection-timeout :format-string (report-string err)
					      :method method :url url))
	       (connection-refused (err) (error 'proxy-connection-refused :format-string (report-string err)
						:method method :url url))
	       (local-network-error (err)  (error 'proxy-local-network-error :format-string (report-string err)
						  :method method :url url :close-connection t))
	       (remote-network-error (err) (error 'proxy-remote-network-error :format-string (report-string err)
						  :method method :url url :close-connection t))))))
    (let ((proxy-access-control (proxy-access-control 8000)))
      (typecase proxy-access-control
	(null ; Always respect subnet security as the default
         (with-subnet-access-control
             ((server-address server) (or *proxy-subnets* *secure-subnets*) 
              :deny-subnets *disallowed-subnets*
              :rejection-form (error 'proxy-access-forbidden :method method :url url))
           (provide-proxy-service server url method http-version)))
	(basic-access-control ; Respect subnet security for basic access control
         (with-subnet-access-control
             ((server-address server) (or *proxy-subnets* *secure-subnets*) 
              :deny-subnets *disallowed-subnets*
              :rejection-form (error 'proxy-access-forbidden :method method :url url))
           (with-proxy-authentication-access-control
               (proxy-access-control
                method
                :server server
                :rejection-form (signal-unauthorized-proxy-access realm url method))
             (provide-proxy-service server url method http-version))))
        (t (with-proxy-authentication-access-control
               (proxy-access-control
                method
                :server server
                :rejection-form (signal-unauthorized-proxy-access realm url method))
             (provide-proxy-service server url method http-version)))))))

(defmethod invoke-proxy-service ((server proxy-server-mixin) (uri http-url) (method (eql :get)) request-http-version)
  (declare (optimize (speed 3)))
  (if *proxy-caching-p*
      (invoke-proxy-cache-service server method request-http-version)
    (proxy-relay-simple-request server method request-http-version)))

(defmethod invoke-proxy-service ((server proxy-server-mixin) (uri http-url) (method (eql :head)) request-http-version)
  (declare (optimize (speed 3)))
  (if *proxy-caching-p*
      (invoke-proxy-cache-service server method request-http-version)
      (proxy-relay-head-request server request-http-version)))

(defmethod invoke-proxy-service ((server proxy-server-mixin) (uri http-url) (method (eql :delete)) request-http-version)
  (declare (optimize (speed 3)))
  (proxy-relay-simple-request server method request-http-version))

;; Can't handle * correctly until intern-url handles hosts w/out paths (e.g. http://wilson.ai.mit.edu)
;; Should relay error response entities.
(defmethod invoke-proxy-service ((server proxy-server-mixin) (uri http-url) (method (eql :options)) http-version)
  (declare (ignore http-version))
  (flet ((message-allow-headers (headers)	; edit allow header for access through proxy
           (let ((allow (get-header-object :allow headers)))
             (when allow
               (setf (%header-value allow) (intersection *proxy-methods* (header-value allow) :test #'eql)))))
	 (write-request-headers (stream method http-version)
				(declare (ignore method))
				(write-proxy-request-headers server (server-headers server) http-version stream)))
    (declare (inline message-allow-headers)
	     (dynamic-extent #'write-request-headers))
    (let ((request-stream (server-stream server)))
      (with-http-request (uri :options :request-headers #'write-request-headers)
        remote-stream				;ignore
	(send-status-line request-stream (client-status client) (client-reason client))
	;; must precede computing response headers
	(message-allow-headers *headers*)
	(write-proxy-response-headers server (client-response-headers client) (client-request-version client) t request-stream)))))

(defmethod invoke-proxy-service ((server proxy-server-mixin) (uri http-url) (method (eql :post)) request-http-version)
  (proxy-relay-request-with-entity server method request-http-version))

(defmethod invoke-proxy-service ((server proxy-server-mixin) (uri http-url) (method (eql :put)) request-http-version)
  (proxy-relay-request-with-entity server method request-http-version))

(defmethod invoke-proxy-service ((server proxy-server-mixin) (uri http-url) (method (eql :trace)) http-version)
  (flet ((set-max-forwards (value headers)
           (let ((maxf (get-header-object :max-forwards headers)))
             (setf (%header-value maxf) value)))
	 (write-request-headers (stream method http-version)
				(declare (ignore method))
				(write-proxy-request-headers server (server-headers server) http-version stream)))
    (declare (dynamic-extent #'write-request-headers))
    (let ((request-url uri)
          (request-headers (server-headers server))
          (request-stream (server-stream server)))
      (with-header-values (max-forwards) request-headers
        (cond ;; bounce the trace back
	  ((and max-forwards (<= max-forwards 0))
	   (with-chunked-transfer-encoding
	     (request-stream '(:message :http) :status :success :location request-url :cache-control '(:no-cache t))
	     (fast-format request-stream "~A~%" (%server-request server))
	     (write-proxy-response-headers server request-headers http-version t request-stream)))
	  ;; Pass the request through
	  (t (set-max-forwards (1- max-forwards) request-headers)	; decrement max-forwards
	     (with-http-request (request-url :trace :request-headers #'write-request-headers)
	       (let* ((client-status (client-status client))
		      (client-http-version (client-request-version client))
		      (client-response-headers (client-response-headers client)))
		 (send-status-line request-stream client-status (client-reason client))
		 (cond ((response-status-code-implies-entity-p client-status)
			(let ((response-content-length (get-header :content-length client-response-headers)))
			  (multiple-value-bind (additional-headers transfer-encoding)
			      (get-transfer-encoding-header-plist response-content-length client-http-version)
			    (write-proxy-response-headers server client-response-headers client-http-version response-content-length
							  request-stream additional-headers)
			    (with-binary-stream (request-stream :output)
			      (with-binary-stream (remote-stream :input)
				(with-transfer-encoding (request-stream transfer-encoding)
				  (with-transfer-decoding* (remote-stream request-url http-version :headers client-response-headers :copy-mode :binary)
				    (stream-copy-until-eof remote-stream request-stream :binary))))))))
		       (t (write-proxy-response-headers server client-response-headers client-http-version t request-stream)))))))))))

(pushnew :cl-http-proxy *features*)

;;;------------------------------------------------------------------- 
;;;
;;; FILE PROXY METHODS
;;;

(defmethod invoke-proxy-service ((server proxy-server-mixin) (url url:file-pathname) (method (eql :get)) request-http-version)
  request-http-version
  (let* ((url (server-url server))
	 (pathname (url::file-url-pathname url))
	 (type (pathname-primary-extension pathname nil))
         (stream (server-stream server))
	 (proxy-response-headers `(:via ,(compute-via-header nil))))
    (declare (dynamic-extent proxy-response-headers))
    (handler-case
        (file-url-copy-file-to-http-stream pathname stream :content-type type :url url :additional-headers proxy-response-headers)
      (file-not-found (err) (error 'document-not-found :url url :method method :format-string (report-string err))))))

(defmethod invoke-proxy-service ((server proxy-server-mixin) (url url:file-directory) (method (eql :get)) request-http-version)
  request-http-version
  (labels ((get-file-directory-info (url)
	     (handler-case 
                 (file-url-directory-info (url::file-url-pathname url))
	       (file-not-found (err) (error 'document-not-found :url url :method method :format-string (report-string err)))))
	   (write-item (path plist stream)
	     (destructuring-bind (&key length-in-bytes creation-date directory
				       #+cl-http-file-author author &allow-other-keys)
		 plist
	       (let ((file-url (pathname-file-url-string path directory)))
		 (html:with-table-row (:stream stream)
		   (html:with-table-cell (:stream stream)
		     (html:with-rendition (:bold :stream stream)
		       (html:with-anchor-noted (:reference file-url :stream stream)
			 (let ((name (pathname-name path))
			       (type (pathname-type path))
			       (version (pathname-version path)))
			   (cond ((null name)
				  (fast-format stream "~A" (car (last (pathname-directory path)))))
				 (t (fast-format stream "~A" name)
				    (when (and type (not (eql type :unspecific)))
				      (fast-format stream ".~A" type)
				      (when (and version (integerp version))
					(fast-format stream ".~D" version)))))))))
		   (if creation-date
		       (html:with-table-cell (:horizontal-alignment :right :stream stream)
			 (write-standard-time creation-date stream))
                     (html:with-table-cell (:horizontal-alignment :center :stream stream)
                       (write-string "--" stream)))
		   (cond (directory
			  (html:with-table-cell (:horizontal-alignment :right :stream stream)))
			 (length-in-bytes
			  (html:with-table-cell (:horizontal-alignment :right :stream stream)
			    (write length-in-bytes :stream stream :escape nil :base 10.)))
			 (t (html:with-table-cell (:horizontal-alignment :center :stream stream)
			      (write-string "--" stream))))
		   #+cl-http-file-author
		   (if author
		       (html:with-table-cell (:horizontal-alignment :right :stream stream)
			 (write-string author stream))
                     (html:with-table-cell (:horizontal-alignment :center :stream stream))))))))
    (declare (inline directory-info))
    (multiple-value-bind (directory-listing directory-exists-p)
	(get-file-directory-info url)
      (cond (directory-exists-p
	     (let* ((title (relative-name-string url))
		    (stream (server-stream server))
		    (proxy-response-headers `(:via ,(compute-via-header nil))))
	       (declare (dynamic-extent title proxy-response-headers))
	       (with-successful-response (stream :html :status :success  :location url :additional-headers proxy-response-headers)
		 (html:with-html-document (:declare-dtd-version-p t :stream stream)
		   (html:with-document-preamble (:stream stream)
		     (html:declare-title title :stream stream))
		   (html:with-standard-document-body (:stream stream)
		     (html:with-section-heading (title :stream stream)
		       (html:horizontal-line  :stream stream)
		       (html:with-table (:cell-spacing 4 :stream stream)
			 (html:with-table-row (:stream stream)
			   (html:with-table-cell (:header-p t :horizontal-alignment :center :stream stream)
			     (write-string "URL" stream))
			   (html:with-table-cell (:header-p t :horizontal-alignment :center :stream stream)
			     (write-string "Creation Date" stream))
			   (html:with-table-cell (:header-p t :horizontal-alignment :center :stream stream)
			     (write-string "Bytes" stream))
			   #+cl-http-file-author
			   (html:with-table-cell (:header-p t :horizontal-alignment :center :stream stream)
			     (write-string "Author" stream)))
			 (loop for (path . plist) in directory-listing
			       for translated = (translated-pathname path)
			       do (write-item translated plist stream)))
		       (html:horizontal-line :stream stream)
		       (cl-http-signature stream)))))))
	    (t (error 'document-not-found :url url :method :get))))))


;;;------------------------------------------------------------------- 
;;;
;;; FTP PROXY METHODS
;;;

(defgeneric proxy-ftp-user-id-and-password (proxy-server ftp-url)
  (declare (values user-id password))
  (:documentation "Returns an appropriate userid and password for FTP access to FTP-URL."))

(defmethod proxy-ftp-user-id-and-password ((server proxy-server-mixin) (url url:ftp-url))
  (flet ((authorization-header-basic-user-id-and-password (authorization)
	   (when authorization
	     (destructuring-bind (authentication-method cookie &rest args) authorization
	       (declare (ignore args))
	       (case authentication-method
		 (:basic
		   (let* ((decoded-authorization (base64:base64-decode-vector cookie :decoded-byte-type *standard-character-type*))
			  (colon (position #\: decoded-authorization))
			  (username (subseq decoded-authorization 0 colon))
			  (pw (subseq decoded-authorization (1+ colon))))
		     (declare (dynamic-extent decoded-authorization))
		     (values username pw)))
		 (t nil))))))
    (declare (inline authorization-header-basic-user-id-and-password))
    (let ((headers (server-headers server)))
      ;; Extract from the headers
      (multiple-value-bind (user-id pw)
	  (authorization-header-basic-user-id-and-password (get-header :authorization headers))
	(when (or user-id pw)
	  (return-from proxy-ftp-user-id-and-password (values user-id pw))))
      ;; Extract from the FTP URL
      (multiple-value-bind (user-id pw)
	  (url:user-id-and-password url)
	(when (or user-id pw)
	  (return-from proxy-ftp-user-id-and-password (values user-id pw))))
      ;; Provide some defaults
      (values "anonymous" (or (get-header :from headers) (server-mail-address))))))

(defmethod http-methods ((url url:ftp-pathname) http-version)
  (case http-version
    (:http/1.0
     '(:get :head :put))
    (:http/0.9
     '(:get :head))
    (t '(:get :head :delete :put :options))))

(defmethod http-methods ((url url:ftp-directory) http-version)
  (case http-version
    ((:http/1.0 :http/0.9)
     '(:get :head))
    (t '(:get :head :delete :options))))

#+CL-HTTP-FTP-CLIENT
(defmethod invoke-proxy-service ((server proxy-server-mixin) (url ftp-url) (method (eql :options)) (http-version (eql :http/1.1)))
   (let ((stream (server-stream server))
        (server-methods (http-methods server http-version))
        (proxy-response-headers `(:via ,(compute-via-header nil)))
        (path (path url)))
    (declare (dynamic-extent proxy-response-headers))
    (cond (path
           (flet ((write-filename (stream)
                    (etypecase url
                      (url:ftp-pathname
                       (url::write-object-name-string url stream t))
                      (url:ftp-directory :current-directory))))
             (declare (dynamic-extent #'write-filename))
             (let ((host (host-string url))
                   (port (host-port url))
                   (url-methods (http-methods url http-version)))
               (multiple-value-bind (user-id pw)
                   (proxy-ftp-user-id-and-password server url)
                 ;; get some information from the FTP server
                 (multiple-value-bind (size last-modification unique-id)
                     (ftp:with-ftp-connection (ftp-connection host port :user-id user-id :password pw)
                       (ftp:ftp-change-directory ftp-connection path)
                       (ftp:ftp-file-info ftp-connection #'write-filename))
                   ;; report back to the client
                   (send-response stream nil
                                  :status :success
                                  :content-location url
                                  :last-modification last-modification
                                  :bytes size
                                  :entity-tag unique-id
                                  :public server-methods
                                  :allow url-methods
                                  :cache-control `(:no-cache t)
                                  :additional-headers proxy-response-headers))))))
          (t (send-response stream nil
                            :status :success
                            :public server-methods
                            :cache-control `(:no-cache t)
                            :additional-headers proxy-response-headers)))))

#+CL-HTTP-FTP-CLIENT
(defmethod invoke-proxy-service :around ((server proxy-server-mixin) (url url:ftp-url) method request-http-version)
  (flet ((handle-invalid-ftp-user-id-and-password (&rest ignore)
           (declare (ignore ignore))
           (signal 'client-unauthorized-ftp-access :url url :method method :authentication-realm "FTP Server" :authentication-method :basic))
         (handle-ftp-directory-not-found (&rest ignore)
	   (declare (ignore ignore))
	   (signal 'document-not-found :url url :method method
                   :format-string "Directory not found for ~A" :format-args (list (name-string url))))
         (handle-ftp-error (error)
           (bug-report-error error)
           (signal 'proxy-bad-ftp-gateway :url url :method method :format-string (report-string error))))
    (declare (dynamic-extent #'handle-invalid-ftp-user-id-and-password #'handle-ftp-directory-not-found #'handle-ftp-error))
    (handler-bind-if
        (not *debug-proxy*)
        ((ftp:ftp-file-not-found #'handle-ftp-directory-not-found)
         (ftp:ftp-not-logged-in #'handle-invalid-ftp-user-id-and-password)
         (ftp:ftp-error #'handle-ftp-error))
      (call-next-method server url method request-http-version))))

(defmethod invoke-proxy-service ((server proxy-server-mixin) (url ftp-url) (method (eql :trace)) (http-version (eql :http/1.1)))
  (let ((proxy-response-headers `(:via ,(compute-via-header nil))))
    (declare (dynamic-extent proxy-response-headers))
    ;; this is the meat of the trace response
    (write-document-trace server url proxy-response-headers)))

#+CL-HTTP-FTP-CLIENT
(defmethod invoke-proxy-service ((server proxy-server-mixin) (url ftp-url) (method (eql :head)) http-version)
  (declare (ignore http-version))
  (flet ((write-filename (stream)
           (etypecase url
             (url:ftp-pathname
              (url::write-object-name-string url stream t))
             (url:ftp-directory :current-directory))))
    (declare (dynamic-extent #'write-filename))
    (let ((host (host-string url))
          (port (host-port url)))
      (multiple-value-bind (user-id pw)
          (proxy-ftp-user-id-and-password server url)
        ;; get some information from the FTP server
        (multiple-value-bind (size last-modification unique-id)
            (ftp:with-ftp-connection (ftp-connection host port :user-id user-id :password pw)
              (ftp:ftp-change-directory ftp-connection (url:path url))
              (ftp:ftp-file-info ftp-connection #'write-filename))
          ;; report back to the client
          (let ((stream (server-stream server))
                (proxy-response-headers `(:via ,(compute-via-header nil))))
            (declare (dynamic-extent proxy-response-headers))
            (send-response stream (url::content-type-keyword url)
                           :status :success
                           :content-location url
                           :last-modification last-modification
                           :bytes size
                           :entity-tag unique-id
                           :cache-control `(:no-cache t)
                           :additional-headers proxy-response-headers)))))))

#+CL-HTTP-FTP-CLIENT
(defmethod invoke-proxy-service ((server proxy-server-mixin) (url ftp-pathname) (method (eql :delete)) http-version)
  (declare (ignore http-version))
  (flet ((write-filename (stream)
           (url::write-object-name-string url stream t)))
    (declare (dynamic-extent #'write-filename))
    (let ((host (host-string url))
          (port (host-port url)))
      (multiple-value-bind (user-id pw)
          (proxy-ftp-user-id-and-password server url)
        ;; invoke FTP
        (ftp:with-ftp-connection (ftp-connection host port :user-id user-id :password pw)
          (ftp:ftp-change-directory ftp-connection (url:path url))
          (ftp:ftp-delete-file ftp-connection #'write-filename)))))
  ;; report back
  (let ((stream (server-stream server))
        (proxy-response-headers `(:via ,(compute-via-header nil))))
    (declare (dynamic-extent proxy-response-headers))
    (send-response stream nil :status :no-content 
                   :location (url:name-string url)
                   :cache-control `(:no-cache t) 
                   :additional-headers proxy-response-headers)))

#+CL-HTTP-FTP-CLIENT
(defmethod invoke-proxy-service ((server proxy-server-mixin) (url ftp-directory) (method (eql :delete)) http-version)
  (declare (ignore http-version))
  (let ((host (host-string url))
        (port (host-port url)))
    (multiple-value-bind (user-id pw)
        (proxy-ftp-user-id-and-password server url)
      ;; invoke FTP
      (ftp:with-ftp-connection (ftp-connection host port :user-id user-id :password pw)
        (ftp:ftp-delete-directory ftp-connection (url:path url)))))
  ;; report back
  (let ((stream (server-stream server))
        (proxy-response-headers `(:via ,(compute-via-header nil))))
    (declare (dynamic-extent proxy-response-headers))
    (send-response stream nil :status :no-content 
                   :location (url:name-string url)
                   :cache-control `(:no-cache t) 
                   :additional-headers proxy-response-headers)))

#+CL-HTTP-FTP-CLIENT
(defmethod invoke-proxy-service ((server proxy-server-mixin) (url url:ftp-pathname) (method (eql :get)) request-http-version)
  (declare (ignore request-http-version))
  (flet ((write-filename (stream)
           (url::write-object-name-string url stream t)))
    (declare (dynamic-extent #'write-filename))
    (let ((stream (server-stream server))
          (host (host-string url))
          (port (host-port url))
          (content-type (content-type-keyword url nil))
          (http-copy-mode (copy-mode url))
          (proxy-response-headers `(:via ,(compute-via-header nil)))
          ftp-mode)
      (declare (dynamic-extent proxy-response-headers))
      (setq ftp-mode (ftp-copy-mode http-copy-mode))
      (multiple-value-bind (user-id pw)
          (proxy-ftp-user-id-and-password server url)
        (ftp:with-ftp-connection (ftp-connection host port :passive-p t :user-id user-id :password pw)
          (ftp:ftp-change-directory ftp-connection (url:path url))
          (multiple-value-bind (file-size last-modification unique-id)
              (ftp:ftp-file-info ftp-connection #'write-filename)
            (http:with-successful-response (stream content-type 
                                                   :bytes (case http-copy-mode (:text nil) (t file-size)) ;may be wrong due to char translation
                                                   :last-modification last-modification
                                                   :content-location url
                                                   :entity-tag unique-id
                                                   :additional-headers proxy-response-headers)
              (case http-copy-mode
                (:text
                 (with-text-stream (stream :output)
                   (ftp:ftp-get-file ftp-connection #'write-filename stream :mode ftp-mode)))
                ((:binary :crlf)
                 (with-binary-stream (stream :output)
                   (ftp:ftp-get-file ftp-connection #'write-filename stream :mode ftp-mode)))))))))))

#+CL-HTTP-FTP-CLIENT
(defun ftp-write-basic-directory-list (ftp-connection url stream)
  (let ((url-directory (name-string url)))
    (flet ((write-entry (string start end)
             (let ((directory-p (ftp:ftp-directory-p ftp-connection string start end)))
               (labels ((write-item (stream)
                          (write-string string stream :start start :end end))
                        (write-escaped-item (stream)
                          (write-string-escaping-special-chars string stream start end))
                        (write-url (stream)
                          (fast-format stream "~A~I~I" url-directory 
                                       (write-escaped-item stream)
                                       (when directory-p (write-char #\/ stream)))))
                 (declare (dynamic-extent #'write-item #'write-url))
                 (with-table-row (:stream stream)
                   (with-table-cell (:stream stream)
                     (with-rendition (:bold :stream stream)
                       (note-anchor #'write-item :reference #'write-url :stream stream))))))))
      (declare (dynamic-extent #'write-entry))
      (let ((title url-directory)
            (proxy-response-headers `(:via ,(compute-via-header nil))))
        (declare (dynamic-extent title proxy-response-headers))
        (with-successful-response (stream :html :status :success  :location url :additional-headers proxy-response-headers)
          (with-cl-http-html-document (:declare-dtd-version-p :strict :stream stream)
            (with-document-look-preamble (:stream stream)
              (declare-title title :stream stream))
            (with-document-look-body (:heading title :header-class "header.cl-http" :body-class "body.cl-http"
                                      :footer-class "footer.cl-http" :stream stream)
              (with-table (:cell-spacing 4 :stream stream)
                (with-table-row (:stream stream)
                  (with-table-cell (:header-p t :horizontal-alignment :center :stream stream)
                    (write-string "URL" stream)))
                (ftp:ftp-map-directory ftp-connection #'write-entry)))))))))

#+CL-HTTP-FTP-CLIENT
(defun ftp-write-size-mdtm-directory-list (ftp-connection url stream)
  (let ((url-directory (name-string url)))
    (flet ((write-entry (item plist)
             (destructuring-bind (&key type size modify &allow-other-keys) plist
               (let ((directory-p (case type (:directory t) (t nil)))
                     (creation-date modify))
                 (labels ((write-url (stream)
                            (fast-format stream "~A~I~I" url-directory 
                                         (write-string-escaping-special-chars item stream)
                                         (when directory-p (write-char #\/ stream)))))
                   (declare (dynamic-extent #'write-url))
                   (with-table-row (:stream stream)
                     (with-table-cell (:stream stream)
                       (with-rendition (:bold :stream stream)
                         (note-anchor item :reference #'write-url :stream stream)))
                     (cond (directory-p
                            (with-table-cell (:horizontal-alignment :right :stream stream)))
                           (size
                            (with-table-cell (:horizontal-alignment :right :stream stream)
                              (write size :stream stream :escape nil :base 10.)))
                           (t (with-table-cell (:horizontal-alignment :center :stream stream)
                                (write-string "--" stream))))
                     (if creation-date
                         (with-table-cell (:horizontal-alignment :right :stream stream)
                           (write-standard-time creation-date stream))
                       (with-table-cell (:horizontal-alignment :center :stream stream)
                         (write-string "--" stream)))))))))
      (let ((title url-directory)
            (proxy-response-headers `(:via ,(compute-via-header nil))))
        (declare (dynamic-extent proxy-response-headers))
        (with-successful-response (stream :html :status :success  :location url :additional-headers proxy-response-headers)
          (with-cl-http-html-document (:declare-dtd-version-p :strict :stream stream)
            (with-document-look-preamble (:stream stream)
              (declare-title title :stream stream))
            (with-document-look-body (:heading title :header-class "header.cl-http" :body-class "body.cl-http"
                                      :footer-class "footer.cl-http" :stream stream)
              (with-table (:cell-spacing 4 :cell-padding 2 :stream stream)
                (with-table-row (:stream stream)
                  (with-table-cell (:header-p t :horizontal-alignment :center :stream stream)
                    (write-string "URL" stream))
                  (with-table-cell (:header-p t :horizontal-alignment :center :stream stream)
                    (write-string "Bytes" stream))
                  (with-table-cell (:header-p t :horizontal-alignment :center :stream stream)
                    (write-string "Creation Date" stream)))
                (ftp:ftp-map-directory-plist ftp-connection #'write-entry)))))))))

#+CL-HTTP-FTP-CLIENT
(defun ftp-write-mlst-directory-list (ftp-connection url stream)
  (let ((url-directory (name-string url)))
    (flet ((write-entry (item plist)
             (destructuring-bind (&key type size modify create perm &allow-other-keys) plist
               (let ((directory-p (case type (:directory t) (t nil)))
                     (creation-date (or modify create)))
                 (labels ((write-url (stream)
                            (fast-format stream "~A~I~I" url-directory 
                                         (write-string-escaping-special-chars item stream)
                                         (when directory-p (write-char #\/ stream)))))
                   (declare (dynamic-extent #'write-url))
                   (with-table-row (:stream stream)
                     (with-table-cell (:stream stream)
                       (with-rendition (:bold :stream stream)
                         (note-anchor item :reference #'write-url :stream stream)))
                     (cond (directory-p
                            (with-table-cell (:horizontal-alignment :right :stream stream)))
                           (size
                            (with-table-cell (:horizontal-alignment :right :stream stream)
                              (write size :stream stream :escape nil :base 10.)))
                           (t (with-table-cell (:horizontal-alignment :center :stream stream)
                                (write-string "--" stream))))
                     (if creation-date
                         (with-table-cell (:horizontal-alignment :right :stream stream)
                           (write-standard-time creation-date stream))
                       (with-table-cell (:horizontal-alignment :center :stream stream)
                         (write-string "--" stream)))
                     (if perm
                         (with-table-cell (:horizontal-alignment :right :stream stream)
                           (write-string perm stream))
                       (with-table-cell (:horizontal-alignment :center :stream stream)
                         (write-string "--" stream)))))))))
      (let ((title url-directory)
            (proxy-response-headers `(:via ,(compute-via-header nil))))
        (declare (dynamic-extent proxy-response-headers))
        (with-successful-response (stream :html :status :success  :location url :additional-headers proxy-response-headers)
          (with-cl-http-html-document (:declare-dtd-version-p :strict :stream stream)
            (with-document-look-preamble (:stream stream)
              (declare-title title :stream stream))
            (with-document-look-body (:heading title :header-class "header.cl-http" :body-class "body.cl-http"
                                      :footer-class "footer.cl-http" :stream stream)
              (with-table (:cell-spacing 4 :cell-padding 2 :stream stream)
                (with-table-row (:stream stream)
                  (with-table-cell (:header-p t :horizontal-alignment :center :stream stream)
                    (write-string "URL" stream))
                  (with-table-cell (:header-p t :horizontal-alignment :center :stream stream)
                    (write-string "Bytes" stream))
                  (with-table-cell (:header-p t :horizontal-alignment :center :stream stream)
                    (write-string "Creation Date" stream))
                  (with-table-cell (:header-p t :horizontal-alignment :center :stream stream)
                    (write-string "Permissions" stream)))
                (ftp:ftp-map-directory-plist ftp-connection #'write-entry)))))))))

#+CL-HTTP-FTP-CLIENT
(defmethod invoke-proxy-service ((server proxy-server-mixin) (url url:ftp-directory) (method (eql :get)) request-http-version)
  (declare (ignore request-http-version))
  (flet ((write-directory (stream)
           (url::write-path url stream)))
    (declare (dynamic-extent #'write-directory))
    (let ((host (host-string url))
          (port (host-port url)))
      (multiple-value-bind (user-id pw)
          (proxy-ftp-user-id-and-password server url)
        (ftp:with-ftp-connection (connection host port :passive-p t :user-id user-id :password pw)
          (ftp:ftp-change-directory connection (url:path url))
          (ftp:ftp-feature-case connection
            (:mlst
             (ftp-write-mlst-directory-list connection url (server-stream server)))
            ((:size :mdtm)
             (ftp-write-size-mdtm-directory-list connection url (server-stream server)))
            (t  (ftp-write-basic-directory-list connection url (server-stream server)))))))))

;; need to get via header on every repsones
#+CL-HTTP-FTP-CLIENT
(defmethod invoke-proxy-service ((server proxy-server-mixin) (url url:ftp-pathname) (method (eql :put)) request-http-version)
  (declare (ignore request-http-version))
  (flet ((write-filename (stream)
           (url::write-object-name-string url stream t)))
    (declare (dynamic-extent #'write-filename))
    (let ((headers (server-headers server))
          (host (host-string url))
          (port (host-port url))
          newly-created-url-p)
      (multiple-value-bind (user-id pw)
                           (proxy-ftp-user-id-and-password server url)
        (ftp:with-ftp-connection (connection host port :passive-p t :user-id user-id :password pw)
          (ftp:ftp-change-directory connection (url:path url))
          (multiple-value-bind (file-size last-modification unique-id)
                               (handler-case
                                 (ftp:ftp-file-info connection #'write-filename)
                                 (ftp::ftp-error () nil)) ;file does not exist
            unique-id ;ignore
            ;; throw out of http transaction if a conflict is detected
            (cond (last-modification ; could use unique-id here but need to check these predicates
                   (let ((version last-modification))
                     (case (server-http-version server) ;should this signalling include a via header?
                       ((:http/0.9 :http/1.0)
                        (check-derived-from-version url version))
                       (t (check-if-match-precondition version t :put headers) 
                          (check-if-unmodified-since-precondition version :put headers)))))
                  (file-size) ;means file exists - are we clobbering it? I guess so because we have a poor ftp server
                  (t (setq newly-created-url-p t)))
            (with-header-values (content-length content-range content-type last-modified transfer-encoding) headers
              ;; check for byte ranges in 1.1
              (when content-range
                (error 'proxy-not-implemented :url url :method :put
                       :format-string "Putting byte ranges to FTP servers is not implemented."))
              (let* ((stream (server-stream server))
                     (http-copy-mode (if content-type (mime-content-type-copy-mode content-type) (url:copy-mode url)))
                     (ftp-mode (ftp-copy-mode http-copy-mode))
                     (modification-date (or last-modified (server-request-time server)))
                     (proxy-via (compute-via-header nil)))
                (declare (dynamic-extent proxy-via))
                (cond (transfer-encoding 
                       (case transfer-encoding
                         #+HTTP-Chunking
                         (:chunked
                          (flet ((copy-data (ftp-stream)
                                   (stream-copy-until-eof stream ftp-stream :binary)))
                            (declare (dynamic-extent #'copy-data))
                            (with-successful-put-response (server stream :via proxy-via)
                              (with-chunked-transfer-decoding (stream :headers headers)
                                (ftp:ftp-put-file connection stream #'write-filename :mode ftp-mode))
                              (values url (if newly-created-url-p :created :modified) modification-date))))
                         (t (error 'proxy-not-implemented :close-connection t :url url :method :put
                                   :format-string "The HTTP transfer encoding, ~A, is not implemented for FTP gateways."
                                   :format-args (list transfer-encoding)))))
                      (content-length
                       (flet ((copy-data (ftp-stream)
                                (stream-copy-bytes stream ftp-stream content-length :binary)))
                         (declare (dynamic-extent #'copy-data))
                         (handler-case-if (not *debug-proxy*) 
                                          (with-successful-put-response (server stream :via proxy-via)
                                            stream ;ignore
                                            (ftp:ftp-put-file connection #'copy-data #'write-filename :mode ftp-mode
                                                              :estimated-size content-length)
                                            (values url (if newly-created-url-p :created :modified) modification-date))
                           (error (err)
                                  (error 'error-handling-put-method :url url :method :put :server-error err :headers (header-plist)
                                         :format-string "Error executing PUT method for ~A."
                                         :format-args (list (url:name-string url)))))))
                      (t (error 'content-length-required :url url :method :put
                                :format-string "No content-length header provided for ~A."
                                :format-args (list (url:name-string url)))))))))))))

#|

;; An example of tightening up FTP proxy security.

(defparameter *ai-lab-basic-realm* (intern-realm "ai-lab-basic"))

(defparameter *cl-http-access-controls* (intern-access-control *ai-lab-basic-realm* :cl-http :if-does-not-exist :error))

(defmethod url:authentication-realm ((url url:ftp-url))
  *ai-lab-basic-realm*)

(defmethod url:capabilities ((url url:ftp-url))
  *cl-http-access-controls*)

(defmethod invoke-proxy-service :around (server (url url:ftp-url) method  request-http-version)
  server request-http-version
  (cond ((and (neti:ns-eq (host-object url) (parse-host "beet-chex.ai.mit.edu")))
	 (let ((realm (url:authentication-realm url)))
	   (with-authentication-access-control
	     (url method (get-header :authorization) realm
		  :require-capabilities (url:capabilities url)
		  :rejection-form (error 'recoverable-unauthorized-client-access :method method :url url
					 :authentication-method (realm-scheme realm) 
					 :authentication-realm realm))
	     (call-next-method))))
	(t (error 'access-forbidden :method method :url url))))

(define-proxy-subnets
  #|"128.52.0.0"|#				; MIT AI Lab
  )

|#


;;;------------------------------------------------------------------- 
;;;
;;; INTERCEPTING PROXY INVOCATIONS OTHER THAN FOR HTTP-URLs
;;;

(defmacro with-standard-proxy ((proxy) &body body)
  "Binds proxy as the standard proxy within the scope of BODY."
  `(let ((*standard-proxy* ,proxy)) . ,body))

(defmacro with-proxy-override ((url standard-form) &body body)
  "When URL is handled by a proxy, body is executed.
Otherwise standard-form is executed."
  `(let ((proxy (choose-proxy ,url)))
     (cond (proxy
            (with-standard-proxy (proxy) . ,body))
	   (t ,standard-form))))

(defmethod invoke-proxy-service :around ((server proxy-server-mixin) (uri url:ftp-url) (method (eql :get)) request-http-version)
   (with-proxy-override (uri (call-next-method))
       (proxy-relay-simple-request server method request-http-version)))

(defmethod invoke-proxy-service :around ((server proxy-server-mixin) (uri url:ftp-url) (method (eql :put)) request-http-version)
   (with-proxy-override (uri (call-next-method))
       (proxy-relay-request-with-entity server method request-http-version))) 

(defmethod invoke-proxy-service :around ((server proxy-server-mixin) (uri url:ftp-url) (method (eql :delete)) request-http-version)
   (with-proxy-override (uri (call-next-method))
       (proxy-relay-simple-request server method request-http-version))) 

(defmethod invoke-proxy-service :around ((server proxy-server-mixin) (uri url:news-url) (method (eql :get)) request-http-version)
   (with-proxy-override (uri (call-next-method))
       (proxy-relay-simple-request server method request-http-version)))

(defmethod invoke-proxy-service :around ((server proxy-server-mixin) (uri url:news-url) (method (eql :post)) request-http-version)
   (with-proxy-override (uri (call-next-method))
       (proxy-relay-request-with-entity server method request-http-version)))

(defmethod invoke-proxy-service :around ((server proxy-server-mixin) (uri url:gopher-url) (method (eql :get)) request-http-version)
  (with-proxy-override (uri (call-next-method))
    (proxy-relay-simple-request server method request-http-version)))

(defmethod invoke-proxy-service :around ((server proxy-server-mixin) (uri url:wais-url) (method (eql :get)) request-http-version)
  (with-proxy-override (uri (call-next-method))
    (proxy-relay-simple-request server method request-http-version)))


;;;------------------------------------------------------------------- 
;;;
;;; REVERSE PROXY URL EXPORTS
;;;

(defstruct (reverse-proxy-forwarding)
  domain-name					;domain name of content server
  port						;port on content server
  (host-object nil)				;parsed host representation
  protocol)					;protocol for content server

(defun %export-reverse-proxy-url (url export-type args)
  (flet ((make-forwardings (proxy-forwardings)
	   (check-type proxy-forwardings cons)
	   (loop for entry in proxy-forwardings
		 for (domain-name port protocol) = entry
		 collect (make-reverse-proxy-forwarding
			   :host-object (parse-host domain-name nil)
			   :domain-name domain-name
			   :port (or port 80)
			   :protocol (or protocol :http)))))

    (destructuring-bind (&key proxy-forwardings &allow-other-keys) args
      (cond (proxy-forwardings
	     (setf (url::reverse-proxy-forwardings url) (make-forwardings proxy-forwardings))
	     (setf (url:translation-method url) export-type)
	     (enable-url-areas t))
	    (t (error "No proxy-forwardings were provided when exporting ~A using the export type, ~S." url export-type))))))

(defmethod url:initialize-specialization ((url url:reverse-proxy-mixin) class init-args)
  (with-class-change-for-initialize-specialization (url class init-args)
    (%export-reverse-proxy-url url :reverse-proxy init-args)
    url))

(defmethod export-url ((url url:reverse-proxy-mixin) (export-type (eql :reverse-proxy)) &rest args)
  (declare (dynamic-extent args))
  (%export-reverse-proxy-url url export-type args))

(defmethod export-url ((url url:http-minimum-object) (translation (eql :reverse-proxy)) &rest args)
  (declare (dynamic-extent args))
  (url:initialize-specialization url 'url:reverse-proxy-http-object args))

(defmethod export-url ((url url:http-path) (translation (eql :reverse-proxy)) &rest args)
  (declare (dynamic-extent args))
  (url:initialize-specialization url 'url:reverse-proxy-http-path args))

(defmethod export-url ((url url:http-search) (translation (eql :reverse-proxy)) &rest args)
  (declare (dynamic-extent args))
  (url:initialize-specialization url 'url:reverse-proxy-http-search args) translation args)

(defmethod export-url ((url url:https-minimum-object) (translation (eql :reverse-proxy)) &rest args)
  (declare (dynamic-extent args))
  (url:initialize-specialization url 'url:reverse-proxy-https-object args))

(defmethod export-url ((url url:https-path) (translation (eql :reverse-proxy)) &rest args)
  (declare (dynamic-extent args))
  (url:initialize-specialization url 'url:reverse-proxy-https-path args))

(defmethod export-url ((url url:https-search) (translation (eql :reverse-proxy)) &rest args)
  (declare (dynamic-extent args))
  (url:initialize-specialization url 'url:reverse-proxy-https-search args) translation args)


;;;------------------------------------------------------------------- 
;;;
;;; REVERSE PROXY SERVER INVOCATION INTERFACE
;;;
;;; Each HTTP method requires an interface to call
;;; INVOKE-REVERSE-PROXY-SERVICE with the exception of PUT and DELETE, which
;;; is wired in.  Any new methods need to be handled here

(defmethod write-document ((url url:reverse-proxy-mixin) (translation (eql :reverse-proxy)) stream)
  (declare (ignore stream))
  (invoke-reverse-proxy-service *server* url :get))

(defmethod write-document-headers ((url url:reverse-proxy-mixin) (translation (eql :reverse-proxy)) stream)
  (declare (ignore stream))
  (invoke-reverse-proxy-service *server* url :head))

(defmethod write-document-post ((server proxy-server-mixin) (url url:reverse-proxy-mixin)  http-version)
   (declare (ignore http-version))
   (case (translation-method url)
      (:reverse-proxy 
        (invoke-reverse-proxy-service server url  :post))
      (t (error 'document-not-found :url url :method :options)))) 

(defmethod write-document-headers ((url url:reverse-proxy-mixin) (translation (eql :options)) stream)
   (declare (ignore stream))
   (case (translation-method url)
      (:reverse-proxy 
        (invoke-reverse-proxy-service *server* url :options))
      (t (error 'document-not-found :url url :method :options))))

(defmethod write-document-trace ((server proxy-server-mixin) (url url:reverse-proxy-mixin) &optional additional-headers)
   (declare (ignore additional-headers))
   (case (translation-method url)
      (:reverse-proxy 
        (invoke-reverse-proxy-service server url  :trace))
      (t (error 'document-not-found :url url :method :options))))

;;;------------------------------------------------------------------- 
;;;
;;; REVERSE PROXYING
;;;

(defgeneric reverse-proxy-select-forwarding (url)
  (declare (values domain-name port host-object protocol))
  (:documentation "Dynamically selects the forwarding host and port for a reverse proxied URL."))

(defmethod reverse-proxy-select-forwarding ((url url:reverse-proxy-mixin))
  (flet ((choose-forwarding (url)
	   (let ((forwardings (url:reverse-proxy-forwardings url)))
	     (if (cdr forwardings)
		 (nth (random (length forwardings)) forwardings)
		 (car forwardings)))))
    (let ((forwarding (choose-forwarding url)))
      (values (reverse-proxy-forwarding-domain-name forwarding)
	      (reverse-proxy-forwarding-port forwarding)
	      (reverse-proxy-forwarding-host-object forwarding)
	      (reverse-proxy-forwarding-protocol forwarding)))))

(defmethod inherit-export-parameters progn ((url url:reverse-proxy-mixin) (parent url:reverse-proxy-mixin))
  (setf (url:reverse-proxy-forwardings url) (url:reverse-proxy-forwardings parent)
	(url:reverse-proxy-superior url) parent
	(translation-method url) :reverse-proxy))

(defgeneric intern-reverse-proxy-url-inferior (superior url-string &optional if-does-not-exist)
  (:documentation "Interns url-string as a reverse proxy url with URL superior, SUPERIOR."))

(defmethod intern-reverse-proxy-url-inferior ((superior url:reverse-proxy-mixin) url-string &optional (if-does-not-exist :uninterned))
  (multiple-value-bind (url newly-interned-p)
      (intern-url url-string :if-does-not-exist if-does-not-exist)
    (typecase url
      (url:reverse-proxy-mixin)
      (t (error "INTERN-URL produced ~S, which is not a reverse proxyable ~A URL." url (protocol url))))
    (when newly-interned-p
      ;; inherit all relevant parameters from superior, including security properties
      (inherit-export-parameters url superior))
    (values url newly-interned-p)))

(defmethod intern-reverse-proxy-url-inferior :around ((superior url:reverse-proxy-http-url) url-string &optional (if-does-not-exist :uninterned))
  (with-url-class-map (:http () ((:object url:reverse-proxy-http-object) 
				 (:path url:reverse-proxy-http-path) 
				 (:search url:reverse-proxy-http-search) 
				 (:search-object url:reverse-proxy-http-searchable-object)))
    (call-next-method superior url-string if-does-not-exist)))

(defmethod intern-reverse-proxy-url-inferior :around ((superior url:reverse-proxy-https-url) url-string &optional (if-does-not-exist :uninterned))
  "Interns url-string as a reverse proxy url with URL superior, SUPERIOR."
  (with-url-class-map (:https () ((:object url:reverse-proxy-https-object) 
                                  (:path url:reverse-proxy-https-path) 
                                  (:search url:reverse-proxy-https-search) 
                                  (:search-object url:reverse-proxy-https-searchable-object)))
    (call-next-method superior url-string if-does-not-exist)))

(defmethod intern-url-inferior ((superior url:reverse-proxy-mixin) (export-type (eql :reverse-proxy)) http-method url-string)
  http-method
  (intern-reverse-proxy-url-inferior superior url-string :uninterned)) 

(defvar *remote-context-table* nil
  "Holds a table of remote contexts for the reverse proxy.")

(declaim (inline %remote-context-table))

(defun %remote-context-table ()
  (or *remote-context-table* (setq *remote-context-table* (make-hash-table :test #'equalp))))

(defun clear-remote-context-table ()
  (when *remote-context-table* (clrhash *remote-context-table*)))

(defvar *remote-port-context-lock* (make-lock "REMOTE-PORT-CONTEXT"))

(defun remote-port-context (host port protocol)
  "Returns the url context for PROTOCOL://HOST:PORT."
  (with-lock-held (*remote-port-context-lock* :write) ;; synchronization not essential as the cache won't be corrupted
    (let ((entry (gethash host (%remote-context-table))))
      (cond (entry
             (cond ((getf entry port))
                   (t (let ((context (%absolutize-relative-url protocol host port nil)))
                        (push (list port context) entry)
                        context))))
            (t (let ((context (%absolutize-relative-url protocol host port nil)))
                 (setf (gethash host (%remote-context-table)) (list port context))
                 context))))))

(defmethod invoke-reverse-proxy-service ((server basic-server-mixin) (url url:url) method)
  (error 'method-not-allowed 
	 :format-string "HTTP Reverse Proxy Service not available for ~S."
	 :format-args (list (name-string url))
	 :method method :url url))

(defmethod invoke-reverse-proxy-service ((server proxy-server-mixin) (url url:reverse-proxy-mixin) method)
  (cond (*proxy-service*
         (let ((local-url (server-url server))
               (local-url-string (server-url-string server))
               buffer)
           (multiple-value-bind (domain-name port host-object protocol)
               (reverse-proxy-select-forwarding url)
             (declare (ignore host-object))
             (unwind-protect
                 (progn 
                   (setq buffer (allocate-resource 'line-buffer *line-buffer-size*))
                   (let* ((remote-context (remote-port-context domain-name port protocol))
                          (remote-url-string (url:swap-url-context (name-string local-url) remote-context buffer))
                          (remote-url (intern-url remote-url-string :if-does-not-exist :uninterned)))
                     (declare (dynamic-extent remote-url-string))
                     (setf (server-url server) remote-url
                           (server-url-string server) remote-url-string
                           (server-proxy-request-p server) t)
                     (suppress-header (server-headers server) :host t)
                     ;; host header should be assured by proxy header copying methods -- JCMa 9/26/2000.
                     (push-header (server-headers server) :host (list (host-string remote-url) (port remote-url)))
                     (invoke-proxy-service server remote-url method (server-http-version server))))
               ;; replace url with local url
               (setf (server-url server) local-url
                     (server-url-string server) local-url-string)
               (deallocate-resource 'line-buffer buffer)))))
        (t (error 'access-forbidden 
                  :format-string "HTTP Reverse Proxy Service is currently unavailable on ~A (~D)." 
                  :format-args (list (local-host-domain-name) (server-host-local-port server))
                  :method method :url url))))

#|
(export-url #u("/" :port 8001) :reverse-proxy
	    :proxy-forwardings '(("wilson.ai.mit.edu" 80 :http)))

(export-url #u("/" :port 8002) :reverse-proxy
	    :proxy-forwardings '(("www.ai.mit.edu" 80 :http)))

(export-url #u("/" :port 8003) :reverse-proxy
	    :proxy-forwardings '(("www.w3.org" 80 :http)))

|# 

;;;------------------------------------------------------------------- 
;;;
;;; SSL PROXY TUNNELLING BASED ON IETF RFC 2817
;;;

(defun make-http-authority (resource host port &optional name-string)
  (declare (ignore resource))
  (make-instance 'url:http-authority
                 :host-string host
                 :port port
                 :name-string name-string))

(defun initialize-http-authority (resource authority host port &optional name-string)
  (declare (ignore resource))
  (setf (host-string authority) host
        (port authority) port
        (slot-value authority 'name-string) name-string)
  authority)

(defun deinitialize-http-authority (resource authority)
  (declare (ignore resource))
  (setf (host-string authority) nil
        (port authority) nil
        (slot-value authority 'name-string) nil
        (slot-value authority 'host-object) nil)
  authority)

(defresource
    http-authority (host port &optional name-string)
  :constructor make-http-authority
  :initializer initialize-http-authority
  :deinitializer deinitialize-http-authority
  :initial-copies 0)

(defmacro with-http-authority ((authority-var host port &optional name-string) &body body)
  `(let (,authority-var)
     (unwind-protect
         (progn
           (setq ,authority-var (allocate-resource 'http-authority ,host ,port ,name-string))
           ,@body)
       (when ,authority-var
         (deallocate-resource 'http-authority ,authority-var)))))

(defclass proxy-tunnel
          ()
  ((client-stream :initarg :client-stream :accessor proxy-tunnel-client-stream)
   (server-stream :initarg :server-stream :accessor proxy-tunnel-server-stream)
   (timeout :initarg :timeout :accessor proxy-tunnel-timeout))
  (:documentation "A binary tunnel between two hosts."))

(defmethod print-object ((tunnel proxy-tunnel) stream)
  (with-slots (client-stream server-stream) tunnel
    (print-unreadable-object (tunnel stream)
      (when (and (slot-boundp tunnel 'client-stream)
                 (slot-boundp tunnel 'server-stream))
        (format stream "~A:~D <-> ~A:~D" (foreign-host client-stream) (foreign-port client-stream)
                (foreign-host server-stream) (foreign-port server-stream))))))

(defun make-proxy-tunnel (resource client-stream server-stream timeout)
  (declare (ignore resource client-stream server-stream timeout))
  (make-instance 'proxy-tunnel))

(defun initialize-proxy-tunnel (resource proxy-tunnel client-stream server-stream timeout)
  (declare (ignore resource))
  (setf (proxy-tunnel-client-stream proxy-tunnel) client-stream
        (proxy-tunnel-server-stream proxy-tunnel) server-stream
        (proxy-tunnel-timeout proxy-tunnel) timeout)
  proxy-tunnel)

(defun deinitialize-proxy-tunnel (resource proxy-tunnel)
  (declare (ignore resource))
  (setf (proxy-tunnel-client-stream proxy-tunnel) nil
        (proxy-tunnel-server-stream proxy-tunnel) nil
        (proxy-tunnel-timeout proxy-tunnel) nil)
  proxy-tunnel)

(defresource
    proxy-tunnel (client-stream server-stream timeout)
  :constructor make-proxy-tunnel
  :initializer initialize-proxy-tunnel
  :deinitializer deinitialize-proxy-tunnel
  :initial-copies 0)

(defmacro with-proxy-tunnel ((tunnel-var request-stream remote-stream timeout) &body body)
  `(let (,tunnel-var)
     (unwind-protect
         (progn
           (setq ,tunnel-var (allocate-resource 'proxy-tunnel ,request-stream ,remote-stream ,timeout))
           ,@body)
       (when ,tunnel-var
         (deallocate-resource 'proxy-tunnel ,tunnel-var)))))

(defgeneric input-available-on-streams-p (streams wait-reason timeout)
  (:documentation "Blocks until input data is available on any stream in STREAMS or until any stream goes down.
When either case occurs, this returns non-null. If any stream goes inactive for more than TIMEOUT
seconds, this returns NIL. PORTS should specialize this for their platform so that wake up is
triggered by the arrival of data on any stream."))

;; default method
(defmethod input-available-on-streams-p (streams wait-reason timeout)
  (labels ((data-available-p (streams)
             (loop for stream in streams
                   when (listen stream)
                   return stream
                   finally (return nil)))
           (continue-p (streams)
             (loop for stream in streams
                   thereis (or (not (www-utils:live-connection-p stream)) ;connection went dead
                               (listen stream))))) ;data available                  
    (declare (inline data-available-p))
    (cond ((not (loop for stream in streams
                      always (www-utils:live-connection-p stream))) nil)
          ((data-available-p streams) t)
          ;; Block until there is reason to take action
          (t (process-wait-with-timeout wait-reason timeout #'continue-p streams)
             ;; Determine whether input data was available without consing.
             (loop with data-p
                   for stream in streams
                   do (cond ((not (www-utils:live-connection-p stream))
                             (return nil))
                            ((and (null data-p) (listen stream))
                             (setq data-p t)))
                   finally (return data-p))))))

(defmethod tunnel-throughput ((tunnel proxy-tunnel))
  (flet ((tunnel-throughput-until-quiesence (client-stream server-stream)
           (flet ((copy-buffer-p (from-stream to-stream)
                    (and (live-connection-p to-stream) (listen from-stream))))
             (declare (inline copy-buffer-p))
             (loop with data-copied-this-cycle-p and server-stream-force-output-p and client-stream-force-output-p
                   doing (cond-every
                          ((copy-buffer-p server-stream client-stream)
                           (stream-copy-input-buffer server-stream client-stream)
                           (setq data-copied-this-cycle-p t
                                 client-stream-force-output-p t))
                          ((copy-buffer-p client-stream server-stream)
                           (stream-copy-input-buffer client-stream server-stream)
                           (setq data-copied-this-cycle-p t
                                 server-stream-force-output-p t)))
                   while (shiftf data-copied-this-cycle-p nil)
                   ;; Ship any pending data in a stream buffer
                   finally (cond-every
                            (server-stream-force-output-p
                             (force-output server-stream))
                            (client-stream-force-output-p
                             (force-output client-stream)))))))
    (let* ((client-stream (proxy-tunnel-client-stream tunnel))
           (server-stream (proxy-tunnel-server-stream tunnel))
           (timeout (proxy-tunnel-timeout tunnel))
           (stream-list (list client-stream server-stream)))
      (declare (dynamic-extent stream-list))
      ;;   (format http::trace "~&TUNNEL-THROUGHPUT: ~S <-> ~S" client-stream server-stream)
      (unwind-protect
          (loop doing (tunnel-throughput-until-quiesence client-stream server-stream)
                while (and (live-connection-p client-stream)
                           (live-connection-p server-stream)
                           (input-available-on-streams-p stream-list "Tunnel Wait" timeout)))
        ;; Copy any residual data already in an input buffer when a stream goes down
        (tunnel-throughput-until-quiesence client-stream server-stream)))))

(defmethod invoke-proxy-service ((server proxy-server-mixin) uri (method (eql :connect)) http-version)
  (multiple-value-bind (host port)
      (url::get-host-port-info uri 0 (length uri) t)
    (with-http-authority (authority host port uri)
      ;; check the controls on connect method
      (let ((allowed-ports *proxy-connect-allowed-ports*)
            (deny-subnets *proxy-connect-disallowed-destination-subnets*))
        (cond-every
         ((not (member (the fixnum port) allowed-ports :test #'=))
          (error 'proxy-connect-method-forbidden :method method :url (make-http-authority nil host port authority)))
         ((and deny-subnets (ip-host-trusted-p (host-object authority) deny-subnets))
          (error 'proxy-connect-method-forbidden :method method :url (make-http-authority nil host port authority))))
        ;; now get down to business
        (flet ((report-tunnnel-established (server)
                 (let ((response-headers (list :content-type '(:application :tunnel) ;loutonen draft RFC
                                               :content-length 0
                                               :via (%compute-via-header nil server)))
                       (stream (server-stream server)))
                   (declare (dynamic-extent response-headers))
                   (send-response stream nil
                                  :status :connection-established
                                  :additional-mime-headers response-headers)
                   (force-output stream)))
               (proxy-report-tunnel-status (server server-stream request-http-version client client-stream client-status)
                 (let ((client-http-version (client-request-version client))
                       (client-response-headers (client-response-headers client)))
                   (cond ((response-status-code-implies-entity-p client-status)
                          (let ((content-length (get-header :content-length client-response-headers)))
                            (multiple-value-bind (additional-headers transfer-encoding)
                                (get-transfer-encoding-header-plist content-length request-http-version)
                              (write-proxy-response-headers server client-response-headers client-http-version content-length server-stream additional-headers)
                              ;; Send the entity body
                              (handling-optional-entity-for-status-code
                                  (client-status client-response-headers :clean-up-form (setf (server-close-connection-p server) t))
                                (with-binary-stream (server-stream :output)
                                  (with-binary-stream (client-stream :input)
                                    (with-transfer-encoding (server-stream transfer-encoding)
                                      (with-transfer-decoding* (client-stream authority http-version :headers client-response-headers :copy-mode :binary)
                                        (stream-copy-until-eof client-stream server-stream :binary)))))))))
                         (t (write-proxy-response-headers server client-response-headers client-http-version t server-stream))))))
          (declare (inline report-tunnnel-established))
          ;; main body
          (when *trace-proxy*
            (format *trace-output*  "~&;Proxying on port ~D a ~A ~A request for ~S." *standard-http-port* http-version method authority)
            (format *trace-output* "~&;~2TClient Request Headers:")
            (write-header-buffer (server-headers server) *trace-output*))
          (let ((request-stream (server-stream server)))
            (let ((host (host-string authority))
                  (port (host-port authority)))
              ;; (format http::trace "~&INVOKE-PROXY-SERVICE: ~A:~D" host port)
              (let ((proxy (standard-proxy-for-host-port :https host port)))
                (cond (proxy
                       (flet ((write-the-request-headers (stream method http-version)
                                (declare (ignore method))
                                (write-proxy-request-headers server (server-headers server) http-version stream)))
                         (declare (dynamic-extent #'write-the-request-headers))
                         (let ((request-http-version http-version))
                           (with-standard-proxy (proxy)
                             (with-http-request (authority :connect :request-headers #'write-the-request-headers)
                               (let* ((client-status (client-status client)))
                                 (declare (fixnum client-status))
                                 (send-status-line request-stream (proxy-maybe-downgrade-status-code client-status request-http-version) (client-reason client))
                                 (cond ((< 199 client-status 300)
                                        (with-proxy-tunnel (tunnel request-stream remote-stream *persistent-connection-timeout*)
                                          (proxy-report-tunnel-status server request-stream request-http-version client remote-stream client-status)
                                          (force-output request-stream)
                                          ;; close connection in both directions because we can't be certain of the termination state
                                          (setf (server-close-connection-p server) t
                                                (connection-close-p *connection*) t)
                                          (tunnel-throughput tunnel)))
                                       (t (proxy-report-tunnel-status server request-stream request-http-version client remote-stream client-status)))))))))
                      (t (let (remote-stream)
                           (unwind-protect
                               (progn (setq remote-stream (open-http-stream-to-host host port))
                                 (with-proxy-tunnel (tunnel request-stream remote-stream *persistent-connection-timeout*)
                                   (report-tunnnel-established server)
                                   (setf (server-close-connection-p server) t) ;close connection
                                   (tunnel-throughput tunnel)))
                             (when remote-stream
                               (close remote-stream :abort (not (live-connection-p remote-stream))))))))))))))))