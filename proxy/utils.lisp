;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: HTTP; Base: 10 -*-
;;;
;;; (C) Copyright 1998, 2000, 2003,  John C. Mallery
;;;     All Rights Reserved.
;;;
;;; (C) Copyright 1996-1997, Christopher R. Vincent and John C. Mallery
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;;  UTILITIES
;;;
(in-package :http)

;;;------------------------------------------------------------------- 
;;;
;;; UNIQUE IDS
;;;

(define-uid-series proxy-uid *proxy-uid-series*
  :directory "http:pw;uid;"
  :interval 10					;could be increased for high performance servers
  :width 0)					;an aethestic question concerning readability of directory listings

(declaim (inline allocate-proxy-uid))

(defun allocate-proxy-uid ()
  "Allocates a numeric ID that is unique across reboots."
  (uid-series-allocate *proxy-uid-series*))

(declaim (inline proxy-uid-string))

(defun proxy-uid-string (uid-number)
  "Returns a string containing the proxy uid, UID-NUMBER, with appropriate leading zeros."
  (uid-series-string *proxy-uid-series* uid-number))


;;;------------------------------------------------------------------- 
;;;
;;; DEBUGGING
;;;

(defun debug-proxy (&optional (on-p (not *debug-proxy*)))
  "Toggle proxy debugging.
Turns off condition handling in proxy-specific code"
  (setf *debug-proxy* on-p))

#+Genera
(add-periodic-task "Turn Off Proxy Debugging" :hourly '(http::debug-proxy nil))

(defun trace-proxy (&optional (on-p (not *trace-proxy*)))
  "Toggle proxy tracing."
  (setf *trace-proxy* on-p))

(defmacro proxy-trace (format-string &rest format-args)
  `(when *trace-proxy*
     (format *trace-output* ,format-string ,@format-args)))


;;;------------------------------------------------------------------- 
;;;
;;; VERSION
;;;

(define proxy-version (&optional (recache-p t))
  "Returns the proxy version, for example \"(CL-HTTP/70.41)\"
which is short that SERVER-VERSION-INFO."
  (cond ((and (not recache-p) *proxy-version*))
	(t (destructuring-bind (major minor &rest port-version-numbers)
	       (www-utils:%server-version-info)
	     (declare (ignore port-version-numbers))
	     (setq *proxy-version* (format nil "(CL-HTTP/~D.~D)" major minor))))))

(add-periodic-task "Initialize Proxy Version" :daily '(http::proxy-version t))


;;;------------------------------------------------------------------- 
;;;
;;; ERRORS
;;;

;; Report bugs when these conditions arise.
(defmethod report-status :around ((condition proxy-unresolvable-domain-name) stream)
  stream                                        ;ignore
  (when (www-utils:live-connection-p stream)
    (call-next-method))
  (bug-report-error condition))

(defmethod report-status :around ((condition proxy-local-network-error) stream)
  stream                                        ;ignore
  (when (www-utils:live-connection-p stream)
    (call-next-method))
  (bug-report-error condition))

(defmethod report-status-message ((condition proxy-invalid-authentication-credentials) stream &optional format-string format-args)
  (flet ((report-invalid-credentials (url stream)
	   (let ((proxy (http-proxy condition))
		 (realm (http-realm condition)))
	     (format stream "Unable to access (~A) URI ~A~%due to local proxy configuration error.
~%Please advise the proxy maintainer for ~A on port ~D~%to provide valid credentials for ~A access to the~%upstream proxy ~A on port ~D."
		     (http-method condition) (name-string url) (local-host-domain-name) (server-host-local-port *server*)
		     (typecase realm
		       (string realm)
		       (cons (first realm))
		       (t (or (http-realm condition) "restricted")))
		     (proxy-domain-name proxy) (proxy-port proxy)))))
    (declare (dynamic-extent #'report-invalid-credentials))
    (let ((format (or format-string (format-string condition))))
      (if format
	  (call-next-method condition stream format (or format-args (format-args condition)))
	  (%report-status-message (http-url condition) (http-reason condition) stream #'report-invalid-credentials)))))


;;;------------------------------------------------------------------- 
;;;
;;; COMPUTING HEADERS
;;;

(defgeneric get-header-value (headers header-keyword)
  (declare (values parsed-value found-p))
  (:documentation "Returns the parsed value for HEADER-KEYWORD in HEADERS.
HEADERS can be either a header-set or a header-plist"))

(defmethod get-header-value ((header-set header-set) header-keyword)
  (get-header header-keyword header-set))

(defmethod get-header-value ((header-plist cons) header-keyword)
  (let ((value (getf header-plist header-keyword :+not-found+)))
    (case value
      (:+not-found+ (values nil nil))
      (t (values value t)))))

(declaim (inline cacheable-response-header-p))

(defun cacheable-response-header-p (header-keyword)
  "Returns non-null if HEADER-KEYWORD denotes a cacheable response header."
  (not (member header-keyword *hop-by-hop-headers*)))

;; errors parsing headers cannot be tolerated
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
      (with-fast-array-references ((.headers. .headers. vector)
				   (index index vector))
	(loop with header-value and parsing-error-p
	      for idx fixnum upfrom 0 below (fill-pointer .headers.)
	      for header = (aref .headers. idx)
	      for header-keyword = (aref index idx)
	      when (collect-p header-keyword header include-headers exclude-headers)
		do (multiple-value-setq (header-value parsing-error-p) (safe-header-value header))
		and unless parsing-error-p
		      collect header-keyword
		      and collect header-value)))))

(declaim (inline cacheable-request-header-p))

(defun cacheable-request-header-p (header-keyword)
  "Returns non-null if HEADER-KEYWORD denotes a cacheable request header."
  (not (member header-keyword '#.`(,@*hop-by-hop-headers*
				   ,@*request-specific-request-headers*
				   ,@*conditional-request-headers*))))

;; errors parsing headers cannot be tolerated 
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
      (with-fast-array-references ((headers .headers. vector)
				   (index index vector))
	(loop with header-value and parsing-error-p
	      for idx fixnum upfrom 0 below (fill-pointer headers)
	      for header = (aref headers idx)
	      for header-keyword = (aref index idx)
	      when (collect-p header-keyword header include-headers exclude-headers)
		do (multiple-value-setq (header-value parsing-error-p) (safe-header-value header))
		and unless parsing-error-p
		      collect header-keyword
		      and collect header-value)))))

;;;------------------------------------------------------------------- 
;;;
;;; COMPUTING HEADERS
;;; 

(defun %compute-via-header (current-via-header &optional (server *server*) (preserve-comments-p *via-header-preserve-comments*))
  "Returns a updated VIA header list that include the local proxy."
  (flet ((local-proxy-port (server)
	   (let ((port (server-host-local-port server)))
	     (case port
	       (80 nil)				;assumes HTTP
	       (t (list port))))))
    (declare (inline local-proxy-port))
    (unless preserve-comments-p			;Destructively strip comments off existing entries 
      (dolist (item current-via-header)
	(setf (third item) nil)))
    ;; Return updated parsed headers
    `((,(server-http-version server)
       (,(local-host-domain-name) ,.(local-proxy-port server))
       ,.(when preserve-comments-p (list (proxy-version))))
      ,.current-via-header)))

(declaim (inline status-code-implies-entity-p))

(defun response-status-code-implies-entity-p (code)
  "Returns whether a status code implies a message body."
  (not (member code *response-status-codes-with-no-entity*)))

(declaim (inline status-code-implies-optional-entity-p))

(defun response-status-code-implies-optional-entity-p (code)
  "Returns whether a status code implies that a message body is optional."
  (member code *response-status-codes-with-optional-entity*))

(declaim (inline response-status-code-requires-no-entity-p))

(defun response-status-code-requires-no-entity-p (code)
  "Returns whether a status code requires no message body."
  (member code *response-status-codes-with-no-entity*))

;; This should also handle network errors, but it needs to determine that the
;; input stream has the error.   8/25/2000 -- JCMa.
(defmacro handling-optional-entity-for-status-code ((status-code headers &key clean-up-form error-var) &body body)
  "Handles cases in which there is an error transfering the entity body from the orgin server.
WHen STATUS-CODE implies an optional entity, this form handles EOF
occurring within BODY, where body is understood to be accessing the entity data from
the orgin server."
  `(cond ((response-status-code-implies-optional-entity-p ,status-code)
	  (with-header-values (content-length) ,headers
	    (unless (eql content-length 0)
	      (handler-case
		(progn . ,body)
		(end-of-file ,(if error-var `(,error-var) '()) ,clean-up-form)))))
	 (t . ,body)))
						
(defgeneric compute-via-header (header-set)
  (:documentation "Returns a new VIA header value based on HEADER-SET that includes the local host."))

(defmethod compute-via-header ((header-set header-set))
  (let ((via (get-header :via header-set)))
    (%compute-via-header via)))

(defmethod compute-via-header ((header-plist cons))
  (let ((via (getf header-plist :via)))
    (%compute-via-header via)))

(defmethod compute-via-header ((header-plist null))
  (%compute-via-header nil))

(define-header :proxy-keep-alive
               (:comma-separated-header :request)
  :print-string "Proxy-Keep-Alive"
  :parse-function 'parse-keep-alive-header
  :print-function 'print-keep-alive-header)

(defun proxy-persistent-connection-parameters (server content-length)
  (declare (values persistent-connection-p connection-header-plist close-connection-p))
  (case (server-http-version server)
    (:http/1.0
      ;; content-length is an integer, NIL, or T. T is used when we know there is no entity body.
      (cond ((and content-length (persistent-connection-p server t))
	     (multiple-value-bind (timeout max-requests)
		 (%server-persistent-connection-parameters server t)
	       (values t `(:proxy-connection (:keep-alive) :keep-alive (:timeout ,timeout :max ,max-requests)))))
	    (t (values nil nil))))
    (:http/0.9 nil)
    ;; Default in HTTP 1.1 is to always use persistent connections.
    (t (if (persistent-connection-p server t)
	   (values t nil)
	   (values nil (list :connection '(:close)))))))

;; Should check (server-http-version server) and (client-connection-version client) to perform appropriate modifications.
(defun %write-proxy-response-headers (stream server response-headers response-http-version content-length date-header-p
					     header-plist modification-plist)
  (declare (optimize (speed 3))
           (ignore response-http-version))
  (let* ((new-via (compute-via-header response-headers))
	 (modification-plist (if date-header-p
				 (list* :via new-via modification-plist)
                               (list* :date (server-request-time server) :via new-via modification-plist)))
	 (additional-headers header-plist))
    (declare (dynamic-extent new-via modification-plist))
    (multiple-value-bind (persistent-connection-p connection-header-plist close-connection-p)
	(proxy-persistent-connection-parameters server content-length)
      (cond-every
       (persistent-connection-p (setf (server-persistent-connection-p server) t))
       (close-connection-p (setf (server-close-connection-p server) t))
       (connection-header-plist (setq additional-headers (nconc connection-header-plist additional-headers))))
      ;; write the response headers
      (when *trace-proxy*
	(fast-format *trace-output* "~&;Proxy Response ~A Status ~D~&;Proxy Response Headers:~&" (server-http-version server) (server-status server))
	(write-modified-headers response-headers *trace-output*  modification-plist *hop-by-hop-headers* t additional-headers))
      (write-modified-headers response-headers stream modification-plist *hop-by-hop-headers* t additional-headers))))

(defgeneric write-proxy-response-headers (server response-headers response-http-version content-length stream
						 &optional header-plist modification-plist))

(defmethod write-proxy-response-headers ((server proxy-server-mixin) (response-headers header-set) response-http-version content-length stream
					 &optional header-plist modification-plist)
  (%write-proxy-response-headers stream server response-headers response-http-version content-length (get-header-object :date response-headers)
				 header-plist modification-plist))

(defmethod write-proxy-response-headers ((server proxy-server-mixin) (response-headers cons) response-http-version content-length stream
					 &optional header-plist modification-plist)
  (declare (optimize (speed 3)))
  (%write-proxy-response-headers stream server response-headers response-http-version content-length (getf response-headers :date)
				 header-plist modification-plist))

(defgeneric write-proxy-request-headers (server request-headers request-http-version stream &optional header-plist))

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

(defmethod write-proxy-request-headers ((server proxy-server-mixin) request-headers (request-http-version (eql :http/0.9)) stream &optional header-plist)
  (%write-proxy-http-1-0-request-headers server request-headers request-http-version stream header-plist))

(defmethod write-proxy-request-headers ((server proxy-server-mixin) request-headers (request-http-version (eql :http/1-0)) stream &optional header-plist)
  (%write-proxy-http-1-0-request-headers server request-headers request-http-version stream header-plist))

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


;;;------------------------------------------------------------------- 
;;;
;;; GENERIC CLIENT LOG CALLBACK METHODS
;;;

(defstruct (client-data)
  message-tag
  http-version
  status 
  connection-requests
  cpu-milliseconds 
  elapsed-milliseconds)

(defgeneric server-push-client-data (server client-data)
  (:documentation "Makes SERVER aware of CLIENT-DATA for proxy logging purposes."))

(defmethod server-push-client-data ((server proxy-server-mixin) (client-data client-data))
  (push client-data (server-proxy-client-data server)))

(defgeneric server-get-client-data (server log)
  (:documentation "Returns client callback data stored on SERVER for LOG."))

(defmethod server-get-client-data ((server proxy-server-mixin) (log proxy-log-client-callback-mixin))
  (let ((client-data-entry (server-proxy-client-data server)))
    (when client-data-entry
      (loop with message-tag = (proxy-log-message-tag log)
	    for item in client-data-entry
	    when (eq message-tag (client-data-message-tag item))
	      return item))))

(defgeneric get-client-request-log (log)
  (declare (values request-log newly-created-p))
  (:documentation "Gets the correct instance of client request log suitable to provide callback data for LOG."))

(defmethod get-client-request-log ((log proxy-log-client-callback-mixin))
  (let ((class (proxy-client-log-class log)))
    (intern-client-request-log (string-capitalize class) :class class :if-does-not-exist :create)))

(defgeneric proxy-log-add-client-request-log (log)
  (declare (values request-log-added-p))
  (:documentation "Adds a client request log that produces callback data for log, as necessary."))

(defmethod proxy-log-add-client-request-log ((log proxy-log-client-callback-mixin))
  (unless (proxy-client-log log)
    (let ((request-log (get-client-request-log log)))
      (add-client-request-log request-log)
      (pushnew log (proxy-server-logs request-log))
      (setf (proxy-client-log log) request-log))))

(defgeneric proxy-log-remove-client-request-log (log)
  (declare (values request-log-removed-p))
  (:documentation "Removes a client request log that produces callback data for log, as necessary."))

(defmethod proxy-log-remove-client-request-log ((log proxy-log-client-callback-mixin))
  (let ((client-log (proxy-client-log log)))
    (when client-log
      (prog1 (unless (setf (proxy-server-logs client-log) (delete log (proxy-server-logs client-log)))
	       (remove-client-request-log client-log)
	       t)
	     (setf (proxy-client-log log) nil)))))

;; These methods ensure that the client request log remains in synch with the server access log
(defmethod add-access-log :after ((log proxy-log-client-callback-mixin) port)
  (declare (ignore port))
  (proxy-log-add-client-request-log log))

(defmethod remove-access-log :before ((log proxy-log-client-callback-mixin) port)
  (declare (ignore port))
  (proxy-log-remove-client-request-log log))


;;;------------------------------------------------------------------- 
;;;
;;; PROXY EXTENDED COMMON LOG FORMAT
;;;

;; define the data capture method on the client request log
(defmethod log-client-request and ((log proxy-extended-common-file-log-client-callback) (client client-logging-mixin) &aux (server *server*))
  (when (and server (server-proxy-request-p server))
    (let ((connection (client-connection client)))
      (server-push-client-data server (make-client-data :message-tag (proxy-log-message-tag log)
							:http-version (connection-version connection)
							:connection-requests (connection-requests-completed connection)
							:status (client-status client)
							:cpu-milliseconds (cpu-milliseconds client)
							:elapsed-milliseconds (elapsed-milliseconds client))))))

;; Write the actual log entry
(defun %write-proxy-extended-common-logfile-entry (host-name request request-time status bytes-transmitted
                                                             user-name bytes-received requests-completed
							     cpu-milliseconds elapsed-milliseconds cache-hit-p client-data
                                                             &optional (gmt-p *log-times-in-gmt*) (stream *standard-output*)
                                                             (delimiter #\Space))
  (%write-common-logfile-entry host-name request request-time status bytes-transmitted user-name gmt-p stream delimiter)
  (fast-format stream "~C~D~C~D~C~D~C~D~C~C"
	       delimiter bytes-received delimiter requests-completed delimiter cpu-milliseconds delimiter elapsed-milliseconds
	       delimiter (if cache-hit-p #\+ #\-))
  (when client-data
    (fast-format stream "~C\"~A\"~C~D~C~D~C~D~C~D"
		 delimiter (client-data-http-version client-data)
		 delimiter (client-data-status client-data)
		 delimiter (client-data-connection-requests client-data)
		 delimiter (round (client-data-cpu-milliseconds client-data))
		 delimiter (round (client-data-elapsed-milliseconds client-data)))))

(defclass proxy-extended-common-log-entry
	  (requests-completed-log-entry-mixin metering-log-entry-mixin common-log-entry)
    ((client-data :initarg :client-data :accessor log-entry-client-data)
     (cache-hit-p :initarg :cache-hit-p :accessor log-entry-cache-hit-p)))

(defmethod write-log-entry ((entry-data proxy-extended-common-log-entry) stream)
  (with-slots (owner host-name request request-time status bytes-transmitted user-name bytes-received requests-completed
		     cpu-time elapsed-time cache-hit-p client-data) entry-data
    (%write-proxy-extended-common-logfile-entry
      host-name request request-time status bytes-transmitted user-name bytes-received
      requests-completed cpu-time elapsed-time cache-hit-p client-data (log-times-in-gmt-p owner) stream #\Space)
    ;; Trailing CR makes this consistently parsable.
    (terpri stream)))

(defmethod deinitialize-log-entry progn (resource (log-entry proxy-extended-common-log-entry))
  (declare (ignorable resource))
  (setf (log-entry-client-data log-entry) nil))

(define-log-entry-allocator allocate-proxy-extended-common-log-entry (log host-name request request-time status bytes-transmitted user-name
							  bytes-received requests-completed cpu-time elapsed-time client-data cache-hit-p)
  (allocate-log-entry proxy-extended-common-log-entry
		 :owner log :host-name host-name :request request :request-time request-time :status status
		 :bytes-transmitted bytes-transmitted :user-name user-name :bytes-received bytes-received
		 :requests-completed requests-completed :cpu-time cpu-time :elapsed-time elapsed-time
		 :client-data client-data :cache-hit-p cache-hit-p))

(defmethod snapshot-log-entry ((log proxy-extended-common-file-format-mixin) (server server-logging-mixin))
  (let ((host-name (host-log-name server))
	(user-name (%server-user-qualified-name server))
	(request (server-request server t))	; don't lose when transaction reset
	(request-time (server-request-time server))
	(status (server-status server))
	(bytes-transmitted (server-bytes-transmitted server))	;total bytes (not number of bytes in a document)
	(bytes-received (server-bytes-received server))
	(requests-completed (server-requests-completed server))
	(cpu-milliseconds (cpu-milliseconds server))
	(elapsed-milliseconds (elapsed-milliseconds server))
	(client-data (server-get-client-data server log))
	(cache-hit-p (server-proxy-cache-hit-p server)))
    (allocate-proxy-extended-common-log-entry log host-name request request-time status bytes-transmitted user-name
						   bytes-received requests-completed cpu-milliseconds elapsed-milliseconds
						   client-data cache-hit-p)))

(define ensure-extended-proxy-log (&key (port *standard-proxy-port*)
                                         (name "Proxy-Extended-Log")
                                         (directory "http:logs;proxy;")
                                         (class 'proxy-extended-common-file-log))
  "Ensures that an extended proxy log is present on PORT."
  (declare (values log newly-created-p))
  (ensure-extended-access-log :port port :name name :directory directory :class class))

