;;; -*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-
;;;
;;; (C) Copyright 1994-2001, 2003, 2005-2007, 2009, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; ANSI COMMON LISP HTTP SERVER
;;;
(in-package :http)

(defun make-server (resource &optional stream host address)
  (declare (ignore resource)) 
  (let ((class *server-class*))
    (case class
      (server
        (make-instance 'server :stream stream :host host :address address))
      (t (make-instance class :stream stream :host host :address address)))))

(defmethod print-object ((server basic-server-mixin) stream)
  (with-slots (host) server
    (let ((server-host (local-host)))
      (print-unreadable-object (server stream :type t :identity t)
        (cond-every
          (server-host (write-string (host-http-name server-host) stream))
          (host
            (write-char #\> stream)
            (write-string (host-http-name host) stream)))
        server))))

(define-generic free-server-resources (server)
  (:documentation "Returns any resourced data structures to free status in their respective resources."))

(defmethod free-server-resources ((server basic-server-mixin))
  (deallocate-resource 'line-buffer (%server-request server))
  (setf (%server-request server) nil)
  (deallocate-resource 'line-buffer (server-url-buffer server))
  (setf (server-url-buffer server) nil)
  (deallocate-resource 'header-set (server-headers server))
  (setf (server-headers server) nil))

(define-generic initialize-resourced-server (resource server set-stream set-host set-address))

(defmethod initialize-resourced-server (resource (server basic-server-mixin) set-stream set-host set-address)
  (declare (ignore resource))
  (let ((process (www-utils:current-process)))
    ;; initialize these if they are not already present
    (unless-every
      ((server-headers server)
       (setf (server-headers server) (allocate-resource 'header-set)))
      ((%server-request server)
       (setf (%server-request server) (allocate-resource 'line-buffer *line-buffer-size*)))
      ((server-url-buffer server)
       (setf (server-url-buffer server) (allocate-resource 'line-buffer *line-buffer-size*))))
    ;; initialize the usual slots
    (setf (server-stream server) set-stream
          (server-address server) set-address
          (server-host server) set-host
          (server-timeout server) *server-timeout*
          (server-life-time server) *server-life-time*
          (server-process server) process
          (server-start-time server) (get-internal-real-time)
          (server-process-start-time server) (www-utils:process-run-time process))
    server))

(define-generic deinitialize-resourced-server (resource server))

(defmethod deinitialize-resourced-server (resource (server basic-server-mixin))
  (declare (ignore resource))
  (with-slots (close-connection-p persistent-connection-p stream address host requests-completed
                                  request-copy method url url-string status form-data plist timeout
                                  life-time start-time process process-start-time) server
    ;; reset resourced slots
    (cond (*preserve-http-server-resources*
	   (setf (fill-pointer (%server-request server)) 0
		 (fill-pointer (server-url-buffer server)) 0)
	   (clear-header-set (server-headers server) t))
	  (t (free-server-resources server)))
    ;;reset instance variables
    (setq stream nil
          address nil
          host nil
	  request-copy nil
          method nil
          url-string nil
          url nil
          plist nil
          status nil
	  form-data nil
          requests-completed 0
          close-connection-p nil
	  persistent-connection-p nil
          timeout 0
          life-time 0
          start-time 0
          process nil
          process-start-time 0))
  server)

(defmethod deinitialize-resourced-server :after (resource (server server-authentication-mixin))
  (declare (ignore resource))
  (with-slots (rfc-931-response-type rfc-931-response user-object) server
    (setq rfc-931-response-type nil
          rfc-931-response nil
          user-object nil)
    server))

(defmethod deinitialize-resourced-server :after (resource (server proxy-server-mixin))
  (declare (ignore resource))
  (with-slots (proxy-request-p client-data) server
    (setq proxy-request-p nil
	  client-data nil)
    server))

(define-generic set-server-status (server status-code)
  (:documentation "Sets the lastest status code to STATUS-CODE for SERVER."))

(defmethod set-server-status ((server null) status-code)
  status-code)

(defmethod set-server-status ((server server-logging-mixin) status-code)
  (check-type status-code integer)
  (with-slots (status) server
    (setq status status-code)))

(defsetf server-status set-server-status)

(define-generic server-proxy-request-p (server)
  (:documentation "Returns non-null when the current request is a proxy request.
When the proxy cache is hit, the value is :CACHE-HIT."))

(defmethod server-proxy-request-p ((server null))
  nil)

(define-generic server-proxy-cache-hit-p (server)
  (:documentation "Returns non-null when the current proxy request was served from the proxy cache."))

(defmethod server-proxy-cache-hit-p ((server proxy-server-mixin))
  (with-slots (proxy-request-p) server
    (eq :cache-hit proxy-request-p)))

(define-generic server-note-proxy-cache-hit (server)
  (:documentation "Informs server that a proxy request has been served out of the proxy cache."))

(defmethod server-note-proxy-cache-hit ((server proxy-server-mixin))
  (with-slots (proxy-request-p) server
    (setq proxy-request-p :cache-hit)))

(define-generic server-live-connection-p (server)
  (:documentation "Returns non-null if SERVER has a live connection to the client."))

(defmethod server-live-connection-p ((server basic-server-mixin))
  (let ((stream (server-stream server)))
    (and stream (live-connection-p stream))))

(define-generic server-bytes-transmitted (server)
  (:documentation "Returns the number of bytes transmitted by the server during the current HTTP transaction."))

(defmethod server-bytes-transmitted ((server basic-server-mixin))
  (bytes-transmitted (server-stream server)))

(define-generic server-bytes-received (server)
  (:documentation "Returns the number of bytes received by the server during the current HTTP transaction."))

(defmethod server-bytes-received ((server basic-server-mixin))
  (bytes-received (server-stream server)))

(define-generic server-http-version (server)
  (:documentation "Returns keyword HTTP version for the client currently connected to SERVER."))

(defmethod server-http-version ((server basic-server-mixin))
  (with-slots (http-version) server
    (or http-version
        :http/1.0)))                            ;default to HTTP/1.0 for now -- jcma 10/21/96

(defmethod server-http-version ((server null)) nil)     ;useful for debugging.

(defmethod (setf server-http-version) ((version symbol) (server basic-server-mixin))
  (with-slots (http-version) server
    (setq  http-version version)))

(define-generic server-request (server &optional durable-p)
  (:documentation "Returns the HTTP request line for the current transaction on SERVER.
When DURABLE-P is non-null, the returned string is valid outside the current HTTP transaction,
otherwise it is not."))

(defmethod server-request ((server basic-server-mixin) &optional (durable-p t))
  (with-slots (request) server
    (cond (durable-p
	   (or (server-request-copy server)
	       (setf (server-request-copy server) (copy-seq request))))
	  (t request))))

(defgeneric server-url-string (server)
  (:documentation "Returns the full-qualified URL that was requested.
Obtains the namestring from the url string buffer. This may be a relative URL up to the point where a
virtual host is selected and merged destructively into this string."))

(defmethod server-url-string ((server null))
  nil)

(defgeneric server-raw-request-relative-url-string (server)
  (:documentation "Returns the raw relative URL string from the request.  
The raw string requested may differ from the namestring for the interned URL due to cannicalization
or escaping issues."))

(defmethod server-raw-request-relative-url-string ((server basic-server-mixin))
  (with-slots (url-string) server
    (with-value-cached (server :raw-request-relative-url-string)
      (let ((string url-string))
        (url::%relative-name-string string (fill-pointer string) nil)))))

(defgeneric server-raw-request-url-string (server)
  (:documentation "Returns the raw URL string from the request.
This is a relative URL for normal server requests and a fully qualified URL for proxy requests.  The
raw string requested may differ from the namestring for the interned URL due to cannicalization or
escaping issues."))

(defmethod server-raw-request-url-string ((server basic-server-mixin))
  (with-slots (url-string) server
    (with-value-cached (server :server-raw-request-url-string)
      (if (server-proxy-request-p server)
          url-string
        (server-raw-request-relative-url-string server)))))

(defmethod server-url ((server null))
  nil)

(defmethod url:authentication-realm ((server server-authentication-mixin))
  (let ((url (server-url server)))
    (when url
      (url:authentication-realm url))))

(defmethod url:capabilities ((server server-authentication-mixin))
  (let ((url (server-url server)))
    (when url
      (url:capabilities url))))

(define-generic server-email-address (server)
  (:documentation "Returns the email address associated with SERVER."))

(defmethod server-email-address ((server server))
  *server-mail-address*)

(define-generic server-host-mail-name (server)
  (:documentation "Returns the mail-name for the client being served by SERVER."))

(defmethod server-host-mail-name ((server basic-server-mixin))
  (with-slots (host) server
    (host-domain-name host t)
    (host-mail-name host)))

(define-generic server-host-domain-name (server)
  (:documentation "Returns the domain name for the client being served by SERVER."))

(defmethod server-host-domain-name ((server basic-server-mixin))
  (with-slots (host) server
    (host-domain-name host t)))

(define-generic server-host-ip-address (server)
  (:documentation "Returns the IP address for the client being served by SERVER."))

(defmethod server-host-ip-address ((server basic-server-mixin))
  (with-slots (host) server
    (host-ip-address host)))

(define-generic server-host-local-port (server)
  (:documentation "Returns the local port from which the client being served by SERVER."))

(defmethod server-host-local-port ((server basic-server-mixin))
  (with-slots (stream) server
    (www-utils:local-port stream)))

(define-generic server-host-local-protocol (server)
  (:documentation "Returns the local protocol used to communicate with the client."))

(defmethod server-host-local-protocol ((server basic-server-mixin))
  (with-slots (stream) server
    (www-utils:local-protocol stream)))

(define-generic server-host (server)
  (:documentation "Returns the parsed host, which is the client being served by SERVER."))

(define-generic server-http-version-string (server)
  (:documentation "Returns the HTTP version string for the requesting client."))

(defmethod server-http-version-string ((server basic-server-mixin))
  (let ((http-version (server-http-version server)))
    (if http-version
        (symbol-name http-version)
        nil)))
(defmethod server-http-version-string ((server null))
  *http-version*)

(define-generic server-send-mail (server user-or-server subject message-writer &key  keywords comments file-references)
  (:documentation "Sends an email message to USER-OR-SERVER with SUBJECT from SERVER.
MESSAGE-WRITER is either a string or a function accepting a single stream argument."))

(defmethod server-send-mail ((server string) (to string) subject message-writer &key keywords comments file-references)
  (send-mail-from server to subject message-writer :keywords keywords :comments comments :file-references file-references))

(defmethod server-send-mail ((server server)  (to string) subject message-writer &key keywords comments file-references)
  (send-mail-from (server-email-address server) to  subject message-writer :keywords keywords :comments comments :file-references file-references))

(defmethod server-send-mail (server (user user) subject message-writer &key keywords comments file-references)
  (send-mail-from server (user-email-address user) subject message-writer :keywords keywords :comments comments :file-references file-references))

(define-generic client-http-version-meets-p (server http-version)
  (:documentation "Returns non-null when the client connected to the server
has a http protocol version that equals or exceeds HTTP-VERSION, which must
be a HTTP protocol version keyword of the form :HTTP/1.1
Returns NIL when server version is unknown."))

;; This can be called to determine protocol response in bad syntax cases where
;; we don't know the HTTP version. Consequently, it always downgrades the
;; protocol version for the client to get a connection closure.
(defmethod client-http-version-meets-p ((server basic-server-mixin) version)
  (let ((client-version (server-http-version server)))
    (not (or (null client-version)
             (http-version-less-p client-version version)))))

(define-generic server-url-search-keys (server)
  (:documentation "When the current URL is a search URL, this returns 
a list of strings which are the search keys. Otherwise it returns NIL."))

(defmethod server-url-search-keys ((server basic-server-mixin))
  (with-slots (url) server
    (search-keys url)))

(define-generic server-authentication-method (server)
  (:documentation "The protocol-specific authentication method used to validate the user."))

;; Nobody implements rfc-931, so use http authentication methods.
(defmethod server-authentication-method-string ((server server-authentication-mixin))
  (with-slots (user-object) server
    (when user-object
      (string (realm-scheme (user-realm user-object))))))

(defmethod server-authentication-method ((server server-authentication-mixin))
  (with-slots (user-object) server
    (when user-object
      (realm-scheme (user-realm user-object)))))

(defmethod allow-user-access-p (access-control (server server-authentication-mixin) http-method)
  (allow-user-access-p access-control (server-user-object server) http-method))

(define-generic server-get-header (server header)
  (:documentation "Returns the parsed value for HEADER for SERVER's current HTTP transaction.
HEADER is a keyword denoting an HTTP header."))

(defmethod server-get-header (server header)
  (check-type header keyword)
  (get-header header (server-headers server)))

(define-generic server-get-raw-header (server header &optional durable-p)
  (:documentation "Returns the parsed value for HEADER for SERVER's current HTTP transaction.
HEADER is a keyword denoting an HTTP header.
When DURABLE-P is non-null, this returns a raw value that
persists beyond the lifetime of the current header set."))

(defmethod server-get-raw-header (server header &optional durable-p)
  (check-type header keyword)
  (get-raw-header header (server-headers server) durable-p))

(define-generic server-software-version (server)
  (:documentation "Returns a string denoting software version which SERVER is running."))

(defmethod server-software-version (server)
  (declare (ignore server))
  *server-version*)

(define-generic server-idle-time (server)
  (:documentation "Returns the idle time in 60ths of a second for SERVER's process,
or NIL when no process exists."))

(defmethod server-idle-time ((server basic-server-mixin))
  (let ((process (server-process server)))
    (and process (www-utils:process-idle-time process))))

(define-generic server-timeout (server)
  (:documentation "Returns the amount of idle time in 60ths of a second before an idle
HTTP connection times out.  A response function can setf this value in
order to allow more or less idle time.  *SERVER-TIMEOUT* provides the
default value."))

(define-generic server-timeout-p (server)
  (:documentation "Returns non-null when SERVER has timed out.
IDLE-TIME is 60ths of a second."))

(defmethod server-timeout-p ((server basic-server-mixin))
  (declare (optimize (speed 3)))
  (let ((idle-time (server-idle-time server)))
    (when idle-time
      (> (the integer idle-time) (the integer (server-timeout server))))))

(define-generic server-life-time (server)
  (:documentation "Returns the amount of time in milliseconds that SERVER is allowed to
live.  A response function can setf this value in order to allow more or
less idle time. *SERVER-LIFE-TIME* provides the default value."))

(define-generic server-process (server)
  (:documentation "Returns the process in which the server is running."))

(define-generic server-process-p (server process)
  (:documentation "Returns non-null when server runs in PROCESS."))

(defmethod server-process-p ((server basic-server-mixin) process)
  (eql (server-process server) process))

(define-generic server-process-start-time (server)
  (:documentation "Returns the microsecond time when SERVER's process started computing HTTP service."))

(define-generic cpu-time (server-client-or-log)
  (:documentation "Returns the microseconds that SERVER-CLIENT-OR-LOG has been computing."))

(defmethod cpu-time ((server basic-server-mixin))
  (let ((process (server-process server)))
    (if process
        (- (the integer (process-run-time process))
           (the integer (server-process-start-time server)))
      0)))

(define-generic cpu-milliseconds (server-client-or-log)
  (:documentation "Returns the milliseconds that SERVER-CLIENT-OR-LOG has been computing."))

(defmethod cpu-milliseconds ((server basic-server-mixin))
  (let ((process (server-process server)))
    (if process
        (truncate (- (process-run-time process) (server-process-start-time server))
		  1000.)
        0)))

(define-generic server-request-time (server)
  (:documentation "Returns the time in universal time of the request currently being processed by the server."))

(defmethod server-request-time ((server basic-server-mixin))
  (or (%server-request-time server) (setf (server-request-time server) (get-universal-time))))

(defmethod (setf server-request-time) (new-value (server basic-server-mixin))
  (with-slots (request-time) server
    (setf request-time new-value)))

(defmethod server-request-time ((server null))
  (get-universal-time))

(define-generic server-start-time (server)
  (:documentation "Returns the time in internal time units when SERVER's process started computing HTTP service."))

(declaim (inline %server-elapsed-time))

(defun %server-elapsed-time (server)
  (- (get-internal-real-time) (server-start-time server)))

(define-generic elapsed-time (server-or-log)
  (:documentation "Returns the elapsed time in internal time units that SERVER-OR-LOG has been computing."))

(defmethod elapsed-time ((server basic-server-mixin))
  (%server-elapsed-time server))

(define-generic elapsed-seconds (server-client-or-log)
  (:documentation "Returns the elapsed seconds that SERVER-CLIENT-OR-LOG has been computing."))

(defmethod elapsed-seconds ((server basic-server-mixin))
  (truncate (%server-elapsed-time server) internal-time-units-per-second))

(define-generic elapsed-milliseconds (server-client-or-log)
  (:documentation "Returns the elapsed milliseconds that SERVER-CLIENT-OR-LOG has been computing."))

(defmethod elapsed-milliseconds ((server basic-server-mixin))
  (* (%server-elapsed-time server) #.(float (/ 1000. internal-time-units-per-second))))

(define-generic server-life-time-expired-p (server)
  (:documentation "Returns non-null when SERVER has exceeded its maximum compute time."))

(defmethod server-life-time-expired-p ((server basic-server-mixin))
  (let ((life-time (server-life-time server)))
    (and life-time
         (< life-time (truncate (cpu-time server) 1000.)))))

(define-generic record-response-times (server url)
  (:documentation "Increments the total requests, elapsed time, and CPU time for URL."))

(defmethod record-response-times ((server basic-server-mixin) (url url:http-url))
  (let ((cpu-time (cpu-time server))
        (elapsed-time (elapsed-time server)))
    (atomic-incf (slot-value url 'cpu-time) cpu-time)
    (atomic-incf (slot-value url 'elapsed-time) elapsed-time)
    (atomic-incf (slot-value url 'n-requests))))

(define-generic server-local-host-domain-name (server)
  (:documentation "Returns the domain name for the local host."))

(defmethod server-local-host-domain-name (server)
  (declare (ignore server))
  *local-host-domain-name*)

(define-generic server-address (server)
  (:documentation "Returns the TCP address of the client being served as a fixnum."))

;; keep algorithms hashing this address happy outside HTTP connections.
(defmethod server-address ((server null)) *local-host-address*)

(define-generic server-user-agent (server)
  (declare (values user-agent version))
  (:documentation "Returns a list of keywords denoting the current user agent and version
(USER-AGENT VERSION PLATFORM-SPECS)."))

(defmethod server-user-agent ((server basic-server-mixin))
  (with-slots (headers) server
    (with-value-cached (server :current-user-agent)
      (multiple-value-list
        (let ((agent (get-header :user-agent (or headers *headers*))))
          (when agent
            ;; when there is more than one user agent header, this becomes a
            ;; list.  The last header is the most recent user-agent in the chain
            ;; but the firt is the final consumer.  7/24/95 -- JCMa.
            (when (consp agent)
              (setq agent (car agent)))
            (parse-user-agent agent)))))))

(define-generic server-full-url-string (server)
  (:documentation "Returns the fully qualified URL name string.
Obtains the namestring from the URL."))

(defmethod server-full-url-string ((server basic-server-mixin))
  (with-slots (url) server
    (url:name-string url)))

(define-generic server-relative-url-string (server)
  (:documentation "Returns the relative URL name string,
which is every except the scheme, host, and port."))

;; Sometimes the server URL is not interned and set untl verya late, as int he
;; case of PUT for a new document, and yet a digest authentication may require
;; the relative  URL beforehand. 10/1/2003 -- JCMa.
(defmethod server-relative-url-string ((server basic-server-mixin))
  (url:relative-name-string (or (server-url server)
				(server-url-string server))))

(defmethod url:name-string ((server server) &optional compute-p)
  (url:name-string (server-url server) compute-p))

(define-generic server-search-suffix (server)
  (:documentation "Returns the search suffix of a search URL, or NIL."))

(defmethod server-search-suffix ((server basic-server-mixin))
  (with-slots (url) server
    (url:search-suffix url)))

(define-generic server-translated-pathname (server)
  (:documentation "Returns the translated pathname if URL accessed by the server denotes a static resource
stored in a file, otherwise NIL."))

(defmethod server-translated-pathname ((server basic-server-mixin))
  (with-slots (url) server
    (url:translated-pathname url)))

(define-generic server-form-alist (server)
  (:documentation "Returns an alist of the form values posted to a URL
for the case of HTTP :POST method. The form alist is available after
the response function has been excuted."))

(defmethod server-form-alist ((server server-logging-mixin))
  (let ((data (server-form-data server)))
    (typecase data
      (list data)
      (t (error "Don't know how to coerce ~D into an alist." data)))))

(defun get-http-methods (&optional (http-version :http/1.1) (server-class *server-class*))
  (loop for method in (generic-function-methods #'invoke-server-method)
        for (class method-spec version-spec) = (method-specializers method)
        for http-method = (and (consp method-spec)
                               (subtypep server-class class)
                               (typecase version-spec
                                 (cons 
                                   (and (eq http-version (second version-spec)) (second method-spec)))
                                 (t (second method-spec))))
        when (and http-method
                  (not (member http-method http-methods)))                
          collect http-method into http-methods
        finally (return http-methods)))

(define-generic http-methods (server-or-url http-version)
  (declare (values method-keywords))
  (:documentation "Returns a series of keywords corresponding to the HTTP methods supported
by SERVER-OR-URL for HTTP version, HTTP-VERSION."))

(defmethod http-methods ((server basic-server-mixin) http-version)
  (get-http-methods http-version (type-of server)))

(defmethod http-methods ((url url:http-url) (http-version (eql :http/1.0)))
  '(:get :head :delete))

(defmethod http-methods ((url url:http-form) (http-version (eql :http/1.0)))
  '(:get :post :head :delete))

(defmethod http-methods ((url url:http-url) http-version)
  (declare (ignore http-version))
  '(:get :head :delete :options))

(defmethod http-methods ((url url:http-object) http-version)
  (declare (ignore http-version))
  '(:get :head :put :delete :options))

(defmethod http-methods ((url url:http-form) http-version)
  (declare (ignore http-version))
  '(:get :post :head :delete :options))

(defmethod http-methods ((url url:http-searchable-object) http-version)
  (declare (ignore http-version))
  '(:get :head :delete :options))

(defmethod http-methods ((url url:http-computed-url) http-version)
  (declare (ignore http-version))
  '(:get :head :delete :options))

(defmethod http-methods ((uri url:authority) http-version)
  (declare (ignore http-version))
  '(:connect))

(define-generic active-connection-p (server)
  (:documentation "Returns non-null if the connection to the remote host remains active,
i.e., the remote host remains connected and continues to respond."))

(defmethod active-connection-p ((server basic-server-mixin))
  (with-slots (stream) server
    (www-utils:live-connection-p stream)))

(define-generic abort-connection (server)
  (:documentation "Aborts all computation associated with SERVER's the HTTP connection."))

(defmethod abort-connection ((server basic-server-mixin))
  (www-utils:abort-current-connection))

(define-generic abort-if-connection-inactive (server)
  (:documentation "If SERVER's connection has become inactive, abort HTTP service immediately.
Use the method to abort a computation if the remote host is no longer connected."))

(defmethod abort-if-connection-inactive ((server basic-server-mixin))
  (with-slots (stream) server
    (www-utils:abort-if-connection-dead stream)))

(define-generic response-writer-to-inform-user-via-email-of-success (server)
  (declare (values response-writer))
  (:documentation "Returns a function to inform a user that their request was 
processed successfully, even though the HTTP connection to the client was lost.."))

(defmethod response-writer-to-inform-user-via-email-of-success ((server basic-server-mixin))
  (let ((url-string (server-url-string server))
        (method (server-method server)))
    (flet ((write-message (stream)
             (fast-format stream "Your HTTP ~A request was successfully processed at ~I.

    URL: ~A

    This notification is being sent to you via e-mail because your HTTP
    client connection to our Web server was lost before the transaction
    completed."
                method url-string (print-date-time :stream stream))))

      #'write-message)))

;;;------------------------------------------------------------------- 
;;;
;;; SSL METHODS
;;;

(defgeneric realm-get-client-credentials (realm server)
  (:documentation "Returns the client authentication credentials the current request, 
given the authentication REALM for the requested URL and SERVER."))

(defmethod realm-get-client-credentials ((realm realm-http-authentication-mixin) (server server-authentication-mixin))
  (get-header :authorization (server-headers server)))

#+CL-HTTP-SSL-CLIENT
(defgeneric peer-certificate (stream-or-server)
  (:documentation "Returns the verified X509-CERTIFICATE object for stream-or-server
If the stream is not in SSL mode or peer certificates are not required this returns NIL."))

#+CL-HTTP-SSL-CLIENT
(defmethod peer-certificate ((server server-ssl-mixin))
  (peer-certificate (server-stream server)))

;; this could be optimized by putting the service object onto either
;; the server or the stream at launch time.

#+CL-HTTP-SSL-CLIENT
(defmethod server-service-object ((server server-ssl-mixin))
  (http::%get-service-on-port (local-port (server-stream server))))

#+CL-HTTP-SSL-CLIENT
(defmethod server-ssl-client-credentials ((server server-ssl-mixin))
  (let ((stream (server-stream server))
        peer-certificate)
    (when (and (eql :https (local-protocol stream))
               (setq peer-certificate (peer-certificate stream)))
      (list :certificate peer-certificate (server-service-object server)))))

#+CL-HTTP-SSL-CLIENT
(defmethod realm-get-client-credentials ((realm realm-certificate-authentication-mixin) (server server-ssl-mixin))
  (declare (ignore realm))
  (server-ssl-client-credentials server))


;;;------------------------------------------------------------------- 
;;;
;;; LOGGING ACCESSES
;;;

;; RFC931 logname (ftp://ds.internic.net/rfc/rfc931.txt)
;; local-port,foreign-port:response-type:operating-system:userid 

;; local-port,foreign-port:error:user-not-known
(defmethod write-rfc-931-logname ((server server-logging-mixin) &optional (log-stream *standard-output*))
  (with-slots (stream) server
    (let ((local-port *standard-http-port*)
          (foreign-port (www-utils:foreign-port stream))
          (response-type (or (server-rfc-931-response-type server) "ERROR"))
          (response (or (server-rfc-931-response server) "USER-NOT-KNOWN")))
      (write foreign-port :stream log-stream :base 10. :escape nil)
      (write-char #\, log-stream)
      (write local-port :stream log-stream :base 10. :escape nil)
      (write-char #\: log-stream)
      (write-string response-type log-stream)
      (write-char #\: log-stream)
      (write-string response log-stream)))) 

(defmethod host-log-name ((server server-logging-mixin))
  (with-slots (address host) server
    (www-utils:%host-log-name address host *log-resolve-ip-addresses*)))

(define-generic user-email-address-via-from-header (server)
  (declare (values user-name@mail-host))
  (:documentation "Returns the user's email address as name@host."))

(defmethod user-email-address-via-from-header ((server basic-server-mixin))
  (with-header-values (from) (server-headers server)
    (when from
      (email-address (car from)))))

(define-generic user-name-via-from-header (server)
  (declare (values user-name))
  (:documentation "Returns the user name as found in the user's email address in the from header."))

(defmethod user-name-via-from-header ((server basic-server-mixin))
  (with-header-values (from) (server-headers server)
    (when from
      (url::user-id (car from)))))

(define-generic user-email-componets-via-from-header (server)
  (declare (values user-name mail-host original-string))
  (:documentation "Returns the components of the user's mail address."))

(defmethod user-email-componets-via-from-header ((server basic-server-mixin))
  (with-header-values (from) (server-headers server)
    (when from
      (let ((mail-address (car from)))
	(values (url::user-id mail-address)
		(host-string mail-address)
		(url::personal-name mail-address))))))

(defmacro define-server-user-operations ((&rest recomputables) &rest operations)
  `(progn
     (eval-when (:compile-toplevel :execute :load-toplevel)
       ,.(loop for op in operations
               for primitive = (symbolize (concatenate 'string "%SERVER-" (string op)))
               when (member op recomputables)
                 nconc `((declaim (inline ,primitive))
                         (defun ,primitive (server &optional recompute-p)
                           (with-slots (user-object) server
                             (and user-object
                                  (,op user-object recompute-p)))))
               else
                 nconc `((declaim (inline ,primitive))
                         (defun ,primitive (server)
                           (with-slots (user-object) server
                             (and user-object
                                  (,op user-object)))))))
     (eval-when (:compile-toplevel :execute :load-toplevel)
       ,.(loop for op in operations
               for primitive = (symbolize (concatenate 'string "%SERVER-" (string op)))
               when (member op recomputables)
                 collect`(defmethod ,op ((server server-authentication-mixin) &optional recompute-p)
                           (,primitive server recompute-p))
               else
                 collect `(defmethod ,op ((server server-authentication-mixin))
                            (,primitive server))))))

(define-generic server-user-object (server-authentication-mixin)
  (:documentation "Returns the authenticated user object for the current HTTP transaction."))

(define-server-user-operations
  (user-qualified-name)
  user-email-address
  user-groups
  user-name
  user-personal-name
  user-qualified-name
  user-realm)

(defmethod set-file-author ((pathname pathname) (server server-authentication-mixin) &optional (error-p t))
  (let ((name (or (user-name server)
		  (user-name-via-from-header server))))
    (when name
      (set-file-author (probe-file pathname) name error-p))))

(defmethod user-email-address ((object null))
  (www-utils:user-mail-address))


;;;------------------------------------------------------------------- 
;;;
;;; WRITING LOG FILE ENTRIES
;;;

(declaim (inline %server-write-common-logfile-entry))

(defun %server-write-common-logfile-entry (server log-stream gmt-p delimiter)
  (%write-common-logfile-entry (host-log-name server)
			       (server-request server t)
			       (server-request-time server)
			       (server-status server)
			       (server-bytes-transmitted server)	;total bytes (not number of bytes in a document)
			       (%server-user-qualified-name server)
			       gmt-p log-stream delimiter))

(define-generic write-common-logfile-entry (server &optional log-stream gmt-p delimiter)
  (:documentation "Writes an entry in Common Logfile Format for SERVER on LOG-STREAM."))

(defmethod write-common-logfile-entry ((server server-logging-mixin) &optional (log-stream *standard-output*)
                                       (gmt-p *log-times-in-gmt*) (delimiter #\space))
  (with-string-for-null-stream (log-stream)
    (%server-write-common-logfile-entry server log-stream gmt-p delimiter)))

(declaim (inline %server-write-extended-common-logfile-entry))

(defun %server-write-extended-common-logfile-entry (server log-stream gmt-p delimiter)
  (with-header-values (user-agent referer) (server-headers server)
    (%write-extended-common-logfile-entry
      (host-log-name server)
      (server-request server t)
      (server-request-time server)
      (server-status server)
      (server-bytes-transmitted server)
      (%server-user-qualified-name server)
      user-agent 
      referer
      gmt-p log-stream delimiter)))

(define-generic write-extended-common-logfile-entry (server &optional log-stream gmt-p delimiter)
  (:documentation "Writes an entry in Extended Common Logfile Format for SERVER on LOG-STREAM.
appending the user-agent and the referring URL to the entry."))

(defmethod write-extended-common-logfile-entry ((server server-logging-mixin) &optional (log-stream *standard-output*)
                                                (gmt-p *log-times-in-gmt*) (delimiter #\space))
  (with-string-for-null-stream (log-stream)
    (%server-write-extended-common-logfile-entry server log-stream gmt-p delimiter)))

(declaim (inline %server-write-console-window-notification))

(defun %server-write-console-window-notification (server log-stream)
  (let* ((requests (server-requests-completed server))
	 (host-name (host-log-name server))
	 (user-name (%server-user-qualified-name server))
	 (request (server-request server t))
	 (request-time nil)
	 (status (server-status server))
	 (port *standard-http-port*)		;stream data may no longer be valid
	 (bytes (bytes-transmitted (server-stream server)))	;total bytes (not number of bytes in a document)
	 (proxy-p (server-proxy-request-p server))
	 header-set cpu-time elapsed-time user-agent-val referer-val)
    (unless proxy-p
      (when (setq header-set (server-headers server)))
      (with-header-values (user-agent referer) header-set
	(setq user-agent-val user-agent
	      referer-val referer)))
    (psetq cpu-time (cpu-time server)
	   elapsed-time (elapsed-time server))
    (%write-console-window-notification
      host-name port proxy-p requests request request-time status bytes user-name user-agent-val referer-val
      cpu-time elapsed-time log-stream)))

(define-generic write-console-window-notification (server &optional log-stream)
  (:documentation "Writes an entry in console notification format for SERVER on LOG-STREAM."))

(defmethod write-console-window-notification ((server server-logging-mixin) &optional (log-stream *standard-output*))
  (with-string-for-null-stream (log-stream)
    (%server-write-console-window-notification server log-stream)))

(defun %server-notify-console-window (server)
  (flet ((notify-the-console (stream)
	   (%server-write-console-window-notification server stream)))
    (declare (dynamic-extent #'notify-the-console))
    (careful-notify-log-window #'notify-the-console)))

(define-generic log-notify-console (server)
  (:documentation "Notifies the console with a server log entry."))

(defmethod log-notify-console ((server server-logging-mixin))
  (%server-notify-console-window server))

(define-generic log-write-entry (log agent stream)
  (:documentation "Standard synchronous method for writing an entry in a log file for a agent.
This method can be specialized on a log class to write out a different format."))

(defmethod log-write-entry ((log common-file-format-mixin) (agent server-logging-mixin) stream)
  (%server-write-common-logfile-entry agent stream (log-times-in-gmt-p log) #\space))

(defmethod log-write-entry ((log extended-common-file-format-mixin) (agent server-logging-mixin) stream)
  (%server-write-extended-common-logfile-entry agent stream (log-times-in-gmt-p log) #\tab))

(defmethod log-write-entry ((log http-post-file-format-mixin) (agent server-logging-mixin) stream)
  (%server-write-post-logfile-entry agent stream (log-times-in-gmt-p log) #\tab))

(defgeneric snapshot-log-entry (log agent)
  (declare (values log-entry))
  (:documentation "Snapshots values of agent according to the class of log and returns a log-entry object.
This method should be specialized for each logging format mixin for appropriate data capture and
WRITE-LOG-ENTRY should be specialized on the log-entry class to write the desired output."))

(defmethod snapshot-log-entry ((log common-file-format-mixin) (server server-logging-mixin))
  (let ((host-name (host-log-name server))
	(user-name (%server-user-qualified-name server))
	(request (server-request server t))	; don't lose when transaction reset
	(request-time (server-request-time server))
	(method (server-method server))
	(status (server-status server))
	(bytes-received (server-bytes-received server))	;total bytes received
	(bytes-transmitted (server-bytes-transmitted server)))	;total bytes-transmitted (not number of bytes-transmitted in a document
    (allocate-common-log-entry log host-name request request-time method status bytes-received bytes-transmitted user-name)))

(defmethod snapshot-log-entry ((log extended-common-file-format-mixin) (server server-logging-mixin))
  (let ((host-name (host-log-name server))
	(user-name (%server-user-qualified-name server))
	(request (server-request server t))	; don't lose when transaction reset
	(request-time (server-request-time server))
	(method (server-method server))
	(status (server-status server))
	(bytes-received (server-bytes-received server))	;total bytes received
	(bytes-transmitted (server-bytes-transmitted server))	;total bytes-transmitted (not number of bytes-transmitted in a document)
	(header-set (server-headers server))
	user-agent-val referer-val)
    (when header-set				;may be null when client drops connection (408)
      (with-header-values (user-agent referer) (server-headers server)
	(setq user-agent-val user-agent
	      referer-val referer)))
    (allocate-extended-common-log-entry log host-name request request-time method status bytes-received bytes-transmitted
					user-name user-agent-val referer-val)))

(defmethod snapshot-log-entry ((log http-post-file-format-mixin) (server server-logging-mixin) &aux form-alist)
  (when (and (eq :post (server-method server))
	     (setq form-alist (server-form-alist server)))
    (let ((host-name (host-log-name server))
	  (user-name (%server-user-qualified-name server))
	  (request-time (server-request-time server))
	  (request (server-request server t))
	  (status (server-status server))
	  (bytes-transmitted (server-bytes-transmitted server))
	  (bytes-received (server-bytes-received server))
	  (headers (server-headers server)))
      (with-header-values (user-agent referer) headers
	(allocate-http-post-log-entry log host-name request request-time status bytes-transmitted user-name
				      user-agent referer bytes-received form-alist)))))

(defmethod snapshot-log-entry ((log notification-log-format-mixin) (server server-logging-mixin))
  (let ((requests-completed (server-requests-completed server))
	(host-name (host-log-name server))
	(user-name (%server-user-qualified-name server))
	(request (server-request server t))
	(request-time (server-request-time server))
	(method (server-method server))
	(status (server-status server))
	(port *standard-http-port*)		;stream data may no longer be valid
	(bytes-received (server-bytes-received server))	;total bytes received
	(bytes-transmitted (bytes-transmitted (server-stream server)))	;total bytes (not number of bytes in a document)
	(proxy-p (server-proxy-request-p server))
	header-set cpu-time elapsed-time user-agent-val referer-val)
    (unless proxy-p
      (when (setq header-set (server-headers server)))
      (with-header-values (user-agent referer) header-set
	(setq user-agent-val user-agent
	      referer-val referer)))
    (psetq cpu-time (cpu-time server)
	   elapsed-time (elapsed-time server))
    (allocate-notification-log-entry log host-name port request request-time method status bytes-received bytes-transmitted user-name
				     user-agent-val referer-val requests-completed cpu-time elapsed-time proxy-p)))

(defmethod snapshot-log-notification ((log asynchronous-log-notification-mixin) (server server-logging-mixin))
  (let ((requests-completed (server-requests-completed server))
	(host-name (host-log-name server))
	(user-name (%server-user-qualified-name server))
	(request (server-request server t))
	(request-time (server-request-time server))
	(method (server-method server))
	(status (server-status server))
	(port *standard-http-port*)		;stream data may no longer be valid
	(bytes-received (server-bytes-received server))	;total bytes received
	(bytes-transmitted (bytes-transmitted (server-stream server)))	;total bytes (not number of bytes in a document)
	(proxy-p (server-proxy-request-p server))
	header-set cpu-time elapsed-time user-agent-val referer-val)
    (unless proxy-p
      (when (setq header-set (server-headers server)))
      (with-header-values (user-agent referer) header-set
	(setq user-agent-val user-agent
	      referer-val referer)))
    (psetq cpu-time (cpu-time server)
	   elapsed-time (elapsed-time server))
    (allocate-console-notification-log-entry log host-name port request request-time method status bytes-received bytes-transmitted
					     user-name user-agent-val referer-val requests-completed cpu-time elapsed-time proxy-p)))

(defmethod snapshot-log-entry ((log asynchronous-log-counters-mixin) (server server-logging-mixin) &aux (stream (server-stream server)))
  (let ((cpu-time (cpu-time server))
	(elapsed-time (elapsed-time server))
	(bytes-received (www-utils:bytes-received stream))
        (bytes-transmitted (www-utils:bytes-transmitted stream))
        (method (server-method server))
	(proxy-p (server-proxy-request-p server))
        (requests-completed (server-requests-completed server))
        (status (server-status server)))
    (allocate-counters-log-entry log method status bytes-received bytes-transmitted requests-completed proxy-p cpu-time elapsed-time)))


;;;------------------------------------------------------------------- 
;;;
;;; ACCESS LOGS
;;;

(define-generic dynamically-log-transaction (server log)
  (:documentation "Interns a transaction object in the dynamic log.
Specialize this method for new log classes."))

(defmethod dynamically-log-transaction ((server server-logging-mixin) (log http-log))
  (with-slots (host http-version method status stream url) server
    (when (and method url)
      (let ((bytes (www-utils:bytes-transmitted stream))        ;total bytes transfered
            (host-object (intern-host log :object host))
            (user (server-user-object server)))
        (log-transaction log url host-object method (server-request-time *server*) http-version
                         :status status :bytes bytes :user user)))))

(defmethod dynamically-log-transaction ((server server-logging-mixin) (log extended-http-log))
  (with-slots (host http-version method status stream url) server
    (when (and method url)
      (let ((bytes (www-utils:bytes-transmitted stream))        ;total bytes transfered
            (host-object (intern-host log :object host))
            (user (server-user-object server))
            (referrer (get-header :referer (server-headers server))))
        (multiple-value-bind (user-agent version comment)
            (current-user-agent)
          (log-transaction log url host-object method  http-version
                           :status status :bytes bytes :user user
                           :user-agent (when user-agent (list* user-agent version comment))
                           :referrer (when referrer
                                       (handler-case
                                         (let* ((url:*url-host-name-resolution* :preferred))
                                           (intern-url referrer :if-does-not-exist :create))
                                         (parsing-error () nil)))))))))

(define-generic log-server-access (log server)
  (declare (values call-next-method-p))
  (:documentation "Standard method for registering in LOG and access to SERVER.
Methods are combined with AND method combination from most specific to most general
inherited methods.  Only if more specific methods return non-null, are the subsequent
methods called.")
  (:method-combination and))

(define-generic bug-report-error-logging-access (server log condition log-operation)
  (:documentation "Reports a bug while logging HTTP access.
LOG-OPERATION should be the name of the method that got the error."))

(defmethod bug-report-error-logging-access ((server server-logging-mixin) (log basic-log-mixin) (condition condition) log-operation)
  (let ((condition-type (type-of condition))
        (log-entry (write-common-logfile-entry server nil)))
    (declare (dynamic-extent log-entry))
    (report-bug *bug-http-server*
                (format nil "HTTP Access Log Error: ~S" condition-type)
                "~&Log Type: ~S~&Log Operation ~S~&Log Entry:~S ~&Error: ~S~:[~;~&Error Report: ~:*~A~]"
                (type-of log) log-operation log-entry condition-type (report-string condition))))

(defmethod log-server-access :around ((log basic-log-mixin) (server server-logging-mixin))
  (handler-case
    (call-next-method)
    (error (err) (bug-report-error-logging-access server log err 'log-server-access))))
           
(define-generic update-log-statistics (log-counters-mixin status method proxy-p requests-completed
                                                          bytes-transmitted bytes-received elapsed-time cpu-time))

;; This could use a log thread if one is available.   3/16/97 -- JCMa.
(defmethod update-log-statistics ((log log-counters-mixin) status method proxy-p requests-completed
                                  transmitted-bytes received-bytes time-elapsed time-cpu)
  (with-slots (bytes-transmitted bytes-received elapsed-time cpu-time) log
    (%note-access-status-code log status proxy-p)
    (%note-http-method log method)
    (atomic-incf bytes-transmitted transmitted-bytes)
    (atomic-incf bytes-received received-bytes)
    (atomic-incf elapsed-time time-elapsed)
    (atomic-incf cpu-time time-cpu)
    (%note-http-connections log requests-completed)))

(defmethod log-server-access and ((log synchronous-log-counters-mixin) (server server-logging-mixin)
                                  &aux (stream (server-stream server)))
  (let ((cpu-time (cpu-time server))
        (elapsed-time (elapsed-time server))
        (status (server-status server))
        (method (server-method server))
	(proxy-p (server-proxy-request-p server))
        (requests-completed (server-requests-completed server))
        (bytes-transmitted (www-utils:bytes-transmitted stream))
        (bytes-received (www-utils:bytes-received stream)))
    (update-log-statistics log status method proxy-p requests-completed
                           bytes-transmitted bytes-received elapsed-time cpu-time))
  t)

(defmethod write-log-entry ((entry-data counters-log-entry) stream)
  (declare (ignore stream))
  (with-slots (owner method status bytes-received bytes-transmitted requests-completed proxy-p cpu-time elapsed-time) entry-data
    (update-log-statistics owner status method proxy-p requests-completed bytes-transmitted bytes-received elapsed-time cpu-time)))

(defmethod log-server-access and ((log asynchronous-log-counters-mixin) (agent server-logging-mixin))
  (let ((log-entry (snapshot-log-entry log agent)))
    (when log-entry
      (tq:push-task-queue log log-entry))
    t))

(defmethod log-server-access and ((log asynchronous-log-notification-mixin) (agent server-logging-mixin))
  (when (log-notification log)
    (let ((log-entry (snapshot-log-notification log agent)))
      (when log-entry
	(tq:push-task-queue log log-entry))))
  t)

(defmethod log-server-access and ((log basic-url-metering) (server server-logging-mixin))
  (let ((url (server-url server)))
    (when url
      (record-response-times server url)))
  t)

(define-generic write-log-entry-to-file (log agent)
  (:documentation "Writes a log entry to a disk logfile."))

(defmethod write-access-log-entry-to-file ((log file-logging-mixin) (agent server-logging-mixin))
  ;; This form is executed atomically with interprocess locking.
  ;; When log-file-stream-stays-open is turned on, entries are written to 
  ;; disk only after the stream buffer is filled.
  ;; While this reduces disk access, it can lose up to a buffer's worth of logs
  ;; in the event of a machine crash. FORCE-OUTPUT here would handle this
  (with-log-stream (log)
    (log-write-entry log agent log-stream)
    ;; Trailing CR makes this consistently parsable.
    (terpri log-stream)))

(defmethod write-log-entry-to-file ((log basic-process-queued-file-logging-mixin) (agent logging-mixin))
  (let ((log-entry (snapshot-log-entry log agent)))
    (when log-entry
      (tq:push-task-queue log log-entry)))) 

(defmethod log-server-access and ((log file-logging-mixin) (server server-logging-mixin))
  (when (log-entry-p log server)
    (write-log-entry-to-file log server))
  t)

(defmethod log-server-access and ((log dynamic-loggin-mixin) (server server-logging-mixin))
  (when (log-entry-p log server)
    (dynamically-log-transaction server log))
  t)

(defmethod log-server-access and ((log synchronous-log-notification-mixin) (server server-logging-mixin))
  (when (log-notification log)
    (log-notify-console server))
  t)

(defmethod log-server-access and ((log custom-notification-log) (server server-logging-mixin))
  (with-slots (notification predicate notifier) log
    (and notification
         (funcall predicate log server)
         (funcall notifier log server))
    t))

(define-generic server-access-logs (server)
  (declare (values current-port-logs multiport-logs))
  (:documentation "Returns the access log objects for SERVER."))

(defmethod server-access-logs ((server server-logging-mixin))
  (values (standard-access-logs *standard-http-port*)
	  (multiport-access-logs)))

(defgeneric log-access (server)
  (:documentation "Logs an HTTP server access according to the request port."))

(defmethod log-access ((server server-logging-mixin)) 
  (dolist (log (standard-access-logs *standard-http-port*))
    (log-server-access log server))
  (dolist (log (multiport-access-logs))
    (log-server-access log server)))

;;;------------------------------------------------------------------- 
;;;
;;; REGISTRY OF DIRECTORY EXPORT TYPES
;;; 

(define-variable *directory-export-types* nil
                 "All directory export types that export a single directory level.")

(define-variable *hierarchical-directory-export-types* nil
                 "All directory export types that export entire directory hierarchies.")

(define-variable *directory-export-type-mime-major-type-alist* nil
                 "An alist of (directory-export-type . mime-major-types).")

(defun %register-directory-export-type-mime-major-type (export-type major-types)
  (check-type export-type keyword)
  (check-type major-types cons)
  (let ((entry (assoc export-type *directory-export-type-mime-major-type-alist* :test #'eq)))
    (cond (entry
           (dolist (item major-types)
             (pullnew item entry :test #'eq)))
          (t (push `(,export-type ,@major-types) *directory-export-type-mime-major-type-alist*)))))

(defun %unregister-directory-export-type-mime-major-type (export-type)
  (let ((entry (assoc export-type *directory-export-type-mime-major-type-alist* :test #'eq)))
    (when entry
      (setq *directory-export-type-mime-major-type-alist* (delete entry *directory-export-type-mime-major-type-alist*)))))

(define directory-type-exports-mime-major-types (directory-type)
  "Returns the major mime types exported by directory."
  (or (cdr (assoc directory-type *directory-export-type-mime-major-type-alist* :test #'eq))
      (error "~S is not a known directory export type." directory-type)))

(defun %register-directory-export-type (export-type &optional hierarchical-p)
  (cond (hierarchical-p
         (pullnew export-type *hierarchical-directory-export-types*))
        (t (pullnew export-type *directory-export-types*))))

(defun %unregister-directory-export-type (export-type &optional hierarchical-p)
  (cond
    (hierarchical-p
     (setq *hierarchical-directory-export-types* (delete export-type *hierarchical-directory-export-types*)))
    (t (setq *directory-export-types* (delete export-type *directory-export-types*)))))

(declaim (inline directory-export-type-p))

(define directory-export-type-p (export-type)
  "Returns non-null if EXPORT-TYPE is a single-level directory export type."
  (member export-type *directory-export-types* :test #'eq))

(declaim (inline hierarchical-directory-export-type-p))

(define hierarchical-directory-export-type-p (export-type)
  "Returns non-null if EXPORT-TYPE is a hierarchical directory export type."
  (member export-type *hierarchical-directory-export-types* :test #'eq))

;;;------------------------------------------------------------------- 
;;;
;;; PROXY METHODS
;;;

(define-generic invoke-proxy-service (server url method http-version)
  (declare (optimize (speed 3)))
  (:documentation "Top-level entry point for proxy service."))

;; Default method when proxy is not loaded
(defmethod invoke-proxy-service ((server proxy-server-mixin) url method http-version)
  (declare (ignore http-version))
  (error 'method-not-allowed :method method :url url))

(defgeneric invoke-reverse-proxy-service (server url method)
  (declare (optimize (speed 3)))
  (:documentation "Top-level method for invoking reverse proxy service."))

;; Default method when proxy is not loaded
(defmethod invoke-reverse-proxy-service (server url method)
  (declare (ignore server))
  (error 'method-not-allowed :method method :url url))

;;;------------------------------------------------------------------- 
;;;
;;; NETWORK SERVER INTERFACE
;;;

(define-generic reset-transaction-state (server)
  (:documentation "Resets the server state associated with the current HTTP transaction.
It is used between transactions on persistent connections."))

(defmethod reset-transaction-state ((server basic-server-mixin))
  (let ((stream (server-stream server))
        (process (server-process server)))
    ;; reset resourced items
    (setf (fill-pointer (%server-request server)) 0
	  (fill-pointer (server-url-buffer server)) 0)
    (clear-header-set (server-headers server) nil)
    ;; reset instance variables
    (setf (server-request-copy server) nil
	  (server-method server) nil
          (server-url-string server) nil
          (server-url server) nil
          (property-list server) nil
          (server-status server) 200
	  (server-form-data server) nil
	  (server-close-connection-p server) nil
	  (server-persistent-connection-p server) nil
          (www-utils:bytes-transmitted stream) 0
          (www-utils:bytes-received stream) 0
          (server-request-time server) nil
          (server-timeout server) *server-timeout*
          (server-life-time server) *server-life-time*
          (server-start-time server) (get-internal-real-time)
          (server-process-start-time server) (www-utils:process-run-time process)
	  (server-proxy-request-p server) nil
	  (server-proxy-client-data server) nil)))

(define-generic persistent-connection-p (server &optional inside-transaction-p)
  (:documentation "Returns non-null if the client is asking for a persistent connection
and the server is willing to oblige."))

(defmethod persistent-connection-p ((server null) &optional inside-transaction-p)
  (declare (ignore inside-transaction-p))
  nil)

(defmethod persistent-connection-p ((server basic-server-mixin) &optional inside-transaction-p)
  (with-slots (requests-completed) server
    (cond ((server-close-connection-p server) nil)
          (t (case (server-http-version server)
	       (:http/0.9 nil)
	       (:http/1.0
		 (let* ((headers (server-headers server))
			(connection (if (server-proxy-request-p server)
					;; :proxy-connection deprecated 1.0 extension supported by Netscape 3,4 & IE 4, 5
					(get-header :proxy-connection headers)
					(get-header :connection headers))))
		   (and connection 
			(member :keep-alive connection)
			(> *persistent-connection-maximum-requests*
			   (if inside-transaction-p (1+ (the fixnum requests-completed)) requests-completed)))))
	       (t (let ((connection (get-header :connection (server-headers server))))
		    (if (member :close connection)
                        nil
                        (> *persistent-connection-maximum-requests*
                           (if inside-transaction-p (1+ (the fixnum requests-completed)) requests-completed))))))))))

(declaim (inline %server-persistent-connection-parameters))

(defun %server-persistent-connection-parameters (server &optional inside-transaction-p)
  (declare (values connection-timout remaining-requests))
  (with-slots (requests-completed close-connection-p) server
    (values *persistent-connection-timeout*
            (if close-connection-p
                0
                (- (the fixnum *persistent-connection-maximum-requests*)
                   (if inside-transaction-p
                       (1+ (the fixnum requests-completed))
                       (the fixnum requests-completed)))))))

(define-generic server-persistent-connection-parameters (server &optional inside-transaction-p)
  (declare (values connection-timout remaining-requests))
  (:documentation "Returns parameters governing the longevity of the current persistent connection.
CONNECTION-TIMOUT is in seconds. REMAINING-REQUESTS is the number of additional requests available
on the connection. Specialize this method to change the behavior of persistent connections."))

(defmethod server-persistent-connection-parameters ((server basic-server-mixin) &optional inside-transaction-p)
  (%server-persistent-connection-parameters server inside-transaction-p))

(define-generic write-server-persistent-connection-headers (server stream content-length)
  (declare (values header-plist))
  (:documentation "Writes persistent connection headers to stream.
These handle negotiations concerning persistent connections with clients."))

(defmethod write-server-persistent-connection-headers ((server basic-server-mixin) stream content-length)
  (case (server-http-version server)
    (:http/1.0
      (cond ((and (case (server-method server)
		    (:get (case (server-status server)
			    ((204 304) t)
			    (t content-length)))
		    (:head t)
		    (t nil))
		  (persistent-connection-p server t))
	     (setf (server-persistent-connection-p server) t)
	     (write-header :connection '(:keep-alive) stream)
	     (multiple-value-bind (timeout max-requests)
		 (%server-persistent-connection-parameters server t)
	       (let ((value `(:timeout ,timeout :max ,max-requests)))
		 (declare (dynamic-extent value))
		 (write-header :keep-alive value stream))
	       t))
	    (t (setf (server-close-connection-p server) t)
	       nil)))
    (:http/0.9 nil)
    ;; Default in HTTP 1.1 is to always use persistent connections.
    (t (cond ((persistent-connection-p server t)
	      (setf (server-persistent-connection-p server) t)
	      nil)
	     (t (setf (server-close-connection-p server) t)
		(write-header :connection '(:close) stream)
		t)))))

(defmethod write-server-persistent-connection-headers ((server null) stream content-length)
  (declare (ignore stream content-length))
  nil)

(defun copy-request-url-string (string1 start1 end1 string2 start2 end2 &optional length2)
  "Copies the contents of STRING1 to STRING2 according to the indices.
When STRING2 is grown and results in a new string the old STRING2 is returned as a second value."
  (declare (fixnum start1 end1 start2 end2)
           (optimize (speed 3)))
  (unless length2
    (setq length2 (the fixnum (- end2 start2))))
  ;; Ensure URL fits in buffer
  (when (> length2 (the fixnum (array-total-size string2)))
    (let ((old-string2 string2))
      (setq string2 (adjust-array string2 length2 :fill-pointer 0 :element-type *standard-character-type*))
      (unless (eq string2 old-string2)
	(deallocate-resource 'line-buffer old-string2))))
  ;; Copy the url into the URL buffer
  (copy-vector-portion string1 start1 end1 string2 start2 end2)
  (setf (fill-pointer string2) end2)
  string2)

(define parse-request (string start end url-buffer)
  "Parses an HTTP request string."
  (declare (values method url-string http-version-keyword)
           (fixnum end)
           (optimize (speed 3)))
  (let* ((e3 (if (and (not (zerop end)) (white-space-char-p (aref string (1- end))))
                 (%fast-position-if-not white-space-char-p string :start start :end (1- end) :from-end t)
               end))
         (s1 (and (not (zerop e3)) (char-position #\space string start e3)))
         (s2 (and s1 (%fast-position-if-not white-space-char-p string :start s1 :end e3)))
         (e2 (and s2 (or (%fast-position-if white-space-char-p string :start s2 :end e3) e3)))
         (s3 (and e2 (1+ (the fixnum (or (%fast-position-if white-space-char-p string :start e2 :end e3 :from-end t) e2)))))
         (method (and s1 (%tokenize-header-keyword string 0 s1)))
         (version (and s3 (< s3 e3) (%tokenize-header-keyword string s3 e3)))
	 (url-length (and s2 e2 (- (the fixnum e2) (the fixnum s2))))
	 url-string)
    (when url-length
      ;; Ensure URL fits in buffer
      (when (> (the fixnum url-length) (the fixnum (array-total-size url-buffer)))
	(setq url-buffer (adjust-array url-buffer url-length :fill-pointer 0 :element-type *standard-character-type*)))
      ;; Copy the url into the URL buffer
      (copy-request-url-string string s2 e2 url-buffer 0 url-length url-length)
      (setq url-string url-buffer))
    (values method url-string version)))

;; for http 1.1 compliance this needs to determine if the host is a valid host
;; on the server.  6/26/96 -- JCMa.
(define request-local-context (url http-version headers)
  (declare (values request-context bind-local-context-p)
           (optimize (speed 3)))
  (let* ((host-header (get-header-object :host headers))
         (host-spec (and host-header (header-value host-header)))
         (local-context (local-context))
         l1 l2)
    (cond ;; HTTP 1.1 requests
          (host-spec
           (destructuring-bind (host port)
               host-spec
             (declare (fixnum port))
             (cond ;; Check if corresponds to standard host and port.
                   ((and (= port *standard-http-port*)
                         (ecase (local-scheme)
                           (:http
                            (and (>= (the fixnum (length local-context)) (setq l2 (+ 7 (the fixnum (setq l1 (length host))))))
                                 (%string-equal host local-context 0 l1 7 l2)))
                           (:https
                            (and (>= (the fixnum (length local-context)) (setq l2 (+ 8 (the fixnum (setq l1 (length host))))))
                                 (%string-equal host local-context 0 l1 8 l2)))))
                    (values local-context nil))
                   ;; handle virtual hosts
                   (t (values (or (virtual-host-local-context host port *standard-protocol*)
                                  (error 'unknown-virtual-host
                                         :format-string "The virtual host ~A on port ~D is unknown."
                                         :format-args (list host port)))
                              t)))))
          ;; handle legacy clients
          ((member http-version '(:http/1.0 :http/0.9))
           local-context)
          (t (error 'request-missing-host-header :format-string "HTTP 1.1, or later client, failed to provide a host header." :url url))))) 

(declaim (inline %proxy-service-on-port))

(defun %proxy-service-on-port (port)
  (member port *proxy-service*))

(define proxy-service-on-port (port)
  "Returns non-null if proxy service is available on PORT."
  (%proxy-service-on-port port))

(define proxy-service-enabled-p (&optional port)
  "Returns non-null if proxy service is enabled on port."
  (not (null (if port (%proxy-service-on-port port) *proxy-service*))))

(defun %execute-request (server request stream)
  (declare (values persistent-connection-p)
	   (optimize (speed 3)))
  (flet ((invoke-local-service (server url-string method http-version)
	   (multiple-value-bind (request-context bind-context-p)
	       (request-local-context url-string http-version *headers*)
             ;; unescape URL for local service and to avoid multiple escaping for search info in search URLs -- JCMa 6/10/2010
	     (setf (server-url-string server) (%merge-url (string-unescape-special-chars url-string) request-context t))
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
          ;; tracing form (fast-format x "~&~S~&~I~&~%" request (write-header-buffer *headers*  x))
	  (cond ((eql method :connect) ;set up a binary tunnel -- used for SSL proxy
                 (setf (server-proxy-request-p server) t)
                 (invoke-proxy-service server url-string method http-version))
                ;; scheme prefixed URL Proxy path
                ((url:scheme-prefixed-url-p url-string)
                 (let ((uri (proxy-intern-url url-string)))  
                   (cond  ;; Actually a local reference, start over as 
                          ((and (url:local-url-p uri) (server-native-url-scheme-p uri))
                           (let* ((local-port (host-port uri))
                                  (local-protocol (local-protocol stream))
                                  (standard-context-p (and (eql local-port *standard-http-port*)
                                                           (eql local-protocol *standard-protocol*))))
                             (cond ((or standard-context-p (http:http-service-enabled-p local-port))
                                    (unwind-protect
                                        (with-local-port-context-if ((not standard-context-p) local-port local-protocol)
                                          (invoke-local-service server (nrelativize-url url-string) method http-version)
                                          ;; log the local HTTP service now, and normally log proxy access
                                          ;; But don't handle signals here for now 12/19/2000 -- JCMa.
                                          (incf (server-requests-completed server))	;increment completed requests
                                          (log-access server))	;request is not turned into a relative URI
                                      (setf (server-proxy-request-p server) t)))
                                   (t (setf (server-proxy-request-p server) t)
                                      (error 'service-unavailable :method method :url uri
                                             :format-string "~A Service is currently unavailable on port ~D" 
                                             :format-args (list local-protocol local-port))))))
                          ((%proxy-service-on-port (server-host-local-port server))
                           (setf (server-url server) uri
                                 (server-proxy-request-p server) t)
                           (invoke-proxy-service server uri method http-version))
                          (t (error 'access-forbidden 
                                    :format-string "HTTP Proxy service is currently unavailable on ~A (~D)." 
                                    :format-args (list (local-host-domain-name) (server-host-local-port server))
                                    :method method :url uri)))))
                
                ;; Standard path, call the primary server method.
                (t (invoke-local-service server url-string method http-version))))))))

(defun %process-request (server stream)
  (declare (values persistent-connection-p)
	   (optimize (speed 3)))
  (labels ((preview-condition (condition)
	     (when *debug-server*
               (break (report-string condition)))
	     nil)
	   (handle-http-condition (condition stream server status-code)
             (case *debug-server*
               (:conditions (break (report-string condition))))
	     (setf (server-status server) status-code)
	     (when (close-connection-p condition) 
	       (setf (server-close-connection-p server) t))
	     (typecase condition
	       ((or network-error request-timeout))
	       (t (handler-case
		    (report-status condition stream)
		    (network-error () nil)
		    (error (err) (bug-report-error err) nil))))
	     (throw 'exit-http-server nil))	;closes connection
	   (handle-reportable-condition (condition)
	     (handle-http-condition condition stream server (status-code condition)))
	   (handle-parsing-error (error)
	     (handle-http-condition error stream server 400))
	   (report-error (error)
	     (typecase error
	       ((or http-condition network-error condition))
	       (t (bug-report-error error)))
	     nil))
    (declare (dynamic-extent #'handle-reportable-condition #'handle-parsing-error))

    (handler-bind
      ((error #'preview-condition)		; MCL 4.1 loses when testing CONDITION here. -- JCMa 7/24/1997.
       (reportable-condition #'handle-reportable-condition)
       (url:parsing-error #'handle-parsing-error)
       (error #'report-error))
      (catch 'exit-http-server
	(multiple-value-bind (request eof delimiter)
	    (read-delimited-line stream '(#\Linefeed #\Return) t (%server-request server))
	  delimiter				;ignore
	  ;; http-version not set, but will default via server-http-version
	  (when eof
	    (error 'request-timeout :format-string "Client dropped connection while reading request line."))
	  (setf (%server-request server) request	;capture in case of growth
		(server-request-time server) (get-universal-time))	;set the request time
	  (%execute-request server request stream))))
    ;; Epilogue
    (prog1 (when (live-connection-p stream)	; Dead connection returns NIL 12/11/2003 -- JCMa.
	     (let ((persistent-connection-p (and (server-persistent-connection-p server)	;set by positive predicate
						 (not (server-close-connection-p server)))))	;errors may set this
	       ;; Don't force output if there is incoming data: pipeline responses 
	       (unless (and persistent-connection-p (http-input-data-available-p stream nil))
		 (force-output stream))		;force output while deciding what to do next
	       persistent-connection-p))	;return whether to continue reading requests from the stream
	   ;; Count completed requests
	   (incf (server-requests-completed server)))))

(define-generic provide-service (server)
  (:documentation "Top-level method for providing HTTP service on STREAM."))

(defmethod provide-service ((server basic-server-mixin))
  (declare (optimize (speed 3)))
  (with-connection-noted                        ;first increment connection counter
    (with-standard-server-io-syntax ()
      (let ((stream (server-stream server)))
	(with-local-port-context ((local-port stream) (local-protocol stream))
	  (loop initially (clear-white-space stream)	;tolerate clients with leading CR-LF or whitespace.
		while (%process-request server stream)
		do (log-access server)		;run server-logging
                (unless (http-input-data-available-p stream *persistent-connection-timeout*)
                  ;; Close with abort because we have timed out. There doesn't seem to
                  ;; be a reliable close here. At least, the client has all the data.   12/1/95 -- JCMa.
                  (close stream :abort (not (live-connection-p stream)))
                  (return-from provide-service))
                ;; Reset the server instance to avoid confusion across
                ;; transactions in a persistent connection
                (reset-transaction-state server)
                ;; Primary close frees the client ASAP without using abort.
		finally (close stream :abort (not (live-connection-p stream)))
                (log-access server)))))))


;;;------------------------------------------------------------------- 
;;;
;;; SERVER METHODS
;;; 

(define locate-controlling-url (url-string method search-url-p)
  "Attempts to locate url-string as an inferior of the most specific superior URL in the path.
If an interned superior is found within one directory level and it is exported, this calls
URL:INTERN-URL-INFERIOR on the parent to obtain the interned inferior or NIL."
  (when (and (not search-url-p) *url-areas*)
    (multiple-value-bind (superior superior-export-type)
        (most-specific-exported-parent-url url-string -1)
      (when (and superior superior-export-type)
        (url:intern-url-inferior superior superior-export-type method url-string)))))

(declaim (inline server-update-url-specific-timeouts))

(defun server-update-url-specific-timeouts (server url)
  "Updates the timeouts for SERVER based on the specific values for URL."
  (cond-every
    ((slot-boundp url 'url::life-time)
     (setf (server-life-time server) (server-life-time url)))
    ((slot-boundp url 'url::timeout)
     (setf (server-timeout server) (server-timeout url)))))

(define-generic invoke-server-method (server method http-version)
  (declare (optimize (speed 3)))
  (:documentation "Top-level entry point for HTTP service.")) 

(defmethod invoke-server-method ((server basic-server-mixin) method http-version)
  http-version                                  ;ignore
  (with-slots (url-string) server
    (error 'unsupported-method :method method :url url-string)))

;; this is turned off by default because Netscape 1.1N is not
;; reading the returned URI header to update its location and
;; merge URLs correctly. 
(define-parameter *fast-redirection* nil
                  "Controls whether local redirects are optimized.")

(declaim (inline local-redirect-p))

(defun local-redirect-p (url)
  (and *fast-redirection* (url:local-url-p url)))

(defmethod invoke-server-method ((server basic-server-mixin) (method (eql :head)) (http-version symbol))
  (macrolet ((handle-redirect (condition tag)
               `(destructuring-bind (target-url &rest other-urls) (new-urls ,condition)
                  (cond ;; optimize redirect by reinvoking for singleton local url.
                    ((and (null other-urls) (local-redirect-p target-url))
                     (setf (server-url-string server) (url:name-string target-url))
                     (go ,tag))
                    (t (report-status ,condition stream))))))
    (with-slots (stream url-string) server
      (prog ((search-url-p (url:valid-search-url-p url-string))
             translation-method)
         retry1
            (handler-case
              ;; We use the search parent for the head method for efficiency
              ;; and in lieu of a regime for providing response functions that
              ;; compute the return of the head method for search inferiors.   8/14/96 -- JCMa.
              (multiple-value-bind (url)
                  (url:intern-url url-string :if-does-not-exist (if search-url-p *search-url-intern-mode* :soft))
                (tagbody
                  retry2
                     (cond
                       (url
			(cond ((setq translation-method (or (translation-method url)
							    (and *auto-export* (auto-export-pathname-url url-string) (translation-method url))))
			       (setf (server-url server) url) 
			       (with-access-control
				 (url method server (or (url:secure-subnets url) *secure-subnets*)
				      :deny-subnets *disallowed-subnets*)
				 (write-document-headers url translation-method stream)))
			      (t (error 'document-not-found :url url :method :head))))
                       ((and (not search-url-p) *auto-export* (auto-export-pathname-url url-string))
                        (go retry1))
                       ((setq url (locate-controlling-url url-string method search-url-p))
                        (go retry2))
                       (t (error 'document-not-found :url url-string :method :head)))))
              (redirection (cond) (handle-redirect cond retry1)))))))

(defmethod invoke-server-method ((server basic-server-mixin) (method (eql :get)) (http-version symbol))
  (macrolet ((handle-redirect (condition tag)
               `(destructuring-bind (target-url &rest other-urls) (new-urls ,condition)
                  (cond ;; optimize redirect by reinvoking for singleton local url.
                    ((and (null other-urls) (local-redirect-p target-url))
                     (setf (server-url-string server) (url:name-string target-url))
                     (go ,tag))
                    (t (report-status ,condition stream))))))
    (with-slots (stream url-string) server
      (prog ((search-url-p (url:valid-search-url-p url-string))
             translation-method)
         retry1
            (handler-case
              (multiple-value-bind (url)
                  (url:intern-url url-string :if-does-not-exist (if search-url-p *search-url-intern-mode* :soft))
                (tagbody
                  retry2
                     (cond
                       (url
                        (cond ((setq translation-method (or (translation-method url)
							    (and *auto-export* (auto-export-pathname-url url-string) (translation-method url))))
			       (setf (server-url server) url) 
			       (with-access-control (url method server (or (url:secure-subnets url) *secure-subnets*)
							 :deny-subnets *disallowed-subnets*)
				 (server-update-url-specific-timeouts server url)
				 (write-document url translation-method stream)))
			      (t (error 'document-not-found :url url :method :get))))
                       ((and (not search-url-p) *auto-export* (auto-export-pathname-url url-string))
                        (go retry1))
                       ((setq url (locate-controlling-url url-string method search-url-p))
                        (go retry2))
                       (t (error 'document-not-found :url (or url url-string) :method :get)))))
              (redirection (cond) (handle-redirect cond retry1)))))))

(defgeneric write-document-post (server url http-version)
  (:documentation "Writes the response to an HTTP POST."))

(defmethod write-document-post ((server basic-server-mixin) (url http-url) http-version)
  (with-slots (stream) server
    (case http-version
      ((:http/0.9 :http/1.0))
      ;; alert HTTP 1.1 or greater clients that we are ready
      (t (report-status-continue stream)
	 (send-cr-line-feed stream)
	 (force-output stream)
	 (setf (server-status server) 100.)))
    ;; process the POST
    (with-header-values (content-type transfer-encoding) *headers*
      (case transfer-encoding
	((nil :chunked))
	(t (error 'server-not-implemented :close-connection t :url url :method :post
		  :format-string "The HTTP transfer encoding, ~A, is not implemented."
		  :format-args (list transfer-encoding))))
      ;; Do the work
      (destructuring-bind (&optional doc-type doc-subtype &rest args) content-type
	(declare (ignore args))
	(post-document url doc-type doc-subtype stream)))))

(defmethod invoke-server-method ((server basic-server-mixin) (method (eql :post)) (http-version symbol) &aux translation-method)
  (macrolet ((handle-redirect (condition tag)
	       `(destructuring-bind (target-url &rest other-urls) (new-urls ,condition)
		  (cond ;; optimize redirect by reinvoking for singleton local url.
		    ((and (null other-urls) (local-redirect-p target-url))
		     (setf (server-url-string server) (url:name-string target-url))
		     (go ,tag))
		    (t (report-status ,condition stream))))))
    (with-slots (stream url-string) server
      (tagbody
	retry1
	   (handler-case 
	     (let ((url (url:intern-url url-string :if-does-not-exist :soft)))
	       (tagbody
		 retry2
		    (cond ((and url (setq translation-method (translation-method url)))
			   (setf (server-url server) url)
			   (with-access-control (url method server (or (url:secure-subnets url) *secure-subnets*)
						     :deny-subnets *disallowed-subnets*)
			     (case translation-method
			       (:temporary-redirect
				 (standard-url-redirect url :temporary-redirect :post))
			       (:redirect
				 (standard-url-redirect url :permanent-redirect :post))
			       (t (server-update-url-specific-timeouts server url)
				  (write-document-post server url http-version)))))
			  ((setq url (locate-controlling-url url-string method (url:valid-search-url-p url-string)))
			   (go retry2))
			  (t (error 'document-not-found :url url-string :method :post :close-connection t)))))
	     (redirection (cond) (handle-redirect cond retry1)))))))

(defmethod invoke-server-method ((server basic-server-mixin) (method (eql :delete)) (http-version symbol))
  (macrolet ((with-delete-response ((server stream) &body body)
               `(multiple-value-bind (url status)
                    (progn . ,body)
                  ;; this could send back a 200 if there was a message to be returned 
                  ;; it would need to force output because
                  ;; provide service won't handle it 6/11/95 -- JCMa.
                  (ecase status
                    (:deleted 
                      (setf (server-status ,server) 204.)
                      (report-status-no-content ,stream))
                    (:accepted
                      (setf (server-status ,server) 202.)
                      (report-status-accepted,stream)))
                  ;; write some headers as the close of transaction
                  (write-headers* ,stream :date (server-request-time *server*) :location (url:name-string url)
                                  :server *server-version*))))
    (flet ((get-url-and-translation-method (url-string &aux url translation-method)
	     (or (and (setq url (intern-url url-string :if-does-not-exist :soft))
		      (setq translation-method (translation-method url)))
		 (and *auto-export*
		      (setq url (auto-export-pathname-url url-string))
		      (setq translation-method (translation-method url)))
		 (and (setq url (locate-controlling-url url-string method (url:valid-search-url-p url-string)))
		      (setq translation-method (translation-method url))))
	     (values url translation-method)))
      (declare (inline get-url-and-translation-method))
      (with-slots (stream url-string) server
	(multiple-value-bind (url translation-method)
	    (get-url-and-translation-method url-string)
	  (cond ((and url translation-method)
		 (setf (server-url server) url)
		 (with-access-control (url method server (or (url::write-subnets url) *write-subnets*)
					   :write-method-p t)
		   (case translation-method	; redirect when there is forwarding.
		     (:temporary-redirect
		       (standard-url-redirect url :temporary-redirect :delete))
		     (:redirect
		       (standard-url-redirect url :permanent-redirect :delete))
		     (:reverse-proxy
		       (invoke-reverse-proxy-service server url :delete))
		     (t (with-delete-response (server stream)
					      (delete-document url stream))))))
		(t (error 'document-not-found :url url-string :method :delete))))))))

(defmethod invoke-server-method ((server basic-server-mixin) (method (eql :options)) (http-version (eql :http/1.1)))
  (macrolet ((handle-redirect (condition tag)
	       `(destructuring-bind (target-url &rest other-urls) (new-urls ,condition)
		  (cond ;; optimize redirect by reinvoking for singleton local url.
		    ((and (null other-urls) (local-redirect-p target-url))
		     (setf (server-url-string server) (url:name-string target-url))
		     (go ,tag))
		    (t (report-status ,condition stream))))))
    (with-slots (stream url-string) server
      (tagbody
	retry1
	   (handler-case
	     (multiple-value-bind (url)
		 (url:intern-url url-string :if-does-not-exist :soft)
	       (tagbody
		 retry2
		    (cond
		      (url
		       (cond ((or (translation-method url)
				  (and *auto-export* (auto-export-pathname-url url-string)))
			      (setf (server-url server) url) 
			      (with-access-control (url method server (or (url:secure-subnets url) *secure-subnets*)
							:deny-subnets *disallowed-subnets*)
				(write-document-headers url :options stream)))
			     (t (error 'document-not-found :url url :method :options))))
		      ((and *auto-export* (auto-export-pathname-url url-string))
		       (go retry1))
		      ((setq url (locate-controlling-url url-string method (url:valid-search-url-p url-string)))
		       (go retry2))
		      (t (error 'document-not-found :url (or url url-string) :method :options)))))
	     (redirection (cond) (handle-redirect cond retry1))))))) 

;; This method does not handle reverse proxying -- JCMa 9/26/2000.
(defmethod invoke-server-method :around ((server basic-server-mixin) (method (eql :options)) (http-version (eql :http/1.1)))
  (with-slots (address url-string stream) server
    (flet ((wild-p (url)
             (let ((pos (1- (length url))))
               (and (eql (aref url pos) #\*)
                    (eql (aref url (decf pos)) #\/)
                    (= (count #\/ url :test #'eql) 3)))))
      (declare (inline wild-p))
      (if (wild-p url-string)
          (with-subnet-access-control (address *secure-subnets* :deny-subnets *disallowed-subnets*
                                               :rejection-form (error 'access-forbidden :method method :url url-string))
            (write-document-headers :wild :options stream))
          (call-next-method server method http-version)))))

(defgeneric write-document-trace (server url &optional additional-headers)
   (:documentation "Writes an HTTP trace message for URL to stream."))

(defmethod write-document-trace ((server basic-server-mixin) (url url) &optional additional-headers)
  (let ((stream (server-stream server))
        (headers (server-headers server)))
    (with-chunked-transfer-encoding
        (stream '(:message :http) :status :success 
                :content-location url
                :cache-control '(:no-cache t)
                :additional-headers additional-headers)
      (fast-format stream "~A~%" (%server-request server))
      (write-header-buffer headers stream t))))

(defmethod invoke-server-method ((server basic-server-mixin) (method (eql :trace)) (http-version (eql :http/1.1)))
  (with-slots (url-string) server
    (let ((url (url:intern-url url-string :if-does-not-exist :soft)))
      (cond ((or (and url (translation-method url))
		 (and *auto-export* (setq url (auto-export-pathname-url url-string)))
		 (and (setq url (locate-controlling-url url-string method (url:valid-search-url-p url-string)))
		      (translation-method url)))
	     (setf (server-url server) url) 
	     (with-access-control
	       (url method server (or (url:secure-subnets url) *secure-subnets*)
		    :deny-subnets *disallowed-subnets*)
	       ;; this is the meat of the trace response
	       (write-document-trace server url)))
	    (t (error 'document-not-found :url url-string :method :trace))))))


;;;------------------------------------------------------------------- 
;;;
;;; PUT METHOD 
;;;

(defmacro with-successful-put-response ((server stream &rest additional-headers) &body body)
  (declare (values url exported))
  `(flet ((execute-body (stream) . ,body))
     (declare (dynamic-extent #'execute-body))
     (let* ((server ,server))
       (case (server-http-version server)
         ((:http/1.0 :http/0.9)
          (multiple-value-bind (url status last-modification)
              (execute-body ,stream)
            (ecase status
              (:modified 
                (setf (server-status server) 204.)
                (report-status-no-content ,stream)
                (write-headers* ,stream 
                                :date (server-request-time *server*)
                                :server *server-version*
                                :location url
                                :last-modified last-modification
                                ,@additional-headers)
                (values url nil))
              (:created
                (setf (server-status server) 201.)
                (report-status-created ,stream)
                (write-headers* ,stream 
                                :date (server-request-time *server*)
                                :server *server-version*
                                :location url
                                :last-modified last-modification
                                ,@additional-headers)
                (values url t)))))
         (t (report-status-continue ,stream)    ; alert the HTTP 1.1 or greater client that we are ready
            (send-cr-line-feed ,stream)
            (force-output ,stream)
            (setf (server-status server) 100.)
            (multiple-value-bind (url status last-modification)
                (execute-body ,stream)
              (ecase status
                (:modified 
                  (setf (server-status server) 204.)
                  (report-status-no-content ,stream)
                  (write-headers* ,stream 
                                  :date (server-request-time *server*)
                                  :server *server-version*
                                  :location url
                                  :last-modified last-modification
                                  ,@additional-headers)
                  (values url nil))
                (:created
                  (setf (server-status server) 201.)
                  (report-status-created ,stream)
                  (write-headers* ,stream 
                                  :date (server-request-time *server*)
                                  :server *server-version*
                                  :location url
                                  :last-modified last-modification
                                  ,@additional-headers)
                  (values url t)))))))))

(defun %put-new-resource (server stream url-string &aux (length (length url-string)))
  (labels ((url-inferior-directory (parent 1st-delim)
             (loop with start = (incf 1st-delim)
                   while (< start length)
                   for delim = (position #\/ url-string :start start :end length :test #'eql)
                   while delim
                   collect (subseq url-string start delim) into inf-path
                   do (setq start (1+ (the fixnum delim)))
                   finally (return (let ((path (translated-pathname parent)))
                                     (make-pathname :host (pathname-host path)
                                                    :device (pathname-device path)
                                                    :directory `(,@(pathname-directory path) ,.inf-path))))))
           (url-inferior-pathname (parent 1st-delim last-delim directory-level)
             (let ((name-and-extension (subseq url-string (1+ (the fixnum last-delim)) length)))
               (declare (dynamic-extent name-and-extension))
               (merge-pathnames name-and-extension (case directory-level
                                                     (0 (translated-pathname parent))
                                                     (t (url-inferior-directory parent 1st-delim))))))
           (put-inferior (parent 1st-delim last-delim directory-level)
             (let ((pathname (url-inferior-pathname parent 1st-delim last-delim directory-level)))
               (unless (or (zerop directory-level) (probe-directory pathname))
                 (create-directories-recursively pathname))
               (multiple-value-bind (url)
                   (intern-url (merge-url url-string (local-context)) :if-does-not-exist :create)
		 (setf (server-url server) url)	;tell the server about it
                 (inherit-export-parameters url parent)
                 (setf (url:translated-pathname url) pathname
                       (url:translation-method url) (export-type-for-pathname-type (pathname-type pathname) t))
		 (server-update-url-specific-timeouts server url)
                 (put-document url stream t nil)))))
    (multiple-value-bind (parent p-export-type export-type directory-level 1st-delim last-delim)
        (most-specific-exported-parent-url url-string -1 length)
      (unless parent (error 'document-not-found :url url-string :method :put))
      (with-access-control (parent :put server (or (url::write-subnets parent) *write-subnets*)
                                   :deny-subnets *disallowed-subnets* :write-method-p t)
        (cond ((not (and export-type ;;object with export type matched to directory export type.
                         (directory-type-exports-pathname-export-type-p p-export-type export-type)))
               (error 'method-not-allowed :method :put :url url-string))
              ((directory-export-type-p p-export-type)  ;single level directory export.
               (case directory-level
                 (0 (put-inferior parent 1st-delim last-delim 0))
                 (t (error 'document-not-found :url url-string :method :put))))
              ((hierarchical-directory-export-type-p p-export-type)
               (case directory-level
                 (0 (put-inferior parent 1st-delim last-delim 0))
                 ;; need to handle the export of newly created intervening directory levels.  6/11/95 -- JCMa.
                 (t (put-inferior parent 1st-delim last-delim directory-level))))
              (t (error 'method-not-allowed :method :put :url url-string)))))))

(defmethod invoke-server-method ((server basic-server-mixin) (method (eql :put)) (http-version symbol)
				 &aux url translation-method)
  (with-slots (stream url-string) server
    (cond ;; resource already exists, so put a new version.
      ((and (setq url (or (url:intern-url url-string :if-does-not-exist :soft)
			  (locate-controlling-url url-string method (url:valid-search-url-p url-string))))
	    (setq translation-method (translation-method url)))
       (setf (server-url server) url)
       (with-access-control (url method server (or (url::write-subnets url) *write-subnets*)
				 :deny-subnets *disallowed-subnets*
				 :write-method-p t)
	 (case translation-method
	   (:temporary-redirect
	     (standard-url-redirect url :temporary-redirect :put))
	   (:redirect
	     (standard-url-redirect url :permanent-redirect :put))
	   (:reverse-proxy
	     (invoke-reverse-proxy-service server url :put))
	   (t (server-update-url-specific-timeouts server url)
	      (put-document url stream nil *check-document-versions*)))))
      (t (%put-new-resource server stream url-string)))))


;;;------------------------------------------------------------------- 
;;;
;;; PARSE FORM RAW VALUES
;;;

(defun %intern-new-form-query-keyword (string &optional (start 0) (end (length string)))
  (declare (fixnum start end))
  (intern (nstring-upcase (subseq string start end) :start 0 :end (- end start)) *keyword-package*))

(tk1:define-tokenizer form-query-keyword
                      :tokenizer '%intern-new-form-query-keyword
                      :test 'char-equal
                      :definer define
                      :documentation "Tokenizes form query keywords without consing.")

(defun %parse-form-collect-form-value (query value baggage alist ptr &aux entry)
  (declare (values new-ptr))
  (macrolet ((make-entry (query value baggage)
	       `(if ,baggage (list (list ,query ,value nil ,baggage)) (list (list ,query ,value)))))
    ;; legend (keyword value collapsed-value-p query-name-baggage)
    ;; (break "Query: ~S~&Value: ~S" query value)
    (cond ((and alist (setq entry (assoc query alist :TEST #'EQ)))
	   (cond
	     ((eql t (third entry))		;collecting values?
	      (setf (second entry) `(,.(second entry) ,value))
	      (etypecase (fourth entry)
		(cons (setf (fourth entry) `(,.(fourth entry) ,baggage)))
		(null (when baggage
			(setf (cdddr entry) `((,baggage)))))))
	     ((fourth entry)			;first collect with prior baggage
	      (setf (cdr entry) `((,(second entry) ,value) t ,(if baggage `(,(fourth entry) ,baggage) `(,(fourth entry))))))
	     ;; first collect with no prior baggage
	     (t (setf (cdr entry) `((,(second entry) ,value) t))))
	   ptr)
	  (ptr (prog1 (setq entry (make-entry query value baggage))
		      (setf (cdr ptr) entry)))
	  (t (make-entry query value baggage)))))

(define parse-form-raw-values (stream buffer bytes &optional (durable-values-p *durable-form-values*))
  "Function that parses bytes with of form values from STREAM.
  It returns an ALIST of (QUERY-ID VALUE COLLECTION-FLAG QUERY-BAGGAGE).  When
  multiple values for QUERY-ID are collected into VALUE, COLLECTION-FLAG
  is non-null.  Otherwise, there is no third value.
  QUERY-BAGGAGE is an overloading field on the query-id that can carry additional information.
  When present it is an atom when COLLECTION-FLAG is null and a list when COLLECTION-FLAG is
  non-null."
  (declare (values raw-query-value-alist))
  (labels ((get-raw-string (buffer start end)
	     (declare (fixnum start end))
	     (multiple-value-bind (string new-end)
		 (nstring-translate-chars buffer start end #\space)
	       (cond ((= start new-end) nil)
		     (durable-values-p (subseq string start new-end))
		     (t (make-array (- (the fixnum new-end) start) :element-type (array-element-type string)
				    :displaced-to string :displaced-index-offset start)))))
	   (unpacked-query-indices (string start end)
	     (declare (fixnum start end))
	     (let ((pos2 (1- end))
		   pos1)
	       (if (and (eql (aref string pos2) #\))
			(setq pos1 (char-position #\( string start pos2)))
		   (values pos1 (1+ (the fixnum pos1)) pos2)
		   (values end))))
	   (get-keyword (buffer start end)
	     (multiple-value-bind (string new-end)
		 (nstring-translate-chars buffer start end #\space)
	       (multiple-value-bind (key-end baggage-start baggage-end)
		   (unpacked-query-indices string start new-end)
		 (values (%tokenize-form-query-keyword string start key-end)
			 (when (and baggage-start baggage-end)
			   (get-raw-string string baggage-start baggage-end)))))))
    (declare (inline unpacked-query-indices get-raw-string get-keyword))
    ;; collect the form data over the HTTP connection by reading bytes of characters.
    (multiple-value-bind (string end)
	(crlf-stream-copy-into-string stream bytes 0 buffer)
      (loop with query and baggage and value and alist and ptr 
	    with start fixnum = 0
	    while (< start end)
	    for key-end fixnum = (or (char-position #\= string start end)
				     (error 'bad-form-data-posted
					    :format-string "Bad Form Data Posted: No Query delimiter found in ~S."
					    :format-args (list (subseq string start end))))
	    do (multiple-value-setq (query baggage)
		 (get-keyword string start key-end))
	       ;; get the value
	       (let* ((val-start (1+ key-end))
		      (val-end (or (char-position #\& string val-start end) end)))
		 (declare (fixnum val-start val-end))
		 (setq value (get-raw-string string val-start val-end)	;get the query value
		       start (1+ val-end)))
	    when (and query value)		;maintain query order
	      do (setq ptr (%parse-form-collect-form-value query value baggage alist ptr))
		 (or alist (setq alist ptr))
		 #+ignore (format t "~&QUERY: ~S~:[~;~&Baggage: ~:*~S~]~&VALUE: ~S" query baggage value)
	    finally (return alist)))))

(define-generic durable-form-values-p (http-url)
  (:documentation "Returns non-null when HTTP-URL requires durable form values."))

(defmethod durable-form-values-p ((url url::form-processing-mixin))
  (multiple-value-bind (value found-p)
      (get-value url :durable-form-values-p)
    (if found-p value *durable-form-values*)))

(define-generic file-upload-maximum-size (http-url)
  (:documentation "Returns the maximum file size allowed for file upload for this URL."))

(defmethod file-upload-maximum-size ((url url::form-processing-mixin))
  (multiple-value-bind (value found-p)
      (get-value url :maximum-upload-file-size)
    (if found-p value *file-upload-maximum-size*)))

(defmacro handling-form-response-errors ((url form-alist) &body body)
  `(flet ((%handle-error-computing-form-response (error)
	    (typecase error
	      (network-error nil)		;Pass through network errors for logging higher up
	      (t (setf (server-status *server*) 500)
		 (error 'error-handling-post-method :url ,url :server-error error
			:headers (header-plist)
			:form-alist (and ,form-alist (durable-form-values-p ,url))
			:format-string "POST Error computing form response for ~A."
			:format-args (list (url:name-string ,url))
			:stack-backtrace (when *stack-backtraces-in-bug-reports*
					   (stack-backtrace-string error)))))))
     (declare (dynamic-extent #'%handle-error-computing-form-response))
     (handler-bind-if (not *debug-server*)
	((error #'%handle-error-computing-form-response))
       . ,body)))

(define-generic apply-form-response-function (URL stream form-alist)
  (:documentation "Applies the form response function for URL according to form-alist with client HTTP stream, stream."))

(defmethod apply-form-response-function (url stream (form-alist null))
  (declare (ignore stream))
  (error 'bad-syntax-provided :format-string "No form values were returned for ~A."
	 :format-args (list (url:name-string url)) :url url :close-connection t))

(defmethod apply-form-response-function ((url url::form-processing-mixin) stream form-alist)
  (let ((fctn (url:response-function url)))
    (unless fctn
      (error 'server-internal-error :url url
	     :format-string "No Response function was found for ~A."
	     :format-args (list (url:name-string url))))
    (handling-form-response-errors (url form-alist)
      (funcall fctn url stream form-alist))))

(defmacro with-post-form-context ((bytes &key (buffer-var 'buffer)) &body body)
  "Provides the context within which form raw values remain valid."
  `(progn
     (unless ,bytes
       (error 'content-length-required :url url
	      :format-string "No content-length header provided for ~A."
	      :format-args (list (url:name-string url))))
     (using-resource (,buffer-var post-form-buffer ,bytes)
       ,@body)))

(defmacro handling-form-parsing-errors ((url) &body body)
  `(flet ((%handle-error-reading-form-values (error)
	    (typecase error
	      (network-error nil)		;Pass through network errors for logging higher up
	      (t (setf (server-status *server*) 500)
		 (error 'error-handling-post-method :url ,url :server-error error
			:headers (header-plist)
			:format-string "POST Error reading form values for ~A."
			:format-args (list (url:name-string ,url))
			:stack-backtrace (when *stack-backtraces-in-bug-reports*
					   (stack-backtrace-string error)))))))
     (declare (dynamic-extent #'%handle-error-reading-form-values))
     (handler-bind-if (not *debug-server*)
	((error #'%handle-error-reading-form-values))
       . ,body)))

(defun %post-document-handle-url-coded-form (url stream)
  (let ((bytes (get-header :content-length))
	(durable-values-p (durable-form-values-p url))
	form-alist)
    (with-post-form-context (bytes)
      (handling-form-parsing-errors (url)
	(setq form-alist (parse-form-raw-values stream buffer bytes durable-values-p))
	;; Only when values are durable, keep track of form values for
	;; possible logging or subsequent access
	(when durable-values-p
	  (setf (server-form-data *server*) form-alist))
	(apply-form-response-function url stream form-alist)))))


;;;------------------------------------------------------------------- 
;;;
;;; MIME MULTIPART FORM DATA
;;;

(defparameter *file-upload-default-pathname-name* "temp"
  "The default filename used when no filename is supplied by file upload.")

(defparameter *file-upload-default-pathname-name-maximum-size* 30
  "The default size for filename used when no filename is supplied by file upload.")

(defun file-upload-generate-file-name ()
  (let* ((server *server*)
	 (name *file-upload-default-pathname-name*)
	 (client-address (server-host-ip-address server))
	 (request-time (write-to-string (server-request-time server)))
	 (hash (concatenate 'string client-address request-time))
	 (digest (sha:sha-digest-vector hash))
	 (integer-chars (with-fast-array-references ((array digest vector))
			  (loop with len = (length array)
				and limit = (- *file-upload-default-pathname-name-maximum-size* (length name)  1)
				for i fixnum upfrom 0 below len by 2
				for integer = (aref array i)
				sum (the fixnum (integer-length integer)) into size
				while (< size limit)
				nconc (digit-chars integer 8)))))
    (declare (dynamic-extent request-time hash digest integer-chars))
    (concatenate 'string name "-" integer-chars)))

;; coerce _ to - due to error in LW 4.3.7
;; (make-pathname :defaults #p"http:uploads;" :name "roctrip_millauF" :TYPE "mov")
;; why not use URL encoding to make directory delimiters go away? 4/13/2001 -- JCMa.
(defun file-upload-parse-filename (filename directory content-type)
  (flet ((strip-directory-info (string)
	   (loop with end fixnum = (length string)
		 with last-idx fixnum = (1- end)
		 for idx fixnum downfrom last-idx to 0
		 when (char-position (aref string idx) "\\/:>" 0 4)	;directory delimiters
		   return (if (= idx last-idx)
			      (error 'bad-multipart-form-data-posted
				     :format-string "File Upload: Bad file name, ~S."
				     :format-args (list string) :url (server-url *server*))
			      (subseq filename (1+ idx) end))
		 finally (return filename)))
	 (trim-characters (string trim-chars)
           (flet ((massage-chars (string)
                    (when string (nsubstitute #\- #\_ string))))	;lw 4.3 chokes on _
             (declare (inline massage-chars))
             (when string
               (let* ((start 0)
                      (end (length string))
                      (chars (- end start))
                      n-chars)
                 (declare (fixnum start end))
                 (unless (zerop chars)
                   (massage-chars
		     ;; removes version numbers and random emacs ~'s
		     (with-string-trim-bounds (trim-chars string start end)
		       (setq n-chars (- end start))
		       (cond ((= chars (setq n-chars (- end start)))
			      string)
			     ((zerop n-chars) nil)
			     (t (subseq string start end)))))))))))
    (let* ((path (pathname (strip-directory-info filename)))
           (path-name (pathname-name path))
	   (p-name (or (ensure-safe-pathname-component path-name 0 (min (length path-name) *file-upload-default-pathname-name-maximum-size*))
		       (file-upload-generate-file-name)))
	   (p-type (trim-characters (pathname-type path) '#.(coerce "~0123456789" 'list))))
      (cond (p-type
	     (let ((content-type-from-p-type (mime-content-type-spec-for-pathname-type p-type nil)))
	       (cond ((and content-type content-type-from-p-type)
		      (unless (mime-content-type-spec-equal-p content-type content-type-from-p-type nil)
			(setq content-type content-type-from-p-type)))
		     (content-type-from-p-type
		      (setq content-type content-type-from-p-type))
		     (t (setq content-type '(:application :octet-stream))))))	;default binary
	    (content-type
	     (setq p-type (or (mime-content-type-primary-extension-string content-type nil)
			      (and (second content-type) (string-downcase (symbol-name (second content-type))))
			      "rdm")))		;random extension
	    (t (setq content-type '(:application :octet-stream))))	;default binary
      (values (make-pathname :defaults directory :name p-name :type p-type)
	      (if content-type (mime-content-type-copy-mode content-type) :binary)))))

(defun parse-mime-multipart-form-data-block (mime-stream boundary &optional buffer)
  "Parses and returns a mime multipart form data block from MIME-STREAM upto BOUNDARY.
BUFFER is a line buffer."
  (declare (values query-name value baggage last-block-p))
  (with-header-values (content-disposition content-type) *headers*
    (destructuring-bind (&key name filename &allow-other-keys)
	(cdr content-disposition)
      (cond ((null name)
	     (if content-disposition
		 (error 'bad-multipart-form-data-posted  :url (server-url *server*)
			:format-string "Multipart Form: No name provided for the form element.")
		 (error 'bad-multipart-form-data-posted :url (server-url *server*)
			:format-string "Multipart Form: No Content-Disposition header.")))
	    ((or (null filename) (null-string-p filename))
	     (multiple-value-bind (raw-value last-block-p)
		 (mime-stream-decode-into-string-until-boundary mime-stream boundary nil buffer)
	       (values (%tokenize-form-query-keyword name) raw-value nil last-block-p)))
	    (t (multiple-value-bind (query-name directory)
		   (html2::file-upload-unpack-query-baggage name) 
		 (multiple-value-bind (destination copy-mode)
		     (file-upload-parse-filename filename directory content-type)
		   (multiple-value-bind (pathname last-block-p)
		       (mime-stream-decode-into-file-until-boundary mime-stream destination boundary copy-mode buffer)
		     ;; Record the pathname used to save the file as the value
		     ;; of the input element.  Include the original filename as
		     ;; an additional keyword value pair.
		     (values (%tokenize-form-query-keyword query-name)
			     pathname
			     `(:upload-filename ,filename :content-type ,content-type :copy-mode ,copy-mode )
			     last-block-p)))))))))

(defun signal-file-upload-maximum-size-exceeded (bytes maximum-size)
  (declare (ignore bytes))
  (error 'file-upload-maximum-size-exceeded :url (server-url *server*)
	 :format-string "File Upload Error: Attempt to upload more than the allowed maximum file size of ~D bytes."
	 :format-args (list maximum-size)))

(defun parse-mime-multipart-form-data (mime-stream maximum-file-size &optional durable-values-p)
  (declare (values raw-query-value-alist)
	   (ignore durable-values-p))
  "Top-level method for parsing multipart form data from mime-stream.
  It returns an ALIST of (QUERY-ID VALUE COLLECTION-FLAG QUERY-BAGGAGE).  When
  multiple values for QUERY-ID are collected into VALUE, COLLECTION-FLAG
  is non-null.  Otherwise, there is no third value.
  QUERY-BAGGAGE is an overloading field on the query-id that can carry additional information.
  When present it is an atom when COLLECTION-FLAG is null and a list when COLLECTION-FLAG is
  non-null."
  (destructuring-bind (&key boundary &allow-other-keys) (cddr (get-header :content-type))
    (unless boundary
      (error 'bad-multipart-form-data-posted
	     :format-string "Multipart Form: No boundary provided with header, Content-Type: ~A"
	     :format-args (list (get-raw-header :content-type *headers* t)) :url (server-url *server*)))
    (with-mime-multipart-block-size-limit (maximum-file-size :error-function 'signal-file-upload-maximum-size-exceeded)
      (using-resource (line-buffer line-buffer *line-buffer-size*)
	(let ((boundary-length (length boundary))
	      alist ptr)
	  (flet ((parse-multipart-block (mime-stream)
		   (with-headers (mime-stream)
		     (with-header-values (content-disposition) *headers*
		       (unless content-disposition
			 (error 'bad-multipart-form-data-posted
				:format-string "Multipart Form: No Content-Dispostion header in data block" :url (server-url *server*)))
		       (destructuring-bind (disposition &rest plist) content-disposition
			 (declare (ignore plist))
			 (case disposition
			   (:form-data
			     (multiple-value-bind (query value baggage last-block-p)
				 (parse-mime-multipart-form-data-block mime-stream boundary line-buffer)
			       (setq ptr (%parse-form-collect-form-value query value baggage alist ptr))
			       (or alist (setq alist ptr))
			       last-block-p))
			   (t (error 'bad-multipart-form-data-posted
				     :format-string "Multipart Form: No handler to parse block with Content Dispostion: ~A"
				     :format-args (list (get-raw-header :content-disposition *headers* t)) :url (server-url *server*)))))))))
	    (declare (inline parse-multipart-block))
	    (multiple-value-bind (line error-p delimiter length)
		(read-delimited-line mime-stream '(#\Return #\Linefeed) nil line-buffer)
	      (declare (ignore delimiter))
	      (when error-p
		(return-from parse-mime-multipart-form-data nil))
	      (multiple-value-bind (multipart-boundry-p last-block-p)
		  (mime-boundary-line-p line boundary length boundary-length)
		(when (and multipart-boundry-p (not last-block-p))
		  (loop until (parse-multipart-block mime-stream)))))
	    alist))))))

(defun %post-document-handle-mime-multipart-form-data (url stream)
  (let ((durable-values-p (durable-form-values-p url))
	(maximum-size (file-upload-maximum-size url))
	form-alist)
    (setq form-alist (handling-form-parsing-errors (url)
		       (parse-mime-multipart-form-data stream maximum-size durable-values-p) ))
    (when durable-values-p
      (setf (server-form-data *server*) form-alist))
    (apply-form-response-function url stream form-alist)))

;;;------------------------------------------------------------------- 
;;;
;;; POST DOCUMENT METHOD
;;;

(define-generic post-document (url type subtype stream)
  (:documentation "Method for posting to url with mime type TYPE/SUBTYPE from HTTP STREAM."))

(defmethod post-document (url type subtype stream)
  (declare (ignore type subtype stream))
  (error 'method-not-allowed :method :post :url url :close-connection t
         :format-string "The POST method is not supported for URLs of class ~A."
         :format-args (list (type-of url))))

(defmethod post-document ((url url:form-processing-mixin) type subtype stream)
  (declare (ignore stream))
  (error 'unsupported-media-type :url url :close-connection t
         :format-string "Unsupported Media Type: POST method with the MIME type ~A/~A is not implemented."
         :format-args (list type subtype)))

(defmethod post-document ((url url:form-processing-mixin) (type null) (subtype null) stream)
  (declare (ignore stream))
  (error 'request-missing-content-type-header :url url :close-connection t))

(defmethod post-document ((url url:form-processing-mixin) (type (eql :application)) (subtype (eql :x-www-form-urlencoded)) stream)
  (%post-document-handle-url-coded-form url stream))

(defmethod post-document ((url url:form-processing-mixin) (type (eql :application)) (subtype (eql :www-form-urlencoded)) stream)
  (%post-document-handle-url-coded-form url stream))

(defmethod post-document ((url url:form-processing-mixin) (type (eql :application)) (subtype (eql :www-form-url-encoded)) stream)
  (%post-document-handle-url-coded-form url stream))

(defmethod post-document ((url url:form-processing-mixin) (type (eql :multipart)) (subtype (eql :form-data)) stream)
  (%post-document-handle-mime-multipart-form-data url stream))

;; Kludge to allow Apple's Sherlock in MacOS to actually work despite
;; their incompetent implementation.   11/6/98 -- JCMa.
(defmethod post-document ((url url:form-processing-mixin) (type (eql :text)) (subtype (eql :plain)) stream)
  ;; "Mozilla/4.x (compatible; Apple Find 2.0; Macintosh PPC)"
  ;; (user-agent-lie version-lie true-loser)
  ;; :MOZILLA :4.X (:COMPATIBLE :APPLE :FIND :|2.0| :MACINTOSH :PPC)
  (multiple-value-bind (user-agent version comment)
      (current-user-agent)
    user-agent version
    (cond ((and (eq (second comment) :apple)
		(eq (third comment) :find)
		(eq (fourth comment) :|2.0|))
	   (post-document url :application :www-form-url-encoded stream))
	  (t (error 'server-not-implemented :url url
		    :format-string "Posting with the MIME type ~A/~A is not implemented."
		    :format-args (list  type subtype))))))

;; Support brain-dead IE 4.0.1 under windows with POST.
(defmethod post-document ((url url:form-processing-mixin) (type cons) (subtype cons) stream)
  (if (and (equal type '(:application :x-www-form-urlencoded))
           (equal subtype '(:application :x-www-form-urlencoded)))
      (%post-document-handle-url-coded-form url stream)
      (call-next-method)))


;;;------------------------------------------------------------------- 
;;;
;;;  PUT DOCUMENT
;;;

(define-generic put-document (url stream &optional newly-created-url-p check-versions-p)
  (declare (values url status content-type-spec bytes last-modification))
  (:documentation "Deposits a document read from STREAM into a pathname denoted by URL.
When CHECK-VERSIONS-P is non-null, the derived-from header is checked against the existing
resource version."))

(defmethod put-document (url stream &optional newly-created-url-p check-versions-p)
  (declare (ignore stream newly-created-url-p check-versions-p))
  (error 'method-not-allowed :method :put :url url
         :format-string "The PUT method is not supported for URLs of class ~A."
         :format-args (list (type-of url))))

;; used by 1.0 put method
(defun check-derived-from-version (url version &aux derived-from)
  (when (and (setq derived-from (get-header :derived-from))
             (not (equal derived-from version)))
    ;; may wish to capture more information here somtime.
    (error 'document-put-conflict :url url :method :put 
           :format-string "Put Conflict: your version of ~a was derived from one created at ~A,
but the original resources was changed at ~A."
           :format-args (list (html:note-anchor (url:name-string url) :reference url :stream nil)
                              (write-time derived-from nil)
                              (write-time version nil)))))

(defmethod put-document ((url url:http-object) stream &optional newly-created-url-p (check-versions-p t))
  (declare (values url exported))
  (flet ((download-crlf-file (stream pathname bytes copy-mode)
	   ;; Version number is not thread safe -- catch condition and retry -- 9/15/2003 -- JCMa.
	   (let ((crlf-pathname #+Genera (versioned-crlf-pathname pathname) 
				#-Genera (crlf-pathname pathname)))
	     (stream-copy-bytes stream crlf-pathname bytes copy-mode)
	     (decode-crlf-file crlf-pathname)
	     (delete-file-carefully crlf-pathname)))
	 (update-file-properties (pathname server modification-date)
	   (when modification-date
	     (set-file-modification-date pathname modification-date nil))
	   (set-file-author pathname server nil)
	   (autotype-file pathname nil)))
    (let ((pathname (url:translated-pathname url))
          (server *server*)
          (headers *headers*))
      ;; throw out of http transaction if a conflict is detected
      (when check-versions-p
        (let ((version (document-version pathname)))
          (case (server-http-version server)
            ((:http/0.9 :http/1.0)
             (check-derived-from-version url version))
            (t (check-if-match-precondition version t :put headers) 
               (check-if-unmodified-since-precondition version :put headers)))))
      (with-header-values (content-length content-range content-type last-modified transfer-encoding) headers
	;; check for byte ranges in 1.1
	(when content-range
	  (error 'server-not-implemented :url url :method :put
		 :format-string "Putting byte ranges is not implemented."))
	(let ((copy-mode (if content-type (mime-content-type-copy-mode content-type) (url:copy-mode url)))
	      (modification-date (or last-modified (server-request-time server))))
	  (cond (transfer-encoding 
		 (case transfer-encoding
		   #+HTTP-Chunking
		   (:chunked
		     (with-successful-put-response (server stream)
		       (with-chunked-transfer-decoding (stream :headers headers)
			 (case copy-mode
			   (:crlf (stream-decode-crlf-until-eof stream pathname))
			   (t (stream-copy-until-eof stream pathname copy-mode))))
		       (update-file-properties pathname server modification-date)
		       (values url (if newly-created-url-p :created :modified) modification-date)))
		   (t (error 'server-not-implemented :close-connection t :url url :method :put
			     :format-string "The HTTP transfer encoding, ~A, is not implemented."
			     :format-args (list transfer-encoding)))))
		(content-length
		 (handler-case-if (not *debug-server*) 
		    (with-successful-put-response (server stream)
		      (case copy-mode
			(:crlf (download-crlf-file stream pathname content-length copy-mode))
			(t (stream-copy-bytes stream pathname content-length copy-mode)))
		      (update-file-properties pathname server modification-date)
		      (values url (if newly-created-url-p :created :modified) modification-date))
		   (error (err)
			  (error 'error-handling-put-method :url url :method :put :server-error err :headers (header-plist)
				 :format-string "Error executing PUT method for ~A."
				 :format-args (list (url:name-string url))))))
		(t (error 'content-length-required :url url :method :put
			  :format-string "No content-length header provided for ~A."
			  :format-args (list (url:name-string url))))))))))

;;;------------------------------------------------------------------- 
;;;
;;; DELETE DOCUMENT 
;;;

(define-generic delete-document (url stream)
  (declare (values url))
  (:documentation "Deletes the document denoted by URL. and reports on STREAM"))

(defmethod delete-document (url stream)
  (declare (ignore stream))
  (error 'method-not-allowed :method :delete :url url
         :format-string "The DELETE method is not supported for URLs of class ~A."
         :format-args (list (type-of url))))

(defmethod delete-document ((url url:http-object) stream)
  (declare (ignore stream))
  (flet ((delete-document-file (pathname)
           (let ((probe-file (probe-file pathname)))
             (when probe-file
               (delete-file* probe-file :expunge t)))))
    (let ((pathname (url:translated-pathname url)))
      (when pathname
        ;; Delete CRLF files when present
        (case (url:copy-mode url)
          (:crlf (delete-document-file (crlf-pathname pathname))))
        ;; Delete the master file
        (delete-document-file pathname)))))

(defmethod delete-document :around  ((url url:translation-method-mixin) stream &aux translation-method) 
  (declare (ignore stream))
  (cond 
    ((setq translation-method (translation-method url))
     ;; unexport the URL before stepping on the internal contents.
     (let ((url (unexport-url url)))
       ;; allow any other methods to run.
       (handler-case-if 
           (not *debug-server*) 
          (call-next-method)
         (error (err)
                (error 'error-handling-delete-method :url url :server-error err :headers (header-plist)
                       :format-string "Error executing DELETE method for ~A."
                       :format-args (list (url:name-string url)))))
       (values url (if translation-method :deleted :accepted))))
    (t (error 'document-not-found :url url))))

;;;------------------------------------------------------------------- 
;;;
;;; WRITING HEADERS
;;;

(declaim (notinline %write-document-headers-no-pathname))

(defun %write-document-headers-no-pathname (url content-type stream &optional length last-modification version
                                                charset public allow)
  (case (server-http-version *server*)
    ((:http/0.9 :http/1.0)
     (report-status-success stream)
     (%write-document-mime-headers stream content-type charset
                                   length last-modification version (expiration-universal-time url)
                                   nil url (languages url) public allow (url:response-cache-control-directives url)))
    (t (if (entity-tag-if-none-match-p last-modification t)
           (report-status-not-modified stream)
           (report-status-success stream))
       (%write-document-mime-headers stream content-type charset
                                     length last-modification version (expiration-universal-time url)
                                     nil url (languages url) public allow (url:response-cache-control-directives url)))))

(declaim (inline %write-document-headers))

(defun %write-document-headers (url content-type stream &optional charset public allow)
  (let ((pathname (url:translated-pathname url)))
    (cond (pathname
           (handler-case
             (multiple-value-bind (length last-modification version)
                 (file-properties pathname)
               (%write-document-headers-no-pathname
                 url content-type stream length last-modification version charset public allow))
             (file-not-found () (error 'document-not-found :url url))))
          (t (error 'document-not-found :url url)))))

(defun %write-crlf-headers (url content-type stream &optional charset public allow)
  (let ((pathname (url:translated-pathname url)))
    (cond (pathname
           (handler-case
             (multiple-value-bind (source-length last-modification version)
                 (file-properties pathname)
               source-length                    ;ignore
               (let* ((crlf-pathname (ensure-crlf-canonical-file pathname))
                      (crlf-length (file-length-in-bytes crlf-pathname)))
                 ;; ship the header information
                 (%write-document-headers-no-pathname
                   url content-type stream crlf-length last-modification version charset public allow)))
             (file-not-found () (error 'document-not-found :url url))))
          (t (error 'document-not-found :url url)))))

;; standard methods are automatically defined by define-url-export-types
(define-generic write-document-headers (url translation stream)
  (:documentation "Writes the document denoted by URL using TRANSLATION on STREAM.")) 

(defmethod write-document-headers (url translation stream)
  (declare (ignore translation stream))
  (error 'method-not-allowed :method :head :url url
         :format-string "The HEAD method is not supported for URLs of class ~A."
         :format-args (list (type-of url))))

(defmethod write-document-headers ((url url:http-url) (translation (eql :options)) stream)
  (let* ((server *server*)
         (client-version (server-http-version server))
         (url-methods (http-methods url client-version))
         (server-methods (http-methods server client-version))
         (bytes (file-length-in-bytes url))
         (directives `(:no-cache t ,.(loop for (directive value) on (url:response-cache-control-directives url) by #'cddr
                                           unless (eql directive :no-cache)
                                             collect directive
                                             and collect value))))
    (declare (dynamic-extent directives))
    (send-response stream nil
                   :status :success
                   :content-location url
                   :expires (url:expiration-universal-time url)
                   :bytes bytes
                   :public server-methods
                   :allow url-methods
                   :cache-control directives)))

(defmethod write-document-headers ((url (eql :wild)) (translation (eql :options)) stream)
  (let* ((server *server*)
         (server-methods (http-methods server (server-http-version server)))
         (directives `(:no-cache t)))
    (declare (dynamic-extent directives))
    (send-response stream nil
                   :status :success
                   :public server-methods
                   :cache-control directives)))

(defmethod write-document-headers ((url http-template-object) (translation (eql :shtml-file)) stream)
  (%write-document-headers-no-pathname url :html stream))


;;;------------------------------------------------------------------- 
;;;
;;; HEADER METHODS FOR STATIC URLS
;;;

(defmethod write-document-headers ((url url:http-path) (translation (eql :html-file)) stream)
  (%write-document-headers url :html stream))

(defmethod write-document-headers ((url url:http-path) (translation (eql :text-file)) stream)
  (%write-document-headers url :text stream))

(defmethod write-document-headers ((url url:http-form) (translation (eql :html-form)) stream)
  (%write-document-headers url :html stream (character-set url)))


;;;------------------------------------------------------------------- 
;;;
;;; HEADER METHODS FOR COMPUTED URLS
;;;

(defmethod write-document-headers ((url url:computed-url-mixin) translation stream)
  (declare (ignore translation))
  (let ((header-fctn (url:header-function url)))
    (if header-fctn
        (multiple-value-bind (content-type length last-modification version charset public allow)
            (funcall header-fctn url)
          (%write-document-headers-no-pathname url content-type stream length last-modification version charset public allow))
        (%write-document-headers-no-pathname url :html stream))))

(defmethod write-document-headers ((url url:http-client-script) (translation (eql :script)) stream)
  (let ((script (or (url:script url)
                    (error "No Script available for ~S." url))))
    (%write-document-headers-no-pathname url (ns2.0:script-content-type script) stream (ns2.0:script-bytes script))))

;;;------------------------------------------------------------------- 
;;;
;;; WRITING TEXT PATHNAMES
;;;

(declaim (inline %write-document-from-pathname))

(defun %write-document-from-pathname (url pathname content-type stream &optional charset)
  "Primitive to write a text document from pathname to stream without CRLF caching."
  ;; Open text files in binary on windows because the native format is CRLF
  ;; Prevents trouble computing file length observed in Lispworks -- JCMa 9/4/2006
  ;; Scieneer CL has a default of using UTF8 for files. Probably we should check the charset and open the file accordingly
  ;; on platforms that support that...
  (with-open-file (file-stream pathname :direction :input :element-type #-MSWindows *standard-character-type* #+MSWindows '(unsigned-byte 8)
			       #+scl :external-format #+scl :iso-8859-1)
    (with-conditional-get-response
        (stream content-type
                :last-modification (file-stream-creation-date file-stream)
                :character-set charset
                :entity-tag (file-stream-version file-stream)
                :bytes (file-stream-length-in-bytes file-stream)
                :expires (expiration-universal-time url)
                :content-location url
                :cache-control (url:response-cache-control-directives url)
                :content-language (languages url))
      ;; send the contents
      (stream-copy-until-eof file-stream stream #-MSWindows :text #+MSWindows :crlf))))

(declaim (notinline %write-document))

(defun %write-document (url content-type stream &optional charset)
  (let ((pathname (url:translated-pathname url)))
    (cond
      (pathname
       (handler-case
         (write-any-document-from-pathname url pathname stream content-type charset)
         (file-not-found () (error 'document-not-found :url url))))
      (t (error 'document-not-found :url url)))))

;; standard methods are automatically defined by define-url-export-types
(define-generic write-document (url translation stream)
  (:documentation "Writes the document denoted by URL using TRANSLATION on STREAM."))

(defmethod write-document (url translation stream)
  (declare (ignore translation stream))
  (error 'method-not-allowed :method :get :url url
         :format-string "The GET method is not supported for URLs of class ~A."
         :format-args (list (type-of url))))

(defmethod write-document ((url url:http-path) (translation (eql :html-file)) stream)
  (%write-document url :html stream (character-set url)))

;; allows a specialized method to write the directory listing.
(defmethod write-document :around ((url url:http-path) translation stream)
  (let ((fctn (url:directory-writer url)))
    (unless (and fctn (funcall fctn url stream))
      (call-next-method url translation stream))))

(defmethod write-document ((url url:http-form) (translation (eql :html-form)) stream)
  (%write-document url :html stream (character-set url)))

(declaim (notinline %compute-response))

(defun %compute-response (url stream function)
  (flet ((%handle-error-computing-response (error)
           (typecase error
             ;; Pass through network errors for logging higher up
             (network-error nil)
             (t (setf (server-status *server*) 500)
                (error 'error-computing-response
                       :url url
                       :server-error error
                       :headers (header-plist)
                       :format-string "Error computing response for ~A."
                       :format-args (list (url:name-string url))
                       :stack-backtrace (when *stack-backtraces-in-bug-reports*
                                          (stack-backtrace-string error)))))))
    (declare (dynamic-extent #'%handle-error-computing-response))
    (cond (function
           (handler-bind-if (not *debug-server*)
              ((error #'%handle-error-computing-response))
             (funcall function url stream)))
          (t (error 'document-not-found :url url)))))

;; old name 
(defmethod write-document ((url url:http-computed-form) (translation (eql :html-computed-form)) stream)
  (%compute-response url stream (url:form-function url)))

(defmethod write-document ((url url:http-computed-form) (translation (eql :computed-form)) stream)
  (%compute-response url stream (url:form-function url)))

;; this inherits to most computed URLs!
(defmethod write-document ((url url:computed-url-mixin) (translation symbol) stream)
  (%compute-response url stream (url:response-function url)))

;; If there are search parameters, execute the search response function,
;; otherwise hand over the document.

(defmethod write-document :around ((url url:http-searchable-object) (translation symbol) stream)
  (with-slots (url:search-keys) url
    (if url:search-keys
        (%compute-response url stream (url:response-function url))
        (call-next-method))))

(defmethod write-document ((url url:http-client-script) (translation (eql :script)) stream)
  (let ((script (or (url:script url)
                    (error "No Script available for ~S." url))))
    (with-successful-response (stream (html4.0:script-content-type script)
                                      :bytes (html4.0:script-bytes script)
                                      :expires (expiration-universal-time url)
                                      :cache-control (url:response-cache-control-directives url))
      ;; send the contents
      (html4.0:write-raw-script script stream))))

;;;------------------------------------------------------------------- 
;;;
;;; WRITING BINARY TEMPLATES
;;;

(defun %%write-binary-template-file (pathname url content-type stream &optional charset)
  (let ((template-parameters (url::template-parameters url)))
    (cond (template-parameters
           (with-open-file (file-stream pathname :direction :input :element-type '(unsigned-byte 8))
             (let ((expires (expiration-universal-time url))
                   (cache-control (url:response-cache-control-directives url))
                   (languages (languages url)))
               (with-successful-response (stream content-type :status :success
                                                 :last-modification (server-request-time *server*)
                                                 :character-set charset
                                                 :expires expires
                                                 :cache-control cache-control
                                                 :content-location url
                                                 :content-language languages
                                                 :termination-line-p t)
                 (with-binary-stream (stream :output)
                   (loop for (start end function . param-alist) in template-parameters
                         for n-bytes = (unless (eql start end) (- (the fixnum end) (the fixnum start)))
                         when n-bytes
                           do (file-position file-stream start)
                              (stream-copy-bytes file-stream stream n-bytes)
                         when function
                           do (funcall function url stream param-alist)))))))
          (t (%%write-binary-file pathname url content-type stream charset)))))

(declaim (inline %write-template-from-pathname))

(defun %write-template-from-pathname (url pathname content-type stream &optional charset)
  (multiple-value-bind (crlf-pathname newly-updated-p)
      (ensure-crlf-canonical-file pathname)
    (when (or newly-updated-p (null (template-parameters url)))
      (url:parse-template url content-type crlf-pathname))
    (%%write-binary-template-file crlf-pathname url content-type stream charset)))

(defun %write-template-file (url content-type stream &optional charset)
  (let ((pathname (url:translated-pathname url)))
    (cond
      (pathname
       (handler-case
         (%write-template-from-pathname url pathname content-type stream charset)
         (file-not-found () (error 'document-not-found :url url))))
      (t (error 'document-not-found :url url)))))

(defmethod write-document ((url url:http-template-object) (translation (eql :shtml-file)) stream)
  (%write-template-file url :shtml stream (character-set url)))


;;;------------------------------------------------------------------- 
;;;
;;; WRITING BINARY FORMATS
;;;

(define-constant *known-byte-range-types* '(:bytes)
  "The known set of byte range types which the serevr is able to handle.")

(defun check-byte-range-type-specifier (type url)
  (unless (member type *known-byte-range-types*)
    (signal 'invalid-range-request :url url :format-string "Unknown byte range type specifier, ~S, in: ~S" :format-args (list type (get-raw-header :range)))))

(defmacro %writing-binary-file ((stream url content-type resource-length last-modification version &key charset)
                                range-copy-form copy-form)
  `(flet ((send-single-byte-range (start end content-length expires cache-control languages)
            (let ((headers `(:content-range (:bytes ,start ,(1- (the integer end)) ,resource-length))))
              (declare (dynamic-extent headers))
              (with-successful-response (,stream content-type :status :partial-content :bytes content-length
                                                 :last-modification ,last-modification
                                                 :character-set ,charset
                                                 :entity-tag ,version :expires expires :cache-control cache-control
                                                 :content-location ,url
                                                 :content-language languages
                                                 :additional-mime-headers headers
                                                 :termination-line-p t)
                ,range-copy-form))))
     (declare (dynamic-extent #'send-single-byte-range))
     (let ((expires (expiration-universal-time ,url))
           (cache-control (url:response-cache-control-directives ,url)))
       (handling-conditional-get (stream :last-modification ,last-modification :character-set ,charset
                                         :entity-tag ,version :expires expires :cache-control cache-control :termination-line-p t)
         (let ((languages (languages ,url))
               (range (get-header :range))) ;ignores syntactically invalid headers, when sends the full content.
           (cond ((null range) ;; Send the full content
                  (with-successful-response (,stream ,content-type :status :success :bytes ,resource-length
                                                     :last-modification ,last-modification
                                                     :character-set ,charset
                                                     :entity-tag ,version :expires expires :cache-control cache-control
                                                     :content-location ,url
                                                     :content-language languages
                                                     :termination-line-p t)
                    ,copy-form))
                 ;; Send a single byte range
                 ((not (cddr range)) ;single case is '(:bytes (start end))
                  (check-byte-range-type-specifier (first range) url)
                  (destructuring-bind (start-byte last-byte) (second range)
                    (multiple-value-bind (start end content-length invalid-range-spec-p) 
                        (byte-range-parameters start-byte last-byte ,resource-length)
                      (when invalid-range-spec-p
                        (signal 'invalid-range-request :url ,url :format-string "Invalid Range Header: ~S" :format-args (list (get-raw-header :range))))
                      (send-single-byte-range start end content-length expires cache-control languages))))
                 ;; Send a multi-part byte range
                 (t (check-byte-range-type-specifier (first range) url)
                    (let ((byte-range-spec (parse-multibyte-range-parameters ,url (cdr range) ,resource-length t)))
                      (declare (dynamic-extent byte-range-spec))
                      (cond ((cdr byte-range-spec)
                             (with-successful-response (,stream :multipart-byteranges :status :partial-content 
                                                                :last-modification ,last-modification
                                                                :character-set ,charset
                                                                :entity-tag ,version :expires expires :cache-control cache-control
                                                                :content-location ,url
                                                                :content-language languages
                                                                :termination-line-p t)
                               (loop for (spec . more) = byte-range-spec then more
                                     while spec
                                     do (destructuring-bind (start end content-length)  spec
                                          (declare (ignore content-length))
                                          (let ((content-range `(:bytes ,start ,(1- (the integer end)) ,resource-length)))
                                            (with-mime-multipart-block :multipart-byteranges 
                                                                       (,stream :last-block-p (not more)
                                                                                :content-range content-range
                                                                                :content-type content-type                                                                                                                ;; Confuses Adobe Reader -- Joswig 7/13/2009
                                                                                #|:content-length content-length|#)
                                              ,range-copy-form))))))
                            (t (destructuring-bind (start end content-length) (car byte-range-spec)
                                 (send-single-byte-range start end content-length expires cache-control languages))))))))))))

(defun %%write-binary-file (pathname url content-type stream &optional charset last-modification version)
  (with-open-file (file-stream pathname :direction :input :element-type '(unsigned-byte 8))
    (let ((resource-length (file-stream-length-in-bytes file-stream)))
      (unless-every
        (last-modification (setq last-modification (file-stream-modification-date file-stream)))
        (version (setq version (file-stream-version file-stream))))
      (%writing-binary-file
        (stream url content-type resource-length last-modification version :charset charset)
        (stream-copy-byte-range file-stream stream start end)
        (stream-copy-until-eof file-stream stream :binary)))))

(defun %%caching-write-binary-file (data-cache url content-type stream &optional charset last-modification version)
  (let ((resource-length (data-cache-size data-cache)))
    (unless-every
      (last-modification (setq last-modification (data-cache-last-modification data-cache)))
      (version (setq version (data-cache-version data-cache))))
    (%writing-binary-file
      (stream url content-type resource-length last-modification version :charset charset)
      (with-binary-stream (stream :output)
        (write-cache-data data-cache stream start end))
      (with-binary-stream (stream :output)
        (write-cache-data data-cache stream 0 resource-length)))))

(declaim (notinline %write-binary-file-from-pathname))

(defun %write-binary-file-from-pathname (url pathname content-type stream &optional charset last-modification version)
  (with-data-cache (pathname)
                   (%%caching-write-binary-file data-cache url content-type stream charset last-modification version)
    (%%write-binary-file pathname url content-type stream charset last-modification version)))

(declaim (notinline %write-binary-file))

(defun %write-binary-file (url content-type stream &optional charset last-modification version)
  (let ((pathname (url:translated-pathname url)))
    (if pathname
        (handler-case
          (%write-binary-file-from-pathname url pathname content-type stream charset last-modification version)
          (file-not-found () (error 'document-not-found :url url)))
        (error 'document-not-found :url url))))

(declaim (notinline %write-document-crlf-from-pathname))

(defun %write-document-crlf-from-pathname (url pathname content-type stream &optional charset last-modification version)
  (writing-from-crlf-data-source
    (with-data-cache (pathname)
		     (%%caching-write-binary-file data-cache url content-type stream charset last-modification version)
      (let ((crlf-pathname (ensure-crlf-canonical-file pathname)))
	(%%write-binary-file crlf-pathname url content-type stream charset last-modification version)))))

(declaim (notinline %write-crlf-file))

(defun %write-crlf-file (url content-type stream &optional charset)
  (let ((pathname (url:translated-pathname url)))
    (cond
      (pathname
       (handler-case
         (%write-document-crlf-from-pathname url pathname content-type stream charset)
         (file-not-found () (error 'document-not-found :url url))))
      (t (error 'document-not-found :url url)))))

(define write-any-document-from-pathname (url pathname stream &optional (content-type nil content-type-supplied-p)
                                              charset)
  "Writes any content type from a PATHNAME for URL on STREAM.
CONTENT-TYPE and CHARSET should be supplied, but will default by guessing from pathname."
  (cond (content-type-supplied-p
         (etypecase content-type
           (keyword (setq content-type (mime-content-type-spec content-type)))
           (cons)))
        (t (setq content-type (mime-content-type-spec pathname))))
  ;; content-type needs to be expanded before calling copy mode.
  (ecase (mime-content-type-copy-mode content-type)
    (:text
      (%write-document-from-pathname url pathname content-type stream charset))
    (:crlf
      (%write-document-crlf-from-pathname url pathname content-type stream charset))
    (:binary (%%write-binary-file pathname url content-type stream charset))))

(defmacro %writing-write-binary-range ((stream url content-type last-modification version 
                                               &key charset start end)
                                       range-copy-form)
  `(let ((expires (expiration-universal-time ,url)))
     (handling-conditional-get (,stream :last-modification ,last-modification :character-set ,charset
                                :entity-tag ,version :expires expires :termination-line-p t)
       ;; Send a byte range
       (with-successful-response (,stream ,content-type :status :success :bytes (- ,end ,start)
                                  :last-modification ,last-modification
                                  :character-set ,charset
                                  :entity-tag ,version :expires expires
                                  :cache-control (url:response-cache-control-directives ,url)
                                  :content-location ,url
                                  :content-language (languages ,url)
                                  :termination-line-p t)
         ,range-copy-form))))

(defun %%write-binary-range (url pathname content-type stream start end &optional charset last-modification version)
  (with-open-file (file-stream pathname :direction :input :element-type '(unsigned-byte 8))
    (unless-every
      (last-modification (setq last-modification (file-stream-modification-date file-stream)))
      (version (setq version (file-stream-version file-stream))))
    (%writing-write-binary-range
      (stream url content-type last-modification version :charset charset :start start :end end)
      (stream-copy-byte-range file-stream stream start end))))

(defun %%caching-write-binary-range (data-cache url content-type stream start end &optional charset last-modification version)
  (unless-every
    (last-modification (setq last-modification (data-cache-last-modification data-cache)))
    (version (setq version (data-cache-version data-cache))))
  (%writing-write-binary-range
    (stream url content-type last-modification version :charset charset :start start :end end)
    (with-binary-stream (stream :output)
      (write-cache-data data-cache stream start end))))

(declaim (notinline %write-document-crlf-range-from-pathname))

(defun %write-document-crlf-range-from-pathname (url pathname content-type stream start end 
                                                     &optional charset last-modification version)
  (writing-from-crlf-data-source
    (with-data-cache (pathname)
		     (%%caching-write-binary-range data-cache url content-type stream start end charset last-modification version)
      (let ((crlf-pathname (ensure-crlf-canonical-file pathname)))
	(%%write-binary-range url crlf-pathname content-type stream start end charset last-modification version)))))

;;;------------------------------------------------------------------- 
;;;
;;; URL FORWARDING
;;;

(define-generic redirect-request (server target-uri &optional target-window)
  (:documentation "Causes SERVER to issue a redirect to TARGET-URI.

REDIRECT-REQUEST should only be called within a computed response to a GET
or HEAD method. The POST method should use STANDARD-URL-REDIRECT as should
any other redirections requiring more elaborate control.

REDIRECT-REQUEST must be called before signalling a status code, for example
before entering the macro HTTP:WITH-SUCCESSFUL-RESPONSE.

TARGET-URI can be a single URI or a list of URIs. When it is a list,
the client is free to choose any of the alternatives. Performance
is best if the URIs are already interned. If the targets vary widely they
are best created as :UNINTERNED.

TARGET-WINDOW can be a string designating the client window for output."))

(defmethod redirect-request ((server basic-server-mixin) (target-uri string) &optional target-window)
  (redirect-request server (intern-url target-uri :if-does-not-exist :create) target-window))

(defmethod redirect-request ((server basic-server-mixin) (target-uri uri) &optional target-window)
  (with-slots (url method) server
    (let ((redirection (case method (:post :forward) (t :temporary-redirect))))
      (%standard-url-redirect url target-uri redirection method target-window))))

(defmethod redirect-request ((server basic-server-mixin) (target-uris cons) &optional target-window)
  (with-slots (url method) server
    (let ((redirection (case method (:post :forward) (t :multiple-choice))))
      (if (every #'url::uri-p target-uris)
          (%standard-url-redirect url target-uris redirection method target-window)
	(let ((alternate-uris (mapcar #'(lambda (uri) (intern-url uri :if-does-not-exist :create)) target-uris)))
	  (declare (dynamic-extent alternate-uris))
	  (%standard-url-redirect url alternate-uris redirection method target-window))))))

(defmethod write-document ((url url:http-object) (translation (eql :redirect)) stream)
  (declare (ignore stream))
  (standard-url-redirect url :permanent-redirect :get))

(defmethod write-document ((url url:http-path) (translation (eql :redirect)) stream)
  (declare (ignore stream))
  (standard-url-redirect url :permanent-redirect :get))

(defmethod write-document-headers ((url url:http-object) (translation (eql :redirect)) stream)
  (declare (ignore stream))
  (standard-url-redirect url :permanent-redirect :head))

(defmethod write-document-headers ((url url:http-path) (translation (eql :redirect)) stream)
  (declare (ignore stream))
  (standard-url-redirect url :permanent-redirect :head))

(defmethod write-document ((url url:http-object) (translation (eql :temporary-redirect)) stream)
  (declare (ignore stream))
  (standard-url-redirect url :temporary-redirect :get))

(defmethod write-document ((url url:http-path) (translation (eql :temporary-redirect)) stream)
  (declare (ignore stream))
  (standard-url-redirect url :temporary-redirect :get))

(defmethod write-document-headers ((url url:http-object) (translation (eql :temporary-redirect)) stream)
  (declare (ignore stream))
  (standard-url-redirect url :temporary-redirect :head))

(defmethod write-document-headers ((url url:http-path) (translation (eql :temporary-redirect)) stream)
  (declare (ignore stream))
  (standard-url-redirect url :temporary-redirect :head))


;;;------------------------------------------------------------------- 
;;;
;;; URL EXPORT PRIMITIVES
;;;

(defun %export-url-init-data-cache (url export-args)
  (with-slots (data-universe) url
    (destructuring-bind (&key cache-file-data &allow-other-keys) export-args
      (cond (cache-file-data
             (destructuring-bind (&key file-data-revalidation file-data-wired &allow-other-keys) export-args
               (let ((universe (etypecase cache-file-data
                                 ((member t)
                                  (intern-data-universe *url-file-data-universe-name* :if-does-not-exist :create :class *data-universe-class*))
                                 (string (intern-data-universe cache-file-data :if-does-not-exist :create :class *data-universe-class*))
                                 (file-data-universe cache-file-data))))
                 (initialize-file-data-cache url universe 
                                             :revalidation-interval (or file-data-revalidation *data-universe-revalidation-interval*)
                                             :wired-p file-data-wired))))
            (data-universe
             (deinitialize-file-data-cache url))
            (t nil)))))

(declaim (inline %export-url-path))

(defun %export-url-path (url method args)
  (destructuring-bind (&key pathname character-set &allow-other-keys) args
    (cond (pathname
           (setf (translated-pathname  url) pathname))
          (t (error "No PATHNAME was provided while exporting the URL, ~S, with method, ~S"
                    url method)))
    (setf (translation-method url) method
          (character-set url) character-set)
    ;; initialize data caching
    (%export-url-init-data-cache url args)
    url))

(declaim (inline %export-url-object))

(defun %export-url-object (url export-type args)
  (destructuring-bind (&key pathname character-set &allow-other-keys) args
    (cond (pathname
           (setf (translated-pathname  url) pathname))
          (t (error "No PATHNAME was provided while exporting the URL, ~S, with EXPORT-TYPE, ~S"
                    url export-type)))
    (setf (translation-method url) export-type
          (character-set url) character-set)
    ;; initialize data caching
    (%export-url-init-data-cache url args)))

(declaim (inline %export-url-redirect))

(defun %export-url-redirect (url method args)
  (destructuring-bind (&key alternate-urls pathname &allow-other-keys) args
    (cond (alternate-urls
           (setf (url:alternate-urls url) (mapcar #'url:intern-url (ensure-list alternate-urls))))
          (t (error "Bad specification of ALTERNATE-URLS, ~S, for URL redirect." alternate-urls)))
    (cond (pathname
           (setf (translated-pathname url) pathname))
          (t (setf (cached-pathname url) pathname)))
    (setf (translation-method url) method)
    url))

(defun good-response-function-p (function)
  (typecase function
    ((and symbol (satisfies symbol-fbound-p)) t)
    (function t)                                ;more severe test
    (t nil)))

(defun %export-url-search (url method response-function search-parser search-writer search-database
                               &optional header-function)
  (cond ((and response-function (good-response-function-p response-function))
         (setf (response-function url) response-function))
        (t (error "RESPONSE-FUNCTION, ~S, must be a defined function when exporting the URL, ~S, with method, ~S"
                  response-function url method)))
  (setf (translation-method url) method
        (search-database url) search-database
        (search-parser url) (or search-parser #'url:standard-parse-search-info)
        (search-writer url) (or search-writer #'url:standard-write-search-info)
        (header-function url) header-function)
  url)

(defun %export-url-searchable-object (url method args)
  (destructuring-bind (&key pathname response-function header-function search-parser search-writer search-database character-set
                            &allow-other-keys) args
    (cond (pathname
           (setf (translated-pathname  url) pathname))
          (t (error "No PATHNAME was provided while exporting the URL, ~S, with method, ~S"
                    url method)))
    (setf (character-set url) character-set)
    ;; initialize data caching
    (%export-url-init-data-cache url args)
    ;; standard search parameters.
    (%export-url-search url method response-function search-parser search-writer search-database header-function)
    url))


;;;------------------------------------------------------------------- 
;;;
;;; URL EXPORT METHODS
;;;

(define-generic unexport-url (url)
  (declare (values url unexported-translation-method))
  (:documentation "Unexports URL so that it can no longer be accessed via the server."))

(defmethod unexport-url ((url string))
  (unexport-url (url:intern-url url :if-does-not-exist :error)))

;; formerly used unintern-url, but now we just reset the URL translation. -- JCMa 5/20/1995.
(defmethod unexport-url ((url url:http-url))
  (let ((translation (translation-method url)))
    (setf (translation-method url) nil
          (character-set url) nil)
    (values url translation)))

(defmethod unexport-url :after ((url url:http-path))
  (setf (directory-writer url) nil))

(defmethod unexport-url :after ((url url::content-language-mixin))
  (setf (languages url) nil))

(define-generic export-url (url export-type &rest args)
  (declare (values url)))

(defmethod export-url (url export-type &rest args)
  (declare (ignore args))
  (error "~S is an unknown export-type for ~S." export-type url))

;; Reverts the class to the origin class whenever a url is re-exported as a
;; different export type/  1/11/97 -- JCMa.
(defmethod export-url :around ((url url:translation-method-mixin) export-type &rest args)
  (declare (ignore args))
  (with-slots (url:translation-method) url
    (when (and url:translation-method
               (not (eql export-type url:translation-method)))
      (url::revert-class url))
    (call-next-method)
    ;; HTTP should not be enabled automatically on the standard port just because an HTTP URL has been exported. -- 3/24/2006 JCMa
;    (case (protocol url)
;      (:http
;	(let ((port (url:host-port url)))
;	  (when (eq port *standard-http-port*)
;	    (ensure-http-protocol-on-port port)))))
    url))

(defmethod export-url :before ((url url:http-path) export-type &rest args)
  (declare (ignore export-type))
  (destructuring-bind (&key directory-writer &allow-other-keys) args
    (unless (or (null directory-writer) (good-response-function-p directory-writer))
      (error "DIRECTORY-WRITER, ~S, must be a defined function when exporting the URL, ~S."
             directory-writer url))
    (setf (directory-writer url) directory-writer)))

(defmethod export-url :before ((url property-list-mixin) export-type &rest args)
  (declare (ignore export-type))
  (destructuring-bind (&key property-list &allow-other-keys) args
    (loop for (key value) on property-list by #'cddr
          do (setf (get-value url key) value))))

;; These must be before methods or directory exports may not pass them down to inferiors.
(defmethod export-url :before ((url url::content-language-mixin) export-type &rest args)
  (declare (ignore export-type))
  (destructuring-bind (&key language &allow-other-keys) args
    (setf (languages url) language)))

(defmethod export-url :before ((url url:expiration-mixin) export-type &rest args)
  (declare (ignore export-type))
  (destructuring-bind (&key expiration &allow-other-keys) args
    (url:initialize-expiration url expiration)))

(defmethod export-url :before ((url url:secure-subnets-mixin) export-type &rest args)
  (declare (ignore export-type))
  ;; SECURE-SUBNETS is an obsolete keyword that is superseded by READ-SUBNETS 3/13/2000 -- JCMa.
  (destructuring-bind (&key read-subnets write-subnets secure-subnets &allow-other-keys) args
    (url:initialize-secure-subnets url (or read-subnets secure-subnets) write-subnets)))

(defmethod export-url :after ((url url:server-timeout-mixin) export-type &rest args)
  (declare (ignore export-type))
  (macrolet ((update-slot (url slot-name value accessor)
	       `(etypecase ,value
		  (null (slot-makunbound ,url ',slot-name))
		  (integer
		    (setf (,accessor ,url) ,value))
		  ((or function symbol)
		   (let ((val (funcall ,value ,url)))
		     (check-type val (integer 0))
		     (setf (,accessor ,url) val))))))
    (destructuring-bind (&key (life-time *server-life-time* life-time-supplied-p)
			      (timeout *server-timeout* timeout-supplied-p) &allow-other-keys) args
      (cond-every
	(life-time-supplied-p
	  (update-slot url url::life-time life-time server-life-time))
	(timeout-supplied-p
	  (update-slot url url::timeout timeout server-timeout)))
      url)))

(defmethod export-url :before ((url url:authentication-mixin) export-type &rest args)
  (declare (ignore export-type))
  (destructuring-bind (&key authentication-realm capabilities &allow-other-keys) args
    (url:initialize-authentication url authentication-realm capabilities)))

(defmethod export-url :before ((url url:http-cache-control-mixin) export-type &rest args)
  (declare (ignore export-type))
  (url:initialize-response-cache-control-directives url args))

(defmethod export-url :before ((url url:documentation-mixin) export-type &rest args)
  (declare (ignore export-type))
  (destructuring-bind (&key keywords documentation &allow-other-keys) args
    (url:initialize-documentation url keywords documentation)))

;; these methods keep searchable objects synchronized with their counterparts.
;; previous approach hung these off of register and unregister as after
;; methods, but that is hazardous if the URL name is ever recompuuted for some
;; reason. Morever, URL should reflect physical resources.  8/4/95 -- JCMa.
(defmethod export-url :after ((url url:http-searchable-object) export-type &rest args)
  (let ((trimmed-name (url::name-string-without-search-suffix url nil)))
    (apply #'export-url (url:intern-url trimmed-name :if-does-not-exist :create)
           ;; Composite maps like :IMAGE-MAP provide an :export-type
           ;; argument in the ARGS this must be used here in order to
           ;; ensure that correct export type is used to export the
           ;; plain, unsearchable object.
           (or (getf args :export-type) export-type)
           args)))

;; This caches the image size  using an after methods that runs for image
;; exports.  URL::GET-IMAGE-SIZE must be specialized for each  image export type
;; for this to have an effect.  Better performance of other URLs could be
;; achieved by specializing the URL object class or converting export-types
;; to objects and relying on inheritance. -- JCMa 12/31/1996.
(defun  %cache-image-size (url export-type &optional recompute-p)
  (when *image-sizes-default-automatically*
    (handler-case-if (not *debug-server*)
       (url:get-image-size url export-type recompute-p)
      (error (err) (bug-report-error err)))))

(defmethod export-url :after ((url http-object) (export-type (eql :gif-image)) &rest args)
  (declare (ignore args))
  (%cache-image-size url export-type t))

(defmethod export-url :after ((url http-object) (export-type (eql :jpeg-image)) &rest args)
  (declare (ignore args))
  (%cache-image-size url export-type t)) 

(defmethod unexport-url :before ((url url:http-searchable-object))
  (let ((trimmed-name (url::name-string-without-search-suffix url nil)))
    (unexport-url (url:intern-url trimmed-name :if-does-not-exist :error))))

(define-macro define-url-exports (&body body)
  "URLs are exported collectively with HTTP:DEFINE-URL-EXPORTS or individually
with HTTP:EXPORT-URL. HTTP:DEFINE-URL-EXPORTS is an interface to
HTTP:EXPORT-URL that eliminates the need to repeat calls to HTTP:EXPORT-URL.
Each entry is a list of arguments suitable for HTTP:EXPORT-URL."
  `(progn . ,(loop for (url method . args) in body
                   collect `(export-url ,url ',method ,@args))))

(defmethod  export-url ((url string) (method t) &rest args) 
  (apply #'export-url
         (url:intern-url url :if-does-not-exist :create)
         method args))

;;;------------------------------------------------------------------- 
;;;
;;; DOCUMENTATION
;;;

(setf (documentation 'export-url 'function)

      "HTTP:EXPORT-URL is the primary method that exports URLS to make them accessible via the http
server. URL is either a string or an interned URL to be exported.  EXPORT-TYPE is the method to use
in exporting URL.


I. Basic Export Types: These involve making the contents of a file accessible via a URL. These types
require URLs that are object (i.e., have a name and extension).

        :HTML-FILE (&key pathname)
        :TEXT-FILE (&key pathname)
        :XML-FILE (&key pathname)
        :CSS-FILE (&key pathname)
        :RTF-FILE (&key pathname)
        :LISP-FILE (&key pathname)

        :GIF-IMAGE (&key pathname)
        :JPEG-IMAGE (&key pathname)
        :X-BITMAP-IMAGE (&key pathname)
        :PICT-IMAGE (&key pathname)
        :TIFF-IMAGE (&key pathname)
        :PNG-IMAGE (&key pathname)
        :ICO-IMAGE (&key pathname)

        :BASIC-AUDIO (&key pathname)
        :AIFF-AUDIO (&key pathname)
        :WAV-AUDIO (&key pathname)
        :REAL-AUDIO (&key pathname)

        :MPEG-VIDEO (&key pathname)
        :QUICKTIME-VIDEO (&key pathname)

        :VRML-WORLD (&key pathname)
        :SHOCKWAVE-FILE (&key pathname)

        :DIRECTORY (&key pathname immediate-export recache recursive-p)
        :HTML-DIRECTORY (&key pathname immediate-export recache recursive-p)
        :TEXT-DIRECTORY (&key pathname immediate-export recache recursive-p)
        :LISP-DIRECTORY (&key pathname immediate-export recache recursive-p)
        :IMAGE-DIRECTORY (&key pathname immediate-export recache recursive-p)
        :AUDIO-DIRECTORY (&key pathname immediate-export recache recursive-p)
        :VIDEO-DIRECTORY (&key pathname immediate-export recache recursive-p)
        :WORLD-DIRECTORY (&key pathname immediate-export recache recursive-p)
        :APPLICATION-DIRECTORY (&key pathname immediate-export recache recursive-p)

:DIRECTORY exports all files whose resource type is known.  Others export a specific content type,
and ignore other file types.  When recursive-p is non-null, all subdirectories are also exported.
Otherwise, subdirectories are ignored.

When *AUTO-EXPORT* is non-null, new files are automatically exported when they are scoped by one of
these directory export types. Auto-export occurs on demand for the GET and HEAD methods. If
*AUTO-EXPORT* is :ON-DEMAND, files are exported when they are first requested rather than at the
time the directory is exported. When exporting a directory, a non-null argument to :IMMEDIATE-EXPORT
overrides lazy directory exporting. In general, on-demand directory exports trade faster server
start up for a slightly slower first access to a file URL within the directory. When :RECACHE is
non-null, a directory export updates the export parameters for every inferior. This parameter forces
traversal of the entire structure, like a non-null :IMMEDIATE-EXPORT would.

A directory-style list in HTML is the default content returned for the get method on a URL directory
path. This behavior is customized by providing a response function via the :DIRECTORY-WRITER
keyword.  This response function is called with the arguments (URL STREAM) and must return non-null
when it handles the response. If it declines to handle the response, it may return NIL, and the
standard method will list the directory as usual. HTTP:WRITE-INDEXED-DIRECTORY-LISTING is a
directory writer that will serve the contents of an index.html file found in the directory. Other
computed returns are possible.

Note that the presence in file or directory names of escaped characters (see *ESCAPED-CHARACTERS*)
will lead to inconsistent results, and possibly errors. Space and question mark are examples.

        :PDF-FILE  (&key pathname)
        :POSTSCRIPT-FILE (&key pathname)

        :BINHEX-FILE (&key pathname)
        :STUFFIT-FILE  (&key pathname)
        :COMPRESSED-FILE (&key pathname)
        :MAC-BINARY-FILE (&key pathname)

        :WORD-FILE  (&key pathname)
        :POWER-POINT-FILE (&key pathname)
        :EXCEL-FILE  (&key pathname)

The Java language provides applets that execute on the client.  This kind of mobile code is
supported with the following export types and the HMTL generation macro WITH-JAVA-APPLET.
        
        :JAVA-FILE (&key pathname)
        :JAVA-BINARY (&key pathname)

:JAVA-FILE exports the source code whereas :JAVA-BINARY provides the byte compiled binary to the
client running the applet.

Various scripting languages can be exported for use in browsers

        :JAVA-SCRIPT-FILE (&key pathname)
        :TCL-FILE (&key pathname)
        :VISUAL-BASIC-SCRIPT-FILE (&key pathname)

:JAVA-SCRIPT-FILE exports the browser scripting language, JavaScript, which is simpler, and easier
to use than Java itself. :TCL-FILE export the popular TCL user interface language while
:VISUAL-BASIC-SCRIPT-FILE exports a MicroSoft Visual Basic Script. Such scripts can be emitted in
HTML 4.0.1 as client-side mobile code using the WITH-SCRIPT macro.

The following export types support inline plug-ins on the client side.  Plug-ins can be referenced
using NS2.0:WITH-EMBEDDED-SCRIPT (deprecated).

        Inline speech synthesis is available using a macintalk plug-in
        from http://www.mvpsolutions.com/PlugInSite/Talker.html

        :TALK-AUDIO (&key pathname)

II. Redirect Export Types: These export types inform the client to look elsewhere for a URL.  They
work for all operations, including GET and HEAD.  The exported URL can be either an HTTP object or
an HTTP path. ALTERNATE-URLS is a list of one or more target URLs where the resource may be
obtained. When more than one target URL is provided, the client is free to choose among them.

        :REDIRECT (&key alternate-urls)
        :TEMPORARY-REDIRECT (&key alternate-urls)

Alternatively, a computed response may call REDIRECT-REQUEST to issue a redirect rather than serving
content itself.

III. Search Export Types: these involve performing searches using the search index or map search
facilities in HTTP. Search URLs must end with ? so that the system can composed the right
combination of classes. In all cases, the response function must compute the answer and return HTML
over the http stream to the client.

General Purpose Searches

        :SEARCH (&key response-function search-parser search-database)

        This exports a URL that performs searches by calling RESPONSE-FUNCTION with the arguments
        URL and STREAM. The search database and search parameters are cached on the URL and
        accessible via URL:SEARCH-DATABASE and URL:SEARCH-KEYS.

        The optional export argument SEARCH-PARSER is the parser that obtains URL:SEARCH-KEYS from
        the suffix of a search URL. Several parsers are predefined:

                URL:STANDARD-PARSE-SEARCH-INFO: This standard parser for search URLs. It tests
                whether the search info encodes form or list data and calls the appropriate of the
                next two parsers.

                URL:PARSE-SEARCH-INFO: This normal parser for search URLs produces a list of search
                parameters using + as the delimiter.

                URL:PARSE-SEARCH-INFO-AS-QUERY-ALIST: This parser for URL encoded form values
                returns an alist just like posting a form would. This parser should be used when an
                HTML form is returned to a search URL.  However, this method of returning form
                values is limited to 1024 characters total in the URL, and therefore, it's use in
                new code is deprecated.

                URL:PARSE-SEARCH-INFO-AS-XML-SAFE-QUERY-ALIST: This parser is recommend for HTML 4.0
                and XHTML documents becauase it uses semi-colon to delimit form fields, thus
                avoiding interactions with SGML's use of ampersand.

        Users may provide other specialized parsers. They should accept the arguments (url-string
        start end) and need not located the ?  suffix delimiter.

        The export argument, SEARCH-WRITER, allows a URL to specialize how the parsed presentation
        on URL:SEARCH-KEYS is written. Several writers are predefined.

                URL:STANDARD-WRITE-SEARCH-INFO: This standard writer for search URLs. It tests
                whether the search info encodes form or list data and calls the appropriate of the
                next two writers.

                URL:WRITE-SEARCH-INFO: This normal writer for search URLs produces a list of search
                parameters using + as the delimiter.

                URL:WRITE-SEARCH-INFO-AS-QUERY-ALIST: This writer for URL encoded form values that
                prints alist values as name value pairs using the urlencoding syntax.

                URL:WRITE-SEARCH-INFO-AS-XML-SAFE-QUERY-ALIST: This writer is recommend for HTML 4.0
                and XHTML documents becauase it uses semi-colon to delimit form fields, thus
                avoiding interactions with SGML's use of ampersand.

        The export argument, HEADER-FUNCTION, allows a search URL to specialize how it responds to
        the HEAD method based on runtime knowledge.  HEADER-FUNCTION is called with the argument URL
        and returns values used to determine respoonse to HEAD. The returned values are:
        (CONTENT-TYPE CONTENT-LENGTH LAST-MODIFICATION VERSION CHARSET PUBLIC ALLOW)
        
                CONTENT-TYPE (required) is a keyword or content-type. Default is HTML.

                CONTENT-LENGTH (optional) is the length in bytes of the entity body. Default is
                none.

                LAST-MODIFICATION (optional) is a universal time indicating when the entity was last
                changed. Default is now.
 
                VERSION (optional) is a number or string that distinguishes different versions of
                the entity. Default is none.

                CHARSET (optional) is a keyword indicating the character set of the entity. Default
                is :ISO-8859-1.

                PUBLIC (optional) is a boolean indicating whether the resource is available to the
                public. Default is none.

                ALLOW (optional) is a list of HTTP method keywords available on the resource.
                Default is none.

Image Searches

   Image Maps

        :IMAGE-MAP (&key pathname export-type map-format map-pathname
                         search-parser search-writer header-function)

        This exports the image in PATHNAME as IMAGE-EXPORT-TYPE and establishes a response function
        based on the image map in MAP-PATHNAME whose MAP-FORMAT is either :CERN or :NCSA.
        EXPORT-TYPE is the appropriate image search export type (see below).

   Direct Searches

        These provide direct control over the response function for image searches.

        :GIF-IMAGE (&key pathname response-function search-database
                         search-parser search-writer header-function)
        :JPEG-IMAGE (&key pathname response-function search-database
                          search-parser search-writer header-function)
        :PNG-IMAGE (&key pathname response-function search-database
                         search-parser search-writer header-function)
        :X-BITMAP-IMAGE (&key pathname response-function search-database
                              search-parser search-writer header-function)
        :PICT-IMAGE (&key pathname response-function search-database)
                          search-parser search-writer header-function)
        :TIFF-IMAGE (&key pathname response-function search-database
                          search-parser search-writer header-function)

        These export types allow the client's user to click on images and invoke a response from the
        server.  These URL are both objects and searchable.  When they are requested without the ?
        suffix, the contents of their associate image file is returned.  When the ? suffix appears,
        their RESPONSE-FUNCTION is called on the arguments URL and STREAM. See the macro
        HTTP:WITH-IMAGE-COORDINATES automatically binds the X and Y coordinates.

IV. Computed Export Types: These compute responses returned to clients.

        :COMPUTED (&key response-function header-function) RESPONSE-FUNCTION is called with the
        arguments URL and STREAM and is responsible for returning HTML to the client. :COMPUTED has
        an optional pathname so that the computation may reference a file, if necessary.
        HEADER-FUNCTION is documented section III.
        
        :HTML-FORM (&key response-function pathname durable-form-values server) :HTML-FORM returns
        the contents of PATHNAME when it is requested via GET.  When there is a POST, its
        RESPONSE-FUNCTION is called with URL, STREAM, and FORM-ALIST.  FORM-ALIST is an alist of
        (QUERY RAW-VALUE) for all the queries in the form that the client returns. QUERY is a
        keyword.  When a client returns multiple raw-values for a QUERY, these are aggregated into a
        list of the values associated with the query in a single, order-preserving entry.
        DURABLE-FORM-VALUES is a boolean and controls whether query values are new, copied strings
        or indirect arrays pointing into a volitile form data buffer. High-volume applications or
        forms large query values can gain efficiency by using durable form values, all operations on
	these values must be completed within the context of the form's response function. When the
        form uses the MIME multipart encoding, the MAXIMUM-UPLOAD-FILE-SIZE may supply the maximum
        allowable file size, if it diverges from the default HTTP:*FILE-UPLOAD-MAXIMUM-SIZE*.

        :COMPUTED-FORM (&key form-function response-function header-functiondurable-form-values
        server) :COMPUTED-FORM is a cross between :COMPUTED and :HTML-FORM that provides
        FORM-FUNCTION to generate html just like :COMPUTED and RESPONSE-FUNCTION to handle the post
        method when form values are returned. FORM-FUNCTION is called with the arguments URL and
        STREAM and is responsible for returning HTML to the client. RESPONSE-FUNCTION takes the same
        arguments as :HTML-FORM.

        :SCRIPT (&key script header-function) Script is a client-side script defined with
        HTML:DEFINE-SCRIPT. These scripts may merely deliver a static copy of the client-side
        script, or they may perform a computation that emits an appropriate script.

        :SHTML-FILE (&key pathname header-function) This is a computed response that is inserted in
        a static HTML file containing server-parsed HTML. When an SHTML element is found by the
        server, it inserts the result of a computation in place of the SHTML element.  SHTML
        elements are delimted by <!--# and --> and look like:

                <!--#include file=\"insert.text\"-->

        INCLUDE is an SHTML operation that requires a FILE. For security reasons, FILE must be in
        the same directory as the STHML file. Access may be controlled by providing the optional
        SUBNETS parameter, which is a comma-separated string of IP addresses without whitespace.

         <!--#include file=\"insert.text\" subnets=\"128.52.0.0,18.21.0.0\"-->
        
        EVAL is a more general SHTML operation that requires an ACTION parameter. ACTION is a string
        denoting an SHTML action. Specific parameters may be required by individual actions. Here,
        DATE-TIME is the operation and FORMAT is a parameter.

              <!--#eval action=\"date-time\" format=\"iso\"-->

        Predefined actions are documented by HTTP:SHOW-SHTML-ACTIONS. New SHTML actions are defined
        with HTTP:DEFINE-SHTML-ACTION. Files with the extention SHTML are autoexported by HTML
        directory export types.


V. Reverse Proxy Export Type

This provides a reverse proxying facility for servers, the reverse of normal client proxying. In a
reverse proxy, when a client requests a specific URL, the server proxies the request to an origin
server and returns the resulting HTTP message to the client. Thus, the reverse proxy behaves as if
it was the origin server.

To perform this task, the reverse proxy needs to know the origin server to which a particular
incoming client HTTP request is mapped. This information is provided at URL export time by the
application. Although all requests on a particular port may be reverse proxied, this facility allows
different segments of the URL space to be delegated to different origin servers according to the
local URL provided to EXPORT-URL. Applications may choose to either run with local proxy caching or
not, depending on the value of *PROXY-CACHING-P*. Normally, local proxy caching will improve
performance.  Note that this facility is only available when the proxy server is loaded.

        :REVERSE-PROXY (&key proxy-forwardings) proxy-forwardings is a list of (DOMAIN-NAME PORT
        PROTOCOL). If more than one host is provided, forwarding distributed over the set of origin
        servers. Only the most general URL needs to be exported because the facility automatically
        forwards requests to inferior URLs. Although the reverse proxy enforces all security
        parameters on the root reverse-proxy URL, all other issues concerning the URL are delegated
        to the origin server, for example, method availability and URL existence. When desired, a
	proxy may be placed between the reverse proxy and the origin server using
	DEFINE-CLIENT-PROXY-MAPPING.

To handle remapping internal url contexts to external contexts, see URL:*RELATIVIZE-URLS* and
DEFINE-URL-CONTEXT-REMAPPINGS.

VI. Export Parameters and Issues

1. Portable Exports: The #u reader macro merges a partial URL specification against the default for
the local host. Use this when you be able to load the same exports file on different hosts. The #u
reader macro has an extended syntax that allows you to overview the default host and port specified
by the server configuration. The syntax is

     #u(url-string :host host-string :port port-number)

URL-STRING is a relative URI. HOST-STRING is the domain name or IP string for the host. PORT-NUMBER
is an integer.


2. Security: 

A. Subnet Security: Secure subnets are a list of IP addresses, where 0 is a wildcard. For example,
128.52.0.0 matches all the subnets at the AI lab.  The following export arguments allow control over
which clients can perform HTTP methods such as GET, HEAD, POST, :OPTIONS, or :TRACE (read access)
versus PUT or DELETE (write access).

        :READ-SUBNETS allows read access to be specified at the level of URLs as they are exported.

        :WRITE-SUBNETS allows write access to be specified at the level of URLs as they are
        exported.

        DEFINE-READ-SUBNETS restricts read access globally to the server.

        DEFINE-WRITE-SUBNETS restricts write access globally to the server.

Write access presumes read access, and consequently, IP addresses from the write subnets need not be
included in the read subnets. To select the global authentication policy for write methods,
HTTP:*ACCEPT-WRITE-METHODS*.

DEFINE-SUBNET can be used to specify named subnet for use in subnet specifications. Note that named
subnets are resolved when they are used, for example by a call to EXPORT-URL, and therefore, changes
to named subnets require re-export of URL referring to them.

B. User Authentication: URL authentication can be specified using the following export arguments.

        :AUTHENTICATION-REALM a realm obtainable via INTERN-REALM. These can be created with
        ADD-REALM.

        :CAPABILITIES a URL access control obtainable via INTERN-ACCESS-CONTROL. These can be
        created with ADD-ACCESS-CONTROL-GROUP.

See also: ADD-GROUP, ADD-USER, and SAVE-AUTHENTICATION-DATA.

C. Connection Timeouts: URL-specific timeouts can override the global timeouts controlling when an
HTTP connection can be killed

        :LIFE-TIME is the number of milliseconds that an HTTP connection can live before it is
        scavengeable.  The gloabl default is *SERVER-LIFE-TIME*.

        :TIMEOUT is 60ths of a second and controls the amount idle time before an HTTP connection is
        scavengeable. The gloabl default is *SERVER-TIMEOUT*.

These value supplied for :LIFE-TIME or :TIMEOUT must be an integer, null or a function.  An integer
is interpreted as timeout value in the appropriate units.  When the the value is null, the field is
is cleared on the URL and the global default applies. If the value is a function, the function is
called with the argument (URL) and must return an integer.

3. Expiration: The expiration time for a url is issued as an EXPIRES header so that proxy servers
can determine when they need to refresh their cache.

Expiration is controlled by providing the :EXPIRATION when exporting any URL.  If expiration is not
provided, the default is no expiration date.

The :EXPIRATION keyword takes one argument, which is either keyword or a list of (keyword &rest
arguments).

     Arguments                       Meaning

 :NO-EXPIRATION-HEADER        --  No EXPIREs header is issued.
 :NEVER                       --  EXPIRES header indicates one year from now.
 (:INTERVAL <universal-time>) --  EXPIRES header indicates now + <universal-time>.
 (:TIME <universal-time>)     --  EXPIRES header indicates an <universal-time>.
 (:FUNCTION <function-spec>)  --  EXPIRES header indicates universal time computed by
                                  applying <function-spec> to URL.  <function-spec>
                                  should return a universal for use in the EXPIRES header
                                  or NIL, in which case no EXPIRES header is issued.

4. Character Sets: The :CHARACTER-SET keyword allows any URLs whose content type is TEXT (e.g.,
text/plain, text/html) to be exported with character sets other than the HTTP default :ISO-8859-1,
or subsets. The valid character sets for HTTP are: :US-ASCII, :ISO-8859-1, :ISO-8859-2, :ISO-8859-3,
:ISO-8859-4, :ISO-8859-5, :ISO-8859-6, :ISO-8859-7, :ISO-8859-8, :ISO-8859-9, :ISO-2022-JP,
:ISO-2022-JP, :ISO-2022-KR, :UNICODE-1-1, :UNICODE-2-2-UTF-7, :UNICODE-2-2-UTF-7, :utf-7 :utf-8
:utf-16 :utf-32.  (Additional IANA character sets may be added for DEFINE-MIME-CONTENT-TYPE of the
media type TEXT.) Whenever TEXT content types use a character set other than :ISO-8859-1, the HTTP
requires explicit specification via this export keyword.

6. Languages: The :LANGUAGE keyword may be supplied for any exported URL. The value is a sequence of
keywords denoting the language(s) in which the resource is written. HTTP 1.1 defines these keywords
in section 3.10 to conform with RFC 1766. They can be a two-letter ISO 639 language abbreviation,
optionally with a two-letter ISO 3166 country code as a subtag.

7. Documentation: Keywords and a descriptive string can be attached to URLs at export time.  For
directory exports, note that these arguments apply to ALL URLs exported during the directory walk.

        :KEYWORDS                  A list of keywords.
        :DOCUMENTATION             A string describing the URL.

8. Virtual Hosts: HTTP 1.1 requires a virtual hosting facility, which this server implements. You
can define a virtual host (or vanity name) that will be served by the physical server from the same
IP address. Any URIs served by a virtual host must be exported by specifying the absolute URI,
including host and port number.  The #u reader macro may be useful here.  The following operators
are available for use with virtual hosts:

        ADD-VIRTUAL-HOST: Adds a virtual host on a virtual port and and makes URLs served by that
                          host available to HTTP 1.1 or greater clients.

        REMOVE-VIRTUAL-HOST: Makes the virtual host unavailable, but does not remove any URLs it
                             exports.

9. HTTP 1.1 Cache Control: The keywords below may be supplied when exporting any URL in order to
statically control how downstream proxies and caches handle the content associated with a URL.

        :PUBLIC -- If the value is T, the entire message is cachable by any cache even if it would
        not normally be cached.

        :PRIVATE -- If the value is T, the entire message is intended for a single user and must not
        be cached by a shared cache. If the value is a list of keywords denoting specific header,
        then only these headers should be considered private.

        :NO-CACHE -- If the value is T, the entire message must not be cached by any cache along the
        request chain.  If the value is a list of keywords denoting specific headers, then only
        these headers should be discarded before caching.

        :NO-STORE -- If the value is T, the entire message must not be stored in any non-volatile
        storage by any cache along the request chain.

        :MAX-AGE -- The value is the number of seconds for which the response is valid, after which
        it should be revalidated.  This directive overrides the expiration header, thus allowing
        HTTP 1.1 server to provide a longer expiration time to HTTP 1.1 servers and proxies.  This
        defaults to a value derived from the expiration time.

        :MUST-REVALIDATE -- If the value is T, caches and proxies must not serve the resource
        without revalidating it once it becomes stale, even if configured to operate with state
        data. Servers should send this directive if and only if failure to revalidate will result in
        incorrect operation. Recipients must not take any automated action that violates this
        directive.

        :PROXY-REVALIDATE -- If the value is T, this directive is the same as :MUST-REVALIDATE
        except that it applies only to proxies and not non-shared user caches. It can be used on
        response to an authenticated request to permit the user's cache to store and later return
        the response without needing to revalidate it.

        :NO-TRANSFORM -- If the value is T, caches and proxies must not change the body of the
        message or change headers describing the content.

10. File Data Caching: When certain files are accessed with exceptionally high frequency, caching them in
memory improves performance and reduces disk contention. This assumes sufficient physical memory is
available or operating system paging algorithms perform better than simple disk access. Both of these
assumption are often true. This caching approach is relevant when your operating system does not already
provide in-memory caching for the file system (many do) or fine-grain control is desired.

When exporting any URL with static content on disk (any export type requiring a :PATHNAME argument), the
following arguments work for both objects and directories, wether export is immediate or deferred.

        :CACHE-FILE-DATA -- Controls whether file data is cached in memory or not. The value is either a
        DATA-UNIVERSE, a STRING naming a data universe, or T (which uses the default
        *url-file-data-universe-name*). If the value is changed to NIL during a subsequent EXPORT-URL,
        Dfile data caching will be turned off for the URL, or the URL structure below in the case of
        directories.

        :FILE-DATA-REVALIDATION -- The number of seconds between checks of the data on disk to see if the
        original data has changed.

        :FILE-DATA-WIRED -- Prevents file data from being dropping from the in-memory when its access
        frequency falls and in-memory cache space is needed for other more frequently accessed data.

If file names are exported under other URLs, turning file data caching off for one URL will turn it off
for any other URLs servicing the same physical files.

See DEFINE-CACHED-DATA for an alternative interface for directly specifying files data to cache in
memory. *DATA-UNIVERSE-MAXIMUM-CACHE-SIZE* controls the maximum size of data caches.
*DATA-UNIVERSE-MAXIMUM-CACHE-ELEMENTS* controls the maximum number of items retained in the cache.
Facilities are also available for auditing the runtime behavior of data caches to support tuning the
performance of applications.

Advanced users can define their own data universes and develop cache management policies appropriate for
their applications, or develop specialized data universe classes for handling dynamic application data.

11. Property List: The PROPERTY-LIST keyword adds a property list of alternating keywords and value
to a URL.  These properties can be read and set via GET-VALUE.

12. New static export types for data stored in files can be defined with DEFINE-URL-EXPORT-TYPE.")


;;;------------------------------------------------------------------- 
;;;
;;; EXPORT TYPES
;;;

(eval-when (:compile-toplevel :execute :load-toplevel)

  (define-variable *pathname-extension-export-type-alist* nil
                   "An alist mapping pathname types to export-types.")

  (tk1:define-tokenizer exported-pathname-extension
                      :tokenizer 'intern-keyword
                      :test 'char-equal
		      :size 110
                      :definer define
                      :documentation "Tokenizes pathname types exported by CL-HTTP.")

  (define export-type-for-pathname-type (keyword-or-string &optional (error-p t))
    "Returns the export type for the pathname extension, KEYWORD."
    (let* ((ext (etypecase keyword-or-string
                  (keyword keyword-or-string)
                  (string (tokenize-exported-pathname-extension keyword-or-string 0 (length keyword-or-string) (if error-p :error :soft)))
                  (null nil)))
           (primary (and ext (http:primary-pathname-extension ext error-p))))
      (cond ((and primary (cdr (assoc primary *pathname-extension-export-type-alist* :test #'eq))))
            (error-p (error "~S is not a pathname type with a known export type." keyword-or-string))
            (t nil))))

  (defun set-export-type-for-pathname-type (keyword export-type)
    (check-type keyword keyword)
    (check-type export-type keyword)
    (let ((entry (assoc keyword *pathname-extension-export-type-alist* :test #'eq)))
      (tokenize-exported-pathname-extension (symbol-name keyword)) ;track pathname extensions
      (cond (entry (setf (cdr entry) export-type))
            (t (setq *pathname-extension-export-type-alist* 
                     (nconc *pathname-extension-export-type-alist* `((,keyword . ,export-type))))))
      *pathname-extension-export-type-alist*))

  (defsetf export-type-for-pathname-type set-export-type-for-pathname-type) 

;; depends on the primary extension appearing first in *pathname-extension-export-type-alist*
;; a sorter should ensure that this is always true. -- JCMa 5/6/1995.
  (define primary-pathname-extension-for-export-type (export-type &optional (error-p t))
    "Returns the primary pathname extension for EXPORT-TYPE
Signals an error if EXPORT-TYPE does not retrieve data from the file system."
    (cond ((car (rassoc export-type *pathname-extension-export-type-alist*  :test #'eq)))
          (error-p (error "~S is not an export type with associated pathanme extensions." export-type))
          (t nil)))

  (define primary-pathname-extension-string (export-type)
    (or (get export-type 'primary-extension-string)
        (setf (get export-type 'primary-extension-string) 
              (string-downcase (symbol-name (primary-pathname-extension-for-export-type export-type))))))

  (define primary-pathname-extension (extension &optional (error-p t))
    "Returns a keyword that is the primary pathname extension for EXTENSION."
    (let ((ext (etypecase extension
                 (keyword extension)
                 (string (tokenize-exported-pathname-extension extension 0 (length extension) :soft))
                 (null extension))))
    (cond ((and ext (get ext 'primary-extension)))
          (error-p (error "~S is not a known pathname extension." extension))
          (t nil))))

  (defun %set-primary-pathname-extension (extension primary-extension)
    (setf (get extension 'primary-extension) primary-extension))

  (defsetf primary-pathname-extension %set-primary-pathname-extension)


  )                                             ; close eval-when

(defmethod  export-url ((url url:http-path) (translation (eql :html-file)) &rest args)
  (%export-url-path url translation args))

(defmethod  export-url ((url url:http-path) (translation (eql :text-file)) &rest args)
  (%export-url-path url translation args))

;;;------------------------------------------------------------------- 
;;;
;;;  DEFINITION CODE FOR STATIC FILE TYPES
;;;

(define note-pathname-extension-export-type (extension export-type data-type)
  "Notes that EXTENSION should be exported using EXPORT-TYPE.
DATA-TYPE is the class of mime-type. See DEFINE-EXPORT-TYPES for info."
  (check-type extension keyword)
  (check-type export-type keyword)
  (check-type data-type keyword)
  (unless (data-type-keyword-p data-type)
    (error "~S is not one of the known data types, ~:{~S~:^, ~}." 
           data-type URL::*DATA-TYPE-PATHNAME-PREDICATE-ALIST*))
  (let* ((primary-extension (primary-pathname-extension-for-export-type export-type))
         (copy-mode (url::%content-type-copy-mode primary-extension)))
    (setf (export-type-for-pathname-type extension) export-type)
    (setf (primary-pathname-extension extension) primary-extension)
    (url:note-pathname-extension-type extension data-type copy-mode)
    extension))

(eval-when (:compile-toplevel :execute :load-toplevel)
(defun %define-url-export-type (export-type extension mime-content-type
					    &optional (copy-mode :text)
					    (url-class 'url:http-object)
					    alternate-extensions data-type
					    no-methods)
  (flet ((charset-arg (content-type)
	   (case (first content-type)
	     (:text `((character-set url)))
	     (t nil))))
    (unless data-type
      (setq data-type (first mime-content-type)))
    `(progn
       ;; set up mappings between mime, extension, and export type.
       (setf (%mime-content-type-spec ,extension) ',mime-content-type)
       (setf (export-type-for-pathname-type ,extension ) ,export-type)
       (setf (primary-pathname-extension ,extension) ,extension)
       (url:note-pathname-extension-type ,extension ,data-type ,copy-mode)
       (url::%note-content-type-copy-mode ,extension ,copy-mode)
       ,.(when alternate-extensions
	   `((dolist (ext ',alternate-extensions)
	       (setf (export-type-for-pathname-type ext ) ',export-type)
	       (setf (primary-pathname-extension ext) ,extension)
	       (url:note-pathname-extension-type ext ,data-type ,copy-mode))))
       ;; write headers
       ,(unless (member :header no-methods)
	  `(defmethod write-document-headers ((url ,url-class) (translation (eql ,export-type)) stream)
	     ,(ecase copy-mode
		((:text :binary)
		 `(%write-document-headers url ,extension stream ,@(charset-arg mime-content-type)))
                                       
		(:crlf
		  `(%write-crlf-headers url ,extension stream ,@(charset-arg mime-content-type))))))
       ;; write body
       ,(unless (member :document no-methods)
	  `(defmethod write-document ((url ,url-class) (translation (eql ,export-type)) stream)
	     ,(ecase copy-mode
		(:text 
		  `(%write-document url ,extension stream ,@(charset-arg mime-content-type)))
		(:binary
		  `(%write-binary-file url ,extension stream ,@(charset-arg mime-content-type)))
		(:crlf
		  `(%write-crlf-file url ,extension stream ,@(charset-arg mime-content-type))))))
       ;; define searchable images
       ,.(case data-type
	   (:image
	     `((defmethod export-url ((url url:http-searchable-object) (translation (eql ,export-type)) &rest args)
		 (%export-url-searchable-object url translation args)))))
       ;; define the export method
       ,(unless (member :export no-methods)
	  `(defmethod export-url ((url ,url-class) (translation (eql ,export-type)) &rest args)
	     (%export-url-object url translation args))))))

(define-macro define-url-export-type (export-type file-extension mime-content-type
						  &key (copy-mode :text)
						  alternate-extensions  data-type 
						  (url-class 'url:http-object) no-methods)
  "Top-level method for defining a new EXPORT-TYPE for a static, file-located data source.
FILE-EXTENSION is a keyword denoting the primary extension used  for the pathname type.
DATA-TYPE is a keyword that describes the type of resource.  The default is to use the
major mime type, but some cases will want to specialize it.  It can be any of :TEXT, :LISP,
:HTML ,:IMAGE, :AUDIO, :VIDEO, :WORLD, or :APPLICATION. 
MIME-CONTENT-TYPE is a keyword list of (major-type subtype).
COPY-MODE is either :TEXT, :BINARY, or :CRLF.
ALTERNATE-EXTENSIONS is an optional list of keywords denoting alternative extensions used for
the pathname type.
Automatic definition of methods can be suppessed with NO-METHODS, which is a list 
and accepts values :EXPORT, :HEADER, :DOCUMENT."
  (%define-url-export-type export-type file-extension mime-content-type
			   copy-mode url-class 
			   alternate-extensions
			   data-type no-methods))

(defmacro define-url-export-types (&rest export-type-specs)
  "Expands into a series of calls to DEFINE-URL-EXPORT-TYPE."
  `(progn
     ,.(loop for entry in export-type-specs
	     nconc (destructuring-bind (export-type file-extension mime-content-type
						    &key (copy-mode :text)
						    alternate-extensions data-type
						    (url-class 'url:http-object)
						    no-methods)
		       entry
		     (rest 
		       (%define-url-export-type
			 export-type file-extension
			 mime-content-type
			 copy-mode url-class 
			 alternate-extensions
			 data-type
			 no-methods))))))

  )                                             ; close eval-when 

;;;------------------------------------------------------------------- 
;;;
;;;  GENERIC DIRECTORY EXPORT FACILITY
;;;

(defmethod pathname-export-type ((pathname pathname) &optional (error-p t))
  (let ((type (pathname-type pathname)))
    (export-type-for-pathname-type type error-p)))

(defun %export-pathname (pathname url-string &optional export-args recache-p) 
  (let* ((export-type (pathname-export-type pathname))
         (extension (primary-pathname-extension-string export-type))
         (name-string (string-append url-string
                                     (pathname-external-name-string (pathname-name pathname))
                                     "."
                                     extension)))
    (multiple-value-bind (url newly-created-p)
        (url:intern-url name-string :if-does-not-exist :create)
      (when (or newly-created-p                 ; new
                recache-p                       ; recaching forced
                (getf export-args :recache)     ; recaching forced
                (null (translation-method url)))        ; no translation, so export
        (let ((local-args `(:pathname ,pathname ,.export-args)))
          (declare (dynamic-extent local-args))
          (apply #'export-url url export-type local-args))))))

(define-generic export-pathname-as-url-inferior (pathname url &optional export-args recache-p)
  (:documentation "Exports pathname as an inferior of URL using EXPORT-ARGS, a plist of export parameters.
URL must be a path. RECACHE-P forces rexexport of already exported URLS."))

(defmethod export-pathname-as-url-inferior ((pathname pathname) (url url:http-path) &optional export-args recache-p)
  (let* ((export-type (pathname-export-type pathname))
         (extension (primary-pathname-extension-string export-type)))
    (multiple-value-bind (url newly-created-p)
        (url:intern-pathname-as-url-inferior pathname url :extension extension :if-does-not-exist :create)
      (when (or newly-created-p                 ; new
                recache-p                       ; recaching forced
                (getf export-args :recache)     ; recaching forced
                (null (translation-method url)))        ; no translation, so export
        (let ((local-args `(:pathname ,pathname ,.export-args)))
          (declare (dynamic-extent local-args))
          (apply #'export-url url export-type local-args))))))

(defmethod export-pathname-as-url-inferior :before ((pathname pathname) (url url:http-object) 
                                                    &optional export-args recache-p)
  (declare (ignore export-args recache-p))
  (error "This operation is not defined for url objects.")) 

(define-generic unexport-pathname-as-url-inferior (pathname url &optional directory-p)
  (:documentation "Unexports pathname as an inferior of URL undoing EXPORT-PATHNAME-AS-URL-INFERIOR.
If pathname is known to be a directory, DIRECTORY-P should be non-null."))

(defmethod unexport-pathname-as-url-inferior ((pathname pathname) (url url:http-path) 
                                              &optional (directory-p  nil directory-p-supplied-p))
  (let ((inf-url (intern-pathname-as-url-inferior pathname url 
                                                  :directory-p (if directory-p-supplied-p 
                                                                   directory-p
                                                                   (pathname-directory-p pathname))
                                                  :if-does-not-exist :soft)))
    (when inf-url
      (unexport-url inf-url))))

(define-generic export-pathname (pathname &key protocol host-name port export-args)
  (:documentation "Exports PATHNAME guessing export types but using EXPORT-ARGS.
HOST-NAME and PORT overide the defaults of the local host and port 80."))

(defmethod export-pathname ((pathname pathname) &key (protocol *standard-protocol*) host-name port export-args (directory-export-type :directory)
                            &aux url-string export-type)
  ;; come up with the export type and url-string
  (cond ((www-utils:pathname-directory-p pathname)
         (setq export-type directory-export-type
               url-string (url:make-url-string-from-pathname protocol pathname t (or host-name (local-host-domain-name)) port)))
        (t  (setq export-type (pathname-export-type pathname)
                  url-string (url:make-url-string-from-pathname protocol pathname nil (or host-name (local-host-domain-name)) port
                                                                (primary-pathname-extension-string export-type)))))
  ;; intern and decide whether to export or not
  (multiple-value-bind (url newly-created-p)
      (url:intern-url url-string :if-does-not-exist :create)
    (when (or newly-created-p                   ; new
              (getf export-args :recache)       ; recaching forced
              (null (translation-method url)))  ; no translation, so export
      (let ((local-args `(:pathname ,pathname ,.export-args)))
        (declare (dynamic-extent local-args))
        (apply #'export-url url export-type local-args))))) 

(define-generic pathname-primary-extension (pathname &optional error-p)
  (:documentation "Returns the primary extension for PATHNAME."))

(defmethod pathname-primary-extension ((pathname pathname) &optional (error-p t))
  (let ((type (pathname-type pathname)))
    (primary-pathname-extension type error-p)) )

(define-generic directory-exportable-p (directory)
  (:documentation "Returns non-null if DIRECTORY is valid for export.
Specialize this method to prevent certain directories from ever being exported."))

;; suppress .svn, and similar UNIX directories
(defmethod directory-exportable-p (pathname)
  (flet ((pathname-dot-directory-p (pathname)
           (let ((dir-name (car (last (pathname-directory pathname)))))
             (or (null dir-name)
                 (null-string-p dir-name)
                 (eql #\. (char dir-name 0))))))
    (declare (inline pathname-dot-directory-p))
    (not (pathname-dot-directory-p pathname))))

(define-generic file-exportable-p (file)
  (:documentation "Returns non-null if FILE is valid for export.
Specialize this method to prevent certain files from ever being exported."))

;; suppress random files like .emacs etc sometimes found in unix
(defmethod file-exportable-p (pathname)
  (flet ((pathname-dot-file-p (pathname)
           (let ((name (pathname-name pathname)))
             (or (null name)
                 (null-string-p name)
                 (eql #\. (char name 0))))))
    (declare (inline pathname-dot-file-p))
    (not (pathname-dot-file-p pathname))))

;;;------------------------------------------------------------------- 
;;;
;;;  WRITE DIRECTORY LISTINGS
;;;

(defun %write-directory-listing-html2 (url stream inclusion-predicate path-url-intern-function anchor-function title-function 
                                           &optional directories-p (enumeration-type :definition) (creation-date-p t))
  #+Genera(declare (sys:downward-funarg inclusion-predicate path-url-intern-function anchor-function title-function))
  (labels ((write-item (path plist directory-file-p stream)
             (multiple-value-bind (url-inf newly-created-p)
                 (funcall path-url-intern-function path url)
               (when url-inf
                 (let ((anchor-text (funcall anchor-function url-inf path directory-file-p)))
                   (declare (dynamic-extent anchor-text))
                   (when anchor-text
                     (when newly-created-p
                       ;; ensure that it has been exported.
                       (let ((export-type  (typecase url-inf
                                             (http-path :directory-hierarchy)   ;export directory paths correctly
                                             (t (pathname-export-type path nil)))))
                         (when export-type
                           (export-url url-inf export-type :pathname path)
                           (inherit-export-parameters url-inf url))))
                     ;; write an item
                     (html:enumerating-item (stream)
                       (html:with-rendition (:bold :stream stream)
                         (html:note-anchor anchor-text :reference url-inf :stream stream))
                       (destructuring-bind (&key length-in-bytes creation-date &allow-other-keys) plist
                         (when (and creation-date creation-date-p)
                           (write-string "  " stream)
                           (write-standard-time creation-date stream))
                         (cond (directory-file-p
                                (write-string " [Directory]" stream))
                               (length-in-bytes
                                (write-string " [" stream)
                                (write length-in-bytes :stream stream :escape nil :base 10.)
                                (write-string " Bytes]" stream)
                                (fresh-line stream)))))
                     (html:break-line :stream stream)))))))
    (let* ((title (funcall title-function url))
           (default (translated-pathname url)))
      (declare (dynamic-extent title))
      ;; get down to business
      (html:with-html-document (:stream stream)
        (html:with-document-preamble (:stream stream)
          (html:declare-title title :stream stream))
        (html:with-document-body (:stream stream)
          (html:with-section-heading (title :stream stream)
            (html:horizontal-line :stream stream)
            (html:with-paragraph (:stream stream)
              (html:with-enumeration (stream enumeration-type :compact t)
                (loop for (path . plist) in (if directories-p
                                                (www-utils:directory-list*
                                                  default inclusion-predicate :files :directories :properties :sorted)
                                                (www-utils:directory-list*
                                                  default inclusion-predicate :files :properties :sorted))
                      for translated = (translated-pathname path)
                      do (cond ((and directories-p (www-utils:pathname-directory-p translated))
                                (write-item translated plist t stream))
                               (t (write-item translated plist nil stream))))))
            (html:horizontal-line :stream stream)
            (cl-http-signature stream))))
      url)))

(defun %write-directory-listing-html3 (url stream inclusion-predicate path-url-intern-function 
                                           anchor-function title-function 
                                           &optional directories-p (eunmeration-type :definition) (creation-date-p t))
  (declare #+Genera(sys:downward-funarg inclusion-predicate path-url-intern-function anchor-function title-function)
           (ignore eunmeration-type))
  (labels ((write-item (path plist directory-file-p stream)
             (multiple-value-bind (url-inf newly-created-p)
                 (funcall path-url-intern-function path url)
               (when url-inf
                 (let ((anchor-text (funcall anchor-function url-inf path directory-file-p)))
                   (declare (dynamic-extent anchor-text))
                   (when anchor-text
                     (when newly-created-p
                       ;; ensure that it has been exported.
                       (let ((export-type  (typecase url-inf
                                             (http-path :directory-hierarchy)   ;export directory paths correctly
                                             (t (pathname-export-type path nil)))))
                         (when export-type
                           (export-url url-inf export-type :pathname path)
                           (inherit-export-parameters url-inf url))))
                     ;; write an item
                     (html:with-table-row (:stream stream)
                       (html:with-table-cell (:stream stream)
                         (html:with-rendition (:bold :stream stream)
                           (html:note-anchor anchor-text :reference url-inf :stream stream)))
                       (destructuring-bind (&key length-in-bytes creation-date
                                                 #+cl-http-file-author author &allow-other-keys)
                           plist
                         (if (and creation-date creation-date-p)
                             (html:with-table-cell (:horizontal-alignment :right :stream stream)
                               (write-standard-time creation-date stream))
                             (html:with-table-cell (:horizontal-alignment :center :stream stream)
                               (write-string "--" stream)))
                         (cond (directory-file-p
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
                             (html:with-table-cell (:horizontal-alignment :center :stream stream)))))))))))
    (let* ((title (funcall title-function url))
           (default (translated-pathname url)))
      (declare (dynamic-extent title))
      ;; get down to business
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
              (loop for (path . plist) in (if directories-p
                                              (www-utils:directory-list*
                                                default inclusion-predicate :files :directories :properties :sorted)
                                              (www-utils:directory-list*
                                                default inclusion-predicate :files :properties :sorted))
                    for translated = (translated-pathname path)
                    do (cond ((and directories-p (www-utils:pathname-directory-p translated))
                              (write-item translated plist t stream))
                             (t (write-item translated plist nil stream)))))
            (html:horizontal-line :stream stream)
            (cl-http-signature stream))))
      url)))

(defun %write-directory-listing-html4 (url stream inclusion-predicate path-url-intern-function 
                                           anchor-function title-function 
                                           &optional directories-p (eunmeration-type :definition) (creation-date-p t))
  (declare #+Genera(sys:downward-funarg inclusion-predicate path-url-intern-function anchor-function title-function)
           (ignore eunmeration-type))
  (labels ((write-item (path plist directory-file-p stream)
             (multiple-value-bind (url-inf newly-created-p)
                 (funcall path-url-intern-function path url)
               (when url-inf
                 (let ((anchor-text (funcall anchor-function url-inf path directory-file-p)))
                   (declare (dynamic-extent anchor-text))
                   (when anchor-text
                     (when newly-created-p
                       ;; ensure that it has been exported.
                       (let ((export-type  (typecase url-inf
                                             (http-path :directory-hierarchy)   ;export directory paths correctly
                                             (t (pathname-export-type path nil)))))
                         (when export-type
                           (export-url url-inf export-type :pathname path)
                           (inherit-export-parameters url-inf url))))
                     ;; write an item
                     (html:with-table-row (:stream stream)
                       (html:with-table-cell (:stream stream)
                         (html:with-rendition (:bold :stream stream)
                           (html:note-anchor anchor-text :reference url-inf :stream stream)))
                       (destructuring-bind (&key length-in-bytes creation-date
                                                 #+cl-http-file-author author &allow-other-keys)
                           plist
                         (if (and creation-date creation-date-p)
                             (html:with-table-cell (:horizontal-alignment :right :stream stream)
                               (write-standard-time creation-date stream))
                           (html:with-table-cell (:horizontal-alignment :center :stream stream)
                             (write-string "--" stream)))
                         (cond (directory-file-p
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
                           (html:with-table-cell (:horizontal-alignment :center :stream stream)))))))))))
    (let* ((title (funcall title-function url))
           (default (translated-pathname url)))
      (declare (dynamic-extent title))
      ;; get down to business
      (with-cl-http-html-document (:declare-dtd-version-p :strict :stream stream)
        (html:with-document-look-preamble (:stream stream)
          (html:declare-title title :stream stream))
        (html:with-document-look-body (:heading title :header-class "header.cl-http" :body-class "body.cl-http"
                                       :footer-class "footer.cl-http" :stream stream)
           (html:with-table (:cell-spacing 4 :stream stream)
             (html:with-table-row-group (:header :stream stream)
               (html:with-table-row (:horizontal-alignment :center :stream stream)
                 (html:with-table-cell (:header-p t :stream stream)
                   (write-string "URL" stream))
                 (html:with-table-cell (:header-p t :stream stream)
                   (write-string "Creation Date" stream))
                 (html:with-table-cell (:header-p t :stream stream)
                   (write-string "Bytes" stream))
                 #+cl-http-file-author
                 (html:with-table-cell (:header-p t :stream stream)
                   (write-string "Author" stream))))
             (html:with-table-row-group (:body :stream stream)
               (loop for (path . plist) in (if directories-p
                                               (www-utils:directory-list*
                                                default inclusion-predicate :files :directories :properties :sorted)
                                             (www-utils:directory-list*
                                              default inclusion-predicate :files :properties :sorted))
                     for translated = (translated-pathname path)
                     do (cond ((and directories-p (www-utils:pathname-directory-p translated))
                               (write-item translated plist t stream))
                              (t (write-item translated plist nil stream)))))))))
      url))

;;;------------------------------------------------------------------- 
;;;
;;; DIRECTORY LIST CACHING
;;; 

(define-variable *directory-index-cache-keys* nil
                 "Contains the cache keys used to locate directory index caches.")

(defun %note-directory-index-cache-key (key)
  "Records use of KEY as a directory index cache key."
  (pushnew key *directory-index-cache-keys*))

(define-macro note-directory-index-cache-keys (&rest keys)
  "Records use of KEYS as a directory index cache keys."
  `(mapc #'%note-directory-index-cache-key ',keys))

(define map-directory-index-caches (function)
  "Maps FUNCTION over the cached directory indexes.
FUNCTION is called with URL URL-STRING CACHE-KEY CACHE-VALUE.
CACHE-VALUE is (CACHE-TIME CACHED-LAST-MODIFICATION  DIRECTORY-STRING)."
  (flet ((clear-directory-index (url-string url)
           (loop for (indicator value) on (property-list url) by #'cddr
                 when (member indicator *directory-index-cache-keys*)
                   do (funcall function url url-string indicator value))))
    (url:map-url-table #'clear-directory-index)))

(define clear-directory-index-caches (&optional before-universal-time)
  "Clears all cached directory indexes.
If  provided, only listings cached before BEFORE-UNIVERSAL-TIME  are cleared."
  (flet ((clear-directory-index (url url-string cache-key cache-value)
           (declare (ignore url-string))
           (when (or (null before-universal-time)
                     (destructuring-bind (cache-time &rest ignore) cache-value
                       (declare (ignore ignore))
                       (< cache-time before-universal-time)))
             (remove-value url cache-key))))
    (declare (dynamic-extent #'clear-directory-index))
    (map-directory-index-caches #'clear-directory-index)))

(define show-directory-index-caches (&optional (stream *standard-output*))
  "Displays the cached directory indexes on STREAM."
  (flet ((show-directory-index (url url-string cache-key cache-value)
           (declare (ignore url))
           (destructuring-bind (cache-time last-modification directory-string) cache-value
             (fast-format stream "~&~A (~S) ~I [~I] ~D" url-string cache-key
                          (write-standard-time cache-time stream)
                          (write-standard-time last-modification stream)
                          (length directory-string)))))
    (declare (dynamic-extent #'show-directory-index))
    (map-directory-index-caches #'show-directory-index)))

;; This could cache the CRLF translated version to speed processing, if anyone really cared.
;; Similarly, we could track the access times but it could be worth avoiding consing universal
;; time on high volume servers. -- JCMa 7/3/1997.
(define-macro with-directory-index-caching ((url stream cache-key &key (last-modification-function ''file-modification-date))
                                            &body body)
  "Use this macro to cache directory views on high volume servers.
When using new cache keys, use NOTE-DIRECTORY-INDEX-CACHE-KEYS so that 
CLEAR-DIRECTORY-INDEX-CACHES will be able to find them."
  `(let ((use-cache *cache-directory-indexes*))
     (cond (use-cache
            (let ((current-modification ,(etypecase last-modification-function
                                           (cons
                                             (case (car last-modification-function)
                                               (quote `(,(second last-modification-function) ,url))
                                               (t `(funcall ,last-modification-function ,url)))))))
              (destructuring-bind (&optional cache-time cached-last-modification directory-string)
                  (get-value ,url ,cache-key)
                ;; unless the modifications are the same recompute
                (unless (and directory-string
                             (eql cached-last-modification current-modification)
                             (or (not (numberp use-cache))
                                 (< (- (the bignum (server-request-time *server*)) (the bignum cache-time))
                                    use-cache)))
                  #+ignore(fast-format *standard-output* "~&[~I] Caching Directory: ~A"
                                       (http::write-standard-time (get-universal-time) stream) ,url)
                  (setq directory-string (with-output-to-string (,stream) ,@body))
                  (setf (get-value ,url ,cache-key) (list (get-universal-time) current-modification directory-string)))
                ;;(format *standard-output* "~&Cache Hit ~A: ~S" ,url directory-string)
                ;; write the string.
                (write-string directory-string ,stream))))
           ;; no caching case
           (t ,@body))))


;;;------------------------------------------------------------------- 
;;;
;;; DIRECTORY LISTS
;;;

(define write-indexed-directory-listing (url stream &optional (index-filename "index.html"))
  "Writes a directory listing based on an index file stored in the directory.
This function is suitable for use as a DIRECTORY-WRITER for EXPORT-URL of
a directory. It includes an HTTP response and headers.
INDEX-FILENAME is the filename and extension where the index file is found."
  (declare (values directory-listing-written-p))
  (let ((index-file (and index-filename (probe-file (merge-pathnames index-filename (translated-pathname url))))))
    (when index-file
      (let* ((content-type-spec (%mime-content-type-spec :html))
             (charset (or (url:character-set url) (getf (cddr content-type-spec) :charset))))
        (write-any-document-from-pathname url index-file stream content-type-spec charset)
        ;;must return non-null if the task was handled
        t))))

(defun write-directory-listing (url stream inclusion-predicate path-url-intern-function &optional directories-p)
  (flet ((anchor-text (url pathname directory-file-p)
	   (flet ((get-anchor-string (url pathname version recompute-p)
		    (with-value-cached (url :directory-string :recompute-p recompute-p)
		      (let ((name (url:object url))
			    (type (pathname-type pathname)))
			(declare (dynamic-extent name))
			(list* version
			       (typecase version
				 ((or keyword null) (string-append name "." type))
				 (t (string-append name "." type "." (write-to-string version :base 10. :escape nil)))))))))
	     (cond (directory-file-p
		    (when directories-p
		      (url:path-most-specific-name url)))
		   (t (let ((version (pathname-version pathname)))
			(multiple-value-bind (spec cached-p)
			    (get-anchor-string url pathname version nil)
			  ;; decache entry when the version changes
			  (if (and cached-p (not (eql (car spec) version)))
			      (cdr (get-anchor-string url pathname version t))
                            (cdr spec)))))))))
    (declare (dynamic-extent #'anchor-text))
    (multiple-value-bind (user-agent version)
	(current-user-agent)
      (let* ((html-directory-version (cond ((user-agent-capability-p :html4 user-agent version) :directory-list-html4)
					   ((user-agent-capability-p :tables user-agent version) :directory-list-html3)
					   (t :directory-list-html2)))
	     (writer (ecase html-directory-version
		       (:directory-list-html4 #'%write-directory-listing-html4)
		       (:directory-list-html3 #'%write-directory-listing-html3)
		       (:directory-list-html2 #'%write-directory-listing-html2))))
	(with-directory-index-caching (url stream html-directory-version :last-modification-function 'file-modification-date) 
	  (funcall writer url stream inclusion-predicate path-url-intern-function #'anchor-text
		   #'(lambda (url) (string-unescape-special-chars (url:path-directory-string url)))
		   directories-p :definition t))))))

;; returns text/uri-list
(defun write-directory-listing-as-uri-list (url stream inclusion-predicate path-url-intern-function &optional directories-p)
  (declare (dynamic-extent inclusion-predicate path-url-intern-function))
  (flet ((write-item (path stream)
	   (multiple-value-bind (url-inf newly-created-p)
	       (funcall path-url-intern-function path url)
	     (when url-inf
	       (when newly-created-p
		 ;; ensure that it has been exported.
		 (let ((export-type  (typecase url-inf
				       (http-path (translation-method url))	;export directory paths correctly
				       (t (pathname-export-type path nil)))))
		   (when export-type
		     (export-url url-inf export-type :pathname path)
		     (inherit-export-parameters url-inf url))))
	       (fresh-line stream)
	       (write-string (coerce-url-string url-inf) stream)))))
    (with-directory-index-caching (url stream :uri-list :last-modification-function 'file-modification-date) 
      (loop with default = (translated-pathname url)
	    for path in (if directories-p
			    (www-utils:directory-list* default inclusion-predicate :files :directories :sorted)
			    (www-utils:directory-list* default inclusion-predicate :files :sorted))
	    for translated = (translated-pathname path)
	    do (write-item translated stream)))
    url))

(define-content-type-name :uri-list :text :uri-list)

;; returns text/x-directory-list
(defun write-directory-listing-as-uri-property-list (url stream inclusion-predicate path-url-intern-function &optional directories-p)
  (declare (dynamic-extent inclusion-predicate path-url-intern-function))
  (flet ((write-item (path plist stream)
	   (multiple-value-bind (url-inf newly-created-p)
	       (funcall path-url-intern-function path url)
	     (when url-inf
	       (when newly-created-p
		 ;; ensure that it has been exported.
		 (let ((export-type  (typecase url-inf
				       (http-path (translation-method url))	;export directory paths correctly
				       (t (pathname-export-type path nil)))))
		   (when export-type
		     (export-url url-inf export-type :pathname path)
		     (inherit-export-parameters url-inf url))))
	       (destructuring-bind (&key length-in-bytes creation-date directory modification-date author &allow-other-keys) plist
		 (fast-format stream "~&(~S~I)"
			      (coerce-url-string url-inf)
			      (cond-every
				(directory (fast-format stream " :directory t"))
				(length-in-bytes (fast-format stream " :length;;
-in-bytes ~D" length-in-bytes))
				(modification-date (fast-format stream " :modification-date ~D" modification-date))
				(creation-date (fast-format stream " :creation-date ~D" creation-date))
				(author (fast-format stream " :author ~S" author)))))))))
    (with-directory-index-caching (url stream :uri-property-list :last-modification-function 'file-modification-date) 
      (loop with default = (translated-pathname url)
	    for (path . plist) in (if directories-p
				      (www-utils:directory-list* default inclusion-predicate :files :directories :properties :sorted)
				      (www-utils:directory-list* default inclusion-predicate :files :properties :sorted))
	    for translated = (translated-pathname path)
	    do (write-item translated plist stream)))
    url))

(define-content-type-name :directory-list :text :x-directory-list)

;; Remember the standard directory cache keys.
(note-directory-index-cache-keys :directory-list-html3 :directory-list-html2 :uri-list :uri-property-list)

(defparameter *default-directory-writer* '(write-directory-listing :html)
  "The default directory writer and its data-type keyword.")

(defparameter *directory-writer-alist* '((:text
					   (:html (() write-directory-listing :html))
					   (:uri-list (() write-directory-listing-as-uri-list :uri-list))
					   (:x-directory-list (() write-directory-listing-as-uri-property-list :directory-list)))
					 (:* (:* (() write-directory-listing :html)))))

(defun get-directory-writer-for-media-type (media-type)
  (flet ((match-parameters-p (request-parameters writer-parameters)
	   (loop for (key value) on request-parameters by #'cddr
		 unless (or (eq key :q)		;skip quality values in request
			    (equalp value (getf writer-parameters key :+not-found+)))
		   return nil
		 finally (return t))))
    (declare (inline match-parameters-p))
    (destructuring-bind (major minor . param-plist) media-type
      (let* ((major-entry (assoc major *directory-writer-alist*))
	     (minor-entry (and major-entry (assoc minor (cdr major-entry)))))
	(if minor-entry
	    (loop for param-entry in (cdr minor-entry)
		  when (match-parameters-p param-plist (first param-entry))
		    return (cdr param-entry)
		  finally (return nil))
	    nil)))))

(defmethod get-directory-writer-acceptable-to-client (server url)
  (when *content-negotiate-directory-views*
    (with-header-values (accept) (server-headers server)
      (when accept
	(loop for media-type in (sort-accept-header-media-types accept)
	      for write-spec = (get-directory-writer-for-media-type media-type)
	      when write-spec
		do (return-from get-directory-writer-acceptable-to-client (values-list write-spec))
	      finally (error 'acceptable-resource-not-found :headers `(:accept ., accept) :method :get :url url)))))
  (values-list *default-directory-writer*))

(defun write-directory-listing-acceptable-to-client (url stream inclusion-predicate path-url-intern-function directories-p)
  (multiple-value-bind (writer data-type)
      (get-directory-writer-acceptable-to-client *server* url)
    (with-conditional-get-response (stream data-type :last-modification (file-modification-date (url::cached-pathname url))
					   :expires (url:expiration-universal-time url) :content-location url
					   :cache-control (url:response-cache-control-directives url)
					   :content-language (languages url))
      (funcall writer url stream inclusion-predicate path-url-intern-function directories-p))))


;;;------------------------------------------------------------------- 
;;;
;;; EXPORTING DIRECTORIES
;;;

(defgeneric %unexport-directory (url translation))
;; null method
(defmethod %unexport-directory ((url url:http-url) (translation symbol)) nil)

;; Arrange for unexport to do directories for which there are %UNEXPORT-DIRECTORY methods..
(defmethod unexport-url :before ((url url:http-path))
  (declare (values url translation))
  ;; handle any inferior structures according to the export type.
  (let ((translation (translation-method url)))
    (when translation
      (%unexport-directory url translation))))

(define-generic export-directory-pathname-p (pathname directory-export-type)
  (:documentation "Returns non-null when PATHNAME would be exported by DIRECTORY-EXPORT-TYPE."))

(defmethod export-directory-pathname-p ((pathname pathname) export-type)
  (error "EXPORT-TYPE, ~S, is not a directory export type." export-type))

(define-generic directory-type-exports-pathname-export-type-p (directory-type export-type)
  (:documentation "Returns non-null when directory-type exports pathnames assocatiated with EXPORT-TYPE."))

(defmethod directory-type-exports-pathname-export-type-p ((directory-type symbol) export-type)
  (error "EXPORT-TYPE, ~S, is not a directory export type." export-type))

(eval-when (:compile-toplevel :execute :load-toplevel)
   
  ;; abstract the predicate constructor. Optimize this for the many predicates case. -- JCMa 5/26/1995.
(defmacro %make-data-type-pathname-predicate (test-var data-types match-directories-p)
  (let ((test `(and (file-exportable-p ,test-var);ability to suppress files -- JCMa 9/15/2005
                    ,(cond ((cdr data-types)
                            `(member (url:data-type ,test-var) ',data-types :test #'eq))
                           ;; general but less efficient than preceeding test.
                           (t (loop for data-type in data-types
                                    for pred = (url:data-type-pathname-predicate data-type)
                                    collect `(,pred ,test-var) into test
                                    finally (return (if (cdr test)
                                                        `(or ,.test)
                                                      (car test)))))))))
    (cond (match-directories-p
	   `(or (and (pathname-directory-p ,test-var)
                     (directory-exportable-p ,test-var)) ;ability to suppress directories -- JCMa 9/15/2005
		,test))
	  (t test))))
   
(defun %define-pathname-export-predicate (export-type data-types directories-p)
  `(progn
     (defmethod export-directory-pathname-p ((pathname pathname) (export-type (eql ,export-type)))
       (%make-data-type-pathname-predicate pathname ,data-types ,directories-p))
     (defmethod directory-type-exports-pathname-export-type-p ((directory-type (eql ,export-type)) (export-type symbol))
       (let ((major-type (mime-content-type-major-type (primary-pathname-extension-for-export-type export-type))))
	 (member major-type ',data-types :test #'eq)))))
   
  ;; abstract the locamotive
(defmacro %%map-export-directory ((&key url data-types dir-options) &body body)
  `(flet ((export-pathname-p (pathname)
	    (%make-data-type-pathname-predicate pathname ,data-types nil)))
     (declare (inline export-pathname-p))
     ;; do the work
     (loop for path in (www-utils:directory-list* (translated-pathname ,url) nil ,.dir-options)
	   for translated = (translated-pathname path)
	   do (progn . ,body))))
   
(defun %define-directory-export-method (export-type data-types &optional directories-p (files-p t))
  (check-type export-type keyword)
  (check-type data-types cons)
  (unless (every #'url:data-type-keyword-p data-types)
    (error "An unknown data-type was supplied in DATA-TYPES, ~S" data-types))
  (let ((options nil))
    (cond-every
      (files-p (push :files options))
      (directories-p (push :directories options)))
    ;; build and return the method
    `(defmethod  export-url ((url url:http-path) (translation (eql ,export-type)) &rest args)
       (destructuring-bind (&key pathname recache (immediate-export (not (eql *auto-export* :on-demand)))
				 character-set
				 &allow-other-keys) args
	 (cond (pathname
		(setf (translated-pathname  url) (make-directory (translated-pathname pathname))))
	       (t (error "No PATHNAME was provided while exporting the URL, ~S, with translation, ~S"
			 url translation)))
	 (setf (translation-method url) ,export-type
	       (character-set url) character-set)
	 ;; Only descend hierarchy when requested. 5/1/96 -- PCH
	 (when immediate-export
	   (let ((url-name-string (url:name-string url)))
	     ;; one could trim out the :files option when
	     ;; immediate-export is null to squeeze out a little faster
	     ;; start up. -- JCMa 9/1/95
	     (%%map-export-directory (:url url :data-types ,data-types :dir-options ,options)
	       (cond ((www-utils:pathname-directory-p translated)
		      ,(when directories-p 
			 `(when (directory-exportable-p translated) ;ability to suppress directories -- JCMa 9/15/2005
                            (let* ((path (www-utils:pathname-as-directory translated))
                                   (local-args `(:pathname ,path ,.args)))
                              (declare (dynamic-extent local-args))
                              (apply #'export-url 
                                     (intern-pathname-as-url-inferior path url :directory-p t :if-does-not-exist :create)
                                     ,export-type local-args)))))
		     ((and (or immediate-export recache) (export-pathname-p translated))
		      (%export-pathname translated url-name-string args recache))))))
	 url))))
   
(defun %define-unexport-directory-method (export-type data-types &optional directories-p (files-p t))
  (check-type export-type keyword)
  (check-type data-types cons)
  (unless (every #'url:data-type-keyword-p data-types)
    (error "An unknown data-type was supplied in DATA-TYPES, ~S" data-types))
  (let ((options nil))
    (cond-every
      (files-p (push :files options))
      (directories-p (push :directories options)))
    ;; build and return the method
    `(defmethod  %unexport-directory ((url url:http-path) (translation (eql ,export-type)))
       (%%map-export-directory (:url url :data-types ,data-types :dir-options ,options) 
	 (cond ((www-utils:pathname-directory-p translated)
		,(when directories-p 
		   `(unexport-pathname-as-url-inferior path url t)))
	       ((export-pathname-p translated) 
		(unexport-pathname-as-url-inferior path url nil))))
       (%unregister-directory-export-type-mime-major-type ,export-type)
       (%unregister-directory-export-type ,export-type ,directories-p)
       url)))
   
(defun %define-directory-write-headers-method (export-type)
  `(defmethod write-document-headers ((url url:http-path) (translation (eql ,export-type)) stream)
     (%write-document-headers-no-pathname url :html stream nil)))
   
#+ignore
(defun %define-directory-write-method (export-type data-types &optional directories-p)
  (check-type export-type keyword)
  (check-type data-types cons)
  (unless (every #'url:data-type-keyword-p data-types)
    (error "An unknown data-type was supplied in DATA-TYPES, ~S" data-types))
  `(defmethod write-document ((url url:http-path) (translation (eql ,export-type)) stream)
     (flet ((intern-path-url (path url)
	      (url:intern-pathname-as-url-inferior path url :if-does-not-exist :create))
	    (anchor-text (url pathname directory-file-p)
	      (cond (directory-file-p
		     ,(when directories-p
			`(url:path-most-specific-name url)))
		    (t (with-value-cached (url :directory-string)
			 (let ((name (url:object url))
			       (type (pathname-type pathname))
			       (version (pathname-version pathname)))
			   (declare (dynamic-extent name))
			   (typecase version
			     ((or keyword null) (string-append name "." type))
			     (t (string-append name "." type "." (write-to-string version :base 10. :escape nil)))))))))
	    (inclusion-predicate (pathname)
	      ,(cond (data-types 
		      `(%make-data-type-pathname-predicate pathname ,data-types ,directories-p))
		     (t t))))
       (declare (dynamic-extent #'anchor-text #'inclusion-predicate))
       (with-conditional-get-response (stream :html :last-modification (file-modification-date (url::cached-pathname url))
					      :expires (url:expiration-universal-time url) :content-location url
					      :cache-control (url:response-cache-control-directives url)
					      :content-language (languages url))
	 (write-directory-listing
	   url stream #'inclusion-predicate #'intern-path-url #'anchor-text #'url:path-directory-string ,directories-p)))))

(defun %define-directory-write-method (export-type data-types &optional directories-p)
  (check-type export-type keyword)
  (check-type data-types cons)
  (unless (every #'url:data-type-keyword-p data-types)
    (error "An unknown data-type was supplied in DATA-TYPES, ~S" data-types))
  `(defmethod write-document ((url url:http-path) (translation (eql ,export-type)) stream)
     (flet ((intern-path-url (path url)
	      (url:intern-pathname-as-url-inferior path url :if-does-not-exist :create))
	    (inclusion-predicate (pathname)
	      ,(cond (data-types 
		      `(%make-data-type-pathname-predicate pathname ,data-types ,directories-p))
		     (t t))))
       (declare (dynamic-extent #'inclusion-predicate))
       (write-directory-listing-acceptable-to-client url stream #'inclusion-predicate #'intern-path-url ,directories-p))))
   
(define-macro define-directory-export-type (export-type data-types &key directories)
  "Defines an export-type named EXPORT-TYPE that exports a directory of files.
All files which fall within data-types are exported.  When directories is non-null,
all subdirectories are checked for exportable files.

Available data-types are:  :HTML, :TEXT, :LISP, :IMAGE, :AUDIO, :VIDEO, :APPLICATION"
  `(progn ,(%define-directory-export-method export-type data-types directories)
	  ,(%define-directory-write-headers-method export-type)
	  ,(%define-directory-write-method export-type data-types directories)
	  ,(%define-unexport-directory-method export-type data-types directories)
	  ,(%define-pathname-export-predicate export-type data-types directories)
	  (%register-directory-export-type-mime-major-type ,export-type ',data-types)
	  (%register-directory-export-type ,export-type ',directories)
	  ,export-type)) 
   
(defun %%def-directory-export-type-unit (export-type export-type-recursive data-types)
  `(progn 
     (define-directory-export-type ,export-type ,data-types)
     (define-directory-export-type ,export-type-recursive ,data-types :directories t)
     ;; dispatch on the recursive arguments to the hierarchical method.
     (defmethod export-url :around ((url url:http-path) (translation (eql ,export-type)) &rest args)
       (destructuring-bind (&key recursive-p &allow-other-keys) args
	 (if recursive-p
	     (apply #'export-url url ,export-type-recursive args)
	     (call-next-method))))))
   
(defun %def-directory-export-type-unit (export-type-name data-types)
  (let* ((data-type-string (string export-type-name))
	 (export-type (symbolize (concatenate 'string data-type-string "-DIRECTORY")
                                 *keyword-package*))
	 (export-type-recursive (symbolize (concatenate 'string
                                                        data-type-string
                                                        "-DIRECTORY-HIERARCHY")
                                           *keyword-package*)))
    (%%def-directory-export-type-unit export-type export-type-recursive data-types)))
   
(defmacro define-standard-directory-export-types (data-types)
  `(progn 
     ,(%%def-directory-export-type-unit :directory :directory-hierarchy data-types)
     ,.(loop with export-type and export-data-types
	     for data-type in data-types
	     do (etypecase data-type
		  (symbol (setq export-type data-type
				export-data-types `(,data-type)))
		  (cons (setq export-type (car data-type)
			      export-data-types data-type)))
	     collect (%def-directory-export-type-unit export-type export-data-types))))
   
  )                                             ; close eval-when 

;;;------------------------------------------------------------------- 
;;;
;;;  STATIC FILE EXPORT-TYPE, PLUS METHODS FOR :GET AND :HEAD
;;;

;; IANA maintains a list of assigned content types per RFC 1590 that can be found at
;;; ftp://ftp.isi.edu/in-notes/iana/assignments/media-types/media-types
;;; http://www.iana.org/assignments/media-types/
;;; See www.iana.org for new interface   8/11/98 -- JCMa.

(define-url-export-types
 (:html-file :html (:text :html :charset :iso-8859-1)
  :copy-mode #.+standard-text-copy-mode+ :data-type :html :alternate-extensions (:htm))
 (:shtml-file :shtml (:text :html :charset :iso-8859-1)
  :copy-mode #.+standard-text-copy-mode+ :data-type :html :alternate-extensions (:stm)
  :url-class url:http-template-object :no-methods (:export :header :document))
 (:text-file :text (:text :plain :charset :iso-8859-1) :copy-mode #.+standard-text-copy-mode+
  :alternate-extensions (:txt :lisp :c :h :sh :script))
 (:lisp-file :lisp (:text :plain :charset :iso-8859-1) :copy-mode #.+standard-text-copy-mode+
  :alternate-extensions (:lsp :cl :asd) :data-type :lisp)
 (:sgml-file :sgml (:text :sgml) :copy-mode #.+standard-text-copy-mode+ :data-type :html :alternate-extensions (:sgm))
 (:xml-file :xml (:text :xml) :copy-mode #.+standard-text-copy-mode+ :data-type :html :alternate-extensions (:rdf :daml :opml))
 (:css-file :css (:text :css) :copy-mode #.+standard-text-copy-mode+ :data-type :html)
 ;; Images
 (:gif-image :gif (:image :gif) :copy-mode :binary)
 (:png-image :png (:image :png) :copy-mode :binary)
 (:x-bitmap-image :xbm (:image :x-xbitmap) :copy-mode #.+standard-text-copy-mode+)
 (:jpeg-image :jpeg (:image :jpeg) :copy-mode :binary 
  :alternate-extensions (:jpe :jpg))
 (:pict-image :pict (:image :pict) :copy-mode :binary :alternate-extensions (:pic))
 (:tiff-image :tiff (:image :tiff) :copy-mode :binary
  :alternate-extensions (:tif))
 (:ico-image :ico (:image :x-icon) :copy-mode :binary)
 ;; Audio
 (:basic-audio :au (:audio :basic) :copy-mode :binary
  :alternate-extensions (:snd))
 (:aiff-audio :aiff (:audio :aiff) :copy-mode :binary :alternate-extensions (:aif))
 (:wav-audio :wav (:audio :wav) :copy-mode :binary)
 (:midi-audio :midi (:audio :midi) :copy-mode :binary :alternate-extensions (:mid :kar :rmi))
 (:real-audio :ram (:audio :x-pn-realaudio) :copy-mode :binary :alternate-extensions (:ra))
 (:mpeg-audio :mpga (:audio :mpeg) :copy-mode :binary :alternate-extensions (:mp2 :mp2a :m2a))
 (:mpeg3-audio :mpeg3 (:audio :mpeg3) :copy-mode :binary :alternate-extensions (:mp3 :m3a))
 ;; iphone compatible audio and viedo mime-types - JCMa 7/13/2009
 (:mp4-audio :mp4a (:audio :mp4) :copy-mode :binary)
 (:aac-audio :m4a (:audio :mp4a-latm) :copy-mode :binary :alternate-extensions (:m4a :m4b :m4p))
 ;; Video
 (:avi-video :avi (:video :avi) :copy-mode :binary)
 (:mpeg-video :mpeg (:video :mpeg) :copy-mode :binary :alternate-extensions (:mpe :mpg :m1v :m2v))
 (:quicktime-video :qt (:video :quicktime) :copy-mode :binary :alternate-extensions (:mov :moov))
 (:mpeg4-video :mpg4 (:video :mpeg4) :copy-mode :binary :alternate-extensions (:mpg4))
 (:mp4-video :mp4 (:video :mp4) :copy-mode :binary :alternate-extensions (:mp4 :mp4v :mpg4))
 ;; the two main iphone video mime types
 (:mp4v-video :mp4v (:video :x-m4v) :copy-mode :binary :alternate-extensions (:m4v)) ; 900KBS max 3G & internet compatible
 (:3gpp-video :3gpp  (:video :3gpp) :copy-mode :binary :alternate-extensions (:3gp)) ;56K Edge compatible
 ;; Windows streaming content types -- JCMa 7/11/2009
 ;; http://www.microsoft.com/windows/windowsmedia/howto/articles/webserver.aspx
 (:asf-video :asf (:video :x-ms-asf) :copy-mode :binary :alternate-extensions (:asx :nsc))
 (:wax-video :wax (:video :x-ms-wax) :copy-mode :binary)
 (:wma-video :wma (:video :x-ms-wma) :copy-mode :binary)
 (:wmd-file :wmd (:application :x-ms-wmd) :copy-mode :binary)
 (:wmv-video :wmv (:video :x-ms-wmv) :copy-mode :binary)
 (:wmx-video :wmx (:video :x-ms-wmx) :copy-mode :binary)
 (:wmz-file :wmz (:application :x-ms-wmz) :copy-mode :binary)
 (:wvx-video :wvx (:video :x-ms-wvx) :copy-mode :binary)
 ;; Applications
 (:pdf-file :pdf (:application :pdf) :copy-mode :binary)
 (:postscript-file :ps (:application :postscript) :copy-mode #.+standard-text-copy-mode+
  :alternate-extensions (:eps :epsf))
 (:compressed-file :zip (:application :x-compressed) :copy-mode :binary
  :alternate-extensions (:z :gz :tgz))
 (:rpm-file :rpm (:application :x-rpm) :copy-mode :binary)
 (:exe-file :exe (:application :x-winexe) :copy-mode :binary)
 (:shockwave-file :dir (:application :x-director) :copy-mode :binary :alternate-extensions (:dcr))
;; Google media types http://earth.google.com/kml/kml_tut.html#kml_server
 (:kml-file :kml (:application :vnd.google-earth.kml+xml) :copy-mode #.http::+standard-text-copy-mode+)
 (:kmz-file :kmz (:application :vnd.google-earth.kmz) :copy-mode :binary)
 ;;  MAC related
 (:talk-file :talk (:plugin :talker) :copy-mode #.+standard-text-copy-mode+ :data-type :audio) ;obsolete
 (:talk-audio :talk (:plugin :talker) :copy-mode #.+standard-text-copy-mode+ :data-type :audio)
 (:binhex-file :hqx (:application :mac-binhex40):copy-mode #.+standard-text-copy-mode+)
 (:stuffit-file :sit (:application :x-stuffit) :copy-mode :binary :alternate-extensions (:sitx))
 (:mac-binary-file :macbin (:application :x-macbinary) :copy-mode :binary #+:Mac :alternate-extensions #+Mac(:bin))
 ;; Micrsoft
 (:binary-file :octet (:application :octet-stream) :copy-mode :binary :alternate-extensions (:bin :exe))
 ;;(:executable-file :exe (:application :octet-stream) :copy-mode :binary)
 (:rtf-file :rtf (:text :richtext) :copy-mode #.+standard-text-copy-mode+)
 (:word-file :word (:application :msword) :copy-mode :binary :alternate-extensions (:doc :docx))
 (:power-point-file :ppt (:application :msppt) :copy-mode :binary :alternate-extensions (:pot :pptx))
 (:excel-file :xl (:application :excel) :copy-mode :binary :alternate-extensions (:xls))
 ;; Java related
 (:java-file :java (:text :plain :charset :iso-8859-1) :copy-mode #.+standard-text-copy-mode+ :data-type :text)
 (:java-binary :class (:application :x-java-binary) :copy-mode :binary :alternate-extensions (:data))
 (:java-script-file :javascript (:text :javascript) :copy-mode #.+standard-text-copy-mode+ :data-type :text :alternate-extensions (:js))
 (:tcl-file :vbscript (:text :tcl) :copy-mode #.+standard-text-copy-mode+ :data-type :text)
 (:visual-basic-script-file :vbscript (:text :vbscript) :copy-mode #.+standard-text-copy-mode+ :data-type :text)
 ;; VRML related
 (:vrml-world :vrml (:x-world :x-vrml) :copy-mode #.+standard-text-copy-mode+ :data-type :world :alternate-extensions (:wrl))
 ;; Apple QuickDraw3D Meta File format
 (:3dmf-world :3dmf (:x-world :x-3dmf) :copy-mode :binary :data-type :world :alternate-extensions (:3dm :qd3d :qd3))
 ;; Apple Meta Content Format file
 (:mcf-file :mcf (:image :vasa) :copy-mode #.+standard-text-copy-mode+)
 ;; Research related
 (:lisp-sexp-file :sexp (:application :lisp-sexp) :copy-mode #.+standard-text-copy-mode+ :data-type :text)
 ;; URN Resolution Format
 (:uri-file :uri (:text :uri-list) :copy-mode #.+standard-text-copy-mode+ :data-type :text)
 ;; http message= request|response & http-headers & message-body RFC 2068
 (:http-message-file :http (:message :http) :copy-mode :binary)
 ;; SMTP message headers & message-body RFC 1521
 (:smtp-message-file :msg (:message :rfc822) :copy-mode :text)
 ;; NNTP Message headers & message-body RFC 1026
 (:nntp-message-file :news (:message :news) :copy-mode :text)
 ;; RFC 822 headers
 (:headers-file :rfc822-headers (:text :rfc822-headers) :copy-mode :text :alternate-extensions (:headers))
 ;; handle lispm reels
 (:lispm-reel :reel-1 (:application :x-lispm-reel) :copy-mode :binary)
 ;; handle text and binary signature files
 (:asc-file :asc (:text :plain :charset :iso-8859-1) :copy-mode #.+standard-text-copy-mode+)
 (:sig-file :sig (:application :x-sig) :copy-mode :binary)
 ;; X-509 format certificates - returned when creating client certificates
 (:x509-file :x509 (:application :x-x509-user-cert) :copy-mode :binary :alternate-extensions (:crt)))

;;;------------------------------------------------------------------- 
;;;
;;;  DEFINE STANDARD DIRECTORY EXPORT TYPES
;;; 

;; This defines all the standard directory export and write methods.
;; :directory can be lost when all the file types are recompiled, thus
;; necessitating a recompilation of this form.   8/23/96 -- JCMa.
(define-standard-directory-export-types (:html :text :lisp :image :audio :video :application :world))
 
;;;------------------------------------------------------------------- 
;;;
;;; COMPLEX EXPORTS
;;;

(defmethod  export-url ((url url:http-path) (translation (eql :redirect)) &rest args)
  (%export-url-redirect url translation args))

(defmethod  export-url ((url url:http-object) (translation (eql :redirect)) &rest args)
  (%export-url-redirect url translation args))

(defmethod  export-url ((url url:http-path) (translation (eql :temporary-redirect)) &rest args)
  (%export-url-redirect url translation args))

(defmethod  export-url ((url url:http-object) (translation (eql :temporary-redirect)) &rest args)
  (%export-url-redirect url translation args)) 

(defun %export-computed-url (url translation args)
  (destructuring-bind (&key response-function header-function pathname character-set &allow-other-keys) args
    (when pathname
      (setf (translated-pathname url) pathname))
    (unless (and response-function (good-response-function-p response-function))
      (error "RESPONSE-FUNCTION, ~S, must be a defined function when exporting the URL, ~S, with translation, ~S"
             response-function url translation))
    (setf (translation-method url) translation
          (character-set url) character-set)
    (let ((init-args `(,response-function ,header-function)))
      (declare (dynamic-extent init-args))
      (url:initialize-specialization url 
                                     (typecase url ;Select correct class for HTTPS
                                       (url:https-url 'url:https-computed-url)
                                       (t 'url:http-computed-url))
                                     init-args))
    url))

;; old name
(defmethod  export-url ((url url:http-path) (translation (eql :html-computed)) &rest args)
  (%export-computed-url url translation args))
;; old name
(defmethod  export-url ((url url:http-object) (translation (eql :html-computed)) &rest args)
  (%export-computed-url url translation args))

(defmethod  export-url ((url url:http-path) (translation (eql :computed)) &rest args)
  (%export-computed-url url translation args))

(defmethod  export-url ((url url:http-object) (translation (eql :computed)) &rest args)
  (%export-computed-url url translation args))

(defmethod  export-url ((url url:http-object) (translation (eql :html-form)) &rest args)
  (destructuring-bind (&key response-function header-function pathname server character-set
			    (durable-form-values nil durable-form-values-p-supplied)
			    (maximum-upload-file-size nil maximum-upload-file-size-supplied-p)
			    &allow-other-keys) args
    (cond (pathname
           (setf (translated-pathname  url) pathname))
          (t (error "No PATHNAME was provided while exporting the URL, ~S, with translation, ~S"
                    url translation)))
    (unless (and response-function (good-response-function-p response-function))
      (error "RESPONSE-FUNCTION, ~S, must be a defined function when exporting the URL, ~S, with translation, ~S"
             response-function url translation))
    (setf (translation-method url) translation
          (character-set url) character-set)
    (if durable-form-values-p-supplied
	(setf (get-value url :durable-form-values-p) durable-form-values)
      (remove-value url :durable-form-values-p))
    (if maximum-upload-file-size-supplied-p
	(setf (get-value url :maximum-upload-file-size) maximum-upload-file-size)
      (remove-value url :maximum-upload-file-size))
    (let ((init-args `(,server ,response-function ,header-function)))
      (declare (dynamic-extent init-args))
      (url:initialize-specialization url 
                                     (typecase url ;Select correct class for HTTPS
                                       (url:https-url 'url:https-form)
                                       (t 'url:http-form))
                                     init-args))
    url))

(defun %export-computed-form (url translation args)
  (destructuring-bind (&key form-function response-function header-function
			    (durable-form-values nil durable-form-values-p-supplied)
			    (maximum-upload-file-size nil maximum-upload-file-size-supplied-p)
			    &allow-other-keys) args
    (unless (and response-function (good-response-function-p response-function))
      (error "RESPONSE-FUNCTION, ~S, must be a defined function when exporting the URL, ~S, with translation, ~S"
             response-function url translation))
    (unless (and form-function (good-response-function-p form-function))
      (error "FORM-FUNCTION, ~S, must be a defined function when exporting the URL, ~S, with translation, ~S"
             form-function url translation))
    (setf (translation-method url) translation)
    (if durable-form-values-p-supplied
	(setf (get-value url :durable-form-values-p) durable-form-values)
      (remove-value url :durable-form-values-p))
    (if maximum-upload-file-size-supplied-p
	(setf (get-value url :maximum-upload-file-size) maximum-upload-file-size)
      (remove-value url :maximum-upload-file-size))
    (let ((init-args `(,form-function ,response-function ,header-function)))
      (declare (dynamic-extent init-args))
      (url:initialize-specialization url 
                                     (typecase url ;Select correct class for HTTPS
                                       (url:https-url 'url:https-computed-form)
                                       (t 'url:http-computed-form))
                                     init-args))
    url))

;; old name 
(defmethod  export-url ((url url:http-object) (translation (eql :html-computed-form)) &rest args)
  (%export-computed-form url translation args ))

(defmethod  export-url ((url url:http-object) (translation (eql :computed-form)) &rest args)
  (%export-computed-form url translation args ))

(defmethod export-url ((url url:http-search) (translation (eql :search)) &rest args)
  (destructuring-bind (&key response-function header-function search-parser search-writer search-database &allow-other-keys) args
    (%export-url-search url translation response-function search-parser search-writer search-database header-function)))

(defmethod respond-to-image-map ((url url:http-searchable-object) stream)
  (with-image-coordinates (url)
    (let* ((url-parent (url:search-parent url))
           (database (url:search-database url-parent))
           (destination-url (get-image-map-hit database x y))
           operator-type)
      (cond ((null destination-url) (error "No destination URL found."))
            ((host-eq (host-object destination-url)
                      (local-host))
             (cond ((setq operator-type (translation-method destination-url))
                    (with-access-control (destination-url :get *server* (or (url:secure-subnets destination-url) *secure-subnets*)
                                                          :deny-subnets *disallowed-subnets*)
                      (write-document destination-url operator-type stream)))
                   (t (error 'document-not-found :url destination-url))))
            (t (signal 'document-moved-permanently :url url :new-urls (list destination-url)))))))

(defmethod export-url ((url url:http-searchable-object) (translation (eql :image-map)) &rest args)
  (destructuring-bind (&key export-type pathname map-format
                            map-pathname search-parser search-writer header-function &allow-other-keys) args
    (let ((search-database (parse-image-map map-pathname map-format)))
      (cond (pathname
             (setf (translated-pathname  url) pathname))
            (t (error "No PATHNAME was provided while exporting the URL, ~S, with export-type, ~S"
                      url export-type)))
      (setf (translation-method url) export-type)
      ;; standard search parameters.
      (%export-url-search url export-type #'respond-to-image-map search-parser search-writer search-database header-function )
      url)))

(defmethod export-url ((url url:http-minimum-object) (translation (eql :script)) &rest args)
  (destructuring-bind (&key script header-function &allow-other-keys) args
    (unless script
      (error "No script was provided while exporting the URL, ~S, with EXPORT-TYPE, ~S"
             url translation))
    (setf (translation-method url) translation)
    (let ((args `(,script ,header-function)))
      (declare (dynamic-extent args))
      (url:initialize-specialization url
                                     (typecase url ;Select correct class for HTTPS
                                       (url:https-url 'url:https-client-script)
                                       (t 'url:http-client-script))
                                     args))
    url))

(defmethod export-url ((url url:http-minimum-object) (translation (eql :shtml-file)) &rest args)
  (let ((template-object (url:initialize-specialization url 
                                                        (typecase url ;Select correct class for HTTPS
                                                          (url:https-url 'url:https-template-object)
                                                          (t 'url:http-template-object))
                                                        args)))
    (apply #'export-url template-object translation args)))

(defmethod export-url ((url url:http-template-object) (translation (eql :shtml-file)) &rest args)
  (destructuring-bind (&key header-function pathname character-set &allow-other-keys) args
    (cond (pathname
	   (setf (translated-pathname  url) pathname))
	  (t (error "No PATHNAME was provided while exporting the URL, ~S, with EXPORT-TYPE, ~S"
		    url translation)))
    (setf (translation-method url) translation
	  (character-set url) character-set
	  (header-function url) header-function)
    url))


;;;------------------------------------------------------------------- 
;;;
;;; DEMAND-DRIVEN EXPORT STATIC, DIRECTORY-EXPORTED URLs
;;;

;; this method needs to inherit all state from directory parents to
;; children for on demand exporting. 
;; Beware of inheriting translation methods because you can clobber directory/pathname exports
;; and create a security problem   9/11/98 -- JCMa.
(define-generic inherit-export-parameters (url parent-url)
  (:method-combination progn)
  (:documentation "Inherits certain slot values from PARENT-URL to URL.
This is used for automatic exporting of URL scoped by a directory exported parent."))

(defmethod inherit-export-parameters progn ((url url:http-path) (parent url:http-path))
  (setf (directory-writer url) (directory-writer parent)))

(defmethod inherit-export-parameters progn ((url url:http-url) (parent url:http-url))
  (setf (character-set url) (character-set parent)))

(defmethod inherit-export-parameters progn ((url url:content-language-mixin) (parent url:content-language-mixin))
  (setf (languages url) (languages parent)))

(defmethod inherit-export-parameters progn ((url url:expiration-mixin) (parent url:expiration-mixin))
  (setf (url:expiration-function url) (url:expiration-function parent)
        (url:max-age-function url) (url:max-age-function parent))) 

(defmethod inherit-export-parameters progn ((url url:secure-subnets-mixin) (parent url:secure-subnets-mixin))
  (setf (url:read-subnets url) (url:read-subnets parent)
	(url:write-subnets url) (url:write-subnets parent)
	(url:secure-subnets url) (url:secure-subnets parent)))

(defmethod inherit-export-parameters progn ((url url:server-timeout-mixin) (parent url:server-timeout-mixin))
  (cond-every
    ((slot-boundp url 'url::life-time)
     (setf (server-life-time url) (server-life-time parent)))
    ((slot-boundp url 'url::timeout)
     (setf (server-timeout url) (server-timeout parent)))))

(defmethod inherit-export-parameters progn ((url url:authentication-mixin) (parent url:authentication-mixin))
  (setf (url:authentication-realm url) (url:authentication-realm parent)
        (url:capabilities url) (url:capabilities parent)))

(defmethod inherit-export-parameters progn ((url url:http-cache-control-mixin) (parent url:http-cache-control-mixin))
  (setf (cache-control-directives url) (cache-control-directives parent)))

(defmethod inherit-export-parameters progn ((url url:documentation-mixin) (parent url:documentation-mixin))
  (let ((keywords (keywords parent))
        (description (description parent)))
    (cond-every
      (keywords (setf (keywords url) keywords))
      (description (setf (description url) description)))))

(defmethod inherit-export-parameters progn ((url url:computed-url-mixin) (parent url:computed-url-mixin))
  (setf (response-function url) (response-function parent)))

(defmethod inherit-export-parameters progn ((url url:file-data-caching-path) (parent url:file-data-caching-path))
  (with-slots (data-universe dc-revalidation-interval dc-wired-p) parent
    (when data-universe
      (initialize-file-data-cache url data-universe
                                  :revalidation-interval dc-revalidation-interval
                                  :wired-p dc-wired-p))))

(defun most-specific-exported-parent-url (string &optional (backoff-level 1) (end (length string)) &aux first-dir-delim)
  "Returns the most specific exported parent URL.
This is intended for static content types only, because the export type is computed from the pathname extension."
  (declare (values parent-url translation export-type directory-levels first-delim last-delim)
           (fixnum backoff-level))
  (flet ((get-export-type (string start end &aux period-pos)
           ;; there is no extension and we aren't going to grovel files to figure it out right now
           (when (setq period-pos (position #\. string :start start :end end :from-end t :test #'eql))
             (let ((extension (subseq string (1+ (the fixnum period-pos)) end)))
               (declare (dynamic-extent extension))
               (export-type-for-pathname-type extension nil)))))
    (declare (inline get-export-type))
    (multiple-value-bind (scheme-prefix scheme-prefix-length)
        (url::%scheme-prefix-for-protocol *standard-protocol*)
      (declare (ignore scheme-prefix))
      (when (setq first-dir-delim (char-position #\/ string scheme-prefix-length end))
        (loop with last-delim = (char-position #\/ string first-dir-delim end t)
              for directory-backoff fixnum downfrom backoff-level
              until (zerop directory-backoff)
              for delim = last-delim then (char-position #\/ string first-dir-delim delim t)
              while delim
              as url = (intern-url string :start 0 :end (1+ (the fixnum delim)) :if-does-not-exist :soft)
              when url
              do (let ((translation (translation-method url)))
                   (if translation
                       (return (values url translation (get-export-type string last-delim end)
                                       (- backoff-level directory-backoff) delim last-delim))
                     (return nil)))
              finally (return nil))))))

(defun auto-export-pathname-url (string &optional (length (length string)) &aux inferior-pathname)
  (declare (values auto-exported-url))
  (labels ((translated-pathname* (pathname)
             (handler-case
               (translated-pathname pathname)
               (parse-pathname-error () (error 'url:parsing-error :url-string string))))
           (url-inferior-directory (parent first-delim)
             (declare (fixnum first-delim))
             (loop with start fixnum = (incf first-delim)
                   while (< start length)
                   for delim = (char-position #\/ string start length)
                   while delim
                   collect (subseq string start delim) into inf-path
                   do (setq start (1+ (the fixnum delim)))
                   finally (return (let ((path (translated-pathname* parent)))
                                     (make-pathname :host (pathname-host path)
                                                    :device (pathname-device path)
                                                    :directory `(,@(pathname-directory path) ,.(mapcar #'string-unescape-special-chars inf-path)))))))
           (url-inferior-pathname (parent first-delim last-delim directory-levels)
             (let ((name-and-extension (string-unescape-special-chars (subseq string (1+ (the fixnum last-delim)) length))))
               (declare (dynamic-extent name-and-extension))
               (merge-pathnames name-and-extension (case directory-levels
                                                     (0 (translated-pathname* parent))
                                                     (t (url-inferior-directory parent first-delim))))))
           (exportable-inferior-object-p (parent parent-export-type first-delim last-delim directory-levels)
             (and (setq inferior-pathname (url-inferior-pathname parent first-delim last-delim directory-levels))
                  ;; parent exports this file type.
                  (export-directory-pathname-p inferior-pathname parent-export-type)
                  ;; must exist in the file system
                  (probe-file* inferior-pathname)))
           (exportable-inferior-path-p (parent first-delim)
             (and (setq inferior-pathname (url-inferior-directory parent first-delim))
                  (directory-exportable-p inferior-pathname) ;ability to suppress directories -- JCMa 9/15/2005
                  (probe-directory inferior-pathname)))
           (export-inferior (parent export-type pathname)
             (multiple-value-bind (url)
                 (intern-url (merge-url string (local-context)) :if-does-not-exist :create)
               ;; very small security lapse for url level access control.  6/3/95 -- JCMa.
               (export-url url export-type :pathname pathname)
               (inherit-export-parameters url parent)
               url))
           (export-intervening-structure (parent parent-export-type first-delim)
             (declare (fixnum first-delim))
             (let* ((pos (char-position #\/ string (1+ first-delim) length))
                    (parent-pathname (translated-pathname* parent))
                    (pathname (make-pathname :host (pathname-host parent-pathname)
                                             :device (pathname-device parent-pathname)
                                             :directory `(,@(pathname-directory parent-pathname)
                                                          ,(string-unescape-special-chars (subseq string (1+ first-delim) pos))))))
               (let ((down-one (intern-url string :start 0 :end (1+ (the fixnum pos)))) 
                     (url-string (merge-url string (local-context))))
                 ;; very small security lapse for url level access control.  6/3/95 -- JCMa.
                 (export-url down-one parent-export-type :pathname pathname)
                 (inherit-export-parameters down-one parent)
                 (case *auto-export*
                   (:on-demand ;; inferiorn may not be there in this case
                     (or (intern-url url-string :if-does-not-exist :soft)
                         ;; this could be a little faster by calling
                         ;; export-inferior but rather more cumbersome.  9/27/95 -- JCMa.
                         (auto-export-pathname-url string)))
                   ;; must be there if auto-export is on
                   (t (intern-url url-string :if-does-not-exist :error)))))))
    (declare (inline translated-pathname*))
    (multiple-value-bind
      (parent parent-export-type export-type directory-levels first-delim last-delim)
        (most-specific-exported-parent-url string -1 length)
      (cond ((null parent) nil)
            ((directory-export-type-p parent-export-type)
             ;; single level directory export 
             (when (and export-type             ;must be an http object with an export type
                        (zerop directory-levels)        ;must not require new directory levels
                        (exportable-inferior-object-p parent parent-export-type first-delim last-delim directory-levels))
               (export-inferior parent export-type inferior-pathname)))
            ((hierarchical-directory-export-type-p parent-export-type)
             (cond (export-type ;; an HTTP object
                    (when (exportable-inferior-object-p parent parent-export-type first-delim last-delim directory-levels)
                      (case directory-levels
                        (0 (export-inferior parent export-type inferior-pathname))
                        (t (export-intervening-structure parent parent-export-type first-delim)))))
                   ((= last-delim (1- (the fixnum length)))     ; an HTTP path
                    (when (exportable-inferior-path-p parent first-delim)
                      (case directory-levels
                        (0 (export-inferior parent parent-export-type inferior-pathname))
                        (t (export-intervening-structure parent parent-export-type first-delim)))))
                   (t nil)))                    ;unexportable object
            (t nil)))))                         ;non-directory superior

(define intern-and-auto-export-url (url-string &optional (local-context (local-context)) error-if-url-remote-p)
  "Interns and auto-exports URL-STRING merging against LOCAL-CONTEXT as necessary.
URL-STRING can only be exported if it is scoped by a parent directory that supports auto-exporting of
files with the media type as url-string."
  (declare (values url autoexported-p))
  (let* ((full-url-string (%merge-url url-string local-context))
	 (url (intern-url full-url-string :if-does-not-exist :soft)))
    (cond ((and url (translation-method url)) url)
	  ;; auto-export local urls as necessary
	  ((and *auto-export* (auto-export-pathname-url full-url-string))
	   (values url t))
	  ;; Error if local and unexportable
	  ((url:local-url-p (setq url (intern-url full-url-string :if-does-not-exist :uninterned)))
	   (error "The url-string URL, ~A, was not already exported and could not be auto-exported."
		  full-url-string))
	  (error-if-url-remote-p
	   (error "The url-string URL, ~A, does not denote a local url and could not be auto-exported."
		  full-url-string))
	  (t url))))				;return remote url
