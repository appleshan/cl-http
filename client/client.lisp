;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-

;;; (C) Copyright 1996-2001, 2006, John C. Mallery
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; CL-HTTP CLIENT SUBSTRATE
;;;

(in-package :http)

;; Export here to preserve modularity in the primary package definitions.
(mapc #'(lambda (x) (export (intern x :http) :http))
      '("*CLIENT-HTTP-VERSION*"
        "*CLIENT-TIMEOUT*"
        "FINGER"
        "INVOKE-HTTP-SERVICE"
        "WITH-HTTP-REQUEST"))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;
(defclass client-proxy-mixin
          ()
    ((proxy :initform nil :initarg :proxy :accessor client-proxy))
  (:documentation "A mixin that allows a client to invoke http service via a proxy.")) 

(defclass client-transaction-info-mixin 
	  ()
    ((reply-line :initform nil :initarg :reply-line :accessor client-reply-line)
     (reason-offset :initform 0 :initarg :reason-offset :accessor client-reason-offset)
     (reason :initform nil :initarg :reason :accessor %client-reason)
     (request-headers :initform nil :initarg :headers :accessor client-request-headers)
     (response-headers :initform nil :initarg :response-headers :accessor client-response-headers)
     (status :initform 408 :initarg :status :accessor client-status)	;default is dropped connection
     (request-version :initform nil :initarg :headers :accessor %client-request-version))	;HTTP version used in the transaction
  (:documentation "Information about the current http transaction."))

(defclass client-timing-mixin
	  ()
    ((request-time :initform nil :accessor %client-request-time)	;time of current request in universal time
     (process :initform nil :accessor client-process)   ;process we running in
     (start-time :initform 0 :accessor client-start-time)	;clock time in INTERNAL-TIME-UNITS-PER-SECOND   
     (process-start-time :initform 0 :accessor client-process-start-time)       ;microsecond start for process.
     (timeout :initform 0 :accessor client-timeout))	;maximum process idle time 
  (:documentation "A mixin that provides all the timing data for a client.")) 

(defclass client-logging-mixin
	  (client-timing-mixin logging-mixin)	; might inherit client-timing-mixin into basic-client-mixin -- JCMa 7/31/2000.
    ()
  (:documentation "Records information information necessary for logging client activity."))

(defclass basic-client-mixin
	  (client-transaction-info-mixin client-proxy-mixin client-logging-mixin property-list-mixin)
    ((connection :initform nil :initarg :connection :accessor client-connection)
     (method :initform nil :initarg :method :accessor client-method)
     (url :initform nil :initarg :url :accessor client-url))
  (:documentation "The essential components of a HTTP client.
    HEADER-FUNCTION should evaluate to a header-plist when applied to the connection HTTP version."))

(defclass client 
          (basic-client-mixin)
    ()
  (:documentation "The HTTP client class.")) 

(defmethod print-object ((client basic-client-mixin) stream)
  (print-unreadable-object (client stream :type t :identity t)
    (when (client-url client)
      (let ((host-domain-name (client-host-domain-name client))
            (port (client-host-port client)))
        (when host-domain-name
          (format stream "~A: ~D" host-domain-name port)))))
  client)


;;;------------------------------------------------------------------- 
;;;
;;; CLIENT HTTP CONDITIONS
;;;

(define-condition client-proxy-error (bad-gateway) ()
		  (:documentation "A client error related to its functioning on behalf of a proxy server."))

(define-condition client-proxy-configuration-error
		  (client-proxy-error)
  ()
  (:documentation "Error occurring when the client proxy is improperly configured
by failing to define a gateway to a foreign protocol."))

(define-condition client-unauthorized-file-access
    (recoverable-unauthorized-client-access)
  ((reason :initform "Unauthorized File Access")))

(define-condition client-unauthorized-ftp-access
    (recoverable-unauthorized-client-access)
  ((reason :initform "Unauthorized FTP Access")))

(defmethod authentication-header-spec ((condition client-unauthorized-ftp-access))
  `(:www-authenticate (,(http-authentication-method condition) ,(http-authentication-realm condition))))

;;;------------------------------------------------------------------- 
;;;
;;; MACROS
;;;

(define-macro with-client-line-buffer (() &body body)
  `(using-resource (*client-line-buffer* line-buffer *line-buffer-size*)
     ,@body))

(define-macro with-headers-for-client ((client stream version) &body body)
  "Reads headers from STREAM for the client and makes them available via GET-HEADER and MAP-HEADERS."
  `(let ((*headers* (resourced-read-headers (client-response-headers ,client) ,stream)))
     (update-connection-status-from-headers ,version *headers*)
     (when (member *trace-client* '(t :headers))
       (format *trace-output* "~&Headers: ")
       (write-header-buffer *headers* *trace-output*))
     ,@body))

(define-macro with-client-connection ((client &key (connection-var 'connection)
                                              (require-connection-p t)) &body body)
  `(let ((,connection-var (client-connection ,client)))
     ,(if require-connection-p  
          `(cond (,connection-var ,@body)
                 (t (error "No connection has been established for the client, ~S." ,client)))
          `(progn . ,body))))

(defmacro with-client-http-stream ((client &key (stream-var 'stream)) &body body)
  `(with-client-connection (,client)
     (let ((,stream-var (connection-stream connection)))
       ,@body)))

(define-generic client-stream (client)
  (:documentation "Returns the HTTP stream to the remote URL associated with CLIENT."))

(defmethod client-stream ((client basic-client-mixin))
  (with-client-http-stream (client)
    stream))

(define-generic client-url (client)
  (:documentation "Returns the request URL for CLIENT."))

(defmethod client-url ((client null))
  nil)

(defmethod url:name-string ((client client) &optional compute-p)
  (url:name-string (client-url client) compute-p))

(defmethod url:protocol ((client basic-client-mixin))
  (url:protocol (client-url client)))

(define-generic client-reason (client)
  (:documentation "Returns the reason string associated with the current server reply."))

(defmethod client-reason ((client client-transaction-info-mixin)) 
  (flet ((make-http-reason (client offset)
	   (let* ((reply-line (client-reply-line client))
		  (length (length reply-line)))
	     (declare (fixnum offset length))
	     (make-array (- length offset) :element-type *standard-character-type*
			 :displaced-to reply-line :displaced-index-offset offset))))
    (declare (inline make-http-reason))
    (cond ((%client-reason client))
	  (t (let* ((offset (client-reason-offset client))
		    (reason (if (zerop offset)
				(get-string-for-status-code (client-status client))
				(make-http-reason client offset))))
	       (setf (%client-reason client) reason)
	       reason)))))

;;;------------------------------------------------------------------- 
;;;
;;;  REMOTE HOST ACCESSORS
;;; 

(defmacro with-client-url ((client) &body body)
  `(let ((url (client-url ,client)))
     (cond (url ,@body)
           (t (error "No URL has been specified for the client, ~S." ,client)))))

(define-generic client-host (client)
  (:documentation "Returns the remote host object for CLIENT."))

(defmethod client-host ((client basic-client-mixin)) 
  (with-client-url (client)
    (host-object url)))

(define-generic client-host-port (client)
  (:documentation "Returns the port used to communicate with the remote host based on the URL."))

(defmethod client-host-port ((client basic-client-mixin)) 
  (with-client-url (client)
    (host-port url)))

(define-generic client-host-domain-name (client)
  (:documentation "Returns the remote host object for CLIENT."))

(defmethod client-host-domain-name ((client basic-client-mixin)) 
  (with-client-url (client)
    (host-string url)))

(defmethod connection-local-port ((client basic-client-mixin))
  (with-client-connection (client :require-connection-p nil)
    (when connection
      (connection-local-port connection))))

(defmethod connection-local-protocol ((client basic-client-mixin))
  (with-client-connection (client :require-connection-p nil)
    (when connection
      (connection-local-protocol connection))))

(defmacro with-client-proxy ((client &key (proxy-var 'proxy)) &body body)
  `(let ((,proxy-var (client-proxy ,client)))
     (when ,proxy-var
       ,@body)))

(define-generic proxy-host (client-or-proxy)
  (:documentation "Returns the proxy host for client-or-proxy."))

(defmethod proxy-host ((client client-proxy-mixin))
  (with-client-proxy (client)
    (proxy-host proxy)))

(define-generic proxy-port (client-or-proxy)
  (:documentation "Returns the proxy port for client-or-proxy."))

(defmethod proxy-port ((client client-proxy-mixin))
  (with-client-proxy (client)
    (proxy-port proxy)))

(define-generic proxy-domain-name (client-or-proxy)
  (:documentation "Returns the proxy domain-name for client-or-proxy."))

(defmethod proxy-domain-name ((client client-proxy-mixin))
  (with-client-proxy (client)
    (proxy-domain-name proxy))) 

(define-generic client-choose-proxy (client url)
  (declare (values proxy))
  (:documentation "Select a proxy host and port based on the URL associated with CLIENT."))

(defmethod client-choose-proxy ((client client-proxy-mixin) url)
  (setf (client-proxy client) (or *standard-proxy* (choose-proxy url))))        ; *standard-proxy* means the proxy has already been selected.

(defmethod return-connection ((client basic-client-mixin))
  (with-client-connection (client :require-connection-p nil)
    (when connection
      (return-connection connection)
      (setf (client-connection client) nil)
      t)))

(define-generic client-connection-version (client)
  (:documentation "Returns the HTTP version keyword for the client's current HTTP connection.
This is the version of the server connected on the other end."))

(defmethod client-connection-version ((client basic-client-mixin))
  (with-client-connection (client :require-connection-p nil)
    (if connection
        (connection-version connection)
        (%tokenize-header-keyword  *client-http-version*))))

(define-generic client-request-version (client)
  (:documentation "Returns the HTTP version keyword for the client's current HTTP connection.
This is the actual HTTP version that the client and server are using to communicate.
It can be lower than the connection version when the client explicitly issues its
request at a lower HTTP level."))

(defmethod client-request-version ((client client-transaction-info-mixin))
  (or (%client-request-version client)
      (setf (%client-request-version client) (client-connection-version client))))

(defmethod deallocate-connection ((client basic-client-mixin))
  (with-slots (connection) client
    (deallocate-connection connection)
    (setf connection nil)))

(defgeneric connection-requests-completed (client-or-connection)
   (:documentation "Returns the number of requests completed over the same HTTP connection for CLIENT-OR-CONNECTION."))

(defmethod connection-requests-completed ((client basic-client-mixin))
   (connection-requests-completed (client-connection client)))

;;;------------------------------------------------------------------- 
;;;
;;; CLIENT LOG CLASSES 
;;;

#|

;; See the method (log-entry-writer ( metering-common-file-format-mixin client-logging-mixin))
;; for capturing the CPU and Elapsed time for a request. This model can e used to define custom logging
;; formats to special purposes. Attention to efficiency and consing are relevant here.

(ensure-current-request-log)            ; create default client log

(clear-request-logs)                   ; clear any client logs laying about

|#


(defclass basic-client-log-mixin
   ()
   ((name :initform "CL-HTTP-Log" :initarg :name :accessor log-name)
     (creation-time :initform nil :reader log-creation-time)))

(defclass client-request-log (basic-client-log-mixin) ()) 

(defclass client-notification-log
          (log-notification-mixin client-request-log)
    ()
  (:documentation "This log class notifies the console concerning client HTTP activity."))

(defclass custom-client-notification-log
          (client-notification-log)
    ((predicate :initarg predicate :accessor notification-log-predicate)
     (notifier :initarg notifier :accessor notification-log-notifier))
  (:documentation "This log class notifies the console concerning client HTTP activity when a predicate is true."))

(defclass common-file-request-log
   (common-file-format-mixin
    #+ignore log-counters-mixin
     ;; log-notification-mixin ;do we really want another window showing client requests?
     client-request-log
     basic-file-logging-mixin)
   ((log-file-name :initform "Common-Request-Log" :initarg :log-file-name :allocation :class))
   (:documentation "This log class records client HTTP transactions according to the Common File Format.")) 

(defclass metering-common-file-format-mixin
	  ()
    ()
  (:documentation "Mixin that addss metering to Common Log File Format logging."))

(defclass metering-common-file-request-log
	  (metering-common-file-format-mixin
	    #+ignore log-counters-mixin
	    ;; log-notification-mixin ;do we really want another window showing client requests?
	    client-request-log
	    basic-file-logging-mixin)
    ((log-file-name :initform "Metering-Request-Log" :initarg :log-file-name :allocation :class))
  (:documentation "This log class records client HTTP transactions according to the Common File Format plus metering fields."))


;;;------------------------------------------------------------------- 
;;;
;;; CLIENT LOGGING
;;;

(define-variable *request-logs* nil
		 "Holds the list of all known client log objects.")

(define map-all-request-logs (function)
  "Maps FUNCTION over all logs."
  (declare (dynamic-extent function))
  (mapc function *request-logs*))

(defparameter *request-log-classes* '(common-file-request-log )
  "The available log classes.")

(setf (documentation '*request-log-classes* 'variable)
      (format nil "This variable controls the class of access log used to record HTTP transactions.
If you change this variable other than in the configuration file read on start up, 
use CLEAR-ACCESS-LOGS to clear server logging datastructures. Although you can define or customize
your own log classes (see http:server;log.lisp), a number of predefined log classes are available.

Currently Defined Log Classes~2%
~:{~S~&~A~:^~2%~}" (mapcar #'(lambda (x) (list x (documentation x 'type)))
                           (sort *log-classes* #'string<))))

(defmethod print-object ((log basic-client-log-mixin) stream)
  (with-slots (name) log
    (print-unreadable-object (log stream :type t :identity t)
      (format stream "~:[~;~:*~A~]]" name)))) 

(define-generic request-log-p (thing)
  (:documentation "Returns non-null when THING is an access log."))

(defmethod request-log-p ((log client-request-log)) t)

(defmethod request-log-p (thing) 
  (declare (ignore thing))
  nil) 

; Documentation is defined in http:server;log.lisp
(define-parameter *log-client-log-class* 'common-file-request-log)

(define default-client-log-file-name (&optional (name *log-client-log-class*))
  (format nil "~:(~A~)" name))

(defvar *standard-request-logs* nil
  "An list of all the standard client logs.")

(define standard-request-logs (&optional create-default-request-log-p)
  "Returns the standard client logs."
  (declare (values logs))
  (cond (*standard-request-logs*)
	(create-default-request-log-p
	 (intern-client-request-log (default-client-log-file-name)
				    :directory *standard-log-directory*
				    :class *log-client-log-class*
				    :if-does-not-exist :create)
	 (or *standard-request-logs*
	     (error "Should never happen: Client log not created!")))
	(t nil)))

(define-generic find-client-request-log-if (predicate &optional start end)
  (:documentation "Finds all client logs satisfying PREDICATE."))

(defmethod find-client-request-log-if (predicate &optional (start 0) end)
  (find-if predicate *request-logs* :start start :end end)) 

(define-generic add-client-request-log (client-request-log)
  (:documentation "Adds a client log, client-request-log."))

(defmethod add-client-request-log ((log client-request-log))
  (unless (member log *standard-request-logs*)
    (setf *standard-request-logs* (nconc *standard-request-logs* (list log))))) 

(define-generic remove-client-request-log (client-request-log)
  (:documentation "Removes an client log, client-request-log."))

(defmethod remove-client-request-log ((log client-request-log) )
  (when (member log *standard-request-logs*)
    (setf *standard-request-logs* (delete log *standard-request-logs*)))) 

;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(define ensure-current-request-log ()
  "Ensures that client logging is enabled, 
creating a log object if necessary of the class, *LOG-ACCESS-LOG-CLASS*."
  (declare (values client-request-logs))
  (mapc #'start-log-queue (standard-request-logs t))
  *standard-request-logs*)

(define create-notification-request-log (predicate notifier &key (name "Notification-Log") (class 'custom-client-notification-log))
  (flet ((equal-log (x)
	   (equalp name (log-name x))))
    (declare (dynamic-extent #'equal-log))
    (check-type predicate function)
    (check-type notifier function)
    (let ((log (find-client-request-log-if #'equal-log)))
      (unless log
	(multiple-value-setq (log)
	  (intern-client-request-log (or name (default-client-log-file-name class))
				     :directory *standard-log-directory*
				     :class class
				     :if-does-not-exist :create)))
      (setf (notification-log-notifier log) notifier
	    (notification-log-predicate log) predicate)
      (log-notifications-on log t)
      (add-client-request-log log)
      log)))

(define current-request-logs ()
  "Returns the current request logs."
  (standard-request-logs))

(define clear-request-logs ()
  "Clears all client logs so that none will be known in the environment."
  (let ((logs *request-logs*))
    (mapc #'log-queue-process-kill logs)
    (setq *request-logs* nil
	  *standard-request-logs* nil)
    logs)) 

(defmethod register-log ((log basic-client-log-mixin))
  (pushnew log *request-logs* :test #'equalp :key #'log-name)) 

(defmethod unregister-log ((log basic-client-log-mixin))
  (setq *request-logs* (delete log *request-logs*))) 

(define allocate-client-request-log (&key (name "CL-HTTP-Request-Log")
					  (local-host (local-host))
					  (class *log-client-log-class*))
  (make-instance class :name name :local-host local-host))

(define intern-client-request-log (name &key (if-does-not-exist :error) (directory *standard-log-directory*)
					host (class *log-client-log-class*))
  (declare (values log newly-created-p))
  (flet ((equal-log-p (x)
	   (and (equalp name (log-name x))
		(typep x class))))
    (declare (dynamic-extent #'equal-log-p))
    (etypecase name
      (string
	(cond
	  ((find-client-request-log-if #'equal-log-p))
	  (t (ecase if-does-not-exist
	       (:soft nil)
	       (:create
		 (let* ((*standard-log-directory* directory)
			(log (allocate-client-request-log :name name :class class :local-host (or host (local-host)))))
		   (add-client-request-log log)
		   (values log t)))
	       (:error (error "Unknown Client HTTP log, ~A." name))))))
      (basic-client-log-mixin name))))

(define-generic unintern-client-request-log (log)
  (:documentation "Removes the client log, LOG from active loging and unregisters it the known client logs."))

(defmethod unintern-client-request-log ((log basic-client-log-mixin))
  (remove-client-request-log log)
  (unregister-log log))

(defmethod initialize-instance :after ((log basic-client-log-mixin) &key &allow-other-keys)
  (with-slots (creation-time) log
    (register-log log)
    (setq creation-time (get-universal-time))
    log)) 

(defmethod %file-name-for-log ((log basic-client-log-mixin))
  (with-slots (log-file-name) log
    log-file-name)) 

;;;------------------------------------------------------------------- 
;;;
;;; RENAME LOG FILES
;;; 

(define switch-all-request-log-files ()
  "Switches all open log files."
  (map-all-request-logs #'switch-log-file))

;; add the daily task to swicth the log files.
(www-utils:add-periodic-task "Switch Client Log Files" :daily '(switch-all-request-log-files))

(define start-all-request-log-queues ()
  "Starts all processes associated with logging."
  (map-all-request-logs #'start-log-queue)) 

(define stop-all-request-log-queues ()
  "Stops all processes associated with logging."
  (map-all-request-logs #'stop-log-queue)) 

;;;------------------------------------------------------------------- 
;;;
;;; 
;;; 

(defmethod client-request ((client client-logging-mixin))
  (concatenate 'string (symbol-name (client-method client))
	       " "
	       (name-string (client-url client))
	       " "
	       (symbol-name (connection-version (client-connection client)))))

(declaim (inline %client-write-common-logfile-entry))

(defun %client-write-common-logfile-entry (client log-stream gmt-p delimiter)
  (%write-common-logfile-entry (client-host-domain-name client)
			       (client-request client)
			       (client-request-time client)
			       (client-status client)
			       (client-bytes-received client)	;total bytes (not number of bytes in a document)
			       nil
			       gmt-p log-stream delimiter))

(defmethod write-common-logfile-entry ((client client-logging-mixin) &optional (log-stream *standard-output*)
                                       (gmt-p *log-times-in-gmt*) (delimiter #\space))
  (with-string-for-null-stream (log-stream)
    (%client-write-common-logfile-entry client log-stream gmt-p delimiter)))

(defmethod snapshot-log-entry ((log common-file-format-mixin) (client client-logging-mixin))
   (let ((host-name (if *server* (server-host-domain-name *server*) (client-host-domain-name client)))
	   (request (client-request client))	; don't lose when transaction reset
	   (request-time (client-request-time client))
           (method (client-method client))
	   (status (client-status client))
	   (bytes-received (client-bytes-received client))	;total bytes
           (bytes-transmitted (client-bytes-transmitted client)))
      (allocate-common-log-entry log host-name request request-time method status bytes-received bytes-transmitted nil)))

(defclass metering-common-log-entry
	  (metering-log-entry-mixin common-log-entry)
    ())

(define-log-entry-allocator allocate-metering-common-log-entry (log host-name request request-time status bytes-transmitted user-name cpu-time elapsed-time)
  (allocate-log-entry metering-common-log-entry
		 :owner log :host-name host-name :request request :request-time request-time :status status
		 :bytes-transmitted bytes-transmitted :user-name user-name :cpu-time cpu-time :elapsed-time elapsed-time))

(defmethod snapshot-log-entry ((log metering-common-file-format-mixin) (client client-logging-mixin))
  (let ((cpu-time (cpu-time client))
	(elapsed-time (elapsed-time client))
	(host-name (if *server* (server-host-domain-name *server*) (client-host-domain-name client)))
	(user-name  nil)
	(request (client-request client))	; don't lose when transaction reset
	(request-time (client-request-time client))
	(status (client-status client))
	(bytes-received (client-bytes-received client)))	;total bytes - not number of bytes in a document
    (allocate-metering-common-log-entry log host-name request request-time status bytes-received user-name cpu-time elapsed-time)))

(declaim (inline %write-metering-common-logfile-entry))

(defun %write-metering-common-logfile-entry (host-name request request-time status bytes user-name cpu-time elapsed-time
						       &optional (gmt-p *log-times-in-gmt*) (stream *standard-output*)
						       (delimiter #\space))
  (macrolet ((write-delimiter (stream)
	       `(write-char delimiter ,stream)))
    (%write-common-logfile-entry host-name request request-time status bytes user-name gmt-p stream delimiter)
    (write-delimiter stream)
    (write cpu-time :base 10 :stream stream)
    (write-delimiter stream)
    (write elapsed-time :base 10 :stream stream)))

(defmethod write-log-entry ((entry-data metering-common-log-entry) stream)
  (with-slots (owner host-name request request-time status bytes-transmitted user-name cpu-time elapsed-time) entry-data
    (%write-metering-common-logfile-entry host-name request request-time status bytes-transmitted user-name cpu-time elapsed-time
					  (log-times-in-gmt-p owner) stream #\space)
    ;; Trailing CR makes this consistently parsable.
    (terpri stream)))

;; replaces bug-report-error-logging-access
(define-generic bug-report-logging-error (agent log condition log-operation)
  (:documentation "Reports a bug while logging HTTP access.
LOG-OPERATION should be the name of the method that got the error."))

(defmethod bug-report-logging-error ((agent client-logging-mixin) (log basic-log-mixin) (condition condition) log-operation)
  (let ((condition-type (type-of condition))
	(log-entry (write-common-logfile-entry agent nil)))
    (declare (dynamic-extent log-entry))
    (report-bug *bug-http-server*
		(format nil "HTTP Client Log Error: ~S" condition-type)
		"~&Log Type: ~S~&Log Operation ~S~&Log Entry:~S ~&Error: ~S~:[~;~&Error Report: ~:*~A~]"
		(type-of log) log-operation log-entry condition-type (report-string condition))))

(define-generic log-client-request (log client)
  (declare (values call-next-method-p))
  (:documentation "Standard method for registering in LOG and access to client.
Methods are combined with AND method combination from most specific to most general
inherited methods.  Only if more specific methods return non-null, are the subsequent
methods called.")
  (:method-combination and)) 

(defmethod log-client-request and ((log file-logging-mixin) (client client-logging-mixin))
  (with-slots (file-logging) log
    (when file-logging
      (write-log-entry-to-file log client))
    t))

(defmethod log-client-request :around ((log basic-log-mixin) (client logging-mixin))
  (handler-case
    (call-next-method)
    (error (err) (bug-report-logging-error client log err 'log-client-request)))) 

(defmethod log-client-request and ((log log-counters-mixin) (client client-logging-mixin))
  (let ((cpu-time (cpu-time client))
	(elapsed-time (elapsed-time client))
	(status (client-status client))
	(method (client-method client))
	(requests-completed (connection-requests-completed client))
	(bytes-transmitted (client-bytes-transmitted client))
	(bytes-received (client-bytes-received client))
	(proxy-p (and *server* (server-proxy-request-p *server*))))
    (update-log-statistics log status method proxy-p requests-completed bytes-transmitted bytes-received elapsed-time cpu-time))
  t) 

(defmethod log-client-request and ((log custom-client-notification-log) (client client-logging-mixin))
  (with-slots (notification predicate notifier) log
    (and notification
	 (funcall predicate client)
	 (funcall notifier client))
    t))

(define-generic client-request-logs (client)
  (:documentation "Returns the client request log objects for client."))

(defmethod client-request-logs ((client client-logging-mixin))
  (standard-request-logs))

;; eliminate this from the calling chain sometime -- JCMa 8/7/1995.
(defgeneric client-log-request (client))

(defmethod client-log-request ((client client-logging-mixin)) 
  (dolist (log (client-request-logs client))
    (log-client-request log client)))

;;;------------------------------------------------------------------- 
;;;
;;; RESOURCING CLIENT OBJECTS
;;; 

;; Only resourcing in genera and MCL pass in the resource to the initializer.
(define-generic initialize-resourced-client (resource client url))

(defmethod initialize-resourced-client (resource (client basic-client-mixin) url)
  (declare (ignore resource)) 
  ;; Initialize the URL first
  (setf (client-url client) url)
  ;; Initialize proxy host and port
  (client-choose-proxy client url)
  (client-trace "~&Allocate Client: ~S" client)
  client)

(defmethod initialize-resourced-client :after (resource (client client-timing-mixin) url)
  (declare (ignore resource url))
  (let ((process (www-utils:current-process)))
    (setf (client-timeout client) *client-timeout*
          (client-process client) process))
  client)

(defun make-client (&optional (class *client-class*))
  "Create a new client object."
  (macrolet ((%make-client (class &rest args)
	       `(case ,class
		  (client (make-instance 'client ,@args))
		  (t  (make-instance ,class ,@args)))))
    (%make-client class 
		  :reply-line (allocate-resource 'line-buffer *line-buffer-size*)
		  :response-headers (allocate-resource 'header-set)))) 

(defun make-resourced-client (resource url)
  (declare (ignore resource url))
  (make-client)) 

(define-generic deinitialize-resourced-client (resource client)
  (:documentation "Deinitializes a client object, deallocating any substructure and resetting instance variable values."))

(defmethod deinitialize-resourced-client (resource (client basic-client-mixin) &aux (connection (client-connection client)))
  (declare (ignore resource))
  ;; critically important to return the connection for a number of platforms.
  (when connection
    (return-connection connection))
  ;; reinitialize instance variables
  (setf (client-connection client) nil
        (client-method client) nil
        (client-url client) nil)
  (client-trace "~&Deallocate Client: ~S" client)
  client)

(defmethod deinitialize-resourced-client :after (resource (client property-list-mixin))
  (declare (ignore resource)) 
  (setf (property-list client) nil)
  client)

(defmethod deinitialize-resourced-client :after (resource (client client-proxy-mixin))
  (declare (ignore resource)) 
  (setf (client-proxy client) nil)
  client) 

(defgeneric initialize-transaction-state (client)
  (:documentation "Initializes client transaction state."))

(defmethod initialize-transaction-state ((client client-timing-mixin))
  (let ((process (client-process client)))
    (setf (client-start-time client) (get-internal-real-time)
	  (client-process-start-time client) (www-utils:process-run-time process))
    client))

(defmethod reset-transaction-state ((client client-transaction-info-mixin))
  (let ((reply-line (client-reply-line client))
	(header-set (client-response-headers client)))
    (cond-every
     (reply-line 
      (setf (fill-pointer (client-reply-line client)) 0))
     (header-set
      (clear-header-set (client-response-headers client) t)))
    ;;reset instance variables
    (setf (%client-reason client) nil
          (client-reason-offset client) 0
          (client-status client) 408
          (%client-request-version client) nil)
    client))

(defmethod reset-transaction-state :after ((client client-timing-mixin))
  (setf (%client-request-time client) nil
	(client-start-time client) 0
	(client-process-start-time client) 0)
  client)

(defgeneric reset-transaction-state-p (client)
  (:documentation "Returns non-null when CLIENT transaction state needs to be reset."))

(defmethod reset-transaction-state-p ((client client-transaction-info-mixin))
  (%client-request-version client))

(defmethod deinitialize-resourced-client :after (resource (client client-transaction-info-mixin))
  (declare (ignore resource))
  (when (reset-transaction-state-p client)
    (reset-transaction-state client))
  ;;reset instance variables
  (setf (client-request-headers client) nil)
  client)

(defmethod deinitialize-resourced-client :after (resource (client client-timing-mixin))
  (declare (ignore resource)) 
  (setf (client-timeout client) 0
	(client-process client) nil)
  client) 

(defun match-http-client-p (resource client url)
  (declare (ignore resource client url))
  t)

(defresource http-client (url)
  :constructor make-resourced-client
  :matcher match-http-client-p
  :initializer initialize-resourced-client
  :deinitializer deinitialize-resourced-client
  :initial-copies 0)

(define clear-client-resource ()
  "Clears the resource of HTTP client objects."
  (clear-resource 'http-client))

(define-generic durable-response-headers (client)
  (:documentation "Returns the response headers for CLIENT after disabling automatic deallocation."))

;; needs to allocate a new header-set in order to keep the client structure usuable.
(defmethod durable-response-headers ((client client-transaction-info-mixin))
  (with-value-cached (client :durable-response-headers)
    (let ((header-set (client-response-headers client)))
      (cond (header-set 
	     (setf (get-value client :durable-response-headers) header-set
		   (client-response-headers client) (allocate-resource 'header-set))
	     header-set)
	    (t (return-from durable-response-headers nil))))))

(define-macro with-standard-client-io-syntax (() &body body)
  "Binds Lisp input/output to standard default values within client instances."
  `(let ((*read-eval* nil)                      ;Turn off throughout the runtime client environment
	 (*read-base* 10.)			;Read numbers in base 10
	 (*print-pretty* nil))			;Turn off pretty printing to prevent weird string emissions
     ,@body))

; formerly called with-client-allocated
(define-macro with-client ((url &key (client-var '*client*)) &body body)
  `(using-resource (,client-var http-client ,url)
     (with-standard-protocol ((url:protocol ,client-var))
       (let ((*standard-proxy* nil))		; reset default proxy to avoid loops and allow selection of proxy client proxies
         ,@body))))

;;;------------------------------------------------------------------- 
;;;
;;; REQUEST
;;;

(declaim (inline %write-request-headers))

(defun %write-request-headers (stream request-headers method http-version)
  "Internal primitive for writing request headers."
  (etypecase request-headers
    (function (funcall request-headers stream method http-version))
    (list (write-request-header-plist stream request-headers method http-version))))

(defun send-request (stream method url http-version request-headers &optional proxy proxy-authorization)
  (declare (optimize (speed 3)))
  ;; write the request
  (fast-format stream "~A " method)
  ;; write request url
  ;; avoid escaping problems by not parsing URLs
  (if proxy
      (write-string (name-string url) stream)	;write entire URL for proxy request (same as WRITE-NAME)
    (write-relative-name-string url stream))	;otherwise write relative URL
  ;; write version
  (fast-format stream " ~A" http-version)
  ;; end the request line
  (send-cr-line-feed stream)
  ;; prepend any proxy authentication
  (cond ((null proxy))
	(proxy-authorization
	 (%write-header :proxy-authorization proxy-authorization stream)
	 (proxy-note-authorization-header proxy proxy-authorization))
	(t (let ((header (proxy-get-authorization-header proxy method url)))
	     (when header
	       (%write-header :proxy-authorization header stream)))))
  ;; send the headers
  (%write-request-headers stream request-headers method http-version)
  ;; end the headers
  (send-cr-line-feed stream)
  (force-output stream))

;;;------------------------------------------------------------------- 
;;;
;;; PARSE REPLY LINE
;;; 

(defun read-reply-line (stream &optional buffer)
  (declare (values line length))
  (multiple-value-bind (line eof delim length)
      (read-delimited-line stream '(#\Linefeed #\Return) t buffer)
    (declare (ignore delim))
    ;;(format t "~&Line: ~S~&EOF: ~S~&Delim: ~S~&Length: ~D" line eof delim length)
    (when eof
      (error 'http-host-stopped-responding :host (foreign-host stream) :port (foreign-port stream)))
    (values line length)))

(defun parse-reply (reply-line &optional (length (length reply-line)) &aux pos1 pos2 pos3)
  (declare (values status-code http-version reason-offset)
	   (fixnum length))
  (flet ((space-p (char) (eql char #\space))
         (validate-http-version (http-version)
           (case http-version
             ((:http/1.1 :http/1.0) http-version)
             #+MacOSX ;;Kludge to handle AMPLE broken HTTP implementation. Used by ITunes under MacOSX -- JCMa 7/28/2006
             (:icy :http/1.0) ; see http://ample.sourceforge.net/index.shtml
             (t (error 'bad-http-version :url (client-url *client*) :format-string "HTTP Client: Bad HTTP version ~S" :format-args (list http-version))))))
    (declare (inline space-p validate-http-version))
    (cond
     ((and (setq pos1 (char-position #\space reply-line 0 length))
           (setq pos2 (%fast-position-if-not space-p reply-line :start pos1 :end length)))
      ;; Client is not required to examine or display the reason
      (setq pos3 (char-position #\space reply-line pos2 length))
      (let ((status-code (parse-integer reply-line :start pos2 :end (or pos3 length) :junk-allowed t))
            (reason-offset (or (and pos3 (%fast-position-if-not space-p reply-line :start pos3 :end length))
                               0))
            (http-version (tokenize-header-keyword reply-line 0 pos1)))
        (when (member *trace-client* '(t :headers))
          (format *trace-output* "~&Response: ~s~&~%" reply-line))
        (values status-code (validate-http-version http-version) reason-offset)))
     (t (error 'bad-server-response :response reply-line :close-connection t
               :format-string "Ill-formed reply from HTTP server, ~S." :format-args (list (copy-seq reply-line)))))))

;;;------------------------------------------------------------------- 
;;;
;;; NETWORK FAILURE RETRIES 
;;;

(define-macro with-network-failure-retries ((client &key (retries '*client-retry-times-for-network-errors*)
                                                    (sleep-interval '*client-retry-sleep-seconds*)
                                                    (report-failures '*client-persistent-connection-report-failures*))
                                            &body body)
  "Execute BODY with retries on network errors.  Releases the client connection on error."
  `(prog ((n-tries 1)
          (retry-limit ,retries))
      try-again
         (handler-case-if 
             (and (not *debug-client*) (< n-tries retry-limit))
            (progn ,@body)
           (host-stopped-responding () 
                                    (incf n-tries) 
                                    (return-connection ,client)
                                    (go try-again))
           (network-error (err)
                          (when ,report-failures
                            (report-bug (email-address-for-bug-reports) "Connection Error"
                                        "~&Error: ~S~&Description: ~A" (type-of err) (report-string err)))
                          (incf n-tries)
                          (process-wait-with-timeout "HTTP Connection Retry" ,sleep-interval #'(lambda () nil))
                          (return-connection ,client)
                          (go try-again))))) 

(define-generic ensure-client-connection (client &optional http-version)
  (declare (values connection new-p))
  (:documentation "Ensure that a client has an open connection to the origin server or a proxy."))

(defmethod ensure-client-connection ((client basic-client-mixin) &optional (http-version *client-http-version*))
  (let ((connection (client-connection client))
	new-p)
    (cond ((and connection (eq :open (connection-state connection)))
           (values connection new-p))
          ;; Allocate a new HTTP connection
          (t ;; Release any existing connection
             (when connection
               ;; make sure to return a closed connection 
               (return-connection connection))
             (let ((proxy (client-proxy client))
                   host port domain-name)
               (if proxy
                   ;; use proxy host if present
                   (multiple-value-setq (host port) 
                       (proxy-host-and-port proxy))
                 ;; Otherwise use the URL host & port
                 (setq host (client-host client)
                       port (client-host-port client)
                       domain-name (client-host-domain-name client)))
               ;; Get a fresh connection 
               (multiple-value-setq (connection new-p)
                   (get-connection (url:protocol client) host port domain-name))
               ;; Set initial HTTP version for connection
               (when new-p
                 (check-type http-version keyword)
                 (let ((known-version (connection-version connection)))
                   (cond ((and known-version (eq known-version http-version)))
                         (known-version
                          (let ((new-version (http-version-min known-version http-version))
                                (version-confirmed-p (connection-version-confirmed-p connection)))
                            (setf (connection-version connection) new-version)
                            (when (and version-confirmed-p
                                       (http-version-less-p known-version http-version))
                              (setf (connection-version-confirmed-p connection) nil))))
                         (t (setf (connection-version connection) http-version)))))
               ;; remember the client connection
               (setf (client-connection client) connection)
               ;; Return the connection
               (values connection new-p))))))

(define transfer-encoding-request-headers (method request-headers)
  "Returns request headers with appropriate TE headers for METHOD."
  (let ((encodings (case method
                     (:get *acceptable-transfer-encodings*)
                     (t nil))))
    (if encodings
        `(:connection (:te) :te (:chunked .,encodings) .,request-headers)
        request-headers)))

(define write-request-header-plist (stream header-plist http-method http-version)
  "Writes request headers when they appear as a header plist."
  (let ((transmitted-headers (case http-version
			       (:http/0.9 header-plist)
			       (:http/1.0 (if (or (not *client-persistent-connections*)
						  (getf header-plist :connection))
					      header-plist
					      `(:connection (:keep-alive) . ,header-plist)))
			       (t (transfer-encoding-request-headers http-method header-plist)))))
    (declare (dynamic-extent transmitted-headers))
    (write-headers stream transmitted-headers nil)))

;; formerly invoke-http-service-on-host
(define-generic invoke-http-service (client method header-writer response-handler &optional request-entity-generator http-version)
  (:documentation "Invokes HTTP service for client on the remote host.
HEADER-WRITER is a function called with (stream method http-version) 
to write the request headers to stream or it is a header plist.
RESPONSE-HANDLER is called with (CLIENT STREAM HTTP-VERSION)
REQUEST-ENTITY-GENERATOR is a function that transmits the body of an HTTP request.
It is called with (CLIENT STREAM HTTP-VERSION). Output is automatically forced on
STREAM.")) 

(defmethod invoke-http-service ((client basic-client-mixin) method header-writer response-handler
				&optional request-entity-generator (http-version *client-http-version*))
  (declare (optimize (speed 3)))
  (flet ((trace-request (url method version header-writer proxy-authorization &aux (trace-stream *trace-output*))
	   (let ((proxy (client-proxy client)))
	     (if proxy
		 (fast-format trace-stream "~&Proxy: ~A (~D)" (proxy-host proxy) (proxy-port proxy))
               (fast-format trace-stream "~&Host: ~A" (host-string url)))
	     (fast-format trace-stream "~%Request: ~A ~I ~A~%Request Headers: ~I" 
			  method (url:write-name url trace-stream) version
			  (progn
			    (when proxy-authorization
			      (%write-header :proxy-authorization proxy-authorization trace-stream))
			    (%write-request-headers trace-stream header-writer method version)))))
	 (%signal-unauthorized-proxy-access (url method http-version headers stream)
	   (destructuring-bind (&optional authentication-method . realm)
	       (get-header :proxy-authenticate headers)
	     (flush-input-entity stream headers http-version)
             (error (ecase authentication-method
                      (:basic 'recoverable-unauthorized-proxy-access)
                      (:digest (if (getf (cdr realm) :stale) 'proxy-access-with-stale-nonce 'recoverable-unauthorized-proxy-access)))
                    :url url :method method :authentication-method authentication-method :authentication-realm realm))))
    (with-standard-client-io-syntax ()
      (handling-proxy-authentication (proxy-authorization)
        (initialize-transaction-state client)
	(with-current-connection (client :http-version http-version :connection-var connection :block-name invoke-http-service)
	  (multiple-value-bind (connection-version connection-version-confirmed-p http-plist)
	      (connection-confirmed-version connection)
	    (let ((url (client-url client))
		  (proxy (client-proxy client))
		  (request-version (if connection-version-confirmed-p (http-version-min connection-version http-version) connection-version))
		  (stream (connection-stream connection)))
	      (setf (client-method client) method
		    (%client-request-version client) request-version)
	      ;; send a request to the remote host
	      (send-request stream method url request-version header-writer proxy proxy-authorization)
	      (when (member *trace-client* '(t :headers))
                (trace-request url method request-version header-writer proxy-authorization))
	      (when request-entity-generator	;Send the request body when provided
		(case request-version
		  ((:http/1.0 :http/0.9)	;1.0 remote server, just send the data.
		   (funcall request-entity-generator client stream request-version)
		   (force-output stream))
		  (t (let (100-continue)
		       (multiple-value-bind (reply reply-length)
			   (cond (connection-version-confirmed-p
				  (ecase (setq 100-continue (getf http-plist :100-continue :unknown))
				    (:implemented
                                     (read-reply-line stream (client-reply-line client)))
				    (:unknown
                                     (with-stream-timeout (stream *client-await-continue-reply-timeout* :error-p nil)
                                       (read-reply-line stream (client-reply-line client))))
				    (:not-implmented
                                     (values nil nil))))
				 (t (with-stream-timeout (stream *client-await-continue-reply-timeout* :error-p nil)
				      (read-reply-line stream (client-reply-line client)))))
			 (cond (reply
				(multiple-value-bind (status server-version reason-offset)
				    (parse-reply reply reply-length)
				  (unless connection-version-confirmed-p
				    (note-server-http-version connection server-version)
				    (setq connection-version-confirmed-p t))
				  (setf (client-status client) status
					(client-reason-offset client) reason-offset)
				  (with-headers-for-client (client stream request-version)	; handles update of connection version
				    (case status
				      (100
                                       (funcall request-entity-generator client stream request-version)
                                       (force-output stream)
                                       (unless (eq 100-continue :implemented)
                                         (note-server-http-property connection :100-continue :implemented))
                                       (clear-header-set (client-response-headers client) t))	; clear continue headers, if any
				      (407	;proxy authentication
                                       (%signal-unauthorized-proxy-access url (client-method client) server-version *headers* (client-stream client)))
				      (417 (signal 'expectation-failed :url url :version server-version))
				      ;; Request was rejected. Handle the rejection response
				      (t (return-from invoke-http-service
					   (multiple-value-prog1	;must return the values returned by the handler
                                               (funcall response-handler client stream request-version))))))))
			       (t (funcall request-entity-generator client stream request-version)
				  (force-output stream)
				  (unless (eq 100-continue :not-implmented)
				    (note-server-http-property connection :100-continue :not-implmented)))))))))
	      (tagbody				;Handle the primary server response
               read-next-reply
               (multiple-value-bind (reply reply-length)
                   (read-reply-line stream (client-reply-line client))
                 (multiple-value-bind (status server-version reason-offset)
                     (parse-reply reply reply-length)
                   (unless (and connection-version-confirmed-p (eq server-version connection-version))	;detect version changes
                     (note-server-http-version connection server-version))
                   (setf (client-status client) status
                         (client-reason-offset client) reason-offset)
                   (with-headers-for-client (client stream request-version)	; handles update of connection version
                     (case status
                       (100 (go read-next-reply))
                       (407 (%signal-unauthorized-proxy-access url (client-method client) server-version *headers* (client-stream client)))
                       (417 (signal 'expectation-failed :url url :version server-version))
                       (t (return-from invoke-http-service
                            (multiple-value-prog1	;must return the values returned by the handler
                                (funcall response-handler client stream request-version))))))))))))))))

(defmethod invoke-http-service ((uri http-uri) method header-writer response-handler &optional request-entity-generator
				(http-version *client-http-version*))
  (declare (optimize (speed 3)))
  (flet ((handle-expectation-failure (condition)
	   (if (http-version-less-p :http/1.0 (http-version condition))
	       (throw 'reinvoke-http-service nil)
             nil)))
    (with-client (uri :client-var *client*)
      (if request-entity-generator
	  (loop with version = http-version
		doing (catch 'reinvoke-http-service
			(handler-bind-if (eq version http-version)
                            ((expectation-failed #'handle-expectation-failure))
			  (return (multiple-value-prog1
                                      (invoke-http-service *client* method header-writer response-handler request-entity-generator version)))))
                (setq version :http/1.0))
        (invoke-http-service *client* method header-writer response-handler request-entity-generator http-version)))))

;; Invocations on urls other than HTTP-URL mean we need an HTTP proxy gateway.
(defmethod invoke-http-service ((uri uri) method header-writer response-handler &optional request-entity-generator
				(http-version *client-http-version*) &aux proxy)
  (with-client (uri :client-var *client*)
    ;; Ensures that the client knows a HTTP accessible proxy that can fulfill the current request
    (unless (and (setq proxy (client-proxy *client*))
		 (proxy-bridges-protocol-p proxy (url:protocol uri)))
      (error 'client-proxy-configuration-error 
             :format-string "Unable to access ~A. No ~A proxy has been specified for the protocol, ~A. "
             :format-args (list uri (url:protocol uri))
             :method method
	     :url (url:name-string uri)
             :version http-version))
    (invoke-http-service *client* method header-writer response-handler request-entity-generator http-version)))

#+ignore
(defmethod invoke-http-service :around ((client basic-client-mixin) method header-generator response-handler
                                        &optional request-entity-generator (http-version *client-http-version*))
  (with-network-failure-retries (client)
    (call-next-method client method header-generator response-handler request-entity-generator http-version))) 

;; formerly with-remote-resource
(define-macro with-http-request ((uri-or-client method &key request-headers request-body) &body response-body)
  "Invokes HTTP service for URI-OR-CLIENT using METHOD.
REQUEST-HEADERS is a function which is called with (STREAM METHOD HTTP-VERSION)
to write the request headers or a header plist.
RESPONSE-BODY is the response-handler for the request.  It can access the
lexical variables: (CLIENT REMOTE-STREAM HTTP-VERSION).
REQUEST-BODY is an advanced option that accepts a s-expression to transmit a
request body. It can access the lexical variables: (CLIENT REMOTE-STREAM
HTTP-VERSION). Output is automatically forced on REMOTE-STREAM."
  `(flet ((response-handler (client remote-stream http-version)
            client http-version remote-stream   ; possibly ignore
            ,@response-body)
          ,@(when request-body
              `((request-handler (client remote-stream http-version)
                                 client http-version    ; possibly ignore
                                 ,request-body))))
     (declare (dynamic-extent #'response-handler
			      ,@(when request-body (list '(function request-handler)))))
     (invoke-http-service ,uri-or-client ,method
                          ,request-headers
                          #'response-handler
                          ,(when request-body `(function request-handler)))))

(defparameter *maximum-redirects* 5
  "Conrtrols the default maximum number of redirects before a too many redirects error is signalled.")

(define-macro handling-redirects ((url &optional (max-redirects '*maximum-redirects*)) &body body)
  "Handles redirects by rerunning BODY with URL bound to the redirected url.
Returns multiple values returned by body."
  `(flet ((do-it (,url) ,@body))
     (declare (dynamic-extent #'do-it))
     (loop named done
           with urls = (ensure-list ,url)
           for count upfrom 0 below ,max-redirects
           for url = (pop urls)
           doing (handler-case
                   (let ((.values. (multiple-value-list (do-it url))))
                     (declare (dynamic-extent .values.))
                     (unless urls
                       (return-from done (values-list .values.))))
                   (document-moved
                     (cond)
                     (unless (setq urls (new-urls cond))
                       (error "Redirect returned no redirection URLS."))
                     #|(format t "~&~D: ~S" count urls)|#))
           finally (error 'too-many-redirects :n-redirects count :url ,url))))

;; this should be converted into a header writer for greater efficiency 11/17/99 -- JCMa.
(defgeneric compute-standard-request-headers (url &key authorization proxy-authorization user-agent range header-plist)
  (:documentation "Add the standard line-browser headers to a header-plist.
RANGE is a list of start end positions indicating one or more byte ranges."))

(defmethod compute-standard-request-headers ((url http-url) &key authorization proxy-authorization (user-agent *server-version*) 
					     range header-plist)
  (macrolet ((push-entry (header value)
	       `(setq headers (list* ,header ,value headers))))
    (let ((headers header-plist))
      (cond-every
	((not (getf header-plist :accept))
	 (push-entry :accept '((:* :*))))
	(range
	  (loop for (start end) on range by #'cddr 
		do (check-type start integer)
		   (check-type end integer)
		collect `(,start ,(1- end)) into spec
		finally (push-entry :range `(:bytes ., spec))))
	(authorization 
	  (push-entry :authorization authorization))
	(proxy-authorization (push-entry :proxy-authorization proxy-authorization))
	(user-agent
	  (push-entry :user-agent user-agent)))
      (push-entry :host `(,(host-string url) ,(host-port url)))
      headers)))

(define-generic flush-input-entity (stream headers http-version)
  (:documentation "Flushes the entity body from input STREAM.")) 

(defmethod flush-input-entity (stream headers (http-version (eql :http/0.9)))
  (declare (ignore headers stream))
  (let ((conn (client-connection *client*)))
    (setf (connection-close-p conn) t)))

(defmethod flush-input-entity (stream headers (http-version (eql :http/1.0)))
  (let ((conn (client-connection *client*))
        content-length)
    (unless (connection-close-p conn)
      (if (setq content-length (get-header :content-length headers))
          (advance-input-buffer stream content-length)
          (setf (connection-close-p conn) t)))))

(defmethod flush-input-entity (stream headers http-version &aux content-length)
  (declare (ignore http-version))
  (cond ((setq content-length (get-header :content-length headers))
         (advance-input-buffer stream content-length))
        (t (let ((transfer-encoding (get-header :transfer-encoding headers)))
             (case transfer-encoding
               (:chunked
                 (with-chunked-transfer-decoding (stream :headers headers)
                   (advance-input-buffer stream)))
               ((nil) (error 'bad-syntax-provided :url (client-url *client*):close-connection t
                             :format-string "No content length header was provided."))
               (t (error 'server-not-implemented :close-connection t :url (client-url *client*)
                         :format-string "The HTTP transfer decoding, ~A, is not implemented."
                         :format-args (list transfer-encoding))))))))



;;;------------------------------------------------------------------- 
;;;
;;; BYTE COUNTS ON CLIENTS
;;;

(define-generic client-bytes-transmitted (client)
  (:documentation "Returns the number of bytes transmitted by CLIENT during the current HTTP transaction."))

(defmethod client-bytes-transmitted ((client basic-client-mixin))
  (connection-bytes-transmitted (client-connection client)))

(define-generic client-bytes-received (client)
  (:documentation "Returns the number of bytes received by CLIENT during the current HTTP transaction."))

(defmethod client-bytes-received ((client basic-client-mixin))
  (connection-bytes-received (client-connection client)))


;;;------------------------------------------------------------------- 
;;;
;;; CLIENT OPERATIONS
;;;

(define-generic client-idle-time (client)
  (:documentation "Returns the idle time in 60ths of a second for client's process,
or NIL when no process exists."))

(defmethod client-idle-time ((client client-timing-mixin))
  (let ((process (client-process client)))
    (and process (www-utils:process-idle-time process))))

(define-generic client-timeout (client)
  (:documentation "Returns the amount of idle time in 60ths of a second before an idle
HTTP connection times out.  A response function can setf this value in
order to allow more or less idle time.  *CLIENT-TIMEOUT* provides the
default value."))

(define-generic client-timeout-p (client)
  (:documentation "Returns non-null when client has timed out.
IDLE-TIME is 60ths of a second."))

(defmethod client-timeout-p ((client client-timing-mixin) &aux idle-time timeout)
   (and (setq idle-time (client-idle-time client))
           (setq timeout (client-timeout client))
           (< timeout idle-time))) 

(define-generic client-process (client)
  (:documentation "Returns the process in which the client is running.")) 

(define-generic client-process-p (client process)
  (:documentation "Returns non-null when client runs in PROCESS."))

(defmethod client-process-p ((client client-timing-mixin) process)
  (eql (client-process client) process))

(define-generic client-process-start-time (client)
  (:documentation "Returns the microsecond time when client's process started computing HTTP service.")) 

(defmethod cpu-time ((client client-timing-mixin))
  (let ((process (client-process client)))
    (if process
	(- (process-run-time process)
	   (client-process-start-time client))
	0)))

(defmethod cpu-milliseconds ((client client-timing-mixin))
  (let ((process (client-process client)))
    (if process
	(truncate (- (process-run-time process) (client-process-start-time client)) 1000)
	0)))

(define-generic client-request-time (client)
  (:documentation "Returns the time in universal time of the request currently being processed by the client.")) 

(defmethod client-request-time ((client client-timing-mixin))
   (or (%client-request-time client)
         ;; If called from inside an HTTP server, then use the server's request time, otherwise cons a new one.
         (let ((server *server*))
            (setf (client-request-time client) (if server (server-request-time server) (get-universal-time))))))

(defmethod (setf client-request-time) (new-value (client client-timing-mixin))
  (with-slots (request-time) client
    (setf request-time new-value)))

(defmethod client-request-time ((client null))
  (get-universal-time))

(define-generic client-start-time (client)
  (:documentation "Returns the time in internal time units when CLIENT's process started computing HTTP service."))

(defmethod elapsed-time ((client client-timing-mixin))
  (- (get-internal-real-time) (client-start-time client))) 

(defmethod elapsed-seconds ((client client-timing-mixin))
  (truncate (elapsed-time client) internal-time-units-per-second))

(defmethod elapsed-milliseconds ((client client-timing-mixin))
  (* (- (get-internal-real-time) (client-start-time client)) #.(float (/ 1000. internal-time-units-per-second))))


;;;------------------------------------------------------------------- 
;;;
;;; AUTOMATIC BUG REPORTING BY ERROR CLASS
;;;

(defmethod bug-report-client-error ((condition condition))
  (let ((condition-type (type-of condition))
	(type-string (typecase condition (error "Error") (t "Condition"))))
    (report-bug *bug-http-server*
                (format nil "Client HTTP ~A: ~S" type-string condition-type)
                "~&Log: ~S~&~A: ~S~:[~;~&~A Report: ~:*~A~]~:[~;~&Backtrace: ~:*~A~]"
                (if *client*
                    (write-common-logfile-entry *client* nil)
                    "NA")
                type-string condition-type type-string (report-string condition)
                (when *stack-backtraces-in-bug-reports*
                  (stack-backtrace-string condition)))))

(pushnew :cl-http-client *features*)


;;;------------------------------------------------------------------- 
;;;
;;;  FINGER FACILITY BUILT ON HTTP STREAMS
;;;

(defun finger (host &optional user whois-p (stream *standard-output*) (no-error-p t) &aux success-p)
  "Uses the finger protocol to find out information about USER at HOST  and reports on STREAM.
HOST is either a fully qualified domain-name or an IP address. USER is either a string or NIL.
When WHOIS-P is non-null and USER is supplied, a whois request is made to HOST."
  (handler-case-if no-error-p
      (with-open-tcp-stream (finger-stream host #.(tcp-service-port-number "Finger" t))
        (when user
          (write-string user finger-stream)
          (when whois-p
            (write-string " /W" finger-stream)))	; send whois flag
        (send-cr-line-feed finger-stream)
        (force-output finger-stream)
        (stream-decode-crlf-until-eof finger-stream stream)
        (setq success-p t))
    (network-error (err) (write-string (report-string err) stream)))
  success-p)

;;;------------------------------------------------------------------- 
;;;
;;;  FILE INTERFACE
;;;

(defgeneric file-url-copy-file (file-url-or-pathname to-stream &key element-type user-id user-pw)
  (:documentation "Copies the content of FROM-PATHNAME to TO-STREAM. 
If a network error is encountered, this returns NIL, otherwise T.
ELEMENT-TYPE is the ANSI file openning argument."))

#-Genera
(defmethod file-url-copy-file ((from-pathname pathname) to-stream &key (element-type *standard-character-type*)
                          (user-id "anonymous") (user-pw (user-mail-address)))
  (let ((host (pathname-host from-pathname)))
    (handler-case 
        (with-automatic-login (host user-id user-pw)
          (with-open-file (file-stream from-pathname :direction :input :element-type element-type)
            (stream-copy-until-eof file-stream to-stream)
            (values t)))
      ;; handle remote connection problems, including dead host, refused connection.
      (remote-network-error () nil))))

(defgeneric file-url-directory-info (directory &optional user-id user-pw)
  (:documentation "Returns a list of pathname spec for directory just like DIRECTORY-INFO.
If a network error is encountered, this returns NIL."))

#-Genera
(defmethod file-url-directory-info ((directory pathname) &optional (user-id "anonymous") (user-pw (user-mail-address)))
  (let* ((path (pathname directory))
         (host (pathname-host path)))
    (handler-case 
        (with-automatic-login (host user-id user-pw)
          ;; ansi CL directory fails due to :fast option  3/13/94 -- JCMa.
          (directory-info directory))
      ;; handle remote connection problems, including dead host, refused connection.
      (remote-network-error () nil))))

(defgeneric file-url-copy-file-to-http-stream (file-url-or-pathname http-stream &key content-type url additional-headers user-id user-pw)
  (declare (values success-p))
  (:documentation "Copies the content of FROM-PATHNAME to TO-STREAM. 
If a network error is encountered, this returns NIL, otherwise T.
ELEMENT-TYPE is the ANSI file openning argument."))

#-Genera
(defmethod file-url-copy-file-to-http-stream ((from-pathname pathname) http-stream &key content-type url additional-headers
                                         (user-id "anonymous") (user-pw (server-mail-address)))
  (declare (values success-p))
  (flet ((handle-invalid-file-user-id-and-password (&rest ignore)
           (declare (ignore ignore))
           (signal 'http::client-unauthorized-file-access :url url :method :get
                   :authentication-realm "FILE Server" :authentication-method :basic)))
    (declare (dynamic-extent #'handle-invalid-file-user-id-and-password))
    (let* ((host (pathname-host from-pathname))
           (copy-mode (url::%content-type-copy-mode content-type nil))
           (element-type (ecase copy-mode
                           (:text *standard-character-type*)
                           ((:binary :crlf) '(unsigned-byte 8)))))
      (with-automatic-login (host user-id user-pw)
        (with-open-file (file-stream from-pathname :direction :input :element-type element-type)
          (http:with-successful-response (http-stream content-type
                                                      :location url :additional-headers additional-headers)
            (case copy-mode
              (:text
               (with-text-stream (http-stream :output)
                 (stream-copy-until-eof file-stream http-stream :text)))
              ((:binary :crlf)
               (with-binary-stream (http-stream :output)
                 (stream-copy-until-eof file-stream http-stream :binary))))
            (values t)))))))

;;;------------------------------------------------------------------- 
;;;
;;;  FTP INTERFACE
;;;

(defun ftp-copy-mode (http-copy-mode)
  "Converts an HTTP copy mode to an FTP copy mode."
  (ecase http-copy-mode
    ((:text :crlf) :text)
    (:binary :binary)))

#+CL-HTTP-FTP-CLIENT
(defmethod ftp-copy-file ((from-url url:ftp-pathname) to-stream &key (user-id "anonymous") (user-pw (user-mail-address)))
  (declare (ignore element-type))
  (flet ((write-filename (stream)
           (url::write-object-name-string from-url stream t))
         (handle-invalid-ftp-user-id-and-password (&rest ignore)
           (declare (ignore ignore))
           (signal 'client-unauthorized-ftp-access :url from-url :method :get :authentication-realm "FTP Server" :authentication-method :basic))
         (handle-ftp-directory-not-found (&rest ignore)
	   (declare (ignore ignore))
	   (signal 'document-not-found :url from-url :method :get :format-string "Directory not found for ~A" :format-args (list (name-string from-url)))))
    (declare (dynamic-extent  #'write-filename))
    (let ((host (host-string from-url))
          (port (host-port from-url))
          (copy-mode (copy-mode from-url))
          ftp-mode)
      (setq ftp-mode (ftp-copy-mode copy-mode))
      (handler-bind-if
          (not *debug-client*)
          ((ftp:ftp-file-not-found #'handle-ftp-directory-not-found)
           (ftp:ftp-not-logged-in #'handle-invalid-ftp-user-id-and-password))
        (ftp:with-ftp-connection (connection host port :passive-p t :user-id user-id :password user-pw)
          (ftp:ftp-change-directory connection (url:path from-url))
          (ftp:ftp-get-file connection #'write-filename to-stream :mode ftp-mode))))))
