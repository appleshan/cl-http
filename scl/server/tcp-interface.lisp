;;;; Server network interface.
;;;
;;; Scieneer Common Lisp port:
;;; (C) Copyright 2006, Scieneer Pty Ltd
;;;     All Rights Reserved.
;;;

(in-package :http)

;;;------------------------------------------------------------------- 
;;;

(defparameter *number-of-listening-processes* 1
  "The number of threads simultaneously listening for HTTP connections.")


;;;------------------------------------------------------------------- 
;;;
;;; 
;;; SET LISP ENVIRONMENT VARIABLES



;;; Resource arg required from server;server
;;;
(resources:defresource
  http-server (stream host address)
  :constructor make-server
  :initializer initialize-resourced-server
  :deinitializer deinitialize-resourced-server
  :initial-copies 0)


;;; Task queue to avoid creating a new thread for each connection, and
;;; to keep resource usage in bounds.

(defvar *task-queue* (thread:make-task-queue
		      :maximum-threads 32
		      :maximum-length 500
		      :priority-levels 4))


;;;------------------------------------------------------------------- 
;;;
;;; LISTENING STREAMS
;;;

(defvar *verbose-connections* nil)

(defclass service ()
  ((address :initarg :address :initform nil :reader service-address)
   (port :initarg :port :initform nil :reader service-port)
   (timeout :initarg :timeout :initform (/ *server-timeout* 60f0)
	    :accessor service-timeout)
   (life-time :initarg :life-time :initform (/ *server-life-time* 1000f0)
	       :accessor service-life-time)
   (backlog :initarg :backlog :initform 100
	    :accessor service-backlog)
   (reuse-address :initarg :reuse-address :initform t
		  :accessor service-reuse-address)
   (priority :initarg :priority :initform 0
	     :accessor service-priority))
  (:documentation "Basic service mixin."))

(defmethod print-object ((service service) stream)
  (with-slots (address port) service
    (print-unreadable-object (service stream :type t :identity t)
      (format stream "~D~:[~; ~:*(~A)~]" port address)
      service)))


(defvar *service-processes* nil
  "An alist of processes awaiting incoming http connections on each service.")


(defclass http-service (service)
  ((protocol :initform :http :reader service-protocol :allocation :class)
   (port :initarg :port :initform 80 :reader service-port))
  (:documentation "HTTP service over TCP."))

(defun %provide-http-service (fd service)
  (declare (type fixnum fd)
	   (type http-service service)
	   (optimize (speed 3)))
  (with-slots (port timeout life-time)
      service
    ;; Create the stream.
    (let* ((binary-stream (scl-http::make-http-stream fd :timeout timeout
						      :expiration life-time))
	   (stream (www-utils::make-http-stream binary-stream))
	   (client-address (www-utils::foreign-host stream))
	   (client-domain-name (host-domain-name client-address)))
      ;; Set the process name.
      (flet ((process-namestring (client-domain-name port)
	       (declare (type simple-base-string client-domain-name))
	       (concatenate 'string "HTTP Server ["
			    (write-to-string port :base 10.)
			    "] (" client-domain-name ")")))
	(setf (mp:process-name (mp:current-process))
	      (process-namestring client-domain-name port)))
      ;;
      (flet ((log-dropped-connection (server)
	       (www-utils:abort-http-stream stream)
	       (set-server-status server 408)
	       (log-access server))
	     (handle-unhandled-error (server error)
	       (handler-case
		   (progn
		     (set-server-status server 500)
		     (log-access server)
		     (report-status-unhandled-error error stream
						    (server-request server))
		     (close stream))	; force output
		 (error ()
		   ;; Make sure it's closed.
		   (www-utils:abort-http-stream stream)))
	       ;; Log the access after the output has been forced.
	       (log-access server)))
	;;
	(resources:using-resource
	 (server http-server stream client-domain-name client-address)
	 (let ((*server* server)) 
	   (handler-case-if (not *debug-server*)
	     (provide-service server)
	     ;; Catch aborts anywhere within server.
	     (http-abort () (www-utils::abort-http-stream stream))
	     (connection-lost () (log-dropped-connection server))
	     (stream-error () (log-dropped-connection server))
	     (sys:io-timeout () (log-dropped-connection server))
	     (error (error) (handle-unhandled-error server error))))))
      ;; Return the streams to the free pools.
      (www-utils::free-http-stream stream t))))


(defun %start-http-service (service)
  "Primitive to begin listening for the HTTP 'service."
  (with-slots (address port backlog priority)
      service
    (flet ((listener ()
	     (declare (optimize (speed 3)))
	     (let ((fd nil))
	       (unwind-protect
		    (progn
		      ;; Try to start the listener - sleep until available.
		      (do ((retry-wait 10 (* 2 retry-wait)))
			  (fd)
			(declare (fixnum retry-wait))
			(handler-case
			    (setf fd (ext:create-inet-listener port :stream
							       :host address
							       :reuse-address t
							       :backlog backlog))
			  (error (condition)
			    (format t "~&Warning: unable to create service on ~
				    port ~d; retry in ~d seconds (~a).~%"
				    port retry-wait condition)
			    (sleep retry-wait))))
		      (loop
		       ;; Wait until ready for input.
		       (let ((thread::*thread-whostate*
			      "Waiting for new HTTP connection"))
			 (mp:process-wait-until-fd-usable fd :input))
		       (let ((new-fd (ignore-errors
				       (ext:accept-tcp-connection fd))))
			 (when *verbose-connections*
			   (format t "* accept new connection fd=~s ~
				    new-fd=~s port-~s~%" fd new-fd port))
			 (cond ((not new-fd))
			       ((> *number-of-connections*
				   *reject-connection-threshold*)
				(scl-http::lingering-close new-fd))
			       ((not *task-queue*)
				(mp:make-process 
				 #'(lambda ()
				     (%provide-http-service new-fd service))))
			       ((thread:queue-task *task-queue*
					#'(lambda ()
					    (%provide-http-service new-fd
								   service))
					priority))
			       (t
				;; Task queue overflow. Drop the connection.
				(scl-http::lingering-close new-fd))))))
		 ;; Close the listener stream.
		 (when fd
		   (unix:unix-close fd))))))

      ;; Make the listening thread.
      (let ((service-process
	     (mp:make-process
	      #'listener :name (format nil "HTTP Service on port ~d" port))))
	(push (cons service service-process) *service-processes*)))))



(defvar *standard-https-port* 443)

(defclass https-service (http-service)
  ((protocol :initform :https :reader service-protocol :allocation :class)
   (port :initform 443)
   (certificate :initarg :certificate :initform nil)
   (private-key :initarg :private-key :initform nil)
   (verify-mode :initarg :verify-mode :initform nil)
   (verify-depth :initarg :verify-depth :initform nil)
   (options :initarg :options :initform nil)
   ;; SSL context parameters.
   (parameters :initarg :parameters :initform nil)
   (password :initarg :password :initform nil)
   (ciphers :initarg :ciphers :initform nil)
   (cafile :initarg :cafile :initform nil)
   (capath :initarg :capath :initform nil)
   (ssl-context :initarg :ssl-context :initform nil)
   )
  (:documentation "HTTP service inside an SSL tunnel over TCP."))

(defun %provide-https-service (fd service)
  ;; Create the stream.
  (with-slots (port timeout life-time certificate private-key
	       verify-mode verify-depth options ssl-context)
      service
    (let ((binary-stream nil))
      (unwind-protect
	   (setf binary-stream
		 (ignore-errors
		   (scl-http::make-https-stream fd
						:certificate certificate
						:key private-key
						:verify-mode verify-mode
						:verify-depth verify-depth
						:options options
						:ssl-context ssl-context
						:timeout timeout
						:expiration life-time)))
	(unless binary-stream
	  (scl-http::lingering-close fd)))
      (when binary-stream
	(let* ((stream (www-utils::make-http-stream binary-stream))
	       (client-address (www-utils::foreign-host stream))
	       (client-domain-name (host-domain-name client-address)))
	  ;; Set the process name.
	  (flet ((process-namestring (client-domain-name port)
		   (declare (type simple-base-string client-domain-name))
		   (concatenate 'string "HTTPS Server ["
				(write-to-string port :base 10.)
				"] (" client-domain-name ")")))
	    (setf (mp:process-name (mp:current-process))
		  (process-namestring client-domain-name port)))
	  ;;
	  (flet ((log-dropped-connection (server)
		   (www-utils:abort-http-stream stream)
		   (set-server-status server 408)
		   (log-access server))
		 (handle-unhandled-error (server error)
		   (handler-case
		       (progn
			 (set-server-status server 500)
			 (log-access server)
			 (report-status-unhandled-error error stream
						(server-request server))
			 (close stream)) ; force output
		     (error ()
		       ;; Make sure it's closed.
		       (www-utils:abort-http-stream stream)))
		   ;; Log the access after the output has been forced.
		   (log-access server)))
	    ;;
	    (resources:using-resource
	     (server http-server stream client-domain-name client-address)
	     (let ((*server* server)) 
	       (handler-case-if (not *debug-server*)
		 (provide-service server)
		 ;; Catch aborts anywhere within server.
		 (http-abort () (www-utils::abort-http-stream stream))
		 (connection-lost () (log-dropped-connection server))
		 (stream-error () (log-dropped-connection server))
		 (sys:io-timeout () (log-dropped-connection server))
		 (error (error) (handle-unhandled-error server error)))))))))))

(defun ssl-cipher-string (cipher-keyword-or-string)
  (etypecase cipher-keyword-or-string
    (keyword
     (ecase cipher-keyword-or-string
       (:all "ALL") ;all ciphers suites except the eNULL ciphers which must be explicitly enabled.
       (:high "HIGH") ;Currently means key lengths larger than 128 bits.
       (:medium "MEDIUM") ;Currently means ciphers using 128 bit encryption.
       (:low "LOW") ;Currently means ciphers using 64 or 56 bit encryption algorithms but excluding export cipher suites.
       (:56-bit-export "EXPORT56") ;56 bit export encryption algorithms
       (:export "EXPORT") ;export encryption algorithms. Including 40 and 56 bits algorithms.
       (:40-bit-export "EXPORT40"))) ;40 bit export encryption algorithms
    ;; Should be validate OpenSSL cipher string.
    ;; See: http://www.openssl.org/docs/apps/ciphers.html
    (string cipher-keyword-or-string)))

(defun %start-https-service (service)
  ;; Initialize the SSL context.
  (with-slots (parameters password ciphers cafile capath)
      service
    (ssl:initialize-ssl-context :password password
				:dh-params parameters
				:ciphers ciphers
				:cafile cafile
				:capath capath))
  ;;
  (with-slots (address port backlog reuse-address priority)
      service
    (flet ((listener ()
	     (declare (optimize (speed 3)))
	     (let ((fd nil))
	       (unwind-protect
		    (progn
		      ;; Try to start the service - sleep until available.
		      (do ((retry-wait 10 (* 2 retry-wait)))
			  (fd)
			(declare (fixnum retry-wait))
			(handler-case
			    (setf fd (ext:create-inet-listener port :stream
							       :host address
							       :reuse-address t
							       :backlog backlog))
			  (error (condition)
			    (format t "~&Warning: unable to create service on ~
				    port ~d; retry in ~d seconds (~a).~%"
				    port retry-wait condition)
			    (sleep retry-wait))))
		      (loop
		       ;; Wait until input ready.
		       (let ((thread::*thread-whostate*
			      "Waiting for new HTTPS connection"))
			 (mp:process-wait-until-fd-usable fd :input))
		       (let ((new-fd (ignore-errors
				       (ext:accept-tcp-connection fd))))
			 (when *verbose-connections*
			   (format t "* accept new connection fd=~s ~
				    new-fd=~s port-~s~%" fd new-fd port))
			 (cond ((not new-fd))
			       ((> *number-of-connections*
				   *reject-connection-threshold*)
				(scl-http::lingering-close new-fd))
			       ((not *task-queue*)
				(mp:make-process 
				 #'(lambda ()
				     (%provide-http-service new-fd service))))
			       ((thread:queue-task *task-queue*
				#'(lambda ()
				    (%provide-https-service new-fd service))
				priority))
			       (t
				;; Task queue overflow. Drop the connection.
				(scl-http::lingering-close new-fd))))))
		 ;; Close the listener stream.
		 (when fd
		   (unix:unix-close fd))))))

      ;; Make the listening thread.
      (let ((service-process
	     (mp:make-process
	      #'listener :name (format nil "HTTPS Service on port ~d" port))))
	(push (cons service service-process) *service-processes*)))))

;;;------------------------------------------------------------------- 
;;;
;;; HIGH LEVEL OPERATIONS TO CONTROL HTTP SERVICE 
;;;

(defvar *services* nil
  "The http services list.")

(defun %coerce-address (address)
  (typecase address
    (keyword
     (ecase address
       (:all nil)
       (:primary (local-host-ip-address))
       (:loopback "127.0.0.1")))
    (t
     address)))

(defun find-service (port &optional (address 0))
  (let ((address (%coerce-address address)))
    (find-if #'(lambda (service)
		 (and (eql (service-port service) port)
		      (eql (service-address service) address)))
	     *services*)))

(defun find-port-service (port)
  (find port *services* :key 'service-port :test 'eql))

(defun www-utils::port-protocol (port)
  "Returns a keyword denoting the protocol specified for PORT,
or NIL if no protocol has been defined for PORT."
  (let ((service (find-port-service port)))
    (when service
      (service-protocol service))))

(defun www-utils::port-address (port)
"Returns the IP address for the network interface associated with PORT.
When PORT is accessible over all interfaces, this returns the local
host domain name.  When no service is defined for port, it returns
nil. The second value indicates whether service is restricted to this
network interface."
  (let ((service (find-port-service port)))
    (when service
      (let ((address (service-address service)))
        (if address
            (values address t)
	    (values (local-host-domain-name) nil))))))

(defun define-http-service (&key (address :all) (port *standard-https-port*)
			    (backlog 5)
			    (reuse-address t)
			    (priority 0)
			    (timeout *server-timeout*)
			    (life-time *server-life-time*))
  ;; Remove existing service.
  (let ((current-service (find-service port address)))
    (setf *services* (remove current-service *services*)))
  ;;
  (let ((service (make-instance 'http-service
				:address (%coerce-address address)
				:port port
				:backlog backlog
				:reuse-address reuse-address
				:priority priority
				:timeout (/ timeout 60f0)
				:life-time (/ life-time 1000f0))))
    (push service *services*)
    service))

(export 'define-http-service :http)

(defun convert-ssl-version-for-scl (version)
  (ecase version
    (:ssl-default :sslv23)
    (:ssl-2-or-3 :sslv23)
    (:ssl-3 :sslv3)
    (:ssl-2 :sslv2)
    (:tls-1 :tls-1)))

(defun define-https-service (&key (address :all) (port *standard-https-port*)
			     (backlog 5)
			     (reuse-address t)
			     (priority 0)
			     (timeout *server-timeout*)
			     (life-time *server-life-time*)
			     certificate private-key
			     (verify :never) verify-depth
			     options
			     (password :prompt) parameters
			     ciphers
			     certificate-authorities capath
			     (ssl-version :ssl-default)
			     enable-service-p)
  (declare (type (or null cl::path-designator) certificate private-key)
	   (type (or null (integer 0 1024)) verify-depth)
	   (type (or null (unsigned-byte 32)) options)
	   (ignore enable-service-p))
  ;; Remove existing service.
  (let ((current-service (find-service port address)))
    (setf *services* (remove current-service *services*)))
  ;;
  (let* ((ciphers (and ciphers (ssl-cipher-string ciphers)))
	 (%ssl-context (convert-ssl-version-for-scl ssl-version))
	 (verify-mode (ecase verify
			(:never :none)
			(:always :require)
			(:once :request)))
	 (password (if (eq password :prompt) nil password))
	 (ssl-context (ssl:make-ssl-context :method %ssl-context
					    :password password
					    :dh-params parameters
					    :ciphers ciphers
					    :cafile certificate-authorities
					    :capath capath))
	 (service (make-instance 'https-service
				 :address (%coerce-address address)
				 :port port
				 :backlog backlog
				 :reuse-address reuse-address
				 :priority priority
				 :timeout (/ timeout 60f0)
				 :life-time (/ life-time 1000f0)
				 :certificate certificate
				 :private-key private-key
				 :verify-mode verify-mode
				 :verify-depth verify-depth
				 :options options
				 :ssl-context ssl-context)))
    (push service *services*)
    service))

(export 'define-https-service :http)

(define listening-on-http-ports ()
  "Returns the ports on which the server is listening for http connections."
  (declare (values http-port-numbers))
  (let ((ports nil))
    (dolist (service *service-processes*)
      (pushnew (service-port (car service)) ports))
    (nreverse ports)))

(export 'listening-on-http-ports :http)

(defun host-interface-name (address)
  (cond ((or (not address) (eql address 0))
	 "all interfaces")
	(t
	 (let* ((entry (ext:lookup-host-entry address)))
	   (unless entry
	     (error "Unknown address: ~S~%" address))
	   (let ((name (ext:host-entry-name entry))
		 (ip-address
		  (ip-address-for-parsed-ip-address
		   (extensions::host-entry-addr entry))))
	     (cond ((equalp name ip-address)
		    (format nil "interface ~A" name))
		   (t
		    (format nil "interface ~A [~A]" name ip-address))))))))

(defmethod report-service-enabled ((service service))
  (with-slots (port address) service
    (let ((protocol (service-protocol service)))
      (expose-log-window)
      (notify-log-window "~A Service Enabled on port ~D of ~A." protocol port
			 (host-interface-name address)))))

;; Disabling a proxy on a port also disables any server running there.
;; More infrastructure needed to tell if only the proxy should be disabled. -- JCMa 3/25/2006
(defmethod report-service-disabled ((service service))
  (with-slots (protocol port address) service
    (expose-log-window)
    (notify-log-window "~A Service Disabled on port ~D of ~A." protocol port
		       (host-interface-name address))))

(define disable-http-service (&key (on-ports :all))
  "Top-level method for shutting down HTTP servers."
  (let ((ports (typecase on-ports
		 (integer
		  (list on-ports))
		 (cons
		  on-ports)
		 ((member :all)
		  (listening-on-http-ports))))
	(rem-services nil))
    (dolist (service-process-cons *service-processes*)
      (let* ((service (car service-process-cons))
	     (port (service-port service)))
	(cond ((member port ports)
	       (mp:destroy-process (cdr service-process-cons))
	       (%decache-local-port-context port)
	       (report-service-disabled service))
	      (t
	       (push service-process-cons rem-services)))))
    (setf *service-processes* rem-services)))

(define http-service-enabled-p (&optional (ports `(8000)))
  "Returns the ports on which HTTP service is enabled or NIL if HTTP is
  enabled on none of PORTS. PORTS is a list of port numbers. :ANY matches
  any port." 
  (cond ((null ports)
	 (error "No ports specified  in ports."))
	((and (member :any ports) *service-processes*)
	 ports)
	(t 
	 (let ((enabled-ports nil))
	   (dolist (service-process-cons *service-processes*)
	     (let ((port (service-port (car service-process-cons))))
	       (when (member port ports)
		 (pushnew port enabled-ports))))
	   (nreverse enabled-ports)))))

(defgeneric ensure-http-service (service))

(defmethod ensure-http-service ((service http-service))
  (unless (assoc service *service-processes*)
    (%start-http-service service)
    (with-slots (protocol port)
	service
      (%decache-local-port-context port)
      (report-service-enabled service))))

(defmethod ensure-http-service ((service https-service))
  (unless (assoc service *service-processes*)
    (%start-https-service service)
    (with-slots (protocol port)
	service
      (%decache-local-port-context port)
      (report-service-enabled service))))

(defun ensure-http-protocol-on-port (port)
  (let ((service (find-port-service port)))
    (unless service
      (setf service (make-instance 'http-service :port port))
      (push service *services*))
    (ensure-http-service service)))
  
(define enable-http-service (&key (on-ports (listening-on-http-ports)))
  "Top-level method for starting up HTTP servers."
  (cond ((eq on-ports :all)
	 (setf on-ports (mapcar 'service-port *services*)))
	(t
	 (setf on-ports (ensure-list on-ports))))
  ;; Shutdown any services that may be awake.
  (disable-http-service :on-ports on-ports)
  ;; before starting a new batch
  (let ((all-ports nil))
    (dolist (service *services*)
      (let ((port (service-port service)))
	(pushnew port all-ports)
	(when (member port on-ports)
	  (ensure-http-service service))))
    ;; 
    (dolist (port on-ports)
      (unless (member port all-ports)
	(let ((service (define-http-service :port port)))
	  (ensure-http-service service)))))
  on-ports)


;;; Stream timeout and expiration are used to implement the CL-HTTP server
;;; timeout and life-time.  The server resets these at the start of a new
;;; request.

(defmethod (setf server-timeout) :after (timeout server)
  (let ((stream (server-stream server)))
    (assert stream)
    (let ((timeout (/ timeout 60f0)))
      (setf (ext:stream-timeout stream) timeout)))
  timeout)

(defmethod (setf server-life-time) :after (time server)
  (let ((stream (server-stream server)))
    (assert stream)
    (let ((time (/ time 1000f0)))
      (setf (ext:stream-expiration stream) time)))
  time)


;;;------------------------------------------------------------------- 
;;;
;;; SPECIALIZED HTTP STREAM POLLING
;;;

(defmethod http-input-data-available-p (stream &optional timeout-seconds)
  (declare (type (or null fixnum) timeout-seconds)
	   (optimize (speed 3)))
  (loop
   (unless (www-utils:live-connection-p stream)
     (return nil))
   (let ((char (read-char-no-hang stream nil :eof)))
     (cond ((eq char :eof)
	    (return nil))
	   ;; Clear any dangling White Space due to buggy clients.
	   ((member char '(#\return #\linefeed #\space #\tab)
		    :test #'eql)
	    (read-char stream t))
	   (char
	    (unread-char char stream)
	    (return t))
	   ((and timeout-seconds (not (zerop timeout-seconds))
		 (mp:process-wait-until-fd-usable
		  (sys::fd-stream-fd stream)
		  :input timeout-seconds))
	    (return t))
	   (t
	    (return nil))))))
