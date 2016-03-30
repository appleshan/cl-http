;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10 -*-

;;; Copyright 1995, 2005-2006, 2009, John C. Mallery.
;;; All rights reserved.
;;;
;;; Copyright (C) 1995, OBC.
;;; All rights reserved on changes for portability to non MCL platforms.
;;;
;;; LispWorks enhancements Copyright (C) 1995-2000 Xanalys Inc.  All rights reserved.
;;;

;;;------------------------------------------------------------------- 
;;;
;;; MULTI-THREADED SERVER INTERFACE 
;;;

(in-package :http)

;;;------------------------------------------------------------------- 
;;;
;;;  STREAM AND SERVER RESOURCES
;;;

(defparameter *listener-process-priority* 0
  "The process priority of HTTP listener processes.")

(defparameter *server-run-interval* 12.
  "The interval of execution quanta allocated for each scheduling of an HTTP thread.")

;; Adjust stream read time out according to values in the server
;; object, which may be supplied on a URL specific basis.
#-LispWorks3.2
(defmethod (setf server-timeout) :after ((timeout integer) (server basic-server-mixin))
  (let ((stream (server-stream server)))
    (when stream
      (setf (stream:stream-read-timeout stream) timeout))
    timeout))

(defparameter *connection-backlog* 5
  "The number of pending connections queued for accept. Connection backlogs greater than this number are dropped.
Production servers should set the number higher to allow, for example, a 5 second backlog of pending connections.")

(export '*connection-backlog* :http)


;;;------------------------------------------------------------------- 
;;;
;;;  RESOURCED HTTP PROCESSES
;;;

(defvar *process-index* 0)

(defun http-process-control (process)
  (getf (mp:process-plist process) 'launch-process-function))

(defun (setf http-process-control) (control process)
  (setf (getf (mp:process-plist process) 'launch-process-function)
        control))

(defun make-http-process (resource name priority quantum)
  (declare (ignore resource name priority quantum) 
           (values process))
  (let* ((idx (incf *process-index*))
	 (name (concatenate 'string "HTTP Process "
                            (with-output-to-string (str)
                              (write idx :escape nil :base 10. :stream str))))
         (process (clim-sys:make-process 'http-process-main-loop :name name)))
    (setf (http-process-control process) (list nil))
    process))

(defun initialize-http-process (resource process name priority quantum)
  (declare (ignore resource name quantum))
  (mp::change-process-priority process priority))

(defun deinitialize-http-process (resource process)
  (declare (ignore resource))
  (mp::change-process-priority process -1)
  process)

(defun http-process-main-loop ()
  (declare (optimize (speed 3)))
  (let ((process mp:*current-process*))
    (mp:process-wait "Waiting to initialize." 'http-process-control process)
    (let ((control (http-process-control process)))
      (loop
       (with-simple-restart (abort "Restart HTTP Process")
         ;; Wait here until someone sets the function.
         (mp:process-wait "Waiting to be regenerated."
                          #'(lambda (control)
                              (car control))
                          control)
         (unwind-protect
             (apply (car control) (cdr control))
           (progn
             (setf (car control) nil
                   (cdr control) nil)
             (resources:deallocate-resource 'http-process process))))))))

(defun launch-process (name keywords function &rest args)
  (declare (optimize (speed 3)))
  (destructuring-bind (&key (priority 0) (quantum *server-run-interval*)) keywords
    (let* ((process (resources:allocate-resource 'http-process name priority quantum))
           (control (http-process-control process)))
      (setf (cdr control) args ; set cdr first since car is the wait flag
            (car control) function)
      process)))


;;;------------------------------------------------------------------- 
;;;
;;; RESOURCED HTTP STREAMS 
;;;

(declaim (inline http-stream-connection-state))

(defun http-stream-connection-state (stream)
  "Returns a Standard keyword describing the connection state of stream."
  ;; Only two states required - see callers
  (cond ((live-connection-p stream) :established)
	(t :closed)))

;;; Resource arg required from server;server
;;;
(defmethod deinitialize-resourced-server :before (resource (server basic-server-mixin))
  (declare (ignore resource))
  (with-slots (stream) server
    (when stream
      (multiple-value-bind (what error)
	  (ignore-errors (close stream))
	(cond (error
	       (warn "Ignoring error")
	       (describe error #-LispWorks3.2 *trace-output*))
	      (t what))))))

(defresource
  http-server (stream host address)
  :constructor make-server
  :initializer initialize-resourced-server
  :deinitializer deinitialize-resourced-server
  :initial-copies 0)

(defresource
  http-process (name priority quantum)
  :constructor make-http-process
  :initializer initialize-http-process
  :deinitializer deinitialize-http-process
  :initial-copies 0)


;;;------------------------------------------------------------------- 
;;;
;;; LISTENING STREAMS
;;;

(defun %provide-service (stream)
  (declare (optimize (speed 3)))
  (let* ((client-address (www-utils::foreign-host stream))
         (client-domain-name (ip-address-for-parsed-ip-address client-address)))
    (using-resource
        ;; Notice the resource name HTTP-SEVER is NOT quoted
        (server http-server stream client-domain-name client-address)
      (flet ((abort-service (error)
               (declare (ignore error))
               (www-utils:abort-http-stream stream)
               (return-from %provide-service t))
             (log-dropped-connection (error)
               (declare (ignore error))
               (www-utils:abort-http-stream stream)
               (set-server-status server 408)       ;client timeout status code changed from 504 -- JCMa 5/29/1995.
               (log-access server)
               (return-from %provide-service t))
             (log-ssl-connection-error (error)
               (declare (ignore error))
               (www-utils:abort-http-stream stream)
               (set-server-status server 403)       ; 403.4 See www.wats.ca/resources/httperrorcodes/24 -- Update CL-HTTP conditions some time -- JCMa 3/25/2006
               (log-access server)
               (return-from %provide-service t))
             (handle-unhandled-error (error)
               (handler-case
                   (progn
                     (set-server-status server 500)
                     (log-access server)
                     (report-status-unhandled-error error stream (server-request server))
                     (close stream))                  ; force output
                 (network-error () (close stream :abort t))
                 #+CL-HTTP-SSL
                 (ssl-condition () (close stream :abort t))
                 (condition (condition) ;handle error or condition -- JCMa 12/1/2003
                   (ignore-errors (http::bug-report-error condition))
                   (close stream :abort t)))
               ;; log the access after the output has been forced.
               (log-access server)
               (return-from %provide-service t)))
        (let ((*server* server)
              #-LispWorks3.2 ;; Use  read timeout passed to stream from http service as default timeout, so reseting of transactions works correctly
              (*server-timeout* (stream:stream-read-timeout stream))) 
          (handler-bind-if
              (not *debug-server*)
              ;; catch aborts anywhere within server.-- JCMa 12/30/1994.
              ((http-abort #'abort-service)
               (protocol-timeout #'log-dropped-connection)
               (bad-connection-state #'log-dropped-connection)
               #+CL-HTTP-SSL
               (ssl-closed #'log-dropped-connection)
               #+CL-HTTP-SSL
               (ssl-failure #'log-ssl-connection-error)
               ;; Condition is not a subclass of error but might escape -- JCMa 11/21/2003
               (condition #'handle-unhandled-error))
            (progn 
              (provide-service server)
              (close stream))))))))

(define accept-connection (stream port)
  (declare (optimize (speed 3))
           (ignore port))
  (flet ((accept-connection-p ()
           (< *number-of-connections* *reject-connection-threshold*))
         (reject-connection (stream string)
           (when string
             (write-string string stream)
             (force-output stream))
           (close stream :abort t)))
    (declare (inline accept-connection-p reject-connection process-namestring))
    (cond ((accept-connection-p)
           ;; set the timeout to the connected value?
           (launch-process "?" '(:priority 0) #'%provide-service stream))
	  (t (reject-connection stream *reject-connection-message*)))))


;;;------------------------------------------------------------------- 
;;;
;;; FAST TESTS FOR WHETHER A PORT IS ACTIVE
;;;

(defvar *http-ports* nil   ; typically defaults to standard port, which is normally 80
  "The ports over which http service is provided.")

(define listening-on-http-ports ()
  "Returns the ports on which the server is listening for http connections."
  (declare (values http-port-numbers))
  *http-ports*)

(export 'listening-on-http-ports :http)

#+(or LispWorks4 LispWorks5)
(defun set-listening-on-http-ports (http-port-numbers)
  (cond ((every #'integerp http-port-numbers)
         (lispworks:without-interrupts
          (unless (eq http-port-numbers *http-ports*)
            (setq *http-ports* http-port-numbers)))
	 *http-ports*)
	(t (error "Every listening port must be a number."))))

#+(and LispWorks (not (or LispWorks4 LispWorks5)))
(defun set-listening-on-http-ports (http-port-numbers)
  (cond ((every #'integerp http-port-numbers)
         ;; assumes no thread safety needed
         (setq *http-ports* http-port-numbers)
	 *http-ports*)
	(t (error "Every listening port must be a number."))))

(defsetf listening-on-http-ports set-listening-on-http-ports)

(declaim (ftype (function) http:http-service-enabled-p))

(define http-service-enabled-p (&optional (ports (listening-on-http-ports)))
  "Returns non-null when HTTP is enabled on PORTS.
PORTS is normally a list of port numbers and defaults to the currently
enabled set of HTTP ports. If any port in PORTS is not enabled for HTTP,
this returns null. PORTS may also be a single port number, in which case
this returns non-null when HTTP is enabled on it. When supplied with the
keyword :ANY, this returns non-null when HTTP is enabled on some port."
  (flet ((http-service-on-port-p (port)
           (declare (fixnum port))
           (member port *http-ports*))
         (any-active-http-service-p ()
           *http-ports*))
    (declare (inline http-service-on-port-p any-active-http-service-p))
    (etypecase ports
      (integer ; called every request
       (http-service-on-port-p ports))
      (cons 
       (if (member :any ports)
           (any-active-http-service-p)
         (loop for port in ports
               unless (http-service-on-port-p port)
               return nil
               finally (return ports))))
      (null (any-active-http-service-p))
      (symbol (cond ((eql ports :any)
                     (any-active-http-service-p))
                    (t (error "Unknown keyword,~S, specified  in PORTS." ports)))))))


;;;------------------------------------------------------------------- 
;;;
;;; DEFINITION OF SERVICES ON PORTS
;;;

(defclass service
          ()
  ((address :initarg :address :accessor service-address)
   (port :initarg :port :accessor service-port :type fixnum)
   (process :initform nil :initarg :process :accessor service-process)
   (timeout :initarg :timeout :accessor service-timeout :type (integer 0))
   (process-priority :initarg :process-priority :accessor service-process-priority :type fixnum)
   (backlog :initarg :backlog :accessor service-backlog :type (fixnum 0))
   (process-string :initform nil :accessor %service-process-string))
  (:documentation "Basic service mixin."))

(defclass http-service
          (service)
  ((protocol :initform :http :reader service-protocol :allocation :class)
   (port :initform 80))
  (:documentation "HTTP service over TCP."))

(defclass https-service
          (service)
  ((protocol :initform :https :reader service-protocol :allocation :class)
   (port :initform 443)
   (ctx :initarg :ssl-ctx :writer %ssl-ctx) ;C pointer
   (parameters :initarg :parameters :accessor ssl-parameters :type pathname)
   (certificate :initarg :certificate :accessor ssl-certificate :type pathname)
   (private-key :initarg :private-key :accessor ssl-private-key :type pathname)
   (password :initarg :password :accessor ssl-password :type string)
   (ciphers :initarg :ciphers :accessor ssl-ciphers :type string)
   (version :initarg :version :accessor ssl-version :type keyword)
   (certificate-authorities :initarg :certificate-authorities :accessor ssl-certificate-authorities :type pathname)
   (client-certificates :initarg :client-certificates :accessor ssl-client-certificates :type pathname)
   (client-ca-list-ptr :initarg :client-ca-list-ptr) ;C pointer
   (verify-mode :initarg :verify-mode :accessor ssl-verify-mode :type keyword)
   (verify-depth :initarg :verify-depth :accessor ssl-verify-depth :type fixnum)
   (security-parameters :initarg :security-parameters :accessor ssl-security-parameters :type (cons keyword)))
  (:documentation "HTTP service inside an SSL tunnel over TCP."))

(defmethod print-object ((service service) stream)
  (with-slots (address port process) service
    (print-unreadable-object (service stream :type t :identity t)
      (format stream "~D~:[~; ~:*(~A)~] ~:[Stopped~;Running~]" port address process)
      service)))

(defvar *service-alist* nil
  "The ports over which network service is provided.")

(defun map-services-if (predicate function)
  (cond ((eq predicate t)
         (mapc #'(lambda (entry)
                   (funcall function (cdr entry)))
               *service-alist*))
        (t (mapc #'(lambda (entry)
                     (when (funcall predicate (cdr entry))
                       (funcall function (cdr entry))))
                 *service-alist*))))

(defun %get-service-on-port (port)
  (cdr (assoc port *service-alist* :test #'=)))

(defmethod %unregister-service ((service service))
  (with-slots (port) service
    (let ((entry (assoc port *service-alist* :test #'=)))
      (when (and entry (eq (cdr entry) service))
        (let ((port (service-port service)))
          (deinitialize-service service)
          (setq *service-alist* (delete entry *service-alist*))
          (%decache-local-port-context port)
          t)))))

(defmethod deinitialize-service ((service service))
  (%stop-listening-for-connections service))

#+CL-HTTP-SSL
(defmethod deinitialize-service :after ((service https-service))
  (ssl-ctx-deinitialize service))

(defmethod %register-service-on-port ((service service))
  (with-slots (port) service
    (let ((entry (assoc port *service-alist* :test #'=)))
      (cond (entry
             (let ((old (cdr entry)))
               (unwind-protect
                   (setf (cdr entry) service)
                 (deinitialize-service old))))
            (t (push (list* port service) *service-alist*))))))

(defun www-utils::port-protocol (port)
  "Returns a keyword denoting the protocol specified for PORT,
or NIL if no protocol has been defined for PORT."
  (let ((service (%get-service-on-port port)))
    (when service
      (service-protocol service))))

(defun www-utils::port-address (port)
"Returns the IP address for the network interface associated with PORT.
When PORT is accessible over all interfaces, this returns the local
host domain name.  When no service is defined for port, it returns
nil. The second value indicates whether service is restricted to this
network interface."
  (declare (values address restricted-to-this-interface-p))
  (let ((service (%get-service-on-port port)))
    (when service
      (let ((address (service-address service)))
        (if address
            (values address t)
          (values (local-host-domain-name) nil))))))

(defmethod process-namestring ((service service))
  (with-slots (process-string port) service
    (or process-string
        (setq process-string (concatenate 
                              'string (symbol-name (service-protocol service)) " Listen [" (write-to-string port :base 10.) "]")))))

(declaim (ftype (function integer) proxy-service-on-port))

(defmethod report-service-enabled ((service service))
  (with-slots (address port) service
    (unless (proxy-service-on-port port) ;don't squawk for proxy enable
      (let ((protocol (service-protocol service))
            (host (or address (www-utils:local-host-domain-name))))
        (expose-log-window)
        (notify-log-window "~A Service Enabled ~:[~;exclusively ~]for: ~(~A~)://~A:~D/" protocol address protocol host port)))))

;; Disabling a proxy on a port also disables any server running there.
;; More infrastructure needed to tell if only the proxy should be disabled. -- JCMa 3/25/2006
(defmethod report-service-disabled ((service service))
  (with-slots (address port) service
    (let ((protocol (service-protocol service))
          (host (or address (www-utils:local-host-domain-name))))
      (expose-log-window)
      (notify-log-window "~A Service Disabled for: ~:*~(~A~)://~A:~D/" protocol host port))))

(defmethod listening-for-connections-p ((service service))
  (let ((process (service-process service)))
    (and process
         (process-active-p process))))

(defmethod %stop-listening-for-connections ((service service))
  (with-slots (port process) service
    (when process
      ;; Is there a better way to stop listening and reclaim the pocess? -- JCMa 3/25/2006
      (ignore-errors (clim-sys:destroy-process process))
      #+ignore(resources:deallocate-resource 'http-process process)
     ;; (process-reset process)
      (setq process nil)
      (setf (listening-on-http-ports) (delete port (listening-on-http-ports)))
      (report-service-disabled service)
      t)))

;; If we can unbind the port when deallocating HTTP processes, then we can resource these processes. -- JCMa 3/25/2006
(defmethod %start-listening-for-connections ((service http-service))
  (with-slots (address port process-priority backlog timeout) service
    (flet ((%listen-for-http-connections (address port backlog read-timeout)
             (ipc:%listen-for-connections address port backlog read-timeout 'accept-connection)))
      (let* ((ip-number (and address (%parse-internet-address address)))
             (keywords `(:priority ,process-priority))
             (process (mp:process-run-function (process-namestring service) keywords #'%listen-for-http-connections ip-number port backlog timeout)
                      #+ignore(launch-process (process-namestring service) keywords #'%listen-for-http-connections ip-number port backlog timeout)))
        (declare (dynamic-extent keywords))
        (setf (service-process service) process
              (listening-on-http-ports) (cons port (listening-on-http-ports)))
        (report-service-enabled service)))))

#+CL-HTTP-SSL
(defmethod %start-listening-for-connections ((service https-service))
  (with-slots (address port process-priority backlog timeout) service
    (flet ((%listen-for-https-connections (address port backlog read-timeout ssl-ctx)
             (ipc:%listen-for-ssl-connections address port backlog read-timeout ssl-ctx 'accept-connection)))
      (let* ((ip-number (and address (%parse-internet-address address)))
             (ssl-ctx (ssl-ctx service))
             (keywords `(:priority ,process-priority))
             (process (mp:process-run-function (process-namestring service) keywords 
                                               #'%listen-for-https-connections ip-number port backlog timeout ssl-ctx)
                      #+ignore(launch-process (process-namestring service) keywords 
                                              #'%listen-for-https-connections ip-number port backlog timeout ssl-ctx)))
        (declare (dynamic-extent keywords))
        (setf (service-process service) process
              (listening-on-http-ports) (cons port (listening-on-http-ports)))
        (report-service-enabled service)))))

#+CL-HTTP-SSL
(defun ssl-filename (pathname &optional (default #p"http:pw;ssl;") (error-title "SSL Launch Error"))
  ;; perform logical pathname translations before passing to OpenSSL
  (let ((file (merge-pathnames (translated-pathname pathname) (translated-pathname default))))
    (cond ((probe-file file)
           (let ((file-name (namestring file)))
             (when (> (length file-name) 255)
               (error "~A: The file name, ~A, contains more than 255 characters." error-title file-name))
             file-name))
          (t (error "~A: The file, ~A, does not exist." error-title file)))))

#+CL-HTTP-SSL
(defun ssl-directoryname (pathname &optional (default #p"http:pw;ssl;"))
  ;; perform logical pathname translations before passing to OpenSSL
  (let ((dir (make-pathname :name nil :type nil :version nil
                            :defaults (merge-pathnames (translated-pathname pathname) (translated-pathname default)))))
    (pathname-create-directory-if-needed dir)
    (let ((dir-name (namestring dir)))
      (when (> (length dir-name) 255)
        (error "SSL Launch Error: The direcrtory name, ~A, contains more than 255 characters." dir-name))
      dir-name)))

#+CL-HTTP-SSL
(defmethod ssl-session-id-context ((service https-service))
  (let ((address (service-address service)))
    (format nil "~A:~D" (or address (local-host-ip-address)) (service-port service))))

#+CL-HTTP-SSL
(defmethod ssl-ctx-initialize ((service https-service))
  (with-slots (address port version ctx certificate private-key password parameters ciphers 
                       certificate-authorities client-certificates client-ca-list-ptr verify-mode verify-depth) service
    (flet ((get-password (password address port)
             (multiple-value-bind (pwd abort-p)
                 (case password
                   (:prompt (get-ssl-server-private-key-password (or address "*") port))
                   (:none (values "" nil))
                   (t (values password nil)))
               (when abort-p
                 (error "SSL Launch Error: No server key password was supplied for https://~A:~D" address port))
               ;; make sure we have a base char string, unless LW is doing robustness.
               (values pwd))))
      (ssl-ctx-deinitialize service)
      ;; Make a new CTX and initialize from slots
      (setq ctx (comm:make-ssl-ctx :ssl-ctx version :ssl-side :server))
      (comm:set-ssl-ctx-dh ctx :filename (ssl-filename parameters))
      ;; Password must be set before calls to use private key file
      (comm:set-ssl-ctx-password-callback ctx :password (get-password password address port))
      (comm:ssl-ctx-use-certificate-chain-file ctx (ssl-filename certificate))
      (comm:ssl-ctx-use-rsaprivatekey-file ctx (ssl-filename private-key) comm:ssl_filetype_pem)
      (comm:set-cipher-list ctx ciphers)
      ;; Set up client certificates
      (when (or certificate-authorities client-certificates)
        (when certificate-authorities
          ;; Load and returns a pointer to a stack of CA names extracted from a PEM (base64 encoded) file. 
          ;; See: http://www.openssl.org/docs/ssl/SSL_load_client_CA_file.html
          (setq client-ca-list-ptr (comm:ssl-load-client-ca-file (ssl-filename certificate-authorities)))
          (cond ((fli:null-pointer-p client-ca-list-ptr)
                 (error "SSL Launch Error: No client certificate authorities found when loading ~A." (ssl-filename certificate-authorities)))
                ;; Set the list of certificate authorities (CAs) that are acceptable from the client. 
                (t (comm:ssl-ctx-set-client-ca-list ctx client-ca-list-ptr))))
        ;; Loads the certificates of the certificate authorities (CAs) that are trusted by this
        ;; application and that will be used to verify certificates that are received from remote
        ;; applications. Certificate revocation lists (CRLs) are also loaded if any exist.
        (comm:ssl-ctx-load-verify-locations ctx ;either certificate-authorities or client-certificates must be non-null
                                            (and certificate-authorities (ssl-filename certificate-authorities))
                                            (and client-certificates (ssl-directoryname client-certificates "http:pw;ssl;clientcerts;")))
        ;; Set the session ID context
        (comm::set-session-id-context ctx (ssl-session-id-context service))
        (comm::set-verification-mode ctx :server verify-mode)
        (comm::ssl-ctx-set-verify-depth ctx verify-depth))
      ;; Set kludge switches don't set unless we see some appreciable improvement -- JCMa 3/26/2006
      ;; (comm:set-ssl-ctx-options ctx :all t)
      ctx)))

#+CL-HTTP-SSL
(defmethod ssl-ctx-deinitialize ((service https-service))
  (macrolet ((deallocate-slots (service &body clauses)
               `(cond-every
                 ,.(loop for (slot . clause) in clauses
                         collect `((slot-boundp ,service ',slot) ,@clause (slot-makunbound ,service ',slot)))))
             (maybe-free-foreign-object (slot)
               `(cond ((not (fli:pointerp ,slot))
                       (cerror "Skip freeing OpenSSL pointer" "~A is not a C pointer." ',slot))
                      ((fli:null-pointer-p ,slot)) ;no action required
                      (t (fli:free-foreign-object ,slot))))) ;free OpenSSL structure
    (with-slots (ctx) service
      (deallocate-slots
       service
       (ctx ;; Free memory in FIFO order or crashes result - CTX first
        (comm:destroy-ssl-ctx ctx)))
      service)))

#+CL-HTTP-SSL
(defmethod ssl-ctx ((service https-service))
  (with-slots (ctx) service
    (if (slot-boundp service 'ctx)
        ctx
      (ssl-ctx-initialize service))))

(defmethod %ensure-service-status ((service service) enabled-p &optional updated-p)
  (cond (updated-p
         (cond-every
          ((listening-for-connections-p service)
           (%stop-listening-for-connections service))
          (enabled-p
           (%start-listening-for-connections service))))
        ((listening-for-connections-p service)
         (unless enabled-p
           (%stop-listening-for-connections service)))
        (enabled-p
         (%start-listening-for-connections service))))

(defun %coerce-address (address)
  (etypecase address
    (keyword
     (ecase address
       (:all nil)
       (:primary (local-host-ip-address))
       (:loopback "127.0.0.1")))
    (t (if (ip-address-string-p address)
           address
         (error "The ADDRESS, ~S, is not a valid keyword or IP address string." address)))))

(defmethod update-http-service ((service http-service) address timeout process-priority backlog)
  (macrolet ((update-service-parameters ((service) &body equal-accessor-value-specs)
               `(let (changed-p)
                  (unless-every
                   ,.(loop for (equal accessor new-value) in equal-accessor-value-specs
                           collect `((,equal (,accessor ,service) ,new-value)
                                     (setf (,accessor ,service),new-value
                                           changed-p t))))
                  changed-p)))
    (check-type backlog (fixnum 0))
    (check-type process-priority fixnum)
    (check-type timeout (fixnum 0))
    (let ((addr (%coerce-address address)))
      (update-service-parameters 
       (service)
       (equal service-address addr)
       (= service-timeout timeout)
       (= service-process-priority process-priority)
       (= service-backlog backlog)))))

(defun ensure-http-protocol-on-port (port &key (address :all) (enable-service-p t) (timeout *server-timeout*) 
                                          (process-priority *listener-process-priority*) (backlog *connection-backlog*))
  (check-type port (fixnum 0))
  (check-type backlog (fixnum 0))
  (check-type process-priority fixnum)
  (check-type timeout (integer 0))
  (flet ((make-http-service (address port timeout process-priority backlog)
           (make-instance 'http-service
                          :address (%coerce-address address)
                          :port port
                          :process-priority process-priority
                          :timeout (floor (/ timeout 60.)) ; LW stream timeout is in seconds
                          :backlog backlog)))
    (let ((service (%get-service-on-port port)))
      (etypecase service
        (null 
         (setq service (make-http-service address port timeout process-priority backlog ))
         (%register-service-on-port service)
         (when enable-service-p
           (%start-listening-for-connections service)))
        (http-service
         (%ensure-service-status service
                                 enable-service-p
                                 (update-http-service service address timeout process-priority backlog)))
        #+CL-HTTP-SSL
        (https-service
         (%stop-listening-for-connections service)
         (%unregister-service service)
         (setq service (make-http-service address port timeout process-priority backlog))
         (%register-service-on-port service)
         (when enable-service-p
           (%start-listening-for-connections service))))
      service)))

(defmacro define-http-service (&key port (address :all) (timeout '*server-timeout*) (process-priority '*listener-process-priority*)
                                    (backlog '*connection-backlog*) enable-service-p)
  "Specifies the parameters for HTTP service on the port, PORT, and enables service when ENABLE-SERVICE-P is non-null. 
The optional parameters are:

ADDRESS          - An IP address string for the network interface on which to accept connections.
                   The value can also be a keyword:
                       :ALL      - Listen on all interfaces
                       :PRIMARY  - Listen only on the primary interface (see also http:*primary-network-host*)
                       :LOOPBACK - Listen only on the loopback interface, 127.0.0.1

TIMEOUT          - Time in 60ths of a second before the server drops an idle HTTP connection.

PROCESS-PRIORITY - An integer specifying process priority for the process listening on PORT.

BACKLOG          - The number of pending connections on PORT that will be queued for accept."
  `(ensure-http-protocol-on-port ,port
                                 :address ,address
                                 :timeout ,timeout
                                 :process-priority ,process-priority
                                 :backlog ,backlog
                                 :enable-service-p ,enable-service-p ))

(export 'define-http-service :http)

#+CL-HTTP-SSL
(defmethod update-https-service ((service https-service) address timeout process-priority backlog 
                                 certificate private-key password parameters ciphers ssl-version 
                                 certificate-authorities client-certificates verify-mode verify-depth)
  (macrolet ((update-service-parameters ((service) &body equal-accessor-value-specs)
               `(let (changed-p ssl-changed-p)
                  (unless-every
                   ,.(loop for (equal accessor new-value ssl-p) in equal-accessor-value-specs
                           collect `((,equal (,accessor ,service) ,new-value)
                                     (setf (,accessor ,service),new-value
                                           changed-p t)
                                     ,.(when ssl-p
                                         `((setq ssl-changed-p t))))))
                  (when ssl-changed-p
                    (ssl-ctx-deinitialize ,service)
                    ;; update the parameters to the lastest args whenever there is a change
                    (setf (ssl-security-parameters ,service) (list ssl-version ciphers verify-mode)))
                  changed-p)))
    (check-type backlog (fixnum 0))
    (check-type process-priority fixnum)
    (check-type timeout (fixnum 0))
    (check-type password (or string (member :prompt :none)))
    (check-type certificate-authorities (or null string pathname))
    (check-type client-certificates (or null string pathname))
    (check-type verify-mode (member :never :always :once))
    (check-type verify-depth (fixnum 1 32))
    (let ((cipher-string (ssl-cipher-string ciphers))
          (version (www-utils::%convert-ssl-versions-for-lw ssl-version))
          (addr (%coerce-address address)))
      (setf (ssl-security-parameters service) (list ssl-version ciphers verify-mode))
      (update-service-parameters (service)
       (equal service-address addr) 
       (= service-timeout timeout)
       (= service-process-priority process-priority)
       (= service-backlog backlog)
       (equal ssl-certificate (pathname certificate) t)
       (equal ssl-private-key (pathname private-key) t)
       (equal ssl-parameters (pathname parameters) t)
       (equal ssl-password password t)
       (equal ssl-ciphers cipher-string t)
       (eql ssl-version version t)
       (equal ssl-certificate-authorities (and certificate-authorities (pathname certificate-authorities)) t)
       (equal ssl-client-certificates (and client-certificates (pathname client-certificates)) t)
       (eql ssl-verify-mode verify-mode t)
       (= ssl-verify-depth verify-depth t)))))

#+CL-HTTP-SSL
(defun ensure-https-protocol-on-port (port &key (address :all) (enable-service-p t) 
                                           certificate private-key password parameters (ciphers :ALL) (ssl-version :ssl-default)
                                           certificate-authorities client-certificates (verify :never) (verify-depth 9)
                                           (timeout *server-timeout*) (process-priority *listener-process-priority*) (backlog *connection-backlog*))
  
  (flet ((make-https-service (address port timeout process-priority backlog certificate private-key password parameters ciphers ssl-version
                                      certificate-authorities client-certificates verify-mode verify-depth)
           (check-type port (integer 0))
           (check-type process-priority fixnum)
           (check-type timeout (fixnum 0))
           (check-type backlog (fixnum 0))
           (check-type password (or string (member :prompt :none)))
           (check-type certificate-authorities (or null string pathname))
           (check-type client-certificates (or null string pathname))
           (check-type verify-mode (member :never :always :once))
           (check-type verify-depth (fixnum 1 32))
           (make-instance 'https-service
                          :address (%coerce-address address)
                          :port port
                          :process-priority process-priority
                          :timeout (floor (/ timeout 60.)) ; LW stream timeout is in seconds
                          :backlog backlog
                          :certificate (pathname certificate)
                          :private-key (pathname private-key)
                          :password password
                          :parameters (pathname parameters)
                          :ciphers (ssl-cipher-string ciphers)
                          :version (www-utils::%convert-ssl-versions-for-lw ssl-version)
                          :certificate-authorities certificate-authorities
                          :client-certificates client-certificates
                          :verify-mode verify-mode
                          :verify-depth verify-depth
                          :security-parameters (list ssl-version ciphers verify-mode))))
    (let ((service (%get-service-on-port port)))
      (etypecase service
        (null 
         (setq service (make-https-service address port timeout process-priority backlog certificate private-key password parameters ciphers ssl-version
                                           certificate-authorities client-certificates verify verify-depth))
         (%register-service-on-port service)
         (when enable-service-p
           (%start-listening-for-connections service)))
        (https-service
         (%ensure-service-status 
          service
          enable-service-p
          (update-https-service service address timeout process-priority backlog certificate private-key password parameters ciphers ssl-version
                                certificate-authorities client-certificates verify verify-depth)))
        (http-service
         (%stop-listening-for-connections service)
         (%unregister-service service)
         (setq service (make-https-service address port timeout process-priority backlog certificate private-key password parameters ciphers ssl-version
                                           certificate-authorities client-certificates verify verify-depth))
         (%register-service-on-port service)
         (when enable-service-p
           (%start-listening-for-connections service))))
      service)))

#+CL-HTTP-SSL
(defmacro define-https-service (&key port (address :all) 
                                     certificate private-key (password :prompt) parameters (ciphers :all) (ssl-version :ssl-default)
                                     certificate-authorities client-certificates (verify :never) (verify-depth 9)
                                     (timeout '*server-timeout*) (process-priority '*listener-process-priority*) (backlog '*connection-backlog*)
                                     enable-service-p)
  "Specifies the parameters for HTTPS service on the port, PORT, and enables service when ENABLE-SERVICE-P is non-null. 
The required parameters are:

CERTIFICATE             - A pathname containing the pem-encoded certificate for the SSL server on PORT. 

PRIVATE-KEY             - A pathname containing the pem-encoded private key for the SSL server on PORT. 

PASSWORD                - A string which is the password for PRIVATE-KEY, or any of the following keywords:

                            :NONE   - No password required for PRIVATE-KEY.
                            :PROMPT - Ask the user for the password when SSL is launched.

PARAMETERS              - A pathname containing the pem-encoded Diffie-Hellman parameters for the SSL server on PORT.
                          Open SSL dhparam shell command generates the parameters. See: http://www.openssl.org/docs/apps/dhparam.html
                          For discussion, see: http://www.openssl.org/docs/ssl/SSL_CTX_set_tmp_dh_callback.html

The optional parameters are:

CIPHERS                 - A keyword or string denoting OpenSSL cipher suites to use with SSL on PORT.
                          The following values are available:

                            :ALL           - all ciphers suites offering at least some encryption.
                            :HIGH          - ciphers with key lengths greater than 128 bits.
                            :MEDIUM        - ciphers using 128 bit encryption.
                            :LOW           - ciphers using 64 or 56 bit encryption algorithms but excluding export cipher suites.
                            :56-BIT-EXPORT - ciphers using 56 bit export encryption algorithms.
                            :EXPORT        - ciphers using export encryption algorithms, including both 40 and 56 bits algorithms.
                            :40-BIT-EXPORT - ciphers using 40 bit export encryption algorithms.

                          Users desiring fine-grained control may provide an OpenSSL cipher string. 
                          These are described here: http://www.openssl.org/docs/apps/ciphers.html

SSL-VERSION             - A keyword that controls the version of SSL/TLS used on PORT. The options are:

                            :TLS-1       - Use only TLS version 1
                            :SSL-2-OR-3  - Use only SSL version 2 or 3 
                            :SSL-3       - Use only SSL version 3
                            :SSL-2       - Use only SSL version 2
                            :SSL-DEFAULT - Use the current default SSL versions (2 or 3)

Client certificates are not checked unless either CERTIFICATE-AUTHORITIES or CLIENT-CERTIFICATES is
supplied, which case the values of VERIFY and VERIFY-DEPTH

CERTIFICATE-AUTHORITIES - A pathname containing the PEM-encoded (base64 encoded) trusted certificate authorities or certificate 
                          revocation lists accepted from clients by the SSL server on PORT.

CLIENT-CERTIFICATES     - A directory containing PEM-encoded (base64 encoded) trusted certificate authorities or certificate 
                          revocation lists accepted from clients by the SSL server on PORT.

VERIFY                  - A keyword that controls client certificate verification by the SSL server on PORT.

                            :NEVER  - The server does not send a client certificate request to the client, so the client will 
                                      not send a certificate.
                            :ALWAYS - The server sends a client certificate request to the client, and any returned certificate 
                                      returned is checked. If the verification process fails, the TLS/SSL handshake is immediately
                                      terminated with an alert message containing the reason for the verification failure.
                            :ONCE   - Same as :ALWAYS except the a client certificate is checked only on the initial TLS/SSL handshake,
                                      and not again in case of renegotiation.

VERIFY-DEPTH            - An integer between 1 and 32 specifiying maximum certificate chain length for authentication of client 
                          or peer certificates.

Standard parameters controlling HTTP service.

ADDRESS                 - An IP address string for the network interface on which to accept connections.
                          The value can also be a keyword:
                              :ALL      - Listen on all interfaces
                              :PRIMARY  - Listen only on the primary interface (see also http:*primary-network-host*)
                              :LOOPBACK - Listen on the loopback address 127.0.0.1

TIMEOUT                 - Time in 60ths of a second before the server drops an idle HTTP connection.

PROCESS-PRIORITY        - An integer specifying process priority for the process listening on PORT.

BACKLOG                 - The number of pending connections on PORT that will be queued for accept."
  `(ensure-https-protocol-on-port ,port
                                  :address ,address
                                  :timeout ,timeout
                                  :process-priority ,process-priority
                                  :backlog ,backlog
                                  :certificate ,certificate
                                  :private-key ,private-key
                                  :password ,password
                                  :parameters ,parameters
                                  :ciphers ,ciphers
                                  :ssl-version ,ssl-version
                                  :certificate-authorities ,certificate-authorities
                                  :client-certificates ,client-certificates
                                  :verify ,verify
                                  :verify-depth ,verify-depth
                                  :enable-service-p ,enable-service-p ))

#+CL-HTTP-SSL
(export 'define-https-service :http)

(defmethod %enable-service ((service service))
  (unless (listening-for-connections-p service)
    (%start-listening-for-connections service)))

(defmethod %disable-service ((service service))
  (when (listening-for-connections-p service)
    (%stop-listening-for-connections service)))

(defmethod %clear-service ((service service))
  (%disable-service service)
  (%unregister-service service))

(defun enable-service-on-port (port)
  "Enables the service defined on PORT."
  (declare (values service-enabled-p))
  (let ((service (%get-service-on-port port)))
    (when service
      (%enable-service service)
      service)))

(defun disable-service-on-port (port)
  "Disables the service defined on PORT."
  (declare (values service-disabled-p))
  (let ((service (%get-service-on-port port)))
    (when service
      (%disable-service service)
      service)))

(defun undefine-service-on-port (port)
  "Undefines the service on PORT."
  (declare (values service-cleared-p))
  (let ((service (%get-service-on-port port)))
    (when service
      (%clear-service service)
      t)))

(export 'undefine-service-on-port :http)

(defun map-services (case function)
  (ecase case
    (:active
     (map-services-if #'listening-for-connections-p function))
    (:inactive
     (map-services-if #'(lambda (service) (not (listening-for-connections-p service))) function))
    (:all
     (map-services-if t function))))

(define enable-http-service (&key (on-ports *standard-http-port*) (address :all))
  "Top-level method for starting up HTTP servers.
ON-PORTS can be a port number, a list of port numbers."
  (let ((ports (ensure-list on-ports)))
    ;; Enable any disabled ports
    (dolist (port ports)
      (unless (enable-service-on-port port)
        ;; create an HTTP service with default parameter on PORT and enable it
        (ensure-http-protocol-on-port port :enable-service-p t :address address)))
    on-ports))

(define start ()
  "Short version of ENABLE-HTTP-SERVICE."
  (enable-http-service :on-ports *standard-http-port*))

(defun disable-http-service (&key (on-ports :all))
  "Top-level method for shutting down HTTP servers."
  (flet ((maybe-disable-service (service)
           (when (listening-for-connections-p service)
             (%stop-listening-for-connections service))))
    (etypecase on-ports
      (symbol
       (ecase on-ports
         (:all 
          (map-services :all #'maybe-disable-service))))
      (integer
       (disable-service-on-port on-ports))
      (cons
       (mapc #'disable-service-on-port on-ports)))))

(defun clear-all-services ()
  "Clears all services and releases any resources they hold."
  (map-services :all #'%clear-service))

(defmethod shutdown-server ((server basic-server-mixin))
  "Forcibly shutdown a running (or idle) HTTP server."
  (with-slots (stream) server
    (when stream
      (close stream :abort t))
    (deallocate-resource 'http-server server)))

(define map-http-resource (keyword allocation function)
  "Maps FUNCTION over the resource denoted by KEYWORD with ALLOCATION. 
KEYWORD can be any of :STREAM, :SERVER, or :PROCESS
ALLOCATION can be any of :ALL, :FREE, or :ALLOCATED."
  (flet ((fctn (item allocated-p resource)
           (declare (ignore resource))
           (when (ecase allocation
                   (:all t)
                   (:free (not allocated-p))
                   (:allocated allocated-p))
             (funcall function item))))
    (resources:map-resource (ecase keyword
		              (:server 'http-server)
		              (:process 'http-process))
        #'fctn)))

(define all-servers (&aux servers)
  (flet ((collect (server)
           (push server servers)))
    (map-http-resource :server :allocated #'collect)
    servers))

(define %clear-http-process-resource ()
  "Deallocates all resourced HTTP processes."
  (map-http-resource :process :free #'clim-sys:destroy-process)
  (when (clim-sys:current-process)
    (clim-sys:process-yield))
  (clear-resource 'http-process))

(define shutdown-http-service ()
  "Forcibly shutdown all HTTP servers and free all resources."
  (prog1 (disable-http-service :on-ports :all)
    (clear-all-services)
    (map-http-resource :server :allocated #'shutdown-server)
    (close-all-logs)))

(define stop ()
  (disable-http-service))

;; Force shutdown of all servers and free resources on exit from lisp.
(add-initialization "Shutdown CL-HTTP" '(shutdown-http-service) 
                    '(:normal) 'http:*shutdown-initialization-list*)

