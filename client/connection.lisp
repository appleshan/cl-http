;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-
;;;
;;; (c) Copyright  1996-2000, 2006, John C. Mallery
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; CLIENT PERSISTENT CONNECTIONS
;;;

(in-package :http) 

(declaim (inline current-connection))

(define current-connection ()
  "Returns the current connection object for a client HTTP transaction."
  (symbol-value '*connection*))

(defun handle-client-connection-condition (condition)	;fails to distinguish server errors in proxy case 9/20/2000 -- JCMa.
  (declare (special .incomplete-transaction-p.))
  (etypecase condition				;assure proper log entries, use *client* to avoid consing
    (reportable-condition
      (setf (client-status *client*) (status-code condition))
      (cond ((http-close-connection-p condition)
	     (setf (connection-close-p *connection*) t))
	    ((connection-close-p *connection*))
	    (t (setq .incomplete-transaction-p. nil))))
    ;; Detect dropped connections
    ((or connection-closed connection-lost host-stopped-responding protocol-timeout
	 #+Genera network-stream-closed #+Genera connection-no-more-data)
     (setf (client-status *client*) 408
	   (connection-close-p *connection*) t))
    ((or #+Genera unknown-address unknown-host-name domain-resolver-error connection-refused)	;detect bad gateways
     (setf (client-status *client*) 502
	   (connection-close-p *connection*) t)
     (bug-report-client-error condition))
    (error
      (setf (client-status *client*) 500
	    (connection-close-p *connection*) t)	;otherwise report an error
      (bug-report-client-error condition))
    (condition
      (setf (connection-close-p *connection*) t)))
  nil)

(defun connection-data-received-p (&optional (connection *connection*))
  "Returns non-null when data has been received over CONNECTION." 
  (and connection
       (not (zerop (connection-bytes-received connection)))))

;; this macro needs to handle the fencepost case where the connection goes
;; down before we get data back from the client.  otherwise we will get proxy
;; or client errors when we try to use a connection that has gone down.
;; 3/10/2000 -- JCMa.
(define-macro with-current-connection ((client &key block-name (http-version '*client-http-version*) (connection-var 'connection)) &body body)
  "Establishes the current connection context and maintains consistent connections."
  `(flet ((handle-dead-connection (condition)
	    (declare (ignore condition))
	    (when (zerop (connection-bytes-received *connection*))	;must be the client because a server would have made a request.
	      (throw 'get-new-connection :retry))))
     (let ((remaining-tries *client-retries-for-unresponsive-hosts*)
	   (.incomplete-transaction-p. t)
	   ,connection-var fresh-connection-p)
       (declare (special .incomplete-transaction-p.))
       (loop doing (catch 'get-new-connection
		     (multiple-value-setq (,connection-var fresh-connection-p)
		       (ensure-client-connection ,client ,http-version))
		     (let* ((*connection* ,connection-var))
		       (declare (special *connection*))
		       (unwind-protect
			   (return (multiple-value-prog1
				     (handler-bind
				       ((host-stopped-responding #'handle-dead-connection)
					(condition #'handle-client-connection-condition))
				       ,(if block-name
					    `(block ,block-name . ,body)
					    `(progn . ,body)))
				     ;; ensure that logging has the correct number
				     (incf (the fixnum (connection-requests-completed ,connection-var)))
				     (setq .incomplete-transaction-p. nil)))
			 ;; Handle client logging here
			 (client-log-request ,client)
                         ;; reset client transaction state here
                         (reset-transaction-state ,client)
			 ;; When an HTTP transaction fails to terminate normally or it
			 ;; needs to be closed, do it here. Otherwise, recheck on next
			 ;; time around the loop in ensure-client-connection.
			 (when (or .incomplete-transaction-p. (connection-close-p ,connection-var))
			   (deallocate-connection ,client)))))
	     when fresh-connection-p do (decf remaining-tries)
	     until (zerop remaining-tries)))))

;;;------------------------------------------------------------------- 
;;;
;;; PERSISTENT CONNECTION CLASSES
;;;

(defclass connection-pool-mixin
          ()
    ((free-since :initform 0 :initarg :free-since :accessor connection-free-since)
     (timeout :initform 0 :initarg :timeout :accessor connection-timeout)
     (close-time :initform 0 :initarg :close-time :accessor connection-close-time)
     (requests-completed :initform 0 :accessor connection-requests-completed)
     (requests-allowed :initform 0 :accessor connection-requests-allowed)
     (next :initform nil :accessor connection-next))
  (:documentation "A mixin that manages when connections close or are garbage collected."))

(defclass basic-connection
          (connection-pool-mixin)
    ((stream :initarg :stream :accessor connection-stream)
     (close-p :initform nil :initarg :close-connection-p :accessor connection-close-p)
     (domain-name :initarg :domain-name :accessor connection-domain-name)
     (host :initarg :host :accessor connection-host)
     (port :initarg :port  :accessor connection-port)
     (state :initform :closed :initarg :state :accessor %connection-state)
     (version :initform nil :initarg :server-version :accessor connection-version)
     (version-confirmed-p :initform nil :initarg :server-version-confirmed-p :accessor connection-version-confirmed-p)
     (http-plist :initform nil :initarg :server-http-plist :accessor connection-http-plist))	;destructive modifications may be retained
  (:documentation "The basic persistent connection infrastrcture for the client side."))

(defclass connection
          (basic-connection)
  ((protocol :allocation :class :initform :http :reader connection-protocol))
  (:documentation "A client-side HTTP connection."))

(defclass ssl-connection
          (basic-connection)
    ((protocol :allocation :class :initform :https :reader connection-protocol))
  (:documentation "A client-side SSL HTTP connection."))

(defmethod print-object ((connection basic-connection) stream)
  (with-slots (domain-name port state) connection
    (print-unreadable-object (connection stream :type t :identity t)
      (format stream "{~D} " (connection-requests-completed connection))
      (cond-every
        (domain-name (write-string domain-name stream))
        (port
          (format stream ": ~D" port))
        (state
          (format stream " [~A]" state)))       
      connection)))

;;;------------------------------------------------------------------- 
;;;
;;; ALLOCATION AND DEALLOCATION
;;;

(declaim (inline %make-connection))

(defun %make-connection (resource protocol host port stream &optional domain-name)
  (declare (ignore resource))
  (ecase protocol
    (:http
     (make-instance 'connection
                    :host host
                    :port port
                    :domain-name domain-name
                    :stream stream))
    (:https
     (make-instance 'ssl-connection
                    :host host
                    :port port
                    :domain-name domain-name
                    :stream stream))))

;; Only resourcing in genera and MCL pass in the resource to the initializer.
(defun %initialize-connection (resource connection protocol host port stream &optional domain-name)
  (declare (ignore resource protocol))
  (setf (connection-stream connection) stream
        (connection-host connection) host
        (connection-port connection) port
        (connection-domain-name connection) domain-name)
  connection)

(defun %match-connection-p (resource connection protocol host port stream &optional domain-name)
  (declare (ignore resource host port stream domain-name))
  (eq protocol (connection-protocol connection))) 

(defresource http-connection (protocol host port stream &optional domain-name)
  :matcher %match-connection-p
  :constructor %make-connection
  :initializer %initialize-connection)

(define clear-connection-resource ()
  "Clears the resource of HTTP connection objects."
  (clear-resource 'http-connection))

(defun open-stream-to-host (protocol host port)
  (ecase protocol
    (:http 
     (open-http-stream-to-host host port))
    (:https
     #+CL-HTTP-SSL
     (open-ssl-stream-to-host host port)
     #-CL-HTTP-SSL
     (let ((client *client*))
       (error 'ssl-not-available :url (when client (client-url client)))))))

(define allocate-connection (protocol host port &optional domain-name)
  "Allocates a new connection to HOST on PORT with DOMAIN-NAME."
  (declare (optimize (speed 3))
           (values connection new-connection-p))
  (multiple-value-bind (http-version confirmed-p http-plist)
      (server-http-version-on-port host port)
    (let* ((stream (open-stream-to-host protocol host port))
	   (conn (allocate-resource 'http-connection protocol host port stream domain-name)))
      (setf (%connection-state conn) :open
	    (connection-close-p conn) nil
	    (connection-timeout conn) *client-persistent-connection-timeout*
	    (connection-requests-allowed conn) *client-persistent-connection-number*
	    (connection-version conn) http-version
	    (connection-version-confirmed-p conn) confirmed-p
	    (connection-http-plist conn) http-plist)
      (atomic-incf *connections-allocated*)
      (client-trace "~&Allocate Connection (~D): ~S" *connections-allocated* conn)
      (values conn t))))

(defun %deallocate-client-http-stream (stream &optional (abort-p t))
  (close stream :abort abort-p)
  (setf (www-utils:bytes-transmitted stream) 0
	(www-utils:bytes-received stream) 0)
  (prog1 (deallocate-client-http-stream stream)
         (atomic-incf *connections-deallocated*)
	 (client-trace "~&Deallocate Stream (~D): ~S" *connections-deallocated* stream)))

(define-generic deallocate-connection (connection-or-client)
  (declare (values connection))
  (:documentation  "Deallocates CONNECTION-OR-CLIENT to the connection resoure, ensuring that stream is closed."))

(defmethod deallocate-connection ((connection basic-connection))
  (with-slots (stream state host port close-p domain-name version) connection
    ;; make sure that open streams are deallocated
    (ecase state
      ((:open :closed)
       (setq state :deallocated)
       (when stream
         (%deallocate-client-http-stream stream)
         (setq stream nil))
       ;; reset instance variables for GC and fast allocation.
       (setq host nil
             port 80
             close-p nil
             domain-name nil
             version nil))
      (:deallocated))
    (when stream (error "Stream not Dellocated"))))

(defmethod deallocate-connection :around ((connection basic-connection))
  (client-trace "~&Deallocate: ~S" connection)
  (call-next-method)
  (deallocate-resource 'http-connection connection)
  connection)

(defmethod deallocate-connection :after ((connection connection-pool-mixin))
  (with-slots (free-since timeout close-time requests-completed requests-allowed next) connection
    (setq free-since 0
          timeout 0
          close-time 0
          requests-completed 0
          requests-allowed 0
          next nil)))

;;;------------------------------------------------------------------- 
;;;
;;; OPENNING AND CLOSING CONNECTIONS
;;;

(define-generic open-connection (connection)
  (declare (values connection))
  (:documentation "Ensures that CONNECTION has an open HTTP connection,
reopenning it as necessary."))

(defmethod open-connection ((connection basic-connection))
  (with-slots (protocol host port state stream) connection
    (ecase state
      (:closed
       (unless stream
         (error "Closed connections should have no stream."))
       (setq stream (open-stream-to-host protocol host port)
             state :open))
      (:open
       (unless (and stream (live-connection-p stream))
         (when stream
           (%deallocate-client-http-stream stream))
         (setq stream (open-stream-to-host protocol host port)))))
    connection))

(define-generic close-connection (connection &optional abort)
  (:documentation "Ensures that connection is closed.
If open, abort controls whether it is closed in abort mode or not."))

(defmethod close-connection ((connection basic-connection) &optional abort)
  (with-slots (stream state) connection
    (ecase state
      (:open
        (setf state :closed)
        (when stream
          (%deallocate-client-http-stream stream abort)
          (setf stream nil)))
      (:closed))
    connection))

(define-generic connection-version-string (connection)
  (:documentation "Returns the HTTP version of CONNECTION as a string."))

(defmethod connection-version-string ((connection basic-connection))
  (let ((version (connection-version connection)))
    (when version
      (symbol-name version))))

(defgeneric connection-confirmed-version (connection)
  (declare (values version version-confirmed-p http-plist))
  (:documentation "Returns the HTTP version of connection-or-client and whether this version has been confirmed."))

(defmethod connection-confirmed-version ((connection basic-connection))
  (with-slots (version version-confirmed-p http-plist) connection
    (values version version-confirmed-p http-plist))) 

;;;------------------------------------------------------------------- 
;;;
;;; ASCERTAINING CONNECTION STATE
;;;

(define-generic connection-state (connection)
  (declare (values state-keyword))
  (:documentation "Returns the connection state of connection,
which can be any of :OPEN, :CLOSED, or :DEALLOCATED"))

(defmethod connection-state ((connection basic-connection))
  (with-slots (state stream) connection
    (ecase state
      (:open
        (cond ((null stream) (setq state :closed))
              ((live-connection-p stream) :open)
              (t (setq state :closed)
                 (%deallocate-client-http-stream stream)
                 (setq stream nil)
                 :closed)))
      (:closed :closed)
      (:deallocated :deallocated))))

(define-generic connection-local-port (client-or-connection)
  (:documentation "Returns the local port in use by the connection stream to communicate with the remote server."))

(defmethod connection-local-port ((connection basic-connection))
  (let ((stream (connection-stream connection)))
    (when stream
      (local-port stream))))

(define-generic connection-local-protocol (client-or-connection)
  (:documentation "Returns the protocol in use by the connection stream to communicate with the remote server."))

(defmethod connection-local-protocol ((connection basic-connection))
  (let ((stream (connection-stream connection)))
    (when stream
      (local-protocol stream))))


;;;------------------------------------------------------------------- 
;;;
;;; BYTE COUNTS ON CONNECTIONS
;;;

(define-generic connection-bytes-transmitted (connection)
  (:documentation "Returns the number of bytes transmitted by CONNECTION during the current HTTP transaction."))

(defmethod connection-bytes-transmitted ((connection basic-connection))
  (bytes-transmitted (connection-stream connection)))

(define-generic connection-bytes-received (connection)
  (:documentation "Returns the number of bytes received by CONNECTION during the current HTTP transaction."))

(defmethod connection-bytes-received ((connection basic-connection))
  (bytes-received (connection-stream connection)))

;;;------------------------------------------------------------------- 
;;;
;;; MATCHING CONNECTIONS
;;;

(declaim (inline %connection-to-host-port-p))

(defun %connection-to-host-port-p (connection host port)
  (and (equal (connection-host connection) host)
       (= (connection-port connection) port)))

(define-generic connection-to-host-port-p (connection host port)
  (:documentation "Returns non-null if CONNECTION is an http connection to HOST on PORT."))

(defmethod connection-to-host-port-p ((connection basic-connection) host port)
  (%connection-to-host-port-p connection host port))

;;;------------------------------------------------------------------- 
;;;
;;; MAINTAIN A POOL OF OPEN HTTP CONNNECTIONS TO HOSTS
;;;

(defvar *connection-table* (make-hash-table :test #'equal))

(defvar *connection-lock* (www-utils:make-lock "Client Connection")
  "The lock used to avoid thread collisions in the connection pool.")

(defmacro with-connection-pool-lock ((&key (mode :write)) &body body)
  `(www-utils:with-lock-held (*connection-lock* ,mode "HTTP Connection Wait") ,@body))

(defun map-connection-pool (function)
  (loop for entry being the hash-values of *connection-table*
        when entry
          do (loop for conn = entry then (connection-next conn)
                   while conn
                   do (funcall function conn))))

(defun push-connection-pool (connection)
  (let ((host (connection-host connection))
	(table *connection-table*))
    (with-connection-pool-lock (:mode :write)
      (let ((entry (gethash host table)))
	(when entry
	  (setf (connection-next connection) entry))
	(setf (gethash host table) connection)))))

(defun %deallocate-host-connections (connection)
  (let ((next (connection-next connection)))
    (when next
      (%deallocate-host-connections next))
    (deallocate-connection connection)))

(defun deallocate-all-connections (&aux (table *connection-table*))
  (with-connection-pool-lock (:mode :write)
    (loop for conn being each hash-value in table
          do (%deallocate-host-connections conn)
          finally (clrhash table))))

(defun %pop-connection-from-pool (protocol connection port)
  (declare (values connection new-top-conn))
  (loop with new-top-conn and prev
	for conn = connection then next
	for next = (and conn (connection-next conn))
	while conn
	do (cond ((and (= port (connection-port conn))
                       (eq protocol (connection-protocol conn)))
		  (if prev
		      (setf (connection-next prev) next)
		      (setq new-top-conn (or next :clear-entry)))
		  (ecase (connection-state conn)
		    (:open
		      (return-from %pop-connection-from-pool
			(values conn new-top-conn)))
		    (:closed
		      (deallocate-connection conn))
		    (:deallocated)))
		 (t (setq prev conn)))
	finally (return (values nil new-top-conn))))

(define get-connection (protocol host port &optional domain-name &aux (table *connection-table*))
  "Returns a client connection to a server from the connection pool."
  (declare (optimize (speed 3))
           (values connection new-connection-p))
  (when *client-persistent-connections*
    (with-connection-pool-lock (:mode :write)
      (multiple-value-bind (entry found-p)
          (gethash host table) 
        (cond (entry
               (multiple-value-bind (conn new-top-conn)
                   (%pop-connection-from-pool protocol entry port)
                 (cond-every
                  (new-top-conn
                   (case new-top-conn
                     (:clear-entry (remhash host table))
                     (t (setf (gethash host table) new-top-conn))))
                  (conn
                   (let ((stream (connection-stream conn)))
                     (clear-input stream)
                     (clear-output stream)
                     (setf (connection-domain-name conn) domain-name
                           (connection-next conn) nil
                           (connection-free-since conn) 0)	;mark as in use
                     (return-from get-connection (values conn nil)))))))
              (found-p
               (remhash host table))))))
  ;; If no live connection available allocate a new one.
  (allocate-connection protocol host port domain-name))

;; Reset connection state per HTTP transaction
(defmethod reset-transaction-state ((connection basic-connection))
  (let ((stream (connection-stream connection)))
    (setf (www-utils:bytes-transmitted stream) 0
	  (www-utils:bytes-received stream) 0)))

(defmethod note-free-connection ((connection connection-pool-mixin))
  (reset-transaction-state connection)
  (cond ((zerop (connection-free-since connection))
	 (let* ((time (get-universal-time))
		(close (+ time (the integer (or (connection-timeout connection) *client-persistent-connection-timeout*)))))
	   (declare (integer time))
	   ;; reset instance variables
	   (setf (connection-free-since connection) time
		 (connection-close-time connection) close))
	 (push-connection-pool connection))
	(t (error "Attempt to note free connection that is already free for ~S." connection))))

(defmethod return-connection ((connection connection-pool-mixin))
  (if (or (not *client-persistent-connections*)	;use persistent connections?
	  (eql :closed (connection-state connection))	;already closed?
	  (connection-close-p connection)	;instructions to close?
	  (> (the fixnum (connection-requests-completed connection))	;exceeding http 1.0 request limit?
	     (the fixnum (connection-requests-allowed connection))))
      (deallocate-connection connection)
      (note-free-connection connection)))

(define update-connection-status-from-headers (version &optional (headers *headers*) (conn (current-connection)))
  "Updates the current client connection according to the VERSION  and HEADERS sent by the server."
  (case version
    (:http/0.9
      (setf (connection-close-p conn) t))
    (:http/1.0
      (let ((connection (get-header :connection headers))
            (keep-alive (get-header :keep-alive headers)))
        (cond ((member :close connection)
	       (setf (connection-close-p conn) t))
	      ((member :keep-alive connection)
	       (if keep-alive
		   (destructuring-bind (&key timeout max)
		       keep-alive
		     (cond-every
		       ((and timeout (< timeout *client-persistent-connection-timeout*))
			(setf (connection-timeout conn) timeout))
		       ((and max (< max *client-persistent-connection-number*))
			(setf (connection-requests-allowed conn) max))))
		   (setf (connection-close-p conn) t))))))
    (t (let ((connection (get-header :connection headers)))
	 (when (member :close connection)
	   (setf (connection-close-p conn) t))))))

;;;------------------------------------------------------------------- 
;;;
;;; CONNECTION SCAVENGER
;;;

(defun %gc-host-connections (connection time)
  (declare (values next-live-connection))
  (let ((next (connection-next connection))
        new-next)
    (when next
      (setq new-next (%gc-host-connections next time)))
    (ecase (connection-state connection)
      (:open
        (cond ((> (connection-close-time connection) time)
               (setf (connection-next connection) new-next)
               connection)
              (t (deallocate-connection connection)
                 new-next)))
      (:closed
        (deallocate-connection connection)
        new-next)
      (:deallocated new-next))))

(define gc-connection-pool (&aux (table *connection-table*))
  "Maps over the connection pool and deallocates any connections that have timed out."
  (when (and table (not (zerop (hash-table-count table))))
    (let ((replace-top-level nil)
          (remove-list nil)
          (time (get-universal-time)))
      (declare (dynamic-extent replace-top-level remove-list time))
      (with-connection-pool-lock (:mode :write)
        (loop for conn being each hash-value in table
                       using (hash-key host)
              do (cond (conn
                        (let ((n-conn (%gc-host-connections conn time)))
                          (cond ((and n-conn (equal n-conn conn)))
                                (n-conn
                                 (push n-conn replace-top-level)
                                 (push host replace-top-level))
                                (t (push host remove-list)))))
                       (t (push host remove-list))))
        (cond-every
          (remove-list
            (dolist (key remove-list)
              (remhash key table)))
          (replace-top-level
            (loop for (host value) on replace-top-level by #'cddr
                  do (setf (gethash host table) value))))))))

(defvar *connection-scavenger* nil
  "A process that cleans up hanging connections to hosts.")

(defun scavenge-connections-main-loop ()
  (flet ((connections-to-scavenge-p ()
           (not (and *connection-scavenger-on*
                     (zerop (hash-table-count *connection-table*)))))
         (false ()
           (not *connection-scavenger-on*)))
    (loop with process = *connection-scavenger* 
          doing (process-wait
                  "Scavenge Wait" #'connections-to-scavenge-p)
                (process-wait-with-timeout
                  "Scavenge Wait" *client-persistent-connection-timeout* #'false)
                (handler-case-if (not *debug-client*)
                   (gc-connection-pool)
                  (network-error () (sleep *client-retry-sleep-seconds*)))
          unless *connection-scavenger-on*
            do (setq *connection-scavenger* nil)
               (process-kill process))))

(define start-connection-scavenger ()
  "Starts the client connection scavenger."
  (let ((process *connection-scavenger*))
    (setq *connection-scavenger-on* t)
    (cond (process
           (process-preset process #'scavenge-connections-main-loop)
           (process-enable process))
          (t (let ((process-name "HTTP Connection Scavenger"))
               (setq process (make-process process-name
                                           :background-p t
                                           :restart-after-reset t
                                           :warm-boot-action :delayed-restart))
               (setq *connection-scavenger* process)
               (process-preset process #'scavenge-connections-main-loop)
               (process-enable process))))
    process))

(define stop-connection-scavenger ()
  "Stops the client connection scavenger."
  (when *connection-scavenger*
    (setq *connection-scavenger-on* nil)))

(defun initialize-client-substrate (&optional redo-p)
  "Initializes client substrate."
  (declare (ignore redo-p))
  #+Multi-Threaded
  (start-connection-scavenger))

(add-initialization "Initialize Client Substrate" '(initialize-client-substrate) '(:normal)
		    '*server-initialization-list*)


;;;------------------------------------------------------------------- 
;;;
;;; SERVER VERSION CACHE
;;;

(defvar *server-http-version-table* (make-hash-table :test #'equal))

(define clear-server-http-version-cache ()
  "Clears the cache associating all known hosts with the http server versions on their ports."
  (clrhash *server-http-version-table*))

(add-periodic-task "Client GC Server Version Cache" :daily '(clear-server-http-version-cache))

(define-generic clear-host-http-version-cache (host-object)
  (declare (values decached-p))
  (:documentation "Decaches the http version information concerning host-object."))

(defmethod clear-host-http-version-cache (host-object)
  (remhash host-object *server-http-version-table*))

(define-generic server-http-version-on-port (host-object port)
  (declare (values http-version confirmed-p http-plist))
  (:documentation "Returns the HTTP version of the server at HOST-OBJECT operating on PORT."))

(defmethod server-http-version-on-port (host-object port)
  (let ((entry (gethash host-object *server-http-version-table*))
	plist)
    (cond (entry
	   (if (setq plist (getf entry port))
	       (destructuring-bind (version . http-plist) plist
		 (if version
		     (values version t http-plist)
		     (values (caadr entry) nil http-plist)))
	       (values (caadr entry) nil)))
	  (t nil))))

(define-generic note-server-http-version-on-port (host-object http-port http-version)
  (:documentation "Notes HTTP-VERSION for the HTTP server at HOST-OBJECT operating on PORT."))

(defmethod note-server-http-version-on-port (host-object http-port http-version)
  (flet ((update-entry (key entry foundp)
	   (declare (ignore key))
	   (cond (foundp
		  (loop for l = entry then more
			for (port plist . more) = l
			when (eql port http-port)
			  do (unless (eq (first plist) http-version)
			       (setf (first plist) http-version))
			     (return entry)
			while more
			finally (setf (cddr l) (list http-port (list http-version)))
				(return entry)))
		 (t (list http-port (list http-version))))))
    (declare (dynamic-extent #'update-entry))
    (check-type http-port integer)
    (check-type http-version keyword)
    ;; check for coherent version in case of read past end of TCP buffer 4/27/2001 -- JCMa.
    (unless (%string-test "HTTP/" (symbol-name http-version) 0 char-equal)
      (error 'bad-http-version :url (client-url *client*) :format-string "HTTP Client: Bad HTTP version ~S" :format-args (list http-version)))
    (modify-hash *server-http-version-table* host-object #'update-entry)))

(define-generic note-server-http-version (connection http-version)
  (:documentation "Notes the HTTP-VERSION for the HTTP server associated with CONNECTION."))

(defmethod note-server-http-version ((connection basic-connection) http-version)
  (note-server-http-version-on-port (connection-host connection) (connection-port connection) http-version))

(define-generic get-host-port-http-server-property (host-object http-port indicator)
  (:documentation "Returns the value of indicator from the properties for the HTTP server operating on HTTP-PORT of HOST-OBJECT."))

(defmethod get-host-port-http-server-property (host-object http-port indicator)
  (let ((entry (gethash host-object *server-http-version-table*))
	plist value)
    (cond ((and entry
		(setq plist (cdr (getf entry http-port))))
	   (case (setq value (getf plist indicator :+no-value+))
	     (:+no-value+ nil)
	     (t (values value t))))
	  (t nil))))

(defmethod (setf get-host-port-http-server-property) (value host-object http-port indicator)
  (let ((entry (gethash host-object *server-http-version-table*)))
    (cond (entry
	   (let ((plist (getf entry http-port)))
	     (if plist
		 (setf (getf (cdr plist) indicator) value)
		 (setf (getf entry http-port) (list nil indicator value)))))
	  (t (setf (gethash host-object *server-http-version-table*) (list http-port (list nil indicator value)))))))

(define-generic note-server-http-property (connection indicator value)
  (:documentation "Notes the VALUE for INDICATOR as property for the HTTP server associated with CONNECTION."))

(defmethod note-server-http-property ((connection basic-connection) indicator value)
  (setf (get-host-port-http-server-property (connection-host connection) (connection-port connection) indicator) value))