;;; -*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-
;;;
;;; (c) Copyright 1994-2000, 2005-2006, John C. Mallery
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; LOGS
;;;

(in-package :http)

;; should use a without interrupts for safety.  7/27/95 -- JCMa.
(define log-file-stream-stays-open (&optional on-p)
  "Controls whether the file log stream remains open all the time,
or it is reopenned for each log transaction. Production servers
should keep the log file stream open."
  (when (and *log-file-stream-stays-open* (not on-p))
    (close-all-logs))
  (setq *log-file-stream-stays-open* (not (null on-p))))

(define-macro with-log-write-lock ((log) &body body)
  `(with-slots (lock) ,log
     (www-utils:with-lock-held (lock :write "HTTP Log Wait") ,@body)))

(defmacro %with-log-stream ((log &key open-file-forces-output-p) &body body)
  `(cond (*log-file-stream-stays-open*
          (let ((log-stream (%log-stream log)))
            ,@body
            ,@(when open-file-forces-output-p
                '((force-output log-stream)))))
         (t (with-slots (filename) ,log
              ;; standard character or base character must be used to
              ;; prevent the Lispm from adding escapes into the file.
              ;; standard-char is better because it ensures
              ;; interoperability across lisp implementations. 9/29/95 -- JCMa.
              (with-open-file (log-stream filename :direction :output :if-exists :append
                                          :if-does-not-exist :create
                                          :element-type #+Genera 'standard-char
                                          #-Genera *standard-character-type*
					  #+scl :external-format #+scl :iso-8859-1)
                ,@body)))))

(define-macro with-log-stream ((log) &body body)
  `(with-log-write-lock (,log)
     (%with-log-stream (,log) . ,body)))

(define map-all-logs (function)
  "Maps FUNCTION over all logs."
  (declare (dynamic-extent function))
  (mapc function *all-logs*))

(defparameter *log-classes* '(basic-common-file-log basic-extended-common-file-log common-file-log
                                                    extended-common-file-log http-log basic-url-metering
                                                    extended-http-log notification-log custom-notification-log
						    asynchronous-stream-notification-log)
  "The available log classes.")

(setf (documentation '*log-access-log-class* 'variable)
      (format nil "This variable controls the class of access log used to record HTTP transactions.
If you change this variable other than in the configuration file read on start up, 
use CLEAR-ACCESS-LOGS to clear server logging datastructures. Although you can define or customize
your own log classes (see http:server;log.lisp), a number of predefined log classes are available.

Currently Defined Log Classes~2%
~:{~S~&~A~:^~2%~}" (mapcar #'(lambda (x) (list x (documentation x 'type)))
                           (sort *log-classes* #'string<))))

(defmethod print-object ((log basic-log-mixin) stream)
  (with-slots (name port) log
    (print-unreadable-object (log stream :type t :identity t)
      (format stream "~:[~;~:*~A~]~:[~;~:* (port: ~D)~]" name port))))


;;;------------------------------------------------------------------- 
;;;
;;; TRANSACTIONS
;;;

(defmethod print-object ((transaction transaction) stream)
  (with-slots (url) transaction
    (print-unreadable-object (transaction stream :type t :identity t)
      (when url
        (write-string (url:name-string url) stream)))))

(defmethod print-object ((log http-log) stream)
  (with-slots (name port n-requests) log
    (print-unreadable-object (log stream :type t :identity t)
      (format stream "~A (~D) [~D transactions]" name port n-requests)))) 

(define-generic access-log-p (thing)
  (:documentation "Returns non-null when THING is an access log."))

(defmethod access-log-p ((log access-log)) t)

(defmethod access-log-p (thing) 
  (declare (ignore thing))
  nil) 


;;;------------------------------------------------------------------- 
;;;
;;; CUSTOM NOTIFICATIONS
;;;

(define ensure-extended-access-log (&key (port *standard-http-port*)
                                         (name "Extended-CL-HTTP-Log")
                                         (directory "HTTP:LOGS;Extended;")
                                         (class 'extended-common-file-log))
  "Add a extended common logfile for PORT."
  (multiple-value-bind (log newly-created-p)
      (intern-access-log (or name (default-log-file-name port class))
                         :port port
                         :if-does-not-exist :create
                         :directory directory
                         :class class)
    (log-file-logging-on log t)
    (log-notifications-on log nil)
    (unless newly-created-p
      (start-log-queue log))
    (values log newly-created-p)))

(define create-notification-access-log (predicate notifier &key (name "Notification-Log")
                                                  (port *standard-http-port*)
                                                  (class 'custom-notification-log))
  (check-type predicate function)
  (check-type notifier function)
  (multiple-value-bind (log newly-created-p)
      (intern-access-log (or name (default-log-file-name port class))
			 :port port
			 :directory *standard-log-directory*
			 :class class
			 :if-does-not-exist :create)
    (when newly-created-p
      (setf (notification-log-notifier log) notifier
	    (notification-log-predicate log) predicate))
    (log-notifications-on log t)
    (add-access-log log port)
    (values log newly-created-p)) )

;;;------------------------------------------------------------------- 
;;;
;;; ALLOCATING LOGS
;;;

(defvar *multiport-access-logs* nil
  "An list of all the multiport access logs.")

(declaim (inline multiport-access-logs))

(define multiport-access-logs ()
  "Returns the list of all the multiport access logs."
  (declare (values logs))
  *multiport-access-logs*)

(define default-log-file-name (port &optional (name *log-access-log-class*))
  (format nil "~:(~A-~D~)" name port))

(defvar *standard-access-logs* nil
  "An alist of all the standard access logs indexed by port number.")

(define standard-access-logs (port)
  "Returns the standard access logs for the port, PORT."
  (declare (values logs))
  (etypecase port
    (integer
      (let ((entry (assoc port *standard-access-logs* :test #'eql)))
	(cond (entry (cdr entry))
	      (t (intern-access-log (default-log-file-name port)
				    :port port
				    :directory *standard-log-directory*
				    :class *log-access-log-class*
				    :if-does-not-exist :create)
		 (or (cdr (assoc port *standard-access-logs* :test #'eql))
		     (error "Should never happen: Access log not created!"))))))
    (keyword
      (ecase port
	(:multiport *multiport-access-logs*)))))

(defun %set-standard-logs (port logs)
  (check-type port integer)
  (check-type logs cons)
  (unless (every #'access-log-p logs)
    (error "Bad argument to LOGS, ~S" logs))
  (let ((entry (assoc port *standard-access-logs* :test #'eql)))
    (cond (entry (setf (cdr entry) logs))
          (t (push (list* port logs) *standard-access-logs*)))
    *standard-access-logs*))

(defsetf standard-access-logs %set-standard-logs)

(define-generic find-access-log-if (predicate port &optional start end)
  (:documentation "Finds all access logs for PORT satisfying PREDICATE.
PORT is either an integer denoting a port number or NIL, 
which case all logs are checked."))

(defmethod find-access-log-if (predicate (port integer) &optional (start 0) end)
  (let ((entry (assoc port *standard-access-logs* :test #'eql)))
    (and entry
         (find-if predicate (cdr entry) :start start :end end))))

(defmethod find-access-log-if (predicate (port null) &optional (start 0) end)
  (find-if predicate *all-logs* :start start :end end))

(defmethod find-access-log-if (predicate (port (eql :multiport)) &optional (start 0) end)
  (find-if predicate *multiport-access-logs* :start start :end end))

(define-generic add-access-log (access-log port)
  (:documentation "Adds an access log, ACCESS-LOG, for PORT."))

(defmethod add-access-log ((log access-log) (port integer))
  (let ((entry (assoc port *standard-access-logs* :test #'eql)))
    (cond (entry
           (unless (member log (cdr entry))
             (nconc entry (list log))))
          (t (push (list port log) *standard-access-logs*)))))

(defmethod add-access-log ((log access-log) (port (eql :multiport)))
  (pushnew log *multiport-access-logs*))

(defmethod add-access-log :after ((log process-queued-logging-mixin) port)
  "Ensure that task queue is active"
  (declare (ignore port))
  (tq:ensure-active-task-queue log))

(define-generic remove-access-log (access-log port)
  (:documentation "Removes an access log, ACCESS-LOG, for PORT.
PORT is either an integer or :ALL"))

(defmethod remove-access-log ((log access-log) (port integer))
  (let ((entry (assoc port *standard-access-logs* :test #'eql)))
    (when (member log (cdr entry))
      (delete log entry)
      (unless (cdr entry)
        (setq *standard-access-logs* (delete entry *standard-access-logs*))))))

(defmethod remove-access-log ((log access-log) (port (eql :all)))
  (remove-access-log log :multiport)
  (loop for entry in *standard-access-logs*
        do (when (member log (cdr entry))
             (delete log entry)
             (unless (cdr entry)
               (setq *standard-access-logs* (delete entry *standard-access-logs*))))))

(defmethod remove-access-log ((log access-log) (port (eql :multiport)))
  (setq *multiport-access-logs* (delete log *multiport-access-logs*)))

;; Make sure these clear the task queue
(defmethod remove-access-log :after ((log process-queued-logging-mixin) port)
  (declare (ignore port))
  (tq:clear-task-queue log))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(define ensure-current-log (&optional (port *standard-http-port*))
  "Ensures that a server log has been created and is current.
This means that there must exist on PORT a log of class *LOG-ACCESS-LOG-CLASS*."
  (declare (values logs-for-port))
  (flet ((standard-server-file-log-p (log)
           (typep log *log-access-log-class*)))
    (let ((logs (standard-access-logs port)))
      (unless (some #'standard-server-file-log-p logs)
        (intern-access-log (default-log-file-name port)
                           :port port
                           :directory *standard-log-directory*
                           :class *log-access-log-class*
                           :if-does-not-exist :create)
        (setq logs (standard-access-logs port)))
      (mapc #'start-log-queue logs)
      logs)))

(define current-access-logs ()
  "Returns the current access logs."
  (mapcar #'second *standard-access-logs*))

(define clear-access-logs (&aux logs)
  "Clears all access logs so that none will be known in the environment."
  (setq logs *all-logs*
        *all-logs* nil
        *standard-access-logs* nil
	*multiport-access-logs* nil)
  (mapc #'log-queue-process-kill logs)
  logs)

(define-generic register-log (log)
  (:documentation "Primitive that registers LOG so that it is found by intern-access-log, but does not make the log active."))

(defmethod register-log ((log basic-log-mixin))
  (pushnew log *all-logs* :test #'equalp :key #'log-name))

(define-generic unregister-log (log)
  (:documentation "Primitive that unregisters LOG. LOC should be removed from active ports with remove log."))

(defmethod unregister-log ((log basic-log-mixin))
  (setq *all-logs* (delete log *all-logs*)))

(defmethod unregister-log :after ((log process-queued-logging-mixin))
  (tq:task-queue-process-kill log))

(define intern-access-log (name &key (port *standard-http-port*)
                                (if-does-not-exist :error)
                                (directory *standard-log-directory*)
                                host
                                (class *log-access-log-class*))
  "Interns a server access log named, NAME, that monitors port, PORT.
If port is null, this returns any log whose name and class match.
If port is :MULTIPORT, it returns a multiport log."
  (declare (values log newly-created-p))
  (flet ((equal-log-p (x)
           (and (eq port (log-port x))
		(equalp name (log-name x))
                (typep x class)))
	 (handle-does-not-exist (name port class host)
	   (ecase if-does-not-exist
	     (:soft nil)
	     (:create
	       (let* ((*standard-log-directory* directory)
		      (log (allocate-log :name name :port port :class class :local-host (or host (local-host)))))
		 (when port (add-access-log log port))
		 (values log t)))
	     (:error (error "Unknown HTTP server access log, ~A~@[, for port ~D~]." name port)))))
    (declare (dynamic-extent #'equal-log-p))
    (etypecase name
      (string
        (cond
          ((find-if #'equal-log-p *all-logs*))
          (t (handle-does-not-exist name port class host))))
      (basic-log-mixin
	(if (and (or (null port) (eql port (log-port name)))
		 (typep name class))
	    name
	    (handle-does-not-exist (log-name name) port class host))))))

(define-generic unintern-access-log (log)
  (:documentation "Removes LOG from any ports and uninterns it."))

(defmethod unintern-access-log ((log basic-log-mixin))
  (remove-access-log log :all)
  (unregister-log log))

(defmethod initialize-instance :after ((log basic-log-mixin) &key &allow-other-keys)
  (with-slots (creation-time) log
    (register-log log)
    (setq creation-time (get-universal-time))
    log))

(defgeneric %file-name-for-log (log)
  (declare (values filename-string))
  (:documentation "Cons the string to use as the filename for LOG's file log."))

(defmethod %file-name-for-log ((log basic-log-mixin))
   (with-slots (log-file-name port) log
     (concatenate (string-type-symbol log-file-name) log-file-name "-" (write-to-string port :base 10.))))

(define-generic initialize-log-filename (file-logging-mixin)
  (declare (values log))
  (:documentation "Initializes the log file and directory for FILE-LOGGING-MIXIN."))

;; default method
(defmethod initialize-log-filename (log) log)

(defmethod initialize-log-filename ((logs cons))
  (mapcar #'initialize-log-filename logs))

(defmethod initialize-log-filename ((log file-logging-mixin))
  (with-slots (filename) log
    (let* ((pathname (translated-pathname *standard-log-directory*))
	   (name-for-file (%file-name-for-log log)))
      (unless (probe-directory pathname)
	(www-utils:create-directories-recursively pathname))
      (setf filename (www-utils:%make-log-pathname
                      (pathname-device pathname)
                      (pathname-directory pathname) name-for-file (pathname-host pathname)))
      log)))

(defmethod initialize-instance :after ((log file-logging-mixin) &key &allow-other-keys)
  (initialize-log-filename log))

(defmethod initialize-instance :after ((log log-locking-mixin) &key &allow-other-keys)
  (with-slots (lock) log
    (setq lock (www-utils:make-lock "HTTP Log Lock" :type :simple))
    log))

(defmethod initialize-instance :after ((log dynamic-loggin-mixin) &key (size *default-log-size*) &allow-other-keys)
  (declare (fixnum size))
  (macrolet ((make-table (table size)
               `(if ,table
                    (clrhash ,table )
                    (setf ,table (make-hash-table :size ,size)))))
    (with-slots (url-table) log
      (make-table url-table size)
      (initialize-host-name-space log (floor size 10.))
      log)))

(define allocate-log (&key (name "CL-HTTP-Log")
                           (local-host (local-host))
                           (port *standard-http-port*)
                           (class *log-access-log-class*))
  (make-instance class :name name :local-host local-host :port port))

;;;------------------------------------------------------------------- 
;;;
;;;  ACCESSING THE FILE STREAM FOR A LOG
;;;

(define-generic open-log-stream-p (log)
  (:documentation "Returns non-null when the log stream is open."))

(defmethod open-log-stream-p ((log file-logging-mixin))
  (with-slots (file-stream) log
    (and file-stream (open-stream-p file-stream))))

(define-generic %log-stream (log)
  (:documentation "Return the log-stream, openning it if necessary.
Does not treat concurency issues."))

(defmethod %log-stream ((log file-logging-mixin))
  (with-slots (filename file-stream) log
    (unless (and file-stream (open-stream-p file-stream))
      ;; the idea here is to maintain a visible record of the log file in the
      ;; file system.  If the file does to exist, create it.
      (unless (probe-file filename)
        (with-open-file (fstream filename :direction :output :if-does-not-exist :create)
          #-(OR Genera Allegro ACLNT CMU sbcl clisp scl) (declare (ignore fstream))))
      ;; then reopen and remember it is open
      (setq file-stream (open filename :direction :output :if-exists :append  #+scl :external-format #+scl :iso-8859-1)))
    file-stream))

(define-generic log-stream (log)
  (:documentation "Return the log-stream, openning it if necessary.
Does treat concurency issues."))

(defmethod log-stream ((log file-logging-mixin))
  (with-slots (file-stream) log
    (unless (and file-stream (open-stream-p file-stream))
      (with-log-write-lock (log)
        (unless (open-log-stream-p log)
          (%log-stream log))))
    file-stream))

(define-generic %close-log-stream (log)
  (:documentation "Closes the file log stream if it is open."))

(defmethod %close-log-stream ((log file-logging-mixin))
  (with-slots (file-stream) log
    (when (open-log-stream-p log)
      (force-output file-stream)
      (close file-stream))))

(define-generic close-log-stream (log)
  (:documentation "Carefully closes the file log stream if it is open."))

(defmethod close-log-stream ((log file-logging-mixin))
  (with-log-write-lock (log)
    (%close-log-stream log)))

(defmethod close-log-stream (log)
  (declare (ignore log))
  nil)

(define close-all-logs ()
  "Closes any open file streams for all HTTP logs."
  (mapc #'close-log-stream *all-logs*))

;;;------------------------------------------------------------------- 
;;;
;;; SETTING SWITCHES
;;;

;; Provide default method
(defmethod log-notification (log)
  (declare (ignore log))
  nil)

(defmethod log-notifications ((logs cons))
  (loop for log in logs
        when (log-notification log)
          return t
        finally (return nil)))

(define-generic log-notifications-on (log-or-logs &optional on-p)
  (:documentation "Turns notifications on and off according to on-p."))

(defmethod log-notifications-on (log &optional on-p)
  (declare (ignore log on-p)))

(defmethod log-notifications-on ((log log-notification-mixin) &optional on-p)
  (setf (log-notification log) (if on-p :tv nil)))

(defmethod log-notifications-on ((logs cons) &optional on-p)
  (dolist (log logs)
    (log-notifications-on log on-p))
  logs)

(define-generic log-dynamic-logging-on (log-or-logs &optional on-p)
  (:documentation "Turns dynamic logging on and off according to ON-P."))

(defmethod log-dynamic-logging-on (log &optional on-p)
  (declare (ignore log on-p)))

(defmethod log-dynamic-logging-on ((log dynamic-loggin-mixin) &optional on-p)
  (setf (log-dynamic-logging log) (if on-p t nil)))

(defmethod log-dynamic-logging-on ((logs cons) &optional on-p)
  (dolist (log logs)
    (log-dynamic-logging-on log on-p))
  logs)

(define-generic log-file-logging-on (log-or-logs &optional on-p)
  (:documentation "Turns file logging on and off according to ON-P."))

(defmethod log-file-logging-on (log &optional on-p)
  (declare (ignore log on-p)))

(defmethod log-file-logging-on ((log file-logging-mixin) &optional on-p)
  (setf (log-file-logging log) (if on-p t nil)))

(defmethod log-file-logging-on ((logs cons) &optional on-p)
  (dolist (log logs)
    (log-file-logging-on log on-p))
  logs)

(define-generic log-compression-on (log-or-logs &optional on-p)
  (:documentation "Turns compression of log files for LOG-OR-LOGS on or off according to ON-P."))

(defmethod log-compression-on ((log compressed-file-logging-mixin) &optional on-p)
  (setf (log-compression-p log) (if on-p t nil)))

(defmethod log-compression-on ((log file-logging-mixin) &optional on-p)
  (not (null on-p)))

(defmethod log-compression-on ((log-or-logs cons) &optional on-p)
  (dolist (log log-or-logs)
    (setf (log-compression-p log) on-p)))

(define-generic log-times-in-gmt-p (log)
  (:documentation "Returns non-null when log times should be written in GMT."))

(defmethod log-times-in-gmt-p (log)
  (declare (ignore log))
  *log-times-in-gmt*)

(define-generic log-server-access-logs-proxy-access (log-or-logs on-p)
  (:documentation "Turns on and off whether normal server logs also log proxy accesses."))

(defmethod log-server-access-logs-proxy-access (log on-p)
  (declare (ignore log on-p)))

(defmethod log-server-access-logs-proxy-access ((logs cons) on-p)
  (dolist (log logs)
    (log-server-access-logs-proxy-access log on-p))
  logs)

(defmethod log-server-access-logs-proxy-access ((log common-file-log) (on-p null))
  (change-class log 'server-common-file-log))

(defmethod log-server-access-logs-proxy-access ((log server-common-file-log) on-p)
  (when on-p
    (change-class log 'common-file-log)))

(defmethod log-server-access-logs-proxy-access ((log extended-common-file-log) (on-p null))
  (change-class log 'server-extended-common-file-log))

(defmethod log-server-access-logs-proxy-access ((log server-extended-common-file-log) on-p)
  (when on-p
    (change-class log 'extended-common-file-log)))

;;;------------------------------------------------------------------- 
;;;
;;; COUNTING
;;;

;; see *status-code-alist* for the current list of status codes.
(declaim (inline %note-access-status-code))

(defun %note-access-status-code (log status-code proxy-p)
  (check-type status-code integer)
  ;; with-slots required for locatives to work in Genera.
  (with-slots (n-requests n-requests-served n-server-errors n-client-errors
                          n-insufficient-resource-denials n-redirects n-access-denials
			  n-gateway-errors n-proxy-requests n-proxy-cache-hits) log
    (atomic-incf n-requests)
    (when proxy-p
      (atomic-incf n-proxy-requests)
      (when  (eql proxy-p :cache-hit)
	(atomic-incf n-proxy-cache-hits)))
    (cond
      ((< 199 status-code 300) (atomic-incf n-requests-served))
      ((< 299 status-code 400) (atomic-incf n-redirects))
      ((and (< 400 status-code 416) (/= status-code 408))
       (atomic-incf n-access-denials))
      ((member status-code '(400 408) :test #'=)
       (atomic-incf n-client-errors))
      ((< 499 status-code)
       (case status-code
	 ((500 501 505) (atomic-incf n-server-errors))
	 (502 (atomic-incf n-gateway-errors))
	 (503 (atomic-incf n-insufficient-resource-denials)))))
    log))

(define-generic note-access-status-code (log status-code proxy-p)
  (:documentation "Updates counters in LOG according to status-code."))

(defmethod note-access-status-code ((log log-counters-mixin) status-code proxy-p)
  (%note-access-status-code log status-code proxy-p))

(declaim (inline %note-http-method))

(defun %note-http-method (log method)
  ;; with-slots required for locatives to work in Genera.
  (with-slots (n-gets n-heads n-posts n-puts n-deletes
                      n-options n-traces n-extension-methods) log
    (case method
      (:get (atomic-incf n-gets))
      (:head (atomic-incf n-heads))
      (:post (atomic-incf n-posts))
      (:put (atomic-incf n-puts))
      (:delete (atomic-incf n-deletes))
      (:options (atomic-incf n-options))
      (:trace (atomic-incf n-traces))
      (t (typecase method
           (keyword (atomic-incf n-extension-methods)))))))

(define-generic note-http-method (log method)
  (:documentation "Updates counters in LOG according to HTTP method."))

(defmethod note-http-method ((log log-counters-mixin) method)
  (%note-http-method log method))

(declaim (inline %note-http-connections))

(defun %note-http-connections (log requests-completed)
  (with-slots (n-connections) log
    (when (< requests-completed 2)
      (atomic-incf n-connections))))

(define-generic note-http-connections (log requests-completed)
  (:documentation "Updates counters in LOG according to REQUESTS-COMPLETED."))

(defmethod note-http-connections ((log log-counters-mixin) requests-completed)
  (%note-http-connections log requests-completed))

(defmethod elapsed-seconds ((log log-counters-mixin))
  (truncate (elapsed-time log) internal-time-units-per-second))

(define ensure-multiport-statistics-log ()
  "Ensures that a multi-port statistics log is running."
  (ensure-extended-access-log :name "Multi-Port-Statistics-Log" :port :multiport :class 'access-statistics-log))

(add-initialization
  "Ensure Multiport Server Statistics"
  '(ensure-multiport-statistics-log)
  '(:normal)
  '*server-initialization-list*)


;;;------------------------------------------------------------------- 
;;;
;;; METERING LOGS
;;;

(define enable-url-metering (&key (port *standard-http-port*) (class 'basic-url-metering))
  "Enable URL metering on port with metering class, CLASS."
  (multiple-value-bind (log newly-created-p)
      (intern-access-log "URL-Metering-Log"
                         :port port
                         :if-does-not-exist :create
                         :class class)
    (setq *proxy-intern-url-status* :create)
    (unless newly-created-p
      (add-access-log log port))
    (values log newly-created-p)))

(define disable-url-metering (&key (port *standard-http-port*) (class 'basic-url-metering))
  "Disables URL metering on PORT for metering class, CLASS.
If metering proxy activity, call URL:GC-URL-TABLE to clean up remote URL 
after extracting metering results ."
  (let ((log (intern-access-log "URL-Metering-Log" :port port
                                :if-does-not-exist :soft
                                :class class)))
    (setq *proxy-intern-url-status*  :uninterned)
    (when log
      (remove-access-log log port))))

;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(defmethod log-host-transaction ((host host) (transaction transaction))
  (with-slots (http-transactions http-n-transactions) host
    (push transaction http-transactions)
    (incf http-n-transactions)
    transaction))

(defmethod register-transaction ((transaction transaction) (log dynamic-loggin-mixin))
  (with-slots (url-table n-transactions) log
    (with-slots (method) transaction
      (let* ((url (transaction-url transaction))
             (entry (gethash url url-table))
             (bucket (and entry (assoc method entry))))
        (cond (bucket
               (push transaction (cddr bucket))
               (incf (second bucket)))
              (entry
               (push `(,method 1 ,transaction) (cdr entry)))
              (t (setf (gethash url url-table) `((,method 1 ,transaction))))))
      (incf n-transactions)
      ;; tack the transaction onto the host
      (log-host-transaction (transaction-host transaction) transaction)
      transaction)))

(define-generic log-transaction (log url host method time http-version &rest parameters))

(defmethod log-transaction ((log dynamic-loggin-mixin) (url url) (host host) method time http-version &rest parameters)
  (check-type method keyword)
  (check-type time integer)
  (destructuring-bind (&key user status bytes) parameters
    (make-instance 'transaction
                   :url url
                   :host host
                   :user user
                   :method method
                   :time time
                   :server-version http-version
                   :status status 
                   :bytes bytes)))

(defmethod log-transaction :around ((log dynamic-loggin-mixin) (url url) (host host) method time http-version &rest parameters)
  (let ((transaction (apply #'call-next-method log url host method time http-version parameters)))
    (unless (host-http-version host)
      (setf (host-http-version host) http-version))
    (register-transaction transaction log)))

(defmethod log-transaction ((log extended-dynamic-loggin-mixin) (url url) (host host) method time http-version &rest parameters)
  (check-type method keyword)
  (check-type time integer)
  (destructuring-bind (&key user status bytes user-agent referrer) parameters
    (make-instance 'extended-common-log-transaction
                   :url url
                   :host host
                   :user user
                   :method method
                   :time time
                   :server-version http-version
                   :status status 
                   :bytes bytes
                   :user-agent user-agent
                   :referrer referrer)))

;;;------------------------------------------------------------------- 
;;;
;;; RENAME LOG FILES
;;;

(define-parameter *log-pathname-universal-time-offset* 0
                  "The offset in seconds used when creating pathnames for log files.
Use this to ensure that when log files are switched each day, the name reflects
the right day rather than the next day.")

(define-generic log-date-pathname (log &optional ut-offset)
  (declare (values pathname))
  (:documentation "Returns an ISO date prefixed log name.
UT-OFFSET is an offset in seconds to use when creating the pathname.
This defaults to *log-pathname-universal-time-offset*."))

(defmethod log-date-pathname ((log file-logging-mixin) &optional (ut-offset *log-pathname-universal-time-offset*))
  (with-slots (filename) log
    (flet ((date-name (log-name)
             (multiple-value-bind (sec min hour day month year)
                 (decode-universal-time (+ (get-universal-time) ut-offset))
               (declare (ignore sec min hour))
               (values (write-to-string year :base 10.)
                       (format nil "~2,'0D" month)
                       (let ((name (pathname-external-name-string log-name)))
                         (concatenate (string-type-symbol name) (write-iso-date year month day nil) "-" name))))))
      (declare (inline date-name))
      (multiple-value-bind (year month name)
          (date-name (pathname-name filename))
        (make-pathname :host (pathname-host filename)
                       :directory `(:absolute ,@(cdr (pathname-directory filename)) ,year ,month)
                       :name name
                       :type (pathname-type filename)
                       :version (pathname-version filename))))))

(define-generic switch-log-file (log)
  (:documentation "Switch log files by renaming the current log file to the current date."))

(defmethod switch-log-file ((log file-logging-mixin))
  (with-slots (filename lock) log
    (www-utils:with-lock-held (lock :write)
      (%close-log-stream log)
      (when (probe-file filename)
        (let ((new-name (log-date-pathname log)))
          (pathname-create-directory-if-needed new-name)
          (rename-file filename new-name))))))

(defmethod log-compression-p ((log file-logging-mixin)) nil)

(defmethod switch-log-file :around ((log compressed-file-logging-mixin))
  (with-slots (compression-mode) log
    (multiple-value-bind (new-pathname old-pathname logfile-pathname)
	(call-next-method)
      (cond ((and (log-compression-p log) compression-mode logfile-pathname)
	     (handler-case 
	       (values new-pathname
		       old-pathname
		       (compress-file logfile-pathname compression-mode :delete t))
	       (error (error)
		      (report-bug *bug-http-server* (format nil "HTTP Log Compression Error: ~S" (type-of error))
				  "~&Log: ~S~&Compression-Mode: ~S~&Status: Log file may not be compressed or deleted.~
                              ~&Error: ~S~:[~;~&Error Report: ~:*~A~]~:[~;~&Backtrace: ~:*~A~]"
				  log compression-mode (type-of error) (report-string error)
				  (when *stack-backtraces-in-bug-reports*
				    (stack-backtrace-string error))))))
	    (t (values new-pathname old-pathname logfile-pathname))))))

(defmethod switch-log-file (log)
  (declare (ignore log))
  nil)

(define switch-all-log-files ()
  "Switches all open log files."
  (map-all-logs #'switch-log-file))

;; add the daily task to swicth the log files.
(www-utils:add-periodic-task "Switch Log Files" :daily '(switch-all-log-files))


;;;------------------------------------------------------------------- 
;;;
;;; ASYNCHRONOUS LOG-ENTRY LOGGING HYGINE
;;;

(defmethod tq:clear-task-queue :around ((task-queue asynchronous-log-entry-logging-mixin) &aux flushed-queue)
  (unwind-protect
      (setq flushed-queue (call-next-method))
    (mapc #'deallocate-log-entry flushed-queue)))

(defmethod tq:task-queue-execute-task ((task-queue asynchronous-log-entry-logging-mixin) (log-entry log-entry))
  (declare (optimize (speed 3)))
  (unwind-protect
      (write-log-entry log-entry nil)
    (deallocate-log-entry log-entry)))


;;;------------------------------------------------------------------- 
;;;
;;; PROCESS QUEUED LOGGING
;;;

(defmethod initialize-instance :after ((log process-queued-logging-mixin) &key &allow-other-keys)
  ;; intialization some strings
  (setf (tq:task-queue-wait-whostate log) "Log Wait"
        (tq:task-queue-process-name log) (concatenate 'string "HTTP-" (substitute #\- #\space (log-name log)) "-Daemon"))
  ;; start up the process
  (start-log-queue log)
  log)

(defmethod tq:task-queue-execute-pending-tasks ((log basic-process-queued-file-logging-mixin))
  (labels ((report-logging-error (log error fatal-p)
             (let ((error-type (type-of error)))
               (report-bug *bug-http-server* (format nil "HTTP~:[~; Fatal~] Logging Error: ~S" fatal-p error-type)
                           "~:[~;~&Logging has been suspended. Attend to the error at once and resume logging.~]~
                            ~&Log: ~S~&Error: ~S~:[~;~&Error Report: ~:*~A~]~:[~;~&Backtrace: ~:*~A~]"
                           fatal-p log error-type (report-string error)
                           (when *stack-backtraces-in-bug-reports*
                             (stack-backtrace-string error)))))
           (%handle-logging-error (error)
             (typecase error
               ((or network-error file-error)
                (report-logging-error log error nil)
                (sleep 5)
                t)
               (t (report-logging-error log error t)
                  (stop-log-queue log)
                  t))))
    (declare (dynamic-extent #'%handle-logging-error))
    (handler-bind-if (not *debug-server*)
       ((error #'%handle-logging-error))
      (%with-log-stream (log :open-file-forces-output-p t)
        (loop with log-entry
              while (and (unwind-protect
			     (when (setq log-entry (tq:pop-task-queue log))
			       (write-log-entry log-entry log-stream)
			       t)
			   (when log-entry
			     (deallocate-log-entry log-entry)))
			 (tq:task-queue-run-p log)))))))

(define-generic start-log-queue (log)
  (:documentation "Starts LOG-QUEUE writing log entries by activing its process."))

(defmethod start-log-queue (log)
  (declare (ignore log))
  nil)

(defmethod start-log-queue ((log process-queued-logging-mixin))
  (tq:start-task-queue log))

(define start-all-log-queues ()
  "Starts all processes associated with logging."
  (map-all-logs #'start-log-queue))

(define-generic log-queue-process-kill (log)
  (:documentation "Stops the log queue process and kills it."))

(defmethod log-queue-process-kill (log)
  (declare (ignore log)))

(defmethod log-queue-process-kill ((log process-queued-logging-mixin))
  (tq:task-queue-process-kill log))

(define-generic stop-log-queue (log)
  (:documentation "Stops LOG-QUEUE queue from writing log entries by stopping its process."))

(defmethod stop-log-queue (log)
  (declare (ignore log))
  nil)

(defmethod stop-log-queue ((log process-queued-logging-mixin))
  (tq:stop-task-queue log))

;; Specialize the primary method to run clean up activity.
(defmethod tq:stop-task-queue ((log process-queued-logging-mixin))
  ;; Close the log stream if it is open
  (close-log-stream log))

(define stop-all-log-queues ()
  "Stops all processes associated with logging."
  (map-all-logs #'stop-log-queue))

(define-generic ensure-active-log-queue (log)
  (:documentation "Ensures that log-queue is active and writing log entries."))

(defmethod ensure-active-log-queue ((log process-queued-logging-mixin))
  (tq:ensure-active-task-queue log))

;; stop the log process because it runs without looking at the lock
(defmethod switch-log-file :around ((log basic-file-logging-mixin))
  (unwind-protect
      (progn (stop-log-queue log)               ;stop the log queue before switching log files
             (call-next-method))                ;do it and return multiple values from the next method
    (start-log-queue log)))                     ;restart

(define-generic log-entry-p (log agent)
  (:documentation "Returns non-null when LOG should write a log entry for the current state of AGENT.
Methods are combined with AND method combination so that NIL is returned if any method returns NIL.")
  (:method-combination and))

(defmethod log-entry-p and ((log file-logging-mixin) (server server-logging-mixin))
  (log-file-logging log))

(defmethod log-entry-p and ((log dynamic-loggin-mixin) (server server-logging-mixin))
  (log-dynamic-logging log))

;; Predicate that decides when we have a proxy log
(defmethod log-entry-p and ((log exclusive-proxy-log-mixin) (server proxy-server-mixin))
  (declare (optimize (speed 3)))
  (server-proxy-request-p server))

;; Predicate that decides when we have a normal server log
(defmethod log-entry-p and ((log exclusive-server-log-mixin) (server proxy-server-mixin))
  (not (server-proxy-request-p server)))

;;;------------------------------------------------------------------- 
;;;
;;; WRITING COMMON LOG ENTRIES
;;;

;; Common Logfile Format (see
;; http://www.w3.org/hypertext/WWW/Daemon/User/Config/Logging.html)

(declaim (inline %write-common-logfile-entry))

(defun %write-common-logfile-entry (host-name request request-time status bytes user-name
                                              &optional (gmt-p *log-times-in-gmt*) (stream *standard-output*)
                                              (delimiter #\space))
  (macrolet ((write-delimiter (stream)
               `(write-char delimiter ,stream)))
    ;; host domain name or IP address.
    (write-string host-name stream)
    ;; RFC932 logname
    (write-delimiter stream)
    (write-char #\- stream)                     ;copy 85% UNIX mentality of NCSA server
    #|(write-rfc-931-logname server stream)|#
    ;; Authenticated User Name
    (write-delimiter stream)
    (if user-name (write user-name :escape t :stream stream) (write-char #\- stream))
    ;; date and time
    (write-delimiter stream)
    (write-char #\[ stream)
    ;; Canonical times in GMT to adhere to the standard and enhance
    ;; portability/comparability.  9/29/95 -- JCMa.
    (if gmt-p
        (write-standard-time request-time stream nil 0)
        (write-standard-time request-time stream t))
    (write-char #\] stream)
    ;; Exact request received from client.
    (write-delimiter stream)
    ;; What should really be done when there is no request string, probably because the
    ;; client got a 408 -- request timeout.  7/20/95 -- JCMa.
    (write (or request "") :stream stream :escape t)
    ;; Status code returned to the client.
    (write-delimiter stream)
    (write status :stream stream :base 10. :escape nil)
    ;; Number of bytes transfered to the client.
    (write-delimiter stream)
    (write bytes :stream stream :base 10. :escape nil)))

(declaim (inline %write-extended-common-logfile-entry))

(defun %write-extended-common-logfile-entry (host-name request request-time status bytes user-name user-agent referer
                                                       &optional (gmt-p *log-times-in-gmt*) (stream *standard-output*)
                                                       (delimiter #\space))
  (macrolet ((write-delimiter (stream)
               `(write-char delimiter ,stream)))
    (%write-common-logfile-entry host-name request request-time status bytes user-name gmt-p stream delimiter)
    (write-delimiter stream)
    (if user-agent (write user-agent :stream stream :escape t) (write-char #\- stream))
    (write-delimiter stream)
    (if referer (write referer :stream stream :escape t) (write-char #\- stream))))

;; PROXY-P must be the value returned by SERVER-PROXY-REQUEST-P because it may be :cache-hit
(defun %write-console-window-notification (host-name port proxy-p requests-per-connection request request-time status bytes user-name user-agent referer
						     cpu-time elapsed-time stream &aux (tab #\tab))
  (flet ((write-milliseconds (milliseconds stream) ;; "Writes milliseconds in 4 characters."
	   (cond ((< milliseconds 1000)
		  (prin1 (float (/ (round milliseconds 10) 100)) stream))
		 ((< milliseconds 10000)
		  (prin1 (float (/ (round milliseconds 10) 100)) stream))
		 ((< milliseconds 100000)
		  (prin1 (float (/ (round milliseconds 100) 10)) stream))
		 (t (prin1 (round milliseconds 1000) stream))))
	 (write-microseconds (microseconds stream) ;; "Writes microseconds in 4 characters."
	   (let* ((milliseconds (round microseconds 1000)))
	     (cond ((> 10 milliseconds)
		    (write-string "   " stream))
		   ((> 100 milliseconds)
		    (write-string "  " stream))
		   ((> 1000 milliseconds)
		    (write-char #\space stream)))
	     (prin1 milliseconds stream)))
         (get-request-indices (request)
           (when request
             (let* ((end (length request))
                    (pos1 (and (not (zerop end)) (%fast-position-if white-space-char-p request :start 0 :end end)))
                    (pos2 (and pos1 (%fast-position-if white-space-char-p request :start (1+ pos1) :end end))))
               (values pos2 (and pos1 end))))))
    (declare (inline write-microseconds write-milliseconds get-request-indices))
    (multiple-value-bind (http-version-pos request-length)
        (get-request-indices request)
      ;; date and time
      (cond (request-time
	     (write-char #\[ stream)
	     (multiple-value-bind (seconds minutes hours)
		 (decode-universal-time request-time)
	       (write-24-hour-time hours minutes seconds stream)
	       ;; milliseconds of elapsed time
	       (write-char #\space stream)
	       (write-milliseconds elapsed-time stream)
	       ;; microsecond CPU time
	       (write-char #\space stream)
	       (write-microseconds cpu-time stream)
	       (write-char #\] stream)))
	    (t (setq tab #\space)
	       (write-char #\{ stream)
	       (write-milliseconds elapsed-time stream)
	       (write-char #\space stream)
	       (write-microseconds cpu-time stream)
	       (write-char #\} stream)))
      ;; host domain name or IP address.
      (write-char tab stream)
      (write-string host-name stream)
      (write-char tab stream)
      ;; Status code returned to the client.
      (prin1 status stream)
      ;; HTTP version
      (write-char #\space stream)
      (cond (http-version-pos
             (write-string request stream :start (1+ http-version-pos) :end request-length))
            (request-length
             (write-string "HTTP/0.9" stream))
            (t (write-string "HTTP/?.?" stream)))
      ;; Server access port
      (fast-format stream " ~D" port)
      (write-char #\space stream)
      ;; number of requests per connection
      (fast-format stream "{~D}" requests-per-connection)
      ;; Number of bytes transfered to the client.
      (write-char #\space stream)
      (prin1 bytes stream)
      ;; Authenticated User Name
      (write-char tab stream)
      (if user-name (write user-name :escape t :stream stream) (write-char #\- stream))
      (when proxy-p
	(write-char #\space stream)
	(write-char (if (eql :cache-hit proxy-p) #\+ #\-) stream))
            ;; Exact request received from client.
      (write-char #\space stream)
      (write-char #\" stream)
      (cond (http-version-pos
             (write-string request stream :start 0 :end http-version-pos))
            (request-length
             (write-string request stream :start 0 :end request-length)))
      (write-char #\" stream)
      (unless proxy-p
	(write-char tab stream)
	(if user-agent (write user-agent :stream stream :escape t) (write-char #\- stream))
	(write-char #\space stream)
	(if referer (write referer :stream stream :escape t) (write-char #\- stream)))
      ;; Trailing CR makes this consistently parsable.
      (terpri stream))))


;;;------------------------------------------------------------------- 
;;;
;;; LOGGING POST METHOD
;;;

(defconstant *log-form-special-characters* '(#\tab #\return #\= #\&)
  "Special characters used for encoding forms POST logs.")

(defun print-form-alist-log-entry (form-alist stream)
  (loop for (keyword . values) in form-alist
	do (loop for value in values
		 do (write-escaped-string (symbol-name keyword) *log-form-special-characters* stream)
		    (fast-format stream "~I=~I&"
				 (write-escaped-string (symbol-name keyword) *log-form-special-characters* stream)
				 (write-escaped-string value *log-form-special-characters* stream)))))

(defun parse-form-alist-log-entry (string &optional (start 0) (end (length string)))
  (flet ((get-keyword (string &optional (start 0) (end (length string)))
	   (declare (fixnum start end))
	   (multiple-value-bind (string unescaped-p new-string-p)
	       (string-unescape-special-chars string start end)
	     unescaped-p
	     (if new-string-p
		 (%tokenize-form-query-keyword string)
		 (%tokenize-form-query-keyword string start end)))))
    (declare (inline get-keyword))
    (unless (= start end)
      (with-fast-array-references ((string string string))
        (loop for s1 = start then (1+ (the fixnum e2))
              while (< s1 end)
              for e1 = (or (char-position #\= string s1 end)
                           (error "ill-formed query alist encoding in ~s" (subseq string start end)))
              for s2 = (1+ (the fixnum e1))
              for e2 = (or (char-position #\& string s2 end) end)
              for keyword = (get-keyword string s1 e1)
              for value = (unless (= s2 e2)
                            (string-unescape-special-chars string s2 e2))
              collect `(,keyword ,value))))))

(declaim (inline %write-http-post-logfile-entry))

(defun %write-http-post-logfile-entry (host-name user-name request-time request status bytes-received bytes-transmitted
						 user-agent referer form-alist
						 &optional (gmt-p *log-times-in-gmt*) (stream *standard-output*)
						 (delimiter #\tab))
  (macrolet ((write-delimiter (stream)
               `(write-char delimiter ,stream)))
    ;; host domain name or IP address.
    (write-string host-name stream)
    ;; Authenticated User Name
    (write-delimiter stream)
    (if user-name (write user-name :escape t :stream stream) (write-char #\- stream))
    ;; date and time
    (write-delimiter stream)
    (write-char #\[ stream)
    ;; Canonical times in GMT to adhere to the standard and enhance
    ;; portability/comparability.  9/29/95 -- JCMa.
    (if gmt-p
        (write-standard-time request-time stream nil 0)
        (write-standard-time request-time stream t))
    (write-char #\] stream)
    ;; Exact request received from client.
    (write-delimiter stream)
    ;; What should really be done when there is no request string, probably because the
    ;; client got a 408 -- request timeout.  7/20/95 -- JCMa.
    (write (or request "") :stream stream :escape t)
    ;; Status code returned to the client.
    (write-delimiter stream)
    (write status :stream stream :base 10. :escape nil)
    ;; Number of bytes received from the client.
    (write-delimiter stream)
    (write bytes-received :stream stream :base 10. :escape nil)
    ;; Number of bytes transfered to the client.
    (write-delimiter stream)
    (write bytes-transmitted :stream stream :base 10. :escape nil)
    ;; User Agent
    (write-delimiter stream)
    (if user-agent (write user-agent :stream stream :escape t) (write-char #\- stream))
    (write-delimiter stream)
    ;; Referrer
    (if referer (write referer :stream stream :escape t) (write-char #\- stream))
    ;; Write form alist
    (write-delimiter stream)
    (print-form-alist-log-entry form-alist stream)))

(defun %server-write-post-logfile-entry (server log-stream gmt-p delimiter)
  (let* ((host-name (host-log-name server))
	 (user-name (%server-user-qualified-name server))
	 (request-time (server-request-time server))
	 (request (server-request server t))
	 (status (server-status server))
	 (bytes-transmitted (server-bytes-transmitted server))
	 (bytes-received (server-bytes-received server))
	 (form-alist (server-form-alist server))
	 (headers (server-headers server))
	 (user-agent (get-header :user-agent headers))
	 (referer (get-header :referer headers)))
    (%write-http-post-logfile-entry host-name user-name request-time request status bytes-received bytes-transmitted
				    user-agent referer form-alist gmt-p log-stream delimiter)))

(define enable-post-logging (&key (port *standard-http-port*) (class 'post-log))
  "Enable HTTP POST logging on port with log class, CLASS."
  (multiple-value-bind (log newly-created-p)
      (intern-access-log "Post-Log" :port port :if-does-not-exist :create :class class)
    (unless newly-created-p
      (add-access-log log port))
    (values log newly-created-p)))

(define disable-post-logging (&key (port *standard-http-port*) (class 'post-log))
  "Disables HTTP POST logging on port with log class, CLASS."
  (let ((log (intern-access-log "Post-Log" :port port :if-does-not-exist :soft :class class)))
    (when log
      (remove-access-log log port))))

;;;------------------------------------------------------------------- 
;;;
;;; PARSE LOG FILES
;;;

;;eis.calstate.edu [Tue May 17 03:43:00 1994] GET /stocks.html HTTP/1.0

;(%parse-ncsa-http-1-2-log-entry "eis.calstate.edu [Tue May 17 03:43:00 1994] GET /stocks.html HTTP/1.0")

(defun %parse-ncsa-http-1-2-log-entry (line &aux (l (length line)))
  (declare (values url host method date server-version status byte-content))
  (flet ((white-space-p (x)
           (char= x #\space)))
    (declare (inline white-space-p))
    (let (p1 p2 p3 p4 p5 p6 p7)
      (cond
        ((and (setq p1 (position-if #'white-space-p line :start 0 :end l))
              (setq p2 (char-position #\[ line p1 l))
              (setq p3 (char-position #\] line p2 l))
              (setq p4 (position-if-not #'white-space-p line :start (the fixnum (1+ p3)) :end l))
              (setq p5 (position-if #'white-space-p line :start p4 :end l))
              (setq p6 (position-if-not #'white-space-p line :start p5 :end l)))
         (setq p7 (position-if #'white-space-p line :start p6 :end l))
         (values (subseq line p6 (or p7 l))
                 (subseq line 0 p1)
                 (intern (nstring-upcase (subseq line p4 p5)) *keyword-package*)
                 (parse-gmt-time line (the fixnum (1+ p2)) p3)
                 (when p7
                   (let ((p8 (position-if-not #'white-space-p line :start p7 :end l)))
                     (subseq line p8 l))) nil nil)) ;; no status or byte-content info for NCSA.
        (t (values nil nil :bad-record))))))

; (%parse-common-log-format-log-entry "GATOR-MAC-9.AI.MIT.EDU - - [1994-09-28 00:51:21] \"GET /homepage HTTP/1.0\" 200 3856")

(defun %parse-common-log-format-log-entry (line &aux (end (length line)))
  (declare (values url host method date server-version status bytes user-name)
	   (fixnum end)
           (optimize (speed 3)))
  (labels ((white-space-p (x)
	     (char= x #\space))
	   (parse-exact-request (exact-request start end)
	     (declare (fixnum end))
	     (unless (eql start end)
	       (let* ((r1 (or (%fast-position-if white-space-p exact-request :start start :end end)
                              (return-from parse-exact-request nil)))
		      (method (%tokenize-header-keyword exact-request start r1))
		      (r2 (%fast-position-if white-space-p exact-request :start (1+ r1) :end end))
		      (url (subseq exact-request (1+ r1) (or r2 end)))
		      (server-version (if r2
					  (%tokenize-header-keyword exact-request (the fixnum (1+ r2)) end)
					  :HTTP/0.9)))	; http 0.9 or telnet hackers don't send version
		 (declare (fixnum r1)
			  (type (or null fixnum) r2))
		 (values method url server-version))))
	   (parse-user (line start end)
	     (declare (fixnum start))
	     (if (and (eql #\- (char line start)) (eql #\space (char line (1+ start))))
		 nil
		 (subseq line start (1+ (the fixnum (%fast-position-if-not white-space-p line :start start :end end :from-end t)))))))
    (declare (inline white-space-p parse-exact-request parse-user))
    (let (p1 p2 p3 p4 p5 p6 p7 p8)
      p3
      (cond ((and (setq p1 (%fast-position-if white-space-p line :start 0 :end end))
                  (setq p2 (%fast-position-if white-space-p line :start (1+ (the fixnum p1)) :end end))
                  (setq p3 (%fast-position-if white-space-p line :start (1+ (the fixnum p2)) :end end))
                  (setq p4 (char-position #\[ line (1+ (the fixnum p3)) end))
                  (setq p5 (char-position #\] line (1+ (the fixnum p4)) end))
                  (setq p6 (char-position #\" line (1+ (the fixnum p5)) end))
                  (setq p7 (char-position #\" line (1+ (the fixnum p6)) end))
                  (< (1+ (the fixnum p6)) p7)       ;empty request string is a bad entry  7/20/95 -- JCMa.
                  (setq p8 (%fast-position-if white-space-p line :start (+ 2 (the fixnum p7)) :end end)))
             (locally
               (declare (fixnum p1 p2 p3 p4 p5 p6 p7 p8))
               (multiple-value-bind (method url server-version)
                   (parse-exact-request line (1+ p6) p7)
                 (cond (method
                        (let ((host (subseq line 0 p1))
                              (date (parse-gmt-time line (1+ p4) p5))
                              (status (parse-integer line :start (+ 2 p7) :end p8))
                              (bytes (parse-integer line :start (1+ p8) :end end))
                              (user (parse-user line (1+ p2) p4)))
                          (values url host method date server-version status bytes user)))
                       (t (values nil nil :bad-record))))))
            (t (values nil nil :bad-record))))))

;; Consider tokenizing BROWSER, REFERER and USER.
(defun %parse-extended-common-log-format-log-entry (line &optional (delimiter #\tab) (time-parser #'parse-gmt-time)
                                                         &aux (end (length line)))
  (declare (values url host method date server-version status bytes user-name browser referer)
           (fixnum end)
           (optimize (speed 3)))
  (labels ((field-delimiter-char-p (x)
             (char= x delimiter))
           (white-space-p (x)
             (char= x #\space))
           (parse-exact-request (exact-request start end)
             (declare (fixnum end))
             (unless (eql start end)
               (let* ((r1 (or (%fast-position-if white-space-p exact-request :start start :end end)
                              (return-from parse-exact-request nil)))
                      (method (%tokenize-header-keyword exact-request start r1))
                      (r2 (%fast-position-if white-space-p exact-request :start (1+ r1) :end end))
                      (url (subseq exact-request (1+ r1) (or r2 end)))
                      (server-version (if r2
                                          (%tokenize-header-keyword exact-request (the fixnum (1+ r2)) end)
                                        :HTTP/0.9)))	; http 0.9 or telnet hackers don't send version
                 (declare (fixnum r1)
                          (type (or null fixnum) r2))
                 (values method url server-version))))
           (parse-field (line start end bounding-delimiter)
             (declare (fixnum start))
             (if (eql #\- (char line start))
                 nil
               (let ((s1 (char-position bounding-delimiter line start end))
                     (s2 (char-position bounding-delimiter line start end t)))
                 (when (and s1 s2)
                   (subseq line (1+ (the fixnum s1)) s2))))))
    (declare (inline field-delimiter-char-p parse-exact-request parse-field))
    (let (p1 p2 p3 p4 p5 p6 p7 p8 p9 p10)
      p3
      (cond ((and (setq p1 (%fast-position-if field-delimiter-char-p line :start 0 :end end))
                  (setq p2 (%fast-position-if field-delimiter-char-p line :start (1+ (the fixnum p1)) :end end))
                  (setq p3 (%fast-position-if field-delimiter-char-p line :start (1+ (the fixnum p2)) :end end))
                  (setq p4 (char-position #\[ line (1+ (the fixnum p3)) end))
                  (setq p5 (char-position #\] line (1+ (the fixnum p4)) end))
                  (setq p6 (char-position #\" line (1+ (the fixnum p5)) end))
                  (setq p7 (char-position #\" line (1+ (the fixnum p6)) end))
                  (< (1+ (the fixnum p6)) p7)       ;empty request string is a bad entry  7/20/95 -- JCMa.
                  (setq p8 (%fast-position-if field-delimiter-char-p line :start (+ 2 (the fixnum p7)) :end end))
                  (setq p9 (%fast-position-if field-delimiter-char-p line :start (1+ (the fixnum p8)) :end end))
                  (setq p10 (%fast-position-if field-delimiter-char-p line :start (1+ (the fixnum p9)) :end end)))
             (locally
               (declare (fixnum p1 p2 p3 p4 p5 p6 p7 p8))
               (multiple-value-bind (method url server-version)
                   (parse-exact-request line (1+ p6) p7)
                 (cond (method
                        (let ((host (subseq line 0 p1))
                              (date (funcall time-parser line (1+ p4) p5))
                              (status (parse-integer line :start (+ 2 p7) :end p8))
                              (bytes (parse-integer line :start (1+ p8) :end p9))
                              (user (parse-field line (1+ p2) p4 #\"))
                              (browser (parse-field line p9 p10 #\"))
                              (referer (parse-field line p10 end #\")))
                          (values url host method date server-version status bytes user browser referer)))
                       (t (values nil nil :bad-record (list p1 p2 p3 p4 p5 p6 p7 p8 p9 p10)))))))
            (t (values nil nil :bad-record (list p1 p2 p3 p4 p5 p6 p7 p8 p9 p10)))))))

#|gnoscere.ai.mit.edu  - [1999-07-12 20:37:28] "POST /cl-http/find-documentation.html HTTP/1.1" 200 560 3577 "Mozilla/4.0 (compatible; MSIE 4.5; Mac_PowerPC)" "http://fuji-vlm.ai.mit.edu/cl-http/find-documentation.html" ((:SUBSTRING "flog") (:MODULE "HTTP") (:LISP-TYPE "ALL") (:DOCUMENTATION "YES") (:EXTERNAL "NO") (:SUBMIT "Submit"))|#

(defun %parse-http-post-log-entry (line &optional (start 0) (end (length line)))
  (declare (values url host method date http-version status bytes-received bytes-transmitted user referer
		   form-alist user-agent ua-version ua-comment)
	   (fixnum start end))
  (labels ((delimiter-char-p (x)
	     (char= x #\tab))
	   (white-space-p (x)
	     (char= x #\space))
	   (null-entry-p (string start end)
	     (declare (fixnum start end))
	     (and (= (1+ start) end) (eql #\- (char string start))))
	   (parse-exact-request (exact-request start end)
	     (declare (fixnum start end))
	     (unless (eql start end)
	       (let* ((r1 (%fast-position-if white-space-p exact-request :start start :end end))
		      (r2 (%fast-position-if white-space-p exact-request :start (1+ r1) :end end))
		      (method (%tokenize-header-keyword exact-request start r1))
		      (url (subseq exact-request (1+ r1) (or r2 end)))
		      (server-version (if r2
					  (%tokenize-header-keyword exact-request (the fixnum (1+ r2)) end)
					  :HTTP/0.9)))	; http 0.9 or telnet hackers don't send version
		 (declare #-ecl (fixnum r1)
                          #+ecl (type (or null fixnum) r1)
			  (type (or null fixnum) r2))
		 (values method url server-version))))
	   (parse-user (line start end)
	     (declare (fixnum start end))
	     (subseq line start (1+ (the fixnum (%fast-position-if-not delimiter-char-p line :start start :end end :from-end t))))))
    (declare (inline delimiter-char-p parse-exact-request parse-user))
    (let (p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13)
      p3
      (cond
	((and (setq p1 (%fast-position-if-not delimiter-char-p line :start start :end end))
	      (setq p2 (%fast-position-if delimiter-char-p line :start (1+ (the fixnum p1)) :end end))
	      (setq p3 (%fast-position-if delimiter-char-p line :start (1+ (the fixnum p2)) :end end))
	      (setq p4 (char-position #\[ line (1+ (the fixnum p3)) end))
	      (setq p5 (char-position #\] line (1+ (the fixnum p4)) end))
	      (setq p6 (char-position #\" line (1+ (the fixnum p5)) end))
	      (setq p7 (char-position #\" line (1+ (the fixnum p6)) end))
	      (< (1+ (the fixnum p6)) p7)	;empty request string is a bad entry  7/20/95 -- JCMa.
	      (setq p8 (%fast-position-if delimiter-char-p line :start (1+ (the fixnum p7)) :end end))
	      (setq p9 (%fast-position-if delimiter-char-p line :start (1+ (the fixnum p8)) :end end))
	      (setq p10 (%fast-position-if delimiter-char-p line :start (1+ (the fixnum p9)) :end end))
	      (setq p11 (%fast-position-if delimiter-char-p line :start (1+ (the fixnum p10)) :end end))
	      (setq p12 (%fast-position-if delimiter-char-p line :start (1+ (the fixnum p11)) :end end))
	      (setq p13 (%fast-position-if delimiter-char-p line :start (1+ (the fixnum p12)) :end end)))
	 (locally
	   (declare (fixnum p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13))
	   (multiple-value-bind (method url http-version)
	       (parse-exact-request line (1+ p6) p7)
	     method
	     (let ((host (subseq line p1 p2))
		   (user (unless (null-entry-p line (1+ p2) p3)
			   (parse-user line (1+ p2) p3)))
		   (date (parse-gmt-time line (1+ p4) p5))
		   (status (parse-integer line :start (1+ p8) :end p9))
		   (bytes-received (parse-integer line :start (1+ p9) :end p10))
		   (bytes-transmitted (parse-integer line :start (1+ p10) :end p11))
		   (referer (unless (null-entry-p line (1+ p12) p13)
			      (subseq line (+ 2 p12) (1- p13))))
		   (form-alist (parse-form-alist-log-entry line (1+ p13) end)))
	       (multiple-value-bind (user-agent ua-version ua-comment)
		   (unless (null-entry-p line (1+ p11) p12)
		     (parse-user-agent line (+ 2 p11) (1- p12)))
		 (values url host method date http-version status bytes-received bytes-transmitted user referer
			 form-alist user-agent ua-version ua-comment))))))
	(t (values nil nil :bad-record))))))

(define-parameter *log-line-parser-alist* '((:ncsa-1-2 . %parse-ncsa-http-1-2-log-entry)
                                            (:common-log-format . %parse-common-log-format-log-entry)
                                            (:extended-common-log-format . %parse-extended-common-log-format-log-entry)
                                            (:post-log-format . %parse-http-post-log-entry)))

(define-parameter *default-log-line-parser* :common-log-format
                  "The default log format for parsing log files.")

(defun log-line-parser (log-type)
  (or (cdr (assoc log-type *log-line-parser-alist*))
      (error "Unknown log type, ~S." log-type)))

(defgeneric parse-log-file (log pathname &key log-format stream)
   (:documentation "Parses the log PATHNAME according to LOG-FORMAT.
LOG-FORMAT defaults to the value of *default-log-line-parser*.
Errors are reported on STREAM."))

(defmethod parse-log-file ((log dynamic-loggin-mixin) pathname &key (log-format *default-log-line-parser*) (stream *standard-output*))
  (with-slots (local-host) log
    (with-open-file (file pathname :direction :input)
      (using-resource (line-buffer line-buffer *line-buffer-size*)
	(loop with parser = (log-line-parser log-format)
	      with context = (concatenate 'string "HTTP://" (host-domain-name local-host))
	      for line = (read-delimited-line file '(#\return #\Linefeed) nil line-buffer)
	      while line
	      do (multiple-value-bind (url host method date server-version)
		     (funcall parser line)
		   (cond
		     ((member method '(:get :head :post))
		      (handler-case
			(let ((url-string (merge-url url context)))
			  (declare (dynamic-extent url-string))
			  (log-transaction log
					   (intern-url url-string :if-does-not-exist :create)
					   (intern-host log :domain-name host :object host)
					   method
					   date
					   server-version))
			(parsing-error () (format stream "~&Bad URL Syntax: ~S" line))))
		     (t (format stream "~&Not Parsed by ~A: ~S" log-format line)))))))))

(defmethod url-transactions ((log dynamic-loggin-mixin) (url url))
  (with-slots (url-table) log
    (values (gethash url url-table) url)))

(defmethod url-transactions ((log dynamic-loggin-mixin) (url-string string))
  (multiple-value-bind (url)
      (url:intern-url url-string :if-does-not-exist :soft)
    (when url
      (url-transactions log url))))

(defmethod client-host-name ((transaction transaction))
  (with-slots (host) transaction
    (host-name host)))

(defgeneric show-url-transactions (log url-string &key stream))

(defmethod show-url-transactions ((log dynamic-loggin-mixin) (url-string string) &key (stream *standard-output*))
  (multiple-value-bind (transactions url)
      (url-transactions log url-string)
    (loop for (method n . trns) in transactions
          do (format stream "~&~D ~A transaction~P on ~A" n method n (name-string url))
             (when (y-or-n-p "Describe them? ")
               (loop for tr in trns ;;(sort trns #'string< :key #'client-host-name)
                     do (describe tr))))))


;;;------------------------------------------------------------------- 
;;;
;;; LOG-ENTRY RESOURCE
;;;

(define-generic initialize-log-entry (resource log-entry class))

(defmethod initialize-log-entry (resource (log-entry log-entry) class)
  (declare (ignorable resource class)) 
  log-entry)

(define-generic deinitialize-log-entry (resource log-entry)
  (:method-combination progn)
  (:documentation "Deinitializes any datastructures and free any garbage quickly for the EGC."))

(defmethod deinitialize-log-entry progn (resource (log-entry log-entry))
  (declare (ignorable resource))
  (setf (log-entry-owner log-entry) nil))

(defmethod deinitialize-log-entry progn (resource (log-entry bytes-received-log-entry-mixin))
  (declare (ignorable resource))
  (setf (log-entry-bytes-received log-entry) nil))

(defmethod deinitialize-log-entry progn (resource (log-entry bytes-transmitted-log-entry-mixin))
  (declare (ignorable resource))
  (setf (log-entry-bytes-transmitted log-entry) nil))

(defmethod deinitialize-log-entry progn (resource (log-entry metering-log-entry-mixin))
  (declare (ignorable resource))
  (setf (log-entry-cpu-time log-entry) nil
	(log-entry-elapsed-time log-entry) nil))

(defmethod deinitialize-log-entry progn (resource (log-entry common-log-entry))
  (declare (ignorable resource))
  (setf (log-entry-host-name log-entry) nil
	(log-entry-request log-entry) nil
	(log-entry-request-time log-entry) nil
	(log-entry-user-name log-entry) nil))

(defmethod deinitialize-log-entry progn (resource (log-entry extended-common-log-entry))
  (declare (ignorable resource))
  (setf (log-entry-user-agent log-entry) nil
	(log-entry-referer log-entry) nil))

(defmethod deinitialize-log-entry progn (resource (log-entry http-post-log-entry))
  (declare (ignorable resource))
  (setf (log-entry-form-alist log-entry) nil))

(defun make-log-entry (resource class)
  (declare (ignore resource))
  (make-instance class))

(defun match-log-entry-p (resource log-entry class)
  (declare (ignore resource))
  (eql class (type-of log-entry)))

(defresource log-entry (class)
  :constructor make-log-entry
  :matcher match-log-entry-p
;;  :initializer initialize-log-entry
  :deinitializer deinitialize-log-entry
  :initial-copies 0)

(define clear-log-entry-resource ()
  "Clears the resource of log-entry objects."
  (clear-resource 'log-entry))

(defgeneric deallocate-log-entry (log-entry)
  (:documentation "Top-level method for deallocating a log-entry."))

(defmethod deallocate-log-entry ((log-entry log-entry))
  (deallocate-resource 'log-entry log-entry))

(eval-when (:execute :compile-toplevel :load-toplevel)
(defun log-entry-slot-accessor (keyword)
  (or (get keyword 'log-entry-slot-accessor)
      (setf (get keyword 'log-entry-slot-accessor) (intern (concatenate 'string "LOG-ENTRY-" (symbol-name keyword)) *http-package*))))

(defmacro allocate-log-entry (class &rest parameters)
  (declare (dynamic-extent parameters))
  (loop for (slot value) on parameters by #'cddr
	for accessor = (log-entry-slot-accessor slot)
	collect `(setf (,accessor log-entry) ,value) into initilizers
	finally (return `(let ((log-entry (allocate-resource 'log-entry ',class)))
			   (declare (type ,class log-entry))
			   ,@initilizers
			   log-entry))))

(defmacro define-log-entry-allocator (name arguments &body body)
  "Defines an inline log-entry allocator."
  `(eval-when (:execute :compile-toplevel :load-toplevel)
     (declaim (inline ,name))
     (defun ,name ,arguments
       #+Genera(declare (sys:function-parent ,name define-log-entry-allocator))
       ,@body)))
)						;close eval-when


;;;------------------------------------------------------------------- 
;;;
;;; LOG-ENTRY ALLOCATION
;;;

(define-log-entry-allocator allocate-common-log-entry (log host-name request request-time method status bytes-received bytes-transmitted user-name)
  (allocate-log-entry common-log-entry
		      :owner log :host-name host-name :request request :request-time request-time :method method :status status
		      :bytes-received bytes-received :bytes-transmitted bytes-transmitted :user-name user-name))

(define-log-entry-allocator allocate-extended-common-log-entry (log host-name request request-time method status bytes-received
								    bytes-transmitted user-name user-agent referer)
  (allocate-log-entry extended-common-log-entry
		      :owner log :host-name host-name :request request :request-time request-time :method method :status status
		      :bytes-received bytes-received :bytes-transmitted bytes-transmitted :user-name user-name
		      :user-agent user-agent :referer referer))

(define-log-entry-allocator allocate-http-post-log-entry (log host-name request request-time status bytes-transmitted user-name
					 user-agent referer bytes-received form-alist)
  (allocate-log-entry http-post-log-entry
		 :owner log :host-name host-name :request request :request-time request-time :status status
		 :bytes-transmitted bytes-transmitted :user-name user-name :user-agent user-agent :referer referer :bytes-received bytes-received
		 :form-alist form-alist))

(define-log-entry-allocator allocate-notification-log-entry (log host-name port request request-time method status bytes-received bytes-transmitted
								 user-name user-agent referer requests-completed cpu-time elapsed-time proxy-p)
  (allocate-log-entry notification-log-entry
		      :owner log :host-name host-name :port port :request request :request-time request-time :method method :status status
		      :bytes-received bytes-received :bytes-transmitted bytes-transmitted :user-name user-name :user-agent user-agent :referer referer
		      :requests-completed requests-completed :cpu-time cpu-time :elapsed-time elapsed-time :proxy-p proxy-p))

(define-log-entry-allocator allocate-console-notification-log-entry (log host-name port request request-time method status bytes-received bytes-transmitted
									 user-name user-agent referer requests-completed cpu-time elapsed-time proxy-p)
  (allocate-log-entry console-notification-log-entry
		      :owner log :host-name host-name :port port :request request :request-time request-time :method method :status status
		      :bytes-received bytes-received :bytes-transmitted bytes-transmitted :user-name user-name :user-agent user-agent :referer referer
		      :requests-completed requests-completed :cpu-time cpu-time :elapsed-time elapsed-time :proxy-p proxy-p))

(define-log-entry-allocator allocate-counters-log-entry (log method status bytes-received bytes-transmitted requests-completed proxy-p cpu-time elapsed-time)
  (allocate-log-entry counters-log-entry
		      :owner log :method method :status status :bytes-received bytes-received :bytes-transmitted bytes-transmitted
		      :requests-completed requests-completed :proxy-p proxy-p :cpu-time cpu-time :elapsed-time elapsed-time))


;;;------------------------------------------------------------------- 
;;;
;;; WRITE-LOG-ENTRY
;;;

(defmacro %common-log-bytes (method status bytes-transmitted bytes-received)
  "Determines the correct bytes to log according to METHOD."
  `(if (and (member ,method *server-methods-receiving-data*)	;log the bytes received for POST and PUT methods
	    (< 199 ,status 300))		;only when input accepted successfully
       ,bytes-received
       ,bytes-transmitted))

(define-generic write-log-entry (log-entry stream)
  (:documentation "Write LOG-ENTRY on STREAM.
Specialize this method for different log entry classes.
See snapshot-log-entry for capturing corresponding data."))

(defmethod write-log-entry ((entry-data common-log-entry) stream)
  (with-slots (owner host-name request request-time method status bytes-received bytes-transmitted user-name) entry-data
    (%write-common-logfile-entry host-name request request-time status (%common-log-bytes method status bytes-transmitted bytes-received)
				 user-name (log-times-in-gmt-p owner) stream #\space)
    ;; Trailing CR makes this consistently parsable.
    (terpri stream)))

(defmethod write-log-entry ((entry-data extended-common-log-entry) stream)
  (with-slots (owner host-name request request-time method status bytes-received bytes-transmitted user-name user-agent referer) entry-data
    (%write-extended-common-logfile-entry
      host-name request request-time status (%common-log-bytes method status bytes-transmitted bytes-received)
      user-name user-agent referer (log-times-in-gmt-p owner) stream #\tab)
    ;; Trailing CR makes this consistently parsable.
    (terpri stream)))

(defmethod write-log-entry ((entry-data http-post-log-entry) stream)
  (with-slots (owner host-name request request-time status bytes-transmitted user-name
		     user-agent referer bytes-received form-alist) entry-data
    (%write-http-post-logfile-entry host-name user-name request-time request status bytes-received bytes-transmitted
				    user-agent referer form-alist (log-times-in-gmt-p owner) stream #\tab)
    ;; Trailing CR makes this consistently parsable.
    (terpri stream)))

(defmethod write-log-entry ((entry-data notification-log-entry) stream)
  (with-slots (owner host-name port request request-time method status bytes-received bytes-transmitted user-name
		     user-agent referer requests-completed cpu-time elapsed-time proxy-p) entry-data
    (when (log-notification owner)
      (%write-console-window-notification
	host-name port proxy-p requests-completed request request-time status (%common-log-bytes method status bytes-transmitted bytes-received)
	user-name user-agent referer cpu-time elapsed-time stream))))

(defmethod write-log-entry ((entry-data console-notification-log-entry) stream)
  (declare (ignore stream))
  (with-slots (owner host-name port request method status bytes-received bytes-transmitted user-name
		     user-agent referer requests-completed cpu-time elapsed-time proxy-p) entry-data
    (when (log-notification owner)
      (flet ((notify-the-console (log-stream)
	       (%write-console-window-notification
		 host-name port proxy-p requests-completed request nil status (%common-log-bytes method status bytes-transmitted bytes-received)
		 user-name user-agent referer
		 cpu-time elapsed-time log-stream)))
	(declare (dynamic-extent #'notify-the-console))
	(careful-notify-log-window #'notify-the-console)))))

;;;------------------------------------------------------------------- 
;;;
;;; METHODS ON PROCESS-QUEUED-STREAM-NOTIFICATION-LOG-MIXIN
;;;
;;; A basic building block for STREAM-NOTIFICATION-LOG .

(defmethod add-access-log :before ((log process-queued-stream-notification-log-mixin) port)
  "Ensure that task queue is active"
  (declare (ignore port))
  (setf (log-notification log) t))

(defmethod remove-access-log :before ((log process-queued-stream-notification-log-mixin) port)
  (declare (ignore port))
  (setf (log-notification log) nil))

(defgeneric maybe-remove-idle-stream-notification-log (notification-log)
  (:documentation "Removes NOTIFICATION-LOG from the active logs when it has no active notification streams."))

(defmethod maybe-remove-idle-stream-notification-log ((log process-queued-stream-notification-log-mixin))
  (declare (values stream-notification-log-removed-p))
  (unless (log-streams log)
    (remove-access-log log (log-port log))
    t))

(defgeneric log-write-notification (log server)
  (:documentation "Write the notification to the client connected to the LOG stream according SERVER parameters."))

(defmethod log-write-notification ((log process-queued-stream-notification-log-mixin) (server server-logging-mixin))
  (with-slots (ticks) log
    (tq:push-task-queue log (snapshot-log-entry log server))
    (incf ticks)
    t))

(defgeneric log-live-stream-p (log stream)
  (declare (values stream-live-p))
  (:documentation "Returns non-null when STREAM is considered alive for the purposes of LOG."))

(defmethod log-live-stream-p ((log process-queued-stream-notification-log-mixin) stream)
  (declare (ignorable log stream))
  t)

(defun %log-deinstall-stream (log stream)
  (setf (log-streams log) (delete stream (log-streams log))))

(defmethod tq:task-queue-execute-pending-tasks ((log process-queued-stream-notification-log-mixin))
  (macrolet ((apply-log-streams ((log-streams timeout &key (stream-var 'stream)) &body body)
	       `(loop for ,stream-var in ,log-streams
		      do (handler-case
			   (cond ((log-live-stream-p log ,stream-var)
				  (with-stream-timeout (,stream-var ,timeout :error-p nil)
				    ,@body))
				 (t (%log-deinstall-stream log ,stream-var)))
			   (network-error () (%log-deinstall-stream log ,stream-var))))))
    (labels ((report-logging-error (log error fatal-p)
	       (let ((error-type (type-of error)))
		 (report-bug *bug-http-server* (format nil "HTTP~:[~; Fatal~] Stream Log Notification Error: ~S" fatal-p error-type)
			     "~:[~;~&Stream log notification has been suspended.~]~
                            ~&Log: ~S~&Error: ~S~:[~;~&Error Report: ~:*~A~]~:[~;~&Backtrace: ~:*~A~]"
			     fatal-p log error-type (report-string error)
			     (when *stack-backtraces-in-bug-reports*
			       (stack-backtrace-string error)))))
	     (%handle-logging-error (error)
	       (typecase error
		 ((or network-error file-error)
		  (report-logging-error log error nil)
		  (sleep 5)
		  t)
		 (t (report-logging-error log error t)
		    (stop-log-queue log)
		    t)))
	     (do-log-entry (log log-streams force-output-p write-timeout &aux log-entry)
	       (unwind-protect
		   (when (setq log-entry (tq:pop-task-queue log))
		     (apply-log-streams (log-streams write-timeout)
					(write-log-entry log-entry stream)
					(when force-output-p (force-output stream)))
		     t)
		 (when log-entry
		   (deallocate-log-entry log-entry)))))
      (declare (dynamic-extent #'%handle-logging-error))
      (handler-bind-if (not *debug-server*)
	 ((error #'%handle-logging-error))
	(let ((write-timeout (notification-log-timeout log))
	      (output-interval (1- (notification-log-interval log))))
	  (loop with count = output-interval and force-output-p
		for log-streams = (log-streams log)
		while (and (cond (log-streams
				  (setq force-output-p (zerop (decf count output-interval)))
				  (prog1 (do-log-entry log log-streams force-output-p write-timeout)
					 (when force-output-p (setq count output-interval))))
				 ;; no clients means deactivate log.
				 (t (remove-access-log log (log-port log))
				    (return nil)))
			   (tq:task-queue-run-p log))
		finally (apply-log-streams ((log-streams log) write-timeout)
					   (force-output stream))))))))


;;;------------------------------------------------------------------- 
;;;
;;; METHODS ON ASYNCHRONOUS-STREAM-NOTIFICATION-LOG
;;;

(defmethod log-install-stream ((log asynchronous-stream-notification-log) stream)
  (with-log-write-lock (log)
    (pushnew stream (log-streams log))))

(defmethod log-deinstall-stream ((log asynchronous-stream-notification-log) stream)
  (with-log-write-lock (log)
    (%log-deinstall-stream log stream)))

(defmethod log-entry-p and ((log asynchronous-stream-notification-log) (server server-logging-mixin))
  (declare (ignorable server))
  (log-streams log))

(defun create-asynchronous-stream-notification-log (&optional (port *log-default-notification-port*))
  (create-notification-access-log #'log-entry-p #'log-write-notification
				  :name "Asynchronous Notification Log"
				  :port port
				  :class 'asynchronous-stream-notification-log))

(declaim (inline asynchronous-stream-notification-log-p))

(defun asynchronous-stream-notification-log-p (log)
  (typep log 'asynchronous-stream-notification-log))

(defun get-asynchronous-stream-notification-log (&optional (port *log-default-notification-port*))
  (find-access-log-if #'asynchronous-stream-notification-log-p port))

(defun ensure-asynchronous-stream-notification-log (&optional (port *log-default-notification-port*))
  (or (get-asynchronous-stream-notification-log port)
      (create-asynchronous-stream-notification-log port)))

