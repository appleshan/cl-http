(in-package :http)

;;; single thread simple server for testing



#|
(unwind-protect
	(loop
	  (restart-case
	      (let ((sock (socket:accept-connection main-socket))
		    )
		
		
		
		; disable the nagle alorithm
		(socket:set-socket-options sock :nodelay t)
		
		#+?
		(socket:socket-control 
		 sock 
		 :read-timeout 60
		 :write-timeout 60)
		       
		(process-connection sock))
	    
	    (:loop ()  ; abort out of error without closing socket
	      nil)))
  (close main-socket))

#+no
(unwind-protect
      (let ((req))
	;; get first command
	(loop
	  (with-timeout-local (*read-request-timeout* 
			       (debug-format :info "request timed out on read~%")
			       ; this is too common to log, it happens with
			       ; every keep alive socket when the user stops
			       ; clicking
			       ;;(log-timed-out-request-read sock)
			       (return-from process-connection nil))
	    (setq req (read-http-request sock)))
	  (if* (null req)
	     then ; end of file, means do nothing
		  ; (logmess "eof when reading request")
		  ; end this connection by closing socket
		  (return-from process-connection nil)
	     else ;; got a request
		  (setq *worker-request* req) 
		  
		  (handle-request req)
		  (force-output-noblock (request-socket req))
		  
		  (log-request req)
		  
		  (setq *worker-request* nil)
		  (free-req-header-block req)
		  
		  (let ((sock (request-socket req)))
		    (if* (member :keep-alive
				 (request-reply-strategy req)
				 :test #'eq)
		       then ; continue to use it
			    (debug-format :info "request over, keep socket alive~%")
			    (force-output-noblock sock)
		       else (return))))))
    ;; do it in two stages since each one could error and both have
    ;; to be attempted
    (ignore-errors (force-output-noblock sock))
  (ignore-errors (close sock :abort t)))

(defmacro with-timeout-local ((time &rest actions) &rest body)
  (declare (ignore time))
  (let ((g-blocktag (gensym)))
    `(block ,g-blocktag
       (handler-bind ((socket-error 
		       #'(lambda (c)
			   (if* (member (stream-error-identifier c) 
					'(:read-timeout :write-timeout)
					:test #'eq)
			      then ; must handle this
				   (return-from ,g-blocktag
				     (progn ,@actions))))))
         ,@body))))

(defun force-output-noblock (stream)
  ;; do a force-output but don't get hung up if we get blocked on output
  ;; this happens enough with sockets that it's a real concern
  ; 30 seconds is enough time to wait
  (with-timeout-local (30) 
    (force-output stream)))


|#


(defvar *main-socket*)
(defvar *main-stream*)

#|
(close *main-stream*)
(class-of *main-stream*)

|#


(defun start-single-thread-server (port host)
  (setq *main-socket*
        (socket:make-socket :connect :passive
                            :local-port port
                            :local-host host
                            :reuse-address t
                            :format :bivalent                         
                            :type :hiper))
  (let ((http:*log-access-log-class* 'http:console-log))
    (http:ensure-current-log port)
    (%single-listen-for-connections *main-socket*))
  )

(defun %single-listen-for-connections (server-socket)
  (let (stream)
    (unwind-protect
        (loop
          ;; if ipc::stream-read produces an error, then retry.
          (loop
            (handler-case
                (progn
                  (setq stream (socket:accept-connection server-socket))	; disable the nagle alorithm
                  (socket:set-socket-options stream :nodelay t)
                  (socket:socket-control 
                   stream 
                   :read-timeout 60
                   :write-timeout 60)
                  (setq *main-stream* stream)
                  (return))
              (error (cond) (declare (ignore cond))
                ;;(warn "ipc::stream-read error ~a" cond)
                ;;(sleep 1)
                )))
          (case (tcp-connection-state stream)
            (8 ;; :established
             (let* ((client-address (www-utils::foreign-host stream))
                    (client-domain-name (www-utils:ip-address-for-parsed-ip-address client-address)))
               ;; set the timeout to the connected value
               (format *trace-output* "~&About to launch new HTTP request...~%")
               (%provide-single-service stream client-domain-name client-address)
               ))
            ((if stream (close stream :abort t))
             )))
      (close server-socket))
    )
  )

(defun %provide-single-service (stream client-address client-host)
  (flet ((log-dropped-connection (server)
                                 (www-utils::abort-http-stream stream)
                                 (set-server-status server 408)       ;client timeout status code changed from 504 -- JCMa 5/29/1995.
                                 (log-access server))
         (handle-unhandled-error (server error)
                                 (handler-case
                                     (progn
                                       (set-server-status server 500)
                                       (log-access server)
                                       (report-status-unhandled-error error stream (server-request server))
                                       (close stream))                  ; force output
                                   (excl:socket-error ()))
                                 ;; log the access after the output has been forced.
                                 (log-access server)))
    ;; make sure we know our process. 
    ;; client-host is NIL because we only parse it on demand.
    (let ((server (make-server nil  stream client-host client-address)))
      (initialize-resourced-server nil server stream client-host client-address)
      
      ;; keep other processes from tampering with our TCP state.
      (let ((*server* server))
        (handler-case-if nil
                         (provide-service server)
                         ;; catch aborts anywhere within server.-- JCMa 12/30/1994.
                         ;; log aborts as dropped connections by client. -- JCMa 5/24/1999.
                         (http-abort () (log-dropped-connection server))
                         (excl:socket-error () (log-dropped-connection server))
                         (error (error) (handle-unhandled-error server error)))))))
  




#+mcl
(defun %provide-service (stream client-address client-host)
   (flet ((log-dropped-connection (server)
               (www-utils::abort-http-stream stream)
               (set-server-status server 408)       ;client timeout status code changed from 504 -- JCMa 5/29/1995.
               (log-access server))
            (handle-unhandled-error (server error)
               (handler-case
                  (progn
                     (set-server-status server 500)
                     (log-access server)
                     (report-status-unhandled-error error stream (server-request server))
                     (close stream))                  ; force output
                  (ccl:network-error ()))
               ;; log the access after the output has been forced.
               (log-access server)))
      ;; make sure we know our process.
      (setf (ccl:tcp-stream-process stream) ccl:*current-process*) 
      ;; client-host is NIL because we only parse it on demand.
      (resources:using-resource (server http-server stream client-host client-address)
         ;; keep other processes from tampering with our TCP state.
         (let ((*server* server))
            (handler-case-if (not *debug-server*)
                                      (provide-service server)
               ;; catch aborts anywhere within server.-- JCMa 12/30/1994.
               ;; log aborts as dropped connections by client. -- JCMa 5/24/1999.
               (http-abort () (log-dropped-connection server))
               (ccl:protocol-timeout () (log-dropped-connection server))
               (ccl:connection-lost () (log-dropped-connection server))
               (ccl:host-stopped-responding () (log-dropped-connection server))
               (ccl:bad-connection-state () (log-dropped-connection server))
                             (error (error) (handle-unhandled-error server error)))))))

#+mcl
(define listen-for-connection (stream port)
   (declare (values control-keyword))
   (flet ((accept-connection-p ()
	       (< *number-of-connections* *reject-connection-threshold*))
	    (reject-connection (stream string)	;counts as a connection because we have wait for close to timeout.
	       (atomic-incf (the fixnum *number-of-connections*))
	       (unwind-protect
	          (progn (when string
                                 (write-string string stream)
			         (force-output stream))
                             (close stream))
	          (atomic-decf (the fixnum *number-of-connections*))))
	    (incoming-connection-p (stream)
               (not (eq :listen (http-stream-connection-state stream)))))
      (declare (inline accept-connection-p reject-connection process-namestring))
      ;;  Listen for connections
      ;;(process-wait "Listen" #'incoming-connection-p stream)
      (ccl::process-poll "Listen" #'incoming-connection-p stream)       ; reduce response latency -- JCMa 5/15/1999.
      (let ((state (http-stream-connection-state stream)))
         (case state
	    (:listen (if (%listening-on-port-p port)
                           :continue			;Keep up the good work
		           :quit))			;Quit listening in response to command
	    ((:established :syn-received :syn-sent)
              (cond ((null (ccl::stream-listen stream))	;Data available?
                         (if (%listening-on-port-p port)
                            :block			;Wait for data
		            :quit))			;Quit listening in response to command
	               ((accept-connection-p)
                         ;; don't resolve domain names because you'll crawl at the rate of DNS   3/8/97 -- JCMa.
                         (let* ((client-address (www-utils::foreign-host stream))
                                   (client-host (www-utils:ip-address-for-parsed-ip-address client-address))
                                   (connection-spec `(:server ,client-address ,port)))
                            (declare (dynamic-extent connection-spec))
		            ;;Set the timeout to the connected value
		            (setf (ccl::stream-read-timeout stream) (floor (the fixnum *server-timeout*) 60)	;convert to seconds
                                     (ccl:tcp-stream-process stream) nil)	;set to NIL pending launch
		            ;;Start a thread to provide http service
                            (launch-process connection-spec '(:priority 0) #'%provide-service stream client-address client-host)
		            :new-stream))			;request new listening stream
	               (t (reject-connection stream *reject-connection-message*)
                           :continue)))			;Continue running 
	    ;;Handle random states.
	    (t (notify-log-window "Shutting down HTTP listener ~S because it went into the state ~S." stream state)
        :replace-stream)))))

#+mcl
(defun %listen-for-connections (port timeout &aux (restarts 0))  ; timeout is 0 in practice
   (flet ((allocate-stream (port &optional old-stream)    ;;allocate an http stream
	       (let ((new-stream (resources:allocate-resource 'http-stream nil port *command-timeout-for-listeners* ccl::*current-process*)))
	          ;; set the control stream so we know that we are listening for new connections.
	          (if old-stream
		     (%swap-listening-stream new-stream old-stream port)
		     (%register-listening-stream new-stream port))
	          new-stream))
	    (report-release (err stream restarts abort-p)
	       (report-bug *bug-http-server* (format nil "HTTP Listener Error: ~S" (type-of err))
		                  "~&Error in the HTTP listening stream ~S.~
                                    ~&Error Type: ~S~
                                    ~&Releases ~D (~D)~
                                    ~&~:[Restarting~;Aborting~]"
		                  stream (type-of err) restarts *connection-releases* abort-p))
	    (start-up-timeout-p (start-tick start-timeout)
	       (> (- (fis_tickcount) start-tick) start-timeout)))
      (declare (inline allocate-stream ))
      (macrolet ((release-connection (stream &optional err)
		          `(let ((restart-p (or (< *maximum-restarts-per-listener* 0)
				                         (< restarts *maximum-restarts-per-listener*))))
		              (atomic-incf *connection-releases*)
		              (incf restarts)
		              (when ,err (report-release ,err ,stream restarts restart-p))
		              ;; Trap when limits exceeded.
		              (unwind-protect
			         (unless restart-p
			            (cerror "Reset counter and restart connection"
				                 "~D connection restarts per listener exceeds the limit, ~D."
				                 restarts *maximum-restarts-per-listener*)
			            (setf restarts 1))
		                 ;; always release bad connections
		                 (ccl::stream-release-connection ,stream))
		              ;; restart
		              (setf (ccl:tcp-stream-process ,stream) nil)	; don't free this process
		              (go start))))
          (tagbody
	     start 
	     (let ((stream (allocate-stream port))
		     (*server-timeout* timeout)	; one needs to stop the listening streams to change the timeout
		     (start-timeout (* 60 (the fixnum ccl::*tcp-read-timeout*))))
	        (unwind-protect
		   (handler-case-if 
		      (not *debug-server*) 
		      (loop with start-tick 
			       doing (ecase (listen-for-connection stream port)
				           (:continue (setq start-tick nil))
				           (:block ;; blocking occurs when the stream is neither in :LISTEN or :ESTABLISHED
				              (unless start-tick (setq start-tick (fis-tickcount)))	; set timeout out first time around
				              (cond
				                ((start-up-timeout-p start-tick start-timeout)	; assume stream wedged once timeout exceeded.
				                  (notify-log-window "Shutting down HTTP listener ~S because it failed to respond within ~D seconds." 
							                         stream (float (/ start-timeout 60. )))
				                  (release-connection stream))
				                (t (ccl::suspend-current-process "Listen Wait"))))
				           (:new-stream	;Standard hand off of listening stream to service connection
				             (setq start-tick nil
					              stream (allocate-stream port stream)))
				           (:replace-stream	;Non-standard replacement of stream shutdown by error.
				             (setf (ccl:tcp-stream-process stream) nil)	; don't free this process
				             (go start))
				           (:quit
				             (setq stream nil)	;%shutdown-idle-listening-stream already does the forceable shutdown
				             (return-from %listen-for-connections))))
		      ;; This error occurs because we can't get the connection into a listening state.
		      ;; Try releasing the connection and firing up a new one.
		      (ccl::tcp-invalid-data-structure (err) (release-connection stream err))
		      (ccl::tcp-connection-state-timeout (err) (release-connection stream err)))
	           (when stream
		       (%forcibly-shutdown-listening-stream stream))))))))