(in-package :net.aserve)

Analysis of aserve 

Start
start

Single-threading with
start-simple-server

Multi-threading with
start-lisp-thread-server
 
 
 
Socket creation with
(socket:make-socket :connect :passive
                    :local-port port
                    :local-host host
                    :reuse-address t
                    :format :bivalent                    
                    :type 
                    :hiper ;;since acl6, else :stream
                    )

has to be closed

Single thread simply with
(progn
  (loop
    (let ((socket (socket:accept-connection main-socket)))
      (socket:set-socket-options socket :nodelay t)
      (socket:socket-control 
       socket 
       :read-timeout 120 ;seconds
       :write-timeout 120 ;seconds)
      (process-connection sock))))
      (close main-socket))

process-connection simply
(progn
  (loop
    (read-request&serve)
    (if (not (keep-alive))
        (return)))
  (ignore-errors (force-output-noblock sock))
  (ignore-errors (close sock :abort t)))


Multithreading:

Create 1 Thread for the accept-connect in
http-accept-thread

Create N Threads for the work

(unwind-protect
    (loop
      (handler-case
          (let ((socket (socket:accept-connection main-socket)))
            (socket:socket-control 
             socket 
             :read-timeout (wserver-io-timeout *wserver*)
             :write-timeout (wserver-io-timeout *wserver*))
            (find-a-thread-for-the socket))
        (error ())))
  (ignore-errors
   (close main-socket)))

do-work=



do-work

(mp:process-run-function 
     (list :name (format nil "aserve-accept-~d" (incf *thread-index*))
	   :initial-bindings
	   `((*wserver*  . ',*wserver*)
	     #+ignore (*debug-io* . ',(wserver-terminal-io *wserver*))
	     ,@excl:*cl-default-special-bindings*))
 #'http-accept-thread)

;;;
(mp:make-process :name name
				:initial-bindings
				`((*wserver*  . ',*wserver*)
				  #+ignore (*debug-io* . ',(wserver-terminal-io 
						   *wserver*))
				  ,@excl:*cl-default-special-bindings*)
                 )
(mp:process-preset proc #'http-worker-thread)
(mp:process-add-run-reason (car workers) sock) to start

(handler-case (process-connection sock)
                      (error (cond)
                        (unless (connection-reset-error cond)
                          (log-error))))

(defun connection-reset-error (c)
  ;; return true if this is what results from a connection reset
  ;; by peer 
  (if* (typep c 'stream-error)
     then (or (eq (stream-error-identifier c) :connection-reset)
	      #+unix (eq (stream-error-code c) 32) ; sigpipe
	      #+aix (eq (stream-error-code c) 73) 
	      )))
