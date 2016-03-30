(in-package :http)

(defun disable-http-service (&key (on-ports :all))
  (unless (eq on-ports :all)
    (warn "Stopping single ports is not implemented ~a~%" on-ports))
  (close-all-logs)
  ;; Plist
  (mapc #'(lambda (x)
	    (if (streamp x)
		(close x :abort nil)))
	*acl-tcp-servers*)
  (mapc #'(lambda (x)
	    (clim-sys:destroy-process x))
	*acl-http-processes*)
  (setq *acl-tcp-servers* nil *acl-http-processes* nil)
  )

(defun enable-http-service (&key (on-ports *http-ports*) log (new-location t) (listeners *number-of-listening-processes*))
  "Top-level method for starting up HTTP servers."
  (unless (listp on-ports)
    (setq on-ports (list on-ports)))
  ;; Shutdown any listeners that may be awake.
  (disable-http-service)
  
  ;; before starting a new batch
  (dolist (port (setq *http-ports* (ensure-list  on-ports)))
    ;; Provide multiple listening streams so that MACTCP is ready to accept the connection.
    ;; Prepare server streams
    (setq port (enable-http-stream-server port :log log :new-location new-location))
    (dotimes (idx (ceiling listeners (length on-ports)))
      (%start-listening-for-connections port))
    ;; Advise the user.
    (expose-log-window)
    (notify-log-window "HTTP service enabled for: http://~A:~D/"
                       (www-utils:local-host-domain-name) port))
  on-ports)

(defun shutdown-http-service ()
  "Forcibly shutdown all HTTP servers."
  (prog1 (disable-http-service :on-ports :all)
         (map-http-resource :server :allocated #'shutdown-server)
    (close-all-logs)))

(define start (&key (log (get-line-argument "log=" 'symbol))
		    (update (get-line-argument "update=" 'symbol))
		    (evaluate (get-line-argument "eval=" 'list))
		    (shadow (get-line-argument "shadow=" 'string))
		    (new-location t)
		    (fast-remap-p (get-line-argument "fastremap=" 'symbol))
		    (listeners *number-of-listening-processes*)
		    ; added for acl
		    ; which port to listen on
		    (port (or (get-line-argument "port=" 'fixnum) 80))
		    (host (get-line-argument "host=" 'string))
		    ; the root directory for cl-http
		    (home (get-line-argument "home=" 'string))
		    
		    proxy ; if true, turn on the proxy service
		    (proxy-port 80) ; port to use for proxy
		    )
  
  
  "Short version of ENABLE-HTTP-SERVICE."
  
  (ipc:compute-host-domain-name (or host http:*http-host-name*))
  
  ;; remember the number of the listeners for a server restart
  (setq *number-of-listening-processes* listeners)
  
  ;; proxy
  (if proxy
      (set-standard-http-proxy-port proxy-port)
    (setq *server-launch-initialization-list*
      (remove "Initialize Proxy" *server-launch-initialization-list* 
	      :test #'(lambda (x y) (string= x (car y))))))
 

  (reset-server-local-host-variables :standard-port port)
  (setq http::*standard-proxy-port* port)
  ;(setq http::*http-ports* (list http:*STANDARD-HTTP-PORT*))
  (set-dispatch-macro-character #\# #\u  #'sharp-sign-u-reader-helper)
  (run-server-initializations t)
  (run-server-launch-initializations t)
  
  ; upon a restart we want to be sure that the toplevel that comes up
  ; doesn't replace the readtable with the #u macro defined with
  ; a fresh readtable.  This will prevent that.
  (setq excl::*cl-default-special-bindings*
    (delete '*readtable* excl::*cl-default-special-bindings* :key #'car))
  
  (setq *fast-remap-p* fast-remap-p)
  (enable-http-service 
   :on-ports (list port)
   :listeners listeners :log log :new-location new-location)
  
  (if shadow
      (install-symbolic-host-shadow shadow))
  (if (consp evaluate)
      (clim-sys:make-process #'(lambda ()
				 (clim-sys:process-sleep 5)
				 (eval evaluate))
			     :name "CL-HTTP Initialization Evaler"))
  (www-utils::periodic-tasks)
  (http::start-connection-scavenger)
  
  ;; persistent connections cause problems when accessing www.cnn.com
  ;; with the proxy server on.
  (setq http::*client-persistent-connections* nil)
  
  
  (terpri))


(defun %provide-service-internal (stream client-domain-name client-address timeout)
  (flet ((log-dropped-connection (server)
                                 (tc "Start of log-dropped-connection in %provide-service")
                                 (www-utils::abort-http-stream stream)
                                 (set-server-status server 408)       ;client timeout status code changed from 504 -- JCMa 5/29/1995.
                                 (log-access server)
                                 (close stream :abort t)           
                                 (tc "end of log-dropped-connection in %provide-service"))
         (handle-unhandled-error (server error)
                                 (handler-case
                                     (progn
                                       (set-server-status server 500)
                                       (log-access server)
                                       (report-status-unhandled-error error stream (server-request server))
                                       (close stream :abort nil))                  ; force output
                                   (network-error (e)
                                                  (tc "network error in %provide-service" e)
                                                  #+FRANZ-INC (close stream :abort t))
                                   (file-error (error)
                                               (tc "file-error in %provide-service" error)
                                               (or (www-utils:report-condition error t))
                                               (close stream :abort t))
                                   ;; Any other error we did not anticipate?
                                   (error (error)
                                     (tc "Other error in %provide-service" error)
                                     (warn "Ignoring the following error.")
                                     (www-utils:report-condition error t)
                                     (close stream :abort t)))
                                 ;; log the access after the output has been forced.
                                 (log-access server)))
    ;; make sure we know our process.
    (setf (ipc::tcp-stream-process stream) (clim-sys:current-process))
    
    (clim-sys:using-resource
     ;; Notice the resource name HTTP-SERVER is NOT quoted
     (server http-server stream client-domain-name client-address)
     (let ((*server* server)) 
       (with-stream-fd-watcher (stream)
         (handler-case-if
          (not *debug-server*)
          (mp:with-timeout (timeout
                            (error "provide-service timeout on read/write to the socket"))
            (progn 
              (www-utils:with-optimal-stream-buffer ()
                (provide-service server))
              (close stream :abort nil)))
          ;; catch aborts anywhere within server.-- JCMa 12/30/1994.
          (http-abort (e) (tc "abort while calling provide-service" e)
                      (www-utils::abort-http-stream stream))
          
          (excl:socket-error (error)
                             (tc "socket-error in %provide-service" error)
                             (log-dropped-connection server)
                             )
          (file-error (error)
                      ;; most likely connection reset by peer on read
                      (tc "file-error in %provide-service" error)
                      (log-dropped-connection server))
          (error (error) (tc "unhandled error while calling provide-service" error)
            (handle-unhandled-error server error))))))))