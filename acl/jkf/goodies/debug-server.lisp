(in-package :http)

(require :autozoom "sys:src;autozoom.cl")

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
            (cl-user::with-auto-zoom-and-exit 
                ("http:port;last-bug.lisp" :exit nil) 
              (www-utils:with-optimal-stream-buffer ()
                (provide-service server))
              (close stream :abort nil)))
          ;; catch aborts anywhere within server.-- JCMa 12/30/1994.
          (http-abort (e) (tc "abort while calling provide-service" e)
                      (www-utils::abort-http-stream stream))
          
          #+ignore(connection-lost (e) (tc "connection lost while calling provide-service" e)
                                   (log-dropped-connection server))
          #+ignore (host-stopped-responding () (tc "host stopped responding while calling provide-service" e)
                                            (log-dropped-connection server))
          #+ignore (bad-connection-state (e) (tc "bad connection state while calling provide-service" e)
                                         (log-dropped-connection server))
          (excl:socket-error (error)
                             (tc "socket-error in %provide-service" error)
                             (log-dropped-connection server))
          (file-error (error)
                      ;; most likely connection reset by peer on read
                      (tc "file-error in %provide-service" error)
                      (log-dropped-connection server))
          (error (error) (tc "unhandled error while calling provide-service" error)
            (handle-unhandled-error server error))))))))