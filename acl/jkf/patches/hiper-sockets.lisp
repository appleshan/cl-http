(in-package :www-utils)

;;; KAP 2003-06-09
;;; use hiper sockets

(defmethod chunk-transfer-encoding-mode ((stream socket::hiper-socket-stream-internet-active) chunk-function)
  (declare (ignore chunk-function))
  nil)

(defmethod note-first-chunk ((stream socket::hiper-socket-stream-internet-active))
  (mp:with-timeout (http1:*persistent-connection-timeout*
                    (error "note-first-chunk timeout on output to the socket"))
    (force-output stream)
    (socket:socket-control stream :output-chunking t)))

(defmethod note-last-chunk ((stream socket::hiper-socket-stream-internet-active) 
                            &optional footers-plist)
  (mp:with-timeout (http1:*persistent-connection-timeout*
                    (error "note-last-chunk timeout on output to the socket"))
    (socket:socket-control stream :output-chunking-eof t)
    (http::write-headers stream footers-plist t)
    (force-output stream)))
  

(defmethod chunk-transfer-decoding-mode ((stream socket::hiper-socket-stream-internet-active))
  (socket:socket-control stream :input-chunking t)
  )

(defmethod end-of-chunk-transfer-decoding ((stream socket::hiper-socket-stream-internet-active))
  ;; this is the same as the next method due to the way the
  ;; with-chunked-transfer-decoding macro is written.
  (socket:socket-control stream :input-chunking nil)
  )


(defmethod chunk-transfer-decoding-mode-end ((stream socket::hiper-socket-stream-internet-active))
  ;; shut down input chunk decoding
  (socket:socket-control stream :input-chunking nil))

(define ip-address-for-parsed-ip-address (ip-number)
  "Returns an IP address as a string from, IP-NUMBER, the parsed address."
  (if (stringp  ip-number)
      ip-number
    (socket:ipaddr-to-dotted ip-number)))

(defmethod www-utils:foreign-host ((http-stream socket::hiper-socket-stream-internet-active))
   (socket:remote-host http-stream))

(defmethod www-utils:foreign-port ((http-stream socket::hiper-socket-stream-internet-active))
  (socket:remote-port http-stream))

(defmethod www-utils:local-port ((http-stream socket::hiper-socket-stream-internet-active))
  (socket:local-port http-stream))

(defmethod http::http-input-data-available-p ((stream socket::hiper-socket-stream-internet-active) 
				      &optional timeout-seconds)
  ;; return true if there is non-whitespace data available
  ;; If timeout is given then don't wait any longer than that
  ;; for the answer

  (flet ((data-available-p (stream)
	   ;; see if there is non-whitespace data readable.
	   ;; return t if so.
	   ;; skip all whitespace characters.
	   ;; block on the first read but not subsequent ones
	   (let ((ch (peek-char nil stream nil)))
	     (loop
	       (if* (null ch)
		  then (return nil) ; eof
		elseif (not (member ch
				 '(#\return #\linefeed #\space #\tab)
				 :test #'eq))
		  then (return t) ; have a character
		  else (read-char stream)
		       (if* (not (listen stream))
			  then (return nil) ; no more now available
			  else (setq ch (peek-char nil stream nil))))))))
    
		       
    (if* (not (www-utils:live-connection-p stream))
       then nil  ; dead connection, no data
     elseif timeout-seconds
       then (mp:with-timeout (timeout-seconds 
			      (and (listen stream)
				   (data-available-p stream)))
	      (loop (if* (data-available-p stream)
		       then (return t))))
       else (and (listen stream)
                 (data-available-p stream)))))


(defun http::enable-http-stream-server (port &key log (new-location t))
  (declare (special http::*http-ports*))
  (let (stream (trueport port))
    (www-utils::default-domain-name)
    (unless (setq stream (getf http::*acl-tcp-servers* port))
      (let (standard-port-change 
            ;; bind80 case
            (listen-fd (get-line-argument "listen_fd=" 'fixnum))
            (host (get-line-argument "host=" 'string))
            (newport (get-line-argument "port=" 'fixnum)))
        (if listen-fd
            (unless (eql newport 80)
              (warn "Assuming port argument value port=80")
              (setq newport 80)))
        (if host
            (install-symbolic-host-shadow host :port (or newport port)
                                          :update-context-p nil
                                          :reset-port-p nil))
        ;; Now avoid (open "" :class ...) bug 3/95
        (mp:without-scheduling
          
          (setq stream (change-class
                        (socket:make-socket :connect :passive 
                                            :local-port port
                                            :reuse-address t
                                            :format :bivalent
                                            :type :hiper)
                        'ipc:tcp-server-stream))
          (setq trueport (local-port stream)))
        (unless (eql trueport port)
          (warn "Using substitute port ~a instead of port ~a." trueport port)
          (when (eql port http::*standard-http-port*)
            (setq standard-port-change trueport)))
        (www-utils::reset-http-server-location :standard-port-change standard-port-change
                                               :reset-location new-location)
        (setf (getf http::*acl-tcp-servers* trueport) stream)))
    (http:log-notifications-on (http:current-access-logs) log)
    (if log
        (notify-log-window "CL-HTTP http::enable-http-stream-server serving at ~a - With log.~%" port)
      (notify-log-window "CL-HTTP http::enable-http-stream-server serving at ~a - NO log.~%" port))
    trueport))

(defmethod (setf ipc::tcp-stream-process) (process (stream socket::hiper-socket-stream-internet-active))
  process)