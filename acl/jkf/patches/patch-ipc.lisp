(in-package :ipc)

;;; don't do the change-class stuff for tcp-client-stream since it fails now
;;; Additionally all the added slots are useles

;;; The only slot to be set seems to be tcp-stream-process
;;; But it is newer read!!!!!

(defmethod (setf tcp-stream-process) (process (stream socket:socket-stream-internet-active))
  process)

;;;; use it
(defmethod stream-read ((stream tcp-server-stream))
  (let ((real-stream (socket:accept-connection stream)))
    real-stream))

(defmethod server-listen ((stream tcp-server-stream))
  (let ((new-connection (socket:accept-connection stream :wait nil)))
    new-connection))


;;; define the missing methods at the superclass SOCKET-STREAM-INTERNET-ACTIVE

(defmethod www-utils:foreign-host ((http-stream socket:socket-stream-internet-active))
   (socket:remote-host http-stream))

(defmethod www-utils:foreign-port ((http-stream socket:socket-stream-internet-active))
  (socket:remote-port http-stream))

(defmethod www-utils:local-port ((http-stream socket:socket-stream-internet-active))
  (socket:local-port http-stream))

(defmethod http::http-input-data-available-p ((stream socket:socket-stream-internet-active) 
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
