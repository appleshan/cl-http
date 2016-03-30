;;;   -*- Mode: LISP; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp; -*-

;;;
;;; (c) Copyright  2000, Rainer Joswig, joswig@corporate-world.lisp.de
;;;     All Rights Reserved.
;;;

;;; This implements a LOG class that is intended to be used
;;;  with window systems to display log entries in windows.
;;; 
;;;  Create a log:
;;;     (ensure-window-notification-log)
;;;  Install the output stream into the log object:
;;;     (log-install-window-stream (get-window-notification-log) the-stream)
;;;  Remove a multiport log:
;;;     (remove-access-log (get-window-notification-log) :multiport)

;;; ================================================================

(in-package "HTTP")

(defclass window-notification-log (notification-log-format-mixin asynchronous-stream-notification-log)
  ((notification-timeout :initform 60 :documentation "Abort writing the log entry after so many seconds."))
  (:documentation "This log class notifies window log streams about HTTP activity.")
  (:default-initargs :notification :clim-window))

(defmethod log-notifications-on ((log window-notification-log) &optional on-p)
  (setf (log-notification log) (if on-p :clim-window nil)))

(defun create-window-notification-log (&optional (port *log-default-notification-port*))
  (create-notification-access-log #'log-entry-p #'log-write-notification
				  :name "CLIM Log Window"
				  :port port
				  :class 'window-notification-log))

(defun window-notification-log-p (log)
  (typep log 'window-notification-log))

(defun get-window-notification-log (&optional (port *log-default-notification-port*))
  (find-access-log-if #'window-notification-log-p port))

(defun ensure-window-notification-log (&optional (port *log-default-notification-port*))
  (or (get-window-notification-log port)
      (create-window-notification-log port)))

; (get-window-notification-log *log-default-notification-port*)

; To be implemented:
(defmethod live-stream-p ((log window-notification-log) stream)
  (declare (ignorable log stream))
  t)

(defmethod write-log-entry :around (entry-data (stream clim:application-pane))
  (declare (ignore entry-data))
  (clim-sys:with-lock-held
   ((http-ui::frame-log-pane-lock (clim:pane-frame stream)) :write)
   (clim:with-text-size (stream :very-small)
     (clim:stream-finish-output stream)
     (clim:stream-fresh-line stream)
     (call-next-method)
     (clim:stream-finish-output stream))))

; implement are more CLIM specific version of this method:
(defmethod write-log-entry ((entry-data notification-log-entry) (stream clim:application-pane))
  (with-slots (owner host-name port request request-time status bytes-transmitted user-name
		     user-agent referer requests-completed cpu-time elapsed-time proxy-p) entry-data
    (when (log-notification owner)
      (%write-console-window-notification
	host-name port proxy-p requests-completed request request-time status bytes-transmitted user-name user-agent referer
	cpu-time elapsed-time stream))))


#|
(defun %write-clim-window-notification (host-name port proxy-p requests-per-connection request request-time
                                        status bytes user-name user-agent referer
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
|#

;;; ================================================================
;;; End of File

