;;; -*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-
;;;
;;; (C) Copyright 1997-98, 2000, 2006, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; REMOTE SERVER LOG WINDOW
;;;
;;; This implementation relies on server push, which is available in the following browsers: Netscape, Mozilla, Firefox -- JCMa 4/1/2006
;;;

;;; But, the selection of activities in the form window fails to pop
;;; up a new window, and so redisplays in the form pane for Mozilla, firefox.  -- JCMa 4/1/2006

(in-package :http)

;;;------------------------------------------------------------------- 
;;;
;;; UTILITIES
;;;

(defparameter *log-window-statistics-refresh-rate* 10)

(declaim (integer *log-window-history-size*))

(defparameter *log-window-history-size* 20
  "The number of log entries to display on the remote client before updating the display.")

(defparameter *log-window-display-window* "CL-HTTP-Window"
  "The client window to use for display.")

(defparameter *log-window-connection-timeout* (* 60. 60. 60. 1.)	;timeout after one hour
  "The amount of idle time before closing down server push connections.")

(defparameter *cl-http-icon-small* "/cl-http/icons/power-small.gif")

(defparameter *log-window-style-sheet-url* "/cl-http/maintenance/log-window-style.css")

(defparameter *log-window-logo-url* "/cl-http/maintenance/log-window-logo.html")

(defparameter *log-window-title* "Commmon Lisp Hypermedia Server (CL-HTTP)")

(defparameter *log-window-title-url* "/cl-http/maintenance/log-window-title.html")

(defparameter *log-window-statistics-url* "/cl-http/maintenance/log-window-statistics.html")

(defparameter *log-window-form-url* "/cl-http/maintenance/log-window-form.html")

(defparameter *log-window-notification-url* "/cl-http/maintenance/log-window-notification.html")

(defparameter *log-window-url* "/cl-http/maintenance/log-window.html")

(defparameter *log-window-logs-url* "/cl-http/maintenance/logs/")

(defparameter *log-window-activities* nil
  "A the choices of activities available from the log window.")

(defparameter *log-window-activity-presentation* nil
  "A presentation that returns the URL to display or NIL.")

(defparameter *log-window-standard-http-port* :multiport
  "The standard port for which the low window displays information.")

(defun log-window-activity-presentation (&optional recache-p)
  (cond ((or recache-p (null *log-window-activity-presentation*))
	 (setq *log-window-activities* (stable-sort *log-window-activities* #'string< :key #'car))
	 (setq *log-window-activity-presentation* `((w3p:member-alist (("None" . nil) . ,*log-window-activities*)
									   :test equalp :value-key cdr)
							 :name-key first)))
	(t *log-window-activity-presentation*)))

(defun %define-log-window-activity (menu-string url)
  (let ((entry (assoc menu-string *log-window-activities* :test #'string-equal)))
    (cond (entry (setf (cdr entry) url))
	  (t (push `(,menu-string . ,url) *log-window-activities*)))
    (setq *log-window-activity-presentation* nil)
    *log-window-activities*))

(defmacro define-log-window-activities (&body activity-specs)
  `(progn
     (setq *log-window-activities* nil)
     . ,(loop for (menu-string url) in activity-specs
	      collect `(%define-log-window-activity ,menu-string ,url))))

(define-log-window-activities
  ("Configure Server" "/cl-http/maintenance/configure-server.html")
  ("Documentation" "/cl-http/docs.html")
  ("Edit User ACL" "/cl-http/edit-user.html")
  ("View Server Logs" "/cl-http/maintenance/logs/"))


;;;------------------------------------------------------------------- 
;;;
;;; LOG STATISTICS
;;;

(defun get-statistics-log (&optional (port *log-window-standard-http-port*))
  (find-if #'(lambda (l) (typep l 'log-counters-mixin)) (standard-access-logs port)))

(define-generic log-statistics-snapshot (log)
  (declare (values n-access-denials n-client-errors n-insufficient-resource-denials n-gateway-errors
		   n-redirects n-requests n-requests-served n-proxy-requests n-proxy-cache-hits n-server-errors
		   n-deletes n-gets n-heads n-options n-posts n-puts n-traces n-extension-methods
		   bytes-transmitted bytes-received elapsed-seconds cpu-time connections 
		   uptime launch-time current-time)))

(defmethod log-statistics-snapshot ((log log-counters-mixin))
  (let ((launch-time (log-creation-time log))
	current-time uptime)
    (with-lock-held ((log-lock log) :write "Log Snapshot")
      (setq current-time (get-universal-time)
	    uptime (- current-time launch-time))
      (values (log-number-of-access-denials log)
	      (log-number-of-client-errors log)
	      (log-number-of-insufficient-resource-denials log)
	      (log-number-of-gateway-errors log)
	      (log-number-of-redirects log)
	      (log-total-number-of-requests log)
	      (log-number-of-requests-served log)
	      (log-number-of-proxy-requests log)
	      (log-number-of-proxy-cache-hits log)
	      (log-number-of-server-errors log)
	      (log-number-of-deletes log)
	      (log-number-of-gets log)
	      (log-number-of-heads log)
	      (log-number-of-options log)
	      (log-number-of-posts log)
	      (log-number-of-puts log)
	      (log-number-of-traces log)
	      (log-number-of-extension-methods log)
	      (log-bytes-transmitted log)
	      (log-bytes-received log)
	      (elapsed-seconds log)
	      (cpu-time log)
	      (log-http-connections log)
	      uptime launch-time current-time))))

(defun useful-interval-ratio  (n seconds)
  (declare (values rate units ))
  (flet ((rate (numerator denominator class)
	   (cond ((zerop denominator)
		  (return-from useful-interval-ratio (values 0 class)))
		 (t (let ((rate (round numerator denominator)))
		      (unless (zerop rate)
			(return-from useful-interval-ratio (values rate class))))))))
    (cond ((zerop n)
	   (values 0 "(na)"))
	  (t (rate n seconds "sec")
	     (rate n (floor seconds 60) "min")
	     (rate n (floor seconds #.(* 60 60)) "hr")
	     (rate n (floor seconds #.(* 60 60 24)) "day")
	     (rate n (floor seconds #.(* 60 60 24 7)) "wk")))))

(defmethod log-display-statistics-as-html-table ((log log-counters-mixin) stream)
  (macrolet ((with-entry ((label stream &optional col-span) &body value-writer)
	       (let ((form (etypecase label
			     ((or string symbol) `(write-string ,label ,stream))
			     (cons label))))
		 `(with-table-row (:stream ,stream)
		    (with-table-cell (:stream ,stream)
		      (with-rendition (:italic :stream ,stream)
			,form))
		    (with-table-cell (:horizontal-alignment :right :stream ,stream
							    ,.(when col-span `(:column-span ,col-span)))
		      ,@value-writer))))
	     (write-rate-entry (label value uptime stream)
	       `(multiple-value-bind (rate unit-string)
		    (useful-interval-ratio ,value ,uptime)
		  (when rate
		    (with-entry ((fast-format ,stream "~A / ~A" ,label unit-string) ,stream)
				(fast-format ,stream "~D" rate))))))
    (flet ((write-entry (label value stream)
	     (with-entry (label stream)
			 (fast-format stream "~D" value)))
	   (write-heading (label stream)
	     (with-table-cell (:header-p t :class "statistics-header" :column-span 2 :stream stream)
	       (write-string label stream))))
      (multiple-value-bind (n-access-denials n-client-errors n-insufficient-resource-denials n-gateway-errors
			    n-redirects n-requests n-requests-served n-proxy-requests n-proxy-cache-hits n-server-errors
			    n-deletes n-gets n-heads n-options n-posts n-puts n-traces n-extension-methods
			    bytes-transmitted bytes-received elapsed-seconds cpu-time connections uptime launch-time current-time)
	  (log-statistics-snapshot log)
	(let ((total-cpu-time (truncate cpu-time 1000000.)))
	  (with-table (:class "statistics" :stream stream)
	    (with-table-row (:stream stream)
	      (with-table-cell (:vertical-alignment :top :horizontal-alignment :center :column-span 3 :stream stream)
		(with-table (:class "time" :stream stream)
		  (with-entry ("Uptime Time"  stream)
			      (write-interval uptime stream))
		  (with-entry ("Launch Time" stream)
			      (write-standard-time launch-time stream t))
		  (with-entry ("Current Time" stream)
			      (write-standard-time current-time stream t))
		  (with-entry ("Elapsed Time" stream)
			      (write-interval elapsed-seconds stream))
		  (with-entry ("CPU Time" stream)
			      (write-interval total-cpu-time stream)))))
	    (with-table-row (:stream stream)
	      (with-table-cell (:vertical-alignment :top :stream stream)
		(with-table (:class "requests" :stream stream)
		  (write-heading "Request Status" stream)
		  (unless-every
		    ((zerop n-requests-served) (write-entry "Requests Served" n-requests-served stream))
		    ((zerop n-redirects) (write-entry "Redirects" n-redirects stream))
		    (nil (write-entry "Total Delivered" (+ n-requests-served n-redirects) stream))
		    ((zerop n-access-denials) (write-entry "Access Denials" n-access-denials stream))
		    ((zerop n-insufficient-resource-denials) (write-entry "Overload Denials" n-insufficient-resource-denials stream))
		    ((zerop n-server-errors) (write-entry "Server Errors" n-server-errors stream))
		    ((zerop n-gateway-errors) (write-entry "Gateway Errors" n-gateway-errors stream))
		    ((zerop n-client-errors) (write-entry "Client Errors" n-client-errors stream))
		    (nil (write-entry "Total Requests" n-requests stream))
		    ((zerop n-proxy-requests) (write-entry "Proxy Requests" n-proxy-requests stream)))))
	      (with-table-cell (:vertical-alignment :top :stream stream)
		(with-table (:class "methods" :stream stream)
		  (write-heading "HTTP Methods" stream)
		  (unless-every
		    ((zerop n-deletes) (write-entry "Delete" n-deletes stream))
		    ((zerop n-gets) (write-entry "Get" n-gets stream))
		    ((zerop n-heads) (write-entry "Head" n-heads stream))
		    ((zerop n-options) (write-entry "Options" n-options stream))
		    ((zerop n-posts) (write-entry "Post" n-posts stream))
		    ((zerop n-puts) (write-entry "Put" n-puts stream))
		    ((zerop n-traces) (write-entry "Trace" n-traces stream))
		    ((zerop n-extension-methods) (write-entry "Extension" n-extension-methods stream)))))
	      (with-table-cell (:vertical-alignment :top :stream stream)
		(with-table (:class "performance" :stream stream)
		  (write-heading "Performance" stream)
		  (write-rate-entry "Requests" n-requests uptime stream)
		  (write-entry "Requests / Conn" (round n-requests connections) stream)
		  (write-entry "CPU Msecs / Request" (truncate (round cpu-time n-requests) 1000.) stream)
		  (write-entry "Msecs / Request" (truncate (* elapsed-seconds 1000.)  n-requests) stream)
		  (write-rate-entry "Bytes Sent" bytes-transmitted uptime stream)
		  (write-rate-entry "Bytes Received" bytes-received uptime stream)
		  (unless-every
		    ((zerop n-proxy-cache-hits) (write-entry "Cache Hits (percent)" (round  (* 100 (/ n-proxy-cache-hits n-proxy-requests))) stream))
		    ((zerop n-insufficient-resource-denials) (write-rate-entry "Overloads" n-insufficient-resource-denials uptime stream))
		    ((zerop n-server-errors) (write-rate-entry "Errors" n-server-errors uptime stream)))
		  (write-entry "Bytes Received" bytes-received stream)
		  (write-entry "Bytes Sent" bytes-transmitted stream))))))))))

(defmethod log-display-statistics-as-plain-text ((log log-counters-mixin) stream)
  (macrolet ((with-entry ((label stream &optional col-span) &body value-writer)
	       (let ((form (etypecase label
			     ((or string symbol) `(write-string ,label ,stream))
			     (cons label))))
		 `(fast-format ,stream "~&  ~I: ~I" ,form ,@value-writer ,col-span)))
	     (write-rate-entry (label value uptime stream)
	       `(multiple-value-bind (rate unit-string)
		    (useful-interval-ratio ,value ,uptime)
		  (when rate
		    (with-entry ((fast-format ,stream "~A/~A" ,label unit-string) ,stream)
				(fast-format ,stream "~D" rate))))))
    (flet ((write-entry (label value stream)
	     (with-entry (label stream)
			 (fast-format stream "~D" value)))
	   (write-heading (label stream)
	     (format stream "~&~:@(~A~)" label)))
      (multiple-value-bind (n-access-denials n-client-errors n-insufficient-resource-denials n-gateway-errors
			    n-redirects n-requests n-requests-served n-proxy-requests n-proxy-cache-hits n-server-errors
			    n-deletes n-gets n-heads n-options n-posts n-puts n-traces n-extension-methods
			    bytes-transmitted bytes-received elapsed-seconds cpu-time connections uptime launch-time current-time)
	  (log-statistics-snapshot log)
	(let ((total-cpu-time (truncate cpu-time 1000000.))) 
	  (with-entry ("Uptime Time" stream)
		      (write-interval uptime stream))
	  (with-entry ("Launch Time" stream)
		      (write-standard-time launch-time stream t))
	  (with-entry ("Current Time" stream)
		      (write-standard-time current-time stream t))
	  (with-entry ("Elapsed Time" stream)
		      (write-interval elapsed-seconds stream))
	  (with-entry ("CPU Time" stream)
		      (write-interval total-cpu-time stream)))
	;; Request status
	(write-heading "Request Status" stream)
	(unless-every
	  ((zerop n-requests-served) (write-entry "Requests Served" n-requests-served stream))
	  ((zerop n-redirects) (write-entry "Redirects" n-redirects stream))
	  (nil (write-entry "Total Delivered" (+ n-requests-served n-redirects) stream))
	  ((zerop n-access-denials) (write-entry "Access Denials" n-access-denials stream))
	  ((zerop n-insufficient-resource-denials) (write-entry "Overload Denials" n-insufficient-resource-denials stream))
	  ((zerop n-server-errors) (write-entry "Server Errors" n-server-errors stream))
	  ((zerop n-gateway-errors) (write-entry "Gateway Errors" n-gateway-errors stream))
	  ((zerop n-client-errors) (write-entry "Client Errors" n-client-errors stream))
	  (nil (write-entry "Total Requests" n-requests stream))
	  ((zerop n-proxy-requests) (write-entry "Proxy Requests" n-proxy-requests stream)))
	;; HTTP METHODS
	(write-heading "HTTP Methods" stream)
	(unless-every
	  ((zerop n-deletes) (write-entry "Delete" n-deletes stream))
	  ((zerop n-gets) (write-entry "Get" n-gets stream))
	  ((zerop n-heads) (write-entry "Head" n-heads stream))
	  ((zerop n-options) (write-entry "Options" n-options stream))
	  ((zerop n-posts) (write-entry "Post" n-posts stream))
	  ((zerop n-puts) (write-entry "Put" n-puts stream))
	  ((zerop n-traces) (write-entry "Trace" n-traces stream))
	  ((zerop n-extension-methods) (write-entry "Extension" n-extension-methods stream)))
	;; Performance
	(write-heading "Performance" stream)
	(write-rate-entry "Requests" n-requests uptime stream)
	(write-entry "Requests / Conn" (round n-requests connections) stream)
	(write-entry "CPU Msecs / Request" (truncate (round cpu-time n-requests) 1000.) stream)
	(write-entry "Msecs / Request" (truncate (* elapsed-seconds 1000.)  n-requests) stream)
	(write-rate-entry "Bytes Sent" bytes-transmitted uptime stream)
	(write-rate-entry "Bytes Received" bytes-received uptime stream)
	(unless-every
	  ((zerop n-proxy-cache-hits) (write-entry "Cache Hits (percent)" (round  (* 100 (/ n-proxy-cache-hits n-proxy-requests))) stream))
	  ((zerop n-insufficient-resource-denials) (write-rate-entry "Overloads" n-insufficient-resource-denials uptime stream))
	  ((zerop n-server-errors) (write-rate-entry "Errors" n-server-errors uptime stream)))
	(write-entry "Bytes Received" bytes-received stream)
	(write-entry "Bytes Sent" bytes-transmitted stream)))))

(defmethod write-log-statistics-pane (url stream)
  (flet ((refresh-p (log ticks stream)
	   (handler-case
               (or (>= (log-total-number-of-requests log) ticks)
                   (not (live-connection-p stream)))
	     (error () t))))
    (let ((log (get-statistics-log *log-window-standard-http-port*)))
      (if log
	  (ns4.0:with-server-push-response (stream)
	    (setf (server-timeout *server*) *log-window-connection-timeout*)
	    (loop doing (ns4.0:with-block
                         (stream :force-output t :content-type :html :content-location url)
                         (with-html-document (:stream stream)
                           (with-document-preamble (:stream stream)
                             (declare-link :reference *log-window-style-sheet-url* 
                                           :relation "stylesheet" :media-type "text/css" :stream stream)
                             (declare-base-reference (relative-name-string url) :target "statistics-pane" :stream stream))
                           (with-document-body (:class "statistics-pane" :stream stream)
                             (with-division (:alignment :center :stream stream)
                               (log-display-statistics-as-html-table log stream)))))
                  (process-wait "Log Counter Refresh Wait" #'refresh-p
                                log (+ (log-total-number-of-requests log) *log-window-statistics-refresh-rate*) stream)
                  (unless (live-connection-p stream)
                    (return))))
        (error 'document-not-found :url url
               :format-string "There is no log with counters on the port ~D."
               :format-args (list *log-window-standard-http-port*))))))


;;;------------------------------------------------------------------- 
;;;
;;; NOTIFICATION PANE
;;;

(defclass remote-notification-log
          (notification-log-format-mixin asynchronous-stream-notification-log)
    ((notification :initform :remote-client))
  (:documentation "This log class notifies remote log streams about HTTP activity."))

(defmethod log-times-in-gmt-p ((log remote-notification-log))
  (declare (ignorable log))
  nil)

(defmethod live-stream-p ((log remote-notification-log) stream)
  (declare (ignorable log))
  (live-connection-p stream))

(defmethod log-notifications-on ((log remote-notification-log) &optional on-p)
  (setf (log-notification log) (if on-p :remote-client nil)))

(defun create-remote-notification-log (&optional (port *log-window-standard-http-port*))
  (create-notification-access-log #'log-entry-p #'log-write-notification
				  :name "Remote Log Window"
				  :port port
				  :class 'remote-notification-log))

(defun remote-notification-log-p (log)
  (typep log 'remote-notification-log))

(defun get-remote-notification-log (&optional (port *log-window-standard-http-port*))
  (find-access-log-if #'remote-notification-log-p port))

(defun ensure-remote-notification-log (&optional (port *log-window-standard-http-port*))
  (or (get-remote-notification-log port)
      (create-remote-notification-log port)))

(defmethod write-log-window-notification-pane (url stream)
  (flet ((refresh-p (log ticks stream)
	   (handler-case
               (or (>= (notification-log-ticks log) ticks)
                   (not (live-connection-p stream)))
	     (error () t))))
    (let ((log nil)
	  (port *log-window-standard-http-port* ))
      (unwind-protect
	  (ns4.0:with-server-push-response (stream)
	    (setf (server-timeout *server*) *log-window-connection-timeout*)
	    (loop doing (progn 
                          (ns1.1:with-block
                           (stream :content-type :html :content-location url)
                           (with-html-document (:stream stream)
                             (with-document-preamble (:stream stream)
                               (declare-link :reference *log-window-style-sheet-url* 
                                             :relation "stylesheet" :media-type "text/css" :stream stream)
                               (declare-base-reference (relative-name-string url) :target "notification-pane" :stream stream))
                             (with-document-body (:class "notification-pane" :stream stream)
                               (with-verbatim-text (:fresh-line nil :stream stream)
                                 (unwind-protect
                                     ;; make sure it is not deinstalled by another log window.
                                     (log-install-stream (setq log (ensure-remote-notification-log port)) stream)
                                   (process-wait "Log Window Refresh Wait" #'refresh-p
                                                 log (+ (the integer (notification-log-ticks log)) *log-window-history-size*) stream)
                                   (log-deinstall-stream log stream))))))
                          (unless (live-connection-p stream) (return))
                          (force-output stream))))
	(when log
	  (maybe-remove-idle-stream-notification-log log))))))


;;;------------------------------------------------------------------- 
;;;
;;; CONTROL FORM PANE
;;;

(defgeneric write-log-window-form (url stream))

(defmethod write-log-window-form (url stream &aux (port-count 0))
  (flet ((accept-activities (label query-id stream)
	   (with-table-row (:stream stream)
	     (with-table-cell (:class "form-label" :stream stream)
	       (write-string label stream))
	     (with-table-cell (:horizontal-alignment :left :stream stream)
	       (w3p:accept (log-window-activity-presentation) :stream stream :view w3p:+html-view+ 
			   :present-p t :default nil :prompt nil :prompt-mode :raw
			   :query-identifier (string query-id) :insert-default t :active-p t))))
	 (accept-parameter (label query-id value stream)
	   (with-table-row (:stream stream)
	     (with-table-cell (:class "form-label" :stream stream)
	       (write-string label stream))
	     (with-table-cell (:horizontal-alignment :left :stream stream)
	       (w3p:accept `(w3p:integer 0 999) :stream stream :view w3p:+html-view+ 
			   :present-p t :default value :prompt nil :prompt-mode :raw
			   :query-identifier (string query-id) :insert-default t :active-p t))))
         (write-port-context (port local-context)
           (declare (ignore port)
                    (fixnum port-count))
           (unless (zerop port-count)
             (break-line :stream stream))
           (fast-format stream "~&~A" local-context)
           (incf port-count)))
    (declare (dynamic-extent #'write-port-context))
    (http:with-successful-response (stream :html
					   :expires (url:expiration-universal-time url)
					   :cache-control (url:response-cache-control-directives url)
					   :content-location url
					   :content-language (url:languages url))
      (with-html-document (:stream stream)
        (with-document-preamble (:stream stream)
          (declare-link :reference *log-window-style-sheet-url* 
                        :relation "stylesheet" :media-type "text/css" :stream stream)
          (declare-base-reference (relative-name-string url) :target "form-pane" :stream stream))
	(with-document-body (:class "form-pane" :stream stream)
          (with-division (:class "form-header" :stream stream)
            (map-local-port-contexts #'write-port-context))
          (break-line :stream stream)
          (with-division (:alignment :center :stream stream)
            (with-fillout-form (:post url :stream stream)
              (with-table (:class "form" :stream stream :border nil :cell-padding 2 :cell-spacing 2)
                (accept-parameter "Statistics Refresh Rate" :statistics-refresh-interval *log-window-statistics-refresh-rate* stream)
                (accept-parameter "Log History Size" :history-size *log-window-history-size* stream)
                (accept-activities "Select Activity" :activity stream)
                (with-table-row (:stream stream)
                  (with-table-cell (:horizontal-alignment :center :stream stream)
                    (accept-input 'reset-button "Reset" :stream stream))
                  (with-table-cell (:horizontal-alignment :left :stream stream)
                    (accept-input 'submit-button "Submit" :stream stream))))))
          (break-line :stream stream)
          ;; sign the document
          (with-emphasis (:address :stream stream)
            (note-anchor *server-version* :reference *cl-http-home-page-url-string* :target *log-window-display-window* :stream stream)))))))

(defmethod respond-log-window-form ((url url:http-form) stream query-alist)
  (labels ((jump-to-activity (query-id presentation-type query-alist)
	     (let ((activity (accept-value query-id presentation-type query-alist)))
	       (when activity
		 (redirect-request *server* (merge-url activity) *log-window-display-window*))))
	   (accept-value (query-id presentation-type query-alist)
	     (let ((raw-value (second (assoc query-id query-alist :test #'eq))))
	       (if raw-value
		   (handler-case 
		     (w3p:accept-from-string presentation-type raw-value :view w3p:+html-view+)
		     (w3p:input-not-of-required-type () (values nil nil)))
		   (values nil nil)))))
    (macrolet ((update-value (query-id reference presentation-type)
		 `(multiple-value-bind (new-value p-type)
		      (accept-value ,query-id ,presentation-type query-alist)
		    (when p-type
		      (setf ,reference new-value)))))
      (update-value :statistics-refresh-interval *log-window-statistics-refresh-rate* '(w3p:integer 0 999))
      (update-value :history-size *log-window-history-size* '(w3p:integer 0 999))
      ;; local control exited here when a jump is present
      (jump-to-activity :activity (log-window-activity-presentation) query-alist)))
  ;; no need to update the form pane
  (http:with-successful-response (stream :html :status :no-content
					 :cache-control (url:response-cache-control-directives url)
					 :content-location url))
  ;; Replace the form with any new values
  #+foo(write-log-window-form url stream))


;;;------------------------------------------------------------------- 
;;;
;;; FRAME
;;;

(defmethod write-log-window-style-sheet (url stream)
  (http:with-conditional-get-response (stream :css :expires (url:expiration-universal-time url)
					      :cache-control (url:response-cache-control-directives url)
					      :content-language (url:languages url))
    (fast-format stream "/* Copyright 2006, CL-HTTP Consortium. All rights reserved. */

body {display: block; background-color: white; 
      background-image: none; 
      font-family: helvetica, sans-serif, serif; times;}

form {font-family: helvetica, sans-serif, serif;}

a {font-family: helvetica, sans-serif, times;}
a:link {color: #000080}
a:visited {color:#003399}
a:active {color: yellow}
a:hover {color: orange}

hr {border: 1px solid #000080; background-color: #000080;}

body.title-pane {text-align: center; color: #800000; font-size: xx-large; font-weight: bolder; font-style: italic;}
body.logo-pane {text-align: center; font-size: x-large; font-weight: bolder;}
body.notification-pane {font-size: x-small;}
body.form-pane {color: #000080; font-size: medium;}
body.statistics-pane {color: #000080; font-size: medium;}

table.statistics {color: #000080; padding-top: 10px;}
table.time {border: 1px solid #000080; color: #000080; background-color: #D3D3D3;}
th.statistics-header {color: #800000;}
table.requests {color: #000080;}
table.methods {color: #000080;}
table.performance {color: #000080;}

table.form {color: #000080; border: 1px solid; background-color: #D3D3D3;}
div.form-header {font-size: x-large; font-weight: bold; text-align: left;}
td.form-label {color: #800000; font-size: medium; font-weight: bolder;}")))

(defmethod write-log-window-title-pane (url stream)
  (http:with-conditional-get-response (stream :html :expires (url:expiration-universal-time url)
					      :cache-control (url:response-cache-control-directives url)
					      :content-language (url:languages url))
    (with-html-document (:stream stream)
      (with-document-preamble (:stream stream)
        (declare-link :reference *log-window-style-sheet-url* 
                      :relation "stylesheet" :media-type "text/css" :stream stream)
        (declare-base-reference (relative-name-string url) :target "title-pane" :stream stream))
      (with-document-body (:class "title-pane" :stream stream)
        (fast-format stream "~A" *log-window-title*)))))

(defmethod write-log-window-logo (url stream)
  (http:with-conditional-get-response (stream :html :expires (url:expiration-universal-time url)
					      :cache-control (url:response-cache-control-directives url)
					      :content-language (url:languages url))
    (with-html-document (:stream stream)
      (with-document-preamble (:stream stream)
        (declare-link :reference *log-window-style-sheet-url* 
                      :relation "stylesheet" :media-type "text/css" :stream stream)
        (declare-base-reference (relative-name-string url) :target "logo-pane" :stream stream))
      (with-document-body (:class "logo-pane" :stream stream)
        (with-anchor-noted (:reference *cl-http-home-page-url-string* :target *log-window-display-window* :stream stream)
          (image *cl-http-icon-small* "CL-HTTP" :stream stream :alignment :middle
                 :vertical-space 0 :horizontal-space 0 :width 67 :height 30))))))

(defmethod write-log-frame-set (url stream)
  (http:with-conditional-get-response (stream :html
                                              :expires (url:expiration-universal-time url)
                                              :cache-control (url:response-cache-control-directives url)
                                              :content-location url
                                              :content-language (url:languages url))
    (with-html-document (:declare-dtd-version-p :frameset :stream stream)
      (with-document-preamble (:stream stream)
        (declare-title "CL-HTTP Log Window" :stream stream)
        (declare-link :reference *log-window-style-sheet-url* 
                      :relation "stylesheet" :media-type "text/css" :stream stream))
      (with-document-frameset (:rows '((:pixel 55) :wild) :stream stream)
	(with-document-frameset (:columns '((:pixel 100) :wild) :stream stream)
	  (note-document-frame :name "logo-pane" :reference *log-window-logo-url*
                               :resizable-p t :scrolling nil :frame-border nil :stream stream)
	  (note-document-frame :name "title-pane" :reference *log-window-title-url*
                               :resizable-p t :scrolling nil :frame-border nil :stream stream))
	(with-document-frameset (:rows '((:percentage 50) :wild) :stream stream)
	  (with-document-frameset (:columns '((:percentage 50) :wild) :stream stream)
	    (note-document-frame :name "statistics-pane" :reference *log-window-statistics-url*
                                 :resizable-p t :frame-border t :stream stream)
	    (note-document-frame :name "form-pane" :reference *log-window-form-url* 
                                 :resizable-p t :frame-border t :stream stream))
	  (note-document-frame :name "notification-pane" :reference *log-window-notification-url* 
                               :resizable-p t :frame-border t :stream stream))))))


;;;------------------------------------------------------------------- 
;;;
;;; EXPORTS
;;;

(defun export-log-window (&rest args &key (host (local-host-domain-name)) (port *standard-http-port*) (protocol *standard-protocol*) &allow-other-keys)
  (flet ((intern-log-url (string)
           (let ((args (list string :host host :port port :protocol protocol)))
             (declare (dynamic-extent args))
             (intern-url (merge-url args (local-context)) :if-does-not-exist :create))))
    (declare (dynamic-extent #'intern-log-url))
    (let ((export-args `(:private t
                         :language :en
                         :keywords (:cl-http :maintenance :log-window)
                         ,@args)))
      (declare (dynamic-extent export-args))
      (apply #'export-url (intern-log-url *log-window-style-sheet-url*)
             :computed
             :response-function #'write-log-window-style-sheet
             :expiration `(:interval ,(* 15. 60.))
             export-args)
      (apply #'export-url (intern-log-url *log-window-logo-url*)
             :computed
             :response-function #'write-log-window-logo
             :expiration `(:interval ,(* 15. 60.))
             export-args)
      (apply #'export-url (intern-log-url *log-window-title-url*)
             :computed
             :response-function #'write-log-window-title-pane
             :expiration `(:interval ,(* 15. 60.))
             export-args)
      (apply #'export-url (intern-log-url *log-window-statistics-url*)
             :computed
             :response-function #'write-log-statistics-pane
             :no-cache t
             :documentation "Displays a table of log statistics."
             export-args)
      (apply #'export-url (intern-log-url *log-window-form-url*)
             :computed-form
             :form-function #'write-log-window-form
             :response-function #'respond-log-window-form
             :must-revalidate t
             :documentation "Displays the log window control form."
             export-args)
      (apply #'export-url (intern-log-url *log-window-notification-url*)
             :computed
             :response-function #'write-log-window-notification-pane
             :no-cache t
             :documentation "Displays a the remote log window."
             export-args)
      (apply #'export-url (intern-log-url *log-window-url*)
             :computed
             :response-function #'write-log-frame-set
             :expiration `(:interval ,(* 15. 60.))
             export-args)
      (apply #'export-url (intern-log-url *log-window-logs-url*)
             :directory
             :pathname *standard-log-directory*
             :recursive-p t
             export-args)
      (destructuring-bind (&key (secure-subnets (list (local-host-ip-address)))
                                (authentication-realm :server)
                                (capabilities :webmasters) &allow-other-keys)
          export-args
        (export-web-configuration-interface :secure-subnets secure-subnets
                                            :authentication-realm authentication-realm
                                            :capabilities capabilities)))))

(export '(*cl-http-icon-small*
	  *log-window-color-scheme*
	  *log-window-connection-timeout*
	  *log-window-form-url*
	  *log-window-history-size*
	  *log-window-logo-url*
	  *log-window-logs-url*
	  *log-window-notification-url*
	  *log-window-statistics-refresh-rate*
	  *log-window-statistics-url*
          *log-window-title*
	  *log-window-title-url*
	  *log-window-url*
	  define-log-window-activities
	  export-log-window)
	:http)

;; Use this form to export CL-HTTP Log Window.

#+Ignore
(export-log-window
  ;; Webmaster access only on server
  :authentication-realm :server
  :capabilities :webmasters
  :secure-subnets `(,(local-host-ip-address)))

#+Ignore
(export-log-window
  :authentication-realm nil
  :capabilities nil
  :secure-subnets nil)

#+Ignore
(export-log-window
 :host (local-host-domain-name)
 :port 8443
 :protocol :https
 :authentication-realm nil
 :capabilities nil
 :secure-subnets nil)
