;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10 -*-

(in-package "HTTP")

;;; Include patches here

;; missing methods for common-file-log for version 70-139-pre??

(defmethod log-notifications-on ((log common-file-log) &optional on-p)
  (with-slots (file-logging) log
     (setf file-logging on-p)))

(defmethod http::write-log-entry-to-file ((log http:common-file-log) (server http::server))
  (with-slots (file-logging file-stream filename) log
    (if file-logging
	(progn (or file-stream
		   (setq file-stream
			 (open filename :direction :output :if-does-not-exist :create :if-exists :append)))
	       (when file-stream
		 (let ((entry (snapshot-log-entry log server)))
		   (write-log-entry entry file-stream)))))))

(defmethod snapshot-log-entry ((log common-file-log) (server server))
  (let ((host-name (host-log-name server))
	(user-name (%server-user-qualified-name server))
	(request (server-request server t))	; don't lose when transaction reset
	(request-time (server-request-time server))
	(method (server-method server))
	(status (server-status server))
	(bytes-received (server-bytes-received server))
	(bytes-transmitted (server-bytes-transmitted server)))	;total bytes-transmitted (not number of bytes-transmitted in a document
    (allocate-common-log-entry log host-name request request-time method status bytes-received bytes-transmitted user-name)))

 ;;
;;;; This needs to be redefined here because server;utils.lisp overrides it
 ;;

;;
;; Add port specific backtrace output (from unix.lisp)
;;
#+ACL5
(defmethod http::write-stack-backtrace (error stream &optional (n-frames http::*stack-backtrace-number-of-frames*))
  (declare (ignore error stream))
  (tpl::zoom-print-stack n-frames t))
