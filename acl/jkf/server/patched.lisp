;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10 -*-

(defpackage "HTTP"
  (:shadowing-import-from #:www-utils #:stream-error)
  (:import-from #:excl #:without-package-locks))

(in-package "HTTP")

;;; Include patches here

#+no
(defmethod report-status-unhandled-error ((error stream-error) stream request)
  (declare (ignore stream request))
  )

#+no
(defmethod report-status :around ((condition http-condition) stream)
  (flet ((report-rentrant-error (primary-error secondary-error)
           (let ((secondary-error-type (type-of secondary-error)))
             (report-bug *bug-http-server*
                         (format nil "REPORT-STATUS Re-entrant Error: ~S" secondary-error-type)
                         "~:[~;~&Log: ~:*~S~]~&Primary Error: ~S~:[~;~&Primary Error Report: ~:*~A~]~
                          ~&Secondary Error: ~S~:[~;~&Secondary Error Report: ~:*~A~]~:[~;~&Backtrace: ~:*~A~]"
                         (when http:*server* (write-extended-common-logfile-entry http:*server* nil))
                         (type-of primary-error) (report-string primary-error) (type-of secondary-error) (report-string secondary-error)
                         (when *stack-backtraces-in-bug-reports* (stack-backtrace-string condition))))))
    (cond (*report-status-condition*
           (report-rentrant-error *report-status-condition* condition))
          (t (let ((*report-status-condition* condition))
               (handler-case-if (not *debug-server*)
		  (with-text-stream (stream :output)	;in case we're in binary mode
		    (call-next-method condition stream))
                 (network-error () nil)         ;no need to report errors telling the user he lost
                 (stream-error () (process-kill (current-process)))
                                (error (error) (bug-report-error error))))))))

#+no
(defmethod report-status :around ((condition http-condition) stream)
  (call-next-method))

(defmethod chunk-transfer-content-length ((stream
                                           #-(version>= 6) socket::socket-stream-internet-active-bivalent
                                           #+(version>= 6) socket::socket-stream-internet-active))
  )

(defmethod set-file-modification-date ((pathname pathname) universal-time &optional error-p)
  (declare (ignore error-p))
  universal-time)

#+(version>= 6)
(without-package-locks
 (defmethod excl:device-write :around ((stream excl:socket-base-simple-stream) buffer start end blocking)
   (declare (ignore buffer start end blocking))
   (mp:with-timeout (*persistent-connection-timeout* (error 'stream-error :format-control "device-write timeout"))
     (call-next-method))))

#+(version>= 6)
(defmethod excl:device-write :around ((stream socket::socket-stream-internet-active) buffer start end blocking)
  (declare (ignore buffer start end blocking))
  (mp:with-timeout (*persistent-connection-timeout* (error 'stream-error :format-control "device-write timeout"))
    (call-next-method)))

