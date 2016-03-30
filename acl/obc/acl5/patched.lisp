;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: WWW-UTILS; Base: 10 -*-

(in-package :www-utils)

;; Until we have chunk encoding decoding for 1.1
;(setq *http-version* "HTTP/1.0")
;(setq http::*persistent-connection-maximum-requests* 1)
;(setq http::*number-of-listening-processes* 1)
;(setq http::*persistent-connection-timeout* 0)
;(setq http1:log-file-stream-stays-open t)

;;; original is http:acl;aclpc;patched.lisp
;;; do something meaningful for the stream type on acl5
(defmethod transfer-buffer-streams ((from-stream stream)
                                    (to-stream stream)
                                    &optional (size 4096))
   (loop with buffer = (make-string size)
       with buffer-size = (min size (length buffer))
       as end = (read-sequence buffer from-stream :end buffer-size)
       do (write-sequence buffer to-stream :end end)
	  (clim-sys:process-yield)
       when (< end buffer-size)
       do (finish-output to-stream)
       until (zerop end)))

#+ignore
(define-macro with-text-stream ((stream direction) &body body)
  "Turns STREAM into a text stream within the scope of BODY.
direction can be :OUTPUT, :INPUT, or :BOTH."
  (declare (ignore stream direction))
  `(progn ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :http1)

;;; have the log entry do nothing but print to the sceen for debugging
#+Ignore
(defmethod write-access-log-entry ((log common-file-format-mixin)
                                   (server server-logging-mixin)
                                   log-stream
                                   gmt-p)
   )

#+Ignore
(defmethod write-access-log-entry ((log extended-common-file-format-mixin)
                                   (server server-logging-mixin)
                                   log-stream
                                   gmt-p)
   )

#+Ignore
(defmethod log-access ((server server-logging-mixin)) nil)


 ;;
;;;; Work around confusion between use of recursive and non recursive locks
 ;;

(in-package :url)

(define remap-url-host (old-host new-host &key (old-port http:*standard-http-port*) (new-port nil new-port-supplied-p)
                                 (report-stream *standard-output*))
  "Remaps all known URLs from OLD-HOST OLD-PORT to NEW-HOST NEW-PORT.
OLD-PORT defaults to the standard HTTP port. NEW-PORT defaults to OLD-PORT.
However, if NEW-PORT is NIL, the current port value is preserved."
  (let ((n-port (if new-port-supplied-p
                    new-port
                    (or old-port http:*standard-http-port*)))
	(remap-url-list))
    (format report-stream "~&Remapping URLs from ~A:~:[*~;~:*~D~] to ~A:~:[*~;~:*~D~] . . . ."
            old-host old-port new-host n-port)
    (with-lock-held ((uri-universe-lock *uri-universe*) :write "Remap URL Host")
      (loop for url being each hash-value in (uri-universe-table *uri-universe*)
	    when (host-match-p url old-host old-port)
	      do ;;; recursive lock issue here, differ change-host:
	         ;;; (change-host url new-host (or n-port (port url)))
	         (push (list url new-host (or n-port (port url))) remap-url-list)
	      and sum 1 into count
	    finally (format report-stream "~&~D URLs remapped from ~A:~:[*~;~:*~D~] to ~A:~:[*~;~:*~D~]."
			    count old-host old-port new-host n-port)))
    (loop for (url new-host port) in remap-url-list
	  do (change-host url new-host port))
    *uri-universe*))
