(in-package :http)

(defclass console-log (synchronous-log-notification-mixin access-log)
  ()
  )

(eval-when (:compile-toplevel :load-toplevel :evaluate)
  (export 'console-log (find-package :http))
  )

;;; (log-notifications-on (current-access-logs) t)
;;; (setq *log-access-log-class* 'console-log)
;;; (ensure-current-log)

(defun notify-log-window (format-string &rest format-args)
  "Top-level method for writing to the HTTP log window."
  (let ((stream excl:*initial-terminal-io*))
    (fresh-line stream)
    (write-char #\[ stream)
    (http::write-standard-time (get-universal-time) stream)
    (write-string "]  " stream)
    (apply #'format stream format-string format-args)))

#+no
(defun %server-notify-console-window (server)
  (let ((stream excl:*initial-terminal-io*))
    (fresh-line stream)
    (write-char #\[ stream)
    (http::write-standard-time (get-universal-time) stream)
    (write-string "]  " stream)
    (%server-write-console-window-notification server stream)))
