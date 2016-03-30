(in-package :http)

(http::log-notifications-on (multiport-access-logs) nil)

(http:log-notifications-on (http:current-access-logs) log)


(setq *log-access-log-class* 'http:http-log)
(setq *log-access-log-class* 'common-file-log)
(setq *log-access-log-class* 'notification-log)
(clear-access-logs)

;; Make sure the log object has been initialized.
(ensure-current-log)

;; Write a common log file.
(log-file-logging-on (current-access-logs) t)
(log-dynamic-logging-on (current-access-logs) t)
(log-notifications-on (current-access-logs) t)


;; Show transactions in a log window
;; Turn this off in production servers to save time writing log entries to the notification window.
(log-notifications-on (multiport-access-logs) t)