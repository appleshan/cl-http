(in-package :http)


;;; Logging with notification-log is crashing without that, joder 

(defmethod log-server-access and ((log notification-log) (agent t))
  ;;; don't know what to log
  )