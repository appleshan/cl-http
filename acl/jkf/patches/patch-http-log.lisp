(in-package :http)

;;; n-requests does not exist


(defmethod print-object ((log http-log) stream)
  (with-slots (name port n-transactions) log
    (print-unreadable-object (log stream :type t :identity t)
      (format stream "~A (~D) [~D transactions]" name port n-transactions))))