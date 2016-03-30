(in-package :http)

(defmacro with-open-tcp-stream ((stream host port &key (timeout '*client-timeout*)) &body body)
  "Opens a TCP stream to HOST on PORT with TIMEOUT within the scope of BODY."
  `(unwind-protect
       (let ((,stream (www-utils::open-tcp-stream,host ,port ,timeout)))
	 (multiple-value-prog1
             (progn . ,body)
	   (when ,stream
             (close ,stream :abort (not (live-connection-p ,stream))))))))