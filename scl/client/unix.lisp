;;; -*- Syntax: ansi-common-lisp; Base: 10; Package: http; Mode: LISP -*-

;;;------------------------------------------------------------------- 
;;;
;;; Scieneer CL HTTP CLIENT NETWORK INTERFACE
;;;

(in-package :http)

;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

;; this is the function used by the portable client code.
(defun %get-user-name+pw (url-string &optional (stream *standard-output*))
  (declare (ignore stream)
	   (values string string))
  (format t "~%~% Enter Password Information for URL ~s:~%" url-string)
  (format t "User Id: ")
  (let ((user-name (read-line)))
    (format t "Password: ")
    (let ((password (read-line)))
      (values user-name password))))

(defun open-tcp-stream (host port &optional (timeout *client-timeout*))
  "Opens a TCP stream to HOST on PORT with TIMEOUT."
  (declare (fixnum timeout)
           (values stream))
  (http::open-http-stream-to-host host port timeout))

(defmacro with-open-tcp-stream ((stream host port &key (timeout '*client-timeout*)) &body body)
  "Opens a TCP stream to HOST on PORT with TIMEOUT within the scope of BODY."
  `(unwind-protect
       (let ((,stream (open-tcp-stream,host ,port ,timeout)))
	 (unwind-protect
	      (progn . ,body)
	   (ignore-errors (close ,stream))
	   (close ,stream :abort t)))))

