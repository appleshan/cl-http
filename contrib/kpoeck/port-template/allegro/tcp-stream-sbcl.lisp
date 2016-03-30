(in-package :www-utils)

(defun create-socket (host port)
  (declare (ignore host))
  (%make-server-socket :port port))

(defun wait-for-connection (socket)
  (%wait-for-connection socket))

(defmacro with-stream-timeout ((stream timeout &key error-p) &body body)
  "Executes BODY and returns the values of the last form in BODY. 
However, if the stream goes idle for longer than TIMEOUT seconds, the
operation is aborted.  If ERROR-P is non-null, the time out error is
signalled, otherwise NIL is returned."
  `(progn ,stream
     (with-timeout (,timeout :error-p ,error-p) . ,body)))

(defmethod note-timeout ((me sb-sys:fd-stream) timeout)
  "Don't know yet"
  (declare (ignore timeout))
  )

;;; don't know how to implement these 3
;;; the stream does not have a pointer back to the socket
;;; Could make a hash-table to have the back-pointer
;;; And delete the pointer in the finaliser??
;;; Or just store these 3 values in the right moment to make the gc happy
;;; Fixme, check whether the weck hash-table works as thought

(defmethod foreign-host ((http-stream sb-sys:fd-stream))
  (let ((socket (%get-socket-for-stream% http-stream)))
    (if socket
      (multiple-value-bind 
       (host port)
       (sb-bsd-sockets:socket-peername socket)
       (declare (ignore port))
       (if (vector-ip-adress-p host)
	   (%ipaddr-to-dotted host)
	   host))
      "localhost")))

(defmethod foreign-port ((http-stream sb-sys:fd-stream))
  (let ((socket (%get-socket-for-stream% http-stream)))
    (if socket
      (multiple-value-bind 
       (host port)
       (sb-bsd-sockets:socket-peername socket)
	(declare (ignore host))
       port)
      http::*standard-http-port*)))

(defmethod local-port ((http-stream sb-sys:fd-stream))
  (let ((socket (%get-socket-for-stream% http-stream)))
    (if socket
      (multiple-value-bind 
       (host port)
       (sb-bsd-sockets:socket-name socket)
	(declare (ignore host))
       port)
      http::*standard-http-port*)))

;;; and these also?


(defmethod bytes-received ((stream sb-sys:fd-stream)) 0)

(defmethod (setf bytes-received) (val (stream sb-sys:fd-stream)) val)

(defmethod bytes-transmitted ((stream sb-sys:fd-stream)) 0)

(defmethod (setf bytes-transmitted) (val (stream sb-sys:fd-stream)) val)


