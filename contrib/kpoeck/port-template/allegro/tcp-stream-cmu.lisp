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

(defmethod note-timeout ((me system:fd-stream) timeout)
  "Don't know yet"
  (declare (ignore timeout))
  )

(defmethod foreign-host ((http-stream system:fd-stream))
  (multiple-value-bind 
       (host port)
       (ext:get-peer-host-and-port (sys:fd-stream-fd http-stream))
	(declare (ignore port))
       host))

(defmethod foreign-port ((http-stream system:fd-stream))
  (multiple-value-bind 
       (host port)
       (ext:get-peer-host-and-port (sys:fd-stream-fd http-stream))
	(declare (ignore host))
       port))

(defmethod local-port ((http-stream system:fd-stream))
  (multiple-value-bind 
       (host port)
       (ext:get-socket-host-and-port (sys:fd-stream-fd http-stream))
	(declare (ignore host))
       port))


;;; and these also?


(defmethod bytes-received ((http-stream system:fd-stream)) 0)

(defmethod (setf bytes-received) (val (http-stream system:fd-stream)) val)

(defmethod bytes-transmitted ((http-stream system:fd-stream)) 0)

(defmethod (setf bytes-transmitted) (val (http-stream system:fd-stream)) val)


