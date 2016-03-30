(in-package :www-utils)

(eval-when
    (:load-toplevel :compile-toplevel :execute)
  (require "comm"))

(defmacro with-stream-timeout ((stream timeout &key error-p) &body body)
  "Executes BODY and returns the values of the last form in BODY. 
However, if the stream goes idle for longer than TIMEOUT seconds, the
operation is aborted.  If ERROR-P is non-null, the time out error is
signalled, otherwise NIL is returned."
  `(progn ,stream
     (with-timeout (,timeout :error-p ,error-p) . ,body)))

(defmethod note-timeout ((me COMM:SOCKET-STREAM) timeout)
  "Don't know yet"
  (declare (ignore timeout))
  )

(defmethod foreign-host ((http-stream COMM:SOCKET-STREAM))
  (multiple-value-bind (address port)
      (comm:socket-stream-peer-address http-stream)
    (declare (ignore port))
    address))

(defmethod foreign-port ((http-stream COMM:SOCKET-STREAM))
  (multiple-value-bind (address port)
      (comm:socket-stream-peer-address http-stream)
    (declare (ignore address))
    port))

(defmethod local-port ((http-stream COMM:SOCKET-STREAM))
  (multiple-value-bind (address port)
      (comm:socket-stream-address http-stream)
    (declare (ignore address))
    port))

(defmethod local-protocol ((http-stream COMM:SOCKET-STREAM))
  (declare (ignore http-stream))
  :http)

;;; how can these be implemented?


(defmethod bytes-received ((stream COMM:SOCKET-STREAM)) 0)
(defmethod (setf bytes-received) (val (stream COMM:SOCKET-STREAM)) val)

(defmethod bytes-transmitted ((stream COMM:SOCKET-STREAM)) 0)
(defmethod (setf bytes-transmitted) (val (stream COMM:SOCKET-STREAM)) val)


