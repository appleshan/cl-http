(in-package :www-utils)

;;; clozure 
;;; the classes
;;; Server Socket = CCL::LISTENER-SOCKET
;;; Client stream and Server stream CCL::BASIC-TCP-STREAM
#|
(defparameter *test-socket* nil)

(setq *test-socket*
 (ccl:make-socket 
  :connect :passive 
  :local-port 8080
  :reuse-address t
  :format :bivalent))

(class-of *test-socket*)
->
SOCKET:SOCKET-STREAM-INTERNET-PASSIVE
CCL::LISTENER-SOCKET

(defparameter *socket-stream*  nil)


(ccl:process-run-function 
 "testaccept"
 #'(lambda(&rest params)
     (declare (ignore params))
     (setq *socket-stream* (ccl:accept-connection *test-socket*))))

(defparameter *test-client-socket* nil)

(setq *test-client-socket* (ccl:make-socket :remote-host "127.0.0.1" :remote-port 8080))
(class-of *test-client-socket*)
-> SOCKET:SOCKET-STREAM-INTERNET-ACTIVE
CCL::BASIC-TCP-STREAM

(class-of *socket-stream*)
->
SOCKET::HIPER-SOCKET-STREAM-INTERNET-ACTIVE
CCL::BASIC-TCP-STREAM

(change-class *socket-stream* 'tcp-client-stream)

(write-line "Test Socket" *test-client-socket*)
(force-output *test-client-socket*)
(listen *socket-stream*)
(read-line *socket-stream*)
(close *test-client-socket*)
(close  *test-socket*)
|#


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

(defmethod note-timeout ((me CCL::BASIC-TCP-STREAM) timeout)
  "Don't know yet"
  (declare (ignore timeout))
  )

(defmethod foreign-host ((http-stream CCL::BASIC-TCP-STREAM))
  (ccl:remote-host http-stream))

(defmethod foreign-port ((http-stream CCL::BASIC-TCP-STREAM))
  (ccl:remote-port http-stream))

(defmethod local-port ((http-stream CCL::BASIC-TCP-STREAM))
  (ccl:local-port http-stream))

;;; how can these be implemented?


(defmethod bytes-received ((stream CCL::BASIC-TCP-STREAM)) 0)
(defmethod (setf bytes-received) (val (stream CCL::BASIC-TCP-STREAM)) val)

(defmethod bytes-transmitted ((stream CCL::BASIC-TCP-STREAM)) 0)
(defmethod (setf bytes-transmitted) (val (stream CCL::BASIC-TCP-STREAM)) val)

