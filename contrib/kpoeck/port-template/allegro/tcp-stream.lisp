(in-package :www-utils)

#|
(defparameter *test-socket* nil)

(setq *test-socket*
 (socket:make-socket 
  :connect :passive 
  :local-port 8080
  :reuse-address t
  :format :bivalent
  :type :hiper))

(class-of *test-socket*)
->
SOCKET:SOCKET-STREAM-INTERNET-PASSIVE

(defparameter *socket-stream*  nil)


(mp:process-run-function 
 "testaccept"
 #'(lambda(&rest params)
     (declare (ignore params))
     (setq *socket-stream* (socket:accept-connection *test-socket*))))

(defparameter *test-client-socket* nil)

(setq *test-client-socket* (socket:make-socket :remote-host "127.0.0.1" :remote-port 8080))
(class-of *test-client-socket*)
-> SOCKET:SOCKET-STREAM-INTERNET-ACTIVE

(class-of *socket-stream*)
->
SOCKET::HIPER-SOCKET-STREAM-INTERNET-ACTIVE

(change-class *socket-stream* 'tcp-client-stream)

(write-line "Test Socket" *test-client-socket*)
(force-output *test-client-socket*)
(read-line *socket-stream*)
(close *test-client-socket*)
(close  *test-socket*)
|#

(defclass tcp-server-stream (socket:socket-stream-internet-passive)
  ()
  )

;;; I wish this were exported
(defclass tcp-client-stream (socket::hiper-socket-stream-internet-active)
  ()
  )

;;; Does not seem to work, change-class errors
(defclass tcp-client-ssl-stream (excl::ssl-server-stream)
  ()
  )

(defun create-socket (host port)
  (let ((socket (socket:make-socket :connect :passive
                             :local-port port
                             :local-host host
                             :reuse-address t
                             :format :bivalent                         
                                    :type :hiper)))
    (handler-case
        (change-class socket 'tcp-server-stream)
      (error (e) (notify-log-window "Error changing class for socket:~s~%" e)))
    socket))

(defun wait-for-connection (socket)
  (let ((stream (socket:accept-connection socket)))
    (handler-case
        (change-class stream 'tcp-client-stream)
      (error (e)(notify-log-window "Error Changing class for socket stream ~s~%" e)))
    stream))

#-:ALLEGRO-CL-TRIAL (defvar *der* nil)
#-:ALLEGRO-CL-TRIAL (defvar *der2* nil)
#-:ALLEGRO-CL-TRIAL
(defun wait-for-ssl-connection (socket)
  (let ((stream (socket:accept-connection socket)))
    (setq *der* stream)
    (setq stream (socket:make-ssl-server-stream 
                  stream 
                  :certificate "c:/programs/acl80/src/aserve/test/server.pem"))
    (setq *der2* stream)                                          
    stream))

(defmacro with-stream-timeout ((stream timeout &key error-p) &body body)
  "Executes BODY and returns the values of the last form in BODY. 
However, if the stream goes idle for longer than TIMEOUT seconds, the
operation is aborted.  If ERROR-P is non-null, the time out error is
signalled, otherwise NIL is returned."
  `(progn ,stream
     (with-timeout (,timeout :error-p ,error-p) . ,body)))

(defmethod note-timeout ((me tcp-client-stream) timeout)
  (socket:socket-control 
   me 
   :read-timeout timeout
   :write-timeout timeout))

(defmethod note-timeout ((me excl::ssl-server-stream) timeout)
  (declare (ignore timeout)))

(defmethod foreign-host ((http-stream tcp-client-stream))
  (socket:remote-host http-stream))

(defmethod foreign-port ((http-stream tcp-client-stream))
  (socket:remote-port http-stream))

(defmethod local-port ((http-stream tcp-client-stream))
  (socket:local-port http-stream))

(defmethod foreign-host ((http-stream excl::ssl-server-stream))
  (socket:remote-host http-stream))

(defmethod foreign-port ((http-stream excl::ssl-server-stream))
  (socket:remote-port http-stream))

(defmethod local-port ((http-stream excl::ssl-server-stream))
  (socket::local-port http-stream))

#+do-I-need-this?
(defmethod excl:device-write :around ((stream socket::socket-stream-internet-active) buffer start end blocking)
  (declare (ignore buffer start end blocking))
  (mp:with-timeout (*persistent-connection-timeout* (error 'stream-error :format-control "device-write timeout"))
    (call-next-method)))


;;; how can these be implemented?


(defmethod bytes-received ((stream tcp-server-stream)) 0)
(defmethod (setf bytes-received) (val (stream tcp-server-stream)) val)

(defmethod bytes-received ((stream tcp-client-stream)) 0)
(defmethod (setf bytes-received) (val (stream tcp-client-stream)) val)

(defmethod bytes-transmitted ((stream tcp-client-stream)) 0)
(defmethod (setf bytes-transmitted) (val (stream tcp-client-stream)) val)

(defmethod bytes-received ((stream excl::ssl-server-stream)) 0)
(defmethod (setf bytes-received) (val (stream excl::ssl-server-stream)) val)

(defmethod bytes-transmitted ((stream excl::ssl-server-stream)) 0)
(defmethod (setf bytes-transmitted) (val (stream excl::ssl-server-stream)) val)

;;; from jkf
;;; Problem, the process-wait might block?

#+no
(defmethod http::http-input-data-available-p ((stream tcp-client-stream) 
                                              &optional timeout-seconds)
  ;; return true if there is non-whitespace data available
  ;; If timeout is given then don't wait any longer than that
  ;; for the answer
  
  (flet ((data-available-p (stream)
                           ;; see if there is non-whitespace data readable.
                           ;; return t if so.
                           ;; skip all whitespace characters.
                           ;; block on the first read but not subsequent ones
                           (let ((ch (peek-char nil stream nil)))
                             (loop
                               (excl:if* (null ch)
                                    then (return nil) ; eof
                                    elseif (not (member ch
                                                        '(#\return #\linefeed #\space #\tab)
                                                        :test #'eq))
                                    then (return t) ; have a character
                                    else (read-char stream)
                                    (excl:if* (not (listen stream))
                                         then (return nil) ; no more now available
                                       else (setq ch (peek-char nil stream nil))))))))
    (handler-case
        (excl:if* (not (live-connection-p stream))
           then nil  ; dead connection, no data
         elseif timeout-seconds
           then (mp:with-timeout (timeout-seconds 
                                  (and (listen stream)
                                       (data-available-p stream)))
                  (loop (excl:if* (data-available-p stream)
                           then (return t))))
           else (and (listen stream)
                     (data-available-p stream)))
      (excl:socket-error 
       (error)
       (notify-log-window "Catch Socket error in http::http-input-data-available-p ~s ~s ~s ~s ~%"
                          (stream-error-stream error)
                          (excl:stream-error-action error)
                          (excl:stream-error-code error)
                          (excl:stream-error-identifier error))
       nil))))

#+no
(defmethod http::http-input-data-available-p ((stream excl::ssl-server-stream) 
                                              &optional timeout-seconds)
  ;; return true if there is non-whitespace data available
  ;; If timeout is given then don't wait any longer than that
  ;; for the answer
  
  (flet ((data-available-p (stream)
                           ;; see if there is non-whitespace data readable.
                           ;; return t if so.
                           ;; skip all whitespace characters.
                           ;; block on the first read but not subsequent ones
                           (let ((ch (peek-char nil stream nil)))
                             (loop
                               (excl:if* (null ch)
                                    then (return nil) ; eof
                                    elseif (not (member ch
                                                        '(#\return #\linefeed #\space #\tab)
                                                        :test #'eq))
                                    then (return t) ; have a character
                                    else (read-char stream)
                                    (excl:if* (not (listen stream))
                                         then (return nil) ; no more now available
                                       else (setq ch (peek-char nil stream nil))))))))
    (handler-case
        (excl:if* (not (live-connection-p stream))
           then nil  ; dead connection, no data
         elseif timeout-seconds
           then (mp:with-timeout (timeout-seconds 
                                  (and (listen stream)
                                       (data-available-p stream)))
                  (loop (excl:if* (data-available-p stream)
                           then (return t))))
           else (and (listen stream)
                     (data-available-p stream)))
      (excl:socket-error 
       (error)
       (notify-log-window "Catch Socket error in http::http-input-data-available-p ~s ~s ~s ~s ~%"
                          (stream-error-stream error)
                          (excl:stream-error-action error)
                          (excl:stream-error-code error)
                          (excl:stream-error-identifier error))
       nil))))