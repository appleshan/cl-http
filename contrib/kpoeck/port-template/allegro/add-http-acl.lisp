(in-package :http)

;;; This is faster, but not an order of magnitud
;;; Probably anyhow buffered

(defparameter *buffer-size* #.(* 1024 1024 1))

(defmethod stream-copy-until-eof ((input-stream excl:fundamental-binary-input-stream)
                                  (socket-stream www-utils::tcp-client-stream) 
                                  &optional copy-mode)
  (declare (ignore copy-mode))
  (let ((buffer (make-array *buffer-size* :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent buffer))
    (loop
      (let ((position (read-sequence buffer input-stream)))
        (cond ((= position *buffer-size*) 
               ;complete buffer filled
               (write-sequence buffer socket-stream))
              (t
               ;partial read
               (write-sequence buffer socket-stream :end position)
               (return)))))))
#|
(export-url #u"/cl-http/bebe.jpeg"
            :jpeg-image
            :pathname "http:www;cl-http;examples;P1000656.JPG"
            :expiration `(:interval 0)
            :public t
            :keywords '(:cl-http :demo))

(with-open-file (file-stream "http:www;cl-http;examples;P1000656.JPG")
  (file-length file-stream))

(www-utils::file-length-in-bytes "http:www;cl-http;examples;P1000656.JPG")

#+no
(defmethod STREAM-COPY-UNTIL-EOF ((input-stream excl:fundamental-binary-input-stream)
                                  (socket-stream www-utils::tcp-client-stream) 
                                  &optional copy-mode)
  (www-utils:notify-log-window "Call my method")
  (call-next-method))
|#

(defmethod http-input-data-available-p ((stream www-utils::tcp-client-stream) &optional timeout-seconds)
  "Returns non-null when input data is available on the HTTP STREAM within
TIMEOUT-SECONDS.  When timeout-seconds is null, data must be immediately
available. A dead HTTP connection means no data is available.
Ports can specialize this as necessary for their stream and process implementations."
  (flet ((data-available-p (stream)
                           (loop for char = (when (listen stream)
                                              (peek-char nil stream nil))
                               while char
                               when (member char '(#\return #\linefeed #\space #\tab) :test #'eql)
                               do (read-char stream t)
                               else
                               return t                   ;something still there.
                               finally (return nil))))
    (declare (dynamic-extent #'data-available-p))
    (handler-case 
        (cond ((not (www-utils:live-connection-p stream)) nil)
              ((data-available-p stream) t)
              ((and timeout-seconds (not (zerop timeout-seconds)))
               ;; Block until there is reason to take action
               (let ((no-timeout-p (mp:wait-for-input-available stream :whostate "HTTP Request Wait" :timeout timeout-seconds)))
                 (if no-timeout-p
                     (and (www-utils:live-connection-p stream)
                          (data-available-p stream))
                   nil)))
              (t nil))
      (excl:socket-error 
       (error)
       (notify-log-window "Catched Socket error in http-input-data-available-p ~s ~s ~s ~s ~%"
                          (stream-error-stream error)
                          (excl:stream-error-action error)
                          (excl:stream-error-code error)
                          (excl:stream-error-identifier error))
       nil))))

;;; The version from JKF
#+no
(defmethod http-input-data-available-p ((stream tcp-client-stream) 
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
;;; And from jcma
#+no
(defmethod http::http-input-data-available-p ((stream cl-http-chunk-transfer-socket-stream) &optional timeout-seconds)
  "Returns non-null when input data is available on the HTTP STREAM within
TIMEOUT-SECONDS.  When timeout-seconds is null, data must be immediately
available. A dead HTTP connection means no data is available.
Ports can specialize this as necessary for their stream and process implementations."
  (labels ((data-available-p (stream)
                             (loop for char = (when (listen stream)
                                                (peek-char nil stream nil))
                                 while char                   ;clear any dangling White Space due to buggy clients.
                                 when (member char '(#\return #\linefeed #\space #\tab) :test #'eql)
                                 do (read-char stream t)
                                 else
                                 return t                   ;something still there.
                                 finally (return nil)))
           (continue-p (stream)
                       (or (not (www-utils:live-connection-p stream))     ;connection went dead
                           (data-available-p stream))))   ;data available
    (declare (inline data-available-p))
    (cond ((not (www-utils:live-connection-p stream)) nil)
          ((data-available-p stream) t)
          #-LispWorks
          ((and timeout-seconds (not (zerop timeout-seconds)))
           ;; Block until there is reason to take action
           (process-wait-with-timeout
            "HTTP Request Wait" timeout-seconds #'continue-p stream)
           ;; Determine whether input data was available without consing.
           (and (www-utils:live-connection-p stream)
                (listen stream)))
          #+LispWorks
          ((and timeout-seconds (not (zerop timeout-seconds)))
           ;; Block until there is reason to take action
           (loop (unless (www-utils:live-connection-p stream)
                   (return nil))
                 (when (data-available-p stream)
                   (return t))
                 (unless (www-utils:process-wait-for-stream
                          "HTTP Request Wait" stream
                          nil timeout-seconds)
                   ;; Timeout expired
                   (return nil))))
          (t nil))))