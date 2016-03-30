(in-package :http)

#+no
(defun pathname-dot-file-p (pathname)
  (let ((name (pathname-name pathname)))
    (if (keywordp name)
        t
      (or (null name)
          (null-string-p name)
          (eql #\. (char name 0))))))

#+no
(defmethod file-exportable-p (pathname)
  (not (pathname-dot-file-p pathname)))


(defmethod file-exportable-p (pathname)
  (flet ((pathname-dot-file-p (pathname)
           (let ((name (pathname-name pathname)))
             (if (keywordp name)
                 t
               (or (null name)
                   (null-string-p name)
                   (eql #\. (char name 0)))))))
    (declare (dynamic-extent #'pathname-dot-file-p))
    (not (pathname-dot-file-p pathname))))

(defmethod http-input-data-available-p ((stream COMM:SOCKET-STREAM) &optional timeout-seconds)
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
               (let ((no-timeout-p (mp::process-wait-for-input-stream stream :wait-reason "HTTP Input Data Available" :timeout timeout-seconds)))
                 (if no-timeout-p
                     (and (www-utils:live-connection-p stream)
                          (data-available-p stream))
                   nil)))
              (t nil))
      (comm:socket-error
       (error)
       (notify-log-window "Catched Socket error in http-input-data-available-p ~s ~s ~s ~s ~%"
                          "dont"
                          "know"
                          "yet" 
                          error)
       nil))))
