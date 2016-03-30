(in-package :http)

(define start (&key 		    
               (port 80)
               )
  
  
  "Short version of ENABLE-HTTP-SERVICE."
  
  
  (reset-server-local-host-variables :standard-port port)
  (set-dispatch-macro-character #\# #\u  #'sharp-sign-u-reader-helper)
  (run-server-initializations t)
  (run-server-launch-initializations t)
  (notify-log-window "HTTP service enabled for: http://~A:~D/"
                     (www-utils:local-host-domain-name) port)
  
  ;; persistent connections cause problems when accessing www.cnn.com
  ;; with the proxy server on.
  (setq http::*client-persistent-connections* nil)
  (setq html::*standard-color-scheme*
        (html:create-color-scheme :background :white
                                  :foreground :black
                                  :link :blue
                                  :visited-link :purple
                                  :active-link :red))
  
  
  (terpri))

(defvar *in-stream*)
(defvar *out-stream*)
(defvar *stream*)
(defvar *server-stream*)

(defun use-chunked-transfer-encoding-p (content-length)
  content-length
  nil)

#-(or ecl clisp sbcl cmu)
(defclass string-server (#. (class-name (class-of (make-two-way-stream *standard-input* *standard-output*))))
  ()
  )

(defmethod WWW-UTILS:NOTE-first-CHUNK ((stream #+(or ecl clisp sbcl cmu) t 
					       #-(or ecl clisp sbcl cmu) string-server)))

(defmethod WWW-UTILS:NOTE-LAST-CHUNK ((stream #+(or ecl clisp sbcl cmu) t 
					      #-(or ecl clisp sbcl cmu)  string-server) &optional footers-plist)
  footers-plist)

(defmethod WWW-UTILS:CHUNK-TRANSFER-ENCODING-MODE ((stream #+(or ecl clisp sbcl cmu) t 
							   #-(or ecl clisp sbcl) string-server) chunk-function)
  chunk-function)

(defmethod http::stream-copy-until-eof (from-stream to-stream &optional (copy-mode :text))
  (declare (ignore copy-mode))
  #+no (loop doing 
             (multiple-value-bind (line)
                 (read-line from-stream nil :end nil)
               (when (eq line :end)
                 (return))
               (write-line line to-stream)
               ))
  #-no (with-binary-stream (from-stream :input)
         (with-binary-stream (to-stream :output)
           (loop for byte = (read-byte from-stream nil)
               while byte
               do (write-char (code-char byte) to-stream)))))

#+(or clisp sbcl cmu lispworks clozure-common-lisp (and allegro unix))
(defmethod http::stream-copy-until-eof (from-stream to-stream &optional (copy-mode :text))
  (declare (ignore copy-mode))
  (with-binary-stream (from-stream :input)
         (with-binary-stream (to-stream :output)
           (loop for char = (read-char from-stream nil)
               while char
               do (write-char char to-stream)))))

(defun test (doc)
  (setq *in-stream* 
        (make-string-input-stream
         (with-output-to-string (*standard-output*)
           (write-line (concatenate 'string "GET /" doc " HTTP/1.1"))
           (write-line "Accept: image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, application/x-shockwave-flash, application/vnd.ms-excel, application/vnd.ms-powerpoint, application/msword, */*")
           (write-line "Referer: http://localhost:8000/cl-http/frame-index.html")
           (write-line "Accept-Language: en-gb")
           (write-line "Accept-Encoding: gzip, deflate")
           (write-line "If-Modified-Since: Fri, 29 Aug 1999 01:10:24 GMT")
           (write-line "If-None-Match: \"3271108224\"")
           (write-line "User-Agent: Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; .NET CLR 1.1.4322)")
           (write-line "Host: localhost:80")
           (write-line ""))))
  ;;;;;Connection: Keep-Alive
  
  
  (setq *out-stream* (make-string-output-stream))
  (setq *stream*
        #+(or ecl clisp sbcl cmu)
        (make-two-way-stream *in-stream* *out-stream*)
      #-(or ecl clisp sbcl cmu)
      (change-class (make-two-way-stream *in-stream* *out-stream*) 'string-server))
  
  (setq *server* (make-server nil  *stream* "localhost" "127.0.0.1"))
  (initialize-resourced-server nil *server* *stream* "localhost" "127.0.0.1")
  (setq *local-context* "http://localhost")
  (setq *server-stream* (server-stream *server*))
  
  (clear-white-space *server-stream*)	;tolerate clients with leading CR-LF or whitespace
  (let ((stream *server-stream*)
        (server *server*)
        )
    (multiple-value-bind (request eof delimiter)
        (read-delimited-line stream '(#\Linefeed #\Return) t (%server-request server))
      delimiter				;ignore
      ;; http-version not set, but will default via server-http-version
      (when eof
        (error 'request-timeout :format-string "Client dropped connection while reading request line."))
      (setf (%server-request server) request	;capture in case of growth
        (server-request-time server) (get-universal-time))	;set the request time
      (handler-case 
          (%execute-request server request stream)
        (reportable-condition (condition)
                              (format t "Reportable condition caught: ~%")
                              ;(break "")
                              (#+no handler-case #-no progn
                                    (report-status condition stream)
                                    #+no (error (err) (bug-report-error err) nil)
                                    )))
      )
    
    (log-access *server*)
    (format t "And the result is:~%")
    (format t "~s~%" (get-output-stream-string *out-stream*))
    )
  )
