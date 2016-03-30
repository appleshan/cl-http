(in-package :www-utils)

;;; For clisp
;;; Does only seem to work on dotted or hostnames
;;; IP-addr as integer does not seem to be used
(defun %ipaddr-to-dotted (ip-number)
  ip-number
  )

(defun %dotted-to-ipaddr (what)
  what)

(defun %lookup-hostname (dotted)
  (let ((addresses (posix:hostent-addr-list (posix:resolve-host-ipaddr dotted)))) 
    (if addresses
        (first addresses)
      nil)))

(defun %ipaddr-to-hostname (dotted-or-hostname)
  (posix:hostent-name (posix:resolve-host-ipaddr dotted-or-hostname)))

(defun %make-client-socket (host &key port timeout)
  (if timeout
      (socket:socket-connect port host :external-format :dos :timeout timeout)
    (socket:socket-connect port host :external-format :dos)))

(defun %connect-with-timeout (host port timeout-in-seconds)
  (%make-client-socket host :port port :timeout timeout-in-seconds))

(defun %make-server-socket (&key port)
  (SOCKET:SOCKET-SERVER port :BACKLOG 5))

(defun %wait-for-connection (socket)
  (let ((stream (SOCKET:SOCKET-ACCEPT socket
				      ;&KEY :ELEMENT-TYPE :EXTERNAL-FORMAT :BUFFERED :TIMEOUT
				      )))
    stream))

(define-condition network-error 
  (error) 
  ())

(defun close-the-socket (socket)
  (SOCKET:SOCKET-SERVER-CLOSE socket)
  )

#|
(DEFUN wget-text (host page &OPTIONAL (port 80))
  ;; HTTP requires the :DOS line terminator
  (WITH-OPEN-STREAM (socket (%make-client-socket host :port port))
     (FORMAT socket "GET ~A HTTP/1.0~2%" page)
     ;; dump the whole thing - header+data - into the output file
     (LOOP :for line = (READ-LINE socket nil nil) :while line
           :do (WRITE-LINE line))))

(www-utils::wget-text "www.spiegel.de" "http://www.spiegel.de/index.html")
|#
