(in-package :www-utils)

;;; For clozure
(defun %ipaddr-to-dotted (ip-number)
  (ccl:ipaddr-to-dotted ip-number)
  )

(defun %lookup-hostname (ip-number)
  (ccl:lookup-hostname ip-number))

(defun %ipaddr-to-hostname (ip-number)
  (ccl:ipaddr-to-hostname ip-number))

(defun %dotted-to-ipaddr (what)
  (ccl:dotted-to-ipaddr what))

(defun %make-client-socket (host &key port)
  (ccl:make-socket :remote-host host 
                   :remote-port port
                   :type :stream
                   :connect :active
                   :format :bivalent))

(defun %make-server-socket (&key port)
  (ccl:make-socket :local-port port
                   :type :stream
                   :connect :passive
		   ;;; at least for debugging
		   :nodelay t
		   :backlog 5
		   :reuse-address t
                   :format :bivalent))

(defun %wait-for-connection (socket)
  (let ((stream (ccl:accept-connection socket :wait t)))
    stream))

(defun %connect-with-timeout (host port timeout-in-seconds)
  (declare (ignore timeout-in-seconds))
    (%make-client-socket host :port port))

(define-condition network-error 
  (ccl:socket-error) 
  ())

(defun close-the-socket (socket)
  (close socket)
  )