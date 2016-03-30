(in-package :www-utils)

;;; For cmu
(defun %ipaddr-to-dotted (ip-number)
  (extensions:ip-string ip-number)
  )

(defun %lookup-hostname (host)
  (let ((host-ent (extensions:lookup-host-entry host)))
    (if host-ent
	(let ((address (extensions:host-entry-addr-list host-ent)))
	  (if address
	      (first address)
	      nil))
	nil)))

;;; (%ipaddr-to-dotted (%lookup-hostname "www.franz.com"))

(defun %ipaddr-to-hostname (ip-number)
  (let ((host-ent (extensions:lookup-host-entry ip-number)))
    (if host-ent
	(extensions:host-entry-name host-ent)
	ip-number)))

;;; (%ipaddr-to-hostname (%lookup-hostname "www.franz.com"))

;;; Not really convincing, but perhaps better than parsing the string
#+nil
(defun %dotted-to-ipaddr (what)
  (%lookup-hostname what))

;;; From pw. Paul F. Werkowski
(defun %dotted-to-ipaddr (string)
  (declare (type simple-string string))
  (multiple-value-bind 
    (n1 p1)(parse-integer string :junk-allowed t)
    (when n1
      (locally (declare (fixnum p1))
	(multiple-value-bind
	  (n2 p2)(parse-integer string :start (1+ p1) :junk-allowed t)
	  (when n2
	    (locally (declare (fixnum p2))
	      (multiple-value-bind
		(n3 p3)(parse-integer string :start (1+ p2) :junk-allowed t)
		(when n3
		  (locally (declare (fixnum p3))
		    (let* ((n4 (parse-integer string :start (1+ p3))))
		      (declare (type (integer 0 255) n1 n2 n3 n4))
		      (+ (ash n1 24)
			 (ash n2 16)
			 (ash n3  8)
			 n4))))))))))))

(defun %make-client-socket (host &key port timeout)
  (let ((fd (extensions:connect-to-inet-socket host port)))
    (if timeout
	(system:make-fd-stream fd
			   :input t
			   :output t
			   :buffering :full
			   :timeout timeout
			   )
	(system:make-fd-stream fd
			   :input t
			   :output t
			   :buffering :full))
    )
  )

(defun %make-server-socket (&key port)
  (extensions:create-inet-listener port
				   :stream
				   :backlog 5
				   :reuse-address t))

(defun %wait-for-connection (socket)
  (let ((fd (extensions:accept-tcp-connection socket)))
    (system:make-fd-stream fd
			   :input t
			   :output t
			   :buffering :full
			   ;;; read timeouts with :timeout
			   )))

(defun %connect-with-timeout (host port timeout-in-seconds)
  (declare (ignore timeout-in-seconds))
    (%make-client-socket host :port port))

(define-condition network-error 
  (ext:socket-error) 
  ())

(defun close-the-socket (socket)
  (extensions:close-socket socket)
  )

#|
(defparameter *socket* (%make-server-socket :port 8000)) 
(setq *stream* (%wait-for-connection *socket*))
|#