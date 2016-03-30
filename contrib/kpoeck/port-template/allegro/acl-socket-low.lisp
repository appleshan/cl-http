(in-package :www-utils)

;;; For allegro
(defun %ipaddr-to-dotted (ip-number)
  (socket:ipaddr-to-dotted ip-number)
  )

(defun %lookup-hostname (ip-number)
  (socket:lookup-hostname ip-number))

(defun %ipaddr-to-hostname (ip-number)
  (socket:ipaddr-to-hostname ip-number))

(defun %dotted-to-ipaddr (what)
  (socket:dotted-to-ipaddr what))

(defun %make-client-socket (host &key port)
  (socket:make-socket :remote-host host 
                      :remote-port port
                      :format :bivalent))

(defun %connect-with-timeout (host port timeout-in-seconds)
  (sys:with-timeout (timeout-in-seconds)
    (%make-client-socket host :port port)))

(define-condition network-error 
  (excl:socket-error) 
  ())

(defun close-the-socket (socket)
  (close socket)
  )