(in-package :www-utils)

(eval-when
    (:load-toplevel :compile-toplevel :execute)
  (require "comm"))

;;; For lispworks
(defun %ipaddr-to-dotted (ip-number)
  (comm:ip-address-string ip-number)
  )

(defun %lookup-hostname (host)
  (comm:get-host-entry host :fields '(:address)))

(defun %ipaddr-to-hostname (ip-number)
  (comm:get-host-entry ip-number :fields '(:name)))

(defun %dotted-to-ipaddr (what)
  (comm:string-ip-address what))

;;; returns a COMM:SOCKET-STREAM
(defun %make-client-socket (host &key port)
  (comm:open-tcp-stream host port))

(defun %connect-with-timeout (host port timeout-in-seconds)
  (comm:open-tcp-stream host port :timeout timeout-in-seconds))

;;; Does not fit to well with the semantics of other lisps
;;; Probably can't simply create a socket and later call wait-for-connection
;;; Have to work more event based on processes

;;; Streams seem to be bivalent
;;; stream COMM:SOCKET-STREAM
;;; socket integer (file-handler)

(defun %make-server-socket-process (host port handle-function)
  (declare (ignore host))
  (comm:start-up-server
          :function #'(lambda(handle)
                        (let ((stream (make-instance 'comm:socket-stream
                                                     :socket handle
                                                     :direction :io
                                                     :element-type 'base-char)))
                          (funcall handle-function handle stream)))
          :service port
          :wait nil))

;;; no clear semantics in lw
#+no
(defun %wait-for-connection (socket)
  )

(define-condition network-error 
  (comm:socket-error) 
  ())

(defun close-the-socket (socket)
 (mp:process-kill socket)
 )

#|
(defvar *server-process*)
(defvar *socket*)
(defvar *stream*)

(setq *server-process*  (%make-server-socket-process 8000 #'(lambda(socket stream)
                                                        (setq *socket* socket
                                                              *stream* stream)
                                                        (write-line "Connected" *standard-output*))
                                                    ))
|#