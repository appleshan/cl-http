(in-package :www-utils)

;;; For sbcl
;;; ip-number are vectors of (unsigned-byte 8)
;;; discard  sb-bsd-sockets:host-ent for now
;;; Partially stolen from hunchentoot and S-SYSDEPS
(defun %ipaddr-to-dotted (ip-number)
  (format nil "~a.~a.~a.~a"
	  (aref ip-number 0)
	  (aref ip-number 1)
	  (aref ip-number 2)
	  (aref ip-number 3)))

(defun %lookup-hostname (host-or-dotted)
  (sb-bsd-sockets:host-ent-address 
   (sb-bsd-sockets:get-host-by-name host-or-dotted)))

(defun %ipaddr-to-hostname (ip-number)
  (sb-bsd-sockets:host-ent-name (sb-bsd-sockets:get-host-by-address ip-number)))

(defun %dotted-to-ipaddr (dotted)
  (sb-bsd-sockets:make-inet-address dotted))
;;;
;;; (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)
;;; sb-bsd-sockets:socket-connect
#|
(describe 'sb-sys:make-fd-stream)
SB-SYS:MAKE-FD-STREAM is an external symbol in #<PACKAGE "SB-SYS">.
Function: #<FUNCTION (SB-C::&OPTIONAL-DISPATCH
                      SB-SYS:MAKE-FD-STREAM) {10762C75}>
Its associated name (as in FUNCTION-LAMBDA-EXPRESSION) is
  (SB-C::&OPTIONAL-DISPATCH SB-SYS:MAKE-FD-STREAM).
The function's arguments are:  (FD &KEY (INPUT NIL INPUT-P)
                                (OUTPUT NIL OUTPUT-P)
                                (ELEMENT-TYPE 'BASE-CHAR) (BUFFERING FULL)
                                (EXTERNAL-FORMAT DEFAULT) TIMEOUT FILE
                                ORIGINAL DELETE-ORIGINAL PATHNAME
                                INPUT-BUFFER-P DUAL-CHANNEL-P
                                (NAME
                                 (IF FILE (FORMAT NIL file ~A FILE)
                                     (FORMAT NIL descriptor ~W FD)))
                                AUTO-CLOSE)
Its defined argument types are:
  ((MOD 536870911) &KEY (:INPUT T) (:OUTPUT T) (:ELEMENT-TYPE T)
   (:BUFFERING (MEMBER :FULL :LINE :NONE)) (:EXTERNAL-FORMAT T)
   (:TIMEOUT (OR SINGLE-FLOAT DOUBLE-FLOAT RATIONAL NULL)) (:FILE T)
   (:ORIGINAL T) (:DELETE-ORIGINAL T) (:PATHNAME T) (:INPUT-BUFFER-P T)
   (:DUAL-CHANNEL-P T) (:NAME T) (:AUTO-CLOSE T))
Its result type is:
  *
|#

;;; this is stupid design, the port is mandatory
;;; fixme, change the parameter list
(defun %make-client-socket (host &key port timeout)
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (sb-bsd-sockets:socket-connect
     socket 
     (%lookup-hostname host)
     port)
    ;;; better be a stream
    (sb-bsd-sockets:socket-make-stream 
     socket 
     :element-type :default ;;; 'octet 'character 
     :input t :output t
     :buffering :none
     :timeout timeout
     )))

(defun %make-server-socket (&key port)
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    #-ecl (setf (sb-bsd-sockets:sockopt-tcp-nodelay socket) t)
    (sb-bsd-sockets:socket-bind socket #(0 0 0 0) port)
    (sb-bsd-sockets:socket-listen socket 5)
    socket))

;;; sb-bsd-sockets:socket-make-stream
(defun %wait-for-connection (socket)
  (let ((socket-accepted (sb-bsd-sockets:socket-accept socket)))
    (let ((stream (sb-bsd-sockets:socket-make-stream 
		   socket-accepted
		   :element-type :default ;;; 'octet 'character 
		   :input t :output t
		   :buffering :none)))
      (%note-stream&socket% stream socket-accepted)
      stream)))

(defun %connect-with-timeout (host port timeout-in-seconds)
  (%make-client-socket host :port port :timeout  timeout-in-seconds))

(define-condition network-error 
  (sb-bsd-sockets:socket-error) 
  ())

(defun close-the-socket (socket)
  (SB-BSD-SOCKETS:SOCKET-CLOSE socket)
  )

(defvar %stream-to-socket-table% 
  #+sbcl (make-hash-table :test 'eq :weakness :key :synchronized t)
  #+ecl (make-hash-table :test 'eq)
  #-(or sbcl ecl)(Error "Not impemented"))

;;;FIXme with-process-lock for ecl
(defun %note-stream&socket% (stream socket)
  (setf (gethash stream  %stream-to-socket-table%) socket))

(defun %get-socket-for-stream% (stream)
  (gethash stream  %stream-to-socket-table% nil))

#|
(defparameter www-utils::*test-socket* nil)
(defparameter www-utils::*socket-stream*  nil)
(defparameter www-utils::*test-client-stream* nil)
(defparameter www-utils::*test-port* 8080)
(defparameter www-utils::*test-thread* nil)
(setq  www-utils::*test-port* 8002)
;;; Server
(setq www-utils::*test-socket* (www-utils::%make-server-socket :port www-utils::*test-port*))

(setq  www-utils::*test-thread*
       (sb-thread:make-thread 
	#'(lambda(&rest params)
	    (declare (ignore params))
	    (setq  www-utils::*socket-stream* (www-utils::%wait-for-connection www-utils::*test-socket*)))
	:name "test-accept"))

;;; Client
(setq www-utils::*test-client-stream* (www-utils::%make-client-socket "127.0.0.1" :port www-utils::*test-port*))

;;; communicate
(write-line "Test Socket" www-utils::*test-client-stream*)
(force-output www-utils::*test-client-stream*)
(listen www-utils::*socket-stream*)
(read-line  www-utils::*socket-stream*)
(close www-utils::*test-client-stream*)
(close www-utils::*socket-stream*)
(sb-bsd-sockets:socket-close www-utils::*test-socket*)
(sb-bsd-sockets:socket-open-p  www-utils::*test-socket*)
|#