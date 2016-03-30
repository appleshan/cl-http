;;;
;;; Scieneer Common Lisp stream support for the FTP client.
;;;
;;; Copyright (C) 2006, John C. Mallery. 
;;; Copyright (C) 2006, Scieneer Pty Ltd.
;;; All rights reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; FTP SOCKETS AND STREAMS
;;;

(in-package :www-utils)

(defclass socket ()
  ((socket-handle :type fixnum :initarg :socket :reader socket-handle)
   (port :initarg :port :reader socket-local-port :type fixnum)
   (element-type :initarg :element-type :reader socket-element-type :type (member signed-byte unsigned-byte base-char))
   (read-timeout :initarg :read-timeout :reader socket-read-timeout :type rational)
   (local-protocol :initarg :local-protocol :accessor socket-local-protocol :type keyword)))

(defclass passive-socket (socket) ())

(defmethod print-object ((socket socket) stream)
  (print-unreadable-object (socket stream :type t :identity t)
    (multiple-value-bind (host port)
        (socket-address socket)
      (format stream "@~D on port ~D" host port))))

(defmethod socket-address ((socket socket))
  (declare (values host port))
  (with-slots (socket-handle) socket
    (ext:get-socket-host-and-port socket-handle)))

(defgeneric stream-input-available-p (stream-or-socket)
  (:documentation "Non-nil if data is waiting to be read from STREAM-OR-SOCKET ."))

(defmethod stream-input-available-p ((socket-handle fixnum))
  (sys:wait-until-fd-usable socket-handle :input 0))

(defmethod stream-input-available-p ((socket socket))
  (sys:wait-until-fd-usable (socket-handle socket) :input 0))

(defmethod stream-input-available-p ((stream sys:fd-stream))
  (listen stream))

(defmethod close ((socket socket) &key abort)
  (declare (ignore abort))
  (unix:unix-close (socket-handle socket)))

(defun make-passive-socket (local-port &key element-type read-timeout no-error-p backlog (protocol :ftp))
  (let ((socket-handle
	 (ignore-errors
	   (ext:create-inet-listener local-port :stream
				     :reuse-address t
				     :backlog backlog))))
    (cond (socket-handle
           (make-instance 'passive-socket
                          :port local-port
                          :socket socket-handle
                          :element-type element-type
                          :read-timeout read-timeout
                          :local-protocol protocol))
          (no-error-p nil)
          (t (error 'www-utils:connection-error)))))

(defmethod accept-connection ((passive-socket passive-socket))
  (let ((read-timeout (socket-read-timeout passive-socket))
        (socket-handle (socket-handle passive-socket)))
    ;; Wait until ready for input.
    (let ((thread::*thread-whostate*
	   "Waiting for new HTTP connection"))
      (mp:process-wait-until-fd-usable socket-handle :input))
    (let ((new-fd (ignore-errors (ext:accept-tcp-connection socket-handle))))
      (format t "* accept passive ftp connection fd=~s new-fd=~s~%"
	      socket-handle new-fd)
      (cond (new-fd
	     (sys:make-fd-stream socket-handle :input t :output t
				 :element-type (socket-element-type passive-socket)
				 :timeout  read-timeout))
	    (t (error 'www-utils:network-error))))))


(defun ftp-open-stream (host port &key (mode :text) timeout)
  (let ((fd (ext:connect-to-inet-socket host port :timeout timeout)))
    (system:make-fd-stream fd :input t :output t
			   :element-type (ecase mode
					   (:text 'base-char)
					   (:binary '(unsigned-byte 8))))))
