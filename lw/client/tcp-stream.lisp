;;; -*- Mode: Lisp; Package: IPC; Syntax: Ansi-Common-Lisp; -*-
;;;
;;; LISPWORKS STREAM SUPPORT FOR FTP CLIENT
;;;
;;; Copyright (C) 2006, John C. Mallery. 
;;; All rights reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; FTP SOCKETS AND STREAMS
;;;

(in-package :IPC)

(defclass socket ()
  ((socket-handle :type fixnum :initarg :socket :reader socket-handle)
   (port :initarg :port :reader socket-local-port :type fixnum)
   (element-type :initarg :element-type :reader socket-element-type :type (member signed-byte unsigned-byte base-char))
   (read-timeout :initarg :read-timeout :reader socket-read-timeout :type (fixnum 0))
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
    (comm:socket-stream-address socket-handle)))

(defgeneric stream-input-available-p (stream-or-socket)
  (:documentation "Non-nil if data is waiting to be read from STREAM-OR-SOCKET ."))

(defmethod stream-input-available-p ((socket-handle fixnum))
  (comm::socket-listen socket-handle))

(defmethod stream-input-available-p ((socket socket))
  (comm::socket-listen (socket-handle socket)))

(defmethod stream-input-available-p ((stream comm:socket-stream))
  (or (comm::socket-listen (comm:socket-stream-socket stream))
      (listen stream)))

(defmethod close ((socket socket) &key abort)
  (declare (ignore abort))
  (comm::close-socket (socket-handle socket)))

(defun make-passive-socket (local-port &key element-type read-timeout no-error-p backlog (protocol :ftp))
  (multiple-value-bind (socket-handle error-location error-code)
      (comm::create-tcp-socket-for-service local-port :backlog backlog :address t)
    (declare (ignore error-location error-code))
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
    (http::with-timeout (read-timeout :error-p t)
      (when (comm::socket-listen socket-handle)
        (multiple-value-bind (foreign-host foreign-port)
            (comm:get-socket-peer-address socket-handle)
          (cond (foreign-host
                 (make-instance 'cl-http-chunk-transfer-socket-stream
                                :socket socket-handle
                                :direction :io
                                :element-type (socket-element-type passive-socket)
                                :local-port (socket-local-port passive-socket)
                                :foreign-host foreign-host
                                :foreign-port foreign-port
                                :read-timeout  read-timeout
                                :local-protocol (socket-local-protocol passive-socket)))
                (t (error 'www-utils:network-error))))))))

(defun %open-ftp-stream-to-host (host port mode timeout)
  (let ((socket-handle nil))
    (unwind-protect
	(progn
	  (setq socket-handle (comm:connect-to-tcp-server host port :errorp nil :timeout timeout))
	  (if socket-handle
              (multiple-value-bind (local-host local-port)
                  (comm:get-socket-address socket-handle)
                (make-instance 'ipc::cl-http-chunk-transfer-socket-stream
                               :socket (shiftf socket-handle nil)
                               :direction :io
                               :element-type (ecase mode
                                               (:text 'base-char)
                                               (:binary '(unsigned-byte 8)))
                               :local-host local-host
                               :local-port local-port
                               :foreign-host host
                               :foreign-port port
                               :read-timeout timeout))
	    (error 'www-utils:connection-error)))
      (when socket-handle
	(comm::close-socket socket-handle)))))

(defun ftp-open-stream (host port &key (connect :active) (mode :text) timeout)
  (ecase connect
    (:active
     (ipc::%open-ftp-stream-to-host host port mode timeout))
    (:passive
     (unless (http::host-eq (http::parse-host host) (http::local-host))
       (error "Cannot open an FTP stream in passive mode: HOST, ~S, is not the local host." host))
     (make-passive-socket port timeout t))))
