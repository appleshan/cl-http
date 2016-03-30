;;; -*- Mode: Lisp; Package: CCL; Syntax: Ansi-Common-Lisp; -*-
;;;
;;; LISPWORKS STREAM SUPPORT FOR FTP CLIENT
;;; Adapted from the LispWorks implementation 7/4/2006
;;;
;;; Copyright (C) 2006, John C. Mallery. 
;;; All rights reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; FTP SOCKETS AND STREAMS
;;;

(in-package :ccl)


(defclass socket ()
  ((stream :type tcp-stream :initarg :stream  :reader socket-stream)
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
  (with-slots (stream) socket
    (values (stream-remote-host stream)
            (stream-local-port stream))))

(defgeneric stream-input-available-p (stream-or-socket)
  (:documentation "Non-nil if data is waiting to be read from STREAM-OR-SOCKET ."))

(defmethod stream-input-available-p ((socket socket))
  (stream-input-available-p (socket-stream socket)))

(defmethod stream-input-available-p ((stream ccl::basic-tcp-stream))
  (and (www-utils:live-connection-p stream)
       (listen stream)))

(defmethod ftp-close ((socket socket) &key abort)
  (ftp-close (socket-stream socket) :abort abort))

(defmethod ftp-close :after ((stream ccl:modal-ascii-or-binary-tcp-stream) &key abort)
  (declare (ignore abort))
  (resources:deallocate-resource 'http::http-stream stream))

(defun make-passive-socket (local-port &key element-type read-timeout no-error-p backlog (protocol :ftp))
  (declare (ignore backlog))
  (flet ((create-stream-for-service (port timeout &aux stream)
           (declare (fixnum timeout))
           (unwind-protect
             (handler-case
               (prog1 (setq stream (resources:allocate-resource 'http-stream nil port (ceiling timeout 60.) ccl::*current-process*))
                 (case (stream-connection-state-name stream)
                   ((:listen:established :syn-received :syn-sent)       ; port is good
                    (cond ((member element-type '(character base-char))
                           (ascii-input-mode stream)
                           (ascii-output-mode stream))
                          (t (binary-input-mode stream)
                             (binary-output-mode stream)))
                    ;; don't deallocate on return
                    (setq stream nil))
                   (t nil)))            ; try another port
               (www-utils:network-error () nil))
             (when stream (resources:deallocate-resource 'http::http-stream stream)))))
    (let ((stream (create-stream-for-service local-port read-timeout)))
      (cond (stream
             (make-instance 'passive-socket
               :stream stream
               :port local-port
               :element-type element-type
               :read-timeout read-timeout
               :local-protocol protocol))
            (no-error-p nil)
            (t (error 'www-utils:connection-error))))))

(defmethod accept-connection ((passive-socket passive-socket))
  (let ((stream (socket-stream passive-socket))
        (read-timeout (socket-read-timeout passive-socket)))
    (http::with-stream-timeout (stream read-timeout :error-p t)
      (when (stream-input-available-p stream)
        (let ((foreign-host (stream-remote-host stream)))
          (cond (foreign-host stream)
                (t (error 'www-utils:network-error))))))))

(defun %open-ftp-stream-to-host (host port mode timeout)
  (declare (fixnum timeout)
           (values stream))
  (let ((stream nil))
    (unwind-protect 
      (prog1 (setq stream (resources:allocate-resource 'http::http-stream host port (ceiling timeout 60.)))
        (unless (http::live-connection-p stream)
          (error 'www-utils:connection-error))
        (ecase mode
          (:text
           (ascii-input-mode stream)
           (ascii-output-mode stream))
          (:binary
           (binary-input-mode stream)
           (binary-output-mode stream)))
        (setq stream nil))
      (when stream
        (resources:deallocate-resource 'http::http-stream stream)))))
           
(defun ftp-open-stream (host port &key (connect :active) (mode :text) timeout)
  (ecase connect
    (:active
     (%open-ftp-stream-to-host host port mode timeout))
    (:passive
     (unless (http::host-eq (http::parse-host host) (http::local-host))
       (error "Cannot open an FTP stream in passive mode: HOST, ~S, is not the local host." host))
     (make-passive-socket port timeout t))))
