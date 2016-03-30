;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10 -*-
;;;
;;; LispWorks client interface to TCP/IP streams
;;;
;;; Copyright (C) 1995-2003 Xanalys LLC.  All rights reserved.
;;;
;;; Enhancements Copyright (C) 2006, John C. Mallery. All rights reserved."
;;;
(in-package :http)

(declaim (inline deallocate-client-http-stream))

;; noop present for implementations that resource their TCP streams, e.g. the
;; mac.
(defun deallocate-client-http-stream (stream)
  (declare (ignore stream)))

(defun open-http-stream-to-host (host port)
  (declare (optimize (speed 3))
           (values stream))
  (%open-http-stream-to-host host port (floor (/ *client-timeout* 60.))))

#+CL-HTTP-SSL
(defun get-client-ssl-ctx (ssl-version)
  (let ((ssl-ctx (www-utils::%convert-ssl-versions-for-lw ssl-version)))
    ssl-ctx))

#+CL-HTTP-SSL
(defun open-ssl-stream-to-host (host port &optional ssl-ctx)
  "Opens and returns an SSL stream to HOST on PORT.
SSL-CTX can be an SSL CTX or one of the following keywords:

     :SSL-DEFAULT - Use the current default SSL versions (2 or 3)
     :SSL-2-OR-3  - Use only SSL version 2 or 3 
     :SSL-3       - Use only SSL version 3
     :SSL-2       - Use only SSL version 2
     :TLS-1       - Use only TLS version 1"
  (declare (values stream))
  (flet ((get-client-ssl-ctx (ssl-ctx)
           (etypecase ssl-ctx
             #-CL-HTTP-SSL-CLIENT
             (null nil)
             #+CL-HTTP-SSL-CLIENT
             (null
              (let ((ssl-control *client-ssl-control*))
                (if ssl-control
                  (ssl-ctx ssl-control)
                  #.(www-utils::%convert-ssl-versions-for-lw :ssl-default))))
             (keyword
              (www-utils::%convert-ssl-versions-for-lw ssl-ctx))
             ((satisfies fli:pointerp) ssl-ctx))))
    (declare (inline get-client-ssl-ctx))
    (let ((ssl-ctx (get-client-ssl-ctx ssl-ctx)))
      (%open-http-stream-to-host host port (floor (/ *client-timeout* 60.)) ssl-ctx))))

;; this is the function used by the portable client code.
(defun %get-user-name+pw (url-string realm method proxy-p &optional (stream *query-io*))
  (declare (values user-id pw abort-p))
  (let* ((user-name (default-client-user-id))
	 (password (default-client-pw)))
    (multiple-value-bind (uid pw abort-p)
	(ask-user-name+pw url-string realm method proxy-p user-name password stream)
      (unless abort-p
	(cond-every
	 (uid (set-default-client-user-id uid))
	 (pw (set-default-client-pw pw))))
      (values uid pw abort-p))))

(defun ask-user-name+pw (url-string realm method proxy-p name pwd stream)
  #+CAPI
  (when (capi:screens)
    (return-from ask-user-name+pw
      (gui-ask-user-name+pw url-string realm method proxy-p name pwd)))
  (let (nname
        npwd)
    (fresh-line stream)
    (ask-user-name+pw-title url-string proxy-p stream)
    (terpri stream)
    (format stream "Realm:     ~:(~A~)~%Scheme:    ~:(~A~) Authentication~%" realm method)
    (if name
        (format stream "~&User Name (default: ~a): " name)
      (format stream "~&User Name: "))
    (setq nname (read-line stream nil nil))
    (if (equal nname "")
        (setq nname name))
    (if pwd
        (format stream "~&Password (default: ~a): " (make-string (length pwd) :initial-element #\*) pwd)
      (format stream "~&Password: "))
    (setq npwd (read-line stream nil nil))
    (if (equal npwd "")
        (setq npwd pwd))
    (values (or nname name) (or npwd pwd) (not (and nname npwd)))))

(defun ask-user-name+pw-title (url-string proxy-p stream)
  (flet ((proxy-string (url-string)
           (let ((proxy (get-client-proxy url-string)))
             (if proxy
                 (format nil "~A:~D" (proxy-domain-name proxy) (proxy-port proxy))
               (format nil "unknown (URI ~A)" url-string)))))
    (let ((proxy-string (and proxy-p (proxy-string url-string))))
      (if proxy-string
	  (format stream "Connect to Proxy: ~A" proxy-string)
	(format stream "Connect to Server ~A" url-string)))))

(defparameter *default-client-user-id* nil)

(defun default-client-user-id ()
   (or *default-client-user-id* ""))

(defun set-default-client-user-id (user-id)
   (when user-id
       (setq *default-client-user-id* user-id)))

(defparameter *default-client-pw* nil)

(defun default-client-pw ()
   (or *default-client-pw* ""))

(defun set-default-client-pw (pw)
   (when pw
       (setq *default-client-pw* pw)))

#+CAPI
(capi:define-interface gui-ask-user-name+pw-dialog ()
  ()
  (:panes
   (realm-pane capi:title-pane)
   (scheme-pane capi:title-pane)
   (user-name-pane capi:text-input-pane)
   (password-pane capi:password-pane)
   )
  (:layouts
   (main-layout capi:grid-layout
                '("Realm:" realm-pane
                  "Scheme:" scheme-pane
                  "User Name:" user-name-pane
                  "Password:" password-pane)
                :columns 2))
  (:default-initargs
   :layout 'main-layout))

#+CAPI
(defmethod initialize-instance :after ((self gui-ask-user-name+pw-dialog)
                                       &key realm method
                                       name password)
  (with-slots (realm-pane scheme-pane user-name-pane password-pane) self
    (setf (capi:title-pane-text realm-pane)
          (string-capitalize realm))
    (setf (capi:title-pane-text scheme-pane)
          (format nil "~:(~A~) Authentication" method))
    (when name
      (setf (capi:text-input-pane-text user-name-pane) name))
    (when password
      (setf (capi:text-input-pane-text password-pane) password))))

#+CAPI
(defun gui-ask-user-name+pw-dialog-name (self)
  (with-slots (user-name-pane) self
    (capi:text-input-pane-text user-name-pane)))

#+CAPI
(defun gui-ask-user-name+pw-dialog-password (self)
  (with-slots (password-pane) self
    (capi:text-input-pane-text password-pane)))

#+CAPI
(defun gui-ask-user-name+pw (url-string realm method proxy-p name pwd)
  (let ((dialog (make-instance 'gui-ask-user-name+pw-dialog
                               :title (with-output-to-string (title)
                                        (ask-user-name+pw-title
                                         url-string proxy-p title))
                               :realm realm
                               :method method
                               :name name
                               :password pwd)))
    (if (capi:popup-confirmer dialog nil)
        (values (gui-ask-user-name+pw-dialog-name dialog)
                (gui-ask-user-name+pw-dialog-password dialog)
                nil)
      (values nil nil t))))

(defun open-tcp-stream (host port &optional (timeout *client-timeout*))
  "Opens a TCP stream to HOST on PORT with TIMEOUT."
  (declare (fixnum timeout)
           (values stream))
  (http::%open-http-stream-to-host host port (ceiling timeout 60.)))

(defmacro with-open-tcp-stream ((stream host port &key (timeout '*client-timeout*)) &body body)
  "Opens a TCP stream to HOST on PORT with TIMEOUT within the scope of BODY."
  `(unwind-protect
       (let ((,stream (open-tcp-stream,host ,port ,timeout)))
	 (multiple-value-prog1
             (progn . ,body)
	   (when ,stream
             (close ,stream :abort (not (live-connection-p ,stream))))))))