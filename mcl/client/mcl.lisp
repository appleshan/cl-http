;;; -*- Syntax: ansi-common-lisp; Base: 10; Package: http; Mode: LISP -*-

;;; (C) Copyright 1995-2001, 2006, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; MAC HTTP CLIENT NETWORK INTERFACE
;;;

(in-package :http)

(declaim (inline deallocate-client-http-stream))

(defun deallocate-client-http-stream (stream)
  (resources:deallocate-resource 'http-stream stream))

(defun open-http-stream-to-host (host port &optional process)
   (declare (values stream))
   (resources:allocate-resource 'http-stream host port (ceiling (the fixnum *client-timeout*) 60.) process))

;;;------------------------------------------------------------------- 
;;;
;;; 
;;; 

(defclass username+password-dialog (ccl:dialog) ())

(defclass exit-box 
   (ccl:button-dialog-item)
   ((selected-p :initform nil :accessor exit-box-selected-p)))

(defclass exit-box-cancel (exit-box) ())

(defclass exit-box-ok (exit-box) ())

(defclass text-input-item (ccl:editable-text-dialog-item) ())

(defclass password-input-item (ccl:password-text-dialog-item text-input-item) ()) 

(defmethod select-item ((exit-box exit-box))
  (setf (exit-box-selected-p exit-box) t))

(defmethod exit-p ((dialog username+password-dialog))
   (declare (values exit-p abort-p))
   (ccl:do-subviews (dialog-item dialog 'exit-box)
      (when (exit-box-selected-p dialog-item)
          (etypecase dialog-item
             (exit-box-cancel (return :abort))
             (exit-box-ok (return t))))))

(defmethod ccl:view-named (nickname (dialog username+password-dialog))
   (ccl:do-subviews (dialog-item dialog 'text-input-item)
      (when (eq (ccl:view-nick-name dialog-item) nickname)
          (return-from ccl:view-named dialog-item)))
   (error "No dialog item corresponds to ~S." nickname))

(defmethod user-name ((dialog username+password-dialog))
  (ccl:dialog-item-text (ccl:view-named 'user-name dialog)))

(defmethod password ((dialog username+password-dialog))
  (ccl:dialog-item-text (ccl:view-named 'password dialog)))

(defmethod get-username+password ((window username+password-dialog))
   (loop with  exit-p
            initially (ccl:window-select window)
	    doing (multiple-value-setq (exit-p)
                         (exit-p window))
	    until exit-p
	    finally (case exit-p
                          (:abort (ccl:window-close window)
                                      (return  (values nil nil t)))
		          (t (let ((username (user-name window))
                                      (password (password window)))
		                (ccl:window-close window)
		                (return (values username password))))))) 

(defun allocate-username+pw-dialog (url-string realm method proxy-p default-user-name default-password)
   (flet ((proxy-string (url-string)
	       (let ((proxy (get-client-proxy url-string)))
                  (if proxy
                     (format nil "~A:~D" (proxy-domain-name proxy) (proxy-port proxy))
		     (format nil "Client associates no proxy with ~S." url-string))))
	    (authentication-string (method)
	       (case method
	          (:basic "Basic Authentication")
	          (:digest "Digest Authentication")
	          (t (or (get method 'authentication-string)
                            (setf (get method 'authentication-string) (format nil "~:(~A~) Authentication" method)))))))
      (declare (inline proxy-string))
      (let* ((window-width 300)
	        (uri-size (ccl:make-point (- window-width 6) 16))
	        (indent 4)
	        (label-size #@(80 16))
	        (button-size #+ccl-5.0 #.(if (ccl::osx-p) #@(100 20) #@(90 20))
                                    #-ccl-5.0 #@(90 20))
	        (input-size #@(140 16))
	        (realm-size  #@(140 16))
	        (y-pos 0)
	        (v-seperation 20)
	        (v-box-seperation 30)
	        (h-2nd-item 80)
	        (h-input-box 86))
         (declare (fixnum window-width))
         (macrolet ((point (x &optional delta-y )
		             `(ccl:make-point ,x ,(if delta-y `(incf y-pos ,delta-y) 'y-pos))))
	     (make-instance 'username+password-dialog 
                 :window-type :movable-dialog
                 :window-title "User Name & Password"
                 :back-color ccl::*tool-back-color*
                 ;;:view-position '(:top 60)
                 :view-size (ccl:make-point window-width 200)
                 :close-box-p nil
                 :color-p t
                 :view-font '("chicago" 12 :srcor :plain)
                 :view-subviews
                 `(,(ccl:make-dialog-item 'ccl:static-text-dialog-item (point 80 4) (ccl:make-point window-width 20)
						       (if proxy-p "Connect to Proxy" "Connect to Server")
						       'NIL :view-font '("Chicago" 14 :SRCOR :PLAIN))
                    ,@(if proxy-p
                           `(,(ccl:make-dialog-item 'ccl:static-text-dialog-item (point indent v-box-seperation) label-size"Proxy:" 'nil )
                              ,(ccl:make-dialog-item 'ccl:static-text-dialog-item (point h-2nd-item) uri-size (proxy-string url-string) 'nil ))
                           `(,(ccl:make-dialog-item 'ccl:static-text-dialog-item  (point indent v-box-seperation) label-size "URI:" 'nil )
                              ,(ccl:make-dialog-item 'ccl:static-text-dialog-item (point h-2nd-item) uri-size url-string 'nil )))
                    ,(ccl:make-dialog-item 'ccl:static-text-dialog-item (point indent v-box-seperation) label-size "User Name: " 'nil)
                    ,(ccl:make-dialog-item 'text-input-item (point h-input-box) input-size default-user-name 'nil  :view-nick-name 'user-name :allow-returns nil)
                    ,(ccl:make-dialog-item 'ccl:static-text-dialog-item (point indent v-box-seperation) label-size "Password: " 'nil)
                    ,(ccl:make-dialog-item 'password-input-item (point h-input-box) input-size default-password 'nil :view-nick-name 'password :allow-returns nil)
                    ,(ccl:make-dialog-item 'ccl:static-text-dialog-item (point indent v-box-seperation) label-size"Realm:" 'nil)
                    ,(ccl:make-dialog-item 'ccl:static-text-dialog-item (point h-2nd-item) Realm-size Realm 'nil)
                    ,(ccl:make-dialog-item 'ccl:static-text-dialog-item (point indent v-seperation) label-size "Scheme:" 'nil)
                    ,(ccl:make-dialog-item 'ccl:static-text-dialog-item (point h-2nd-item) Realm-size (authentication-string method) 'nil)
                    ,(ccl:make-dialog-item 'exit-box-cancel (point 45 v-box-seperation) button-size "Cancel" #'select-item :default-button nil)
                    ,(ccl:make-dialog-item 'exit-box-ok (point 165) button-size "Authenticate" #'select-item :default-button t)))))))

#| (%get-user-name+pw "http://foo.baz.bar/foo.html" "Proxy-realm" :basic nil) |#

(defparameter *default-client-user-id* nil)

(define default-client-user-id ()
  (or *default-client-user-id* ""))

(defun set-default-client-user-id (user-id)
   (when user-id
       (setq *default-client-user-id* user-id)))

(defparameter *default-client-pw* nil)

(define default-client-pw ()
  (or *default-client-pw* ""))

(defun set-default-client-pw (pw)
  (when pw
    (setq *default-client-pw* pw)))

(define-macro with-client-user-id+pw ((user-name password) &body body)
  "Binds the default the default user name and password within BODY."
  `(let ((*default-client-user-id* ,user-name)
	 (*default-client-pw* ,password))
     ,@body))

(export '(with-client-user-id+pw) :http)

;; this is the function used by the portable client code.
(defun %get-user-name+pw (url-string  realm method  proxy-p &optional (stream *standard-output*))
   (declare (ignore stream)
                 (values user-id pw abort-p))
   (let* ((user-name (default-client-user-id))
	     (password (default-client-pw))
	     (window (allocate-username+pw-dialog url-string realm method proxy-p user-name password))) 
      (multiple-value-bind (uid pw abort-p)
                                      (get-username+password window)
          (unless abort-p
	     (cond-every
               (uid (set-default-client-user-id uid))
	       (pw (set-default-client-pw pw))))
          (values uid pw abort-p))))

#|(defun %get-user-name+pw (url-string &optional stream)
   (declare (values user-id pw))
   (let (user-id pw)
      (dw:accepting-values 
        (stream :own-window t
                     :label (format nil "~'bPassword Information~"))
        (dw:redisplayable-format stream "URL: ~v~A~" '(nil nil :small) string)
        (terpri stream)
        (setq user-id (scl:accept 'scl:string :prompt "User" :default user-id :stream stream))
        (setq pw (scl:accept 'scl:string :prompt "Password" :default pw :stream stream)))
      (values user-id pw)))|#

(defun open-tcp-stream (host port &optional (timeout *client-timeout*) process)
  (declare (fixnum timeout)
           (values stream))
  (resources:allocate-resource 'http-stream host port (ceiling timeout 60.) process))

(defmacro with-open-tcp-stream ((stream host port &key (timeout '*client-timeout*) process) &body body)
  "Opens a TCP stream to host port with timeout, TIMEOUT."
  `(unwind-protect
       (let ((,stream (open-tcp-stream ,host ,port ,timeout ,process)))
	 (multiple-value-prog1
             (progn . ,body)
	   (when ,stream
	     (resources:deallocate-resource 'http-stream ,stream))))))

(define-macro with-automatic-login ((host user-id user-pw) &body body)
  "Supplies userid and PW to ensure successul FTP login to host with BODY."
  `(progn (notify-log-window "~&(WITH-AUTOMATIC-LOGIN (~S ~S ~S) - Not available on MAC"
                             ,host ,user-id ,user-pw)
          ,@body))

(defmethod file-url-directory-info (directory &optional (user-id "anonymous") (user-pw (user-mail-address)))
  "Returns a list of pathname spec for directory just like DIRECTORY-INFO.
If a network error is encountered, this returns NIL."
  (let* ((path (pathname directory))
         (host (pathname-host path)))
    (handler-case 
      (with-automatic-login (host user-id user-pw)
        ;; ansi CL directory fails due to :fast option  3/13/94 -- JCMa.
        (directory-info directory))
      ;; handle remote connection problems, including dead host, refused connection.
      (ccl:remote-network-error () nil))))

(defmethod file-url-copy-file (from-pathname to-stream &key (element-type 'character)
                                             (user-id "anonymous") (user-pw (user-mail-address)))
  "Copies the content of FROM-PATHNAME to TO-STREAM. 
If a network error is encountered, this returns NIL, otherwise T.
ELEMENT-TYPE is the ANSI file openning argument."
  (let ((host (pathname-host from-pathname)))
    (handler-case 
      (with-automatic-login (host user-id user-pw)
        (with-open-file (ftp-stream from-pathname :direction :input :element-type element-type)
          (http::stream-copy-until-eof ftp-stream to-stream)
          (values t)))
      ;; handle remote connection problems, including dead host, refused connection.
      (ccl:remote-network-error () nil))))

(defmethod file-url-copy-file-to-http-stream (from-pathname http-stream &key content-type url additional-headers
                                                            (user-id "anonymous") (user-pw (server-mail-address)))
  "Copies the content of FROM-PATHNAME to HTTP-STREAM."
  (declare (values success-p))
  (flet ((handle-invalid-ftp-user-id-and-password (&rest ignore)
           (declare (ignore ignore))
           (signal 'http::client-unauthorized-ftp-access :url url :method :get
                   :authentication-realm "FTP Server" :authentication-method :basic)))
    (declare (dynamic-extent #'handle-invalid-ftp-user-id-and-password))
    (let* ((host (pathname-host from-pathname))
           (copy-mode (or (url::%content-type-copy-mode content-type nil) :binary))
           (element-type (ecase copy-mode
                           (:text 'character)
                           ((:binary :crlf) '(unsigned-byte 8)))))
      (with-automatic-login (host user-id user-pw)
        (with-open-file (ftp-stream from-pathname :direction :input :element-type element-type)
          (http:with-successful-response (http-stream (case content-type ((:unknown nil) :text) (t content-type))
                                                      :location url :additional-headers additional-headers)
            (case copy-mode
              (:text
               (with-text-stream (http-stream :output)
                 (http::stream-copy-until-eof ftp-stream http-stream :text)))
              ((:binary :crlf)
               (with-binary-stream (http-stream :output)
                 (http::stream-copy-until-eof ftp-stream http-stream :binary))))
            (values t)))))))


