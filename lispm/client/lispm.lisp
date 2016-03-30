;;; -*- Syntax: ansi-common-lisp; Base: 10; Package: www-utils; Mode: LISP -*-

;;; (C) Copyright 1994, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; LISPM CLIENT NETWORK INTERFACE
;;;

(mapcar #'(lambda (x)
	    (export (intern x :www-utils) :www-utils))
	'("OPEN-HTTP-STREAM-TO-HOST" "FTP-DIRECTORY-INFO" "FTP-COPY-FILE" "FTP-COPY-FILE-TO-HTTP-STREAM"))

(defun ensure-services (host &rest services)
  (let* ((host-object (neti:parse-host host))
	 (host-services (scl:send host-object :get :service))
	 (new-services (loop for service in services
			     unless (member service host-services :test #'equal)
			       collect service)))
    (when new-services
      (let* ((uninterned (scl:send host-object :uninterned-p))
	     (primary-name (scl:send host-object :primary-name))
	     (primary-namespace (and (not uninterned) (scl:send primary-name :namespace))))
	(scl:send host-object :putprop (append host-services new-services) :service)
	(unless uninterned
	  ;; Don't use update-object-permanently on uninterned hosts as it will
	  ;; intern the host which can cause all sorts of problems later on ...
	  (neti:update-object-permanently :host primary-namespace primary-name
					  (scl:send host-object :property-list)
					  t))))
    host-object))

(net:define-protocol :http (:http :tcp)
  (:invoke (access-path)
    (net:get-connection-for-service access-path :translation :modal :characters t)))

(declaim (inline deallocate-client-http-stream))

;; a no-op on the lisp machine but required for platforms such as the mac.
;; 8/13/96 -- JCMa.
(defun deallocate-client-http-stream (stream)
  (declare (ignore stream)))

(declaim (special http::*client-timeout*))

(defun open-http-stream-to-host (host port)
  (declare (values stream))
  (let ((host-object (ensure-services host '(:http :tcp :http))))
    (with-tcp-port-for-protocol (:http port)
      (let ((tcp:*tcp-connect-timeout* http::*client-timeout*))
	(neti::invoke-service-on-host :http host-object)))))

;;;------------------------------------------------------------------- 
;;;
;;; LISPM CLIENT FTP INTERFACE
;;;

(defun host-access-path-for-host-p (access-path host)
  (let ((ahost (scl:send access-path :host))
        (rhost (parse-host host t)))
    (host-eq ahost rhost)))

(defvar *standard-get-user-id-and-password* #'fs:get-user-id-and-password)
(defvar *nfs-authentication-function* #'rpc:authentication-initialize)

(define-macro with-automatic-login ((host user-id user-pw) &body body)
  "Supplies userid and PW to ensure successul FTP login to host with BODY."
  `(labels ((standard-get-user-id-and-password (access-path host-user-id host-password condition)
              (funcall *standard-get-user-id-and-password* access-path host-user-id host-password condition))
            (nfs-authenticate (authentication-mixin host-user-id host-password)
              (funcall *nfs-authentication-function* authentication-mixin
                       (or host-user-id ,user-id)
                       (or host-password ,user-pw)))
            (automatic-get-user-id-and-password
               (access-path host-user-id host-password condition)
              (cond ((and (null host-user-id)
                          (null host-password)
                          (host-access-path-for-host-p access-path ,host))
                     (values ,user-id ,user-pw))
                    (t (standard-get-user-id-and-password access-path host-user-id host-password condition)))))
     (scl:letf ((#'fs:get-user-id-and-password #'automatic-get-user-id-and-password)
                (#'rpc:authentication-initialize #'nfs-authenticate))
       ,@body)))

(define ftp-copy-file (from-pathname to-stream &key (element-type 'character) (port 21)
                                     (user-id "anonymous") (user-pw (server-mail-address)))
  "Copies the content of FROM-PATHNAME to TO-STREAM. 
If a network error is encountered, this returns NIL, otherwise T.
ELEMENT-TYPE is the ANSI file openning argument."
  (declare (values success-p))
  (let ((host (pathname-host from-pathname)))
    (ensure-services host '(:file :tcp :tcp-ftp))
    (with-tcp-port-for-protocol (:ftp port)
      (let ((tcp:*tcp-connect-timeout* http::http::*client-timeout*))
	(with-automatic-login (host user-id user-pw)
	  (with-open-file (ftp-stream from-pathname :direction :input :element-type element-type)
	    (http::stream-copy-until-eof ftp-stream to-stream (case element-type
								(character :text)
								(t :binary)))
	    (values t)))))))

(define ftp-directory-info (directory &optional (port 21) url (user-id "anonymous") (user-pw (server-mail-address)))
  "Returns a list of pathname spec for directory just like DIRECTORY-INFO.
If a network error is encountered, this returns NIL."
  (declare (values directory-listing directory-exists-p))
  (flet ((handle-invalid-ftp-user-id-and-password (&rest ignore)
	   (declare (ignore ignore))
	   (signal 'http::client-unauthorized-ftp-access :url url :method :get
		   :authentication-realm "FTP Server" :authentication-method :basic)))
    (declare (dynamic-extent #'handle-invalid-ftp-user-id-and-password))
    (let* ((path (pathname directory))
	   (host (pathname-host path))
	   (*standard-get-user-id-and-password* #'handle-invalid-ftp-user-id-and-password))
      (ensure-services host '(:file :tcp :tcp-ftp))
      (with-tcp-port-for-protocol (:ftp port)
	(let ((tcp:*tcp-connect-timeout* http::http::*client-timeout*))
	  (with-automatic-login (host user-id user-pw)
	    ;; ansi CL directory fails due to :fast option  3/13/94 -- JCMa.
	    (let ((path (make-pathname :defaults directory))
		  (listing (cdr (fs:directory-list (make-pathname :defaults directory) :sorted))))
	      (if listing
		  (values listing t)
		  (values nil (ignore-errors (open path :direction :probe-directory :if-does-not-exist nil)))))))))))

(define ftp-copy-file-to-http-stream (from-pathname http-stream &key (port 21) data-type
						    url additional-headers
						    (user-id "anonymous") (user-pw (server-mail-address)))
  "Copies the content of FROM-PATHNAME to HTTP-STREAM."
  (declare (values success-p))
  (flet ((handle-invalid-ftp-user-id-and-password (&rest ignore)
	   (declare (ignore ignore))
	   (signal 'http::client-unauthorized-ftp-access :url url :method :get
		   :authentication-realm "FTP Server" :authentication-method :basic)))
    (declare (dynamic-extent #'handle-invalid-ftp-user-id-and-password))
    (let* ((host (pathname-host from-pathname))
	   (copy-mode (or (url::%content-type-copy-mode data-type nil) :binary))
	   (element-type (ecase copy-mode
			   (:text 'character)
			   ((:binary :crlf) '(unsigned-byte 8))))
	   (*standard-get-user-id-and-password* #'handle-invalid-ftp-user-id-and-password))
      (ensure-services host '(:file :tcp :tcp-ftp))
      (with-tcp-port-for-protocol (:ftp port)
	(let ((tcp:*tcp-connect-timeout* http::http::*client-timeout*))
	  (with-automatic-login (host user-id user-pw)
	    (with-open-file (ftp-stream from-pathname :direction :input :element-type element-type)
	      (http:with-successful-response (http-stream (case data-type ((:unknown nil) :text) (t data-type))
							  :location url :additional-headers additional-headers)
		(case copy-mode
		  (:text
		    (with-text-stream (http-stream :output)
		      (http::stream-copy-until-eof ftp-stream http-stream :text)))
		  ((:binary :crlf)
		   (with-binary-stream (http-stream :output)
		     (http::stream-copy-until-eof ftp-stream http-stream :binary))))
		(values t)))))))))