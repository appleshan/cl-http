;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-
;;;
;;; Copyright John C. Mallery,  1995-96, 2001, 2003.
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;; W4 WEB WALKER HTTP CLIENT SUPPORT 
;;;
;;;------------------------------------------------------------------- 
;;;
;;; UTILITIES
;;;

(in-package :http) 

;;;------------------------------------------------------------------- 
;;;
;;; METHODS FOR WEB WALKER
;;;

(defun %get-url-headers (url headers report-stream authorization)
  (handling-redirects (url)
    (with-http-request (url :head 
                            :request-headers (compute-standard-request-headers 
                                               url :authorization authorization :header-plist headers
					       :user-agent (if (getf headers :user-agent) nil *server-version*)))
      (let ((status (client-status client))
            (response-headers (client-response-headers client))
            redirection)
        (case status
          ((200 203 204 205 206))
          ((301 302)
           (let ((alternate-urls (mapcar #'url:intern-url (ensure-list (or (get-header :location response-headers) 
                                                                           (get-header :content-location response-headers))))))
             (push alternate-urls redirection)
             (signal (ecase status
                       (301 'document-moved-permanently)
                       (302 'document-moved-temporarily))
                     :new-urls alternate-urls :version http-version)))
          ((402 403 405 406 407))
          (404
            (when *debug-client*
              (fresh-line report-stream)
              (%write-common-logfile-entry (host-string url) (concatenate 'string (url:name-string url) " HEAD")
                                           status  0 "-"  *log-times-in-gmt* report-stream)))
          ;; do something about authentication -- JCMa 12/10/1996.
          (401 (destructuring-bind (&optional authentication-method . realm) (get-header :WWW-Authenticate response-headers)
                 (declare (ignore authentication-method realm))
                 nil))
          ((nil) (setq status 408))             ; didn't return a status code
          ((408 500 501 502 503 504 505))
          (t (client-signal-http-code url status :head :headers response-headers :reason (client-reason client) :version http-version)))
        ;; return values for walker
        (values (durable-response-headers client) status redirection http-version)))))

(define-generic get-url-headers (url &optional headers report-stream authorization)
  (declare (values resource-headers status-code redirection http-version))
  (:documentation "Returns the HTTP headers for URL by invoking the HEAD method."))

(defmethod get-url-headers ((url url:http-url) &optional headers (report-stream *standard-output*) authorization)
  (%get-url-headers url headers report-stream authorization))

(defmethod get-url-headers ((string string) &optional headers (report-stream *standard-output*) authorization)
  (get-url-headers (url:intern-url string) headers report-stream authorization))

(export 'get-url-headers :http)


;;;------------------------------------------------------------------- 
;;;
;;; WEB WALKER RESOURCES
;;;

(defparameter *web-walker-cache-default-array-size* 1000
  "The default size the cache arrays when nospecific size is known.")

(defconstant *web-walker-cache-array-size-hysterisis* .10
  "The maximum fraction of space that can be wasted when allocating a cache array.")

(defun make-web-walker-cache-array (resource element-type size)
  (declare (ignore resource))
  (make-array size :element-type element-type :adjustable t :fill-pointer 0))

(defun match-web-walker-cache-array-p (resource array element-type size)
  (declare (ignore resource))
  (and (equal element-type (array-element-type array))
       (let ((array-size (array-total-size array)))
	 (declare (fixnum size array-size))
	 (and (<= size array-size)		;fit inside array?
	      ;; within hysterisis -- maintain resource distribution & frequency
	      (< (float (/ size array-size)) *web-walker-cache-array-size-hysterisis*)))))

;; this must allocate and deallocate rapidly
(defresource web-walker-cache-array (element-type size)
  :constructor make-web-walker-cache-array
  :matcher match-web-walker-cache-array-p)

(define clear-web-walker-cache-array-resource ()
  "Clears the cache arrays usd by the web walker.
Frees all existing array for garbage collection."
  (clear-resource 'web-walker-cache-array))

(defun allocate-web-walker-cache-array (element-type &optional size)
  (allocate-resource 'web-walker-cache-array element-type (or size *web-walker-cache-default-array-size*)))

(defun deallocate-web-walker-cache-array (array)
  (setf (fill-pointer array) 0)
  (deallocate-resource 'web-walker-cache-array array))

(defmacro without-web-walker-cache-resource-exceptional-leaks ((var) &body body)
  "Intercepts exceptional conditions and deallocates the resource bound to VAR."
  (declare (values var))
  `(let ((,var nil))
     (flet ((maybe-deallocate-cache-resource (condition)
	      (declare (ignore condition))
	      (when ,var
		(deallocate-web-walker-cache-array ,var)
		(setq ,var nil))))
       (declare (dynamic-extent #'maybe-deallocate-cache-resource))
       (handler-bind
	 ((condition #'maybe-deallocate-cache-resource))
	 ,@body))
     ,var))


;;;------------------------------------------------------------------- 
;;;
;;; GETTING URL HEADERS AND BODY
;;;

(define-generic chunked-input-capture (stream copy-mode headers)
  (declare (values vector))
  (:documentation "Captures chunked input from an HTTP stream.
Ports may specialize this method to improve performance."))

(defmethod chunked-input-capture (stream copy-mode headers)
  (declare (values vector))
  (without-web-walker-cache-resource-exceptional-leaks (vector)
    (with-chunked-transfer-decoding (stream :headers headers)
      (ecase copy-mode
	((:text :crlf)
	 (setq vector (allocate-web-walker-cache-array *standard-character-type*))
	 (crlf-stream-copy-into-string stream nil 0 vector))
	(:binary
	  (with-binary-stream (stream :input)
	    (setq vector (allocate-web-walker-cache-array '(unsigned-byte 8)))
	    (binary-stream-copy-into-8-bit-array stream nil 0 vector)))))))

(defun %get-url-headers-and-body (url headers report-stream authorization)
  (flet ((standard-capture (stream copy-mode length)
	   (without-web-walker-cache-resource-exceptional-leaks (vector)
	     (ecase copy-mode
	       ((:text :crlf)
		(setq vector (allocate-web-walker-cache-array *standard-character-type* length))
		(crlf-stream-copy-into-string stream length 0 vector))
	       (:binary
                (with-binary-stream (stream :input)
                  (setq vector (allocate-web-walker-cache-array '(unsigned-byte 8) length))
                  (binary-stream-copy-into-8-bit-array stream length 0 vector)))))))
    (declare (inline standard-capture))
    (handling-redirects (url)
      (handling-authentication (authorization proxy-authorization :client-authentication authorization)
        (with-http-request (url :get 
                                :request-headers (compute-standard-request-headers
                                                  url :authorization authorization :header-plist headers
                                                  :user-agent (if (getf headers :user-agent) nil *server-version*)))
          (let ((status (client-status client))
                (response-headers (client-response-headers client))
                response-body redirection)
            (case status
              ((200 205 206)
               (let* ((content-type (get-header :content-type response-headers))
                      (copy-mode (mime-content-type-copy-mode content-type))
                      (content-length (get-header :content-length response-headers)))
                 (setq response-body (cond ((or content-length (member http-version '(:http/1.0 :http/0.9)))
                                            (standard-capture remote-stream copy-mode content-length))
                                           (t (let ((transfer-encoding (get-header :transfer-encoding response-headers)))
                                                (case transfer-encoding
                                                  (:chunked (chunked-input-capture remote-stream copy-mode response-headers))
                                                  ((nil)
                                                   (error 'bad-syntax-provided :url url :method :get
                                                          :format-string "No content length header was provided."))
                                                  (t (error 'server-not-implemented :close-connection t :url url :method :get
                                                            :format-string "The HTTP transfer decoding, ~A, is not implemented."
                                                            :format-args (list transfer-encoding))))))))))
              ((201 202 203 204))
              ((300 402 403 405 406 407 415))
              ((301 302)
               (let ((alternate-urls (mapcar #'url:intern-url (ensure-list (or (get-header :location response-headers) 
                                                                               (get-header :content-location response-headers))))))
                 (flush-input-entity remote-stream response-headers http-version)
                 (push alternate-urls redirection)
                 (signal (ecase status
                           (301 'document-moved-permanently)
                           (302 'document-moved-temporarily))
                         :new-urls alternate-urls :version http-version)))
              ;; do something about authentication -- JCMa 12/10/1996.
              (401 (destructuring-bind (&optional authentication-method . realm) (get-header :WWW-Authenticate response-headers)
                     (flush-input-entity remote-stream response-headers http-version)
                     (error (ecase authentication-method
                              (:basic 'recoverable-unauthorized-client-access)
                              (:digest (if (getf (cdr realm) :stale) 'client-access-with-stale-nonce 'recoverable-unauthorized-client-access)))
                            :url url
                            :method (client-method client)
                            :authentication-method authentication-method
                            :authentication-realm realm)))
              (404
               (when *debug-client*
                 (fresh-line report-stream)
                 (%write-common-logfile-entry (host-string url) (concatenate 'string (url:name-string url) " GET")
                                              status 0 "-" *log-times-in-gmt* report-stream)))
              ((nil) (setq status 408))		; didn't return a status code
              ((408 411 414 500 501 502 503 504 505))
              (t (client-signal-http-code url status :get :headers response-headers :reason (client-reason client) :version http-version)))
            (values response-body (durable-response-headers client) status redirection http-version url)))))))

(define-generic get-url-headers-and-body (url &optional headers report-stream authentication)
  (declare (values resource-content resource-headers status-code redirection http-version url))
  (:documentation "Returns the body of URL as a 1-d array and the headers as an header alist."))

(defmethod get-url-headers-and-body ((url url:http-url) &optional headers (report-stream *standard-output*) authentication)
  (%get-url-headers-and-body url headers report-stream authentication))

(export 'get-url-headers-and-body :http)


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

#|
(defmethod write-binary-url ((string string) pathname &key headers (stream *standard-output*))
  (write-binary-url (url:intern-url string) (pathname pathname) :headers headers :stream stream)) 

(defmethod write-binary-url ((url url:http-url) (pathname pathname) &key headers (stream *standard-output*))
  (labels ((handle-response (remote-stream)
             (multiple-value-bind (code reason version)
                 (parse-reply (read-reply-line remote-stream))
               (%handle-response-for-client-get (url code reason remote-stream version headers)
                                                (print-headers stream)
                                                (terpri stream)
                                                (with-open-file (file pathname :direction :output :element-type '(unsigned-byte 8)
                                                                      :if-does-not-exist :create :if-exists :supersede)
                                                  (www-utils:with-binary-stream (remote-stream :input)
                                                    (stream-copy-until-eof remote-stream file)))))))
    (declare (dynamic-extent #'handle-response))
    (let* ((port (url:host-port url))
           (outgoing-headers `(,@*standard-client-get-headers*
                               ,.headers)))
      (declare (dynamic-extent outgoing-headers))
      (with-client-line-buffer ()
        (handler-case-if (not *debug-client*)
           (handling-redirects (url)
             (invoke-http-service-on-host 
               (url:host-object url)
               port
               :get :url url :continuation #'handle-response :headers outgoing-headers))
          (client-condition (cond) (www-utils:report-condition cond stream)))))))
|#
