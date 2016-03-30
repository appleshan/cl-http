;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-

;;; (C) Copyright 1996-2001, 2003, 2005-2007, John C.Mallery.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; CL-HTTP S-EXPRESSION BROWSER
;;; 

(in-package :http)

;; Export here to preserve modularity in the primary package definitions.
(mapc #'(lambda (x) (export (intern x :http) :http))
      '("CAPTURE-RAW-URL"
	"DELETE-URL"
	"POST-URL"
	"PUT-URL"
	"SHOW-RAW-URL" 
	"SHOW-RAW-URL-TRACE"
	"SHOW-RAW-URL-OPTIONS"             
	"SHOW-URL"
	"SHOW-URL-HEADERS"))

;;;------------------------------------------------------------------- 
;;;
;;; GENERIC METHOD TO DISPLAY CONTENT ON A STREAM
;;; 

(defun %display-crlf-encoded-stream (remote-stream local-stream)
  "Primtive for displaying CRLF encoded text from REMOTE-STREAM to LOCAL-STREAM."
  (with-crlf-stream (remote-stream :input) 
    (with-header-values (content-length) *headers*
      (if content-length
	  (stream-decode-crlf-bytes remote-stream local-stream content-length)
	  (stream-decode-crlf-until-eof remote-stream local-stream)))))

(defgeneric display (url major-type minor-type remote-stream local-stream)
  (:documentation "The primary method for displaying URL on LOCAL-STREAM
according to the MIME content type MAJOR-TYPE/MINOR-TYPE, where the data is
read from REMOTE-STREAM."))

(defmethod display ((url url:url) major-type minor-type remote-stream local-stream)
  (declare (ignore remote-stream local-stream))
  (error 'unsupported-media-type :url url
         :format-string "No display method defined for the MIME content type ~A/~A."
         :format-args (list major-type minor-type)))

;; Handle buggy servers passing multiple content-type headers -- JCMa 7/16/2006
(defmethod display :around ((url url:http-url) (major-type cons) minor-type remote-stream local-stream)
  (declare (ignore minor-type))
  (destructuring-bind (major-type1 minor-type1 &key &allow-other-keys) major-type
    (call-next-method url major-type1 minor-type1 remote-stream local-stream)))

;; Takes care of handling any transfer encodings 3/28/2001 -- JCMa.
(defmethod display :around ((url url:http-url) major-type minor-type remote-stream local-stream)
  (let ((headers *headers*))
    (let ((transfer-encoding (or (get-header :transfer-encoding headers) :fixed-length)))
      (case transfer-encoding
	(:fixed-length
	  (call-next-method url major-type minor-type remote-stream local-stream))
	(:chunked 
	  (with-chunked-transfer-decoding (remote-stream :headers headers)
	    (call-next-method url major-type minor-type remote-stream local-stream)))
	(t (error 'server-not-implemented :close-connection t :url url
		  :format-string "The HTTP transfer decoding, ~A, is not implemented."
		  :format-args (list transfer-encoding)))))))

;; Display text as a default
(defmethod display ((http-url url:http-url) (major-type (eql :text)) minor-type remote-stream stream)
  (declare (ignore minor-type))
  (fresh-line stream)
  (%display-crlf-encoded-stream remote-stream stream))

(defmethod display ((http-url url:http-url) (major-type (eql :text)) (minor-type (eql :plain)) remote-stream stream)
  (fresh-line stream)
  (%display-crlf-encoded-stream remote-stream stream))

(defmethod display ((http-url url:http-url) (major-type (eql :text)) (minor-type (eql :html)) remote-stream stream)
  (fresh-line stream)
  (%display-crlf-encoded-stream remote-stream stream))

(defmethod display ((http-url url:http-url) (major-type (eql :text)) (minor-type (eql :uri-list)) remote-stream stream)
  (fresh-line stream)
  (%display-crlf-encoded-stream remote-stream stream))

(defmethod display ((url url:http-url) (major-type (eql :image)) minor-type remote-stream local-stream)
  (declare (ignore remote-stream minor-type))
  (html:note-anchor "Image Not Shown" :reference url :stream local-stream))

(defmethod display ((url url:http-url) (major-type (eql :audio)) minor-type remote-stream local-stream)
  (declare (ignore remote-stream minor-type))
  (html:note-anchor "Audio Not Shown" :reference url :stream local-stream)) 

(defmethod display ((url url:http-url) (major-type (eql :video)) minor-type remote-stream local-stream)
  (declare (ignore remote-stream minor-type))
  (html:note-anchor "Video Not Shown" :reference url :stream local-stream))

(defmethod display ((url url:http-url) (major-type (eql :application)) minor-type remote-stream local-stream)
  (declare (ignore remote-stream minor-type))
  (html:note-anchor "Application Not launched" :reference url :stream local-stream))

;; Used to return values to an application from the POST Method.
(defmethod display ((url url:http-url) (major-type (eql :application)) (minor-type (eql :lisp-sexp)) remote-stream local-stream)
  (declare (ignore local-stream))
  (let ((*read-eval* nil))
    (read remote-stream nil nil)))

;; Useful for debugging client certificate creation.
(defmethod display ((url url:http-url) (major-type (eql :application)) (minor-type (eql :x-x509-user-cert)) remote-stream local-stream)
  (display url :text :plain remote-stream local-stream))

;;;------------------------------------------------------------------- 
;;;
;;; UTILITIES
;;;

(defgeneric display-raw-output (client headers &optional display-stream))

(defmethod display-raw-output ((client client) headers &optional (display-stream *standard-output*))
  (let ((remote-stream (client-stream client))
	(status (client-status client)))
    (format display-stream "~&Status Code: ~D (~A)~%Server Version: ~(~A~)" status (get-string-for-status-code status) (client-connection-version client))
    (fresh-line display-stream) 
    (print-headers display-stream headers)
    (terpri display-stream)
    (unless (eql 0 (get-header :content-length *headers*))
      (destructuring-bind (major-type minor-type &key &allow-other-keys)
	  (or (get-header :content-type *headers*) '(:text :plain))
	(display (client-url client) major-type minor-type remote-stream display-stream)))))

(define-macro with-status-code-dispatch ((&key (client '*client*)
					       (headers '*headers*)
					       (status '(client-status client))
					       (url '(client-url client))
					       (success-status-codes '(200 203))
					       exceptions-flush-entities
					       (http-version '(client-request-version client)))
					 &body body)
  `(let ((client ,client)
	 (status ,status)
	 (headers ,headers))
     (flet ((do-it () ,@body))
       (declare (dynamic-extent #'do-it))
       (case status
	 (,success-status-codes
	  (do-it))
	 ((301 302 303)
	  ,@(when exceptions-flush-entities
	      `((flush-input-entity (client-stream client) headers ,http-version)))
	  (signal (ecase status
		    (301 'document-moved-permanently)
		    (302 'document-moved-temporarily)
		    (303 'document-forwarded))
		  :new-urls (mapcar #'url:intern-url
				    (ensure-list (or (get-header :location headers)
						     (get-header :content-location headers))))
		  :version ,http-version))
	 (401
	   (destructuring-bind (&optional authentication-method . realm)
	       (get-header :WWW-Authenticate headers)
	     ,@(when exceptions-flush-entities
		 `((flush-input-entity (client-stream client) headers ,http-version)))
	     (error (ecase authentication-method
                      (:basic 'recoverable-unauthorized-client-access)
                      (:digest (if (getf (cdr realm) :stale) 'client-access-with-stale-nonce 'recoverable-unauthorized-client-access)))
		    :url ,url
		    :method (client-method client)
		    :authentication-method authentication-method
		    :authentication-realm realm)))
	 (t (client-signal-http-code ,url status (client-method client) 
				     :headers headers 
				     :reason (client-reason client)
				     :version ,http-version))))))

;;;------------------------------------------------------------------- 
;;;
;;; STANDARD HEAD METHOD
;;;

(defun %show-url-headers (url headers stream)
  (handler-case
    (handling-redirects (url)
      (handling-authentication (authorization proxy-authorization)
	(with-http-request
	  (url :head 
	       :request-headers (compute-standard-request-headers url :authorization authorization
								  :proxy-authorization proxy-authorization :header-plist headers))
	  remote-stream				;ignore 
	  (with-status-code-dispatch (:client client :url url :status (client-status client)
					      :success-status-codes (200 203) :exceptions-flush-entities nil)
	    (fresh-line stream)
	    (print-headers stream *headers*)
	    (terpri stream)))))
    (http-condition (cond) (www-utils:report-condition cond stream))))

(defgeneric show-url-headers (url &key headers stream)
  (:documentation "Access headers URL and display it on STREAM."))

(defmethod show-url-headers ((string string) &key headers (stream *standard-output*))
  (show-url-headers (url:intern-url string) :headers headers :stream stream))

(defmethod show-url-headers ((url url:http-url) &key headers (stream *standard-output*))
  (%show-url-headers url headers stream))

;;;------------------------------------------------------------------- 
;;;
;;;  STANDARD GET METHOD
;;; 

(defmacro %with-open-url ((url request-headers &key (start nil start-supplied-p) (end nil end-supplied-p)) &body body)
  (let* ((range-p (or start-supplied-p end-supplied-p))
	 (code `(handling-redirects (,url)
		  (handling-authentication (authorization proxy-authorization)
		    (with-http-request
		      (,url :get 
		       :request-headers (compute-standard-request-headers
					  ,url :authorization authorization :proxy-authorization proxy-authorization
					  ,@(when range-p '(:range range)) :header-plist ,request-headers))
		      ,@body)))))
    (if range-p
	`(let ((range (and ,start ,end (list ,start ,end))))
	   (declare (dynamic-extent range))
	   ,code)
	code)))

(defun %show-url (url request-headers output-stream &key raw-output-p start end response-hook signal-conditions-p)
  (handler-case-if (not signal-conditions-p)
     (%with-open-url (url request-headers :start start :end end)
       (let ((response-headers (client-response-headers client)))
	 (multiple-value-prog1
	   (if raw-output-p
	       (display-raw-output client response-headers output-stream)
	       (with-status-code-dispatch (:client client :url url :status (client-status client)
						   :success-status-codes (200 203 205 206)
						   :exceptions-flush-entities t) 
		 (destructuring-bind (major-type minor-type &key &allow-other-keys)
		     (get-header :content-type response-headers)
		   (display url major-type minor-type remote-stream output-stream))))
	   (when response-hook
	     (funcall response-hook client)))))
    (http-condition (cond) (www-utils:report-condition cond output-stream))))

(define-generic show-raw-url (url &key headers stream start end)
  (:documentation "Access URL and display it on STREAM without display processing.
HEADERS are a plist of outgoing headers to accompany the request."))

(defmethod show-raw-url ((url url:http-url) &key headers (stream *standard-output*) start end)
  (%show-url url headers stream :raw-output-p t :start start :end end))

(defmethod show-raw-url ((string string) &key headers (stream *standard-output*) start end)
  (show-raw-url (url:intern-url string) :headers headers :stream stream :start start :end end))

(define-generic show-url (url &key headers stream)
  (:documentation "Access URL and display it on STREAM.
HEADERS are a plist of outgoing headers to accompany the request."))

(defmethod show-url ((url url:http-url) &key headers (stream *standard-output*))
  (%show-url url headers stream :raw-output-p nil))

(defmethod show-url ((string string) &key headers (stream *standard-output*))
  (show-url (url:intern-url string) :headers headers :stream stream))

(defmethod show-url ((url url:file-pathname) &key headers (stream *standard-output*))
  (declare (ignore headers))
  (file-url-copy-file (url::file-url-pathname url) stream :element-type (element-type url)))

(defmethod show-url ((url url:file-directory) &key headers (stream *standard-output*))
  (declare (ignore headers))
  (loop for (path . plist) in (file-url-directory-info (url::file-url-pathname url))
        for file-url = (pathname-file-url-string path)
        for length = (getf plist :length-in-bytes)
        for creation-date = (getf plist :creation-date)
        for reference-date = (getf plist :reference-date)
        do #+Genera (format stream "~&~A~60T~:[~;~:*~\\time\\~]~:[~; (~:*~\\time\\)~]~:[~;~:* [~D Bytes]~] " file-url creation-date reference-date length)
        #-Genera (format stream "~&~A~60T~:[~;~:*~A~]~:[~; (~:*~A)~]~:[~;~:* [~D Bytes]~] "
                         file-url (when creation-date (write-standard-time creation-date nil))
                         (when reference-date (write-standard-time reference-date nil))
                         length)))

(define-macro handling-ftp-authentication ((authorization-var) &body body)
  "Handles client authentication by rerunning body with AUTHENTICATION-VAR bound
to a user-specified authentication."
  `(loop with ,authorization-var and found-in-cache-p and prompt-p = t and recompute-header-p
	 with tries fixnum = 0 and proxy-tries fixnum = 0 and retry-limit fixnum = *number-of-authentication-retries*
	 doing (handler-case-if (and (< tries retry-limit) (< proxy-tries retry-limit))
                   (return (progn . ,body))
		 (recoverable-unauthorized-client-access
                  (cond)
                  (multiple-value-setq (,authorization-var found-in-cache-p)
                      (client-authenticate-user cond :recompute-authorization-header-p recompute-header-p
                                                :prompt-p prompt-p :stream *query-io*))
                  (incf tries)
                  (unless ,authorization-var
                    (return nil))
                  (setq prompt-p (if (or recompute-header-p (not found-in-cache-p)) :reprompt t))
                  (unless recompute-header-p
                    (setq recompute-header-p t))))))

(defun authorization-header-basic-user-id-and-password (authorization)
  (when authorization
    (destructuring-bind (authentication-method cookie &rest args) authorization
      (declare (ignore args))
      (case authentication-method
        (:basic
         (let* ((decoded-authorization (base64:base64-decode-vector cookie :decoded-byte-type *standard-character-type*))
                (colon (position #\: decoded-authorization))
                (username (subseq decoded-authorization 0 colon))
                (pw (subseq decoded-authorization (1+ colon))))
           (declare (dynamic-extent decoded-authorization))
           (values username pw)))
        (t nil)))))

(defmethod client-ftp-user-id-and-password ((url url:ftp-url) authorization)
  ;; Extract from the headers
  (multiple-value-bind (user-id pw)
      (authorization-header-basic-user-id-and-password authorization)
    (when (or user-id pw)
      (return-from client-ftp-user-id-and-password (values user-id pw))))
  ;; Extract from the FTP URL
  (multiple-value-bind (user-id pw)
      (url:user-id-and-password url)
    (when (or user-id pw)
      (return-from client-ftp-user-id-and-password (values user-id pw))))
  ;; Provide some defaults
  (values "anonymous" (server-mail-address)))

(defmethod show-url ((url url:ftp-pathname) &key headers (stream *standard-output*))
  (declare (ignore headers))
  (handling-ftp-authentication (authorization)
    (multiple-value-bind (user-id pw)
        (client-ftp-user-id-and-password url authorization)
      (ftp-copy-file url stream  :user-id user-id :user-pw pw))))

#|
;; rewrite for new ftp client
(defmethod show-url ((url url:ftp-directory) &key headers (stream *standard-output*))
  (declare (ignore headers))
  (flet ((directory-info (url)
           (multiple-value-bind (user-id pw)
               (url:user-id-and-password url)
             (ftp-directory-info #+Genera (url::ftp-url-pathname url)
                                 #-Genera (if (local-url-p url) (ftp-url-pathname url) url)
                                 (or user-id "anonymous")
                                 (or pw (www-utils:user-mail-address))))))
    (declare (inline directory-info))
    ;; get down to business
    (loop for (path . plist) in (directory-info url)
	  for ftp-url = (pathname-ftp-url-string path)
	  for length = (getf plist :length-in-bytes)
	  for creation-date = (getf plist :creation-date)
	  for reference-date = (getf plist :reference-date)
	  do (format stream
		     #-(or CLIM-SYS CMU) "~&~A~60T~:[~;~:*~\\time\\~]~:[~; (~:*~\\time\\)~]~:[~;~:* [~D Bytes]~] "
		     ;; Where \\time\\ escape is not recognized here - OBC
		     #+(or CLIM-SYS CMU) "~&~A~60T~:[~;~:*~a~]~:[~; (~:*~a)~]~:[~;~:* [~D Bytes]~] "
		     ftp-url creation-date reference-date length) 
          #+Genera (format stream "~&~A~60T~:[~;~:*~\\time\\~]~:[~; (~:*~\\time\\)~]~:[~;~:* [~D Bytes]~] "
                           ftp-url creation-date reference-date length)
          #-Genera (format stream "~&~A~60T~:[~;~:*~A~]~:[~; (~:*~A)~]~:[~;~:* [~D Bytes]~] "
                           ftp-url (when creation-date (write-standard-time creation-date nil))
                           (when reference-date (write-standard-time reference-date nil))
                           length))))  
|#
;;;------------------------------------------------------------------- 
;;;
;;; TRACE METHOD
;;;

(defun %show-url-trace (url headers stream &key (max-forwards 5.))
  (handler-case
    (handling-redirects (url)
      (handling-authentication (authorization proxy-authorization)
	(with-http-request
	  (url :trace
	       :request-headers (compute-standard-request-headers
				  url :authorization authorization :proxy-authorization proxy-authorization 
				  :header-plist `(,@(when max-forwards `(:max-forwards ,max-forwards)) ,@headers)))
	  (with-status-code-dispatch (:client client :url url :status (client-status client) :exceptions-flush-entities t)
	    (fresh-line stream)
	    (print-headers stream *headers*)
	    (terpri stream)
	    (with-transfer-decoding* (remote-stream url http-version :headers *headers*)
	      (stream-decode-crlf-until-eof remote-stream stream))))))
    (http-condition (cond) (www-utils:report-condition cond stream))))
                              
(define-generic show-url-trace (url &key max-forwards headers stream )
  (:documentation "Execute the TRACE method URL and display the result on STREAM."))
      
(defmethod show-url-trace ((url url:http-url) &key (max-forwards 5) headers (stream *standard-output*))
  (%show-url-trace url headers stream :max-forwards max-forwards)) 

(defmethod show-url-trace ((string string) &key (max-forwards 5) headers (stream *standard-output*))
  (show-url-trace (url:intern-url string) :max-forwards max-forwards :headers headers :stream stream)) 

;;;------------------------------------------------------------------- 
;;;
;;; OPTIONS METHOD
;;;

(defun %show-url-options (url headers stream)
  (handler-case
    (handling-redirects (url)
      (handling-authentication (authorization proxy-authorization)
	(with-http-request
	  (url :options 
	       :request-headers (compute-standard-request-headers
				  url :authorization authorization
				  :proxy-authorization proxy-authorization :header-plist headers))
	  remote-stream				;ignore
	  (with-status-code-dispatch (:client client :url url :status (client-status client) :exceptions-flush-entities t)
	    (fresh-line stream)
	    (print-headers stream *headers*)
	    (terpri stream)))))
    (http-condition (cond) (www-utils:report-condition cond stream))))

(define-generic show-url-options (url &key headers stream)
  (:documentation "Execute the options method URL and display the result on STREAM."))

(defmethod show-url-options ((url url:http-url) &key headers (stream *standard-output*))
  (%show-url-options url headers stream))

(defmethod show-url-options ((string string) &key headers (stream *standard-output*))
  (show-url-options (url:intern-url string) :headers headers :stream stream)) 

;;;------------------------------------------------------------------- 
;;;
;;;  DELETE METHOD
;;;

(defun %delete-url (url headers stream)
  (handler-case
    (handling-redirects (url)
      (handling-authentication (authorization proxy-authorization)
	(with-http-request
	  (url :delete 
	       :request-headers (compute-standard-request-headers
				  url :authorization authorization
				  :proxy-authorization proxy-authorization :header-plist headers))
	  (when *debug-client*
	    (fresh-line stream)
	    (print-headers stream)
	    (terpri stream))
	  (case (client-status client)
	    (200
	      (with-transfer-decoding* (remote-stream url http-version :headers *headers*)
		(destructuring-bind (major-type minor-type) (get-header :content-type)
		  (display url major-type minor-type remote-stream stream)))
	      (values url :deleted))
	    (202
	      (values url :accepted))
	    (204
	      (values url :deleted))
	    (t (client-signal-http-code url (client-status client) :delete 
					:reason (client-reason client) :version http-version))))))
    (http-condition (cond) (www-utils:report-condition cond stream))))

(defgeneric delete-url (url &key headers stream)
  (:documentation "Deletes URL from the origin server."))

(defmethod delete-url ((url url:http-url) &key headers  (stream *standard-output*))
  (%delete-url url headers stream))

(defmethod delete-url ((string string) &key headers (stream *standard-output*))
  (delete-url (url:intern-url string) :headers headers :stream stream))

;;;------------------------------------------------------------------- 
;;;
;;; POST URL 
;;; 

(defun %push-entry-url-encoded-vector (vector key value)
  (labels ((vector-safe-push-extend (char vector)
             (cond ((escaped-character-p char)
                    (let ((digit-chars (escape-character char 'cons)))
                      (declare (dynamic-extent digit-chars))
                      (dolist (ch digit-chars)
                        (vector-push-extend (char-code ch) vector))))
                   (t (vector-push-extend (char-code char) vector))))
           (push-key (key vector)
             (with-fast-array-references ((key key string))
               (loop for idx upfrom 0 below (length key)
                     for char = (aref key idx)
                     do (vector-safe-push-extend char vector)
                     finally (vector-push-extend #.(char-code #\=) vector))))
           (push-value (value vector)
             (with-fast-array-references ((value value string))
               (loop for idx upfrom 0 below (length value)
                     for char = (aref value idx)
                     do (case char
                          (#\newline ;; CRLF encode
			   (vector-push-extend #.(char-code #\Return) vector)
			   (vector-push-extend #.(char-code #\Linefeed) vector))
                          (#.(if (eql #\newline #\Return) #\Linefeed #\Return)) ;ignore other delimiter
                          (t (vector-safe-push-extend char vector)))
                     finally (vector-push-extend #.(char-code #\&) vector)))))
    (declare (inline vector-safe-push-extend push-key))
    ;; push the key
    (push-key (etypecase key
		(symbol (symbol-name key))
		(string key))
	      vector)
    ;; push the value
    (typecase value
      (string (push-value value vector))
      (pathname
	(with-open-file (file value :direction :input :element-type *standard-character-type* :if-does-not-exist :error)
	  (let ((size (file-stream-length-in-bytes file)))
	    (using-resource (buffer post-form-buffer size)
	      (setq buffer (crlf-stream-copy-into-string file size 0 buffer))
	      (push-value buffer vector)))))
      (t (let ((val-string (write-to-string value :base 10. :escape t)))
           (declare (dynamic-extent val-string)) 
           (push-value val-string vector))))))

(defmacro %url-encoded-vector (source type vector vector-size)
  `(loop with vector = (or ,vector 
                           (make-array ,vector-size :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))
	   ,@(ecase type
	       (:plist `(for (key value) on ,source by (function cddr)))
	       (:alist `(for (key value) in ,source)))
	 do (%push-entry-url-encoded-vector vector key value)
	 finally (return vector)))

(defun url-encoded-vector-from-alist (alist &optional vector (size *post-form-buffer-size*))
  (%url-encoded-vector alist :alist vector size))

(defun url-encoded-vector-from-plist (plist &optional vector (size *post-form-buffer-size*))
  (%url-encoded-vector plist :plist vector size))

(defmacro with-url-encoded-vector ((vector spec &optional (size '*post-form-buffer-size*)) &body body)
  `(let ((spec ,spec)
         (size ,size))
     (using-resource (,vector data-cache-array size) 
       (etypecase (car spec)
         (atom (url-encoded-vector-from-plist spec ,vector size))
         (cons (url-encoded-vector-from-alist spec ,vector size)))
       ,@body)))

#|
(defun print-url-encoded-vector (vector &optional (unescape-p t) (stream *standard-output*))
  (flet ((unescaped-char (vector start)
           (let ((buf (make-string 2)))
             (setf (schar buf 0) (code-char (aref vector start))
                   (schar buf 1) (code-char (aref vector (1+ start))))
             (code-char (parse-integer buf :radix 16 :start 0 :end 2)))))
    (loop with end = (length vector)
          for idx upfrom 0 below end
          for char = (code-char (aref vector idx))
          unless (and unescape-p (escape-character-p char))
          do (write-char char stream)
          else 
          do (write-char (unescaped-char vector (1+ idx)) stream)
          (incf idx 2)))
  (values))
|#

(define-generic post-url (url form-values &key headers stream)
  (:documentation "Posts FORM-VALUES to URL using the HTTP method.
URL is either an interned URL or a string. FORM-VALUES is either a
URL encoded vector, an alist of (keyword values), or a property list of
keyword values.  This method returns the values returned by the application
of the DISPLAY method to the MIME content type of the server reply.
Application/Lisp-SExp can return lisp values."))

(defun %post-url (url vector headers stream)
  (flet ((send-data (remote-stream vector content-length)
	   (with-binary-stream (remote-stream :output)
	     (write-vector remote-stream vector 0 content-length)
	     (force-output remote-stream))))
    (handler-case
        (let* ((content-length (fill-pointer vector))
               (outgoing-headers `(,@headers 
                                   :content-type (:application :x-www-form-urlencoded)
                                   :content-length ,content-length
                                   ,@(unless (http-version-less-p :http/1.1 *client-http-version*)
                                       '(:expect (:100-continue t))))))
          (declare (dynamic-extent outgoing-headers))
          (handling-redirects (url)
            (handling-authentication (authorization proxy-authorization)
              (with-http-request
                  (url :post
                       :request-headers (compute-standard-request-headers
                                         url :authorization authorization :proxy-authorization proxy-authorization
                                         :header-plist outgoing-headers)
                       :request-body (send-data remote-stream vector content-length))
                (with-status-code-dispatch (:client client :url url :status (client-status client) :http-version http-version
                                            :headers *headers* :success-status-codes (200 201 204) :exceptions-flush-entities t)
                  (ecase status
                    ((200 201)
                     (destructuring-bind (major-type minor-type &key &allow-other-keys)
                         (get-header :content-type)
                       (display url major-type minor-type remote-stream stream)))
                    (204
                     (values url nil))))))))
      (http-condition (cond) (www-utils:report-condition cond stream)))))

(defmethod post-url ((url url:http-url) (vector vector) &key headers (stream *standard-output*))
  (%post-url url vector headers stream))

(defmethod post-url ((string string) vector &key headers (stream *standard-output*))
  (post-url (url:intern-url string) vector :headers headers :stream stream))

(defmethod post-url (url (list cons) &key headers (stream *standard-output*))
  (with-url-encoded-vector (vector list)
    (post-url url vector :headers headers :stream stream)))

(define-generic post-form-values (url &rest args &key headers stream &allow-other-keys)
  (declare (values returned-form-values))
  (:documentation "Posts form values URL. 
Call this with the destination URL and an alternating set of keyword and values pairs.
values are readable lisp objects. HEADERS and STREAM are priveledged arguments
that are passed directly to POST-URL.  All other keywords and values are passed through
to the server via the HTTP POST method.  Values are transfered using write-to-string with
escape t and base 10. They are written to strings, and so, BIND-QUERY-VALUES*
should be used to restore them in the server response function."))

(defmethod post-form-values (url &rest args &key headers (stream *standard-output*) &allow-other-keys)
  (declare (dynamic-extent args))
  (let ((plist (loop for (key val) on args by #'cddr
                     unless (member key '(:headers :stream))
		       collect key
		       and collect val)))
    (declare (dynamic-extent plist))
    (post-url url plist :headers headers :stream stream)))

;;;------------------------------------------------------------------- 
;;;
;;; PUT METHOD
;;; 

;; Handles the 1.1 PUT and the 1.0 cretenous PUT
(defun %put-url (url resource-writer content-length content-type headers stream version)
  (flet ((send-data (writer remote-stream url content-length)
           (with-transfer-encoding (remote-stream (if content-length
						      :fixed-length :chunked))
	     (funcall writer url remote-stream))
           (force-output remote-stream)))
    (handler-case
        (let ((outgoing-headers `(,@headers
                                  ,.(cond ((eq version :overwrite) nil)
                                          ((numberp version) `(:derived-from ,version))
                                          (t nil))
                                  :content-type ,content-type
                                  ,@(if content-length
                                        `(:content-length ,content-length)
				      `(:transfer-encoding :chunked))
                                  ,@(unless (http-version-less-p :http/1.1 *client-http-version*)
                                      '(:expect (:100-continue t))))))
          (declare (dynamic-extent outgoing-headers))
          (handling-redirects (url)
            (handling-authentication (authorization proxy-authorization)
              (with-http-request
                  (url :put 
                       :request-headers (compute-standard-request-headers
                                         url :authorization authorization :proxy-authorization proxy-authorization
                                         :header-plist outgoing-headers)
                       :request-body (send-data resource-writer remote-stream url content-length))
                (with-status-code-dispatch (:success-status-codes (200 201 204) :status (client-status client)
                                            :url (client-url client) :client client
                                            :headers (client-response-headers client) :http-version http-version
                                            :exceptions-flush-entities t)
                  (let ((content-location (get-header :content-location))
                        (content-version (get-header :content-version))
                        (last-modified (get-header :last-modified))
                        (keyword (if (eql status 201) :created :modified)))
                    (when content-location
                      (setq content-location (intern-url content-location :if-does-not-exist :uninterned)))
                    (case status
                      (200 (destructuring-bind (major-type minor-type)
                               (get-header :content-type)
                             (display (or content-location url) major-type minor-type remote-stream stream))))
                    (values (or content-location url) keyword content-version last-modified)))))))
      (http-condition (cond) (www-utils:report-condition cond stream)))))

(defun %put-url-from-pathname (url pathname headers stream &optional content-type content-length version)
  (let* ((copy-mode (url:copy-mode url))
	 (content-type-spec (or content-type (url::mime-content-type-spec url)))
	 (path (ecase copy-mode
		 ((:binary :text) pathname)
		 (:crlf (ensure-crlf-canonical-file pathname))))
	 (file-length (or content-length (www-utils:file-length-in-bytes path)))
	 (file-version (or version (file-version pathname))))
    (flet ((send-data (url stream)
	     (declare (ignore url))
	     (stream-copy-bytes path stream file-length copy-mode)))
      (declare (dynamic-extent #'send-data))
      (%put-url url #'send-data file-length content-type-spec headers stream file-version))))

(defun %put-url-from-vector (url vector headers stream start end &optional content-type version )
  (let ((content-type-spec (or content-type (url::mime-content-type-spec url)))
	(active-length (unless (stringp vector) (- end start))))
    (flet ((send-data (url stream)
	     (declare (ignore url))
	     (write-vector stream vector start end)))
      (declare (dynamic-extent #'send-data))
      (%put-url url #'send-data active-length content-type-spec headers stream version))))

(define-generic put-url (url resource &key headers stream content-type version &allow-other-keys)
  (declare (values remote-uri status version last-modified))
  (:documentation "Puts RESOURCE in URL reporting returns on STREAM.
RESOURCE is a pathname, a vector, or a writer function that receives the arguments: (URL REMOTE-STREAM).
VERSION defaults to the universal time when RESOURCE was created.
When VERSION is :OVERWRITE no conflict checking is done.
VERSION may also be passed in.
CONTENT-TYPE defaults to the content-type associated with the URL, but may be passed in.
CONTENT-LENGTH is optional for computed puts and not recommended when resource is a pathname.
START and END are available to specify subsequences when RESOURCE is a vector."))

(defmethod put-url ((string string) resource &key headers (stream *standard-output*)
		    content-type content-length version)
  (put-url (url:intern-url string) resource :headers headers
	   :content-type content-type :content-length content-length
	   :version version :stream stream))

(defmethod put-url ((url url:http-url) (pathname pathname) &key headers (stream *standard-output*)
		    content-type content-length version)
  (%put-url-from-pathname url pathname headers stream content-type content-length version))

(defmethod put-url ((url url:http-url) (writer function) &key headers (stream *standard-output*)
		    (content-type (url::mime-content-type-spec url)) content-length (version (get-universal-time)))
  (%put-url url writer content-length content-type headers stream version))

(defmethod put-url ((url url:http-url) (vector vector) &key headers (stream *standard-output*) (start 0) (end (length vector))
		    content-type (version (get-universal-time)))
  (%put-url-from-vector url vector headers stream start end content-type version))


;;;------------------------------------------------------------------- 
;;;
;;; TEST VECTORS FOR FIXED LENGTH AND CHUNKED PUT
;;;


#|

(defun file-equal (path1 path2)
  (with-open-file (file1 path1 :direction :input :element-type '(unsigned-byte 8))
    (with-open-file (file2 path2 :direction :input :element-type '(unsigned-byte 8))
      (loop for idx upfrom 0
	    for byte1 = (read-byte file1 nil nil)
	    for byte2 = (read-byte file2 nil nil)
	    while (and byte1 byte2)
	    unless (eql byte1 byte2)
	      return (values nil idx)
	    finally (return (values (eql byte1 byte2) idx))))))

(defun test (url n)
  (flet ((fctn (url stream)
	   (declare (ignore url))
	   (loop with vector = "abcdefghijklmnopqrstuvwxyz1234567890 
"
		 with radix = (length vector)
		 for count upfrom 0 below n
		 doing (multiple-value-bind (item idx)
			   (truncate count radix)
			 (write-char (aref vector idx) stream)
			 item))))
    (put-url url #'fctn)))

(defun test2 (url)
  (flet ((fctn (url stream)
	   (declare (ignore url))
	   (stream-copy-until-eof #p"j:>jcma>hot.gif" stream :binary)))
    (put-url url #'fctn)))

(put-url "http://jcma.ai.mit.edu/jcma/launch.lisp" #p"j:>jcma>launch-demo.lisp" :version :overwrite)
(file-equal #p"j:>jcma>launch-demo.lisp.newest" #p"j:>jcma>html>launch.lisp.newest")

(put-url "http://jcma.ai.mit.edu/jcma/hot.gif" #p"j:>jcma>hot.gif" :version :overwrite)
(file-equal #p"j:>jcma>hot.gif.newest" #p"j:>jcma>html>hot.gif.newest")

(test #u"http://jcma.ai.mit.edu/jcma/test.text" 100)

(test2 #u"http://jcma.ai.mit.edu/jcma/test.gif")

(file-equal #p"j:>jcma>hot.gif.newest" #p"j:>jcma>html>test.gif.newest") 

|#


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(define-generic capture-raw-url (url &key headers display-stream start end)
  (declare (values vector header-alist))
  (:documentation "Access URL and a vector containing it.
HEADERS are a plist of outgoing headers to accompany the request."))

(defmethod capture-raw-url ((url url:http-url) &key headers (display-stream *standard-output*)  start end)
  (let ((range (when (and start end)
		 `(,start ,end))))
    (declare (dynamic-extent range))
    (handler-case
      (handling-redirects (url)
	(handling-authentication (authorization proxy-authorization)
	  (with-http-request
	    (url :get :request-headers (compute-standard-request-headers url :authorization authorization
									 :proxy-authorization proxy-authorization
									 :range range :header-plist headers)) 
	    (let ((remote-stream (client-stream client))
		  (status (client-status client))
		  (http-version (client-request-version client)))
	      (when display-stream
		(format display-stream "~&Status Code: ~D (~A)~%Server Version: ~(~A~)" status (get-string-for-status-code status) http-version)
		(fresh-line display-stream) 
		(print-headers display-stream *headers*)
		(terpri display-stream))
	      (flet ((stream-capture-until-eof (from-stream ignore copy-mode)
		       (declare (ignore ignore))
		       (let ((content-length (get-header :content-length)))
			 (ecase copy-mode
			   ((:text :crlf)
			    (crlf-stream-copy-into-string from-stream content-length))
			   (:binary
			     (with-binary-stream (from-stream :input)
			       (binary-stream-copy-into-8-bit-array from-stream content-length)))))))
		(let ((copy-mode (mime-content-type-copy-mode (get-header :content-type *headers*))))
		  (with-transfer-decoding* (remote-stream (client-url client) http-version :headers *headers* :copy-mode copy-mode
							  :stream-functions '(stream-capture-until-eof))
		    (stream-capture-until-eof remote-stream nil copy-mode))))))))
      (http-condition (cond) (www-utils:report-condition cond display-stream))))) 

(defmethod capture-raw-url ((string string) &key headers (display-stream *standard-output*)  start end)
  (capture-raw-url (url:intern-url string) :headers headers :display-stream display-stream :start start :end end)) 


;;;------------------------------------------------------------------- 
;;;
;;; EXTENDED FILE COPYING
;;;

(define-generic copy-url-to-stream (URL output-stream &key request-headers start end before-copy-hook after-copy-hook)
  (:documentation "Copies the contents of URL to stream.
REQUEST-HEADERS is aplist of any special headers for inclusion into the request headers.
START and END, when provided, are offsets into the URL contents suitable for a range request,
typically byte offsets.
BEFORE-COPY-HOOK is an optional function that is called before the copy with the arguments (CLIENT CONTENT-TYPE COPY-MODE).
AFTER-COPY-HOOK is an optional function after the copy completes with the argument (CLIENT)."))

(defmethod copy-url-to-stream ((url url:http-url) output-stream &key request-headers start end before-copy-hook after-copy-hook)
  (%with-open-url (url request-headers :start start :end end)
    (with-status-code-dispatch (:client client :url url :status (client-status client)
					:success-status-codes (200 203 205 206)
					:exceptions-flush-entities t) 
      (let ((response-headers (client-response-headers client))
	    copy-mode)
	(with-header-values (content-type) response-headers
	  (setq copy-mode (mime-content-type-copy-mode content-type))
	  (when before-copy-hook
	    (funcall before-copy-hook client content-type copy-mode))
	  (with-transfer-decoding* (remote-stream url http-version :headers response-headers)
	    (stream-copy-until-eof remote-stream output-stream copy-mode))))
      (when after-copy-hook
	(funcall after-copy-hook client)))))

(defmethod copy-file ((from-pathname pathname) (to-url http-path) &rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (flet ((wild-p (pathname)
           (or (eql :wild (pathname-name pathname))
               (eql :wild (pathname-type pathname))
               (eql :wild (pathname-version pathname))))
         (unspecific-p (pathname)
           (and (member (pathname-name pathname) '(:unspecific nil))
                (member (pathname-type pathname) '(:unspecific nil))
                (member (pathname-version pathname) '(:unspecific nil))))
         (copy-file-p (pathname)		;don't copy files with no name and directories
           (let ((name (pathname-name pathname))
                 (type (pathname-type pathname)))
             (and (stringp name) (not (null-string-p name))
                  (stringp type) (not (null-string-p type)))))
         (copy-one-file (from to args)
           (let ((spec (url:make-url-string :scheme (protocol to-url)
                                            :host (url:host-string to)
                                            :port (url:port to)
                                            :path (url:path to)
                                            :name (pathname-name from)
                                            :extension (pathname-type from))))
             (apply #'copy-file from (intern-url spec :if-does-not-exist :uninterned) args))))
    (cond ((or (wild-p from-pathname)
               (unspecific-p from-pathname))
           (dolist (file (directory from-pathname))
             (when (copy-file-p file)
               (copy-one-file file to-url args))))
          (t (copy-one-file from-pathname to-url args)))))

(defmethod copy-file ((from-pathnames cons) (to-url http-path) &rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (loop for pathname in from-pathnames
	do (apply #'copy-file pathname to-url args)))

(defmethod copy-file ((from-pathname pathname) (to-url http-object) &key copy-mode content-type
                      (user-email-address (www-utils:user-mail-address)) report-stream &allow-other-keys)
  (with-copy-file-environment (from-pathname to-url report-stream)
    (let ((media-type (or content-type
			  (let ((type (pathname-type from-pathname)))
			    (when type (mime-content-type-spec-for-pathname-type type nil)))
			  (case copy-mode	;generic media types based on copy-mode
			    (:text '(:text :plain))
			    (:binary '(:message :http))
			    (t nil))))
	  (header-plist (and user-email-address `(:from (,user-email-address) :last-modified ,(file-modification-date from-pathname)))))
      (declare (dynamic-extent header-plist))
      (put-url to-url from-pathname :content-type media-type :headers header-plist))))

(defmethod copy-file ((from-url http-url) (to-pathname pathname) &key copy-mode request-headers report-stream &allow-other-keys)
  (with-copy-file-environment (from-url to-pathname report-stream)
    (%with-open-url (from-url request-headers)
      (with-status-code-dispatch (:client client :url from-url :status (client-status client)
					  :success-status-codes (200 203 205 206)
					  :exceptions-flush-entities t) 
	(let ((response-headers (client-response-headers client))
	      (to-file (make-pathname :name (or (pathname-name to-pathname) (url:object from-url))
				      :type (or (pathname-type to-pathname) (url:extension from-url))
				      :version (or (pathname-version to-pathname) :newest)
				      :defaults to-pathname))
	      c-mode)
	  (with-header-values (content-type last-modified #+CL-HTTP-File-Author from) response-headers
	    (setq c-mode (or (mime-content-type-copy-mode content-type) copy-mode))
	    (with-open-file (file to-file :direction :output :if-does-not-exist :create :if-exists :supersede
				  :element-type (ecase c-mode
						  ((:crlf :text) *standard-character-type*)
						  (:binary '(unsigned-byte 8))))
	      (ecase c-mode
		((:crlf :text)
		 (with-transfer-decoding* (remote-stream from-url http-version :headers response-headers
							 :stream-functions (stream-decode-crlf-until-eof))
		   (stream-decode-crlf-until-eof remote-stream file)))
		(:binary
		  (with-transfer-decoding* (remote-stream from-url http-version :headers response-headers)
		    (stream-copy-until-eof remote-stream file copy-mode))))
	      (setq to-file (truename file)))
	    (autotype-file to-file)
	    (cond-every
	      (last-modified 
		(setf (file-modification-date to-file) last-modified)
		#-UNIX ;; creation date is not available under unix
		(setf (file-creation-date to-file) last-modified))
	      #+CL-HTTP-File-Author
	      (from (set-file-author to-file from nil)))
	    to-file))))))

(defmethod copy-file ((from-urls cons) (to-pathname pathname) &rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (loop with directory = (make-pathname :name :unspecific :type :unspecific :version :unspecific)
        for from-url in from-urls
	do (apply #'copy-file from-url directory args)))

;; This does not handle the via-header. Should it?
;; It parses entity headers only to resend them. So, some efficiency could be gained
;; by just writing the  desired headers directly while breaking the put-url abstraction
(defmethod copy-file ((from-url http-url) (to-url http-object) &key copy-mode request-headers report-stream 
		      (user-email-address (www-utils:user-mail-address)) &allow-other-keys)
  (with-copy-file-environment (from-url to-url report-stream)
    (%with-open-url (from-url request-headers)
      (with-status-code-dispatch (:client client :url from-url :status (client-status client)
					  :success-status-codes (200 203 205 206)
					  :exceptions-flush-entities t) 
	(let ((response-headers (client-response-headers client)))
	  (with-header-values (content-type content-length last-modified date) response-headers
	    (let* ((c-mode (or copy-mode (mime-content-type-copy-mode content-type)))
		   (content-type-spec (or content-type (url::mime-content-type-spec from-url)))
		   (version (or last-modified date))
		   (new-headers `(,.(entity-header-plist response-headers '(:content-type :content-length :expires))
				  ,.(and user-email-address `(:from (,user-email-address))))))
	      (declare (dynamic-extent new-headers))
	      (flet ((send-data (url to-stream)
		       (declare (ignore url))
		       (with-transfer-decoding* (remote-stream from-url http-version :headers response-headers)
			 (stream-copy-until-eof remote-stream to-stream c-mode))))
		(declare (dynamic-extent #'send-data))
		(%put-url to-url #'send-data content-length content-type-spec new-headers report-stream version)))))))
    to-url))

(defmethod copy-file ((from-url http-object) (to-url http-path) &rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (let ((spec (url:make-url-string :scheme (protocol to-url)
                                   :host (url:host-string to-url)
                                   :port (url:port to-url)
                                   :path (url:path to-url)
                                   :name (url:object from-url)
                                   :extension (url:extension from-url))))
    (apply #'copy-file from-url (intern-url spec :if-does-not-exist :uninterned) args)))

#+ignore
(defmethod conditional-copy-file (from-pathname to-pathname &key (copy-mode :text) &allow-other-keys)
  (let ((from-path (probe-file from-pathname))
        (to-path (probe-file to-pathname))
        (to-directory (probe-directory to-pathname)))
    (when (and from-path
               to-directory
               (or (null to-path)
                   (> (file-write-date from-path)
                      (file-write-date to-path))))
      (copy-file from-pathname to-pathname :copy-mode copy-mode))))

;;;------------------------------------------------------------------- 
;;;
;;; COPYING FTP URLS 
;;;

#+CL-HTTP-FTP-CLIENT
(defmethod copy-file ((from-url ftp-pathname) (to-pathname pathname) &key copy-mode user-id password report-stream &allow-other-keys)
  (flet ((write-filename (stream)
           (url::write-object-name-string from-url stream t))
         (destination (to-pathname from-url)
           (make-pathname :name (or (pathname-name to-pathname) (url:object from-url))
                          :type (or (pathname-type to-pathname) (url:extension from-url))
                          :version (or (pathname-version to-pathname) :newest)
                          :defaults to-pathname)))
    (declare (dynamic-extent #'write-filename))
    (with-copy-file-environment (from-url to-pathname report-stream)
      (let ((host (url:host-string from-url))
            (port (url:host-port from-url))
            (c-mode (or copy-mode (copy-mode from-url)))
            last-modified to-file)
        (ftp:with-ftp-connection (ftp-connection host port :user-id user-id :password password)
          (ftp:ftp-change-directory ftp-connection (url:path from-url))
          (multiple-value-bind (size last-modification)
              (ftp:ftp-file-info ftp-connection #'write-filename)
            (declare (ignore size))
            (setq last-modified last-modification)
            (with-open-file (file (destination to-pathname from-url) :direction :output :element-type (copy-mode-element-type c-mode)
                                  :if-does-not-exist :create :if-exists :supersede)
              (flet ((write-resource (remote-stream)
                       (stream-decode-crlf-until-eof remote-stream file)))
                (declare (dynamic-extent #'write-resource))
                (ftp:ftp-get-file ftp-connection #'write-filename #'write-resource :mode (ftp-copy-mode c-mode)))
              (setq to-file (truename file)))))
        (autotype-file to-file)
        (cond-every
         (last-modified 
          (setf (file-modification-date to-file) last-modified)
          #-UNIX ;; creation date is not available under unix
          (setf (file-creation-date to-file) last-modified))
         ;; come up with the author sometime -- JCMa 4/29/2006
         ;; #+CL-HTTP-File-Author (from (set-file-author to-file from nil)))
         )))))

;; It would be nice to set the modification date and author on the FTP side
#+CL-HTTP-FTP-CLIENT
(defmethod copy-file ((from-pathname pathname) (to-url ftp-url) &key copy-mode user-id password report-stream &allow-other-keys)
  (flet ((write-filename (stream)
           (etypecase to-url
             (ftp-pathname
              (url::write-object-name-string to-url stream t))
             (ftp-directory
              (let ((name (pathname-name from-pathname))
                    (type (pathname-type from-pathname)))
                (fast-format stream "~A" name)
                (when type
                  (fast-format stream ".~A" type)))))))
    (declare (dynamic-extent #'write-filename))
    (with-copy-file-environment (from-pathname to-url report-stream)
      (let ((host (url:host-string to-url))
            (port (url:host-port to-url))
            (c-mode (or copy-mode (pathname-copy-mode from-pathname nil) (copy-mode to-url))))
        (ftp:with-ftp-connection (ftp-connection host port :user-id user-id :password password)
          (ftp:ftp-change-directory ftp-connection (url:path to-url))
          (with-open-file (file from-pathname :direction :input :element-type (copy-mode-element-type c-mode))
            (ftp:ftp-put-file ftp-connection file #'write-filename :mode (ftp-copy-mode c-mode)
                              :estimated-size (file-stream-length-in-bytes file))))))))

;; It would be nice to set the modification date and author on the FTP side
#+CL-HTTP-FTP-CLIENT
(defmethod copy-file ((from-url ftp-pathname) (to-url ftp-url) &key copy-mode 
                      from-user-id from-password to-user-id to-password report-stream &allow-other-keys)
  (flet ((write-from-filename (stream)
           (url::write-object-name-string from-url stream t))
         (write-to-filename (stream)
           (etypecase to-url
             (ftp-pathname
              (url::write-object-name-string to-url stream t))
             (ftp-directory
              (url::write-object-name-string from-url stream t)))))
    (declare (dynamic-extent #'write-from-filename #'write-to-filename))
    (with-copy-file-environment (from-url to-url report-stream)
      (let ((from-host (url:host-string from-url))
            (from-port (url:host-port from-url))
            (to-host (url:host-string to-url))
            (to-port (url:host-port to-url))
            (c-mode (or copy-mode (copy-mode from-url)))
            file-size ftp-copy-mode)
        (setq ftp-copy-mode (ftp-copy-mode c-mode))
        (ftp:with-ftp-connection (from-ftp-connection from-host from-port :user-id from-user-id :password from-password)
          (ftp:ftp-change-directory from-ftp-connection (url:path from-url))
          (setq file-size (ftp:ftp-file-size from-ftp-connection #'write-from-filename))
          (ftp:with-ftp-connection (to-ftp-connection to-host to-port :user-id to-user-id :password to-password)
            (ftp:ftp-change-directory to-ftp-connection (url:path to-url))
            (flet ((write-resource (to-stream)
                     (ftp:ftp-get-file from-ftp-connection #'write-from-filename to-stream :mode ftp-copy-mode)))
              (declare (dynamic-extent #'write-resource))
              (ftp:ftp-put-file to-ftp-connection #'write-resource #'write-to-filename :mode ftp-copy-mode
                                :estimated-size file-size))))))))

;; It would be nice to record modification date and author for the FTP URL -- JCMa 4/29/2006
#+CL-HTTP-FTP-CLIENT
(defmethod copy-file ((from-url http-object) (to-url ftp-url) &key user-id password copy-mode request-headers report-stream &allow-other-keys)
  (flet ((write-filename (stream)
           (etypecase to-url
             (ftp-pathname
              (url::write-object-name-string to-url stream t))
             (ftp-directory
              (url::write-object-name-string from-url stream t)))))
    (declare (dynamic-extent #'write-filename))
    (with-copy-file-environment (from-url to-url report-stream)
      (let ((host (url:host-string to-url))
            (port (url:host-port to-url)))
        (ftp:with-ftp-connection (ftp-connection host port :user-id user-id :password password)
          (ftp:ftp-change-directory ftp-connection (url:path to-url))
          (%with-open-url (from-url request-headers)
            (with-status-code-dispatch (:client client :url from-url :status (client-status client)
                                        :success-status-codes (200 203 205 206)
                                        :exceptions-flush-entities t) 
              (let ((response-headers (client-response-headers client))
                    c-mode)
                (with-header-values (content-type content-length #|last-modified #+CL-HTTP-File-Author from|#) response-headers
                  (ecase (setq c-mode (or (mime-content-type-copy-mode content-type) copy-mode))
                    (:text
                     (with-binary-stream (remote-stream :input)
                       (with-transfer-decoding* (remote-stream from-url http-version :headers response-headers)
                         (ftp:ftp-put-file ftp-connection remote-stream #'write-filename :mode (ftp-copy-mode c-mode)
                                           :estimated-size content-length))))
                    ((:binary :crlf)
                     (with-binary-stream (remote-stream :input)
                       (with-transfer-decoding* (remote-stream from-url http-version :headers response-headers)
                         (ftp:ftp-put-file ftp-connection remote-stream #'write-filename :mode (ftp-copy-mode c-mode)
                                           :estimated-size content-length))))))))))))))

#+CL-HTTP-FTP-CLIENT
(defmethod copy-file ((from-url ftp-pathname) (to-url http-object) &key copy-mode content-type user-id password
                      (user-email-address (www-utils:user-mail-address)) report-stream &allow-other-keys)
  (flet ((write-filename (stream)
           (url::write-object-name-string from-url stream t)))
    (declare (dynamic-extent #'write-filename))
    (with-copy-file-environment (from-url to-url report-stream)
      (let ((host (url:host-string from-url))
            (port (url:host-port from-url))
            (media-type (or content-type (mime-content-type-spec from-url)))
            (c-mode (or copy-mode (copy-mode from-url))))
        (ftp:with-ftp-connection (ftp-connection host port :user-id user-id :password password)
          (ftp:ftp-change-directory ftp-connection (url:path from-url))
          (multiple-value-bind (file-size last-modification unique-id)
              (ftp:ftp-file-info ftp-connection #'write-filename)
            (flet ((write-resource (to-url remote-stream)
                     (declare (ignore to-url))
                     (ftp:ftp-get-file ftp-connection #'write-filename remote-stream :mode (ftp-copy-mode c-mode))))
              (declare (dynamic-extent #'write-resource))
              (let ((header-plist `(,.(when last-modification
                                        `(:last-modified ,last-modification))
                                      ,.(when unique-id
                                          `(:e-tag ,unique-id))
                                      ,.(when user-email-address
                                          `(:from (,user-email-address))))))
                (declare (dynamic-extent header-plist))
                (put-url to-url #'write-resource
                         :content-type media-type
                         :content-length (case c-mode (:binary file-size) (t nil))
                         :headers header-plist)))))))))

(defmethod copy-file ((from-url ftp-pathname) (to-url http-path) &rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (let ((spec (url:make-url-string :scheme (protocol to-url)
                                   :host (url:host-string to-url)
                                   :port (url:port to-url)
                                   :path (url:path to-url)
                                   :name (object from-url)
                                   :extension (extension from-url))))
    (apply #'copy-file from-url (intern-url spec :if-does-not-exist :uninterned) args)))
