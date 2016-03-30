;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for HTTP-BASE-CLIENT version 51.2
;;; Reason: Function HTTP::HANDLING-AUTHENTICATION:  update.
;;; Function HTTP::%WITH-OPEN-URL:  -
;;; Function HTTP::%SHOW-URL:  -
;;; Function HTTP::%DELETE-URL:  -
;;; Function HTTP::%POST-URL:  -
;;; Function HTTP::%PUT-URL:  -
;;; Function HTTP::%SHOW-URL:  -
;;; Function HTTP::%SHOW-URL:  -
;;; Function HTTP::%SHOW-URL-HEADERS:  -
;;; Function HTTP::%SHOW-URL-OPTIONS:  -
;;; Function HTTP::%SHOW-URL-TRACE:  -
;;; Function (CLOS:METHOD HTTP::ACCESS-URL (HTTP::POST-TRANSACTION-CONTROL (EQL :POST) T T T)):  -
;;; Function (CLOS:METHOD HTTP::ACCESS-URL (T (EQL :GET) T T T)):  -
;;; Function (CLOS:METHOD HTTP::ACCESS-URL (T (EQL :HEAD) T T T)):  -
;;; Function (CLOS:METHOD HTTP:CAPTURE-RAW-URL (URL:HTTP-URL)):  -
;;; Function (CLOS:METHOD HTTP:COPY-FILE (URL:HTTP-URL CL:PATHNAME)):  -
;;; Function (CLOS:METHOD HTTP::COPY-URL-TO-STREAM (URL:HTTP-URL T)):  -
;;; Written by JCMa, 11/01/01 14:42:02
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.6,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.154,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.4, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.5, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.39, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.3,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Jcma 44, HTTP Proxy Server 6.27,
;;; HTTP Client Substrate 4.20, Statice Server 466.2, HTTP Client 51.1,
;;; Image Substrate 440.4, Essential Image Substrate 433.0,
;;; W4 Constraint-Guide Web Walker 45.11, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x994 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number -2141189585,
;;; Domain Fixes (from CML:MAILER;DOMAIN-FIXES.LISP.33),
;;; Don't force in the mail-x host (from CML:MAILER;MAILBOX-FORMAT.LISP.24),
;;; Make Mailer More Robust (from CML:MAILER;MAILER-FIXES.LISP.15),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add CLIM presentation and text style format directives. (from FV:SCLC;FORMAT.LISP.20),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; Make update schema work on set-value attributes with accessor names (from CML:LISPM;STATICE-SET-VALUED-UPDATE.LISP.1),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.107),
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.11),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:CLIENT;FLOGGER.LISP.53"
  "HTTP:CLIENT;SEXP-BROWSER.LISP.91")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.91")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;; Used to throw into a loop when user supplied wrong password, fixed.  -cvince 8/30/96
(define-macro handling-authentication ((authorization-var proxy-authorization-var) &body body)
  "Handles client authentication by rerunning body with AUTHENTICATION-VAR bound
to a user-specified authentication."
  `(loop with ,authorization-var and found-in-cache-p and prompt-p = t and recompute-header-p
	 with ,proxy-authorization-var and proxy-found-in-cache-p and proxy-prompt-p = t and proxy-recompute-header-p 
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
		     (setq recompute-header-p t)))
		 (recoverable-unauthorized-proxy-access
		   (cond)
		   (multiple-value-setq (,proxy-authorization-var proxy-found-in-cache-p)
		     (client-authenticate-user cond :recompute-authorization-header-p proxy-recompute-header-p
					       :prompt-p proxy-prompt-p :stream *query-io*))
		   (incf proxy-tries)
		   (unless ,proxy-authorization-var
		     (return nil))
		   (setq proxy-prompt-p (if (or proxy-recompute-header-p (not proxy-found-in-cache-p)) :reprompt t))
		   (unless proxy-recompute-header-p
		     (setq proxy-recompute-header-p t))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.91")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

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

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.91")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

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


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.91")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun %post-url (url vector headers stream)
  (flet ((send-data (remote-stream vector content-length)
	   (with-binary-stream (remote-stream :output)
	     (write-vector remote-stream vector 0 content-length)
	     (force-output remote-stream))))
    (handler-case
      (let* ((content-length (fill-pointer vector))
	     (outgoing-headers `(,@headers :content-type (:application :x-www-form-urlencoded)
				 :content-length ,content-length)))
	(handling-redirects (url)
	  (handling-authentication (authorization proxy-authorization)
	    (with-http-request
	      (url :post
		   :request-headers (compute-standard-request-headers
				      url :authorization authorization :proxy-authorization proxy-authorization
				      :header-plist outgoing-headers)
		   :request-body (send-data remote-stream vector content-length))
	      (case (client-status client)
		(200
		  (destructuring-bind (major-type minor-type &key &allow-other-keys)
		      (get-header :content-type)
		    (display url major-type minor-type remote-stream stream)))
		(204
		  (values url nil))
		(401
		  (destructuring-bind (&optional authentication-method . realm)
		      (get-header :WWW-Authenticate headers)
		    (flush-input-entity (client-stream client) headers http-version)
		    (error 'recoverable-unauthorized-client-access
			   :url url
			   :method (client-method client)
			   :authentication-method authentication-method
			   :authentication-realm realm)))
		(t (client-signal-http-code url (client-status client) :post :reason (client-reason client)
					    :version http-version)))))))
      (http-condition (cond) (www-utils:report-condition cond stream)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.91")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

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
				      `(:transfer-encoding :chunked)))))
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
		    (setq content-location (intern-url content-location :if-does-not-exist :create)))
		  (case status
		    (200 (with-transfer-decoding* (remote-stream url http-version :headers *headers*)
			   (destructuring-bind (major-type minor-type)
			       (get-header :content-type)
			     (display (or content-location url) major-type minor-type remote-stream stream)))))
		  (values (or content-location url) keyword content-version last-modified)))))))
      (http-condition (cond) (www-utils:report-condition cond stream)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.91")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

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


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.91")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

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


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.91")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

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


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.91")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

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


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;FLOGGER.LISP.53")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod access-url ((transaction-control post-transaction-control) (http-method (eql :post)) http-version persistent-connections-p report-stream)
  (declare (ignore http-version persistent-connections-p))
  (flet ((send-data (remote-stream vector content-length)
           (with-binary-stream (remote-stream :output)
	     (write-vector remote-stream vector 0 content-length)
	     (force-output remote-stream))))
    (let* ((url (transaction-control-url transaction-control))
	   (headers (transaction-control-post-headers transaction-control))
	   (vector (transaction-control-form-data-vector transaction-control)))
      (handler-case
	(handling-redirects (url)
	  (handling-authentication (authorization proxy-authorization)
	    (with-http-request
	      (url :post
		   :request-headers (compute-standard-request-headers url :authorization authorization
								      :proxy-authorization proxy-authorization :header-plist headers)
		   :request-body (send-data remote-stream vector (fill-pointer vector)))
	      (with-status-code-dispatch (:client client :url url :status (client-status client)
						  :success-status-codes (200)
						  :exceptions-flush-entities t
						  :http-version http-version) 
		(with-transfer-decoding* (remote-stream url http-version :headers *headers*)
		  (advance-input-buffer remote-stream))))))
	(http-condition (cond) (www-utils:report-condition cond report-stream))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;FLOGGER.LISP.53")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod access-url (transaction-control (http-method (eql :get)) http-version persistent-connections-p report-stream)
  (declare (ignore http-version persistent-connections-p report-stream))
  (let ((url (transaction-control-url transaction-control))
	(headers (transaction-control-headers transaction-control)))
    (handling-redirects (url)
      (handling-authentication (authorization proxy-authorization)
	(with-http-request
	  (url :get :request-headers (compute-standard-request-headers url :authorization authorization
								       :proxy-authorization proxy-authorization :header-plist headers))
	  (with-status-code-dispatch (:client client :url url :status (client-status client)
					      :success-status-codes (200 203 205 206)
					      :exceptions-flush-entities t
					      :http-version http-version) 
	    (with-transfer-decoding* (remote-stream url http-version :headers *headers*)
	      (advance-input-buffer remote-stream))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;FLOGGER.LISP.53")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod access-url (transaction-control (http-method (eql :head)) http-version persistent-connections-p report-stream)
  (declare (ignore http-version persistent-connections-p report-stream))
  (let ((url (transaction-control-url transaction-control))
	(headers (transaction-control-headers transaction-control)))
    (handling-redirects (url)
      (handling-authentication (authorization proxy-authorization)
	(with-http-request
	  (url :head 
	       :request-headers (compute-standard-request-headers url :authorization authorization
								  :proxy-authorization proxy-authorization :header-plist headers))
	  remote-stream				;ignore 
	  (with-status-code-dispatch (:client client :url url :status (client-status client)
					      :exceptions-flush-entities nil :http-version http-version)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.91")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

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
	      (format display-stream "~&Status Code: ~D (~A)~%Server Version: ~(~A~)" status (get-string-for-status-code status) http-version)
	      (fresh-line display-stream) 
	      (print-headers display-stream *headers*)
	      (terpri display-stream)
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


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.91")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod copy-file ((from-url http-url) (to-pathname pathname) &key copy-mode request-headers report-stream &allow-other-keys)
  (%with-open-url (from-url request-headers)
    (with-status-code-dispatch (:client client :url from-url :status (client-status client)
                                        :success-status-codes (200 203 205 206)
                                        :exceptions-flush-entities t) 
      (let ((response-headers (client-response-headers client))
            (to-file (make-pathname :name (or (pathname-name to-pathname) (url:object from-url))
                                    :type (or (pathname-type to-pathname) (url:extension from-url))
                                    :version (or (pathname-version to-pathname) :newest)
                                    :defaults to-pathname))
            last-modified)
        (with-header-values (content-type) response-headers
	  (when report-stream
	    (format report-stream "~&Copying ~A to ~A...." from-url to-pathname))
          (multiple-value-prog1
            (with-open-file (file to-file :direction :output :if-does-not-exist :create :if-exists :supersede
                                  :element-type (ecase (or copy-mode 
                                                           (mime-content-type-copy-mode content-type))
                                                  (:text *standard-character-type*)
                                                  ((:binary :crlf) '(unsigned-byte 8))))
              (with-transfer-decoding* (remote-stream from-url http-version :headers response-headers)
                (stream-copy-until-eof remote-stream file copy-mode)))
            (autotype-file to-pathname)
            (when (setq last-modified (get-header :last-modified response-headers))
              (set-file-modification-date to-file last-modified nil))
	    (when report-stream
	      (format report-stream "~&Copied ~A to ~A." from-url to-pathname))))
        to-file))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.91")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

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

