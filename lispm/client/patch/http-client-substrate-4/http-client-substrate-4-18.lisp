;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for HTTP-CLIENT-SUBSTRATE version 4.18
;;; Reason: Function (CLOS:METHOD HTTP:INVOKE-HTTP-SERVICE (HTTP::BASIC-CLIENT-MIXIN T T T)):  detect and signal unauthorized proxy access.
;;; Written by JCMa, 9/07/01 18:48:58
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.6,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.146,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.4, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.35, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, HTTP Proxy Server 6.25,
;;; HTTP Client Substrate 4.17, Statice Server 466.2, HTTP Client 50.11,
;;; Image Substrate 440.4, Essential Image Substrate 433.0, Jcma 44,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.16,
;;; DEC OSF/1 V4.0 (Rev. 110),
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
;;; Increase disk wired wait timeout from 30 to 900 seconds (from W:>Reti>disk-wait-900-patch.lisp.2),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1).

(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:CLIENT;CLIENT.LISP.287"
  "HTTP:CLIENT;SEXP-BROWSER.LISP.85")



;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(SCL:FUNDEFINE 'GET-USER-NAME+PW)

(SCL:FUNDEFINE 'CACHE-AUTHORIZATION-HEADER-FOR-REALM)

(SCL:FUNDEFINE 'RECALL-AUTHORIZATION-HEADER-FOR-REALM)

(SCL:FUNDEFINE 'INVALIDATE-AUTHORIZATION-HEADER-CACHE)

(SCL:FUNDEFINE 'GET-AUTHORIZATION-HEADER)

(SCL:FUNDEFINE '(METHOD CLIENT-AUTHENTICATE-USER (UNAUTHORIZED-ACCESS)))



;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CLIENT.LISP.287")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod invoke-http-service ((client basic-client-mixin) method header-writer response-handler
				&optional request-entity-generator (http-version *client-http-version*))
  (flet ((trace-request (url method version header-writer &aux (trace-stream *trace-output*))
	   (let ((proxy (client-proxy client)))
	     (if proxy
		 (fast-format trace-stream "~&Proxy: ~A (~D)" (proxy-host proxy) (proxy-port proxy))
		 (fast-format trace-stream "~&Host: ~A" (host-string url)))
	     (fast-format trace-stream "~%Request: ~A ~I ~A~%Request Headers: ~I" 
			  method (url:write-name url trace-stream) version
			  (%write-request-headers trace-stream header-writer method version))))
	 (signal-unauthorized-proxy-access (url method http-version headers stream)
	   (destructuring-bind (&optional authentication-method . realm)
	       (get-header :proxy-authenticate headers)
	     (flush-input-entity stream headers http-version)
	     (error 'recoverable-unauthorized-proxy-access :url url :method method 
		    :authentication-method authentication-method :authentication-realm realm))))
    (with-standard-client-io-syntax ()
      (with-current-connection (client :http-version http-version :connection-var connection :block-name invoke-http-service)
	(multiple-value-bind (connection-version connection-version-confirmed-p http-plist)
	    (connection-confirmed-version connection)
	  (let ((url (client-url client))
		(request-version (if connection-version-confirmed-p (http-version-min connection-version http-version) connection-version))
		(stream (connection-stream connection)))
	    (setf (client-method client) method
		  (%client-request-version client) request-version)
	    ;; send a request to the remote host
	    (send-request stream method url request-version header-writer (client-proxy client))
	    (when *trace-client* (trace-request url method request-version header-writer))
	    (when request-entity-generator	;Send the request body when provided
	      (case request-version
		((:http/1.0 :http/0.9)		;1.0 remote server, just send the data.
		 (funcall request-entity-generator client stream request-version)
		 (force-output stream))
		(t (let (100-continue)
		     (multiple-value-bind (reply reply-length)
			 (cond (connection-version-confirmed-p
				(ecase (setq 100-continue (getf http-plist :100-continue :unknown))
				  (:implemented
				    (read-reply-line stream (client-reply-line client)))
				  (:unknown
				    (with-timeout (*client-await-continue-reply-timeout* :error-p nil)
				      (read-reply-line stream (client-reply-line client))))
				  (:not-implmented
				    (values nil nil))))
			       (t (with-timeout (*client-await-continue-reply-timeout* :error-p nil)
				    (read-reply-line stream (client-reply-line client)))))
		       (cond (reply
			      (multiple-value-bind (status server-version reason-offset)
				  (parse-reply reply reply-length)
				(unless connection-version-confirmed-p
				  (note-server-http-version connection server-version)
				  (setq connection-version-confirmed-p t))
				(setf (client-status client) status
				      (client-reason-offset client) reason-offset)
				(with-headers-for-client (client stream request-version)	; handles update of connection version
				  (case status
				    (100
				      (funcall request-entity-generator client stream request-version)
				      (force-output stream)
				      (unless (eq 100-continue :implemented)
					(note-server-http-property connection :100-continue :implemented))
				      (clear-header-set (client-response-headers client) t))	; clear continue headers, if any
				    (407	;proxy authentication
				     (signal-unauthorized-proxy-access url (client-method client) server-version *headers* (client-stream client)))
				    (417 (signal 'expectation-failed :url url :version server-version))
				    ;; Request was rejected. Handle the rejection response
				    (t (return-from invoke-http-service
					 (multiple-value-prog1	;must return the values returned by the handler
					   (funcall response-handler client stream request-version))))))))
			     (t (funcall request-entity-generator client stream request-version)
				(force-output stream)
				(unless (eq 100-continue :not-implmented)
				  (note-server-http-property connection :100-continue :not-implmented)))))))))
	    (tagbody				;Handle the primary server response
	      read-next-reply
		 (multiple-value-bind (reply reply-length)
		     (read-reply-line stream (client-reply-line client))
		   (multiple-value-bind (status server-version reason-offset)
		       (parse-reply reply reply-length)
		     (unless (and connection-version-confirmed-p (eq server-version connection-version))	;detect version changes
		       (note-server-http-version connection server-version))
		     (setf (client-status client) status
			   (client-reason-offset client) reason-offset)
		     (with-headers-for-client (client stream request-version)	; handles update of connection version
		       (case status
			 (100 (go read-next-reply))
			 (407 (signal-unauthorized-proxy-access url (client-method client) server-version *headers* (client-stream client)))
			 (417 (signal 'expectation-failed :url url :version server-version))
			 (t (return-from invoke-http-service
			      (multiple-value-prog1	;must return the values returned by the handler
				(funcall response-handler client stream request-version)))))))))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
; From buffer authentication.lisp >http>client W: (1)
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(PROGN
;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-

;;; (C) Copyright 1996-2001, John C.Mallery.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; CLIENT AUTHENTICATION
;;; 

(defvar *realm-username-table* (make-hash-table :test #'equalp))

(defgeneric encode-realm-username (realm user-name password))

(defmethod encode-realm-username (realm user-name password)
  (declare (ignore realm))
  `(,user-name ,password))

(defgeneric decode-realm-username (realm entry)
  (declare (values user-name pw)))

(defmethod decode-realm-username (realm entry)
  (declare (ignore realm))
  (values-list entry))

(defgeneric pw-data (url realm method)
  (declare (values user-name pw)))

(defmethod pw-data (url (realm string) (method (eql :basic)))
  (declare (ignore url))
  (let (entry)
    (when (setq entry (gethash realm *realm-username-table*))
      (decode-realm-username realm entry))))

(defmethod pw-data (url (realm cons) (method (eql :basic)))
  (destructuring-bind (realm-name &key &allow-other-keys) realm
    (pw-data url realm-name method)))

(defmethod pw-data ((url url:http-url) (realm cons) (method (eql :digest)))
  (destructuring-bind (realm-name &key &allow-other-keys) realm
    (let ((table *realm-username-table*)
	  entry)
      (when (setq entry (or (gethash (url:name-string url) table)
			    (gethash (url:directory-context-string url) table)
			    (gethash (url:root-context-string url) table)
			    (gethash realm table)))
	(decode-realm-username realm-name entry)))))

(defmethod pw-data ((proxy http-proxy) (realm cons) (method (eql :digest)))
  (destructuring-bind (realm-name &key &allow-other-keys) realm
    (let ((table *realm-username-table*)
	  entry)
      (when (setq entry (or (gethash proxy table)
			    (gethash realm table)))
	(decode-realm-username realm-name entry)))))

(defgeneric cache-pw-data (url realm method user-name password))

(defmethod cache-pw-data ((url string) realm method user-name password)
  (cache-pw-data (url:intern-url url :if-does-not-exist :create) realm method user-name password))

(defmethod cache-pw-data (url (realm realm) method user-name password)
  (cache-pw-data url (realm-name realm) method user-name password))

(defmethod cache-pw-data (url (realm string) (method (eql :basic)) user-name password)
  (declare (ignore url))
  (let ((entry (encode-realm-username realm user-name password)))
    (setf (gethash realm *realm-username-table*) entry)))

(defmethod cache-pw-data (url (realm cons) (method (eql :basic)) user-name password)
  (destructuring-bind (realm-name &key &allow-other-keys) realm
    (cache-pw-data url realm-name method user-name password)))

(defmethod cache-pw-data (url realm (method (eql :digest)) user-name password)
  (destructuring-bind (realm-name &key domain &allow-other-keys) realm
    (let ((entry (encode-realm-username realm-name user-name password))
	  (table *realm-username-table*))
      (dolist (uri domain)
	(setf (gethash uri table) entry))
      (setf (gethash (url:name-string url) table) entry
	    (gethash (url:directory-context-string url) table) entry
	    (gethash (url:root-context-string url) table) entry
	    (gethash realm-name table) entry))))

(defmethod cache-pw-data ((proxy http-proxy) realm (method (eql :digest)) user-name password)
  (destructuring-bind (realm-name &key domain &allow-other-keys) realm
    (let ((entry (encode-realm-username realm-name user-name password))
	  (table *realm-username-table*))
      (dolist (uri domain)
	(setf (gethash uri table) entry))
      (setf (gethash (gethash proxy table) table) entry
	    (gethash realm-name table) entry))))

(defgeneric get-user-name+pw (url realm method proxy-p reprompt-p &optional stream)
  (declare (values user-name password))
  (:documentation "Returns the USER-ID and PASSWORD for URL given REALM
and authentication method, METHOD, prompting for input on STREAM."))

(defmethod get-user-name+pw ((url url:http-url) (realm cons) (method (eql :basic)) proxy-p reprompt-p &optional (stream *terminal-io*))
  (destructuring-bind (realm-name) realm
    (get-user-name+pw url realm-name method proxy-p reprompt-p stream)))

(defmethod get-user-name+pw ((url url:http-url) (realm cons) (method (eql :digest)) proxy-p reprompt-p &optional (stream *terminal-io*))
  (destructuring-bind (realm-name &rest plist) realm
    (declare (ignore plist))
    (get-user-name+pw url realm-name method proxy-p reprompt-p stream)))

(defmethod get-user-name+pw :around ((url url:http-url) (realm cons) method proxy-p reprompt-p &optional (stream *terminal-io*))
  (flet ((proxy-pw-key (url) 
	   (or (get-client-proxy url)
	       (error "Client Associates no proxy with ~S." (url:name-string url)))))
    (let ((pw-cache-key (if proxy-p (proxy-pw-key url) url)))
      (unless reprompt-p
	(multiple-value-bind (user-name password)
	    (pw-data pw-cache-key realm method)
	  (when (and user-name password)
	    (return-from get-user-name+pw (values user-name password)))))
      ;; if no cache hit, then prompt the user.
      (multiple-value-bind (user-name password)
	  (call-next-method url realm method proxy-p reprompt-p stream)
	(cache-pw-data pw-cache-key realm method user-name password)
	(values user-name password)))))

(define-variable *basic-authorization-header-cache* (make-hash-table :test #'equal))

(define clear-authentication-caches ()
  "Clears all authentication caches."
  (clrhash *basic-authorization-header-cache*)
  (clrhash *realm-username-table*)
  (map-url-table #'(lambda (key url)
		     (declare (ignore key))
		     (clear-authorization-header-cache url))))

(defgeneric cache-authorization-header-for-realm (url method authentication-method realm proxy-p authorization-header)
  (declare (values authorization-header))
  (:documentation "Caches AUTHORIZATION-HEADER for future use with URL, HTTP-METHOD, AUTHENTICATION-METHOD, or REALM."))

(defmethod cache-authorization-header-for-realm (url method (authentication-method (eql :basic)) (realm string) proxy-p authorization-header)
   (declare (ignore url method proxy-p))
   (setf (gethash realm *basic-authorization-header-cache*) authorization-header)
   authorization-header)

(defmethod cache-authorization-header-for-realm ((url url:http-url) method (authentication-method (eql :digest)) realm proxy-p authorization-header)
  (declare (ignore realm ))
  (let* ((indicator (if proxy-p :proxy-authorization-header-cache-plist :authorization-header-cache-plist))
	 (entry (get-value url indicator)))
    (cond (entry
	   (setf (getf entry method) authorization-header))
	  (t (setf (get-value url indicator) `(,method ,authorization-header)))))
  authorization-header)

(defgeneric recall-authorization-header-for-realm (url method authentication-method realm proxy-p)
  (declare (values authorization-header found-p)))

(defmethod recall-authorization-header-for-realm (url method (authentication-method (eql :basic)) realm proxy-p)
  (declare (ignore url method proxy-p))
  (destructuring-bind (realm-name) realm
    (gethash realm-name *basic-authorization-header-cache*)))

(defmethod recall-authorization-header-for-realm ((url url:http-url) method (authentication-method (eql :digest)) realm proxy-p)
  (declare (ignore realm))
  (let* ((indicator (if proxy-p :proxy-authorization-header-cache-plist :authorization-header-cache-plist))
	 (entry (get-value url indicator)))
    (when entry
      (getf entry method))))

(defgeneric invalidate-authorization-header-cache (url method authentication-method realm proxy-p))

(defmethod invalidate-authorization-header-cache (url method (authentication-method (eql :basic)) realm proxy-p)
  (declare (ignore url method proxy-p))
  (destructuring-bind (realm-name) realm
    (remhash realm-name *basic-authorization-header-cache*)
    (remhash realm-name *realm-username-table*)))

(defmethod invalidate-authorization-header-cache ((url url:http-url) method (authentication-method (eql :digest)) realm proxy-p)
  (declare (ignore realm))
  (let* ((indicator (if proxy-p :proxy-authorization-header-cache-plist :authorization-header-cache-plist))
	 (entry (get-value url indicator)))
    (when entry
      (remf entry method)
      (unless entry
	(remove-value url indicator)))))

(defgeneric clear-authorization-header-cache (url)
  (:documentation "Clears the authorization header cache on URL."))

(defmethod clear-authorization-header-cache ((url url:http-url))
  (remove-value url :proxy-authorization-header-cache-plist)
  (remove-value url :authorization-header-cache-plist))

(define-generic get-authorization-header (url http-method authentication-method realm proxy-p &optional recompute-p reprompt-p stream)
  (declare (values authorization-header found-in-cache-p))
  (:documentation "Returns a header plist suitable for returning with the authorization retry."))

(defmethod get-authorization-header ((url url:http-url) http-method method realm proxy-p &optional recompute-p reprompt-p stream)
  (declare (ignore http-method realm stream proxy-p recompute-p reprompt-p))
  (error "~S is an unknown authorization method for an HTTP URL." method))

(defmethod get-authorization-header ((url string) http-method method realm proxy-p &optional recompute-p reprompt-p (stream *terminal-io*))
  (get-authorization-header (url:intern-url url :if-does-not-exist :error) http-method method realm proxy-p recompute-p reprompt-p stream))

(defmethod get-authorization-header ((url url:http-url) http-method (method (eql :basic)) realm proxy-p
				     &optional recompute-p reprompt-p (stream *terminal-io*))
  (declare (ignore recompute-p))
  (multiple-value-bind (user-id pw)
      (get-user-name+pw url realm method proxy-p reprompt-p stream)
    (when (and user-id pw)
      (destructuring-bind (realm-name) realm
	(let ((hash (concatenate 'string user-id ":" pw)))
	  (declare (dynamic-extent hash))
	  (cache-authorization-header-for-realm url http-method method realm-name proxy-p 
						`(:basic ,(base64:base64-encode-vector hash :characters-p t))))))))

(defmethod get-authorization-header ((url url:http-url) http-method (method (eql :digest)) realm proxy-p
				     &optional recompute-p reprompt-p (stream *terminal-io*))
  (declare (ignore recompute-p))
  (multiple-value-bind (user-id pw)
      (get-user-name+pw url realm method proxy-p reprompt-p stream)
    (when (and user-id pw)
      (destructuring-bind (realm-name &key algorithm nonce opaque &allow-other-keys) realm
	(let* ((digest-fctn (algorithm-digest-function algorithm))
	       (user-realm-pw (concatenate 'string user-id ":" realm-name ":" pw))
	       (user-realm-pw-digest (funcall digest-fctn user-realm-pw))
	       (uri-method (concatenate 'string (symbol-name http-method) ":" (relative-name-string url)))
	       (uri-method-digest (funcall digest-fctn uri-method))
	       (response (concatenate 'string user-realm-pw-digest ":" nonce ":" uri-method-digest))
	       (response-digest (funcall digest-fctn response)))
	  (declare (dynamic-extent user-realm-pw user-realm-pw-digest uri-method uri-method-digest response))
	  (cache-authorization-header-for-realm
	    url http-method method realm-name proxy-p
	    `(:digest :username ,user-id :realm ,realm-name :nonce ,nonce
		      :uri ,(name-string url) :response ,response-digest
		      :opaque ,opaque :algorithmm ,algorithm)))))))

(defmethod get-authorization-header :around ((url url:http-url) http-method method realm proxy-p
					     &optional recompute-p reprompt-p (stream *terminal-io*))
  (cond (recompute-p
	 (invalidate-authorization-header-cache url http-method method realm proxy-p)
	 (call-next-method url http-method method realm proxy-p recompute-p reprompt-p stream))
	;; returns the header and found-in-cache-p
	((recall-authorization-header-for-realm url http-method method realm proxy-p))
	(t (call-next-method url http-method method realm proxy-p recompute-p reprompt-p stream))))

(defgeneric client-authenticate-user (condition &key recompute-authorization-header-p prompt-for-password-p stream)
  (declare (values authorization-header found-in-cache-p)))

(defmethod client-authenticate-user ((condition recoverable-unauthorized-access) &key recompute-authorization-header-p
				     prompt-for-password-p (stream *terminal-io*))
  (let ((url (http-url condition))
	(http-method (http-method condition))
	(authentication-method (http-authentication-method condition))
	(realm (http-authentication-realm condition)))
    (get-authorization-header url http-method authentication-method realm nil recompute-authorization-header-p prompt-for-password-p stream)))

(defmethod client-authenticate-user ((condition recoverable-unauthorized-proxy-access) &key recompute-authorization-header-p
				     prompt-for-password-p (stream *terminal-io*))
  (let ((url (http-url condition))
	(http-method (http-method condition))
	(authentication-method (http-authentication-method condition))
	(realm (http-authentication-realm condition)))
    (get-authorization-header url http-method authentication-method realm t recompute-authorization-header-p prompt-for-password-p stream)))

)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.85")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;; Used to throw into a loop when user supplied wrong password, fixed.  -cvince 8/30/96
(define-macro handling-authentication ((authorization-var proxy-authorization-var) &body body)
  "Handles authentication by rerunning body with AUTHENTICATION-VAR bound
to a user-specified authentication."
  `(loop with ,authorization-var and found-in-cache-p and recompute-header-p and reprompt-p
	 with ,proxy-authorization-var and proxy-found-in-cache-p and proxy-recompute-header-p and proxy-reprompt-p
	 with tries fixnum = 0 and proxy-tries fixnum = 0 and retry-limit fixnum = *number-of-authentication-retries*
	 doing (handler-case-if (and (< tries retry-limit) (< proxy-tries retry-limit))
		  (return (progn . ,body))
		 (recoverable-unauthorized-client-access
		   (cond)
		   (multiple-value-setq (,authorization-var found-in-cache-p)
		     (client-authenticate-user cond :recompute-authorization-header-p recompute-header-p
					       :prompt-for-password-p reprompt-p :stream *query-io*))
		   (incf tries)
		   (unless ,authorization-var
		     (return nil))
		   (when (or recompute-header-p (not found-in-cache-p))
		     (setq reprompt-p t))
		   (unless recompute-header-p
		     (setq recompute-header-p t)))
		 (recoverable-unauthorized-proxy-access
		   (cond)
		   (multiple-value-setq (,proxy-authorization-var proxy-found-in-cache-p)
		     (client-authenticate-user cond :recompute-authorization-header-p proxy-recompute-header-p
					       :prompt-for-password-p proxy-reprompt-p :stream *query-io*))
		   (incf proxy-tries)
		   (unless ,proxy-authorization-var
		     (return nil))
		   (when (or proxy-recompute-header-p (not proxy-found-in-cache-p))
		     (setq proxy-reprompt-p t))
		   (unless proxy-recompute-header-p
		     (setq proxy-recompute-header-p t))))))

