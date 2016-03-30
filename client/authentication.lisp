;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-

;;; (C) Copyright 1996-2001, 2003 John C.Mallery.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; CLIENT AUTHENTICATION
;;; 

(in-package :http)

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

(defmacro with-pw-realm-key ((url-or-proxy realm &key (variable-name 'realm-key) (stack-cons-p t)) &body body)
  `(let ((,variable-name (list (etypecase ,url-or-proxy 
				 (url (url:host-string ,url-or-proxy))
				 (proxy (proxy-domain-name ,url-or-proxy)))
			       ,realm)))
     ,@(when stack-cons-p `((declare (dynamic-extent ,variable-name))))
     ,@body))

(defgeneric pw-data (url realm method)
  (declare (values user-name pw)))

(defmethod pw-data (url-or-proxy (realm cons) method)
  (destructuring-bind (realm-name &key &allow-other-keys) realm
    (pw-data url-or-proxy realm-name method)))

(defmethod pw-data ((url url:http-url) (realm string) (method (eql :basic)))
  (let (entry)
    (when (setq entry (with-pw-realm-key (url realm)
			(gethash realm-key *realm-username-table*)))
      (decode-realm-username realm entry))))

(defmethod pw-data ((url url:http-url) (realm string) (method (eql :digest)))
  (let ((table *realm-username-table*)
	entry)
    (when (setq entry (or (gethash (url:name-string url) table)
			  (gethash (url:directory-context-string url) table)
			  (gethash (url:root-context-string url) table)
			  (with-pw-realm-key (url realm)
			    (gethash realm-key table))))
      (decode-realm-username realm entry))))

(defmethod pw-data ((proxy http-proxy) (realm string) (method (eql :digest)))
  (let ((table *realm-username-table*)
	entry)
    (when (setq entry (or (gethash proxy table)
			  (with-pw-realm-key (proxy realm)
			    (gethash realm-key table))))
      (decode-realm-username realm entry))))

(defgeneric cache-pw-data (url realm method user-name password))

(defmethod cache-pw-data ((url string) realm method user-name password)
  (cache-pw-data (url:intern-url url :if-does-not-exist :create) realm method user-name password))

(defmethod cache-pw-data (url (realm realm) method user-name password)
  (cache-pw-data url (realm-name realm) method user-name password))

(defmethod cache-pw-data ((url url:http-url) (realm string) (method (eql :basic)) user-name password)
  (let ((entry (encode-realm-username realm user-name password)))
    (with-pw-realm-key (url realm :stack-cons-p nil)
      (setf (gethash realm-key *realm-username-table*) entry))))

(defmethod cache-pw-data (url (realm cons) (method (eql :basic)) user-name password)
  (destructuring-bind (realm-name &key &allow-other-keys) realm
    (cache-pw-data url realm-name method user-name password)))

(defmethod cache-pw-data (url (realm string) (method (eql :digest)) user-name password)
  (let ((entry (encode-realm-username realm user-name password))
	(table *realm-username-table*))
    (with-pw-realm-key (url realm :stack-cons-p nil)
      (setf (gethash (url:name-string url) table) entry
	    (gethash (url:directory-context-string url) table) entry
	    (gethash (url:root-context-string url) table) entry
	    (gethash realm-key table) entry))))

(defmethod cache-pw-data (url (realm cons) (method (eql :digest)) user-name password)
  (destructuring-bind (realm-name &key domain &allow-other-keys) realm
    (when domain
      (let ((entry (encode-realm-username realm-name user-name password))
	    (table *realm-username-table*))
	(dolist (uri domain)
	  (setf (gethash uri table) entry))))
    (cache-pw-data url realm-name method user-name password)))

(defmethod cache-pw-data ((proxy http-proxy) (realm string) (method (eql :digest)) user-name password)
  (let ((entry (encode-realm-username realm user-name password))
	(table *realm-username-table*))
    (with-pw-realm-key (proxy realm :stack-cons-p nil)
      (setf (gethash proxy table) entry
	    (gethash realm-key table) entry))))

(defmethod cache-pw-data ((proxy http-proxy) (realm cons) (method (eql :digest)) user-name password)
  (destructuring-bind (realm-name &key domain &allow-other-keys) realm
    (when domain
      (let ((entry (encode-realm-username realm-name user-name password))
	    (table *realm-username-table*))
	(dolist (uri domain)
	  (setf (gethash uri table) entry))))
    (cache-pw-data proxy realm-name method user-name password)))

(defgeneric set-proxy-authentication-credentials (proxy method realm user-name password)
  (:documentation "Sets the authentication credentials for the access-controlled upstream proxy, PROXY.
REALM is a string denoting the remote realm to which user-name and password apply.
USER-NAME and PASSWORD are strings."))

(defmethod set-proxy-authentication-credentials ((proxy http-proxy) method realm user-name password)
  (cache-pw-data proxy realm method user-name password))

(defgeneric get-user-name+pw (uri realm method proxy-p prompt-p &optional stream)
  (declare (values user-name password))
  (:documentation "Returns the USER-ID and PASSWORD for URI given REALM
and authentication method, METHOD. When PROMPT-P is null and no cached
password is available for realm, an ACCESS-WITHOUT-CREDENTIALS condition
is signalled. When PROMPT-P is :REPROMPT, the user is unconditionally
queried for a password, which is recached. Otherwise, when PROMPT-P is non-null,
the user is queried only when no cached password is available. STREAM is
the stream for interacting with the user."))

;; the default method just signals the conditions because prompting the user
;; requires the base client 11/1/2001 -- JCMa.
(defmethod get-user-name+pw (uri realm method proxy-p reprompt-p &optional (stream *terminal-io*))
  (declare (ignore reprompt-p stream))
  (let ((client *client*))
    (cond (client
	   (error (if proxy-p 'recoverable-unauthorized-proxy-access 'recoverable-unauthorized-client-access)
		  :url uri
		  :method (client-method client)
		  :authentication-method method
		  :authentication-realm realm
		  :headers *headers*
		  :reason "No user to prompt for password."
		  :version (client-request-version client)))
	  (t (error 'recoverable-unauthorized-client-access
		    :url uri :authentication-method method :authentication-realm realm :headers *headers*)))))

;; this can receive a realm that is either a cons or a string
(defmethod get-user-name+pw :around ((uri url:http-uri) realm method proxy-p prompt-p &optional (stream *query-io*))
  (flet ((proxy-pw-key (uri) 
	   (or (get-client-proxy uri)
	       (error "Client associates no proxy with ~S." (url:name-string uri)))))
    (declare (inline proxy-pw-key))
    (let ((pw-cache-key (if proxy-p (proxy-pw-key uri) uri)))
      (unless (eql prompt-p :reprompt)
	(multiple-value-bind (user-name password)
	    (pw-data pw-cache-key realm method)
	  (when (and user-name password)
	    (return-from get-user-name+pw (values user-name password)))))
      ;; if no cache hit, then prompt the user.
      (multiple-value-bind (user-name password)
	  (call-next-method uri realm method proxy-p prompt-p stream)
	(cache-pw-data pw-cache-key realm method user-name password)
	(values user-name password)))))

(defmethod get-user-name+pw ((uri url:http-uri) (realm cons) (method (eql :basic)) proxy-p prompt-p &optional (stream *query-io*))
  (destructuring-bind (realm-name) realm
    (get-user-name+pw uri realm-name method proxy-p prompt-p stream)))

(defmethod get-user-name+pw ((uri url:http-uri) (realm cons) (method (eql :digest)) proxy-p prompt-p &optional (stream *query-io*))
  (destructuring-bind (realm-name &rest plist) realm
    (declare (ignore plist))
    (get-user-name+pw uri realm-name method proxy-p prompt-p stream)))

(define-variable *basic-authorization-header-cache* (make-hash-table :test #'equal))

(define clear-authentication-caches ()
  "Clears all authentication caches."
  (clrhash *basic-authorization-header-cache*)
  (clrhash *realm-username-table*)
  (map-client-proxies #'proxy-clear-authorization)
  (map-url-table #'(lambda (key url)
		     (declare (ignore key))
		     (clear-authorization-header-cache url))))

(defgeneric cache-authorization-header-for-realm (uri method authentication-method realm proxy-p authorization-header)
  (declare (values authorization-header))
  (:documentation "Caches AUTHORIZATION-HEADER for future use with URI, HTTP-METHOD, AUTHENTICATION-METHOD, or REALM."))

(defmethod cache-authorization-header-for-realm (uri method (authentication-method (eql :basic)) (realm string) proxy-p authorization-header)
   (declare (ignore uri method proxy-p))
   (setf (gethash realm *basic-authorization-header-cache*) authorization-header)
   authorization-header)

(defmethod cache-authorization-header-for-realm ((uri url:http-uri) method (authentication-method (eql :digest)) realm proxy-p authorization-header)
  (declare (ignore realm ))
  (let* ((indicator (if proxy-p :proxy-authorization-header-cache-plist :authorization-header-cache-plist))
	 (entry (get-value uri indicator)))
    (cond (entry
	   (setf (getf entry method) authorization-header))
	  (t (setf (get-value uri indicator) `(,method ,authorization-header)))))
  authorization-header)

(defgeneric recall-authorization-header-for-realm (uri method authentication-method realm proxy-p)
  (declare (values authorization-header found-p)))

(defmethod recall-authorization-header-for-realm (uri method (authentication-method (eql :basic)) realm proxy-p)
  (declare (ignore uri method proxy-p))
  (destructuring-bind (realm-name) realm
    (gethash realm-name *basic-authorization-header-cache*)))

(defmethod recall-authorization-header-for-realm ((uri url:http-uri) method (authentication-method (eql :digest)) realm proxy-p)
  (declare (ignore realm))
  (let* ((indicator (if proxy-p :proxy-authorization-header-cache-plist :authorization-header-cache-plist))
	 (entry (get-value uri indicator)))
    (when entry
      (getf entry method))))

(defgeneric invalidate-authorization-header-cache (uri method authentication-method realm proxy-p))

(defmethod invalidate-authorization-header-cache (uri method (authentication-method (eql :basic)) realm proxy-p)
  (declare (ignore uri method proxy-p))
  (destructuring-bind (realm-name) realm
    (remhash realm-name *basic-authorization-header-cache*)
    (remhash realm-name *realm-username-table*)))

(defmethod invalidate-authorization-header-cache ((uri url:http-uri) method (authentication-method (eql :digest)) realm proxy-p)
  (declare (ignore realm))
  (let* ((indicator (if proxy-p :proxy-authorization-header-cache-plist :authorization-header-cache-plist))
	 (entry (get-value uri indicator)))
    (when entry
      (remf entry method)
      (unless entry
	(remove-value uri indicator)))))

(defgeneric clear-authorization-header-cache (uri)
  (:documentation "Clears the authorization header cache on URI."))

(defmethod clear-authorization-header-cache ((uri url:uri))
  (remove-value uri :proxy-authorization-header-cache-plist)
  (remove-value uri :authorization-header-cache-plist))

(defun compute-basic-authorization-header (user-id pw)
  (let ((hash (concatenate 'string user-id ":" pw)))
    (declare (dynamic-extent hash))
    `(:basic ,(base64:base64-encode-vector hash :characters-p t))))

(defun compute-digest-authorization-header (uri http-method realm-name proxy-p user-id pw algorithm nonce opaque)
  (let* ((digest-fctn (algorithm-digest-function algorithm))
	 (user-realm-pw (concatenate 'string user-id ":" realm-name ":" pw))
	 (user-realm-pw-digest (funcall digest-fctn user-realm-pw))
         (escaped-uri (if proxy-p (name-string uri) (relative-name-string uri)))
	 (uri-method (concatenate 'string (symbol-name http-method) ":" escaped-uri))
	 (uri-method-digest (funcall digest-fctn uri-method))
	 (response (concatenate 'string user-realm-pw-digest ":" nonce ":" uri-method-digest))
	 (response-digest (funcall digest-fctn response)))
    (declare (dynamic-extent user-realm-pw user-realm-pw-digest uri-method uri-method-digest response))
    `(:digest :username ,user-id :realm ,realm-name :uri ,escaped-uri :response ,response-digest
      :nonce ,nonce :opaque ,opaque :algorithm ,algorithm)))

(define-generic get-authorization-header (uri http-method authentication-method realm proxy-p &optional recompute-p prompt-p stream)
  (declare (values authorization-header found-in-cache-p))
  (:documentation "Returns a header plist suitable for returning with the authorization retry."))

(defmethod get-authorization-header ((uri url:http-uri) http-method method realm proxy-p &optional recompute-p prompt-p stream)
  (declare (ignore http-method realm stream proxy-p recompute-p prompt-p))
  (error "~S is an unknown authorization method for an HTTP URI." method))

(defmethod get-authorization-header ((uri string) http-method method realm proxy-p &optional recompute-p prompt-p (stream *query-io*))
  (get-authorization-header (url:intern-url uri :if-does-not-exist :error) http-method method realm proxy-p recompute-p prompt-p stream))

(defmethod get-authorization-header ((uri url:http-uri) http-method (method (eql :basic)) realm proxy-p
				     &optional recompute-p prompt-p (stream *query-io*))
  (declare (ignore recompute-p))
  (multiple-value-bind (user-id pw)
      (get-user-name+pw uri realm method proxy-p prompt-p stream)
    (when (and user-id pw)
      (destructuring-bind (realm-name) realm
	(cache-authorization-header-for-realm uri http-method method realm-name proxy-p 
					      (compute-basic-authorization-header user-id pw))))))

(defmethod get-authorization-header ((uri url:http-uri) http-method (method (eql :digest)) realm proxy-p
				     &optional recompute-p prompt-p (stream *query-io*))
  (declare (ignore recompute-p))
  (multiple-value-bind (user-id pw)
      (get-user-name+pw uri realm method proxy-p prompt-p stream)
    (when (and user-id pw)
      (destructuring-bind (realm-name &key (algorithm :md5) nonce opaque &allow-other-keys) realm
	(cache-authorization-header-for-realm
	  uri http-method method realm proxy-p	;pass down the realm plist because other method may want domain 10/1/2003 -- JCMa.
	  (compute-digest-authorization-header uri http-method realm-name proxy-p user-id pw algorithm nonce opaque))))))

(defmethod get-authorization-header :around ((uri url:http-uri) http-method method realm proxy-p
					     &optional recompute-p prompt-p (stream *query-io*))
  (cond (recompute-p
	 (invalidate-authorization-header-cache uri http-method method realm proxy-p)
	 (call-next-method uri http-method method realm proxy-p recompute-p prompt-p stream))
	;; returns the header and found-in-cache-p
	((recall-authorization-header-for-realm uri http-method method realm proxy-p))
	(t (call-next-method uri http-method method realm proxy-p recompute-p prompt-p stream))))

(defgeneric client-authenticate-user (condition &key recompute-authorization-header-p prompt-p stream)
  (declare (values authorization-header found-in-cache-p))
  (:documentation "Returns an authorization header suitable for authenicating the user making an HTTP request.
CONDITION is an unauthorized access condition. RECOMPUTE-AUTHORIZATION-HEADER-P controls whether a cached
authorization header can be used. PROMPT-P controls whether the user is queried for a password. When
PROMPT-P :REPROMPT, the user is queried regardless of whether a cached password is available. STREAM is
the query stream."))

(defmethod client-authenticate-user ((condition recoverable-unauthorized-access) &key recompute-authorization-header-p
				     prompt-p (stream *query-io*))
  (let ((url (http-url condition))
	(http-method (http-method condition))
	(authentication-method (http-authentication-method condition))
	(realm (http-authentication-realm condition)))
    (get-authorization-header url http-method authentication-method realm nil 
                              (or recompute-authorization-header-p (http-recompute-authorization-header-p condition)) prompt-p stream)))

(defmethod client-authenticate-user ((condition recoverable-unauthorized-proxy-access) &key recompute-authorization-header-p
				     prompt-p (stream *query-io*))
  (let ((url (http-url condition))
	(http-method (http-method condition))
	(authentication-method (http-authentication-method condition))
	(realm (http-authentication-realm condition)))
    (get-authorization-header url http-method authentication-method realm t 
                              (or recompute-authorization-header-p (http-recompute-authorization-header-p condition)) prompt-p stream)))

(defgeneric proxy-compute-authorization-header (http-proxy method http-method uri)
  (:documentation "Returns an authorization header suitable for an HTTP-METHOD request for URI to HTTP-PROXY
using METHOD for authentication."))

(defmethod proxy-compute-authorization-header ((proxy proxy-authentication-mixin) (method (eql :basic)) http-method uri)
  (declare (ignore http-method uri))
  (proxy-authentication-header proxy))

(defmethod proxy-compute-authorization-header ((proxy proxy-authentication-mixin) (method (eql :digest)) http-method uri)
  (destructuring-bind (authentication-method &key realm algorithm nonce opaque &allow-other-keys) (proxy-authentication-header proxy)
    authentication-method
    (multiple-value-bind (user-id pw)
	(get-user-name+pw uri realm method t nil)  
      (when (and user-id pw)
	(compute-digest-authorization-header uri http-method realm t user-id pw algorithm nonce opaque)))))

(defun proxy-get-authorization-header (proxy http-method uri)
  (let ((method (proxy-authentication-method proxy)))
    (when method
      (proxy-compute-authorization-header proxy method http-method uri))))

#+ignore
(defun proxy-note-authorization-header (proxy authorization-header)
  (destructuring-bind (method . args) authorization-header
    args
    (without-preemption
      (setf (proxy-authentication-method proxy) method
	    (proxy-authentication-header proxy) authorization-header))))

; version introduced with LispWorks6, do we really need WITHOUT-PREEMPTION?
(defun proxy-note-authorization-header (proxy authorization-header)
  (destructuring-bind (method . args) authorization-header
    args
    (setf (proxy-authentication-method proxy) method
          (proxy-authentication-header proxy) authorization-header)))

(defgeneric proxy-clear-authorization (proxy)
  (:documentation "Clears any proxy authorization caches for PORXY."))

(defmethod proxy-clear-authorization ((proxy proxy-authentication-mixin))
  (setf (proxy-authentication-method proxy) nil
	(proxy-authentication-header proxy) nil))

(defmethod proxy-clear-authorization ((proxy basic-proxy-mixin))
  nil)

(defparameter *proxy-authentication-retries* 1
  "Controls the number of retries for to authenticate a proxy connection before giving up.")

(define-macro handling-proxy-authentication ((proxy-authorization-var) &body body)
  "Handles authentication by rerunning body with AUTHENTICATION-VAR bound
to a user-specified authentication."
  `(let (proxy-recompute-header-p ,proxy-authorization-var)
     (flet ((handle-unauthorized-proxy-access (cond)
	      (handler-case
		(multiple-value-setq (,proxy-authorization-var)
		  (client-authenticate-user cond :recompute-authorization-header-p proxy-recompute-header-p :prompt-p nil))
		(proxy-access-without-credentials () (return-from handle-unauthorized-proxy-access nil)))
	      (unless-every
		(,proxy-authorization-var (return-from handle-unauthorized-proxy-access nil))
		(proxy-recompute-header-p (setq proxy-recompute-header-p t)))
	      (throw 'retry-with-proxy-authentication t)))
       (declare (dynamic-extent #'handle-unauthorized-proxy-access))
       (loop with retry-limit fixnum = *proxy-authentication-retries*
	     for proxy-tries fixnum upfrom 0
	     doing (catch 'retry-with-proxy-authentication
		     (handler-bind-if (< proxy-tries retry-limit)
			((recoverable-unauthorized-proxy-access #'handle-unauthorized-proxy-access))
		       (return (progn . ,body))))))))

;;;------------------------------------------------------------------- 
;;;
;;; HANDLE AUTHENTICATION
;;;

(defparameter *prompt-user-for-passwords* t
  "Controls whether the user is prompted for passwords.")

(defmacro without-password-prompting (() &body body)
  `(let ((*prompt-user-for-passwords* nil)) . ,body))

(defmethod get-user-name+pw ((url url:http-url) (realm string) method proxy-p reprompt-p &optional (stream *terminal-io*))
  (declare (ignore reprompt-p))
  (cond (*prompt-user-for-passwords*
	 (let ((string (url:name-string url)))
	   #-(or Genera MCL LispWorks)		;obsolete user prompt fails to display proxy and other info 9/7/2001 -- JCMa.
	   (%get-user-name+pw string stream)
	   #+(or Genera MCL LispWorks)
	   (%get-user-name+pw string realm method proxy-p stream)))
	(t (call-next-method))))		;pass control to default method, which signals

(define-parameter *number-of-authentication-retries* 5
		  "The number of times to reprompt the user for bad passwords.")

;; Used to throw into a loop when user supplied wrong password, fixed.  -cvince 8/30/96
(define-macro handling-authentication ((authorization-var proxy-authorization-var &key client-authentication) &body body)
  "Handles client authentication by rerunning body with AUTHENTICATION-VAR bound
to a user-specified authentication."
  `(loop with ,authorization-var ,@(when client-authentication `(= , client-authentication))
         and found-in-cache-p and prompt-p = t and recompute-header-p
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