;;; -*- Mode: lisp; Syntax: common-lisp; Package: user; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.76
;;; Reason: Update for latest HTTP 1.1 redirection status codes
;;; 
;;; DEFINE-CONDITION HTTP::DOCUMENT-FOUND:  rename in accordance with http 1.1
;;; DEFINE-CONDITION HTTP::ACCESS-VIA-PROXY:  new redirect class
;;; DEFINE-CONDITION HTTP:DOCUMENT-MOVED-PERMANENTLY:  http 1.1 307
;;; Variable HTTP::*STATUS-CODE-ALIST*:  update classes.
;;; DEFINE-CONDITION HTTP::DOCUMENT-MULTIPLE-CHOICES:  -
;;; Function HTTP::SIGNAL-STANDARD-URL-REDIRECT:  new.
;;; Function (CLOS:METHOD HTTP::INVOKE-SERVER-METHOD (HTTP::BASIC-SERVER-MIXIN (EQL :DELETE) SYMBOL)):  use signal-standard-url-redirect.
;;; Function (CLOS:METHOD HTTP::INVOKE-SERVER-METHOD (HTTP::BASIC-SERVER-MIXIN (EQL :POST) SYMBOL)):  -
;;; Function (CLOS:METHOD HTTP::INVOKE-SERVER-METHOD (HTTP::BASIC-SERVER-MIXIN (EQL :PUT) SYMBOL)):  -
;;; Function (CLOS:METHOD HTTP:WRITE-DOCUMENT (URL:HTTP-OBJECT (EQL :REDIRECT) T)):  -
;;; Function (CLOS:METHOD HTTP:WRITE-DOCUMENT (URL:HTTP-PATH (EQL :REDIRECT) T)):  -
;;; Function (CLOS:METHOD HTTP:WRITE-DOCUMENT (URL:HTTP-OBJECT (EQL :TEMPORARY-REDIRECT) T)):  -
;;; Function (CLOS:METHOD HTTP:WRITE-DOCUMENT-HEADERS (URL:HTTP-OBJECT (EQL :REDIRECT) T)):  -
;;; Function (CLOS:METHOD HTTP:WRITE-DOCUMENT-HEADERS (URL:HTTP-PATH (EQL :REDIRECT) T)):  -
;;; Function (CLOS:METHOD HTTP:WRITE-DOCUMENT (URL:HTTP-PATH (EQL :TEMPORARY-REDIRECT) T)):  -
;;; Function (CLOS:METHOD HTTP:WRITE-DOCUMENT-HEADERS (URL:HTTP-OBJECT (EQL :TEMPORARY-REDIRECT) T)):  -
;;; Function (CLOS:METHOD HTTP:WRITE-DOCUMENT-HEADERS (URL:HTTP-PATH (EQL :TEMPORARY-REDIRECT) T)):  -
;;; Function HTTP::%STANDARD-URL-REDIRECT:  -
;;; Function HTTP::STANDARD-URL-REDIRECT:  -
;;; Function (CLOS:METHOD HTTP:REDIRECT-REQUEST (HTTP::BASIC-SERVER-MIXIN URL:HTTP-URL)):  -
;;; Remove function (CLOS:METHOD HTTP:REDIRECT-REQUEST (HTTP::BASIC-SERVER-MIXIN URL:GOPHER-URL)): undefine.
;;; Remove function (CLOS:METHOD HTTP:REDIRECT-REQUEST (HTTP::BASIC-SERVER-MIXIN URL:HTTP-URL)): undefine.
;;; Function (CLOS:METHOD HTTP:REDIRECT-REQUEST (HTTP::BASIC-SERVER-MIXIN STRING)):  -
;;; Function (CLOS:METHOD HTTP:REDIRECT-REQUEST (HTTP::BASIC-SERVER-MIXIN URL:URI)):  -
;;; Function URL::URI-P:  -
;;; Function (CLOS:METHOD HTTP:REDIRECT-REQUEST (HTTP::BASIC-SERVER-MIXIN CONS)):  -
;;; Function (CLOS:METHOD HTTP:REDIRECT-REQUEST (HTTP::BASIC-SERVER-MIXIN CONS)):  -
;;; Function HTTP:REDIRECT-REQUEST:  -
;;; Remove function HTTP::HANDLE-URL-STANDARD-REDIRECTION: -
;;; DEFINE-CONDITION HTTP::HTTP/1.1-REDIRECTION-DOWNGRADE-MIXIN:  -
;;; DEFINE-CONDITION HTTP::DOCUMENT-FORWARDED:  -
;;; Function (CLOS:METHOD HTTP::REPORT-STATUS-LINE (HTTP::HTTP/1.1-REDIRECTION-DOWNGRADE-MIXIN T)):  -
;;; Function URL::EXPLICIT-EXPIRATION-P:  -
;;; Function (CLOS:METHOD URL::EXPLICIT-EXPIRATION-P (URL:EXPIRATION-MIXIN)):  -
;;; DEFINE-CONDITION HTTP::CACHEABLE-CONDITION-MIXIN:  -
;;; Function (CLOS:METHOD HTTP::REPORT-HTTP-HEADERS (HTTP::CACHEABLE-CONDITION-MIXIN T)):  -
;;; Written by JCMa, 9/19/00 19:35:17
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.75,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Documentation Patches 1.0,
;;; Genera 8 5 Clim Patches 1.0, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; HTTP Client Substrate 3.22, Jcma 42, HTTP Proxy Server 5.27, HTTP Client 49.10,
;;; Image Substrate 440.4, Essential Image Substrate 433.0,
;;; W4 Constraint-Guide Web Walker 45.4, W4 Examples 15.0, Genera Extensions 16.0,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.11,
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
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.5),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).

;;; Patch file for CL-HTTP version 70.76
;;; Written by JCMa, 9/21/00 05:04:59
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.76,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Documentation Patches 1.0,
;;; Genera 8 5 Clim Patches 1.0, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; HTTP Client Substrate 3.22, Jcma 42, HTTP Proxy Server 5.29, HTTP Client 49.11,
;;; Image Substrate 440.4, Essential Image Substrate 433.0,
;;; W4 Constraint-Guide Web Walker 45.4, W4 Examples 15.0, Genera Extensions 16.0,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.11,
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
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.5),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).


(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;HTTP-CONDITIONS.LISP.179"
  "HTTP:SERVER;URL.LISP.408"
  "HTTP:SERVER;SERVER.LISP.863"
  "HTTP:SERVER;REPORT.LISP.177"
  "HTTP:SERVER;HTTP-CONDITIONS.LISP.181"
  "HTTP:SERVER;REPORT.LISP.178"
  "HTTP:SERVER;HTTP-CONDITIONS.LISP.182"
  "HTTP:SERVER;URL.LISP.409"
  "HTTP:SERVER;REPORT.LISP.179"
  "HTTP:SERVER;HTTP-CONDITIONS.LISP.183")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.179")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-parameter *status-code-alist*
  '((100 "Continue")                            ;1.1
    (101 "Switching Protocols")                 ;1.1
    (200 "OK")
    (201 "Created")
    (202 "Accepted")
    (203 "Non-Authoritative Information")
    (204 "No Content")
    (205 "Reset Content")                       ;1.1
    (206 "Partial Content")                     ;1.1
    (300 "Multiple Choices" document-multiple-choices)	;1.1
    (301 "Moved Permanently" document-moved-permanently)
    (302 "Found" document-found)
    (303 "See Other" document-forwarded)        ;1.1
    (304 "Not Modified")
    (305 "Use Proxy" document-access-via-proxy)	;1.1
    (306 "Switch Proxy")                        ;1.1 No longer used  cf RFC 2616
    (307 "Temporary Redirect" document-moved-temporarily)	;1.1
    (400 "Bad Request" bad-syntax-provided)
    (401 "Unauthorized" recoverable-unauthorized-client-access)
    (402 "Payment Required" access-requires-payment)
    (403 "Forbidden" access-forbidden)
    (404 "Not Found" document-not-found)
    (405 "Method Not Allowed" method-not-allowed)
    (406 "Not Acceptable" acceptable-resource-not-found)
    (407 "Proxy Authentication Required" unauthorized-proxy-access)
    (408 "Request Timeout" request-timeout)
    (409 "Conflict" document-put-conflict)
    (410 "Gone" document-gone)
    (411 "Length Required" content-length-required)     ;1.1
    (412 "Precondition Failed" precondition-failed)     ;1.1
    (413 "Request Entity Too Large" request-entity-too-large)   ; 1.1
    (414 "Request URI Too Long" request-uri-too-long)   ; 1.1
    (415 "Unsupported Media Type" unsupported-media-type)       ; 1.1
    (416 "Requested Range Not Satisfiable" invalid-range-request)       ; 1.1
    (417 "Expectation Failed")                  ; 1.1
    (500 "Internal Server Error" server-internal-error)
    (501 "Not Implemented" server-not-implemented)
    (502 "Bad Gateway" bad-gateway)
    (503 "Service Unavailable" service-unavailable)
    (504 "Gateway Timeout" gateway-timeout)
    (505 "HTTP Version Not Supported" http-version-not-supported)       ; 1.1
    (506 "Redirection Failed"))                 ; 1.1
  "An alist of status code number, documentation, and optionally, condition name.")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;REPORT.LISP.177")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(defun signal-standard-url-redirect (url redirection-type method &optional target-window)
  "Signals a redirection to alternative resources URL according to METHOD and REDIRECTION-TYPE.
METHOD is the http method of the request.
REDIRECTION-TYPE is one of:

          :FORWARD             Exists primarily to allow a POST to redirect the 
                               client to a URI.
          :MULTIPLE-CHOICES    Redirects the client to a set of representations each 
                               with their own agent driven negotiation information.
          :PERMANENT-REDIRECT  Redirects the client to a new URI where the resource 
                               permanently resides. All references can be permanently
                               updated.
          :PROXY               Indicates that the client should retry the request via
                               a proxy.
          :TEMPORARY-REDIRECT  Redirects the client to a new URI where the resource 
                               temporarily resides.

TARGET-WINDOW is the name of the window to the display the result of the redirection."
  (check-type target-window (or null string))
  (let ((alternate-urls (url:alternate-urls url)))
    (unless alternate-urls
      (signal 'document-not-found :url url))
    (ecase redirection-type
      (:temporary-redirect
	(case method
	  (:get (signal 'document-found :url url :method method :new-urls alternate-urls :target-window target-window))
	  (t (signal 'document-moved-temporarily :url url :method method :new-urls alternate-urls :target-window target-window))))
      (:permanent-redirect
	(signal 'document-moved-permanently :url url :method method :new-urls alternate-urls :target-window target-window))
      (:forward
	(case method
	  (:get (signal 'document-forwarded :url url :method method :new-urls alternate-urls :target-window target-window))
	  (t (signal 'document-moved-temporarily :url url :method method :new-urls alternate-urls :target-window target-window))))
      (:proxy
	(signal 'document-access-via-proxy :url url :method method :new-urls alternate-urls :target-window target-window))
      (:multiple-choices
	(signal 'document-multiple-choices :url url :method method :new-urls alternate-urls :target-window target-window)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.863")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod invoke-server-method ((server basic-server-mixin) (method (eql :delete)) (http-version symbol)
                                 &aux translation-method)
  (macrolet ((with-delete-response ((server stream) &body body)
               `(multiple-value-bind (url status)
                    (progn . ,body)
                  ;; this could send back a 200 if there was a message to be returned 
                  ;; it would need to force output because
                  ;; provide service won't handle it 6/11/95 -- JCMa.
                  (ecase status
                    (:deleted 
                      (setf (server-status ,server) 204.)
                      (report-status-no-content ,stream))
                    (:accepted
                      (setf (server-status ,server) 202.)
                      (report-status-accepted,stream)))
                  ;; write some headers as the close of transaction
                  (write-headers* ,stream :date (server-request-time *server*)
                                  :location (url:name-string url)
                                  :server *server-version*))))
    (with-slots (address stream url-string) server
      (multiple-value-bind (url)
          (url:intern-url url-string :if-does-not-exist :soft)
        (cond ((and url (setq translation-method (translation-method url)))
               (setf (server-url server) url)
               (with-access-control (url method server (or (url::write-subnets url) *write-subnets*)
                                         :write-method-p t)
                 (case translation-method	; redirect when there is forwarding.
                   (:temporary-redirect
		     (signal-standard-url-redirect url :temporary-redirect :delete))
		   (:redirect
		     (signal-standard-url-redirect url :permanent-redirect :delete))
                   (t (with-delete-response (server stream)
                                            (delete-document url stream))))))
              (t (error 'document-not-found :url url-string :method :delete)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.863")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod invoke-server-method ((server basic-server-mixin) (method (eql :post)) (http-version symbol) &aux translation-method)
  (macrolet ((handle-redirect (condition tag)
               `(destructuring-bind (target-url &rest other-urls) (new-urls ,condition)
                  (cond ;; optimize redirect by reinvoking for singleton local url.
                    ((and (null other-urls) (local-redirect-p target-url))
                     (setf (server-url-string server) (url:name-string target-url))
                     (go ,tag))
                    (t (report-status ,condition stream))))))
    (with-slots (stream url-string) server
      (tagbody
        retry1
           (handler-case
             (destructuring-bind (&optional doc-type doc-subtype &rest args) (get-header :content-type)
               (declare (ignore args))
	       (multiple-value-bind (url)
		   (url:intern-url url-string :if-does-not-exist :soft)
		 (tagbody
		   retry1
		      (cond ((and url (setq translation-method (translation-method url)))
			     (setf (server-url server) url)
			     (with-access-control (url method server (or (url:secure-subnets url) *secure-subnets*)
						       :deny-subnets *disallowed-subnets*)
			       (case translation-method
				 (:temporary-redirect
				   (signal-standard-url-redirect url :temporary-redirect :post))
				 (:redirect
				   (signal-standard-url-redirect url :permanent-redirect :post))
				 (t (case http-version
				      ((:http/0.9 :http/1.0))
				      ;; alert HTTP 1.1 or greater clients that we are ready
				      (t (report-status-continue stream)
					 (send-cr-line-feed stream)
					 (force-output stream)
					 (setf (server-status server) 100.)))
				    ;; Upgrade this when reading chunked encodings is available. 7/24/96 -- JCMa.
				    (let ((transfer-encoding (get-header :transfer-encoding)))
				      (when transfer-encoding
					(error 'server-not-implemented :close-connection t :url url :method :post
					       :format-string "The HTTP transfer encoding, ~A, is not implemented."
					       :format-args (list transfer-encoding)))
				      (post-document url doc-type doc-subtype stream))))))
			    ((setq url (locate-controlling-url url-string method (url:valid-search-url-p url-string)))
			     (go retry1))
			    (t (error 'document-not-found :url url-string :method :post :close-connection t))))))
             (redirection (cond) (handle-redirect cond retry1)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.863")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod invoke-server-method ((server basic-server-mixin) (method (eql :put)) (http-version symbol)
                                 &aux url translation-method)
  (with-slots (address stream url-string) server
    (cond ;; resource already exists, so put a new version.
      ((and (setq url (url:intern-url url-string :if-does-not-exist :soft))
            (setq translation-method (translation-method url)))
       (setf (server-url server) url)
       (with-access-control (url method server (or (url::write-subnets url) *write-subnets*)
                                 :deny-subnets *disallowed-subnets*
                                 :write-method-p t)
         (case translation-method
	   (:temporary-redirect
	     (signal-standard-url-redirect url :temporary-redirect :put))
	   (:redirect
	     (signal-standard-url-redirect url :permanent-redirect :put))
           (t (put-document url stream nil *check-document-versions*)))))
      (t (%put-new-resource server stream url-string)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.863")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod write-document ((url url:http-object) (translation (eql :redirect)) stream)
  (declare (ignore stream))
  (signal-standard-url-redirect url :permanent-redirect :get))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.863")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod write-document ((url url:http-path) (translation (eql :redirect)) stream)
  (declare (ignore stream))
  (signal-standard-url-redirect url :permanent-redirect :get))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.863")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod write-document ((url url:http-object) (translation (eql :temporary-redirect)) stream)
  (declare (ignore stream))
  (signal-standard-url-redirect url :temporary-redirect :get))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.863")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod write-document-headers ((url url:http-object) (translation (eql :redirect)) stream)
  (declare (ignore stream))
  (signal-standard-url-redirect url :permanent-redirect :head))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.863")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod write-document-headers ((url url:http-path) (translation (eql :redirect)) stream)
  (declare (ignore stream))
  (signal-standard-url-redirect url :permanent-redirect :head))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.863")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod write-document ((url url:http-path) (translation (eql :temporary-redirect)) stream)
  (declare (ignore stream))
  (signal-standard-url-redirect url :temporary-redirect :get))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.863")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod write-document-headers ((url url:http-object) (translation (eql :temporary-redirect)) stream)
  (declare (ignore stream))
  (signal-standard-url-redirect url :temporary-redirect :head))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.863")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod write-document-headers ((url url:http-path) (translation (eql :temporary-redirect)) stream)
  (declare (ignore stream))
  (signal-standard-url-redirect url :temporary-redirect :head))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.863")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod invoke-server-method ((server basic-server-mixin) (method (eql :delete)) (http-version symbol)
                                 &aux translation-method)
  (macrolet ((with-delete-response ((server stream) &body body)
               `(multiple-value-bind (url status)
                    (progn . ,body)
                  ;; this could send back a 200 if there was a message to be returned 
                  ;; it would need to force output because
                  ;; provide service won't handle it 6/11/95 -- JCMa.
                  (ecase status
                    (:deleted 
                      (setf (server-status ,server) 204.)
                      (report-status-no-content ,stream))
                    (:accepted
                      (setf (server-status ,server) 202.)
                      (report-status-accepted,stream)))
                  ;; write some headers as the close of transaction
                  (write-headers* ,stream :date (server-request-time *server*)
                                  :location (url:name-string url)
                                  :server *server-version*))))
    (with-slots (address stream url-string) server
      (multiple-value-bind (url)
          (url:intern-url url-string :if-does-not-exist :soft)
        (cond ((and url (setq translation-method (translation-method url)))
               (setf (server-url server) url)
               (with-access-control (url method server (or (url::write-subnets url) *write-subnets*)
                                         :write-method-p t)
                 (case translation-method	; redirect when there is forwarding.
                   (:temporary-redirect
		     (standard-url-redirect url :temporary-redirect :delete))
		   (:redirect
		     (standard-url-redirect url :permanent-redirect :delete))
                   (t (with-delete-response (server stream)
                                            (delete-document url stream))))))
              (t (error 'document-not-found :url url-string :method :delete)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.863")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod invoke-server-method ((server basic-server-mixin) (method (eql :post)) (http-version symbol) &aux translation-method)
  (macrolet ((handle-redirect (condition tag)
               `(destructuring-bind (target-url &rest other-urls) (new-urls ,condition)
                  (cond ;; optimize redirect by reinvoking for singleton local url.
                    ((and (null other-urls) (local-redirect-p target-url))
                     (setf (server-url-string server) (url:name-string target-url))
                     (go ,tag))
                    (t (report-status ,condition stream))))))
    (with-slots (stream url-string) server
      (tagbody
        retry1
           (handler-case
             (destructuring-bind (&optional doc-type doc-subtype &rest args) (get-header :content-type)
               (declare (ignore args))
	       (multiple-value-bind (url)
		   (url:intern-url url-string :if-does-not-exist :soft)
		 (tagbody
		   retry1
		      (cond ((and url (setq translation-method (translation-method url)))
			     (setf (server-url server) url)
			     (with-access-control (url method server (or (url:secure-subnets url) *secure-subnets*)
						       :deny-subnets *disallowed-subnets*)
			       (case translation-method
				 (:temporary-redirect
				   (standard-url-redirect url :temporary-redirect :post))
				 (:redirect
				   (standard-url-redirect url :permanent-redirect :post))
				 (t (case http-version
				      ((:http/0.9 :http/1.0))
				      ;; alert HTTP 1.1 or greater clients that we are ready
				      (t (report-status-continue stream)
					 (send-cr-line-feed stream)
					 (force-output stream)
					 (setf (server-status server) 100.)))
				    ;; Upgrade this when reading chunked encodings is available. 7/24/96 -- JCMa.
				    (let ((transfer-encoding (get-header :transfer-encoding)))
				      (when transfer-encoding
					(error 'server-not-implemented :close-connection t :url url :method :post
					       :format-string "The HTTP transfer encoding, ~A, is not implemented."
					       :format-args (list transfer-encoding)))
				      (post-document url doc-type doc-subtype stream))))))
			    ((setq url (locate-controlling-url url-string method (url:valid-search-url-p url-string)))
			     (go retry1))
			    (t (error 'document-not-found :url url-string :method :post :close-connection t))))))
             (redirection (cond) (handle-redirect cond retry1)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.863")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod invoke-server-method ((server basic-server-mixin) (method (eql :put)) (http-version symbol)
                                 &aux url translation-method)
  (with-slots (address stream url-string) server
    (cond ;; resource already exists, so put a new version.
      ((and (setq url (url:intern-url url-string :if-does-not-exist :soft))
            (setq translation-method (translation-method url)))
       (setf (server-url server) url)
       (with-access-control (url method server (or (url::write-subnets url) *write-subnets*)
                                 :deny-subnets *disallowed-subnets*
                                 :write-method-p t)
         (case translation-method
	   (:temporary-redirect
	     (standard-url-redirect url :temporary-redirect :put))
	   (:redirect
	     (standard-url-redirect url :permanent-redirect :put))
           (t (put-document url stream nil *check-document-versions*)))))
      (t (%put-new-resource server stream url-string)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.863")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(PROGN
(defmethod write-document ((url url:http-object) (translation (eql :redirect)) stream)
  (declare (ignore stream))
  (standard-url-redirect url :permanent-redirect :get))

(defmethod write-document ((url url:http-path) (translation (eql :redirect)) stream)
  (declare (ignore stream))
  (standard-url-redirect url :permanent-redirect :get))

(defmethod write-document-headers ((url url:http-object) (translation (eql :redirect)) stream)
  (declare (ignore stream))
  (standard-url-redirect url :permanent-redirect :head))

(defmethod write-document-headers ((url url:http-path) (translation (eql :redirect)) stream)
  (declare (ignore stream))
  (standard-url-redirect url :permanent-redirect :head))

(defmethod write-document ((url url:http-object) (translation (eql :temporary-redirect)) stream)
  (declare (ignore stream))
  (standard-url-redirect url :temporary-redirect :get))

(defmethod write-document ((url url:http-path) (translation (eql :temporary-redirect)) stream)
  (declare (ignore stream))
  (standard-url-redirect url :temporary-redirect :get))

(defmethod write-document-headers ((url url:http-object) (translation (eql :temporary-redirect)) stream)
  (declare (ignore stream))
  (standard-url-redirect url :temporary-redirect :head))

(defmethod write-document-headers ((url url:http-path) (translation (eql :temporary-redirect)) stream)
  (declare (ignore stream))
  (standard-url-redirect url :temporary-redirect :head))
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;REPORT.LISP.177")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(defun standard-url-redirect (url redirection-type method &optional target-window)
  "Signals a redirection to alternative resources URL according to METHOD and REDIRECTION-TYPE.
METHOD is the http method of the request.
REDIRECTION-TYPE is one of:

          :FORWARD             Exists primarily to allow a POST to redirect the 
                               client to a URI.
          :MULTIPLE-CHOICES    Redirects the client to a set of representations each 
                               with their own agent driven negotiation information.
          :PERMANENT-REDIRECT  Redirects the client to a new URI where the resource 
                               permanently resides. All references can be permanently
                               updated.
          :PROXY               Indicates that the client should retry the request via
                               a proxy.
          :TEMPORARY-REDIRECT  Redirects the client to a new URI where the resource 
                               temporarily resides.

TARGET-WINDOW is the name of the window to the display the result of the redirection."
  (%standard-url-redirect url (url:alternate-urls url) redirection-type method target-window))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.863")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod redirect-request ((server basic-server-mixin) (target-url url:http-url) &optional target-window)
  (with-slots (url method) server
    (%standard-url-redirect url (list target-url) :temporary-redirect method target-window)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.863")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(SCL:FUNDEFINE '(METHOD REDIRECT-REQUEST (BASIC-SERVER-MIXIN GOPHER-URL)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.863")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(SCL:FUNDEFINE '(METHOD REDIRECT-REQUEST (BASIC-SERVER-MIXIN HTTP-URL)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.863")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod redirect-request ((server basic-server-mixin) (target-uri string) &optional target-window)
  (redirect-request server (intern-url target-uri :if-does-not-exist :create) target-window))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.863")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod redirect-request ((server basic-server-mixin) (target-uri uri) &optional target-window)
  (with-slots (url method) server
    (%standard-url-redirect url target-uri :temporary-redirect method target-window)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.408")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define uri-p (x)
  "Returns non-null if X is a URI."
  (typep x 'uri))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.408")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(export (intern "URI-P" :url) :url)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.863")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod redirect-request ((server basic-server-mixin) (target-uris cons) &optional target-window)
  (unless (every #'url::uri-p target-uris)
    (error 'server-internal-error
	   :format-string "Attempt to redirect to target URIs, ~S, which are not URIs."
	   :format-args (list target-uris)))
  (with-slots (url method) server
    (%standard-url-redirect url target-uris :temporary-redirect method target-window)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.863")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod redirect-request ((server basic-server-mixin) (target-uris cons) &optional target-window)
  (with-slots (url method) server
    (if (every #'url::uri-p target-uris)
	(%standard-url-redirect url target-uris :multiple-choice method target-window)
	(let ((alternate-uris (mapcar #'(lambda (uri) (intern-url uri :if-does-not-exist :create)) target-uris)))
	  (declare (dynamic-extent alternate-uris))
	  (%standard-url-redirect url alternate-uris :multiple-choice method target-window)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.863")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

;;;------------------------------------------------------------------- 
;;;
;;; URL FORWARDING
;;;

(define-generic redirect-request (server target-uri &optional target-window)
  (:documentation "Causes SERVER to issue a redirect to TARGET-URI.

REDIRECT-REQUEST must be called before signalling a status code, for example
before entering the macro HTTP:WITH-SUCCESSFUL-RESPONSE.

TARGET-URI can be a single URI or a list of URIs. When it is a list,
the client is free to choose any of the alternatives. Performance
is best if the URIs are already interned. If the targets are vary
widely the best created as :UNINTERNED.

TARGET-WINDOW can be a string designating the client window for output."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;REPORT.LISP.177")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(SCL:FUNDEFINE 'HANDLE-URL-STANDARD-REDIRECTION)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.181")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition http/1.1-redirection-downgrade-mixin () ())

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;REPORT.LISP.178")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

;; Status codes 303, 307 are not reliably understood by http 1.0 clients.
;; This method automatical downgrades the result.
(defmethod report-status-line ((condition http/1.1-redirection-downgrade-mixin) stream)
  (let* ((status-code (status-code condition))
         (reason (or (http-reason condition)
                     (get-string-for-status-code status-code))))
    (send-status-line stream (if (client-http-version-meets-p *server* :http/1.1)  status-code 302) reason)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.182")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition temporary-redirection (document-moved) ())

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.182")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition document-forwarded
                  (http/1.1-redirection-downgrade-mixin temporary-redirection)
  ((status-code :initform 303)
   (reason :initform "See Other")))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.409")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define-generic explicit-expiration-p (URL)
  (:documentation "Returns non-null if URL specifies explicit expiration information."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.409")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod explicit-expiration-p ((expiration-mixin expiration-mixin))
  (with-slots (expiration-function max-age-function) expiration-mixin
    (not (null (or expiration-function max-age-function)))))


(eval-when (load eval compile)
(export (intern "EXPLICIT-EXPIRATION-P" :url) :url))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;REPORT.LISP.179")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(defun %standard-url-redirect (url target-url redirection-type method target-window)
  (check-type target-window (or null string))
  (let ((alternate-urls (ensure-list target-url)))
    (unless alternate-urls
      (signal 'document-not-found :url url))
    (ecase redirection-type
      (:temporary-redirect
	(case method
	  (:get (signal 'document-found :url url :method method :new-urls alternate-urls :target-window target-window))
	  (t (signal 'document-moved-temporarily :url url :method method :new-urls alternate-urls :target-window target-window))))
      (:permanent-redirect
	(signal 'document-moved-permanently :url url :method method :new-urls alternate-urls :target-window target-window))
      (:forward
	(if (and (eq :get method)
		 (not (explicit-expiration-p url)))
	    (signal 'document-forwarded :url url :method method :new-urls alternate-urls :target-window target-window)
	    (signal 'document-moved-temporarily :url url :method method :new-urls alternate-urls :target-window target-window)))
      (:proxy
	(signal 'document-access-via-proxy :url url :method method :new-urls alternate-urls :target-window target-window))
      (:multiple-choices
	(signal 'document-multiple-choices :url url :method method :new-urls alternate-urls :target-window target-window)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.183")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition cacheable-condition-mixin () ())


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;REPORT.LISP.179")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(defmethod report-http-headers ((condition cacheable-condition-mixin) stream &optional (termination-line-p t)
                                header-plist (content-type :html))
  (let ((server *server*)
	(url (http-url condition))
	cache-control expires)
    (when (http-close-connection-p condition)
      (setf (server-close-connection-p server) t))
    (if (client-http-version-meets-p server :http/1.1)
	(setq cache-control (url:response-cache-control-directives url))
	(setq expires (url:expiration-universal-time url)))
    (%write-document-mime-headers stream content-type nil nil nil nil expires nil nil nil nil nil cache-control
				  termination-line-p nil header-plist)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.183")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition document-multiple-choices
                  (cacheable-condition-mixin document-moved)
  ((status-code :initform 300)
   (reason :initform "Multiple Choices")))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.183")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition document-moved-permanently
                  (cacheable-condition-mixin document-moved)
  ((status-code :initform 301)
   (reason :initform "Moved Permanently")))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.183")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition document-found		;formerly document-moved-temporarily 9/19/2000 -- JCMa. 
                  (cacheable-condition-mixin temporary-redirection)
  ((status-code :initform 302)
   (reason :initform "Found")))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.183")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition document-access-via-proxy
                  (cacheable-condition-mixin temporary-redirection)
  ((status-code :initform 305)
   (reason :initform "Use Proxy")))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.183")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition document-moved-temporarily
                  (http/1.1-redirection-downgrade-mixin cacheable-condition-mixin temporary-redirection)
  ((status-code :initform 307)
   (reason :initform "Temporary Redirect")))
