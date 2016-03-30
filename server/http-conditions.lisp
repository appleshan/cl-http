;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-

;;; (C) Copyright 1994-2001, 2003, 2005-2006, John C. Mallery
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; GENERIC SIGNALLING
;;;

(in-package :http)

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
    (407 "Proxy Authentication Required" recoverable-unauthorized-proxy-access)
    (408 "Request Timeout" request-timeout)
    (409 "Conflict" document-put-conflict)
    (410 "Gone" document-gone)
    (411 "Length Required" content-length-required)     ;1.1
    (412 "Precondition Failed" precondition-failed)     ;1.1
    (413 "Request Entity Too Large" request-entity-too-large)   ; 1.1
    (414 "Request URI Too Long" request-uri-too-long)   ; 1.1
    (415 "Unsupported Media Type" unsupported-media-type)       ; 1.1
    (416 "Requested Range Not Satisfiable" invalid-range-request)       ; 1.1
    (417 "Expectation Failed" expectation-failed)	; 1.1
    (500 "Internal Server Error" server-internal-error)
    (501 "Not Implemented" server-not-implemented)
    (502 "Bad Gateway" bad-gateway)
    (503 "Service Unavailable" service-unavailable)
    (504 "Gateway Timeout" gateway-timeout)
    (505 "HTTP Version Not Supported" http-version-not-supported)       ; 1.1
    (506 "Redirection Failed"))                 ; 1.1
  "An alist of status code number, documentation, and optionally, condition name.")

(define get-condition-for-status-code (code &optional no-error-p)
  (cond ((and (integerp code) (third (assoc code *status-code-alist* :test #'=))))
        (no-error-p nil)
        (t (error "Unknown status code."))))

(define get-string-for-status-code (code &optional no-error-p)
  (cond ((and (integerp code) (second (assoc code *status-code-alist* :test #'=))))
        (no-error-p nil)
        (t (error "Unknown status code."))))

(define client-signal-http-code (url code method &key headers reason version)
  (let ((cond-type (get-condition-for-status-code code t)))
    (cond (cond-type
           (signal cond-type :url url :method method :headers headers
                   :reason reason :version version))
          (t (error "Applying Method ~S to ~A elicited a ~D (~A) in HTTP version ~A. ~:[~;~:*~{~A: ~A~^~}~]"
                    method (url:name-string url) code reason *http-version*
		    (and headers (header-set-header-plist headers #'header-raw-value)))))))

(define describe-status-codes (&optional (stream *standard-output*) (print-legend-p t))
  "Describes the HTTP status codes on stream."
  (loop initially (when print-legend-p
                    (format stream "~&~2TCode ~8TDescription ~40TCondition Class~2%"))
        for (code description error-class) in *status-code-alist*
        do (format stream "~&~2T~D~8T~A~:[~;~40T~:*~(~S~)~]" code description error-class)))


;;;------------------------------------------------------------------- 
;;;
;;; NETWORK ERRORS
;;;

(define-condition http-host-stopped-responding
                  (www-utils:host-stopped-responding www-utils:network-error-mixin)
  ((host :initform nil :initarg :host :reader http-host)
   (port :initform 80 :initarg :port :reader http-port))
  (:report (lambda (cond stream)
             (let ((host (http-host cond))
                   (port (http-port cond)))
               (format stream "The host, ~A, stopped responding on port ~D."
                       (host-domain-name host) port)))))

;;;------------------------------------------------------------------- 
;;;
;;; CONDITIONS
;;;

;; Other protocols and can specialize this, eg FTP.
(define-condition protocol-condition (condition) ())

(define-condition http-condition
                  (protocol-condition)
  ((close-connection-p :initform nil :initarg :close-connection :reader http-close-connection-p)))

(define close-connection-p (condition)
  "Returns non-null unless CONDITION warrants that the connection may safely remain open."
  (typecase condition
    (http-condition (http-close-connection-p condition))
    ;;always close connections when error state unknown.  1/12/97 -- JCMa.
    (t t)))

(define-condition http-abort (http-condition) ())

(define-condition reportable-condition
                  (http-condition)
  ((url :initform nil :initarg :url :reader http-url)
   (method :initform nil :initarg :method :reader http-method)
   (status-code :reader status-code)
   (reason :initform nil :initarg :reason :reader http-reason)
   (version :initform *http-version* :initarg :version :reader http-version)
   (headers :initform nil :initarg :headers :reader http-transaction-headers)
   (format-string :initform nil :initarg :format-string :reader format-string)
   (format-args :initform nil :initarg :format-args :reader format-args)
   (reporter :initform nil :initarg :reporter :reader http-reporter))
  (:report report-status-message))

;; Mix into conditions that are cacheable by proxies
(define-condition cacheable-condition-mixin () ())

(define-condition client-condition (http-condition) ())

(define-condition server-condition (http-condition) ())

(define-condition proxy-condition (server-condition) ())

(define-condition client-reportable-condition (reportable-condition client-condition) ())

(define-condition bad-syntax-provided
                  (client-reportable-condition)
  ((status-code :initform 400)
   (reason :initform "Bad Request")
   (close-connection-p :initform t		;always close connection on bad syntax
                       :initarg :close-connection :reader http-close-connection-p)))

(define-condition request-missing-host-header
                  (bad-syntax-provided)
  ((reason :initform "Bad Request: Missing Host Header")))

(define-condition unknown-virtual-host
                  (bad-syntax-provided)
  ((reason :initform "Bad Request: Unknown Virtual Host")))

(define-condition bad-range-header-value
                  (bad-syntax-provided)
  ((reason :initform "Bad Request: Ill-Formed Range"))
  (:documentation "Signalled when the value for a range header is ill-formed."))

(define-condition bad-cookie-header-value
                  (bad-syntax-provided)
  ((reason :initform "Bad Request: Ill-Formed Cookie"))
  (:documentation "Signalled when the value for a cookie header is ill-formed."))

(define-condition bad-server-response
                  (bad-syntax-provided)
  ((reason :initform "Bad Response: Ill-Formed Server Response")
   (response :initform nil :initarg :response))
  (:documentation "Signalled a server response to an HTTP request is unparsable."))

(define-condition bad-http-version
                  (bad-server-response)
  ((reason :initform "Bad Response: Invalid HTTP Version")
   (response :initform nil :initarg :response))
  (:documentation "Signalled when a server response to an HTTP request contains a bad HTTP version."))

(define-condition ssl-not-available
    (bad-syntax-provided)
  ((reason :initform "Bad Request: SSL client connections are not available."))
  (:documentation "Signalled when SSL is not available to handle client requests."))

;; typed errors for time parsing -- JCMa 7/24/2003.
#-Genera
(define-condition time-parse-error (bad-syntax-provided error) 
  ()
  (:documentation "Signalled when the time parser gets errors parsing dates."))

(define-condition bad-form-data
                  ()
  ((reason :initform "Unparsable form data.")))

(define-condition bad-form-data-posted
                  (bad-form-data bad-syntax-provided)
  ((reason :initform "Bad Form Data Posted: Client provided unparsable URL-encoded data.")))

(define-condition bad-multipart-form-data
                  ()
  ((reason :initform "Unparsable MIME multipart form data.")))

(define-condition bad-multipart-form-data-posted
                  (bad-multipart-form-data bad-form-data-posted)
  ((reason :initform "Bad Form Data Posted: Client provided unparsable MIME Multipart encoded data.")))

(define-condition bad-escaping
		  (bad-syntax-provided)
  ((reason :initform "Bad Escaping: Error while unescaping a URL or string."))
  (:documentation "Signalled when an ill-formed escape sequence is encountered while unescaping a URL or string."))

(define-condition request-missing-content-type-header
                  (bad-syntax-provided)
  ((reason :initform "Bad Request: Missing Content-Type Header")))

(define-condition digest-authorization-error
    (bad-syntax-provided)
  ((reason :initform "Digest Authorization Error")
   (proxy-p :initform nil :initarg :proxy-p :reader http-proxy-p)
   (authorization :initform nil :initarg :authorization :reader http-authorization)))

(define-condition inconsistent-digest-authorization-uri
    (digest-authorization-error)
  ((reason :initform "Digest Authorization: Request URI Inconsistent With Authorization URI")))

(define-condition access-control-condition
                  (client-reportable-condition)
  ())

(define-condition unauthorized-access (access-control-condition) ())

(define-condition recoverable-unauthorized-access () ())

(define-condition access-with-stale-nonce 
    () 
  ((recompute-authorization-header-p :initform t)))

(define-condition unauthorized-client-access (unauthorized-access) ())

(define-condition authorization-credentials-unavailable () ())

(define-condition recoverable-unauthorized-client-access
    (recoverable-unauthorized-access unauthorized-client-access)
  ((authentication-method :initarg :authentication-method :reader http-authentication-method)
   (authentication-realm :initarg :authentication-realm :reader http-authentication-realm)
   (recompute-authorization-header-p :initform nil :initarg :recompute-authorization-header-p :reader http-recompute-authorization-header-p)
   (status-code :initform 401)
   (reason :initform "Unauthorized")))

(define-condition unknown-authentication-method
                  (access-control-condition)
  ((authentication-method :initarg :authentication-method :reader http-authentication-method)
   (status-code :initform 400)))

(define-condition client-access-with-stale-nonce (access-with-stale-nonce recoverable-unauthorized-client-access) ())

(define-condition client-access-without-credentials (recoverable-unauthorized-client-access authorization-credentials-unavailable) ())

(define-condition access-requires-payment
                  (access-control-condition)
  ((status-code :initform 402)
   (reason :initform "Payment Required")))

(define-condition access-forbidden
                  (access-control-condition)
  ((status-code :initform 403)
   (reason :initform "Access Forbidden")))

(define-condition proxy-access-forbidden
                  (access-forbidden)
  ((reason :initform "Proxy Access Forbidden")
   (reporter :initform 'report-proxy-access-forbidden)
   (close-connection-p :initform t)))		;always close when access forbidden

(define-condition proxy-connect-method-forbidden
    (proxy-access-forbidden)
  ((reason :initform "Proxy Connect Forbidden")
   (reporter :initform 'report-proxy-connect-method-forbidden)))

(define-condition document-not-found
                  (client-reportable-condition)
  ((status-code :initform 404)
   (reason :initform "Not Found")))

(define-condition too-many-redirects
                  (document-not-found)
  ((reason :initform "Too Many Redirects")
   (n-redirects :initform 0 :initarg :n-redirects))
  (:documentation "Used to signal that the document was not found
because too many redirects were encountered."))

(define-condition method-not-allowed
                  (access-control-condition)
  ((status-code :initform 405)
   (method :initform nil :initarg :method :reader http-method)
   (reason :initform "Method Not Allowed")))

;; needs to send the headers -- JCMa 5/29/1995.
(define-condition acceptable-resource-not-found
                  (access-control-condition)
  ((status-code :initform 406)
   (headers :initform nil :initarg :headers :reader headers)
   (reason :initform "None Acceptable")))

(define-condition unauthorized-proxy-access
                  (unauthorized-access)
  ((status-code :initform 407)
   (reason :initform "Proxy Authentication Required")))

(define-condition recoverable-unauthorized-proxy-access
    (recoverable-unauthorized-access unauthorized-proxy-access)
  ((authentication-method :initarg :authentication-method :reader http-authentication-method)
   (authentication-realm :initarg :authentication-realm :reader http-authentication-realm)
   (recompute-authorization-header-p :initform nil :initarg :recompute-authorization-header-p :reader http-recompute-authorization-header-p)))

(define-condition proxy-access-with-stale-nonce (access-with-stale-nonce recoverable-unauthorized-proxy-access) ())

(define-condition proxy-access-without-credentials (recoverable-unauthorized-proxy-access authorization-credentials-unavailable) ())

(define-condition request-timeout
                  (access-control-condition)
  ((status-code :initform 408)
   (reason :initform "Request Timeout"))) 

(define-condition document-put-conflict
                  (access-control-condition)
  ((status-code :initform 409)
   (reason :initform "Conflict"))) 

(define-condition document-gone
                  (document-not-found)
  ((status-code :initform 410)
   (reason :initform "Gone")))

(define-condition content-length-required
                  (client-reportable-condition)
  ((status-code :initform 411)
   (reason :initform "Length Required")
   (close-connection-p :initform t)))

(define-condition precondition-failed
                  (client-reportable-condition)
  ((status-code :initform 412)
   (reason :initform "Precondition Failed")))

(define-condition request-entity-too-large
                  (client-reportable-condition)
  ((status-code :initform 413)
   (reason :initform "Request Entity Too Large")
   (close-connection-p :initform t)
   (retry-after :initform nil)))                ; should send a retry after header when non-null

(define-condition file-upload-maximum-size-exceeded
                  (request-entity-too-large bad-multipart-form-data-posted)
  ((reason :initform "Maximum Upload File Size Exceeded")))

(define-condition request-uri-too-long
                  (bad-syntax-provided)
  ((status-code :initform 414)
   (reason :initform "Request URI Too Long")))

(define-condition unsupported-media-type
                  (client-reportable-condition)
  ((status-code :initform 415)
   (reason :initform "Unsupported Media Type")))

(define-condition invalid-range-request
                  (bad-syntax-provided)
  ((status-code :initform 416)
   (reason :initform "Requested Range Not Valid")))

(define-condition expectation-failed
                  (precondition-failed)
  ((status-code :initform 417)
   (reason :initform "Expectation Failed")))

(define-condition server-reportable-condition
                  (reportable-condition server-condition) ())

(define-condition server-internal-error
                  (server-reportable-condition)
  ((status-code :initform 500)
   (close-connection-p :initform t)
   (server-error :initform nil :initarg :server-error :reader server-error)))

(define-condition error-computing-response
                  (server-internal-error)
  ((headers :initform nil :initarg :headers :reader http-headers)
   (stack-backtrace :initform nil :initarg :stack-backtrace :reader http-stack-backtrace)))

(define-condition error-handling-post-method
                  (error-computing-response)
  ((form-alist :initform nil :initarg :form-alist :reader http-form-alist)))

(define-condition html-generation-error (error-computing-response) ())

(define-condition error-handling-put-method (server-internal-error) ())

(define-condition error-handling-delete-method (server-internal-error) ())

(define-condition server-error
                  (server-reportable-condition)
  ((close-connection-p :initform t :initarg :close-connection  :reader http-close-connection-p)))

(define-condition server-not-implemented
                  (server-error)
  ((status-code :initform 501)))

(define-condition unsupported-method
                  (server-not-implemented)
  ((method :initarg :method :reader http-method)))

(define-condition proxy-not-implemented (server-not-implemented proxy-condition) ())

(define-condition bad-gateway
                  (server-error)
  ((status-code :initform 502)))

(define-condition proxy-bad-gateway (bad-gateway proxy-condition) ())

(define-condition proxy-bad-ftp-gateway (bad-gateway proxy-condition) ())

;; this is signalled when a data source provides less data than expected by content-length
(define-condition insufficient-data
                  (bad-gateway end-of-file)
  ((reason :initform "Insuffient Data")))

(define-condition proxy-unresolvable-domain-name
		  (proxy-bad-gateway)
  ((reason :initform "Domain Name Unresolvable" :initarg :reason :reader http-reason)
   (close-connection-p :initform nil :initarg :close-connection :reader http-close-connection-p)))

(define-condition proxy-local-network-error (proxy-bad-gateway) ())

(define-condition proxy-invalid-authentication-credentials
		  (proxy-local-network-error)
  ((reason :initform "Invalid Proxy Authentication Credentials" :initarg :reason :reader http-reason)
   (proxy :initform nil :initarg :proxy :reader http-proxy)
   (realm :initform nil :initarg :realm :reader http-realm)))

(define-condition proxy-remote-network-error (proxy-bad-gateway) ())

(define-condition service-unavailable
                  (server-error)
  ((status-code :initform 503)))

(define-condition server-overloaded (service-unavailable) ())

(define-condition proxy-service-unavailable (service-unavailable proxy-condition) ())

(define-condition proxy-connection-refused (proxy-service-unavailable) ())

(define-condition gateway-timeout
                  (server-error)
  ((status-code :initform 504)))

(define-condition client-timed-out (gateway-timeout) ())

(define-condition proxy-connection-timeout (gateway-timeout proxy-condition) ())

(define-condition http-version-not-supported
                  (server-error)
  ((status-code :initform 505)))

(define-condition redirection (reportable-condition) ())

(define-condition http/1.1-redirection-downgrade-mixin () ())

(define-condition document-moved
                  (redirection)
  ((new-urls  :initarg :new-urls :reader new-urls)
   (target-window :initform nil :initarg :target-window :reader http-target-window)))

(define-condition temporary-redirection (document-moved) ())

(define-condition document-multiple-choices
                  (cacheable-condition-mixin document-moved)
  ((status-code :initform 300)
   (reason :initform "Multiple Choices")))

(define-condition document-moved-permanently
                  (cacheable-condition-mixin document-moved)
  ((status-code :initform 301)
   (reason :initform "Moved Permanently")))

(define-condition document-found		;formerly document-moved-temporarily 9/19/2000 -- JCMa. 
                  (cacheable-condition-mixin temporary-redirection)
  ((status-code :initform 302)
   (reason :initform "Found")))

(define-condition document-forwarded
                  (http/1.1-redirection-downgrade-mixin temporary-redirection)
  ((status-code :initform 303)
   (reason :initform "See Other")))

(define-condition document-access-via-proxy
                  (cacheable-condition-mixin temporary-redirection)
  ((status-code :initform 305)
   (reason :initform "Use Proxy")))

(define-condition document-moved-temporarily
                  (http/1.1-redirection-downgrade-mixin cacheable-condition-mixin temporary-redirection)
  ((status-code :initform 307)
   (reason :initform "Temporary Redirect")))
