;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-

;;; (C) Copyright 1996, 2000 John C. Mallery
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; CL-HTTP BASE CLIENT
;;;

(in-package :http)

(define-parameter *acceptable-transfer-encodings* nil
		  "a list of (encoding . quality-value) indicating the relative preferences for particular transfer encodings
 supported by the client/proxy. Encoding is a keyword such as :deflate. QUALITY-VALUE is a number from 0 to 1, inclusive.
Use this to indicate ability to handle compression or encryption schemes.")

(define-variable *client* nil
                 "Bound to the http client running in the current environment.")

(define-parameter *client-class* 'client
                  "The class of HTTP client to use.
This allows you to customize the client you run.
Use CLEAR-CLIENT-RESOURCE when changing this.") 

(define-parameter *client-http-version* #-(or Genera MCL LispWorks Allegro (and CMU mp)) :HTTP/1.0
		                        #+(or Genera MCL LispWorks Allegro (and CMU mp)) :HTTP/1.1
                  "Controls the HTTP version that the client advertises.")

(define-variable *client-line-buffer* nil
                 "Holds the line buffer for a client  connection.")

(define-parameter *client-persistent-connections* t
                  "Controls whether the client uses persistent connections or not.")

(define-parameter *client-persistent-connection-number* 999.
                  "The default number of requests allowed over a persistent connection by a server.")

(define-parameter *client-persistent-connection-report-failures* t
                  "Whether or not to report all connections failures via email.")

(define-parameter *client-persistent-connection-timeout* 60.
                  "The default timeout for client presistent connections to servers.")

(define-parameter *client-retry-sleep-seconds* 3
                  "The number of seconds to sleep between client retries of failing HTTP operations.")

(define-parameter *client-retry-times-for-network-errors* 2
                  "The number of times a client retries an HTTP operation in the face of network errors.")

(define-parameter *client-retries-for-unresponsive-hosts* 8
                  "The number of times a client retries an HTTP operation on a host that is unresponsive or drops connections.")

(define-parameter *client-await-continue-reply-timeout* 5
                  "The default number of seconds to wait for a status code 100 continue reply before sending entity data.")

(declaim (integer *client-timeout*))

(define-parameter *client-timeout* (* 30. 60.)
                  "Sixthieths of a second before a client request times out waiting for a server to respond.")

(define-variable *connection* nil
                 "The current HTTP connection for a client operation.")

(define-parameter *connection-scavenger-on* t
                  "Controls whether the connection scavenger runs or not.")

(define-variable *connections-allocated* 0
                 "Total number of connections allocated.") 

(define-variable *connections-deallocated* 0
                 "Total number of connections deallocated.")

(define-variable *debug-client* nil
                 "Controls whether debuging information is displayed.")

(define-parameter *standard-proxy* nil
                  "The standard proxy for client HTTP accesss.")

(define-variable *standard-proxy-control* nil
		 "Holds the datastructures controlling selection of client proxies.")

(define-parameter *proxy-class* 'http-proxy
                  "The class of HTTP proxy to use.
This allows you to customize the proxy you run.")

(define-variable *trace-client* nil
                 "Controls whether tracing information is displayed.")

;;;------------------------------------------------------------------- 
;;;
;;; DEBUGGING
;;;

(define debug-client (&optional (on-p (not *debug-client*)))
  "Toggles client debugging according to ON-P."
  (setq *debug-client* (not (null on-p))))

(define trace-client (&optional (trace (not *trace-client*)))
  "Toggles client tracing according to TRACE.
TRACE can be: :HEADERS or T for everything."
  (setq *trace-client* (ecase trace
                         ((nil t) trace)
                         (:headers trace))))

(defmacro client-trace (format-string &rest format-args)
  `(when (eq *trace-client* t)
     (format *trace-output* ,format-string ,@format-args)))

;; Export here to preserve modularity in the primary package definitions.
(mapc #'(lambda (x) (export (intern x :http) :http))
      '("*DEBUG-CLIENT*"             
        "DEBUG-CLIENT"))