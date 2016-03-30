;;;   -*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-
;;;
;;; (c) Copyright  2000, 2006, John C. Mallery
;;;     All Rights Reserved.
;;;
;;; (C) Copyright 1996-1997, Christopher R. Vincent and John C. Mallery
;;;     All Rights Reserved.
;;;------------------------------------------------------------------- 
;;;
;;; VARIABLES
;;;
(in-package :http)

(defvar *cache-counter* 0
  "A number used to identify caches.")

;; 206 requires more general handling of range requests   5/4/2000 -- JCMa.
(defconstant *cacheable-response-status-codes* '(200 203 300 301 302 307 410)
  "Response code that are cacheable.")

(defconstant *conditional-request-headers* '(:if-match :if-modified-since :if-none-match :if-range :if-unmodified-since)
  "Headers involving some kind of condition response from an origin server.")

(defvar *database-counter* 0
  "A number used to identify databases.")

(defparameter *database-log-process-priority* 0
  "The default process priority for database log manager processes.")

(define-parameter *debug-proxy* nil
  "Controls proxy debugging.")

(defconstant *entity-data-file-type* "entity"
  "The file type for cached entity files.")

;; All other headers are end-to-end by default. (:proxy-connection is Netscape only)
(defconstant *hop-by-hop-headers* 
	     '(:connection :keep-alive :proxy-authenticate :proxy-authorization :proxy-connection :te :trailer :transfer-encoding :upgrade))

(defconstant *metadata-file-type* "mdata"
  "The file type for cached metadata files.")

(defparameter *metadata-storage-mode* #+(or Genera LispWorks) :binary #-(or Genera LispWorks) :text
  "Controls the mode used to persistently store entity metadata.")

(defparameter *proxy-cacheable-request-headers* '(:authorization :cookie)
  "Headers that are cacheable even though they have some request or user specificity.
This parameter is adjustable to control the desired level of privacy and security.")

(define-parameter *proxy-cache-class* 'proxy-cache
		  "Controls the class of HTTP cache used by the proxy.")

(defparameter *proxy-cache* nil
  "The primary HTTP proxy cache.")

(defparameter *proxy-cache-default-expiration-interval* (* 15 60)	;15 minutes
  "The default expiration time in seconds for resources not specifying an expiration time.")

(defparameter *proxy-cache-full-gc-free-space-ratio* .2
  "The free space as a fraction between 0 and 1 
that a full garbage collection must achieve.  For daily GCs, a ratio between
.2 and .4 prevents running out of space during the day, depending on loading")

(defparameter *proxy-cache-incremental-gc* t
  "Controls whether the incremental GC is active to collect proxy cache garbage.")

(declaim (float *proxy-cache-incremental-gc-free-space-ratio*))

(defparameter *proxy-cache-incremental-gc-free-space-ratio* .05
  "The free space as a fraction between 0 and 1 that an incremental garbage collection must achieve.")

(declaim (float *proxy-cache-incremental-gc-free-space-trigger-ratio*))

(defparameter *proxy-cache-incremental-gc-free-space-trigger-ratio* .02
  "The minimum free space that triggers an incremental GC.
This is a fraction between 0 and 1")

(defparameter *proxy-cache-incremental-gc-process-priority* 0
  "The default process priority for the proxy cache incremental GC processes.")

(defparameter *proxy-cache-maximum-expiration-time* (* 60 60 24 7 12)
  "The maximum time in seconds before a cached object expires.
All expirations are truncated down to this value.")

(defparameter *proxy-cache-maximum-resources* most-positive-fixnum
  "The maximum number of resources that a proxy cache is allowed to cache.")

(defparameter *proxy-cache-maximum-size* most-positive-fixnum
  "The maximum size in bytes that a proxy cache is allows to consume in storage.")

(declaim (fixnum *proxy-cache-minimum-expiration-time*))

(defparameter *proxy-cache-minimum-expiration-time* (* 60 2)
   "The minimum expiration time in seconds for an object to be cached.
This bounds resource expiration times.") 

(define-parameter *proxy-caching-p*
  #+(OR MCL Genera LispWorks) t
  #-(OR MCL Genera LispWorks) nil
  "Allow the proxy to use caching operations.")

(defparameter *proxy-cache-directory-depth* 2
  "The depth of the directory structure used to hold proxy cache files.
This value cannot be changed without remapping all the file names in the cache.")

(defparameter *proxy-cache-uid-limit* nil
  "Internal variable that holds a number which is limit value for UID numbers, given the configuration of the proxy.
This limit is (expt files-per-directory directory-depth.")

(defparameter *proxy-cache-uid-directory-base-list* nil
  "Internal variable that is used to map UIDs to directory hierarchies.")

(defparameter *proxy-cache-uids-per-directory* 256
  "The number of of UIDs stored in each subdirectory of the proxy cache.
The actual number of files is twice the value of this variable.")

(define-parameter *proxy-connect-allowed-ports* '(443 563)
                  "The list of destination ports for which the HTTP CONNECT is allowed.
This is intended to prevent the proxy from being used to anonymize connections to arbitrary destintion ports.
By default, only the default HTTPS port, 443, and the default SNEWS port, 563, are enabled.")

(define-parameter *proxy-database-class* 'proxy-cache-database
  "Controls the class of cache database used by the proxy.")

(define-parameter *proxy-methods* '(:get :head :options :trace :post :put :delete :connect)
  "The methods generally supported through the proxy.  Used for OPTIONS.")

(define-variable *proxy-server-life-time* (* 1000. 60. 5)     ;default is five minutes
                 "Controls the maximum time an HTTP proxy server server can live.
Time is in milliseconds.")

(defvar *proxy-version* nil
  "The version of the current proxy for use in headers.")

(define-parameter *resource-class* 'resource
		  "Controls the class of cached resource used.")

(define-parameter *representation-class* 'representation
  "Controls the class of cached representation used.")

(defconstant *response-status-codes-defaultedly-not-cached* '(302 307)
  "Response code that MUST NOT be cached unless explicitly specified by cache-control or expires headers.")

(defconstant *response-status-codes-with-no-entity* '(100 204 304)
  "Response status codes that MUST NOT return entity bodies.")

;; RFC 2616 does not specify whether 305 is in this set
(defconstant *response-status-codes-with-optional-entity* '(300 301 302 303 305 307)
  "Response status codes that SHOULD return entity bodies but MAY NOT.")

;; MS IE Extension headers: (:ua-os :ua-cpu)
(defconstant *request-specific-request-headers* '(:cache-control :date :expect :extension :host :max-forwards :pragma :referer :user-agent :via
								 :ua-os :ua-cpu)
  "Headers associated with a specific request by a user-agent that are not cacheable.")

(define-parameter *standard-proxy-port* #-UNIX 80 #+UNIX 8000
  "This is the standard port on which the HTTP proxy operates.")

(define-parameter *trace-proxy* nil
  "Controls proxy tracing.")

(defconstant *user-specific-request-headers* '(:authorization :cookie :from)
  "Headers associated with a specific user that involve privacy considerations.")

(define-parameter *via-header-preserve-comments* t
  "When NIL, strips the optional comments from VIA header entries.")
