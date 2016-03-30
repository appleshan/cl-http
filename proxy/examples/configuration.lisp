;;; -*- Mode: LISP; Package: HTTP; Syntax: ANSI-Common-Lisp -*-

;;; Copyright 2000-2001, 2006, John C. Mallery.
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;; CONFIGURATION FILE FOR CL-HTTP CACHING PROXY 
;;;
(in-package :http)

;; This is the standard port on which the HTTP proxy operates.
#-(or Allegro LispWorks Lucid CMU (and MCL OSX))
(set-standard-http-proxy-port 80)

;; UNIX tends to lose with some implementation on port 80 so most people use 8000
#+(or Allegro LispWorks Lucid CMU SCL)
(set-standard-http-proxy-port 8000)
#+(and MCL OSX)
(set-standard-http-proxy-port 8001)

;; Specify upstream proxies by protocol and within URL space.
#+Ignore
(define-client-proxy-mapping
  :standard-proxies ((:ftp :default "fuji-vlm.ai.mit.edu" 8000)))

;; The default process priority for database log manager processes.  If the
;; file log manager is falling behind writing metadata files, you should
;; increase this priority.
(setq *database-log-process-priority* 0)

;; The default expiration time in seconds for resources not specifying an
;; expiration time.
(setq *proxy-cache-default-expiration-interval* (* 15 60))	;15 minutes

;; The free space as a fraction between 0 and 1 that a full garbage
;; collection must achieve.  For daily GCs, a ratio between .2 and .4 prevents
;; running out of space during the day, depending on loading
(setq *proxy-cache-full-gc-free-space-ratio* .2)

;; Controls whether the incremental GC is active to collect proxy cache
;; garbage.
(setq *proxy-cache-incremental-gc* t)

;; The free space as a fraction between 0 and 1 that an incremental garbage
;; collection must achieve.
(setq *proxy-cache-incremental-gc-free-space-ratio* .05)

;; The minimum free space that triggers an incremental GC.
(setq *proxy-cache-incremental-gc-free-space-trigger-ratio* .02)

;; The default process priority for the proxy cache incremental GC processes.
(setq *proxy-cache-incremental-gc-process-priority* 0)

;; The maximum time in seconds before a cached object expires.  All
;; expirations are truncated down to this value.
(setq *proxy-cache-maximum-expiration-time* (* 60 60 24 7 12))	; one year

;; The maximum number of resources that a proxy cache is allowed to cache.
;; Two directory levels deep of resources (expt 256 3) is 16,777,216.
(setq *proxy-cache-maximum-resources* (min 16777216 most-positive-fixnum))

;; The maximum size in bytes that a proxy cache is allows to consume in
;; storage. Use this parameter to control garbage collection.
(setq *proxy-cache-maximum-size* 20000000)	;20 megabytes for a small proxy

;; The minimum expiration time in seconds for an object to be cached.  This
;; bounds resource expiration times.
(setq *proxy-cache-minimum-expiration-time* (* 60 2))	;two minutes

;; Controls the maximum time an HTTP proxy server server can live.  Time is in
;; milliseconds.
(setq *proxy-server-life-time* (* 1000. 60. 5))	;five minutes

;; Headers that are cacheable even though they have some request or user specificity.
;; This parameter is adjustable to control the desired level of privacy and security.
(setq *proxy-cacheable-request-headers* '(:authorization :cookie))

;; Make sure the default log class does not log proxy requests
(setq *log-access-log-class* 'server-common-file-log)

;; Turn off proxy logging for any existing server access logs
;; CLEAR-ACCESS-LOGS can be used to remove any existing logs, but
;; it is better to arrange your configuration to use the server only classes
#+ignore
(log-server-access-logs-proxy-access (current-access-logs) nil)

;; Turn this off in proxy servers to improve performance
(log-notifications-on (multiport-access-logs) nil)

;;Ensures that extended proxy logging is enabled, creating a log object if necessary
(ensure-extended-proxy-log)

;; Define default subnet security for proxy service to avoid a breach of your
;; site's IP security
(define-proxy-subnets
  #|"128.52.0.0"|#				; MIT AI Lab
  #.(local-host-ip-address)			;restrict proxy service to local host as default
  #+UNIX "127.0.0.1"                            ;loopback address is ok as well
  )

;; The list of destination ports for which the HTTP CONNECT is allowed.
;; This is intended to prevent the proxy from being used to anonymize connections to arbitrary destintion ports.
;; By default, only the default HTTPS port, 443, and the default SNEWS port, 563, are enabled.
(setq *proxy-connect-allowed-ports* '(443 563))

;; Define the list of destination subnets for which the HTTP CONNECT is disallowed.
;; This can be used, for example, to prevent use of CONNECT to obtain a tunnel
;; to a host behind a firewall or the local host.
(define-proxy-connect-disallowed-destination-subnets
  #.(local-host-ip-address)			;restrict proxy service to local host as default
  #+UNIX "127.0.0.1"                            ;loopback address is ok as well
  ;; Restrict connections behind a firewall
  #|"127.0.0.0" 
    "169.0.0.0"
    "10.0.0.0"|#)
