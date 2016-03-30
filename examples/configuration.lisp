;;; -*- Mode: LISP; Package: HTTP; Syntax: ANSI-Common-Lisp -*-

;;; Copyright 1994-2006, 2009, John C. Mallery.
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;; CONFIGURATION FILE FOR CL-HTTP 
;;;
(in-package :http)

;;  Override local host domain name with a special name to be used as the local context when
;;  exporting URLs.
;(setq http:*http-host-name* "MAC-Winner.ai.mit.edu")

;; The name of the mailing list to receive reports about bugs in the server.
(setq *server-maintainer* "Chun Tian (binghe)")

;; Use localhost as primary network host
(setq *primary-network-host* "localhost")

;; The standard mailing list for bug reports.
(setq *server-bug-list*
      #-(or Genera CMU) "BUG-CL-HTTP"
      #+(or Genera CMU) (www-utils:http-user-email-address))

;; Controls whether stack backtraces are included in automatic bug reporting.
(setq *stack-backtraces-in-bug-reports* t)

;; Controls whether IP addresses are resolved when writing log entries.
;; Production servers should turn this off to avoid the overhead of DNS
;; lookup during logging.
(setq *log-resolve-ip-addresses* nil)

;; Controls whether IP addresses are resolved in all contexts other than
;; logging.
(setq *resolve-ip-addresses*  (if (member :cl-http-no-dns *features*) nil t))

;; Controls whether host names are DNS resolved and connonicalized on
;; the primary DNS name for the host when the URL is interned. Choices
;; are :always, :preferred, :never. Relevant when *resolve-ip-addresses*
;; is non-null.
(setq url:*url-host-name-resolution* (if (member :cl-http-no-dns *features*) :never :always))

;; When non-null, :GET and :HEAD methods auto-export static URLs when
;; they have a parent that is exported with a directory export type.
;; When *auto-export* is :ON-DEMAND, pathnames in exported directories
;; are exported as URL when they are first accessed over HTTP rather
;; than at export time.  This feature trades a fast start up of the
;; server for a small addition time when the URL is first accessed. If
;; the value is T, all URLs are exported at export time.
(setq *auto-export* :on-demand)

;; Load the proxy configuration file as appropriate.
(when (member :cl-http-proxy *features*)
  (load "http:proxy;examples;configuration.lisp"))

;; Initialize all server variables for current host
;; (reset-server-local-host-variables) and perform any other required
;; initializations.
(run-server-initializations t)

;; Set the standard port number and standard protocol serviced by this HTTP server.
;; The normal protocol is :HTTP.  When SSL is implemented, the protocol can be :HTTPS
#-(or Allegro LispWorks Lucid CMU (and MCL OSX)) 
(set-standard-http-port 80 :http)

;; UNIX tends to lose with some implementation on port 80 so most people use 8000
#+(or Allegro LispWorks Lucid CMU SCL)
(set-standard-http-port 8000 :http)
#+(and MCL OSX)
(set-standard-http-port 8001 :http)

;; Maximum number of simultaneous connections.
(set-maximum-number-of-connections 20.)

;; The number of pending connections queued for accept. Connection backlogs greater than this number
;; are dropped.  Production servers should set the number higher to allow, for example, to allow a 5
;; second backlog of pending connections. Mac OS X uses a default of 128, but 1024 could be
;; reasonable for a high volume server.")
#+LispWorks
(setq *connection-backlog* 128)
 
;; The default mail host to use in return address when sending any email
;; associated with the WWW server, for example bug reports/
(setq *default-mailer-host* (local-host-domain-name))

;; Recache the bug report list to be *server-bug-list* @ *default-mailer-host*
(http::email-address-for-bug-reports t)

;; Recache the server mail address (used as the from field) to be *server-maintainer* @ *default-mailer-host*
(http::server-mail-address t)

;; The primary store and forward mail host at the local site.  This is the
;; domain name of the mail host. It may also by the IP address.  If this is
;; NIL, no mail will be sent by functions REPORT-BUG and SEND-MAIL-FROM."
#+(or MCL (and CMU MP) SCL)
(setq smtp:*network-mail-host*  nil)

;; This is a list of all store and forward mail hosts accessible from the
;; site.  This mail hosts will be used whenever the primary mail host is
;; unaccessible.  Mail hosts should be listed in decreasing order of priority.
#+MCL
(setq smtp:*store-and-forward-mail-hosts* nil)

;; Controls whether HTTP clients may specify the desired directory view by
;; providing preferred media types in the accepts header.
(setq *content-negotiate-directory-views*  t)

;; Customize the message issued when server overloaded.
;(setq *overload-message* "This server is operating at capacity now. Please try later.")

;; CONFIGURE SERVER LOGGING

;; Common logs are written to the merge of this directory and the local host.
(setq *standard-log-directory* "http:logs;")

;; Controls whether the times in log file entries are in written in Greenwich
;; Mean Time or not.
(setq *log-times-in-gmt* t)

;; Controls whether the file log stream remains open all the time, or it is
;; reopenned for each log transaction. Production servers should keep the log
;; file stream open.
(log-file-stream-stays-open nil)

;; Controls the class of log used for logging.
;; Current options: server-common-file-log, common-file-log,
;; server-extended-common-file-log.   extended-common-file-log, http-log,
;; extended-http-log
;; Note that server-common-file-log and server-extended-common-file-log
;; automatically compress the log files when compression is available on the
;; platform. The default is to compress.
(setq *log-access-log-class* 'server-common-file-log)

;; Make sure the log object has been initialized.
(ensure-current-log)

;; Write a common log file.
(log-file-logging-on (current-access-logs) t)

;; Show transactions in a log window
;; Turn this off in production servers to save time writing log entries to the notification window.
(log-notifications-on (multiport-access-logs) t)

;; Don't build a datastructure of the log entries
(log-dynamic-logging-on (current-access-logs) nil)

;; Compress log files as appropriate
(log-compression-on (current-access-logs) t)

;; CONFIGURE SECURITY

;; Read Subnets: Define default subnet security for reading all URLs exported by this server.
(define-read-subnets
  #|"128.52.0.0"|#                              ; MIT AI Lab
  )

;; Write Subnets: Define default subnet security for writing all URLs exported by this server.
(define-write-subnets
  #|"128.52.0.0"|#                              ; MIT AI Lab
  )

;; Define default subnet security for proxy service to avoid a breach of your
;; site's IP security in case someone loads the proxy without configuring it.
(unless (member :cl-http-proxy *features*)
  (define-proxy-subnets
    #|"128.52.0.0"|#				; MIT AI Lab
    ))

;; Controls the security policy for side-effecting methods such as PUT or
;; DELETE.  Each security policy imposes minimum the requirements to invoke
;; these methods.  Policies are implemented globally by setting this variable
;; to the appropriate keyword.  The security level of each policy is indicated
;; in increasing level of insecurity after each keyword, assuming that the
;; authentication method is BASIC. Use of more effective authentication
;; methods could mean that authentication would be more meaningful than subnet
;; security.  The following security policies are available:
;;
;;     :ACCESS-CONTROLLED (5) -- Requires URLs to restrict users via either user
;;     authentication or subnet security.
;;
;;     :AUTHENTICATED-USERS (6) -- Requires URLs to restrict users only via user
;;     authentication.
;;
;;     :AUTHENTICATED-USERS-ON-WRITE-SUBNETS (3) -- Requires URLs to restrict users
;;     via both user authentication and write subnet security.
;;
;;     :LOCAL-HOST (2) -- Requires users to be on the local host running the server.
;;
;;     :NONE (1) -- No users can invoke side-effecting methods.
;;
;;     :REMOTE-HOST -- Does requires URLs to control access, but respects any
;;     global or URL level access controls.
;;
;;     :WRITE-SUBNETS (4) -- Requires URLs to restrict access to hosts trusted for 
;;     write operations.
(setq *accept-write-methods* :access-controlled)

;; The pathname holding the disk representation of server authentication
;; information as lisp source code.
(setq *authentication-data-pathname* (pathname "http:pw;authentication-data.lisp"))

;; Password data is automatically initialized by the server.
(run-server-launch-initializations t)

;; If you have not already provided a password for the Webmaster,
;; use INTERN-USER to add the password and SAVE-AUTHENTICATION-OBJECT to save
;; the user object.
#+ignore
(save-authentication-object
  (intern-user :server "Webmaster"
               :password "password"
               :groups '("Webmasters")
               :email-address *bug-http-server*))

;; Intialize the random seed for Digest Authentication.  When first bringing
;; up CL-HTTP at your site, you should evaluate this form 15 times BY HAND in
;; order to build up a reasonable level of randomness in the random seed.
;; Thereafter, the server will automatically initialize this. Applications may
;; want to set *digest-authentication-nonce-life-time* to narrow the window
;; during which replay attacks on digest authenticated URLs are possible.

#+ignore
(digest-authentication-random-seed t)

;; Export export the standard WWW robot control file.
;;(export-url #u"/robots.txt"
;;            :text-file
;;            :pathname (pathname "http:www;robots.text"))

;; The number of threads simultaneously listening for HTTP connections.
#+MCL
(setq *number-of-listening-processes* 5)
#+(or Lucid CMU)
(setq *number-of-listening-processes* 1)

;; The standard color scheme to use on automatically generated HTML pages prior to HTML 4.0 
;; See HTML:CREATE-COLOR-SCHEME and HTML:WITH-STANDARD-DOCUMENT-BODY.
(setq *standard-color-scheme* (create-color-scheme :background :white
                                                   :foreground :black
                                                   :link :blue
                                                   :visited-link :purple
                                                   :active-link :red))

;; The document look to use for HTML automatically generated by CL-HTTP using from HTML 4.0 or XHTML
;; 1.0 For user applications, see: HTML4.0:*STANDARD-DOCUMENT-LOOK*, HTML4.0:CREATE-DOCUMENT-LOOK and
;; HTML4.0::WITH-DOCUMENT-LOOK-HTML If you are not running the examples, you will need to provide an
;; appropriate style sheet here. Use a relative URL in order work over proxies and behind NAT or 
;; firewalls, but for FTP proxy gateway you need a fully qualified URL. When in doubt, if you use
;; "http://www.cl-http.org:8001/cl-http/css/base.css" you will get a reasonable and current style sheet for
;; error reports etc.
(setq *cl-http-document-look* (create-document-look 
                               :style-url  #+ignore "http://www.cl-http.org:8001/cl-http/css/base.css"
                               (make-url-string :scheme *standard-protocol*
                                                :host *http-host-name* ;external host name, if set
                                                :port *standard-http-port*
                                                :path '("cl-http" "css") :name "base" :extension "css")
                               :style-type "text/css"
                               :class 'cl-http-look))

;; Increase the number of packet buffers for better performance
#+(and Genera VLM)
(when (eql 192 neti:*maximum-number-of-wired-packet-buffers*)
  (setq neti:*maximum-number-of-wired-packet-buffers* 1920
	neti:*target-number-of-wired-packet-buffers* 640))
