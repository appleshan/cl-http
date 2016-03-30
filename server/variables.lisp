;;;   -*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp;-*-

;;;
;;; (c) Copyright  1994-2000, 2005-2006, John C. Mallery
;;;     All Rights Reserved.
;;;


;;;------------------------------------------------------------------- 
;;;
;;; SERVER VARIABLES AND CONSTANTS
;;;

(in-package :HTTP)

;; Advise the lisp environment that we have portable CL-HTTP loaded.
(pushnew :cl-http *features*)

(define-parameter *accept-write-methods* :access-controlled
                  "Controls the security policy for side-effecting methods such
as PUT or DELETE.  Each security policy imposes minimum the requirements to invoke these methods.
Policies are implemented globally by setting this variable to the appropriate keyword.
The security level of each policy is indicated in increasing level of insecurity after each keyword,
assuming that the authentication method is BASIC. Use of more effective authentication methods
could mean that authentication would be more meaningful than subnet security.
The following security policies are available:

     :ACCESS-CONTROLLED (5) -- Requires URLs to restrict users via either user
     authentication or subnet security.

     :AUTHENTICATED-USERS (6) -- Requires URLs to restrict users only via user
     authentication.

     :AUTHENTICATED-USERS-ON-WRITE-SUBNETS (3) -- Requires URLs to restrict users
     via both user authentication and write subnet security.

     :LOCAL-HOST (2) -- Requires users to be on the local host running the server.

     :NONE (1) -- No users can invoke side-effecting methods.

     :REMOTE-HOST -- Does requires URLs to control access, but respects any
     global or URL level access controls.

     :WRITE-SUBNETS (4) -- Requires URLs to restrict access to hosts trusted for write operations.")

(define-variable *all-logs* nil
                 "Holds the list of all known log objects.")

(define-variable *access-control-realms* nil
                 "The hash table pointing to authentication realms on the server.")

(define-variable *armor-plated-string-line-length* 1024
                 "Controls the number of characters per line of encoded output.
In cases where RETURN causes truncation, this number should be high enough so
no line breaks occur.")

(define-parameter *authentication-data-pathname* (pathname "http:pw;authentication-data.lisp")
                  "The pathname holding the disk representation of server authentication information as lisp source code.")

(define-parameter *auto-export* :ON-DEMAND
                  "When non-null, :GET and :HEAD methods auto-export static URLs
when they have a parent that is exported with a directory export type.
When *auto-export* is :ON-DEMAND, pathnames in exported directories are
exported as URLs when they are first accessed over HTTP rather than at export time.
This feature trades a fast start up of the server for a small additional time when
the URL is first accessed. If the value is T, all URLs are exported at export time.")

(define-parameter *bug-http-server* nil
                  "The standard email address used for sending mail from HTTP servers.") 

(define-parameter *bug-report-destination* :log-window
  "The destination for bug reports.
One of :log-window, :mail or nil.")

(define-parameter *cache-directory-indexes* nil
                  "Controls whether directory indexes are cached in dynamic memory or recomputed from disk on demand.
Production servers may use this to speed up access to URL directory indexes.
This variable can have the values:

     NIL           -- Never cache
     T              -- Always cache
     {Seconds} -- Always cache but recompute any caches older than this number of seconds.

Some ports may not register changes to existing files as directory modifications,
meaning that byte counts may be off. However, all ports should recache directory
lists when files are added or deleted.")

(define-variable *cache-hysterisis* 60          ;one minute either way
                 "Controls the fence post size for proxy refreshes and for conditional gets.")

(define-parameter *check-document-versions* t
                  "When non-null, the PUT method checks document versions.")

(defvar *cl-http-user-init-file* "cl-http-init.lisp"
  "The file name for the CL-HTTP user initialization file.
This file is loaded at server initialization time, before variables and parameters are reset")

(defparameter *content-negotiate-directory-views* nil
  "Controls whether HTTP clients may specify the desired directory view
by providing preferred media types in the  accepts header.")

(defparameter *data-cache-window-frequency* 250
  "The default window within which data source access frequencies are computed.")

(defparameter *data-universe-audit-accesses* nil
  "The default value for whether data elements are audited by new data universes.")

(defparameter *data-universe-audited-elements* 100
  "The default number of elements audited for caching.")

(defparameter *data-universe-maximum-cache-size* 5000000        ;default to 5 million bytes
  "The default maximize amount of memory in bytes which cached data may consume in any single data universe.")

(defparameter *data-universe-maximum-cache-elements* 100
  "The default maximize number of elements allowed in any single data universe at any one time.")

(defparameter *data-universe-revalidation-interval* (* 15. 60.) ;default to 15 minutes
  "The default interval in seconds between revalidation of data cached by data universes.")

(define-parameter *debug-server* nil
                  "Controls handling of internal server errors.
It can take on the values:

                              T          Turns normal debugging on
                              NIL        Turns debugging off
                              :CONDITIONS Turns on debugging of unhandled HTTP conditions
                              :ERRORS     Turns on normal debugging.")

(defparameter *digest-authentication-nonce-life-time* 30   ;default to 30 seconds
  "The number of seconds during which a nonce remains valid.
This number can be set higher or lower depending on the size window
a site is willing to allow for replay attacks on a specific URI.")

(defparameter *durable-form-values* t
  "Controls whether HTTP form processing conses new strings 
or indirect arrays into the form buffer for query values and query baggage.")

(define-constant *cl-http-home-page-url-string* "http://www.cl-http.org:8001/")

;; set to a number commensurate with log entries when used
(define-parameter *default-log-size* 20.
                  "Default size to make tables for dynamic logs.")

(define-variable *default-mailer-host* nil
                 "The default mail host to use for sending any email associated with the WWW server.")

(define-parameter *default-pathname* nil
                  "The default pathname for use by the server.")

(defconstant *escape-character* #\%
  "The character indicating a character encoding follows.")

(define-constant *escaped-characters* '(#\space #\? #\# #\= #\& #\% #\+ #\< #\> #\/ #\: #\")
                 "Characters that are always escaped in search strings and POST bodies.")

(define-constant *file-exists-supersede-action* #+Genera :new-version #-Genera :supersede
                 "Controls the supersession of files as keyword passed to the IF-EXISTS argument of OPEN.
Operations such as HTTP PUT use this to supersede an existing file with a new version.")

(define-parameter *file-upload-maximum-size* 10000000
		  "The maximum number of bytes accepted for a single file upload.
No multipart MIME block may exceed this threshold during file upload.")

(define-constant *fragment-delimiter-character* #\#
                 "The character delimiting URL fragments.")

(declaim (fixnum *header-fill-column*))

(defparameter *header-fill-column* 72.
  "The number of characters per line when filling header values.")

(declaim (fixnum *header-set-index-size*))

(defparameter *header-set-index-size* 14
  "The default number of headers for a HEADER-SET.")

(declaim (float *header-set-growth-factor*))

(defparameter *header-set-growth-factor* 1.2
  "Factor by HEADER-SET data structures are automatically grown when current sizes are exceeded.")

(define-variable *headers* nil
                 "Bound to the current HEADER-SET with a server binding context.")

(define-parameter *http-host-name* nil
                  "This is a string with host name serviced by this http server.
When non-null, this overrides the primary name of the host. Local context will use this name or IP string.")

(define-constant *http-known-versions* '(:http/0.9 :http/1.0 :http/1.1)
                 "All known versions of the HTTP protocol in strictly ascending order.")

(define-variable *http-package* (find-package :http))

(define-parameter *http-version*
  #+(or Genera clozure-common-lisp MCL allegro Franz-Inc LispWorks (and CMU mp) scl) "HTTP/1.1"
  #-(or Genera clozure-common-lisp MCL allegro Franz-Inc LispWorks (and CMU mp) scl) "HTTP/1.0")

(declaim (fixnum *http-version-length*))

(define-constant *http-version-length* #.(length "HTTP/1.1"))

(define-parameter *image-sizes-default-automatically* t
                  "Controls whether images sizes are automatically emitted by NS1.1:IMAGE
during HTML generation.  When non-null, image sizes are cached at export time for all image
formats for which URL:GET-IMAGE-SIZE is defined.")

(define-variable *keyword-package* (find-package :keyword))

(declaim (fixnum *line-buffer-size*))

;; Max URL size + http version + 2 spaces + max method length.  12/18/95 -- JCMa.
(define-parameter *line-buffer-size* (+ 1024. #.(length "HTTP/1.0") 2. 6.)
                  "The default size of line buffers.")

(define-parameter *post-form-buffer-size* *line-buffer-size*
                  "The default size of line buffers used for POST url-encoded forms.")

(define-variable *local-context* nil
                 "The local context prepended to partial URL specifications.")

(define-variable *local-host-address* nil
                 "The IP number for the local host.")

(define-variable *local-host-domain-name* nil
                 "Domain name string for the local host. Can be overridden by *http-host-name*.")

(define-variable *local-host-ip-address*  nil
                 "IP address string for the local host.")

(define-variable *local-port-context-alist* nil
                 "A cache of the local context by local port.")

(define-variable *local-scheme* nil
  "The local scheme prepended to partial URL specifications. Options include :HTTP, :HTTPS.")

;; Documentation is defined in http:server;log.lisp
(define-variable *log-access-log-class* 'common-file-log)

(define-variable *log-file-stream-stays-open* nil
                 "Controls whether the file log stream remains open all the time.
Use the function LOG-FILE-STREAM-STAYS-OPEN to change this value.")

(define-parameter *log-resolve-ip-addresses* nil
                  "Controls whether IP addresses are resolved when writing log entries.")

(declaim (fixnum *log-default-notification-interval*))

(defparameter *log-default-notification-interval* 5
  "The default number of entries to write before forcing output on a log display notification stream.")

(declaim (fixnum *log-default-notification-timeout*))

(defparameter *log-default-notification-timeout* 1
  "The default number of seconds to wait for a log stream notification display to accept the update.")

(defparameter *log-default-notification-port* :multiport
  "The default port for which the log stream displays information.")

(define-variable *log-times-in-gmt* t
                 "Controls whether the times in log entries are in written in Greenwich Mean Time or not.")

(declaim (fixnum *maximum-number-of-connections*))

(define-parameter *maximum-number-of-connections* 20.
                  "This is the maximum number of simultaneous HTTP connections
before the system returns the overload message.")

(defconstant *mime-version* "1.0")

(declaim (fixnum *number-of-connections*))

(define-variable *number-of-connections* 0
                 "The current number of HTTP connections.")

(define-variable *overload-message* nil
                 "This is either null, a string or a function on condition and stream.
It is used to generate the message reported to users when the server is operating at capacity and
can accept no additional requests.")

(define-constant *pathname-crlf-extension* "crlf"
                 "The pathname extension for CRLF canonical text files.")

(declaim (fixnum *persistent-connection-maximum-requests*))

(define-parameter *persistent-connection-maximum-requests* 100.
                  "The maximum number of requests served for each persistent connection.")

(declaim (fixnum *persistent-connection-timeout*))

(define-parameter *persistent-connection-timeout* 10.
                  "The seconds before an inactive persistent connection times out.")

(defparameter *preserve-http-server-resources* t
  "Controls whether resourced objects used by HTTP servers are deallocated 
when the server object is freed or preserved for reuse when the server is reallocated.")

(define-constant *protocols-served-natively* '(:http #+CL-HTTP-SSL :https)
  "The Internet protocols that are served natively by the server.")

(defparameter *proxy-intern-url-status* :uninterned
  "Controls how URLs are created when the proxy encounters unknown URLs.
The values can be :UNINTERNED or :CREATE.")

(define-variable *proxy-service* nil
                 "A list of proxy ports available to clients in *PROXY-SUBNETS*.")

(define-parameter *random-seed-pathname* "http:pw;random-seed.lisp"
                  "Pathname holding the random seed for Digest Authentication.")

(define-parameter *realm-user-table-size* 10.
                  "The default size for making hash tables holding the user objects for realms.")

(define-parameter *realm-group-table-size* 10.
                  "The default size for making hash tables holding the group objects for realms.")

(define-parameter *realm-access-control-table-size* 10.
                  "The default size for making hash tables holding the access-control objects for realms.")

(define-parameter *reject-connection-message* "Insufficient Resources: Server too busy."
                  "The string returned when the server is too busy to respond politely.")

(declaim (fixnum *reject-connection-threshold*))

(define-parameter *reject-connection-threshold* 25.
                  "When number of simultaneous connections reaches this number,
they are rejected without returning an overload message and without logging.")

(define-parameter *report-idle-connection-scavenges* t
                  "Controls whether each scavenge of an idle HTTP connection is bug reported.
A high volume server may generate a large volume of these bug reports.")

(define-parameter *report-time-parsing-errors* nil
  "Controls whether the server reports time-parsing errors via the notification service
 (for example by sending emails).")

(define-parameter *resolve-ip-addresses* t
                  "Controls whether IP addresses are resolved in all contexts other than logging.")

(define-variable *search-url-intern-mode* :uninterned
                 "Controls whether search URL are interned (:create) or not interned (:uninterned), the default.
The default is :uninterned to avoid bloating the URL table. :create can be used if there are a small number of
options or one wishes to trace the pattern.")

(define-parameter *secure-subnets* nil
                  "The list of trusted subnets for IP-subnet level access control.")

(define-variable *server* nil
                 "Bound to the http server running in the current environment.")

(define-parameter *server-bug-list* nil
                  "The standard bug list for reporting server bugs. Used to compute *bug-http-server*.")

(define-variable *server-class* 'server
                 "The class of HTTP server to use.
This allows you to customize the server you run.
Use CLEAR-SERVER-RESOURCE when changing this.")

(define-variable *server-initialization-list* nil
                 "The cold initialization list for the CL-HTTP server.")

(define-variable *server-launch-initialization-list* nil
                 "The initialization list for launching the CL-HTTP server.")

(define-variable *shutdown-initialization-list* nil
  "The initialization list for shutting down CL-HTTP.")

(declaim (integer *server-life-time*))

(define-variable *server-life-time* (* 1000. 60. 5)     ;default is five minutes
                 "Controls the maximum time an HTTP server can live.
Time is in milliseconds.")

(define-parameter *server-mail-address* nil
                  "The standard email address used for sending mail from HTTP servers.")

(defconstant *server-native-url-schemes* '(:http #+CL-HTTP-SSL :https)
  "The URL scheme that CL-HTTP is able to serve natively.")

(define-parameter *server-maintainer* "Webmaster"
                  "The name of the mailing list to receive reports about bugs in the server.")

(defconstant *server-methods-receiving-data* '(:put)
  "HTTP methods where a server receives data.")

(declaim (fixnum *server-timeout*))

(define-variable *server-timeout* (* 60. 60. 5) ;default is five minutes
                 "Controls the amount of idle time before the HTTP server drops the connection.
Time is in 60ths of a second.")

(define-parameter *server-version* nil
                  "The string indicating the running HTTP server, server version, and operating environment.")

#+CL-HTTP-SSL
(defvar *ssl-implementation-version* nil
  "Holds the implementation and version of the SSL library used by CL-HTTP.
See: SSL-IMPLEMENTATION-TYPE and SSL-IMPLEMENTATION-VERSION.")

(define-parameter *stack-backtrace-number-of-frames* 30
                  "Default number of stack frames to display.")

(defparameter *stack-backtraces-in-bug-reports* nil
  "Controls whether stack backtraces are included in automatic bug reporting.")

(defconstant *standard-character-type* 
  #+(or Genera ACLPC) 'character
  #+(and MCL (not CCL-5.2))  (symbol-value 'ccl:*default-character-type*)
  #+(and MCL CCL-5.2)  'base-char 
  #+(or Allegro Lucid CMU scl clozure-common-lisp (and LispWorks (not LispWorks3.2)) cormanlisp clisp ecl) 'base-char
  #+sbcl 'character
  #-(or Genera Allegro ACLPC MCL clozure-common-lisp (and LispWorks (not LispWorks3.2)) Lucid CMU scl cormanlisp clisp ecl sbcl) 'base-char)

(define-constant +standard-text-copy-mode+
  #+(or Genera MCL LispWorks-UNIX) :crlf
  #+(or clozure-common-lisp allegro Franz-Inc LispWorks-PC (and lispworks win32) Lucid CMU scl cormanlisp clisp ecl sbcl) :text
  #-(or Genera clozure-common-lisp MCL LispWorks-UNIX allegro Franz-Inc LispWorks-PC (and lispworks win32) Lucid CMU scl cormanlisp clisp ecl sbcl) 
  (Error "Need to define +standard-text-copy-mode+")
  
  "Controls the HTTP copy mode for static files with MIME content type text.
Because this decision is made at compile time, the system must be recompiled.

The options are :CRLF and :TEXT. :CRLF is a binary format that is ready to
send and for which content length is known in advance.  It allows ASCII
translating architectures like the Genera and MCL to maintain persistent
connection in HTTP 1.1. UNIX systems may live with :TEXT because they do not
perform translation.  However, :CRLF has advantages in computing byte offsets
and digital signatures. Using the CRLF mode incurs a penalty of doubling the
disk space consumed, but improves HTTP performance.")

(define-variable *standard-configuration-pathname* "http:pw;configuration.lisp"
                 "Configurations are written to the merge of this file and the local host.")

(define-variable *standard-configuration-write-file-p* nil
                 "Controls whether a remote server configuration is saved to a file.")

(define-variable *standard-export-pathnames* nil
                 "The standard list of pathnames that export URLs on load.")

(define-parameter *standard-log-directory* "http:logs;"
                  "The standard directory of the server's log file.")

(declaim (fixnum *standard-http-port*))

(define-parameter *standard-http-port* 80
                  "This is the standard port number serviced by this http server.")

(define-parameter *standard-protocol* :http
                  "This is the standard HTTP protocol serviced by this server. Options are :HTTP or :HTTPS")

(defparameter *url-file-data-universe-name* "URL File Data Cache"
  "The default data universe used to cache URL file data in memory.")

(define-constant *uri-reserved-characters* '(#\; #\/ #\? #\: #\@ #\& #\= #\+)
                 "Characters that may reserved for special meaning with URL schemes.")

(define-constant *uri-extra-characters* '(#\! #\* #\' #\( #\) #\,)
                 "Characters that have no special meaning within URL schemes.")

(define-constant *uri-safe-characters* '(#\$ #\- #\_ #\.)
                 "Characters that are safe within URL schemes.")

(eval-when (compile eval)
  (defun %uri-control-characters ()
    `(,(code-char 127)
      .,(loop for i from 0 upto 31
              collecting (code-char i)))))

;; Check to see how this maps for each platform's character set. -- JCMa 8/1/1997.
(define-constant *uri-control-characters* '#.(%uri-control-characters)
                 "ASCII control characters that must be escaped in URL schemes.")

(define-constant *uri-unsafe-characters* '#.`(,.(%uri-control-characters) #\Space #\" #\# #\% #\< #\>)
                 "Characters that must be escaped in URL schemes.")

(define-parameter *url-areas* nil
                  "Controls whether URL areas are active or not.
When active, uninterned URL inferiors within the scope of an exported superior
compute using inherited parameters from the superior.")

;; There are many more interesting user agents out there.  Typical user
;; agents are now Mozilla (and related), MSIE, Opera and Safari (Apple).
;; Predicates on universally-implemented features should just return t
;; for unknown browsers -- JCMa 9/1/2003
(define-parameter *user-agent-capabilities*
    '((:html4 
       (:msie)
       (:safari)
       (:mozilla))
      (:tables 
       (:msie)
       (:mozilla)
       (:ncsa-mosaic)
       (:safari)
       (:opera)
       (:spyglass_mosaic :|2.1| :|2.1F9|)
       (:macweb :|2.0| :|2.0A3E| :|2.0B1E|)
       (:icab))
      (:frames
       (:msie)
       (:mozilla)
       (:safari)
       (:opera)
       (:icab))
      (:client-side-image-maps
       (:msie)
       (:mozilla)
       (:safari)
       (:opera)
       (:spyglass_mosaic :|2.1| :|2.1F9|)
       (:icab))
      (:cookies
       (:msie)
       (:mozilla)
       (:safari)
       (:opera)
       (:icab))
      (:java-script
       (:msie)
       (:mozilla)
       (:safari)
       (:opera)
       (:icab))
      (:java
       (:msie)
       (:mozilla)
       (:safari)
       (:opera)
       (:HotJava)))
  "Maps capabilities to user agents.
Entries are (capability &rest (user-agent &rest versions)).
No versions means that all versions handle the capability.")

(defconstant *white-space-chars* '(#\space #\tab)
  "Characters to treat as white space.")

;; declare special variables for conventional architectures.
(declaim (fixnum *cache-hysterisis*
		 *log-default-notification-interval*
		 *log-default-notification-timeout*
                 *maximum-number-of-connections*
                 *number-of-connections*
                 *reject-connection-threshold*
                 *server-timeout*))
