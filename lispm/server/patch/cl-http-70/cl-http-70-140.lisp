;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.140
;;; Reason: Function HTML2:DECLARE-BASE-REFERENCE:  use the full namestring when referenceis an interned URL.
;;; Function HTTP::%WRITING-BINARY-FILE:  use resource-length rather than
;;; content-length in the CONTENT-RANGE header.
;;; Function HTTP::%%CACHING-WRITE-BINARY-FILE:  update.
;;; Function HTTP::%%WRITE-BINARY-FILE:  update.
;;; Function HTTP::PARSE-CONTENT-RANGE-HEADER:  handle unknown entity length.
;;; Function HTTP::PRINT-CONTENT-RANGE-HEADER:  handle unknown length.
;;; DEFINE-HEADER :CONTENT-RANGE:  update.
;;; CLOS class HTTP::COMMON-LOG-ENTRY:  mixin bytes-received-log-entry-mixin.
;;; DEFINE-LOG-ENTRY-ALLOCATOR HTTP::ALLOCATE-COMMON-LOG-ENTRY:  add bytes-received arg.
;;; DEFINE-LOG-ENTRY-ALLOCATOR HTTP::ALLOCATE-EXTENDED-COMMON-LOG-ENTRY:  -
;;; Function (CLOS:METHOD HTTP::SNAPSHOT-LOG-ENTRY (HTTP::COMMON-FILE-FORMAT-MIXIN HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Function (CLOS:METHOD HTTP::SNAPSHOT-LOG-ENTRY (HTTP::EXTENDED-COMMON-FILE-FORMAT-MIXIN HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Function HTTP::%COMMON-LOG-BYTES:  -
;;; Function (CLOS:METHOD HTTP::WRITE-LOG-ENTRY (HTTP::COMMON-LOG-ENTRY T)):  -
;;; Function (CLOS:METHOD HTTP::WRITE-LOG-ENTRY (HTTP::EXTENDED-COMMON-LOG-ENTRY T)):  -
;;; DEFINE-LOG-ENTRY-ALLOCATOR HTTP::ALLOCATE-NOTIFICATION-LOG-ENTRY:  -
;;; DEFINE-LOG-ENTRY-ALLOCATOR HTTP::ALLOCATE-CONSOLE-NOTIFICATION-LOG-ENTRY:  -
;;; Function (CLOS:METHOD HTTP::SNAPSHOT-LOG-ENTRY (HTTP::NOTIFICATION-LOG-FORMAT-MIXIN HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Function (CLOS:METHOD HTTP::SNAPSHOT-LOG-NOTIFICATION (HTTP::ASYNCHRONOUS-LOG-NOTIFICATION-MIXIN HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Function (CLOS:METHOD HTTP::WRITE-LOG-ENTRY (HTTP::CONSOLE-NOTIFICATION-LOG-ENTRY T)):  -
;;; Function (CLOS:METHOD HTTP::WRITE-LOG-ENTRY (HTTP::NOTIFICATION-LOG-ENTRY T)):  -
;;; Variable HTTP::*SERVER-METHODS-RECEIVING-DATA*:  -
;;; Written by JCMa, 8/13/01 19:33:04
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.6,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.139,
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
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Lock Simple Patches 1.0, Jcma 44,
;;; HTTP Proxy Server 6.24, HTTP Client Substrate 4.16, Statice Server 466.2,
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
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;HTML2.LISP.301"
  "HTTP:SERVER;SERVER.LISP.913"
  "HTTP:SERVER;HEADERS.LISP.510"
  "HTTP:SERVER;CLASS.LISP.52"
  "HTTP:SERVER;LOG.LISP.216"
  "HTTP:SERVER;SERVER.LISP.914"
  "HTTP:SERVER;VARIABLES.LISP.195"
  "HTTP:SERVER;LOG.LISP.217")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.301")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

(define declare-base-reference (reference &key (stream *output-stream*))
  "Declares REFERENCE to be the base URL for a html page.
   This should appear once in the headers section of a document.
   When REFERENCE is a URL, the directory URL is automatically computed."
  (%issue-command ("BASE" stream :fresh-line t :trailing-line t)
    (%write-command-key-arg stream "HREF" (etypecase reference
                                            (url:url (url::name-string reference))
                                            (string reference)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.913")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

;;;------------------------------------------------------------------- 
;;;
;;; WRITING BINARY FORMATS
;;;

;; Only handle a single range spec until chunking content transfer available
;; to ship over the mime multipart ranges.  6/25/96 -- JCMa.
;; Add multiple ranges sometime. 10/5/99 -- JCMa.
(defmacro %writing-binary-file ((stream url content-type resource-length last-modification version &key charset)
                                range-copy-form copy-form)
  `(let ((expires (expiration-universal-time ,url))
         (cache-control (url:response-cache-control-directives ,url)))
     (handling-conditional-get (stream :last-modification ,last-modification :character-set ,charset
                                       :entity-tag ,version :expires expires :cache-control cache-control :termination-line-p t)
       (let ((languages (languages ,url))
             (range (get-header :range)))
         ;; if more than one range, send whole resource for now.  6/25/96 -- JCMa.
         (cond ((and range (null (cddr range))) ;; Send a byte range
                (destructuring-bind (start-byte last-byte) (second range)
                  (multiple-value-bind (start end content-length) 
                      (byte-range-parameters start-byte last-byte ,resource-length)
                    (let ((headers `(:content-range (:bytes ,start ,end ,resource-length))))
                      (declare (dynamic-extent headers))
                      (with-successful-response (,stream content-type :status :partial-content :bytes content-length
                                                 :last-modification ,last-modification
                                                 :character-set ,charset
                                                 :entity-tag ,version :expires expires :cache-control cache-control
                                                 :content-location ,url
                                                 :content-language languages
                                                 :additional-mime-headers headers
                                                 :termination-line-p t)
                        ,range-copy-form)))))
               ;; Send the full content
               (t (with-successful-response (,stream ,content-type :status :success :bytes ,resource-length
                                             :last-modification ,last-modification
                                             :character-set ,charset
                                             :entity-tag ,version :expires expires :cache-control cache-control
                                             :content-location ,url
                                             :content-language languages
                                             :termination-line-p t)
                    ,copy-form)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.913")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun %%caching-write-binary-file (data-cache url content-type stream &optional charset last-modification version)
  (let ((resource-length (data-cache-size data-cache)))
    (unless-every
      (last-modification (setq last-modification (data-cache-last-modification data-cache)))
      (version (setq version (data-cache-version data-cache))))
    (%writing-binary-file
      (stream url content-type resource-length last-modification version :charset charset)
      (with-binary-stream (stream :output)
        (write-cache-data data-cache stream start end))
      (with-binary-stream (stream :output)
        (write-cache-data data-cache stream 0 resource-length)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.913")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun %%write-binary-file (pathname url content-type stream &optional charset last-modification version)
  (with-open-file (file-stream pathname :direction :input :element-type '(unsigned-byte 8))
    (let ((resource-length (file-stream-length-in-bytes file-stream)))
      (unless-every
        (last-modification (setq last-modification (file-stream-modification-date file-stream)))
        (version (setq version (file-stream-version file-stream))))
      (%writing-binary-file
        (stream url content-type resource-length last-modification version :charset charset)
        (stream-copy-byte-range file-stream stream start end)
        (stream-copy-until-eof file-stream stream :binary)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.510")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun parse-content-range-header (string &optional (start 0) (end (length string)))
  (with-string-trim-bounds (*white-space-chars* string start end)
    (let (pos1 start-pos pos2 last-pos pos3 length-pos)
      (cond ((and (setq pos1 (char-position #\space string start end))
		  (setq start-pos (1+ (the fixnum pos1)))
		  (setq pos2 (char-position #\- string start-pos end))
		  (setq last-pos (1+ (the fixnum pos2)))
		  (setq pos3 (char-position #\/ string last-pos end))
		  (setq length-pos (1+ (the fixnum pos3))))
	     `(,(%tokenize-header-keyword string start pos1)
	       ,(parse-integer string :start start-pos :end pos2 :radix 10)	;start postion
	       ,(parse-integer string :start last-pos :end pos3 :radix 10)	;last position
	       ,.(unless (eql #\* (aref string length-pos))	;entity length unknown
		 (list (parse-integer string :start length-pos :end end :radix 10)))))	;entity length
	    (t (error 'bad-range-header-value :format-string "Bad value for Content-Range header: ~S"
		      :format-args (list (subseq string start end))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.510")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun print-content-range-header (value stream)
  (destructuring-bind (unit-type start-pos last-pos &optional (entity-length #\*)) value
    (fast-format stream "~A ~D-~D/~D" (string-for-tokenized-header-keyword unit-type)
                 start-pos last-pos (or entity-length #\*))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.510")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :content-range
               (:header :entity)
  :print-string "Content-Range"
  :parse-function 'parse-content-range-header
  :print-function 'print-content-range-header)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.52")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defclass common-log-entry
	  (bytes-received-log-entry-mixin bytes-transmitted-log-entry-mixin status-log-entry-mixin log-entry)
    ((host-name :initarg :host-name :accessor log-entry-host-name)
     (request :initarg :request :accessor log-entry-request)
     (method :initarg :method :accessor log-entry-method)
     (request-time :initarg :request-time :accessor log-entry-request-time)
     (user-name :initarg :user-name :accessor log-entry-user-name))
  (:documentation "The log entry class that writes common log file entries."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.216")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

;;;------------------------------------------------------------------- 
;;;
;;; LOG-ENTRY ALLOCATION
;;;

(define-log-entry-allocator allocate-common-log-entry (log host-name request request-time method status bytes-received bytes-transmitted user-name)
  (allocate-log-entry common-log-entry
		      :owner log :host-name host-name :request request :request-time request-time :method method :status status
		      :bytes-received bytes-received :bytes-transmitted bytes-transmitted :user-name user-name))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.216")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(define-log-entry-allocator allocate-extended-common-log-entry (log host-name request request-time method status bytes-received
								    bytes-transmitted user-name user-agent referer)
  (allocate-log-entry extended-common-log-entry
		      :owner log :host-name host-name :request request :request-time request-time :method method :status status
		      :bytes-received bytes-received :bytes-transmitted bytes-transmitted :user-name user-name
		      :user-agent user-agent :referer referer))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.914")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod snapshot-log-entry ((log common-file-format-mixin) (server server-logging-mixin))
  (let ((host-name (host-log-name server))
	(user-name (%server-user-qualified-name server))
	(request (server-request server t))	; don't lose when transaction reset
	(request-time (server-request-time server))
	(method (server-method server))
	(status (server-status server))
	(bytes-received (server-bytes-received server))	;total bytes received
	(bytes-transmitted (server-bytes-transmitted server)))	;total bytes-transmitted (not number of bytes-transmitted in a document
    (allocate-common-log-entry log host-name request request-time method status bytes-received bytes-transmitted user-name)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.914")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod snapshot-log-entry ((log extended-common-file-format-mixin) (server server-logging-mixin))
  (let ((host-name (host-log-name server))
	(user-name (%server-user-qualified-name server))
	(request (server-request server t))	; don't lose when transaction reset
	(request-time (server-request-time server))
	(method (server-method server))
	(status (server-status server))
	(bytes-received (server-bytes-received server))	;total bytes received
	(bytes-transmitted (server-bytes-transmitted server))	;total bytes-transmitted (not number of bytes-transmitted in a document)
	(header-set (server-headers server))
	user-agent-val referer-val)
    (when header-set				;may be null when client drops connection (408)
      (with-header-values (user-agent referer) (server-headers server)
	(setq user-agent-val user-agent
	      referer-val referer)))
    (allocate-extended-common-log-entry log host-name request request-time method status bytes-received bytes-transmitted
					user-name user-agent-val referer-val)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.216")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(define-log-entry-allocator allocate-notification-log-entry (log host-name port request request-time method status bytes-received bytes-transmitted
								 user-name user-agent referer requests-completed cpu-time elapsed-time proxy-p)
  (allocate-log-entry notification-log-entry
		      :owner log :host-name host-name :port port :request request :request-time request-time :method method :status status
		      :bytes-received bytes-received :bytes-transmitted bytes-transmitted :user-name user-name :user-agent user-agent :referer referer
		      :requests-completed requests-completed :cpu-time cpu-time :elapsed-time elapsed-time :proxy-p proxy-p))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.216")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(define-log-entry-allocator allocate-console-notification-log-entry (log host-name port request request-time method status bytes-received bytes-transmitted
									 user-name user-agent referer requests-completed cpu-time elapsed-time proxy-p)
  (allocate-log-entry console-notification-log-entry
		      :owner log :host-name host-name :port port :request request :request-time request-time :method method :status status
		      :bytes-received bytes-received :bytes-transmitted bytes-transmitted :user-name user-name :user-agent user-agent :referer referer
		      :requests-completed requests-completed :cpu-time cpu-time :elapsed-time elapsed-time :proxy-p proxy-p))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.914")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod snapshot-log-entry ((log notification-log-format-mixin) (server server-logging-mixin))
  (let ((requests-completed (server-requests-completed server))
	(host-name (host-log-name server))
	(user-name (%server-user-qualified-name server))
	(request (server-request server t))
	(request-time (server-request-time server))
	(method (server-method server))
	(status (server-status server))
	(port *standard-http-port*)		;stream data may no longer be valid
	(bytes-received (server-bytes-received server))	;total bytes received
	(bytes-transmitted (bytes-transmitted (server-stream server)))	;total bytes (not number of bytes in a document)
	(proxy-p (server-proxy-request-p server))
	header-set cpu-time elapsed-time user-agent-val referer-val)
    (unless proxy-p
      (when (setq header-set (server-headers server)))
      (with-header-values (user-agent referer) header-set
	(setq user-agent-val user-agent
	      referer-val referer)))
    (psetq cpu-time (cpu-time server)
	   elapsed-time (elapsed-time server))
    (allocate-notification-log-entry log host-name port request request-time method status bytes-received bytes-transmitted user-name
				     user-agent-val referer-val requests-completed cpu-time elapsed-time proxy-p)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.914")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod snapshot-log-notification ((log asynchronous-log-notification-mixin) (server server-logging-mixin))
  (let ((requests-completed (server-requests-completed server))
	(host-name (host-log-name server))
	(user-name (%server-user-qualified-name server))
	(request (server-request server t))
	(request-time (server-request-time server))
	(method (server-method server))
	(status (server-status server))
	(port *standard-http-port*)		;stream data may no longer be valid
	(bytes-received (server-bytes-received server))	;total bytes received
	(bytes-transmitted (bytes-transmitted (server-stream server)))	;total bytes (not number of bytes in a document)
	(proxy-p (server-proxy-request-p server))
	header-set cpu-time elapsed-time user-agent-val referer-val)
    (unless proxy-p
      (when (setq header-set (server-headers server)))
      (with-header-values (user-agent referer) header-set
	(setq user-agent-val user-agent
	      referer-val referer)))
    (psetq cpu-time (cpu-time server)
	   elapsed-time (elapsed-time server))
    (allocate-console-notification-log-entry log host-name port request request-time method status bytes-received bytes-transmitted
					     user-name user-agent-val referer-val requests-completed cpu-time elapsed-time proxy-p)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;VARIABLES.LISP.195")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp;-*-")

(defconstant *server-methods-receiving-data* '(:put)
  "HTTP methods where a server receives data.")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.217")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

;;;------------------------------------------------------------------- 
;;;
;;; WRITE-LOG-ENTRY
;;;

(defmacro %common-log-bytes (method status bytes-transmitted bytes-received)
  "Determines the correct bytes to log according to METHOD."
  `(if (and (member ,method *server-methods-receiving-data*)	;log the bytes received for POST and PUT methods
	    (< 199 ,status 300))		;only when input accepted successfully
       ,bytes-received
       ,bytes-transmitted))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.217")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod write-log-entry ((entry-data console-notification-log-entry) stream)
  (declare (ignore stream))
  (with-slots (owner host-name port request method status bytes-received bytes-transmitted user-name
		     user-agent referer requests-completed cpu-time elapsed-time proxy-p) entry-data
    (when (log-notification owner)
      (flet ((notify-the-console (log-stream)
	       (%write-console-window-notification
		 host-name port proxy-p requests-completed request nil status (%common-log-bytes method status bytes-transmitted bytes-received)
		 user-name user-agent referer
		 cpu-time elapsed-time log-stream)))
	(declare (dynamic-extent #'notify-the-console))
	(careful-notify-log-window #'notify-the-console)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.217")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod write-log-entry ((entry-data common-log-entry) stream)
  (with-slots (owner host-name request request-time method status bytes-received bytes-transmitted user-name) entry-data
    (%write-common-logfile-entry host-name request request-time status (%common-log-bytes method status bytes-transmitted bytes-received)
				 user-name (log-times-in-gmt-p owner) stream #\space)
    ;; Trailing CR makes this consistently parsable.
    (terpri stream)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.217")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod write-log-entry ((entry-data extended-common-log-entry) stream)
  (with-slots (owner host-name request request-time method status bytes-received bytes-transmitted user-name user-agent referer) entry-data
    (%write-extended-common-logfile-entry
      host-name request request-time status (%common-log-bytes method status bytes-transmitted bytes-received)
      user-name user-agent referer (log-times-in-gmt-p owner) stream #\tab)
    ;; Trailing CR makes this consistently parsable.
    (terpri stream)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.217")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod write-log-entry ((entry-data notification-log-entry) stream)
  (with-slots (owner host-name port request request-time method status bytes-received bytes-transmitted user-name
		     user-agent referer requests-completed cpu-time elapsed-time proxy-p) entry-data
    (when (log-notification owner)
      (%write-console-window-notification
	host-name port proxy-p requests-completed request request-time status (%common-log-bytes method status bytes-transmitted bytes-received)
	user-name user-agent referer cpu-time elapsed-time stream))))

