;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.137
;;; Reason: CLOS class URL:SERVER-TIMEOUT-MIXIN:  fix bug.
;;; Variable HTTP::*STATUS-CODE-ALIST*:  map 407 correctly.
;;; Remove function (CLOS:METHOD URL:DIRECTORY-NAME-STRING (URL:HTTP-URL)): undefine.
;;; Function (CLOS:METHOD URL:DIRECTORY-NAME-STRING (URL::PATH-MIXIN)):  relocate to make available on FTP, gopher, and file URLs.
;;; CLOS class URL::FILE-URL-PATH-MIXIN:  new.
;;; CLOS class URL::FILE-DIRECTORY:  use file-url-path-mixin instead of path-mixin
;;; CLOS class URL::FILE-PATHNAME:  ditto.
;;; Function (CLOS:METHOD URL:DIRECTORY-NAME-STRING (URL::FILE-URL-PATH-MIXIN)):  provide specialized directory method.
;;; Written by JCMa, 6/06/01 13:07:04
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.136,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 54.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.4, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.26, Genera 8 5 Macivory Support Patches 1.0,
;;; Genera 8 5 Mailer Patches 1.1, Genera 8 5 Joshua Patches 1.0,
;;; Genera 8 5 Statice Runtime Patches 1.0, Genera 8 5 Statice Patches 1.0,
;;; Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Jcma 43, HTTP Proxy Server 6.22,
;;; HTTP Client Substrate 4.14, Statice Server 466.2, HTTP Client 50.9,
;;; Image Substrate 440.4, Essential Image Substrate 433.0, MacIvory Support 447.0,
;;; W4 Constraint-Guide Web Walker 45.10, W4 Examples 15.0,
;;; Experimental CL-HTTP CLIM User Interface 1.0, CL-HTTP Documentation 3.0,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.16,
;;; DEC OSF/1 V4.0 (Rev. 110),
;;; 1152x718 24-bit TRUE-COLOR X Screen JCMA-ISDN:0.0 with 224 Genera fonts (eXodus 7.1  (c) 2000 White Pine Software,
;;; Inc. R7100),
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
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;URL-CLASS.LISP.22"
  "HTTP:SERVER;HTTP-CONDITIONS.LISP.199"
  "HTTP:SERVER;URL.LISP.426"
  "HTTP:SERVER;URL-CLASS.LISP.23"
  "HTTP:SERVER;URL.LISP.427")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL-CLASS.LISP.22")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: URL; Base: 10 -*-")

(defclass server-timeout-mixin
	  ()
    ((life-time :initarg :life-time :accessor http::server-life-time)
     (timeout :initarg :timeout :accessor http::server-timeout))
  (:documentation "A mixin that allows the life time or the idle time for an HTTP connection 
to be specialized on a per URL basis."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.199")
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


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.426")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(SCL:FUNDEFINE '(METHOD DIRECTORY-NAME-STRING (HTTP-URL)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.427")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod directory-name-string ((url path-mixin) &optional recompute-p)
  (with-value-cached (url :directory-name-string :recompute-p recompute-p)
    (with-output-to-string (stream)
      (write-scheme-prefix url stream)
      (write-host-port-string url stream)
      (write-path url stream)
      (write-char #\/ stream))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL-CLASS.LISP.23")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: URL; Base: 10 -*-")

(defclass file-url-path-mixin (path-mixin) ())


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL-CLASS.LISP.23")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: URL; Base: 10 -*-")

(defclass file-directory (file-url file-url-path-mixin) ())


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL-CLASS.LISP.23")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: URL; Base: 10 -*-")

(defclass file-pathname (object-mixin file-url file-url-path-mixin) ())


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.427")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod directory-name-string ((url file-url-path-mixin) &optional recompute-p)
  (with-value-cached (url :directory-name-string :recompute-p recompute-p)
    (with-output-to-string (stream)
      (write-scheme-prefix url stream)
      (write-host-port-string url stream)
      (write-char #\/ stream)
      (write-path url stream))))

