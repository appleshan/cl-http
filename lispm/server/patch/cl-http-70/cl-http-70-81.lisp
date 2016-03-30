;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.81
;;; Reason: Function HTTP::HTTP-VERSION-MIN:  new
;;; Function HTTP::HTTP-VERSION-MAX:  new
;;; DEFINE-CONDITION HTTP::EXPECTATION-FAILED:  new condition.
;;; Variable HTTP::*STATUS-CODE-ALIST*:  update.
;;; Written by JCMa, 9/30/00 19:46:36
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.80,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, HTTP Proxy Server 5.32,
;;; HTTP Client Substrate 3.24, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Documentation Patches 1.0,
;;; Genera 8 5 Clim Patches 1.0, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; Jcma 42, HTTP Client 49.11, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
;;; 1024x718 24-bit TRUE-COLOR X Screen JCMA-ISDN:0.0 with 224 Genera fonts (eXodusPowerPC 7.0  (c) 1998 White Pine Software,
;;; Inc. R6300),
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
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.14),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.5),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.4),
;;; Retry invoke patch (from W:>Reti>retry-invoke-patch).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;UTILS.LISP.465"
  "HTTP:SERVER;HTTP-CONDITIONS.LISP.184"
  "HTTP:SERVER;UTILS.LISP.466")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.465")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(declaim (inline http-version-min))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.465")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(define http-version-min (version-1 version-2)
  "Returns the lowest protocol version of VERSION-1 and VERSION-2."
  (if (http-version-less-p version-1 version-2)
      version-1
      version-2))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.465")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(declaim (inline http-version-max))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.465")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(define http-version-max (version-1 version-2)
  "Returns the highest protocol version of VERSION-1 and VERSION-2."
  (if (http-version-less-p version-1 version-2)
      version-2
      version-1))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.184")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition expectation-failed
                  (precondition-failed)
  ((status-code :initform 417)
   (reason :initform "Expectation Failed")))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.184")
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
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.466")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(declaim (inline http-version-greater-p))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.466")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(define http-version-greater-p (version-1 version-2)
  "Returns non-null if VERSION-1 is a higher HTTP protocol version than VERSION-2.
Both arguments must be protocol version keywords of the form :HTTP/1.1"
  (http-version-less-p version-2 version-1))

