;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.182
;;; Reason: Add facility for remapping local context in URLs when behind a reverse proxy.
;;; 
;;; DEFINE-HEADER :X-FORWARDED-HOST: apache reverse proxy entension header.
;;; DEFINE-HEADER :X-FORWARDED-FOR:  -
;;; DEFINE-HEADER :X-FORWARDED-SERVER:  -
;;; DEFINE-CONDITION HTTP::DOCUMENT-MOVED:  add cache slot for remapped URLs.
;;; Function HTTP::ABSOLUTIZE-RELATIVE-URL:  -
;;; Function HTTP::%MAKE-CONTEXT:  use absolutize-relative-url.
;;; Function HTTP::WRITE-URL-REMAPPING-CONTEXT:  -
;;; Function HTTP::PRINT-LOCATION-HEADER:  enable URL context remapping.
;;; DEFINE-HEADER :LOCATION:  update
;;; DEFINE-HEADER :CONTENT-LOCATION:  update header.
;;; Function HTML2:DECLARE-BASE-REFERENCE:  Enable URL context remapping.
;;; Function NS2.0:DECLARE-BASE-REFERENCE:  ditto.
;;; Function NS11::DECLARE-REFRESH-RATE*:  ditto.
;;; DEFINE-HEADER :CONTENT-LOCATION:  uodate.
;;; Written by JCMa, 10/15/03 14:24:45
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5-inc1.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.6,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.181,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.4, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.5, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.41, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Documentation Patches 1.0,
;;; Genera 8 5 Clim Patches 1.3, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; Jcma 44, HTTP Proxy Server 6.34, HTTP Client Substrate 4.23, HTTP Client 51.6,
;;; Image Substrate 440.4, Essential Image Substrate 433.0,
;;; W4 Constraint-Guide Web Walker 45.13, W4 Examples 15.0,
;;; Experimental CL-HTTP CLIM User Interface 1.1, CL-HTTP Documentation 3.0,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.18,
;;; DEC OSF/1 V4.0 (Rev. 110),
;;; 1585x1133 1-bit STATIC-GRAY X Screen RELATUS:0.0 with 224 Genera fonts (eXodus 8.0  (c) 2001 Powerlan USA,
;;; Inc. R7100),
;;; 1585x1133 1-bit STATIC-GRAY X Screen RELATUS:0.0 with 224 Genera fonts (eXodus 8.0  (c) 2001 Powerlan USA,
;;; Inc. R7100),
;;; 1560x1120 24-bit TRUE-COLOR X Screen FUJI:1.0 with 224 Genera fonts (The Olivetti & Oracle Research Laboratory R3323),
;;; Machine serial number -2141194968,
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).

;;; Patch file for CL-HTTP version 70.182
;;; Written by JCMa, 10/15/03 23:50:42
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5-inc1.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.6,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.181,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.4, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.5, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.41, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Documentation Patches 1.0,
;;; Genera 8 5 Clim Patches 1.3, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; Jcma 44, HTTP Proxy Server 6.34, HTTP Client Substrate 4.23, HTTP Client 51.6,
;;; Image Substrate 440.4, Essential Image Substrate 433.0,
;;; W4 Constraint-Guide Web Walker 45.13, W4 Examples 15.0,
;;; Experimental CL-HTTP CLIM User Interface 1.1, CL-HTTP Documentation 3.0,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.18,
;;; DEC OSF/1 V4.0 (Rev. 110),
;;; 1137x679 1-bit STATIC-GRAY X Screen INTERNET|128.52.30.18:0.0 with 224 Genera fonts (eXodus 8.0  (c) 2001 Powerlan USA,
;;; Inc. R7100),
;;; 1585x1133 1-bit STATIC-GRAY X Screen RELATUS:0.0 with 224 Genera fonts (eXodus 8.0  (c) 2001 Powerlan USA,
;;; Inc. R7100),
;;; 1560x1120 24-bit TRUE-COLOR X Screen FUJI:1.0 with 224 Genera fonts (The Olivetti & Oracle Research Laboratory R3323),
;;; Machine serial number -2141194968,
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).




(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;HEADERS.LISP.531"
  "HTTP:SERVER;UTILS.LISP.543"
  "HTTP:SERVER;UTILS.LISP.540"
  "HTTP:SERVER;UTILS.LISP.539"
  "HTTP:SERVER;HEADERS.LISP.532"
  "HTTP:SERVER;NETSCAPE-2-0.LISP.124"
  "HTTP:SERVER;HTML2.LISP.303")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.531")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;;;------------------------------------------------------------------- 
;;;
;;; APACHE EXTENSION HEADERS
;;;

;; Reverse proxy extension headers

(define-header :x-forwarded-host
               (:header :request)
  :print-string "X-Forwarded-Host"
  :parse-function 'parse-host-header
  :print-function 'print-host-header)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.531")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;; IP address of the forwarding reverse proxy
(define-header :x-forwarded-for
               (:header :general)
  :print-string "X-Forwarded-For")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.531")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;; Domain name of the forwarding reverse proxy
(define-header :x-forwarded-server
               (:header :general)
  :print-string "X-Forwarded-Server")

;;;------------------------------------------------------------------- 
;;;
;;; INTIALIZATIONS
;;;

(defun absolutize-relative-url (scheme host port &rest relative-url)
  (declare (dynamic-extent relative-url))
  (let ((domain-name (string-downcase (host-domain-name host)))
	(scheme-string (ecase scheme
			 (:http "http://")
			 (:https "https://")
			 (:ftp "ftp://"))))
    (declare (dynamic-extent domain-name))
    (case port
      (80 (apply #'concatenate 'string scheme-string domain-name relative-url))
      (t (apply #'concatenate 'string  scheme-string domain-name ":" (write-to-string port :base 10.) relative-url)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.543")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(PROGN
(declaim (inline %make-context))

(defun %make-context (host &optional (port *standard-http-port*))
  (absolutize-relative-url :http host port))

(defun %make-local-context (&optional (port *standard-http-port*))
  (%make-context (local-host-domain-name) port))
)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.540")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(PROGN
(define set-standard-http-port (&optional (port 80))
  "Primary method for setting the standard port for HTTP connections."
  (setq *standard-http-port* port
        *local-context* (%make-local-context *standard-http-port*)
        *local-port-context-alist* nil))

(defvar *url-context-remapping-alist* nil
  "Holds the context remappings for use with reverse proxiess.")

(defun clear-url-context-remapping ()
  (setq *url-context-remapping-alist* nil))

(defun set-url-context-remapping (scheme host port new-host &optional (new-port port) (new-scheme scheme))
  (let* ((old-context (absolutize-relative-url scheme host port))
	 (new-context (absolutize-relative-url new-scheme new-host new-port))
	 (entry (assoc old-context *url-context-remapping-alist* :test #'equalp))) 
    (if entry
	(setf (cdr entry) new-context)
	(setq *url-context-remapping-alist* (nconc *url-context-remapping-alist* `((,old-context . ,new-context)))))))

(defun %define-url-context-remappings (specs)
  (loop initially (clear-url-context-remapping)
	for entry in specs
	do (destructuring-bind (scheme host port new-host &optional (new-port port) (new-scheme scheme)) entry
	     (set-url-context-remapping scheme host port new-host new-port new-scheme))))

(defmacro define-url-context-remappings (&rest specs)
  "Defines remappings from local contexts handled by the server to remote context,
such as the external name when operating through a reverse proxy.
SPECS is a list of remapping specifications of the form:
    (scheme host port new-host &optional (new-port port) (new-scheme scheme))"
  `(%define-url-context-remappings ',specs))

(defun write-url-remapping-context (url &optional (stream *standard-output*))
  (let ((remappings *url-context-remapping-alist*))
    (when remappings
      (loop for (old-context . new-context) in remappings
	    when (url::local-context-match-p old-context url)
	      do (write-string new-context stream)
		 (write-string (coerce-url-string url t t) stream)
		 (return-from write-url-remapping-context)))
    (write-string (coerce-url-string url t nil) stream)))
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.539")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(eval-when (load eval compile)
(export (intern "DEFINE-URL-CONTEXT-REMAPPINGS" :http) :http))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.532")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun print-location-header (uri-spec stream)
  (write-url-remapping-context uri-spec stream))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.532")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :location (:header :response)
  :print-string "Location"
  :parse-function 'parse-location-header
  :print-function 'print-location-header)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.532")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :content-location
               (:header :entity)
  :print-string "Content-Location"
  :parse-function 'parse-location-header
  :print-function 'print-location-header)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;NETSCAPE-2-0.LISP.124")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: netscape2.0; BASE: 10; Syntax: ansi-common-lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define declare-base-reference (&key reference target (stream *output-stream*))
  "Declares REFERENCE to be the base URL for a html page.
   Also, declares TARGET to be the default target window.
   This should appear once in the headers section of a document."
  (declare (notinline %write-command-key-arg))
  (flet ((write-base (stream)
	   (fast-format stream "\"~I\""
			(http::write-url-remapping-context reference stream))))
    (declare (dynamic-extent #'write-base))
    (%issue-command ("BASE" stream :fresh-line t :trailing-line t)
      (cond-every
	(reference
	  (html2::%write-command-key-arg stream "HREF" #'write-base))
	(target
	  (%write-target-window-command-key-arg stream target))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.303")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

(define declare-base-reference (reference &key (stream *output-stream*))
  "Declares REFERENCE to be the base URL for a html page.
   This should appear once in the headers section of a document.
   When REFERENCE is a URL, the directory URL is automatically computed."
  (flet ((write-base (stream)
	   (fast-format stream "\"~I\""
			(http::write-url-remapping-context reference stream))))
    (declare (dynamic-extent #'write-base))
    (%issue-command ("BASE" stream :fresh-line t :trailing-line t)
      (%write-command-key-arg stream "HREF" #'write-base))))
