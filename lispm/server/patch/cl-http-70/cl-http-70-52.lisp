;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.52
;;; Reason: Function URL::%RELATIVE-NAME-STRING-INDICES:  -
;;; Function URL::WITH-RELATIVE-NAME-STRING-INDICES:  -
;;; Function (CLOS:METHOD URL::WRITE-RELATIVE-NAME-STRING (STRING)):  -
;;; Function URL::WRITE-RELATIVE-NAME-STRING:  -
;;; Function (CLOS:METHOD URL::WRITE-RELATIVE-NAME-STRING (URL:URL)):  -
;;; Remove function (CLOS:METHOD HTTP::WRITE-MODIFIED-HEADERS (CONS T)): undefine.
;;; Function (CLOS:METHOD HTTP::WRITE-MODIFIED-HEADERS (LIST T)):  allow null header plists.
;;; Written by JCMa, 6/16/00 01:40:30
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.6, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.51,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42, HTTP Client Substrate 3.12,
;;; HTTP Proxy Server 5.15, HTTP Client 49.8, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, W4 Constraint-Guide Web Walker 41.5,
;;; W4 Examples 13.0, Ivory Revision 5, VLM Debugger 329, Genera program 8.11,
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
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;URL.LISP.396"
  "HTTP:SERVER;HEADERS.LISP.460"
  "HTTP:SERVER;HEADERS.LISP.461")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.396")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(declaim (inline %relative-name-string-indices))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.396")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defun %relative-name-string-indices (url-string start end)
  (declare (fixnum start end)
	   (string url-string))
  (let* ((pos1 (or (string-search= "://" url-string 0 3 start (+ start 3 (the fixnum *scheme-maximum-length*)))
		   (error 'no-scheme-found :url-string (subseq url-string start end))))
	 (pos2 (char-position #\/ url-string (+ 3 (the fixnum pos1)) end)))
    (cond (pos2 (values pos2 end))
	  (t (values end end)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.396")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmacro with-relative-name-string-indices ((url-string start end) &body body)
  `(multiple-value-bind (,(intern "START" *package*) ,(intern "END" *package*))
       (%relative-name-string-indices ,url-string ,start ,end)
     ,@body))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.396")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod write-relative-name-string ((url-string string) &optional (stream *standard-output*))
  (with-relative-name-string-indices (url-string 0 (length  url-string))
    (write-string url-string stream :start start :end end)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.396")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod write-relative-name-string ((url url) &optional (stream *standard-output*))
  (let ((relative-name-string (get-value url :relative-name)))
    (if relative-name-string
	(write-string relative-name-string stream)
	(let ((name-string (name-string url)))
	  (with-relative-name-string-indices (name-string 0 (length name-string))
	    (write-string name-string stream :start start :end end))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.396")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define-generic write-relative-name-string (url &optional stream)
  (:documentation "Writes the name string for the URL without the scheme or host/port prefix to STREAM.
This does not cons a new string. Careful to used cached version if present."))


(export (intern "WRITE-RELATIVE-NAME-STRING" :url) :url)
;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.460")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(SCL:FUNDEFINE '(METHOD WRITE-MODIFIED-HEADERS (CONS T)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.461")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod write-modified-headers ((header-plist list) stream &optional modification-plist excluded-headers termination-line-p additional-headers)
  (declare (ignore termination-line-p additional-headers))
  (loop for (header-name header-value) on header-plist by #'cddr
	unless (member header-name excluded-headers)
	  do (multiple-value-bind (new-value found-p)
		 (%get-modification modification-plist header-name)
	       (%write-header header-name (if found-p new-value header-value) stream)))
  ;; send any remaining headers
  (loop for (header-name header-value) on modification-plist by #'cddr
	do (%write-header header-name header-value stream)))

