;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.49
;;; Reason: DEFINE-CONDITION URL::SEARCH-URL-BAD-FORM-ENCODING:  -
;;; Variable URL::*SEARCH-URL-SIGNAL-BAD-FORM-ENCODING*:  new variable.
;;; Function URL:PARSE-SEARCH-INFO-AS-QUERY-ALIST:  handle bad syntax more gracefully.
;;; Written by JCMa, 5/16/00 04:30:20
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.48,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42, HTTP Proxy Server 5.11,
;;; HTTP Client Substrate 3.10, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.11, DEC OSF/1 V4.0 (Rev. 110),
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
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48).

(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;URL.LISP.393")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.393")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define-condition search-url-bad-form-encoding
		  (parsing-error)
  ((search-info :initarg :search-info :reader search-info))
  (:report (lambda (condition stream)
             (format stream "Ill-formed form URL encoding in the search component of ~S"
                     (coerce-url-string (search-info condition))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.393")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define-parameter *search-url-signal-bad-form-encoding* t
		  "Controls whether bad URL form encodings are signalled or interpreted as standard search URLs.")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.393")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defun parse-search-info-as-query-alist (string &optional (start 0) (end (length string)))
  "Parses the search component of a search URL according to URL encoding rules
for use with form submission via the GET method."
  (declare (values search-alist))
  (flet ((handle-bad-form-encoding (string start end)
	   (if *search-url-signal-bad-form-encoding*
	       (error 'search-url-bad-form-encoding :search-info (subseq string start end))
	       (return-from parse-search-info-as-query-alist (parse-search-info string start end)))))
    (declare (inline handle-bad-form-encoding))
    (unless (= start end)
      (with-fast-array-references ((string string string))
	(loop for s1 = start then (1+ (the fixnum e2))
	      while (< s1 end)
	      for e1 = (or (char-position #\= string s1 end)
			   (handle-bad-form-encoding string start end))
	      for s2 = (1+ (the fixnum e1))
	      for e2 = (or (char-position #\& string s2 end) end)
	      for keyword = (http::%tokenize-form-query-keyword string s1 e1)
	      for value = (unless (= s2 e2)
			    (string-unescape-special-chars string s2 e2))
	      collect `(,keyword ,value))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.393")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(mapc #'(lambda (x) (export (intern x :url) :url))
      `("*SEARCH-URL-SIGNAL-BAD-FORM-ENCODING*" "BAD-HOST-PORT-SPECIFICATION" "HOST-PARSING-ERROR" "NO-PARSER-FOR-SCHEME" "SEARCH-URL-BAD-FORM-ENCODING"))

