;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Patch-File: T -*-
;;; Patch file for CL-HTTP version 70.84
;;; Reason: Function HTTP::PRINT-SET-COOKIE-HEADER:  handle max-age.
;;; Function HTTP::PARSE-SET-COOKIE-HEADER:  add max age.
;;; DEFINE-HEADER :SET-COOKIE:  update.
;;; Remove variable HTTP::*LOG*: undefine.
;;; Written by JCMa, 10/10/00 22:37:45
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.83,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Documentation Patches 1.0,
;;; Genera 8 5 Clim Patches 1.0, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; HTTP Proxy Server 5.35, HTTP Client Substrate 3.26, HTTP Client 49.11,
;;; Image Substrate 440.4, Essential Image Substrate 433.0, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x976 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
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
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.6).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;HEADERS.LISP.481"
  "HTTP:SERVER;HEADERS.LISP.482"
  "HTTP:SERVER;VARIABLES.LISP.187")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.481")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun print-set-cookie-header (spec stream)
  (etypecase (car spec)
    (atom
      (destructuring-bind (keyword value &key expires max-age domain path secure) spec
	(fast-format stream "~A=~A;" (string-for-tokenized-header-keyword keyword) value)
	;; parameters
	(cond-every
	  (expires
	    (fast-format stream "expires=~I;" (print-gmt-time stream expires #\-)))
	  (max-age
	    (fast-format stream "max-age=~D;" max-age))
	  (domain
	    (fast-format stream "domain=~A;" domain))
	  (path
	    (fast-format stream "path=~A;" (url:coerce-url-string path)))
	  (secure
	    (fast-format stream "secure;")))))
    ;; handle multiple cookie headers from losing sites.   4/20/97 -- JCMa.
    (cons (loop for plist in spec
		do (print-set-cookie-header plist stream)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.482")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun parse-set-cookie-header (string &optional (start 0) (end (length string)))
  (flet ((parse-parameters (string start end)
           (when (< start end)
             (loop for s = start then (1+ e2)
                   while (< s end)
                   for s1 = (position-if-not* #'white-space-char-p string :start s :end end)
                   while s1
                   for e1 = (char-position #\= string s1 end)
                   for e2 = (or (char-position #\; string (or e1 s1) end) end)
                   for keyword = (%intern-header-keyword-value string s1 (or e1 e2))
                   for value = (ecase keyword
				 ((:domain :path) (subseq string (1+ e1) e2))
				 ;; errors parsing dates mean expire the
				 ;; cookie similar to what happens in HTTP,
				 ;; but there is no guidance in RFC 2109
				 ;; 8/10/2000 -- JCMa.
                                 (:expires (parse-expires-header string (1+ e1) e2))
				 (:max-age (parse-integer string :start (1+ e1) :end e2))
                                 (:secure t))
                   collect keyword
                   collect value))))
    (declare (inline parse-parameters))
    (with-string-trim-bounds (*white-space-chars* string start end)
      (let* ((e1 (char-position #\= string start end))
	     (e2 (or (char-position #\; string e1 end) end)))
	`(,(%intern-header-keyword-value string start e1)
	  ,(subseq string (1+ e1) e2)
	  ,.(parse-parameters string (1+ e2) end))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.482")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :set-cookie (:header :response)
  :print-string "Set-Cookie"
  :parse-function 'parse-set-cookie-header
  :print-function 'print-set-cookie-header)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;VARIABLES.LISP.187")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp;-*-")

(funcall #'(lambda (spec)
	     (multiple-value-prog1 (lt:undefine-variable spec)
               (spt::retract-from-btrees spec)))
         '*log*)