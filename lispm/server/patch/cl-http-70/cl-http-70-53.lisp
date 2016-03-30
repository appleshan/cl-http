;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.53
;;; Reason: Function HTTP::PARSE-USER-AGENT:  remove trailing right paren in user-agent comment info.
;;; Function HTTP::PARSE-EXPIRES-HEADER:  robust handling of ill-formed date specs.
;;; DEFINE-HEADER :EXPIRES:  update to use parse-expires-header.
;;; Written by JCMa, 6/16/00 02:28:35
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.52,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42, HTTP Client Substrate 3.13,
;;; HTTP Proxy Server 5.16, HTTP Client 49.8, Image Substrate 440.4,
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
  "HTTP:SERVER;UTILS.LISP.452"
  "HTTP:SERVER;HEADERS.LISP.462")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.452")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defun parse-user-agent (string &optional (start 0) (end (length string)))
  (declare (fixnum start end)
	   (values user-agent version comment))
  (labels ((delimiter1-char-p (char)
             (eql char #\/))
           (delimiter2-char-p (char)
             (member char '(#\( #\/ #\space) :test #'eql))
	   (delimiter3-char-p (char)
             (member char '(#\; #\/ #\( #\) #\space) :test #'eql))
           (parse-platform (string start end)
             (declare (fixnum start end))
             (loop for s = (%fast-position-if-not delimiter3-char-p string :start start :end end)
                         then (%fast-position-if-not delimiter3-char-p string :start (1+ e) :end end)
                   while s
                   for e fixnum = (or (%fast-position-if delimiter3-char-p string :start (1+ (the fixnum s)) :end end)
				      end)
                   for key = (tokenize-header-keyword string s e)
                   collect key
                   while (< e end))))
    (declare (inline delimiter1-char-p delimiter2-char-p delimiter3-char-p parse-platform))
    (let* ((pos1 (%fast-position-if delimiter1-char-p string :start start :end end))
           (pos2 (and pos1 (or (%fast-position-if delimiter2-char-p string :start (1+ (the fixnum pos1)) :end end) end)))
           (user-agent (nsubstitute #\- #\space (subseq string start pos1)))	;conses due to hyphen   7/12/99 -- JCMa.
           (version (and pos1 (tokenize-header-keyword string (1+ (the fixnum pos1)) pos2)))
           (comment (and pos2 (parse-platform string pos2 end))))
      (values (tokenize-header-keyword user-agent)
              version
              comment))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.462")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun parse-expires-header (string &optional (start 0) (end (length string)))
  (handler-case-if (not *debug-server*)
     (parse-date-header string start end)
    (error () 0)))				;HTTP 1.1 spec 14.21 requires errors parsing expires to be treated as expired 


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.462")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :expires
               (:date-header :entity)
  :print-string "Expires"
  :parse-function 'parse-expires-header)

