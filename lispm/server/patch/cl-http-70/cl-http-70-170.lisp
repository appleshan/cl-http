;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.170
;;; Reason: Function (CLOS:METHOD HTTP:STREAM-ENCODE-CRLF-UNTIL-EOF (T T)):  respect eof
;;; at end of file so as not to insert an extra CRLF.
;;; Function WWW-UTILS::%STREAM-ENCODE-CRLF-UNTIL-EOF:  fix same bug.
;;; Function (CLOS:METHOD HTTP:STREAM-ENCODE-CRLF-UNTIL-EOF (SI:CHARACTER-STREAM SI:BINARY-STREAM)):  update.
;;; Function (CLOS:METHOD HTTP:STREAM-ENCODE-CRLF-UNTIL-EOF (SI:CHARACTER-STREAM SI:UNSIGNED-BYTE-8-STREAM)):  update.
;;; Written by JCMa, 9/23/03 17:58:08
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5-inc1.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.169,
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
;;; Genera 8 5 Lock Simple Patches 1.0, Jcma 44, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.18, DEC OSF/1 V4.0 (Rev. 110),
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;UTILS.LISP.522"
  "HTTP:LISPM;SERVER;LISPM.LISP.515")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.522")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

;; ports should specialize this for high performance.
(defmethod stream-encode-crlf-until-eof (from-stream to-stream)
  (using-resource (line-buffer line-buffer *line-buffer-size*)
    (loop with line and eof and delimiter and length
	  do (multiple-value-setq (line eof delimiter length)
	       (read-delimited-line from-stream '(#\Return #\Linefeed) nil line-buffer))
	  unless (zerop length)
	    do (with-fast-array-references ((array line vector))
		 delimiter			;ignore
		 (loop for idx upfrom 0 below length
		       do (write-byte (char-code (aref array idx)) to-stream)))
	  until eof
	  do (write-byte #.(char-code #\Return) to-stream)
	     (write-byte #.(char-code #\Linefeed) to-stream))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.515")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

;; Find a faster translating stream if possible.   6/24/96 -- JCMa.
(defun %stream-encode-crlf-until-eof (from-stream to-stream)
  (using-resource (line-buffer http::line-buffer http::*line-buffer-size*)
    (loop with line and eof and delimiter and length
	  do (multiple-value-setq (line eof delimiter length)
	       (read-delimited-line from-stream '(#\Return #\Linefeed) nil line-buffer))
	  unless (zerop length)
	    do (with-fast-array-references ((array line vector))
		 delimiter			;ignore
		 (loop for idx upfrom 0 below length
		       do (scl:send to-stream :tyo (scl:char-to-ascii (aref array idx)))))
	  until eof
	  when (eql delimiter #\Return)
	    do (scl:send to-stream :tyo #.(scl:char-to-ascii #\Return))
	       (scl:send to-stream :tyo #.(scl:char-to-ascii #\Linefeed)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.515")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(declaim (inline %stream-decode-crlf-until-eof))

;; allows mac-fs:mac-rpc-file-binary-output-stream to work
(defmethod http:stream-encode-crlf-until-eof ((from-stream si:character-stream) (to-stream si:binary-stream))
  (%stream-encode-crlf-until-eof from-stream to-stream))

(defmethod http:stream-encode-crlf-until-eof ((from-stream si:character-stream) (to-stream si:unsigned-byte-8-stream))
  (%stream-encode-crlf-until-eof from-stream to-stream))

(declaim (notinline %stream-decode-crlf-until-eof))