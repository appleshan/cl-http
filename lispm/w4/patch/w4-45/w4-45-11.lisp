;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for W4 version 45.11
;;; Reason: Function W4::GET-REFERENCED-URLS-FROM-HTML-STRING:  handle arbirary white space before href or src attributes.
;;; Function W4::GET-REFERENCED-URLS-FROM-HTML: watch out for multiple white space chars in elements.
;;; Written by JCMa, 10/29/01 19:05:13
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.6,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.152,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.4, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.4, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.39, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.3,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Jcma 44, HTTP Proxy Server 6.27,
;;; HTTP Client Substrate 4.20, Statice Server 466.2, HTTP Client 51.1,
;;; Image Substrate 440.4, Essential Image Substrate 433.0,
;;; W4 Constraint-Guide Web Walker 45.10, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
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
  "HTTP:W4;WALKER.LISP.107")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:W4;WALKER.LISP.107")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: w4; -*-")

(define get-referenced-urls-from-html (stream)
  (loop for char = (read-char stream nil nil t)
        until (or (null char) (char= char *close-element-delimiter*))
        when (and (member char *ignore-chars-in-element*)
		  (prog2 (clear-white-space stream)
			 (match-stream-pattern-p stream ("href=" "src=") :match :case-insensitive)))
          do (clear-white-space stream)
             (return-from get-referenced-urls-from-html (read-url stream))
        finally (return nil)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:W4;WALKER.LISP.107")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: w4; -*-")

(define get-referenced-urls-from-html-string (string &optional (start 0) (end (length string)))
  (declare (values url next-index))
  (macrolet ((match-key (keys string index)
	       (loop for key in keys
		     for length = (length key)
		     collect `(and (< (+ ,length ,index) end)	; MCL loses if end2 is greater than (length string)
				   (string-equal ,key ,string :start1 0 :end1 ,length
						 :start2 ,index :end2 (+ ,length ,index))
				   (incf ,index ,length))
		       into clauses
		     finally (return `(or ,.clauses))))
	     (string-clear-whitespace (string idx)	;skip any amount of white space when white space encountered
	       `(when (member char *ignore-chars-in-element*)
		  (incf ,idx)
		  (loop while (member (aref ,string ,idx) *ignore-chars-in-element*)
			do (incf ,idx))
		  t)))
    (loop for idx upfrom start below end
	  for char = (aref string idx)
	  until (or (null char) (eql char *close-element-delimiter*))
	  when (and (string-clear-whitespace string idx)
		    (match-key ("href=" "src=") string idx))
	    do (multiple-value-bind (url newly-interned-p next-index)
		   (read-url-from-string string idx end)
		 (declare (ignore newly-interned-p))
		 (when url
		   (return-from get-referenced-urls-from-html-string
		     (values url next-index))))
	  finally (return (values nil (1+ idx))))))

