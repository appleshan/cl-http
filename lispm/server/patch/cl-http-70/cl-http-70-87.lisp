;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for CL-HTTP version 70.87
;;; Reason: Function HTTP::NSTRING-UNESCAPE-SPECIAL-CHARS:  fix unesaping bug when only-safe-characters-p is NIL.
;;; Written by JCMa, 10/27/00 17:56:44
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.86,
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
;;; Genera 8 5 Lock Simple Patches 1.0, HTTP Proxy Server 5.38,
;;; HTTP Client Substrate 3.29, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
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
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.20),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.6),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.5).


(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;UTILS.LISP.471")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.471")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(define nstring-unescape-special-chars (string &optional (start 0) (end (length string))
					       only-safe-characters-p (pad-char #\space) set-fill-pointer-p)
  "When any escaped characters are present, this returns a string with these characters unescaped.
STRING is destructively modified when escaped characters are present.
ONLY-SAFE-CHARACTERS-P set to non-nill skips reserved or unsafe URL characters.
When SET-FILL-POINTER-P is non-null, the fill-pointer on STRING is moved backwards
by two times the number of characters unescaped. Otherwise, that number of characters
are padded on the right using PAD-CHAR"
  (declare (values unescaped-string new-end chars-unescaped-p)
           (fixnum start end))
  (with-fast-array-references ((string string string))
    (loop with read-idx fixnum = start
	  with write-idx fixnum = start
	  and new-char and chars-unescaped-p 
          while (< read-idx end)
          for char = (aref string read-idx)
	  do (incf read-idx)
          when (escape-character-p char)
            do (setf new-char (unescape-character string read-idx (+ read-idx 2)))
               (cond  ;; Skip unescaping a char, just incf idx to skip the hex pair.
                 ((and only-safe-characters-p (uri-reserved-or-unsafe-char-p new-char))
		  (cond (chars-unescaped-p
			 (setf (aref string write-idx) char
			       (aref string (incf write-idx)) (aref string read-idx)
			       (aref string (incf write-idx)) (aref string (incf read-idx)))
			 (incf write-idx)
			 (incf read-idx))
			(t (incf write-idx 3)
			   (incf read-idx 2))))
                 ;; Escape a char, we have already started a new string.
                 ((or chars-unescaped-p (setq chars-unescaped-p t))
		  (setf (aref string write-idx) new-char)
		  (incf write-idx)
		  (incf read-idx 2)))
	  else do (when chars-unescaped-p
		    (setf (aref string write-idx) char))
		  (incf write-idx)
	  finally (return (cond (chars-unescaped-p
				 (if set-fill-pointer-p
				     (setf (fill-pointer string) write-idx)
				     (loop for idx upfrom write-idx below end
					   do (setf (aref string idx) pad-char)))	;pad out the end
				 (values string write-idx chars-unescaped-p))
				(t (values string read-idx)))))))

