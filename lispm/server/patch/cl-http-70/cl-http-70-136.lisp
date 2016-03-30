;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.136
;;; Reason: Function (DEFUN-IN-FLAVOR TCP::PARSE-CHUNK-SIZE-ARGUMENTS TCP::CHUNK-TRANSFER-DECODING-INPUT-STREAM-MIXIN):  a little faster.
;;; Function (DEFUN-IN-FLAVOR TCP::%NOTE-CHUNK-START TCP::CHUNK-TRANSFER-DECODING-INPUT-STREAM-MIXIN):  
;;; Get the next physical buffer when reading the chunk size leaves us exactly at the end of a physical buffer.
;;; Written by JCMa, 5/22/01 19:42:15
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.135,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
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
;;; HTTP Client Substrate 4.14, Statice Server 466.2, HTTP Client 50.8,
;;; Image Substrate 440.4, Essential Image Substrate 433.0, MacIvory Support 447.0,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.16,
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
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.11),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.199")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.199")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

(defun-in-flavor (parse-chunk-size-arguments chunk-transfer-decoding-input-stream-mixin) (byte)
  (declare (values args-plist))
  (with-chunk-transfer-decoding-traced
    (format *trace-output* "~&~'bParse-Chunk-Size-Args-Start:~ ~D~&" si:stream-input-index)
    (format *trace-output* "~&~'bByte1:~ ~:C" (si:ascii-to-char byte)))
  (prog1
    (ecase byte
      (#.(char-to-ascii #\Return)
       (%read-lf)
       nil)
      (#.(char-to-ascii #\Linefeed)
       nil)
      (#.(char-to-ascii #\;)
       (let ((vector (allocate-input-chunk-args-vector)))
	 (declare (si:array-register vector))
	 (loop with idx = (fill-pointer vector)
	       with vector-size = (array-total-size vector)
	       for byte = (%read-byte)
	       until (member byte '#.(mapcar #'char-to-ascii '(#\Return #\Linefeed)))
	       do (unless (< idx vector-size)
		    (multiple-value-setq (vector vector-size)
		      (grow-chunk-string vector 100. vector-size)))
	       do (setf (aref vector idx) (code-char byte))
		  (incf idx)
	       finally (with-chunk-transfer-decoding-traced
			 (format *trace-output* "~&~'bByte2:~ ~:C" (si:ascii-to-char byte)))
		       (ecase byte
			 (#.(char-to-ascii #\Return)
			  (%read-lf))
			 (#.(char-to-ascii #\Linefeed)))
		       (setf (fill-pointer vector) idx)
		       (return (cond ((zerop idx) nil)
				     (t (setf (fill-pointer vector) idx)
					(http::parse-equal-sign-delimited-pairs vector 0 idx #\; nil))))))))
    (with-chunk-transfer-decoding-traced
      (format *trace-output* "~&~'bParse-Chunk-Size-Args-End:~ ~D~&" si:stream-input-index))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.199")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

(defun-in-flavor (%note-chunk-start chunk-transfer-decoding-input-stream-mixin) ()
  (multiple-value-bind (chunk-size chunk-args)
      (%read-chunk-size)
    (declare (ignore chunk-args))
    (cond ((zerop chunk-size)
	   ;;(break "foo")
	   (setq input-chunk-size 0
		 at-end si:stream-input-index)	;pretend last ascii translation window ended here to start the right place.              
	   ;; Discard input buffer when all input has been used to avoid danger of reading past the end 9/6/2000 -- JCMa.
	   (when (and si:stream-input-buffer (not (< si:stream-input-index si:stream-input-limit)))
	     (send self :discard-current-input-buffer))
	   (signal 'end-of-chunk-transfer-decoding :stream self))
	  (t (unless (and si:stream-input-buffer (< si:stream-input-index si:stream-input-limit))
	       (%setup-next-physical-input-buffer nil t))
	     (setq input-chunk-size chunk-size
		   at-end si:stream-input-index	;pretend last ascii translation window ended here to start the right place.
		   input-scan-start si:stream-input-index
		   input-scan-end (min (+ si:stream-input-index input-chunk-size) si:stream-input-limit)
		   input-scan-length (- input-scan-end input-scan-start)
		   input-content-length (+ input-content-length input-chunk-size)
		   input-chunk-content-length input-scan-length
		   input-chunk-crosses-buffer-p (> input-chunk-size input-chunk-content-length)
		   input-buffer-limit si:stream-input-limit
		   si:stream-input-limit input-scan-end)
	     (with-chunk-transfer-decoding-traced
	       (format *trace-output* "~&~'bScan-Start:~ ~D ~'bScan-End:~ ~D  ~:[~;~'bCross-buffer:~ yes~]~&"
		       input-scan-start input-scan-end input-chunk-crosses-buffer-p))))))

