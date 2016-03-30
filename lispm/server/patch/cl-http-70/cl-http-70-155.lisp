;;; -*- Mode: lisp; Syntax: common-lisp; Package: user; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.155
;;; Reason: CLOS class HTTP::HTTP-POST-LOG-ENTRY:  remove bytes-received-log-entry-mixin to avoid class precedence errors in MCL.
;;; Function (CLOS:METHOD HTTP::MIME-STREAM-DECODE-UNTIL-BOUNDARY (T T T (EQL :BINARY))):  fix boundary detection bug.
;;; Written by JCMa, 11/06/01 15:23:49
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.6,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.154,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.4, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.5, Jcma 44,
;;; Experimental Genera 8 5 Patches 1.0, Genera 8 5 System Patches 1.39,
;;; Genera 8 5 Mailer Patches 1.1, Genera 8 5 Joshua Patches 1.0,
;;; Genera 8 5 Statice Runtime Patches 1.0, Genera 8 5 Statice Patches 1.0,
;;; Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.3,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; HTTP Proxy Server 6.27, HTTP Client Substrate 4.20, Statice Server 466.2,
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

;;; Patch file for CL-HTTP version 70.155
;;; Written by JCMa, 11/06/01 16:34:16
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.6,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.154,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.4, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.5, Jcma 44,
;;; Experimental Genera 8 5 Patches 1.0, Genera 8 5 System Patches 1.39,
;;; Genera 8 5 Mailer Patches 1.1, Genera 8 5 Joshua Patches 1.0,
;;; Genera 8 5 Statice Runtime Patches 1.0, Genera 8 5 Statice Patches 1.0,
;;; Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.3,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; HTTP Proxy Server 6.27, HTTP Client Substrate 4.20, Statice Server 466.2,
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
  "HTTP:SERVER;CLASS.LISP.53"
  "HTTP:SERVER;UTILS.LISP.502")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.53")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defclass http-post-log-entry
	  (extended-common-log-entry)
    ((form-alist :initarg :form-alist :accessor log-entry-form-alist)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.502")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmethod mime-stream-decode-until-boundary (mime-stream to-stream boundary (mode (eql :binary)) &optional buffer
							  &aux (max-bytes *mime-multipart-block-maximum-size*) (total-bytes 0))
  (declare (values last-block-p)
	   (ignore buffer)
	   (integer total-bytes))
  (macrolet ((binary-char-code (char)
	       (typecase char
		 (#.*standard-character-type* #+Genera (si:ascii-code char) #-Genera (char-code char))
		 (t #+Genera `(si:ascii-code ,char) #-Genera `(char-code ,char)))))
    (labels ((fill-boundary-array (array boundary boundary-length)
	       (with-fast-array-references ((array array vector)
					    (boundary boundary string))
		 (loop initially (setf (aref array 0) (binary-char-code #\Return)
				       (aref array 1) (binary-char-code #\Linefeed) )
		       for i fixnum below boundary-length
		       do (setf (aref array (+ 4 i)) (char-code (aref boundary i))))))
	     (protected-write-byte (byte stream)
	       (and max-bytes
		    (> (incf total-bytes) (the integer max-bytes))
		    (signal-mime-multipart-block-maximum-size-exceeded total-bytes max-bytes))
	       (write-byte byte stream))
	     (write-array-portion (to-stream array n &rest extra-bytes)
	       (declare (dynamic-extent extra-bytes))
	       (with-fast-array-references ((array array vector))
		 (loop for idx fixnum upfrom 0 below n
		       do (protected-write-byte (aref array idx) to-stream)))
	       (dolist (byte extra-bytes)
		 (protected-write-byte byte to-stream)))
	     (check-for-mime-boundry (mime-stream to-stream boundary-array boundary-array-length)
	       (with-fast-array-references ((boundary-array boundary-array vector))
		 (tagbody
		   recheck (loop for idx fixnum upfrom 1 below boundary-array-length
				 for next-byte = (read-byte mime-stream)
				 while (eql next-byte (aref boundary-array idx))
				 finally (cond ((= idx boundary-array-length))	;success - drop through
					       ;;failing on return char - could be start of a boundary
					       ((eql next-byte (binary-char-code #\Return))
						(write-array-portion to-stream boundary-array idx)
						(go recheck))
					       ;; write out consumed bytes - return NIL
					       (t (write-array-portion to-stream boundary-array idx next-byte)
						  (return-from check-for-mime-boundry nil))))))
	       (let ((byte1 (read-byte mime-stream))
		     (byte2 (read-byte mime-stream)) )
		 (cond ((and (= byte1 (binary-char-code #\-)) (= byte2 (binary-char-code #\-)))
			(setq byte1 (read-byte mime-stream)
			      byte2 (read-byte mime-stream))
			(cond ((and (eql byte1 (binary-char-code #\Return)) (eql byte2 (binary-char-code #\Linefeed)))
			       (return-from mime-stream-decode-until-boundary t))
			      (t (write-array-portion to-stream boundary-array boundary-array-length
						      (binary-char-code #\-) (binary-char-code #\-) byte1 byte2))))
		       ((and (eql byte1 (binary-char-code #\Return)) (eql byte2 (binary-char-code #\Linefeed)))
			(return-from mime-stream-decode-until-boundary nil))
		       (t (write-array-portion to-stream boundary-array boundary-array-length byte1 byte2))))))
      (declare (inline fill-boundary-array)
	       (dynamic-extent #'protected-write-byte))
      (let* ((boundary-length (length boundary) )
	     (boundary-array-length (+ 4 boundary-length)) ;; len + CR + LF + 2 #\-'s
	     (boundary-array  (make-array boundary-array-length :initial-element #.(char-code #\-))))
	(declare (dynamic-extent boundary-array)
		 (fixnum boundary-length boundary-array-length))
	(with-binary-stream (mime-stream :input)
	  (loop initially (fill-boundary-array boundary-array boundary boundary-length)
		for byte = (read-byte mime-stream)
		do (if (eql byte (binary-char-code #\Return))
		       (check-for-mime-boundry mime-stream to-stream boundary-array boundary-array-length)
		       (protected-write-byte byte to-stream))))))))
