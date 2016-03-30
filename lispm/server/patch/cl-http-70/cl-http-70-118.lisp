;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.118
;;; Reason: Specialize HTTP::STREAM-DECODE-CRLF-BYTES for Lispm 
;;; and make a couple of fixes to HTTP:STREAM-DECODE-CRLF-UNTIL-EOF.
;;; 
;;; Function WWW-UTILS::%STREAM-DECODE-CRLF-BYTES:  new stream primitive
;;; Function (CLOS:METHOD HTTP::STREAM-DECODE-CRLF-BYTES (SI:STREAM SI:STREAM T)):  default method.
;;; Function (CLOS:METHOD HTTP::STREAM-DECODE-CRLF-BYTES (T SYMBOL T)):  -
;;; Function (CLOS:METHOD HTTP::STREAM-DECODE-CRLF-BYTES (TCP::TCP-MODAL-HTTP-STREAM SI:STREAM T)):  -
;;; Function (CLOS:METHOD HTTP::STREAM-DECODE-CRLF-BYTES (SI:UNSIGNED-BYTE-8-STREAM SI:CHARACTER-STREAM T)):  workhorse
;;; Function (CLOS:METHOD HTTP::STREAM-DECODE-CRLF-BYTES (SI:BINARY-STREAM SI:CHARACTER-STREAM T)):  macivory compatibility.
;;; Function WWW-UTILS::FIND-REAL-STREAM:  new.
;;; Function (CLOS:METHOD HTTP:STREAM-DECODE-CRLF-UNTIL-EOF (TCP::TCP-MODAL-HTTP-STREAM SI:STREAM)):  specialize.
;;; Function (CLOS:METHOD HTTP:STREAM-DECODE-CRLF-UNTIL-EOF (T SYMBOL)): -
;;; Remove function (CLOS:METHOD HTTP:STREAM-DECODE-CRLF-UNTIL-EOF (SI:CHARACTER-STREAM SYMBOL)): obsolete
;;; Function WWW-UTILS::%STREAM-DECODE-CRLF-UNTIL-EOF:  better LF handling.
;;; Function (CLOS:METHOD HTTP:STREAM-DECODE-CRLF-UNTIL-EOF (T SYMBOL)):  -
;;; Written by JCMa, 3/28/01 19:02:47
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.117,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.26, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Jcma 42, HTTP Proxy Server 6.18,
;;; HTTP Client Substrate 4.9, Statice Server 466.2,
;;; W4 Constraint-Guide Web Walker 45.10, HTTP Client 50.5, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, Paralation Lisp Simulator F0401 8.0,
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.7),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1),
;;; Ansi common lisp as synonym patch (from W:>reti>ansi-common-lisp-as-synonym-patch.lisp.9),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).

;;; Patch file for CL-HTTP version 70.118
;;; Written by JCMa, 4/08/01 19:24:26
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.118,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.26, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, HTTP Proxy Server 6.18,
;;; HTTP Client Substrate 4.9, Statice Server 466.2, HTTP Client 50.6,
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
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.7),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1).


;;; Patch file for CL-HTTP version 70.118
;;; Written by JCMa, 3/28/01 21:14:16
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.118,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.26, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Jcma 42, HTTP Proxy Server 6.18,
;;; HTTP Client Substrate 4.9, Statice Server 466.2,
;;; W4 Constraint-Guide Web Walker 45.10, HTTP Client 50.6, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, Paralation Lisp Simulator F0401 8.0,
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.7),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1),
;;; Ansi common lisp as synonym patch (from W:>reti>ansi-common-lisp-as-synonym-patch.lisp.9),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).


(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:LISPM;SERVER;LISPM.LISP.489"
  "HTTP:LISPM;SERVER;LISPM.LISP.490"
  "HTTP:LISPM;SERVER;LISPM.LISP.492"
  "HTTP:LISPM;SERVER;LISPM.LISP.494"
  "HTTP:LISPM;SERVER;LISPM.LISP.495")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.494")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defun %stream-decode-crlf-bytes (from-stream to-stream bytes)
  (using-resource (line-buffer http::line-buffer http::*line-buffer-size*)
    (with-fast-array-references ((output line-buffer vector))
      (unless (and (scl:operation-handled-p from-stream :read-input-buffer)
		   (scl:operation-handled-p to-stream :string-out))
	(error "Copying methods not supported on streams."))
      (loop named do-input-buffers
	    with at-cr-flag and idx2 = 0 and input-limit = (array-total-size line-buffer)
	    and bytes-remaining = bytes
	    doing (multiple-value-bind (input-buffer offset limit)
		      (scl:send from-stream :read-input-buffer t)
		    ;; If this is reached signal EOF. Shouldn't happen to EOF
		    ;; T arg to :read-input-buffer 7/16/96 -- JCMa.
		    (unless input-buffer (error 'sys:end-of-file))
		    (with-fast-array-references ((input input-buffer array))
		      (let ((read-bytes (min (- limit offset) bytes-remaining)))
			(loop for idx1 upfrom offset below (+ offset read-bytes)
			      for ch = (aref input idx1)
			      do (cond ((and at-cr-flag (prog1 (= ch #.(si:ascii-code #\Linefeed))
							       (setq at-cr-flag nil))))
				       (t (cond ((member ch '(#.(si:ascii-code #\Return) #.(si:ascii-code #\Linefeed)) :test #'=)
						 (setf (aref output idx2) #\Return)
						 (setq at-cr-flag t))
						(t (setf (aref output idx2) (si:ascii-to-char ch))))
					  (unless (< (incf idx2) input-limit)
					    (scl:send to-stream :string-out output 0 idx2)
					    (setq idx2 0))))
			      finally (cond ((zerop (decf bytes-remaining read-bytes))
					     (unless (zerop idx2)
					       (scl:send to-stream :string-out output 0 idx2))
					     (scl:send from-stream :advance-input-buffer idx1)
					     (return-from do-input-buffers nil))
					    (t (scl:send from-stream :advance-input-buffer)))))))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.489")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defmethod http::stream-decode-crlf-bytes ((from-stream si:stream) (to-stream si:stream) bytes)
  (declare (ignore bytes))
  (error "This operation has not been defined from ~S to ~S." (type-of from-stream) (type-of to-stream)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.489")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defmethod http::stream-decode-crlf-bytes ((from-stream si:unsigned-byte-8-stream) (to-stream si:character-stream) bytes)
  (%stream-decode-crlf-bytes from-stream to-stream bytes))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.489")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

;; allows mac-fs:mac-rpc-file-binary-output-stream to work
(defmethod http::stream-decode-crlf-bytes ((from-stream si:binary-stream) (to-stream si:character-stream) bytes)
  (%stream-decode-crlf-bytes from-stream to-stream bytes))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.490")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

;;;------------------------------------------------------------------- 
;;;
;;; STREAM HACKING
;;;

(defun find-real-stream (stream)
  "Recurses down syn-stream indirections to find real stream."
  (cond ((and (symbolp stream)
	      (http::string-search "SYN-STREAM" (symbol-name stream) 0 10 0))
	 (find-real-stream (cdr (scl:locf (scl:symbol-function stream)))))
	(t stream)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.492")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defmethod http::stream-decode-crlf-bytes (from-stream (to-stream symbol) bytes)
  (let ((real-to-stream (find-real-stream to-stream)))
    (typecase real-to-stream
      (symbol (error "Failed to find the real stream for ~S." to-stream))
      (t (http::stream-decode-crlf-bytes from-stream real-to-stream bytes)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.492")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

;; the symbol is going to be a #:TERMINAL-IO-SYN-STREAM or similar   2/8/99 -- JCMa.
(defmethod http:stream-decode-crlf-until-eof (from-stream (to-stream symbol))
  (let ((real-to-stream (find-real-stream to-stream)))
    (typecase real-to-stream
      (symbol (error "Failed to find the real stream for ~S." to-stream))
      (t (http:stream-decode-crlf-until-eof from-stream real-to-stream)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.492")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defmethod http::stream-decode-crlf-bytes ((from-stream tcp::tcp-modal-http-stream) (to-stream si:stream) bytes)
  (%stream-decode-crlf-bytes from-stream to-stream bytes))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.494")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(declaim (notinline %stream-decode-crlf-until-eof))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.494")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defun %stream-decode-crlf-until-eof (from-stream to-stream)
  (using-resource (line-buffer http::line-buffer http::*line-buffer-size*)
    (with-fast-array-references ((output line-buffer vector))
      (unless (and (scl:operation-handled-p from-stream :read-input-buffer)
		   (scl:operation-handled-p to-stream :string-out))
	(error "Copying methods not supported on streams."))
      (loop with at-cr-flag and idx2 = 0 and input-limit = (array-total-size line-buffer)
	    doing (multiple-value-bind (buffer offset limit)
		      (scl:send from-stream :read-input-buffer)
		    (cond ((null buffer)
			   (unless (zerop idx2)
			     (scl:send to-stream :string-out output 0 idx2))
			   (return nil))
			  (t (with-fast-array-references ((input buffer array))
			       (loop for idx1 upfrom offset below limit
				     for ch = (aref input idx1)
				     do (cond ((and at-cr-flag (prog1 (= ch #.(si:ascii-code #\Linefeed))
								      (setq at-cr-flag nil))))
					      (t (cond ((member ch '(#.(si:ascii-code #\Return) #.(si:ascii-code #\Linefeed)) :test #'=)
							(setf (aref output idx2) #\Return)
							(setq at-cr-flag t))
						       (t (setf (aref output idx2) (si:ascii-to-char ch))))
						 (unless (< (incf idx2) input-limit)
						   (scl:send to-stream :string-out output 0 idx2)
						   (setq idx2 0))))
				     finally (scl:send from-stream :advance-input-buffer))))))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.494")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defmethod http:stream-decode-crlf-until-eof ((from-stream tcp::tcp-modal-http-stream) (to-stream si:stream))
  (%stream-decode-crlf-until-eof from-stream to-stream))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.494")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defmethod http:stream-decode-crlf-until-eof ((from-stream si:unsigned-byte-8-stream) (to-stream si:character-stream))
  (%stream-decode-crlf-until-eof from-stream to-stream))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.494")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

;; allows mac-fs:mac-rpc-file-binary-output-stream to work
(defmethod http:stream-decode-crlf-until-eof ((from-stream si:binary-stream) (to-stream si:character-stream))
  (%stream-decode-crlf-until-eof from-stream to-stream))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.495")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(SCL:FUNDEFINE '(METHOD HTTP:STREAM-DECODE-CRLF-UNTIL-EOF (SI:CHARACTER-STREAM SYMBOL)))
