;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.148
;;; Reason: Function (FLAVOR:METHOD TCP::NOTE-LAST-CHUNK TCP::CHUNK-TRANSFER-ENCODING-OUTPUT-STREAM-MIXIN):  
;;; Fix bug writing zero length chunk when output buffer is null.
;;; Function (CLOS:METHOD HTTP:STREAM-COPY-UNTIL-EOF (TCP::TCP-MODAL-HTTP-STREAM TCP::TCP-MODAL-HTTP-STREAM)):  
;;; Eliminate call to si:stream-copy-until-eof to avoid unhelpful force-outputs.
;;; Function (CLOS:METHOD HTTP:STREAM-COPY-UNTIL-EOF (T TCP::TCP-MODAL-HTTP-STREAM)):  
;;; Ditto when buffer level copying possible.
;;; Written by JCMa, 9/19/01 17:21:11
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.6,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.147,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.4, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.4, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.37, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Jcma 44, HTTP Proxy Server 6.25,
;;; HTTP Client Substrate 4.19, Statice Server 466.2, HTTP Client 51.0,
;;; Image Substrate 440.4, Essential Image Substrate 433.0, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
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
  "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.200"
  "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.201")


;========================
(sct:begin-patch-section)
(sct:patch-section-source-file "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.200")
(sct:patch-section-attributes
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

(defmethod (note-last-chunk chunk-transfer-encoding-output-stream-mixin) (&optional footers-plist)
  (flet ((ensure-chunk-headroom (amount)
	   (unless (and si:stream-output-buffer (< amount (- si:stream-output-limit si:stream-output-index)))
	     ;;Force new packets to be gotten when chunking to avoid headroom
	     ;;errors. Because chunking has been turned off, we can't rely on
	     ;;:setup-new-output-buffer to bind this variable.  8/17/2000 -- JCMa.
	     (let ((*tcp-segment-combination-disabled* t))	
	       (send self :setup-new-output-buffer)))))
    (declare (inline ensure-chunk-headroom))
    (cond D,#TD1PsT[Begin using 006 escapes](1 0 (NIL 0) (NIL :ITALIC NIL) "CPTFONTI");; first case is a little weird. Why would we see it and why handle it this way?0 19/19/2001 -- JCMa.
0      ((and (null chunk-output) (null si:stream-output-buffer))	;there is no stream buffer1 and not chunking
0       (ascii-output-mode self)			;clean up a little and return 8/31/2000 -- JCMa.
       (return-from note-last-chunk nil))
      ;; if no data written to body yet, convert to end header
      (t (setq chunk-output nil)		; Prevent chunked output buffer setup from repeating
	 (unless (or (null si:stream-output-buffer)	1;no buffer due to a preceeding force-output0 19/19/2001 -- JCMa.
0		     (= chunk-body-start si:stream-output-index))	1;no data written to buffer yet
0	   (multiple-value-setq (si:stream-output-index)
	     (%note-body-end si:stream-output-buffer si:stream-output-index)))
	 (ensure-chunk-headroom 3)		;make sure we have room for termination chunk
	 (setf (aref si:stream-output-buffer si:stream-output-index) #.(si:ascii-code #\0))
	 (incf si:stream-output-index)
	 (write-8-bit-crlf si:stream-output-buffer si:stream-output-index (incf si:stream-output-index 2))))
    ;; ensure that we're in ascii mode before writing footers
    (ascii-output-mode self)
    ;; write the footers
    (http::write-headers self footers-plist t)))


;========================
(sct:begin-patch-section)
(sct:patch-section-source-file "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.201")
(sct:patch-section-attributes
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

(http::defmethod http::stream-copy-until-eof ((from-stream tcp-modal-http-stream) (to-stream tcp-modal-http-stream)
					      &optional (copy-mode :text))
  (with-input-mode (from-stream copy-mode)
    (with-output-mode (to-stream copy-mode)
      (loop doing (multiple-value-bind (buffer offset limit)
		      (send from-stream :read-input-buffer)
		    (unless buffer (return nil))
		    (send to-stream :string-out buffer offset limit)
		    (send from-stream :advance-input-buffer))))))


;========================
(sct:begin-patch-section)
(sct:patch-section-source-file "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.201")
(sct:patch-section-attributes
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

(http::defmethod http::stream-copy-until-eof (from-stream (to-stream tcp-modal-http-stream) &optional (copy-mode :text))
  (with-output-mode (to-stream copy-mode)
    (cond ((and (operation-handled-p from-stream :read-input-buffer)
		(operation-handled-p to-stream :string-out))
	   (loop doing (multiple-value-bind (buffer offset limit)
			   (send from-stream :read-input-buffer)
			 (unless buffer (return nil))
			 (send to-stream :string-out buffer offset limit)
			 (send from-stream :advance-input-buffer))))
	  (t (si:stream-copy-until-eof from-stream to-stream)))))

