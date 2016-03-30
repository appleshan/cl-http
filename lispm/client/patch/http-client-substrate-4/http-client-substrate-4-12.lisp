;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for HTTP-CLIENT-SUBSTRATE version 4.12
;;; Reason: Function (CLOS:METHOD HTTP::RESET-TRANSACTION-STATE (HTTP::BASIC-CONNECTION)):  remodularize
;;; Function (CLOS:METHOD HTTP::NOTE-FREE-CONNECTION (HTTP::CONNECTION-POOL-MIXIN)):  remodularize
;;; Function (CLOS:METHOD HTTP::RETURN-CONNECTION (HTTP::CONNECTION-POOL-MIXIN)):  call reset translation.
;;; Written by JCMa, 5/11/01 17:49:13
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.132,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.4, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.26, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Jcma 43, HTTP Proxy Server 6.22,
;;; HTTP Client Substrate 4.11, Statice Server 466.2, HTTP Client 50.8,
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
  "HTTP:CLIENT;CONNECTION.LISP.85")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CONNECTION.LISP.85")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

;; Reset connection state per HTTP transaction
(defmethod reset-transaction-state ((connection basic-connection))
  (let ((stream (connection-stream connection)))
    (setf (www-utils:bytes-transmitted stream) 0
	  (www-utils:bytes-received stream) 0)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CONNECTION.LISP.85")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(defmethod note-free-connection ((connection connection-pool-mixin))
  (reset-transaction-state connection)
  (cond ((zerop (connection-free-since connection))
	 (let* ((time (get-universal-time))
		(close (+ time (the integer (or (connection-timeout connection) *client-persistent-connection-timeout*)))))
	   (declare (bignum time))
	   ;; reset instance variables
	   (setf (connection-free-since connection) time
		 (connection-close-time connection) close))
	 (push-connection-pool connection))
	(t (error "Attempt to note free connection that is already free for ~S." connection))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CONNECTION.LISP.85")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(defmethod return-connection ((connection connection-pool-mixin))
  (if (or (not *client-persistent-connections*)	;use persistent connections?
	  (eql :closed (connection-state connection))	;already closed?
	  (connection-close-p connection)	;instructions to close?
	  (> (the fixnum (connection-requests-completed connection))	;exceeding http 1.0 request limit?
	     (the fixnum (connection-requests-allowed connection))))
      (deallocate-connection connection)
      (note-free-connection connection)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CONNECTION.LISP.85")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(defun %deallocate-client-http-stream (stream &optional (abort-p t))
  (close stream :abort abort-p)
  (setf (www-utils:bytes-transmitted stream) 0
	(www-utils:bytes-received stream) 0)
  (prog1 (deallocate-client-http-stream stream)
         (atomic-incf *connections-deallocated*)
	 (client-trace "~&Deallocate Stream (~D): ~S" *connections-deallocated* stream)))

