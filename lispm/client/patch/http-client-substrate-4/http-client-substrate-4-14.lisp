;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: cl-user; Base: 10; Patch-File: t -*-
;;; Patch file for HTTP-CLIENT-SUBSTRATE version 4.14
;;; Reason: Function HTTP::PARSE-REPLY:  close connection on bad server response.
;;; Function (CLOS:METHOD HTTP::FLUSH-INPUT-ENTITY (T T T)):  -
;;; Written by JCMa, 5/12/01 18:13:15
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.133,
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
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Lock Simple Patches 1.0, Jcma 43,
;;; HTTP Proxy Server 6.22, HTTP Client Substrate 4.13, Statice Server 466.2,
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
  "HTTP:CLIENT;CLIENT.LISP.283")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CLIENT.LISP.283")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun parse-reply (reply-line &optional (length (length reply-line)) &aux pos1 pos2 pos3)
  (declare (values status-code http-version reason-offset)
	   (fixnum length))
  (flet ((space-p (char) (eql char #\space)))
    (declare (inline space-p))
    (cond
      ((and (setq pos1 (char-position #\space reply-line 0 length))
	    (setq pos2 (%fast-position-if-not space-p reply-line :start pos1 :end length)))
       ;; Client is not required to examine or display the reason
       (setq pos3 (char-position #\space reply-line pos2 length))
       (let ((status-code (parse-integer reply-line :start pos2 :end (or pos3 length) :junk-allowed t))
	     (reason-offset (or (and pos3 (%fast-position-if-not space-p reply-line :start pos3 :end length))
				0))
	     (http-version (tokenize-header-keyword reply-line 0 pos1)))
	 (client-trace "~&Response: ~S~&~%" reply-line)
	 (values status-code http-version reason-offset)))
      (t (error 'bad-server-response :response reply-line :close-connection t
		:format-string "Ill-formed reply from HTTP server, ~S." :format-args (list (copy-seq reply-line)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CLIENT.LISP.283")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod flush-input-entity (stream headers http-version &aux content-length)
  (declare (ignore http-version))
  (cond ((setq content-length (get-header :content-length headers))
         (advance-input-buffer stream content-length))
        (t (let ((transfer-encoding (get-header :transfer-encoding headers)))
             (case transfer-encoding
               (:chunked
                 (with-chunked-transfer-decoding (stream :headers headers)
                   (advance-input-buffer stream)))
               ((nil) (error 'bad-syntax-provided :url (client-url *client*):close-connection t
                             :format-string "No content length header was provided."))
               (t (error 'server-not-implemented :close-connection t :url (client-url *client*)
                         :format-string "The HTTP transfer decoding, ~A, is not implemented."
                         :format-args (list transfer-encoding))))))))

