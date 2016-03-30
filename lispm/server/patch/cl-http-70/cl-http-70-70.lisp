;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.70
;;; Reason: Function (CLOS:METHOD HTTP::PERSISTENT-CONNECTION-P (HTTP::BASIC-SERVER-MIXIN)):  only recognize :proxy-connection when proxying for http 1.0 clients.
;;; CLOS class HTTP::SERVER-EXTENDED-COMMON-FILE-LOG:  -
;;; Function HTML::%WRITE-TABLE-ARGUMENTS:  pass in width when signalling error and reduce consing.
;;; Written by JCMa, 9/13/00 16:50:23
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.6, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.69,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42, HTTP Proxy Server 5.22,
;;; HTTP Client Substrate 3.16, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.11, DEC OSF/1 V4.0 (Rev. 110),
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
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;SERVER.LISP.852"
  "HTTP:SERVER;CLASS.LISP.33"
  "HTTP:SERVER;HTML-3-2.LISP.37")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.852")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod persistent-connection-p ((server basic-server-mixin) &optional inside-transaction-p)
  (with-slots (requests-completed) server
    (cond ((server-close-connection-p server) nil)
          (t (case (server-http-version server)
	       (:http/0.9 nil)
	       (:http/1.0
		 (let* ((headers (server-headers server))
			(connection (if (server-proxy-request-p server)
					;; :proxy-connection deprecated 1.0 extension supported by Netscape & IE 4
					(get-header :proxy-connection headers)
					(get-header :connection headers))))
		   (and connection 
			(member :keep-alive connection)
			(> *persistent-connection-maximum-requests*
			   (if inside-transaction-p (1+ (the fixnum requests-completed)) requests-completed)))))
	       (t (let ((connection (get-header :connection (server-headers server))))
		    (if (member :close connection)
                        nil
                        (> *persistent-connection-maximum-requests*
                           (if inside-transaction-p (1+ (the fixnum requests-completed)) requests-completed))))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.33")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defclass server-extended-common-file-log
          (exclusive-server-log-mixin extended-common-file-log)
    ((log-file-name :initform "Ext-Common-Server-Log" :initarg :log-file-name :allocation :class))
  (:documentation "This log class records accesses according to the Common File Format without logging proxy requests.
It also records the referrer field and the user agent when they are present.
This will cons more than common-file-log because it must copy two header values."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML-3-2.LISP.37")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: html3.2; -*-")

(defun %write-table-arguments (stream width border cell-spacing cell-padding)
  (flet ((%write-width-argument (width stream)
           (if (<= 0 width 1)
	       (flet ((write-val (stream)
			(declare (float width))
			(fast-format stream "\"~D%\"" (floor (* width 100)))))
		 (declare (dynamic-extent #'write-val))
		 (%write-command-key-arg stream "WIDTH" #'write-val))
               (error "Table WIDTH is ~S, which is not a float between 0 and 1 as required by HTML 3.2." width))))
    (declare (inline %write-width-argument))
    (cond-every
      (width (%write-width-argument width stream))
      (border
        (%write-command-key-arg stream "BORDER" (if (integerp border) border 1.) t))
      (cell-spacing
        (%write-command-key-arg stream "CELLSPACING" cell-spacing t))
      (cell-padding
        (%write-command-key-arg stream "CELLPADDING" cell-padding t)))))

