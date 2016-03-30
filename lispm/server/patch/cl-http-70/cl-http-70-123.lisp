;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.123
;;; Reason: Fix console notification bug resulting from new logging architecture.
;;; 
;;; Remove function (CLOS:METHOD HTTP::SNAPSHOT-LOG-ENTRY (HTTP::ASYNCHRONOUS-LOG-NOTIFICATION-MIXIN HTTP::SERVER-LOGGING-MIXIN)): undefine.
;;; Function (CLOS:METHOD HTTP::SNAPSHOT-LOG-NOTIFICATION (HTTP::ASYNCHRONOUS-LOG-NOTIFICATION-MIXIN HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Function (CLOS:METHOD HTTP::LOG-SERVER-ACCESS (HTTP::ASYNCHRONOUS-LOG-NOTIFICATION-MIXIN HTTP::SERVER-LOGGING-MIXIN) AND):  -
;;; Written by JCMa, 4/23/01 03:14:42
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.122,
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
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Images Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; HTTP Proxy Server 6.19, HTTP Client Substrate 4.9, Statice Server 466.2,
;;; Images 431.2, Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Image Substrate 440.4, Jcma 43, Ivory Revision 5, VLM Debugger 329,
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
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;SERVER.LISP.905")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.905")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(SCL:FUNDEFINE '(METHOD SNAPSHOT-LOG-ENTRY
                 (ASYNCHRONOUS-LOG-NOTIFICATION-MIXIN SERVER-LOGGING-MIXIN)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.905")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod snapshot-log-notification ((log asynchronous-log-notification-mixin) (server server-logging-mixin))
  (let ((requests-completed (server-requests-completed server))
	(host-name (host-log-name server))
	(user-name (%server-user-qualified-name server))
	(request (server-request server t))
	(request-time (server-request-time server))
	(status (server-status server))
	(port *standard-http-port*)		;stream data may no longer be valid
	(bytes-transmitted (bytes-transmitted (server-stream server)))	;total bytes (not number of bytes in a document)
	(proxy-p (server-proxy-request-p server))
	header-set cpu-time elapsed-time user-agent-val referer-val)
    (unless proxy-p
      (when (setq header-set (server-headers server)))
      (with-header-values (user-agent referer) header-set
	(setq user-agent-val user-agent
	      referer-val referer)))
    (psetq cpu-time (cpu-time server)
	   elapsed-time (elapsed-time server))
    (allocate-console-notification-log-entry log host-name port request request-time status bytes-transmitted user-name
					     user-agent-val referer-val requests-completed cpu-time elapsed-time proxy-p)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.905")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod log-server-access and ((log asynchronous-log-notification-mixin) (agent server-logging-mixin))
  (when (log-notification log)
    (let ((log-entry (snapshot-log-notification log agent)))
      (when log-entry
	(tq:push-task-queue log log-entry))))
  t)

