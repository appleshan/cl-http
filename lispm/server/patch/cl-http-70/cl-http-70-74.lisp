;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.74
;;; Reason: Fix bugs in multiport statistics class.
;;; 
;;; Function (CLOS:METHOD HTTP::LOG-SERVER-ACCESS (HTTP::ASYNCHRONOUS-LOG-COUNTERS-MIXIN HTTP::SERVER-LOGGING-MIXIN) AND):  -
;;; CLOS class HTTP:ACCESS-STATISTICS-LOG:  -
;;; Remove function (CLOS:METHOD HTTP:LOG-ENTRY-WRITER (HTTP::ASYNCHRONOUS-LOG-COUNTERS-MIXIN HTTP::SERVER-LOGGING-MIXIN)): undefine.
;;; Function HTTP::LOG-COUNTERS-UPDATER:  -
;;; Function (CLOS:METHOD HTTP::LOG-COUNTERS-UPDATER (HTTP::ASYNCHRONOUS-LOG-COUNTERS-MIXIN HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Function HTTP::LOG-CONSOLE-NOTIFIER:  -
;;; Remove function (CLOS:METHOD HTTP:LOG-ENTRY-WRITER (HTTP::ASYNCHRONOUS-LOG-NOTIFICATION-MIXIN HTTP::SERVER-LOGGING-MIXIN)): -
;;; Function (CLOS:METHOD HTTP::LOG-CONSOLE-NOTIFIER (HTTP::ASYNCHRONOUS-LOG-NOTIFICATION-MIXIN HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Function (CLOS:METHOD HTTP::LOG-SERVER-ACCESS (HTTP::ASYNCHRONOUS-LOG-COUNTERS-MIXIN HTTP::SERVER-LOGGING-MIXIN) AND):  use log-counters-updater
;;; Function (CLOS:METHOD HTTP::LOG-SERVER-ACCESS (HTTP::ASYNCHRONOUS-LOG-NOTIFICATION-MIXIN HTTP::SERVER-LOGGING-MIXIN) AND):  -
;;; Written by JCMa, 9/17/00 01:40:57
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.73,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Documentation Patches 1.0,
;;; Genera 8 5 Clim Patches 1.0, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; HTTP Client Substrate 3.22, Jcma 42, HTTP Proxy Server 5.26, HTTP Client 49.10,
;;; Image Substrate 440.4, Essential Image Substrate 433.0,
;;; W4 Constraint-Guide Web Walker 45.4, W4 Examples 15.0, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.11, DEC OSF/1 V4.0 (Rev. 110),
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
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.1),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.5),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).

(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;CLASS.LISP.43"
  "HTTP:SERVER;SERVER.LISP.862")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.43")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defclass access-statistics-log
	  (asynchronous-log-counters-mixin
	    asynchronous-log-notification-mixin
	    log-locking-mixin
	    access-log)
    ()
  (:documentation "This log class maintains statistics on HTTP service."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.862")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(SCL:FUNDEFINE '(METHOD LOG-ENTRY-WRITER (ASYNCHRONOUS-LOG-COUNTERS-MIXIN SERVER-LOGGING-MIXIN)
                 ))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.862")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defgeneric log-counters-updater (asynchronous-log-counters-mixin server)
  (declare (values closure))
  (:documentation "Returns a closure of one argument that updates the log counters based on server.
This typically runs after a server or client has been deallocated or recycled on the next connection
and therefore must not share any structure."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.862")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod log-counters-updater ((log asynchronous-log-counters-mixin) (server server-logging-mixin)
				 &aux (stream (server-stream server)))
  (let ((cpu-time (cpu-time server))
        (elapsed-time (elapsed-time server))
        (status (server-status server))
        (method (server-method server))
	(proxy-p (server-proxy-request-p server))
        (requests-completed (server-requests-completed server))
        (bytes-transmitted (www-utils:bytes-transmitted stream))
        (bytes-received (www-utils:bytes-received stream)))
    (flet ((update-stats (log)
	     (update-log-statistics log status method proxy-p requests-completed
				    bytes-transmitted bytes-received elapsed-time cpu-time)))
      #'update-stats)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.862")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defgeneric log-console-notifier (asynchronous-log-counters-mixin server)
  (declare (values closure))
  (:documentation "Returns a closure of one argument that notifies the console based on SERVER.
This typically runs after a server or client has been deallocated or recycled on the next connection
and therefore must not share any structure."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.862")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(SCL:FUNDEFINE '(METHOD LOG-ENTRY-WRITER
                 (ASYNCHRONOUS-LOG-NOTIFICATION-MIXIN SERVER-LOGGING-MIXIN)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.862")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod log-console-notifier ((log asynchronous-log-notification-mixin) (server server-logging-mixin)
				 &aux (stream (server-stream server)))
  (let* ((requests (server-requests-completed server))
	 (host-name (host-log-name server))
	 (user-name (%server-user-qualified-name server))
	 (request (server-request server t))
	 (status (server-status server))
	 (port *standard-http-port*)		;stream data may no longer be valid
	 (bytes (bytes-transmitted (server-stream server)))	;total bytes (not number of bytes in a document)
	 (proxy-p (server-proxy-request-p server))
	 header-set cpu-time elapsed-time user-agent-val referer-val)
    (unless proxy-p
      (when (setq header-set (server-headers server)))
      (with-header-values (user-agent referer) header-set
	(setq user-agent-val user-agent
	      referer-val referer)))
    (psetq cpu-time (cpu-time server)
	   elapsed-time (elapsed-time server))
    (flet ((notify (log)
	     (when (log-notification log)	;don't lose with a slew of these backed up in the queue
	       (flet ((notify-the-console (log-stream)
			(%write-console-window-notification
			  host-name port proxy-p requests request nil status bytes user-name user-agent-val referer-val
			  cpu-time elapsed-time log-stream)))
		 (declare (dynamic-extent #'notify-the-console))
		 (careful-notify-log-window #'notify-the-console)))))
      #'notify)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.862")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod log-server-access and ((log asynchronous-log-notification-mixin) (agent server-logging-mixin))
  (when (log-notification log)
    (let ((notifier (log-console-notifier log agent)))
      (when notifier
	(tq:push-task-queue log notifier))))
  t) 


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.862")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod log-server-access and ((log asynchronous-log-counters-mixin) (agent server-logging-mixin))
  (let ((updater (log-counters-updater log agent)))
    (when updater
      (tq:push-task-queue log updater))
    t))

