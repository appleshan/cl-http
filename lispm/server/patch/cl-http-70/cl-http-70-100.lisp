;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.100
;;; Reason: Variable HTTP::*LOG-DEFAULT-NOTIFICATION-INTERVAL*:  -
;;; Variable HTTP::*LOG-DEFAULT-NOTIFICATION-TIMEOUT*:  -
;;; Variable HTTP::*LOG-DEFAULT-NOTIFICATION-PORT*:  -
;;; CLOS class HTTP::PROCESS-QUEUED-STREAM-NOTIFICATION-LOG-MIXIN:  -
;;; CLOS class HTTP::ASYNCHRONOUS-STREAM-NOTIFICATION-LOG:  -
;;; 
;;; Function (CLOS:METHOD HTTP:ADD-ACCESS-LOG (HTTP::PROCESS-QUEUED-STREAM-NOTIFICATION-LOG-MIXIN T) :BEFORE):  -
;;; Function (CLOS:METHOD HTTP:REMOVE-ACCESS-LOG (HTTP::PROCESS-QUEUED-STREAM-NOTIFICATION-LOG-MIXIN T) :AROUND):  -
;;; Function (CLOS:METHOD HTTP::UNREGISTER-LOG (HTTP::PROCESS-QUEUED-STREAM-NOTIFICATION-LOG-MIXIN) :BEFORE):  -
;;; Function (CLOS:METHOD HTTP::MAYBE-REMOVE-IDLE-STREAM-NOTIFICATION-LOG (HTTP::PROCESS-QUEUED-STREAM-NOTIFICATION-LOG-MIXIN)):  -
;;; Function (CLOS:METHOD HTTP::%WRITE-LOG-STREAM-NOTIFICATION (HTTP::PROCESS-QUEUED-STREAM-NOTIFICATION-LOG-MIXIN T T)):  -
;;; Function (CLOS:METHOD HTTP:LOG-ENTRY-WRITER (HTTP::PROCESS-QUEUED-STREAM-NOTIFICATION-LOG-MIXIN HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Function HTTP::LOG-WRITE-NOTIFICATION:  -
;;; Function HTTP::LOG-LIVE-STREAM-P:  -
;;; Function (CLOS:METHOD HTTP::LOG-LIVE-STREAM-P (HTTP::PROCESS-QUEUED-STREAM-NOTIFICATION-LOG-MIXIN T)):  -
;;; Function HTTP::%LOG-DEINSTALL-STREAM:  -
;;; Function (CLOS:METHOD TQ:TASK-QUEUE-EXECUTE-PENDING-TASKS (HTTP::PROCESS-QUEUED-STREAM-NOTIFICATION-LOG-MIXIN)):  -
;;; Function HTTP::CREATE-ASYNCHRONOUS-STREAM-NOTIFICATION-LOG:  -
;;; Function HTTP::ENSURE-ASYNCHRONOUS-STREAM-NOTIFICATION-LOG:  -
;;; Function (CLOS:METHOD HTTP::LOG-SERVER-ACCESS (HTTP::ASYNCHRONOUS-STREAM-NOTIFICATION-LOG HTTP::SERVER-LOGGING-MIXIN) AND):  -
;;; Function (CLOS:METHOD HTTP::LOG-INSTALL-STREAM (HTTP::ASYNCHRONOUS-STREAM-NOTIFICATION-LOG T)):  -
;;; Function (CLOS:METHOD HTTP::LOG-DEINSTALL-STREAM (HTTP::ASYNCHRONOUS-STREAM-NOTIFICATION-LOG T)):  -
;;; Function (CLOS:METHOD HTTP::LOG-SERVER-ACCESS (HTTP:CUSTOM-NOTIFICATION-LOG HTTP::SERVER-LOGGING-MIXIN) AND):  -
;;; Function (CLOS:METHOD HTTP::LOG-SERVER-ACCESS (HTTP::ASYNCHRONOUS-STREAM-NOTIFICATION-LOG HTTP::SERVER-LOGGING-MIXIN) AND):  -
;;; Function (CLOS:METHOD HTTP::LOG-ENTRY-P (HTTP::ASYNCHRONOUS-STREAM-NOTIFICATION-LOG HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Variable HTTP::*LOG-CLASSES*:  update.
;;; Written by JCMa, 12/14/00 19:02:04
;;; while running on FUJI-3 from FUJI:/usr/lib/symbolics/Inc-CL-HTTP-70-90-LMFS.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, LMFS 442.1, Color 427.1,
;;; Graphics Support 431.0, Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Color System Documentation 10.0, SGD Book Design 10.0, Images 431.2,
;;; Image Substrate 440.4, CLIM 72.0, Genera CLIM 72.0, CLX CLIM 72.0,
;;; PostScript CLIM 72.0, CLIM Demo 72.0, CLIM Documentation 72.0,
;;; Statice Runtime 466.1, Statice 466.0, Statice Browser 466.0,
;;; Statice Server 466.2, Statice Documentation 426.0, Metering 444.0,
;;; Metering Substrate 444.1, Symbolics Concordia 444.0, Graphic Editor 440.0,
;;; Graphic Editing 441.0, Bitmap Editor 441.0, Graphic Editing Documentation 432.0,
;;; Postscript 436.0, Concordia Documentation 432.0, Joshua 237.4,
;;; Joshua Documentation 216.0, Joshua Metering 206.0, Jericho 237.0, C 440.0,
;;; Lexer Runtime 438.0, Lexer Package 438.0, Minimal Lexer Runtime 439.0,
;;; Lalr 1 434.0, Context Free Grammar 439.0, Context Free Grammar Package 439.0,
;;; C Runtime 438.0, Compiler Tools Package 434.0, Compiler Tools Runtime 434.0,
;;; C Packages 436.0, Syntax Editor Runtime 434.0, C Library Headers 434,
;;; Compiler Tools Development 435.0, Compiler Tools Debugger 434.0,
;;; C Documentation 426.0, Syntax Editor Support 434.0, LL-1 support system 438.0,
;;; Pascal 433.0, Pascal Runtime 434.0, Pascal Package 434.0, Pascal Doc 427.0,
;;; Fortran 434.0, Fortran Runtime 434.0, Fortran Package 434.0, Fortran Doc 427.0,
;;; HTTP Proxy Server 6.11, HTTP Server 70.99, Showable Procedures 36.3,
;;; Binary Tree 34.0, W3 Presentation System 8.1, HTTP Client Substrate 4.4,
;;; HTTP Client 50.1, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Metering Patches 1.0,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Jericho Patches 1.0,
;;; Genera 8 5 Joshua Doc Patches 1.0, Genera 8 5 Joshua Metering Patches 1.0,
;;; Genera 8 5 Statice Runtime Patches 1.0, Genera 8 5 Statice Patches 1.0,
;;; Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.0,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clx Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Clim Demo Patches 1.0, Genera 8 5 Color Patches 1.1,
;;; Genera 8 5 Images Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Genera 8 5 Concordia Patches 1.0,
;;; Genera 8 5 Concordia Doc Patches 1.0, Genera 8 5 C Patches 1.0,
;;; Genera 8 5 Pascal Patches 1.0, Genera 8 5 Fortran Patches 1.0, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x976 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number -2141189522,
;;; Vlm lmfs patch (from W:>Reti>vlm-lmfs-patch.lisp.12),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.7).

;;; Patch file for CL-HTTP version 70.100
;;; Written by JCMa, 12/15/00 04:21:56
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.99,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42,
;;; Experimental Genera 8 5 Patches 1.0, Genera 8 5 System Patches 1.21,
;;; Genera 8 5 Mailer Patches 1.1, Genera 8 5 Joshua Patches 1.0,
;;; Genera 8 5 Statice Runtime Patches 1.0, Genera 8 5 Statice Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.0,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.7).


;;; Patch file for CL-HTTP version 70.100
;;; Written by JCMa, 12/14/00 23:20:18
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.99,
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
;;; Genera 8 5 Lock Simple Patches 1.0, Jcma 42, Ivory Revision 5, VLM Debugger 329,
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.7).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;VARIABLES.LISP.191"
  "HTTP:SERVER;VARIABLES.LISP.192"
  "HTTP:SERVER;CLASS.LISP.48"
  "HTTP:SERVER;LOG.LISP.203"
  "HTTP:SERVER;SERVER.LISP.879"
  "HTTP:SERVER;LOG.LISP.206"
  "HTTP:SERVER;LOG.LISP.207")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;VARIABLES.LISP.191")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp;-*-")

(defparameter *log-default-notification-interval* 5
  "The default number of entries to write before forcing output on a log display notification stream.")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;VARIABLES.LISP.191")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp;-*-")

(defparameter *log-default-notification-timeout* 1
  "The default number of seconds to wait for a log stream notification display to accept the update.")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;VARIABLES.LISP.192")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp;-*-")

(defparameter *log-default-notification-port* :multiport
  "The default port for which the log stream displays information.")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.48")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defclass process-queued-stream-notification-log-mixin (process-queued-logging-mixin)
    ((streams :initform nil :accessor log-streams)
     (ticks :initform 0 :accessor notification-log-ticks)
     (notification-timeout :initform *log-default-notification-timeout* :initarg :notification-timeout :accessor notification-log-timeout)
     (notification-interval :initform *log-default-notification-interval* :initarg :notification-interval :accessor notification-log-interval))
  (:documentation "Notifies stream clients asynchronously from the active HTTP connection to be logged."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.48")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

;;; This implements a common substrate for log displays, where the
;;; displays are streams.  This substrate can be used for remote log
;;; displays and for CLIM displays.

(defclass asynchronous-stream-notification-log
	  (process-queued-stream-notification-log-mixin
	    custom-notification-log
	    log-locking-mixin property-list-mixin)
    ()
  (:documentation "This log class notifies log streams about HTTP activity.
This class can be specialized for various log windows."))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.203")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod maybe-remove-idle-stream-notification-log ((log process-queued-stream-notification-log-mixin))
  (declare (values stream-notification-log-removed-p))
  (unless (log-streams log)
    (remove-access-log log (log-port log))
    t))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.203")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defgeneric %write-log-stream-notification (notification-log log-stream data)
  (:documentation "Writes the notification for NOTIFICATION-LOG TO LOG-STREAM according to DATA"))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.203")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod %write-log-stream-notification ((log process-queued-stream-notification-log-mixin) log-stream data)
  "The default method writes nothing."
  (declare (ignore log-stream data)
	   (ignorable log))
  nil)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.203")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod log-entry-writer ((log process-queued-stream-notification-log-mixin) (server server-logging-mixin))
  "Collects log data and calls %WRITE-LOG-STREAM-NOTIFICATION."
  (let* ((requests (server-requests-completed server))
	 (host-name (host-log-name server))
	 (user-name (%server-user-qualified-name server))
	 (request (server-request server t))
	 (request-time (server-request-time server))
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
    (flet ((write-the-entry (log-stream)
	     (%write-log-stream-notification
	       log log-stream
	       (list host-name port proxy-p requests request request-time status bytes
		     user-name user-agent-val referer-val cpu-time elapsed-time))))
      #'write-the-entry)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.203")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defgeneric log-write-notification (log server)
  (:documentation "Write the notification to the client connected to the LOG stream according SERVER parameters."))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.203")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defgeneric log-live-stream-p (log stream)
  (declare (values stream-live-p))
  (:documentation "Returns non-null when STREAM is considered alive for the purposes of LOG."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.203")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod log-live-stream-p ((log process-queued-stream-notification-log-mixin) stream)
  (declare (ignorable log stream))
  t)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.203")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defun %log-deinstall-stream (log stream)
  (setf (log-streams log) (delete stream (log-streams log))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.203")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod tq:task-queue-execute-pending-tasks ((log process-queued-stream-notification-log-mixin))
  (labels ((report-logging-error (log error fatal-p)
             (let ((error-type (type-of error)))
               (report-bug *bug-http-server* (format nil "HTTP~:[~; Fatal~] Stream Log Notification Error: ~S" fatal-p error-type)
			   "~:[~;~&stream Log notification has been suspended.~]~
                            ~&Log: ~S~&Error: ~S~:[~;~&Error Report: ~:*~A~]~:[~;~&Backtrace: ~:*~A~]"
			   fatal-p log error-type (report-string error)
			   (when *stack-backtraces-in-bug-reports*
			     (stack-backtrace-string error)))))
	   (%handle-logging-error (error)
	     (typecase error
	       ((or network-error file-error)
		(report-logging-error log error nil)
		(sleep 5)
		t)
	       (t (report-logging-error log error t)
		  (stop-log-queue log)
		  t))))
    (declare (dynamic-extent #'%handle-logging-error))
    (macrolet ((apply-log-streams ((log-streams timeout &key (stream-var 'stream)) &body body)
		 `(loop for ,stream-var in ,log-streams
			do (handler-case
			     (cond ((log-live-stream-p log ,stream-var)
				    (with-timeout (,timeout :error-p nil)
				      ,@body))
				   (t (%log-deinstall-stream log ,stream-var)))
			     (network-error () (%log-deinstall-stream log ,stream-var))))))
      (handler-bind-if (not *debug-server*)
	 ((error #'%handle-logging-error))
	(with-slots (notification-timeout notification-interval) log
	  (loop with write-timeout = notification-timeout
		with output-interval = (1- notification-interval)
		for closure = (tq:pop-task-queue log)
		while closure
		for count downfrom output-interval
		for log-streams = (log-streams log)
		do (cond (log-streams
			  (let ((force-output-p (zerop count)))
			    (apply-log-streams (log-streams write-timeout)
					       (funcall closure stream)
					       (when force-output-p (force-output stream)))
			    (when force-output-p (setq count output-interval))))
			 ;; no clients means deactivate log.
			 (t (remove-access-log log (log-port log))))
		while (tq:task-queue-run-p log)
		finally (apply-log-streams ((log-streams log) write-timeout)
					   (force-output stream))))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.203")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(PROGN
(declaim (inline asynchronous-stream-notification-log-p))

(defun asynchronous-stream-notification-log-p (log)
  (typep log 'asynchronous-stream-notification-log))
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.203")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defun get-asynchronous-stream-notification-log (&optional (port *log-default-notification-port*))
  (find-access-log-if #'asynchronous-stream-notification-log-p port))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.203")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defun ensure-asynchronous-stream-notification-log (&optional (port *log-default-notification-port*))
  (or (get-asynchronous-stream-notification-log port)
      (create-asynchronous-stream-notification-log port)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.203")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod log-install-stream ((log asynchronous-stream-notification-log) stream)
  (with-log-write-lock (log)
    (pushnew stream (log-streams log))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.203")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod log-deinstall-stream ((log asynchronous-stream-notification-log) stream)
  (with-log-write-lock (log)
    (%log-deinstall-stream log stream)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.879")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod log-server-access and ((log custom-notification-log) (server server-logging-mixin))
  (with-slots (notification predicate notifier) log
    (and notification
         (funcall predicate log server)
         (funcall notifier log server))
    t))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.206")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod log-write-notification ((log process-queued-stream-notification-log-mixin) (server server-logging-mixin))
  (with-slots (ticks) log
    (tq:push-task-queue log (log-entry-writer log server))
    (incf ticks)
    t))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.206")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defun create-asynchronous-stream-notification-log (&optional (port *log-default-notification-port*))
  (create-notification-access-log #'log-entry-p #'log-write-notification
				  :name "Asynchronous Notification Log"
				  :port port
				  :class 'asynchronous-stream-notification-log))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.206")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod log-entry-p and ((log asynchronous-stream-notification-log) (server server-logging-mixin))
  (declare (ignorable server))
  (log-streams log))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.207")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defparameter *log-classes* '(basic-common-file-log basic-extended-common-file-log common-file-log
                                                    extended-common-file-log http-log basic-url-metering
                                                    extended-http-log notification-log custom-notification-log
						    asynchronous-stream-notification-log)
  "The available log classes.")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.879")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(SCL:FUNDEFINE 'WRITE-LOG-ENTRY)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.880")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(define-generic log-write-entry (log agent stream)
  (:documentation "Standard synchronous method for writing an entry in a log file for a agent.
This method can be specialized on a log class to write out a different format."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.880")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod log-write-entry ((log common-file-format-mixin) (agent server-logging-mixin) stream)
  (%server-write-common-logfile-entry agent stream (log-times-in-gmt-p log) #\space))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.880")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod log-write-entry ((log extended-common-file-format-mixin) (agent server-logging-mixin) stream)
  (%server-write-extended-common-logfile-entry agent stream (log-times-in-gmt-p log) #\tab))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.880")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod log-write-entry ((log http-post-file-format-mixin) (agent server-logging-mixin) stream)
  (%server-write-post-logfile-entry agent stream (log-times-in-gmt-p log) #\tab))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.880")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod write-access-log-entry-to-file ((log file-logging-mixin) (agent server-logging-mixin))
  ;; This form is executed atomically with interprocess locking.
  ;; When log-file-stream-stays-open is turned on, entries are written to 
  ;; disk only after the stream buffer is filled.
  ;; While this reduces disk access, it can lose up to a buffer's worth of logs
  ;; in the event of a machine crash. FORCE-OUTPUT here would handle this
  (with-log-stream (log)
    (log-write-entry log agent log-stream)
    ;; Trailing CR makes this consistently parsable.
    (terpri log-stream)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.209")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(define-generic log-times-in-gmt-p (log)
  (:documentation "Returns non-null when log times should be written in GMT."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.209")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod log-times-in-gmt-p (log)
  (declare (ignore log))
  *log-times-in-gmt*)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.211")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

D,#TD1PsT[Begin using 006 escapes](1 0 (NIL 0) (NIL :ITALIC NIL) "CPTFONTI");; Make sure these clear the task queue
0(defmethod remove-access-log :after ((log process-queued-logging-mixin) port)
  (prog1 (call-next-method log port)
	 (tq:clear-task-queue log)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.211")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod remove-access-log :before ((log process-queued-stream-notification-log-mixin) port)
  (declare (ignore port))
  (setf (log-notification log) nil))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.211")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod add-access-log :after ((log process-queued-logging-mixin) port)
  "Ensure that task queue is active"
  (declare (ignore port))
  (tq:ensure-active-task-queue log))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.211")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

;;;------------------------------------------------------------------- 
;;;
;;; METHODS ON PROCESS-QUEUED-STREAM-NOTIFICATION-LOG-MIXIN
;;;
;;; A basic building block for STREAM-NOTIFICATION-LOG .

(defmethod add-access-log :before ((log process-queued-stream-notification-log-mixin) port)
  "Ensure that task queue is active"
  (declare (ignore port))
  (setf (log-notification log) t))
