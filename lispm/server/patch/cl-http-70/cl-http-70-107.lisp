;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.107
;;; Reason: Make logging completely object-oriented.
;;; 
;;; Reduces consing dramatically and abstracts the process of snapshotting agent
;;; state into a set of log-entry classes.
;;; 
;;; Running systems should be rebooted for a smooth transition.
;;; 
;;; CLOS class HTTP::NOTIFICATION-LOG-FORMAT-MIXIN:  -
;;; Remove function HTTP::LOG-CONSOLE-NOTIFIER: obsolete
;;; Remove function HTTP::LOG-COUNTERS-UPDATER: obsolete
;;; Remove function HTTP:LOG-ENTRY-WRITER: obsolete.
;;; Function HTTP::SNAPSHOT-LOG-ENTRY:  define.
;;; Function (CLOS:METHOD HTTP::SNAPSHOT-LOG-ENTRY (HTTP::COMMON-FILE-FORMAT-MIXIN HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Function (CLOS:METHOD HTTP::SNAPSHOT-LOG-ENTRY (HTTP::EXTENDED-COMMON-FILE-FORMAT-MIXIN HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Function (CLOS:METHOD HTTP::SNAPSHOT-LOG-ENTRY (HTTP::HTTP-POST-FILE-FORMAT-MIXIN HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Function (CLOS:METHOD HTTP::SNAPSHOT-LOG-ENTRY (HTTP::PROXY-EXTENDED-COMMON-FILE-FORMAT-MIXIN HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Function (CLOS:METHOD HTTP::SNAPSHOT-LOG-ENTRY (HTTP::NOTIFICATION-LOG-FORMAT-MIXIN HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Function (CLOS:METHOD HTTP::SNAPSHOT-LOG-ENTRY (HTTP::ASYNCHRONOUS-LOG-NOTIFICATION-MIXIN HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Function (CLOS:METHOD HTTP::SNAPSHOT-LOG-ENTRY (HTTP::ASYNCHRONOUS-LOG-COUNTERS-MIXIN HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Function (CLOS:METHOD HTTP::WRITE-LOG-ENTRY (HTTP::COUNTERS-LOG-ENTRY T)):  -
;;; Remove function (CLOS:METHOD HTTP::LOG-SERVER-ACCESS (HTTP::ASYNCHRONOUS-LOG-COUNTERS-MIXIN HTTP::SERVER-LOGGING-MIXIN) AND): -
;;; Remove function (CLOS:METHOD HTTP::LOG-SERVER-ACCESS (HTTP::ASYNCHRONOUS-LOG-NOTIFICATION-MIXIN HTTP::SERVER-LOGGING-MIXIN) AND): -
;;; Function (CLOS:METHOD HTTP::LOG-SERVER-ACCESS (HTTP::ASYNCHRONOUS-LOG-COUNTERS-MIXIN HTTP::SERVER-LOGGING-MIXIN) AND):  -
;;; Function (CLOS:METHOD HTTP::LOG-SERVER-ACCESS (HTTP::ASYNCHRONOUS-LOG-NOTIFICATION-MIXIN HTTP::SERVER-LOGGING-MIXIN) AND):  -
;;; Function (CLOS:METHOD HTTP::WRITE-LOG-ENTRY-TO-FILE (HTTP::BASIC-PROCESS-QUEUED-FILE-LOGGING-MIXIN HTTP::LOGGING-MIXIN)):  -
;;; Function (CLOS:METHOD HTTP::LOG-WRITE-NOTIFICATION (HTTP::PROCESS-QUEUED-STREAM-NOTIFICATION-LOG-MIXIN HTTP::SERVER-LOGGING-MIXIN)):  -
;;; CLOS class HTTP::ASYNCHRONOUS-LOG-ENTRY-LOGGING-MIXIN:  -
;;; CLOS class HTTP::BASIC-PROCESS-QUEUED-FILE-LOGGING-MIXIN:  -
;;; CLOS class HTTP::ASYNCHRONOUS-LOG-COUNTERS-MIXIN:  -
;;; CLOS class HTTP::ASYNCHRONOUS-LOG-NOTIFICATION-MIXIN:  -
;;; CLOS class HTTP::PROCESS-QUEUED-STREAM-NOTIFICATION-LOG-MIXIN:  -
;;; CLOS class HTTP::NOTIFICATION-LOG-FORMAT-MIXIN:  -
;;; Function (CLOS:METHOD TQ:TASK-QUEUE-EXECUTE-PENDING-TASKS (HTTP::BASIC-PROCESS-QUEUED-FILE-LOGGING-MIXIN)):  -
;;; Function (CLOS:METHOD TQ:CLEAR-TASK-QUEUE (HTTP::ASYNCHRONOUS-LOG-ENTRY-LOGGING-MIXIN) :AROUND):  -
;;; Function (CLOS:METHOD TQ:TASK-QUEUE-EXECUTE-TASK (HTTP::ASYNCHRONOUS-LOG-ENTRY-LOGGING-MIXIN HTTP::LOG-ENTRY)):  -
;;; Remove function HTTP::%WRITE-LOG-STREAM-NOTIFICATION: -
;;; Written by JCMa, 12/17/00 02:02:28
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.106,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 54.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42,
;;; Experimental Genera 8 5 Patches 1.0, Genera 8 5 System Patches 1.21,
;;; Genera 8 5 Mailer Patches 1.1, Genera 8 5 Joshua Patches 1.0,
;;; Genera 8 5 Statice Runtime Patches 1.0, Genera 8 5 Statice Patches 1.0,
;;; Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.0,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, HTTP Proxy Server 6.11,
;;; HTTP Client Substrate 4.4, Statice Server 466.2, HTTP Client 50.1,
;;; Image Substrate 440.4, Essential Image Substrate 433.0,
;;; W4 Constraint-Guide Web Walker 45.7, W4 Examples 14.0,
;;; Experimental CL-HTTP CLIM User Interface 1.0, Ivory Revision 5, VLM Debugger 329,
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.7),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).

;;; Patch file for CL-HTTP version 70.107
;;; Written by JCMa, 12/17/00 03:40:01
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.106,
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
  "HTTP:SERVER;CLASS.LISP.49"
  "HTTP:SERVER;SERVER.LISP.882"
  "HTTP:SERVER;LOG.LISP.212"
  "HTTP:SERVER;LOG.LISP.213")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.882")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(SCL:FUNDEFINE 'LOG-CONSOLE-NOTIFIER)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.882")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(SCL:FUNDEFINE 'LOG-COUNTERS-UPDATER)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.882")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(SCL:FUNDEFINE 'LOG-ENTRY-WRITER)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.213")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(SCL:FUNDEFINE '%WRITE-LOG-STREAM-NOTIFICATION)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.49")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defclass notification-log-format-mixin
          ()
    ()
  (:documentation "Mixes in Notification Log format Format logging."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.49")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defclass asynchronous-log-entry-logging-mixin
	  ()
    ()
  (:documentation "Mixin that provides asynchronous methods for logging server access and log-entry resource management."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.49")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defclass basic-process-queued-file-logging-mixin
          (asynchronous-log-entry-logging-mixin process-queued-logging-mixin file-logging-mixin)
    ())


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.49")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defclass asynchronous-log-counters-mixin
	  (log-counters-mixin asynchronous-log-entry-logging-mixin process-queued-logging-mixin)
    ()
  (:documentation "A mixin that updates statistics asynchonously."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.49")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defclass asynchronous-log-notification-mixin
          (log-notification-mixin asynchronous-log-entry-logging-mixin process-queued-logging-mixin)
    ())


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.49")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defclass process-queued-stream-notification-log-mixin
	  (asynchronous-log-entry-logging-mixin process-queued-logging-mixin)
    ((streams :initform nil :accessor log-streams)
     (ticks :initform 0 :accessor notification-log-ticks)
     (notification-timeout :initform *log-default-notification-timeout* :initarg :notification-timeout :accessor notification-log-timeout)
     (notification-interval :initform *log-default-notification-interval* :initarg :notification-interval :accessor notification-log-interval))
  (:documentation "Notifies stream clients asynchronously from the active HTTP connection to be logged."))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.49")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(PROGN
(defclass log-entry
	  ()
    ((owner :initarg :owner :accessor log-entry-owner))
  (:documentation "The basic class for log entry snapshots."))

(defclass bytes-received-log-entry-mixin
	  ()
    ((bytes-received :initarg :bytes-received :accessor log-entry-bytes-received)))

(defclass bytes-transmitted-log-entry-mixin
	  ()
    ((bytes-transmitted :initarg :bytes-transmitted :accessor log-entry-bytes-transmitted)))	;total bytes-transmitted

(defclass metering-log-entry-mixin
	  ()
    ((cpu-time :initarg :cpu-time :accessor log-entry-cpu-time)
     (elapsed-time :initarg :elapsed-time :accessor log-entry-elapsed-time)))

(defclass method-log-entry-mixin
	  ()
    ((method :initarg :method :accessor log-entry-method)))

(defclass proxy-p-log-entry-mixin
	  ()
    ((proxy-p :initarg :proxy-p :accessor log-entry-proxy-p)))

(defclass requests-completed-log-entry-mixin
	  ()
    ((requests-completed :initarg :requests-completed :accessor log-entry-requests-completed)))

(defclass status-log-entry-mixin
	  ()
    ((status :initarg :status :accessor log-entry-status)))

(defclass common-log-entry
	  (bytes-transmitted-log-entry-mixin status-log-entry-mixin log-entry)
    ((host-name :initarg :host-name :accessor log-entry-host-name)
     (request :initarg :request :accessor log-entry-request)
     (request-time :initarg :request-time :accessor log-entry-request-time)
     (user-name :initarg :user-name :accessor log-entry-user-name))
  (:documentation "The log entry class that writes common log file entries."))

(defclass extended-common-log-entry
	  (common-log-entry)
    ((user-agent :initarg :user-agent :accessor log-entry-user-agent)
     (referer :initarg :referer :accessor log-entry-referer)))

(defclass notification-log-entry
	  (metering-log-entry-mixin proxy-p-log-entry-mixin requests-completed-log-entry-mixin extended-common-log-entry)
    ((port :initarg :port :accessor log-entry-port)))

(defclass console-notification-mixin () ())

(defclass console-notification-log-entry (console-notification-mixin notification-log-entry) ())

(defclass http-post-log-entry
	  (bytes-received-log-entry-mixin extended-common-log-entry)
    ((form-alist :initarg :form-alist :accessor log-entry-form-alist)))

(defclass counters-log-entry
	  (metering-log-entry-mixin proxy-p-log-entry-mixin requests-completed-log-entry-mixin
				    bytes-received-log-entry-mixin bytes-transmitted-log-entry-mixin
				    method-log-entry-mixin status-log-entry-mixin log-entry)
    ()
  (:documentation "The log entry class collects statistics on server usage."))
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.212")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(PROGN

;;;------------------------------------------------------------------- 
;;;
;;; LOG-ENTRY RESOURCE
;;;

(define-generic initialize-log-entry (resource log-entry class))

(defmethod initialize-log-entry (resource (log-entry log-entry) class)
  (declare (ignorable resource class)) 
  log-entry)

(define-generic deinitialize-log-entry (resource log-entry)
  (:method-combination progn)
  (:documentation "Deinitializes any datastructures and free any garbage quickly for the EGC."))

(defmethod deinitialize-log-entry progn (resource (log-entry log-entry))
  (declare (ignorable resource))
  (setf (log-entry-owner log-entry) nil))

(defmethod deinitialize-log-entry progn (resource (log-entry bytes-received-log-entry-mixin))
  (declare (ignorable resource))
  (setf (log-entry-bytes-received log-entry) nil))

(defmethod deinitialize-log-entry progn (resource (log-entry bytes-transmitted-log-entry-mixin))
  (declare (ignorable resource))
  (setf (log-entry-bytes-transmitted log-entry) nil))

(defmethod deinitialize-log-entry progn (resource (log-entry metering-log-entry-mixin))
  (declare (ignorable resource))
  (setf (log-entry-cpu-time log-entry) nil
	(log-entry-elapsed-time log-entry) nil))

(defmethod deinitialize-log-entry progn (resource (log-entry common-log-entry))
  (declare (ignorable resource))
  (setf (log-entry-host-name log-entry) nil
	(log-entry-request log-entry) nil
	(log-entry-request-time log-entry) nil
	(log-entry-user-name log-entry) nil))

(defmethod deinitialize-log-entry progn (resource (log-entry extended-common-log-entry))
  (declare (ignorable resource))
  (setf (log-entry-user-agent log-entry) nil
	(log-entry-referer log-entry) nil))

(defmethod deinitialize-log-entry progn (resource (log-entry http-post-log-entry))
  (declare (ignorable resource))
  (setf (log-entry-form-alist log-entry) nil))

(defun make-log-entry (resource class)
  (declare (ignore resource))
  (make-instance class))

(defun match-log-entry-p (resource log-entry class)
  (declare (ignore resource))
  (eql class (type-of log-entry)))

(defresource log-entry (class)
  :constructor make-log-entry
  :matcher match-log-entry-p
;;  :initializer initialize-log-entry
  :deinitializer deinitialize-log-entry
  :initial-copies 0)

(define clear-log-entry-resource ()
  "Clears the resource of log-entry objects."
  (clear-resource 'log-entry))

(defgeneric deallocate-log-entry (log-entry)
  (:documentation "Top-level method for deallocating a log-entry."))

(defmethod deallocate-log-entry ((log-entry log-entry))
  (deallocate-resource 'log-entry log-entry))

(eval-when (:execute :compile-toplevel :load-toplevel)
(defun log-entry-slot-accessor (keyword)
  (or (get keyword 'log-entry-slot-accessor)
      (setf (get keyword 'log-entry-slot-accessor) (intern (concatenate 'string "LOG-ENTRY-" (symbol-name keyword)) *http-package*))))

(defmacro allocate-log-entry (class &rest parameters)
  (declare (dynamic-extent parameters))
  (loop for (slot value) on parameters by #'cddr
	for accessor = (log-entry-slot-accessor slot)
	collect `(setf (,accessor log-entry) ,value) into initilizers
	finally (return `(let ((log-entry (allocate-resource 'log-entry ',class)))
			   (declare (type log-entry ,class))
			   ,@initilizers
			   log-entry))))

(defmacro define-log-entry-allocator (name arguments &body body)
  "Defines an inline log-entry allocator."
  `(eval-when (:execute :compile-toplevel :load-toplevel)
     (declaim (inline ,name))
     (defun ,name ,arguments
       #+Genera(declare (sys:function-parent ,name define-log-entry-allocator))
       ,@body)))
)						;close eval-when


;;;------------------------------------------------------------------- 
;;;
;;; LOG-ENTRY ALLOCATION
;;;

(define-log-entry-allocator allocate-common-log-entry (log host-name request request-time status bytes-transmitted user-name)
  (allocate-log-entry common-log-entry
		      :owner log :host-name host-name :request request :request-time request-time :status status
		      :bytes-transmitted bytes-transmitted :user-name user-name))

(define-log-entry-allocator allocate-extended-common-log-entry (log host-name request request-time status bytes-transmitted user-name user-agent referer)
  (allocate-log-entry extended-common-log-entry
		 :owner log :host-name host-name :request request :request-time request-time :status status
		 :bytes-transmitted bytes-transmitted :user-name user-name :user-agent user-agent :referer referer))

(define-log-entry-allocator allocate-http-post-log-entry (log host-name request request-time status bytes-transmitted user-name
					 user-agent referer bytes-received form-alist)
  (allocate-log-entry http-post-log-entry
		 :owner log :host-name host-name :request request :request-time request-time :status status
		 :bytes-transmitted bytes-transmitted :user-name user-name :user-agent user-agent :referer referer :bytes-received bytes-received
		 :form-alist form-alist))

(define-log-entry-allocator allocate-notification-log-entry (log host-name port request request-time status bytes-transmitted user-name
					    user-agent referer requests-completed cpu-time elapsed-time proxy-p)
  (allocate-log-entry notification-log-entry
		 :owner log :host-name host-name :port port :request request :request-time request-time :status status
		 :bytes-transmitted bytes-transmitted :user-name user-name :user-agent user-agent :referer referer
		 :requests-completed requests-completed :cpu-time cpu-time :elapsed-time elapsed-time :proxy-p proxy-p))

(define-log-entry-allocator allocate-console-notification-log-entry (log host-name port request request-time status bytes-transmitted user-name
					    user-agent referer requests-completed cpu-time elapsed-time proxy-p)
  (allocate-log-entry console-notification-log-entry
		 :owner log :host-name host-name :port port :request request :request-time request-time :status status
		 :bytes-transmitted bytes-transmitted :user-name user-name :user-agent user-agent :referer referer
		 :requests-completed requests-completed :cpu-time cpu-time :elapsed-time elapsed-time :proxy-p proxy-p))

(define-log-entry-allocator allocate-counters-log-entry (log method status bytes-received bytes-transmitted requests-completed proxy-p cpu-time elapsed-time)
  (allocate-log-entry counters-log-entry
		      :owner log :method method :status status :bytes-received bytes-received :bytes-transmitted bytes-transmitted
		      :requests-completed requests-completed :proxy-p proxy-p :cpu-time cpu-time :elapsed-time elapsed-time))
)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.882")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defgeneric snapshot-log-entry (log agent)
  (declare (values log-entry))
  (:documentation "Snapshots values of agent according to the class of log and returns a log-entry object.
This method should be specialized for each logging format mixin for appropriate data capture and
WRITE-LOG-ENTRY should be specialized on the log-entry class to write the desired output."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.882")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod snapshot-log-entry ((log common-file-format-mixin) (server server-logging-mixin))
  (let ((host-name (host-log-name server))
	(user-name (%server-user-qualified-name server))
	(request (server-request server t))	; don't lose when transaction reset
	(request-time (server-request-time server))
	(status (server-status server))
	(bytes-transmitted (server-bytes-transmitted server)))	;total bytes-transmitted (not number of bytes-transmitted in a document
    (allocate-common-log-entry log host-name request request-time status bytes-transmitted user-name)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.882")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod snapshot-log-entry ((log extended-common-file-format-mixin) (server server-logging-mixin))
  (let ((host-name (host-log-name server))
	(user-name (%server-user-qualified-name server))
	(request (server-request server t))	; don't lose when transaction reset
	(request-time (server-request-time server))
	(status (server-status server))
	(bytes-transmitted (server-bytes-transmitted server))	;total bytes-transmitted (not number of bytes-transmitted in a document)
	(header-set (server-headers server))
	user-agent-val referer-val)
    (when header-set				;may be null when client drops connection (408)
      (with-header-values (user-agent referer) (server-headers server)
	(setq user-agent-val user-agent
	      referer-val referer)))
    (allocate-extended-common-log-entry log host-name request request-time status bytes-transmitted user-name user-agent-val referer-val)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.882")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod snapshot-log-entry ((log http-post-file-format-mixin) (server server-logging-mixin) &aux form-alist)
  (when (and (eq :post (server-method server))
	     (setq form-alist (server-form-alist server)))
    (let ((host-name (host-log-name server))
	  (user-name (%server-user-qualified-name server))
	  (request-time (server-request-time server))
	  (request (server-request server t))
	  (status (server-status server))
	  (bytes-transmitted (server-bytes-transmitted server))
	  (bytes-received (server-bytes-received server))
	  (headers (server-headers server)))
      (with-header-values (user-agent referer) headers
	(allocate-http-post-log-entry log host-name request request-time status bytes-transmitted user-name
				      user-agent referer bytes-received form-alist)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.882")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod snapshot-log-entry ((log notification-log-format-mixin) (server server-logging-mixin))
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
    (allocate-notification-log-entry log host-name port request request-time status bytes-transmitted user-name
				     user-agent-val referer-val requests-completed cpu-time elapsed-time proxy-p)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.882")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod snapshot-log-entry ((log asynchronous-log-notification-mixin) (server server-logging-mixin))
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
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.882")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod snapshot-log-entry ((log asynchronous-log-counters-mixin) (server server-logging-mixin) &aux (stream (server-stream server)))
  (let ((cpu-time (cpu-time server))
	(elapsed-time (elapsed-time server))
	(bytes-received (www-utils:bytes-received stream))
        (bytes-transmitted (www-utils:bytes-transmitted stream))
        (method (server-method server))
	(proxy-p (server-proxy-request-p server))
        (requests-completed (server-requests-completed server))
        (status (server-status server)))
    (allocate-counters-log-entry log method status bytes-received bytes-transmitted requests-completed proxy-p cpu-time elapsed-time)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.212")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(PROGN

;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(define-generic write-log-entry (log-entry stream)
  (:documentation "Write LOG-ENTRY on STREAM.
Specialize this method for different log entry classes.
See snapshot-log-entry for capturing corresponding data."))

(defmethod write-log-entry ((entry-data common-log-entry) stream)
  (with-slots (owner host-name request request-time status bytes-transmitted user-name) entry-data
    (%write-common-logfile-entry host-name request request-time status bytes-transmitted user-name (log-times-in-gmt-p owner) stream #\space)
    ;; Trailing CR makes this consistently parsable.
    (terpri stream)))

(defmethod write-log-entry ((entry-data extended-common-log-entry) stream)
  (with-slots (owner host-name request request-time status bytes-transmitted user-name user-agent referer) entry-data
    (%write-extended-common-logfile-entry
      host-name request request-time status bytes-transmitted user-name user-agent referer (log-times-in-gmt-p owner) stream #\tab)
    ;; Trailing CR makes this consistently parsable.
    (terpri stream)))

(defmethod write-log-entry ((entry-data http-post-log-entry) stream)
  (with-slots (owner host-name request request-time status bytes-transmitted user-name
		     user-agent referer bytes-received form-alist) entry-data
    (%write-http-post-logfile-entry host-name user-name request-time request status bytes-received bytes-transmitted
				    user-agent referer form-alist (log-times-in-gmt-p owner) stream #\tab)
    ;; Trailing CR makes this consistently parsable.
    (terpri stream)))

(defmethod write-log-entry ((entry-data notification-log-entry) stream)
  (with-slots (owner host-name port request request-time status bytes-transmitted user-name
		     user-agent referer requests-completed cpu-time elapsed-time proxy-p) entry-data
    (when (log-notification owner)
      (%write-console-window-notification
	host-name port proxy-p requests-completed request request-time status bytes-transmitted user-name user-agent referer
	cpu-time elapsed-time stream))))

(defmethod write-log-entry ((entry-data console-notification-log-entry) stream)
  (declare (ignore stream))
  (with-slots (owner host-name port request status bytes-transmitted user-name
		     user-agent referer requests-completed cpu-time elapsed-time proxy-p) entry-data
    (when (log-notification owner)
      (flet ((notify-the-console (log-stream)
	       (%write-console-window-notification
		 host-name port proxy-p requests-completed request nil status bytes-transmitted user-name user-agent referer
		 cpu-time elapsed-time log-stream)))
	(declare (dynamic-extent #'notify-the-console))
	(careful-notify-log-window #'notify-the-console)))))

(defmethod write-log-entry ((entry-data counters-log-entry) stream)
  (declare (ignore stream))
  (with-slots (owner method status bytes-received bytes-transmitted requests-completed proxy-p cpu-time elapsed-time) entry-data
    (update-log-statistics owner status method proxy-p requests-completed bytes-transmitted bytes-received elapsed-time cpu-time)))
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.882")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod log-server-access and ((log asynchronous-log-counters-mixin) (agent server-logging-mixin))
  (let ((log-entry (snapshot-log-entry log agent)))
    (when log-entry
      (tq:push-task-queue log log-entry))
    t))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.882")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod log-server-access and ((log asynchronous-log-notification-mixin) (agent server-logging-mixin))
  (when (log-notification log)
    (let ((log-entry (snapshot-log-entry log agent)))
      (when log-entry
	(tq:push-task-queue log log-entry))))
  t)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.882")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod write-log-entry-to-file ((log basic-process-queued-file-logging-mixin) (agent logging-mixin))
  (let ((log-entry (snapshot-log-entry log agent)))
    (when log-entry
      (tq:push-task-queue log log-entry)))) 


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.212")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod log-write-notification ((log process-queued-stream-notification-log-mixin) (server server-logging-mixin))
  (with-slots (ticks) log
    (tq:push-task-queue log (snapshot-log-entry log server))
    (incf ticks)
    t))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.213")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod tq:task-queue-execute-pending-tasks ((log basic-process-queued-file-logging-mixin))
  (labels ((report-logging-error (log error fatal-p)
             (let ((error-type (type-of error)))
               (report-bug *bug-http-server* (format nil "HTTP~:[~; Fatal~] Logging Error: ~S" fatal-p error-type)
                           "~:[~;~&Logging has been suspended. Attend to the error at once and resume logging.~]~
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
    (handler-bind-if (not *debug-server*)
       ((error #'%handle-logging-error))
      (%with-log-stream (log :open-file-forces-output-p t)
        (loop with log-entry
              while (and (unwind-protect
			     (when (setq log-entry (tq:pop-task-queue log))
			       (write-log-entry log-entry log-stream)
			       t)
			   (when log-entry
			     (deallocate-log-entry log-entry)))
			 (tq:task-queue-run-p log)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.213")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

;;;------------------------------------------------------------------- 
;;;
;;; ASYNCHRONOUS LOG-ENTRY LOGGING HYGINE
;;;
(defmethod tq:clear-task-queue :around ((task-queue asynchronous-log-entry-logging-mixin) &aux flushed-queue)
  (unwind-protect
      (setq flushed-queue (call-next-method))
    (mapc #'deallocate-log-entry flushed-queue)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.213")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod tq:task-queue-execute-task ((task-queue asynchronous-log-entry-logging-mixin) (log-entry log-entry))
  (unwind-protect
      (write-log-entry log-entry nil)
    (deallocate-log-entry log-entry)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.213")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod tq:task-queue-execute-pending-tasks ((log process-queued-stream-notification-log-mixin))
  (macrolet ((apply-log-streams ((log-streams timeout &key (stream-var 'stream)) &body body)
	       `(loop for ,stream-var in ,log-streams
		      do (handler-case
			   (cond ((log-live-stream-p log ,stream-var)
				  (with-timeout (,timeout :error-p nil)
				    ,@body))
				 (t (%log-deinstall-stream log ,stream-var)))
			   (network-error () (%log-deinstall-stream log ,stream-var))))))
    (labels ((report-logging-error (log error fatal-p)
	       (let ((error-type (type-of error)))
		 (report-bug *bug-http-server* (format nil "HTTP~:[~; Fatal~] Stream Log Notification Error: ~S" fatal-p error-type)
			     "~:[~;~&Stream log notification has been suspended.~]~
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
		    t)))
	     (do-log-entry (log log-streams force-output-p write-timeout &aux log-entry)
	       (unwind-protect
		   (when (setq log-entry (tq:pop-task-queue log))
		     (apply-log-streams (log-streams write-timeout)
					(write-log-entry log-entry stream)
					(when force-output-p (force-output stream)))
		     t)
		 (when log-entry
		   (deallocate-log-entry log-entry)))))
      (declare (dynamic-extent #'%handle-logging-error))
      (handler-bind-if (not *debug-server*)
	 ((error #'%handle-logging-error))
	(let ((write-timeout (notification-log-timeout log))
	      (output-interval (1- (notification-log-interval log))))
	  (loop with count = output-interval and force-output-p
		for log-streams = (log-streams log)
		while (and (cond (log-streams
				  (setq force-output-p (zerop (decf count output-interval)))
				  (prog1 (do-log-entry log log-streams force-output-p write-timeout)
					 (when force-output-p (setq count output-interval))))
				 ;; no clients means deactivate log.
				 (t (remove-access-log log (log-port log))
				    (return nil)))
			   (tq:task-queue-run-p log))
		finally (apply-log-streams ((log-streams log) write-timeout)
					   (force-output stream))))))))
l