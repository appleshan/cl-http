;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.58
;;; Reason: Infrastructure for client logging.
;;; 
;;; Function HTTP::%FILE-NAME-FOR-LOG:  new.
;;; Function (CLOS:METHOD HTTP::%FILE-NAME-FOR-LOG (HTTP::BASIC-LOG-MIXIN)):  -
;;; Function (CLOS:METHOD HTTP::INITIALIZE-LOG-FILENAME (HTTP::FILE-LOGGING-MIXIN)):  use %file-name-for-log.
;;; CLOS class HTTP::LOGGING-MIXIN:  new.
;;; CLOS class HTTP::BASIC-LOG-MIXIN:  use it.
;;; Function HTTP::WRITE-LOG-ENTRY-TO-FILE:  -
;;; Function (CLOS:METHOD HTTP::WRITE-LOG-ENTRY-TO-FILE (HTTP::BASIC-PROCESS-QUEUED-FILE-LOGGING-MIXIN HTTP::LOGGING-MIXIN)):  -
;;; Remove function HTTP:WRITE-ACCESS-LOG-ENTRY: undefine.
;;; Function HTTP::WRITE-LOG-ENTRY:  renamed.
;;; Function (CLOS:METHOD HTTP::WRITE-LOG-ENTRY (HTTP::COMMON-FILE-FORMAT-MIXIN HTTP::SERVER-LOGGING-MIXIN T T)):  -
;;; Function (CLOS:METHOD HTTP::WRITE-LOG-ENTRY (HTTP::EXTENDED-COMMON-FILE-FORMAT-MIXIN HTTP::SERVER-LOGGING-MIXIN T T)):  -
;;; Function (CLOS:METHOD HTTP::WRITE-LOG-ENTRY (HTTP::HTTP-POST-FILE-FORMAT-MIXIN HTTP::SERVER-LOGGING-MIXIN T T)):  -
;;; Function (CLOS:METHOD HTTP::WRITE-ACCESS-LOG-ENTRY-TO-FILE (HTTP::FILE-LOGGING-MIXIN HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Function (CLOS:METHOD HTTP::LOG-SERVER-ACCESS (HTTP::FILE-LOGGING-MIXIN HTTP::SERVER-LOGGING-MIXIN) AND):  -
;;; Remove function HTTP::WRITE-ACCESS-LOG-ENTRY-TO-FILE: undefine.
;;; Function HTTP:LOG-ENTRY-WRITER:  -
;;; Function HTTP::CPU-TIME:  -
;;; Function HTTP::ELAPSED-SECONDS:  -
;;; CLOS class HTTP::LOGGING-MIXIN:  -
;;; CLOS class HTTP::SERVER-LOGGING-MIXIN:  -
;;; Written by JCMa, 8/09/00 14:48:33
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.57,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42, HTTP Proxy Server 5.17,
;;; HTTP Client Substrate 3.13, Ivory Revision 5, VLM Debugger 329,
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
  "HTTP:SERVER;LOG.LISP.182"
  "HTTP:SERVER;SERVER.LISP.838"
  "HTTP:SERVER;CLASS.LISP.30")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.182")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defgeneric %file-name-for-log (log)
  (declare (values filename-string))
  (:documentation "Cons the string to use as the filename for LOG's file log."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.182")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod %file-name-for-log ((log basic-log-mixin))
   (with-slots (log-file-name port) log
       (concatenate 'string log-file-name "-" (write-to-string port :base 10.))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.182")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod initialize-log-filename ((log file-logging-mixin))
  (with-slots (name port filename log-file-name) log
    (let* ((pathname (translated-pathname *standard-log-directory*))
	   (name-for-file (%file-name-for-log log)))
      (unless (probe-directory pathname)
	(www-utils:create-directories-recursively pathname))
      (setf filename (www-utils:%make-log-pathname
		       (pathname-device pathname)
		       (pathname-directory pathname) name-for-file (pathname-host pathname)))
      log)))                                    ;must return log 

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.838")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(define-generic write-log-entry-to-file (log agent)
  (:documentation "Writes a log entry to a disk logfile."))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.838")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(SCL:FUNDEFINE 'WRITE-ACCESS-LOG-ENTRY)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.838")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(define-generic write-log-entry (log agent log-stream gmt-p)
  (:documentation "Standard method for writing an entry in a log file for a agent.
This method can be specialized on a log class to write out a different format."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.838")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod write-log-entry ((log common-file-format-mixin) (agent server-logging-mixin) log-stream gmt-p)
  (%server-write-common-logfile-entry agent log-stream gmt-p #\space))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.838")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod write-log-entry ((log extended-common-file-format-mixin) (agent server-logging-mixin) log-stream gmt-p)
  (%server-write-extended-common-logfile-entry agent log-stream gmt-p #\tab))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.838")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod write-log-entry ((log http-post-file-format-mixin) (agent server-logging-mixin) log-stream gmt-p)
  (%server-write-post-logfile-entry agent log-stream gmt-p #\tab))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.838")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod write-access-log-entry-to-file ((log file-logging-mixin) (agent server-logging-mixin))
  ;; This form is executed atomically with interprocess locking.
  ;; When log-file-stream-stays-open is turned on, entries are written to 
  ;; disk only after the stream buffer is filled.
  ;; While this reduces disk access, it can lose up to a buffer's worth of logs
  ;; in the event of a machine crash. FORCE-OUTPUT here would handle this
  (with-slots (log-times-in-gmt-p) log
    (with-log-stream (log)
      (write-log-entry log agent log-stream log-times-in-gmt-p)
      ;; Trailing CR makes this consistently parsable.
      (terpri log-stream))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.30")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defclass logging-mixin
   ()
   ()
   (:documentation "Provides generic methods for logging  server or client activity."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.838")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod write-log-entry-to-file ((log basic-process-queued-file-logging-mixin) (agent logging-mixin))
  (let ((entry-writer (log-entry-writer log agent)))
    (when entry-writer
      (tq:push-task-queue log entry-writer))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.838")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod log-server-access and ((log file-logging-mixin) (server server-logging-mixin))
  (with-slots (file-logging) log
    (when file-logging
      (write-log-entry-to-file log server))
    t))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.838")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(SCL:FUNDEFINE 'WRITE-ACCESS-LOG-ENTRY-TO-FILE)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.838")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(define-generic log-entry-writer (log agent)
  (declare (values closure))
  (:documentation "Returns a closure of one argument that writes the log entry to a stream.
This typically runs after a server or client has been deallocated or recycled on the next connection
and therefore must not share any structure."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.838")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(define-generic cpu-time (server-client-or-log)
  (:documentation "Returns the microseconds that SERVER-CLIENT-OR-LOG has been computing."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.838")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(define-generic elapsed-seconds (server-client-or-log)
  (:documentation "Returns the elapsed seconds that SERVER-CLIENT-OR-LOG has been computing."))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.30")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defclass server-logging-mixin
          (logging-mixin)
    ()
  (:documentation "Records information information necessary for logging server activity."))

