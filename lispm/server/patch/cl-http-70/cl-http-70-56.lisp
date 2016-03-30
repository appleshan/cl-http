;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: task-queue; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.56
;;; Reason: Function (CLOS:METHOD TQ::%TRIM-TASK-THREADS (TQ:MULTI-THREADED-TASK-QUEUE)):  fix array bounds bug.
;;; CLOS class TQ:TASK-THREAD:  add current-task slot.
;;; Function (CLOS:METHOD TQ::TASK-THREAD-EXECUTE-PENDING-TASKS (TQ:TASK-THREAD TQ:MULTI-THREADED-TASK-QUEUE)):  specialize
;;; Function (CLOS:METHOD TQ:TASK-MAIN-LOOP (TQ:TASK-THREAD)):  use it.
;;; Function TQ::TASK-THREAD-EXECUTING-TASK-P:  -
;;; Function (CLOS:METHOD TQ::TASK-THREAD-EXECUTING-TASK-P (TQ:TASK-THREAD)):  -
;;; Function TQ::TASK-QUEUE-EXHAUSTED-P:  -
;;; Function (CLOS:METHOD TQ::TASK-QUEUE-EXHAUSTED-P (TQ::QUEUE-MIXIN)):  -
;;; Function (CLOS:METHOD TQ::TASK-QUEUE-EXHAUSTED-P (TQ:MULTI-THREADED-TASK-QUEUE)):  -
;;; Written by JCMa, 7/13/00 12:07:25
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.55,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42, HTTP Client Substrate 3.13,
;;; HTTP Proxy Server 5.16, HTTP Client 49.8, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, W4 Constraint-Guide Web Walker 43.0,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.11,
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
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).

;;; Patch file for CL-HTTP version 70.56
;;; Written by JCMa, 7/15/00 00:22:10
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.55,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42, HTTP Client Substrate 3.13,
;;; HTTP Proxy Server 5.16, HTTP Client 49.8, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, Ivory Revision 5, VLM Debugger 329,
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
  "HTTP:SERVER;TASK-QUEUE.LISP.41"
  "HTTP:SERVER;TASK-QUEUE.LISP.42"
  "HTTP:SERVER;TASK-QUEUE.LISP.43"
  "HTTP:SERVER;TASK-QUEUE.LISP.44"
  "HTTP:SERVER;TASK-QUEUE.LISP.46"
  "HTTP:SERVER;URL.LISP.398"
  "HTTP:SERVER;TASK-QUEUE.LISP.47")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.41")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

;; Could deallocate threads if they become resourced.
(defmethod %trim-task-threads ((task-queue multi-threaded-task-queue))
  (loop with threads =  (task-queue-threads task-queue)
	for idx fixnum downfrom (1- (the fixnum (fill-pointer threads))) to (task-queue-thread-number task-queue)
	for thread = (aref threads idx)
	do (stop-task thread)
	   (task-process-kill thread)
	   (decf (the fixnum (fill-pointer threads)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.42")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

;;;------------------------------------------------------------------- 
;;;
;;;  MULTITHREADED TASK QUEUES
;;;

(defclass task-thread
	  (background-task)
    ((task-queue :initarg :task-queue :accessor task-thread-task-queue)
     (current-task :initform nil :initarg :current-task :accessor task-thread-current-task))
  (:documentation "A task thread for a multithreaded task queue."))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.42")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod task-main-loop ((thread task-thread))
  (loop with task-queue = (task-thread-task-queue thread)
	and wait-whostate = (task-wait-whostate thread)
	doing (process-wait wait-whostate #'task-thread-pending-tasks-p thread task-queue)
	      (task-thread-execute-pending-tasks thread task-queue)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.43")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defgeneric task-thread-executing-task-p (task-thread)
  (:documentation "Returns non-null when task-thread is executing a task.
This needs to be used in conjunction with a lock on the task queue
to avoid race conditions."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.43")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod task-thread-executing-task-p ((task-thread task-thread))
  (with-slots (current-task) task-thread
    (not (null current-task))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.44")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defgeneric task-queue-exhausted-p (task-queue)
  (:documentation "Returns non-null when all tasks in TASK-QUEUE are completed.
This is the primary method used to determine if a there is more work remaining."))

(export (intern "TASK-QUEUE-EXHAUSTED-P" :tq) :tq)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.46")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod task-queue-exhausted-p ((task-queue multi-threaded-task-queue))
  (unless (task-queue-queue task-queue)
    (with-lock-held ((task-queue-lock task-queue) :read "Check Completion")
      (and (null (task-queue-queue task-queue))	;queue empty
	   (loop for thread across (task-queue-threads task-queue)	;all threads idle
		 never (task-thread-executing-task-p thread))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.46")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod task-queue-exhausted-p ((task-queue queue-mixin))
  (unless (task-queue-queue task-queue)
    (with-lock-held ((task-queue-lock task-queue) :read "Check Completion")
      (null (task-queue-queue task-queue)))))	;queue empty 


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.398")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(initialize-http-scheme-parser)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.47")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

;; Duplicates some code as the single thread version. Consider further abstraction 7/13/2000 -- JCMa.
(defmethod task-thread-execute-pending-tasks ((thread task-thread) (task-queue multi-threaded-task-queue))
  (loop	with task
	do (unwind-protect
	       ;; Careful not to lose entries when error occur executing tasks
	       (cond ((setq task (pop-task-queue task-queue))
		      (setf (task-thread-current-task thread) task)	;keep track of current task
		      (task-queue-execute-task task-queue task)
		      (setf (task-thread-current-task thread) nil)
		      (setq task nil))
		     (t (return-from task-thread-execute-pending-tasks nil)))
	     (when task
	       (setf (task-thread-current-task thread) nil)	;clear current task
	       (task-queue-push-front task-queue task)))
	while (task-run-p task-queue)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.47")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod task-queue-execute-pending-tasks ((task-queue queue-mixin))
  (loop	with task
	do (unwind-protect
	       ;; Careful not to lose entries when error occur executing tasks
	       (cond ((setq task (pop-task-queue task-queue))
		      (task-queue-execute-task task-queue task)
		      (setq task nil))
		     (t (return-from task-queue-execute-pending-tasks nil)))
	     (when task
	       (task-queue-push-front task-queue task)))
	while (task-run-p task-queue))) 

