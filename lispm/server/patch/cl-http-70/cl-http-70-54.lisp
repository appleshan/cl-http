;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Base: 10; Package: task-queue; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.54
;;; Reason: Tune up multi-threaded tasks.
;;; 
;;; CLOS class TQ::TASK-RUNNABLE-MIXIN:  -
;;; CLOS class TQ:BACKGROUND-TASK:  -
;;; CLOS class TQ:MULTI-THREADED-TASK-QUEUE:  -
;;; Function (CLOS:METHOD TQ::%FILL-TASK-THREADS (TQ:MULTI-THREADED-TASK-QUEUE)):  -
;;; Function (CLOS:METHOD TQ:START-TASK (TQ:MULTI-THREADED-TASK-QUEUE)):  -
;;; Function (CLOS:METHOD TQ:STOP-TASK (TQ:MULTI-THREADED-TASK-QUEUE)):  -
;;; Function (CLOS:METHOD CLOS:INITIALIZE-INSTANCE (TQ::QUEUE-MIXIN) :AFTER):  -
;;; Written by JCMa, 6/23/00 23:33:29
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.6, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.53,
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



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;TASK-QUEUE.LISP.37"
  "HTTP:SERVER;TASK-QUEUE.LISP.38"
  "HTTP:SERVER;TASK-QUEUE.LISP.39")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.37")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

;;;------------------------------------------------------------------- 
;;;
;;; BACKGROUND TASKS
;;;

(defclass task-runnable-mixin
	  ()
    ((run-p :initform t :accessor task-run-p)))	;return non-null when the process should run.


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.37")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defclass background-task
          (task-runnable-mixin)
    ((process-priority :initform 0 :initarg :process-priority :accessor task-process-priority)
     (process :initform nil :accessor task-process)	;process running the task
     (process-name :initform "Background Task" :initarg :process-name :accessor task-process-name)
     (wait-whostate :initform "Task Wait" :initarg :wait-whostate :accessor task-wait-whostate))
  (:documentation "A mixin that executes task entries with a separate process."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.37")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defclass multi-threaded-task-queue
	  (task-runnable-mixin queue-mixin)
    ((threads :initform nil :initarg :threads :accessor task-queue-threads)
     (thread-number :initform 1 :initarg :thread-number :accessor task-queue-thread-number)
     (process-priority :initform 0 :initarg :process-priority :accessor task-queue-process-priority)
     (thread-name :initform "Task-Queue-Thread" :initarg :thread-name :reader task-queue-thread-name
		  :reader task-process-name :allocation :class)
     (task-thread-class :initform 'task-thread :reader task-queue-task-thread-class :allocation :class))
  (:documentation "A multithreaded version of task-queue.")) 


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.38")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod %fill-task-threads ((task-queue multi-threaded-task-queue))
  (let ((threads (task-queue-threads task-queue)))
    (unless threads
      (setq threads (make-array (task-queue-thread-number task-queue) :adjustable t :fill-pointer 0))
      (setf (task-queue-threads task-queue) threads))
    (loop for idx fixnum upfrom (fill-pointer threads) below (task-queue-thread-number task-queue)
	  for process-name = (make-task-thread-name task-queue idx)
	  for process-priority = (task-queue-process-priority task-queue)
	  for thread = (make-instance (task-queue-task-thread-class task-queue) :task-queue task-queue
				      :process-name process-name :process-priority process-priority)
	  do (vector-push-extend thread threads)))
  task-queue) 


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.38")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod start-task ((task-queue multi-threaded-task-queue))
  ;; create any new ones to bring the total up to thread-number
  (%fill-task-threads task-queue)
  ;; start up existing threads
  (%activate-task-threads task-queue)
  (setf (task-run-p task-queue) t)
  task-queue) 


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.38")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod stop-task ((task-queue multi-threaded-task-queue))
  (setf (task-run-p task-queue) nil)
  (map-task-threads task-queue #'stop-task)
  task-queue) 


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.39")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(mapc #'(lambda (x) (export (intern x :tq) :tq))
      '("TASK-QUEUE-THREAD-NUMBER" "TASK-QUEUE-PENDING-TASKS" "TASK-QUEUE-PENDING-TASKS-P"))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.39")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod initialize-instance :after ((task-queue queue-mixin) &key)
  (setf (task-queue-lock task-queue) (make-lock (task-process-name task-queue) :type :simple))
  task-queue)

