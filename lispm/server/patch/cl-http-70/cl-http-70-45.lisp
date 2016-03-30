;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: task-queue; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.45
;;; Reason: Abstract BACKGROUND-TASK out of TASK-QUEUE and remodularize code.
;;; This patch should only be loaded when freshly booting a world.
;;; 
;;; CLOS class TQ:TASK-QUEUE:  build on the background task.
;;; Function (CLOS:METHOD (CL:SETF TQ::TASK-QUEUE-PROCESS-PRIORITY) (LISP:INTEGER TQ:TASK-QUEUE) :AFTER):  -
;;; Function (CLOS:METHOD TQ:START-TASK-QUEUE (TQ:TASK-QUEUE)):  -
;;; Function (CLOS:METHOD TQ:TASK-QUEUE-PROCESS-KILL (TQ:TASK-QUEUE)):  -
;;; Function (CLOS:METHOD TQ:ENSURE-ACTIVE-TASK-QUEUE (TQ:TASK-QUEUE)):  -
;;; Function (CLOS:METHOD TQ:ENSURE-ACTIVE-TASK-QUEUE (SYMBOL)):  -
;;; Remove function (CLOS:METHOD TQ:STOP-TASK-QUEUE (TQ:TASK-QUEUE) :AROUND): -
;;; Function TQ::SHUTTING-TASK-DOWN:  -
;;; Function (CLOS:METHOD TQ:START-TASK-QUEUE (TQ:TASK-QUEUE)):  -
;;; Function (CLOS:METHOD TQ:STOP-TASK-QUEUE (TQ:TASK-QUEUE) :AROUND):  -
;;; Function (CLOS:METHOD TQ:TASK-QUEUE-PROCESS-NAME (TQ:TASK-QUEUE)):  -
;;; Function (CLOS:METHOD TQ:TASK-QUEUE-RUN-P (TQ:TASK-QUEUE)):  -
;;; Function (CLOS:METHOD TQ:TASK-QUEUE-WAIT-WHOSTATE (TQ:TASK-QUEUE)):  -
;;; Function (CLOS:METHOD TQ:TASK-QUEUE-EXECUTE-PENDING-TASKS (TQ:TASK-QUEUE)):  -
;;; Function (CLOS:METHOD TQ::TASK-MAIN-LOOP (TQ:TASK-QUEUE)):  -
;;; Written by JCMa, 5/13/00 20:31:38
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.44,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42, HTTP Proxy Server 5.7,
;;; HTTP Client Substrate 3.10, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.11, DEC OSF/1 V4.0 (Rev. 110),
;;; 1024x718 24-bit TRUE-COLOR X Screen JCMA-ISDN:0.0 with 224 Genera fonts (eXodusPowerPC 7.0  (c) 1998 White Pine Software,
;;; Inc. R6300),
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

;;; Patch file for CL-HTTP version 70.45
;;; Written by JCMa, 5/14/00 18:27:20
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.44,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42, Ivory Revision 5,
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
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48).




(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;TASK-QUEUE.LISP.31")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.31")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (mapc #'(lambda (x) (import (intern x :www-utils) :task-queue))
	'("CURRENT-PROCESS" "PROCESS-RUN-FUNCTION")))_


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.31")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(PROGN
(defclass background-task
          ()
    ((run-p :initform t :accessor task-run-p)     ;return non-null when the process should run.
     (process-priority :initform 0 :initarg :process-priority :accessor task-process-priority)
     (process :initform nil :accessor task-process)       ;process running the task
     (process-name :initform "Background Task" :initarg :process-name :accessor task-process-name)
     (wait-whostate :initform "Task Wait" :initarg :wait-whostate :accessor task-wait-whostate))
  (:documentation "A mixin that executes task entries with a separate process."))

(defmethod print-object ((task background-task) stream)
  (print-unreadable-object (task stream :type t :identity t)
    (write-string (task-process-name task) stream)))

(defmethod (setf task-process-priority ) :after ((process-priority integer) (task background-task))
  (with-slots (process) task
    (when process
      (setf (process-priority process) process-priority))))

(defgeneric task-work-p (task)
  (:documentation "Returns non-null when there is work for the task and the process should become active.
Specialize this method to serve as the wait function for the task main loop."))

(defmethod task-work-p ((task background-task))
  (with-slots (run-p) task
    run-p))

(defgeneric task-waiting-p (task)
  (:documentation "Returns non-null if TASK is in a wait state rather than executing."))

(defmethod task-waiting-p ((task background-task))
  (with-slots (process wait-whostate) task
    (equalp (process-whostate process) wait-whostate)))

(defgeneric task-execute (task)
  (:documentation "Specialize this method as the main execution routine for the task."))

(defgeneric task-main-loop (task)
  (:documentation "The main execution loop.
Waits until tasks are TASK-WORK-P returns non-null and then executes TASK-EXECUTE."))

(defmethod task-main-loop ((task background-task))
  (loop with wait-whostate = (task-wait-whostate task)
        doing (process-wait wait-whostate #'task-work-p task)
              (task-execute task)))

(defgeneric start-task (task)
  (:documentation "Starts TASK executing by activing its process."))

(defmethod start-task ((task background-task))
  (with-slots (process process-name) task
    (cond (process
           (process-preset process #'task-main-loop task)
           (process-enable process))
          (t (setq process (make-process process-name
					 :background-p t
                                         :priority (task-process-priority task)
                                         :restart-after-reset t
                                         :warm-boot-action :delayed-restart))
             (process-preset process #'task-main-loop task)
             (process-enable process)))
    (setf (task-run-p task) t)
    process))

(defgeneric task-process-kill (task)
  (:documentation "Stops the task task process and kills it."))

(defmethod task-process-kill ((task background-task))
  (with-slots (process) task
    (when process
      (stop-task task)
      (prog1 (process-kill process)
             (setq process nil)))))

(defgeneric stop-task (task)
  (:documentation "Stops task task from executing task by stopping its process."))

;; specialize this method to perform clean up activity.
(defmethod stop-task ((task background-task))
  (declare (values task)))

(declaim (inline %task-active-p))

(defgeneric task-active-p (task)
  (:documentation "Returns non-null when the TASK is active."))

(defgeneric ensure-active-task (task)
  (declare (values task))
  (:documentation "Ensures that TASK is active and executing."))

(defmethod ensure-active-task ((task background-task))
  (prog1 task
         (or (task-active-p task)
             (start-task task))))

(defmethod ensure-active-task ((task-class symbol))
  (ensure-active-task (make-instance task-class)))

)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.31")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

;;;------------------------------------------------------------------- 
;;;
;;; TASK QUEUE
;;;

(defclass task-queue
          (background-task)
    ((queue :initform nil :accessor task-queue-queue)   ;queue containing the entries
     (pointer :initform nil :accessor task-queue-pointer)       ;pointer to the last cons in the queue
     (n-pending-tasks :initform 0 :accessor task-queue-pending-tasks)   ;number of incomplete tasks
     (lock :initform nil :initarg :lock :accessor task-queue-lock)
     (process-name :initform "Task Queue" :initarg :process-name :accessor task-queue-process-name)
     (wait-whostate :initform "Task Wait" :initarg :wait-whostate :accessor task-queue-wait-whostate))
  (:documentation "A mixin that executes task entries with a separate process."))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.31")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

;; Obsolete backward compatibility 5/13/2000 -- JCMa.
(defmethod (setf task-queue-process-priority) :after ((process-priority integer) (task-queue task-queue))
  (setf (task-process-priority task-queue) process-priority))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.31")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod task-queue-process-kill ((task-queue task-queue))
  (task-process-kill task-queue))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.31")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod ensure-active-task-queue ((task-queue task-queue))
  (ensure-active-task task-queue))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.31")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod ensure-active-task-queue ((task-queue-class symbol))
  (ensure-active-task task-queue-class))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.31")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defun %task-active-p (task)
  (let ((process (task-process task)))
    (and process (process-active-p process))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.31")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod task-active-p ((task background-task))
  (%task-active-p task))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.31")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(SCL:FUNDEFINE '(METHOD STOP-TASK-QUEUE (TASK-QUEUE) :AROUND))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.31")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod start-task-queue ((task-queue task-queue))
  (start-task task-queue))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.31")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmacro shutting-task-down ((task) &body body)
  `(with-slots (process) ,task
     (when process
       (let ((in-task-process-p (eq process (current-process))))
	 ;; set a flag tell the process to shutdown
	 (setf (task-run-p ,task) nil)
	 ;; wait until the shutdown is complete
	 (unless in-task-process-p 
	   (process-wait "Task Shutdown" #'task-waiting-p ,task))
	 (progn . ,body)
	 ;; disable the processes run reasons
	 (if in-task-process-p
	     (process-run-function "Task Shutdown" #'process-disable process)
	     (process-disable process))
	 process))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.31")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod stop-task :around ((task background-task))
  (shutting-task-down (task)
    (call-next-method)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.31")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod stop-task-queue :around ((task-queue task-queue))
  (shutting-task-down (task-queue)
    (call-next-method)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.31")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

;; Backward compatibility
(defmethod task-queue-run-p ((task-queue task-queue))
  (task-run-p task-queue))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.31")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod task-queue-execute-pending-tasks ((task-queue task-queue))
  (loop for task = (pop-task-queue task-queue)
        while task
	;; Careful not to lose entries when error occur executing tasks
        do (unwind-protect
	       (progn 
		 (task-queue-execute-task task-queue task)
		 (setq task nil))
	     (when task
	       (task-queue-push-front task-queue task)))
        while (task-run-p task-queue)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.31")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod task-main-loop ((task-queue task-queue))
  (loop with wait-whostate = (task-wait-whostate task-queue)
        doing (process-wait wait-whostate #'task-queue-pending-tasks-p task-queue)
              (task-queue-execute-pending-tasks task-queue)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.31")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(mapc #'(lambda (x)
	  (export (intern x :tq) :tq))
      `("BACKGROUND-TASK" "ENSURE-ACTIVE-TASK" "START-TASK" "STOP-TASK" "TASK-EXECUTE"
	"TASK-MAIN-LOOP" "TASK-PROCESS-KILL" "TASK-PROCESS-PRIORITY" "TASK-WORK-P"))
