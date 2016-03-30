;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: task-queue; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.51
;;; Reason: Patch in Multi-Threaded Task Queue.
;;; 
;;; Function TQ::PUSH-ORDERED: Duplicated due  to compile order dependencies.
;;; CLOS class TQ:BACKGROUND-TASK:  -
;;; CLOS class TQ::QUEUE-MIXIN:  -
;;; CLOS class TQ:TASK-QUEUE:  -
;;; Function (CLOS:METHOD TQ:PUSH-TASK-QUEUE (TQ::QUEUE-MIXIN T)):  -
;;; Function (CLOS:METHOD TQ:TASK-QUEUE-PUSH-FRONT (TQ::QUEUE-MIXIN T)):  -
;;; Function TQ:TASK-QUEUE-PUSH-ORDERED:  -
;;; Function (CLOS:METHOD TQ:TASK-QUEUE-PUSH-ORDERED (TQ::QUEUE-MIXIN T T)):  -
;;; Function (CLOS:METHOD TQ:POP-TASK-QUEUE (TQ::QUEUE-MIXIN)):  -
;;; Function (CLOS:METHOD TQ:TASK-QUEUE-NEXT (TQ::QUEUE-MIXIN)):  -
;;; Function (CLOS:METHOD TQ:CLEAR-TASK-QUEUE (TQ::QUEUE-MIXIN)):  -
;;; Function (CLOS:METHOD TQ:TASK-QUEUE-MAP-ENTRIES (TQ::QUEUE-MIXIN T)):  -
;;; Function TQ:TASK-QUEUE-MAP-ENTRIES:  -
;;; Function (CLOS:METHOD TQ:TASK-QUEUE-EXECUTE-TASK (TQ::QUEUE-MIXIN T)):  -
;;; Function (CLOS:METHOD TQ:TASK-QUEUE-EXECUTE-PENDING-TASKS (TQ::QUEUE-MIXIN)):  -
;;; Written by JCMa, 6/16/00 00:21:53
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.6, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.50,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42, HTTP Client Substrate 3.12,
;;; HTTP Proxy Server 5.15, HTTP Client 49.8, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, W4 Constraint-Guide Web Walker 41.5,
;;; W4 Examples 13.0, Ivory Revision 5, VLM Debugger 329, Genera program 8.11,
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
  "HTTP:SERVER;TASK-QUEUE.LISP.33")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.33")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(mapc #'(lambda (x) (export (intern x :tq) :tq))
      '("MULTI-THREADED-TASK-QUEUE" "TASK-QUEUE-MAP-ENTRIES" "TASK-THREAD" "TASK-QUEUE-PUSH-ORDERED"))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.33")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

;;;------------------------------------------------------------------- 
;;;
;;; UTILITIES 
;;;

;; Duplicated from http:server;utils.lisp due  to compile order dependencies. -- JCMa 6/4/2000.
(defmacro push-ordered (item place predicate &key key) 
  `(let ((item-key ,(if key `(funcall ,key ,item) item)))
     (cond ((or (null ,place)
		(funcall ,predicate item-key ,(if key `(funcall ,key (first ,place)) `(first ,place))))
	    (push ,item ,place))
	   (t (loop for list = ,place then (cdr list)
		    for next = (cdr list)
		    while next
		    when (funcall ,predicate item-key ,(if key `(funcall ,key (car next)) '(car next)))
		      do (push ,item (cdr list))
			 (return )
		    finally (nconc list (list ,item)))
	      ,place))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.33")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

;;;------------------------------------------------------------------- 
;;;
;;; BACKGROUND TASKS
;;;

(defclass background-task
          ()
    ((run-p :initform t :accessor task-run-p)	;return non-null when the process should run.
     (process-priority :initform 0 :initarg :process-priority :accessor task-process-priority)
     (process :initform nil :accessor task-process)	;process running the task
     (process-name :initform "Background Task" :initarg :process-name :accessor task-process-name)
     (wait-whostate :initform "Task Wait" :initarg :wait-whostate :accessor task-wait-whostate))
  (:documentation "A mixin that executes task entries with a separate process."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.33")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

;;;------------------------------------------------------------------- 
;;;
;;; TASK QUEUE
;;;

(defclass queue-mixin
	  ()
    ((queue :initform nil :accessor task-queue-queue)   ;queue containing the entries
     (pointer :initform nil :accessor task-queue-pointer)       ;pointer to the last cons in the queue
     (n-pending-tasks :initform 0 :accessor task-queue-pending-tasks)   ;number of incomplete tasks
     (lock :initform nil :initarg :lock :accessor task-queue-lock))
  (:documentation "A mixin that provides a queue capability."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.33")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defclass task-queue
	  (queue-mixin background-task)
    ((process-name :initform "Task Queue" :initarg :process-name :accessor task-queue-process-name)
     (wait-whostate :initform "Task Wait" :initarg :wait-whostate :accessor task-queue-wait-whostate))
  (:documentation "A mixin that executes task entries with a separate process."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.33")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod push-task-queue ((task-queue queue-mixin) task)
  (with-slots (lock queue pointer n-pending-tasks) task-queue 
    (let ((entry (list task)))
      (with-lock-held (lock :write "Task Queue Push")
        (if queue
            (setf (cdr pointer) entry)
            (setq queue entry))
        (setq pointer entry)
        (incf (the fixnum n-pending-tasks))))))

(scl:fundefine '(method push-task-queue (task-queue t)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.33")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod task-queue-push-front ((task-queue queue-mixin) task)
  (with-slots (lock queue pointer n-pending-tasks) task-queue 
    (with-lock-held (lock :write "Task Queue Push")
      (cond (queue
	     (push task queue))
	    (t (let ((entry (list task)))
		 (setf queue entry
		       pointer entry))))
      (incf (the fixnum n-pending-tasks)))))

(scl:fundefine '(method task-queue-push-front (task-queue t)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.33")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defgeneric task-queue-push-ordered (task-queue task predicate)
  (:documentation "Push an entry onto TASK-QUEUE by inserting TASK according to PREDICATE
with inter-process locking."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.33")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

;; This may need to become more clever to handle long queues. -- JCMa 6/3/2000.
(defmethod task-queue-push-ordered ((task-queue queue-mixin) task predicate)
  (with-slots (lock queue pointer n-pending-tasks) task-queue 
    (with-lock-held (lock :write "Task Queue Push")
      (cond (queue
	     (push-ordered  task queue predicate)
	     (when (cdr pointer)		; keep the pointer to end correct
	       (setf pointer (cdr pointer))))
	    (t (let ((entry (list task)))
		 (setf queue entry
		       pointer entry))))
      (incf (the fixnum n-pending-tasks)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.33")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod pop-task-queue ((task-queue queue-mixin) &aux entry)
  (with-slots (lock queue pointer n-pending-tasks) task-queue
    (with-lock-held (lock :write "Task Queue Pop")
      (when (setq entry (pop queue))
	(decf (the fixnum n-pending-tasks))
	(unless queue
	  (setq pointer nil))))
    entry))

(scl:fundefine '(method pop-task-queue (task-queue t)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.33")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod task-queue-next ((task-queue queue-mixin))
  (with-lock-held ((task-queue-lock task-queue) :read "Task Queue Next")
    (car (task-queue-queue task-queue))))

(scl:fundefine '(method task-queue-next (task-queue t)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.33")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod clear-task-queue ((task-queue queue-mixin))
  (with-slots (lock queue pointer n-pending-tasks) task-queue
    (with-lock-held (lock :write "Task Queue Clear")
      (prog1 queue
             (setq queue nil
                   pointer nil
                   n-pending-tasks 0)))))

(scl:fundefine '(method clear-task-queue (task-queue t)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.33")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod task-queue-map-entries ((task-queue queue-mixin) function &optional (lock-mode :no-lock))
  (ecase lock-mode
    (:no-lock
      (mapc function (task-queue-queue task-queue)))
    ((:read :write)
     (with-lock-held ((task-queue-lock task-queue) lock-mode "Map Task Queue")
       (mapc function (task-queue-queue task-queue))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.33")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defgeneric task-queue-map-entries (task-queue function &optional LOCK-MODE)
  (:documentation "Maps FUNCTION over the entries in task-queue grabbing lock according to lock-mode,
which can be one of :NO-LOCK, :READ or :WRITE"))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.33")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod task-queue-execute-task ((task-queue queue-mixin) task)
  (funcall task task-queue))

(scl:fundefine '(method task-queue-execute-task (task-queue t)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.33")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod task-queue-execute-pending-tasks ((task-queue queue-mixin))
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

(scl:fundefine '(method task-queue-execute-pending-tasks (task-queue t)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.33")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(PROGN
;;;------------------------------------------------------------------- 
;;;
;;;  MULTITHREADED TASK QUEUES
;;;

(defclass task-thread
	  (background-task)
    ((task-queue :initarg :task-queue :accessor task-thread-task-queue))
  (:documentation "A task thread for a multithreaded task queue."))

(defclass multi-threaded-task-queue
	  (queue-mixin)
    ((threads :initform nil :initarg :threads :accessor task-queue-threads)
     (thread-number :initform 1 :initarg :thread-number :accessor task-queue-thread-number)
     (process-priority :initform 0 :initarg :process-priority :accessor task-queue-process-priority)
     (thread-name :initform "Task-Queue-Thread" :initarg :thread-name :reader task-queue-thread-name :allocation :class)
     (task-thread-class :initform 'task-thread :reader task-queue-task-thread-class :allocation :class))
  (:documentation "A multithreaded version of task-queue.")) 

(defmethod task-queue-pending-tasks-p ((task-queue multi-threaded-task-queue))
  (not (null (task-queue-queue task-queue))))

(defmethod task-thread-pending-tasks-p ((task-thread task-thread) (task-queue multi-threaded-task-queue))
  (with-slots (run-p) task-thread
    (with-slots (queue ) task-queue
      (and queue run-p))))

(defmethod task-main-loop ((task task-thread))
  (loop with task-queue = (task-thread-task-queue task)
	and wait-whostate = (task-wait-whostate task)
	doing (process-wait wait-whostate #'task-thread-pending-tasks-p task task-queue)
	      (task-queue-execute-pending-tasks task-queue))) 

(defgeneric nth-task-thread (multi-threaded-task-queue index)
  (:documentation "Returns the nth task thread  at INDEX associated with MULTI-THREADED-TASK-QUEUE."))

(defmethod nth-task-thread ((task-queue multi-threaded-task-queue) index)
  (let ((threads (task-queue-threads task-queue))) 
    (aref threads index)))

(defgeneric map-task-threads (multi-threaded-task-queue function)
  (:documentation "Maps FUNCTION over all task threads of MULTI-THREADED-TASK-QUEUE."))

(defmethod map-task-threads ((task-queue multi-threaded-task-queue) function)
  (loop for thread across (task-queue-threads task-queue)
	do (funcall function thread))) 

(defgeneric %activate-task-threads (multi-threaded-task-queue)
  (:documentation "Ensures that all task threads are running."))

(defmethod %activate-task-threads ((task-queue multi-threaded-task-queue))
  (flet ((activate-thread (thread)
	   (unless (task-active-p  thread)
	     (start-task thread))))
    (map-task-threads task-queue #'activate-thread)
    task-queue))

(defgeneric make-task-thread-name (multi-threaded-task-queue index)
  (:documentation "Returns a name for a task thread stored under INDEX.
The name should be suitable for use as a process name."))

(defmethod make-task-thread-name ((task-queue multi-threaded-task-queue) index)
  (declare (fixnum index))
  (format nil "~A-~D" (task-queue-thread-name task-queue) (1+ index)))

(defgeneric %fill-task-threads (multi-threaded-task-queue)
  (:documentation "Creates task threads up to the specified number of task threads."))

(defmethod %fill-task-threads ((task-queue multi-threaded-task-queue))
  (let ((threads (task-queue-threads task-queue)))
    (unless threads
      (setq threads (make-array (task-queue-thread-number task-queue) :adjustable t :fill-pointer t))
      (setf (task-queue-threads task-queue) threads))
    (loop for idx fixnum upfrom (fill-pointer threads) below (task-queue-thread-number task-queue)
	  for process-name = (make-task-thread-name task-queue idx)
	  for process-priority = (task-queue-process-priority task-queue)
	  for thread = (make-instance (task-queue-task-thread-class task-queue) :task-queue task-queue
				      :process-name process-name :process-priority process-priority)
	  do (vector-push-extend thread threads)))
  task-queue) 

(defgeneric %trim-task-threads (multi-threaded-task-queue)
  (:documentation "Reduces the number of actual task threads down to the specified number of task threads."))

;; Could deallocate threads if they become resourced.
(defmethod %trim-task-threads ((task-queue multi-threaded-task-queue))
  (loop with threads =  (task-queue-threads task-queue)
	for idx fixnum downfrom (fill-pointer threads) to (task-queue-thread-number task-queue)
	for thread = (aref threads idx)
	do (stop-task thread)
	   (task-process-kill thread)
	   (decf (fill-pointer threads))))

(defmethod start-task ((task-queue multi-threaded-task-queue))
  ;; create any new ones to bring the total up to thread-number
  (%fill-task-threads task-queue)
  ;; start up existing threads
  (%activate-task-threads task-queue)
  task-queue) 

;; Make sure the running task threads always correspond to the specified number.
(defmethod (setf task-queue-thread-number) :around (integer (task-queue multi-threaded-task-queue))
  (let ((delta (- integer (task-queue-thread-number task-queue))))
    (prog1 (call-next-method)
	   (cond ((zerop delta))
		 ((plusp delta) 
		  (%fill-task-threads task-queue)
		  (when (task-active-p (nth-task-thread task-queue 0))
		    (%activate-task-threads task-queue)))
		 (t (%trim-task-threads task-queue)))))) 

(defmethod (setf task-queue-process-priority) :around (priority (task-queue multi-threaded-task-queue))
  (flet ((set-thread-priority (thread)
	   (setf (task-process-priority thread) priority)))
    (declare (dynamic-extent #'set-thread-priority))
    (prog1 (call-next-method)
	   (map-task-threads task-queue #'set-thread-priority)))) 

(defmethod task-process-kill ((task-queue multi-threaded-task-queue))
  (map-task-threads task-queue #'task-process-kill)
  task-queue) 

(defmethod stop-task-queue ((task-queue multi-threaded-task-queue))
  (map-task-threads task-queue #'stop-task)
  task-queue) 

(defmethod ensure-active-task ((task-queue multi-threaded-task-queue))
  (start-task task-queue))

)
