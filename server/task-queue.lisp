;;; -*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; (c) Copyright  1997-2000, John C. Mallery
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; GENERIC TASK QUEUE FACILITY
;;;
;;; Branched off cml:cml-utils;task-queue.lisp.7 8/25/97 -- JCMa.

(defpackage task-queue
  (:nicknames tq)
  (:use future-common-lisp)
  (:import-from "WWW-UTILS"
                "CURRENT-PROCESS"
                "MAKE-LOCK"
                "MAKE-PROCESS"
                "PROCESS-ACTIVE-P"
                "PROCESS-DISABLE"
                "PROCESS-ENABLE"
                "PROCESS-KILL"
                "PROCESS-PRESET"
                "PROCESS-PRIORITY"
                "PROCESS-RUN-FUNCTION"
                "PROCESS-WAIT"
                "PROCESS-WHOSTATE"
                "WITH-LOCK-HELD")
  #+(and :mit-values)(:shadowing-import-from :mit-values "VALUES")
  (:export
   "BACKGROUND-TASK"
   "CLEAR-TASK-QUEUE"
   "ENSURE-ACTIVE-TASK" 
   "ENSURE-ACTIVE-TASK-QUEUE"
   "MULTI-THREADED-TASK-QUEUE"
   "POP-TASK-QUEUE"
   "PUSH-TASK-QUEUE" 
   "START-TASK"
   "START-TASK-QUEUE"
   "STOP-TASK"
   "STOP-TASK-QUEUE"
   "TASK-EXECUTE"
   "TASK-MAIN-LOOP"
   "TASK-PROCESS-KILL"
   "TASK-PROCESS-PRIORITY"
   "TASK-QUEUE"
   "TASK-QUEUE-EXECUTE-PENDING-TASKS"
   "TASK-QUEUE-EXECUTE-TASK"
   "TASK-QUEUE-EXHAUSTED-P"
   "TASK-QUEUE-MAP-ENTRIES"
   "TASK-QUEUE-NEXT"
   "TASK-QUEUE-PENDING-TASKS"
   "TASK-QUEUE-PENDING-TASKS-P"
   "TASK-QUEUE-PROCESS-KILL"
   "TASK-QUEUE-PROCESS-NAME"
   "TASK-QUEUE-PUSH-FRONT"
   "TASK-QUEUE-PUSH-ORDERED"
   "TASK-QUEUE-RUN-P"
   "TASK-QUEUE-THREAD-NUMBER"
   "TASK-QUEUE-WAIT-WHOSTATE"
   "TASK-THREAD"
   "TASK-WORK-P"))

(in-package :task-queue) 


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

;;;------------------------------------------------------------------- 
;;;
;;; BACKGROUND TASKS
;;;

(defclass task-runnable-mixin
	  ()
    ((run-p :initform t :accessor task-run-p)))	;return non-null when the process should run.

(defclass background-task
          (task-runnable-mixin)
    ((process-priority :initform 0 :initarg :process-priority :accessor task-process-priority)
     (process :initform nil :accessor task-process)	;process running the task
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

(defmethod stop-task :around ((task background-task))
  (shutting-task-down (task)
    (call-next-method)))

(declaim (inline %task-active-p))

(defun %task-active-p (task)
  (let ((process (task-process task)))
    (and process (process-active-p process))))

(defgeneric task-active-p (task)
  (:documentation "Returns non-null when the TASK is active."))

(defmethod task-active-p ((task background-task))
  (%task-active-p task))

(defgeneric ensure-active-task (task)
  #-(or clisp ecl) (declare (values task))
  (:documentation "Ensures that TASK is active and executing."))

(defmethod ensure-active-task ((task background-task))
  (prog1 task
         (or (task-active-p task)
             (start-task task))))

(defmethod ensure-active-task ((task-class symbol))
  (ensure-active-task (make-instance task-class))) 

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

(defclass task-queue
	  (queue-mixin background-task)
    ((process-name :initform "Task Queue" :initarg :process-name :accessor task-queue-process-name)
     (wait-whostate :initform "Task Wait" :initarg :wait-whostate :accessor task-queue-wait-whostate))
  (:documentation "A mixin that executes task entries with a separate process."))

(defmethod initialize-instance :after ((task-queue queue-mixin) &key)
  (setf (task-queue-lock task-queue) (make-lock (task-process-name task-queue) :type :simple))
  task-queue)

;; Obsolete backward compatibility 5/13/2000 -- JCMa.
(defmethod (setf task-queue-process-priority) :after ((process-priority integer) (task-queue task-queue))
  (setf (task-process-priority task-queue) process-priority))

(defgeneric push-task-queue (task-queue task)
  (:documentation "Push an entry onto task-queue with inter-process locking."))

(defmethod push-task-queue ((task-queue queue-mixin) task)
  (with-slots (lock queue pointer n-pending-tasks) task-queue 
    (let ((entry (list task)))
      (with-lock-held (lock :write "Task Queue Push")
        (if queue
            (setf (cdr pointer) entry)
            (setq queue entry))
        (setq pointer entry)
        (incf (the fixnum n-pending-tasks))))))

(defgeneric task-queue-push-front (task-queue task)
  (:documentation "Push an entry onto TASK-QUEUE with inter-process locking."))

(defmethod task-queue-push-front ((task-queue queue-mixin) task)
  (with-slots (lock queue pointer n-pending-tasks) task-queue 
    (with-lock-held (lock :write "Task Queue Push")
      (cond (queue
	     (push task queue))
	    (t (let ((entry (list task)))
		 (setf queue entry
		       pointer entry))))
      (incf (the fixnum n-pending-tasks)))))

(defgeneric task-queue-push-ordered (task-queue task predicate)
  (:documentation "Push an entry onto TASK-QUEUE by inserting TASK according to PREDICATE
with inter-process locking."))

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

(defgeneric pop-task-queue (task-queue)
  #-(or clisp ecl) (declare (values first-entry))
  (:documentation "Pops an entry off the queue with inter-process locking."))

(defmethod pop-task-queue ((task-queue queue-mixin) &aux entry)
  (with-slots (lock queue pointer n-pending-tasks) task-queue
    (with-lock-held (lock :write "Task Queue Pop")
      (when (setq entry (pop queue))
	(decf (the fixnum n-pending-tasks))
	(unless queue
	  (setq pointer nil))))
    entry))

(defgeneric task-queue-next (task-queue)
  #-(or clisp ecl) (declare (values first-entry))
  (:documentation "Gets the first task without removing it from the queue  with inter-process locking."))

(defmethod task-queue-next ((task-queue queue-mixin))
  (with-lock-held ((task-queue-lock task-queue) :read "Task Queue Next")
    (car (task-queue-queue task-queue))))

(defgeneric clear-task-queue (task-queue)
  #-(or clisp ecl) (declare (values flushed-queue))
  (:documentation "Clears all entries from the task queue with inter-process locking."))

(defmethod clear-task-queue ((task-queue queue-mixin))
  (with-slots (lock queue pointer n-pending-tasks) task-queue
    (with-lock-held (lock :write "Task Queue Clear")
      (prog1 queue
             (setq queue nil
                   pointer nil
                   n-pending-tasks 0)))))

(defgeneric task-queue-map-entries (task-queue function &optional LOCK-MODE)
  (:documentation "Maps FUNCTION over the entries in task-queue grabbing lock according to lock-mode,
which can be one of :NO-LOCK, :READ or :WRITE"))

(defmethod task-queue-map-entries ((task-queue queue-mixin) function &optional (lock-mode :no-lock))
  (ecase lock-mode
    (:no-lock
      (mapc function (task-queue-queue task-queue)))
    ((:read :write)
     (with-lock-held ((task-queue-lock task-queue) lock-mode "Map Task Queue")
       (mapc function (task-queue-queue task-queue))))))

(defgeneric task-queue-pending-tasks-p (task-queue)
  (:documentation "Returns non-null when there are pending tasks and the process is active."))

(defmethod task-queue-pending-tasks-p ((task-queue task-queue))
  (declare (optimize (speed 3)))
  (with-slots (queue run-p) task-queue
    (and queue run-p)))

(defgeneric task-queue-execute-task (task-queue task)
  (:documentation "Execute a single task. Specialize this as needed for applications."))

(defmethod task-queue-execute-task ((task-queue queue-mixin) task)
  (funcall task task-queue))

;; See also task-thread-execute-pending-tasks
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

(defmethod task-main-loop ((task-queue task-queue))
  (loop with wait-whostate = (task-wait-whostate task-queue)
	doing (process-wait wait-whostate #'task-queue-pending-tasks-p task-queue)
        (task-queue-execute-pending-tasks task-queue)))

;; Backward compatibility
(defmethod task-queue-run-p ((task-queue task-queue))
  (task-run-p task-queue))

;; Backward compatibility
(defgeneric start-task-queue (task-queue)
  (:documentation "Starts TASK-QUEUE executing tasks by activing its process."))

(defmethod start-task-queue ((task-queue task-queue))
  (start-task task-queue))

;; Backward compatibility
(defgeneric task-queue-process-kill (task-queue)
  (:documentation "Stops the task queue process and kills it."))

(defmethod task-queue-process-kill ((task-queue task-queue))
  (task-process-kill task-queue))

;; Backward compatibility
(defgeneric stop-task-queue (task-queue)
  (:documentation "Stops TASK-QUEUE queue from executing tasks by stopping its process."))

;; specialize this method to perform clean up activity.
(defmethod stop-task-queue ((task-queue task-queue))
  (declare (values task-queue)))

(defmethod stop-task-queue :around ((task-queue task-queue))
  (shutting-task-down (task-queue)
    (call-next-method)))

;; Backward compatibility
(defgeneric ensure-active-task-queue (task-queue)
  #-(or clisp ecl) (declare (values task-queue))
  (:documentation "Ensures that task-queue is active and executing tasks."))

(defmethod ensure-active-task-queue ((task-queue task-queue))
  (ensure-active-task task-queue))

(defmethod ensure-active-task-queue ((task-queue-class symbol))
  (ensure-active-task task-queue-class))

(defgeneric task-queue-exhausted-p (task-queue)
  (:documentation "Returns non-null when all tasks in TASK-QUEUE are completed.
This is the primary method used to determine if a there is more work remaining."))

(defmethod task-queue-exhausted-p ((task-queue queue-mixin))
  (unless (task-queue-queue task-queue)
    (with-lock-held ((task-queue-lock task-queue) :read "Check Completion")
      (null (task-queue-queue task-queue)))))	;queue empty 

;;;------------------------------------------------------------------- 
;;;
;;;  MULTITHREADED TASK QUEUES
;;;

(defclass task-thread
	  (background-task)
    ((task-queue :initarg :task-queue :accessor task-thread-task-queue)
     (current-task :initform nil :initarg :current-task :accessor task-thread-current-task))
  (:documentation "A task thread for a multithreaded task queue."))

(defclass multi-threaded-task-queue
	  (task-runnable-mixin queue-mixin)
    ((threads :initform nil :initarg :threads :accessor task-queue-threads)
     (thread-number :initform 1 :initarg :thread-number :accessor task-queue-thread-number)
     (process-priority :initform 0 :initarg :process-priority :accessor task-queue-process-priority)
     (thread-name :initform "Task-Queue-Thread" :initarg :thread-name :reader task-queue-thread-name
		  :reader task-process-name :allocation :class)
     (task-thread-class :initform 'task-thread :reader task-queue-task-thread-class :allocation :class))
  (:documentation "A multithreaded version of task-queue.")) 

(defmethod task-queue-pending-tasks-p ((task-queue multi-threaded-task-queue))
  (not (null (task-queue-queue task-queue))))

(defmethod task-thread-pending-tasks-p ((task-thread task-thread) (task-queue multi-threaded-task-queue))
  (with-slots (run-p) task-thread
    (with-slots (queue ) task-queue
      (and queue run-p))))

(defgeneric task-thread-executing-task-p (task-thread)
  (:documentation "Returns non-null when task-thread is executing a task.
This needs to be used in conjunction with a lock on the task queue
to avoid race conditions."))

(defmethod task-thread-executing-task-p ((task-thread task-thread))
  (with-slots (current-task) task-thread
    (not (null current-task))))

(defmethod task-main-loop ((thread task-thread))
  (loop with task-queue = (task-thread-task-queue thread)
	and wait-whostate = (task-wait-whostate thread)
	doing (process-wait wait-whostate #'task-thread-pending-tasks-p thread task-queue)
	      (task-thread-execute-pending-tasks thread task-queue)))

;; See also task-queue-execute-pending-tasks
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
      (setq threads (make-array (task-queue-thread-number task-queue) :adjustable t :fill-pointer 0))
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
	for idx fixnum downfrom (1- (the fixnum (fill-pointer threads))) to (task-queue-thread-number task-queue)
	for thread = (aref threads idx)
	do (stop-task thread)
	   (task-process-kill thread)
	   (decf (the fixnum (fill-pointer threads)))))

(defmethod start-task ((task-queue multi-threaded-task-queue))
  ;; create any new ones to bring the total up to thread-number
  (%fill-task-threads task-queue)
  ;; start up existing threads
  (%activate-task-threads task-queue)
  (setf (task-run-p task-queue) t)
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

(defmethod stop-task ((task-queue multi-threaded-task-queue))
  (setf (task-run-p task-queue) nil)
  (map-task-threads task-queue #'stop-task)
  task-queue) 

(defmethod ensure-active-task ((task-queue multi-threaded-task-queue))
  (start-task task-queue))

(defmethod task-queue-exhausted-p ((task-queue multi-threaded-task-queue))
  (unless (task-queue-queue task-queue)
    (with-lock-held ((task-queue-lock task-queue) :read "Check Completion")
      (and (null (task-queue-queue task-queue))	;queue empty
	   (loop for thread across (task-queue-threads task-queue)	;all threads idle
		 never (task-thread-executing-task-p thread))))))


;;;------------------------------------------------------------------- 
;;;
;;; TESTING
;;;

#|

(setq tq (ensure-active-task-queue 'task-queue))

(defparameter *task-number* 0)

(defun task (tq)
  (declare (ignore tq))
  (format tv:initial-lisp-listener "~&Task: ~D" (incf *task-number*)))

(loop for i from 1 to 100
      do (push-task-queue tq #'task))

|#
