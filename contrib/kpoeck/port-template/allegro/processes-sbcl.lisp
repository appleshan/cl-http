(in-package :www-utils)

;;; Process layer of CL-HTTP for sbcl
;;; sb-thread:list-all-threads

(defun process-idle-time (process)
  "Returns the amount of time the process has been up, in seconds."
  (declare (ignore process))
    ;;; Can't find an exported interface
  0
  )

(defun process-run-time (process)
  "Returns the amount of run time the process has accumulated, in microseconds."
  ;;; used to be multiplied by 1000, don't know why
  (declare (ignore process))
  ;;; Can't find an exported interface
  0
  )

;;; Begin these do not follow the sbcl model of threads
;;; Easiest would be to create a process class and model the 
;;; interface on top of them
;;; Perhaps borrow from openmcl

(defclass %process% ()
  (
   (function-to-run :initform nil :accessor function-to-run)
   (args-to-use :initform nil :accessor args-to-use)
   (state :initform :stopped :accessor state)
   (name :initform "Unnamed process" :initarg :name :accessor name)
   (who-state :initform "Whatever State" :accessor who-state)
   (thread :initform nil :accessor thread)
   )
  )

(defmethod process-active-p ((me %process%))
  (eq (state me) :disable))

(defmethod process-active-p ((process sb-thread:thread))
  (sb-thread:thread-alive-p process))

(defmethod process-disable ((me %process%))
  "Don't know what to do here"
 ;;; Can't find an exported interface
  (setf (state me) :disable)
  (warn "Don't know yet how to disable thread")
  )

(defmethod process-enable ((me %process%))
    "Don't know what to do here"
 ;;; Can't find an exported interface
    (setf (state me) :enable)
    ;;; Create the thread and go
    (warn "Don't know yet how to enable thread")
  )

(defun make-process (name &key background-p restart-after-reset warm-boot-action priority &allow-other-keys)
  (declare (ignore background-p restart-after-reset warm-boot-action priority))
  (make-instance '%process% :name name))

(defmethod process-preset ((me %process%) initial-function &rest initial-args)
  (setf (function-to-run me) initial-function)
  (setf (args-to-use me) initial-args)
  me)

(defmethod process-whostate ((me %process%))
  (who-state me)
  )

;;; End these do not follow the sbcl model of threads

(defmethod process-kill ((process sb-thread:thread))
  (sb-thread:terminate-thread process))

(defmethod process-kill ((me %process%))
  (setf (state me) :killed))

(defun current-process ()
  sb-thread:*current-thread*)

(defmethod (setf process-priority) (val (process t))
  (declare (ignore val process))
  (values)
  )

(defun process-run-function (name function &rest args)
  (sb-thread:make-thread #'(lambda () (apply function args)) :name name)
  )

;;; can we implement this with sb-thread:interrupt-thread & Waitqueue/condition variables
(defun process-wait (reason &rest args)
  (declare (ignore reason args))
  (warn "Don't know yet how to wait for a process"))

(defun process-wait-with-timeout (whostate seconds function &rest args)
  (declare (ignore whostate seconds function args))
  (warn "Don't know yet how to wait for a process with timeout"))

;;; Assume tha we get threads
(defun process-name (process)
  (sb-thread:thread-name process)
  )

(defun all-processes ()
  (sb-thread:list-all-threads)
  )

;;;

(defun make-lock (name &key type &allow-other-keys)
  "Returns a lock named name that is suitable for use with with-lock-held."
  (declare (ignore type))
  (sb-thread:make-mutex :name name)) 

;;; sb-thread:with-mutex or sb-thread:with-recursive-lock???
(defmacro with-lock-held ((lock &optional (mode :write) (whostate "Wait for Lock")) &body body)
  "Executes BODY with LOCK held in MODE, which is one of :READ or :WRITE."
  (declare (ignore mode whostate))
  `(sb-thread:with-mutex (,lock :wait-p t)
     ,@body))

(defmacro with-timeout ((timeout &key error-p) &body body)
  "Executes BODY and returns the values of the last form in BODY. However, if
the execution takes longer than TIMEOUT seconds, abort it. If :ERROR-P is
unsupplied or false, just return nil. If :ERROR-P is true, signal an error."
  `(handler-case 
       (sb-ext:with-timeout ,timeout ,@body)
     (sb-ext:timeout (timeout)
       (if ,error-p (error "Timeout ~a~%" timeout)
	   (values)))))

#|
(progn
  (with-timeout (1 :error-p nil)
    (sleep 3))
  42)

(progn
  (with-timeout (1 :error-p t)
    (sleep 3))
  42)
|#


;;; mp:without-scheduling or mp:without-interrupts
;;; use as in aserve

;;; this is defined in without-preemption
;;; it better shoudn't
;;; fixme
(defvar %other-mutex% (make-lock "Mutex for any other business"))

(defmacro without-preemption (&body body)
  `(sb-thread:with-recursive-lock (%other-mutex%)  ,@body))

(defvar %incf-mutext% (make-lock "Incf mutex for incf, decf, ush & replacef"))

(defmacro without-preemption-special (&body body)
  `(sb-thread:with-recursive-lock (%incf-mutext%)  ,@body))

(defmacro atomic-conditional-replacef (reference predicate new-value)
  "When PREDICATE returns non-null, this setfs REFERENCE to NEW-VALUE.
Predicate is called (OLD-VALUE NEW-VALUE). The operation
assures that predicate application and swap are atomic."
  (let ((old-value (gensym))
        (new-value-var (gensym)))
    `(without-preemption-special  
       (let ((,old-value ,reference)
             (,new-value-var ,new-value))
         (when (funcall ,predicate ,old-value ,new-value-var)
           (setf ,reference ,new-value-var)
           (values ,old-value t))))))

(defmacro atomic-decf (reference &optional (delta 1))
  "Atomically decrements REFERENCE by DELTA."
  `(without-preemption-special
    (decf ,reference ,delta)))

(defmacro atomic-incf (reference &optional (delta 1))
  "Atomically increments REFERENCE by DELTA."
  `(without-preemption-special
    (incf ,reference ,delta)))

(defmacro atomic-push (item reference)
  "Atomically pushes ITEM onto REFERENCE."
  `(without-preemption-special
     (push ,item ,reference)))