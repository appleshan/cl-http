(in-package :www-utils)

;;; Dummy process stuff in a single threaded lisp
;;; Processes

(defclass %process% ()
  (
   (function-to-run :initform nil :accessor function-to-run)
   (args-to-use :initform nil :accessor args-to-use)
   (state :initform :stopped :accessor state)
   (name :initform "Unnamed process" :initarg :name :accessor name)
   (who-state :initform "Whatever State" :accessor who-state)
   )
  )

(defparameter %current-process% (make-instance '%process% :name "Dummy current process"))

(defun process-idle-time (process)
  "Returns the amount of time the process has been up, in seconds."
  (declare (ignore process))
  0
  )

(defun process-run-time (process)
  "Returns the amount of run time the process has accumulated, in microseconds."
  (declare (ignore process))
  0
  )

(defmethod process-active-p ((process t))
  nil
  )

(defun process-disable (process)
  process
  )

(defun process-enable (process)
  process
  )

(defun process-wait (reason &rest args)
  (values reason args)
  )

(defun process-kill (process)
  process
  )

(defun current-process ()
  %current-process%
  )

(defun make-process (name &key &allow-other-keys)
  (make-instance '%process% :name name)
  )

(defmethod process-preset ((process t) initial-function &rest initial-args)
  (values initial-function initial-args)
  )

(defmethod process-whostate ((process t))
  )

(defmethod (setf process-priority) (val (process t))
  val
  )

(defun process-name (process)
  (error "process-name should not be called")
  )

(defun all-processes ()
  (error "all-processes should not be called")
  )

(defun make-lock (name &key type &allow-other-keys)
  "Returns a lock named name that is suitable for use with with-lock-held."
  (values name type)
  )

(defun process-run-function (name function &rest args)
  (values name function args))

(defun process-wait-with-timeout (whostate seconds function &rest args)
  (values whostate seconds function args)
  )

(defmacro atomic-conditional-replacef (reference predicate new-value)
  "When PREDICATE returns non-null, this setfs REFERENCE to NEW-VALUE.
Predicate is called (OLD-VALUE NEW-VALUE). The operation
assures that predicate application and swap are atomic."
  `(when ,predicate
     (setf ,reference ,new-value))
  )

(defmacro atomic-decf (reference &optional (delta 1))
  "Atomically decrements REFERENCE by DELTA."
  `(decf ,reference ,delta)
  )

(defmacro atomic-push (item reference)
  "Atomically pushes ITEM onto REFERENCE."
  `(push ,item ,reference)
  )

(defmacro atomic-incf (reference &optional (delta 1))
  "Atomically increments REFERENCE by DELTA."
  `(incf ,reference ,delta))

(defmacro with-lock-held ((lock &optional (mode :write) (whostate "Wait for Lock")) &body body)
  "Executes BODY with LOCK held in MODE, which is one of :READ or :WRITE."
  `(progn
     (values ,lock ,mode ,whostate)
     (progn ,@body)))

(defmacro with-timeout ((timeout &key error-p) &body body)
  "Executes BODY and returns the values of the last form in BODY. However, if
the execution takes longer than TIMEOUT seconds, abort it. If :ERROR-P is
unsupplied or false, just return nil. If :ERROR-P is true, signal an error."
  `(progn ,timeout ,error-p . ,body))

(defmacro with-stream-timeout ((stream timeout &key error-p) &body body)
  "Executes BODY and returns the values of the last form in BODY. 
However, if the stream goes idle for longer than TIMEOUT seconds, the
operation is aborted.  If ERROR-P is non-null, the time out error is
signalled, otherwise NIL is returned."
  `(progn ,stream
     (with-timeout (,timeout :error-p ,error-p) . ,body)))
