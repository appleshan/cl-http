(in-package :www-utils)

;;; Process layer of CL-HTTP for ecls (very much like acl)

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
  (mp:process-active-p process))

(defun process-disable (process)
  #+no (mp:process-disable process)
  (mp:interrupt-process process 'mp:exit-process)
  )

(defun process-enable (process)
  (unless (mp:process-active-p process)
    (mp:process-enable process)))

(defun process-wait (reason &rest args)
  #+no (apply #'mp:process-wait reason args)
  (warn "Don't know how to process wait  ~A ~%" reason)
  )

(defun process-kill (process)
  (mp:process-kill process))

(defun current-process ()
  mp:*current-process*)

(defun make-process (name &key background-p restart-after-reset warm-boot-action priority &allow-other-keys)
  (declare (ignore background-p restart-after-reset warm-boot-action priority))
  (mp:make-process :name name))

(defmethod process-preset ((process t) initial-function &rest initial-args)
  (apply #'mp:process-preset process initial-function initial-args))

(defmethod process-whostate ((process t))
  (mp:process-whostate process)
  )

(defmethod (setf process-priority) (val (process t))
  val
  )

(defun process-run-function (name function &rest args)
  (apply #'mp:process-run-function name function args)
  )

(defun process-wait-with-timeout (whostate seconds function &rest args)
  #+no (apply #'mp:process-wait-with-timeout whostate seconds function args)
  (warn "Don't know how to process wait with timeout ~A ~%" whostate)
  )

(defun process-name (process)
  (mp:process-name process)
  )

(defun all-processes ()
  (mp:all-processes)
  )

(defun make-lock (name &key type &allow-other-keys)
  "Returns a lock named name that is suitable for use with with-lock-held."
  (declare (ignore type))
  (mp:make-lock :name name))

(defmacro with-lock-held ((lock &optional (mode :write) (whostate "Wait for Lock")) &body body)
  "Executes BODY with LOCK held in MODE, which is one of :READ or :WRITE."
  (declare (ignore mode whostate))
  `(mp:with-lock
       (,lock) ,@body))


(defmacro with-timeout ((timeout &key error-p) &body body)
  "Executes BODY and returns the values of the last form in BODY. However, if
the execution takes longer than TIMEOUT seconds, abort it. If :ERROR-P is
unsupplied or false, just return nil. If :ERROR-P is true, signal an error."
  (declare (ignore timeout error-p))
  `(progn ,@body))

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

(defparameter %my-global-lock (mp:make-lock :name :global-lock))

(defmacro %without-scheduling (&body body)
  `(mp:with-lock (%my-global-lock)
    ,@body))

(defmacro atomic-conditional-replacef (reference predicate new-value)
  "When PREDICATE returns non-null, this setfs REFERENCE to NEW-VALUE.
Predicate is called (OLD-VALUE NEW-VALUE). The operation
assures that predicate application and swap are atomic."
  (let ((old-value (gensym))
        (new-value-var (gensym)))
    `(%without-scheduling  
       (let ((,old-value ,reference)
             (,new-value-var ,new-value))
         (when (funcall ,predicate ,old-value ,new-value-var)
           (setf ,reference ,new-value-var)
           (values ,old-value t))))))

(defmacro atomic-decf (reference &optional (delta 1))
  "Atomically decrements REFERENCE by DELTA."
  `(%without-scheduling
     (decf ,reference ,delta)))

(defmacro atomic-push (item reference)
  "Atomically pushes ITEM onto REFERENCE."
  `(%without-scheduling
     (push ,item ,reference)))

(defmacro atomic-incf (reference &optional (delta 1))
  "Atomically increments REFERENCE by DELTA."
  `(%without-scheduling
     (incf ,reference ,delta)))

;;; this is defined in without-preemption
;;; it better shoudn't
;;; fixme
(defmacro without-preemption (&body body)
  `(%without-scheduling ,@body))