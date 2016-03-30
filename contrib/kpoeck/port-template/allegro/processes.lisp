(in-package :www-utils)

;;; Process layer of CL-HTTP for Allegro

 #+(and :allegro (version>= 7 0))
(defun process-idle-time (process)
  "Returns the amount of time the process has been up, in seconds."
  (declare (ignore process)) 
  0
  )

#+(and :allegro (version< 7 0))
(defun process-idle-time (process)
  "Returns the amount of time the process has been up, in seconds."
   (or (mp::process-start-secs process) 0)
  )

(defun process-run-time (process)
  "Returns the amount of run time the process has accumulated, in microseconds."
  ;;; used to be multiplied by 1000, don't know why
  (or (mp:process-cpu-msec-used process) 0)
  )

(defmethod process-active-p ((process t))
  (mp:process-active-p process))

(defun process-disable (process)
  (mp:process-disable process))

(defun process-enable (process)
  (mp:process-enable process))

(defun process-wait (reason &rest args)
  (apply #'mp:process-wait reason args))

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
  (setf (mp:process-priority process) val)
  )

(defun process-run-function (name function &rest args)
  (apply #'mp:process-run-function name function args)
  )

(defun process-wait-with-timeout (whostate seconds function &rest args)
  (apply #'mp:process-wait-with-timeout whostate seconds function args))

(defun process-name (process)
  (mp:process-name process)
  )

(defun all-processes ()
  (copy-list mp:*all-processes*)
  )

(defun make-lock (name &key type &allow-other-keys)
  "Returns a lock named name that is suitable for use with with-lock-held."
  (declare (ignore type))
  (mp:make-process-lock :name name))

(defmacro with-lock-held ((lock &optional (mode :write) (whostate "Wait for Lock")) &body body)
  "Executes BODY with LOCK held in MODE, which is one of :READ or :WRITE."
  (declare (ignore mode))
  `(mp:with-process-lock
       (,lock :norecursive t :whostate ,whostate) ,@body))

#+(and :allegro (version< 7 0))
(defmacro with-timeout ((timeout &key error-p) &body body)
  "Executes BODY and returns the values of the last form in BODY. However, if
the execution takes longer than TIMEOUT seconds, abort it. If :ERROR-P is
unsupplied or false, just return nil. If :ERROR-P is true, signal an error."
  `(mp:with-timeout ((or ,timeout 5.) (if ,error-p (error "Error Timeout occured.") nil)) ,@body))


#+(and :allegro (version>= 7 0))
(defmacro with-timeout ((timeout &key error-p) &body body)
  "Executes BODY and returns the values of the last form in BODY. However, if
the execution takes longer than TIMEOUT seconds, abort it. If :ERROR-P is
unsupplied or false, just return nil. If :ERROR-P is true, signal an error."
  `(if ,timeout 
       (sys:with-timeout (,timeout (if ,error-p (error "Error Timeout occured.") nil)) ,@body)
     (progn ,@body)))
#|
(progn
  (with-timeout (1 :error-p nil)
    (sleep 3))
  42)

(progn
  (with-timeout (nil :error-p nil)
    (sleep 3)
    4)
  42)

(progn
  (with-timeout (1 :error-p t)
    (sleep 3))
  42)

(progn
  (with-timeout (nil :error-p t)
    (sleep 3))
  42)
|#


;;; mp:without-scheduling or mp:without-interrupts
;;; use as in aserve

(defmacro atomic-conditional-replacef (reference predicate new-value)
  "When PREDICATE returns non-null, this setfs REFERENCE to NEW-VALUE.
Predicate is called (OLD-VALUE NEW-VALUE). The operation
assures that predicate application and swap are atomic."
  (let ((old-value (gensym))
        (new-value-var (gensym)))
    `(mp:without-scheduling  
       (let ((,old-value ,reference)
             (,new-value-var ,new-value))
         (when (funcall ,predicate ,old-value ,new-value-var)
           (setf ,reference ,new-value-var)
           (values ,old-value t))))))

(defmacro atomic-decf (reference &optional (delta 1))
  "Atomically decrements REFERENCE by DELTA."
  `(mp:without-scheduling
     (decf ,reference ,delta)))

(defmacro atomic-push (item reference)
  "Atomically pushes ITEM onto REFERENCE."
  `(mp:without-scheduling
     (push ,item ,reference)))

(defmacro atomic-incf (reference &optional (delta 1))
  "Atomically increments REFERENCE by DELTA."
  `(mp:without-scheduling
     (incf ,reference ,delta)))

;;; this is defined in without-preemption
;;; it better shoudn't
;;; fixme
(defmacro without-preemption (&body body)
  `(mp:without-scheduling ,@body))