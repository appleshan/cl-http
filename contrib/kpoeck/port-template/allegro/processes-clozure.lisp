(in-package :www-utils)

;;; Process layer of CL-HTTP for clozure

(defun process-idle-time (process)
  "Returns the amount of time the process has been up, in seconds."
  (declare (ignore process))
  0
  )

(defun process-run-time (process)
  "Returns the amount of run time the process has accumulated, in microseconds."
  ;;; used to be multiplied by 1000, don't know why
  (let ((ticks (ccl:process-total-run-time process)))
    (or ticks 0))
  )

(defmethod process-active-p ((process t))
  (ccl::process-active-p process))

(defun process-disable (process)
  "Don't know what to do here"
  (ccl:process-reset process)
  )

;;; Not reaally sure about the semantic here in a model with real threads
;;; Enabling a running process has probably no effect
;;; Legal use seems to be just after make-process, process-preset
(defun process-enable (process)
  (when (string= "Reset" (ccl:process-whostate process))
    (handler-case
        (ccl:process-enable process 2)
      (error (error)
             (warn "Coud not enable process ~a with error ~a ~%" process error)))))

(defun process-wait (reason &rest args)
  (apply #'ccl:process-wait reason args))

(defun process-kill (process)
  (ccl:process-kill process))

(defun current-process ()
  ccl:*current-process*)

(defun make-process (name &key background-p restart-after-reset warm-boot-action priority &allow-other-keys)
  (declare (ignore background-p restart-after-reset warm-boot-action priority))
  (ccl:make-process name))

(defmethod process-preset ((process t) initial-function &rest initial-args)
  (apply #'ccl:process-preset process initial-function initial-args))

(defmethod process-whostate ((process t))
  (ccl:process-whostate process)
  )

(defmethod (setf process-priority) (val (process t))
  (declare (ignore val process))
  (values)
  )

(defun process-run-function (name function &rest args)
  (apply #'ccl:process-run-function name function args)
  )

(defun process-wait-with-timeout (whostate seconds function &rest args)
  (apply #'ccl:process-wait-with-timeout whostate seconds function args))

(defun process-name (process)
  (ccl:process-name process)
  )

(defun all-processes ()
  (ccl:all-processes)
  )

(defun make-lock (name &key type &allow-other-keys)
  "Returns a lock named name that is suitable for use with with-lock-held."
  (declare (ignore name type))
  (ccl:make-read-write-lock))

;;; or with-lock-grabbed???
(defmacro with-lock-held ((lock &optional (mode :write) (whostate "Wait for Lock")) &body body)
  "Executes BODY with LOCK held in MODE, which is one of :READ or :WRITE."
  (declare (ignore whostate))
  `(ecase ,mode
     (:read (ccl:with-read-lock (,lock) ,@body))
     (:write (ccl:with-read-lock (,lock) ,@body))))

#+no
(defun invoke-with-timeout (seconds bodyfn timeoutfn)
  "Executes the function \(with no arguments) BODY-FN and returns
its results but stops execution after DURATION seconds and then
instead calls TIMEOUT-FN and returns its values."
  ;; from Portable AllegroServe
  (block timeout
    (let* ((timer (ccl::make-timer-request seconds
                                           #'(lambda ()
                                               (return-from timeout (funcall timeoutfn))))))
      (ccl::enqueue-timer-request timer)
      (unwind-protect (funcall bodyfn)
        (ccl::dequeue-timer-request timer)))))

#+no
(defmacro with-timeout ((seconds &body timeout-forms) &body body)
  "Executes the code BODY and returns the results of the last
form but stops execution after SECONDS seconds and then instead
executes the code in TIMEOUT-FORMS."
  ;; from Portable AllegroServe
  `(invoke-with-timeout ,seconds
                        #'(lambda () ,@body)
                        #'(lambda () ,@timeout-forms)))


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

;;; this is defined in wwww-utils
;;; it better shoudn't
;;; fixme
(defmacro without-preemption (&body body)
  `(ccl:without-interrupts ,@body))

(defmacro atomic-conditional-replacef (reference predicate new-value)
  "When PREDICATE returns non-null, this setfs REFERENCE to NEW-VALUE.
Predicate is called (OLD-VALUE NEW-VALUE). The operation
assures that predicate application and swap are atomic."
  (let ((old-value (gensym))
        (new-value-var (gensym)))
    `(without-preemption  
       (let ((,old-value ,reference)
             (,new-value-var ,new-value))
         (when (funcall ,predicate ,old-value ,new-value-var)
           (setf ,reference ,new-value-var)
           (values ,old-value t))))))

#+nil
(defmacro atomic-decf (reference &optional (delta 1))
  "Atomically decrements REFERENCE by DELTA."
  `(ccl::atomic-incf-decf ,reference ,delta))

(defmacro atomic-decf (reference &optional (delta 1))
  "Atomically decrements REFERENCE by DELTA."
  `(without-preemption
    (decf ,reference ,delta)))

#+nil
(defmacro atomic-incf (reference &optional (delta 1))
  "Atomically increments REFERENCE by DELTA."
  `(ccl::atomic-incf-decf ,reference ,delta))

(defmacro atomic-incf (reference &optional (delta 1))
  "Atomically increments REFERENCE by DELTA."
  `(without-preemption
    (incf ,reference ,delta)))

(defmacro atomic-push (item reference)
  "Atomically pushes ITEM onto REFERENCE."
  `(without-preemption
     (push ,item ,reference)))