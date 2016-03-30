(in-package :www-utils)

;;; Missing definitions in the jkf/unix.lisp

(defun alphalessp (a b)
  (string< a b))

(defun forget-domain-names ()
  (ipc:getdomainname nil)
  (clrhash *domain-name-lookup*))

(defmethod (setf process-priority) (val (process t))
  (setf (mp:process-priority process) val)
  )

(defmacro with-crlf-stream ((stream direction) &body body)
  "Turns STREAM into a CRLF stream within the scope of BODY.
direction can be :OUTPUT, :INPUT, or :BOTH."
  (declare (ignore stream direction))
  `(progn ,@body))

(defmacro atomic-conditional-replacef (reference predicate new-value)
  "When PREDICATE returns non-null, this setfs REFERENCE to NEW-VALUE.
Predicate is called (OLD-VALUE NEW-VALUE). The operation
assures that predicate application and swap are atomic."
  (let ((old-value (gensym))
        (new-value-var (gensym)))
    `(clim-sys:without-scheduling
      (let ((,old-value ,reference)
            (,new-value-var ,new-value))
        (when (funcall ,predicate ,old-value ,new-value-var)
          (setf ,reference ,new-value-var)
          (values ,old-value t))))))

;;; (SETF HTTP:LOCAL-CONTEXT) is defined in utils.lisp but used before in unix.lisp
;;; don't know what to do
