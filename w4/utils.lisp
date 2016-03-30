;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: (w4 :use (future-common-lisp www-utils url http)); -*-

;;; Copyright John C. Mallery,  1995-96, 2000.
;;; All rights reserved.


;;;------------------------------------------------------------------- 
;;;
;;; MACROS AND UTILITIES
;;;
(in-package :w4)

(define debug-walker (&optional (on-p (not *debug-walker*)))
  "Toggles walker debugging according to ON-P."
  (setq *debug-walker* (not (null on-p))))

(define %constraint-class-for-type (type)
  (or (cdr (assoc type *constraint-class-alist*))
      (error "Unknown constraint type, ~S." type)))

(define constraint-types ()
  (mapcar #'car *constraint-class-alist*))

(define %action-class-for-type (type)
  (or (cdr (assoc type *action-class-alist*))
      (error "Unknown action type, ~S." type)))

(defun %queue-class-for-type (type)
  (or (cdr (assoc type *queue-class-alist*))
      (error "Unknown activity queue type, ~S." type)))

(define action-types ()
  (mapcar #'car *action-class-alist*))

(define-variable *depth* 0)

(defmacro with-activity-bindings ((activity url depth url-stack) &body body)
  `(let ((*activity* ,activity)
	 (*depth* (1+ (the integer ,depth)))
         (*url-stack* (cons ,url ,url-stack))
	 (*report-stream* (report-stream ,activity))
	 (url:*url-host-name-resolution* (activity-url-host-name-resolution ,activity))
	 (http:*client-timeout* (activity-connection-timeout ,activity)))
     (with-walking-traced (,url ,activity)
       (url:with-uri-universe ((activity-uri-universe-object ,activity))
	 ,@body))))

(declaim (inline depth))

(define depth ()
  "Returns the depth within the Web walk."
  *depth*)

(declaim (inline current-url))

(define current-url ()
  (first *url-stack*))

(declaim (inline parent-url))

(define parent-url ()
  (first *url-stack*))

(defun indent-for-depth (depth &optional (stream *standard-output*))
  (loop for i upto depth
	do (progn i (write-char #\space stream))))

(define trace-report (stream format-string &rest format-args)
  (declare (dynamic-extent format-args))
  (fresh-line stream)
  (indent-for-depth *depth* stream)
  (apply #'format stream format-string format-args))

(defun trace-report-stack-depth (depth url activity)
  (trace-report (report-stream activity) "~D | ~A" depth (name-string url)))

(defmacro with-walking-traced ((url activity) &body body)
  `(progn
     (when *trace-constraints*
       (trace-report-stack-depth *depth* ,url ,activity))
     ,@body))

(defmacro with-some-success ((atom-or-set) &body body)
  `(flet ((winner-p (item)
            (symbol-macrolet ((,atom-or-set item))
              ,@body)))
     (declare (inline winner-p))
     (typecase ,atom-or-set
       (cons (loop for item in ,atom-or-set
                   when (winner-p item)
                     return t
                   finally (return nil)))
       (t (winner-p ,atom-or-set)))))

(define-macro with-activity-value-cached ((activity key &key (recompute-p nil recompute-supplied-p)) &body value-form)
  "Caches the value returned by value-form on ACTIVITY's property list under the indicator KEY.
When RECOMPUTE-P is non-null, the value is recomputed and recached.
The returned values are VALUE and RETRIEVE-FROM-CACHE-P."
  (declare (values retrieved-from-cache-p))
  (let ((form `(let ((val (getf plist ,key :+not-found+)))
                 (case val
                   (:+not-found+ 
                     (setf (getf plist ,key) (progn  . ,value-form)))
                   (t (values val t))))))      
    (cond (recompute-supplied-p
           `(with-slots (plist) ,activity
              (cond (,recompute-p
                     (setf (getf plist ,key) (progn . ,value-form)))
                    (t ,form))))
          (t `(with-slots (plist),activity ,form)))))

(define abort-activity-on-resource (&optional condition)
  (throw 'abort-activity-on-resource condition))

(defmacro handling-activity-aborts ((&rest ignore) &body body)
  (declare (ignore ignore))
  `(catch 'abort-activity-on-resource
     ,@body))

(defun %make-hash-table (resource)
  (declare (ignore resource))
  (make-hash-table :test #'equalp))

(defun clear-table-resource (resource table)
  (declare (ignore resource))
  (clrhash table))

(defresource w4-hash-table ()
  :constructor %make-hash-table
  :deinitializer clear-table-resource)

(defun robot-version ()
  (concatenate 'string "W4-" (http::server-version)))

(defun set-local-proxy (host port)
  "Sets the Web walker local proxy to HOST and PORT.
When HOST is null, the local proxy is cleared."
  (setq *local-proxy* (if host (http::intern-proxy host port :if-does-not-exist :create) nil)))

(declaim (inline local-proxy))

(defun local-proxy ()
   "Returns the Web walker local proxy."
   *local-proxy*)

(define-macro with-network-operation-retries ((op-string n-of-retries
                                                         &key (wait-interval 0) (stream *report-stream*)
                                                         error-values-form)
                                              &body body)
  "Executes body.  If a network error is encountered, 
this retries NUMBER-OF-RETRIES waiting for WAIT-INTERVAL, and then punts if no luck."
  `(loop named try-hard
         for tries upfrom 1
         doing (handler-case-if (not *debug-walker*)
                  (return-from try-hard (values-list (multiple-value-list (progn ,@body))))
                 (network-error                 ; host-not-responding bad-connection-state 
                   (error)
                   (cond
                     ((= tries ,n-of-retries)
                      (format ,stream "~&Try ~D: ~&~A~&Persisting ~A errors during ~A.~&Punting.~&"
                              tries ,op-string (type-of error) (report-string error) )
                      ,(if error-values-form
                           `(return-from try-hard ,error-values-form)
                           `(return-from try-hard nil)))
                     (t (format ,stream "~&Try ~D: ~A error during ~A.  ~&~A~&Will retry."
                                tries (type-of error) (report-string error) ,op-string)
                        (unless (zerop ,wait-interval) (sleep ,wait-interval))))))))