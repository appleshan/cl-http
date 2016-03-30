(in-package :www-utils)

(defun arglist (function)
  "Returns the arglist for FUNCTION."
  #+allegro
  (excl:arglist function)
  #+clozure-common-lisp
  (ccl:arglist function)
  #+sbcl
  (sb-introspect:function-arglist function)
  #+clisp
  (ext:arglist function)
  #+ecl
  (%arglist% function)
  #+lispworks
  (lw:function-lambda-list function)
  ;;; this is too much code to copy
  #+cmu
  (if (find-package :swank-backend)
      (funcall (find-symbol "ARGLIST" (find-package :swank-backend)) 'function)
      :better-load-slime)
  )

;;; from slime
#+ecl
(defun %arglist% (name)
  (or (functionp name) (setf name (symbol-function name)))
  (if (functionp name)
      (typecase name 
        (generic-function
         (clos::generic-function-lambda-list name))
        (compiled-function
         ; most of the compiled functions have an Args: line in their docs
         (with-input-from-string (s (or
                                     (si::get-documentation
                                      (si:compiled-function-name name) 'function)
                                     ""))
           (do ((line (read-line s nil) (read-line s nil)))
               ((not line) :not-available)
             (ignore-errors
               (if (string= (subseq line 0 6) "Args: ")
                   (return-from nil
                     (read-from-string (subseq line 6))))))))
         ;
        (function
         (let ((fle (function-lambda-expression name)))
           (case (car fle)
             (si:lambda-block (caddr fle))
             (t               :not-available)))))
      :not-available))

(defvar *%cached-time-zone%* nil)

(defun time-zone (&optional update-p)
  (flet ((compute-time-zone ()
                           (multiple-value-bind
                                 (second minute hour date month year day daylight-p zone)
                               (get-decoded-time)
                             (declare (ignore second minute hour date month year day daylight-p))
                             zone)))
    (cond ((or (null *%cached-time-zone%*) update-p)
           (setq *%cached-time-zone%* (compute-time-zone)))
          (t *%cached-time-zone%*)))
  )

(defun char-bits (char)
  (declare (ignore char))
  0)

(defun string-thin (string)
  "Strips font description"
  string)


;;; would be nice if allegro compiler would note that this is missing, don't know why not
(defun log-http-server-error (format-string &rest format-args)
  (apply #'notify-log-window  format-string format-args))


(defun notify-log-window (format-string &rest format-args)
  "Top-level method for writing to the HTTP log window."
  (let ((stream (get-logging-stream)))
    (fresh-line stream)
    (write-char #\[ stream)
    (http::write-standard-time (get-universal-time) stream)
    (write-string "]  " stream)
    (apply #'format stream format-string format-args)))

(defun expose-log-window ())

(defun get-logging-stream ()
  #+(and :allegro (version>= 7 0)) excl:*initial-terminal-io*
  #+(and :allegro (version< 7 0)) *trace-output*
  #+(or sbcl clozure-common-lisp) *trace-output*
  #-(or allegro sbcl clozure-common-lisp) *standard-output*
  )

(defun report-condition (condition stream)
  "Prints the report string for CONDITION onto STREAM."
  (handler-case (format stream "~A" condition)
    ;; Cannot guaranty all errors are printable.
    (error ()
      (describe condition stream))))

#+(and :does-this-still-apply? :allegro :sparc :solaris2 (version>= 6 0))
(defun report-condition (condition stream)
  "Prints the report string for CONDITION onto STREAM."
  (typecase condition
    (excl:socket-error)
    (stream-error)
    (otherwise
     (handler-case (format stream "~A" condition)
       ;; Cannot guaranty all errors are printable.
       (error ()
         (describe condition stream))))))

(defun report-string (condition)
  "Returns the report string for CONDITION."
  (with-output-to-string (stream)
    (report-condition condition stream)))


(defun %make-log-pathname (device directory name host)
  "Returns the pathname to which current log entries are written."
  (make-pathname
    :host host
    :device device
    :directory directory
    :name name
    :type "log"))
