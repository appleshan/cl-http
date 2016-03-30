;;;   -*- Mode: lisp; Package: RREP; BASE: 10; Syntax: ANSI-Common-Lisp;-*-
;;;
;;; REMOTE READ-EVAL-PRINT LOOP FOR LISPWORKS
;;;
;;; Based on initial code from Sven Van Caekenberghe <sven@beta9.be> Mon, 24 May 2004 19:31:17 +0200
;;;
;;; Revised by John C.Mallery, Mon, 24 May 2004 19:31:17 +0600

(defpackage :RREP
  (:use :common-lisp)
  (:documentation "A remote Read-Eval-Print Loop for LispWorks")
  (:export 
   "*SERVICE-PORT*"
   "DEBUG-PROCESS"
   "DISABLE-RREP-SERVICE"
   "ENABLE-RREP-SERVICE"
   "EXIT"
   "KILL-PROCESS"
   "SHOW-PROCESSES"))

(in-package :RREP)

(defparameter *service-port* 25001
  "The TCP port on which to listen for RREP connnections.")

(defvar *inside-debugger* nil)

(defun rrep-listener-top-level (stream &aux (count 0))
  (declare (fixnum count))
  (labels ((live-stream-p (stream)
             (and (www-utils:live-connection-p stream)
                  (open-stream-p stream)))
           (debug (condition)
             (when (and (not *inside-debugger*)
                        (live-stream-p stream))
               (format stream "~&DEBUG ~D> Invoking debugger outside listener top level." (incf count))
               (let ((*inside-debugger* t)
                     (*terminal-io* stream))
                 (invoke-debugger condition))))
           (exit-listener (condition)
             (declare (ignore condition))
             (return-from rrep-listener-top-level)))
    (declare (dynamic-extent #'debug))
    (unwind-protect
        (tagbody
         (format stream "~&Welcome to ~A ~A on ~A~
                        ~&This is a remote Read-Eval-Print Loop, use (RREP:EXIT) to stop~%"
                 (lisp-implementation-type) (lisp-implementation-version) (www-utils:local-host-domain-name))
         restart
         (handler-bind ((condition #'debug)
                        (condition #'exit-listener))
           (let ((tag-val (catch 'top-level (system::listener-top-level stream))))
             (if (and (not (eq tag-val :exit)) (live-stream-p stream))
                 (go restart)
               (return-from rrep-listener-top-level)))))
      (close stream :abort t)
      (kill-current-process))))

(defun rrep-listen-for-connection (socket-handle)
  "Function to be used as :function parameter to comm:start-up-server (only allows local connections)."
  (cond ((eql (comm:get-socket-address socket-handle) (comm:get-socket-peer-address socket-handle))
         (let ((socket-stream (make-instance 'comm:socket-stream :socket socket-handle :direction :io :element-type 'base-char)))
           (mp:process-run-function "Remote-Listener-Top-Level" () #'rrep-listener-top-level socket-stream)))
        (t (format *terminal-io* "Non-local connection refused~%")
           (comm::close-socket socket-handle))))

(defun rrep-make-service-process-name (port)
  (format nil "Read-Eval-Print Server (port ~D)" port))

(defun find-service-process (port &optional (service-name (rrep-make-service-process-name port)))
  (declare (dynamic-extent service-name))
  (flet ((fctn (process)
           (when (string-equal (mp:process-name process) service-name)
             (return-from find-service-process process))))
    (declare (dynamic-extent #'fctn))
    (mp:map-processes #'fctn)))

(defun enable-rrep-service (&optional (port *service-port*))
  "Start up a server that accepts remote listener connections only from the local host."
  (let ((service-name (rrep-make-service-process-name port)))
    (cond ((find-service-process port service-name))
          (t (comm:start-up-server :function #'rrep-listen-for-connection 
                                   :process-name (rrep-make-service-process-name port)
                                   :service port)))))

(defun disable-rrep-service (&optional (port *service-port*))
  "Start up a server that accepts remote listener connections only from the local host."
  (let ((process (find-service-process port)))
    (when process
      (mp:process-kill process))))

(defun kill-current-process ()
  (mp:process-kill mp:*current-process*))

(defun exit ()
  "Exit this REPL."
  (throw 'top-level :exit))

(defun show-processes (&key (stream *standard-output*))
  (flet ((fctn (process)
           (format stream "~&~S~42T~10,,D~54T~:(~A~) ~60T~A" 
                   (mp:process-name process) (mp:process-priority process) 
                   (mp:process-state process) (mp:process-whostate process))))
    (declare (dynamic-extent #'fctn))
    (format  stream "~&~14TProcess Name ~43TPriority ~54TState ~65TStatus")
    (mp:map-processes #'fctn)
    (values)))

(defun kill-process (name)
  "If a process named NAME is found, it is killed.
Otherwise NIL is returned."
  (let ((process (mp:find-process-from-name name)))
    (when process
      (mp:process-kill process))))

(defun debug-process (name)
  (let ((process (mp:find-process-from-name name)))
    (when process
      (mp:debug-other-process process))))
  

;; eof