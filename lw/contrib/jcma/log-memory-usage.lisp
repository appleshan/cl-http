;;; -*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-
;;;
;;; (C) Copyright 2006, John C. Mallery
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; MONITOR MEMORY USAGE
;;;

(in-package :http)

#+UNIX
(defun write-memory-usage (&optional (stream *standard-output*))
  (fast-format stream "~&Date: ~I~&~%Lisp Memory Usage~&" (write-standard-time (get-universal-time) stream))
  (let ((*standard-output* stream))
    (room)
    ;; get virtual memory size
    (multiple-value-bind (out)
        (sys:run-shell-command (format nil "ps -u ~A" (sys::getpid)) :wait nil :output :stream)
      (with-open-stream (output out)
        (http::stream-copy-until-eof output stream)))
    (format stream "~&--------------------------------------------------------------------------------~&")))

(defun log-memory-usage (pathname)
  (with-open-file (file pathname :direction :output :element-type 'character :if-does-not-exist :create :if-exists :append)
    (write-memory-usage file)))

(defvar *daily-memory-usage-log-file* #p"http:logs;memory-usage.text")

(defun daily-log-memory-usage ()
  (log-memory-usage *daily-memory-usage-log-file*))

(add-periodic-task "Log Memory Usage" :daily '(daily-log-memory-usage))

(eval-when (:load-toplevel)
  (daily-log-memory-usage))
