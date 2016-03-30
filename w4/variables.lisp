;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: (w4 :use (future-common-lisp www-utils url http)); -*-

;;; Copyright John C. Mallery,  1995-96, 2000.
;;; All rights reserved.



;;;------------------------------------------------------------------- 
;;;
;;; VARIABLES
;;;
(in-package :w4)

(defvar *activity* nil
  "Bound to the ACTIVITY controlling the current walk.")

(define-parameter *cache-url-data* t
                  "Controls whether URL data is cached or not.")

(define-parameter *debug-walker* nil
  "Controls debugging of the Web walker.")

(define-parameter *retries-on-network-error* 5)

(define-parameter *standard-get-robot-headers* '(:accept ((:* :*)) :accept ((:image :gif)) :accept ((:image :jpeg))))

(define-parameter *standard-head-robot-headers* nil)

(define-parameter *trace-constraints* nil
  "Constraints whose application to trace on the *report-stream*.
The value can be either a list of constraint names or T for all constraints.")

(define-parameter *trace-walker* nil
  "Traces the activities of the walking printing results on the *report-stream*.")

(define-parameter *user-agent* "W4 Constraint-Guided Walker"
  "The name of this Web walker.")

(define-parameter *wait-interval-before-retry* 1)

(define-variable *local-proxy* nil
		 "Holds the value of the local proxy through which the web walker operates.")

(defparameter *action-class-alist*
              '((:standard . action)
                (:open-http-stream . open-http-action)
                (:encapsulating . encapsulating-action)
                (:generator . generator))
  "An alist of action classes accessible from define-action-type.")

(defparameter *cache-indicators* '(:headers :content :header-status-code :content-status-code
                                            :redirection :http-version :url-inferiors :local-context-string))

(defparameter *constraint-class-alist*
              '((:context . context-constraint)
                (:header . header-constraint)
                (:html-body . html-body-constraint)
                (:html-head . html-head-constraint)
                (:url . url-constraint)
                (:dns-url . dns-url-constraint)
                (:circumstance . circumstance-constraint))
  "An alist of constraint classes accessible from define-constraint-type.")

(defparameter *queue-class-alist*
	      '((:depth-first . single-thread-depth-first-queue)
		(:breadth-first . single-thread-breadth-first-queue)
		(:best-first . single-thread-best-first-queue)
		#|(:multi-threaded-depth-first . multi-threaded-depth-first-queue)|#
		(:multi-thread-breadth-first . multi-threaded-breadth-first-queue)
		(:multi-thread-best-first . multi-threaded-best-first-queue)
		#|(:shared-multi-threaded-depth-first .  shared-multi-threaded-depth-first-queue)|#
		(:shared-multi-thread-breadth-first . shared-multi-threaded-breadth-first-queue)
		(:shared-multi-thread-best-first . shared-multi-threaded-best-first-queue))
  "An alist of W4 work queue classes.")

(defvar *report-stream*)

(defvar *url-stack* nil
  "Bound to the list of urls currently being walked.")