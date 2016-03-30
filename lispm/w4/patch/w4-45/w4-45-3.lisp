;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: w4; Base: 10; Patch-File: t -*-
;;; Patch file for W4 version 45.3
;;; Reason: Function W4::%SATISFIES-CONTRAINT-P:  abstract.
;;; Function (CLOS:METHOD W4:SATISFIES-P (T W4::ACTIVITY W4:CONTEXT-CONSTRAINT)):  -
;;; Function (CLOS:METHOD W4:SATISFIES-P (URL:URL W4::ACTIVITY W4:CONSTRAINT)):  -
;;; Remove function (CLOS:METHOD W4::SATISFIES-CONTEXT-CONSTRAINTS-P (URL:HTTP-URL W4::ACTIVITY)): -
;;; Function (CLOS:METHOD W4::SATISFIES-CONTEXT-CONSTRAINTS-P (T W4::ACTIVITY)):  -
;;; DEFINE-ACTION-TYPE W4::GENERATE-INFERIORS:  don't generate inferiors unless context constraints hold.
;;; DEFINE-ACTION-TYPE W4::GENERATE-SORTED-INFERIORS:  ditto.
;;; Function (CLOS:METHOD W4::PERFORM-ACTION (URL:HTTP-URL W4::GENERATOR W4::ACTIVITY)):  one application of context contraints for whole set.
;;; Remove function W4::URL-INFERIORS-SATISFYING-ACTIVITY: -
;;; Function W4::URL-INFERIORS-SATISFYING-ACTIVITY:  -
;;; Function (CLOS:METHOD W4::URL-INFERIORS-SATISFYING-ACTIVITY (URL:HTTP-URL W4::ACTIVITY)):  -
;;; Written by JCMa, 8/16/00 16:23:17
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.60,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42, HTTP Proxy Server 5.18,
;;; HTTP Client Substrate 3.15, W4 Constraint-Guide Web Walker 45.2,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.11,
;;; DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x994 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number -2141189585,
;;; Domain Fixes (from CML:MAILER;DOMAIN-FIXES.LISP.33),
;;; Don't force in the mail-x host (from CML:MAILER;MAILBOX-FORMAT.LISP.24),
;;; Make Mailer More Robust (from CML:MAILER;MAILER-FIXES.LISP.15),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add CLIM presentation and text style format directives. (from FV:SCLC;FORMAT.LISP.20),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; Make update schema work on set-value attributes with accessor names (from CML:LISPM;STATICE-SET-VALUED-UPDATE.LISP.1),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.107),
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:W4;WALKER.LISP.98"
  "HTTP:W4;ACTIONS.LISP.10")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:W4;WALKER.LISP.98")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: w4; -*-")

(declaim (inline %satisfies-contraint-p))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:W4;WALKER.LISP.98")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: w4; -*-")

(defun %satisfies-contraint-p (url activity constraint)
  (let (type function result trace)
    (cond ((setq type (constraint-type constraint))
	   (unless (setq function (constraint-type-function type))
	     (error "No constraint function available for ~S." type))
	   (setq result (handler-case
			  (apply function constraint activity url (constraint-arguments constraint))
			  ;; This should log the problem for possible retry 
			  (unknown-host-name () nil)
			  (network-error () nil)))
	   (when (and (setq trace *trace-constraints*)
		      (or (eql trace t)
			  (member (constraint-type-name type) trace :test #'string-equal)))
	     (trace-report (report-stream activity) "(~A~{ ~A~}) => ~S ~40T~S"
			   (constraint-type-name type) (constraint-arguments constraint) result (name-string url)))
	   result)
	  (t (error "No constraint type available for ~S." constraint)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:W4;WALKER.LISP.98")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: w4; -*-")

(defmethod satisfies-p (url (activity activity) (constraint context-constraint))
  (%satisfies-contraint-p url activity constraint))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:W4;WALKER.LISP.98")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: w4; -*-")

(defmethod satisfies-p ((url url:url) (activity activity) (constraint constraint))
  (%satisfies-contraint-p url activity constraint))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:W4;WALKER.LISP.98")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: w4; -*-")

(SCL:FUNDEFINE '(METHOD SATISFIES-CONTEXT-CONSTRAINTS-P (HTTP-URL ACTIVITY)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:W4;WALKER.LISP.98")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: w4; -*-")

(defmethod satisfies-context-constraints-p (url (activity activity))
  (with-slots (constraint-set) activity
    (with-slots (context-constraints sorted-p) constraint-set
      ;; ensure that constraints are always sorted.
      (unless sorted-p (sort-constraints constraint-set))
      (loop for constraint in context-constraints
            unless (satisfies-p url activity constraint)
              do (return-from satisfies-context-constraints-p nil)
            finally (return-from satisfies-context-constraints-p t)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:W4;ACTIONS.LISP.10")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: w4; -*-")

;;;------------------------------------------------------------------- 
;;;
;;; ACTIONS
;;;

(define-action-type
  generate-inferiors
  (:generator
    :documentation "Primary action for walking the inferiors of a URL."
    :class generator-type)
  (action activity url)
  (declare (ignore action))
  (when (satisfies-context-constraints-p t activity)
    (url-inferiors activity url)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:W4;WALKER.LISP.98")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: w4; -*-")

(defmethod perform-action ((url url:http-url) (action generator) (activity activity))
  (flet ((generate-candidates (url action activity &aux fctn)
           (with-slots (type arguments) action
             (if (setq fctn (action-type-function (or type (error "No action type available for ~S." action))))
                 (apply fctn action activity url arguments)
                 (error "No action function available for ~S." type)))))
    (declare (inline generate-candidates))
    (multiple-value-bind (candidates constraints-applied-p)
        (generate-candidates url action activity)
      (when candidates
        (let* ((queue (activity-queue activity))
               (parent-stack (parent-stack queue))
               (depth *depth*))
          (if constraints-applied-p
              (loop for item in candidates
                    do (push-queue item queue activity depth parent-stack t :pending))
              (let ((constraint-set (activity-constraint-set activity)))
		(when (satisfies-context-constraints-p t activity)
		  (dolist (item candidates)
		    (when (satisfies-p item activity constraint-set)
		      (push-queue item queue activity depth parent-stack t :pending))))))
          ;; execution of the queue here replicates the earlier depth-first enumeration.
          (typecase queue
            (single-thread-depth-first-queue
	      (execute-pending-tasks queue))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:W4;WALKER.LISP.98")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: w4; -*-")

(SCL:FUNDEFINE 'URL-INFERIORS-SATISFYING-ACTIVITY)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:W4;WALKER.LISP.98")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: w4; -*-")

(define-generic url-inferiors-satisfying-activity (url activity &optional context-constraints-applied-p))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:W4;WALKER.LISP.98")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: w4; -*-")

(defmethod url-inferiors-satisfying-activity ((url url:http-url) (activity activity) &optional context-constraints-applied-p)
  (when (or context-constraints-applied-p (satisfies-context-constraints-p t activity))
    (loop with constraint-set = (activity-constraint-set activity)
	  for inferior in (url-inferiors activity url)
	  when (satisfies-p inferior activity constraint-set)
	    collect inferior)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:W4;ACTIONS.LISP.10")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: w4; -*-")

(define-action-type
  generate-sorted-inferiors
  (:generator
    :documentation "Primary action for walking the inferiors of a URL."
    :class generator-type)
  (action activity url predicate)
  (declare (ignore action))
  (when (satisfies-context-constraints-p t activity)
    (values (stable-sort (url-inferiors-satisfying-activity url activity t) predicate)
	    t)))

