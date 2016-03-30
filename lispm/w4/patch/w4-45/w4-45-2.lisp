;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for W4 version 45.2
;;; Reason: Function (CLOS:METHOD W4::REPORT-STREAM (W4::ACTIVITY)):  handle stream objects.
;;; Function (CLOS:METHOD W4::POP-QUEUE (W4::SINGLE-THREAD-DEPTH-FIRST-QUEUE)):  restore depth-first cut-off.
;;; Function (CLOS:METHOD W4::PERFORM-ACTION (URL:HTTP-URL W4::GENERATOR W4::ACTIVITY)):  restore perform-activity depth-first call.
;;; Function W4::%DEFINE-ACTIVITY:  always provide a URI universe name.
;;; Written by JCMa, 8/10/00 18:00:35
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.58,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42, HTTP Proxy Server 5.18,
;;; HTTP Client Substrate 3.15, W4 Constraint-Guide Web Walker 45.1,
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
  "HTTP:W4;WALKER.LISP.95"
  "HTTP:W4;WALKER.LISP.96")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:W4;WALKER.LISP.95")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: w4; -*-")

(defmethod report-stream ((activity activity))
  (with-slots (report-stream) activity
    (etypecase report-stream
      (symbol (if (boundp report-stream)
		  (symbol-value report-stream)
		  *standard-output*))
      (cons (apply (first report-stream) (rest report-stream)))
      (function report-stream)
      (stream report-stream)
      #+CLIM(clim:sheet report-stream)
      #+Genera(tv:sheet report-stream))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:W4;WALKER.LISP.96")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: w4; -*-")

(defmethod pop-queue ((queue single-thread-depth-first-queue))
  (with-slots (lock queue pointer number-of-queue-entries) queue
    (with-lock-held (lock :write "Pop Queue")
      (destructuring-bind (&optional first . rest) queue
	(when (and first (= (qe-depth first) (depth)))
	  (setf queue rest)
	  (unless rest
	    (setf pointer nil))
	  (decf number-of-queue-entries)
	  first)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:W4;WALKER.LISP.96")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: w4; -*-")

(define %define-activity (name constraint-set actions
			       &key unsatisfied-actions documentation report-stream
			       user-agent operator connection-timeout life-time
			       (url-host-name-resolution :never)
			       (search-method :depth-first)
			       (threads 1)
			       predicate
			       (proxy (local-proxy))
			       proxy-port
			       uri-universe
			       (if-does-not-exist :uninterned)
			       (if-exists :error)
			       (class 'activity))
  "Program level interface for defining activities."
  (flet ((parse-operator (operator)
	   (etypecase operator
	     (string (list (parse-internet-mail-address operator)))
	     (cons (mapcar #'parse-internet-mail-address operator))
	     (mail-address (list operator))
	     (null (when http:*server-mail-address*
		     (parse-internet-mail-address http:*server-mail-address*))))))
    (let ((activity (intern-activity name
				     :if-does-not-exist if-does-not-exist
				     :if-exists if-exists
				     :class class)))
      (unless (member url-host-name-resolution '(:always :never :preferred))
	(error "URL-HOST-NAME-RESOLUTION is not one of the valid values, ~S"
	       '(:always :never :preferred)))
      (unless (assoc search-method *queue-class-alist*)
	(error "SEARCH-METHOD, ~S, is not one of the known search methods, ~{~S~^, ~}."
	       (mapcar #'car *queue-class-alist*)))
      (setf (activity-constraint-set activity) (allocate-constraint-structure constraint-set)
	    (activity-actions activity) (allocate-action-structure actions)
	    (activity-unsatisfied-actions activity) (allocate-action-structure unsatisfied-actions)
	    (activity-url-host-name-resolution activity) url-host-name-resolution
	    (activity-queue-type activity) search-method
	    (activity-best-first-predicate activity) predicate
	    (activity-number-of-threads activity) threads
	    (activity-proxy activity) (etypecase proxy
					(null nil)
					(http::proxy proxy)
					(string (http::intern-proxy proxy (or proxy-port 80) :if-does-not-exist :create)))
	    (activity-uri-universe activity) (or uri-universe name)
	    (activity-report-stream activity) (or report-stream (with-null-stream (stream) stream))
	    (activity-user-agent activity) (or user-agent (robot-version))
	    (activity-operator activity) (parse-operator operator)
	    (activity-connection-timeout activity) (if connection-timeout (* 60. connection-timeout) http:*client-timeout*)
	    (activity-life-time activity) (if life-time (* 60. life-time) http:*server-timeout*)
	    (documentation-string activity) (or documentation "Undocumented."))
      activity)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:W4;WALKER.LISP.96")
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
              (loop with constraint-set = (activity-constraint-set activity)
                    for item in candidates
                    do (when (and (satisfies-context-constraints-p item activity)
                                  (satisfies-p item activity constraint-set))
                         (push-queue item queue activity depth parent-stack t :pending))))
          ;; execution of the queue here replicates the earlier depth-first enumeration.
          (typecase queue
            (single-thread-depth-first-queue
	      (execute-pending-tasks queue))))))))

