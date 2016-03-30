;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for W4 version 45.14
;;; Reason: Function W4::%DEFINE-ACTIVITY:  fix error string.
;;; Written by jcma, 8/20/05 22:30:54
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/Plus-CL-HTTP-A-CSAIL-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, Metering 444.0,
;;; Metering Substrate 444.1, Conversion Tools 436.0, Hacks 440.0, CLIM 72.0,
;;; Genera CLIM 72.0, CLX CLIM 72.0, PostScript CLIM 72.0, CLIM Demo 72.0,
;;; CLIM Documentation 72.0, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.40, Genera 8 5 Macivory Support Patches 1.0,
;;; Genera 8 5 Metering Patches 1.0, Genera 8 5 Joshua Patches 1.0,
;;; Genera 8 5 Jericho Patches 1.0, Genera 8 5 Joshua Doc Patches 1.0,
;;; Genera 8 5 Joshua Metering Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.3,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clx Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Clim Demo Patches 1.0, Genera 8 5 Color Patches 1.1,
;;; Genera 8 5 Images Patches 1.0, Genera 8 5 Color Demo Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; Genera 8 5 Concordia Patches 1.2, Genera 8 5 Concordia Doc Patches 1.0,
;;; Genera 8 5 C Patches 1.0, Genera 8 5 Pascal Patches 1.0,
;;; Genera 8 5 Fortran Patches 1.0, MAC 415.2, MacIvory Support 447.0,
;;; MacIvory Development 434.0, Color 427.1, Graphics Support 431.0,
;;; Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Color System Documentation 10.0, SGD Book Design 10.0, Color Demo 422.0,
;;; Images 431.2, Image Substrate 440.4, Statice Runtime 466.1, Statice 466.0,
;;; Statice Browser 466.0, Statice Server 466.2, Statice Documentation 426.0,
;;; Symbolics Concordia 444.0, Graphic Editor 440.0, Graphic Editing 441.0,
;;; Bitmap Editor 441.0, Graphic Editing Documentation 432.0, Postscript 436.0,
;;; Concordia Documentation 432.0, Joshua 237.6, Joshua Documentation 216.0,
;;; Joshua Metering 206.0, Jericho 237.0, C 440.0, Lexer Runtime 438.0,
;;; Lexer Package 438.0, Minimal Lexer Runtime 439.0, Lalr 1 434.0,
;;; Context Free Grammar 439.0, Context Free Grammar Package 439.0, C Runtime 438.0,
;;; Compiler Tools Package 434.0, Compiler Tools Runtime 434.0, C Packages 436.0,
;;; Syntax Editor Runtime 434.0, C Library Headers 434,
;;; Compiler Tools Development 435.0, Compiler Tools Debugger 434.0,
;;; C Documentation 426.0, Syntax Editor Support 434.0, LL-1 support system 438.0,
;;; Fortran 434.0, Fortran Runtime 434.0, Fortran Package 434.0, Fortran Doc 427.0,
;;; Pascal 433.0, Pascal Runtime 434.0, Pascal Package 434.0, Pascal Doc 427.0,
;;; HTTP Server 70.202, Showable Procedures 36.3, Binary Tree 34.0,
;;; Experimental W3 Presentation System 8.1, CL-HTTP Server Interface 54.0,
;;; HTTP Proxy Server 6.34, HTTP Client Substrate 4.23,
;;; W4 Constraint-Guide Web Walker 45.13, HTTP Client 51.9, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.18, DEC OSF/1 V4.0 (Rev. 110),
;;; 1728x985 24-bit TRUE-COLOR X Screen FUJI:1.0 with 224 Genera fonts (The Olivetti & Oracle Research Laboratory R3323),
;;; Machine serial number -2142637960,
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7),
;;; Get Xauthority pathname from user namespace object. (from W:>jcma>fixes>xauthority-pathname.lisp.2),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:W4;WALKER.LISP.109")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:W4;WALKER.LISP.109")
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
               search-method
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

