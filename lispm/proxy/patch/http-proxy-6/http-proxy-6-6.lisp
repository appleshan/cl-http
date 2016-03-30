;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for HTTP-PROXY version 6.6
;;; Reason: Function (CLOS:METHOD HTTP::DATABASE-CLOSE (T HTTP::FILESYSTEM-HANDLE (EQL :OUTPUT)) :AFTER):  more care on invalid files.
;;; Function (CLOS:METHOD HTTP::SAVE-METADATA-PERSISTENTLY (HTTP::FILESYSTEM-DATABASE HTTP::REPRESENTATION T) :AROUND):  grab read lock to ensure coherent metadata save.
;;; Written by JCMa, 12/05/00 17:34:17
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.90,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.0,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Lock Simple Patches 1.0, Jcma 42,
;;; HTTP Proxy Server 6.5, HTTP Client Substrate 4.1, Statice Server 466.2,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.16,
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
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.6),
;;; Cname level patch (from W:>reti>cname-level-patch).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:PROXY;DATABASE.LISP.85"
  "HTTP:PROXY;REPRESENTATION.LISP.62")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;DATABASE.LISP.85")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(PROGN
;;file-stream-length-in-bytes on a stream doesn't work in Genera   5/9/2000 -- JCMa.
#+Genera
(defmethod database-close :after (stream (handle filesystem-handle) (direction (eql :output)) &optional abort-p)
  (declare (ignore stream))
  (if abort-p
      (handle-valid-p handle t)
      (let ((pathname (filesystem-handle-pathname handle)))
	(handler-case 
	  (setf (%handle-object-size handle) (file-length-in-bytes pathname))
	  (file-error () (setf (%handle-object-size handle) (if (handle-valid-p handle t) (file-length-in-bytes pathname) 0)))))))

)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;REPRESENTATION.LISP.62")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod save-metadata-persistently :around ((database filesystem-database) (representation representation) mode)
  (tagbody retry
	   (restart-case
	     (with-lock-held ((cache-object-lock representation) :read "Save MetaData")
	       (when (representation-valid-p representation)
		 (let ((*package* *http-package*)	;relative to HTTP package
		       (*print-radix* nil)	;prevents writing integers to strings with a trailing decimal point
		       (*print-base* 10)	;always in base 10
		       (*print-escape* t)	;readable strings
		       (*print-readably* t))	;readable output
		   (call-next-method database representation mode))))
	     (retry ()
		    :report (lambda (stream) 
			      (format stream "Retry saving metadata to ~A" (representation-metadata-pathname representation)))
		    (go retry))
	     (skip ()
		   :report (lambda (stream) 
			     (format stream "Skip saving metadata to ~A" (representation-metadata-pathname representation)))
		   (return-from save-metadata-persistently nil))))
  ;; update the size of the metadata
  (let* ((handle (representation-metadata-handle representation))
	 (size (handle-compute-object-size handle)))
    (setf (%handle-object-size handle) size
	  (representation-metadata-size representation) size)
    (decache-object-size representation)
    ;; Clean up excess file versions in Genera
    #+Genera (handle-database-hygine handle)
    #+Genera (handle-database-hygine (representation-entity-handle representation)))
  representation)

