;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.75
;;; Reason: Function HTTP::INTERN-ACCESS-LOG:  clean up
;;; Function HTTP::CREATE-NOTIFICATION-ACCESS-LOG:  -
;;; Function HTTP::PARSE-EXPIRES-HEADER:  explicitly handle zero-valued expires.
;;; Function HTTP::PRINT-EXPIRES-HEADER:  -
;;; DEFINE-HEADER :EXPIRES:  update.
;;; Written by JCMa, 9/18/00 17:57:07
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.74,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Documentation Patches 1.0,
;;; Genera 8 5 Clim Patches 1.0, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; HTTP Client Substrate 3.22, Jcma 42, HTTP Proxy Server 5.27, HTTP Client 49.10,
;;; Image Substrate 440.4, Essential Image Substrate 433.0,
;;; W4 Constraint-Guide Web Walker 45.4, W4 Examples 15.0, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.11, DEC OSF/1 V4.0 (Rev. 110),
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
;;; Domain ad host patch (from W:>Reti>domain-ad-host-patch.lisp.3),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.5),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).

(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;LOG.LISP.195"
  "HTTP:SERVER;HEADERS.LISP.475")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.195")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(define intern-access-log (name &key (port *standard-http-port*)
                                (if-does-not-exist :error)
                                (directory *standard-log-directory*)
                                host
                                (class *log-access-log-class*))
  "Interns a server access log named, NAME, that monitors port, PORT.
If port is null, this returns any log whose name and class match.
If port is :MULTIPORT, it returns a multiport log."
  (declare (values log newly-created-p))
  (flet ((equal-log-p (x)
           (and (eq port (log-port x))
		(equalp name (log-name x))
                (typep x class)))
	 (handle-does-not-exist (name port class host)
	   (ecase if-does-not-exist
	     (:soft nil)
	     (:create
	       (let* ((*standard-log-directory* directory)
		      (log (allocate-log :name name :port port :class class :local-host (or host (local-host)))))
		 (when port (add-access-log log port))
		 (values log t)))
	     (:error (error "Unknown HTTP server access log, ~A~:[~:*, for port ~D~]." name port)))))
    (declare (dynamic-extent #'equal-log-p))
    (etypecase name
      (string
        (cond
          ((find-if #'equal-log-p *all-logs*))
          (t (handle-does-not-exist name port class host))))
      (basic-log-mixin
	(if (and (or (null port) (eql port (log-port name)))
		 (typep name class))
	    name
	    (handle-does-not-exist (log-name name) port class host))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.195")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(define create-notification-access-log (predicate notifier &key (name "Notification-Log")
                                                  (port *standard-http-port*)
                                                  (class 'custom-notification-log))
  (check-type predicate function)
  (check-type notifier function)
  (multiple-value-bind (log newly-created-p)
      (intern-access-log (or name (default-log-file-name port class))
			 :port port
			 :directory *standard-log-directory*
			 :class class
			 :if-does-not-exist :create)
    (when newly-created-p
      (setf (notification-log-notifier log) notifier
	    (notification-log-predicate log) predicate))
    (log-notifications-on log t)
    (add-access-log log port)
    (values log newly-created-p)) )


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.475")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun parse-expires-header (string &optional (start 0) (end (length string)))
  (flet ((handle-zero-valued-expiration (string start end)
	   (with-fast-array-references ((string string))
	     (loop for idx upfrom start below end
		   for char = (aref string idx)
		   unless (member char '(#\space #\0))
		     do (return-from parse-expires-header 0)))))
    (declare (inline handle-zero-valued-expiration))
    (handle-zero-valued-expiration string start end)
    (handler-case-if (not *debug-server*)
       (parse-date-header string start end)
      (error () 0))))				;HTTP 1.1 spec 14.21 requires errors parsing expires to be treated as expired 


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.475")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun print-expires-header (universal-time stream)
  (if (zerop universal-time)
      (write-char #\0 stream)
      (print-gmt-time stream universal-time)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.475")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :expires
               (:date-header :entity)
  :print-string "Expires"
  :parse-function 'parse-expires-header
  :print-function 'print-expires-header
  :atomic-value-reducer 'header-value-min)

