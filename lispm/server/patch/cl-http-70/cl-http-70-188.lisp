;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.188
;;; Reason: Remove function (CLOS:METHOD HTTP::REPORT-STATUS-UNHANDLED-ERROR (CONDITION T
;;; T)): undefine because duplicates functionality inherited from
;;; http-reportable-condition
;;; Function (CLOS:METHOD HTTP::REPORT-STATUS (URL:PARSING-ERROR T) :AROUND):  Doesn't inherit from reportable condition
;;; Function (CLOS:METHOD HTTP::REPORT-STATUS (CONDITION T) :AROUND):  move upto condition for good measure.
;;; Remove function (CLOS:METHOD HTTP::REPORT-STATUS (HTTP::HTTP-CONDITION T) :AROUND): undefine.
;;; Written by JCMa, 12/01/03 20:52:16
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5-inc1.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.6,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.187,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.4, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.5, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.41, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Documentation Patches 1.0,
;;; Genera 8 5 Clim Patches 1.3, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; Jcma 44, HTTP Client 51.7, HTTP Client Substrate 4.23, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, HTTP Proxy Server 6.34,
;;; W4 Constraint-Guide Web Walker 45.13, W4 Examples 15.0,
;;; Experimental CL-HTTP CLIM User Interface 1.1, CL-HTTP Documentation 3.0,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.18,
;;; DEC OSF/1 V4.0 (Rev. 110),
;;; 1585x1133 1-bit STATIC-GRAY X Screen RELATUS:0.0 with 224 Genera fonts (eXodus 8.0  (c) 2001 Powerlan USA,
;;; Inc. R7100),
;;; 1152x696 24-bit TRUE-COLOR X Screen FUJI:2.0 with 224 Genera fonts (The Olivetti & Oracle Research Laboratory R3323),
;;; Machine serial number -2141194968,
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;REPORT.LISP.189")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;REPORT.LISP.189")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(SCL:FUNDEFINE '(METHOD REPORT-STATUS-UNHANDLED-ERROR (CONDITION T T)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;REPORT.LISP.189")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

;; Doesn't inherit from reportable condition
(defmethod report-status :around ((condition url:parsing-error) stream)
  (when (www-utils:live-connection-p stream)
    (call-next-method)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;REPORT.LISP.189")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

;; Catch any errors reporting errors.  Errors that re-enter this code
;; are sure losers attempting to resignal within the process of
;; signalling the original error.  These cases are detected and handled
;; in a safe manner. 9/22/99 -- JCMa.
;; D,#TD1PsT[Begin using 006 escapes](1 0 (NIL 0) (NIL :ITALIC NIL) "CPTFONTI")Move this up to the highest condition to catch anything -- JCMa 12/1/2003
0(defmethod report-status :around ((condition condition) stream)
  (flet ((report-rentrant-error (primary-error secondary-error)
           (let ((secondary-error-type (type-of secondary-error)))
             (report-bug *bug-http-server*
                         (format nil "REPORT-STATUS Re-entrant Error: ~S" secondary-error-type)
                         "~:[~;~&Log: ~:*~S~]~&Primary Error: ~S~:[~;~&Primary Error Report: ~:*~A~]~
                          ~&Secondary Error: ~S~:[~;~&Secondary Error Report: ~:*~A~]~:[~;~&Backtrace: ~:*~A~]"
                         (when http:*server* (write-extended-common-logfile-entry http:*server* nil))
                         (type-of primary-error) (report-string primary-error) (type-of secondary-error) (report-string secondary-error)
                         (when *stack-backtraces-in-bug-reports* (stack-backtrace-string condition))))))
    (cond (*report-status-condition*
           (report-rentrant-error *report-status-condition* condition))
          (t (let ((*report-status-condition* condition))
               (handler-case-if (not *debug-server*)
                   (with-text-stream (stream :output)	;in case we're in binary mode
                     (call-next-method condition stream))
                 (network-error () nil)         ;no need to report errors telling the user he lost
                 (condition (cond) (bug-report-error cond))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;REPORT.LISP.189")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(SCL:FUNDEFINE '(METHOD REPORT-STATUS (HTTP-CONDITION T) :AROUND))
