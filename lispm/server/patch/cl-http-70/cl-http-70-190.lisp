;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.190
;;; Reason: Function HTTP::%PROCESS-REQUEST:  No persistent conenction checks or force-output on dead connection.
;;; Written by JCMa, 12/11/03 18:22:03
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5-inc1.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.6,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.189,
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
  "HTTP:SERVER;SERVER.LISP.933")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.933")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun %process-request (server stream)
  (declare (values persistent-connection-p)
	   (optimize (speed 3)))
  (labels ((preview-condition (condition)
	     (when *debug-server*
               (break (report-string condition)))
	     nil)
	   (handle-http-condition (condition stream server status-code)
             (case *debug-server*
               (:conditions (break (report-string condition))))
	     (setf (server-status server) status-code)
	     (when (close-connection-p condition) 
	       (setf (server-close-connection-p server) t))
	     (typecase condition
	       ((or network-error request-timeout))
	       (t (handler-case
		    (report-status condition stream)
		    (network-error () nil)
		    (error (err) (bug-report-error err) nil))))
	     (throw 'exit-http-server nil))	;closes connection
	   (handle-reportable-condition (condition)
	     (handle-http-condition condition stream server (status-code condition)))
	   (handle-parsing-error (error)
	     (handle-http-condition error stream server 400))
	   (report-error (error)
	     (typecase error
	       ((or http-condition network-error condition))
	       (t (bug-report-error error)))
	     nil))
    (declare (dynamic-extent #'handle-reportable-condition #'handle-parsing-error))

    (handler-bind
      ((error #'preview-condition)		; MCL 4.1 loses when testing CONDITION here. -- JCMa 7/24/1997.
       (reportable-condition #'handle-reportable-condition)
       (url:parsing-error #'handle-parsing-error)
       (error #'report-error))
      (catch 'exit-http-server
	(multiple-value-bind (request eof delimiter)
	    (read-delimited-line stream '(#\Linefeed #\Return) t (%server-request server))
	  delimiter				;ignore
	  ;; http-version not set, but will default via server-http-version
	  (when eof
	    (error 'request-timeout :format-string "Client dropped connection while reading request line."))
	  (setf (%server-request server) request	;capture in case of growth
		(server-request-time server) (get-universal-time))	;set the request time
	  (%execute-request server request stream))))
    ;; Epilogue
    (prog1 (when (live-connection-p stream)	; Dead connection returns NIL 12/11/2003 -- JCMa.
	     (let ((persistent-connection-p (and (server-persistent-connection-p server)	;set by positive predicate
						 (not (server-close-connection-p server)))))	;errors may set this
	       ;; Don't force output if there is incoming data: pipeline responses 
	       (unless (and persistent-connection-p (http-input-data-available-p stream nil))
		 (force-output stream))		;force output while deciding what to do next
	       persistent-connection-p))	;return whether to continue reading requests from the stream
	   ;; Count completed requests
	   (incf (server-requests-completed server)))))

