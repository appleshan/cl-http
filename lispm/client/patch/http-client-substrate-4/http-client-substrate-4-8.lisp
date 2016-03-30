;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for HTTP-CLIENT-SUBSTRATE version 4.8
;;; Reason: Handle lower protocal version requests to higher version servers properly.
;;; 
;;; Function (CLOS:METHOD HTTP:INVOKE-HTTP-SERVICE (HTTP::BASIC-CLIENT-MIXIN T T T)):
;;; Distringuish request version from connection version.  
;;; CLOS class HTTP::CLIENT-TRANSACTION-INFO-MIXIN:  new instance variable request-version
;;; Function HTTP::CLIENT-REQUEST-VERSION:  new abstraction.
;;; Function (CLOS:METHOD HTTP::CLIENT-REQUEST-VERSION (HTTP::CLIENT-TRANSACTION-INFO-MIXIN)):  implement.
;;; Function (CLOS:METHOD HTTP::DEINITIALIZE-RESOURCED-CLIENT (T HTTP::CLIENT-TRANSACTION-INFO-MIXIN) :AFTER):  -
;;; Written by JCMa, 2/28/01 15:51:45
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.110,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.25, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.1,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Jcma 42, HTTP Proxy Server 6.14,
;;; HTTP Client Substrate 4.7, Statice Server 466.2,
;;; W4 Constraint-Guide Web Walker 45.9, HTTP Client 50.2, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.7),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1),
;;; Ansi common lisp as synonym patch (from W:>reti>ansi-common-lisp-as-synonym-patch.lisp.9),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).

(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:CLIENT;CLIENT.LISP.277")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CLIENT.LISP.277")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defclass client-transaction-info-mixin 
	  ()
    ((reply-line :initform nil :initarg :reply-line :accessor client-reply-line)
     (reason-offset :initform 0 :initarg :reason-offset :accessor client-reason-offset)
     (reason :initform nil :initarg :reason :accessor %client-reason)
     (request-headers :initform nil :initarg :headers :accessor client-request-headers)
     (response-headers :initform nil :initarg :response-headers :accessor client-response-headers)
     (status :initform 408 :initarg :status :accessor client-status)	;default is dropped connection
     (request-version :initform nil :initarg :headers :accessor %client-request-version))	;HTTP version used in the transaction
  (:documentation "Information about the current http transaction."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CLIENT.LISP.277")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(define-generic client-connection-version (client)
  (:documentation "Returns the HTTP version keyword for the client's current HTTP connection.
This is the version of the server connected on the other end."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CLIENT.LISP.277")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(define-generic client-request-version (client)
  (:documentation "Returns the HTTP version keyword for the client's current HTTP connection.
This is the actual HTTP version that the client and server are using to communicate.
It can be lower than the connection version when the client explicitly issues its
request at a lower HTTP level."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CLIENT.LISP.277")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod client-request-version ((client client-transaction-info-mixin))
  (or (%client-request-version client)
      (setf (%client-request-version client) (client-connection-version client))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CLIENT.LISP.277")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod deinitialize-resourced-client :after (resource (client client-transaction-info-mixin))
  (declare (ignore resource)) 
  ;; deallocate resourced headers
  (let ((reply-line (client-reply-line client))
	(header-set (client-response-headers client)))
    (cond-every
      (reply-line 
	(setf (fill-pointer (client-reply-line client)) 0))
      (header-set
	(clear-header-set (client-response-headers client) t))))
  ;;reset instance variables
  (setf (%client-reason client) nil
	(client-reason-offset client) 0
	(client-request-headers client) nil          
	(client-status client) 408
	(%client-request-version client) nil)
  
  client)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CLIENT.LISP.277")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod invoke-http-service ((client basic-client-mixin) method header-writer response-handler
				&optional request-entity-generator (http-version *client-http-version*))
  (flet ((trace-request (url method version &aux (trace-stream *trace-output*))
	   (let ((proxy (client-proxy client)))
	     (if proxy
		 (fast-format trace-stream "~&Proxy: ~A (~D) ~%Request: ~A ~I ~A" 
			      (proxy-host proxy) (proxy-port proxy) method (url:write-name url trace-stream) version)
		 (fast-format trace-stream "~&Host: ~A~%Request: ~A ~I ~A"
			      (host-string url) method (url:write-local-name url trace-stream) version)))))
    (with-standard-client-io-syntax ()
      (with-current-connection (client :http-version http-version :connection-var connection)
	(multiple-value-bind (connection-version connection-version-confirmed-p http-plist)
	    (connection-confirmed-version connection)
	  (let ((url (client-url client))
		(request-version (if connection-version-confirmed-p (http-version-min connection-version http-version) connection-version))
		(stream (connection-stream connection)))
	    (setf (client-method client) method
		  (%client-request-version client) request-version)
	    ;; send a request to the remote host
	    (send-request stream method url request-version header-writer (client-proxy client))
	    (when *trace-client* (trace-request url method request-version))
	    (when request-entity-generator	;Send the request body when provided
	      (case request-version
		((:http/1.0 :http/0.9)		;1.0 remote server, just send the data.
		 (funcall request-entity-generator client stream request-version)
		 (force-output stream))
		(t (let (100-continue)
		     (multiple-value-bind (reply reply-length)
			 (cond (connection-version-confirmed-p
				(ecase (setq 100-continue (getf http-plist :100-continue :unknown))
				  (:implemented
				    (read-reply-line stream (client-reply-line client)))
				  (:unknown
				    (with-timeout (*client-await-continue-reply-timeout* :error-p nil)
				      (read-reply-line stream (client-reply-line client))))
				  (:not-implmented
				    (values nil nil))))
			       (t (with-timeout (*client-await-continue-reply-timeout* :error-p nil)
				      (read-reply-line stream (client-reply-line client)))))
		       (cond (reply
			      (multiple-value-bind (status server-version reason-offset)
				  (parse-reply reply reply-length)
				(unless connection-version-confirmed-p
				  (note-server-http-version connection server-version)
				  (setq connection-version-confirmed-p t))
				(setf (client-status client) status
				      (client-reason-offset client) reason-offset)
				(with-headers-for-client (client stream request-version)	; handles update of connection version
				  (case status
				    (100
				      (funcall request-entity-generator client stream request-version)
				      (force-output stream)
				      (unless (eq 100-continue :implemented)
					(note-server-http-property connection :100-continue :implemented))
				      (clear-header-set (client-response-headers client) t))	; clear continue headers, if any
				    (417 (signal 'expectation-failed :url url :version server-version))
				    ;; Request was rejected. Handle the rejection response
				    (t (return-from invoke-http-service
					 (multiple-value-prog1	;must return the values returned by the handler
					   (funcall response-handler client stream request-version))))))))
			     (t (funcall request-entity-generator client stream request-version)
				(force-output stream)
				(unless (eq 100-continue :not-implmented)
				  (note-server-http-property connection :100-continue :not-implmented)))))))))
	    (tagbody				;Handle the primary server response
	      read-next-reply
		 (multiple-value-bind (reply reply-length)
		     (read-reply-line stream (client-reply-line client))
		   (multiple-value-bind (status server-version reason-offset)
		       (parse-reply reply reply-length)
		     (unless (and connection-version-confirmed-p (eq server-version connection-version))	;detect version changes
		       (note-server-http-version connection server-version))
		     (setf (client-status client) status
			   (client-reason-offset client) reason-offset)
		     (with-headers-for-client (client stream request-version)	; handles update of connection version
		       (case status
			 (100 (go read-next-reply))
			 (417 (signal 'expectation-failed :url url :version server-version))
			 (t (return-from invoke-http-service
			      (multiple-value-prog1	;must return the values returned by the handler
				(funcall response-handler client stream request-version)))))))))))))))

