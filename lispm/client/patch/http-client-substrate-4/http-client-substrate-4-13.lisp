;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for HTTP-CLIENT-SUBSTRATE version 4.13
;;; Reason: Function HTTP::WITH-CURRENT-CONNECTION:  add block-name argument.
;;; Function (CLOS:METHOD HTTP:INVOKE-HTTP-SERVICE (HTTP::BASIC-CLIENT-MIXIN T T T)):  update.
;;; Function HTTP::HANDLE-CLIENT-CONNECTION-CONDITION:  -
;;; Written by JCMa, 5/11/01 22:40:01
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.132,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.4, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.26, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Lock Simple Patches 1.0, Jcma 43,
;;; HTTP Proxy Server 6.22, HTTP Client Substrate 4.12, Statice Server 466.2,
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.11),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1).

(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:CLIENT;CONNECTION.LISP.86"
  "HTTP:CLIENT;CLIENT.LISP.282")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CONNECTION.LISP.86")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(defun handle-client-connection-condition (condition)	;fails to distinguish server errors in proxy case 9/20/2000 -- JCMa.
  (declare (special .incomplete-transaction-p.))
  (etypecase condition				;assure proper log entries, use *client* to avoid consing
    (reportable-condition
      (setf (client-status *client*) (status-code condition))
      (cond ((http-close-connection-p condition)
	     (setf (connection-close-p *connection*) t))
	    ((connection-close-p *connection*))
	    (t (setq .incomplete-transaction-p. nil))))
    ;; Detect dropped connections
    ((or connection-closed connection-lost host-stopped-responding protocol-timeout
	 #+Genera network-stream-closed #+Genera connection-no-more-data)
     (setf (client-status *client*) 408
	   (connection-close-p *connection*) t))
    ((or #+Genera unknown-address unknown-host-name domain-resolver-error connection-refused)	;detect bad gateways
     (setf (client-status *client*) 502
	   (connection-close-p *connection*) t)
     (bug-report-client-error condition))
    (error
      (setf (client-status *client*) 500
	    (connection-close-p *connection*) t)	;otherwise report an error
      (bug-report-client-error condition))
    (condition
      (setf (connection-close-p *connection*) t)))
  nil)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CONNECTION.LISP.86")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

;; this macro needs to handle the fencepost case where the connection goes
;; down before we get data back from the client.  otherwise we will get proxy
;; or client errors when we try to use a connection that has gone down.
;; 3/10/2000 -- JCMa.
(define-macro with-current-connection ((client &key block-name (http-version '*client-http-version*) (connection-var 'connection)) &body body)
  "Establishes the current connection context and maintains consistent connections."
  `(flet ((handle-dead-connection (condition)
	    (declare (ignore condition))
	    (when (zerop (connection-bytes-received *connection*))	;must be the client because a server would have made a request.
	      (throw 'get-new-connection :retry))))
     (let ((remaining-tries *client-retries-for-unresponsive-hosts*)
	   (.incomplete-transaction-p. t)
	   ,connection-var fresh-connection-p)
       (declare (special .incomplete-transaction-p.))
       (loop doing (catch 'get-new-connection
		     (multiple-value-setq (,connection-var fresh-connection-p)
		       (ensure-client-connection ,client ,http-version))
		     (let* ((*connection* ,connection-var))
		       (declare (special *connection*))
		       (unwind-protect
			   (return (multiple-value-prog1
				     (handler-bind
				       ((host-stopped-responding #'handle-dead-connection)
					(condition #'handle-client-connection-condition))
				       ,(if block-name
					    `(block ,block-name . ,body)
					    `(progn . ,body)))
				     ;; ensure that logging has the correct number
				     (incf (the fixnum (connection-requests-completed ,connection-var)))
				     (setq .incomplete-transaction-p. nil)))
			 ;; Handle client logging here
			 (client-log-request ,client)
			 ;; When an HTTP transaction fails to terminate normally or it
			 ;; needs to be closed, do it here. Otherwise, recheck on next
			 ;; time around the loop in ensure-client-connection.
			 (when (or .incomplete-transaction-p. (connection-close-p ,connection-var))
			   (deallocate-connection ,client)))))
	     when fresh-connection-p do (decf remaining-tries)
	     until (zerop remaining-tries)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CLIENT.LISP.282")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod invoke-http-service ((client basic-client-mixin) method header-writer response-handler
				&optional request-entity-generator (http-version *client-http-version*))
  (flet ((trace-request (url method version header-writer &aux (trace-stream *trace-output*))
	   (let ((proxy (client-proxy client)))
	     (if proxy
		 (fast-format trace-stream "~&Proxy: ~A (~D)" (proxy-host proxy) (proxy-port proxy))
		 (fast-format trace-stream "~&Host: ~A" (host-string url)))
	     (fast-format trace-stream "~%Request: ~A ~I ~A~%Request Headers: ~I" 
			  method (url:write-name url trace-stream) version
			  (%write-request-headers trace-stream header-writer method version)))))
    (with-standard-client-io-syntax ()
      (with-current-connection (client :http-version http-version :connection-var connection :block-name invoke-http-service)
	(multiple-value-bind (connection-version connection-version-confirmed-p http-plist)
	    (connection-confirmed-version connection)
	  (let ((url (client-url client))
		(request-version (if connection-version-confirmed-p (http-version-min connection-version http-version) connection-version))
		(stream (connection-stream connection)))
	    (setf (client-method client) method
		  (%client-request-version client) request-version)
	    ;; send a request to the remote host
	    (send-request stream method url request-version header-writer (client-proxy client))
	    (when *trace-client* (trace-request url method request-version header-writer))
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

