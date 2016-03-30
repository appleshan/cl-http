;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for HTTP-PROXY version 6.32
;;; Reason: Function HTTP::PROXY-RELAY-REQUEST-WITH-ENTITY:  trace post method.
;;; Written by JCMa, 8/25/03 21:04:47
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5-inc1.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.6,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.161,
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
;;; Jcma 44, W4 Constraint-Guide Web Walker 45.11, HTTP Client Substrate 4.21,
;;; HTTP Client 51.2, Image Substrate 440.4, Essential Image Substrate 433.0,
;;; HTTP Proxy Server 6.31, Ivory Revision 5, VLM Debugger 329, Genera program 8.18,
;;; DEC OSF/1 V4.0 (Rev. 110),
;;; 1570x1120 24-bit TRUE-COLOR X Screen FUJI:4.0 with 224 Genera fonts (The Olivetti & Oracle Research Laboratory R3323),
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
  "HTTP:PROXY;PROXY.LISP.78")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY.LISP.78")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun proxy-relay-request-with-entity (server method request-http-version)
  (let* ((request-url (server-url server))
	 (request-headers (server-headers server))
	 (request-content-length (get-header :content-length request-headers))
	 (request-stream (server-stream server)))
    (flet ((send-data (from-stream to-stream url headers content-length from-http-version to-http-version)
	     ;; Send 100 code to continue as appropriate
	     (case from-http-version
	       ((:http/1.0 :http/0.9))
	       (t (report-status-continue from-stream)
		  (send-cr-line-feed from-stream) 
		  (force-output from-stream)))
	     ;; transfer the body
	     (with-binary-stream (from-stream :input)
	       (with-binary-stream (to-stream :output)
		 (with-transfer-encoding (to-stream (proxy-relay-transfer-encoding content-length to-http-version))
		   (with-transfer-decoding* (from-stream url from-http-version :headers headers :copy-mode :binary)
		     (stream-copy-until-eof from-stream to-stream :binary))))))
	   (write-the-request-headers (stream method http-version)
	     (declare (ignore method))
	     (let ((additional-headers (get-transfer-encoding-header-plist (get-header :content-length request-headers) http-version)))
	       (write-proxy-request-headers server request-headers http-version stream additional-headers))))
      (declare (dynamic-extent #'write-the-request-headers))
;      (let-if (member method '(:post :put))
;	      ((*client-http-version* (http-version-max :http/1.0 (server-http-version server))
;				      #+ignore :http/1.0 ))	;kludge to deal with broken HTTP 1.1 servers   9/30/2000 -- JCMa.
      (when *trace-proxy*
	(format *trace-output*"~&--------------------------------------------------------------------------------")
	(format *trace-output*  "~&;Proxying a ~A ~A request for ~S." request-http-version method (server-url-string server))
	(format *trace-output* "~&;~2TClient Request Headers:~&")
	(write-header-buffer (server-headers server) *trace-output*))
      (with-http-request
	(request-url method 
		     :request-headers #'write-the-request-headers
		     :request-body (send-data request-stream remote-stream request-url request-headers 
					      request-content-length request-http-version http-version)) 
	(let* ((client-status (client-status client))
	       (client-http-version (client-connection-version client))
	       (client-response-headers (client-response-headers client)))
	  (when *trace-proxy*
	    (format *trace-output* "~&;Origin Server ~A Status: ~D" client-http-version client-status)
	    (format *trace-output* "~&;Origin Server Headers:~&")
	    (write-header-buffer client-response-headers *trace-output*))
	  (send-status-line request-stream (proxy-maybe-downgrade-status-code client-status request-http-version) (client-reason client))
	  (cond ((response-status-code-requires-no-entity-p client-status)
		 (write-proxy-response-headers server client-response-headers client-http-version t request-stream))
		(t (let ((content-length (get-header :content-length client-response-headers)))
		     (multiple-value-bind (additional-headers transfer-encoding)
			 (get-transfer-encoding-header-plist content-length request-http-version)
		       (write-proxy-response-headers server client-response-headers client-http-version content-length request-stream additional-headers)
		       (handling-optional-entity-for-status-code
			 (client-status client-response-headers :clean-up-form (setf (server-close-connection-p server) t))
			 (with-binary-stream (request-stream :output)
			   (with-binary-stream (remote-stream :input)
			     (with-transfer-encoding (request-stream transfer-encoding)
			       (with-transfer-decoding* (remote-stream request-url http-version :headers client-response-headers :copy-mode :binary)
				 (stream-copy-until-eof remote-stream request-stream :binary)))))))))))))))

