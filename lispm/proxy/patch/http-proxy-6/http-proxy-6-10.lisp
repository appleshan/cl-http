;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for HTTP-PROXY version 6.10
;;; Reason: Function HTTP::PROXY-RELAY-REQUEST-WITH-ENTITY:  remive kludge
;;; Written by JCMa, 12/08/00 18:09:38
;;; while running on FUJI-3 from FUJI:/usr/lib/symbolics/Inc-CL-HTTP-70-90-LMFS.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, LMFS 442.1, Color 427.1,
;;; Graphics Support 431.0, Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Color System Documentation 10.0, SGD Book Design 10.0, Images 431.2,
;;; Image Substrate 440.4, CLIM 72.0, Genera CLIM 72.0, CLX CLIM 72.0,
;;; PostScript CLIM 72.0, CLIM Demo 72.0, CLIM Documentation 72.0,
;;; Statice Runtime 466.1, Statice 466.0, Statice Browser 466.0,
;;; Statice Server 466.2, Statice Documentation 426.0, Metering 444.0,
;;; Metering Substrate 444.1, Symbolics Concordia 444.0, Graphic Editor 440.0,
;;; Graphic Editing 441.0, Bitmap Editor 441.0, Graphic Editing Documentation 432.0,
;;; Postscript 436.0, Concordia Documentation 432.0, Joshua 237.4,
;;; Joshua Documentation 216.0, Joshua Metering 206.0, Jericho 237.0, C 440.0,
;;; Lexer Runtime 438.0, Lexer Package 438.0, Minimal Lexer Runtime 439.0,
;;; Lalr 1 434.0, Context Free Grammar 439.0, Context Free Grammar Package 439.0,
;;; C Runtime 438.0, Compiler Tools Package 434.0, Compiler Tools Runtime 434.0,
;;; C Packages 436.0, Syntax Editor Runtime 434.0, C Library Headers 434,
;;; Compiler Tools Development 435.0, Compiler Tools Debugger 434.0,
;;; C Documentation 426.0, Syntax Editor Support 434.0, LL-1 support system 438.0,
;;; Pascal 433.0, Pascal Runtime 434.0, Pascal Package 434.0, Pascal Doc 427.0,
;;; Fortran 434.0, Fortran Runtime 434.0, Fortran Package 434.0, Fortran Doc 427.0,
;;; HTTP Proxy Server 6.9, HTTP Server 70.96, Showable Procedures 36.3,
;;; Binary Tree 34.0, W3 Presentation System 8.1, HTTP Client Substrate 4.3,
;;; HTTP Client 50.1, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Metering Patches 1.0, Genera 8 5 Joshua Patches 1.0,
;;; Genera 8 5 Jericho Patches 1.0, Genera 8 5 Joshua Doc Patches 1.0,
;;; Genera 8 5 Joshua Metering Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.0,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clx Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Clim Demo Patches 1.0, Genera 8 5 Color Patches 1.1,
;;; Genera 8 5 Images Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Genera 8 5 Concordia Patches 1.0,
;;; Genera 8 5 Concordia Doc Patches 1.0, Genera 8 5 C Patches 1.0,
;;; Genera 8 5 Pascal Patches 1.0, Genera 8 5 Fortran Patches 1.0, Mailer 438.0,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.16,
;;; DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x976 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number -2141189522,
;;; Vlm lmfs patch (from W:>Reti>vlm-lmfs-patch.lisp.12),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.7),
;;; Pht debugging patch (from W:>Reti>pht-debugging-patch.lisp.4),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.6).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:PROXY;PROXY.LISP.65")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY.LISP.65")
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
      (with-http-request
	(request-url method 
		     :request-headers #'write-the-request-headers
		     :request-body (send-data request-stream remote-stream request-url request-headers 
					      request-content-length request-http-version http-version)) 
	(let* ((client-status (client-status client))
	       (client-http-version (client-connection-version client))
	       (client-response-headers (client-response-headers client)))
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

