;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for W4 version 45.13
;;; Reason: Function HTTP::%GET-URL-HEADERS-AND-BODY:  pass in offset and vector within capture function.
;;; Written by JCMa, 10/06/03 08:53:51
;;; while running on FUJI-3 from FUJI:/usr/lib/symbolics/vlmlmfs2.vlod
;;; with Open Genera 2.0, Genera 8.5, Logical Pathnames Translation Files NEWEST,
;;; Lock Simple 437.0, Color Demo 422.0, Color 427.1, Graphics Support 431.0,
;;; Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Color System Documentation 10.0, SGD Book Design 10.0, Images 431.2,
;;; Image Substrate 440.4, CLIM 72.0, Genera CLIM 72.0, CLX CLIM 72.0,
;;; PostScript CLIM 72.0, CLIM Demo 72.0, CLIM Documentation 72.0,
;;; Statice Runtime 466.1, Statice 466.0, Statice Browser 466.0,
;;; Statice Server 466.2, Statice Documentation 426.0, Metering 444.0,
;;; Metering Substrate 444.1, Symbolics Concordia 444.0, Graphic Editor 440.0,
;;; Graphic Editing 441.0, Bitmap Editor 441.0, Graphic Editing Documentation 432.0,
;;; Postscript 436.0, Concordia Documentation 432.0, Joshua 237.6,
;;; Joshua Documentation 216.0, Joshua Metering 206.0, Jericho 237.0, C 440.0,
;;; Lexer Runtime 438.0, Lexer Package 438.0, Minimal Lexer Runtime 439.0,
;;; Lalr 1 434.0, Context Free Grammar 439.0, Context Free Grammar Package 439.0,
;;; C Runtime 438.0, Compiler Tools Package 434.0, Compiler Tools Runtime 434.0,
;;; C Packages 436.0, Syntax Editor Runtime 434.0, C Library Headers 434,
;;; Compiler Tools Development 435.0, Compiler Tools Debugger 434.0,
;;; Experimental C Documentation 427.0, Syntax Editor Support 434.0,
;;; LL-1 support system 438.0, Fortran 434.0, Fortran Runtime 434.0,
;;; Fortran Package 434.0, Experimental Fortran Doc 428.0, Pascal 433.0,
;;; Pascal Runtime 434.0, Pascal Package 434.0, Pascal Doc 427.0,
;;; MacIvory Support 447.0, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.41, Genera 8 5 Macivory Support Patches 1.0,
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
;;; Genera 8 5 Fortran Patches 1.0, Binary Tree 34.0, Showable Procedures 36.3,
;;; HTTP Server 70.172, W3 Presentation System 8.1, HTTP Client Substrate 4.23,
;;; HTTP Client 51.4, CL-HTTP Server Interface 54.0, HTTP Proxy Server 6.32,
;;; CL-HTTP Documentation 3.0, Experimental CL-HTTP CLIM User Interface 1.1,
;;; MAC 414.0, LMFS 442.1, W4 Constraint-Guide Web Walker 45.12, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.18, DEC OSF/1 V4.0 (Rev. 110),
;;; 1152x678 24-bit TRUE-COLOR X Screen FUJI:2.0 with 224 Genera fonts (The Olivetti & Oracle Research Laboratory R3323),
;;; Machine serial number 6288682,
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add support for Apple's Gestalt and Speech Managers. (from SYS:MAC;MACIVORY-SPEECH-SUPPORT.LISP.1),
;;; Vlm lmfs patch (from W:>reti>vlm-lmfs-patch.lisp.18),
;;; Pht debugging patch (from W:>Reti>pht-debugging-patch.lisp.4),
;;; Domain ad host patch (from W:>Reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing),
;;; Cname level patch (from W:>reti>cname-level-patch),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:CLIENT;W4-CLIENT.LISP.181")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;W4-CLIENT.LISP.181")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun %get-url-headers-and-body (url headers report-stream authorization)
  (flet ((standard-capture (stream copy-mode length)
	   (without-web-walker-cache-resource-exceptional-leaks (vector)
	     (ecase copy-mode
	       ((:text :crlf)
		(setq vector (allocate-web-walker-cache-array *standard-character-type* length))
		(crlf-stream-copy-into-string stream length 0 vector))
	       (:binary
		 (with-binary-stream (stream :input)
		   (setq vector (allocate-web-walker-cache-array '(unsigned-byte 8) length))
		   (binary-stream-copy-into-8-bit-array stream length 0 vector)))))))
    (declare (inline standard-capture))
    (handling-redirects (url)
      (with-http-request (url :get 
			      :request-headers (compute-standard-request-headers
						 url :authorization authorization :header-plist headers
						 :user-agent (if (getf headers :user-agent) nil *server-version*)))
	(let ((status (client-status client))
	      (response-headers (client-response-headers client))
	      response-body redirection)
	  (case status
	    ((200 205 206)
	     (let* ((content-type (get-header :content-type response-headers))
		    (copy-mode (mime-content-type-copy-mode content-type))
		    (content-length (get-header :content-length response-headers)))
	       (setq response-body (cond ((or content-length (member http-version '(:http/1.0 :http/0.9)))
					  (standard-capture remote-stream copy-mode content-length))
					 (t (let ((transfer-encoding (get-header :transfer-encoding response-headers)))
					      (case transfer-encoding
						(:chunked (chunked-input-capture remote-stream copy-mode response-headers))
						((nil)
						 (error 'bad-syntax-provided :url url :method :get
							:format-string "No content length header was provided."))
						(t (error 'server-not-implemented :close-connection t :url url :method :get
							  :format-string "The HTTP transfer decoding, ~A, is not implemented."
							  :format-args (list transfer-encoding))))))))))
	    ((201 202 203 204))
	    ((300 402 403 405 406 407 415))
	    ((301 302)
	     (let ((alternate-urls (mapcar #'url:intern-url (ensure-list (or (get-header :location response-headers) 
									     (get-header :content-location response-headers))))))
	       (flush-input-entity remote-stream response-headers http-version)
	       (push alternate-urls redirection)
	       (signal (ecase status
			 (301 'document-moved-permanently)
			 (302 'document-moved-temporarily))
		       :new-urls alternate-urls :version http-version)))
	    ;; do something about authentication -- JCMa 12/10/1996.
	    (401 (destructuring-bind (&optional authentication-method . realm) (get-header :WWW-Authenticate response-headers)
		   (declare (ignore authentication-method realm))
		   nil))
	    (404
	      (when *debug-client*
		(fresh-line report-stream)
		(%write-common-logfile-entry (host-string url) (concatenate 'string (url:name-string url) " GET")
					     status 0 "-" *log-times-in-gmt* report-stream)))
	    ((nil) (setq status 408))		; didn't return a status code
	    ((408 411 414 500 501 502 503 504 505))
	    (t (client-signal-http-code url status :get :headers response-headers :reason (client-reason client) :version http-version)))
	  (values response-body (durable-response-headers client) status redirection http-version))))))

