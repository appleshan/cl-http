;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for W4 version 45.10
;;; Reason: Rely on the substrate for handling request versions lower than server versions.
;;; 
;;; Function HTTP::%GET-URL-HEADERS-AND-BODY:  handle HTTP 1.0 requests to HTTP 1.1 servers correctly.
;;; Function HTTP::%GET-URL-HEADERS:  ditto.
;;; Written by JCMa, 2/28/01 14:18:45
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
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Lock Simple Patches 1.0, Jcma 42,
;;; HTTP Proxy Server 6.14, HTTP Client Substrate 4.7, Statice Server 466.2,
;;; W4 Constraint-Guide Web Walker 45.9, Ivory Revision 5, VLM Debugger 329,
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

;;; Patch file for W4 version 45.10
;;; Written by JCMa, 2/28/01 15:55:20
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




(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL)
  (SCT:REQUIRE-PATCH-LEVEL-FOR-PATCH '(HTTP-CLIENT-SUBSTRATE 4. 8.)))


(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:CLIENT;W4-CLIENT.LISP.178")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;W4-CLIENT.LISP.178")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun %get-url-headers-and-body (url headers report-stream authorization)
  (flet ((standard-capture (stream copy-mode length)
	   (without-web-walker-cache-resource-exceptional-leaks (vector)
	     (ecase copy-mode
	       ((:text :crlf)
		(setq vector (allocate-web-walker-cache-array *standard-character-type* length))
		(crlf-stream-copy-into-string stream length))
	       (:binary
		 (with-binary-stream (stream :input)
		   (setq vector (allocate-web-walker-cache-array '(unsigned-byte 8) length))
		   (binary-stream-copy-into-8-bit-array stream length)))))))
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


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;W4-CLIENT.LISP.178")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;;;------------------------------------------------------------------- 
;;;
;;; METHODS FOR WEB WALKER
;;;

(defun %get-url-headers (url headers report-stream authorization)
  (handling-redirects (url)
    (with-http-request (url :head 
                            :request-headers (compute-standard-request-headers 
                                               url :authorization authorization :header-plist headers
					       :user-agent (if (getf headers :user-agent) nil *server-version*)))
      (let ((status (client-status client))
            (response-headers (client-response-headers client))
            redirection)
        (case status
          ((200 203 204 205 206))
          ((301 302)
           (let ((alternate-urls (mapcar #'url:intern-url (ensure-list (or (get-header :location response-headers) 
                                                                           (get-header :content-location response-headers))))))
             (push alternate-urls redirection)
             (signal (ecase status
                       (301 'document-moved-permanently)
                       (302 'document-moved-temporarily))
                     :new-urls alternate-urls :version http-version)))
          ((402 403 405 406 407))
          (404
            (when *debug-client*
              (fresh-line report-stream)
              (%write-common-logfile-entry (host-string url) (concatenate 'string (url:name-string url) " HEAD")
                                           status  0 "-"  *log-times-in-gmt* report-stream)))
          ;; do something about authentication -- JCMa 12/10/1996.
          (401 (destructuring-bind (&optional authentication-method . realm) (get-header :WWW-Authenticate response-headers)
                 (declare (ignore authentication-method realm))
                 nil))
          ((nil) (setq status 408))             ; didn't return a status code
          ((408 500 501 502 503 504 505))
          (t (client-signal-http-code url status :head :headers response-headers :reason (client-reason client) :version http-version)))
        ;; return values for walker
        (values (durable-response-headers client) status redirection http-version)))))

