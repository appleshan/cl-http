;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for HTTP-BASE-CLIENT version 51.6
;;; Reason: Function (CLOS:METHOD HTTP:COPY-FILE (URL:HTTP-URL CL:PATHNAME)):  update for
;;; proper transfer decoding, handling CRLF, and prefering content-type as a
;;; source for copy-mode
;;; Function HTTP::%PUT-URL:  tune up.
;;; Written by JCMa, 10/08/03 21:30:42
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5-inc1.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.176,
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
;;; Jcma 44, HTTP Proxy Server 6.32, HTTP Client Substrate 4.23, HTTP Client 51.5,
;;; Image Substrate 440.4, Essential Image Substrate 433.0, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.18, DEC OSF/1 V4.0 (Rev. 110),
;;; 1560x1120 24-bit TRUE-COLOR X Screen FUJI:1.0 with 224 Genera fonts (The Olivetti & Oracle Research Laboratory R3323),
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing).

(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL)
  (SCT:REQUIRE-PATCH-LEVEL-FOR-PATCH '(CL-HTTP 70. 177.)))


(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:CLIENT;SEXP-BROWSER.LISP.105"
  "HTTP:CLIENT;SEXP-BROWSER.LISP.106")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.105")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod copy-file ((from-url http-url) (to-pathname pathname) &key copy-mode request-headers report-stream &allow-other-keys)
  (with-copy-file-environment (from-url to-pathname report-stream)
    (%with-open-url (from-url request-headers)
      (with-status-code-dispatch (:client client :url from-url :status (client-status client)
					  :success-status-codes (200 203 205 206)
					  :exceptions-flush-entities t) 
	(let ((response-headers (client-response-headers client))
	      (to-file (make-pathname :name (or (pathname-name to-pathname) (url:object from-url))
				      :type (or (pathname-type to-pathname) (url:extension from-url))
				      :version (or (pathname-version to-pathname) :newest)
				      :defaults to-pathname))
	      c-mode)
	  (with-header-values (content-type last-modified #+CL-HTTP-File-Author from) response-headers
	    (setq c-mode (or (mime-content-type-copy-mode content-type) copy-mode))
	    (with-open-file (file to-file :direction :output :if-does-not-exist :create :if-exists :supersede
				  :element-type (ecase c-mode
						  ((:crlf :text) *standard-character-type*)
						  (:binary '(unsigned-byte 8))))
	      (ecase c-mode
		((:crlf :text)
		 (with-transfer-decoding* (remote-stream from-url http-version :headers response-headers
							 :stream-functions (stream-decode-crlf-until-eof))
		   (stream-decode-crlf-until-eof remote-stream file)))
		(:binary
		  (with-transfer-decoding* (remote-stream from-url http-version :headers response-headers)
		    (stream-copy-until-eof remote-stream file copy-mode))))
	      (setq to-file (truename file)))
	    (autotype-file to-file)
	    (cond-every
	      (last-modified 
		(setf (file-modification-date to-file) last-modified)
		#-UNIX ;; creation date is not available under unix
		(setf (file-creation-date to-file) last-modified))
	      #+CL-HTTP-File-Author
	      (from (set-file-author to-file from nil)))
	    to-file))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.106")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;;;------------------------------------------------------------------- 
;;;
;;; PUT METHOD
;;; 

;; Handles the 1.1 PUT and the 1.0 cretenous PUT
(defun %put-url (url resource-writer content-length content-type headers stream version)
  (flet ((send-data (writer remote-stream url content-length)
           (with-transfer-encoding (remote-stream (if content-length
						      :fixed-length :chunked))
	     (funcall writer url remote-stream))
           (force-output remote-stream)))
    (handler-case
      (let ((outgoing-headers `(,@headers
                                ,.(cond ((eq version :overwrite) nil)
                                        ((numberp version) `(:derived-from ,version))
                                        (t nil))
                                :content-type ,content-type
                                ,@(if content-length
				      `(:content-length ,content-length)
				      `(:transfer-encoding :chunked)))))
        (declare (dynamic-extent outgoing-headers))
        (handling-redirects (url)
	  (handling-authentication (authorization proxy-authorization)
	    (with-http-request
	      (url :put 
		   :request-headers (compute-standard-request-headers
				      url :authorization authorization :proxy-authorization proxy-authorization
				      :header-plist outgoing-headers)
		   :request-body (send-data resource-writer remote-stream url content-length))
	      (with-status-code-dispatch (:success-status-codes (200 201 204) :status (client-status client)
								:url (client-url client) :client client
								:headers (client-response-headers client) :http-version http-version
								:exceptions-flush-entities t)
		(let ((content-location (get-header :content-location))
		      (content-version (get-header :content-version))
		      (last-modified (get-header :last-modified))
		      (keyword (if (eql status 201) :created :modified)))
		  (when content-location
		    (setq content-location (intern-url content-location :if-does-not-exist :uninterned)))
		  (case status
		    (200 (destructuring-bind (major-type minor-type)
			     (get-header :content-type)
			   (display (or content-location url) major-type minor-type remote-stream stream))))
		  (values (or content-location url) keyword content-version last-modified)))))))
      (http-condition (cond) (www-utils:report-condition cond stream)))))

