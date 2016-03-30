;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for HTTP-BASE-CLIENT version 51.3
;;; Reason: Function (CLOS:METHOD HTTP:COPY-FILE (URL:HTTP-URL CL:PATHNAME)): Fix destination file printing.
;;; Function HTTP::%POST-URL:  -
;;; Function HTTP::%PUSH-ENTRY-URL-ENCODED-VECTOR:  add pathname as value type.
;;; Written by JCMa, 8/25/03 19:56:06
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
  "HTTP:CLIENT;SEXP-BROWSER.LISP.94"
  "HTTP:CLIENT;SEXP-BROWSER.LISP.95"
  "HTTP:CLIENT;SEXP-BROWSER.LISP.96")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.94")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod copy-file ((from-url http-url) (to-pathname pathname) &key copy-mode request-headers report-stream &allow-other-keys)
  (%with-open-url (from-url request-headers)
    (with-status-code-dispatch (:client client :url from-url :status (client-status client)
					:success-status-codes (200 203 205 206)
					:exceptions-flush-entities t) 
      (let ((response-headers (client-response-headers client))
	    (to-file (make-pathname :name (or (pathname-name to-pathname) (url:object from-url))
				    :type (or (pathname-type to-pathname) (url:extension from-url))
				    :version (or (pathname-version to-pathname) :newest)
				    :defaults to-pathname))
	    last-modified)
	(with-header-values (content-type) response-headers
	  (when report-stream
	    (format report-stream "~&Copying ~A to ~A...." from-url to-file))
	  (multiple-value-prog1
	    (with-open-file (file to-file :direction :output :if-does-not-exist :create :if-exists :supersede
				  :element-type (ecase (or copy-mode 
							   (mime-content-type-copy-mode content-type))
						  (:text *standard-character-type*)
						  ((:binary :crlf) '(unsigned-byte 8))))
	      (with-transfer-decoding* (remote-stream from-url http-version :headers response-headers)
		(stream-copy-until-eof remote-stream file copy-mode)))
	    (autotype-file to-pathname)
	    (when (setq last-modified (get-header :last-modified response-headers))
	      (set-file-modification-date to-file last-modified nil))
	    (when report-stream
	      (format report-stream "~&Copied ~A to ~A." from-url to-file))))
	to-file))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.95")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun %post-url (url vector headers stream)
  (flet ((send-data (remote-stream vector content-length)
	   (with-binary-stream (remote-stream :output)
	     (write-vector remote-stream vector 0 content-length)
	     (force-output remote-stream))))
    (handler-case
      (let* ((content-length (fill-pointer vector))
	     (outgoing-headers `(,@headers :content-type (:application :x-www-form-urlencoded)
				 :content-length ,content-length)))
	(handling-redirects (url)
	  (handling-authentication (authorization proxy-authorization)
	    (with-http-request
	      (url :post
		   :request-headers (compute-standard-request-headers
				      url :authorization authorization :proxy-authorization proxy-authorization
				      :header-plist outgoing-headers)
		   :request-body (send-data remote-stream vector content-length))
	      (with-status-code-dispatch (:client client :url url :status (client-status client) :http-version http-version
						  :headers *headers* :success-status-codes (200 204) :exceptions-flush-entities t)
		(ecase status
		  (200
		    (destructuring-bind (major-type minor-type &key &allow-other-keys)
			(get-header :content-type)
		      (display url major-type minor-type remote-stream stream)))
		  (204
		    (values url nil))))))))
      (http-condition (cond) (www-utils:report-condition cond stream)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.96")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;;;------------------------------------------------------------------- 
;;;
;;; POST URL 
;;; 

(defun %push-entry-url-encoded-vector (vector key value)
  (flet ((push-key (key vector)
	   (loop for idx upfrom 0 below (length key)
		 for char = (aref key idx)
		 for code = (char-code char)
		 do (vector-push-extend code vector)
		 finally (vector-push-extend #.(char-code #\=) vector)))
	 (push-value (value vector)
	   (loop for idx upfrom 0 below (length value)
		 for char = (aref value idx)
		 for code = (char-code char)
		 do (vector-push-extend code vector)
		 when (eql code #.(char-code #\Return))
		   do (vector-push-extend #.(char-code #\Linefeed) vector)
		 finally (vector-push-extend #.(char-code #\&) vector))))
    (declare (inline push-key push-value))
    ;; push the key
    (push-key (etypecase key
		(symbol (symbol-name key))
		(string key))
	      vector)
    ;; push the value
    (let ((val-string (typecase value
			(string value)
			(number (write-to-string value :base 10. :escape nil))
			(pathname (stream-encode-crlf-until-eof value vector))
			(t (write-to-string value :base 10. :escape nil)))))
      (declare (dynamic-extent val-string))
      (push-value val-string vector))))

