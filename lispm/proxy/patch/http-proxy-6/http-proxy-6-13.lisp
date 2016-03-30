;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for HTTP-PROXY version 6.13
;;; Reason: Function (CLOS:METHOD HTTP::INVOKE-PROXY-SERVICE (HTTP::PROXY-SERVER-MIXIN URL:FTP-DIRECTORY (EQL :GET) T)):  fix bug.
;;; Function (CLOS:METHOD HTTP::INVOKE-PROXY-SERVICE (HTTP::PROXY-SERVER-MIXIN T T T) :AROUND):  use specialized proxy rejection error class.
;;; Written by JCMa, 12/18/00 17:31:27
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.107,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 54.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.0,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Jcma 42, HTTP Proxy Server 6.12,
;;; HTTP Client Substrate 4.6, Statice Server 466.2, CL-HTTP Documentation 3.0,
;;; Experimental CL-HTTP CLIM User Interface 1.0, HTTP Client 50.1,
;;; Image Substrate 440.4, Essential Image Substrate 433.0,
;;; W4 Constraint-Guide Web Walker 45.8, W4 Examples 15.0, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
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
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:PROXY;PROXY.LISP.66"
  "HTTP:PROXY;PROXY.LISP.67")


(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL)
  (SCT:REQUIRE-PATCH-LEVEL-FOR-PATCH '(CL-HTTP 70. 108.)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY.LISP.66")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod invoke-proxy-service ((server proxy-server-mixin) (url url:ftp-directory) (method (eql :get)) request-http-version)
  request-http-version
  (labels ((get-ftp-directory-info (url)
	     (handler-case 
	       (multiple-value-bind (user-id pw)
		   (proxy-ftp-user-id-and-password server url)
		 (ftp-directory-info (url::ftp-url-pathname url) (host-port url) user-id pw))
	       (file-not-found (err) (error 'document-not-found :url url :method method :format-string (report-string err)))))
	   (write-item (path plist stream)
	     (destructuring-bind (&key length-in-bytes creation-date directory
				       #+cl-http-file-author author &allow-other-keys)
		 plist
	       (let ((ftp-url (pathname-ftp-url-string path directory)))
		 (html:with-table-row (:stream stream)
		   (html:with-table-cell (:stream stream)
		     (html:with-rendition (:bold :stream stream)
		       (html:with-anchor-noted (:reference ftp-url :stream stream)
			 (let ((name (pathname-name path))
			       (type (pathname-type path))
			       (version (pathname-version path)))
			   (cond ((null name)
				  (fast-format stream "~A" (car (last (pathname-directory path)))))
				 (t (fast-format stream "~A" name)
				    (when (and type (not (eql type :unspecific)))
				      (fast-format stream ".~A" type)
				      (when (and version (integerp version))
					(fast-format stream ".~D" version)))))))))
		   (if creation-date
		       (html:with-table-cell (:horizontal-alignment :right :stream stream)
			 (write-standard-time creation-date stream))
		       (html:with-table-cell (:horizontal-alignment :center :stream stream)
			 (write-string "--" stream)))
		   (cond (directory
			  (html:with-table-cell (:horizontal-alignment :right :stream stream)))
			 (length-in-bytes
			  (html:with-table-cell (:horizontal-alignment :right :stream stream)
			    (write length-in-bytes :stream stream :escape nil :base 10.)))
			 (t (html:with-table-cell (:horizontal-alignment :center :stream stream)
			      (write-string "--" stream))))
		   #+cl-http-file-author
		   (if author
		       (html:with-table-cell (:horizontal-alignment :right :stream stream)
			 (write-string author stream))
		       (html:with-table-cell (:horizontal-alignment :center :stream stream))))))))
    (declare (inline directory-info))
    (multiple-value-bind (directory-listing directory-exists-p)
	(get-ftp-directory-info url)
      (cond (directory-exists-p
	     (let* ((title (relative-name-string url))
		    (stream (server-stream server))
		    (proxy-response-headers `(:via ,(compute-via-header nil))))
	       (declare (dynamic-extent title proxy-response-headers))
	       (with-successful-response (stream :html :status :success  :location url :additional-headers proxy-response-headers)
		 (html:with-html-document (:declare-dtd-version-p t :stream stream)
		   (html:with-document-preamble (:stream stream)
		     (html:declare-title title :stream stream))
		   (html:with-standard-document-body (:stream stream)
		     (html:with-section-heading (title :stream stream)
		       (html:horizontal-line  :stream stream)
		       (html:with-table (:cell-spacing 4 :stream stream)
			 (html:with-table-row (:stream stream)
			   (html:with-table-cell (:header-p t :horizontal-alignment :center :stream stream)
			     (write-string "URL" stream))
			   (html:with-table-cell (:header-p t :horizontal-alignment :center :stream stream)
			     (write-string "Creation Date" stream))
			   (html:with-table-cell (:header-p t :horizontal-alignment :center :stream stream)
			     (write-string "Bytes" stream))
			   #+cl-http-file-author
			   (html:with-table-cell (:header-p t :horizontal-alignment :center :stream stream)
			     (write-string "Author" stream)))
			 (loop for (path . plist) in directory-listing
			       for translated = (translated-pathname path)
			       do (write-item translated plist stream)))
		       (html:horizontal-line :stream stream)
		       (cl-http-signature stream)))))))
	    (t (error 'document-not-found :url url :method :get))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY.LISP.67")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;;;------------------------------------------------------------------- 
;;;
;;; PROXY METHODS 
;;; 

(defmethod invoke-proxy-service :around ((server proxy-server-mixin) url method http-version
					 &aux (catch-error-p (not *debug-proxy*)))
  (flet ((ensure-live-upstream-connection (condition)
	   (declare (ignore condition))
	   (abort-if-connection-inactive *server*)
	   nil))
    (with-subnet-access-control
      ((server-address server) (or *proxy-subnets* *secure-subnets*) 
       :deny-subnets *disallowed-subnets*
       :rejection-form (error 'proxy-access-forbidden :method method :url url))
      (handler-case-if 
	  catch-error-p
	 ;; Nasty signalling ensues if the client has dropped the connection, 
	 ;; so intercept errors here and abort the connection if the client is gone. -- JCMa 5/24/1999.
	 (handler-bind-if catch-error-p
	    ((condition #'ensure-live-upstream-connection))
	   ;; Set the life time for the connection 
	   (setf (server-life-time server) *proxy-server-life-time*)
	   ;; call the primary method
	   (call-next-method server url method http-version))
	(unknown-host-name (err) (error 'proxy-unresolvable-domain-name :format-string (report-string err)
					:method method :url url))
	(protocol-timeout (err) (error 'proxy-connection-timeout :format-string (report-string err)
				       :method method :url url))
	(connection-refused (err) (error 'proxy-connection-refused :format-string (report-string err)
					 :method method :url url))
	(local-network-error (err)  (error 'proxy-local-network-error :format-string (report-string err)
					   :method method :url url :close-connection t))
	(remote-network-error (err) (error 'proxy-remote-network-error :format-string (report-string err)
					   :method method :url url :close-connection t))))))

