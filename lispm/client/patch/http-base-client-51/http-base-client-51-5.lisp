;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for HTTP-BASE-CLIENT version 51.5
;;; Reason: Overhaul copy-file and add url to url copying.
;;; 
;;; Function (CLOS:METHOD HTTP:COPY-FILE (CL:PATHNAME URL:HTTP-PATH)):  fix method calling.
;;; Function (CLOS:METHOD HTTP:COPY-FILE (CONS URL:HTTP-PATH)):  -
;;; Function (CLOS:METHOD HTTP:COPY-FILE (CL:PATHNAME URL:HTTP-URL)):  -
;;; Remove function (CLOS:METHOD HTTP:COPY-FILE (CL:PATHNAME URL:HTTP-URL)): -
;;; Function (CLOS:METHOD HTTP:COPY-FILE (CL:PATHNAME URL:HTTP-OBJECT)):  -
;;; Function (CLOS:METHOD HTTP:COPY-FILE (URL:HTTP-URL CL:PATHNAME)):  -
;;; Function (CLOS:METHOD HTTP:COPY-FILE (CONS CL:PATHNAME)):  -
;;; Function (CLOS:METHOD HTTP:COPY-FILE (URL:HTTP-URL URL:HTTP-OBJECT)):  new
;;; Function (CLOS:METHOD HTTP:COPY-FILE (URL:HTTP-OBJECT URL:HTTP-PATH)):  -
;;; Function (CLOS:METHOD HTTP:COPY-FILE (CL:PATHNAME URL:HTTP-PATH)):  -
;;; Written by JCMa, 10/06/03 09:31:10
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

;;; Patch file for HTTP-BASE-CLIENT version 51.5
;;; Written by JCMa, 10/07/03 20:10:21
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5-inc1.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.173,
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
;;; Jcma 44, HTTP Proxy Server 6.32, HTTP Client Substrate 4.22, HTTP Client 51.4,
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


;;; Patch file for HTTP-BASE-CLIENT version 51.5
;;; Written by JCMa, 10/07/03 09:36:04
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
;;; HTTP Server 70.173, W3 Presentation System 8.1, HTTP Client Substrate 4.23,
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

(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL)
  (SCT:REQUIRE-PATCH-LEVEL-FOR-PATCH '(CL-HTTP 70. 173.)))


(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:CLIENT;SEXP-BROWSER.LISP.100"
  "HTTP:CLIENT;SEXP-BROWSER.LISP.101"
  "HTTP:CLIENT;SEXP-BROWSER.LISP.102"
  "HTTP:CLIENT;SEXP-BROWSER.LISP.103")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.100")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod copy-file ((from-pathnames cons) (to-url http-path) &rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (loop for pathname in from-pathnames
	do (apply #'copy-file pathname to-url args)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.100")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(SCL:FUNDEFINE '(METHOD COPY-FILE (PATHNAME HTTP-URL)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.100")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod copy-file ((from-urls cons) (to-pathname pathname) &rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (loop with directory = (make-pathname :name :unspecific :type :unspecific :version :unspecific)
        for from-url in from-urls
	do (apply #'copy-file from-url directory args)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.100")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod copy-file ((from-url http-object) (to-url http-path) &rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (let ((spec (url:make-http-url-string :host (url:host-string to-url)
                                        :port (url:port to-url)
                                        :path (url:path to-url)
                                        :name (url:object from-url)
                                        :extension (url:extension from-url))))
    (apply #'copy-file from-url (intern-url spec :if-does-not-exist :uninterned) args)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.100")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod copy-file ((from-pathname pathname) (to-url http-path) &rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (let ((spec (url:make-http-url-string :host (url:host-string to-url)
                                        :port (url:port to-url)
                                        :path (url:path to-url)
                                        :name (pathname-name from-pathname)
                                        :extension (pathname-type from-pathname))))
    (apply #'copy-file from-pathname (intern-url spec :if-does-not-exist :uninterned) args)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.101")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(SCL:FUNDEFINE '(METHOD COPY-FILE (PATHNAME HTTP-OBJECT) :AROUND))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.102")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod copy-file ((from-pathname pathname) (to-url http-object) &key copy-mode content-type
                      (user-email-address (www-utils:user-mail-address)) report-stream &allow-other-keys)
  (with-copy-file-environment (from-pathname to-url report-stream)
    (let ((media-type (or content-type
			  (let ((type (pathname-type from-pathname)))
			    (when type (mime-content-type-spec-for-pathname-type type nil)))
			  (case copy-mode	;generic media types based on copy-mode
			    (:text '(:text :plain))
			    (:binary '(:message :http))
			    (t nil))))
	  (header-plist (and user-email-address `(:from (,user-email-address) :last-modified ,(file-modification-date from-pathname)))))
      (declare (dynamic-extent header-plist))
      (put-url to-url from-pathname :content-type media-type :headers header-plist))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.102")
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
	      last-modified c-mode)
	  (with-header-values (content-type) response-headers
	    (setq c-mode (or copy-mode (mime-content-type-copy-mode content-type)))
	    (multiple-value-prog1
	      (with-open-file (file to-file :direction :output :if-does-not-exist :create :if-exists :supersede
				    :element-type (ecase c-mode
						    (:text *standard-character-type*)
						    ((:binary :crlf) '(unsigned-byte 8))))
		(ecase c-mode
		  (:text
		    (with-transfer-decoding* (remote-stream from-url http-version :headers response-headers
							    :stream-functions '(stream-decode-crlf-until-eof))
		      (stream-decode-crlf-until-eof remote-stream file)))
		  (:binary
		    (with-transfer-decoding* (remote-stream from-url http-version :headers response-headers)
		      (stream-copy-until-eof remote-stream file copy-mode)))))
	      (autotype-file to-pathname)
	      (when (setq last-modified (get-header :last-modified response-headers))
		(set-file-modification-date to-file last-modified nil))))
	  to-file)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.102")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(SCL:FUNDEFINE '(METHOD COPY-FILE (HTTP-URL PATHNAME) :AROUND))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.102")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(SCL:FUNDEFINE '(METHOD COPY-FILE (HTTP-URL HTTP-OBJECT) :AROUND))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.103")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;; This does not handle the via-header. Should it?
;; It parses entity headers only to resend them. So, some efficiency could be gained
;; by just writing the  desired headers directly while breaking the put-url abstraction
(defmethod copy-file ((from-url http-url) (to-url http-object) &key copy-mode request-headers report-stream 
		      (user-email-address (www-utils:user-mail-address)) &allow-other-keys)
  (with-copy-file-environment (from-url to-url report-stream)
    (%with-open-url (from-url request-headers)
      (with-status-code-dispatch (:client client :url from-url :status (client-status client)
					  :success-status-codes (200 203 205 206)
					  :exceptions-flush-entities t) 
	(let ((response-headers (client-response-headers client)))
	  (with-header-values (content-type content-length last-modified date) response-headers
	    (let* ((c-mode (or copy-mode (mime-content-type-copy-mode content-type)))
		   (content-type-spec (or content-type (url::mime-content-type-spec from-url)))
		   (version (or last-modified date))
		   (new-headers `(,.(entity-header-plist response-headers '(:content-type :content-length :expires))
				  ,.(and user-email-address `(:from (,user-email-address))))))
	      (declare (dynamic-extent new-headers))
	      (flet ((send-data (url to-stream)
		       (declare (ignore url))
		       (with-transfer-decoding* (remote-stream from-url http-version :headers response-headers)
			 (stream-copy-until-eof remote-stream to-stream c-mode))))
		(declare (dynamic-extent #'send-data))
		(%put-url to-url #'send-data content-length content-type-spec new-headers report-stream version)))))))
    to-url))

