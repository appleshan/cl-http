;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.80
;;; Reason: Reverse Proxying Infrastructure for Server.
;;; 
;;; Remove potential infinite loops based on auto export from GET, HEAD, and
;;; OPTION.
;;; 
;;; CLOS class URL::REVERSE-PROXY-MIXIN:  new class.
;;; CLOS class URL::REVERSE-PROXY-HTTP-URL:  -
;;; CLOS class URL::REVERSE-PROXY-HTTP-PATH:  -
;;; CLOS class URL::REVERSE-PROXY-HTTP-OBJECT:  -
;;; CLOS class URL::REVERSE-PROXY-HTTP-SEARCH:  -
;;; CLOS class URL::REVERSE-PROXY-HTTP-SEARCHABLE-OBJECT:  -
;;; Function HTTP::WRITE-DOCUMENT-POST:  -
;;; Function (CLOS:METHOD HTTP::WRITE-DOCUMENT-POST (HTTP::BASIC-SERVER-MIXIN URL:HTTP-URL T)):  -
;;; Function (CLOS:METHOD HTTP::INVOKE-SERVER-METHOD (HTTP::BASIC-SERVER-MIXIN (EQL :POST) SYMBOL)):  -
;;; Function HTTP::WRITE-DOCUMENT-TRACE:  -
;;; Function (CLOS:METHOD HTTP::WRITE-DOCUMENT-TRACE (HTTP::BASIC-SERVER-MIXIN URL:URL T)):  -
;;; Function (CLOS:METHOD HTTP::INVOKE-SERVER-METHOD (HTTP::BASIC-SERVER-MIXIN (EQL :TRACE) (EQL :HTTP/1.1))):  -
;;; Function (CLOS:METHOD HTTP::INVOKE-SERVER-METHOD (HTTP::BASIC-SERVER-MIXIN (EQL :OPTIONS) (EQL :HTTP/1.1))):  -
;;; Function (CLOS:METHOD HTTP::INVOKE-SERVER-METHOD (HTTP::BASIC-SERVER-MIXIN (EQL :GET) SYMBOL)):  -
;;; Function (CLOS:METHOD HTTP::INVOKE-SERVER-METHOD (HTTP::BASIC-SERVER-MIXIN (EQL :HEAD) SYMBOL)):  -
;;; Function (CLOS:METHOD HTTP::INVOKE-SERVER-METHOD (HTTP::BASIC-SERVER-MIXIN (EQL :PUT) SYMBOL)):  -
;;; Function HTTP::INVOKE-REVERSE-PROXY-SERVICE:  -
;;; Function (CLOS:METHOD HTTP::INVOKE-REVERSE-PROXY-SERVICE (T T T)):  -
;;; Function HTTP::ENABLE-URL-AREAS:  -
;;; Function (CLOS:METHOD HTTP::INVOKE-SERVER-METHOD (HTTP::BASIC-SERVER-MIXIN (EQL :DELETE) SYMBOL)):  -
;;; Remove function (CLOS:METHOD URL:WRITE-LOCAL-NAME (URL:HTTP-PATH)): undefine.
;;; Function (CLOS:METHOD URL:WRITE-LOCAL-NAME (URL::PATH-MIXIN)):  move onto the correct mixin.
;;; Function (CLOS:METHOD HTTP:EXPORT-URL (URL:TRANSLATION-METHOD-MIXIN T) :AROUND):  
;;; Make sure HTTP running on the port when there is a translation method for an HTTP protocol URL.
;;; Written by JCMa, 9/29/00 21:15:30
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.79,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, HTTP Proxy Server 5.31,
;;; HTTP Client Substrate 3.24, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Documentation Patches 1.0,
;;; Genera 8 5 Clim Patches 1.0, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Jcma 42, Ivory Revision 5, VLM Debugger 329,
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
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.14),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.5),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.4),
;;; Retry invoke patch (from W:>Reti>retry-invoke-patch).

;;; Patch file for CL-HTTP version 70.80
;;; Written by JCMa, 9/30/00 16:45:33
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.80,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, HTTP Proxy Server 5.32,
;;; HTTP Client Substrate 3.24, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Documentation Patches 1.0,
;;; Genera 8 5 Clim Patches 1.0, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Jcma 42, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
;;; 1024x718 24-bit TRUE-COLOR X Screen JCMA-ISDN:0.0 with 224 Genera fonts (eXodusPowerPC 7.0  (c) 1998 White Pine Software,
;;; Inc. R6300),
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
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.14),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.5),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.4),
;;; Retry invoke patch (from W:>Reti>retry-invoke-patch).


;;; Patch file for CL-HTTP version 70.80
;;; Written by JCMa, 9/29/00 23:31:05
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.80,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, HTTP Proxy Server 5.31,
;;; HTTP Client Substrate 3.24, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Documentation Patches 1.0,
;;; Genera 8 5 Clim Patches 1.0, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Jcma 42, Ivory Revision 5, VLM Debugger 329,
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
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.14),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.5),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.4),
;;; Retry invoke patch (from W:>Reti>retry-invoke-patch).


;;; Patch file for CL-HTTP version 70.80
;;; Written by JCMa, 9/29/00 23:00:48
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.80,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, HTTP Proxy Server 5.31,
;;; HTTP Client Substrate 3.24, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Documentation Patches 1.0,
;;; Genera 8 5 Clim Patches 1.0, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Jcma 42, Ivory Revision 5, VLM Debugger 329,
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
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.14),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.5),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.4),
;;; Retry invoke patch (from W:>Reti>retry-invoke-patch).

(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;URL-CLASS.LISP.20"
  "HTTP:SERVER;PACKAGE.LISP.465"
  "HTTP:SERVER;SERVER.LISP.869"
  "HTTP:SERVER;UTILS.LISP.464"
  "HTTP:SERVER;SERVER.LISP.870"
  "HTTP:SERVER;SERVER.LISP.871"
  "HTTP:SERVER;URL.LISP.413"
  "HTTP:SERVER;SERVER.LISP.873")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL-CLASS.LISP.20")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: URL; Base: 10 -*-")

(defclass reverse-proxy-mixin
          ()
    ((forwardings :initform nil :initarg :forwardings :accessor reverse-proxy-forwardings)	;specs for where to proxy requests
     (superior :initform nil :initarg :superior :accessor reverse-proxy-superior))	;controling URL
  (:documentation "A mixin that provides a reverse proxying capability."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL-CLASS.LISP.20")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: URL; Base: 10 -*-")

(defclass reverse-proxy-http-url
          (reverse-proxy-mixin)
    ()
  (:documentation "A HTTP URL that is reverse proxied."))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL-CLASS.LISP.20")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: URL; Base: 10 -*-")

(defclass reverse-proxy-http-path
          (reverse-proxy-http-url path-mixin http-url)
    ()
  (:documentation "A reverse proxy HTTP path URL."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL-CLASS.LISP.20")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: URL; Base: 10 -*-")

(defclass reverse-proxy-http-object
          (reverse-proxy-http-url http-minimum-object)
    ()
  (:documentation "A reverse proxy HTTP object URL."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL-CLASS.LISP.20")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: URL; Base: 10 -*-")

(defclass reverse-proxy-http-search
          (reverse-proxy-http-object search-parser-mixin search-mixin)
    ()
  (:documentation "A reverse proxy HTTP search URL."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL-CLASS.LISP.20")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: URL; Base: 10 -*-")

(defclass reverse-proxy-http-searchable-object
          (reverse-proxy-http-search)
    ()
  (:documentation "Allow searches of http-objects such as images."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;PACKAGE.LISP.465")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: CL-USER; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(mapc #'(lambda (x) (export (intern x :url) :url)) '("REVERSE-PROXY-MIXIN" "REVERSE-PROXY-HTTP-URL"
						     "REVERSE-PROXY-HTTP-PATH" "REVERSE-PROXY-HTTP-OBJECT" "REVERSE-PROXY-HTTP-SEARCH"
                                                     "REVERSE-PROXY-HTTP-SEARCHABLE-OBJECT" "REVERSE-PROXY-FORWARDINGS" "REVERSE-PROXY-SUPERIOR"))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.869")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defgeneric write-document-post (server url http-version)
  (:documentation "Writes the response to an HTTP POST."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.869")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod write-document-post ((server basic-server-mixin) (url http-url) http-version)
  (with-slots (stream) server
    (case http-version
      ((:http/0.9 :http/1.0))
      ;; alert HTTP 1.1 or greater clients that we are ready
      (t (report-status-continue stream)
	 (send-cr-line-feed stream)
	 (force-output stream)
	 (setf (server-status server) 100.)))
    ;; process the POST
    (with-header-values (content-type transfer-encoding) *headers*
      (case transfer-encoding
	((nil :chunked))
	(t (error 'server-not-implemented :close-connection t :url url :method :post
		  :format-string "The HTTP transfer encoding, ~A, is not implemented."
		  :format-args (list transfer-encoding))))
      ;; Do the work
      (destructuring-bind (&optional doc-type doc-subtype &rest args) content-type
	(declare (ignore args))
	(post-document url doc-type doc-subtype stream)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.869")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod invoke-server-method ((server basic-server-mixin) (method (eql :post)) (http-version symbol) &aux translation-method)
  (macrolet ((handle-redirect (condition tag)
	       `(destructuring-bind (target-url &rest other-urls) (new-urls ,condition)
		  (cond ;; optimize redirect by reinvoking for singleton local url.
		    ((and (null other-urls) (local-redirect-p target-url))
		     (setf (server-url-string server) (url:name-string target-url))
		     (go ,tag))
		    (t (report-status ,condition stream))))))
    (with-slots (stream url-string) server
      (tagbody
	retry1
	   (handler-case 
	     (let ((url (url:intern-url url-string :if-does-not-exist :soft)))
	       (tagbody
		 retry2
		    (cond ((and url (setq translation-method (translation-method url)))
			   (setf (server-url server) url)
			   (with-access-control (url method server (or (url:secure-subnets url) *secure-subnets*)
						     :deny-subnets *disallowed-subnets*)
			     (case translation-method
			       (:temporary-redirect
				 (standard-url-redirect url :temporary-redirect :post))
			       (:redirect
				 (standard-url-redirect url :permanent-redirect :post))
			       (t (write-document-post server url http-version)))))
			  ((setq url (locate-controlling-url url-string method (url:valid-search-url-p url-string)))
			   (go retry2))
			  (t (error 'document-not-found :url url-string :method :post :close-connection t)))))
	     (redirection (cond) (handle-redirect cond retry1)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.869")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defgeneric write-document-trace (server url stream)
   (:documentation "Writes an HTTP trace message for URL to stream."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.869")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod write-document-trace ((server basic-server-mixin) (url url) stream)
   (with-slots (headers) server
       (with-chunked-transfer-encoding
           (stream '(:message :http) :status :success :content-location url :cache-control '(:no-cache t))
           (fast-format stream "~A~%" (%server-request server))
           (write-header-buffer headers stream t))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.869")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod invoke-server-method ((server basic-server-mixin) (method (eql :trace)) (http-version (eql :http/1.1)))
   (with-slots (stream url-string headers) server
       (let ((url (url:intern-url url-string :if-does-not-exist :soft)))
          (cond ((or (and url (translation-method url))
		          (and *auto-export* (setq url (auto-export-pathname-url url-string)))
                          (and (setq url (locate-controlling-url url-string method (url:valid-search-url-p url-string)))
                                  (translation-method url)))
                     (setf (server-url server) url) 
	             (with-access-control
	                 (url method server (or (url:secure-subnets url) *secure-subnets*)
                                :deny-subnets *disallowed-subnets*)
	                 ;; this is the meat of the trace response
	                 (write-document-trace server url stream)))
	           (t (error 'document-not-found :url url-string :method :trace))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.869")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod invoke-server-method ((server basic-server-mixin) (method (eql :options)) (http-version (eql :http/1.1)))
  (macrolet ((handle-redirect (condition tag)
	       `(destructuring-bind (target-url &rest other-urls) (new-urls ,condition)
		  (cond ;; optimize redirect by reinvoking for singleton local url.
		    ((and (null other-urls) (local-redirect-p target-url))
		     (setf (server-url-string server) (url:name-string target-url))
		     (go ,tag))
		    (t (report-status ,condition stream))))))
    (with-slots (stream url-string) server
      (tagbody
	retry1
	   (handler-case
	     (multiple-value-bind (url)
		 (url:intern-url url-string :if-does-not-exist :soft)
	       (tagbody
		 retry2
		    (cond
		      (url
		       (cond ((or (translation-method url)
				  (and *auto-export* (auto-export-pathname-url url-string)))
			      (setf (server-url server) url) 
			      (with-access-control (url method server (or (url:secure-subnets url) *secure-subnets*)
							:deny-subnets *disallowed-subnets*)
				(write-document-headers url :options stream)))
			     (t (error 'document-not-found :url url :method :options))))
		      ((and *auto-export* (auto-export-pathname-url url-string))
		       (go retry1))
		      ((setq url (locate-controlling-url url-string method (url:valid-search-url-p url-string)))
		       (go retry2))
		      (t (error 'document-not-found :url (or url url-string) :method :options)))))
	     (redirection (cond) (handle-redirect cond retry1))))))) 


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.869")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod invoke-server-method ((server basic-server-mixin) (method (eql :get)) (http-version symbol))
  (macrolet ((handle-redirect (condition tag)
               `(destructuring-bind (target-url &rest other-urls) (new-urls ,condition)
                  (cond ;; optimize redirect by reinvoking for singleton local url.
                    ((and (null other-urls) (local-redirect-p target-url))
                     (setf (server-url-string server) (url:name-string target-url))
                     (go ,tag))
                    (t (report-status ,condition stream))))))
    (with-slots (stream url-string) server
      (prog ((search-url-p (url:valid-search-url-p url-string))
             translation-method)
         retry1
            (handler-case
              (multiple-value-bind (url)
                  (url:intern-url url-string :if-does-not-exist (if search-url-p *search-url-intern-mode* :soft))
                (tagbody
                  retry2
                     (cond
                       (url
                        (cond ((setq translation-method (or (translation-method url)
							    (and *auto-export* (auto-export-pathname-url url-string) (translation-method url))))
			       (setf (server-url server) url) 
			       (with-access-control (url method server (or (url:secure-subnets url) *secure-subnets*)
							 :deny-subnets *disallowed-subnets*)
				 (write-document url translation-method stream)))
			      (t (error 'document-not-found :url url :method :get))))
                       ((and (not search-url-p) *auto-export* (auto-export-pathname-url url-string))
                        (go retry1))
                       ((setq url (locate-controlling-url url-string method search-url-p))
                        (go retry2))
                       (t (error 'document-not-found :url (or url url-string) :method :get)))))
              (redirection (cond) (handle-redirect cond retry1)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.869")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod invoke-server-method ((server basic-server-mixin) (method (eql :head)) (http-version symbol))
  (macrolet ((handle-redirect (condition tag)
               `(destructuring-bind (target-url &rest other-urls) (new-urls ,condition)
                  (cond ;; optimize redirect by reinvoking for singleton local url.
                    ((and (null other-urls) (local-redirect-p target-url))
                     (setf (server-url-string server) (url:name-string target-url))
                     (go ,tag))
                    (t (report-status ,condition stream))))))
    (with-slots (stream url-string) server
      (prog ((search-url-p (url:valid-search-url-p url-string))
             translation-method)
         retry1
            (handler-case
              ;; We use the search parent for the head method for efficiency
              ;; and in lieu of a regime for providing response functions that
              ;; compute the return of the head method for search inferiors.   8/14/96 -- JCMa.
              (multiple-value-bind (url)
                  (url:intern-url url-string :if-does-not-exist (if search-url-p *search-url-intern-mode* :soft))
                (tagbody
                  retry2
                     (cond
                       (url
			(cond ((setq translation-method (or (translation-method url)
							    (and *auto-export* (auto-export-pathname-url url-string) (translation-method url))))
			       (setf (server-url server) url) 
			       (with-access-control
				 (url method server (or (url:secure-subnets url) *secure-subnets*)
				      :deny-subnets *disallowed-subnets*)
				 (write-document-headers url translation-method stream)))
			      (t (error 'document-not-found :url url :method :head))))
                       ((and (not search-url-p) *auto-export* (auto-export-pathname-url url-string))
                        (go retry1))
                       ((setq url (locate-controlling-url url-string method search-url-p))
                        (go retry2))
                       (t (error 'document-not-found :url url-string :method :head)))))
              (redirection (cond) (handle-redirect cond retry1)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.869")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod invoke-server-method ((server basic-server-mixin) (method (eql :put)) (http-version symbol)
				 &aux url translation-method)
  (with-slots (address stream url-string) server
    (cond ;; resource already exists, so put a new version.
      ((and (setq url (or (url:intern-url url-string :if-does-not-exist :soft)
			  (locate-controlling-url url-string method (url:valid-search-url-p url-string))))
	    (setq translation-method (translation-method url)))
       (setf (server-url server) url)
       (with-access-control (url method server (or (url::write-subnets url) *write-subnets*)
				 :deny-subnets *disallowed-subnets*
				 :write-method-p t)
	 (case translation-method
	   (:temporary-redirect
	     (standard-url-redirect url :temporary-redirect :put))
	   (:redirect
	     (standard-url-redirect url :permanent-redirect :put))
	   (:reverse-proxy
	     (invoke-reverse-proxy-service server url :put))
	   (t (put-document url stream nil *check-document-versions*)))))
      (t (%put-new-resource server stream url-string)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.869")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defgeneric invoke-reverse-proxy-service (server url method)
  (:documentation "Top-level method for invoking reverse proxy service."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.869")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

;; Default method when proxy is not loaded
(defmethod invoke-reverse-proxy-service (server url method)
  (declare (ignore url http-version))
  (error 'method-not-allowed :method method :url (server-url-string server)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.464")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defun enable-url-areas (&optional (on-p (not *url-areas*)) (notify-p t))
  "Enables URL areas."
  (cond ((eq (not on-p) (not *url-areas*)) *url-areas*)
	(t (when notify-p 
	     (www-utils:notify-log-window "HTTP Server ~:[disabling~;enabling~] URL areas globally." on-p))
	   (setq *url-areas* on-p))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.870")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod invoke-server-method ((server basic-server-mixin) (method (eql :delete)) (http-version symbol))
  (macrolet ((with-delete-response ((server stream) &body body)
               `(multiple-value-bind (url status)
                    (progn . ,body)
                  ;; this could send back a 200 if there was a message to be returned 
                  ;; it would need to force output because
                  ;; provide service won't handle it 6/11/95 -- JCMa.
                  (ecase status
                    (:deleted 
                      (setf (server-status ,server) 204.)
                      (report-status-no-content ,stream))
                    (:accepted
                      (setf (server-status ,server) 202.)
                      (report-status-accepted,stream)))
                  ;; write some headers as the close of transaction
                  (write-headers* ,stream :date (server-request-time *server*) :location (url:name-string url)
                                  :server *server-version*))))
    (flet ((get-url-and-translation-method (url-string &aux url translation-method)
	     (or (and (setq url (intern-url url-string :if-does-not-exist :soft))
		      (setq translation-method (translation-method url)))
		 (and *auto-export*
		      (setq url (auto-export-pathname-url url-string))
		      (setq translation-method (translation-method url)))
		 (and (setq url (locate-controlling-url url-string method (url:valid-search-url-p url-string)))
		      (setq translation-method (translation-method url))))
	     (values url translation-method)))
      (declare (inline get-url-and-translation-method))
      (with-slots (address stream url-string) server
	(multiple-value-bind (url translation-method)
	    (get-url-and-translation-method url-string)
	  (cond ((and url translation-method)
		 (setf (server-url server) url)
		 (with-access-control (url method server (or (url::write-subnets url) *write-subnets*)
					   :write-method-p t)
		   (case translation-method	; redirect when there is forwarding.
		     (:temporary-redirect
		       (standard-url-redirect url :temporary-redirect :delete))
		     (:redirect
		       (standard-url-redirect url :permanent-redirect :delete))
		     (:reverse-proxy
		       (invoke-reverse-proxy-service server url :delete))
		     (t (with-delete-response (server stream)
					      (delete-document url stream))))))
		(t (error 'document-not-found :url url-string :method :delete))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.871")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(setf (documentation 'export-url 'function)

      "HTTP:EXPORT-URL is the primary method that exports URLS to make them
accessible via the http server. URL is either a string or an interned URL to
be exported.  EXPORT-TYPE is the method to use in exporting URL.


I. Basic Export Types: These involve making the contents of a file accessible via
a URL. These types require URLs that are object (i.e., have a name and extension).

        :HTML-FILE (&key pathname)
        :TEXT-FILE (&key pathname)
        :RTF-FILE (&key pathname)

        :GIF-IMAGE (&key pathname)
        :JPEG-IMAGE (&key pathname)
        :X-BITMAP-IMAGE (&key pathname)
        :PICT-IMAGE (&key pathname)
        :TIFF-IMAGE (&key pathname)

        :BASIC-AUDIO (&key pathname)
        :AIFF-AUDIO (&key pathname)
        :WAV-AUDIO (&key pathname)

        :MPEG-VIDEO (&key pathname)
        :QUICKTIME-VIDEO (&key pathname)

        :VRML-WORLD (&key pathname)

        :DIRECTORY (&key pathname immediate-export recache recursive-p)
        :HTML-DIRECTORY (&key pathname immediate-export recache recursive-p)
        :TEXT-DIRECTORY (&key pathname immediate-export recache recursive-p)
        :LISP-DIRECTORY (&key pathname immediate-export recache recursive-p)
        :IMAGE-DIRECTORY (&key pathname immediate-export recache recursive-p)
        :AUDIO-DIRECTORY (&key pathname immediate-export recache recursive-p)
        :VIDEO-DIRECTORY (&key pathname immediate-export recache recursive-p)
        :WORLD-DIRECTORY (&key pathname immediate-export recache recursive-p)
        :APPLICATION-DIRECTORY (&key pathname immediate-export recache recursive-p)

:DIRECTORY exports all files whose resource type is known.
Others export a specific content type, and ignore other file types.
When recursive-p is non-null, all subdirectories are also exported.
Otherwise, subdirectories are ignored.

When *AUTO-EXPORT* is non-null, new files are automatically exported when they
are scoped by one of these directory export types. Auto-export occurs on
demand for the GET and HEAD methods. If *AUTO-EXPORT* is :ON-DEMAND,
files are exported when they are first requested rather than at the
time the directory is exported. When exporting a directory, a non-null
argument to :IMMEDIATE-EXPORT overrides lazy directory exporting. In general,
on-demand directory exports trade faster server start up for a slightly slower
first access to a file URL within the directory. When :RECACHE is non-null,
a directory export updates the export parameters for every inferior. This
parameter forces traversal of the entire structure, like a non-null
:IMMEDIATE-EXPORT would.

A directory-style list in HTML is the default content returned for the
get method on a URL directory path. This behavior is customized by
providing a response function via the :DIRECTORY-WRITER keyword.  This
response function is called with the arguments (URL STREAM) and must
return non-null when it handles the response. If it declines to handle
the response, it may return NIL, and the standard method will list the
directory as usual. HTTP:WRITE-INDEXED-DIRECTORY-LISTING is a directory
writer that will serve the contents of an index.html file found in
the directory. Other computed returns are possible.

Note that the presence in file or directory names of escaped characters 
(see *ESCAPED-CHARACTERS*) will lead to inconsistent results, and possibly
errors. Space and question mark are examples.

        :PDF-FILE  (&key pathname)
        :POSTSCRIPT-FILE (&key pathname)

        :BINHEX-FILE (&key pathname)
        :STUFFIT-FILE  (&key pathname)
        :COMPRESSED-FILE (&key pathname)
        :MAC-BINARY-FILE (&key pathname)

        :WORD-FILE  (&key pathname)
        :POWER-POINT-FILE (&key pathname)
        :EXCEL-FILE  (&key pathname)

The following export types support inline plug-ins on the client side.
Plug-ins can be referenced using NS2.0:WITH-EMBEDDED-SCRIPT

        Inline speech synthesis is available using a macintalk plug-in
        from http://www.mvpsolutions.com/PlugInSite/Talker.html

        :TALK-AUDIO (&key pathname)

The Java language provides applets that execute on the client.  This kind of
mobile code is supported with the following export types and the HMTL
generation macro WITH-JAVA-APPLET.
        
        :JAVA-FILE (&key pathname)
        :JAVA-BINARY (&key pathname)
        :JAVA-SCRIPT-FILE (&key pathname)

:JAVA-FILE exports the source code whereas :JAVA-BINARY provides the byte
compiled binary to the client running the applet. :JAVA-SCRIPT-FILE exports
the browser scripting language, JavaScript, which is simpler and easier to use
than Java itself.

II. Redirect Export Types: These export types inform the client to
look elsewhere for a URL.  They work for the GET and HEAD operations.
The exported URL can be either an HTTP object or an HTTP path.

        :REDIRECT (&key alternate-urls pathname)
        :TEMPORARY-REDIRECT (&key alternate-urls pathname)

Alternatively, a computed response may call REDIRECT-REQUEST to issue a
redirect rather than serving content itself.

III. Search Export Types: these involve performing searches using the search
index or map search facilities in HTTP. Search URLs must end with ? so that
the system can composed the right combination of classes. In all cases, the
response function must compute the answer and return HTML over the http stream
to the client.

General Purpose Searches

        :SEARCH (&key response-function search-parser search-database)

        This exports a URL that performs searches by calling RESPONSE-FUNCTION with
        the arguments URL and STREAM. The search database and search parameters are
        cached on the URL and accessible via URL:SEARCH-DATABASE and URL:SEARCH-KEYS.

        The optional export argument SEARCH-PARSER is the parser that obtains URL:SEARCH-KEYS
        from the suffix of a search URL. Several parsers are predefined:

                URL:STANDARD-PARSE-SEARCH-INFO: This standard parser for
                search URLs. It tests whether the search info encodes form or
                list data and calls the appropriate of the next two parsers.

                URL:PARSE-SEARCH-INFO: This normal parser for search URLs
                produces a list of search parameters using + as the delimiter.

                URL:PARSE-SEARCH-INFO-AS-QUERY-ALIST: This parser for URL
                encoded form values returns an alist just like posting a form
                would. This parser should be used when an HTML form is
                returned to a search URL.  However, this method of returning
                form values is limited to 1024 characters total in the URL,
                and therefore, it's use in new code is deprecated.

        Users may provide other specialized parsers. They should accept
        the arguments (url-string start end) and need not located the ?
        suffix delimiter.

        The export argument, SEARCH-WRITER, allows a URL to specialize how the
        parsed presentation on URL:SEARCH-KEYS is written. Several writers are
        predefined.

                URL:STANDARD-WRITE-SEARCH-INFO: This standard writer for
                search URLs. It tests whether the search info encodes form or
                list data and calls the appropriate of the next two writers.

                URL:WRITE-SEARCH-INFO: This normal writer for search URLs
                produces a list of search parameters using + as the delimiter.

                URL:WRITE-SEARCH-INFO-AS-QUERY-ALIST: This writer for URL
                encoded form values that prints alist values as name value pairs
                using the urlencoding syntax.

        The export argument, HEADER-FUNCTION, allows a search URL to specialize
        how it responds to the HEAD method based on runtime knowledge.
        HEADER-FUNCTION is called with the argument URL  and returns values
        used to determine respoonse to HEAD. The returned values are:
        (CONTENT-TYPE CONTENT-LENGTH LAST-MODIFICATION VERSION CHARSET PUBLIC ALLOW)
        
                CONTENT-TYPE (required) is a keyword or content-type. Default
                is HTML.

                CONTENT-LENGTH (optional) is the length in bytes of the entity
                body. Default is none.

                LAST-MODIFICATION (optional) is a universal time indicating
                when the entity was last changed. Default is now.
 
                VERSION (optional) is a number or string that distinguishes
                different versions of the entity. Default is none.

                CHARSET (optional) is a keyword indicating the character set
                of the entity. Default is :ISO-8859-1.

                PUBLIC (optional) is a boolean indicating whether the resource
                is available to the public. Default is none.

                ALLOW (optional) is a list of HTTP method keywords available
                on the resource. Default is none.

Image Searches

   Image Maps

        :IMAGE-MAP (&key pathname export-type map-format map-pathname
                         search-parser search-writer header-function)

        This exports the image in PATHNAME as IMAGE-EXPORT-TYPE and
        establishes a response function based on the image map in MAP-PATHNAME
        whose MAP-FORMAT is either :CERN or :NCSA. EXPORT-TYPE is the
        appropriate image search export type (see below).

   Direct Searches

        These provide direct control over the response function for image searches.

        :GIF-IMAGE (&key pathname response-function search-database
                         search-parser search-writer header-function)
        :JPEG-IMAGE (&key pathname response-function search-database
                          search-parser search-writer header-function)
        :PNG-IMAGE (&key pathname response-function search-database
                         search-parser search-writer header-function)
        :X-BITMAP-IMAGE (&key pathname response-function search-database
                              search-parser search-writer header-function)
        :PICT-IMAGE (&key pathname response-function search-database)
                          search-parser search-writer header-function)
        :TIFF-IMAGE (&key pathname response-function search-database
                          search-parser search-writer header-function)

        These export types allow the client's user to click on images and
        invoke a response from the server.  These URL are both objects and
        searchable.  When they are requested without the ? suffix, the
        contents of their associate image file is returned.  When the ? suffix
        appears, their RESPONSE-FUNCTION is called on the arguments URL and
        STREAM. See the macro HTTP:WITH-IMAGE-COORDINATES automatically binds
        the X and Y coordinates.

IV. Computed Export Types: These compute responses returned to clients.

        :COMPUTED (&key response-function header-function) RESPONSE-FUNCTION
        is called with the arguments URL and STREAM and is responsible for
        returning HTML to the client. :COMPUTED has an optional pathname so
        that the computation may reference a file, if necessary.
        HEADER-FUNCTION is documented section III.
        
        :HTML-FORM (&key response-function pathname durable-form-values
        server) :HTML-FORM returns the contents of PATHNAME when it is
        requested via GET.  When there is a POST, its RESPONSE-FUNCTION is
        called with URL, STREAM, and FORM-ALIST.  FORM-ALIST is an alist of
        (QUERY RAW-VALUE) for all the queries in the form that the client
        returns. QUERY is a keyword.  When a client returns multiple
        raw-values for a QUERY, these are aggregated into a list of the values
        associated with the query in a single, order-preserving entry.
        DURABLE-FORM-VALUES is a boolean and controls whether query values
	are new, copied strings or indirect arrays pointing into a volitile
	form data buffer. High-volume applications or forms large query values
	can gain efficiency by using durable form values, all operations on
	these values must be completed within the context of the form's
	response function.

        :COMPUTED-FORM (&key form-function response-function header-function
        durable-form-values server) :COMPUTED-FORM is a cross between
        :COMPUTED and :HTML-FORM that provides FORM-FUNCTION to generate html
        just like :COMPUTED and RESPONSE-FUNCTION to handle the post method
        when form values are returned. FORM-FUNCTION is called with the
        arguments URL and STREAM and is responsible for returning HTML to the
        client. response-function takes the same arguments as :HTML-FORM.

        :SCRIPT (&key script header-function) Script is a client-side script
        defined with NS2.0:DEFINE-SCRIPT. These scripts may merely deliver a
        static copy of the client-side script, or they may perform a
        computation that emits an appropriate script.

        :SHTML-FILE (&key pathname header-function) This is a computed
        response that is inserted in a static HTML file containing
        server-parsed HTML. When an SHTML element is found by the server, it
        inserts the result of a computation in place of the SHTML element.
        SHTML elements are delimted by <!--# and --> and look like:

                <!--#include file=\"insert.text\"-->

        INCLUDE is an SHTML operation that requires a FILE. For security
        reasons, FILE must be in the same directory as the STHML file. Access
        may be controlled by providing the optional SUBNETS parameter, which
        is a comma-separated string of IP addresses without whitespace.

         <!--#include file=\"insert.text\" subnets=\"128.52.0.0,18.21.0.0\"-->
        
        EVAL is a more general SHTML operation that requires an ACTION
        parameter. ACTION is a string denoting an SHTML action. Specific
        parameters may be required by individual actions. Here, DATE-TIME
        is the operation and FORMAT is a parameter.

              <!--#eval action=\"date-time\" format=\"iso\"-->

        Predefined actions are documented by HTTP:SHOW-SHTML-ACTIONS. New
        SHTML actions are defined with HTTP:DEFINE-SHTML-ACTION. Files with
        the extention SHTML are autoexported by HTML directory export types.


V. Reverse Proxy Export Type

This provides a reverse proxying facility for servers, the reverse of normal
client proxying. In a reverse proxy, when a client requests a specific URL,
the server proxies the request to an origin server and returns the resulting
HTTP message to the client. Thus, the reverse proxy behaves as if it was the
origin server.

To perform this task, the reverse proxy needs to know the origin server to
which a particular incoming client HTTP request is mapped. This information is
provided at URL export time by the application. Although all requests on a
particular port may be reverse proxied, this facility allows different
segments of the URL space to be delegated to different origin servers by
according to the local URL provided to EXPORT-URL. Applications may choose to
either run with local proxy caching or not, depending on the value of
*PROXY-CACHING-P*. Normally, local proxy caching will improve performance.
Note that this facility is only available when the proxy server is loaded.

        :REVERSE-PROXY (&key proxy-forwardings) proxy-forwardings is a list of
        (DOMAIN-NAME PORT PROTOCOL). If more than one host is provided,
        forwarding distributed over the set of origin servers. Only the most
        general URL needs to be exported because the facility automatically
        forwards requests to inferior URLs. Although the reverse proxy
        enforces all security parameters on the root reverse-proxy URL,
	all other issues concerning the URL are delegated to the origin server,
	for example, method availability and URL existence. When desired, a 
	proxy may be placed between the reverse proxy and the origin server using
	DEFINE-CLIENT-PROXY-MAPPING.


VI. Export Parameters and Issues

1. Portable Exports: The #u reader macro merges a partial URL specification
against the default for the local host. Use this when you be able to load the
same exports file on different hosts. The #u reader macro has an extended syntax
that allows you to overview the default host and port specified by
the server configuration. The syntax is 

     #u(url-string :host host-string :port port-number)

URL-STRING is a relative URI. HOST-STRING is the domain name or
IP string for the host. PORT-NUMBER is an integer.


2. Security: 

  A. Subnet Security: Secure subnets are a list of IP addresses, where 0 is a
  wildcard. For example, 128.52.0.0 matches all the subnets at the AI lab.
  The follow export arguments allow control over which clients can perform
  HTTP methods such as GET, HEAD, POST, :OPTIONS, or :TRACE (read access)
  versus PUT or DELETE (write access).

     :READ-SUBNETS allows read access to be specified at the level of
     URLs as they are exported.

     :WRITE-SUBNETS allows write access to be specified at the level of
     URLs as they are exported.

     DEFINE-READ-SUBNETS restricts read access globally to the server.

     DEFINE-WRITE-SUBNETS restricts write access globally to the server.

  Write access presumes read access, and consequently, IP addresses from the
  write subnets need not be included in the read subnets. To select the global
  authentication policy for write methods, HTTP:*ACCEPT-WRITE-METHODS*.

  DEFINE-SUBNET can be used to specify named subnet for use in subnet 
  specifications. Note that named subnets are resolved when they are used,
  for example by a call to EXPORT-URL, and therefore, changes to named
  subnets require re-export of URL referring to them.

  B. User Authentication: URL authentication can be specified using the
  following export arguments.

     :AUTHENTICATION-REALM a realm obtainable via INTERN-REALM.
     These can be created with ADD-REALM.

     :CAPABILITIES a URL access control obtainable via INTERN-ACCESS-CONTROL.
     These can be created with ADD-ACCESS-CONTROL-GROUP.

  See also: ADD-GROUP, ADD-USER, and SAVE-AUTHENTICATION-DATA.

3. Expiration: The expiration time for a url is issued as an EXPIRES header so
that proxy servers can determine when they need to refresh their cache.

Expiration is controlled by providing the :EXPIRATION when exporting any URL.
If expiration is not provided, the default is no expiration date.

The :EXPIRATION keyword takes one argument, which is either keyword or a list
of (keyword &rest arguments).

     Arguments                       Meaning

 :NO-EXPIRATION-HEADER        --  No EXPIREs header is issued.
 :NEVER                       --  EXPIRES header indicates one year from now.
 (:INTERVAL <universal-time>) --  EXPIRES header indicates now + <universal-time>.
 (:TIME <universal-time>)     --  EXPIRES header indicates an <universal-time>.
 (:FUNCTION <function-spec>)  --  EXPIRES header indicates universal time computed by
                                  applying <function-spec> to URL.  <function-spec>
                                  should return a universal for use in the EXPIRES header
                                  or NIL, in which case no EXPIRES header is issued.

4. Character Sets: The :CHARACTER-SET keyword allows any URLs whose content
type is TEXT (e.g., text/plain, text/html) to be exported with character sets
other than the HTTP default :ISO-8859-1, or subsets. The valid character sets
for HTTP are:  :US-ASCII, :ISO-8859-1, :ISO-8859-2, :ISO-8859-3, :ISO-8859-4,
:ISO-8859-5, :ISO-8859-6, :ISO-8859-7, :ISO-8859-8, :ISO-8859-9, :ISO-2022-JP,
:ISO-2022-JP, :ISO-2022-KR, :UNICODE-1-1, :UNICODE-2-2-UTF-7,
:UNICODE-2-2-UTF-7.  Whenever TEXT content types use a character set other
than :ISO-8859-1, the HTTP requires explicit specification via this export
keyword.

6. Languages:  The :LANGUAGE keyword may be supplied for any exported
URL. The value is a sequence of keywords denoting the language(s) in which the
resource is written. HTTP 1.1 defines these keywords in section 3.10 to
conform with RFC 1766. They can be a two-letter ISO 639 language abbreviation,
optionally with a two-letter ISO 3166 country code as a subtag.

7. Documentation: Keywords and a descriptive string can be attached to URLs at
export time.  For directory exports, note that these arguments apply to ALL
URLs exported during the directory walk.

     :KEYWORDS                  A list of keywords.
     :DOCUMENTATION             A string describing the URL.


8. Virtual Hosts: HTTP 1.1 requires a virtual hosting facility,
which this server implements. You can define a virtual host (or
vanity name) that will be served by the physical server from
the same IP address. Any URIs served by a virtual host must be
exported by specifying the absolute URI, including host and port
number.  The #u reader macro may be useful here.  The following
operators are available for use with virtual hosts:

     ADD-VIRTUAL-HOST: Adds a virtual host on a virtual port and
                       and makes URLs served by that host available
                       to HTTP 1.1 or greater clients.

     REMOVE-VIRTUAL-HOST: Makes the virtual host unavailable, but does
                          does not remove any URLs it exports.

9. New static export types for data stored in files can be defined with
DEFINE-URL-EXPORT-TYPE.

10. HTTP 1.1 Cache Control: The keywords below may be supplied when exporting
any URL in order to statically control how downstream proxies and caches
handle the content associated with a URL.

        :PUBLIC -- If the value is T, the entire message is cachable by any
        cache even if it would not normally be cached.

        :PRIVATE -- If the value is T, the entire message is intended for a
        single user and must not be cached by a shared cache. If the value is
        a list of keywords denoting specific header, then only these headers
        should be considered private.

        :NO-CACHE -- If the value is T, the entire message must not be cached
        by any cache along the request chain.  If the value is a list of
        keywords denoting specific headers, then only these headers should be
        discarded before caching.

        :NO-STORE -- If the value is T, the entire message must not be stored
        in any non-volatile storage by any cache along the request chain.

        :MAX-AGE -- The value is the number of seconds for which the response
        is valid, after which it should be revalidated.  This directive
        overrides the expiration header, thus allowing HTTP 1.1 server to
        provide a longer expiration time to HTTP 1.1 servers and proxies.
        This defaults to a value derived from the expiration time.

        :MUST-REVALIDATE -- If the value is T, caches and proxies must not
        serve the resource without revalidating it once it becomes stale, even
        if configured to operate with state data. Servers should send this
        directive if and only if failure to revalidate will result in
        incorrect operation. Recipients must not take any automated action
        that violates this directive.

        :PROXY-REVALIDATE -- If the value is T, this directive is the same as
        :MUST-REVALIDATE except that it applies only to proxies and not
        non-shared user caches. It can be used on response to an authenticated
        request to permit the user's cache to store and later return the
        response without needing to revalidate it.

        :NO-TRANSFORM -- If the value is T, caches and proxies must not change
        the body of the message or change headers describing the content.

11. Property List: The PROPERTY-LIST keyword adds a property list of
alternating keywords and value to a URL.  These properties can be read
and set via GET-VALUE.")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.413")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(SCL:FUNDEFINE '(METHOD WRITE-LOCAL-NAME (HTTP-PATH)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.413")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod write-local-name ((url path-mixin) &optional stream)
  (write-path url stream)
  (write-char #\/ stream))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.873")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

;; Reverts the class to the origin class whenever a url is re-exported as a
;; different export type/  1/11/97 -- JCMa.
(defmethod export-url :around ((url url:translation-method-mixin) export-type &rest args)
  (declare (ignore args))
  (with-slots (url:translation-method) url
    (when (and url:translation-method
               (not (eql export-type url:translation-method)))
      (url::revert-class url))
    (call-next-method)
    (case (protocol url)
      (:http
	(let ((port (url:host-port url)))
	  (when (eq port *standard-http-port*)
	    (ensure-http-protocol-on-port port)))))
    url))

