;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for HTTP-CLIENT-SUBSTRATE version 4.20
;;; Reason: Remove function (CLOS:METHOD HTTP::GET-USER-NAME+PW (URL:HTTP-URL CONS T T T) :AROUND): undefine.
;;; Remove function HTTP::ENCODE-REALM-USERNAME: undefine.
;;; Function HTTP::WITH-PW-REALM-KEY:  -
;;; Function (CLOS:METHOD HTTP::PW-DATA (URL:HTTP-URL CONS (EQL :DIGEST))):  -
;;; Remove function (CLOS:METHOD HTTP::PW-DATA (T STRING (EQL :BASIC))): -
;;; Function (CLOS:METHOD HTTP::PW-DATA (URL:HTTP-URL STRING (EQL :BASIC))):  -
;;; Function HTTP::WITH-PW-REALM-KEY:  -
;;; Function (CLOS:METHOD HTTP::PW-DATA (HTTP::HTTP-PROXY CONS (EQL :DIGEST))):  -
;;; Remove function (CLOS:METHOD HTTP::CACHE-PW-DATA (T STRING (EQL :BASIC) T T)): -
;;; Function (CLOS:METHOD HTTP::CACHE-PW-DATA (URL:HTTP-URL STRING (EQL :BASIC) T T)):  -
;;; Function (CLOS:METHOD HTTP::CACHE-PW-DATA (T T (EQL :DIGEST) T T)):  -
;;; Function (CLOS:METHOD HTTP::CACHE-PW-DATA (HTTP::HTTP-PROXY T (EQL :DIGEST) T T)):  -
;;; Function (CLOS:METHOD HTTP::GET-USER-NAME+PW (URL:HTTP-URL STRING T T T) :AROUND):  -
;;; Remove function HTTP::CLIENT-AUTHENTICATE-USER: -
;;; Function (CLOS:METHOD HTTP::PW-DATA (HTTP::HTTP-PROXY STRING (EQL :DIGEST))):  -
;;; Function (CLOS:METHOD HTTP::CACHE-PW-DATA (HTTP::HTTP-PROXY STRING (EQL :BASIC) T T)):  -
;;; Function (CLOS:METHOD HTTP::CACHE-PW-DATA (HTTP::HTTP-PROXY CONS (EQL :DIGEST) T T)):  -
;;; Function (CLOS:METHOD HTTP::CACHE-PW-DATA (HTTP::HTTP-PROXY STRING (EQL :DIGEST) T T)):  -
;;; Variable HTTP::*PROXY-AUTHENTICATION-RETRIES*:  -
;;; Function HTTP::HANDLING-PROXY-AUTHENTICATION:  -
;;; Function HTTP::SEND-REQUEST:  add proxy-authorization arg.
;;; Variable HTTP::*PROXY-AUTHENTICATION-RETRIES*:  -
;;; Function HTTP::HANDLING-PROXY-AUTHENTICATION:  -
;;; Function HTTP::SET-PROXY-AUTHENTICATION-CREDENTIALS:  -
;;; Function (CLOS:METHOD HTTP::SET-PROXY-AUTHENTICATION-CREDENTIALS (HTTP::HTTP-PROXY T T T T)):  -
;;; Function HTTP::COMPUTE-BASIC-AUTHORIZATION-HEADER:  -
;;; Function (CLOS:METHOD HTTP::GET-AUTHORIZATION-HEADER (URL:HTTP-URL T (EQL :BASIC) T T)):  -
;;; Function HTTP::COMPUTE-DIGEST-AUTHORIZATION-HEADER:  -
;;; CLOS class HTTP::PROXY-AUTHENTICATION-MIXIN:  -
;;; CLOS class HTTP::HTTP-PROXY:  -
;;; Remove function (CLOS:METHOD HTTP::GET-USER-NAME+PW (URL:HTTP-URL CONS T T T) :AROUND): undefine.
;;; Remove function HTTP::GET-USER-NAME+PW: undefine.
;;; Remove function HTTP::GET-AUTHORIZATION-HEADER: -
;;; Remove function HTTP::CLIENT-AUTHENTICATE-USER: -
;;; Function (CLOS:METHOD HTTP::GET-USER-NAME+PW (T T T T T)):  default method.
;;; Written by JCMa, 9/18/01 18:06:04
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.6,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.147,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.4, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.37, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Jcma 44, HTTP Proxy Server 6.25,
;;; HTTP Client Substrate 4.19, Statice Server 466.2, HTTP Client 51.0,
;;; Image Substrate 440.4, Essential Image Substrate 433.0, Ivory Revision 5,
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.11),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1).

;;; Patch file for HTTP-CLIENT-SUBSTRATE version 4.20
;;; Written by JCMa, 11/01/01 18:51:18
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.6,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.154,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.4, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.5, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.39, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.3,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Jcma 44, HTTP Proxy Server 6.27,
;;; HTTP Client Substrate 4.20, Statice Server 466.2, HTTP Client 51.1,
;;; Image Substrate 440.4, Essential Image Substrate 433.0,
;;; W4 Constraint-Guide Web Walker 45.11, Ivory Revision 5, VLM Debugger 329,
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.11),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).


;;; Patch file for HTTP-CLIENT-SUBSTRATE version 4.20
;;; Written by JCMa, 10/20/01 16:59:28
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.6,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.152,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.4, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.4, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.38, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.3,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, HTTP Proxy Server 6.26,
;;; HTTP Client Substrate 4.20, Statice Server 466.2, HTTP Client 51.0,
;;; Image Substrate 440.4, Essential Image Substrate 433.0, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x976 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
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


;;; Patch file for HTTP-CLIENT-SUBSTRATE version 4.20
;;; Written by JCMa, 10/20/01 16:22:57
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.6,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.151,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.4, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.4, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.38, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.3,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; HTTP Proxy Server 6.26, HTTP Client Substrate 4.19, Statice Server 466.2,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.16,
;;; DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x976 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
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




(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL)
  (SCT:REQUIRE-PATCH-LEVEL-FOR-PATCH '(CL-HTTP 70. 152.)))


(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:CLIENT;AUTHENTICATION.LISP.3"
  "HTTP:CLIENT;AUTHENTICATION.LISP.6"
  "HTTP:CLIENT;AUTHENTICATION.LISP.10"
  "HTTP:CLIENT;AUTHENTICATION.LISP.11"
  "HTTP:CLIENT;CLIENT.LISP.290"
  "HTTP:CLIENT;AUTHENTICATION.LISP.12"
  "HTTP:CLIENT;AUTHENTICATION.LISP.13"
  "HTTP:CLIENT;AUTHENTICATION.LISP.14"
  "HTTP:CLIENT;AUTHENTICATION.LISP.15"
  "HTTP:CLIENT;AUTHENTICATION.LISP.17"
  "HTTP:CLIENT;AUTHENTICATION.LISP.18"
  "HTTP:CLIENT;AUTHENTICATION.LISP.19"
  "HTTP:CLIENT;CLIENT.LISP.291"
  "HTTP:CLIENT;AUTHENTICATION.LISP.21")





;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.3")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(SCL:FUNDEFINE '(METHOD GET-USER-NAME+PW (HTTP-URL CONS T T T) :AROUND))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.3")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmacro with-pw-realm-key ((url-or-proxy realm &key (variable-name 'realm-key) (stack-cons-p t)) &body body)
  `(let ((,variable-name (list (etypecase ,url-or-proxy 
				 (url (url:host-string ,url-or-proxy))
				 (proxy (proxy-domain-name ,url-or-proxy)))
			       ,realm)))
     ,@(when stack-cons-p `((declare (dynamic-extent ,variable-name))))
     ,@body))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.3")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod pw-data ((url url:http-url) (realm cons) (method (eql :digest)))
  (destructuring-bind (realm-name &key &allow-other-keys) realm
    (let ((table *realm-username-table*)
	  entry)
      (when (setq entry (or (gethash (url:name-string url) table)
			    (gethash (url:directory-context-string url) table)
			    (gethash (url:root-context-string url) table)
			    (with-pw-realm-key (url realm)
			      (gethash realm-key table))))
	(decode-realm-username realm-name entry)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.3")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(SCL:FUNDEFINE '(METHOD PW-DATA (T STRING (EQL :BASIC))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.3")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod pw-data ((url url:http-url) (realm string) (method (eql :basic)))
  (let (entry)
    (when (setq entry (with-pw-realm-key (url realm)
			(gethash realm-key *realm-username-table*)))
      (decode-realm-username realm entry))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.3")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(SCL:FUNDEFINE '(METHOD CACHE-PW-DATA (T STRING (EQL :BASIC) T T)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.3")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod cache-pw-data ((url url:http-url) (realm string) (method (eql :basic)) user-name password)
  (let ((entry (encode-realm-username realm user-name password)))
    (with-pw-realm-key (url realm :stack-cons-p nil)
      (setf (gethash realm-key *realm-username-table*) entry))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.3")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod cache-pw-data (url realm (method (eql :digest)) user-name password)
  (destructuring-bind (realm-name &key domain &allow-other-keys) realm
    (let ((entry (encode-realm-username realm-name user-name password))
	  (table *realm-username-table*))
      (dolist (uri domain)
	(setf (gethash uri table) entry))
      (with-pw-realm-key (url realm :stack-cons-p nil)
	(setf (gethash (url:name-string url) table) entry
	      (gethash (url:directory-context-string url) table) entry
	      (gethash (url:root-context-string url) table) entry
	      (gethash realm-key table) entry)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.3")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod cache-pw-data ((proxy http-proxy) realm (method (eql :digest)) user-name password)
  (destructuring-bind (realm-name &key domain &allow-other-keys) realm
    (let ((entry (encode-realm-username realm-name user-name password))
	  (table *realm-username-table*))
      (dolist (uri domain)
	(setf (gethash uri table) entry))
      (with-pw-realm-key (proxy realm :stack-cons-p nil)
	(setf (gethash (gethash proxy table) table) entry
	      (gethash realm-key table) entry)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.3")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(SCL:FUNDEFINE 'CLIENT-AUTHENTICATE-USER)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.6")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod pw-data ((proxy http-proxy) (realm string) (method (eql :digest)))
  (let ((table *realm-username-table*)
	entry)
    (when (setq entry (or (gethash proxy table)
			  (with-pw-realm-key (proxy realm)
			    (gethash realm-key table))))
      (decode-realm-username realm entry))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.6")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod pw-data ((proxy http-proxy) (realm cons) (method (eql :digest)))
  (destructuring-bind (realm-name &key &allow-other-keys) realm
    (pw-data proxy realm-name method)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.6")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod get-user-name+pw :around ((url url:http-url) (realm cons) method proxy-p reprompt-p &optional (stream *terminal-io*))
  (flet ((proxy-pw-key (url) 
	   (or (get-client-proxy url)
	       (error "Client associates no proxy with ~S." (url:name-string url)))))
    (declare (inline proxy-pw-key))
    (let ((pw-cache-key (if proxy-p (proxy-pw-key url) url)))
      (unless reprompt-p
	(multiple-value-bind (user-name password)
	    (pw-data pw-cache-key realm method)
	  (when (and user-name password)
	    (return-from get-user-name+pw (values user-name password)))))
      ;; if no cache hit, then prompt the user.
      (multiple-value-bind (user-name password)
	  (call-next-method url realm method proxy-p reprompt-p stream)
	(cache-pw-data pw-cache-key realm method user-name password)
	(values user-name password)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.6")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod cache-pw-data ((proxy http-proxy) (realm cons) (method (eql :digest)) user-name password)
  (destructuring-bind (realm-name &key domain &allow-other-keys) realm
    (when domain
      (let ((entry (encode-realm-username realm-name user-name password))
	    (table *realm-username-table*))
	(dolist (uri domain)
	  (setf (gethash uri table) entry))))
    (cache-pw-data proxy realm-name method user-name password)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.10")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defgeneric set-proxy-authentication-credentials (proxy method realm user-name password)
  (:documentation "Sets the authentication credentials for the access-controlled upstream proxy, PROXY.
REALM is a string denoting the remote realm to which user-name and password apply.
USER-NAME and PASSWORD are strings."))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.10")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod set-proxy-authentication-credentials ((proxy http-proxy) method realm user-name password)
  (cache-pw-data proxy realm method user-name password))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.11")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod cache-pw-data ((proxy http-proxy) (realm string) (method (eql :digest)) user-name password)
  (let ((entry (encode-realm-username realm user-name password))
	(table *realm-username-table*))
    (with-pw-realm-key (proxy realm :stack-cons-p nil)
      (setf (gethash proxy table) entry
	    (gethash realm-key table) entry))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.12")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun compute-basic-authorization-header (user-id pw)
  (let ((hash (concatenate 'string user-id ":" pw)))
    (declare (dynamic-extent hash))
    `(:basic ,(base64:base64-encode-vector hash :characters-p t))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CLIENT.LISP.290")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defclass proxy-authentication-mixin
	  ()
    ((authentication-method :initform nil :initarg :authentication-method :accessor proxy-authentication-method)
     (authentication-header :initform nil :initarg :authentication-header :accessor proxy-authentication-header)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CLIENT.LISP.290")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defclass http-proxy
	  (proxy-authentication-mixin basic-proxy-mixin)
    ()
  (:documentation "HTTP Proxy object.")) 


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.12")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(PROGN
(defgeneric proxy-compute-authorization-header (http-proxy method http-method uri)
  (:documentation "Returns an authorization header suitable for an HTTP-METHOD request for URI to HTTP-PROXY
using METHOD for authentication."))

(defmethod proxy-compute-authorization-header ((proxy http-proxy) (method (eql :basic)) http-method uri)
  (declare (ignore http-method uri))
  (proxy-authentication-header proxy))


)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.12")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun proxy-get-authorization-header (proxy http-method uri)
  (let ((method (proxy-authentication-method proxy)))
    (when method
      (proxy-compute-authorization-header proxy method http-method uri))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.12")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun proxy-note-authorization-header (proxy authorization-header)
  (destructuring-bind (method . args) authorization-header
    args
    (without-preemption
      (setf (proxy-authentication-method proxy) method
	    (proxy-authentication-header proxy) authorization-header))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.13")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(SCL:FUNDEFINE '(METHOD GET-USER-NAME+PW (HTTP-URL CONS T T T) :AROUND))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.13")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod proxy-compute-authorization-header ((proxy http-proxy) (method (eql :digest)) http-method uri)
  (destructuring-bind (authentication-method &key realm algorithm nonce opaque &allow-other-keys) (proxy-authentication-header proxy)
    authentication-method
    (multiple-value-bind (user-id pw)
	(get-user-name+pw uri realm method t nil)  
      (when (and user-id pw)
	(compute-digest-authorization-header uri http-method realm user-id pw algorithm nonce opaque)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.13")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun compute-digest-authorization-header (url http-method realm-name user-id pw algorithm nonce opaque)
  (let* ((digest-fctn (algorithm-digest-function algorithm))
	 (user-realm-pw (concatenate 'string user-id ":" realm-name ":" pw))
	 (user-realm-pw-digest (funcall digest-fctn user-realm-pw))
	 (uri-method (concatenate 'string (symbol-name http-method) ":" (relative-name-string url)))
	 (uri-method-digest (funcall digest-fctn uri-method))
	 (response (concatenate 'string user-realm-pw-digest ":" nonce ":" uri-method-digest))
	 (response-digest (funcall digest-fctn response)))
    (declare (dynamic-extent user-realm-pw user-realm-pw-digest uri-method uri-method-digest response))
    `(:digest :username ,user-id :realm ,realm-name :nonce ,nonce
	      :uri ,(name-string url) :response ,response-digest
	      :opaque ,opaque :algorithm ,algorithm)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.13")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod proxy-compute-authorization-header ((proxy proxy-authentication-mixin) (method (eql :basic)) http-method uri)
  (declare (ignore http-method uri))
  (proxy-authentication-header proxy))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.13")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod proxy-compute-authorization-header ((proxy proxy-authentication-mixin) (method (eql :digest)) http-method uri)
  (destructuring-bind (authentication-method &key realm algorithm nonce opaque &allow-other-keys) (proxy-authentication-header proxy)
    authentication-method
    (multiple-value-bind (user-id pw)
	(get-user-name+pw uri realm method t nil)  
      (when (and user-id pw)
	(compute-digest-authorization-header uri http-method realm user-id pw algorithm nonce opaque)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.13")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod proxy-clear-authorization ((proxy proxy-authentication-mixin))
  (setf (proxy-authentication-method proxy) nil
	(proxy-authentication-header proxy) nil))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.13")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defgeneric proxy-clear-authorization (proxy)
  (:documentation "Clears any proxy authorization caches for PORXY."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.13")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod proxy-clear-authorization ((proxy basic-proxy-mixin))
  nil)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CLIENT.LISP.291")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun send-request (stream method url http-version request-headers &optional proxy proxy-authorization)
  ;; write the request
  (fast-format stream "~A " method)
  ;; write request url
  ;; avoid escaping problems by not parsing URLs
  (if proxy
      (write-string (name-string url) stream)	;write entire URL for proxy request (same as WRITE-NAME)
      (write-relative-name-string url stream))	;otherwise write relative URL
  ;; write version
  (fast-format stream " ~A" http-version)
  ;; end the request line
  (send-cr-line-feed stream)
  ;; prepend any proxy authentication
  (cond ((null proxy))
	(proxy-authorization
	 (%write-header :proxy-authorization proxy-authorization stream)
	 (proxy-note-authorization-header proxy proxy-authorization))
	(t (let ((header (proxy-get-authorization-header proxy method url)))
	     (when header
	       (%write-header :proxy-authorization header stream)))))
  ;; send the headers
  (%write-request-headers stream request-headers method http-version)
  ;; end the headers
  (send-cr-line-feed stream)
  (force-output stream))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.14")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(SCL:FUNDEFINE 'GET-USER-NAME+PW)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.15")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defgeneric get-user-name+pw (url realm method proxy-p prompt-p &optional stream)
  (declare (values user-name password))
  (:documentation "Returns the USER-ID and PASSWORD for URL given REALM
and authentication method, METHOD. When PROMPT-P is null and no cached
password is available for realm, an ACCESS-WITHOUT-CREDENTIALS condition
is signalled. When PROMPT-P is :REPROMPT, the user is unconditionally
queried for a password, which is recached. Otherwise, when PROMPT-P is non-null,
the user is queried only when no cached password is available. STREAM is
the stream for interacting with the user."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.15")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(SCL:FUNDEFINE 'GET-AUTHORIZATION-HEADER)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.15")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(PROGN
(define-generic get-authorization-header (url http-method authentication-method realm proxy-p &optional recompute-p prompt-p stream)
  (declare (values authorization-header found-in-cache-p))
  (:documentation "Returns a header plist suitable for returning with the authorization retry."))

(defmethod get-authorization-header ((url url:http-url) http-method method realm proxy-p &optional recompute-p prompt-p stream)
  (declare (ignore http-method realm stream proxy-p recompute-p prompt-p))
  (error "~S is an unknown authorization method for an HTTP URL." method))

)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.15")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(SCL:FUNDEFINE 'CLIENT-AUTHENTICATE-USER)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.15")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defgeneric client-authenticate-user (condition &key recompute-authorization-header-p prompt-p stream)
  (declare (values authorization-header found-in-cache-p))
  (:documentation "Returns an authorization header suitable for authenicating the user making an HTTP request.
CONDITION is an unauthorized access condition. RECOMPUTE-AUTHORIZATION-HEADER-P controls whether a cached
authorization header can be used. PROMPT-P controls whether the user is queried for a password. When
PROMPT-P :REPROMPT, the user is queried regardless of whether a cached password is available. STREAM is
the query stream."))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.17")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(PROGN
(defmethod client-authenticate-user ((condition recoverable-unauthorized-access) &key recompute-authorization-header-p
				     prompt-p (stream *query-io*))
  (let ((url (http-url condition))
	(http-method (http-method condition))
	(authentication-method (http-authentication-method condition))
	(realm (http-authentication-realm condition)))
    (get-authorization-header url http-method authentication-method realm nil recompute-authorization-header-p prompt-p stream)))

(defmethod client-authenticate-user ((condition recoverable-unauthorized-proxy-access) &key recompute-authorization-header-p
				     prompt-p (stream *query-io*))
  (let ((url (http-url condition))
	(http-method (http-method condition))
	(authentication-method (http-authentication-method condition))
	(realm (http-authentication-realm condition)))
    (get-authorization-header url http-method authentication-method realm t recompute-authorization-header-p prompt-p stream)))
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.17")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(PROGN
(defmethod get-authorization-header ((url string) http-method method realm proxy-p &optional recompute-p prompt-p (stream *query-io*))
  (get-authorization-header (url:intern-url url :if-does-not-exist :error) http-method method realm proxy-p recompute-p prompt-p stream))

(defmethod get-authorization-header ((url url:http-url) http-method (method (eql :basic)) realm proxy-p
				     &optional recompute-p prompt-p (stream *query-io*))
  (declare (ignore recompute-p))
  (multiple-value-bind (user-id pw)
      (get-user-name+pw url realm method proxy-p prompt-p stream)
    (when (and user-id pw)
      (destructuring-bind (realm-name) realm
	(cache-authorization-header-for-realm url http-method method realm-name proxy-p 
					      (compute-basic-authorization-header user-id pw))))))

(defmethod get-authorization-header ((url url:http-url) http-method (method (eql :digest)) realm proxy-p
				     &optional recompute-p prompt-p (stream *query-io*))
  (declare (ignore recompute-p))
  (multiple-value-bind (user-id pw)
      (get-user-name+pw url realm method proxy-p prompt-p stream)
    (when (and user-id pw)
      (destructuring-bind (realm-name &key algorithm nonce opaque &allow-other-keys) realm
	(cache-authorization-header-for-realm
	  url http-method method realm-name proxy-p
	  (compute-digest-authorization-header url http-method realm-name user-id pw algorithm nonce opaque))))))

(defmethod get-authorization-header :around ((url url:http-url) http-method method realm proxy-p
					     &optional recompute-p prompt-p (stream *query-io*))
  (cond (recompute-p
	 (invalidate-authorization-header-cache url http-method method realm proxy-p)
	 (call-next-method url http-method method realm proxy-p recompute-p prompt-p stream))
	;; returns the header and found-in-cache-p
	((recall-authorization-header-for-realm url http-method method realm proxy-p))
	(t (call-next-method url http-method method realm proxy-p recompute-p prompt-p stream))))
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.17")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(PROGN

(defmethod get-user-name+pw :around ((url url:http-url) (realm string) method proxy-p prompt-p &optional (stream *query-io*))
  (flet ((proxy-pw-key (url) 
	   (or (get-client-proxy url)
	       (error "Client associates no proxy with ~S." (url:name-string url)))))
    (declare (inline proxy-pw-key))
    (let ((pw-cache-key (if proxy-p (proxy-pw-key url) url)))
      (unless (eql prompt-p :reprompt)
	(multiple-value-bind (user-name password)
	    (pw-data pw-cache-key realm method)
	  (when (and user-name password)
	    (return-from get-user-name+pw (values user-name password)))))
      ;; if no cache hit, then prompt the user.
      (multiple-value-bind (user-name password)
	  (call-next-method url realm method proxy-p prompt-p stream)
	(cache-pw-data pw-cache-key realm method user-name password)
	(values user-name password)))))

(defmethod get-user-name+pw ((url url:http-url) (realm cons) (method (eql :basic)) proxy-p prompt-p &optional (stream *query-io*))
  (destructuring-bind (realm-name) realm
    (get-user-name+pw url realm-name method proxy-p prompt-p stream)))

(defmethod get-user-name+pw ((url url:http-url) (realm cons) (method (eql :digest)) proxy-p prompt-p &optional (stream *query-io*))
  (destructuring-bind (realm-name &rest plist) realm
    (declare (ignore plist))
    (get-user-name+pw url realm-name method proxy-p prompt-p stream)))
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.18")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod clear-authorization-header-cache ((url url:http-url))
  (remove-value url :proxy-authorization-header-cache-plist)
  (remove-value url :authorization-header-cache-plist))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.18")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(define clear-authentication-caches ()
  "Clears all authentication caches."
  (clrhash *basic-authorization-header-cache*)
  (clrhash *realm-username-table*)
  (map-client-proxies #'proxy-clear-authorization)
  (map-url-table #'(lambda (key url)
		     (declare (ignore key))
		     (clear-authorization-header-cache url))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.19")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defparameter *proxy-authentication-retries* 1
  "Controls the number of retries for to authenticate a proxy connection before giving up.")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.19")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(define-macro handling-proxy-authentication ((proxy-authorization-var) &body body)
  "Handles authentication by rerunning body with AUTHENTICATION-VAR bound
to a user-specified authentication."
  `(let (proxy-recompute-header-p ,proxy-authorization-var)
     (flet ((handle-unauthorized-proxy-access (cond)
	      (handler-case
		(multiple-value-setq (,proxy-authorization-var)
		  (client-authenticate-user cond :recompute-authorization-header-p proxy-recompute-header-p :prompt-p nil))
		(proxy-access-without-credentials () (return-from handle-unauthorized-proxy-access nil)))
	      (unless-every
		(,proxy-authorization-var (return-from handle-unauthorized-proxy-access nil))
		(proxy-recompute-header-p (setq proxy-recompute-header-p t)))
	      (throw 'retry-with-proxy-authentication t)))
       (declare (dynamic-extent #'handle-unauthorized-proxy-access))
       (loop with retry-limit fixnum = *proxy-authentication-retries*
	     for proxy-tries fixnum upfrom 0
	     doing (catch 'retry-with-proxy-authentication
		     (handler-bind-if (< proxy-tries retry-limit)
			((recoverable-unauthorized-proxy-access #'handle-unauthorized-proxy-access))
		       (return (progn . ,body))))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CLIENT.LISP.291")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod invoke-http-service ((client basic-client-mixin) method header-writer response-handler
				&optional request-entity-generator (http-version *client-http-version*))
  (flet ((trace-request (url method version header-writer proxy-authorization &aux (trace-stream *trace-output*))
	   (let ((proxy (client-proxy client)))
	     (if proxy
		 (fast-format trace-stream "~&Proxy: ~A (~D)" (proxy-host proxy) (proxy-port proxy))
		 (fast-format trace-stream "~&Host: ~A" (host-string url)))
	     (fast-format trace-stream "~%Request: ~A ~I ~A~%Request Headers: ~I" 
			  method (url:write-name url trace-stream) version
			  (progn
			    (when proxy-authorization
			      (%write-header :proxy-authorization proxy-authorization trace-stream))
			    (%write-request-headers trace-stream header-writer method version)))))
	 (signal-unauthorized-proxy-access (url method http-version headers stream)
	   (destructuring-bind (&optional authentication-method . realm)
	       (get-header :proxy-authenticate headers)
	     (flush-input-entity stream headers http-version)
	     (error 'recoverable-unauthorized-proxy-access :url url :method method 
		    :authentication-method authentication-method :authentication-realm realm))))
    (with-standard-client-io-syntax ()
      (handling-proxy-authentication (proxy-authorization)
	(with-current-connection (client :http-version http-version :connection-var connection :block-name invoke-http-service)
	  (multiple-value-bind (connection-version connection-version-confirmed-p http-plist)
	      (connection-confirmed-version connection)
	    (let ((url (client-url client))
		  (proxy (client-proxy client))
		  (request-version (if connection-version-confirmed-p (http-version-min connection-version http-version) connection-version))
		  (stream (connection-stream connection)))
	      (setf (client-method client) method
		    (%client-request-version client) request-version)
	      ;; send a request to the remote host
	      (send-request stream method url request-version header-writer proxy proxy-authorization)
	      (when *trace-client* (trace-request url method request-version header-writer proxy-authorization))
	      (when request-entity-generator	;Send the request body when provided
		(case request-version
		  ((:http/1.0 :http/0.9)	;1.0 remote server, just send the data.
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
				      (407	;proxy authentication
					(signal-unauthorized-proxy-access url (client-method client) server-version *headers* (client-stream client)))
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
			   (407 (signal-unauthorized-proxy-access url (client-method client) server-version *headers* (client-stream client)))
			   (417 (signal 'expectation-failed :url url :version server-version))
			   (t (return-from invoke-http-service
				(multiple-value-prog1	;must return the values returned by the handler
				  (funcall response-handler client stream request-version))))))))))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.21")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;; the default method just signals the conditions because prompting the user
;; requires the base client 11/1/2001 -- JCMa.
(defmethod get-user-name+pw (url realm method proxy-p reprompt-p &optional (stream *terminal-io*))
  (declare (ignore reprompt-p stream))
  (let ((client *client*))
    (cond (client
	   (error (if proxy-p 'recoverable-unauthorized-proxy-access 'recoverable-unauthorized-client-access)
		  :url url
		  :method (client-method client)
		  :authentication-method method
		  :authentication-realm realm
		  :headers *headers*
		  :reason "No user to prompt for password."
		  :version (client-request-version client)))
	  (t (error 'recoverable-unauthorized-client-access
		    :url url :authentication-method method :authentication-realm realm :headers *headers*)))))

