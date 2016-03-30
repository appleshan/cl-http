;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for HTTP-CLIENT-SUBSTRATE version 4.23
;;; Reason: Fix bug copying files to a url using the digest authentication scheme.
;;; Clean up the calling structure in neighborhood.
;;; 
;;; Function (CLOS:METHOD HTTP::PW-DATA (URL:HTTP-URL STRING (EQL :DIGEST))):  provide missing method.
;;; Remove function (CLOS:METHOD HTTP::PW-DATA (URL:HTTP-URL CONS (EQL :DIGEST))): -
;;; Remove function (CLOS:METHOD HTTP::PW-DATA (T CONS (EQL :BASIC))): -
;;; Remove function (CLOS:METHOD HTTP::PW-DATA (HTTP::HTTP-PROXY CONS (EQL :DIGEST))): -
;;; Function (CLOS:METHOD HTTP::PW-DATA (T CONS T)):  one method handles realm as
;;; a cons, calling the specialized methods after splicing off list structure.
;;; Function (CLOS:METHOD HTTP::CACHE-PW-DATA (T CONS (EQL :DIGEST) T T)):  -
;;; Function (CLOS:METHOD HTTP::GET-AUTHORIZATION-HEADER (URL:HTTP-URL T (EQL :DIGEST) T T)):  pass down realm plist to cache-authorization-header-for-realm
;;; Remove function (CLOS:METHOD HTTP::GET-USER-NAME+PW (URL:HTTP-URL STRING T T T) :AROUND): -
;;; Function (CLOS:METHOD HTTP::GET-USER-NAME+PW (URL:HTTP-URL CONS T T T) :AROUND):  -
;;; Function (CLOS:METHOD HTTP::CACHE-PW-DATA (T STRING (EQL :DIGEST) T T)):  -
;;; Function (CLOS:METHOD HTTP::CACHE-PW-DATA (T CONS (EQL :DIGEST) T T)):  -
;;; Written by JCMa, 10/01/03 14:53:15
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5-inc1.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.170,
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



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:CLIENT;AUTHENTICATION.LISP.24"
  "HTTP:CLIENT;AUTHENTICATION.LISP.26"
  "HTTP:CLIENT;AUTHENTICATION.LISP.25"
  "HTTP:CLIENT;AUTHENTICATION.LISP.27")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.24")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod pw-data ((url url:http-url) (realm string) (method (eql :digest)))
  (let ((table *realm-username-table*)
	entry)
    (when (setq entry (or (gethash (url:name-string url) table)
			  (gethash (url:directory-context-string url) table)
			  (gethash (url:root-context-string url) table)
			  (with-pw-realm-key (url realm)
			    (gethash realm-key table))))
      (decode-realm-username realm entry))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.24")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(SCL:FUNDEFINE '(METHOD PW-DATA (HTTP-URL CONS (EQL :DIGEST))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.24")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(SCL:FUNDEFINE '(METHOD PW-DATA (T CONS (EQL :BASIC))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.24")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(SCL:FUNDEFINE '(METHOD PW-DATA (HTTP-PROXY CONS (EQL :DIGEST))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.24")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod pw-data (url-or-proxy (realm cons) method)
  (destructuring-bind (realm-name &key &allow-other-keys) realm
    (pw-data url-or-proxy realm-name method)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.25")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod get-authorization-header ((url url:http-url) http-method (method (eql :digest)) realm proxy-p
				     &optional recompute-p prompt-p (stream *query-io*))
  (declare (ignore recompute-p))
  (multiple-value-bind (user-id pw)
      (get-user-name+pw url realm method proxy-p prompt-p stream)
    (when (and user-id pw)
      (destructuring-bind (realm-name &key algorithm nonce opaque &allow-other-keys) realm
	(cache-authorization-header-for-realm
	  url http-method method realm proxy-p	;pass down the realm plist because other method may want domain 10/1/2003 -- JCMa.
	  (compute-digest-authorization-header url http-method realm-name user-id pw algorithm nonce opaque))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.26")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod cache-pw-data (url (realm string) (method (eql :digest)) user-name password)
  (let ((entry (encode-realm-username realm user-name password))
	(table *realm-username-table*))
    (with-pw-realm-key (url realm :stack-cons-p nil)
      (setf (gethash (url:name-string url) table) entry
	    (gethash (url:directory-context-string url) table) entry
	    (gethash (url:root-context-string url) table) entry
	    (gethash realm-key table) entry))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.26")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod cache-pw-data (url (realm cons) (method (eql :digest)) user-name password)
  (destructuring-bind (realm-name &key domain &allow-other-keys) realm
    (when domain
      (let ((entry (encode-realm-username realm-name user-name password))
	    (table *realm-username-table*))
	(dolist (uri domain)
	  (setf (gethash uri table) entry))))
    (cache-pw-data url realm-name method user-name password)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.25")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(SCL:FUNDEFINE '(METHOD GET-USER-NAME+PW (HTTP-URL STRING T T T) :AROUND))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;AUTHENTICATION.LISP.27")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;; this can receive a realm that is either a cons or a string
(defmethod get-user-name+pw :around ((url url:http-url) realm method proxy-p prompt-p &optional (stream *query-io*))
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

