;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for HTTP-CLIENT-SUBSTRATE version 4.19
;;; Reason: Function WWW-UTILS:FTP-COPY-FILE:  update.
;;; Function WWW-UTILS:FTP-COPY-FILE-TO-HTTP-STREAM:  update.
;;; Function WWW-UTILS:FTP-DIRECTORY-INFO:  update.
;;; Function WWW-UTILS:OPEN-HTTP-STREAM-TO-HOST:  update.
;;; Written by JCMa, 9/18/01 17:30:10
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.6,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.146,
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
;;; HTTP Client Substrate 4.18, Statice Server 466.2, HTTP Client 51.0,
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



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:LISPM;CLIENT;LISPM.LISP.41")


(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL)
  (SCT:REQUIRE-PATCH-LEVEL-FOR-PATCH '(CL-HTTP 70. 147.)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;CLIENT;LISPM.LISP.41")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: ansi-common-lisp; Base: 10; Package: www-utils; Mode: LISP -*-")

(define ftp-copy-file (from-pathname to-stream &key (element-type 'character) (port 21)
                                     (user-id "anonymous") (user-pw (server-mail-address)))
  "Copies the content of FROM-PATHNAME to TO-STREAM. 
If a network error is encountered, this returns NIL, otherwise T.
ELEMENT-TYPE is the ANSI file openning argument."
  (declare (values success-p))
  (let ((host (pathname-host from-pathname)))
    (ensure-services host '(:file :tcp :tcp-ftp))
    (with-tcp-port-for-protocol (:ftp port)
      (let ((tcp:*tcp-connect-timeout* http::http::*client-timeout*))
	(with-automatic-login (host user-id user-pw)
	  (with-open-file (ftp-stream from-pathname :direction :input :element-type element-type)
	    (http::stream-copy-until-eof ftp-stream to-stream (case element-type
								(character :text)
								(t :binary)))
	    (values t)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;CLIENT;LISPM.LISP.41")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: ansi-common-lisp; Base: 10; Package: www-utils; Mode: LISP -*-")

(define ftp-copy-file-to-http-stream (from-pathname http-stream &key (port 21) data-type
						    url additional-headers
						    (user-id "anonymous") (user-pw (server-mail-address)))
  "Copies the content of FROM-PATHNAME to HTTP-STREAM."
  (declare (values success-p))
  (flet ((handle-invalid-ftp-user-id-and-password (&rest ignore)
	   (declare (ignore ignore))
	   (signal 'http::client-unauthorized-ftp-access :url url :method :get
		   :authentication-realm "FTP Server" :authentication-method :basic)))
    (declare (dynamic-extent #'handle-invalid-ftp-user-id-and-password))
    (let* ((host (pathname-host from-pathname))
	   (copy-mode (or (url::%content-type-copy-mode data-type nil) :binary))
	   (element-type (ecase copy-mode
			   (:text 'character)
			   ((:binary :crlf) '(unsigned-byte 8))))
	   (*standard-get-user-id-and-password* #'handle-invalid-ftp-user-id-and-password))
      (ensure-services host '(:file :tcp :tcp-ftp))
      (with-tcp-port-for-protocol (:ftp port)
	(let ((tcp:*tcp-connect-timeout* http::http::*client-timeout*))
	  (with-automatic-login (host user-id user-pw)
	    (with-open-file (ftp-stream from-pathname :direction :input :element-type element-type)
	      (http:with-successful-response (http-stream (case data-type ((:unknown nil) :text) (t data-type))
							  :location url :additional-headers additional-headers)
		(case copy-mode
		  (:text
		    (with-text-stream (http-stream :output)
		      (http::stream-copy-until-eof ftp-stream http-stream :text)))
		  ((:binary :crlf)
		   (with-binary-stream (http-stream :output)
		     (http::stream-copy-until-eof ftp-stream http-stream :binary))))
		(values t)))))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;CLIENT;LISPM.LISP.41")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: ansi-common-lisp; Base: 10; Package: www-utils; Mode: LISP -*-")

(define ftp-directory-info (directory &optional (port 21) url (user-id "anonymous") (user-pw (server-mail-address)))
  "Returns a list of pathname spec for directory just like DIRECTORY-INFO.
If a network error is encountered, this returns NIL."
  (declare (values directory-listing directory-exists-p))
  (flet ((handle-invalid-ftp-user-id-and-password (&rest ignore)
	   (declare (ignore ignore))
	   (signal 'http::client-unauthorized-ftp-access :url url :method :get
		   :authentication-realm "FTP Server" :authentication-method :basic)))
    (declare (dynamic-extent #'handle-invalid-ftp-user-id-and-password))
    (let* ((path (pathname directory))
	   (host (pathname-host path))
	   (*standard-get-user-id-and-password* #'handle-invalid-ftp-user-id-and-password))
      (ensure-services host '(:file :tcp :tcp-ftp))
      (with-tcp-port-for-protocol (:ftp port)
	(let ((tcp:*tcp-connect-timeout* http::http::*client-timeout*))
	  (with-automatic-login (host user-id user-pw)
	    ;; ansi CL directory fails due to :fast option  3/13/94 -- JCMa.
	    (let ((path (make-pathname :defaults directory))
		  (listing (cdr (fs:directory-list (make-pathname :defaults directory) :sorted))))
	      (if listing
		  (values listing t)
		  (values nil (ignore-errors (open path :direction :probe-directory :if-does-not-exist nil)))))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;CLIENT;LISPM.LISP.41")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: ansi-common-lisp; Base: 10; Package: www-utils; Mode: LISP -*-")

(defun open-http-stream-to-host (host port)
  (declare (values stream))
  (let ((host-object (ensure-services host '(:http :tcp :http))))
    (with-tcp-port-for-protocol (:http port)
      (let ((tcp:*tcp-connect-timeout* http::*client-timeout*))
	(neti::invoke-service-on-host :http host-object)))))

