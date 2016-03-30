;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.104
;;; Reason: Function HTTP:WRITE-DIRECTORY-LISTING:  decache anchor text when versions change.
;;; Written by JCMa, 12/16/00 06:07:26
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.103,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42,
;;; Experimental Genera 8 5 Patches 1.0, Genera 8 5 System Patches 1.21,
;;; Genera 8 5 Mailer Patches 1.1, Genera 8 5 Joshua Patches 1.0,
;;; Genera 8 5 Statice Runtime Patches 1.0, Genera 8 5 Statice Patches 1.0,
;;; Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.0,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, HTTP Proxy Server 6.11,
;;; HTTP Client Substrate 4.4, Statice Server 466.2, HTTP Client 50.1,
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.7),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;SERVER.LISP.881")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.881")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun write-directory-listing (url stream inclusion-predicate path-url-intern-function &optional directories-p)
  (flet ((anchor-text (url pathname directory-file-p)
	   (flet ((get-anchor-string (url pathname version recompute-p)
		    (with-value-cached (url :directory-string :recompute-p recompute-p)
		      (let ((name (url:object url))
			    (type (pathname-type pathname)))
			(declare (dynamic-extent name))
			(list* version
			       (typecase version
				 ((or keyword null) (concatenate 'string name "." type))
				 (t (concatenate 'string name "." type "." (write-to-string version :base 10. :escape nil)))))))))
	     (cond (directory-file-p
		    (when directories-p
		      (url:path-most-specific-name url)))
		   (t (let ((version (pathname-version pathname)))
			(multiple-value-bind (spec cached-p)
			    (get-anchor-string url pathname version nil)
			  ;; decache entry when the version changes
			  (if (and cached-p (or (stringp spec)	;runtime patch version
						(not (eql (car spec) version))))
			      (cdr (get-anchor-string url pathname version t))
			      (cdr spec)))))))))
    (declare (dynamic-extent #'anchor-text))
    (multiple-value-bind (user-agent version)
	(current-user-agent)
      (let ((tables-aware-p (user-agent-capability-p :tables user-agent version)))
	(with-directory-index-caching (url stream (if tables-aware-p :directory-list-html3 :directory-list-html2)
					   :last-modification-function 'file-modification-date) 
	  (funcall (if tables-aware-p #'%write-directory-listing-html3 #'%write-directory-listing-html2)
		   url stream inclusion-predicate path-url-intern-function #'anchor-text #'url:path-directory-string 
		   directories-p :definition t))))))

