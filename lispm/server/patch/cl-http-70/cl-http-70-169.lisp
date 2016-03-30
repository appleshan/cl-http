;;; -*- Mode: lisp; Syntax: common-lisp; Package: user; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.169
;;; Reason: Function (CLOS:METHOD HTTP:COPY-FILE (T T)):  make portable version copy dates and authors.
;;; Function HTTP::VERSIONED-CRLF-PATHNAME:  new
;;; Function (CLOS:METHOD HTTP::PUT-DOCUMENT (URL:HTTP-OBJECT T)):  make this behave for versioned files.
;;; Written by JCMa, 9/15/03 12:43:38
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5-inc1.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.168,
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
;;; Genera 8 5 Lock Simple Patches 1.0, Jcma 44, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.18, DEC OSF/1 V4.0 (Rev. 110),
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

;;; Patch file for CL-HTTP version 70.169
;;; Written by JCMa, 9/15/03 13:15:43
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5-inc1.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.168,
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
;;; Genera 8 5 Lock Simple Patches 1.0, Jcma 44, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.18, DEC OSF/1 V4.0 (Rev. 110),
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
  "HTTP:SERVER;UTILS.LISP.521"
  "HTTP:LISPM;SERVER;LISPM.LISP.513"
  "HTTP:SERVER;SERVER.LISP.927"
  "HTTP:LISPM;SERVER;LISPM.LISP.514")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.521")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

;; Ports may optionally specialize this method to pathnames to use platform specific file copying and/or property setting.
(defmethod copy-file (from-pathname to-pathname &key (copy-mode :text) (copy-creation-date t) &allow-other-keys)
  (let ((element-type (copy-mode-element-type copy-mode))
        modification-date #-UNIX creation-date #+CL-HTTP-FILE-AUTHOR author)
    (with-open-file (from from-pathname :direction :input :element-type element-type :if-does-not-exist :error)
      (with-open-file (to to-pathname :direction :output :element-type element-type :if-exists :supersede
			  :if-does-not-exist :create)
	(stream-copy-until-eof from to copy-mode)
	(when copy-creation-date
          (setq modification-date (file-stream-modification-date from))
          #-UNIX ;; creation date is not available under unix
          (setq creation-date (file-stream-creation-date from))
          #+CL-HTTP-FILE-AUTHOR
          (setq author (file-author from-pathname))))) ;can we get the file author from the stream?
    (when copy-creation-date
      (cond-every
       (modification-date (setf (file-modification-date to-pathname) modification-date)) 
       #-UNIX
       (creation-date (setf (file-creation-date to-pathname) creation-date))
       #+CL-HTTP-FILE-AUTHOR
       (author (set-file-author to-pathname author nil))))
    to-pathname))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.927")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod put-document ((url url:http-object) stream &optional newly-created-url-p (check-versions-p t))
  (declare (values url exported))
  (flet ((download-crlf-file (stream pathname bytes copy-mode)
	   ;; Version number is not thread safe -- catch condition and retry -- 9/15/2003 -- JCMa.
	   (let ((crlf-pathname #+Genera (versioned-crlf-pathname pathname) 
				#-Genera (crlf-pathname pathname)))
	     (stream-copy-bytes stream crlf-pathname bytes copy-mode)
	     (decode-crlf-file crlf-pathname)
	     (delete-file-carefully crlf-pathname)))
	 (update-file-properties (pathname server modification-date)
	   (when modification-date
	     (set-file-modification-date pathname modification-date nil))
	   (set-file-author pathname server nil)
	   (autotype-file pathname nil)))
    (let ((pathname (url:translated-pathname url))
          (server *server*)
          (headers *headers*))
      ;; throw out of http transaction if a conflict is detected
      (when check-versions-p
        (let ((version (document-version pathname)))
          (case (server-http-version server)
            ((:http/0.9 :http/1.0)
             (check-derived-from-version url version))
            (t (check-if-match-precondition version t :put headers) 
               (check-if-unmodified-since-precondition version :put headers)))))
      (with-header-values (content-length content-range content-type last-modified transfer-encoding) headers
	;; check for byte ranges in 1.1
	(when content-range
	  (error 'server-not-implemented :url url :method :put
		 :format-string "Putting byte ranges is not implemented."))
	(let ((copy-mode (if content-type (mime-content-type-copy-mode content-type) (url:copy-mode url)))
	      (modification-date (or last-modified (server-request-time server))))
	  (cond (transfer-encoding 
		 (case transfer-encoding
		   #+HTTP-Chunking
		   (:chunked
		     (with-successful-put-response (server stream)
		       (with-chunked-transfer-decoding (stream :headers headers)
			 (case copy-mode
			   (:crlf (stream-decode-crlf-until-eof stream pathname))
			   (t (stream-copy-until-eof stream pathname copy-mode))))
		       (update-file-properties pathname server modification-date)
		       (values url (if newly-created-url-p :created :modified) modification-date)))
		   (t (error 'server-not-implemented :close-connection t :url url :method :put
			     :format-string "The HTTP transfer encoding, ~A, is not implemented."
			     :format-args (list transfer-encoding)))))
		(content-length
		 (handler-case-if (not *debug-server*) 
		    (with-successful-put-response (server stream)
		      (case copy-mode
			(:crlf (download-crlf-file stream pathname content-length copy-mode))
			(t (stream-copy-bytes stream pathname content-length copy-mode)))
		      (update-file-properties pathname server modification-date)
		      (values url (if newly-created-url-p :created :modified) modification-date))
		   (error (err)
			  (error 'error-handling-put-method :url url :method :put :server-error err :headers (header-plist)
				 :format-string "Error executing PUT method for ~A."
				 :format-args (list (url:name-string url))))))
		(t (error 'content-length-required :url url :method :put
			  :format-string "no content-length header provided for ~A."
			  :format-args (list (url:name-string url))))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.514")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

;; this should handle the Genera versioning schem for unix file systems -- 9/15/2003 -- JCMa.
(defun http::versioned-crlf-pathname (pathname)
  "Returns the correctly versioned CRLF pathname,
which is required for a versioned ed file system such as LMFS."
  (http::crlf-pathname
    (let ((path (http::translated-pathname pathname)))
      (typecase path
	(fs:lmfs-pathname 
	  (let* ((probed-file (probe-file (make-pathname :version :newest :defaults path)))
		 (version (and probed-file (pathname-version probed-file))))
	    (if (and version (integerp version))
		(setf (pathname-version path) (1+ version))
		path)))
	(t path)))))

