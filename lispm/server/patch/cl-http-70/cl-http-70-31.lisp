;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.31
;;; Reason: Function HTTP::DELETE-FILE-CAREFULLY:  new.
;;; Function (CLOS:METHOD HTTP::DELETE-FILE-CAREFULLY (T)):  implement.
;;; Function (CLOS:METHOD HTTP:STREAM-DECODE-CRLF-UNTIL-EOF (T CL:PATHNAME)):  define.
;;; Function (CLOS:METHOD HTTP:STREAM-ENCODE-CRLF-UNTIL-EOF (CL:PATHNAME T)):  -
;;; Function (CLOS:METHOD WWW-UTILS::SET-FILE-MODIFICATION-DATE (CL:PATHNAME LISP:BIGNUM)):  new.
;;; Function HTTP::SET-FILE-MODIFICATION-DATE:  new.
;;; Function (CLOS:METHOD HTTP::PUT-DOCUMENT (URL:HTTP-OBJECT T)):  revamp.
;;; Function HTTP::AUTOTYPE-FILE:  new facility for automatically setting file types
;;; and creators on certain operating systems.
;;; Function (CLOS:METHOD HTTP::AUTOTYPE-FILE (T)):  default implementation.
;;; Function HTTP::USER-NAME-VIA-FROM-HEADER:  new.
;;; Function (CLOS:METHOD HTTP::USER-NAME-VIA-FROM-HEADER (HTTP::BASIC-SERVER-MIXIN)):  -
;;; Function (CLOS:METHOD WWW-UTILS:SET-FILE-AUTHOR (CL:PATHNAME HTTP::SERVER-AUTHENTICATION-MIXIN)):  -
;;; Written by JCMa, 3/20/00 15:28:46
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.30,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, HTTP Proxy Server 4.5,
;;; HTTP Client Substrate 3.4, Jcma 41, HTTP Client 49.5, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.11, DEC OSF/1 V4.0 (Rev. 110),
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
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;UTILS.LISP.432"
  "HTTP:SERVER;UTILS.LISP.433"
  "HTTP:LISPM;SERVER;LISPM.LISP.451"
  "HTTP:SERVER;UTILS.LISP.434"
  "HTTP:SERVER;UTILS.LISP.435"
  "HTTP:SERVER;SERVER.LISP.832"
  "HTTP:SERVER;SERVER.LISP.833")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.451")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(eval-when (:execute :compile-toplevel :load-toplevel)

  (export (intern "SET-FILE-MODIFICATION-DATE" :www-utils) :www-utils)
  (export (intern "AUTOTYPE-FILE" :www-utils) :www-utils))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.432")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(define-generic delete-file-carefully (pathname &key &allow-other-keys)
  (declare (values file-deleted-p))
  (:documentation "Deletes FILE after probing to ensure its presence."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.432")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmethod delete-file-carefully (pathname &key &allow-other-keys)
  (let ((probe-file (probe-file pathname)))
    (when probe-file
      (delete-file probe-file))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.433")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmethod stream-decode-crlf-until-eof (from-stream (to-pathname pathname))
  (with-open-file (file to-pathname :direction :output :element-type *standard-character-type*
			:if-does-not-exist :create :if-exists :supersede)
    (stream-decode-crlf-until-eof from-stream file)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.433")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmethod stream-encode-crlf-until-eof ((from-pathname pathname) to-stream)
  (with-open-file (file from-pathname :direction :output :element-type *standard-character-type*
			:if-does-not-exist :error)
    (stream-encode-crlf-until-eof file to-stream)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.451")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defmethod set-file-modification-date ((pathname pathname) (universal-time bignum) &optional error-p)
  (fs:change-file-properties pathname error-p :creation-date universal-time :modification-date universal-time))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.434")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

;; Ports should specialize this.
(defgeneric set-file-modification-date (pathname universal-time &optional error-p)
  (:documentation "Sets the modification date of PATHNAME to UNIVERSAL-TIME."))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.435")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defgeneric autotype-file (pathname &optional report-stream)
  (:documentation "Automatically sets the file type and creator of PATHNAME.
Only relevant for certain operating systems. Reports activity on report-stream,
when it is non-null."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.435")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmethod autotype-file (pathname &optional report-stream)
  (declare (ignore report-stream))
  pathname)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.832")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(define-generic user-name-via-from-header (server)
  (declare (values user-name))
  (:documentation "Returns the user name as found in the user's email address in the from header."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.832")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod user-name-via-from-header ((server basic-server-mixin))
  (with-header-values (from) (server-headers server)
    (when from
      (url::user-id (car from)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.832")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod set-file-author ((pathname pathname) (server server-authentication-mixin) &optional (error-p t))
  (let ((name (or (user-name server)
		  (user-name-via-from-header server))))
    (when name
      (set-file-author (probe-file pathname) name error-p))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.833")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod put-document ((url url:http-object) stream &optional newly-created-url-p (check-versions-p t))
  (declare (values url exported))
  (flet ((download-crlf-file (stream pathname bytes copy-mode)
	   (let ((crlf-pathname (crlf-pathname pathname)))
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
      (with-header-values ( content-length content-range content-type last-modified transfer-encoding) headers
	;; check for byte ranges in 1.1
	(when content-range
	  (error 'server-not-implemented :url url :method :put
		 :format-string "Putting byte ranges is not implemented."))
	(let ((copy-mode (if content-type (mime-content-type-copy-mode content-type) (url:copy-mode url)))
	      (modification-date (or last-modified (server-request-time server))))
	  (cond (transfer-encoding 
		 (case transfer-encoding
		   #+(or Genera MCL LispWorks)
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

