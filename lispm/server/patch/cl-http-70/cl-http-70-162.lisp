;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.162
;;; Reason: Function HTTP:COPY-FILE:  -
;;; Function (CLOS:METHOD WWW-UTILS::SET-FILE-CREATION-DATE (CL:PATHNAME LISP:BIGNUM)):  -
;;; add setf method for file-creation-date
;;; ditto for file-modification-date
;;; Function (CLOS:METHOD HTTP:COPY-FILE (T T)):  set file properties.
;;; Function HTTP::STANDARD-HTTP-PORT:  -
;;; Function HTTP::%DEFINE-HOST-NAME-MAPPING:  -
;;; Function HTTP::PRINT-DATE:  new
;;; Function HTTP::SERVER-EMAIL-ADDRESS:  -
;;; Function (CLOS:METHOD HTTP::SERVER-EMAIL-ADDRESS (HTTP::SERVER)):  -
;;; Function HTTP::SERVER-SEND-MAIL:  -
;;; Function (CLOS:METHOD HTTP::SERVER-SEND-MAIL (STRING STRING T T)):  -
;;; Function (CLOS:METHOD HTTP::SERVER-SEND-MAIL (HTTP::SERVER STRING T T)):  -
;;; Function (CLOS:METHOD HTTP::SERVER-SEND-MAIL (T HTTP::USER T T)):  -
;;; Function (CLOS:METHOD HTML2:ACCEPT-INPUT (HTML2:STRING+ T)):  -
;;; DEFINE-CONDITION URL::SEARCH-URL-FORM-ENCODING:  -
;;; DEFINE-CONDITION URL::SEARCH-URL-FORM-VALUE-NOT-FOUND:  -
;;; DEFINE-CONDITION URL:SEARCH-URL-BAD-FORM-ENCODING:  -
;;; Function URL::SEARCH-KEYS-GET-VALUE:  -
;;; Function (CLOS:METHOD URL::SEARCH-KEYS-GET-VALUE (URL::SEARCH-MIXIN T)):  -
;;; Function URL::%RELATIVE-PATH-MATCH-P:  -
;;; Function URL::RELATIVE-PATH-MATCH-P:  -
;;; Function (CLOS:METHOD URL::RELATIVE-PATH-MATCH-P (URL:URL URL:URL)):  -
;;; Function (CLOS:METHOD URL::RELATIVE-PATH-MATCH-P (URL:URL STRING)):  -
;;; Function (CLOS:METHOD URL::RELATIVE-PATH-MATCH-P (STRING URL:URL)):  -
;;; Function HTTP::WITH-STRING-TRIM-BOUNDS:  -
;;; Function HTTP::SET-FILE-CREATION-DATE:  -
;;; Function (CLOS:METHOD HTTP::SET-FILE-CREATION-DATE (CL:PATHNAME LISP:BIGNUM)):  -
;;; Written by JCMa, 8/25/03 18:27:08
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
;;; Genera 8 5 Lock Simple Patches 1.0, Jcma 44, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.18, DEC OSF/1 V4.0 (Rev. 110),
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing).

;;; Patch file for CL-HTTP version 70.162
;;; Written by JCMa, 8/26/03 19:09:17
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5-inc1.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.6,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.162,
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
;;; Jcma 44, W4 Constraint-Guide Web Walker 45.12, HTTP Client Substrate 4.22,
;;; HTTP Client 51.3, Image Substrate 440.4, Essential Image Substrate 433.0,
;;; HTTP Proxy Server 6.32, Ivory Revision 5, VLM Debugger 329, Genera program 8.18,
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
  "HTTP:LISPM;SERVER;LISPM.LISP.511"
  "HTTP:SERVER;UTILS.LISP.511"
  "HTTP:SERVER;UTILS.LISP.508"
  "HTTP:SERVER;UTILS.LISP.509"
  "HTTP:SERVER;SERVER.LISP.922"
  "HTTP:SERVER;SERVER.LISP.923"
  "HTTP:SERVER;HTML2.LISP.302"
  "HTTP:SERVER;URL.LISP.430"
  "HTTP:SERVER;URL.LISP.431"
  "HTTP:SERVER;UTILS.LISP.510")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.511")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defmethod http::set-file-modification-date ((pathname pathname) (universal-time bignum) &optional error-p)
  (fs:change-file-properties pathname error-p :creation-date universal-time :modification-date universal-time))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.511")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defsetf file-modification-date set-file-modification-date)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.511")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

;; Ports should specialize this.
(defgeneric set-file-creation-date (pathname universal-time &optional error-p)
  (:documentation "Sets the creation date of PATHNAME to UNIVERSAL-TIME."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.511")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defmethod http::set-file-creation-date ((pathname pathname) (universal-time bignum) &optional error-p)
  (fs:change-file-properties pathname error-p :creation-date universal-time :modification-date universal-time))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.511")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defsetf file-creation-date set-file-creation-date)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.508")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

;;;------------------------------------------------------------------- 
;;;
;;; CONDITIONAL FILE COPY
;;;

(define-generic copy-file (from-pathname to-pathname &key copy-mode report-stream &allow-other-keys)
  (declare (values to-pathname))
  (:documentation "A portable file copy.
COPY-MODE is one of :TEXT, CRLF, or binary.
When REPORT-STREAM is non-null, COPY-FILE reports its activities."))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.508")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

;; Make this set the file properties 3/20/2000 -- JCMa.
(defmethod copy-file (from-pathname to-pathname &key (copy-mode :text) (copy-creation-date t) &allow-other-keys)
  (let ((element-type (copy-mode-element-type copy-mode)))
    (with-open-file (from from-pathname :direction :input :element-type element-type :if-does-not-exist :error)
      (with-open-file (to to-pathname :direction :output :element-type element-type :if-exists :supersede
			  :if-does-not-exist :create)
	(stream-copy-until-eof from to copy-mode)
	(when copy-creation-date
	  (let ((creation-date (file-stream-creation-date from))
		(modification-date (file-stream-modification-date from)))
	    (cond-every 
	      (creation-date (setf (file-creation-date to-pathname) creation-date))
	      (modification-date (setf (file-modification-date to-pathname) modification-date)))))
	to-pathname))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.508")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(define standard-http-port ()
  "Returns the standard port for HTTP connections."
  *standard-http-port*)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.508")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defun %define-host-name-mapping (domain-name port validate-host-p entry-contructor) 
  (declare (dynamic-extent entry-contructor))
  (flet ((make-entry (contructor host port)
	   (funcall contructor (string-downcase host) port)))
    (check-type domain-name string)
    (check-type port integer)
    (when validate-host-p
      (parse-host domain-name))
    (let ((entry (gethash domain-name *virtual-host-table*)))
      (cond (entry
	     (let ((spec (assoc port entry :test #'=)))
	       (if spec
		   (setf (cdr spec) (cdr (make-entry entry-contructor domain-name port)))
		   (setf (gethash domain-name *virtual-host-table*)
			 (list* (make-entry entry-contructor domain-name port) entry)))))
	    (t (setf (gethash domain-name *virtual-host-table*)
		     (list (make-entry entry-contructor domain-name port)))))
      domain-name)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.509")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(define print-date (&key (universal-time (get-universal-time)) (format :iso) (stream *standard-output*))
  "Prints the UNIVERSAL-TIME on STREAM using FORMAT.

  FORMATS: :ISO
  :US-ENGLISH
  :US-ENGLISH+WEEKDAY"
   (multiple-value-bind (seconds minutes hours day month year day-of-the-week)
                                   (decode-universal-time universal-time)
       (declare (ignore seconds minutes hours))
       (ecase format
          (:iso (format stream "~4D-~2,'0D-~2,'0D" year month day))
          (:us-english
	    (let ((month (month-string month)))
	       (format stream "~A ~D, ~D" month day year)))
          (:us-english+weekday
	    (let ((day-of-the-week (day-of-the-week-string day-of-the-week))
	            (month (month-string month)))
	       (format stream "~A, ~A ~D, ~D" day-of-the-week month day year))))))

(eval-when (load) (export (intern "PRINT-DATE" :http) :http))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.922")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(define-generic server-email-address (server)
  (:documentation "Returns the email address associated with SERVER."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.922")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod server-email-address ((server server))
   *server-mail-address*)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.923")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(define-generic server-send-mail (server user-or-server subject message-writer &key  keywords comments file-references)
  (:documentation "Sends an email message to USER-OR-SERVER with SUBJECT from SERVER.
MESSAGE-WRITER is either a string or a function accepting a single stream argument."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.923")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod server-send-mail ((server string) (to string) subject message-writer &key keywords comments file-references)
  (send-mail-from server to subject message-writer :keywords keywords :comments comments :file-references file-references))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.923")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod server-send-mail ((server server)  (to string) subject message-writer &key keywords comments file-references)
  (send-mail-from (server-email-address server) to  subject message-writer :keywords keywords :comments comments :file-references file-references))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.923")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod server-send-mail (server (user user) subject message-writer &key keywords comments file-references)
  (send-mail-from server (user-email-address user) subject message-writer :keywords keywords :comments comments :file-references file-references))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.302")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

(defmethod accept-input ((string+ string+) query-name &rest args &key preamble (stream *output-stream*) &allow-other-keys)
  (with-slots (default-size default-max-size) string+
    (%accept-input-write-preamble preamble string+ stream args)
    (%issue-command ("INPUT" stream)
      (write-standard-input-type-args (stream query-name string+ args :bind-default t)
	(destructuring-bind (&key size max-length &allow-other-keys) args
	  (let ((local-size (or size default-size))
		(max (or max-length default-max-size)))
	    (cond-every
	      (local-size
		(%write-command-key-arg stream "SIZE" local-size))
	      (max
		(unless (<= max 1024.)
		  (error "String fields cannot exceed 1024 characters in HMTL 2.0"))
		(%write-command-key-arg stream "MAXLENGTH" max t)))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.430")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define-condition search-url-form-encoding
		  (url-error)
  ()
  (:report (lambda (condition stream)
	     condition				D,#TD1PsT[Begin using 006 escapes](1 0 (NIL 0) (NIL :ITALIC NIL) "CPTFONTI");ignore
0	     (format stream "URL form encoding error"))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.430")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define-condition search-url-form-value-not-found
		  (search-url-form-encoding )
  ((indicator :initarg :indicator :reader search-indicator)
   (search-keys :initarg :search-keys :reader search-keys))
  (:report (lambda (condition stream)
	     (format stream "No value found for the search key, ~S, in search keys, ~S"
		     (search-indicator condition) (search-keys condition)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.430")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define-condition search-url-bad-form-encoding
		  (search-url-form-encoding parsing-error)
  ((search-info :initarg :search-info :reader search-info))
  (:report (lambda (condition stream)
	     (format stream "Ill-formed form URL encoding in the search component of ~S"
		     (coerce-url-string (search-info condition))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.431")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defgeneric search-keys-get-value (search-url keyword &optional default)
  (declare (values value found-p))
  (:documentation "Get the value denoted by keyword in form-encoded SEARCH-URL."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.431")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod search-keys-get-value ((url search-mixin) keyword &optional (default nil default-supplied-p))
  (with-slots (search-keys) url
    (unless (consp (car search-keys))
      (error 'search-url-form-encoding :format-string "Search keys do not encode a form"))
    (let ((entry (assoc keyword search-keys :test #'eq)))
      (cond (entry 
	     (values (second entry) t))
	    (default-supplied-p
	     (values default nil))
	    (t (error 'search-url-form-value-not-found :indicator keyword :search-keys search-keys)))))) 


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.431")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(eval-when (load) (export 'url::search-keys-get-value :url))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.431")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defun %relative-path-match-p (path-string string)
  (let* ((path-length (length path-string))
	 (length (length string)))
    (and (<= path-length length)
	 (string= path-string string :start1 0 :end1 path-length :start2 0 :end2 path-length))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.431")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define-generic relative-path-match-p (url path)
   (:documentation "Returns non-null if the relative path, PATH, subsumes URL.
URL and PATH can be any combination of URLs or strings."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.431")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod relative-path-match-p ((url url) (path url))
   (%relative-path-match-p (relative-path-string path) (relative-path-string url)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.431")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod relative-path-match-p ((url url) (path string))
  (%relative-path-match-p path  (relative-path-string url)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.431")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod relative-path-match-p ((url string) (path url))
  (%relative-path-match-p (relative-path-string path) url)) 


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.431")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(eval-when (load) (export 'url::relative-path-match-p :url))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.510")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(define-macro with-string-trim-bounds ((charset string start end) &body body)
  "Rebinds START and END to ignore leading and trailing characters in CHARSET."
  `(locally
     (declare (inline %string-trim-bounds))
     (multiple-value-bind (,start ,end)
	 (%string-trim-bounds ,charset ,string ,start ,end)
       ,@body)))
