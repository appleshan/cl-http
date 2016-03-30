;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.29
;;; Reason: Define internet mail address parsing building from mailto-url and use it to
;;; parse and print the FROM header. Use the FROM header as a secondary source
;;; for the author in the PUT method.
;;; 
;;; DEFINE-HEADER-TYPE :MAIL-ADDRESS-SEQUENCE-HEADER:  new
;;; DEFINE-HEADER :FROM:  define as mail-address-sequence-header.
;;; 
;;; CLOS class URL::MAIL-ADDRESS:  specialize mailto-url to handle email addresses.
;;; Variable URL::*SMTP-HEADER-SPECIAL-CHARACTERS*:  -
;;; Function URL::SMTP-HEADER-SPECIAL-CHARACTER-P:  -
;;; Function URL::CREATE-INTERNET-MAIL-ADDRESS:  -
;;; Function URL::PARSE-INTERNET-MAIL-ADDRESS:  -
;;; Function URL::WRITE-INTERNET-MAIL-ADDRESS:  -
;;; Function (CLOS:METHOD URL::WRITE-INTERNET-MAIL-ADDRESS (URL::MAIL-ADDRESS T)):  -
;;; Function (CLOS:METHOD URL::WRITE-INTERNET-MAIL-ADDRESS (URL:MAILTO-URL T)):  -
;;; Function URL::%MAKE-EMAIL-ADDRESS:  -
;;; Function URL::EMAIL-ADDRESS:  -
;;; Function (CLOS:METHOD URL::EMAIL-ADDRESS (URL:MAILTO-URL)):  -
;;; Function URL::%MAKE-FULL-EMAIL-ADDRESS:  -
;;; Function (CLOS:METHOD URL::FULL-EMAIL-ADDRESS (URL:MAILTO-URL)):  -
;;; Function (CLOS:METHOD URL::FULL-EMAIL-ADDRESS (URL:MAIL-ADDRESS)):  -
;;; Function (CLOS:METHOD URL:FULL-EMAIL-ADDRESS (STRING)):  -
;;; Function HTTP::USER-EMAIL-ADDRESS-VIA-FROM-HEADER:  new
;;; Function (CLOS:METHOD HTTP::USER-EMAIL-ADDRESS-VIA-FROM-HEADER (HTTP::BASIC-SERVER-MIXIN)):  implement.
;;; Function HTTP::USER-EMAIL-COMPONETS-VIA-FROM-HEADER:  new
;;; Function (CLOS:METHOD HTTP::USER-EMAIL-COMPONETS-VIA-FROM-HEADER (HTTP::BASIC-SERVER-MIXIN)):  implement.
;;; Remove function (CLOS:METHOD WWW-UTILS:SET-FILE-AUTHOR (CL:PATHNAME HTTP::SERVER)): undefine.
;;; Function (CLOS:METHOD WWW-UTILS:SET-FILE-AUTHOR (CL:PATHNAME HTTP::SERVER-AUTHENTICATION-MIXIN)):
;;; Use FROM header as secondary author source and move onto server-authentication-mixin.
;;; Remove function (CLOS:METHOD HTTP:COPY-FILE (STRING STRING)): undefine.
;;; Function (CLOS:METHOD HTTP:COPY-FILE (STRING T)):  define.
;;; Function URL:HTTP-URL-STRING-P:  make sure this distringuishes the http: logical host.
;;; Function URL::URL-STRING-P:  new.
;;; Function (CLOS:METHOD HTTP:COPY-FILE (T STRING)):  intern urls too.
;;; Function (CLOS:METHOD HTTP:COPY-FILE (STRING T)):  ditoo.
;;; Remove function (CLOS:METHOD HTTP:COPY-FILE (T T)): undefine.
;;; Written by JCMa, 3/19/00 18:40:38
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.28,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, HTTP Proxy Server 4.5,
;;; HTTP Client Substrate 3.3, Jcma 41, HTTP Client 49.3, Image Substrate 440.4,
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

;;; Patch file for CL-HTTP version 70.29
;;; Written by JCMa, 3/20/00 01:35:10
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.28,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, HTTP Proxy Server 4.5,
;;; HTTP Client Substrate 3.3, Jcma 41, HTTP Client 49.3, Image Substrate 440.4,
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
  "HTTP:SERVER;SERVER.LISP.826"
  "HTTP:SERVER;URL-CLASS.LISP.15"
  "HTTP:SERVER;URL.LISP.379"
  "HTTP:SERVER;URL.LISP.380"
  "HTTP:SERVER;URL.LISP.383"
  "HTTP:SERVER;HEADERS.LISP.452"
  "HTTP:SERVER;URL.LISP.384"
  "HTTP:SERVER;HEADERS.LISP.450"
  "HTTP:SERVER;SERVER.LISP.827"
  "HTTP:SERVER;SERVER.LISP.828"
  "HTTP:SERVER;UTILS.LISP.430"
  "HTTP:SERVER;URL.LISP.385"
  "HTTP:SERVER;URL.LISP.386"
  "HTTP:SERVER;UTILS.LISP.431")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.379")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(PROGN
(eval-when (:execute :compile-toplevel :load-toplevel)
  (mapc #'(lambda (x)
	    (import (intern x :http) :url))
	'("WITH-STRING-TRIM-BOUNDS" "IP-ADDRESS-STRING-P" "*STANDARD-CHARACTER-TYPE*" "*WHITE-SPACE-CHARS*" "%FAST-POSITION-IF" "FAST-FORMAT")))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (mapc #'(lambda (x)
	    (export (intern x :http) :http))
	'("IP-ADDRESS-STRING-P" "*STANDARD-CHARACTER-TYPE*")))

(eval-when (:execute :compile-toplevel :load-toplevel)
  #+Genera
  (let ((sym (scl:intern-local-soft "EMAIL-ADDRESS" :http)))
    (when sym (unintern sym :http)))
  (mapc #'(lambda (x)
	    (export (intern x :url) :url))
	'("MAIL-ADDRESS" "EMAIL-ADDRESS" "WRITE-INTERNET-MAIL-ADDRESS" "PARSE-INTERNET-MAIL-ADDRESS" "FULL-EMAIL-ADDRESS" "URL-STRING-P")))

)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.826")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod set-file-author ((pathname pathname) (server server) &optional (error-p t))
  (let ((name (or (user-name server)
		  (user-email-address-via-from-header server))))
    (when name
      (set-file-author (probe-file pathname) name error-p))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL-CLASS.LISP.15")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: URL; Base: 10 -*-")

(defclass mail-address
	  (mailto-url)
    ((personal-name :initform nil :initarg :personal-name :reader personal-name)	; personal name
     (original-string :initform nil :initarg :original-string :reader original-string))	; raw string before parsing
  (:documentation "RFC 822 Internet mail address."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.379")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

;;;------------------------------------------------------------------- 
;;;
;;; INTERNET MAIL ADDRESSES
;;;

(defconstant *smtp-header-special-characters* '(#\( #\) #\< #\> #\@ #\, #\; #\: #\\ #\" #\. #\[ #\])
  "Special characters for SMTP mail addresses per RFC 822.")

(declaim (inline smtp-header-special-character-p))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.379")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(declaim (inline smtp-header-special-character-p))

(defun smtp-header-special-character-p (char)
  "Returns non-null if CHAR is a special character for RFC 822 mail addresses."
  (member char *smtp-header-special-characters* :test #'eql))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.379")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(declaim (inline create-mail-address))

(defun create-internet-mail-address (name host-string &optional personal-name original-string)
   (make-instance 'mail-address
       :user-id name
       :host-string host-string
       :personal-name personal-name
       :original-string original-string))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.379")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defgeneric write-internet-mail-address (mailto-url stream &optional recompute-p)
  (:documentation "Writes an internet email address for MAILTO-URL on STREAM."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.379")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod write-internet-mail-address ((mail-address mail-address) stream &optional (recompute-p t))
  (flet ((write-personal-name (personal-name stream)
	   (if (%fast-position-if smtp-header-special-character-p personal-name)
	       (fast-format stream "~S" personal-name)
	       (fast-format stream "~A" personal-name)))
	 (write-host-string (host-string stream)
	   (if (ip-address-string-p host-string)
	       (fast-format stream "[~A]" host-string)
	       (fast-format stream "~A" host-string))))
    (declare (inline write-personal-name))
    (with-slots (original-string host-string user-id personal-name) mail-address
      (cond ((and original-string (not recompute-p)) (write-string original-string stream))
	    ((not (and user-id host-string)) (error "Ill-formed Internet mail address, ~S" mail-address))
	    (personal-name
	     (write-personal-name personal-name stream)
	     (fast-format stream "<~A@~I>" user-id (write-host-string host-string stream)))
	    (t (fast-format stream "~A@~I" user-id (write-host-string host-string stream)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.379")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod write-internet-mail-address ((mail-address mailto-url) stream &optional recompute-p)
  (declare (ignore recompute-p))
  (flet ((write-host-string (host-string stream)
	   (if (ip-address-string-p host-string)
	       (fast-format stream "[~A]" host-string)
	       (fast-format stream "~A" host-string))))
    (declare (inline write-host-string))
    (with-slots (host-string user-id) mail-address
      (if (and user-id host-string)
	  (fast-format stream "~A@~I" user-id (write-host-string host-string stream))
	  (error "Ill-formed Internet mail address, ~S" mail-address)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.379")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defun %make-email-address (user-id host-string)
  (let* ((l1 (length user-id))
	 (l2 (length host-string))
	 (ip-string-p (ip-address-string-p host-string))
	 (l3 (+ l1 l2 (if ip-string-p 3 1)))
	 (string (make-array l3 :element-type http::*standard-character-type*)))
    (declare (fixnum l1 l2 l3))
    (copy-vector-portion user-id 0 l1 string 0 l1)
    (setf (aref string l1) #\@)
    (when ip-string-p
      (setf (aref string (setq l1 (1+ l1))) #\[
	    (aref string (setq l3 (1- l3))) #\]))
    (copy-vector-portion host-string 0 l2 string (1+ l1) l3)
    string))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.379")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defgeneric email-address (mailto-url &optional recompute-p)
  (declare (values email-address))
  (:documentation "Returns the basic email address for mailto-url."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.379")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod email-address ((mail-address mailto-url) &optional recompute-p)
  (with-slots (user-id host-string) mail-address
    (with-value-cached (mail-address :email-address :recompute-p recompute-p)
      (%make-email-address user-id host-string))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.380")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defun %make-full-email-address (user-id host-string personal-name)
  (let* ((special-chars-p (%fast-position-if smtp-header-special-character-p personal-name))
	 (l1 (length personal-name))
	 (s2 (+ l1 (if special-chars-p 4 2)))
	 (l2 (length user-id))
	 (e2 (+ s2 l2))
	 (l3 (length host-string))
	 (ip-string-p (ip-address-string-p host-string))
	 (l4 (+ l1 l2 l3 (if special-chars-p 5 3) (if ip-string-p 3 1)))
	 (e3 (1- l4))
	 (string (make-array l4 :element-type *standard-character-type*)))
    (declare (fixnum l1 s2 l2 e2 l3 l4 e3))
    (cond (special-chars-p
	   (setf (aref string 0) #\"
		 (aref string (1+ l1)) #\")
	   (copy-vector-portion personal-name 0 l1 string 1 (1+ l1)))
	  (t (copy-vector-portion personal-name 0 l1 string 0 l1)))
    (setf (aref string (- s2 2)) #\space
	  (aref string (1- s2)) #\<
	  (aref string e3) #\>)
    (copy-vector-portion user-id 0 l2 string s2 e2)
    (setf (aref string e2) #\@)
    (when ip-string-p
      (setf (aref string (setq e2 (1+ e2))) #\[
	    (aref string (setq e3 (1- e3))) #\]))
    (copy-vector-portion host-string 0 l3 string (1+ e2) e3)
    string))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.380")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod full-email-address ((mail-address mailto-url) &optional recompute-p)
  (email-address mail-address recompute-p))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.383")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod full-email-address ((mail-address mail-address) &optional recompute-p)
  (with-slots (user-id host-string) mail-address
    (let (original-string personal-name)
      (cond ((and (not recompute-p)
		  (setq original-string (original-string mail-address)))
	     original-string)
	    ((setq personal-name (personal-name mail-address))
	     (with-value-cached (mail-address :full-email-address :recompute-p recompute-p)
	       (%make-full-email-address user-id host-string personal-name)))
	    (t (email-address mail-address recompute-p))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.452")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;;;------------------------------------------------------------------- 
;;;
;;; FROM HEADER
;;;

(defun parse-mail-addresses (string &optional (start 0) (end (length string)))
  (flet ((first-non-blank (start end)
           (position-if-not* #'white-space-char-p string :start start :end end)))
    (declare (inline first-non-blank))
    (loop for s = (first-non-blank start end) then (first-non-blank (1+ idx) end)
	  while s
	  for at-idx = (char-position #\@ string s end)
	  while at-idx
          for idx fixnum = (or (char-position #\, string at-idx end) end)
          for last = (position-if-not* #'white-space-char-p string :start s :end idx :from-end t)
          when last
            collect (url:parse-internet-mail-address string s (1+ (the fixnum last)))
          while (< idx end))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.384")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defun parse-internet-mail-address (string &optional (start 0) (end (length string)))
  (declare (fixnum start end))
  (flet ((parse-address (string start end)
	   (declare (fixnum start end))
	   (let* ((at-pos (char-position #\@ string start end t))
		  (at1 (and at-pos (1+ (the fixnum at-pos)))))
	     (declare (fixnum at-pos at1))
	     (when at-pos
	       (values (make-array (- at-pos start) :element-type (array-element-type string)
				   :displaced-to string :displaced-index-offset start)
		       (with-string-trim-bounds (`(#\[ #\]) string at1 end)
			 (make-array (- end at1) :element-type (array-element-type string)
				     :displaced-to string :displaced-index-offset at1))))))
	 (parse-personal-name (string start end)
	   (with-string-trim-bounds ('(#\" . #.*white-space-chars*) string start end)
	     (unless (= start end)
	       (make-array (- end start) :element-type (array-element-type string)
			   :displaced-to string :displaced-index-offset start)))))
    (declare (inline parse-personal-name))
    (with-string-trim-bounds (*white-space-chars* string start end)
      (let* ((original-string (subseq string start end))
	     (s1 0)
	     (e1 (- end start))
	     (e (char-position #\> original-string s1 e1 t))
	     (s (and e (char-position #\< original-string s1 e t)))
	     (s2 (and s (1+ (the fixnum s))))
	     (personal-name-p (and s2 e (/= s2 e))))
	(declare (fixnum s e s1))
	(multiple-value-bind (user host)
	    (if personal-name-p
		(parse-address original-string s2 e)
		(parse-address original-string s1 e1))
	  (cond ((and user host)
		 (create-internet-mail-address user host
					       (and personal-name-p (parse-personal-name original-string s1 s))
					       original-string))
		(t (error "Ill-formed RFC 822 Mail Address, ~S" original-string))))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.452")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun print-mail-addresses (mail-address-list stream)
  (print-comma-separated-header mail-address-list stream #'url:full-email-address))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.384")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod full-email-address ((mail-address string) &optional recompute-p)
  (declare (ignore recompute-p))
  mail-address)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.450")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header-type :mail-address-sequence-header (:comma-separated-header)
  :parse-function parse-mail-addresses
  :print-function print-mail-addresses)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.450")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;; parse RFC 822, 1123 email address headers sometime.
(define-header :from
               (:mail-address-sequence-header :request)
  :print-string "From")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.827")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(define-generic user-email-address-via-from-header (server)
  (declare (values user-name@mail-host))
  (:documentation "Returns the user's email address as name@host."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.828")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod user-email-address-via-from-header ((server basic-server-mixin))
  (with-header-values (from) (server-headers server)
    (when from
      (url::email-address (car from)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.828")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(define-generic user-email-componets-via-from-header (server)
  (declare (values user-name mail-host original-string))
  (:documentation "Returns the components of the user's mail address."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.828")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod user-email-componets-via-from-header ((server basic-server-mixin))
  (with-header-values (from) (server-headers server)
    (when from
      (let ((mail-address (car from)))
	(values (url::user-id mail-address)
		(host-string mail-address)
		(url::personal-name mail-address))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.828")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(SCL:FUNDEFINE '(METHOD SET-FILE-AUTHOR (PATHNAME SERVER)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.828")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod set-file-author ((pathname pathname) (server server-authentication-mixin) &optional (error-p t))
  (let ((name (or (user-name server)
		  (user-email-address-via-from-header server))))
    (when name
      (set-file-author (probe-file pathname) name error-p))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.430")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(SCL:FUNDEFINE '(METHOD COPY-FILE (STRING STRING)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.385")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define http-url-string-p (url-string &optional (start 0) (end (length url-string)))
  (declare (values http-scheme-p relative-url-p)
	   (fixnum start end))
  (with-fast-array-references ((url-string url-string string))
    (let ((colon (char-position #\: url-string start end)))
      (declare (fixnum colon))
      (cond ((and colon (eq *http-scheme-parser* (get-scheme-parser url-string start (1+ colon) t)))
	     (values (and (< (+ 2 colon) end)
			  (eql (aref url-string (1+ colon)) #\/)
			  (eql (aref url-string (+ 2 colon)) #\/))
		     nil))
	    ;; Some unknown scheme is present.
	    (colon (values nil nil))
	    ;; it's potentially a relative HTTP URL if it starts with / or a legal url character.
	    ((< start end) (values nil t))
	    (t (values nil nil))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.386")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define url-string-p (url-string &optional (start 0) (end (length url-string)))
  "Returns non-null if url-string is a fully qualified URL string."
  (declare (fixnum start end))
  (with-fast-array-references ((url-string url-string string))
    (let* ((colon (char-position #\: url-string start end))
	   (parser-p (and colon (get-scheme-parser url-string start (1+ colon) t))))
      (declare (fixnum colon))
      (cond ((null parser-p) nil)
	    ((or (string-equal "http" url-string :start1 0 :end1 4 :start2 start :end2 colon)
		 (string-equal "ftp" url-string :start1 0 :end1 3 :start2 start :end2 colon))
	     (and (< (+ 2 colon) end)
		  (eql (aref url-string (1+ colon)) #\/)
		  (eql (aref url-string (+ 2 colon)) #\/)))
	    (t t)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.431")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmethod copy-file (from-pathname (to-pathname string) &key (copy-mode :text) &allow-other-keys)
  (copy-file from-pathname 
	     (if (url::url-string-p to-pathname)
		 (url:intern-url to-pathname :if-does-not-exist :uninterned)
		 (pathname to-pathname))
	     :copy-mode copy-mode))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.431")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmethod copy-file ((from-pathname string) to-pathname &key (copy-mode :text) &allow-other-keys)
  (copy-file (if (url::url-string-p from-pathname)
		 (url:intern-url from-pathname :if-does-not-exist :uninterned)
		 (pathname from-pathname))
	     to-pathname :copy-mode copy-mode))
