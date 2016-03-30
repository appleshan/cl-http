;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.38
;;; Reason: Support for generalized client proxy capability.
;;; 
;;; Function HTTP::VIRTUAL-HOST-P:  new predicate.
;;; CLOS class URL:TELNET-URL:  add protocol and standard-port class allocated slots.
;;; CLOS class URL:FTP-URL:  ditto.
;;; CLOS class URL:NEWS-URL:  ditto.
;;; CLOS class URL:HTTP-URL:  ditto.
;;; CLOS class URL:GOPHER-URL:  ditto.
;;; CLOS class URL:WAIS-URL:  ditto.
;;; Function URL::PROTOCOL:  new.
;;; Function (CLOS:METHOD URL::PROTOCOL (URL:URI)):  default.
;;; Function URL::STANDARD-PORT:  new
;;; Function (CLOS:METHOD URL::PROTOCOL (URL:URI)):  default.
;;; Remove function (CLOS:METHOD URL:HOST-PORT (URL:HTTP-URL)): undefine.
;;; Function (CLOS:METHOD URL:HOST-PORT (URL::HOST-PORT-MIXIN)):  genericall return the standard port for a URL.
;;; Remove function (CLOS:METHOD URL:HOST-PORT (URL:URL)): undefine.
;;; Function (CLOS:METHOD URL:HOST-PORT (URL:URI)):  port is NIL unless thr URI uses host-port-mixin.
;;; Function URL::ROOT-URL-P:  new.
;;; Function (CLOS:METHOD URL::ROOT-URL-P (URL::PATH-MIXIN)):  implement.
;;; Export from URL: PROTOCOL, STANDARD-PORT, ROOT-URL-P.
;;; Function URL::%RELATIVE-PATH-STRING-INDICES:  abstract.
;;; Function URL::WITH-RELATIVE-PATH-STRING-INDICES:  provide handy macro interface.
;;; Function URL::%RELATIVE-PATH-STRING:  define using the abstraction.
;;; Written by JCMa, 5/02/00 02:57:16
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.37,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 41, HTTP Client Substrate 3.7,
;;; HTTP Proxy Server 4.6, HTTP Client 49.7, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, W4 Constraint-Guide Web Walker 41.3,
;;; W4 Examples 13.0, Ivory Revision 5, VLM Debugger 329, Genera program 8.11,
;;; DEC OSF/1 V4.0 (Rev. 110),
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
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;UTILS.LISP.439"
  "HTTP:SERVER;URL-CLASS.LISP.16"
  "HTTP:SERVER;URL.LISP.388")



;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.388")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

#+Genera
(mapc #'(lambda (x)
	  (let ((sym (scl:intern-local-soft x :http)))
	    (when sym (unintern sym :http)))
	  (export (intern x :url) :url))
      '("PROTOCOL" "STANDARD-PORT" "ROOT-URL-P"))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.439")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(define virtual-host-p (domain-name port)
  "Returns non-null if DOMAIN-NAME is defined as a virtual host on PORT."
  (let ((entry (gethash domain-name *virtual-host-table*)))
    (and entry (loop for (host-port) in entry
		     when (eql host-port port)
		       return t
		     finally (return nil)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL-CLASS.LISP.16")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: URL; Base: 10 -*-")

(defclass telnet-url
	  (user-id-mixin host-port-mixin url)
    ((scheme :initform "telnet" :reader scheme :allocation :class)
     (protocol :initform :telnet :reader protocol :allocation :class)
     (standard-port :initform 23 :reader standard-port :allocation :class))
  (:documentation "Telnet url class."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL-CLASS.LISP.16")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: URL; Base: 10 -*-")

(defclass ftp-url
	  (url user-id-and-pw-mixin)
    ((scheme :initform "ftp" :reader scheme :allocation :class)
     (protocol :initform :ftp :reader protocol :allocation :class)
     (standard-port :initform 21 :reader standard-port :allocation :class))
  (:documentation "File Transfer Protocol URL class."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL-CLASS.LISP.16")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: URL; Base: 10 -*-")

(defclass news-url (colon-scheme-prefix-mixin url)
    ((scheme :initform "news" :reader scheme :allocation :class)
     (protocol :initform :nttp :reader protocol :allocation :class)
     (standard-port :initform 119 :reader standard-port :allocation :class))
  (:documentation "News URL class."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL-CLASS.LISP.16")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: URL; Base: 10 -*-")

(defclass http-url
   (translation-method-mixin
     host-port-mixin
     http-cache-control-mixin
     expiration-mixin
     secure-subnets-mixin
     authentication-mixin
     content-language-mixin
     url)
   ((scheme :initform "http" :reader scheme :allocation :class)
     (protocol :initform :http :reader protocol :allocation :class)
      (standard-port :initform 80 :reader standard-port :allocation :class))
   (:documentation "Root class of hypertext transfer protocol urls."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL-CLASS.LISP.16")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: URL; Base: 10 -*-")

(defclass gopher-url 
	  (url)
    ((scheme :initform "gopher" :reader scheme :allocation :class)
     (protocol :initform :gopher :reader protocol :allocation :class)
     (standard-port :initform 70 :reader standard-port :allocation :class))
  (:documentation "Gopher url class."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL-CLASS.LISP.16")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: URL; Base: 10 -*-")

(defclass wais-url (url)
    ((scheme :initform "wais" :reader scheme :allocation :class)
     (protocol :initform :wais :reader protocol :allocation :class))
  (:documentation "Wide area information server url class."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.388")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defgeneric protocol (uri)
   (declare (values protocol-keyword))
   (:documentation "Returns the standard protocol over which URI is normally transported."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.388")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod protocol ((uri uri))
   (error "No standard protocol is defined for the URI, ~A." (name-string uri)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.388")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defgeneric standard-port (uri)
   (declare (values port-number))
   (:documentation "Returns the standard port over which URI is normally transported."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.388")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod standard-port ((uri uri))
   (error "No standard port is defined for the URI, ~A." (name-string uri)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.388")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(SCL:FUNDEFINE '(METHOD HOST-PORT (HTTP-URL)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.388")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod host-port ((url host-port-mixin))
  (or (port url) (standard-port url)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.388")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(SCL:FUNDEFINE '(METHOD HOST-PORT (URL)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.388")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod host-port ((uri uri))
   nil)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.388")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define-generic root-url-p (url)
  (:documentation "Returns non-null if URL is the root URL for a given host and port."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.388")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod root-url-p ((url path-mixin))
  (not (or (object url)
	   (path url))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.388")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

#+Genera
(mapc #'(lambda (x)
	  (let ((sym (scl:intern-local-soft x :http)))
	    (when sym (unintern sym :http)))
	  (export (intern x :url) :url))
      '("PROTOCOL" "STANDARD-PORT" "ROOT-URL-P"))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.388")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(declaim (inline %relative-path-string-indices))

(defun %relative-path-string-indices (url-string start end)
  (declare (fixnum start end)
	   (string url-string))
  (let* ((pos1 (or (string-search= "://" url-string 0 3 start (min end (+ 3 (the fixnum *scheme-maximum-length*))))
		   (error 'no-scheme-found :url-string (subseq url-string start end))))
	 (pos2 (char-position #\/ url-string (+ 3 (the fixnum pos1)) end))
	 pos3)
    (cond (pos2
	   (if (setq pos3 (char-position #\/ url-string (1+ (the fixnum pos2)) end t))
	       (values pos2 (1+ (the fixnum pos3)))
	       (values pos2 (1+ (the fixnum pos2)))))
	  (t (values end end)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.388")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmacro with-relative-path-string-indices ((url-string start end) &body body)
  `(multiple-value-bind (,(intern "START" *package*) ,(intern "END" *package*))
       (%relative-path-string-indices ,url-string ,start ,end)
     ,@body))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.388")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defun %relative-path-string (url-string &optional (end (length url-string)))
  (with-relative-path-string-indices (url-string 0 end)
    (cond ((or (= start end)
	       (= (1+ (the fixnum start)) end))
	   "/")
	  (t (make-array (the fixnum (- (the fixnum end) (the fixnum start)))
			 :element-type (array-element-type url-string)
			 :displaced-to url-string :displaced-index-offset start)))))

(declaim (notinline %relative-path-string-indices))  

