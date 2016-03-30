;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.55
;;; Reason: Add HTTPS URL scheme.
;;; 
;;; CLOS class URL::HTTPS-URL:  -
;;; CLOS class URL::HTTPS-PATH:  -
;;; CLOS class URL::HTTPS-MINIMUM-OBJECT:  -
;;; CLOS class URL::HTTPS-OBJECT:  -
;;; CLOS class URL::HTTPS-SEARCH:  -
;;; CLOS class URL::HTTPS-SEARCHABLE-OBJECT:  -
;;; Function URL::DEFINE-HTTP-SCHEME-PARSER:  new
;;; DEFINE-HTTP-SCHEME-PARSER URL:HTTP-URL:  -
;;; Function (CLOS:METHOD URL:CANONICALIZE-URL ((EQL :HTTPS) T)):  -
;;; Written by JCMa, 7/10/00 10:08:59
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.54,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42, HTTP Client Substrate 3.13,
;;; HTTP Proxy Server 5.16, HTTP Client 49.8, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, W4 Constraint-Guide Web Walker 43.0,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.11,
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
  "HTTP:SERVER;URL-CLASS.LISP.17"
  "HTTP:SERVER;URL.LISP.398")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL-CLASS.LISP.17")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: URL; Base: 10 -*-")

;;;------------------------------------------------------------------- 
;;;
;;; HTTPS URL
;;;

(defclass https-url
	  (translation-method-mixin
	    host-port-mixin
	    http-cache-control-mixin
	    expiration-mixin
	    secure-subnets-mixin
	    authentication-mixin
	    content-language-mixin
	    url)
    ((scheme :initform "https" :reader scheme :allocation :class)
     (protocol :initform :https :reader protocol :allocation :class)
     (standard-port :initform 80 :reader standard-port :allocation :class))
  (:documentation "Root class of Secure HTTP URLs."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL-CLASS.LISP.17")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: URL; Base: 10 -*-")

(defclass https-path
          (alternate-url-mixin caching-path-mixin https-url)
    ((directory-writer :initform nil :initarg :directory-writer :accessor directory-writer))
  (:documentation "URL path to a https resource."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL-CLASS.LISP.17")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: URL; Base: 10 -*-")

(defclass https-minimum-object
          (object-mixin path-mixin https-url)
    ()
  (:documentation "The minimum class for https objects."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL-CLASS.LISP.17")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: URL; Base: 10 -*-")

(defclass https-object
          (alternate-url-mixin caching-object-mixin https-minimum-object)
    ()
  (:documentation "Root class for standard https objects on the server."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL-CLASS.LISP.17")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: URL; Base: 10 -*-")

(defclass https-search
          (search-parser-mixin search-mixin https-object)
    ()
  (:documentation "Computes a response based on search suffix supplied in url."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL-CLASS.LISP.17")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: URL; Base: 10 -*-")

(defclass https-searchable-object
          (search-parser-mixin search-mixin https-object)
    ()
  (:documentation "Allow searches of https-objects such as images."))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.398")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(eval-when (:compile-toplevel :execute :load-toplevel)
(defmacro define-http-scheme-parser (scheme url-class object-class path-class search-class searchable-object-class)
  `(define-scheme-parser
     ,scheme
     (:classes (,url-class))
     (url start end)
     (let (object extension search-keys search-parent)
       ;; extract the host parameters
       (multiple-value-bind (host-string port path-index)
	   (get-host-port-info url start end)
	 ;; extract the path components
	 (multiple-value-bind (path object-index next-index search-p)
	     (get-path-info url path-index end)
	   ;; get the object components when present
	   (when object-index
	     (multiple-value-setq (object extension)
	       (get-object-info url object-index next-index)))
	   ;; get the search keys where necessary
	   (when search-p
	     (let ((s-suffix (1+ (the fixnum next-index))))
	       (unless (= s-suffix end)
		 (setq search-parent (intern-url url :start start :end s-suffix)
		       search-keys (funcall (search-parser search-parent) url s-suffix end)))))
	   ;; create the appropriate URL
	   (cond
	     (search-p
	      (let ((object (if extension
				;; searchable object (used for searchable images)
				(make-instance ',searchable-object-class
					       :name-string (when *retain-original-name-string* (subseq url start end))
					       :host-string host-string
					       :port port
					       :path path
					       :object object
					       :extension extension
					       :search-keys search-keys
					       :search-parent search-parent)
				;; regular search urls
				(make-instance ',search-class
					       :name-string (when *retain-original-name-string* (subseq url start end))
					       :host-string host-string
					       :port port
					       :path path
					       :object object
					       :search-keys search-keys
					       :search-parent search-parent))))
		;; inherit the parent's security properties on creation
		(if search-keys
		    (inherit-parent-access-controls object)
		    ;; set the search parent so we know we're the root.
		    (setf (%search-parent object) object))
		object))
	     (object
	      (make-instance ',object-class
			     :name-string (when *retain-original-name-string* (subseq url start end))
			     :host-string host-string
			     :port port
			     :path path
			     :object object
			     :extension extension))
	     (t (make-instance ',path-class
			       :name-string (when *retain-original-name-string* (subseq url start end))
			       :host-string host-string
			       :port port
			       :path path))))))))
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.398")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define-http-scheme-parser http http-url http-object http-path http-search http-searchable-object)

(define-http-scheme-parser https https-url https-object https-path https-search https-searchable-object)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.398")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod canonicalize-url ((scheme (eql :https)) url-string &optional (start 0) (end (length url-string)) destructive-p)
  (%canonicalize-host-prefixed-url "https" 4 url-string start end destructive-p))

