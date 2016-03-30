;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: url; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.79
;;; Reason: Provide URL class creation remapping facility
;;; 
;;; Function URL:DEFINE-SCHEME-PARSER:  incorporate URL class-mapping.
;;; Function URL::WITH-URL-CLASS-MAP:  new class remapping facility.
;;; Function URL::DEFINE-HTTP-SCHEME-PARSER:  use class maps.
;;; DEFINE-HTTP-SCHEME-PARSER URL::HTTP:  update.
;;; DEFINE-HTTP-SCHEME-PARSER URL::HTTPS:  update.
;;; Written by JCMa, 9/29/00 15:10:12
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.78,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, HTTP Proxy Server 5.31,
;;; HTTP Client Substrate 3.24, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Documentation Patches 1.0,
;;; Genera 8 5 Clim Patches 1.0, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Jcma 42, Ivory Revision 5, VLM Debugger 329,
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
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.14),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.5),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.4),
;;; Retry invoke patch (from W:>Reti>retry-invoke-patch).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;URL.LISP.410")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.410")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(eval-when (:compile-toplevel :execute :load-toplevel)

(defvar *url-class-mapping-alist* nil)

(defmacro %canonicalize-scheme (scheme)
  `(typecase scheme
     (keyword scheme)
     (t (intern (string-upcase ,scheme) http:*keyword-package*)))) 

(defun %get-url-class-mapping-variable (scheme)
  (or (second (assoc scheme *url-class-mapping-alist*))
      (error "Unknown URL Scheme, ~S." scheme)))

(defun %get-standard-url-class-map (scheme)
  (get (%get-url-class-mapping-variable scheme) 'standard-class-map)) 

(defun %%get-standard-url-class-mapping-alist (variable)
  (get variable 'standard-class-mapping-alist))

(defun %get-standard-url-class-mapping-alist (scheme)
  (%%get-standard-url-class-mapping-alist (%get-url-class-mapping-variable scheme)))

(defun initialize-standard-url-class-mapping (scheme)
  "Initializes the standard class mapping for the URL class denoted by SCHEME."
  (loop with variable = (%get-url-class-mapping-variable scheme)
	for class in (%get-standard-url-class-map scheme)
	collect (find-class class t) into result
	finally (return (set variable (copy-list result))))) 

(defun %define-standard-url-class-map (scheme variable keyword-class-pairs)
  ;; check all class definitions to avoid runtime errors
  (loop for (keyword class) in keyword-class-pairs
	do (check-type keyword keyword)
	   (find-class class t)
	collect class into class-map
	finally (setf (get variable 'standard-class-map) class-map
		      (get variable 'standard-class-mapping-alist) keyword-class-pairs))
  ;; update scheme entry
  (let* ((scheme (%canonicalize-scheme scheme))
	 (entry (assoc scheme *url-class-mapping-alist*)))
    (if entry 
	(setf (cdr entry) (list* variable keyword-class-pairs))
	(push (list* scheme variable keyword-class-pairs) *url-class-mapping-alist*))
    variable))

(defun %make-url-class-mapping-variable (scheme)
  (intern (string-upcase (format nil "*~A-URL-CLASS-MAP*" scheme)) :url))

(defmacro define-standard-url-class-map ((scheme) &body keyword-class-pairs)
  "Defines a class mapping for use when creating urls with intern-url.
KEYWORD-CLASS-PAIRS are pairs of (KEYWORD URL-CLASS)."
  (let ((scheme-keyword (intern (string-upcase scheme) :keyword))
	(variable (%make-url-class-mapping-variable scheme)))
    `(progn (defvar ,variable nil
	      ,(format nil "The standard class mapping for ~A URLs." scheme))
	    (%define-standard-url-class-map ',scheme-keyword ',variable ',keyword-class-pairs)
	    (initialize-standard-url-class-mapping ',scheme-keyword)
	    ',variable))) 

(defun %get-url-class-accessor (scheme class-type)
  (let ((entry (assoc scheme *url-class-mapping-alist*)))
    (cond (entry 
	   (loop for (keyword) in (cddr entry)
		 for accessor in '(first second third fourth fifth sixth seventh)
		 unless accessor do (error "Too few accessors for the scheme class map, ~S." scheme)
		 when (eq keyword class-type)
		   return accessor
		 finally (error "Unknown class type, ~S, for the scheme, ~S." class-type scheme)))
	  (t (error "Unknown URL Scheme, ~S." scheme)))))

(defun %url-class-accessor-form (scheme class-type)
  (let ((scheme (intern (string-upcase scheme) http:*keyword-package*)))
    `(,(%get-url-class-accessor scheme class-type)
      ,(%get-url-class-mapping-variable scheme))))

(defmacro url-class-accessor-form (scheme class-type)
  (%url-class-accessor-form scheme class-type)) 

(defun %merged-url-class-map (variable fast-p class-map)
  (loop for (keyword default-class) in (%%get-standard-url-class-mapping-alist variable)
	for class = (second (assoc keyword class-map))
	collect (or class default-class) into classes
	finally (return (if fast-p
			    (let ((var (http:symbolize (format nil "*~{~A~^+~}*" classes))))
			      `(handler-case
				 (symbol-value ',var)
				 (error ()
					(setf (symbol-value ',var) (mapcar #'(lambda (class) (find-class class t)) ',classes)))))
			    `(quote ,classes)))))

(defmacro with-url-class-map ((scheme (&key (fast-p t)) class-map) &body body)
  "Overides defaults class map for creation of URL instance for SCHEME.
CLASS-MAP is an alist of (CLASS-KEYWORD CLASS) that is merged against
the default class map for SCHEME. Any URLs created within the scope of BODY
will used the merged class map. When FAST-P is non-null, CLOS classes are
compiled in place."
  (let ((variable (%make-url-class-mapping-variable scheme)))
    `(let ((,variable ,(%merged-url-class-map variable  fast-p class-map)))
       ,@body)))

(export (intern "WITH-URL-CLASS-MAP" :url) :url)

)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.410")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(eval-when (:compile-toplevel :execute :load-toplevel) 

(defun %define-scheme-parser (scheme classes class-map url start end body)
  (flet ((build-code (scheme classes class-map url start end body)
	   (let* ((prefix (string-thin (string scheme)))
		  (parser-name (intern (format nil "%PARSE-URL-~A-SCHEME" prefix) *package*)))
	     `(progn
		,@(when class-map
		    `((define-standard-url-class-map (,scheme) ,@class-map)))
		(defun ,parser-name (url &optional (start 0) end (if-does-not-exist :create))
		  (flet ((create-url (,url ,start ,end interned-p)
			   (let ((url-object (progn . ,body)))
			     (initialize-url url-object interned-p))))
		    (declare (inline create-url))
		    (typecase ,url
		      (string
			;; method dispatch can be removed when debugged -- JCMa 8/15/1997.
			(multiple-value-bind (canonical-url new-url-p)
			    (canonicalize-url ,(intern prefix :keyword) ,url ,start ,end)
			  (declare (dynamic-extent canonical-url))
			  (or (get-url canonical-url)
			      (ecase if-does-not-exist
				(:soft nil)
				(:uninterned (create-url canonical-url 0 (length canonical-url) nil))
				(:create (create-url canonical-url 0 (length canonical-url) t))
				(:error (error 'no-interned-url-found :url-string (if new-url-p (copy-seq canonical-url) canonical-url)))))))
		      ,.(loop for class in classes
			      collect `(,class ,url))
		      (t (error 'bad-data-type-for-scheme :url-string ,url)))))
		(register-scheme ',prefix (function ,parser-name))))))
    (if class-map
	(let* ((scheme-keyword (intern (string-upcase scheme) :keyword))
	       (variable (%make-url-class-mapping-variable scheme-keyword)))
	  (%define-standard-url-class-map scheme-keyword variable class-map )
	  (build-code scheme classes class-map url start end body))
	(build-code scheme classes class-map url start end body))))
  
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.410")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define-macro define-scheme-parser (scheme (&key classes class-map) (url start end) &body body)
  "Defines a parser and object creator for scheme URL.
CLASSES are the valid URL classes which this parser will return.
 (one spanning class is fine). The set of arguments specifies
the names of the arguments passed into BODY.
When CLASS-MAP is provided URL-CLASS-ACCESSOR-FORM may be used to
obtain a form that returns the runtime class for use with make-instance.
CLASS-MAP  is an alist of  (KEYWORD URL-CLASS), where keyword accesses a
particular type of CLOS class."
  (%define-scheme-parser scheme classes class-map url start end body))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.410")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(eval-when (:compile-toplevel :execute :load-toplevel)

(defun %define-http-scheme-parser (scheme url-class object-class path-class search-class searchable-object-class)
  `(define-scheme-parser
     ,scheme
     (:classes (,url-class)
      :class-map ((:object ,object-class) (:path ,path-class) (:search ,search-class) (:search-object ,searchable-object-class)))
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
				(make-instance (url-class-accessor-form ,scheme :search-object)
					       :name-string (when *retain-original-name-string* (subseq url start end))
					       :host-string host-string
					       :port port
					       :path path
					       :object object
					       :extension extension
					       :search-keys search-keys
					       :search-parent search-parent)
				;; regular search urls
				(make-instance (url-class-accessor-form ,scheme :search)
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
	      (make-instance (url-class-accessor-form ,scheme :object)
			     :name-string (when *retain-original-name-string* (subseq url start end))
			     :host-string host-string
			     :port port
			     :path path
			     :object object
			     :extension extension))
	     (t (make-instance (url-class-accessor-form ,scheme :path)
			       :name-string (when *retain-original-name-string* (subseq url start end))
			       :host-string host-string
			       :port port
			       :path path))))))))


  
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.410")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmacro define-http-scheme-parser (scheme url-class object-class path-class search-class searchable-object-class)
    (%define-http-scheme-parser scheme url-class object-class path-class search-class searchable-object-class))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.410")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define-http-scheme-parser http http-url http-object http-path http-search http-searchable-object)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.410")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define-http-scheme-parser https https-url https-object https-path https-search https-searchable-object)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.410")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(initialize-http-scheme-parser)
