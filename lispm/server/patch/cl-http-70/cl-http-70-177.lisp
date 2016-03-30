;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.177
;;; Reason: Enable relativization of URLs for transport through a reverse proxy.
;;; 
;;; Function HTTP::%MAKE-LOCAL-STREAM-FUNCTION-FOR-WITH-TRANSFER-DECODING:  fix bugs for CRLF decoding.
;;; Function URL::RELATIVE-NAME-STRING-WITH-UNESCAPED-SEARCH-SUFFIX:  new
;;; Function (CLOS:METHOD URL::RELATIVE-NAME-STRING-WITH-UNESCAPED-SEARCH-SUFFIX (URL::SEARCH-MIXIN)):  -
;;; Function HTTP::WRITE-DIRECTORY-LISTING-AS-URI-LIST:  use coerce-url-string
;;; Function HTTP::WRITE-DIRECTORY-LISTING-AS-URI-PROPERTY-LIST:  ditto.
;;; Function (CLOS:METHOD URL:LOCAL-CONTEXT-STRING (URL::HOST-PORT-MIXIN)):  faster method on correct mixin.
;;; Remove function (CLOS:METHOD URL:LOCAL-CONTEXT-STRING (URL:HTTP-URL)): remove overly specialized and slower method.
;;; Remove function URL:COERCE-URL-STRING: undefine.
;;; Variable URL::*RELATIVIZE-URLS*:  new variable.
;;; Function URL:COERCE-URL-STRING:  newly haired up.
;;; Function (CLOS:METHOD URL:COERCE-URL-STRING (STRING)):  -
;;; Function (CLOS:METHOD URL:COERCE-URL-STRING (URL:URL)):  -
;;; Function (CLOS:METHOD URL:COERCE-URL-STRING (URL:URL)):  -
;;; Function (CLOS:METHOD URL:COERCE-URL-STRING (URL::SEARCH-MIXIN)):  -
;;; Function NS11::BODY-ARGUMENTS:  remove downcase arg
;;; Function HTTP::SET-LOCAL-CONTEXT:  use url:local-context-string
;;; Function HTTP:WITH-LOCAL-CONTEXT:  update.
;;; Written by JCMa, 10/08/03 21:28:26
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5-inc1.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.176,
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
  "HTTP:SERVER;HEADERS.LISP.529"
  "HTTP:SERVER;SERVER.LISP.930"
  "HTTP:SERVER;URL.LISP.436"
  "HTTP:SERVER;URL.LISP.437"
  "HTTP:SERVER;URL.LISP.438"
  "HTTP:SERVER;PACKAGE.LISP.480"
  "HTTP:SERVER;NETSCAPE-1-1.LISP.129"
  "HTTP:SERVER;UTILS.LISP.529")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.529")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun %make-local-stream-function-for-with-transfer-decoding (functions content-length headers default-copy-mode body &aux fctns)
  (cond-every
    ((member 'stream-copy-until-eof functions)
     (push `(stream-copy-until-eof
	      (from-stream to-stream &optional mode)
	      (let ((content-length ,(if content-length content-length
					 `(get-header :content-length ,headers)))
		    (copy-mode (or mode ,default-copy-mode)))
		(if content-length
		    (handler-bind
		      ((end-of-file #'%handle-end-of-file-for-with-transfer-decoding))
		      (stream-copy-bytes from-stream to-stream content-length copy-mode))
		    (stream-copy-until-eof from-stream to-stream copy-mode))))
	   fctns))
    ((member 'stream-decode-crlf-until-eof functions)
     (push `(stream-decode-crlf-until-eof
	      (from-stream to-stream)
	      (let ((content-length ,(if content-length content-length
					 `(get-header :content-length ,headers))))
		(if content-length
		    (handler-bind
		      ((end-of-file #'%handle-end-of-file-for-with-transfer-decoding))
		      (stream-decode-crlf-bytes from-stream to-stream content-length))
		    (stream-decode-crlf-until-eof from-stream to-stream))))
	   fctns)))
  (if fctns
      `(flet ,fctns
	 ,@(loop for fctn in functions
		 collect `(function ,fctn) into fspecs
		 finally (return (when fspecs
				   `((declare (dynamic-extent . ,fspecs))
				     ,@fspecs))))       ;no compiler warnings
	 ,@body)
      `(progn . ,body)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.436")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define-generic relative-name-string-with-unescaped-search-suffix (url &optional recache-p)
  (:documentation "Returns the relative name-string of URL without escaping the search suffix."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.436")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod relative-name-string-with-unescaped-search-suffix ((url search-mixin) &optional recache-p)
  (with-value-cached (url 'relative-name-string-with-unescaped-search-suffix :recompute-p recache-p)
    (%url-string-with-unescaped-search-suffix (relative-name-string url))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.930")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

;; returns text/uri-list
(defun write-directory-listing-as-uri-list (url stream inclusion-predicate path-url-intern-function &optional directories-p)
  (declare (dynamic-extent inclusion-predicate path-url-intern-function))
  (flet ((write-item (path stream)
	   (multiple-value-bind (url-inf newly-created-p)
	       (funcall path-url-intern-function path url)
	     (when url-inf
	       (when newly-created-p
		 ;; ensure that it has been exported.
		 (let ((export-type  (typecase url-inf
				       (http-path (translation-method url))	;export directory paths correctly
				       (t (pathname-export-type path nil)))))
		   (when export-type
		     (export-url url-inf export-type :pathname path)
		     (inherit-export-parameters url-inf url))))
	       (fresh-line stream)
	       (write-string (coerce-url-string url-inf) stream)))))
    (with-directory-index-caching (url stream :uri-list :last-modification-function 'file-modification-date) 
      (loop with default = (translated-pathname url)
	    for path in (if directories-p
			    (www-utils:directory-list* default inclusion-predicate :files :directories :sorted)
			    (www-utils:directory-list* default inclusion-predicate :files :sorted))
	    for translated = (translated-pathname path)
	    do (write-item translated stream)))
    url))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.930")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

;; returns text/x-directory-list
(defun write-directory-listing-as-uri-property-list (url stream inclusion-predicate path-url-intern-function &optional directories-p)
  (declare (dynamic-extent inclusion-predicate path-url-intern-function))
  (flet ((write-item (path plist stream)
	   (multiple-value-bind (url-inf newly-created-p)
	       (funcall path-url-intern-function path url)
	     (when url-inf
	       (when newly-created-p
		 ;; ensure that it has been exported.
		 (let ((export-type  (typecase url-inf
				       (http-path (translation-method url))	;export directory paths correctly
				       (t (pathname-export-type path nil)))))
		   (when export-type
		     (export-url url-inf export-type :pathname path)
		     (inherit-export-parameters url-inf url))))
	       (destructuring-bind (&key length-in-bytes creation-date directory modification-date author &allow-other-keys) plist
		 (fast-format stream "~&(~S~I)"
			      (coerce-url-string url-inf)
			      (cond-every
				(directory (fast-format stream " :directory t"))
				(length-in-bytes (fast-format stream " :length;;
-in-bytes ~D" length-in-bytes))
				(modification-date (fast-format stream " :modification-date ~D" modification-date))
				(creation-date (fast-format stream " :creation-date ~D" creation-date))
				(author (fast-format stream " :author ~S" author)))))))))
    (with-directory-index-caching (url stream :uri-property-list :last-modification-function 'file-modification-date) 
      (loop with default = (translated-pathname url)
	    for (path . plist) in (if directories-p
				      (www-utils:directory-list* default inclusion-predicate :files :directories :properties :sorted)
				      (www-utils:directory-list* default inclusion-predicate :files :properties :sorted))
	    for translated = (translated-pathname path)
	    do (write-item translated plist stream)))
    url))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.436")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defun %local-context-string-indices (url-string &optional (start 0) (end (length url-string)) no-error-p)
  (declare (fixnum start end)
	   (string url-string))
  (let* ((pos0 (or (http::%fast-position-if alpha-char-p url-string :start start :end end)
                   start))			;blowout in the next clause
         (pos1 (or (string-search= "://" url-string 0 3 pos0 (min end (+ (the fixnum pos0) 3 (the fixnum *scheme-maximum-length*))))
		   (if no-error-p
                       (return-from %local-context-string-indices nil)
		       (error 'no-scheme-found :url-string (subseq url-string (or pos0 start) end)))))
	 (pos2 (or (char-position #\/ url-string (+ 3 (the fixnum pos1)) end) end)))
    (values pos0 pos2)))

(defmacro with-local-context-string-indices ((url-string start end &optional no-error-p) &body body)
  `(multiple-value-bind (,(intern "START" *package*) ,(intern "END" *package*))
       (%local-context-string-indices ,url-string ,start ,end ,no-error-p)
     ,@body))

(declaim (inline %local-context-string))

(defun %local-context-string (url-string &optional (end (length url-string)) no-error-p)
  (with-local-context-string-indices (url-string 0 end no-error-p)
    (when start
      (subseq url-string start end))))

(defmethod local-context-string ((url host-port-mixin) &optional recompute-p)
  (with-value-cached (url :local-context-string :recompute-p recompute-p)
    (%local-context-string (name-string url recompute-p))))

(defmethod local-context-string ((url string) &optional recompute-p)
  (declare (ignore recompute-p))
  (nstring-downcase (%local-context-string url)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.436")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(PROGN
(declaim (notinline %local-context-string))

(defun %%%local-context-match-p (local-context1 url-string2 &optional (start1 0) (end1 (length local-context1))
                                                (start2 0) (end2 (length url-string2)))
  (declare (values success-start2 success-end2))
  (when (http::%string-equal local-context1 url-string2 start1 end1 start2 end2)
    (return-from  %%%local-context-match-p (values start2 end2))))

(declaim (inline %%local-context-match-p))

(defun %%local-context-match-p (local-context1 url-string2 &optional (start1 0) (end1 (length local-context1))
                                               (start2 0) (end2 (length url-string2)))
  (declare (values success-start2 success-end2))
  (multiple-value-bind (s2 e2)
      (%local-context-string-indices url-string2 start2 end2 t)
    (when s2
      (%%%local-context-match-p local-context1 url-string2 start1 end1 s2 e2))))

(declaim (inline %local-context-match-p))

(defun %local-context-match-p (local-context1 url-string2 &optional (start1 0) (end1 (length local-context1))
                                              (start2 0) (end2 (length url-string2)))
  (declare (values success-start2 success-end2))
  (multiple-value-bind (s1 e1)
      (%local-context-string-indices local-context1 start1 end1 t)
    (when s1
      (%%local-context-match-p local-context1 url-string2 s1 e1 start2 end2))))

(defgeneric local-context-match-p (url1 url2 &optional recache-p)
    (declare (values success-start2 success-end2))
    (:documentation "If the local context matches for url1 and url1, this returns non-null.
When successful, this returns multiple values for the start and end indices of the local context in URL2.
The local context is the first part of a URL upto the first slash delimiting directories."))

(defmethod local-context-match-p ((url-string1 string) (url-string2 string) &optional recache-p)
  (declare (ignore recache-p))
  (%local-context-match-p url-string1 url-string2 0 (length url-string1) 0 (length url-string2)))

(declaim (notinline %local-context-match-p))
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.436")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(PROGN
(defmethod local-context-match-p ((url1 url) (url2 url) &optional recache-p)
  (flet ((get-indices (url url-string recompute-p)
           (with-value-cached (url :local-context-indices :recompute-p recompute-p)
             (multiple-value-list 
              (%local-context-string-indices url-string 0 (length url-string))))))
    (let ((url-string1 (name-string url1))
          (url-string2 (name-string url2)))
      (destructuring-bind (s1 e1)
          (get-indices url1 url-string1 recache-p)
        (destructuring-bind (s2 e2)
            (get-indices url2 url-string2 recache-p)
          (%%%local-context-match-p url-string1 url-string2 s1 e1 s2 e2))))))

(defmethod local-context-match-p ((url-string1 string) (url2 url) &optional recache-p)
  (flet ((get-indices (url url-string recompute-p)
           (with-value-cached (url :local-context-indices :recompute-p recompute-p)
             (multiple-value-list
              (%local-context-string-indices url-string 0 (length url-string))))))
    (declare (inline get-indices))
    (multiple-value-bind (s1 e1)
        (%local-context-string-indices url-string1 0 (length url-string1) t)
      (when s1
        (let ((url-string2 (name-string url2)))
          (destructuring-bind (s2 e2)
              (get-indices url2 url-string2 recache-p)
            (%%%local-context-match-p url-string1 url-string2 s1 e1 s2 e2)))))))

(defmethod local-context-match-p ((url1 url) (url-string2 string) &optional recache-p)
  (flet ((get-indices (url url-string recompute-p)
           (with-value-cached (url :local-context-indices :recompute-p recompute-p)
             (multiple-value-list
              (%local-context-string-indices url-string 0 (length url-string))))))
    (multiple-value-bind (s2 e2)
        (%local-context-string-indices url-string2 0 (length url-string2) t)
      (when s2
        (let ((url-string1 (name-string url1)))
          (destructuring-bind (s1 e1)
              (get-indices url1 url-string1 recache-p)
            (%%%local-context-match-p url-string1 url-string2 s1 e1 s2 e2)))))))
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.437")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(SCL:FUNDEFINE 'COERCE-URL-STRING)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.437")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

;;;------------------------------------------------------------------- 
;;;
;;; COERCE-URL
;;;

;(defun coerce-url-string (url &optional (escape-search-url-p *escape-search-urls*)
;			      (downcase-p *downcase-url-strings*)
;			      &aux s)
;  "Returns a string for URL."
;  (declare (notinline))
;  (setq s (cond (escape-search-url-p
;                 (etypecase url
;                   (string (ensure-escaped-search-url url))
;                   (url (name-string url))))
;                (t (etypecase url
;                     (string url)
;		     (search-mixin (name-string-with-unescaped-search-suffix url))
;                     (url (name-string url))))))
;  (cond-every
;    (downcase-p
;      (setq s (string-downcase s))))
;  s)

(defparameter *relativize-urls* nil
  "When non-null URLs are relativized in code generating HTML.
This switch should be turned on when operating through reverse proxies
or gateways translating from a private network address. The possible
values are:

          NIL                    -- Emit fully qualified URLs
          :LOCAL-CONTEXT         -- Relativize all URLs matching the local server context
          <local-context-string> -- Relativize all URLs matching the local context string
          <local-context-list>   -- Relativize all URLs matching any local context ub the list.

See the function URL:COERCE-URL-STRING, which can be useful in application code.")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.438")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defgeneric coerce-url-string (url &optional escape-search-url-p relativize)
  (declare (values url-string))
  (:documentation "Coerces URL into the correct string according to the arguments.
When non-null RELATIVIZE causes all URLs to be converted into relative URLs with
the protocol, host, and port stripped off. If RELATIVIZE is :LOCAL-CONTEXT, only
URL matching (local-context) are relativized. If RELATIVIZE is a local context string,
then only those URLs matching it are relativized. If RELATIVIZE is a list of local
context strings, then only those URLs matching any one of them are relativized"))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.438")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod coerce-url-string ((url string) &optional (escape-search-url-p *escape-search-urls*)
                              (relativize *relativize-urls*))
  (macrolet ((setq-if (var value)
               `(let ((.val. ,value))
                  (when .val.
                    (setq ,var .val.)))))
    (flet ((maybe-relativize (context string)
             (let ((end (length string)))
               (multiple-value-bind (success-p start)
                   (%local-context-match-p context string 0 (length context) 0 end)
                 (when success-p
                   (subseq string start end))))))
      (let ((s (if escape-search-url-p (ensure-escaped-search-url url) url)))
        (etypecase relativize
          (null)				;no-op
          (symbol
	    (ecase relativize
	      (:local-context
		(setq-if s (maybe-relativize (http:local-context) s)))
	      ((t) (setq s (%relative-name-string s (length s) nil)))))
          (string
	    (setq-if s (maybe-relativize relativize s)))
          (cons
	    (loop for context in relativize
		  doing (when (setq-if s (maybe-relativize context s))
			  (return)))))
        s))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.438")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod coerce-url-string ((url url) &optional escape-search-url-p (relativize *relativize-urls*))
  (declare (ignore escape-search-url-p))
  (flet ((get-relative-url-based-on-context (context url)
           (if (local-context-match-p context url)
               (relative-name-string url)
	       (name-string url))))
    (declare (inline get-relative-url-based-on-context))
    (etypecase relativize
      (null (name-string url))
      (symbol
	(ecase relativize
	  (:local-context
	    (get-relative-url-based-on-context (http:local-context) url))
	  ((t) (relative-name-string url))))
      (string
	(get-relative-url-based-on-context relativize url))
      (cons
	(loop for context in relativize
	      when (local-context-match-p context url)
		return (relative-name-string url)
	      finally (return (name-string url)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.438")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod coerce-url-string ((url search-mixin) &optional (escape-search-url-p *escape-search-urls*)
			      (relativize *relativize-urls*))
  (flet ((get-relative-url-based-on-context (context url)
           (if (local-context-match-p context url)
               (relative-name-string-with-unescaped-search-suffix url)
	       (name-string-with-unescaped-search-suffix url))))
    (declare (inline get-relative-url-based-on-context))
    (cond (escape-search-url-p
	   (call-next-method))
	  (t (etypecase relativize
	       (null (name-string-with-unescaped-search-suffix url))
	       (symbol
		 (ecase relativize
		   (:local-context
		     (get-relative-url-based-on-context (http:local-context) url))
		   ((t) (relative-name-string-with-unescaped-search-suffix url))))
	       (string
		 (get-relative-url-based-on-context relativize url))
	       (cons
		 (loop for context in relativize
		       when (local-context-match-p context url)
			 return (relative-name-string-with-unescaped-search-suffix url)
		       finally (return (name-string-with-unescaped-search-suffix url)))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;PACKAGE.LISP.480")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: CL-USER; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (load)
(mapc #'(lambda (sym) (export (intern sym :url) :url)) '("*RELATIVIZE-URLS*" "COERCE-URL-STRING*")))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;NETSCAPE-1-1.LISP.129")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: netscape1.1; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun body-arguments (background-url background foreground link visited-link active-link)
  (macrolet ((collect (vars)
               (flet ((%body-arg-value (value url-ok-p)
                        (if url-ok-p
                            `(typecase ,value
                               (keyword (background-url ,value))
                               (t (url:coerce-url-string ,value nil)))
                            `(color-mapping ,value))))
                 `(let ((args nil))
                    (cond-every
                      ,.(loop for var in (reverse vars)
                              for url-ok-p = (eql var 'background-url)
                              collect `(,var
                                        (push (list ,(%body-arg-key var)
                                                    ,(%body-arg-value var url-ok-p))
                                              args))))
                    args))))
    
    (collect (background-url background foreground link visited-link active-link))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.529")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(define-macro with-local-context ((url) &body body)
  "Binds the local context to URL within the scope of BODY."
  `(let (*local-context*)
     (setf (local-context) ,url)
     ,@body))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.529")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defun set-local-context (url)
  (let ((context (url:local-context-string url)))
    (cond ((string-search= "http://" context 0 6 0 (length context))
	   (setq *local-context* context))
	  (t (error "Bad Syntax: Local context must start with http://host.domain/")))))

