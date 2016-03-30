;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.184
;;; Reason: DEFINE-HEADER-TYPE :QUALITY-PAIR-SEQUENCE-HEADER:  quality values are optional in modern standard.
;;; DEFINE-HEADER :ACCEPT-CHARSET:  update.
;;; DEFINE-HEADER :ACCEPT-ENCODING:  -
;;; DEFINE-HEADER :ACCEPT-LANGUAGE:  -
;;; DEFINE-HEADER :TE:  -
;;; Function URL::WITH-VALUE-CACHED:  install new regime for clearing cached values.
;;; Function (CLOS:METHOD URL:RELATIVE-NAME-STRING (URL:URL)):  -
;;; Function (CLOS:METHOD URL:RELATIVE-PATH-STRING (URL:URL)):  -
;;; Function (CLOS:METHOD URL:LOCAL-CONTEXT-STRING (URL::HOST-PORT-MIXIN)):  -
;;; Function (CLOS:METHOD URL::LOCAL-CONTEXT-MATCH-P (URL:URL URL:URL)):  -
;;; Function (CLOS:METHOD URL::LOCAL-CONTEXT-MATCH-P (STRING URL:URL)):  -
;;; Function (CLOS:METHOD URL::LOCAL-CONTEXT-MATCH-P (URL:URL STRING)):  -
;;; Function (CLOS:METHOD URL:DIRECTORY-CONTEXT-STRING (URL:HTTP-URL)):  -
;;; Function (CLOS:METHOD URL:ROOT-CONTEXT-STRING (URL:HTTP-URL)):  -
;;; Function (CLOS:METHOD URL:PATH-DIRECTORY-STRING (URL:HTTP-PATH)):  -
;;; Function (CLOS:METHOD URL:DIRECTORY-NAME-STRING (URL::PATH-MIXIN)):  -
;;; Function (CLOS:METHOD URL:DIRECTORY-NAME-STRING (URL::FILE-URL-PATH-MIXIN)):  -
;;; Function (CLOS:METHOD URL::NAME-STRING-WITH-UNESCAPED-SEARCH-SUFFIX (URL::SEARCH-MIXIN)):  -
;;; Function (CLOS:METHOD URL::RELATIVE-NAME-STRING-WITH-UNESCAPED-SEARCH-SUFFIX (URL::SEARCH-MIXIN)):  -
;;; Function (CLOS:METHOD URL:SEARCH-PARENT (URL::SEARCH-MIXIN)):  -
;;; Function (CLOS:METHOD URL:RESPONSE-CACHE-CONTROL-DIRECTIVES (URL:HTTP-CACHE-CONTROL-MIXIN)):  -
;;; Function (CLOS:METHOD URL:GET-IMAGE-SIZE (URL:HTTP-OBJECT T) :AROUND):  -
;;; Function (CLOS:METHOD WWW-UTILS:FILE-LENGTH-IN-BYTES (URL::PATHNAME-CACHING-MIXIN)):  -
;;; Function (CLOS:METHOD URL:EMAIL-ADDRESS (URL:MAILTO-URL)):  -
;;; Function (CLOS:METHOD URL:FULL-EMAIL-ADDRESS (URL:MAIL-ADDRESS)):  -
;;; -
;;; Function (CLOS:METHOD URL:NAME-STRING (URL:URL)):  use new decaching regime.
;;; Function (CLOS:METHOD URL:CHANGE-HOST (URL::HOST-MIXIN STRING LISP:INTEGER)):  ditto.
;;; Function (CLOS:METHOD URL::NAME-STRING-WITH-UNESCAPED-SEARCH-SUFFIX (URL::SEARCH-MIXIN)):  keywordize cache key
;;; Function (CLOS:METHOD URL::RELATIVE-NAME-STRING-WITH-UNESCAPED-SEARCH-SUFFIX (URL::SEARCH-MIXIN)):  ditto
;;; Function HTTP::MAP-VIRTUAL-HOSTS:  new
;;; Function HTTP::MOVE-SERVER-TO-NEW-HOST:  new function enables mobile computers
;;; to change URLs and reiniatialize without reloading and restarting.
;;; Function WWW-UTILS::%LOCAL-HOST-DOMAIN-NAME:  -
;;; Function WWW-UTILS:LOCAL-HOST-DOMAIN-NAME:  use new abstraction.
;;; Function HTTP::MAYBE-MOVE-SERVER-IP-ADDRESS:  new.
;;; Written by JCMa, 11/13/03 20:33:35
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5-inc1.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.6,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.183,
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
;;; 1137x679 1-bit STATIC-GRAY X Screen INTERNET|128.52.30.16:0.0 with 224 Genera fonts (eXodus 8.0  (c) 2001 Powerlan USA,
;;; Inc. R7100),
;;; 1152x696 24-bit TRUE-COLOR X Screen FUJI:2.0 with 224 Genera fonts (The Olivetti & Oracle Research Laboratory R3323),
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
  "HTTP:SERVER;HEADERS.LISP.533"
  "HTTP:SERVER;URL.LISP.444"
  "HTTP:LISPM;SERVER;LISPM.LISP.521"
  "HTTP:SERVER;UTILS.LISP.546"
  "HTTP:SERVER;UTILS.LISP.545")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.533")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;; Updated to modern spec, in which quality values are optional. 11/13/2003 -- JCMa.
(define-header-type :quality-pair-sequence-header (:comma-separated-header)
  :parse-function parse-comma-separated-quality-pairs
  :print-function print-comma-separated-quality-pair-or-tokens
  :print-series-predicate list-valued-header-series-value-p)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.533")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :accept-charset
               (:quality-pair-sequence-header :request)
  :print-string "Accept-Charset")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.533")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :accept-encoding
               (:quality-pair-sequence-header :request)
  :print-string "Accept-Encoding")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.533")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :accept-language
               (:quality-pair-sequence-header :request)
  :print-string "Accept-Language")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.533")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :te
               (:quality-pair-sequence-header :request)
               :print-string "TE")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.533")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header-keywords "gzip")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.444")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(PROGN
(eval-when (:compile-toplevel :execute)
(defvar *url-cache-value-keys* nil))

(eval-when (:load-toplevel)
(defvar *url-cache-value-keys*))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun %note-url-cache-key (key)
  " Remembers KEY used to cache values on URL PLISTs."
  (flet ((coerce-key (key)
           (etypecase key
             (keyword key)
             (cons
              (destructuring-bind (fctn . args) key
                (ecase fctn
                  (quote (car args))))))))
    (pushnew (coerce-key key) *url-cache-value-keys*)))

(defmacro remember-url-value-cache-keys ()
 `(setq *url-cache-value-keys* ',(sort *url-cache-value-keys*  #'string< :key #'symbol-name))))

(define-generic clear-cached-values (uri)
  (:documentation "Clears any cached values for URI."))

(defmethod clear-cached-values ((uri uri))
  (loop with plist = (property-list uri)
        for ptr = plist then (cddr ptr)
        while ptr
        do (let ((entry (cddr ptr)))
             (when (member (first entry) *url-cache-value-keys*)
               (setf (cddr ptr) (cddr entry))))
        finally (return (if (member (first plist) *url-cache-value-keys*)
                            (setf (property-list uri) (cddr plist))
                          plist))))

;; recompile all callers in http:server;url.lisp
(define-macro with-value-cached ((url key &key (recompute-p nil recompute-supplied-p)) &body value-form)
  "Caches the value returned by value-form on URL's property list under the indicator KEY.
When RECOMPUTE-P is non-null, the value is recomputed and recached.
The returned values are VALUE and RETRIEVE-FROM-CACHE-P."
  (declare (values retrieved-from-cache-p))
  (let ((form `(let ((val (getf plist ,key :+not-found+)))
                 (case val
                   (:+not-found+ 
                    (setf (getf plist ,key) (progn  . ,value-form)))
                   (t (values val t))))))
    (%note-url-cache-key key) ;remember the cache keys
    (cond (recompute-supplied-p
           `(with-slots (plist) ,url
              (cond (,recompute-p
                     (setf (getf plist ,key) (progn . ,value-form)))
                    (t ,form))))
          (t `(with-slots (plist),url ,form)))))
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.444")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod relative-name-string ((url url) &optional compute-p)
  (with-value-cached (url :relative-name :recompute-p compute-p)
    (let* ((name (name-string url compute-p))
           (length (length name)))
      (%relative-name-string name length t))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.444")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod relative-path-string ((url url) &optional compute-p)
  (with-value-cached (url :relative-path :recompute-p compute-p)
    (let* ((name (name-string url compute-p))
           (length (length name)))
      (%relative-path-string name length))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.444")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod local-context-string ((url host-port-mixin) &optional recompute-p)
  (with-value-cached (url :local-context-string :recompute-p recompute-p)
    (%local-context-string (name-string url recompute-p))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.444")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

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


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.444")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

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


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.444")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

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


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.444")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod directory-context-string ((url url:http-url) &optional recompute-p)
  (with-value-cached (url :directory-context-string :recompute-p recompute-p)
    (with-output-to-string (stream)
      (write-scheme-prefix url stream)
      (write-host-port-string url stream)
      (write-path url stream))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.444")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod root-context-string ((url url:http-url) &optional recompute-p)
  (with-value-cached (url :root-context-string :recompute-p recompute-p)
    (with-output-to-string (stream)
      (write-scheme-prefix url stream)
      (write-host-port-string url stream)
      (write-char #\/ stream))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.444")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod path-directory-string ((url url:http-path) &optional recompute-p)
  (with-value-cached (url :directory-string :recompute-p recompute-p)
    (format nil "/~{~A/~}" (path url))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.444")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod directory-name-string ((url path-mixin) &optional recompute-p)
  (with-value-cached (url :directory-name-string :recompute-p recompute-p)
    (with-output-to-string (stream)
      (write-scheme-prefix url stream)
      (write-host-port-string url stream)
      (write-path url stream)
      (write-char #\/ stream))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.444")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod directory-name-string ((url file-url-path-mixin) &optional recompute-p)
  (with-value-cached (url :directory-name-string :recompute-p recompute-p)
    (with-output-to-string (stream)
      (write-scheme-prefix url stream)
      (write-host-port-string url stream)
      (write-char #\/ stream)
      (write-path url stream))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.444")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

;(defmethod search-parent ((url search-mixin) &key (lexical-parent t) (if-does-not-exist :create) &aux pos)
;  (with-slots (name-string) url
;    (cond ((and (null lexical-parent) *url-search-parent*))
;          ((setq pos (position *search-url-delimiter* name-string :test #'char=))
;           (with-value-cached (url :search-parent)
;             (intern-url name-string :start 0 :end (the fixnum (1+ pos)) :if-does-not-exist if-does-not-exist)))
;          ((eq if-does-not-exist :soft) nil)
;          (t (error 'no-search-parent-found :url url)))))

(defmethod search-parent ((url search-mixin) &key (lexical-parent t) (if-does-not-exist :create) recache-p &aux pos)
  (with-slots (name-string) url
    (cond ((and (null lexical-parent) *url-search-parent*))
          ((and (null recache-p) (%search-parent url)))
          ((setq pos (char-position *search-url-delimiter* name-string))
           (setf (%search-parent url) (intern-url name-string :start 0 :end (the fixnum (1+ pos))
                                                  :if-does-not-exist if-does-not-exist)))
          ((eq if-does-not-exist :soft) nil)
          (t (error 'no-search-parent-found :url url)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.444")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod response-cache-control-directives ((url http-cache-control-mixin))
  (flet ((proxy-revalidation-required-p (directives)
	   (loop for (keyword value) on directives by #'cddr
		 when (and (member keyword '(:proxy-revalidate :must-revalidate))
			   value)
		   return nil			; already specified don't need it again.
		 finally (return (or *secure-subnets*
				     (secure-subnets url)
				     (http:server-user-object *server*))))))
    (declare (inline proxy-revalidation-required-p))
    (let ((directives (cache-control-directives url))
	  (max-age (max-age-seconds url)))
      (cond-every 
	;; Extend directives first time when computed from expiration function
	;; must precede revalidation check.
	((and max-age (not (getf directives :max-age)))
	 (setf (cache-control-directives url) `(:max-age ,max-age ,. directives)))
	;; Require revalidation when security present
	((proxy-revalidation-required-p directives)
	 (setq directives (with-value-cached (url :proxy-revalidate-directives)
			    `(:proxy-revalidate t ,.directives))))
	;; update max when a new time is computed
	(max-age
	  (setf (getf directives :max-age) max-age)))
      directives)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.444")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod get-image-size :around ((url http-object) translation-method &optional recompute-p)
  (flet ((report-an-error (url error)
	   (let ((error-type (type-of error)))
	     (report-bug http:*bug-http-server* (format nil "Image Size Error: ~S" error-type)
			 "~&URL: ~S~&Pathname: ~A~&Format: ~A~&Function: ~S~&Error: ~S~:[~;~&Error Report: ~:*~A~]"
			 (name-string url) (cached-pathname url) translation-method 'get-image-size
			 error-type (report-string error)))))
    (values-list
      (with-value-cached (url :image-size :recompute-p recompute-p)
	(multiple-value-bind (width height match-image-type-p)
	    (handler-case
	      (call-next-method url translation-method recompute-p)
	      (error (err) (report-an-error url err) nil))
	  (when (and width height)
	    (list width height match-image-type-p)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.444")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod file-length-in-bytes ((url pathname-caching-mixin) &optional recompute-p)
  (with-slots (pathname) url
    (with-value-cached (url :file-size :recompute-p recompute-p)
      (when (and pathname (not (pathname-directory-p pathname)))
        (file-length-in-bytes pathname recompute-p)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.444")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod email-address ((mail-address mailto-url) &optional recompute-p)
  (with-slots (user-id host-string) mail-address
    (with-value-cached (mail-address :email-address :recompute-p recompute-p)
      (%make-email-address user-id host-string))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.444")
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
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.444")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod name-string ((url url) &optional compute-p)
  (with-slots (name-string) url
    (cond ((or (null name-string) compute-p)
           (prog1 (setq name-string (compute-name-string url))
		  (when compute-p
		    (clear-cached-values url))))
          (t name-string))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.444")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod change-host ((url host-mixin) (host-name string) (port integer))
  (unregister url)
  ;; Remove any cached values the refer to the old host or port
  (clear-cached-values url)
  (setf (host-string url) host-name
        (port url) (if (and (= port 80) (typep url 'http-url)) nil port)
        (%host-object url) nil)
  (name-string url t)
  (register url))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.444")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod name-string-with-unescaped-search-suffix ((url search-mixin) &optional recache-p)
  (with-slots (name-string) url
    (with-value-cached (url :name-string-with-unescaped-search-suffix :recompute-p recache-p)
      (%url-string-with-unescaped-search-suffix name-string))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.444")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod relative-name-string-with-unescaped-search-suffix ((url search-mixin) &optional recache-p)
  (with-value-cached (url :relative-name-string-with-unescaped-search-suffix :recompute-p recache-p)
    (%url-string-with-unescaped-search-suffix (relative-name-string url))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.444")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(eval-when (:load-toplevel)
  (remember-url-value-cache-keys))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.545")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defun map-virtual-hosts (function)
  "Maps FUNCTION over entries for virtual hosts.
FUNCTION is called with (host port local-context)"
  (flet ((fctn (host entry)
           (loop for (port . local-context) in entry
                 do (funcall function host port local-context))))
    (declare (dynamic-extent #'fctn))
    (maphash #'fctn *virtual-host-table*)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.545")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defun move-server-to-new-host (host &key (port *standard-http-port*)
                                     (old-host (local-host-domain-name))
                                     (old-port *standard-http-port*)
                                     (reinitialize-p t)
                                     (report-stream *standard-output*) 
                                     &aux virtual-host-entries)
  "Remaps host and port attributes for a running server for mobile servers 
on portable computers.  URLs and virtual hosts from OLD-HOST
and PORT to new HOST and PORT.  When REINITIALIZE-P is non-null, this
reinitializes the server variables and runs the server initializations
and launch initializations."
  (flet ((collect-virtual-host-mappings ()
           (let ((e1 (length old-host)))
             (flet ((capture-entry (vhost vport local-context)
                      (flet ((maybe-remap-local-context (local-context)
                               (let* ((start 8) ;; assume (length "http://")
                                      (end (length local-context))
                                      (pos-colon (char-position #\: local-context 8 end t)))
                                 (cond ((and (%string-equal old-host local-context 0 e1 start (or pos-colon end))
                                             (eql old-port (if pos-colon (parse-integer local-context :start (1+ pos-colon) :end end) 80)))
                                        (%make-context host port))
                                       (t local-context)))))
                        (declare (dynamic-extent #'maybe-remap-local-context))
                        (push (list* vhost vport (maybe-remap-local-context local-context)) virtual-host-entries))))
               (declare (dynamic-extent #'capture-entry))
               (map-virtual-hosts #'capture-entry))))
         (reinstate-virtual-hosts (virtual-host-entries)
           (loop for (vhost vport . local-context) in virtual-host-entries
                 do (add-virtual-host-nick-name vhost vport local-context))))
    (declare (dynamic-extent #'collect-virtual-host-mappings))
    (when reinitialize-p 
      (disable-http-service)
      (run-server-initializations t))
    (collect-virtual-host-mappings)
    (url:remap-url-host old-host host :old-port old-port :new-port port :report-stream report-stream)
    (when reinitialize-p
      (run-server-launch-initializations t)
      (reinstate-virtual-hosts virtual-host-entries)
      (enable-http-service))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.521")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(declaim (inline %local-host-domain-name))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.521")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(define %local-host-domain-name ()
  (host-mail-name neti:*local-host*))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.521")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(define local-host-domain-name (&optional recache-p)
  "Returns the local host domain name."
  (cond ((and (not recache-p) http:*local-host-domain-name*))
        (t (setq http:*local-host-domain-name*
                 (or http:*http-host-name* (%local-host-domain-name))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.521")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(PROGN
(declaim (inline %local-host-ip-address))

(define %local-host-ip-address ()
  (host-internet-address neti:*local-host*))

(define local-host-ip-address-moved-p ()
  "Returns non-null if the local host IP address has changed."
  (not (equal http:*local-host-ip-address* (%local-host-ip-address))))

(define local-host-ip-address (&optional recache-p)
  "Returns the IP address of the local host."
  (cond ((and (not recache-p) http:*local-host-ip-address*))
        (t (setq http:*local-host-ip-address* (%local-host-ip-address)))))
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.546")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defun maybe-move-server-ip-address (&key (report-stream *standard-output*))
  (when (www-utils::local-host-ip-address-moved-p)
    (move-server-to-new-host (www-utils::%local-host-domain-name)
                             :old-host (local-host-domain-name)
                             :reinitialize-p t
                             :report-stream report-stream)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.545")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(eval-when (load)
  (export (intern "MOVE-SERVER-TO-NEW-HOST" :http) :http)
  (export (intern "LOCAL-HOST-IP-ADDRESS-MOVED-P" :www-utils) :www-utils)
  (export (intern "%LOCAL-HOST-DOMAIN-NAME" :www-utils) :www-utils))
