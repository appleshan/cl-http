;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.28
;;; Reason: Provide content negotiation on directory views.
;;; 
;;; When http:*content-negotiate-directory-views* is non null, the server will
;;; provide alternate views when the client provides an accept header indicating
;;; that it accepts either text/uri-list or text/x-directory-list.
;;; 
;;; Variable HTTP::*CONTENT-NEGOTIATE-DIRECTORY-VIEWS*:  new variable.
;;; Function HTTP::WRITE-DIRECTORY-LISTING-AS-URI-LIST:  new.
;;; Function HTTP::WRITE-DIRECTORY-LISTING-AS-URI-PROPERTY-LIST:  new.
;;; Function HTTP::PARSE-NUMBER:  new.
;;; Function HTTP::NUMERIC-VALUE-P:  new.
;;; Function HTTP::PARSE-NUMBER:  new.
;;; Function HTTP::PARSE-MIME-HEADER-PARAMETERS:  handle quality values.
;;; Function HTTP::SORT-ACCEPT-HEADER-MEDIA-TYPES:  new.
;;; Function HTTP::SORT-PARSED-QUALITY-PAIRS:  new.
;;; Function HTTP::PARSE-QUALITY-VALUE:  handle absense of leading zero on fractional values.
;;; DEFINE-CONTENT-TYPE-NAME :URI-LIST:  new mime type.
;;; DEFINE-CONTENT-TYPE-NAME :DIRECTORY-LIST:  define.
;;; Function HTTP::GET-DIRECTORY-WRITER-FOR-MEDIA-TYPE:  new
;;; Function (CLOS:METHOD HTTP::GET-DIRECTORY-WRITER-ACCEPTABLE-TO-CLIENT (T T)):  new
;;; Function HTTP::WRITE-DIRECTORY-LISTING-ACCEPTABLE-TO-CLIENT:  new
;;; Function HTTP:WRITE-DIRECTORY-LISTING:  revise.
;;; Function HTTP::WRITE-DIRECTORY-LISTING-ACCEPTABLE-TO-CLIENT:  new.
;;; Function HTTP::%DEFINE-DIRECTORY-WRITE-METHOD:  revise.
;;; Written by JCMa, 3/14/00 19:30:27
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.27,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, HTTP Proxy Server 4.5,
;;; HTTP Client Substrate 3.3, Jcma 41, HTTP Client 49.2, Image Substrate 440.4,
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
  "HTTP:SERVER;HEADERS.LISP.447"
  "HTTP:SERVER;HEADERS.LISP.448"
  "HTTP:SERVER;HEADERS.LISP.449"
  "HTTP:SERVER;UTILS.LISP.430"
  "HTTP:SERVER;SERVER.LISP.824"
  "HTTP:SERVER;VARIABLES.LISP.182"
  "HTTP:SERVER;SERVER.LISP.825")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.447")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;;;------------------------------------------------------------------- 
;;;
;;; PARSING HEADER PARAMETERS
;;;

(defun numeric-value-p (string &optional (start 0) (end (length string)))
  "Returns non-null when STRING is a numeric value."
  (with-fast-array-references ((string string string))
    (loop for idx fixnum upfrom start below end
	  for char = (aref string idx)
	  unless (or (digit-char-p char 10)
		     (eql char #\.))
	    return nil
	  finally (return t))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.447")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun parse-number (string &optional (start 0) (end (length string)) (float-p nil float-supplied-p))
  "Parses a number which can be either and integer or a float."
  (if (or (and float-supplied-p float-p) (char-position #\. string start end))
      (parse-quality-value string start end)
      (parse-integer string :start start :end end :radix 10)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.448")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun parse-mime-header-parameters (string &optional (start 0) (end (length string)))
  (flet ((delimiter-p (char)
           (member char '(#\; #\space #\tab) :test #'eql))
	 (intern-value (string s e)
	   (declare (fixnum s e))
	   (with-fast-array-references ((string string string))
	     (loop with quality-value-p
		   for idx fixnum upfrom s below e
		   for ch = (aref string idx)
		   unless (or (digit-char-p ch)
			      (setq quality-value-p (eql ch #\.)))
		     return (%tokenize-header-keyword string s e)
		   finally (return (parse-number string s e quality-value-p))))))
    (declare (inline intern-value))
    (loop for s = (position-if-not* #'delimiter-p string :start start :end end) then (the fixnum (1+ idx))
	  while (and s (< s end))
          for idx fixnum = (or (char-position #\; string (1+ s) end) end)
          for delim fixnum = (char-position #\= string (1+ s) idx)
          for p1 = (position-valid-mime-char string (1+ delim) idx)
          for p2 = (position-if* #'mime-valid-char-for-token-p string :start p1 :end idx :from-end t)
          for param-value = (and p2 (intern-value string p1 (1+ (the fixnum p2))))
          when param-value
            collect (%tokenize-header-keyword string (position-valid-mime-char string s delim) delim)
            and collect param-value)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.449")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun sort-accept-header-media-types (parsed-values)
  "Sorts PARSED-VALUES of an ACCEPT header according to quality values and media type generality."
  (flet ((media-type-more-acceptable-p (x y)
	   (destructuring-bind (x-major x-minor . x-params) x
	     (destructuring-bind (y-major y-minor . y-params) y
	       (let ((qx (getf x-params :q 1))
		     (qy (getf y-params :q 1)))
		 (or (and (numberp qx) (numberp qy) (> qx qy))
		     (cond ((eq x-major y-major)
			    (cond ((eq x-minor y-minor)	;handle parameter-based sorting here
				   (let ((lx (length x-params))	;Simple parameter length-based hack to handle specificity
					 (ly (length y-params)))
				     (cond ((= lx ly) nil)
					   ((> (if (getf x-params :q) (1- lx) lx)
					       (if (getf y-params :q) (1- ly) ly)))
					   (t nil))))
				  ((eq x-minor :*) nil)
				  ((eq y-minor :*) t)
				  (t nil)))
			   ((eq x-major :*) nil)
			   (T t))))))))
    (stable-sort parsed-values #'media-type-more-acceptable-p)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.449")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun sort-parsed-quality-pairs (parsed-values)
  "Sorts PARSED-VALUES of a header according to quality values."
  (stable-sort parsed-values #'> :key #'cdr))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.430")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(define parse-quality-value (string &optional (start 0) (end (length string)))
  "Read a qvalue from STRING starting at START.
Returns the qvalue as a floating point number (between 0.0 and 1.0) 
and the new value of START.  Any parsing error returns 0.0."
  (declare (values quality-value next-index)
           (fixnum start end))
  (let((n 0)
       (scale 1))
    (declare (fixnum n scale))
    (macrolet ((scale ()
                 `(setq n (%+ (%* n 10) it)
                        scale (%* scale 10))))
      (with-string-parsing-operators
        (is #\0
            (is #\. (opt (digit-char-p) (scale) 
                         (opt (digit-char-p) (scale) 
                              (opt (digit-char-p) (scale)
                                   (values (float (/ n scale)) start))))
                (values 0.0 start))
            (is #\1
                (is #\. (opt #\0 (opt #\0 (opt #\0 (values 1.0 start))))
                    (values 1.0 start))
		(is #\. (opt (digit-char-p) (scale) 
			     (opt (digit-char-p) (scale) 
				  (opt (digit-char-p) (scale)
				       (values (float (/ n scale)) start))))
		    (values 0.0 start))))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.824")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun write-directory-listing (url stream inclusion-predicate path-url-intern-function &optional directories-p)
  (flet ((anchor-text (url pathname directory-file-p)
	   (cond (directory-file-p
		  (when directories-p
		    (url:path-most-specific-name url)))
		 (t (with-value-cached (url :directory-string)
		      (let ((name (url:object url))
			    (type (pathname-type pathname))
			    (version (pathname-version pathname)))
			(declare (dynamic-extent name))
			(typecase version
			  ((or keyword null) (concatenate 'string name "." type))
			  (t (concatenate 'string name "." type "." (write-to-string version :base 10. :escape nil))))))))))
    (declare (dynamic-extent #'anchor-text))
    (multiple-value-bind (user-agent version)
	(current-user-agent)
      (let ((tables-aware-p (user-agent-capability-p :tables user-agent version)))
	(with-directory-index-caching (url stream (if tables-aware-p :directory-list-html3 :directory-list-html2)
					   :last-modification-function 'file-modification-date) 
	  (funcall (if tables-aware-p #'%write-directory-listing-html3 #'%write-directory-listing-html2)
		   url stream inclusion-predicate path-url-intern-function #'anchor-text #'url:path-directory-string 
		   directories-p :definition t))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.824")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(eval-when (:compile-toplevel :execute :load-toplevel)
(defun %define-directory-write-method (export-type data-types &optional directories-p)
  (check-type export-type keyword)
  (check-type data-types cons)
  (unless (every #'url:data-type-keyword-p data-types)
    (error "An unknown data-type was supplied in DATA-TYPES, ~S" data-types))
  `(defmethod write-document ((url url:http-path) (translation (eql ,export-type)) stream)
     (flet ((intern-path-url (path url)
	      (url:intern-pathname-as-url-inferior path url :if-does-not-exist :create))
	    (inclusion-predicate (pathname)
	      ,(cond (data-types 
		      `(%make-data-type-pathname-predicate pathname ,data-types ,directories-p))
		     (t t))))
       (declare (dynamic-extent #'inclusion-predicate))
       (write-directory-listing-acceptable-to-client url stream #'inclusion-predicate #'intern-path-url ,directories-p)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.824")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(define-standard-directory-export-types (:html :text :lisp :image :audio :video :application :world))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.824")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

;; Remember the standard directory cache keys.
(note-directory-index-cache-keys :directory-list-html3 :directory-list-html2 :uri-list :directory-plist)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.824")
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
	       (write-string (name-string url-inf) stream)))))
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
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.824")
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
			      (name-string url-inf)
			      (cond-every
				(directory (fast-format stream " :directory t"))
				(length-in-bytes (fast-format stream " :length-in-bytes ~D" length-in-bytes))
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
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.824")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

;; Remember the standard directory cache keys.
(note-directory-index-cache-keys :directory-list-html3 :directory-list-html2 :uri-list :uri-property-list)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.824")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(define-content-type-name :uri-list :text :uri-list)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.824")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(define-content-type-name :directory-list :text :x-directory-list)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.824")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun write-directory-listing-acceptable-to-client (url stream inclusion-predicate path-url-intern-function directories-p)
  (multiple-value-bind (writer data-type)
      (get-directory-writer-acceptable-to-client *server* url)
    (with-conditional-get-response (stream data-type :last-modification (file-modification-date (url::cached-pathname url))
					   :expires (url:expiration-universal-time url) :content-location url
					   :cache-control (url:response-cache-control-directives url)
					   :content-language (languages url))
      (funcall writer url stream inclusion-predicate path-url-intern-function directories-p))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.824")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defparameter *directory-writer-alist* '((:text
					   (:html (() write-directory-listing :html))
					   (:uri-list (() write-directory-listing-as-uri-list :uri-list))
					   (:x-directory-list (() write-directory-listing-as-uri-property-list :directory-list)))
					 (:* (:* (() write-directory-listing :html)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.824")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defparameter *default-directory-writer* '(write-directory-listing :html)
  "The default directory writer and its data-type keyword.")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.824")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun get-directory-writer-for-media-type (media-type)
  (flet ((match-parameters-p (request-parameters writer-parameters)
	   (loop for (key value) on request-parameters by #'cddr
		 unless (or (eq key :q)		;skip quality values in request
			    (equalp value (getf writer-parameters key :+not-found+)))
		   return nil
		 finally (return t))))
    (declare (inline match-parameters-p))
    (destructuring-bind (major minor . param-plist) media-type
      (let* ((major-entry (assoc major *directory-writer-alist*))
	     (minor-entry (and major-entry (assoc minor (cdr major-entry)))))
	(if minor-entry
	    (loop for param-entry in (cdr minor-entry)
		  when (match-parameters-p param-plist (first param-entry))
		    return (cdr param-entry)
		  finally (return nil))
	    nil)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;VARIABLES.LISP.182")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp;-*-")

(defparameter *content-negotiate-directory-views* nil
  "Controls whether HTTP clients may specify the desired directory view
by providing preferred media types in the  accepts header.")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;VARIABLES.LISP.182")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp;-*-")

(export (intern "*CONTENT-NEGOTIATE-DIRECTORY-VIEWS*" :http) :http)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.825")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod get-directory-writer-acceptable-to-client (server url)
  (when *content-negotiate-directory-views*
    (with-header-values (accept) (server-headers server)
      (when accept
	(loop for media-type in (sort-accept-header-media-types accept)
	      for write-spec = (get-directory-writer-for-media-type media-type)
	      when write-spec
		do (return-from get-directory-writer-acceptable-to-client (values-list write-spec))
	      finally (error 'acceptable-resource-not-found :headers `(:accept ., accept) :method :get :url url)))))
  (values-list *default-directory-writer*))
