;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.68
;;; Reason: Function (DEFUN-IN-FLAVOR TCP::%NOTE-CHUNK-START TCP::CHUNK-TRANSFER-DECODING-INPUT-STREAM-MIXIN):  
;;; Discard input buffer when no data remains after reading last chunk size & headers.
;;; 
;;; Provide a means to reduce multiple valued atomic headers.
;;; 
;;; Function HTTP::%HEADER-ATOMIC-VALUE-REDUCER:  -
;;; SETF function HTTP::%HEADER-ATOMIC-VALUE-REDUCER:  -
;;; Function HTTP::%DEFINE-HEADER-TYPE:  add atomic-value-reducer.
;;; Function HTTP::DEFINE-HEADER-TYPE:  add atomic-value-reducer
;;; Function HTTP::%DEFINE-HEADER:  -
;;; Function HTTP::DEFINE-HEADER:  -
;;; Function (CLOS:METHOD HTTP:HEADER-VALUE (HTTP::BUFFERED-HEADER) :AROUND):  handle atomic value reduction
;;; Function (CLOS:METHOD HTTP:HEADER-VALUE (HTTP::HEADER)):  -
;;; Function HTTP::HEADER-VALUE-MAX:  -
;;; Function HTTP::HEADER-VALUE-MIN:  -
;;; 
;;; Apply it to date headers.
;;; 
;;; DEFINE-HEADER-TYPE :DATE-HEADER:  Default to first
;;; DEFINE-HEADER :DATE:  reduce multiple values by max
;;; DEFINE-HEADER :EXPIRES:  reduce multiple values by min
;;; DEFINE-HEADER :IF-MODIFIED-SINCE:  reduce by min.
;;; DEFINE-HEADER :IF-UNMODIFIED-SINCE: reduce by min. 
;;; DEFINE-HEADER :LAST-MODIFIED:  reduce by min.
;;; Written by JCMa, 9/06/00 14:43:02
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.67,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42, HTTP Proxy Server 5.20,
;;; HTTP Client Substrate 3.15, HTTP Client 49.8, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, W4 Constraint-Guide Web Walker 45.3,
;;; W4 Examples 15.0, Ivory Revision 5, VLM Debugger 329, Genera program 8.11,
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
  "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.192"
  "HTTP:SERVER;HEADERS.LISP.469"
  "HTTP:SERVER;HEADERS.LISP.470")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.192")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

#+Genera
(defun-in-flavor (%note-chunk-start chunk-transfer-decoding-input-stream-mixin) ()
  (multiple-value-bind (chunk-size chunk-args)
      (%read-chunk-size)
    (declare (ignore chunk-args))
    (cond ((zerop chunk-size)
	   ;;(break "foo")
	   (setq input-chunk-size 0
		 at-end si:stream-input-index)	;pretend last ascii translation window ended here to start the right place.              
	   ;; Discard input buffer when all input has been used to avoid danger of reading past the end 9/6/2000 -- JCMa.
	     (when (and si::stream-input-buffer (not (< si:stream-input-index si:stream-input-limit)))
	       (send self :discard-current-input-buffer))
	   (signal 'end-of-chunk-transfer-decoding :stream self))
	  (t (setq input-chunk-size chunk-size
		   at-end si:stream-input-index	;pretend last ascii translation window ended here to start the right place.
		   input-scan-start si:stream-input-index
		   input-scan-end (min (+ si:stream-input-index input-chunk-size) si:stream-input-limit)
		   input-scan-length (- input-scan-end input-scan-start)
		   input-content-length (+ input-content-length input-chunk-size)
		   input-chunk-content-length input-scan-length
		   input-chunk-crosses-buffer-p (> input-chunk-size input-chunk-content-length)
		   input-buffer-limit si:stream-input-limit
		   si:stream-input-limit input-scan-end)
	     (with-chunk-transfer-decoding-traced
	       (format *trace-output* "~&~'bScan-Start:~ ~D ~'bScan-End:~ ~D  ~:[~;~'bCross-buffer:~ yes~]~&"
		       input-scan-start input-scan-end input-chunk-crosses-buffer-p))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.469")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;; Reduces a series header value to an atomic value when multiple values are
;; encountered during parsing.
(define %header-atomic-value-reducer (header)
  (get header 'header-atomic-value-reducer))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.469")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defsetf %header-atomic-value-reducer (header) (atomic-value-reducer)
  `(setf (get ,header 'header-atomic-value-reducer) ,atomic-value-reducer))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.469")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;;;------------------------------------------------------------------- 
;;;
;;; DEFINITION FUNCTIONS
;;;

(defun %define-header-type (header-type supertype parse-function print-function print-series-predicate atomic-value-reducer)
  (check-type header-type keyword)
  (check-type supertype keyword)
  ;; make sure there is no loop at the top of the header type pyramid.
  (if (eq header-type supertype)
      (remprop header-type 'header-supertype)
      (setf (%header-supertype header-type) supertype))
  ;; make this point to the function eventually
  (setf (%header-parse-function header-type) (when parse-function (fdefinition parse-function))
        (%header-print-function header-type) (when print-function (fdefinition print-function))
	(%header-print-series-predicate header-type) (when print-series-predicate (fdefinition print-series-predicate))
	(%header-atomic-value-reducer header-type) (when atomic-value-reducer (fdefinition atomic-value-reducer)))
  header-type)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.469")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-macro define-header-type (header-type (supertype)
                                              &key (parse-function 'parse-standard-header)
                                              (print-function 'print-standard-header)
					      print-series-predicate
					      atomic-value-reducer)
  "Top-level function for defining a header type."
  `(%define-header-type
     ',header-type ',supertype ',parse-function ',print-function ',print-series-predicate ',atomic-value-reducer))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.469")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun %define-header (header header-type header-class &optional print-name default parse-function print-function
			      print-series-predicate atomic-value-reducer)
  (check-type header keyword)
  (check-type header-type keyword)
  (macrolet ((update-header-function (local-arg accessor path)
	       `(let ((function (if ,local-arg (fdefinition ,local-arg) (%inherit-header-function header ',path nil))))
		  (if function
		      (setf (,accessor header) function)
		      (remprop header ',path)))))
    (let ((psym (intern (or print-name (symbol-name header)) *keyword-package*)))
      (setf (%header-supertype header) header-type
	    (%header-print-name header) psym
	    (%header-keyword psym) header
	    (%header-parse-function header) (if parse-function
						(fdefinition parse-function) 
						(%inherit-header-function header 'header-parse-function))
	    (%header-print-function header) (if print-function
						(fdefinition print-function)
						(%inherit-header-function header 'header-print-function))
	    (%header-superclass header) (or header-class :unknown))
      (update-header-function print-series-predicate %header-print-series-predicate header-print-series-predicate)
      (update-header-function atomic-value-reducer %header-atomic-value-reducer header-atomic-value-reducer)
      (if default
	  (setf (%header-default-value header) default)
	  (remprop header 'header-default))
      (%tokenize-header (symbol-name psym))	;update the tokenizer
      header)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.469")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-macro define-header (header (header-type &optional header-class) &key print-string default
                                    parse-function print-function print-series-predicate atomic-value-reducer)
  "Top-level function for defining a header."
  `(%define-header ,header ,header-type ,header-class ,print-string ,default
                   ,parse-function ,print-function ,print-series-predicate ,atomic-value-reducer))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.469")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod header-value :around ((header buffered-header))
  (flet ((parse-header (keyword string current)
	   (let ((header-parser (%header-parse-function keyword)))
	     (cond ((header-position-next current)
		    (loop with series-p = (%header-print-series-predicate keyword)
			  for val = (loop with start = (header-position-start current)
					  for end = (header-position-end current)
					  do (setq current (header-position-next current))
					  while (and current
						     (white-space-sequence-p string end (header-position-start current)))
					  finally (return (unless (= start end)
							    (funcall header-parser string start end))))
			  when series-p
			    nconc val into result
			  else collect val into result
			  while current
			  finally (return (cond (series-p result)
						((cdr result)
						 (let ((value-reducer (%header-atomic-value-reducer keyword)))
						   (if value-reducer
						       (funcall value-reducer result)
						       result)))
						(t (car result))))))
		   (t (funcall header-parser string (header-position-start current) (header-position-end current)))))))
    (declare (inline parse-header))
    (cond ((slot-boundp header 'value) (%header-value header))
	  ((%header-buffer header)
	   (setf (%header-value header) (parse-header (header-keyword header) (%header-buffer header) (%header-raw-value-position header))))
	  ;; let the base method do the job
	  (t (call-next-method)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.469")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod header-value ((header header))
  (flet ((parse-header (keyword raw-value)
	   (let ((parser (%header-parse-function keyword)))
	     (cond ((cdr raw-value)		;ignore null strings as header values in order to improve parsing robustness 
		    (loop with series-p = (%header-print-series-predicate keyword)
			  for val in raw-value
			  unless (null-string-p val)
			    when series-p
			      nconc (funcall parser val) into result
			  else
			    collect (funcall parser val) into result
			  finally (return (cond (series-p result)
						((cdr result)
						 (let ((value-reducer (%header-atomic-value-reducer keyword)))
						   (if value-reducer
						       (funcall value-reducer result)
						       result)))
						(t (car result))))))
		   ((null-string-p (car raw-value)) nil)
		   (t (funcall parser (car raw-value)))))))
    (declare (inline parse-header))
    (if (slot-boundp header 'value)
	(%header-value header)
	(setf (%header-value header) (parse-header (header-keyword header) (header-raw-value header))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.469")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header-type :date-header (:integer-header)
  :parse-function parse-date-header
  :print-function print-date-header
  :atomic-value-reducer first)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.470")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun header-value-max (value)
  (apply #'max value))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.470")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun header-value-min (value)
  (apply #'min value)) 


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.470")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :date
               (:date-header :general)
  :print-string "Date"
  :atomic-value-reducer 'header-value-max
  :default 'gmt-time)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.470")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :expires
               (:date-header :entity)
  :print-string "Expires"
  :parse-function 'parse-expires-header
  :atomic-value-reducer 'header-value-min)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.470")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :if-modified-since
               (:date-header :request)
  :print-string "If-Modified-Since"
  :atomic-value-reducer 'header-value-min)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.470")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :if-unmodified-since
               (:date-header :request)
  :print-string "If-Unmodified-Since"
  :atomic-value-reducer 'header-value-min)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.470")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :last-modified
               (:date-header :entity)
  :print-string "Last-Modified"
  :atomic-value-reducer 'header-value-min)

