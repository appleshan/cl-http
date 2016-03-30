;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.40
;;; Reason: Bring VIA header up to HTTP 1.1 standard and reduce consing when printing
;;; comma separated headers.
;;; 
;;; Function HTTP::PARSE-VIA-HEADER:  bring parsing up to spec.
;;; Function HTTP::PARSE-VIA-HEADER:  clean up.
;;; Function HTTP::PRINT-VIA-HEADER:  clean up and reduce consing.
;;; Variable HTTP::*ELIDE-HTTP-VERSION-IN-VIA-HEADER*:  switch.
;;; Function HTTP::STRING-CONCATENATE:  some declarations and accept null arguments like CONCATENATE.
;;; Function HTTP::PRINT-COMMA-SEPARATED-QUALITY-PAIR-OR-TOKENS:  reduce consing.
;;; Function HTTP::PRINT-COMMA-SEPARATED-QUALITY-PAIRS:  reduce consing.
;;; Function HTTP::PRINT-MIME-CONTENT-TYPE-SEQUENCE-HEADER:  reduce consing.
;;; Function HTTP::PRINT-CACHE-CONTROL-HEADER:  reduce consing.
;;; 
;;; DEFINE-HEADER-TYPE :QUALITY-PAIR-SEQUENCE-HEADER:  update
;;; DEFINE-HEADER :ACCEPT-CHARSET:  update.
;;; DEFINE-HEADER :ACCEPT-LANGUAGE:  update
;;; DEFINE-HEADER :TE:  update.
;;; DEFINE-HEADER-TYPE :CONTENT-TYPE-SEQUENCE-HEADER:  update.
;;; DEFINE-HEADER :ACCEPT:  update.
;;; DEFINE-HEADER-TYPE :CACHE-CONTROL-HEADER:  update.
;;; DEFINE-HEADER :CACHE-CONTROL:  update
;;; DEFINE-HEADER-TYPE :VIA-HEADER:  update.
;;; DEFINE-HEADER :VIA:  update.
;;; Written by JCMa, 5/02/00 10:35:24
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.39,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 41, HTTP Client Substrate 3.9,
;;; HTTP Proxy Server 4.8, HTTP Client 49.7, Image Substrate 440.4,
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
  "HTTP:SERVER;UTILS.LISP.440"
  "HTTP:SERVER;HEADERS.LISP.456"
  "HTTP:SERVER;HEADERS.LISP.457")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.456")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;;;------------------------------------------------------------------- 
;;;
;;; PARSING THE VIA HEADER
;;;

(defun parse-via-header (string &optional (start 0) (end (length string)))
  (labels ((protocol-defaulted-p (string start end)
	     (with-fast-array-references ((string string string))
	       (loop for idx upfrom start below end
		     for char = (aref string idx)
		     when (alpha-char-p char) 
		       return nil
		     when (and (eql #\/ char)	;handle numeric protocols
			       (< start idx))
		       return nil
		     finally (return t))))
	   (tokenize-defaulted-protocol (string start end)
	     (declare (fixnum start end))
	     (let* ((length (the fixnum (+ 5 (- end start))))
		    (temp (make-array length :element-type  *standard-character-type*)))
	       (declare (dynamic-extent temp))
	       (copy-vector-portion "HTTP/" 0 5 temp 0 5)
	       (copy-vector-portion string start end temp 5 length)
	       (%tokenize-header-keyword temp 0 length)))
	   (parse-host-spec (string start end)
	     (let ((colon (char-position #\: string start end t)))
	       (if colon
		   (list (subseq string start colon) (parse-integer string :start (1+ colon) :end end))
		   (list (subseq string start end)))))
	   (parse-comment (string start end)
	     (let ((s1 (char-position #\( string start end))
		   (e1 (char-position #\) string start end t)))
	       (list (subseq string (if s1 (1+ (the fixnum s1)) start) (or e1 end)))))
	   (parse-entry (string &optional (start 0) (end (length string)))
	     (with-string-trim-bounds (*white-space-chars* string start end)
	       (let* ((e1 (position-if* #'white-space-char-p string :start start :end end))
		      (s2 (position-if-not* #'white-space-char-p string :start (1+ (the fixnum e1)) :end end))
		      (e2 (or (position-if* #'white-space-char-p string :start s2 :end end) end))
		      (s3 (when (and e2 (< e2 end))
			    (position-if-not* #'white-space-char-p string :start (1+ (the fixnum e2)) :end end))))
		 (list* (if (protocol-defaulted-p string start e1)
			   (tokenize-defaulted-protocol string start e1)
			   (%tokenize-header-keyword string start e1))
		       (parse-host-spec string s2 e2) 
		       (when s3 (parse-comment string s3 end)))))))
    (declare (inline protocol-defaulted-p parse-host-spec parse-comment))
    (parse-comma-separated-header string start end #'parse-entry)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.456")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defparameter *elide-http-version-in-via-header* nil
  "Controls whether http protocol version elide the protocol name.")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.456")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun print-via-header (header-value-plist stream)
  (using-resource (string line-buffer *line-buffer-size*)
    (flet ((entry-string (value)
	     (destructuring-bind (protocol (received-by &optional port) &optional comment) value
	       (let* ((protocol-string (symbol-name protocol))
		      (e1 (length protocol-string))
		      (s1 (if (and *elide-http-version-in-via-header*
				   (< 5 e1)
				   (string-equal "HTTP/" protocol-string :start1 0 :end1 5 :start2 0 :end2 5))
			      5
			      0))
		      (l1 (- e1 s1))
		      (s2 (1+ l1))
		      (l2 (length received-by))
		      (e2 (+ s2 l2))
		      (port-string (and port (write-to-string port :base 10 :escape nil))) 
		      s3 e3 l3 s4 e4 l4)
		 (declare (fixnum s1 l1 e1 s2 l2 e2)
			  (dynamic-extent port-string))
		 (cond-every
		   (port (setq s3 (the fixnum (1+ e2))
			       l3 (length port-string)
			       e3 (the fixnum (+ s3 l3))))
		   (comment
		     (setq l4 (length comment)
			   s4 (the fixnum (+ 2 (or e3 e2)))
			   e4 (the fixnum (+ s4 (the fixnum l4))))))
		 (setf (fill-pointer string) (if e4 (1+ e4) (or e4 e3 e2)))
		 (copy-vector-portion protocol-string s1 e1 string 0 l1)
		 (setf (aref string l1) #\space)
		 (copy-vector-portion received-by 0 l2 string s2 e2)
		 (cond-every
		   (port
		     (setf (aref string e2) #\:)
		     (copy-vector-portion port-string 0 l3 string s3 e3))
		   (l4				;comment
		     (setf (aref string (the fixnum (- s4 2))) #\space
			   (aref string (the finuxm (1- s4))) #\()
		     (copy-vector-portion comment 0 l4 string s4 e4)
		     (setf (aref string e4) #\))))
		 string))))
      (declare (dynamic-extent #'entry-string))
      (print-comma-separated-header header-value-plist stream #'entry-string))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.456")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header-type :via-header (:header)
  :parse-function parse-via-header
  :print-function print-via-header)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.456")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :via
               (:via-header :general)
  :print-string "Via")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.456")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun print-comma-separated-quality-pairs (header-value-list stream)
  (using-resource (string line-buffer *line-buffer-size*)
    (flet ((quality-pair-string (quality-pair)
	     (destructuring-bind (keyword . quality) quality-pair
	       (let ((val (write-to-string quality :base 10)))
		 (declare (dynamic-extent val))
		 (setf (fill-pointer string) 0)
		 (string-concatenate string (string-for-tokenized-header-keyword keyword) ";q=" val)))))
      (declare (dynamic-extent #'quality-pair-string))
      (print-comma-separated-header header-value-list stream #'quality-pair-string))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.456")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;; used by the TE header   5/21/98 -- JCMa.
(defun print-comma-separated-quality-pair-or-tokens (header-value-list stream &aux string)
  (unwind-protect
      (flet ((quality-pair-string (quality-pair)
	       (destructuring-bind (keyword &rest quality) quality-pair
		 (if (and quality (not (eql quality 1)))
		     (let ((val (write-to-string quality :base 10)))
		       (declare (dynamic-extent val))
		       (unless string
			 (setq string (allocate-resource 'line-buffer *line-buffer-size*)))
		       (setf (fill-pointer string) 0)
		       (string-concatenate string (string-for-tokenized-header-keyword keyword) ";q=" val))
		     (string-for-tokenized-header-keyword keyword)))))
	(declare (dynamic-extent #'quality-pair-string))
	(print-comma-separated-header header-value-list stream #'quality-pair-string))
    (when string
      (deallocate-resource 'line-buffer string))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.440")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(define string-concatenate (buffer &rest strings)
  "Concatenates STRINGS together in BUFFER.
when non-null BUFFER must have a fill pointer and be adjustable.
When BUFFER is null, this conses a new result.
BUFFER may appear as an element of STRINGS, in which case the
initial contents of BUFFER appears in the appropriate position of
the final pattern."
  (declare (dynamic-extent strings))
  (flet ((compute-prefix-and-suffix-sizes (buffer strings)
	   (loop for (string . more) = strings then more
		 while string
		 until (eq string buffer)
		 sum (the fixnum (length string)) into prefix-size
		 finally (loop for string in more
			       sum (the fixnum (length string)) into suffix-size
			       finally (return-from compute-prefix-and-suffix-sizes (values prefix-size suffix-size))))))
    (declare (inline compute-prefix-and-suffix-sizes))
    (cond (buffer
	   (let ((size (array-total-size buffer))
		 (fill-pointer (length buffer))
		 (occurrences (count buffer strings :test #'eq)))
	     (declare (fixnum size fill-pointer))
	     (case occurrences
	       (0
		 (let ((final-fill-pointer (loop for string in strings
						 sum (the fixnum (length string)))))
		   (when (< size final-fill-pointer)
		     (setq buffer (adjust-array buffer final-fill-pointer :fill-pointer t)))
		   (loop with start2 fixnum = 0
			 for string in strings
			 when string
			   do (let* ((end1 (length string))
				     (end2 (+ start2 end1)))
				(declare (fixnum end1 end2))
				(copy-vector-portion string 0 end1 buffer start2 end2)
				(setq start2 end2))
			 finally (setf (fill-pointer buffer) final-fill-pointer))))
	       (1 (multiple-value-bind (prefix-size suffix-size)
		      (compute-prefix-and-suffix-sizes buffer strings)
		    (let ((final-fill-pointer (+ prefix-size suffix-size fill-pointer)))
		      (when (< size final-fill-pointer)
			(let ((nbuffer (adjust-array buffer final-fill-pointer :fill-pointer t)))
			  (setq strings (nsubstitute nbuffer buffer strings :test #'eq :count occurrences) 
				buffer nbuffer)))
		      (setq buffer (nshift-vector buffer prefix-size 0 fill-pointer))
		      (loop with start2 fixnum = 0
			    for string in strings
			    when (and string (not (eq string buffer)))
			      do (let* ((end1 (length string))
					(end2 (+ start2 end1)))
				   (declare (fixnum end1 end2))
				   (copy-vector-portion string 0 end1 buffer start2 end2)
				   (setq start2 end2))
			    else do (the fixnum (incf start2 fill-pointer))
			    finally (setf (fill-pointer buffer) final-fill-pointer)))))
	       (t (error "Can't handle more than one occurrence of buffer, ~S, in strings, ~S" buffer (copy-seq strings))))
	     buffer))
	  (t (apply #'concatenate 'string strings)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.456")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun print-mime-content-type-sequence-header (content-type-sequence stream)
  (using-resource (string line-buffer *line-buffer-size*)
    (flet ((mime-content-type-header-string (content-type)
	     (destructuring-bind (type subtype . param-plist) content-type
	       (string-concatenate string (mime-content-type-string type) "/" (mime-content-type-string subtype))
	       (when param-plist
		 (with-output-to-string (stream string)
		   (print-mime-header-parameters param-plist stream)))
	       string)))
      (declare (dynamic-extent #'mime-content-type-header-string))
      (print-comma-separated-header content-type-sequence stream #'mime-content-type-header-string))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.456")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun print-cache-control-header (header-value-plist stream &aux string)
  (macrolet ((ensure-string-buffer (string)
	       `(if ,string
		    (setf (fill-pointer ,string) 0)
		    (setq ,string (allocate-resource 'line-buffer *line-buffer-size*))))
	     (string-for-keyword-integer-pair (string keyword value)
	       `(progn (check-type ,value integer)
		       (ensure-string-buffer ,string)
		       (string-concatenate ,string (string-for-tokenized-header-keyword ,keyword) "=")
		       (with-output-to-string (stream ,string)
			 (write ,value :base 10. :stream stream))
		       ,string)))
    (unwind-protect
	(flet ((directive-string (keyword value)
		 (case keyword
		   ((:no-store :only-if-cached :public :no-transform :must-revalidate :proxy-revalidate)
		    (string-for-tokenized-header-keyword keyword))
		   ((:max-age :min-fresh)
		    (string-for-keyword-integer-pair string keyword value))
		   (:max-stale
		     (if (eq value t)
			 (string-for-tokenized-header-keyword keyword)
			 (string-for-keyword-integer-pair string keyword value)))
		   ((:no-cache :private)
		    (cond ((eq value t)
			   (string-for-tokenized-header-keyword keyword))
			  (t (ensure-string-buffer string)
			     (string-concatenate string (string-for-tokenized-header-keyword keyword) "=\"")
			     (with-output-to-string (stream string)
			       (print-comma-separated-header value stream #'string-for-tokenized-header-keyword))
			     (vector-push #\" string)
			     string)))
		   (t (if (eq value t)
			  (string-for-tokenized-header-keyword keyword)
			  (let ((val (typecase value 
				       (symbol (string-for-tokenized-header-keyword value))
				       (t nil))))
			    (string-concatenate string (string-for-tokenized-header-keyword keyword) "=" val)
			    (unless val
			      (with-output-to-string (stream string)
				(etypecase value
				  (integer (write value :base 10. :stream stream))
				  (string (write value :escape t :stream stream)))))
			    string))))))
	  (declare (dynamic-extent #'directive-string))
	  (etypecase (car header-value-plist)
	    (atom (print-comma-separated-header-plist header-value-plist stream #'directive-string))
	    (cons (loop for (item . more) = header-value-plist then more
			do (print-comma-separated-header-plist item stream #'directive-string)
			while more
			do (write-string ", " stream)))))
      (when string
	(deallocate-resource 'line-buffer string)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.456")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header-type :quality-pair-sequence-header (:comma-separated-header)
  :parse-function parse-comma-separated-quality-pairs
  :print-function print-comma-separated-quality-pairs
  :print-series-predicate list-valued-header-series-value-p)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.456")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :accept-charset
               (:quality-pair-sequence-header :request)
  :print-string "Accept-Charset")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.456")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :accept-language
               (:quality-pair-sequence-header :request)
  :print-string "Accept-Language")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.456")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :te
               (:header :request)
  :print-string "TE"
  :print-function 'print-comma-separated-quality-pair-or-tokens
  :parse-function 'parse-comma-separated-quality-pairs)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.457")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;; need to add singleton parsing option for sequence type headers.
(define-header-type :content-type-sequence-header (:header)
  :parse-function parse-mime-content-type-sequence-header
  :print-function print-mime-content-type-sequence-header
  :print-series-predicate list-valued-header-series-value-p)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.457")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;;;------------------------------------------------------------------- 
;;;
;;; HTTP HEADERS
;;;

(define-header :accept
               (:content-type-sequence-header
                :request)
  :print-string "Accept")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.457")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header-type :cache-control-header (:header)
  :parse-function parse-cache-control-header
  :print-function print-cache-control-header
  :print-series-predicate atomic-valued-header-series-value-p)	; allow for multiple cache-control headers


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.457")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :cache-control
               (:cache-control-header :general)
  :print-string "Cache-Control")

