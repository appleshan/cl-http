;;; -*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-
;;;
;;; (c) Copyright  1994-2001, 2003, 2005-2007, 2009, John C. Mallery
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; HTTP AND MIME HEADERS
;;;
(in-package :http)

;;;------------------------------------------------------------------- 
;;;
;;; SPECIAL CHARACTERS IN HTTP HEADERS
;;;

(eval-when (:load-toplevel :execute :compile-toplevel)
  (define-constant *header-special-characters*
    '(#\( #\) #\< #\> #\@ #\, #\; #\: #\\ #\" #\/ #\[ #\] #\? #\= #\{ #\} #\space #\tab)
    "Special characters that MUST be quoted in HTTP headers."))

(define-constant *header-quote-character* #\\
                 "The quote character for use in HTTP headers.")

(eval-when (:load-toplevel :execute :compile-toplevel)
;; the #. is here to work around a bug in the LispWorks compiler.   8/31/95 -- JCMa.
  (defconstant *header-special-characters-bits*
               #.(let ((v (make-array (1+ (loop for ch in *header-special-characters*
                                                    maximizing (char-code ch)))
                                      :element-type 'bit
                                      :initial-element 0)))
                   (loop for ch in *header-special-characters*
                         do (setf (sbit v (char-code ch)) 1))
                   v)))

(define header-special-character-p (char)
  "Returns non-null if CHAR is a special character that must be escaped.
CHAR is compared with CHAR=."
  (let ((code (char-code char)))
    (and (< code #.(length *header-special-characters-bits*))
         (eql 1 (sbit (the simple-bit-vector *header-special-characters-bits*) code)))))

(declaim (inline escape-character-p))

(define header-quote-character-p (char)
  "Returns non-null if CHAR is the character used for quoting special characters in HTTP headers.
CHAR is compared with CHAR=."
  (char= *header-quote-character* char))

(define string-contains-header-special-characters-p (string &optional (start 0) (end (length string)))
  "Returns non-null if STRING contains any header specail characters between START and END."
  (with-fast-array-references ((string string string))
    (loop for idx upfrom start below end
          when (header-special-character-p (aref string idx))
            return t
          finally (return nil))))

(define write-header-parameter-quoting-specials (string &optional (stream *standard-output*))
  "Writes STRING to STREAM ensuing that double quotes are used if any special characters are present."
  (cond ((string-contains-header-special-characters-p string)
         (with-fast-array-references ((string string string))
           (loop initially (write-char #.(code-char 34) stream)
                 for idx upfrom 0 below (length string)
                 for char = (aref string idx)
                 when (eql char #.(code-char 34))
                   do (write-char *header-quote-character* stream)
                 do (write-char char stream)
                 finally (write-char #.(code-char 34) stream))))
        (t (write-string string stream))))

;;;------------------------------------------------------------------- 
;;;
;;; HEADER ACCESS FUNCTIONS
;;;

(declaim (inline %header-supertype))

(define %header-supertype (header)
  "Returns the supertype of HEADER."
  (get header 'header-supertype))

(defsetf %header-supertype (header) (supertype)
  `(setf (get ,header 'header-supertype) ,supertype))

(define-variable *header-superclasses* '(:general :request :response :entity :email :unknown))

(declaim (inline %header-superclass))

(define %header-superclass (header)
  "Returns the superclass of HEADER.
The super classes group headers according to their role in HTTP."
  (get header 'header-superclass))

(defsetf %header-superclass (header) (superclass)
  `(progn
     (unless (member ,superclass *header-superclasses*)
       (error "~S is not one of the known header super-classes, ~S."
              ,superclass *header-superclasses*))
     (setf (get ,header 'header-superclass) ,superclass)))

(declaim (inline %header-class-p))

(define %header-class-p (header class)
  "Returns non-null if header belongs to the HTTP Class CLASS.
Class can be any member of *header-superclasses*."
  (eq (%header-superclass header) class))

(declaim (inline %header-print-name))

(define %header-print-name (header)
  "Returns an interned symbol wich is the print name for HEADER."
  (get header 'header-print-name))

(defsetf %header-print-name (header) (print-name)
  `(setf (get ,header 'header-print-name) ,print-name))

(declaim (type function %header-keyword))

(defsetf %header-keyword (header) (keyword)
  `(setf (get ,header 'header-keyword) ,keyword))

(define %header-keyword (header-print-name)
  "Returns the keyword for %header-print-name."
  (declare (notinline))
  (check-type header-print-name keyword)
  (cond ((get header-print-name 'header-keyword))
        (t (let ((standard-header-key (intern (string-upcase (symbol-name header-print-name)) *keyword-package*)))
             ;; if this is a new header then track the print name on the canonical form.
             (unless (%header-print-name standard-header-key)
               (setf (%header-print-name standard-header-key) header-print-name))
             ;; update the print name so that the cannonical form is found right away next time.
             (setf (%header-keyword header-print-name) standard-header-key)
             standard-header-key))))

(defun %intern-new-header (string &optional (start 0) (end (length string)))
  (http::%header-keyword
    (intern (subseq string start end) *keyword-package*)))

(tk1:define-tokenizer header
                      :tokenizer '%intern-new-header
                      :test 'char-equal
		      :size 70
                      :definer define
                      :documentation "Tokenizes HTTP headers without consing.")

(defun %intern-new-header-value (string &optional (start 0) (end (length string)))
  (intern (nstring-upcase (subseq string start end) :start 0 :end (- (the fixnum end) (the fixnum start)))
          *keyword-package*))

(tk1:define-tokenizer header-value
                      :tokenizer '%intern-new-header-value
                      :test 'char-equal
                      :definer define
                      :documentation "Tokenizes HTTP header values without consing.")

(defun %intern-header-keyword-value (string &optional (start 0) (end (length string)))
  (let ((n-string (subseq string start end)))
    (multiple-value-bind (keyword old-p)
        (intern (nstring-upcase n-string :start 0 :end (- (the fixnum end) (the fixnum start)))
                *keyword-package*)
      (unless old-p
        (setf (get keyword 'keyword-header-value) n-string))
      (values keyword (not old-p)))))

(define string-for-tokenized-header-keyword (keyword)
  (or (get keyword 'keyword-header-value)
      (setf (get keyword 'keyword-header-value) (string-downcase (symbol-name keyword)))))

(tk1:define-tokenizer header-keyword
                      :tokenizer '%intern-header-keyword-value
                      :test 'char-equal
		      :size 50
                      :definer define
                      :documentation "Tokenizes HTTP header values that parse to keywords without consing.")

(defun %define-header-keyword (item)
  (let ((keyword (tokenize-header-keyword item)))
    (setf (get keyword 'keyword-header-value) item)
    keyword))

(define-macro define-header-keywords (&rest values)
  `(dolist (item ',values)
     (%define-header-keyword item)))

(define %header-default-value (header)
  "Returns the default value for HEADER."
  (let ((val (get header 'header-default-value)))
    (typecase val
      (null nil)
      (string val)
      (symbol (if (fboundp val)
                  (funcall val)
                  (symbol-name val))))))

(defsetf %header-default-value (header) (default-value)
  `(setf (get ,header 'header-default-value) ,default-value))

(declaim (inline %header-remove-default-value))

(define %header-remove-default-value (header)
  (remprop header 'header-default-value))

(declaim (inline %header-parse-function))

(define %header-parse-function (header)
  (or (get header 'header-parse-function)
      #'parse-standard-header))

(defsetf %header-parse-function (header) (parse-function)
  `(setf (get ,header 'header-parse-function) ,parse-function))

(declaim (inline %header-print-function))

(define %header-print-function (header)
  (or (get header 'header-print-function)
      #'print-standard-header))

(defsetf %header-print-function (header) (print-function)
  `(setf (get ,header 'header-print-function) ,print-function))

(define %header-print-series-predicate (header)
  (get header 'header-print-series-predicate))

(defsetf %header-print-series-predicate (header) (print-series-predicate)
  `(setf (get ,header 'header-print-series-predicate) ,print-series-predicate))

;; Reduces a series header value to an atomic value when multiple values are
;; encountered during parsing.
(define %header-atomic-value-reducer (header)
  (get header 'header-atomic-value-reducer))

(defsetf %header-atomic-value-reducer (header) (atomic-value-reducer)
  `(setf (get ,header 'header-atomic-value-reducer) ,atomic-value-reducer))

(declaim (inline %header-collapsable-p))

(defun %header-collapsable-p (header)
  (get header 'header-collapsable-p))

(defsetf %header-collapsable-p (header) (collapsable-p)
  `(setf (get ,header 'header-collapsable-p) ,collapsable-p))

(defun %inherit-header-function (header path &optional (error-p t))
  (let* ((header-type (get header 'header-supertype)))
    (cond ((null header-type) nil)
          ((get header-type path))
          ((%inherit-header-function header-type path error-p))
          (error-p (error "No ~A function found." path))
          (t nil)))) 

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

(define-macro define-header-type (header-type (supertype)
                                              &key (parse-function 'parse-standard-header)
                                              (print-function 'print-standard-header)
					      print-series-predicate
					      atomic-value-reducer)
  "Top-level function for defining a header type."
  `(%define-header-type
     ',header-type ',supertype ',parse-function ',print-function ',print-series-predicate ',atomic-value-reducer))

(defun %define-header (header header-type header-class &optional print-name default parse-function print-function
			      print-series-predicate atomic-value-reducer (collapsable-p t))
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
	    (%header-collapsable-p psym) collapsable-p
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

(define-macro define-header (header (header-type &optional header-class) &key print-string default
				    parse-function print-function print-series-predicate atomic-value-reducer (collapsable-p t))
  "Top-level function for defining a header."
  `(%define-header ,header ,header-type ,header-class ,print-string ,default
		   ,parse-function ,print-function ,print-series-predicate ,atomic-value-reducer ,collapsable-p))

;;;------------------------------------------------------------------- 
;;;
;;; HEADER PARSERS AND PRINTERS
;;;

(declaim (inline parse-standard-header))

(defun parse-standard-header (string &optional (start 0) (end (length string)))
  (triming-substring *white-space-chars* string start end t))

(declaim (inline print-standard-header))

(defun print-standard-header (string stream)
  (etypecase string
    (string (write-string string stream))
    (cons
      (loop initially (fast-format stream "~A" (first string))
	    for string in (cdr string)
            do (fast-format stream "~% ~A" string)))))

(defun parse-integer-header (string &optional (start 0) (end (length string)))
  (with-string-trim-bounds (*white-space-chars* string start end)
    (handler-case
      (parse-integer string :radix 10. :start start :end end :junk-allowed t)
      (error () (subseq string start end)))))

(declaim (inline print-integer-header))

(defun print-integer-header (integer stream)
  (write integer :stream stream :escape nil :base 10.))

(defun parse-version-header (string &optional (start 0) (end (length string)))
  (flet ((digit-value-p (string start end)
           (loop for idx upfrom start below end
                 unless (digit-char-p (aref string idx))
                   return nil
                 finally (return t)))
         (quote-char-p (char)
           (member char '(#\" #\space))))
    (declare (inline digit-value-p))
    (with-string-trim-bounds (*white-space-chars* string start end)
      (if (= start end)
	  ""
	  (let* ((s (or (position-if-not* #'quote-char-p string :start start :end end) start))
		 (e (position-if-not* #'quote-char-p string :start s :end end :from-end t)))
	    (setq e (if e (1+ (the fixnum e)) end))
	    (cond ((digit-value-p string s e)
		   (handler-case
		     (parse-integer string :radix 10. :start s :end e)
		     (error () (subseq string s e))))
		  (t (subseq string s e))))))))

(declaim (inline print-version-header))

(defun print-version-header (version stream)
  (fast-format stream "\"~A\"" version))


;;;------------------------------------------------------------------- 
;;;
;;; ENTITY TAG HEADERS
;;;

(defmethod print-object ((entity-tag entity-tag) stream)
  (with-slots (value) entity-tag
    (print-unreadable-object (entity-tag stream :type t :identity t)
      (when (slot-boundp entity-tag 'value)
        (write value :stream stream :escape nil)))))

#-(or ANSI-CL Draft-ANSI-CL-2)
(defmethod make-load-form ((entity-tag entity-tag))
  `(make-instance ',(type-of entity-tag) :value ,(entity-tag-value entity-tag)))

#+(or ANSI-CL Draft-ANSI-CL-2)
(defmethod make-load-form ((entity-tag entity-tag) &optional environment)
  (declare (ignore environment))
  `(make-instance ',(type-of entity-tag) :value ,(entity-tag-value entity-tag)))

(defun coerce-entity-tag-value (value)
  (if (every #'digit-char-p value)
      (parse-integer value)
      value))

(declaim (inline allocate-weak-entity-tag))

(define allocate-weak-entity-tag (value)
  "Returns a weak entity tag with value, VALUE."
  (make-instance 'weak-entity-tag :value value))

(declaim (inline allocate-strong-entity-tag))

(define allocate-strong-entity-tag (value)
  "Returns a strong entity tag with value, VALUE."
  (make-instance 'strong-entity-tag :value value))

(declaim (inline allocate-wild-entity-tag))

(define allocate-wild-entity-tag ()
  "Returns a wild entity tag."
  (make-instance 'wild-entity-tag :value "*"))

(defgeneric print-entity-tag-header (entity-tag stream)
  (:documentation "Prints ENTITY-TAG as an entity tag header value on STREAM.
ENTITY-TAG can any entity tag object, or an integer or the keyword :wild."))

(defmethod print-entity-tag-header ((entity-tag (eql :wild)) stream)
  (write-string "*" stream))

(defmethod print-entity-tag-header ((entity-tag integer) stream)
  (fast-format stream "\"~A\"" entity-tag))

(defmethod print-entity-tag-header ((entity-tag string) stream)
  (fast-format stream "~S" entity-tag))

(defmethod print-entity-tag-header ((entity-tags cons) stream)
  (loop for (etag . more) = entity-tags then more
	while etag
	do (print-entity-tag-header etag stream)
	when more
	  do (fast-format stream ", ")))

(defmethod entity-tag-value :around ((entity-tag strong-entity-tag))
  (let ((tag (call-next-method)))
    (typecase tag
      (integer (write-to-string tag :escape t :base 10))
      (t tag))))

(defmethod entity-tag-value ((entity-tag integer))
  (write-to-string entity-tag :escape t :base 10))

(define-generic entity-tag-raw-value (entity-tag)
  (:documentation "Returns the raw value of ENTITY-TAG.
This does not coerce integer values to strings like ENTITY-TAG-VALUE."))

(defmethod entity-tag-raw-value ((entity-tag entity-tag))
  (with-slots (value) entity-tag
    value))

(defmethod entity-tag-raw-value (entity-tag)
  entity-tag)

(defmethod print-entity-tag-header ((entity-tag wild-entity-tag) stream)
  (write-string (entity-tag-value entity-tag) stream))

(defmethod print-entity-tag-header ((entity-tag entity-tag) stream)
  (fast-format stream "\"~A\"" (entity-tag-value entity-tag)))

(defmethod print-entity-tag-header ((entity-tag weak-entity-tag) stream)
  (fast-format stream "W/\"~A\"" (entity-tag-value entity-tag)))

(defgeneric entity-tag-header-string (entity-tag))

(defmethod entity-tag-header-string ((entity-tag wild-entity-tag))
  (entity-tag-value entity-tag))

(defmethod entity-tag-header-string ((entity-tag entity-tag))
  (concatenate 'string "\"" (entity-tag-value entity-tag) "\""))

(defmethod entity-tag-header-string ((entity-tag weak-entity-tag))
  (concatenate 'string "W/\"" (entity-tag-value entity-tag) "\""))

(defun parse-entity-tag-header (string &optional (start 0) (end (length string)))
  (flet ((quote-char-p (char)
           (member char '(#\" #\space)))
         (digit-value-p (string start end)
           (loop for idx upfrom start below end
                 unless (digit-char-p (aref string idx))
                   return nil
                 finally (return t)))
         (weak-tag-p (string start end)
           (and end
                (= (1+ (the fixnum start)) end)
                (char-equal (aref string start) #\W))))
    (declare (inline weak-tag-p digit-value-p))
    (with-string-trim-bounds (*white-space-chars* string start end)
      (if (= start end)
	  ""
	  (let* ((s1 (char-position #\" string start end))
		 (s2 (char-position #\/ string start (or s1 end)))
		 (s (cond (s1 (1+ (the fixnum s1)))
			  (s2 (1+ (the fixnum s2)))
			  (t start)))
		 (e (position-if-not* #'quote-char-p string :start s :end end :from-end t))
		 (weak-p (weak-tag-p string start s2))
		 entity-tag-value)
	    (cond ((eql #\* (aref string start))
		   (allocate-wild-entity-tag))
		  (t (setq e (if e (1+ (the fixnum e)) end))
		     (setq entity-tag-value (if (digit-value-p string s e)
						(parse-integer string :start s :end e)
						(subseq string s e)))
		     (if weak-p
			 (allocate-weak-entity-tag entity-tag-value)
			 (allocate-strong-entity-tag entity-tag-value)))))))))

(define-generic entity-tag-equal (entity-tag1 entity-tag2)
  (:documentation "Returns non-null if ENTITY-TAG1 is equal to ENTITY-TAG2."))

(defmethod entity-tag-equal ((entity-tag1 entity-tag) (entity-tag2 entity-tag) &aux type-1 type-2)
  (or (eq (setq type-1 (type-of entity-tag1)) 'wild-entity-tag)
      (eq (setq type-2 (type-of entity-tag2)) 'wild-entity-tag)
      (and (eq type-1 type-2)
           (equal (entity-tag-raw-value entity-tag1) (entity-tag-raw-value entity-tag2)))))

(defmethod entity-tag-equal ((entity-tag1 integer) (entity-tag2 integer))
  (= entity-tag1 entity-tag2))

(defmethod entity-tag-equal ((entity-tag1 string) (entity-tag2 string))
  (equal entity-tag1 entity-tag2))

(defmethod entity-tag-equal ((entity-tag1 entity-tag) (entity-tag2 integer))
  (equal (entity-tag-raw-value entity-tag1) entity-tag2))

(defmethod entity-tag-equal ((entity-tag1 integer) (entity-tag2 entity-tag))
  (equal entity-tag1 (entity-tag-raw-value entity-tag2)))

(define-generic wild-entity-tag-p (entity-tag)
  (:documentation "Returns non-null if entity-tag is a wildcard entity tag."))

(defmethod wild-entity-tag-p (entity-tag)
  (declare (ignore entity-tag))
  nil)

(defmethod wild-entity-tag-p ((entity-tag wild-entity-tag))
  t)

(defun parse-entity-tag-sequence-header (string &optional (start 0) (end (length string)))
  (with-string-trim-bounds (*white-space-chars* string start end)
    (parse-comma-separated-header string start end #'parse-entity-tag-header)))

(defun print-entity-tag-sequence-header (entity-tag-sequence stream)
  (etypecase entity-tag-sequence
    (cons
      (print-comma-separated-header entity-tag-sequence stream #'entity-tag-header-string))
    (entity-tag
      (print-entity-tag-header entity-tag-sequence stream))))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(declaim (notinline parse-keyword-header))

(defun parse-keyword-header (string &optional (start 0) (end (length string)))
  (with-string-trim-bounds (*white-space-chars* string start end)
    (multiple-value-bind (keyword)
	(%tokenize-header-keyword string start end)
      keyword)))

(declaim (inline print-keyword-header))

(defun print-keyword-header (keyword stream)
  (write-string (string-for-tokenized-header-keyword keyword) stream))

(defun parse-date-header (string &optional (start 0) (end (length string)) (error-p *debug-server*))
  (declare (optimize (speed 3)))
  (let ((e (or (char-position #\; string start end)	;handle Lou Montoulli brain damage in Netscape 2.0b1
	       end)))
    (with-string-trim-bounds (*white-space-chars* string start e)
      (handler-case-if (not error-p)
	 (parse-gmt-time (nsubstitute #\space #\? string :start start :end e)	;deal with misconfigured Windows clients/servers
			 start e)
	(error () (get-universal-time))))))

(defun print-date-header (universal-time stream)
  (print-gmt-time stream universal-time))				;HTTP 1.1 spec 14.21 requires errors parsing expires to be treated as expired 

(defun parse-expires-header (string &optional (start 0) (end (length string)))
  (flet ((handle-zero-valued-expiration (string start end)
	   (with-fast-array-references ((string string))
	     (loop for idx upfrom start below end
		   for char = (aref string idx)
		   unless (eql char #\space)
		     do (if (eql char #\0)
			    (return-from parse-expires-header 0)
			    (return-from handle-zero-valued-expiration))))))
    (declare (inline handle-zero-valued-expiration))
    (handle-zero-valued-expiration string start end)
    (handler-case-if (not *debug-server*)
       (parse-date-header string start end t)
      (error () 0))))

(defun print-expires-header (universal-time stream)
  (if (zerop universal-time)
      (write-char #\0 stream)
      (print-gmt-time stream universal-time)))

(defun parse-comma-separated-header (string &optional (start 0) (end (length string)) (header-value-parser #'subseq))
  "Applies header-value-parser to each comma separated header-value in STRING.
If HEADER-VALUE-PARSER return multiple values, they are concatenated together into the returned list."
  (flet ((first-non-blank (start end)
           (position-if-not* #'white-space-char-p string :start start :end end)))
    (declare (inline first-non-blank))
    (loop for s = (first-non-blank start end) then (first-non-blank (1+ idx) end)
	  while s
          for idx fixnum = (or (char-position #\, string s end) end)
          for last = (position-if-not* #'white-space-char-p string :start s :end idx :from-end t)
          when last
            nconc (multiple-value-list (funcall header-value-parser string s (1+ (the fixnum last))))
          while (< idx end))))

(defun print-comma-separated-header (header-value-list stream &optional (string-generator #'string)
                                                       (fill-column *header-fill-column*))
  "Applies STRING-GENERATOR to each value in HEADER-VALUE-LIST to write header value to STREAM."
  (declare (fixnum fill-column))
  (loop with  position fixnum = 0
        for header-value in (ensure-list header-value-list)
        for item = (funcall string-generator header-value)
        for count fixnum upfrom 0
        for len fixnum = (length item) 
        unless (zerop count)
          do (fast-format stream ", ")
             (the fixnum (incf position (the fixnum 2)))
        when (> (the fixnum (+ position len 1)) fill-column)    ;don't exceed specified fill column
          do (fast-format stream "~% ")
             (setf position 1)
        do (write-string item stream :end len)
           (the fixnum (incf position len))))

(defun parse-comma-separated-header-plist (string &optional (start 0) (end (length string)) (header-value-parser #'subseq))
  "Applies header-value-parser to each comma separated header-value in STRING."
  (flet ((first-non-blank (start end)
           (position-if-not* #'white-space-char-p string :start start :end end)))
    (declare (inline first-non-blank))
    (loop with s = (first-non-blank start end) and keyword and value
          for idx = (or (char-position #\, string s end) end)
          do (multiple-value-setq (keyword value) (funcall header-value-parser string s idx))
          collect keyword
          collect value
          while (< idx end)
          do (setq s (first-non-blank (the fixnum (1+ idx)) end)))))

(defun print-comma-separated-header-plist (header-value-plist stream &optional (string-generator #'string)
                                                              (fill-column *header-fill-column*))
  "Applies STRING-GENERATOR to each value in header-value-plist to write header value to STREAM."
  (loop with position fixnum = 0
        for (keyword value) on header-value-plist by #'cddr
        for item = (funcall string-generator keyword value)
        for count fixnum upfrom 0
        for len = (the fixnum (length item))
        unless (zerop count)
          do (fast-format stream ", ")
             (the fixnum (incf position (the fixnum 2)))
        when (> (the fixnum (+ position len 1)) fill-column)    ;don't exceed specified fill column
          do (fast-format stream "~% ")
             (setf position 1)
        do (write-string item stream :end len)
           (the fixnum (incf position len))))

(defun parse-comma-separated-keywords (string &optional (start 0) (end (length string)))
  (parse-comma-separated-header string start end #'parse-keyword-header))

(defun print-comma-separated-keywords (header-value-list stream)
  (print-comma-separated-header header-value-list stream #'string-for-tokenized-header-keyword))

(defun print-equal-sign-delimited-pair (key value stream &optional (escape-p nil) (require-value-p t))
  (cond ((and (eql value t) (not require-value-p))
         (write-string (string-for-tokenized-header-keyword key) stream))
        (t (write-string (string-for-tokenized-header-keyword key) stream)
           (write-char #\= stream)
	   (typecase value
	     (keyword
	       (write-string (string-for-tokenized-header-keyword value) stream))
	     (function (funcall value stream))
	     (t (write value :stream stream :escape escape-p :base 10))))))

(defun parse-equal-sign-delimited-pair (string &optional (start 0) (end (length string)) (header-value-parser #'subseq)
                                               (require-value-p t))
  (declare (values keyword value))
  (flet ((%get-value (string delimiter end)
           (loop with v-pos = (1+ (the fixnum delimiter))
                 for idx upfrom v-pos below end
                 for char = (aref string idx)
                 unless (or (digit-char-p char)
                            (white-space-char-p char))
                   do (return-from %get-value (funcall header-value-parser string v-pos end))
                 finally (return-from %get-value (parse-integer string :start v-pos :end end :junk-allowed t)))))
    (declare (inline %get-value))
    (let* ((delim-pos (char-position #\= string start end))
           (pos (position-if-not* #'white-space-char-p string :start start :end (or delim-pos end) :from-end t)))
      (cond (delim-pos
             (values (%tokenize-header-keyword string start (1+ (the fixnum pos)))
                     (%get-value string delim-pos end)))
            (require-value-p
             (error "No = delimiter found in the equal sign delimited pair, ~A" (subseq string start end)))
            (t (values (%tokenize-header-keyword string start (1+ (the fixnum pos))) t))))))

(defun print-equal-sign-delimited-pairs (plist stream &optional (delimiter #\;) (escape-p nil) (require-value-p t))
  (loop for (keyword value . more) = plist then more
        do (print-equal-sign-delimited-pair keyword value stream escape-p require-value-p)
        while more
        do (write-char delimiter stream)))

(defun parse-equal-sign-delimited-pairs (string &optional (start 0) (end (length string)) (delimiter #\;) (require-value-p t))
  (flet ((value-getter (string start end)
           (triming-substring '(#\" #\space #\tab) string start end)))
    (loop with keyword and value 
          for s = start then (1+ (the fixnum e))
          while (< s end)
          for s1 = (position-if-not* #'white-space-char-p string :start s :end end)
	  while s1
          for e = (or (char-position delimiter string s1 end) end)
          do (multiple-value-setq (keyword value)
                 (parse-equal-sign-delimited-pair string s1 e #'value-getter require-value-p))
          collect keyword
          collect value)))

(defun sort-parsed-quality-pairs (parsed-values)
  "Sorts PARSED-VALUES of a header according to quality values."
  (stable-sort parsed-values #'> :key #'cdr))

;; Consider changing the representation of qualities from floats to integers
;; if it makes sense for efficiency.  7/3/96 -- JCMa.
(defun parse-comma-separated-quality-pairs (string &optional (start 0) (end (length string)))
  (declare (optimize (speed 3)))
  (flet ((intern-quality-pair (string &optional (start 0) (end (length string)))
	   (let* ((pos (char-position #\; string start end t))
		  (q-pos (and pos (char-position #\= string (1+ (the fixnum pos)) end t)))
		  (key (%tokenize-header-keyword string start (or pos end)))
		  (val (if q-pos (parse-quality-value string (1+ (the fixnum q-pos)) end) 1.0)))
	     (list* key val))))
    (parse-comma-separated-header string start end #'intern-quality-pair)))

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

;; used by the TE header   5/21/98 -- JCMa.
(defun print-comma-separated-quality-pair-or-tokens (header-value-list stream &aux string)
  (unwind-protect
      (flet ((quality-pair-string (quality-pair)
	       (destructuring-bind (keyword &rest quality) quality-pair
		 (if (and quality (not (= (the number quality) 1.0)))
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

;;;------------------------------------------------------------------- 
;;;
;;; AUTHENTICATION HEADER PARSING AND PRINTING
;;; 

(defgeneric write-www-authenticate-header-value (stream method realm &optional args))

;; basic method uses the default method
(defmethod write-www-authenticate-header-value (stream method realm &optional args)
  (declare (ignore args))
  (let ((pname (string-for-tokenized-header-keyword method))
        (realm-string (realm-name realm)))
    (write-string pname stream)
    (write-char #\space stream)
    (write-string "realm=" stream)
    (write realm-string :escape t :stream stream)))

; old version   7/13/96 -- JCMa.
;(defmethod write-www-authenticate-header-value :after (stream (method (eql :digest)) realm &optional args)
;  (declare (ignore realm))
;  (let ((nonce-opaque (digest-nonce-opaque-pair)))
;    (declare (dynamic-extent nonce-opaque))
;    (write-string ", nonce=\"" stream)
;    (write-string (car nonce-opaque) stream)
;    (write-string "\", opaque=\"" stream)
;    (write-string (cdr nonce-opaque) stream)
;    (write-char #\" stream)
;    (when (getf args :stale) 
;      (write-string ", stale=\"TRUE\"" stream))))

;; Digest add some parameters.
(defmethod write-www-authenticate-header-value :after (stream (method (eql :digest)) realm &optional args)
  (declare (ignore realm))
  (let ((nonce-opaque (digest-nonce-opaque-pair)))
    (declare (dynamic-extent nonce-opaque))
    (destructuring-bind (nonce . opaque) nonce-opaque
      (write-string ", " stream)
      (print-equal-sign-delimited-pair :nonce nonce stream t) 
      (write-string ", " stream)
      (print-equal-sign-delimited-pair :opaque opaque stream t)
      (loop for (keyword value) on args by #'cddr
            do (ecase keyword
                 (:algorithm
                   (write-string ", " stream)
                   (print-equal-sign-delimited-pair keyword (string-for-tokenized-header-keyword value) stream t))
                 (:domain
                   (write-string ", " stream)
                   (print-equal-sign-delimited-pair keyword (url:coerce-url-string value) stream t))
                 (:stale
                   (when value
                     (write-string ", " stream)
                     (print-equal-sign-delimited-pair :stale "TRUE" stream t)))
                 ((:nonce :opaque)))))))

(defun print-www-authenticate-header (spec stream)
  (destructuring-bind (method realm &rest args) spec
    (write-www-authenticate-header-value stream method realm args)))

;; Not so efficient or slick.  Fix sometime.   7/13/96 -- JCMa.
(defun parse-www-authenticate-header (string &optional (start 0) (end (length string)))
  (flet ((parse-domain (string &optional (start 0) (end (length string)))
           (flet ((get-uri (string start end)
                    (let ((s (position-if-not* #'(lambda (ch) (member ch '(#\< #\space #\") :test #'eql))
                                              string :start start :end end))
                          (e (position-if-not* #'(lambda (ch) (member ch '(#\> #\space #\") :test #'eql))
                                              string :start start :end end :from-end t)))
                      (subseq string s e))))
             (parse-comma-separated-header string start end #'get-uri))))
    (declare (inline parse-domain))
    (with-string-trim-bounds (*white-space-chars* string start end)
      (let* ((pos (position-if* #'white-space-char-p string :start start :end end))
	     (method (%tokenize-header-keyword string start pos))
	     (args (parse-equal-sign-delimited-pairs string (the fixnum (1+ pos)) end #\,)))
	(loop with realm
	      for (keyword value) on args by #'cddr
	      when (eq keyword :realm)
		do (setq realm (string-trim '(#\") value))
	      else
		collect keyword into plist
		and
	      collect (case keyword
			(:stale (equalp value "TRUE"))
			(:algorithm
			  (let ((s 0)
				(e (length value)))
			    (with-string-trim-bounds ('(#\") value s e) 
			      (%tokenize-header-keyword value s e))))
			(:domain (parse-domain value))                  
			(t (string-trim '(#\") value)))
		into plist
	      finally (return `(,method ,realm ,.plist)))))))

(defmethod %parse-authorization-header (string method &optional (start 0) (end (length string)))
  (declare (ignore start end))
  (error 'unknown-authentication-method :authentication-method method
	 :format-string "Unknown Authentication Method: ~S"
	 :format-args (list (if (keywordp method) (string-for-tokenized-header-keyword method) method))))

(defmethod %parse-authorization-header (string (method (eql :basic)) &optional (start 0) (end (length string)))
  `(,method ,(subseq string start end)))

(defmethod %parse-authorization-header (string (method (eql :digest)) &optional (start 0) (end (length string)))
  (flet ((value-getter (string start end)
           (triming-substring '(#\" #\space #\tab) string start end))
	 (value-end-index (string start end)
	   (declare (fixnum start end))
	   (let* ((p1 (char-position #\= string start end))
		  (p2 (position-if-not* #'white-space-char-p string :start (1+ (the fixnum p1)) :end end))
		  p3)
	     (if (eql (char string p2) #\")
		 (and (setq p3 (char-position #\" string (1+ (the fixnum p2)) end))
		      (char-position #\, string (1+ (the fixnum p3)) end))
               (char-position #\, string p1 end)))))
    (declare (inline value-end-index))
    (loop with keyword and value
	  for s = start then (1+ (the fixnum e))
	  while (< s end)
	  for s1 = (position-if-not* #'white-space-char-p string :start s :end end)
	  while s1
	  for e = (or (value-end-index string s1 end) end)
	  do (multiple-value-setq (keyword value)
                 (parse-equal-sign-delimited-pair string s1 e #'value-getter t))
	  collect keyword into plist
	  collect value ;do not unescape URI because it shows what the client used in computing digests for digest authentication -- JCMa 6/19/2006
          into plist
	  finally (return `(,method . ,plist)))))
     
(defun parse-authorization-header (string &optional (start 0) (end (length string)))
  (with-string-trim-bounds (*white-space-chars* string start end)
    (unless (= start end)
      (let* ((pos (position-if* #'white-space-char-p string :start start :end end))
	     (method (%tokenize-header-keyword string start (or pos pos))))
	(%parse-authorization-header string method (if pos (the fixnum (1+ pos)) end) end)))))

(defmethod %print-authorization-header ((method (eql :basic)) spec stream)
  (destructuring-bind (authorization) spec
    (write-string (string-for-tokenized-header-keyword method) stream)
    (write-char #\space stream)
    (write-string authorization stream)))

(defmethod %print-authorization-header ((method (eql :digest)) spec stream)
  (write-string (string-for-tokenized-header-keyword method) stream)
  (write-char #\space stream)
  (loop for (keyword value . more) = spec then more
        for val = (case keyword
                    (:uri (etypecase value
                            (string value)
                            (url (relative-name-string value))))
                    (t value))
        do (print-equal-sign-delimited-pair keyword val stream t t)
        while more
        do (write-char #\, stream)))

(defun print-authorization-header (spec stream)
  (%print-authorization-header (car spec) (cdr spec) stream))

(defun parse-authentication-info-header (string &optional (start 0) (end (length string)))
  (with-string-trim-bounds (*white-space-chars* string start end)
    (parse-equal-sign-delimited-pairs string start end #\,)))

(defun print-authentication-info-header (spec stream)
  (destructuring-bind (&key nextnonce digest) spec
    (unless (or nextnonce digest)
      (error "No values for AUTHENTICATION-INFO header."))
    (cond-every
      (nextnonce
	(print-equal-sign-delimited-pair :nextnounce nextnonce stream t))
      (digest
	(when nextnonce
	  (write-string ", " stream)
	  (print-equal-sign-delimited-pair :digest digest stream t))))))

;;;------------------------------------------------------------------- 
;;;
;;; PARSING FORWARDED HEADERS
;;;

;; case where origin host concealed by proxy
;;" by http://humes.ih.att.com:8000 (Netscape-Proxy/1.1)"   6/29/95 -- JCMa.

;; value:: (origin proxy &optional port proxy-product)
(defun parse-forwarded-header (string &optional (start 0) (end (length string)) &aux pos1 pos2 s-proxy e-proxy s-origin)
  (flet ((get-port (string port-delim e-proxy)
	   (declare (fixnum))
	   (when port-delim
	     (let ((s-port (1+ (the fixnum port-delim))))
	       (unless (eql s-port e-proxy)
		 (list (parse-integer string :start s-port :end e-proxy :radix 10.))))))
	 (get-product (string start end &aux s-product e-product)
	   (declare (fixnum start))
	   (when (and (setq s-product (char-position #\( string (1+ start) end))
		      (setq e-product (char-position #\) string (1+ (the fixnum s-product)) end)))
	     (list (subseq string (1+ (the fixnum s-product)) e-product))))
	 (get-origin (string start end &aux pos7 e-origin)
	   (cond ((and (setq pos7 (string-search "for " string 0 3 start end))
		       (setq s-origin (position-if-not* #'white-space-char-p string :start (+ 4 (the fixnum pos7)) :end end))
		       (setq e-origin (1+ (the fixnum (position-if-not* #'white-space-char-p string :start s-origin :end end :from-end t)))))
		  (subseq string s-origin e-origin))
		 (t nil))))
    (declare (inline get-port get-product get-origin))
    (multiple-value-bind (scheme-prefix scheme-prefix-length)
        (url:%scheme-prefix-for-protocol *standard-protocol*)
      (with-string-trim-bounds (*white-space-chars* string start end)
        (cond
         ((and (setq pos1 (string-search "by " string 0 3 start end))
               (setq pos2 (string-search scheme-prefix string 0 scheme-prefix-length (+ 3 (the fixnum pos1)) end))
               (setq s-proxy (+ (the fixnum scheme-prefix-length) (the fixnum pos2)))
               (setq e-proxy (position-if* #'(lambda (ch) (member ch '(#\/ #\space) :test #'eql)) string :start s-proxy :end end)))
          (let ((port-delim (char-position #\: string s-proxy e-proxy)))
            `(,(get-origin string (1+ e-proxy) end)
              ,(subseq string s-proxy (or port-delim e-proxy))
              ,.(get-port string port-delim e-proxy)
              ,.(get-product string e-proxy (or s-origin end)))))
         (t (error "Bad syntax in the forwarded header: ~S." string)))))))

(defun print-forwarded-header (value stream)
  (destructuring-bind (origin proxy &optional port proxy-product) value
    (fast-format stream "by ~A~A" (url:%scheme-prefix-for-protocol *standard-protocol*) (host-domain-name proxy))
    (when (and port (/= port (url:protocol-default-port *standard-protocol*)))
      (write-char #\: stream)
      (write port :base 10 :escape nil :stream stream))
    (write-char #\/ stream)
    (when proxy-product
      (fast-format stream " (~A)" proxy-product))
    (fast-format stream " for ~A" (host-domain-name origin))))

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

(defparameter *elide-http-version-in-via-header* nil
  "Controls whether http protocol version elide the protocol name.")

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
			   (aref string (the fixnum (1- s4))) #\()
		     (copy-vector-portion comment 0 l4 string s4 e4)
		     (setf (aref string e4) #\))))
		 string))))
      (declare (dynamic-extent #'entry-string))
      (print-comma-separated-header header-value-plist stream #'entry-string)))) 


;;;------------------------------------------------------------------- 
;;;
;;; PARSING & PRINTING CACHE CONTROL HEADERS
;;;

(defun parse-cache-control-header (string &optional (start 0) (end (length string))) 
  (labels ((parse-quoted-keyword-sequence (string &optional (start 0) (end (length string)))
	     (let ((open-pos (char-position #\" string start end))
		   (close-pos (char-position #\" string start end t)))
	       (parse-comma-separated-keywords string (1+ (the fixnum open-pos)) close-pos)))
           (parse-directive (string &optional (start 0) (end (length string)))
	     (with-string-trim-bounds (*white-space-chars* string start end)
	       (let* ((pos (%fast-position-if header-special-character-p string :start start :end end))
		      (directive (%tokenize-header-keyword string start (or pos end))))
		 (case directive
		   ((:no-store :only-if-cached :public :no-transform :must-revalidate :proxy-revalidate)
		    (list directive t))
		   ((:max-age :min-fresh)
		    (multiple-value-list
		      (parse-equal-sign-delimited-pair string start end #'parse-integer-header)))
		   (:max-stale
		     (if (and pos (char-position #\= string pos end))
			 (multiple-value-list
			   (parse-equal-sign-delimited-pair string start end #'parse-integer-header))
			 (list directive t)))
		   ((:no-cache :private)
		    (if (and pos (char-position #\= string pos end))
			(multiple-value-list
			  (parse-equal-sign-delimited-pair string start end #'parse-quoted-keyword-sequence))
			(list directive t)))
		   (t (if (and pos (char-position #\= string pos end))
			  (let ((open-pos (char-position #\" string start end)))
			    (if open-pos 
				(list directive (subseq string (1+ (the fixnum open-pos))
							(char-position #\" string start end t)))
				(multiple-value-list
				  (parse-equal-sign-delimited-pair string start end #'parse-keyword-header))))
			  (list directive t))))))))
    (with-fast-array-references ((string string string))
      (loop with escape and first = start
	    for idx fixnum upfrom start below end
	    for char = (aref string idx)
	    do (case char
		 (#\" (setq escape (not escape))))
	    when (and (not escape) (eql char #\,))
	      nconc (prog1 (parse-directive string first idx)
			   (setq first (1+ idx)))
		into result
	    finally (return (nconc result (parse-directive string first end)))))))

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
                            (ensure-string-buffer string)
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


;;;------------------------------------------------------------------- 
;;;
;;; EXPECT HEADER
;;;

(defun print-expect-header (plist stream)
  (print-equal-sign-delimited-pairs plist stream #\, t nil))

(defun parse-expect-header (string &optional (start 0) (end (length string)))
  (flet ((parse-entry (string start end)
	   (flet ((parse-value (string start end)
		    (flet ((quote-char-p (ch)
			     (eql ch #\")))
		      (declare (inline quote-char-p))
		      (let* ((s (position-if-not* #'quote-char-p string :start start :end end))
			     (e (1+ (the fixnum (position-if-not* #'quote-char-p string :start s :end end :from-end t)))))
			(%tokenize-header-keyword string s e)))))
	     (declare (inline parse-value))
	     (parse-equal-sign-delimited-pair string start end #'parse-value nil))))
    (parse-comma-separated-header string start end #'parse-entry)))


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

(defun print-mail-addresses (mail-address-list stream)
  (print-comma-separated-header mail-address-list stream #'url:full-email-address))

;;;------------------------------------------------------------------- 
;;;
;;; MIME CONTENT-TYPE HEADER
;;;

(define intern-keyword (string &optional (start 0) (end (length string)))
  (declare (fixnum start end))
  (let ((string (nstring-upcase (subseq string start end) :start 0 :end (the fixnum (- end start)))))
    (intern string *keyword-package*)))

;; must be escaped in parameters
(defparameter *mime-tspecial-chars* '(#\( #\) #\< #\> #\@ #\, #\; #\: #\\ #\" #\/ #\[ #\] #\? #\=))

(defun mime-valid-char-for-token-p (char)
  (not (or (eql char #\space)
           (not (zerop (www-utils:char-bits char)))     ;no control characters
           (member char *mime-tspecial-chars* :test #'eql))))

(declaim (inline position-valid-mime-char))

(defun position-valid-mime-char (string start end)
  (or (position-if* #'mime-valid-char-for-token-p string :start start :end end) end))

(declaim (inline position-invalid-mime-char))

(defun position-invalid-mime-char (string start end)
  (or (position-if-not* #'mime-valid-char-for-token-p string :start start :end end) end))

(defun mime-experimental-token-p (token)
  (let ((string (etypecase token
                  (string token)
                  (symbol (symbol-name token)))))
    (and (< 2 (length string))
         (string-search "X-" string 0 2 0 2)
         t)))

;;;------------------------------------------------------------------- 
;;;
;;; DEFINING MIME CONTENT TYPES
;;;

(defvar *mime-content-type-major-types* nil)

(define mime-content-type-major-type-p (keyword)
  "Returns non-null if KEYWORD is a known major content type for MIME."
  (member keyword *mime-content-type-major-types* :test #'eq))

(defun %define-mime-content-type (major-type minor-types &optional parameter-specs)
  "Defines a mime-content type."
  (flet ((canonicalize-item (item)
           (let ((keyword (intern-keyword (string item))))
             (setf (get keyword 'mime-string) (string-downcase keyword))
             keyword)))
    (declare (dynamic-extent #'canonicalize-item))
    (let ((major (canonicalize-item major-type))
          (minor-types (mapcar #'canonicalize-item (ensure-list minor-types)))
          (params (loop for param-spec in parameter-specs
                        collect (mapcar #'canonicalize-item param-spec))))
      ;; record knowledge of the mime-content-type
      (pushnew major *mime-content-type-major-types*)
      ;; record minor types
      (if minor-types
          (setf (get major 'mime-content-type-minor-types) minor-types)
          (remprop major 'mime-content-type-minor-types))
      ;; record parameters for content type.
      (if params
          (setf (get major 'mime-content-type-parameters) params)
          (remprop major 'mime-content-type-parameters))
      major)))

(define-macro define-mime-content-type (major-type &key minor-types parameters)
  "Top-level method for defining a new mime content type."
  `(%define-mime-content-type ',major-type ',minor-types ',parameters))

(declaim (inline mime-content-type-minor-types))

(defun mime-content-type-minor-types (major-type)
  (get major-type 'mime-content-type-minor-types))

(declaim (inline mime-content-type-parameters))

(defun mime-content-type-parameters (major-type)
  (get major-type 'mime-content-type-parameters))

(declaim (inline mime-content-type-string))

(defun mime-content-type-string (mime-content-type-keyword)
  (or (get mime-content-type-keyword 'mime-string)
      (setf (get mime-content-type-keyword 'mime-string)
            (string-downcase mime-content-type-keyword))))

(defun undefine-mime-content-type (major-type)
  (check-type major-type keyword)
  (flet ((flush-mime-string (item)
           (remprop item 'mime-string)))
    (declare (dynamic-extent #'flush-mime-string))
    (let ((minor-types (mime-content-type-minor-types major-type))
          (parameters (mime-content-type-parameters major-type)))
      (mapc #'flush-mime-string minor-types)
      (loop for entry in parameters
            do (mapc #'flush-mime-string entry))
      (remprop major-type 'mime-content-type-minor-types)
      (remprop major-type 'mime-content-type-parameters)
      (flush-mime-string major-type))))

(define character-set-keywords-for-mime-content-type-text ()
  "Returns the valid character set keywords for HTTP."
  (cdr (assoc :charset (mime-content-type-parameters :text))))

(define valid-character-set-for-mime-content-type-text-p (charset)
  "Returns non-null if the CHARSET keyword is a valid character set for MIME content type TEXT."
  (not (null (member charset (character-set-keywords-for-mime-content-type-text)))))

(define check-character-set-for-mime-content-type-text (charset)
  "Ensures that the CHARSET keyword is a valid character set for MIME content type TEXT,
signalling an error when it is not."
  (unless (valid-character-set-for-mime-content-type-text-p charset)
    (error "For MIME content type text, ~S is not one of the valid character set keywords:~&~{~S~^, ~}."
           charset (character-set-keywords-for-mime-content-type-text))))

;;;------------------------------------------------------------------- 
;;;
;;; DEFINING MIME CONTENT TYPES
;;;
;;; http://www.iana.org/assignments/media-types/text/

(define-mime-content-type
  :application
  :minor-types (:octet-stream :postscript
                :lisp-sexp))                    ;lisp s-expression, client knows about these

(define-mime-content-type
  :audio
  :minor-types (:basic))

(define-mime-content-type
  :image
  :minor-types (:gif :jpeg :png :png :pict :tiff :ico))

(define-mime-content-type
  :message
  :minor-types (:rfc822 :partial :external-body))

(define-mime-content-type
  :multipart
  :minor-types (:mixed :parallel :digest :alternative :form-data :byteranges
                       :x-mixed-replace)        ;netscape server push extension   9/5/95 -- JCMa.
  :parameters ((:boundary)))

;; http://www.iana.org/assignments/character-sets
;; default charset for HTTP is  ISO-8859-1, which subsumes US-ASCII
(define-mime-content-type
  :text
  :minor-types (:plain :html)
  :parameters ((:charset :us-ascii :iso-8859-1 :iso-8859-2 :iso-8859-3 :iso-8859-4
                         :iso-8859-5 :iso-8859-6 :iso-8859-7 :iso-8859-8 :iso-8859-9
                         :iso-2022-jp :iso-2022-jp :iso-2022-kr 
                         :unicode-1-1 :unicode-2-2-utf-7 :unicode-2-2-utf-7
                         :utf-7 :utf-8 :utf-16 :utf-32
			 :koi8-r)))		; http://www.nagual.pp.ru/~ache/koi8/framed-koi.html

(define-mime-content-type
  :video
  :minor-types (:mpeg))

;; defined to support the macintalk plug-in talker
;; per http://www.mvpsolutions.com/PlugInSite/TalkerTutor.html
(define-mime-content-type
  :plugin
  :minor-types (:talker))

;; defined to support VRML
(define-mime-content-type
  :x-world
  :minor-types (:x-vrml :oogl :iv :x-3dmf))

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

(defun parse-number (string &optional (start 0) (end (length string)) (float-p nil float-supplied-p))
  "Parses a number which can be either and integer or a float."
  (if (or (and float-supplied-p float-p) (char-position #\. string start end))
      (parse-quality-value string start end)
      (parse-integer string :start start :end end :radix 10)))

(declaim (inline mime-header-parameter-index))

(defun mime-header-parameter-index (string &optional (start 0) (end (length string)))
  (char-position #\; string start end))

(defun parse-mime-header-parameters (string &optional (start 0) (end (length string)))
  (flet ((delimiter-p (char)
           (member char '(#\; #\space #\tab) :test #'eql))
	 (intern-value (key string s e)
	   (declare (fixnum s e))
	   (case key
	     ((:boundary :name :filename)	;parameters used in content-disposition for file upload 4/13/2001 -- JCMa.
	      (subseq string s e))		;some parameters are strings
	     (t (with-fast-array-references ((string string string))
		  (loop with quality-value-p
			for idx fixnum upfrom s below e
			for ch = (aref string idx)
			unless (or (digit-char-p ch)
				   (setq quality-value-p (eql ch #\.)))
			  return (%tokenize-header-keyword string s e)
			finally (return (parse-number string s e quality-value-p))))))))
    (declare (inline intern-value))
    (loop with param-key and param-value
	  for s = (position-if-not* #'delimiter-p string :start start :end end) then (the fixnum (1+ idx))
	  while (and s (< s end))
	  for idx fixnum = (or (char-position #\; string (1+ s) end) end)
          for delim of-type (or null fixnum) = (char-position #\= string (1+ s) idx)	; if this returns NIL, there is an error
	  for p1 = (and delim (position-valid-mime-char string (1+ delim) idx))
	  for p2 = (and delim (position-if* #'mime-valid-char-for-token-p string :start p1 :end idx :from-end t))
	  when p2
	    do (setq param-key (%tokenize-header-keyword string (position-valid-mime-char string s delim) delim))
	       (setq param-value (intern-value param-key string p1 (1+ (the fixnum p2))))
	    and collect param-key
	    and collect param-value)))

(defun print-mime-header-parameters (parameter-plist stream)
  (flet ((write-parameter-value (value stream)
           (typecase value
             (string
               (write value :stream stream :escape (not (every #'mime-valid-char-for-token-p value))))
             (number
               (write value :stream stream :base 10))
             (symbol
               (write (symbol-name value) :stream stream
                      :escape (not (every #'mime-valid-char-for-token-p (symbol-name value)))))
             (t (let ((string (write-to-string value :base 10. :escape nil)))
                  (declare (dynamic-extent string))
                  (write string :stream stream :escape (not (every #'mime-valid-char-for-token-p string))))))))
    (declare (inline write-parameter-value))
    (loop for (param value) on parameter-plist by #'cddr
          do (write-char #\; stream)
             (write-char #\space stream)
             (write (mime-content-type-string param) :stream stream :escape nil)
             (write-char #\= stream)
             (write-parameter-value value stream))))

(defun parse-mime-content-type (string &optional (start 0) (end (length string)))
  (declare (optimize (speed 3)))
  (let* ((subtype-index (char-position #\/ string start end))
	 (start-index (position-valid-mime-char string start (or subtype-index end)))
	 (major-type (and (or (null subtype-index) (< start-index (the fixnum subtype-index)))
			  (%tokenize-header-keyword string start-index (or subtype-index end)))))
    (declare (fixnum start-index))
    (cond ;; Content types normally have a major and minor type.
      (subtype-index
       (let* ((subtype-start (1+ (the fixnum subtype-index)))
	      (subtype-end (position-invalid-mime-char string subtype-start end)))
         (declare (fixnum subtype-start subtype-end))
	 (values major-type (and (< subtype-start subtype-end)
				 (%tokenize-header-keyword string subtype-start subtype-end)))))
      (t (values major-type nil)))))

(defun parse-mime-content-type-header (string &optional (start 0) (end (length string))
                                              &aux parameters)
  (declare (optimize (speed 3)))
  (with-string-trim-bounds (*white-space-chars* string start end)
    (let ((parameter-index (mime-header-parameter-index string start end)))
      (multiple-value-bind (type subtype)
	  (parse-mime-content-type string start (or parameter-index end))
	(when parameter-index
	  (setq parameters (parse-mime-header-parameters string (the fixnum (1+ parameter-index)) end)))
	(list* type subtype parameters)))))

(declaim (inline print-mime-content-type-header))

(defun print-mime-content-type-header (content-type stream)
  (destructuring-bind (type subtype . param-plist) content-type
    (when type
      (write (mime-content-type-string type) :stream stream :escape nil))
    (write-char #\/ stream)
    (when subtype
      (write (mime-content-type-string subtype) :stream stream :escape nil))
    (when param-plist
      (print-mime-header-parameters param-plist stream))))

(defun write-mime-content-type (content-type &optional stream)
  (with-string-for-null-stream (stream)
    (print-mime-content-type-header content-type stream)))

(defun write-mime-content-type-as-string (content-type &optional stream)
  (write-char #\" stream)
  (print-mime-content-type-header content-type stream)
  (write-char #\" stream))

(defun write-mime-content-type-keyword-as-string (content-type-keyword &optional stream)
  (write-char #\" stream)
  (print-mime-content-type-header (http::%mime-content-type-spec content-type-keyword) stream)
  (write-char #\" stream))

(defun parse-mime-content-disposition-header (string &optional (start 0) (end (length string))
                                                     &aux parameters)
  (with-string-trim-bounds (*white-space-chars* string start end)
    (let* ((parameter-index (mime-header-parameter-index string start end))
	   (disposition (%tokenize-header-keyword string start (or parameter-index end))))
      (when parameter-index
	(setq parameters (parse-mime-header-parameters string (the fixnum (1+ (the fixnum parameter-index))) end)))
      (list* disposition parameters))))

(defun print-mime-content-disposition-header (disposition-spec stream)
  (destructuring-bind (disposition . param-plist) disposition-spec
    (write (mime-content-type-string disposition) :stream stream :escape nil)
    (when param-plist
      (print-mime-header-parameters param-plist stream))))

(defun atomic-valued-header-series-value-p (value)
  (consp (car value)))

(defun list-valued-header-series-value-p (value)
  (and (consp (car value))
       (consp (caar value))))

(defun header-value-max (value)
  (apply #'max value))

(defun header-value-min (value)
  (apply #'min value)) 

(defun parse-mime-content-type-sequence-header (string &optional (start 0) (end (length string)))
  (parse-comma-separated-header string start end #'parse-mime-content-type-header))

(declaim (notinline print-mime-content-type-header))

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

(defun print-uri-header (uri-spec stream)
  (flet ((write-uri (uri stream)
           (write-char #\< stream)
           (url:write-name uri stream)
           (write-char #\> stream))
         (write-header-name (header stream)
           (write-string (or (header-print-name header) (symbol-name header)) stream)))
    (declare (inline write-uri write-header-name))
    (labels ((write-full-uri-spec (spec stream)
               (destructuring-bind (uri &rest headers) spec
                 (write-uri uri stream)
                 (when headers
                   (write-string ";vary=\"" stream)
                   (loop initially (write-header-name (first headers) stream)
                         for hdr in (cdr headers)
                         do (write-char #\, stream)
                            (write-header-name hdr stream))
                   (write-char #\" stream)))))
      (declare (dynamic-extent #'write-full-uri-spec))
      (typecase uri-spec
        (cons
          (typecase (car uri-spec)
            (cons 
              (loop for items = uri-spec then (cdr items)
                    while items
                    for (item) = items
                    do (typecase item
                         (cons (write-full-uri-spec item stream))
                         (t (write-uri item stream)))
                       (when (cdr items)
                         (write-char #\, stream))))
            (t (write-full-uri-spec uri-spec stream))))
        (t (write-uri uri-spec stream))))))

(defun parse-uri-header (string &optional (start 0) (end (length string)))
  (labels ((signal-error (problem string start end)
	     (error 'bad-syntax-provided :url (server-url *server*)
		    :format-string "Bad syntax in URI header: ~A in ~S."
		    :format-args (list problem (subseq string start end))))
	   (parse-vary (string start end &aux pos1)
             (when (and (string-search= ";vary=" string 0 6 start end)
                        (setq pos1 (char-position #\" string (+ start 6) end)))
               (let ((pos2 (char-position #\" string (1+ pos1) end t)))
                 (cond (pos2
                        (loop for s1 = (1+ pos1) then (1+ e1)
                              for e1 = (or (char-position #\, string s1 pos2) pos2)
                              collect (%tokenize-header-keyword string s1 e1)
                              until (eql e1 pos2)))
                       (t (signal-error "Missing closing \"" string start end))))))
           (parse-entry (string s1 e1)
             (let ((e2 (char-position #\> string s1 e1)))
               (cond (e2
                      `(,(subseq string (1+ s1) e2) ,.(parse-vary string (1+ e2) e1)))
                     (t (signal-error "Missing closing >" string s1 e1))))))
    (declare (inline parse-entry parse-vary))
    (with-string-trim-bounds (*white-space-chars* string start end)
      (let ((s1 (char-position #\< string start end)))
	(cond (s1
	       (loop for s = s1 then e
		     for e = (char-position #\< string (1+ s) end)
		     collect (parse-entry string s (or e end))
		     while e))
	      ((or (char-position #\> string start end)
		   (string-search= ";vary=" string 0 6 start end))
	       (signal-error "Missing openning <" string start end))
	      ;; handle losers robustly
	      (t (list (subseq string start end))))))))

;; If called when (or *client* *server*), relative URLs are resolved against
;; the current url contained by these objects. 11/18/99 -- JCMa.
(defun parse-location-header (string &optional (start 0) (end (length string)))
  (with-string-trim-bounds (*white-space-chars* string start end)
    (with-bad-escaping-resignalled (string :start start :end end :reason "Bad Escaping: Ill-Formed Location Header")
      (multiple-value-bind (nstring unescaped-p new-string-p)
	  (string-unescape-special-chars string start end)	;handle broken Microsoft location headers   4/21/97 -- JCMa.
	unescaped-p				;ignore
        (flet ((relative-object ()
		 (or (and (boundp '*client*) (symbol-value '*client*))	;defined by the client not the server
		     *server*)))
	  (declare (inline relative-object))
	  (let ((relative-object (relative-object)))
	    (handler-case-if relative-object
	       (if new-string-p
		   (url:intern-url nstring :if-does-not-exist :uninterned)
		   (url:intern-url string :start start :end end :if-does-not-exist :uninterned))
	      (url::no-scheme-found ()
				    (url:intern-url (if new-string-p
							(url::%merge-relative-url nstring (name-string relative-object))
							(url::%merge-relative-url string (name-string relative-object) start end))
						    :if-does-not-exist :uninterned)))))))))

(defun print-location-header (uri-spec stream)
  (write-url-remapping-context uri-spec stream nil))

(defun parse-keep-alive-header (string &optional (start 0) (end (length string)))
  (with-string-trim-bounds (*white-space-chars* string start end)
    (if (with-fast-array-references ((string string string))	;Buggy Mozilla clients can have just a number 
	  (loop for idx from start below end	;according to RJ -- JCMa 9/9/2003
		always (digit-char-p (aref string idx))))
	;; guessing that the omitted keyword is the maximum number of requests on the connection -- JCMa 9/9/2003
	(list (%tokenize-header-keyword "max" 0 3) (parse-integer string :start start :end end :junk-allowed nil))
	;; standard approach
	(parse-comma-separated-header-plist string start end #'parse-equal-sign-delimited-pair))))

(defun print-keep-alive-header (spec stream)
  (destructuring-bind (&key (timeout *persistent-connection-timeout*)
			    (max *persistent-connection-maximum-requests*))
      spec
    (cond-every
      (timeout
	(fast-format stream "timeout=~D" timeout))
      ((and max timeout)
       (fast-format stream ", "))
      (max
	(fast-format stream "max=~D" max)))))

(defun parse-host-header (string &optional (start 0) (end (length string)))
  (with-string-trim-bounds (*white-space-chars* string start end)
    (unless (= start end)
      (multiple-value-bind (port-number host-end)
          (url::get-port-info string start end)
        `(,(subseq string start host-end) ,(or port-number (url:protocol-default-port *standard-protocol*)))))))

(defun print-host-header (value stream)
  (when value
    (destructuring-bind (host &optional port) value
      (cond ((= port (url:protocol-default-port *standard-protocol*))
             (write-string host stream))
            (t (fast-format stream "~A:~D" host port))))))

(defun parse-content-range-header (string &optional (start 0) (end (length string)))
  (with-string-trim-bounds (*white-space-chars* string start end)
    (let (pos1 start-pos pos2 last-pos pos3 length-pos)
      (cond ((and (setq pos1 (char-position #\space string start end))
		  (setq start-pos (1+ (the fixnum pos1)))
		  (setq pos2 (char-position #\- string start-pos end))
		  (setq last-pos (1+ (the fixnum pos2)))
		  (setq pos3 (char-position #\/ string last-pos end))
		  (setq length-pos (1+ (the fixnum pos3))))
	     `(,(%tokenize-header-keyword string start pos1)
	       ,(parse-integer string :start start-pos :end pos2 :radix 10)	;start postion
	       ,(parse-integer string :start last-pos :end pos3 :radix 10)	;last position
	       ,.(unless (eql #\* (aref string length-pos))	;entity length unknown
		 (list (parse-integer string :start length-pos :end end :radix 10)))))	;entity length
	    (t (error 'bad-range-header-value :format-string "Bad value for Content-Range header: ~S"
		      :format-args (list (subseq string start end))))))))

(defun print-content-range-header (value stream)
  (destructuring-bind (unit-type start-pos last-pos &optional (entity-length #\*)) value
    (fast-format stream "~A ~D-~D/~D" (string-for-tokenized-header-keyword unit-type)
                 start-pos last-pos (or entity-length #\*))))

;; RFC 2616 says a an invalid range specs requires the whole header to be ignored. 10/5/99 -- JCMa.
 (defun parse-range-header (string &optional (start 0) (end (length string)))
  (flet ((parse-range-entry (string s e)
           (let ((pos (char-position #\- string s e))
                 pos2 range-start range-end)
             (cond (pos
                    (setq pos2 (1+ (the fixnum pos)))
		    (handler-case-if (not *debug-server*) 
                        (unless-every
                         ((= pos s)
                          (setq range-start (parse-integer string :start s :end pos :radix 10)))
                         ((= pos2 e)
                          (setq range-end (parse-integer string :start pos2 :end e :radix 10))))
		      (error () (return-from parse-range-header nil)))
		    (when (or range-start range-end) ;may send first or last byte position
                      (list range-start range-end)))
                   (t (return-from parse-range-entry nil))))))
    (declare (inline parse-range-entry))
    (with-string-trim-bounds (*white-space-chars* string start end)
      (unless (= start end)
	(loop with pos = (char-position #\= string start end)
	      for s = (1+ (the fixnum pos)) then (1+ (the fixnum e))
	      for e = (or (char-position #\, string s end) end)
              for entry = (parse-range-entry string s e)
	      when entry
              collect entry into ranges
	      until (= e end)
	      finally (return (cons (%tokenize-header-keyword string start pos) ranges)))))))

(defun print-range-header (value stream)
  (destructuring-bind (unit-type . range-specs) value
    (loop initially (fast-format stream "~A=" (string-for-tokenized-header-keyword unit-type))
          for (range . more) = range-specs then more
          while range
          for (start last) = range 
          do (cond-every
               (start (write start :stream stream :base 10 :escape nil))
               (t (write-char #\- stream))
               (last (write last :stream stream :base 10 :escape nil))
               (more (write-char #\, stream))))))

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

;;;------------------------------------------------------------------- 
;;;
;;; HTTP HEADER TYPES
;;;

(define-header-type :header (:header)
  :parse-function parse-standard-header
  :print-function print-standard-header)

(define-header-type :keyword-header (:header)
  :parse-function parse-keyword-header
  :print-function print-keyword-header) 

(define-header-type :integer-header (:header)
  :parse-function parse-integer-header
  :print-function print-integer-header)

(define-header-type :version-header (:header)
  :parse-function parse-version-header
  :print-function print-version-header)

(define-header-type :date-header (:integer-header)
  :parse-function parse-date-header
  :print-function print-date-header
  :atomic-value-reducer first)

(define-header-type :authentication-header (:header))

(define-header-type :comma-separated-header (:header)
  :parse-function parse-comma-separated-header
  :print-function print-comma-separated-header
  :print-series-predicate atomic-valued-header-series-value-p)

(define-header-type :content-type-header (:header)
  :parse-function parse-mime-content-type-header
  :print-function print-mime-content-type-header)

;; need to add singleton parsing option for sequence type headers.
(define-header-type :content-type-sequence-header (:header)
  :parse-function parse-mime-content-type-sequence-header
  :print-function print-mime-content-type-sequence-header
  :print-series-predicate list-valued-header-series-value-p)

(define-header-type :keyword-sequence-header (:comma-separated-header)
  :parse-function parse-comma-separated-keywords
  :print-function print-comma-separated-keywords)

(define-header-type :mail-address-sequence-header (:comma-separated-header)
  :parse-function parse-mail-addresses
  :print-function print-mail-addresses)

;; Updated to modern spec, in which quality values are optional. 11/13/2003 -- JCMa.
(define-header-type :quality-pair-sequence-header (:comma-separated-header)
  :parse-function parse-comma-separated-quality-pairs
  :print-function print-comma-separated-quality-pair-or-tokens
  :print-series-predicate list-valued-header-series-value-p)

(define-header-type :via-header (:header)
  :parse-function parse-via-header
  :print-function print-via-header
  :print-series-predicate  list-valued-header-series-value-p)	; allow for multiple via headers 

(define-header-type :cache-control-header (:header)
  :parse-function parse-cache-control-header
  :print-function print-cache-control-header
  :print-series-predicate atomic-valued-header-series-value-p)	; allow for multiple cache-control headers

(define-header-type :entity-tag-header (:header)
  :parse-function parse-entity-tag-header
  :print-function print-entity-tag-header)

(define-header-type :entity-tag-sequence-header (:entity-tag-header)
  :parse-function parse-entity-tag-sequence-header
  :print-function print-entity-tag-sequence-header
  :print-series-predicate atomic-valued-header-series-value-p)


;;;------------------------------------------------------------------- 
;;;
;;; HTTP HEADERS
;;;

(define-header :accept
               (:content-type-sequence-header
                :request)
  :print-string "Accept")

(define-header :accept-charset
               (:quality-pair-sequence-header :request)
  :print-string "Accept-Charset")

(define-header :accept-encoding
               (:quality-pair-sequence-header :request)
  :print-string "Accept-Encoding")

(define-header :accept-language
               (:quality-pair-sequence-header :request)
  :print-string "Accept-Language")

(define-header :accept-ranges
               (:keyword-header :request)       ;missing from 1.1 spec p29
  :print-string "Accept-Ranges")

(define-header-keywords "bytes" "none")

(define-header :allow
               (:keyword-sequence-header :entity)
  :print-string "Allow")

(define-header :authorization                   ;RFC 2069
               (:authentication-header :request)
  :print-string "Authorization"
  :parse-function 'parse-authorization-header
  :print-function 'print-authorization-header)

(define-header-keywords "realm" "nonce" "username" "uri" "response" "digest" "algorithm" "opaque"
			"basic" "digest")

(define-header :authentication-info             ;RFC 2069
               (:authentication-header :response)
  :print-string "Authentication-Info"
  :parse-function 'parse-authentication-info-header
  :print-function 'print-authentication-info-header)

(define-header-keywords "nextnonce" "digest")

(define-header :cache-control
               (:cache-control-header :general)
  :print-string "Cache-Control")

(define-header-keywords "no-cache" "no-store" "max-age" "max-stale" "min-fresh" "only-if-cached" "public" 
                        "private" "no-transform" "must-revalidate" "proxy-revalidate")

(define-header :connection
               (:keyword-sequence-header :general)
  :print-string "Connection")

(define-header-keywords "close" "Upgrade" "Keep-Alive")   ; Keep-Alive is deprecated 1.0 extension

;; Obsolete Header  removed from the spec in RFC 2616
(define-header :content-base
               (:header :entity)
  :print-string "Content-Base")

(define-header :content-encoding
               (:keyword-header :entity)
  :print-string "Content-Encoding")

(define-header :content-disposition             ;not in 1.1
               (:header :entity)
  :print-string "Content-Disposition"
  :parse-function 'parse-mime-content-disposition-header
  :print-function 'print-mime-content-disposition-header)

(define-header :content-id                      ;not in 1.1
               (:header :entity)
  :print-string "Content-ID")

(define-header :content-language
               (:keyword-sequence-header :entity)
  :print-string "Content-Language")

(define-header :content-length
	       (:integer-header :entity)
  :print-string "Content-length"
  :atomic-value-reducer 'header-value-max)	; handles mulitple content-length headers

(define-header :content-location
               (:header :entity)
  :print-string "Content-Location"
  :parse-function 'parse-location-header
  :print-function 'print-location-header)

(define-header :content-md5
               (:header :entity)
  :print-string "Content-MD5")

(define-header :content-range
               (:header :entity)
  :print-string "Content-Range"
  :parse-function 'parse-content-range-header
  :print-function 'print-content-range-header)

;; This is the MIME version, not the HTTP 1.1 Transfer-Encoding
(define-header :content-transfer-encoding
               (:keyword-header :entity)        ;not in 1.1
  :print-string "Content-Transfer-Encoding")

(define-header :content-type
               (:content-type-header :entity)
  :print-string "Content-Type")

(define-header :content-version
               (:version-header :entity)        ;not in 1.1 spec p,31
  :print-string "Content-Version")

(define-header :date
               (:date-header :general)
  :print-string "Date"
  :atomic-value-reducer 'header-value-max
  :default 'gmt-time)

(define-header :derived-from
               (:version-header :entity)
  :print-string "Derived-From")

(define-header :etag
               (:entity-tag-header :entity)
  :print-string "ETag")

(define-header :expect
               (:header :request)
  :print-string "Expect"
  :print-function 'print-expect-header
  :parse-function 'parse-expect-header)

(define-header-keywords "100-continue")

(define-header :expires
               (:date-header :entity)
  :print-string "Expires"
  :parse-function 'parse-expires-header
  :print-function 'print-expires-header
  :atomic-value-reducer 'header-value-min)

;; value:: (origin proxy &optional port proxy-product)
(define-header :forwarded
               (:header :response)
  :print-string "Forwarded"
  :parse-function 'parse-forwarded-header
  :print-function 'print-forwarded-header)

(define-header :from
               (:mail-address-sequence-header :request)
  :print-string "From")

(define-header :host
               (:header :request)
  :print-string "Host"
  :parse-function 'parse-host-header
  :print-function 'print-host-header)

(define-header :if-match
               (:entity-tag-sequence-header :request)
  :print-string "If-Match")

(define-header :if-modified-since
               (:date-header :request)
  :print-string "If-Modified-Since"
  :atomic-value-reducer 'header-value-min)

(define-header :if-none-match
               (:entity-tag-sequence-header :request)
  :print-string "If-None-Match")

(define-header :if-unmodified-since
               (:date-header :request)
  :print-string "If-Unmodified-Since"
  :atomic-value-reducer 'header-value-min)

;; deprecated 1.0 extension header  6/29/96 -- JCMa.
(define-header :keep-alive
               (:comma-separated-header :request)
  :print-string "Keep-Alive"
  :parse-function 'parse-keep-alive-header
  :print-function 'print-keep-alive-header)

(define-header :last-modified
               (:date-header :entity)
  :print-string "Last-Modified"
  :atomic-value-reducer 'header-value-min)

(define-header :location (:header :response)
  :print-string "Location"
  :parse-function 'parse-location-header
  :print-function 'print-location-header)

(define-header :max-forwards
               (:integer-header :request)
  :print-string "Max-Forwards")

(define-header :method
               (:comma-separated-header :unknown)
  :print-string "Method")

(define-header :mime-version
               (:header :entity)
  :print-string "MIME-version")

(define-header :pragma
               (:keyword-header :general)
  :print-string "Pragma")

(define-header-keywords "no-cache")

(define-header :proxy-authenticate                ;RFC 2069
               (:authentication-header :response)
  :print-string "Proxy-Authenticate"
  :parse-function 'parse-www-authenticate-header
  :print-function 'print-www-authenticate-header)

(define-header :proxy-authorization                   ;RFC 2069
               (:authentication-header :request)
  :print-string "Proxy-Authorization"
  :parse-function 'parse-authorization-header
  :print-function 'print-authorization-header)

(define-header :proxy-connection		;deprecated 1.0 Extension
               (:keyword-sequence-header :general)
  :print-string "Proxy-Connection")

(define-header :public
               (:keyword-sequence-header :response)
  :print-string "Public")

(define-header-keywords  "delete" "get" "head" "options" "post" "put" "trace")

(define-header :range
               (:header :request)
  :print-string "Range"
  :parse-function 'parse-range-header
  :print-function 'print-range-header)

(define-header :referer
               (:header :request)
  :print-string "Referer")

(define-header :server 
               (:header :response)
  :print-string "Server"
  :default *server-version*)

(define-header :te
               (:quality-pair-sequence-header :request)
               :print-string "TE")

(define-header-keywords "chunked" "identity" "deflate" "gzip")

(define-header :trailer
               (:keyword-sequence-header :general)
  :print-string "Trailer")

(define-header :transfer-encoding
               (:keyword-header :general)
  :print-string "Transfer-Encoding")

(define-header :upgrade
               (:keyword-header :unknown)
  :print-string "Upgrade")

(define-header-keywords "TLS/1.0")

;; deprecated in HTTP 1.1
(define-header :uri (:header :entity)
  :print-string "URI"
  :parse-function 'parse-uri-header
  :print-function 'print-uri-header)

(define-header :User-Agent (:header :request)
  :print-string "User-Agent") 

;; old name replaced by content-version for 1.1
(define-header :version
               (:integer-header :entity)
  :print-string "Version")

(define-header :via
               (:via-header :general)
  :print-string "Via")

(define-header :www-authenticate                ;RFC 2069
               (:authentication-header :response)
  :print-string "WWW-Authenticate"
  :parse-function 'parse-www-authenticate-header
  :print-function 'print-www-authenticate-header)

(define-header-keywords "realm" "domain" "nonce" "opaque" "stale" "algorithm")

;; Digest authentication algorithms.
(define-header-keywords "md5" "sha")


;;;------------------------------------------------------------------- 
;;;
;;; APACHE EXTENSION HEADERS
;;;

;; Carries the original host header information as received by the reverse
;; proxy
(define-header :x-forwarded-host
               (:header :request)
  :print-string "X-Forwarded-Host"
  :parse-function 'parse-host-header
  :print-function 'print-host-header)

;; IP address of the forwarding reverse proxy
(define-header :x-forwarded-for
               (:header :general)
  :print-string "X-Forwarded-For")

;; Domain name of the forwarding reverse proxy
(define-header :x-forwarded-server
               (:header :general)
  :print-string "X-Forwarded-Server")


;;;------------------------------------------------------------------- 
;;;
;;; NETSCAPE EXTENSION HEADERS
;;;

;; http://home.netscape.com/eng/mozilla/2.0/relnotes/demo/target.html
;; this header can be sent to netscape with the name of the target window.
(define-header :window-target 
               (:header :response)
  :print-string "Window-Target")

(defconstant +cookie-value-trimmed-characters+ `(,@*white-space-chars* #\")
  "The characters trimmed off the front and back of cookie values.")

;; http://www.netscape.com/newsref/std/cookie_spec.html
(defun parse-cookie-header (string &optional (start 0) (end (length string)) error-p)
  (flet ((get-entry-indices (string start end)
	   (with-fast-array-references ((string string string))
	     (loop with key-delimiter
		   for idx upfrom start below end
		   for char = (aref  string idx)
		   do (case char
			(#\= (setq key-delimiter idx))
			(#\; (return (values key-delimiter idx))))
		   finally (return (values key-delimiter end))))))
    (declare (inline get-entry-indices))
    (with-string-trim-bounds (*white-space-chars* string start end)
      (loop with e1 and e2 and s2
	    for s = start then (1+ (the fixnum e2))
	    while (< s end)
	    for s1 = (position-if-not* #'white-space-char-p string :start s :end end)
	    while s1
	    do (multiple-value-setq (e1 e2) 
		 (get-entry-indices string s1 end))
	    when (and e1 (< s1 e1) (setq s2 (1+ (the fixnum e1))) (< s2 e2))
	      collect (%intern-header-keyword-value string s1 e1)
	      and collect (with-string-trim-bounds (+cookie-value-trimmed-characters+ string s2 e2)
                            (subseq string s2 e2))
	    else do (when error-p
		      (error 'bad-cookie-header-value
			     :format-string "No value for Cookie header: ~S"
			     :format-args (list (subseq string s1 e2)))))))) 

(defun print-cookie-header (plist stream)
  (loop for (keyword value) on plist by #'cddr
        do (fast-format stream "~A=~A;" (string-for-tokenized-header-keyword keyword) value)))

(define-header :cookie (:header :request)
  :print-string "Cookie"
  :parse-function 'parse-cookie-header
  :print-function 'print-cookie-header)

;; http://tools.ietf.org/html/rfc2965
(defun parse-set-cookie-header (string &optional (start 0) (end (length string)))
  (labels ((parse-cookie-value (string start end)
             (with-string-trim-bounds (+cookie-value-trimmed-characters+ string start end)
               (subseq string start end)))
           (parse-port-list (string start end)
             (loop with idx-e
                   for idx-s = (position-if* #'digit-char-p string :start start :end end)
                   then (position-if* #'digit-char-p string :start idx-e :end end)
                   while idx-s
                   do (setq idx-e (or (position-if-not* #'digit-char-p string :start idx-s  :end end) end))
                   collect (parse-integer string :start idx-s :end idx-e)))
           (parse-parameters (string start end)
             (when (< start end)
               (loop for s = start then (1+ e2)
                     while (< s end)
                     for s1 = (position-if-not* #'white-space-char-p string :start s :end end)
                     while s1
                     for e1 = (char-position #\= string s1 end)
                     for e2 = (or (char-position #\; string (or e1 s1) end) end)
                     for s2 = (if e1 (1+ e1) s1)
                     for keyword = (%intern-header-keyword-value string s1 (or e1 e2))
                     for value = (with-string-trim-bounds (+cookie-value-trimmed-characters+ string s2 e2)
                                   (ecase keyword
                                     ((:domain :path :comment :commenturl) (subseq string s2 e2))
                                     (:portlist (parse-port-list string s2 e2))
                                     ;; errors parsing dates mean expire the cookie similar to what happens in HTTP,
                                     ;; but there is no guidance in RFC 2109
                                     ;; 8/10/2000 -- JCMa.
                                     (:expires (parse-expires-header string s2 e2))
                                     ((:max-age :version) (parse-integer string :start s2 :end e2))
                                     ((:secure :discard) t)
                                     (:httponly t))) ;microsoft extension to protect against XXS --JCMa 11/14/2010
                     collect keyword
                     collect value))))
    (declare (inline parse-cookie-value parse-parameters))
    (with-string-trim-bounds (*white-space-chars* string start end)
      (let* ((e1 (char-position #\= string start end))
	     (e2 (or (char-position #\; string e1 end) end)))
	`(,(%intern-header-keyword-value string start e1)
	  ,(parse-cookie-value string (1+ e1) e2)
	  ,.(parse-parameters string (1+ e2) end))))))

(defun print-set-cookie-header (spec stream)
  (etypecase (car spec)
    (atom
     (destructuring-bind (keyword value &key expires max-age version portlist domain path comment commenturl 
                                  secure discard httponly) spec
       (fast-format stream "~A=~A;" (string-for-tokenized-header-keyword keyword) value)
       ;; parameters
       (cond-every
        (expires
         (fast-format stream "expires=~I;" (print-gmt-time stream expires #\-)))
        (max-age
         (fast-format stream "max-age=~D;" max-age))
        (version
         (fast-format stream "version=~D;" version))
        (portlist
         (fast-format stream "portlist=\"~I\";" 
                      (loop for ports = portlist then (cdr ports)
                            do (fast-format stream "~D" (car ports))
                            while (setq ports (cdr ports))
                            do (fast-format stream " "))))
        (domain
         (fast-format stream "domain=~A;" domain))
        (path
         (fast-format stream "path=~A;" (url:coerce-url-string path)))
        (comment
         (fast-format stream "comment=~A;" comment))
        (commenturl
         (fast-format stream "commenturl=\"~A\";" commenturl))
        (secure
         (fast-format stream "secure;"))
        (discard
         (fast-format stream "discard;"))
        (httponly
         (fast-format stream "httponly;")))))
    ;; Replicate the header because some (most) clients lose if they are collapsed. -- JCMa 2/19/2003.
    (cons (print-set-cookie-header (first spec) stream)
	  (loop for plist in (rest spec)
		do (fast-format stream "~&~A: " (%header-print-name :set-cookie))
                (print-set-cookie-header plist stream)))))

(define-header :set-cookie (:header :response)
  :print-string "Set-Cookie"
  :parse-function 'parse-set-cookie-header
  :print-function 'print-set-cookie-header
  :collapsable-p nil)

#-ecl
(declaim (ftype (function) url-p))

#+ecl
(declaim (ftype (function (t) t) url-p))

(define make-set-cookie-header-value (name value &key expires max-age version port-list domain path 
                                           comment comment-url secure discard http-only)
  "Creates a header value for use with the :SET-COOKIE header
  that will store a cookie named NAME with value VALUE on a client.  This value
  created with this function should be passed as the value of :SET-COOKIE using
  the ADDITIONAL-HEADERS argument to WITH-SUCCESSFUL-RESPONSE, and related
  macros.

  EXPIRES is a universal time when the cookie expires. MAX-AGE is the time in
  seconds before the cookie should be discarded. VERSION is the version of
  the state management specification. DOMAIN is the server domain name for
  which the cookie is valid, defaults to the server host name.  PATH is a
  relative URL denoting the range of URLs for DOMAIN for which the cookie is
  valid. The client tests to see if the current URL is spanned by PATH. PATH
  defaults to /. PORT-LIST is a list of ports over which use of the cookie
  should be limited.

  SECURE is a boolean value indicating whether the cookie should sent over
  insecure connections (i.e., non-SSL). DISCARD is a boolean value indicating
  the cookie should be discarded when the user agent exits. HTTP-ONLY is a
  boolean value indicated that the cookie should only be used for HTTP.

  COMMENT is a string describing what the cookie does. COMMENT-URL is a URL or
  string where more information about the cookie can be obtained.

  For each cookie, the name and the value must not exceed 4k bytes. Each domain
  name is limited to 20 cookies. When the 300 total cookies per client or 20
  cookies per domain name limit are exceeded, cookies are deleted by clients
  according to least recent usage. Servers may force cookies to be deleted by
  providing an expiration that is in the past.

  Applications may wish to use WRITE-TO-ARMOR-PLATED-STRING and
  READ-FROM-ARMOR-PLATED-STRING to protect lisp forms stored in the client.
  However, this encoding reduces the amount of data that can be store in a
  cookie by approximately 25 percent. Alternatively,
  STRING-ESCAPE-SPECIAL-CHARS and STRING-UNESCAPE-SPECIAL-CHARS may be used as a
  transfer encoding. "
  (check-type name keyword)
  (check-type domain (or null string))
  (check-type path (or null string (satisfies url-p)))
  (unless (> 4001 (+ (the fixnum (length (symbol-name name))) (the fixnum (length value))))
    (error "The combined size of NAME and VALUE exceed 4k bytes."))
  (let ((args nil))
    (cond-every
     (http-only
      (push t args)
      (push :httponly args))
     (discard
      (push t args)
      (push :discard args))
     (secure
      (push t args)
      (push :secure args))
     (comment
      (check-type comment string)
      (push comment args)
      (push :comment args))
     (comment-url
      (push (etypecase comment-url
              (string comment-url)
              (url (name-string comment-url)))
            args)
      (push :commenturl args))
     (path
      (check-type path (or null string (satisfies url-p)))
      (push path args)
      (push :path args))
     (domain
      (let ((string (etypecase domain
                      (symbol (symbol-name domain))
                      (string domain))))
        (unless (valid-domain-name-string-p string)
          (error "The domain name, ~A, is not valid." string))
        (push string args)
        (push :domain args)))
     (port-list
      (check-type port-list (cons (fixnum 0)))
      (push port-list args)
      (push :portlist args))
     (version
      (check-type version (fixnum 0))
      (push version args)
      (push :version args))
     (max-age 
      (check-type max-age (integer 0))
      (push max-age args)
      (push :max-age args))
     (expires
      (check-type expires integer)
      (push expires args)
      (push :expires args)))
    `(,name ,value ,.args)))

(define-macro set-cookie-http-headers (&rest specs)
  "Top-level Interface that returns a header plist to set client-side cookies based on specs.
SPECS is a list of (name value &key expires domain path secure) that
are expanded into calls to MAKE-SET-COOKIE-HEADER-VALUE."
  `(multiple-value-bind (current-user-agent current-user-agent-version)
       (current-user-agent)
     (when (http:user-agent-capability-p :cookies current-user-agent current-user-agent-version)
       (list ,.(loop for spec in specs
                     nconc (destructuring-bind (name value &key expires max-age version domain path port-list 
                                                     comment comment-url secure discard http-only) spec
                             `(:set-cookie (make-set-cookie-header-value
                                            ,name ,value
                                            ,.(when expires `(:expires ,expires))
                                            ,.(when max-age `(:max-age ,max-age))
                                            ,.(when version `(:version ,version))
                                            ,.(when domain `(:domain ,domain))
                                            ,.(when path `(:path ,path))
                                            ,.(when port-list `(:portlist ,port-list))
                                            ,.(when comment `(:comment ,comment))
                                            ,.(when comment-url `(:comment-url ,comment-url))
                                            ,.(when secure `(:secure ,secure))
                                            ,.(when discard `(:discard ,discard))
                                            ,.(when http-only `(:httponly ,http-only))))))))))

(define-macro with-cookie-values ((variables &key (headers '*headers*)) &body body)
  "Binds the variables in VARIABLES to the corresponding values of cookies
recieved from the client within BODY. Within BODY, HTTP:COOKIES is bound to a
property list of all cookies sent by the client and GET-COOKIE obtains the
value of a specific cookie denoted by its keyword argument.  The variable
HTTP:CURRENT-USER-AGENT and HTTP:CURRENT-USER-AGENT-VERSION are bound within
BODY."
  `(multiple-value-bind (current-user-agent current-user-agent-version)
       (current-user-agent)
     (when (http:user-agent-capability-p :cookies current-user-agent current-user-agent-version)
       (let* ((cookies (get-header :cookie ,headers))
              ,.(loop for var in variables
                      collect `(,var (getf cookies ,(intern-keyword (symbol-name var))))))
         (macrolet ((get-cookie (keyword) `(getf cookies ,keyword)))
           ,@body)))))


;;;------------------------------------------------------------------- 
;;;
;;; EMAIL EXTENSION HEADERS
;;;

;;; these are used in the message archive facility.
(defun parse-message-id-header (string &optional (start 0) (end (length string)))
  (with-string-trim-bounds (*white-space-chars* string start end)
    (let* ((s (char-position #\< string start end))
	   (e (and s (char-position #\> string (1+ (the fixnum s)) end t))))
      (when (and s e)
	(subseq string s (1+ (the fixnum e)))))))

(define-header :message-id (:header :email)
  :print-string "Message-ID"
  :parse-function 'parse-message-id-header)

(defun parse-comma-separated-message-id-header (string &optional (start 0) (end (length string)))
  (with-string-trim-bounds (*white-space-chars* string start end)
    (loop with s = start
	  for idx = (char-position #\< string s end)
	  for close = (and idx (char-position #\> string (1+ (the fixnum idx)) end))
	  while close
	  collect (subseq string idx (1+ (the fixnum close)))
	  while (< (setq s (1+ (the fixnum close))) end))))

(define-header :in-reply-to  (:comma-separated-header :email)
  :print-string "In-Reply-To"
  :parse-function 'parse-comma-separated-message-id-header)

(define-header :references (:comma-separated-header :email)
  :print-string "References"
  :parse-function 'parse-comma-separated-message-id-header)

(define-header :keywords
               (:keyword-sequence-header :email)
  :print-string "Keywords")



;;;------------------------------------------------------------------- 
;;;
;;; HTML 4.01 EXTENSION HEADERS
;;;

(define-header :content-style-type
               (:content-type-header :entity)
  :print-string "Content-Style-Type")

(define-header :content-script-type
               (:content-type-header :entity)
  :print-string "Content-Script-Type")


;;;------------------------------------------------------------------- 
;;;
;;; RESOURCED HEADER DATASTRUCTURES 
;;;

(defun %make-header-position (resource start end)
  (declare (ignore resource))
  (make-header-position :start start :end end))

(defun match-make-header-position-p (resource position start end)
  (declare (ignore resource position start end))
  t)

(defun initialize-header-position (resource position start end)
  (declare (ignore resource))
  (setf (header-position-start position) start
	(header-position-end position) end)
  position)

(defun deinitialize-header-position (resource position)
  (declare (ignore resource))
  (let ((next (header-position-next position)))
    (when next
      (deallocate-resource 'header-position next)
      (setf (header-position-next position) nil)))
  position)

(defresource header-position (start end)
  :matcher match-make-header-position-p
  :constructor %make-header-position
  :initializer initialize-header-position
  :deinitializer deinitialize-header-position)

(define clear-header-position-resource ()
  (clear-resource 'header-position))

(declaim (inline allocate-header-position))

(defun allocate-header-position (start end)
  (allocate-resource 'header-position start end))

(declaim (inline deallocate-header-position))

;; Deallocate from the back forward to ensure resource free
(defun deallocate-header-position (position)
  (deallocate-resource 'header-position position))

(defun header-position-push (header-position start end)
  (loop for position = header-position then next
	for next = (header-position-next position)
	while next 
	finally (setf (header-position-next position) (allocate-header-position start end))))


;;;------------------------------------------------------------------- 
;;;
;;; BUFFERED HEADER RESOURCE
;;;

(defun make-buffered-header (resource keyword buffer start end)
  (declare (ignore resource))
  (make-instance 'buffered-header
		 :keyword keyword
		 :buffer buffer
		 :raw-value-position (make-header-position :start start :end end)
		 :suppress-p nil))

(defun match-buffered-header-p (resource header keyword buffer start end)
  (declare (ignore resource header keyword buffer start end))
  t)

(defun initialize-buffered-header (resource header keyword buffer start end)
  (declare (ignore resource))
  (setf (header-keyword header) keyword
	(%header-buffer header) buffer)
  (let ((position (%header-raw-value-position header)))
    (setf (header-position-start position) start
	  (header-position-end position) end))
  header)

(defun deinitialize-buffered-header (resource header)
  (declare (ignore resource))
  ;; deallocate the multiline positions
  (let* ((position (%header-raw-value-position header))
	 (next-position (header-position-next position)))
    (when next-position
      (deallocate-header-position next-position)
      (setf (header-position-next position) nil)))
  ;; nullify pointers
  (setf (header-keyword header) nil
	(%header-buffer header) nil
	(%header-raw-value header) nil
	(%header-suppress-p header) nil)
  ;; slots unbound as expected by some code
  (slot-makunbound header 'value)
  header)

(defresource buffered-header (keyword buffer start end)
  :matcher match-buffered-header-p
  :constructor make-buffered-header
  :initializer initialize-buffered-header
  :deinitializer deinitialize-buffered-header)

(define clear-buffered-header-resource ()
  (clear-resource 'buffered-header))

(declaim (inline allocate-buffered-header))

(defun allocate-buffered-header (keyword buffer start end)
  (allocate-resource 'buffered-header keyword buffer start end))

(declaim (inline instantiate-buffered-header))

(defun instantiate-buffered-header (index headers keyword buffer start end &aux hdr)
  (if (and index (setq hdr (aref headers index)))
      (initialize-buffered-header nil hdr keyword buffer start end)
      (allocate-buffered-header keyword buffer start end)))

(defmethod deallocate-header ((header buffered-header))
  (deallocate-resource 'buffered-header header))

;; assume normal headers will never be resourced. 4/6/99 -- JCMa.
(defmethod deallocate-header ((header header))
  nil)

(defgeneric clear-header (header)
  (:documentation "Clears all state in HEADER."))

(defmethod clear-header ((header buffered-header))
  (deinitialize-buffered-header nil header))

(defun make-header-set (resource)
  (declare (ignore resource))
  (let* ((index-size *header-set-index-size*)
	 (buffer-size (floor (* index-size 60)))	;assume 60 chars average
	 (num-lines (floor (* index-size 1.15))))	;assume 15% probability of multi-line header
    (declare (fixnum index-size))
    (make-instance 'header-set
		   :buffer (make-array buffer-size :element-type *standard-character-type* :adjustable t :fill-pointer 0)
		   :line-ends (make-array num-lines :element-type 'fixnum :adjustable t :fill-pointer 0)
		   :index (list* (make-array index-size :initial-element nil :adjustable t :fill-pointer 0)
				 (make-array index-size :initial-element nil :adjustable t :fill-pointer 0)))))

(defmethod deallocate-header-objects ((header-set header-set))
  ;; the header objects could be left in place to reduce latency on the front and back ends.  4/7/99 -- JCMa.
  (%with-header-set-index (header-set)
    (with-fast-array-references ((.headers. .headers. vector)
				 (index index vector))
      (loop for idx fixnum upfrom 0 below (the fixnum (array-total-size .headers.))
	    for hdr = (aref .headers. idx)
	    while hdr				;headers always in the front
	    do (deallocate-header hdr)
	       (setf (aref .headers. idx) nil
		     (aref index idx) nil)
	    finally (setf (fill-pointer .headers.) 0
			  (fill-pointer index) 0)))))

(defgeneric clear-header-set (header-set &optional deallocate-surplus-headers-p)
  (:documentation "Resets HEADER-SET for reuse, preserving header resources.
When DEALLOCATE-SURPLUS-HEADERS-P is non-null, any additional header objects
beyond *HEADER-SET-INDEX-SIZE* are deallocated."))

(defmethod clear-header-set ((header-set header-set) &optional deallocate-surplus-headers-p)
  ;; reset other indices
  (setf (fill-pointer (%header-set-buffer header-set)) 0
	(fill-pointer (%header-set-line-ends header-set)) 0)
  ;; reset header objects
  (%with-header-set-index (header-set)
    (with-fast-array-references ((.headers. .headers. vector)
				 (index index vector))
      
      (let ((fill-pointer (fill-pointer .headers.)))
	(cond ((and deallocate-surplus-headers-p (< *header-set-index-size* fill-pointer))
	       (loop for idx fixnum upfrom 0 below *header-set-index-size*
		     do (clear-header (aref .headers. idx)))
	       (loop for idx fixnum upfrom *header-set-index-size* below fill-pointer
		     do (deallocate-header (aref .headers. idx))
			(setf (aref .headers. idx) nil
			      (aref index idx) nil)))
	      (t (loop for idx fixnum upfrom 0 below fill-pointer
		       do (clear-header (aref .headers. idx))))))
      (setf (fill-pointer .headers.) 0
	    (fill-pointer index) 0))))

(defun deinitialize-header-set (resource header-set)
  (declare (ignore resource))
  (deallocate-header-objects header-set)
  ;; reset other indices
  (setf (fill-pointer (%header-set-buffer header-set)) 0
	(fill-pointer (%header-set-line-ends header-set)) 0)
  header-set)

(defun match-header-set-p (resource header-set)
  (declare (ignore resource header-set))
  t)

(defresource header-set (keyword buffer raw-value-position)
  :matcher match-header-set-p
  :constructor make-header-set
  :deinitializer deinitialize-header-set)

(define clear-header-set-resource ()
  (clear-resource 'header-set))


;;;------------------------------------------------------------------- 
;;;
;;; RESOURCED HEADER READER
;;;

;; Ports should specialize this method for greater efficiency.
(define-generic read-headers-into-buffer (header-set stream)
  (:documentation "Reaads HTTP headers from STREAM into buffer structures of HEADER-SET."))

;; portable method
(defmethod read-headers-into-buffer ((header-set header-set) stream)
  (macrolet ((grow-vector (vector size requested-size element-type)
	       `(let ((n-size (floor (* (the fixnum ,requested-size) *header-set-growth-factor*))))
		  (setq ,vector (adjust-array ,vector n-size :element-type ,element-type)
			,size n-size))))
    (with-fast-array-references ((line-ends (%header-set-line-ends header-set) vector))
      (let* ((line-ends-size (array-total-size line-ends))
	     (buffer (%header-set-buffer header-set))
	     (buffer-size (array-total-size buffer))
	     (end (fill-pointer buffer))
	     line error-p delimiter length)
	  delimiter					;ignore
	(using-resource (line-buffer line-buffer *line-buffer-size*)
	  (loop for line-idx fixnum upfrom (fill-pointer line-ends)
		for idx fixnum = end
		do (multiple-value-setq (line error-p delimiter length)
		     (read-delimited-line stream '(#\Linefeed #\return) t line-buffer))
		until (or error-p (blank-line-p line 0 length))
		do (setq end (+ idx (the fixnum length)))
		   (when (< buffer-size end)
		     (grow-vector buffer buffer-size end *standard-character-type*)
		     (setf (%header-set-buffer header-set) buffer))
		   (copy-vector-portion line 0 length buffer idx end)
		   (unless (< line-idx line-ends-size)
		     (grow-vector line-ends line-ends-size line-idx 'fixnum)
		     (setf (%header-set-line-ends header-set) line-ends))
		   ;; Track line end
		   (setf (aref line-ends line-idx) end)
		finally (setf (fill-pointer buffer) end
			      (fill-pointer line-ends) line-idx)))))))

;; Read headers directly from tcp buffer into header buffer.
;; Defined here because header-set, is not defined when the lispm port code loads.
#+(or Genera (and MCL Open-Transport) LispWorks)
(defmethod read-headers-into-buffer ((header-set header-set) (stream #+Genera si:buffered-input-stream
								     #+MCL ccl::modal-ascii-or-binary-tcp-stream-mixin
                                                                     #+LispWorks stream:buffered-stream)
				     &aux error-p delimiter end)
  delimiter					;ignore
  (macrolet ((grow-vector (vector size element-type)
	       `(let ((n-size (floor (* (the fixnum ,size) *header-set-growth-factor*))))
		  (setq ,vector (adjust-array ,vector n-size :element-type ,element-type)
			,size n-size))))
    (with-fast-array-references ((line-ends (%header-set-line-ends header-set) vector))
      (let* ((line-ends-size (array-total-size line-ends))
	     (buffer (%header-set-buffer header-set)))
	(loop with next-buffer
	      for line-idx fixnum upfrom (fill-pointer line-ends)
	      for start = (fill-pointer buffer)
	      do (multiple-value-setq (next-buffer error-p delimiter end)
                     (www-utils::%buffered-stream-read-delimited-line stream '(#\Return #\Linefeed) nil buffer))
	      until (or error-p (blank-line-p next-buffer start end))
	      do (setq buffer next-buffer)	;update buffer in case the array grows
              (unless (< line-idx line-ends-size)
                (grow-vector line-ends line-ends-size 'fixnum)
                (setf (%header-set-line-ends header-set) line-ends))
              ;; Track line end
              (setf (aref line-ends line-idx) end)
	      finally (setf (fill-pointer line-ends) line-idx
			    (%header-set-buffer header-set) buffer))))))	;in case the array grows

(define-generic parse-header-buffer (header-set &optional start start-line-ends))

(defmethod parse-header-buffer ((header-set header-set) &optional (start 0) (start-line-ends 0))
  (macrolet ((adjust-header-vector (var index-ptr new-size)
               `(setf ,var (adjust-array ,var ,new-size :element-type t :initial-element nil)
                      ,index-ptr ,var)))  ;update the stored pointer to this structure
    (flet ((push-multi-line (header start end)
             (let ((raw-value-position (%header-raw-value-position header)))
               (header-position-push raw-value-position start end))))
      (declare (inline push-multi-line))
      (let ((buffer (%header-set-buffer header-set))
            (line-ends (%header-set-line-ends header-set)))
        (%with-header-set-index (header-set)
          (with-fast-array-references ((buffer buffer string) (line-ends line-ends vector)
                                       (index index vector) (.headers. .headers. vector))
            (let ((index-fill-pointer (fill-pointer index))
                  (index-size (array-total-size index))
                  keyword current-header delim-pos)
              (declare (type fixnum index-fill-pointer index-size))
              ;; Ensure syncrhonized sizes. These may occasionally get out of sync. -- JCMa 5/7/2006 
              (unless (= index-size (array-total-size .headers.))
                (adjust-header-vector .headers. (cdr index-ptr) index-size))
              (loop with room-p
                    for idx fixnum upfrom start-line-ends below (fill-pointer line-ends)
                    for s fixnum = start then e
                    for e fixnum = (aref line-ends idx)
                    ;;(format t "~&Parse: ") (write-string buffer t :start s :end e)
                    do (cond ((white-space-char-p (aref buffer s))
                              (when current-header
                                (push-multi-line current-header s e)))
                             ((and (setq delim-pos (char-position #\: buffer s e))
                                   (setq keyword (%tokenize-header buffer s delim-pos)))
                              (setq s (1+ (the fixnum delim-pos)))	;advance pointer
                              (cond ((setq current-header (%%get-header-object keyword index .headers.))
                                     (push-multi-line current-header s e))
                                    ;; push a new header
                                    (t (setq room-p (< index-fill-pointer index-size)
                                             current-header (instantiate-buffered-header (and room-p index-fill-pointer) .headers.
                                                                                         keyword buffer s e))
                                       ;; possibly grow indices
                                       (unless room-p
                                         (let ((n-size (floor (* (the fixnum index-size) *header-set-growth-factor*))))
                                           (adjust-header-vector index (car index-ptr) n-size)
                                           (adjust-header-vector .headers. (cdr index-ptr) n-size)
                                           (setf index-size (array-total-size index))))
                                       ;; push the new header
                                       (setf (aref index index-fill-pointer) keyword
                                             (aref .headers. index-fill-pointer) current-header
                                             (fill-pointer index) (incf index-fill-pointer)
                                             (fill-pointer .headers.) index-fill-pointer))))
                             (t nil))))))))))	;ignore undelimited headers

(define-generic resourced-read-headers (header-set stream)
  (declare (values header-set))
  (:documentation "Reads HTTP headers from stream into HEADER-SET.
If HEADER-SET already contains headers, the new ones are appended to the end."))

(defmethod resourced-read-headers ((header-set header-set) stream)
  (declare (values header-set))
  (let ((start (fill-pointer (%header-set-buffer header-set)))
	(start-line-ends (fill-pointer (%header-set-line-ends header-set))))
    ;; snarf down the headers
    (read-headers-into-buffer header-set stream)
    ;; parse into header objects
    (parse-header-buffer header-set start start-line-ends))
  header-set)

(define-macro with-headers ((stream) &body body)
  "Reads headers from STREAM and makes them accessible via SERVER-GET-HEADER and MAP-HEADER-OBJECTS.
GET-HEADER can be used for extra speed."
  `(using-resource (*headers* header-set)
     (resourced-read-headers *headers* ,stream)
     ,@body))

(declaim (inline allocate-header))

(defun allocate-header (keyword raw-value)
  (make-instance 'header :keyword keyword :raw-value raw-value))

;; Old header reader
(define read-headers (stream &aux current-header line delimiter length error-p header-delimiter header-key multi-line-buffer)
  "Reads and assembles headers from STREAM."
  (declare (values header-alist))
  (labels ((clear-multi-line-buffer (header)
             (flet ((trim-p (char)
                      (member char '(#\space #\tab #\return #\linefeed) :test #'eql)))
	       (when (and header multi-line-buffer)
		 (let* ((val (%header-raw-value header))
			(lines `(,@val ,.(nreverse multi-line-buffer))))
		   (declare (dynamic-extent lines))
		   (setf (car (last val)) (concatenate-lines lines #'trim-p)
			 multi-line-buffer nil)))))
           (push-header-multi-line-string (string)
             (push string multi-line-buffer))
           (push-header-string (header string)
             (setf (%header-raw-value header) (nconc (%header-raw-value header) (list string))))
           (push-new-header-string (header string alist)
             (let ((hdr (assoc header alist)))
               (cond (hdr
                      (push-header-string (cdr hdr) string)
                      (values (cdr hdr) nil))
                     (t (values (allocate-header header-key (list string)) t)))))
           (get-header-value (string start end) ;handles no whitespace case and null value (losers)
             (let ((s (position-if-not* #'white-space-char-p string :start start :end end)))
               (if s (subseq string s length) ""))))
    (declare (inline clear-multi-line-buffer push-header-multi-line-string push-header-string
                     push-new-header-string get-header-value))
    delimiter                                   ;ignore
    (using-resource (line-buffer line-buffer *line-buffer-size*)
      (loop do (multiple-value-setq (line error-p delimiter length)
		 (read-delimited-line stream '(#\Linefeed #\return) nil line-buffer))
	    until (or error-p (blank-line-p line))
	    when (and (setq header-delimiter (if (white-space-char-p (aref line 0))
						 nil
						 (char-position #\: line 0 length)))
		      (setq header-key (%tokenize-header line 0 header-delimiter)))
	      do (clear-multi-line-buffer current-header)
	      and
	    when (multiple-value-bind (header new-p)
		     (push-new-header-string
		       header-key
		       (get-header-value line (1+ (the fixnum header-delimiter)) length)
		       header-alist)
		   (setq current-header header)
		   new-p)
	      collect (list* header-key current-header) into header-alist
		end
	    else do (push-header-multi-line-string (subseq line 0 length))
	    finally (clear-multi-line-buffer current-header)
		    (return-from read-headers header-alist)))))


;;;------------------------------------------------------------------- 
;;;
;;; OPERATIONS ON HEADER SETS
;;;

;; (declaim (inline header-plist))

(define-generic null-header-set-p (header-set)
  (:documentation "Returns non-null if header-set contains no headers."))

(defmethod null-header-set-p ((header-set header-set))
  (%with-header-set-index (header-set)
    .headers.
    (zerop (fill-pointer index))))

(define-generic header-set-count (header-set)
  (:documentation "Returns the number of headers that HEADER-SET contains."))

(defmethod header-set-count ((header-set header-set))
  (%with-header-set-index (header-set)
    .headers.
    (fill-pointer index)))

(define-generic map-header-objects (header-set function)
  (:documentation "Maps function over all the header-objects in HEADER-SET 
FUNCTION is called with HEADER-OBJECT."))

(defmethod map-header-objects (header-set function)
  (%with-header-set-index (header-set)
    index					;ignore
    (with-fast-array-references ((.headers. .headers. vector))
      (loop for idx fixnum upfrom 0 below (fill-pointer .headers.)
	    do (funcall function (aref .headers. idx))))))

;; obsolete
(define map-headers (function &optional (headers *headers*))
  "Maps function over all the current headers.
FUNCTION is called with (KEYWORD HEADER-OBJECT).
Use MAP-HEADER-OBJECTS for better performance."
  (flet ((fctn (header)
	   (funcall function (header-keyword header) header)))
    (declare (dynamic-extent #'fctn))
    (map-header-objects headers #'fctn)))

(define-generic header-set-header-plist (header-set &optional value-key predicate)
  (:documentation "Converts HEADER-SET into a property list of (keyword value).
The value is computed by VALUE-KEY. Useful value-keys are: 
IDENTITY, HEADER-RAW-VALUE, HEADER-VALUE."))

(defmethod header-set-header-plist (header-set &optional (value-key #'header-value) predicate)
  (%with-header-set-index (header-set)
    (with-fast-array-references ((index index vector)
				 (.headers. .headers. vector))
      (if predicate
	  (loop for idx fixnum upfrom 0 below (fill-pointer index)
		for key = (aref index idx)
		for value = (funcall value-key (aref .headers. idx))
		when (funcall predicate key value)
		  collect key
		  and
		collect value)
	  (loop for idx fixnum upfrom 0 below (fill-pointer index)
		collect (aref index idx)
		collect (funcall value-key (aref .headers. idx)))))))

;; obsolete
(defun header-plist (&optional (headers *headers*) (value-key #'header-value))
  "Converts HEADERS, a header alist, into a property list of (keyword value).
The value is computed by VALUE-KEY. Useful value-keys are: 
IDENTITY, HEADER-RAW-VALUE, HEADER-VALUE."
  (header-set-header-plist headers value-key))

;; obsolete
(defun header-alist-to-plist (header-set &optional predicate)
  "Converts HEADER-SET, into a property list of (keyword
header-object).  When provided, PREDICATE is called with (header-keyword
header-object) to determine whether to include the header."
  (header-set-header-plist header-set #'identity predicate))

(define-generic suppress-header (header-set keyword &optional suppress-p error-p)
  (:documentation "Controls whether HEADER-SET transmits the header denoted by keyword."))

(defmethod suppress-header ((header-set header-set) keyword &optional (suppress-p t) (error-p t))
  (let ((header (%get-header-object header-set keyword)))
    (cond (header
	   (setf (%header-suppress-p header) suppress-p))
	  (error-p
	   (error "No header named, ~S, was found." keyword))
	  (t nil))))

(define-generic push-header (header-set keyword value)
  (:documentation "Adds a KEYWORD header with parsed value, VALUE, to HEADER-SET.
The header buffer is updated as are all associated data structures."))

(defmethod push-header ((header-set header-set) keyword value)
  (declare (optimize (speed 3)))
  (check-type keyword keyword)
  (macrolet ((ensure-room-vector (vector index element-type update-form)
	       `(let ((size (array-total-size ,vector)))
		  (unless (< ,index size)
		    (setf size (floor (* (the fixnum size) *header-set-growth-factor*))
			  ,vector (adjust-array ,vector size :element-type ,element-type)
			  ,update-form ,vector)
		    t))))
    (when (%header-print-series-predicate keyword)
      (error "Don't know to handle series valued headers at this time."))
    (%with-header-set-index (header-set) 
      (let* ((buffer (%header-set-buffer header-set))
	     (start (fill-pointer buffer))
	     (line-ends (%header-set-line-ends header-set))
	     (line-ends-fill-pointer (fill-pointer line-ends))
	     (pname (symbol-name (or (%header-print-name keyword) keyword)))
	     (pname-length (length pname))
	     (print-fctn (%header-print-function keyword))
	     (raw-value (with-output-to-string (stream)
			  (funcall print-fctn value stream)))
	     (raw-value-length (length raw-value))
	     (pos1 (+ start pname-length))
	     (pos2 (+ pos1 2))
	     (pos3 (+ pos2 raw-value-length))
	     (index-fill-pointer (fill-pointer index))
	     header header-array-grown-p)
	(declare (fixnum start line-ends-fill-pointer pname-length raw-value-length pos1 pos2 pos3)
                 (dynamic-extent raw-value))
	(ensure-room-vector buffer pos3 *standard-character-type* (%header-set-buffer header-set))
	(ensure-room-vector line-ends line-ends-fill-pointer 'fixnum (%header-set-line-ends header-set))
	(setq header-array-grown-p (ensure-room-vector index index-fill-pointer t (car index-ptr)))
	(ensure-room-vector .headers. index-fill-pointer t (cdr index-ptr))
	;; copy raw values into buffer
	(copy-vector-portion pname 0 pname-length buffer start pos1)
	(copy-vector-portion ": " 0 2 buffer pos1 pos2)
	(copy-vector-portion raw-value 0 (length raw-value) buffer pos2 pos3)
	;; update data associated data structures
	(setf (fill-pointer buffer) pos3	;extend buffer pointer
	      (aref line-ends line-ends-fill-pointer) pos3	;assume a single line header for simplicity  4/7/99 -- JCMa.
	      (fill-pointer line-ends) (1+ line-ends-fill-pointer)	;extend line ends pointer
	      header (instantiate-buffered-header (and (not header-array-grown-p) index-fill-pointer) .headers. keyword buffer pos2 pos3)
	      (aref index index-fill-pointer) keyword	;update index vector
	      (aref .headers. index-fill-pointer) header	;update data vector
	      (fill-pointer index) (incf index-fill-pointer)	;extend index pointer
	      (fill-pointer .headers.) index-fill-pointer)	;extend data pointer
	header))))

(defmethod print-header ((header-set header-set) &optional (stream *standard-output*))
  (flet ((fctn (hdr) (print-header hdr stream)))
    (declare (dynamic-extent #'fctn))
    (map-header-objects header-set #'fctn)))

(define-generic write-header-buffer (header-set stream &optional termination-line-p)
  (:documentation "Writes the raw header buffer associated with HEADER-SET to STREAM.
When TERMINATION-LINE-P is non-null, a blank line is transmitted to terminate the headers."))

(defmethod write-header-buffer ((header-set header-set) stream &optional termination-line-p)
  (with-fast-array-references ((buffer (%header-set-buffer header-set) string)
			       (line-ends (%header-set-line-ends header-set) vector))
    (loop for start = 0 then end
	  for idx fixnum upfrom 0 below (fill-pointer line-ends)
	  for end = (aref line-ends idx)
	  do (write-line buffer stream :start start :end end))
    (when termination-line-p
      (send-cr-line-feed stream))))

;; this destructively removed transmitted headers from the modification list
(defmacro %get-modification (modification-plist header)
  `(loop with reset-plist-p = t
	 for ptr = ,modification-plist then (cddr ptr)
	 while ptr
	 do (if (eq ,header (car ptr))
		(return (values (prog1 (second ptr)
				       (if reset-plist-p
					   (setf ,modification-plist (cddr ptr))
					   (setf ptr (cddr ptr))))
				,header))
		(and reset-plist-p (setq reset-plist-p nil)))
	 finally (return nil)))

(defvar *suppressed-headers* nil
  "A list of header types that are suppressed when using WRITE-MODIFIED-HEADERS.")

(defmacro with-suppressed-headers ((suppress-headers) &body body)
  "Prevents WRITE-MODIFIED-HEADERS from writing SUPPRESS-HEADERS within the scope of BODY."
  `(let ((*suppressed-headers* ,suppress-headers))
     ,@body))

(define-generic write-modified-headers (header-set stream &optional modification-plist excluded-headers termination-line-p additional-headers)
  (:documentation "Writes the raw header buffer associated with HEADER-SET to STREAM.
MODIFICATION-PLIST is a property list of HEADER-KEYWORD HEADER-VALUE that
supplies new values for any existing headers. MODIFICATION-PLIST is
destructively modified and should be consed on the stack by callers.  When
header-set does not contain these headers, they are appended to the headers
written. Note that MODIFICATION-PLIST is destructively modified by this
operation.  EXCLUDED-HEADERS is a list of headers that should not be
transmitted.  When TERMINATION-LINE-P is non-null, a blank line is transmitted
to terminate the headers.  ADDITIONAL-HEADERS is a list of ancillary headers
that are appended to the transmitted headers."))

(defmethod write-modified-headers ((header-set header-set) stream &optional modification-plist excluded-headers termination-line-p additional-headers)
  (declare (ignore termination-line-p additional-headers))
  (%with-header-set-index (header-set)
    (with-fast-array-references ((index index vector)
				 (.headers. .headers. vector))
      (loop with suppressed-headers = *suppressed-headers* and header-object
	    for idx fixnum upfrom 0 below (fill-pointer .headers.)
	    for header-name = (aref index idx)
	    unless (or (member header-name excluded-headers)
		       (member header-name suppressed-headers))
	      do (multiple-value-bind (new-value found-p)
		     (%get-modification modification-plist header-name)
		   (cond (found-p
			  (%write-header header-name new-value stream))
			 ((%header-suppress-p (setq header-object (aref .headers. idx))))
			 (t (write-header header-name header-object stream)))))))
  ;; send any remaining headers
  (loop for (header-name header-value) on modification-plist by #'cddr
	do (%write-header header-name header-value stream)))

(defmethod write-modified-headers ((header-plist list) stream &optional modification-plist excluded-headers termination-line-p additional-headers)
  (declare (ignore termination-line-p additional-headers))
  (loop with suppressed-headers = *suppressed-headers*
	for (header-name header-value) on header-plist by #'cddr
	unless (or (member header-name excluded-headers)
		   (member header-name suppressed-headers))
	  do (multiple-value-bind (new-value found-p)
		 (%get-modification modification-plist header-name)
	       (%write-header header-name (if found-p new-value header-value) stream)))
  ;; send any remaining headers
  (loop for (header-name header-value) on modification-plist by #'cddr
	do (%write-header header-name header-value stream)))

(defmethod write-modified-headers :around (header-set stream &optional modification-plist excluded-headers termination-line-p additional-headers)
  (declare (optimize (speed 3))
           (ignore header-set modification-plist excluded-headers))
  (call-next-method)
  (loop for (header-name header-value) on additional-headers by #'cddr
	do (%write-header header-name header-value stream))
  (when termination-line-p
    (send-cr-line-feed stream)))

;;;------------------------------------------------------------------- 
;;;
;;; OPERATIONS ON HEADER OBJECTS
;;;

(defmethod series-header-p ((header header))
  (not (null (%header-print-series-predicate (header-keyword header)))))

;; internal macro for apply a function to the buffered values of a header
(defmacro %apply-header-buffered-value (header function &optional (inline-p t))
  (flet ((make-function-call (inline-p function &rest args)
	   (if inline-p
	       (cons function (copy-list args))
	       (list* 'funcall function (copy-list args)))))
    `(let ((position (%header-raw-value-position ,header)))
       (cond ((header-position-next position)
	      (loop with buf = (%header-buffer ,header)
		    for pos = position then (header-position-next pos)
		    while pos
		    as start = (header-position-start pos)
		    as end = (header-position-end pos)
		    unless (= start end)	;ignore null values
		      do ,(make-function-call inline-p function 'buf 'start 'end)))
	     (t ,(make-function-call
		   inline-p function `(%header-buffer ,header)
		   '(header-position-start position) '(header-position-end position)))))))

(defmethod header-print-name ((header header))
  (with-slots (keyword) header
    (or (%header-print-name keyword) keyword)))

(defmethod print-object ((header header) stream &aux keyword)
  (print-unreadable-object (header stream :type t :identity t)
    (when (setq keyword (header-keyword header))
      (write (%header-print-name keyword) :stream stream :escape nil))))

(define-generic header-value (header)
  (:documentation "Returns the parsed value of a header object."))

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

(defmethod header-value :around ((header buffered-header))
  (declare (optimize (speed 3)))
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

(defun safe-header-value (header)
  "Returns the parsed value of HEADER signalling any errors parsing the header raw value.
Errors are automatically bug reported."
  (declare (optimize (speed 3))
           (values value error-p))
  (handler-case-if (not *debug-server*)
      (header-value header)
    (error (error)
	   (let ((error-type (type-of error)))
	     (report-bug *bug-http-server*
			 (format nil "HTTP Header Parsing Error: ~S" error-type)
			 "~&Error: ~S~&Header: ~A~&Header Raw Value: ~S~:[~;~&Error Report: ~:*~A~]"
			 error-type (header-keyword header) (header-raw-value header t)
			 (report-string error))
	     (return-from safe-header-value (values nil t))))))

(define-generic header-raw-value (header &optional durable-p)
  (:documentation "Returns the raw value of HEADER, 
The value is list of strings, one for each occurence of the header type in a
header set.  When DURABLE-P is non-null, this returns a raw value that
persists beyond the lifetime of the current header set."))

(defmethod header-raw-value ((header header) &optional durable-p)
  (declare (ignore durable-p))
  (%header-raw-value header))

(defmethod header-raw-value ((header buffered-header) &optional durable-p)
  (flet ((parse-raw-header (string current durable-p)
	   (loop for raw-value = (loop with start = (header-position-start current)
				       for end = (header-position-end current)
				       do (setq current (header-position-next current))
				       while (and current
						  (white-space-sequence-p string end (header-position-start current)))
				       finally (with-string-trim-bounds (*white-space-chars* string start end)
						 (return (unless (= start end)
							   (if durable-p
							       (subseq string start end)
							       (make-array (- (the fixnum end) (the fixnum start))
									   :element-type (array-element-type string)
									   :displaced-to string :displaced-index-offset start))))))
		 when raw-value
		   collect raw-value
		 while current)))
    (declare (inline parse-raw-header))
    (let ((raw-value (%header-raw-value header)))
      (cond (raw-value
	     (if (and durable-p (array-displacement (car raw-value)))
		 (mapcar #'copy-seq raw-value)
		 raw-value))
	    (t (setf (%header-raw-value header) (parse-raw-header (%header-buffer header) (%header-raw-value-position header) durable-p)))))))

(defmethod header-supertype ((header header))
  (with-slots (keyword) header
    (%header-supertype keyword)))

(defmethod header-default-value ((header header))
  (with-slots (keyword) header
    (%header-default-value keyword)))

(define-generic print-header (header &optional stream)
  (:documentation "Writes the full HEADER on STREAM."))

(defmethod print-header ((header header) &optional (stream *standard-output*))
  (with-slots (keyword) header
    (when (header-raw-value header)
      (fast-format stream "~A: " (header-print-name header))
      (funcall (%header-print-function keyword) (header-value header) stream)
      (terpri stream))))

(defmethod print-header :around ((header buffered-header) &optional (stream *standard-output*))
  (cond ((%header-buffer header)
	 (let* ((keyword (header-keyword header))
		(collapsable-p (%header-collapsable-p keyword))
		(print-name (%header-print-name keyword)))
	   (cond (collapsable-p
		  (let ((n-vals 1))
		    (flet ((write-value (buffer start end) 
			     (when (< 1 n-vals)
			       (write-char #\space stream))
			     (write-string buffer stream :start start :end end)
			     (terpri stream)
			     (incf n-vals)))
		      (declare (inline write-value))
		      (fast-format stream "~A:" (header-print-name header))
		      (%apply-header-buffered-value header write-value t))))
		 (t (flet ((write-value (buffer start end)
			     (fast-format stream "~A:" print-name)
			     (write-string buffer stream :start start :end end)
			     (terpri stream)))
		      (declare (inline write-value)) 
		      (%apply-header-buffered-value header write-value t))))))
	;; let the base method do the job
	(t (call-next-method))))

(declaim (inline %write-header))

(defun %write-header (header value stream)
  (declare (optimize (speed 3)))
  (flet ((write-a-header (print-fctn header-name value stream)
           (fast-format stream "~A: ~I~I"
                        (symbol-name header-name)
                        (funcall print-fctn value stream)
                        (send-cr-line-feed stream))))
    (declare (inline write-a-header))
    (let ((header-name (or (%header-print-name header) header))
          (print-fctn (%header-print-function header))
          (series-p (%header-print-series-predicate header)))
      (cond ((and series-p (funcall series-p value))
             (dolist (entry value)
               (write-a-header print-fctn header-name entry stream)))
            (t (write-a-header print-fctn header-name value stream))))))

(define-generic write-header (header value stream)
  (:documentation "Writes HEADER with VALUE on STREAM.
Used for sending headers over HTTP. VALUE can be a header object,
which in case, the raw value is used. If no raw value is available,
the header value is written from the internal representation of the value."))

(defmethod write-header (header value stream)
  (%write-header header value stream))

(defmethod write-header (header-name (header header) stream &aux raw-value)
  (cond ((setq raw-value (%header-raw-value header))
	 (loop initially (fast-format stream "~A: " (symbol-name header-name))
	       for (val . more) = raw-value then more
	       do (write-string val stream)
		  (send-cr-line-feed stream)
	       while more
	       do (write-char #\space stream)))
	((slot-boundp header 'value)
	 (%write-header header-name (%header-value header) stream))
	(t (error "Unable to extract a header value from ~A." header))))

(defmethod write-header :around (header-name (header buffered-header) stream)
  (cond ((%header-buffer header)
	 (flet ((write-value (buffer start end)
		  (with-fast-array-references ((buffer buffer string))
		    (unless (eql #\space (aref buffer start))	;write a leading space when no
		      (write-char #\space stream))	;leading space in orginal header value
		    ;; Write header value directly out of the header buffer
		    (write-string buffer stream :start start :end end)
		    (send-cr-line-feed stream))))
	   (declare (inline write-value))
	   (let ((print-name (symbol-name (or (%header-print-name header-name) header-name))))
	     (cond ((%header-collapsable-p header-name)
		    (fast-format stream "~A:" print-name)
		    (%apply-header-buffered-value header write-value t))
		   (t (flet ((write-header+value (buffer start end)
			       (fast-format stream "~A:" print-name)
			       (write-value buffer start end)))
			(declare (inline write-header+value))
			(%apply-header-buffered-value header write-header+value t)))))))
	;; let the base method do the job
	(t (call-next-method))))

;;;------------------------------------------------------------------- 
;;;
;;; WRITING HEADERS
;;;

(defun %write-raw-header (header value stream)
  (flet ((write-a-header (header-name value stream)
           (write header-name :stream stream :escape nil)
           (write-char #\: stream)
           (write-char #\space stream)
           (write-string value stream)
           (send-cr-line-feed stream)))
    (declare (inline write-a-header))
    (let ((header-name (or (%header-print-name header) header))
          (series-p (%header-print-series-predicate header)))
      (cond ((and series-p (funcall series-p value))
             (dolist (entry value)
               (write-a-header header-name entry stream)))
            (t (typecase value
                 (cons
                   (dolist (val value)
                     (write-a-header header-name val stream)))
                 (t (write-a-header header-name value stream))))))))

(define print-headers (stream &optional (headers *headers*) no-error-p)
  "Used for printing header to a window or non-HTTP stream."
  (cond (no-error-p
	 (flet ((print-it (header &aux value)
		  (cond ((and (typep header 'buffered-header) (%header-buffer header))
			 (print-header header stream))
			((setq value (header-raw-value header))
			 (%write-raw-header (header-keyword header) value stream))
			((setq value (handler-case 
				       (header-value header)
				       (error () nil)))
			 (print-header header stream)))))
	   (declare (dynamic-extent #'print-it))
	   (map-header-objects headers #'print-it)))
        (t (print-header headers stream))))

(defun write-headers (stream headers-plist &optional (termination-line-p t))
  (loop for (header value) on headers-plist by #'cddr
        when (and header value)
          do (write-header header value stream))
  (when termination-line-p
    (send-cr-line-feed stream)))

(declaim (inline write-headers*))

(define write-headers* (stream &rest headers-plist)
  "Writes headers to STREAM based on HEADERS-PLIST,
which is a series of KEYWORD VALUE."
  (declare (dynamic-extent headers-plist))
  (write-headers stream headers-plist t))

(define-variable *mime-content-type-alist* nil
                 "Maps file suffix keywords to mime content types.")

(define mime-content-type-keyword-p (keyword)
  "Returns non-null if keyword is a known MIME content type."
  (not (null (cdr (assoc keyword *mime-content-type-alist* :test #'eq)))))

(declaim (inline %mime-content-type-spec))

(defun %mime-content-type-spec (keyword-or-mime-spec)
  (etypecase keyword-or-mime-spec
    (keyword 
      (or (cdr (assoc keyword-or-mime-spec *mime-content-type-alist* :test #'eq))
          (error "Unknown keyword, ~S, for a mime content type spec." keyword-or-mime-spec)))
    (cons keyword-or-mime-spec)))

(defun set-%mime-content-type-spec (keyword content-type-spec)
  (check-type keyword keyword)
  (check-type content-type-spec cons)
  (unless (and (every #'keywordp content-type-spec) 
               (evenp (length content-type-spec))
               (mime-content-type-major-type-p (first content-type-spec)))
    (error "Bad content-type-spec provided."))
  (let ((entry (assoc keyword *mime-content-type-alist* :test #'eq)))
    (cond (entry (setf (cdr entry) content-type-spec))
          (t (setq *mime-content-type-alist* 
                   (nconc *mime-content-type-alist* `((,keyword . ,content-type-spec))))))
    *mime-content-type-alist*))

(defsetf %mime-content-type-spec set-%mime-content-type-spec)

(define mime-content-type-keyword (content-type-spec &optional (error-p t))
  "Returns the keyword for the content-type-spec."
  (destructuring-bind (major minor &rest parameters) content-type-spec
    (declare (ignore parameters))
    (loop for item in *mime-content-type-alist*
          when (and (eq (second item) major)
                    (eq (third item) minor))
            return (first item)
          finally (if error-p
		      (error "Unknown content type, ~S." content-type-spec)
		      (return nil)))))

(define mime-content-type-primary-extension (content-type-spec)
  "Returns the primary pathname extension for the MIME content type, CONTENT-TYPE-SPEC."
  (primary-pathname-extension (etypecase content-type-spec
                                (keyword content-type-spec)
                                (cons (mime-content-type-keyword content-type-spec)))
                              t))

(define mime-content-type-primary-extension-string (content-type-spec &optional (error-p t))
  "Returns the primary pathname extension string for the MIME content type, CONTENT-TYPE-SPEC."
  (let ((keyword (mime-content-type-keyword content-type-spec error-p)))
    (cond ((null keyword) nil)
	  ((get keyword 'primary-extension-string))
	  (t (setf (get keyword 'primary-extension-string) 
		   (string-downcase (symbol-name (primary-pathname-extension keyword t))))))))

(defun mime-content-type-spec-for-pathname-type (keyword-or-string &optional (error-p t))
  "Returns the MIME content type for the pathname type KEYWORD-OR-STRING." 
  (let ((export-type (export-type-for-pathname-type keyword-or-string error-p)))
    (when export-type
      (%mime-content-type-spec (primary-pathname-extension-for-export-type export-type t)))))

(define mime-content-type-spec-merging-character-set (keyword-or-mime-spec &optional charset)
  "Adds the CHARSET to KEYWORD-OR-MIME-SPEC using non-destructive operations.
When CHARSET is :ISO-8859-1, the HTTP default, no charset is added."
  (let ((spec (%mime-content-type-spec keyword-or-mime-spec)))
    (unless (or (null charset)
                (eql charset :ISO-8859-1))      ;HTTP default character set
      (unless (eql (first spec) :text)
        (error "Attempt to add a character set parameter to a MIME content type which is not TEXT."))
      (setq spec (copy-list spec))
      (setf (getf (cddr spec) :charset) charset))
    spec))

(declaim (notinline %mime-content-type-spec))

(define mime-content-type-keywords ()
  "Returns all the known keywords denoting defined MIME content-types."
  (mapcar #'car *mime-content-type-alist*))

(define-macro define-content-type-name (name major minor &rest parameter-plist)
  (labels ((coerce-to-keyword (x)
             (symbolize (string x) *keyword-package*))
           (munge-paramters (major minor parameters)
             (list* (coerce-to-keyword major)
                    (coerce-to-keyword minor)
                    (when parameters
                      (loop for (param val . more) = parameters then (cddr more)
                            collect (coerce-to-keyword param)
                            collect (intern (string val) *keyword-package*)
                            while more)))))
    `(setf (%mime-content-type-spec ,(coerce-to-keyword name)) ',(munge-paramters major minor parameter-plist))))

(define-content-type-name :multipart-mixed
                          :multipart :mixed
                          :boundary "multipart-mixed-boundary-1x2y3z4q5")

;; Provided for netscape 1.1 server push.  9/5/95 -- JCMa.
(define-content-type-name :multipart-mixed-replace
                          :multipart :x-mixed-replace
                          :boundary "x-multipart-mixed-replace-boundary-1x2y3z4q5")

(define-content-type-name :multipart-form-data
                          :multipart :form-data
                          :boundary "multipart-form-data-boundary-1x2y3z4q5")

(define-content-type-name :multipart-byteranges
                          :multipart :byteranges
                          :boundary "multipart-byteranges-boundary-1x2y3z4q5")

(defun mime-multipart-boundary (multipart-keyword)
  (destructuring-bind (major minor &rest parameters)
      (%mime-content-type-spec multipart-keyword)
    (or (getf parameters :boundary)
        (error "No boundary was specified for the mime multipart type ~A/~A" major minor))))

(declaim (inline mime-multipart-boundary-string))

(defun mime-multipart-boundary-string (multipart-keyword)
  (symbol-name (mime-multipart-boundary multipart-keyword))) 

(declaim (notinline write-mime-multipart-boundary))

(defun write-mime-multipart-boundary (boundary stream &optional last-bock-p)
  (send-cr-line-feed stream)
  (write-string "--" stream)
  (write-string boundary stream)
  (when last-bock-p
    (write-string "--" stream))
  (send-cr-line-feed stream))

(defmacro with-mime-multipart-block (multipart-keyword (stream &key (last-block-p nil last-block-p-supplied-p)
                                                               (force-output t) 
                                                               (sleep-interval  nil sleep-interval-supplied-p)
                                                               content-type content-length content-range content-location last-modified
                                                               expires boundary) &body body)
  "Use this macro when BODY is writing blocks within a mime multipart document.
LAST-BLOCK-P should be non-null when writing the last block of a multipart mime message.
When FORCE-OUTPUT is non-null, output is forced on STREAM
after executing BODY. The HTTP 1.0 specification recommends including a CONTENT-LOCATION within multipart blocks.
Beware that CONTENT-LENGTH, CONTENT-LOCATION, LAST-MODIFIED, expires, and CONTENT-LOCATION can be multiply evaluated.
When SLEEP-INTERVAL is supplied, the process sleeps for SLEEP-INTERVAL seconds after executing the
body and forcing output if FORCE-OUTPUT is non-null."
  (let ((boundary-writer (if boundary
                             `(write-mime-multipart-boundary ,boundary ,stream)
                           `(write-mime-multipart-boundary 
                             (mime-multipart-boundary-string ,multipart-keyword) ,stream))))
    `(let ((headers `(:content-type ,,(typecase content-type
                                        (keyword `(quote ,(%mime-content-type-spec content-type)))
                                        (t `(%mime-content-type-spec ,content-type)))
                      ,,.(when content-length `(:content-length ,content-length))
                      ,,.(when content-range `(:content-range ,content-range))
                      ,,.(when content-location `(:content-location ,content-location))
                      ,,.(when last-modified `(:last-modified ,last-modified))
                      ,,.(when expires `(:expires ,expires)))))
       (declare (dynamic-extent headers))
       ,boundary-writer
       (write-headers ,stream headers t)
       (multiple-value-prog1
           (progn . ,body)
         ,.(when last-block-p-supplied-p `((when ,last-block-p (,@boundary-writer t))))
         (when ,force-output (force-output ,stream)))
       ,.(when sleep-interval-supplied-p 
           `((when (and ,sleep-interval (not ,last-block-p)) (sleep ,sleep-interval)))))))

#|
(write-mime-multipart-block :multipart-mixed-replace
                            (*standard-output* :content-type (print :html)
                                               :content-length (+ 2 3)
                                               :content-location "http://host.local-talk.net/test.html")
                            (print 'foo))
|#

(define mime-content-type-copy-mode (content-type-spec)
  "Returns a keyword indicating the mode in which to copy data accessed by URL.
Current modes are :TEXT, :CRLF, and :BINARY."
  (destructuring-bind (major minor &rest plist)
      content-type-spec
    plist
    (loop for (key mjr mnr) in *mime-content-type-alist*
          when (and (eq mjr major)
                    (eq mnr minor))
            do (return-from mime-content-type-copy-mode
                 (url::%content-type-copy-mode key))
          finally (return-from mime-content-type-copy-mode :binary))))

(defgeneric pathname-copy-mode (pathname &optional error-p)
  (declare (values copy-mode))
  (:documentation "Returns the copy mode keyword for pathname."))

(defmethod pathname-copy-mode ((pathname pathname) &optional (error-p t))
  (let ((type (pathname-type pathname))
	content-type)
    (cond (type
	   (when (setq content-type (mime-content-type-spec-for-pathname-type type error-p))
	     (mime-content-type-copy-mode content-type)))
	  (error-p (error "Can't determine the copy mode because ~A has no pathname type." type))
	  (t nil))))

(defmethod pathname-copy-mode ((string string) &optional (error-p t))
  (let ((pathname (handler-case-if (not error-p) (pathname string) (error () nil))))
    (when pathname
      (pathname-copy-mode pathname error-p))))

(define mime-content-type-element-type (content-type)
  "Returns an element type suitable calls to OPEN for CONTENT-TYPE
Where CONTENT-TYPE, is a data-type keyword, a pathname, a url, or a MIME content-type spec."
  (let ((content-type-spec (typecase content-type
                             (cons content-type)
                             (t (mime-content-type-spec content-type)))))
    (ecase (mime-content-type-copy-mode content-type-spec)
      ((:text :crlf)
       *standard-character-type*)
      (:binary '(unsigned-byte 8)))))

(declaim (inline %mime-content-type-major-type))

(defun %mime-content-type-major-type (keyword)
  (first (%mime-content-type-spec keyword)))

(define mime-content-type-spec-equal-p (content-type-spec1 content-type-spec2 &optional parameter-equality-p)
  "Returns non-null when CONTENT-TYPE-SPEC1 is equal to CONTENT-TYPE-SPEC2,
as determined by comparing the major and minor types of each.
When PARAMETER-EQUALITY-P is non-null, all parameters must match as well." 
  (when content-type-spec1
    (destructuring-bind (major minor &rest plist) content-type-spec1
      (and (eq major (first content-type-spec2))
	   (eq minor (second content-type-spec2))
	   (or (not parameter-equality-p)
	       (and (= (length content-type-spec1) (length content-type-spec2))
		    (loop for (key value) in (cddr content-type-spec2) by #'cddr
			  unless (equalp (getf plist key) value)
			    return nil
			  finally (return t))))))))

;;;------------------------------------------------------------------- 
;;;
;;; HEADER ACCESS UTILITIES
;;;

(declaim (inline remote-user-agent))

(define remote-user-agent ()
  "Returns the string for the remote user agent.
See also CURRENT-USER-AGENT."
  (get-header :user-agent))



(defgeneric entity-header-p (header)
  (:documentation "Returns non-null if HEADER is an entity header."))

(defmethod entity-header-p ((header header))
  (%header-class-p (header-keyword header) :entity))

(defgeneric entity-header-plist (header-set &optional excluded-headers)
  (:documentation "Returns a header PLIST for all entity headers in HEADER-SET,
excluding any headers mentioned in EXCLUDED-HEADERS."))

(defmethod entity-header-plist ((header-set header-set) &optional excluded-headers &aux plist)
  (flet ((maybe-collect-header (header)
           (let ((keyword (header-keyword header)))
             (when (%header-class-p keyword :entity)
               (unless (member keyword excluded-headers)
                 (push (header-value header) plist)
                 (push keyword plist))))))
    (declare (dynamic-extent #'maybe-collect-header))
    (map-header-objects header-set #'maybe-collect-header)
    plist))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(declaim (type function write-server-persistent-connection-headers))

(defun %write-document-mime-headers (stream &rest args)
  ;; content-length can be :bytes when one wants an :accept-ranges without a content-length
  (declare (dynamic-extent args)
           #+Genera(scl:arglist stream content-type charset 
                                length last-modification entity-tag expires location content-location content-language
                                public allow cache-control &optional (termination-line-p t) mime-header-plist header-plist))
  (destructuring-bind
    (content-type charset length last-modification entity-tag expires location content-location content-language
                  public allow cache-control &optional (termination-line-p t) header-plist mime-header-plist)
      args
    (let* ((server *server*)
           (http-version (server-http-version server))
           (1-0-protocol-p (member http-version '(:http/1.0 :http/0.9)))
           (content-type (cond ((null content-type) nil)
                               (charset (mime-content-type-spec-merging-character-set content-type charset))
                               (t (%mime-content-type-spec content-type))))
           ;; Lispm and MCL do ascii translations skewing the content-length downwards.
           (content-length (etypecase length
			     (null nil)
			     (integer
			       #+character-translation-skews-file-size
			       (case (server-method server)
				 (:get (if (and (url::content-type-copy-mode-is-text-p content-type)
						(not *crlf-document-data*))
					   nil
					   length))
				 (t length))
			       #-character-translation-skews-file-size
			       length)))
           (accept-bytes-p (or content-length (eql length :bytes))))
      (declare (dynamic-extent content-type))
      (write-header :date (server-request-time server) stream)
      (write-header :server *server-version* stream)
      (write-server-persistent-connection-headers server stream content-length)
      (cond-every
	(last-modification (write-header :last-modified last-modification stream))
	(expires (write-header :expires expires stream))
	(location (write-header :location location stream))
	(accept-bytes-p (write-header :accept-ranges :bytes stream))
	(public (write-header :public public stream))
	(allow (write-header :allow allow stream))
	((and cache-control (not 1-0-protocol-p)) (write-header :cache-control cache-control stream))
	(header-plist (write-headers stream header-plist nil)))
      (cond-every
	(content-type (write-header :content-type content-type stream))
	(content-length (write-header :content-length content-length stream))
	(content-location (write-header :content-location content-location stream))
	(content-language (write-header :content-language content-language stream))
	(entity-tag
	  (if 1-0-protocol-p
	      (write-header :content-version (entity-tag-value entity-tag) stream)
	      (write-header :etag entity-tag stream)))
	(mime-header-plist (write-headers stream mime-header-plist nil)))
      (when termination-line-p			;send header termination line.
	(send-cr-line-feed stream)))))

;; URI and VERSION are deprecated in http 1.1 We replace it with content-location.   6/29/96 -- JCMa.
(define-macro send-response (stream data-type &key (status :success) bytes last-modification
                                    character-set entity-tag expires location content-location content-language
                                    public allow cache-control additional-headers additional-mime-headers
                                    (termination-line-p t) version)
  "See documentation for WITH-SUCCESSFUL-RESPONSE."
  `(progn
     ;; send a status code and then send some headers
     ,(ecase status
        (:success `(report-status-success ,stream))
        (:connection-established `(report-connection-established ,stream))
        (:created `(report-status-created ,stream))
        (:accepted `(report-status-accepted ,stream))
        (:non-authoritative `(report-status-non-authoritative-information ,stream))
        (:no-content `(report-status-no-content ,stream))
        (:reset-content `(report-status-reset-content ,stream))
        (:partial-content `(report-status-partial-content ,stream))
        (:not-modified `(report-status-not-modified ,stream)))
     ;; write some headers
     (%write-document-mime-headers
       ,stream ,data-type ,character-set ,bytes ,last-modification ,(or entity-tag version) ,expires ,location ,content-location
       ,content-language ,public ,allow  ,cache-control ,termination-line-p ,additional-headers ,additional-mime-headers)))


;;;------------------------------------------------------------------- 
;;;
;;; CHUNKED TRANSFER ENCODING
;;;

#-scl
(defmacro %with-chunked-transfer-encoding ((stream &key chunk-function footer-plist) response-and-headers-form &body body)
  "Executes BODY chunk transfer encoding on STREAM.
When non-null, RESPONSE-AND-HEADERS-FORM must send the request or reply line
and headers, which this macro follows with an appropriated tranfer-encoding
header and blank line to end the headers. When RESPONSE-AND-HEADERS-FORM is
not provided, only the BODY is executed."
  `(unwind-protect
       (progn (www-utils:chunk-transfer-encoding-mode ,stream ,chunk-function)
              ,@(when response-and-headers-form
                  `(,response-and-headers-form
                    ;; write the special headers used in chunking content transfer.
                    (write-header :transfer-encoding :chunked ,stream)
                    (http::send-cr-line-feed ,stream)))
              ;; set up the first chunk
              (www-utils:note-first-chunk ,stream)
              ;; execute the body generating the dynamic data.
              ,@body)
     (www-utils:note-last-chunk ,stream ,footer-plist)))

#+scl
(defmacro %with-chunked-transfer-encoding ((stream &key chunk-function footer-plist) response-and-headers-form &body body)
  "Executes BODY chunk transfer encoding on STREAM.
When non-null, RESPONSE-AND-HEADERS-FORM must send the request or reply line
and headers, which this macro follows with an appropriated tranfer-encoding
header and blank line to end the headers. When RESPONSE-AND-HEADERS-FORM is
not provided, only the BODY is executed."
  (declare (ignore chunk-function))
  `(progn
     ,@(when response-and-headers-form
        `(,response-and-headers-form
          ;; write the special headers used in chunking content transfer.
          (write-header :transfer-encoding :chunked ,stream)
          (http::send-cr-line-feed ,stream)))
    (let ((,stream (www-utils:note-first-chunk ,stream)))
      (unwind-protect
          (progn
              ;; execute the body generating the dynamic data.
            ,@body)
       (www-utils:note-last-chunk ,stream ,footer-plist)))))


(define-macro with-transfer-encoding ((stream transfer-encoding &key chunk-function footer-plist) &body body)
  "Automatically encodes operations on STREAM with TRANSFER-ENCODING.
TRANSFER-ENCODING can be either :FIXED-LENGTH or :CHUNKED."
  `(ecase ,transfer-encoding
     (:fixed-length ,@body)
     (:chunked 
       (%with-chunked-transfer-encoding
         (,stream :chunk-function ,chunk-function :footer-plist ,footer-plist)
         ()
         ,@body))))

(define-macro with-chunked-transfer-encoding ((stream data-type &key
                                                      (status :success)
                                                      chunk-function footer-plist
                                                      last-modification character-set entity-tag expires
                                                      location content-location content-language
                                                      allow cache-control additional-headers additional-mime-headers
                                                      version) &body body)
  `(%with-chunked-transfer-encoding
     (,stream :chunk-function ,chunk-function :footer-plist ,footer-plist)
     (send-response ,stream ,data-type
                    :status ,status
                    :last-modification ,last-modification
                    :character-set ,character-set
                    :entity-tag ,(or entity-tag version)
                    :expires ,expires
                    :location ,location
                    :content-location ,content-location
                    :content-language ,content-language
                    :allow ,allow
                    :cache-control ,cache-control
                    :additional-headers ,additional-headers
                    :additional-mime-headers ,additional-mime-headers
                    :termination-line-p nil)
     ,@body))

;;; For not yet working chunking
#-disable-chunking
(declaim (inline use-chunked-transfer-encoding-p))

(define use-chunked-transfer-encoding-p (content-length)
   #+disable-chunking (declare (ignorable content-length))
  (and 
   #+disable-chunking nil
   (null content-length)
   (client-http-version-meets-p *server* :http/1.1)))


;;;------------------------------------------------------------------- 
;;;
;;; CHUNKED TRANSFER DECODING
;;;

;; destructively removes  the transfer-encoding header, inserts a content-length,
;; and adds any footers sent with the message.
#+(or Genera MCL LispWorks Allegro CMU clozure-common-lisp sbcl clisp scl)
(defun update-chunk-transfer-decoded-headers (header-set stream content-length)
  (declare (values header-set))
  ;; add a content length header now that we know it
  (when content-length
    (push-header header-set :content-length content-length))
  ;; suppress the transfer encoding
  (suppress-header header-set :transfer-encoding t t)
  ;; read any trailers on the chunked data
  (with-text-stream (stream :input)		;make sure stream in in text mode
    (resourced-read-headers *headers* stream))
  header-set)

;; CHUNK-TRANSFER-DECODING-MODE-END Must precede reading headers because
;; further reads on the Stream is blocked by zero-length chunk and chunk
;; decoding mode
(defun chunk-transfer-decoding-mode-finish (stream headers)
  (declare (values headers))
  (cond (headers
	 (let ((content-length (chunk-transfer-content-length stream)))
	   (www-utils:chunk-transfer-decoding-mode-end stream)
	   (update-chunk-transfer-decoded-headers headers stream content-length)))
	(t (www-utils:chunk-transfer-decoding-mode-end stream)
	   (read-headers stream)		;read and toss headers
	   nil)))

;;; KAP don't really understand this condition, should be better done by using :http-chunking rather than the implementations
#+(or Genera MCL LispWorks Allegro CMU)
(define-macro with-chunked-transfer-decoding ((stream &key (headers '*headers*)) &body body)
  "Automatically decodes chunked content transfer encodings.
On exit from BODY, HEADERS is extended to include content-length and any footers transmitted with
the chunked transfer. HEADERS must be amenable to generalized value setting with SETF." 
  `(let ((values nil))
     (unwind-protect
         (handler-case
           (progn (www-utils:chunk-transfer-decoding-mode ,stream)
                  (setq values (multiple-value-list (progn ,@body))))
           (www-utils:end-of-chunk-transfer-decoding ()))
       ,(if headers
	    `(setf ,headers (chunk-transfer-decoding-mode-finish ,stream ,headers))
	    `(chunk-transfer-decoding-mode-finish ,stream ,headers)))
     (values-list values)))

#+scl
(define-macro with-chunked-transfer-decoding ((stream &key (headers '*headers*)) &body body)
  "Automatically decodes chunked content transfer encodings.
On exit from BODY, HEADERS is extended to include content-length and any footers transmitted with
the chunked transfer. HEADERS must be amenable to generalized value setting with SETF."
  `(let ((values nil)
        (,stream (www-utils:chunk-transfer-decoding-mode ,stream)))
     (unwind-protect
         (handler-case
             (setq values (multiple-value-list (progn ,@body)))
           (end-of-file ()))
       ,(if headers
           `(setf ,headers (chunk-transfer-decoding-mode-finish ,stream ,headers))
           `(chunk-transfer-decoding-mode-finish ,stream ,headers)))
     (values-list values)))

#-(or Genera MCL LispWorks Allegro CMU scl)
(define-macro with-chunked-transfer-decoding ((stream &key (headers '*headers*)) &body body)
  "Chunked transfer decoding not available for this implementation.
Automatically decodes chunked content transfer encodings.
On exit from BODY, HEADERS is extended to include content-length and any footers transmitted with
the chunked transfer. HEADERS must be amenable to generalized value setting with SETF." 
  (declare (ignore stream headers))
  `(progn
     (cerror "Proceed with Incorrect Behavior"
             "Chunked transfer decoding has not been implemented for the ~A port."  (lisp-implementation-type))
     ,@body)) 

(defun %handle-end-of-file-for-with-transfer-decoding (condition)
  ;; Data received test declines handling for client when a recycled connection has gone down before responding
  (when (or (not (fboundp 'connection-data-received-p)) (connection-data-received-p))
    (let ((stream (stream-error-stream condition)))
      (error 'insufficient-data			;data source delivered less bytes than content-length 4/27/2001 -- JCMa.
	     :format-string "Insufficient Data: Source (~S) provided less data than the ~D bytes expected."
	     :format-args (list stream (get-header :content-length))
	     :stream stream
	     :url (server-url *server*)
	     :close-connection t))))

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

(define-macro with-transfer-decoding ((stream url &key (headers '*headers*) content-length (copy-mode :binary)
                                              (stream-functions '(stream-copy-until-eof) )) &body body)
  "Automatically decodes transfer encodings.
COPY-MODE is the default mode to use when copying data." 
  `(let ((transfer-encoding (or (get-header :transfer-encoding ,headers) :fixed-length)))
     (case transfer-encoding
       (:fixed-length
         ,(%make-local-stream-function-for-with-transfer-decoding
            stream-functions content-length headers copy-mode body))
       (:chunked 
         (with-chunked-transfer-decoding (,stream :headers ,headers)
           ,@body))
       (t (error 'server-not-implemented :close-connection t :url ,url
                 :format-string "The HTTP transfer decoding, ~A, is not implemented."
                 :format-args (list transfer-encoding))))))

(define-macro with-transfer-decoding* ((stream url http-version &key
                                               (headers '*headers*) (copy-mode :binary)
                                               (stream-functions '(stream-copy-until-eof))) &body body)
  `(case ,http-version
     ((:http/1.0 :http/0.9)
      (let ((content-length (get-header :content-length ,headers)))
        (cond (content-length
               ,(%make-local-stream-function-for-with-transfer-decoding
                  stream-functions nil headers copy-mode body))
              (t ,@body))))
     (t (with-transfer-decoding (,stream ,url :headers ,headers
                                 :stream-functions ,stream-functions :copy-mode ,copy-mode)
          ,@body))))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(define-macro with-successful-response ((stream content-type &key (status :success) bytes last-modification 
                                                character-set entity-tag expires location content-location
                                                content-language allow cache-control
                                                additional-headers additional-mime-headers
                                                (termination-line-p t) chunk-function footer-plist version) &body body)
  "Wrap this macro around successful responses to GET, HEAD, or POST methods.
CONTENT-TYPE is the single keyword denoting the MIME content type of the resource
to be transmitted. If necessary, the full specification of a MIME type and
parameters may be supplied here.  

STATUS can be any of :SUCCESS, :CREATED, :ACCEPTED, :NON-AUTHORITATIVE,
:NO-CONTENT, :RESET-CONTENT, :PARTIAL-CONTENT, :NOT-MODIFIED.  

BYTES is the number of bytes to follow, when this is known in advance.

LAST-MODIFICATION is the universal time when the resource was created.

CHARACTER-SET is valid only for text content types. It is required when
transmitting any text which does not use the default HTTP character set,
:ISO-8859-1. The valid character sets for HTTP are: :US-ASCII, :ISO-8859-1,
:ISO-8859-2, :ISO-8859-3, :ISO-8859-4, :ISO-8859-5, :ISO-8859-6, :ISO-8859-7,
:ISO-8859-8, :ISO-8859-9, :ISO-2022-JP, :ISO-2022-JP, :ISO-2022-KR,
:UNICODE-1-1, :UNICODE-2-2-UTF-7, and :UNICODE-2-2-UTF-7. This argument
should almost always be the value returned by (URL:CHARACTER-SET URL)

ENTITY-TAG is an identifier indicating the version of this resource.
This can be obtained with (URL:ENTITY-TAG URL), or a universal time
when the rsource was creation may be supplied.

EXPIRES should almost always be computed by (URL:EXPIRATION-UNIVERSAL-TIME
URL).  This is required for the :EXPIRATION argument to EXPORT-URL to be
respected and an expires header to be returned to the client.

LOCATION indicates a location other than the request URI, for example, the URI
for a resource created in response to a POST or PUT, whereas the
CONTENT-LOCATION refers to the body returned describing the action.

CONTENT-LOCATION describes the content being transfered in the request.

CONTENT-LANGUAGE is a sequence of keywords denoting the language(s) in which the
resource is written. HTTP 1.1 defines these keywords in section 3.10 to
conform with RFC 1766. They can be a two-letter ISO 639 language abbreviation,
optionally with a two-letter ISO 3166 country code as a subtag.  This argument
should almost always be the value returned by (URL:LANGUAGES URL).

ALLOW is a list of HTTP methods allowed on this URI.

ADDITIONAL-HEADERS allows you to transmit extra application-specific headers.
ADDITIONAL-HEADERS is a property list of (:header-keyword header-value).

CACHE-CONTROL is a property list of keywords and values suitable for the cache
control header.  The value for this argument is normally provided by
 (URL:RESPONSE-CACHE-CONTROL-DIRECTIVES URL), which takes care of directives
available at export time and access control related caching issues at run
time. However, finer grain control is available at run time using the same
cache control directives described in the EXPORT-URL documentation.

If you want to perform a conditional get that will only send back headers when
the client has a valid cache for the url, you need to use
WITH-CONDITIONAL-GET-RESPONSEin place of this. However, dynamically generated
output should use this macro because it provides chunking content transfer.

When HTTP responses are being dynamically generated for HTTP 1.1 or higher
clients, CHUNK-FUNCTION may be supplied to gather information about each chunk
transfered using the chunking transfer encoding mode. CHUNK-FUNCTION is called
on the OUTPUT-BUFFER of STREAM, the START index, and the END index.  The
content of OUTPUT-BUFFER are 8-bit binary bytes ready for transfer over the
network.  The FOOTER-PLIST is an optional property list of headers that are
sent following the entity body. A computation with CHUNK-FUNCTION might
create,for example, a MD5 message digest that would be sent as a Content-MD5
footer following the entity."
  `(cond ((use-chunked-transfer-encoding-p ,bytes)
          (with-chunked-transfer-encoding (,stream ,content-type
                                           :status ,status
                                           :last-modification ,last-modification
                                           :character-set ,character-set
                                           :entity-tag ,(or entity-tag version)
                                           :expires ,expires
                                           :location ,location
                                           :content-location ,content-location
                                           :content-language ,content-language
                                           :allow ,allow
                                           :cache-control ,cache-control
                                           :additional-headers ,additional-headers
                                           :additional-mime-headers ,additional-mime-headers
                                           :chunk-function ,chunk-function
                                           :footer-plist ,footer-plist)
            ,@body))
         (t (send-response ,stream ,content-type
                           :status ,status
                           :bytes ,bytes
                           :last-modification ,last-modification
                           :character-set ,character-set
                           :entity-tag ,(or entity-tag version)
                           :expires ,expires
                           :location ,location
                           :content-location ,content-location
                           :content-language ,content-language
                           :allow ,allow
                           :cache-control ,cache-control
                           :additional-headers ,additional-headers
                           :additional-mime-headers ,additional-mime-headers
                           :termination-line-p ,termination-line-p)
            ,@body)))

(declaim (inline cached-document-remains-valid-p))

(define cached-document-remains-valid-p (last-modification cache-time)
  "Returns non-null when a cached document should be refreshed.
Both LAST-MODIFICATION and CACHE-TIME are universal times."
  (and cache-time
       last-modification
       (let ((margin *cache-hysterisis*)
             (cache-universal-time (etypecase cache-time
                                     (integer cache-time)
                                     (cons (apply #'min cache-time)))))
         (declare (fixnum margin)
                  (integer last-modification cache-time cache-universal-time))
         (< (- last-modification margin) (+ cache-universal-time margin)))))

(declaim (inline if-modified-since-p))

(define if-modified-since-p (last-modification &optional (headers *headers*))
  "Returns non-null if the cached time from the if-modified-since header remains current."
  (cached-document-remains-valid-p last-modification (get-header :if-modified-since headers)))

(declaim (inline if-unmodified-since-p))

(define if-unmodified-since-p (last-modification &optional (headers *headers*))
  (let ((if-unmodified-since (get-header :if-unmodified-since headers)))
    (if if-unmodified-since
        (cached-document-remains-valid-p last-modification if-unmodified-since)
        t)))

(define entity-tag-if-match-p (entity-tag &optional (entity-exists-p t) (headers *headers*))
  "Returns the matching entity tag from the IF-MATCH header or NIL.
ENTITY-TAG is the entity tag for the resource. ENTITY-EXISTS-P is a boolean
indicating whether the entity exists."
  (declare (values true-p matching-entity-tag))
  (let ((if-match-entities (get-header :if-match headers))
        match)
    (cond ((null if-match-entities) t)
          ((setq match (find entity-tag if-match-entities :test #'entity-tag-equal))
           (values t match))
          ((and entity-exists-p (some #'wild-entity-tag-p if-match-entities))
           (values t entity-tag))
          (t nil))))

(define entity-tag-if-none-match-p (entity-tag &optional (entity-exists-p t) (headers *headers*))
  (declare (values true-p matching-entity-tag))
  (let ((if-none-match-entities (get-header :if-none-match headers))
        match)
    (cond ((null if-none-match-entities) nil)
          ((setq match (find entity-tag if-none-match-entities :test #'entity-tag-equal))
           (values t match))
          ((and entity-exists-p (some #'wild-entity-tag-p if-none-match-entities))
           (values t entity-tag))
          (t nil))))

(define check-if-match-precondition (entity-tag entity-exists-p &optional method (headers *headers*))
  (unless (entity-tag-if-match-p entity-tag entity-exists-p headers)
    (error 'precondition-failed :method method)))

(define check-if-unmodified-since-precondition (last-modification &optional method (headers *headers*))
  (unless (if-unmodified-since-p last-modification headers)
    (error 'precondition-failed :method method)))

;; This predicate should become obsolete once the netscape client goes to 1.1   8/2/96 -- JCMa.
(define proxy-directive-no-cache-p (&optional (client-version (server-http-version *server*)))
  "Returns non-null if a CACHE-CONTROL header contains the no cache directive."
  (case client-version
    ((:http/0.9 :http/1.0)
     (multiple-value-bind (directive found-p) 
         (get-header :pragma)
       (cond (found-p
              (etypecase directive
                (atom (eq :no-cache directive))
                (cons (member :no-cache directive))))
             (t nil))))
    ;; equivalent to previous netscape lossage for http 1.1
    (t (multiple-value-bind (directive found-p)
           (get-header :cache-control)
         (cond (found-p
                (or (eql t (getf directive :no-cache))
                    (eql 0 (getf directive :min-age))))
               (t nil))))))

;; proxy-directive-no-cache-p removed from condition because netscape sends
;; pragma no-cache all the time.  1.1 clients MAY specify min-age, min-stale,
;; and min-fresh cache-control directives which this predicate might want to
;; handle, BUT for now do the simple thing.   9/23/98 -- JCMa.
(define conditional-get-not-modified-p (last-modification &optional (headers *headers*)
                                                          &aux (client-version (server-http-version *server*)))
  "Returns non-null when a conditional get does not need to receive the resource
because the client's cache remains valid. LAST-MODIFICATION is a universal time or a list of universal times.
When it is a list (due to redundant headers), the most recent is used."
  (case client-version
    ((:http/0.9 :http/1.0)
     (if-modified-since-p last-modification headers))
    (t (or (entity-tag-if-none-match-p last-modification t headers)
	   (if-modified-since-p last-modification headers)))))

(define-macro handling-conditional-get ((stream &key last-modification character-set entity-tag expires
                                                cache-control additional-headers additional-mime-headers
                                                (termination-line-p t)) &body body)
  "This allows a client to update its cache with new information and avoids
transfering the content of the resource unless the client's cache is stale.
BODY is executed except when the the client has provided the IF-MODIFIED-SINCE header.
In the case where the resource has not been modified more recently than IF-MODIFIED-SINCE,
only new headers and a status code are returned.  BODY must call WITH-SUCCESSFUL-RESPONSE
or equivalent directly."
  `(let ((last-modification ,last-modification))
     (cond ;; when both times are available perform the check
       ((conditional-get-not-modified-p last-modification)
        ;; report that no modification has occured.
        (send-response ,stream nil
                       :status :not-modified
                       :last-modification last-modification
                       :character-set ,character-set
                       :entity-tag ,entity-tag
                       :expires ,expires
                       :cache-control ,cache-control
                       :additional-headers ,additional-headers
                       :additional-mime-headers ,additional-mime-headers
                       :termination-line-p ,termination-line-p))
       (t ,@body))))

(define-macro with-conditional-get-response ((stream content-type &key bytes last-modification
                                                     character-set entity-tag expires location
                                                     cache-control content-location content-language
                                                     additional-headers additional-mime-headers
                                                     (termination-line-p t) version) &body body)
  "This allows a client to update its cache with new information and avoids
transfering the content of the resource unless the client's cache is stale.
BODY is executed and the standard headers and reply code are returned
except when the the client has provided the IF-MODIFIED-SINCE header.
In the case where the resource has not been modified more recently than IF-MODIFIED-SINCE,
only new headers and a status code are returned.  See also the documentation for
WITH-SUCCESSFUL-RESPONSE"
  `(let ((last-modification ,last-modification))
     (handling-conditional-get (,stream :last-modification last-modification
                                :character-set ,character-set
                                :entity-tag ,(or entity-tag version)
                                :expires ,expires
                                :cache-control ,cache-control
                                :additional-headers ,additional-headers
                                :additional-mime-headers ,additional-mime-headers
                                :termination-line-p ,termination-line-p)
       ;; excute if the client client needs to be refreshed.
       (with-successful-response (,stream ,content-type
                                  :bytes ,bytes
                                  :last-modification last-modification
                                  :character-set ,character-set
                                  :entity-tag ,(or entity-tag version)
                                  :expires ,expires
                                  :location ,location
                                  :cache-control ,cache-control
                                  :content-location ,content-location
                                  :content-language ,content-language
                                  :additional-headers ,additional-headers
                                  :additional-mime-headers ,additional-mime-headers
                                  :termination-line-p ,termination-line-p)
         ,@body))))
