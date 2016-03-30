;;; -*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-
;;;
;;; (c) Copyright  1994-2001, 2003, 2005-2006, John C. Mallery
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; SERVER UTILITIES
;;;

(in-package :http)

;;;------------------------------------------------------------------- 
;;;
;;; INTEGER BITVECTOR OPERATIONS
;;;

(define set-mask-bit-p (index mask boolean)
  (declare (values new-byte))
  (dpb (if boolean 1 0) (byte 1 index) mask))

(declaim (inline %bitmask))

(define %bitmask (integer)
  "Encode a non-negative integer as an integer with binary interpretation."
  (check-type integer integer)
  (dpb 1 (byte 1 integer) 0))

;; this could cons the max size bignum on the stack   3/25/97 -- JCMa.
(define bitmask (integers)
  "Encode non-negative integers as an integer with binary interpretation."
  (declare (values bitmask))
  (loop with result = 0
        for integer in integers
        ;; conses a bignum every time through
        do (setq result (logior result (%bitmask integer)))
        finally (return result)))

(declaim (inline identity-bitmask))

(define identity-bitmask (size)
  "Returns a bitmask of size SIZE with all bits set to 1."
  (1- (%bitmask (1+ size))))

(declaim (inline mask-bit-p))

(define mask-bit-p (index mask)
  (declare (values boolean))
  (logbitp index mask))

(declaim (inline mask-subsumes-p))

(define mask-subsumes-p (mask1 mask2)
  "Returns non-null if mask1 subumes every feature of mask2.
If mask2 is empty, then every mask1 subsumes it."
  (declare (values boolean))
  (= (logand mask1 mask2) mask2))

(declaim (inline mask-intersection))

(define mask-intersection (mask1 mask2)
  "Returns a mask which is the intersection of mask1 mask2."
  (declare (values bitmask))
  (logand mask1 mask2))

(declaim (inline mask-union))

(define mask-union (mask1 mask2)
  "Returns a mask which is the union of mask1 mask2."
  (declare (values bitmask))
  (logior mask1 mask2))

(declaim (inline mask-set-difference))

(define mask-set-difference (mask1 mask2)
  "Returns a mask of mask1 for features not present in mask2."
  (declare (values bitmask))
  (logandc2 mask1 mask2))

(declaim (inline mask-exclusive-or))

(define mask-exclusive-or (mask1 mask2)
  "Returns a mask of the exlcusive or between mask1 and  mask2."
  (declare (values bitmask))
  (logxor mask1 mask2))

(declaim (inline mask-width))

(define mask-width (mask)
  "Returns the width of MASK (number of bits)."
  (declare (values integer))
  (integer-length mask))

;;;------------------------------------------------------------------- 
;;;
;;; MACROS
;;;

(define-macro fast-make-instance (specialized-class (default-class) &body init-args)
  "Makes an instance of the class, SPECIALIZED-CLASS, using the initialziations INIT-ARGS.
When SPECIALIZED-CLASS is DEFAULT-CLASS, this macro exposes the constant DEFAULT-CLASS 
to the Lisp compiler to allow compilation into faster code. SPECIALIZED-CLASS is
evaluated at runtime whereas DEFAULT-CLASS is a compile-time class."
  `(let ((class ,specialized-class))
     (case class
       (,default-class
	(make-instance ',default-class ,@init-args))
       (t (make-instance class ,@init-args)))))

(define-macro cond-every (&rest clauses)
  (loop for (cond . forms) in clauses
        collect (if
                  (eql cond 't)
                  `(progn . ,forms)
                  `(when ,cond ,@forms))
          into code
        finally (return `(progn . ,code))))

(define-macro unless-every (&rest clauses)
  (loop for (cond . forms) in clauses
        collect `(unless ,cond ,@forms) into code
        finally (return `(progn ,.code))))

(define-macro pullnew (item location &key test test-not key)
  `(let ((elt ,item)
         (loc ,location))
     (cond ((member elt loc
                    ,.(when test `(:test ,test))
                    ,.(when test-not `(:test-not ,test-not))
                    ,.(when key `(:key ,key)))
            loc)
           (t (setf ,location (nconc loc (list elt)))))))

(define-macro push-ordered (item place predicate &key key)
  "Pushes item onlist list stored in  PLACE maintaining order with PREDICATE
and accessing the value for comparison with key."
  `(let ((item-key ,(if key `(funcall ,key ,item) item)))
     (cond ((or (null ,place)
                (funcall ,predicate item-key ,(if key `(funcall ,key (first ,place)) `(first ,place))))
            (push ,item ,place))
           (t (loop for list = ,place then (cdr list)
                    for next = (cdr list)
                    while next
                    when (funcall ,predicate item-key ,(if key `(funcall ,key (car next)) '(car next)))
                      do (push ,item (cdr list))
                         (return )
                    finally (nconc list (list ,item)))
              ,place))))

(define-macro handler-case-if (condition form &body clauses)
  "Sets up condition handlers when condition returns non-null."
  #+Genera(declare (zwei:indentation 0 3 1 2 2 1))
  `(flet ((execute-form () ,form))
     (declare (inline execute-form))
     (cond (,condition
            (handler-case (execute-form) ,@clauses))
           (t (execute-form)))))

(define-macro handler-bind-if (condition bindings &body forms)
  #+Genera(declare (zwei:indentation 0 3 1 2 2 1))
  `(cond (,condition
          (handler-bind ,bindings . ,forms))
         (t . ,forms)))

(define-macro handler-break-if (condition test &body body)
  "Breaks on unhandled conditions according to COND and TEST.
When the form CONDITION evaluates null, the break handler is bound.
When the break handler is bound, the form TEST is evaluated to
determine whether enter the debugger procedably. TEST may acccess
the variable CONDITION, which is bound to the condition or error
currently being signalled."
  #+Genera (declare (zwei:indentation 0 3 1 2 2 1))
  `(handler-bind-if ,condition
      ((condition #'(lambda (condition)
                      (when ,test
                        (break (report-string condition))
                        nil))))
     ,@body))

(declaim (inline white-space-char-p))

(define white-space-char-p (char)
  (member char *white-space-chars*))

(define white-space-sequence-p (string &optional (start 0) (end (length string)))
  "Returns non-null when every character in STRING from START upto END is a white space character."
  (with-fast-array-references ((string string string))
    (loop for idx upfrom start below end
	  unless (white-space-char-p (aref string idx))
	    return nil
	  finally (return t))))

(declaim (inline null-string-p))

(define null-string-p (string)
  "Returns non-null when string is NULL."
  (declare (type string string))
  (zerop (length string)))

(declaim (inline ensure-list))

(define ensure-list (thing)
  (typecase thing
    (list thing)
    (t (list thing))))

(declaim (inline ensure-string))

(define ensure-string (item)
  (etypecase item
    (string item)
    (symbol (symbol-name item))))

(declaim (inline nsymbolize))

(define nsymbolize (string &optional (package *package*))
  "Interns STRING in PACKAGE destructively assuring uppercase characters in STRING."
  (intern (nstring-upcase string) package))

(eval-when (:load-toplevel :execute :compile-toplevel)
  (declaim (inline symbolize))

  (define symbolize (string &optional (package *package*))
    "Interns STRING in PACKAGE assuring uppercase characters in STRING."
    (intern (string-upcase string) package)))

(defun char-position-2-case (char string  &optional (start 0) (end (length string)) from-end)
  (declare (fixnum start end))
  (with-fast-array-references ((string string string))
    (if from-end
	(loop for idx fixnum downfrom (1- end) to start
	      when (eql char (char string idx))
		return idx
	      finally (return nil))
	(loop for idx fixnum upfrom start below end
	      when (eql char (char string idx))
		return idx
	      finally (return nil)))))

(define-macro char-position (char string  &optional (start 0) end from-end)
  "Returns the position of CHAR in string from START upto END.
when FROM-END is non-null, the string is scanned backward."
  (case from-end
    ((t)
     `(let ((ch ,char))
	(with-fast-array-references ((string ,string string))
	  (loop for idx fixnum downfrom (1- (the fixnum ,(or end '(length string)))) to (the fixnum ,start)
		when (eql ch (char string idx))
		  return idx
		finally (return nil)))))
    ((nil)
     `(let ((ch ,char))
	(with-fast-array-references ((string ,string string))
	  (loop for idx fixnum upfrom (the fixnum ,start) below (the fixnum ,(or end '(length string)))
		when (eql ch (char string idx))
		  return idx
		finally (return nil)))))
    (t (if end
	   `(char-position-2-case ,char ,string ,start ,end ,from-end)
	   `(let ((string ,string))
	      (char-position-2-case ,char ,string ,start (length string) ,from-end))))))

(define fast-position-if (predicate string start end from-end)
  (declare (fixnum start end))
  (with-fast-array-references ((string string string))
    (if from-end
	(loop for idx fixnum downfrom (1- end) to start
	      when (funcall predicate (char string idx))
		return idx
	      finally (return nil))
	(loop for idx fixnum upfrom start below end
	      when (funcall predicate (char string idx))
		return idx
	      finally (return nil)))))

(defmacro %fast-position-if (predicate string &key (start 0) end from-end)
  (check-type predicate symbol)
  (case from-end
    ((t)
     `(with-fast-array-references ((string ,string string))
	(loop for idx fixnum downfrom (1- (the fixnum ,(or end '(length string)))) to (the fixnum ,start)
	      when (,predicate (char string idx))
		return idx
	      finally (return nil))))
    ((nil)
     `(with-fast-array-references ((string ,string string))
	(loop for idx fixnum upfrom (the fixnum ,start) below (the fixnum ,(or end '(length string)))
	      when (,predicate (char string idx))
		return idx
	      finally (return nil))))
    (t (if end
	   `(fast-position-if (function ,predicate) ,string ,start ,end ,from-end)
	   `(let ((string ,string))
	      (fast-position-if (function ,predicate) ,string ,start (length string) ,from-end))))))

(define-macro position-if* (predicate string &key (start 0) (end nil end-supplied-p) from-end)
  (if end-supplied-p
      `(fast-position-if ,predicate ,string ,start ,end ,from-end)
      `(let ((string ,string))
	 (fast-position-if ,predicate string ,start (length string) ,from-end))))

(define fast-position-if-not (predicate string start end from-end)
  (declare (fixnum start end))
  (with-fast-array-references ((string string string))
    (if from-end
	(loop for idx fixnum downfrom (1- end) to start
	      unless (funcall predicate (char string idx))
		return idx
	      finally (return nil))
	(loop for idx fixnum upfrom start below end
	      unless (funcall predicate (char string idx))
		return idx
	      finally (return nil)))))

(defmacro %fast-position-if-not (predicate string &key (start 0) end from-end)
  (check-type predicate symbol)
  (case from-end
    ((t)
     `(with-fast-array-references ((string ,string string))
	(loop for idx fixnum downfrom (1- (the fixnum ,(or end '(length string)))) to (the fixnum ,start)
	      unless (,predicate (char string idx))
		return idx
	      finally (return nil))))
    ((nil)
     `(with-fast-array-references ((string ,string string))
	(loop for idx fixnum upfrom (the fixnum ,start) below (the fixnum ,(or end '(length string)))
	      unless (,predicate (char string idx))
		return idx
	      finally (return nil))))
    (t (if end
	   `(fast-position-if-not (function ,predicate) ,string ,start ,end ,from-end)
	   `(let ((string ,string))
	      (fast-position-if-not (function ,predicate) ,string ,start (length string) ,from-end))))))

(define-macro position-if-not* (predicate string &key (start 0) (end nil end-supplied-p) from-end)
  (if end-supplied-p
      `(fast-position-if-not ,predicate ,string ,start ,end ,from-end)
      `(let ((string ,string))
	 (fast-position-if-not ,predicate string ,start (length string) ,from-end))))

(declaim (inline %string-equal))

(defun %string-equal (string1 string2 &optional (start1 0) (end1 (length string1)) (start2 0) (end2 (length string2)))
  "Returns non-null if STRING1 is equal to STRING2 using CHAR-EQUAL as the test."
  (declare (fixnum start1 end1 start2 end2))
  (when (= (- end1 start1) (- end2 start2))
    (check-type string1 string)
    (check-type string2 string)
    (with-fast-array-references ((string1 string1 string)
				 (string2 string2 string))
      (loop for idx1 fixnum upfrom start1 below end1
	    for idx2 fixnum upfrom start2
	    unless (char-equal (char string1 idx1) (char string2 idx2))
	      return nil
	    finally (return t)))))

(declaim (inline %string=))

(defun %string= (string1 string2 &optional (start1 0) (end1 (length string1)) (start2 0) (end2 (length string2)))
  "Returns non-null if string1 is equal to string2 using EQL as the test."
  (declare (fixnum start1 end1 start2 end2))
  (when (= (- end1 start1) (- end2 start2))
    (check-type string1 string)
    (check-type string2 string)
    (with-fast-array-references ((string1 string1 string)
				 (string2 string2 string))
      (loop for idx1 fixnum upfrom start1 below end1
	    for idx2 fixnum upfrom start2
	    unless (eql (char string1 idx1) (char string2 idx2))
	      return nil
	    finally (return t)))))

(declaim (inline %string-trim-bounds))

(defun %string-trim-bounds (charset string start end)
  (declare (values new-start new-end)
	   (fixnum start end))
  (with-fast-array-references ((string string string))
    (let* ((s (loop for idx fixnum upfrom start below end
		    unless (member (char string idx) charset)
		      return idx
		    finally (return idx)))
	   (e (if (= s end)
		  end
		  (loop for idx fixnum downfrom (1- end) above (the fixnum s)
			unless (member (char string idx) charset)
			  return (1+ idx)
			finally (return (1+ idx))))))
      (values s e))))

(define string-trim-bounds (charset string &optional (start 0) (end (length string)))
  "Returns the boundary indices to ignore leading and trailing characters in CHARSET."
  (declare (values new-start new-end))
  (%string-trim-bounds charset string start end))

(define-macro with-string-trim-bounds ((charset string start end) &body body)
  "Rebinds START and END to ignore leading and trailing characters in CHARSET."
  `(locally
     (declare (inline %string-trim-bounds))
     (multiple-value-bind (,start ,end)
	 (%string-trim-bounds ,charset ,string ,start ,end)
       ,@body)))

(defun write-downcase-string (string &optional (stream *standard-output*) (start 0) (end (length string)))
  "Writes string to stream converting all upper case characters to lower case."
  (loop for idx upfrom start below end
        for char = (char string idx)
        do (write-char (char-downcase char) stream)))

(declaim (inline nfill-array))

(define nfill-array (array1 array2 &key (start1 0) (end1 (length array1))
                            (start2 0) (end2 (length array2)))
  "Destructively copies the portion of ARRAY2 between START2 and END2 into ARRAY1 between START1 and END1.
The elements of ARRAY1 are changed while those of ARRAY2 remain untouched."
  (declare (values array1))
  (with-fast-array-references ((arr1 array1 array)
                               (arr2 array2 array))
    (loop for idx1 upfrom start1 below end1
          for idx2 upfrom start2 below end2
          do (setf (aref arr1 idx1) (aref arr2 idx2)))
    array1))

(define triming-substring (charset string &optional (start 0) (end (length string)) null-string-is-nil)
  "Returns a substring of STRING from START to END trimming charset from the ends."
  (with-string-trim-bounds (charset string start end)
    (let* ((l (- (the fixnum end) (the fixnum start)))
	   nstring)
      (cond ((and (zerop l) null-string-is-nil) nil)
	    (t (setq nstring (make-string l))
	       (nfill-array nstring string :start1 0 :end1 l :start2 start :end2 end)
	       nstring)))))

(eval-when (:load-toplevel :execute :compile-toplevel)
  (define-macro with-string-for-null-stream ((stream &key (inline t)) &body body)
    "When STREAM is NIL, returns output to STREAM within body as as STRING."
    (if inline
        `(cond (,stream ,@body)
               (t (with-output-to-string (,stream)
                    ,@body)))
        `(flet ((run-body (,stream)
                  ,@body))
           (declare (dynamic-extent #'run-body))
           (if ,stream
               (run-body ,stream)
               (with-output-to-string (,stream)
                 (run-body ,stream))))))
  )                                             ; close eval when

(define nshift-vector (vector offset &optional (start 0) (end (length vector)))
  "Shifts the elements of VECTOR between start and end offset elements left or right.
Shifting elements off the ends of the vector is ignored, so use accordingly.
Fill pointers are not reset."
  (declare (fixnum offset start end))
  (with-fast-array-references ((vector vector vector))
    (cond ((zerop offset))
	  ((plusp offset)
	   (let* ((last-idx1 (1- end))
		  (last-idx2 (+ offset last-idx1))
		  (max-idx2 (1- (the fixnum (array-total-size vector))))
		  (fit (- max-idx2 last-idx2)))
	     (declare (fixnum last-idx1 max-idx2 last-idx2))
	     (when (minusp fit)
	       (setq last-idx1 (+ last-idx1 fit)
		     last-idx2 max-idx2))
	     (loop for idx1 fixnum downfrom last-idx1 to start
		   for idx2 fixnum downfrom last-idx2
		   do (setf (aref vector idx2) (aref vector idx1)))))
	  (t (loop with first-idx = (+ start offset)
		   for idx1 fixnum upfrom (if (minusp first-idx)
					      (+ start (abs first-idx))
					      start)
			    below end
		   for idx2 fixnum upfrom (max first-idx 0)
		   do (setf (aref vector idx2) (aref vector idx1)))))
    vector))


(declaim (inline string-append))

;; The function STRING-APPEND is needed when the result type STRING
;; used in something like (concatenate 'string ...)
;; is not sufficient to store something like Unicode
;; characters and some of the strings to be concatenated
;; contains Unicode characters.

(define string-append (&rest strings)
  "Appends strings. Makes sure that the result string can
contain all character types of all the string arguments."
  (declare (dynamic-extent strings))
  #-(or lispworks)
  (apply #'concatenate 'string strings)
  #+lispworks
  (apply #'lw:string-append strings))

(define string-type-symbol (string)
  "Returns the type of the string as a symbol. Does not return a full compound type.
This function can be used to compute the result type
for CONCATENATE based on the type of STRING."
  (let ((type (type-of string)))
    (if (consp type)
        (first type)
      type)))

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
	  (t (apply #'string-append strings)))))

(defun elide-string (string from to &optional (start 0) (end (length string)) destructive-p (pad-char #\space))
  "Removes the elements of STRING from FROM upto but not including TO.
DESTRUCTIVE-P controls whether string is destructively modified."
  (declare (values elided-string copied-p padded-p)
           (fixnum from to start end)
           (optimize (speed 3)))
  (flet ((pad-string (pad-char string start end)
           (declare (fixnum start end))
           (with-fast-array-references ((string string string))
             (loop for idx fixnum upfrom start below end
                   do (setf (char string idx) pad-char)))))
    (declare (inline pad-string))
    (let ((pos (- from start)))
      (declare (fixnum pos))
      (cond (destructive-p
             (let ((new-fill-pointer (- end (- to from))))
               (copy-vector-portion string to end string from new-fill-pointer)
               (cond ((array-has-fill-pointer-p string)
                      (cond ((= (fill-pointer string) end)
                             (setf (fill-pointer string) new-fill-pointer)                      
                             (values string nil))
                            (t (pad-string pad-char string new-fill-pointer end)
                               (values string nil t))))
                     (t (pad-string pad-char string new-fill-pointer end)
                        (values string nil t)))))
            (t (let* ((size (- (the fixnum (- end start)) (the fixnum (- to from))))
                      (nstring (make-string size :element-type *standard-character-type*)))
                 (copy-vector-portion string start from nstring 0 pos)
                 (copy-vector-portion string to end nstring pos (+ pos (- end to)))
                 (values nstring t)))))))

(define-macro with-standard-server-io-syntax (() &body body)
  "Binds Lisp input/output to standard default values within server instances."
  `(let ((*read-eval* nil)                      ;turn off throughout the runtime server environment
         (*print-pretty* nil))                  ;turn off pretty printing to prevent weird string emissions
     ,@body))

(define-macro define-cached-computation (name (&key life-time cache-validator documentation) &body body)
  "Defines a computation, named NAME, whose returned values are cached.
LIFE-TIME (required) is the number of seconds for which the cache remains valid.
CACHE-VALIDATOR (optional) is a function of no arguments that returns non-null while
the cached value remains valid. BODY performs the computation and returns any number of values."
  (let ((var-name (symbolize (format nil "*~A-CACHE*" name)))
        (fctn-name (symbolize (format nil "~A" name)))
        (interval life-time))
    `(progn
       (defvar ,var-name nil)
       (setf (get ',var-name :life-time) ,interval)
       (define ,fctn-name (&optional recache-p)
         ,.(when documentation (list documentation))
         #+Genera(declare (sys:function-parent ,fctn-name define-cached-computation))
         (destructuring-bind (&optional expiration . values) ,var-name
           (when (or recache-p
                     (null values)
                     (< (get-universal-time) expiration)
                     ,.(when cache-validator `((not (funcall ,cache-validator)))))
             (setq expiration (+ ,interval (get-universal-time))
                   values (multiple-value-list (progn . ,body))
                   ,var-name (list* expiration values)))
           (values-list values))))))

;;;------------------------------------------------------------------- 
;;;
;;; HTTP VERSION
;;;

(defmacro define-protocol-version-accessors (&rest versions)
  (loop for v in versions
        for class = (symbolize (symbol-name v) :http)
        for var = (symbolize (format nil "+~A+" (symbol-name v)) :http)
        collect `(defvar ,var (make-instance ',class)) into vars
        collect `(,v (symbol-value ',var)) into clauses
        finally (let ((clauses (nreverse clauses)))
                  (return `(progn
                             ,.vars
                             (define http-protocol (protocol)
                               "Returns the version class instance for protocol."
                               (ecase protocol
                                 ,.clauses)))))))

(define-protocol-version-accessors :http/0.9 :http/1.0 :http/1.1)

(define-macro protocol-case (protocol &body clauses)
  `(typecase ,protocol
     ,(loop for (version . forms) in clauses
            collect `(,(symbolize (string version) :http) ,.forms))))

(define-macro eprotocol-case (protocol &body clauses)
  `(etypecase ,protocol
     ,(loop for (version . forms) in clauses
            collect `(,(symbolize (string version) :http) ,.forms))))

(define http-version-less-p (version-1 version-2 &aux pos1 pos2)
  "Returns non-null if VERSION-1 is a lower HTTP protocol version than VERSION-2.
Both arguments must be protocol version keywords of the form :HTTP/1.1"
  (if (and (setq pos1 (position version-1 *http-known-versions* :test #'eq :from-end t))
           (setq pos2 (position version-2 *http-known-versions* :test #'eq :from-end t)))
      (< pos1 pos2)
      (error "Unknown HTTP version encountered in HTTP-VERSION-LESS-P:~:[~&VERSION-1= ~S~;~*~]~:[~&VERSION-2= ~S~;~*~]"
             pos1 version-1 pos2 version-2)))

(declaim (inline http-version-greater-p))

(define http-version-greater-p (version-1 version-2)
  "Returns non-null if VERSION-1 is a higher HTTP protocol version than VERSION-2.
Both arguments must be protocol version keywords of the form :HTTP/1.1"
  (http-version-less-p version-2 version-1))

(declaim (inline http-version-min))

(define http-version-min (version-1 version-2)
  "Returns the lowest protocol version of VERSION-1 and VERSION-2."
  (if (http-version-less-p version-1 version-2)
      version-1
      version-2))

(declaim (inline http-version-max))

(define http-version-max (version-1 version-2)
  "Returns the highest protocol version of VERSION-1 and VERSION-2."
  (if (http-version-less-p version-1 version-2)
      version-2
      version-1))

(define default-condition-close-connection-p (&optional (server *server*) &aux client-version)
  "Returns non-null when the the connection should close as a function of the server version."
  (if (and server
           (setq client-version (server-http-version server))
           (member client-version *http-known-versions*))
      (http-version-less-p client-version :http/1.1 )
      t))

;;;------------------------------------------------------------------- 
;;;
;;; LOCAL CONTEXTS
;;;

(declaim (inline server-native-url-scheme-p))

(defun server-native-url-scheme-p (url)
  "Returns non null when the scheme of URL is served natively by CL-HTTP."
  (member (url:scheme url) *server-native-url-schemes*))

(defun %absolutize-relative-url (scheme host port &optional relative-url canonicalize-host-p)
  (let ((domain-name (string-downcase (if canonicalize-host-p (host-domain-name host) host)))
        (scheme-string (ecase scheme
                         (:http "http://")
                         (:https "https://")
                         (:ftp "ftp://"))))
    (declare (dynamic-extent domain-name))
    (cond ((eql port (url:protocol-default-port scheme))
           (apply #'concatenate 'string scheme-string domain-name relative-url))
          (t (apply #'concatenate 'string  scheme-string domain-name ":" (write-to-string port :base 10.) relative-url)))))

(defun absolutize-relative-url (scheme host port &rest relative-url)
  (declare (dynamic-extent relative-url))
  (%absolutize-relative-url scheme host port relative-url t))

(declaim (inline %make-context))

(defun %make-context (host &optional (port *standard-http-port*) (protocol *standard-protocol*))
  (absolutize-relative-url protocol host port))

(defun %make-local-context (&optional (port *standard-http-port*) (protocol *standard-protocol*))
  (if *http-host-name*
      (%absolutize-relative-url protocol *http-host-name* port nil nil)
    (%make-context (local-host-domain-name) port protocol)))

(define set-standard-http-port (&optional (port 80) (protocol *standard-protocol*))
  "Primary method for setting the standard port for HTTP connections.
PROTOCOL can be either :HTTP or :HTTPS."
  (check-type protocol (member :http :https))
  (check-type port (integer 0))
  (setq *standard-http-port* port
        *standard-protocol* protocol
        *local-scheme* protocol
        *local-context* (%make-local-context *standard-http-port* *standard-protocol*)
        *local-port-context-alist* nil))

(defvar *url-context-remapping-alist* nil
  "Holds the context remappings for use with reverse proxiess.")

(defun clear-url-context-remapping ()
  (setq *url-context-remapping-alist* nil))

(defun set-url-context-remapping (scheme host port new-host &optional (new-port port) (new-scheme scheme))
  (let* ((old-context (absolutize-relative-url scheme host port))
	 (new-context (%absolutize-relative-url new-scheme new-host new-port nil nil))
	 (entry (assoc old-context *url-context-remapping-alist* :test #'equalp))) 
    (if entry
	(setf (cdr entry) new-context)
      (setq *url-context-remapping-alist* (nconc *url-context-remapping-alist* `((,old-context . ,new-context)))))))

(defun %define-url-context-remappings (specs)
  (loop initially (clear-url-context-remapping)
	for entry in specs
	do (destructuring-bind (scheme host port new-host &optional (new-port port) (new-scheme scheme)) entry
	     (set-url-context-remapping scheme host port new-host new-port new-scheme)))
  *url-context-remapping-alist*)

(defmacro define-url-context-remappings (&rest specs)
  "Defines remappings from local contexts handled by the server to remote context,
such as the external name when operating through a reverse proxy.
SPECS is a list of remapping specifications of the form:
    (SCHEME HOST PORT NEW-HOST &optional (NEW-PORT port) (NEW-SCHEME scheme))
NEW-HOST is not canonicalized by a reverse DNS lookup."
  `(%define-url-context-remappings ',specs))

(defun write-url-remapping-context (url &optional (stream *standard-output*) (escape-search-url-p t))
  "Writes an absolute URL string for URL to STREAM, remapping the context according to *URL-CONTEXT-REMAPPING-ALIST*."
  (let ((remappings *url-context-remapping-alist*))
    (when remappings
      (loop for (old-context . new-context) in remappings
	    when (url::local-context-match-p old-context url)
            do (write-string new-context stream)
               (write-string (coerce-url-string url t t) stream)
               (return-from write-url-remapping-context)))
    (write-string (coerce-url-string url escape-search-url-p nil) stream)))

(defun url-string-remapping-context (url &optional (remappings *url-context-remapping-alist*))
  "Returns a fully an absolute URL string for URL whose context has been remapped according to REMAPPINGS."
  (when remappings
    (loop for (old-context . new-context) in remappings
          when (url::local-context-match-p old-context url)
          do (return-from url-string-remapping-context (concatenate 'string new-context (coerce-url-string url t t)))))
  (coerce-url-string url t nil))

;;;------------------------------------------------------------------- 
;;;
;;; INTIALIZATIONS
;;;

(define load-user-init-file (&optional host)
  "Load the user's CL-HTTP init file, if it exists."
  (let ((init-file (merge-pathnames *cl-http-user-init-file* (if host (user-homedir-pathname host) (user-homedir-pathname)))))
    (when (probe-file init-file)
      (load init-file :print nil :verbose nil))))

(define server-version ()
  "Returns the server version,
for example \"CL-HTTP/41.4 (Macintosh Common Lisp; 1.7.1)\""
  (destructuring-bind (major minor &rest port-version-numbers)
      (www-utils:%server-version-info)
    (format nil "CL-HTTP/~D.~D (~A~:[~;~:*; ~{~D~^.~}~])"
            major minor (lisp-implementation-type) port-version-numbers)))

(define initialize-server-version ()
  (setq *server-version* (server-version)))

(define run-server-initializations (&optional redo-flag)
  "Runs the cl-http server cold initializations."
  (run-initializations '*server-initialization-list* redo-flag))

(define run-server-launch-initializations (&optional redo-flag)
  "Runs the cl-http server launch server initializations."
  (run-initializations '*server-launch-initialization-list* redo-flag))

(define run-shutdown-initializations (&optional redo-flag)
  "Runs the cl-http shutdown server initializations."
  (run-initializations '*shutdown-initialization-list* redo-flag))

(define reset-server-local-host-variables (&key (standard-port *standard-http-port*) (protocol *standard-protocol*))
  "Primary method for ensuring that switches and variables are bound correctly for the local host."
  ;; the local internet must be initialized when moving worlds around.
  #+Genera
  (setq www-utils::*internet-network* (neti:find-object-named :network "INTERNET"))
  ;; set the variables
  (setq *default-mailer-host* (local-host-domain-name t)
        *reject-connection-message* (with-output-to-string (string)
                                      (http::send-status-line string 503 "Insufficient Resources: Server too busy.")))
  (time-zone t)
  (set-standard-http-port standard-port protocol)
  (initialize-server-version)
  (set-maximum-number-of-connections *maximum-number-of-connections*)
  (local-host-ip-address t)
  (local-host-parsed-ip-address t)
  (local-host-domain-name t)
  (clear-virtual-hosts)
  (server-mail-address t)
  (initialize-default-pathname)
  (email-address-for-bug-reports t))

#+Genera
(add-initialization
  "Load HTTP Logical Host."
  '(load-http-logical-host)
  '(:normal)
  '*server-initialization-list*)

(add-initialization
  "Reset Server Local Host Variables."
  '(reset-server-local-host-variables)
  '(:normal)
  '*server-initialization-list*)

(add-initialization
  "Load user CL-HTTP Init File."
  '(load-user-init-file)
  '(:normal)
  '*server-initialization-list*)

(add-initialization
  "Synchronize Daily Server Tasks"
  '(www-utils:synchronize-daily-server-tasks)
  '(:normal :last)
  '*server-initialization-list*)

;; Initialize the idle http process scavenger
;; This is a key feature for production servers and a security feature.
#-(or CMU scl)
(add-initialization
  "Synchronize Idle Connection Scavenger"
  '(www-utils::synchronize-idle-http-process-scavenger)
  '(:normal :last)
  '*server-initialization-list*)

(add-initialization
  "Close All Log File Streams"
  '(close-all-logs)
  '(:normal)
  '*shutdown-initialization-list*)

(define-generic set-logical-host-directory (logical-host directory physical-pathname)
  (:documentation "Sets the directory, DIRECTORY, for the logical host, LOGICAL-HOST, to be PHYSICAL-PATHNAME."))

(defmethod set-logical-host-directory ((logical-host string) (directory string) (physical-pathname pathname))
  (flet ((directory-match-p (key entry key-length)
           (destructuring-bind (dir path) entry
             (declare (ignore path))
             (and (>= (length dir) key-length)
                  (string-equal key dir :end2 key-length))))
         (make-dir-spec (directory length)
           (cond ((not (char-equal #\; (char directory (1- length)))) directory)
                 ((char-equal #\* (char directory (- length 2)))
                  (concatenate 'string directory #+Genera "*.*.*" #-Genera "*.*"))
                 (t (concatenate 'string directory #+Genera "**;*.*.*" #-Genera "**;*.*")))))
    (declare (inline directory-match-p make-dir-spec))
    (loop with directory-length fixnum = (length directory) 
          with http-translations = (logical-pathname-translations logical-host)
          for entry in http-translations
          when (directory-match-p directory entry directory-length)
	    do (progn (setf (second entry) (translated-pathname physical-pathname))
		      (return (setf (logical-pathname-translations logical-host) http-translations)))
          finally (return (setf (logical-pathname-translations logical-host) 
				(push (list (make-dir-spec directory directory-length) (translated-pathname physical-pathname))
				      (logical-pathname-translations logical-host)))))))

(defmethod set-logical-host-directory ((logical-host string) (directory string) (physical-pathname string))
  (set-logical-host-directory logical-host directory (pathname physical-pathname)))

(defun set-http-logical-directory-location (directory physical-pathname)
  "Sets the directory, DIRECTORY, for the HTTP logical host to be PHYSICAL-PATHNAME."
  (set-logical-host-directory "HTTP" directory physical-pathname))

;; Needs to be reworked to handle https -- JCMa 65/3/2006
(defun move-server-to-new-host (host &key (port *standard-http-port*)
                                     (old-host (local-host-domain-name))
                                     (old-port *standard-http-port*)
                                     (reinitialize-p t)
                                     (report-stream *standard-output*) 
                                     &aux virtual-host-entries (protocol *standard-protocol*))
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
                                        (%make-context host port protocol))
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

;; Other ports can run this once the provide the supporting port functions
#+(or LispWorks Genera MCL)
(defun maybe-move-server-ip-address (&key (report-stream *standard-output*))
  (when (local-host-ip-address-moved-p)
    (move-server-to-new-host (%local-host-domain-name)
                             :old-host (local-host-domain-name)
                             :reinitialize-p t
                             :report-stream report-stream)))

;;;------------------------------------------------------------------- 
;;;
;;; PATHNAME UTILITIES
;;; 

(defmethod translated-pathname ((pathname string))
  (translated-pathname (pathname pathname)))

(defmethod translated-pathname ((pathname logical-pathname))
  (translate-logical-pathname pathname))

(defmethod translated-pathname ((pathname pathname))
  pathname)

(define initialize-default-pathname (&optional (pathname "http:www;foo.html"))
  "Initializes the default pathname used by the server."
  (setq *default-pathname* (translated-pathname pathname)))

(declaim (inline host-default-pathname))

(define host-default-pathname (&optional host)
  (www-utils:default-pathname nil host))

(define make-directory (pathname)
  (let ((path (pathname pathname)))
    (make-pathname 
      :host (pathname-host path)
      :device (pathname-device path)
      :directory 
      #+Genera 
      (if (string-equal (pathname-type pathname) "directory")
          `(,@(pathname-directory path) ,(pathname-name path))
          (pathname-directory path))
      #-Genera (pathname-directory path))))

(define map-directory-pathnames (pathname function &key (file-type :text))
  "Maps FUNCTION over the newest version pathnames of the directory, PATHNAME
according to FILE-TYPE."
  (dolist (pathname (directory (make-pathname :name :wild :type (etypecase file-type
								  (string file-type)
								  (symbol
								    (case file-type
								      (:wild file-type)
								      (t (symbol-name file-type)))))
					      :version :newest :defaults pathname)))
    (funcall function pathname))) 

(define pathname-create-directory-if-needed (pathname)
  "Recursively creates the directory PATHNAME if not already present."
  (unless (probe-directory pathname)
    (www-utils:create-directories-recursively pathname)))

(define probe-file* (pathname)
  "Just like probe-file except it doesn't get errors when directories are not found."
  (handler-case
    (probe-file pathname)
    (file-error () nil)))

(define-generic delete-file* (pathname &key expunge &allow-other-keys)
  (:documentation "Deletes FILE and optionally handles expunging the directory."))

(defmethod delete-file* (pathname &key &allow-other-keys)
  (delete-file pathname))

(define-generic expunge-deleted-files (&optional directory)
  (:documentation "Expunges deleted files in DIRECTORY, 
or no directory provided expunges all directories with recently deleted files."))

#-Genera
(defmethod expunge-deleted-files (&optional directory)
  directory)

(define-generic delete-file-carefully (pathname &key &allow-other-keys)
  (declare (values file-deleted-p))
  (:documentation "Deletes FILE after probing to ensure its presence."))

(defmethod delete-file-carefully (pathname &key &allow-other-keys)
  (let ((probe-file (probe-file pathname)))
    (when probe-file
      (delete-file* probe-file :expunge t))))

;; Ports should specialize this.
(defgeneric set-file-author (pathname author &optional error-p)
   (declare (values success-p))
  (:documentation "Sets autheor of PATHNAME to be AUTHOR."))

(defmethod set-file-author (pathname (author string) &optional error-p)
  (declare (ignore pathname error-p))
  nil)

;; Ports should specialize this.
(defgeneric set-file-modification-date (pathname universal-time &optional error-p)
   (declare (values universal-time))
  (:documentation "Sets the modification date of PATHNAME to
UNIVERSAL-TIME."))

(defmethod set-file-modification-date (pathname universal-time &optional error-p)
   (declare (ignore pathname error-p))
   universal-time)

(defsetf file-modification-date set-file-modification-date)

;; Ports should specialize this.
(defgeneric set-file-creation-date (pathname universal-time &optional error-p)
   (declare (values universal-time))
   (:documentation "Sets the creation date of PATHNAME to UNIVERSAL-TIME."))

(defmethod set-file-creation-date (pathname universal-time &optional error-p)
   (declare (ignore pathname error-p))
   universal-time)

(defsetf file-creation-date set-file-creation-date)

;; Relevant for Macintosh ports
(defgeneric autotype-file (pathname &optional report-stream)
   (declare (values pathname))
  (:documentation "Automatically sets the file type and creator of PATHNAME.
Only relevant for certain operating systems. Reports activity on report-stream,
when it is non-null."))

(defmethod autotype-file (pathname &optional report-stream)
  (declare (ignore report-stream))
  pathname)

(defun load-files-safely (files &key (verbose *load-verbose*) (print *load-print*))
  "Load FILES only if they exist, otherwise skips them."
  (dolist (file files)
    (cond ((and (probe-directory file)
                (probe-file file))
           (load file :verbose verbose :print print))
          (print
           (format *terminal-io* "~&File not found for ~A." (pathname file)))
          (t nil))))

;;;------------------------------------------------------------------- 
;;;
;;; POST URL-ENCODED FORM RESOURCE
;;;

(defun make-post-form-buffer (resource &optional (size *post-form-buffer-size*))
  (declare (ignore resource))
  (make-array size :element-type *standard-character-type* :adjustable t :fill-pointer 0))

(defun match-post-form-buffer-p (resource buffer size)
  (declare (ignore resource))
  (let ((array-size (array-total-size buffer)))
    (declare (fixnum size array-size))
    (and (<= size array-size)			;fit inside array?
	 (< (float (/ size array-size)) .10))))	;within hysterisis -- maintain resource distribution & frequency

(defun deinitialize-post-form-buffer (resource post-form-buffer)
  (declare (ignore resource))
  (setf (fill-pointer post-form-buffer) 0)
  post-form-buffer)

;; this must allocate and deallocate rapidly
(defresource post-form-buffer (size)
  :constructor make-post-form-buffer
  :deinitializer deinitialize-post-form-buffer
  :matcher match-post-form-buffer-p)

(define clear-post-form-buffer-resource ()
  (clear-resource 'post-form-buffer))


;;;------------------------------------------------------------------- 
;;;
;;; LINE BUFFER RESOURCE
;;;

(defun make-line-buffer (resource &optional (size *line-buffer-size*))
  (declare (ignore resource))
  (make-array size :element-type *standard-character-type* :adjustable t :fill-pointer 0))

(defun match-line-buffer-p (resource buffer size)
  (declare (ignore resource))
  (<= size (array-total-size buffer)))

(defun deinitialize-line-buffer (resource line-buffer)
  (declare (ignore resource))
  (setf (fill-pointer line-buffer) 0)
  line-buffer)

;; this must allocate and deallocate rapidly
(defresource line-buffer (resource &optional (size *line-buffer-size*))
  :constructor make-line-buffer
  :deinitializer deinitialize-line-buffer
  :matcher match-line-buffer-p)

(define clear-line-buffer-resource ()
  (clear-resource 'line-buffer))

(define-variable *server-line-buffer* nil
                 "Holds the line buffer for a server instance.")

(define-macro with-server-line-buffer (() &body body)
  `(using-resource (*server-line-buffer* line-buffer *line-buffer-size*)
                   ,@body))


;;;------------------------------------------------------------------- 
;;;
;;; READING DELIMITED LINES
;;;

;; Use the EOF handling to prevent running off the end and failing to detect
;; that the server is no longer handing out the bits.  7/17/95 -- JCMa.
(define-generic read-delimited-line (stream &optional delimiters eof buffer)
  (declare (values line error-p delimiter length))
  (:documentation "Reads a line from stream which is delimited by DELIMITERS
The argument EOF is the value returned in case of end of file error.  The
returned value EOF is non-null when no more data remains in STREAM.  It may be
the case that the returned values LINE is a string (non-eof valued) and EOF is
non-null, in which case a line has been read that is not terminated by
DELIMITERS.  This is defined by each port and may be specialized for certain
streams."))

;;;------------------------------------------------------------------- 
;;;
;;; FUNCTIONS
;;;

(eval-when (:compile-toplevel :execute :load-toplevel)
  #+(or Genera MCL LispWorks-UNIX)
  (pushnew :character-translation-skews-file-size *features*))

#+character-translation-skews-file-size
(defvar *crlf-document-data* nil)

(defmacro writing-from-crlf-data-source (&body body)
  "Wrap this macro around CRLF data sources when serving text
in order to assure optimal use of content-length."
  #+character-translation-skews-file-size
  `(let ((*crlf-document-data* t)) . ,body)
  #-character-translation-skews-file-size
  `(progn . ,body))

(define copy-mode-element-type (copy-mode)
  "Returns the element type associated with COPY-MODE."
  (ecase copy-mode
    ((:binary :crlf) '(unsigned-byte 8))
    (:text *standard-character-type*)))

(define-generic stream-copy-until-eof (from-stream to-stream &optional copy-mode)
  (:documentation "Copies all input from FROM-STREAM to TO-STREAM.
COPY-MODE can be any of :TEXT, :BINARY, or :CRLF, but defaults to :TEXT.
Ports and applications may specialize this method to optimize data transfer rates."))

;; ports should specialize this for high performance.
(defmethod stream-copy-until-eof (from-stream to-stream &optional (copy-mode :text))
  (ecase copy-mode
    (:text
      (with-text-stream  (from-stream :input)
        (with-text-stream (to-stream :output)
          (using-resource (buffer line-buffer *line-buffer-size*)
            (loop doing (multiple-value-bind (line eof delimiter length)
                            (read-delimited-line from-stream '(#\Linefeed #\Return) nil buffer)
                          (declare (ignore delimiter))
                          (unless line (return))
                          (write-line line to-stream :start 0 :end length)
                          (when eof (return))))))))
    ((:binary :crlf)
     (with-binary-stream (from-stream :input)
       (with-binary-stream (to-stream :output)
         (loop for byte = (read-byte from-stream nil)
               while byte
               do (write-byte byte to-stream)))))))

(defmethod stream-copy-until-eof (from-stream (pathname string) &optional (copy-mode :text))
  (stream-copy-until-eof from-stream (pathname pathname) copy-mode))

(defmethod stream-copy-until-eof (from-stream (pathname pathname) &optional (copy-mode :text))
  (with-open-file (file-stream pathname :direction :output :if-does-not-exist :create
                               :element-type (copy-mode-element-type copy-mode)
                               :if-exists *file-exists-supersede-action*)
    (stream-copy-until-eof from-stream file-stream copy-mode)))

(defmethod stream-copy-until-eof ((pathname string) to-stream &optional (copy-mode :text))
  (stream-copy-until-eof (pathname pathname) to-stream copy-mode))

(defmethod stream-copy-until-eof ((pathname pathname) to-stream &optional (copy-mode :text))
  (with-open-file (file-stream pathname :direction :input :if-does-not-exist :error
                               :element-type (copy-mode-element-type copy-mode))
    (stream-copy-until-eof file-stream to-stream copy-mode)))

(define-generic stream-copy-input-buffer (from-stream to-stream)
  (:documentation "Copies the input buffer from FROM-STREAM to TO-STREAM and advances the the input buffer.
Ports should specialize this method to enable proxy tunnelling to work."))

;; ports should specialize this for high performance.
;; Should only copy a buffer at a time.
(defmethod stream-copy-input-buffer (from-stream to-stream)
  (with-binary-stream (from-stream :input)
    (with-binary-stream (to-stream :output)
      (loop while (listen from-stream)
            for byte = (read-byte from-stream nil)
            while byte
            do (write-byte byte to-stream)))))

(define-generic advance-input-buffer (stream &optional delta)
  (:documentation "Flushes DELTA bytes of input from STREAM.  DELTA is a positive integer or null.
When delta is null, all input is flushed to the end of the current HTTP message.  Practically, this
method can be viewed as moving the input buffer pointer forward. This method should be specialized
by ports for higher performance."))

;; ports should specialize this for high performance.
(defmethod advance-input-buffer (stream &optional delta)
  (with-binary-stream (stream :input)
    (if delta
        (dotimes (i delta)
          (read-byte stream t))
        (loop while (read-byte stream nil nil)))))

;;;------------------------------------------------------------------- 
;;;
;;; CRLF COPY INTO STRING
;;;

;; Specialize this method on ports for higher performance.
(define-generic crlf-stream-copy-into-string (stream &optional n-bytes start string)
  (declare (values string length))
  (:documentation "Copies the content of STREAM into a string which is returned.  If N-BYTES is
supplied, that many characters will be copied into STRING.  On certain platforms (e.g., Lisp Machine
and MAC), the actual filled length of the returned string may be less than (+ n-bytes start) due to
character translation. Therefore, only the second returned value, length or the fill pointer of
STRING can be relied on in portable code for the actual length.  START is index at which to start
copying. A new string will not be created if STRING is supplied. STRING must have a fill pointer and
accept elements of type *STANDARD-CHARACTER-TYPE*. If n-bytes is not supplied, STRING must be
adjustable."))

;; We're not using read-line because it copies into an array which is thrown
;; away.  If we could copy out of the input buffer chunks at a time, that
;; would be much better.   7/26/95 -- JCMa.
(defmethod crlf-stream-copy-into-string (stream &optional n-bytes (start 0) string &aux size)
  (flet ((make-the-string (size fill-pointer)
	   (make-array size :fill-pointer fill-pointer :adjustable t :element-type *standard-character-type*))
	 (adjust-the-string (string size fill-pointer)
	   (adjust-array string size :fill-pointer fill-pointer :element-type *standard-character-type*))
	 (new-size (size)
	   (cond ((< size 64000) (* 2 size))
		 (t (truncate (* size 1.2))))))
    (declare (inline make-the-string adjust-the-string new-size))
    (cond (n-bytes
	   (setq size (+ n-bytes start))
	   (cond ((null string)
		  (setq string (make-the-string size start)))
		 ((< (array-total-size string) size)
		  (setq string (adjust-array string size :fill-pointer start :element-type *standard-character-type*))))
	   (with-fast-array-references ((string string string))
	     (loop with fill-pointer = start
		   repeat size 
		   for char = (read-char stream t nil nil)
		   do (setf (aref string fill-pointer) char)
			     (incf fill-pointer)
		   finally (setf (fill-pointer string) fill-pointer)
			   (return (values string fill-pointer)))))
	  ;; the size and growth issues are open to experimentation and better
	  ;; algorithms that do less work.  7/26/95 -- JCMa.
	  (t (cond ((null string)
		    (setq size (+ 1000 start)
			  string (make-the-string size start)))
		   (t (setq size (array-total-size string))))
	     (with-fast-array-references ((string string string))
	       (loop with fill-pointer = start
		     for char = (read-char stream nil nil nil)
		     while char
		     do (when (= size fill-pointer)
			  (setq string (adjust-the-string string (setq size (new-size size)) fill-pointer)))
			(setf (aref string fill-pointer) char)
			(incf fill-pointer)
		     finally (setf (fill-pointer string) fill-pointer)
			     (return (values string fill-pointer))))))))


;;;------------------------------------------------------------------- 
;;;
;;; CRLF ENCODE & DECODE
;;;

(define-generic stream-decode-crlf-bytes (from-stream to-stream bytes)
  (:documentation "Copies N-BYTES from FROM-STREAM to TO-STREAM while performing CRLF translation.
The byte count on from-stream is absolute, whereas the bytes output to TO-STREAM may
vary according to the operating system and the number of CRLFs present in FROM-STREAM.
Ports and applications may specialize this method to optimize data transfer rates."))

;; ports should specialize this for high performance.
(defmethod stream-decode-crlf-bytes (from-stream to-stream n-bytes)
  (with-crlf-stream (from-stream :input)
    (with-text-stream (to-stream :output)
      (loop with at-cr-p
	    repeat n-bytes
	    for byte = (read-byte from-stream t)
	    for char = (code-char byte)
	    do (cond ((and at-cr-p (prog1 (eql char #\Linefeed) (setq at-cr-p nil))))
		     ((member char '(#\Return #\Linefeed))
		      (terpri to-stream)
		      (setq at-cr-p t))
		     (t (write-char char to-stream)))))))

(define-generic stream-decode-crlf-until-eof (from-stream to-stream)
  (:documentation "Copies FROM-STREAM to TO-STREAM,  converting CRLF in
FROM-STREAM to CR (or LF depending on platform) in TO-STREAM.
Ports and applications may specialize this method to optimize data transfer rates."))

;; ports should specialize this for high performance.
(defmethod stream-decode-crlf-until-eof (from-stream to-stream)
  (using-resource (line-buffer line-buffer *line-buffer-size*)
    (loop doing (multiple-value-bind (line eof delimiter length)
		    (read-delimited-line from-stream '(#\Linefeed #\Return) nil line-buffer)
		  (declare (ignore delimiter))
		  (unless line (return))
		  (write-line line to-stream :start 0 :end length)
		  (when eof (return))))))

(defmethod stream-decode-crlf-until-eof (from-stream (to-pathname pathname))
  (with-open-file (file to-pathname :direction :output :element-type *standard-character-type*
			:if-does-not-exist :create 
			:if-exists *file-exists-supersede-action*)
    (stream-decode-crlf-until-eof from-stream file)))

(define-generic stream-encode-crlf-until-eof (from-stream to-stream)
  (:documentation "Copies FROM-STREAM to TO-STREAM,  canonicalizing bare CR, 
bare LF, or CRLF in FROM-STREAM to CRLF in TO-STREAM.
Ports and applications may specialize this method to optimize data transfer rates."))

;; ports should specialize this for high performance.
(defmethod stream-encode-crlf-until-eof (from-stream to-stream)
  (using-resource (line-buffer line-buffer *line-buffer-size*)
    (loop with line and eof and delimiter and length
	  do (multiple-value-setq (line eof delimiter length)
	       (read-delimited-line from-stream '(#\Return #\Linefeed) nil line-buffer))
	  unless (zerop length)
	    do (with-fast-array-references ((array line vector))
		 delimiter			;ignore
		 (loop for idx upfrom 0 below length
		       do (write-byte (char-code (aref array idx)) to-stream)))
	  until eof
	  do (write-byte #.(char-code #\Return) to-stream)
	     (write-byte #.(char-code #\Linefeed) to-stream))))

(defmethod stream-encode-crlf-until-eof ((from-pathname pathname) to-stream)
  (with-open-file (file from-pathname :direction :output :element-type *standard-character-type*
			:if-does-not-exist :error)
    (stream-encode-crlf-until-eof file to-stream)))

(define-variable *crlf-type-alist* '(("html" . "html-crlf") ("text" . "text-crlf"))
                 "Caches the CRLF pathname type for a particular extension.")

(define crlf-pathname-type (type)
  "Returns the CRLF type given TYPE, the type of pathname."
  (let ((entry (assoc type *crlf-type-alist* :test #'equalp)))
    (cond (entry (cdr entry))
          (t (setq entry (case type
                           ((:unspecific nil) *pathname-crlf-extension*)
                           (t (concatenate 'string type "-" *pathname-crlf-extension*)))
                   *crlf-type-alist* `(,.*crlf-type-alist* (,type . ,entry)))
             entry))))

(define crlf-pathname-origin-type (type)
  "Returns the original pathname type given the CRLF type, TYPE."
  (let ((entry (rassoc type *crlf-type-alist* :test #'equalp)))
    (cond (entry (car entry))
          (t (error "~S is not a known CRLF pathname type." type)))))

(define-generic crlf-pathname (pathname)
  (declare (values crlf-pathname))
  (:documentation "Returns the canonical text pathname for PATHNAME.
Ports may wish to specialize this to handle vagaries of pathname in their filesystem."))

(defmethod crlf-pathname (pathname)
  (let ((crlf-type (crlf-pathname-type (pathname-type pathname))))
    (setf (pathname-type (pathname pathname)) crlf-type)))

(defmethod crlf-pathname ((pathname string))
  (crlf-pathname (pathname pathname)))

(define-generic crlf-origin-pathname (pathname)
  (declare (values crlf-origin-pathname))
  (:documentation "Returns the origin pathname for the CRLF pathname, PATHNAME.
Ports may wish to specialize this to handle vagaries of pathname in their filesystem."))

(defmethod crlf-origin-pathname (pathname)
  (let ((crlf-type (crlf-pathname-origin-type (pathname-type pathname))))
    (make-pathname :defaults pathname :type crlf-type)))

(defmethod crlf-origin-pathname ((pathname string))
  (crlf-origin-pathname (pathname pathname)))

(declaim (inline %crlf-pathname-type-p))

(defun %crlf-pathname-type-p (pathname-type)
  (not (null (rassoc pathname-type *crlf-type-alist* :test #'equalp))))

(define-generic crlf-pathname-p (pathname)
  (:documentation "Returns non-null when PATHNAME is a CRLF canonical pathname."))

(defmethod crlf-pathname-p (pathname)
  (%crlf-pathname-type-p (pathname-type pathname)))

(defmethod crlf-pathname-p ((pathname string))
  (crlf-pathname-p (pathname pathname)))

(declaim (inline %crlf-canonicalizable-pathname-type-p))

(defun %crlf-canonicalizable-pathname-type-p (pathname-type)
  (not (null (assoc pathname-type *crlf-type-alist* :test #'equalp))))

(define-generic crlf-canonicalizable-pathname-p (pathname)
  (:documentation "Returns non-null if pathname accepts CRLF canonicalization."))

(defmethod crlf-canonicalizable-pathname-p (pathname)
  (%crlf-canonicalizable-pathname-type-p (pathname-type pathname)))

(defmethod crlf-canonicalizable-pathname-p ((pathname string))
  (crlf-canonicalizable-pathname-p (pathname pathname)))

(define-generic crlf-canonicalize-file (pathname &optional destination-pathname)
  (declare (values crlf-canonicalized-pathname))
  (:documentation "CRLF canonicalizes the text file PATHNAME.
The CRLF canonicalized version of PATHNAME is written to
DESTINATION-PATHNAME, which defaults to PATHNAME merged with a CRLF
extension. The returned value, CRLF-CANONICALIZED-PATHNAME, is
the truename of the destination pathname."))

(defmethod crlf-canonicalize-file (pathname &optional destination-pathname &aux source destination true-destination)
  (if destination-pathname
      (setq source pathname
            destination destination-pathname)
      (setq source (probe-file pathname)
            destination (crlf-pathname source)))
  (with-open-file (file source :direction :input :element-type *standard-character-type* :if-does-not-exist :error)
    (with-open-file (to-stream destination :direction :output :element-type '(unsigned-byte 8)
                               :if-exists :supersede :if-does-not-exist :create)
      (stream-encode-crlf-until-eof file to-stream)
      (setq true-destination (truename to-stream)))
    ;; Update file properties
    (let ((modification-date (file-stream-modification-date file))
	  #-UNIX ;; creation date is not available under unix
	  (creation-date (file-stream-creation-date file)))
      (cond-every
	(modification-date (setf (file-modification-date true-destination) modification-date)) 
	#-UNIX
	(creation-date (setf (file-creation-date true-destination) creation-date)))))
  ;; return the true pathname
  true-destination)

(defmethod crlf-canonicalize-file ((pathname string) &optional destination-pathname)
  (crlf-canonicalize-file (pathname pathname) destination-pathname))

(define-generic decode-crlf-file (pathname &optional destination-pathname)
  (declare (values crlf-canonicalized-pathname))
  (:documentation "Decodes a CRLF canonical the text file PATHNAME.
The decoded version of PATHNAME is written to
DESTINATION-PATHNAME, which defaults to PATHNAME unmerged with a CRLF
extension.  Unless DESTINATION-PATHNAME is provided, PATHNAME is probed
in order to synchronize versions.  When DESTINATION-PATHNAME is
supplied, the caller is responsible for any file probes that might be
necessary."))

(defmethod decode-crlf-file (pathname &optional destination-pathname &aux source destination true-destination)
  (if destination-pathname
      (setq source pathname
            destination destination-pathname)
      (setq source (probe-file pathname)
            destination (crlf-origin-pathname source)))
  (with-open-file (file source :direction :input :element-type (copy-mode-element-type #+Genera :binary #-Genera :text)
			:if-does-not-exist :error)
    (with-open-file (to-stream destination :direction :output :element-type *standard-character-type*
                               :if-exists :supersede :if-does-not-exist :create)
      (stream-decode-crlf-until-eof file to-stream)
      (setq true-destination (truename to-stream)))
    ;; Update file properties
    (let ((modification-date (file-stream-modification-date file))
	  #-UNIX ;; creation date is not available under unix
	  (creation-date (file-stream-creation-date file)))
      (cond-every
	(modification-date (setf (file-modification-date true-destination) modification-date)) 
	#-UNIX
	(creation-date (setf (file-creation-date true-destination) creation-date)))))
  ;; return the true pathname
  true-destination)

(defmethod decode-crlf-file ((pathname string) &optional destination-pathname)
  (decode-crlf-file (pathname pathname) destination-pathname))

(define-generic valid-crlf-cache-file-p (pathname)
  (declare (values valid-crlf-cache-p source-pathname crlf-pathname cache-pathname))
  (:documentation "Returns non-null when the CRLF cache file for PATHNAME is valid.
The values SOURCE-PATHNAME and CRLF-PATHNAME are the truenames of the files as
returned by probe-file. These can also be null when no corresponding file exists.
CACHE-PATHNAME is file where the CRLF cache should be for SOURCE-PATHNAME."))

(defmethod valid-crlf-cache-file-p (pathname &aux source canonical c-probe)
  (cond ((and (setq source (probe-file pathname))
              (setq canonical (crlf-pathname source))
              (setq c-probe (probe-file canonical))
              (< (file-modification-date source) (file-modification-date c-probe)))
         (values t source c-probe))
        (t (values nil source c-probe canonical))))

(defmethod valid-crlf-cache-file-p ((pathname string))
  (valid-crlf-cache-file-p (pathname pathname)))

(define-generic ensure-crlf-canonical-file (pathname)
  (declare (values crlf-canonicalized-pathname newly-updated-p))
  (:documentation "Returns the CRLF canonicalized version of pathname.
Ensures that the CRLF version exists and is current.
The second returned value, newly-updated-p, is non-null whenever
a new CRLF file is written."))

;; this should have locking to avoid race conditions.   6/24/96 -- JCMa.
(defmethod ensure-crlf-canonical-file (pathname)
  (declare (values crlf-canonicalized-pathname newly-updated-p))
  (multiple-value-bind (valid-crlf-cache-p source-pathname crlf-pathname canonical-pathname)
      (valid-crlf-cache-file-p pathname)
    (cond (valid-crlf-cache-p crlf-pathname)
          (t (values (crlf-canonicalize-file source-pathname canonical-pathname) t)))))

(defmethod ensure-crlf-canonical-file ((pathname string))
  (ensure-crlf-canonical-file (pathname pathname)))

(define crlf-file-parameters (pathname)
  "Returns file parameters for the CRLF version of pathname."
  (declare (values (length-in-bytes last-modification version)))
  (let ((crlf-pathname (ensure-crlf-canonical-file pathname)))
    (file-properties crlf-pathname)))

(define crlf-file-length-in-bytes (pathname)
  "Returns file length for the CRLF version of pathname."
  (declare (values (length-in-bytes)))
  (let ((crlf-pathname (ensure-crlf-canonical-file pathname)))
    (file-length-in-bytes crlf-pathname)))


;;;------------------------------------------------------------------- 
;;;
;;; STANDARDIZING LINE BREAKS
;;;

(defgeneric text-file-p (pathname)
  (:documentation "Returns non-null when pathname is known to be a text file.
Platforms may specialize this method for better heuristic accuracy."))

(defmethod text-file-p (pathname)
  (let ((text-file-types `("lisp" "html" "htm" "text" "txt" "xml" "ps" "cern-map" "mak" "ncsa-map"
                                  "c" "cl" "script" "translations" "hqx")))
    (or (member (pathname-type pathname) text-file-types :test #'equalp)
        (eql :crlf (pathname-copy-mode pathname nil)))))

(define-generic stream-standardize-line-breaks-until-eof (from-stream to-stream line-break)
  (:documentation "Copies FROM-STREAM to TO-STREAM, canonicalizing line breaks 
to LINE-BREAK, which can be any one of :CR (carriage return), :LF (Linefeed), 
or :CRLF (return linefeed).  Ports and applications may specialize this method 
to optimize data transfer rates."))

;; ports should specialize this for high performance.
(defmethod stream-standardize-line-breaks-until-eof (from-stream to-stream line-break)
  (with-text-stream (from-stream :input)
    (with-text-stream (to-stream :output)
      (using-resource (line-buffer line-buffer *line-buffer-size*)
        (loop (multiple-value-bind (line eof delimiter length)
		  (read-delimited-line from-stream '(#\Return #\Linefeed) nil line-buffer)
		delimiter			;ignore
		(unless (zerop length)
		  (write-string line to-stream :start 0 :end length))
		(when eof (return))
		(ecase line-break
		  (:lf (write-char #\Linefeed to-stream))
		  (:cr (write-char #\Return to-stream))
		  (:crlf 
                    (write-char #\Return to-stream)
                    (write-char #\Linefeed to-stream)))))))))

(defgeneric standardize-line-breaks (pathname &optional standard-line-break stream)
  (:documentation "Standardizes all line breaks for PATHNAME according
to STANDARD-LINE-BREAK, which can be any of:

     :LF   -- Linefeed (UNIX)
     :CR   -- Carriage Return (MacOS Classic)
     :CRLF -- Carriage Return followed by Linefeed (PC)

PATHNAME can be either an individual file or a directory.  When it is
a directory, all text files within it are standardized. If STREAM is
provided, progress is reported."))

(defmethod standardize-line-breaks ((pathname string) &optional (standard-line-break :lf) stream)
   (standardize-line-breaks (pathname pathname) standard-line-break stream))

(defmethod standardize-line-breaks :around ((pathname pathname) &optional (standard-line-break :lf) stream)
  (if (pathname-directory-p pathname)
      (dolist (path #+(or MCL LispWorks Genera)
                    (directory (merge-pathnames "*.*" pathname) :directories t)
                    #-(or MCL LispWorks Genera)
                    (directory (merge-pathnames "*.*" pathname)))
	(standardize-line-breaks path standard-line-break stream))
      (call-next-method pathname standard-line-break stream)))

(defmethod standardize-line-breaks ((pathname pathname) &optional (standard-line-break :lf) stream)
  (flet ((%standardize-file-line-breaks (pathname line-break stream)
           (let ((temp (make-pathname :name (concatenate 'string (pathname-name pathname) "-temp") :defaults pathname))
                 (old (make-pathname :name (concatenate 'string (pathname-name pathname) "-old") :defaults pathname))
                 (modification-date (file-modification-date pathname))
                 #-UNIX
                 (creation-date (file-creation-date pathname))
                 (author (file-author pathname)))
             (with-open-file (file-in pathname :direction :input :element-type *standard-character-type* :if-does-not-exist :error)
               (with-open-file (file-out temp :direction :output :element-type *standard-character-type* :if-exists :supersede)
                 (stream-standardize-line-breaks-until-eof file-in file-out line-break)))
             (when modification-date
               (set-file-modification-date temp modification-date))
             #-UNIX
             (when creation-date
               (set-file-creation-date temp creation-date))
             (when author
               (set-file-author temp author nil))
             (rename-file pathname old)
             (rename-file temp pathname)
             (delete-file old)
             (when stream
               (format stream "~&Standardize Line Breaks (~A): ~A" line-break pathname))
             pathname)))
    (declare (inline %standardize-file-line-breaks))
    (when (text-file-p pathname)
      (tagbody
       start
       (restart-case
           (return-from standardize-line-breaks 
             (%standardize-file-line-breaks pathname standard-line-break stream))
         (retry-load () :report (lambda (stream) (format stream "Retry standardizing ~A in ~A" standard-line-break pathname))
                     (go start))
         (skip-load () :report (lambda (stream) (format stream "Skip standardizing ~A in ~A" standard-line-break pathname))
                    (return-from standardize-line-breaks nil)))))))

;;;------------------------------------------------------------------- 
;;;
;;; FILE COMPRESSION
;;;

;; ports should specialize this to enable capabilities
(defgeneric compress-file (pathname mode &key delete)
  (declare (values compressed-pathname))
  (:documentation "Compresses PATHNAME according to MODE.
MODES are: COMPRESS, DECOMPRESS.
Ports should specialize this method to provide compression and decompression capabilities."))

;; Default method is a no-op
(defmethod compress-file (pathname mode &key delete)
  (declare (ignore pathname mode delete))
  nil)

(defgeneric compressed-file-p (pathname)
  (:documentation "Returns non-null when PATHNAME is a compressed file."))

(defgeneric compress-directory (pathname mode &key delete recursive report-stream)
  (declare (values pathname))
  (:documentation "Compresses all the files in the directory, PATHNAME, according to compression MODE.
Wild-card patterns may be supplied in PATHNAME to the extent that the file system supports them.
When DELETE is non-null, input files are delete after they are compressed.
When RECURSIVE is non-null, directories are recursively descended.
When REPORT-STREAM is non-null, activity is reported on REPORT-STREAM."))

(defmethod compress-directory ((pathname pathname) mode &key delete recursive (report-stream *standard-output*))
  (loop for path in (directory (pathname-as-directory pathname))
	when (pathname-directory-p path)
	  do (when recursive
	       (compress-directory path mode :delete delete :recursive t))
	else do (when report-stream (format report-stream "~&Compressing ~A ...." path))
		(multiple-value-bind (compressed-pathname)
		    (compress-file path mode :delete delete)
		  (when report-stream (format report-stream "~&~A compressed into ~A." path compressed-pathname))))
  pathname)

(defmethod compress-directory ((pathname string) mode &key delete recursive (report-stream *standard-output*))
  (compress-directory (pathname pathname) mode :delete delete :recursive recursive :report-stream report-stream))


;;;------------------------------------------------------------------- 
;;;
;;; COPYING BYTES
;;;

(define byte-range-parameters (start last resource-length)
  "Returns values suitable for returning a resource range.
START and LAST are the positions of the first and last bytes.
The logic is defined by the HTTP 1.1 spec in the section discussing ranges.
Ports and applications may specialize this method to optimize data transfer rates."
  (declare (values start end content-length invalid-range-spec-p))
  (check-type resource-length integer)
  (cond ((and start last)
         (cond ((and (integerp start) (integerp last) (< (the integer start) (the integer last)))
                (values start (1+ (the integer last)) (- (1+ (the integer last)) (the integer start))))
               (t (values nil nil nil t))))
        ((and start (integerp start) (< resource-length))
         (values start resource-length (- resource-length start)))
        ((and last (integerp last) (< last resource-length))
         (values (- resource-length last) resource-length last))
        ((or start last)
         (values nil nil nil t))
        (t nil)))

;; Compresses the byte range spec to make multipart blocks larger or collapse them into a single range
;; Compression confuses the Adobe Reader - JCMa 7/13/2009
#|(defun ncompress-byte-range-spec (spec &aux drop-entries)
  (flet ((compress-byte-range-1 (item1 more)
           (let ((item2 (first more)))
             (cond ((= (the integer (second item1)) (the integer (first item2)))
                    (setf (second item1) (second item2) 
                          (third item1) (+ (the integer (third item1)) (the integer (third item2))))
                    (push item2 drop-entries)
                    (cdr more))
                   ((and (= (the integer (first item1)) (the integer (first item2)))
                         (the integer (second item1)) (the integer (second item2)))
                    (push item2 drop-entries)
                    (cdr more))))))
    (declare (inline compress-byte-range-1))
    (loop for (item . more) = (print (sort spec #'< :key #'car)) then more
          while more
          do (loop doing (multiple-value-bind (next-more)
                             (compress-byte-range-1 item more)
                           (cond ((or (null next-more) (eq more next-more))
                                  (return))
                                 (t (setq more next-more)))))
          finally (return (if drop-entries
                              (delete-if #'(lambda (x) (member x drop-entries :test #'eq)) spec)
                            spec)))))|#

#| (parse-multibyte-range-parameters "http://foo" (cdr (parse-range-header "bytes=2000-2100,500-700,500-700,601-999,1000-1999,500-700,500-700,")) 9999)|#

(defun parse-multibyte-range-parameters (url byte-range-spec resource-length &optional error-p)
  (declare (values byte-range-spec invalid-range-spec-p))
  (let (start end content-length invalid-spec-p invalid-range-spec-p)
    (loop for entry in byte-range-spec
          do (destructuring-bind (first last) entry
               (multiple-value-setq (start end content-length invalid-spec-p)
                 (byte-range-parameters first last resource-length)))
          when (and start end content-length)
          collect (list start end content-length) into result
          else do (when invalid-spec-p (setq invalid-range-spec-p t))
          finally (cond (result (return result #+ignore (ncompress-byte-range-spec result)))
                        (invalid-range-spec-p
                         (if error-p
                             (signal 'invalid-range-request :url url :format-string "Invalid Range Header: ~S" :format-args (list (get-raw-header :range)))
                           (return (values nil t))))))))

(define-generic stream-copy-byte-range (from-stream to-stream start end)
  (:documentation "Copies bytes from from-stream to to-stream.
The copy starts with START and continues upto but not including END."))

;; ports should specialize this for high performance.
(defmethod stream-copy-byte-range (from-stream to-stream start end)
  (cond ((file-position from-stream start)
         (with-binary-stream (to-stream :output)
           (loop for bytes upfrom start below end
                 while (< bytes end)
                 do (write-byte (read-byte from-stream) to-stream))))
        (t (error "Unable to set file position for byte range copy."))))

(define-generic stream-copy-bytes (from-stream to-stream n-bytes &optional copy-mode)
  (:documentation "Copies N-BYTES from FROM-STREAM to TO-STREAM.
COPY-MODE can be any of :TEXT, :BINARY, or :CRLF, but defaults to :BINARY.
Ports and applications may specialize this method to optimize data transfer rates."))

;; ports should specialize this for high performance.
(defmethod stream-copy-bytes (from-stream to-stream n-bytes &optional (copy-mode :binary))
  (ecase copy-mode
    ((:binary :crlf)
     (loop for bytes upfrom 0
           while (< bytes n-bytes)
           do (write-byte (read-byte from-stream t) to-stream)))
    ;; when copying from a local stream to an http-stream, this should perform
    ;; CRLF translations (implementation: CRLF, bare CR, or bare LF => CRLF)
    ;; when copying from an http-stream to a local stream, this should perform
    ;; CRLF decoding (implementation: CRLF, bare CR, or bare LF => native line break)
    (:text
      (loop for bytes upfrom 0
	    while (< bytes n-bytes)
	    do (write-char (read-char from-stream t) to-stream)))))

(defmethod stream-copy-bytes (from-stream (pathname string) bytes &optional (copy-mode :binary))
  (stream-copy-bytes from-stream (pathname pathname) bytes copy-mode))

(defmethod stream-copy-bytes (from-stream (pathname pathname) bytes &optional (copy-mode :binary))
  (with-open-file (file-stream pathname :direction :output :if-does-not-exist :create
                               :element-type (copy-mode-element-type copy-mode)
                               :if-exists *file-exists-supersede-action*)
    (stream-copy-bytes from-stream file-stream bytes copy-mode)))

(defmethod stream-copy-bytes ((pathname string) to-stream bytes &optional (copy-mode :binary))
  (stream-copy-bytes (pathname pathname) to-stream bytes copy-mode))

(defmethod stream-copy-bytes ((pathname pathname) to-stream bytes &optional (copy-mode :binary))
  (with-open-file (file-stream pathname :direction :input :if-does-not-exist :error
                               :element-type (copy-mode-element-type copy-mode))
    (stream-copy-bytes file-stream to-stream bytes copy-mode)))

;; Specialize this method on ports for higher performance.
(define-generic binary-stream-copy-into-8-bit-array (stream n-bytes &optional start 8-bit-array)
  (declare (values 8-bit-array length))
  (:documentation "Copies N-BYTES bytes from STREAM into an array which is returned.
The returned array will have size (+ start n-bytes)  and a fill pointer.
A new array is not created when 8-BIT-ARRAY is supplied.
If N-BYTES is NIL, this copies bytes until EOF."))

(defmethod binary-stream-copy-into-8-bit-array (stream n-bytes &optional (start 0) 8-bit-array &aux size)
  (flet ((make-the-array (size fill-pointer)
           (make-array size :fill-pointer fill-pointer :adjustable t :element-type '(unsigned-byte 8)))
         (adjust-the-array (array size fill-pointer)
           (let ((new-array (adjust-array array size :fill-pointer fill-pointer :element-type '(unsigned-byte 8))))
             #+testing(unless (eq new-array array) (format t "New array in adjustment."))
             new-array))
         (new-size (size)
           (cond ((< size 64000) (* 2 size))
                 (t (truncate (* size 1.2))))))
    (declare (inline make-the-array adjust-the-array new-size))
    (cond (n-bytes
           (setq size (+ n-bytes start))
           (cond ((null 8-bit-array)
                  (setq 8-bit-array (make-the-array size start)))
                 ((< (array-total-size 8-bit-array) size)
                  (setq 8-bit-array (adjust-the-array 8-bit-array size start))))
           (with-fast-array-references ((array 8-bit-array array))
             (loop for idx upfrom start below size 
                   for byte = (read-byte stream t)
                   do (setf (aref array idx) byte)
                   finally (setf (fill-pointer array) idx)
                           (return (values array idx)))))
          ;; the size and growth issues are open to experimentation and better
          ;; algorithms that do less work.  7/26/95 -- JCMa.
          (t (cond ((null 8-bit-array)
                    (setq size (+ 1000 start)
                          8-bit-array (make-the-array size start)))
                   (t (setq size (array-total-size 8-bit-array))))
             (with-fast-array-references ((array 8-bit-array array))
               (loop with fill-pointer = start
                     for byte = (read-byte stream nil nil)
                     while byte
                     do (when (= size fill-pointer)
                          (setq array (adjust-the-array array (setq size (new-size size)) fill-pointer)))
                        (setf (aref array fill-pointer) byte)
                        (incf fill-pointer)
                     finally (setf (fill-pointer array) fill-pointer)
                             (return (values array fill-pointer))))))))

;; Specialize this method on ports for higher performance.
(define-generic binary-stream-copy-from-8-bit-array (from-array stream &optional start end)
  (:documentation "Copies the contents of FROM-ARRAY to STREAM.
FROM-ARRAY must a one dimensional array of 8 bit bytes and have a fill pointer."))

(defmethod binary-stream-copy-from-8-bit-array (from-array stream &optional (start 0) end)
   (declare (fixnum start))
   (with-fast-array-references ((from-array from-array array))
       (loop for idx fixnum upfrom start below (the fixnum (or end (length from-array)))
                do (write-byte (aref from-array idx) stream))))

;;;------------------------------------------------------------------- 
;;;
;;; CONDITIONAL FILE COPY
;;;

(defmacro with-copy-file-environment ((from to stream &optional (block-name 'copy-file)) &body body)
  `(tagbody
     retry
	(when ,stream
	  (format ,stream "~&Copying ~A to ~A . . ." (name-string ,from) (name-string ,to)))
	(restart-case
	  (return-from ,block-name
	    (multiple-value-prog1 (progn ,@body)
	      (when ,stream
		(format ,stream "~&Copied ~A to ~A." (name-string ,from) (name-string ,to)))))
	  (retry ()
		 :report (lambda (stream)
			   (format stream "Retry copying ~A" (name-string ,from)))
		 (go retry))
	  (skip ()
		:report (lambda (stream)
			  (format stream "Skip copying ~A" (name-string ,from)))
		(when ,stream
		  (format ,stream "~A not copied." (name-string ,from)))
		(return-from ,block-name nil)))))

(define-generic copy-file (source destination &key copy-mode report-stream		
				  request-headers user-email-address &allow-other-keys)
  (declare (values destination))
  (:documentation "A portable file copy that works over files and URLs.

  Without the base client loaded, this copies files or lists of files.  If the
  name and type are omitted in DESTINATION, they are defaulted from SOURCE. When
  SOURCE is a list, files are copied to the directory specified in DESTINATION,
  with appropriate merging of name and type.

  COPY-MODE is one of :TEXT, :CRLF, or :BINARY.  When REPORT-STREAM is non-null,
  COPY-FILE reports its activities.

  With the base client loaded, copying can be done for any combination of
  pathnames and URLs. This facility is particularly useful in conjunction with
  writable Web directories provided by CL-HTTP (see: EXPORT-URL).

  When copying to a URL, REQUEST-HEADERS are a header property list that is sent
  to the destination server in addition to the normal request headers. Similarly,
  USER-EMAIL-ADDRESS identifies the author of the destination file."))

;; Ports may optionally specialize this method to pathnames to use platform specific file copying and/or property setting.
(defmethod copy-file (from-pathname to-pathname &key (copy-mode :text) (copy-creation-date t) report-stream &allow-other-keys)
  (with-copy-file-environment (from-pathname to-pathname report-stream)
    (let ((element-type (copy-mode-element-type copy-mode))
	  modification-date #-UNIX creation-date #+CL-HTTP-FILE-AUTHOR author)
      (with-open-file (from from-pathname :direction :input :element-type element-type :if-does-not-exist :error)
	(with-open-file (to to-pathname :direction :output :element-type element-type :if-exists :supersede
			    :if-does-not-exist :create)
	  (stream-copy-until-eof from to copy-mode))
	(when copy-creation-date
	  (cond-every
	    ((setq modification-date (file-stream-modification-date from))
	     (setf (file-modification-date to-pathname) modification-date)) 
	    #-UNIX ;; creation date is not available under unix
	    ((setq creation-date (file-stream-creation-date from))
	     (setf (file-creation-date to-pathname) creation-date)))))
      #+CL-HTTP-File-Author			;can we get the file author from the stream?
      (when (and copy-creation-date (setq author (file-author from-pathname)))
	(set-file-author to-pathname author nil))
      to-pathname)))

(defmethod copy-file ((from-pathname string) to-pathname &rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (apply #'copy-file (if (url:fully-qualified-url-string-p from-pathname)
                         (url:intern-url from-pathname :if-does-not-exist :uninterned)
                       (pathname from-pathname))
         to-pathname args))

(defmethod copy-file (from-pathname (to-pathname string) &rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (apply #' copy-file from-pathname 
         (if (url:fully-qualified-url-string-p to-pathname)
             (url:intern-url to-pathname :if-does-not-exist :uninterned)
           (pathname to-pathname)) args))

(define-generic conditional-copy-file (from-pathname to-pathname &key &allow-other-keys)
  (:documentation "Copies FROM-PATHNAME to TO-PATHNAME only when TO-PATHNAME
has an earlier write date than FROM-PATHNAME or does not exist."))

(defmethod conditional-copy-file (from-pathname to-pathname &rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (let ((from-path (probe-file from-pathname))
        (to-path (probe-file to-pathname))
        (to-directory (probe-directory to-pathname)))
    (when (and from-path
               to-directory
               (or (null to-path)
                   (> (file-write-date from-path)
                      (file-write-date to-path))))
      (apply #'copy-file from-pathname to-pathname args))))

(defmethod conditional-copy-file ((from-pathname string) (to-pathname string) &rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (apply #'conditional-copy-file (pathname from-pathname) (pathname to-pathname) args))

;;;------------------------------------------------------------------- 
;;;
;;; MIME MULTIPART STREAM DECODING
;;;

(defparameter *mime-multipart-block-maximum-size* nil
  "When non-null, this controls the maximum number of bytes that will be copied from a MIME multipart block.")

(defparameter *mime-multipart-block-maximum-size-exceeded-error-function* 'default-signal-mime-multipart-block-maximum-size-exceeded
  "The function used to signal an error when the number of bytes in a MIME multipart block exceeds a maximum threshold.")

(defun default-signal-mime-multipart-block-maximum-size-exceeded (bytes maximum-bytes)
  (error "MIME Multipart Block Size Exceeded: Attempt to decode ~D bytes which is more than the allowed maximum, ~D bytes."
	 bytes maximum-bytes))

(defun signal-mime-multipart-block-maximum-size-exceeded (bytes maximum-bytes)
  (funcall *mime-multipart-block-maximum-size-exceeded-error-function* bytes maximum-bytes))

(define-macro with-mime-multipart-block-size-limit ((maximum-bytes &key (error-function 'default-signal-mime-multipart-block-maximum-size-exceeded))
						    &body body)
  "Controls the maximum bytes than a MIME multipart block may contain witn the scope of BODY.
ERROR-FUNCTION is a function that receives the arguments (BYTES MAXIMUM-BYTES) and signals an appropriate error."
  `(let ((*mime-multipart-block-maximum-size* ,maximum-bytes)
	 (*mime-multipart-block-maximum-size-exceeded-error-function* ,error-function))
     ,@body))

(defun mime-boundary-line-p (line boundary &optional (line-length (length line)) (boundary-length (length boundary))
                                  &aux pos pos2)
  "Returns non-null when line matches the MIME multipart boundary, BOUNDARY."
  (declare (values boundary-p last-block-p)
	   (fixnum line-length boundary-length))
  (if (and (> line-length boundary-length)
           (%string-equal "--" line 0 2 0 2)
           (%string-equal boundary line 0 boundary-length 2 (setq pos (+ 2 boundary-length))))
      (values t (and (>= line-length (setq pos2 (+ 2 (the fixnum pos))))
		     (%string-equal "--" line 0 2 pos pos2)))
      (values nil)))

;; Specialize this method on ports for higher performance.
(defgeneric mime-stream-decode-until-boundary (mime-stream to-stream boundary mode &optional buffer)
  (declare (values last-block-p))
  (:documentation "Decodes MIME multipart data from MIME-STREAM and copies it to TO-STREAM upto BOUNDARY
according to MODE. MODE can be any of :TEXT, CRLF, :BINARY.
Specialize these methods for higher performance on specific platforms."))

(defmethod mime-stream-decode-until-boundary (mime-stream to-stream boundary (mode (eql :text)) &optional buffer
							  &aux (max-bytes *mime-multipart-block-maximum-size*) (total-bytes 0))
  (declare (values last-block-p)
	   (integer total-bytes))
  (macrolet ((check-size-limit (n-chars total-bytes max-bytes &optional crlf-p)
	       `(when (and ,max-bytes (> (setq ,total-bytes (+ ,total-bytes (the fixnum ,n-chars)
							       ,@(when crlf-p `((if ,crlf-p 2 0)))))
					 (the integer ,max-bytes)))
		  (signal-mime-multipart-block-maximum-size-exceeded ,total-bytes ,max-bytes))))
    (with-text-stream (mime-stream :input)
      (loop with subsequent-line-p and  boundary-length = (length boundary)
	    doing (multiple-value-bind (line error-p delmiter length)
		      (read-delimited-line mime-stream '(#\Return #\Linefeed) nil buffer)
		    (when error-p
		      (return nil))
		    (multiple-value-bind (finish-p last-block-p)
			(mime-boundary-line-p line boundary length boundary-length)
		      (cond (finish-p (return (values last-block-p)))
			    ((member delmiter '(#\Return #\Linefeed))
			     (check-size-limit length total-bytes max-bytes subsequent-line-p)
			     (if subsequent-line-p (terpri to-stream) (setq subsequent-line-p t))
			     (write-string line to-stream :start 0 :end length))
			    (t (check-size-limit length total-bytes max-bytes subsequent-line-p)			     
			       (write-string line to-stream :start 0 :end length)))))))))

(defmethod mime-stream-decode-until-boundary (mime-stream to-stream boundary (mode (eql :binary)) &optional buffer
							  &aux (max-bytes *mime-multipart-block-maximum-size*) (total-bytes 0))
  (declare (values last-block-p)
	   (ignore buffer)
	   (integer total-bytes))
  (macrolet ((binary-char-code (char)
	       (typecase char
		 (#.*standard-character-type* #+Genera (si:ascii-code char) #-Genera (char-code char))
		 (t #+Genera `(si:ascii-code ,char) #-Genera `(char-code ,char)))))
    (labels ((fill-boundary-array (array boundary boundary-length)
	       (with-fast-array-references ((array array vector)
					    (boundary boundary string))
		 (loop initially (setf (aref array 0) (binary-char-code #\Return)
				       (aref array 1) (binary-char-code #\Linefeed) )
		       for i fixnum below boundary-length
		       do (setf (aref array (+ 4 i)) (char-code (aref boundary i))))))
	     (protected-write-byte (byte stream)
	       (and max-bytes
		    (> (incf total-bytes) (the integer max-bytes))
		    (signal-mime-multipart-block-maximum-size-exceeded total-bytes max-bytes))
	       (write-byte byte stream))
	     (write-array-portion (to-stream array n &rest extra-bytes)
	       (declare (dynamic-extent extra-bytes))
	       (with-fast-array-references ((array array vector))
		 (loop for idx fixnum upfrom 0 below n
		       do (protected-write-byte (aref array idx) to-stream)))
	       (dolist (byte extra-bytes)
		 (protected-write-byte byte to-stream)))
	     (check-for-mime-boundry (mime-stream to-stream boundary-array boundary-array-length)
	       (with-fast-array-references ((boundary-array boundary-array vector))
		 (tagbody
		   recheck (loop for idx fixnum upfrom 1 below boundary-array-length
				 for next-byte = (read-byte mime-stream)
				 while (eql next-byte (aref boundary-array idx))
				 finally (cond ((= idx boundary-array-length))	;success - drop through
					       ;;failing on return char - could be start of a boundary
					       ((eql next-byte (binary-char-code #\Return))
						(write-array-portion to-stream boundary-array idx)
						(go recheck))
					       ;; write out consumed bytes - return NIL
					       (t (write-array-portion to-stream boundary-array idx next-byte)
						  (return-from check-for-mime-boundry nil))))))
	       (let ((byte1 (read-byte mime-stream))
		     (byte2 (read-byte mime-stream)) )
		 (cond ((and (= byte1 (binary-char-code #\-)) (= byte2 (binary-char-code #\-)))
			(setq byte1 (read-byte mime-stream)
			      byte2 (read-byte mime-stream))
			(cond ((and (eql byte1 (binary-char-code #\Return)) (eql byte2 (binary-char-code #\Linefeed)))
			       (return-from mime-stream-decode-until-boundary t))
			      (t (write-array-portion to-stream boundary-array boundary-array-length
						      (binary-char-code #\-) (binary-char-code #\-) byte1 byte2))))
		       ((and (eql byte1 (binary-char-code #\Return)) (eql byte2 (binary-char-code #\Linefeed)))
			(return-from mime-stream-decode-until-boundary nil))
		       (t (write-array-portion to-stream boundary-array boundary-array-length byte1 byte2))))))
      (declare (inline fill-boundary-array)
	       (dynamic-extent #'protected-write-byte))
      (let* ((boundary-length (length boundary) )
	     (boundary-array-length (+ 4 boundary-length)) ;; len + CR + LF + 2 #\-'s
	     (boundary-array  (make-array boundary-array-length :initial-element #.(char-code #\-))))
	(declare (dynamic-extent boundary-array)
		 (fixnum boundary-length boundary-array-length))
	(with-binary-stream (mime-stream :input)
	  (loop initially (fill-boundary-array boundary-array boundary boundary-length)
		for byte = (read-byte mime-stream)
		do (if (eql byte (binary-char-code #\Return))
		       (check-for-mime-boundry mime-stream to-stream boundary-array boundary-array-length)
		       (protected-write-byte byte to-stream))))))))

(defmethod mime-stream-decode-until-boundary (mime-stream to-stream boundary (mode (eql :crlf)) &optional buffer)
  (mime-stream-decode-until-boundary mime-stream to-stream boundary :binary buffer))

(defun mime-stream-decode-into-file-until-boundary (mime-stream pathname boundary mode &optional buffer &aux last-block-p)
  "Captures MIME multipart data from MIME-STREAM into the file, pathname, upto the next BOUNDARY
according to MODE. MODE can be any of :TEXT, CRLF, :BINARY."
  (declare (values pathname last-block-p))
  (let ((capture-mode (ecase mode
			((:text :crlf) :text)	;local file should normally be in native charset
			(:binary :binary))))
    (with-open-file (file-stream pathname :direction :output
				 :element-type (copy-mode-element-type capture-mode) :if-exists :supersede)
      (setq last-block-p (mime-stream-decode-until-boundary mime-stream file-stream boundary capture-mode buffer)))
    #+MCL(autotype-file pathname)
    (values pathname last-block-p)))

(define-generic mime-stream-decode-into-string-until-boundary (mime-stream boundary &optional string buffer)
  (declare (values decoded-string last-block-p))
  (:documentation "Captures MIME multipart data from MIME-STREAM into STRING upto the next BOUNDARY.
If STRING is provided, data is copied into STRING with size adjustment as needed.
BUFFER is a temporary line buffer."))

(defmethod mime-stream-decode-into-string-until-boundary (mime-stream boundary &optional string buffer &aux last-block-p)
  (cond (string
	 (values (with-output-to-string (stream string)
		   (setq last-block-p (mime-stream-decode-until-boundary mime-stream stream boundary :text buffer)))
		 last-block-p))
	(t (values (with-output-to-string (stream)
		     (setq last-block-p (mime-stream-decode-until-boundary mime-stream stream boundary :text buffer)))
		   last-block-p))))

(defgeneric mime-stream-flush-until-boundary (mime-stream boundary mode &optional buffer)
  (declare (values decoded-string last-block-p))
  (:documentation "Flushes MIME multipart data from mime-stream upto the next boundary.
MODE can be any of :TEXT, CRLF, :BINARY."))

(defmethod mime-stream-flush-until-boundary (mime-stream boundary mode &optional buffer)
  (declare (values end-p))
  (with-null-stream (null-stream)
    (mime-stream-decode-until-boundary mime-stream null-stream boundary mode buffer)))


;;;------------------------------------------------------------------- 
;;;
;;; STRING UTILITIES
;;;

(define-generic write-vector (stream vector &optional start end)
  (:documentation "Writes the bytes of VECTOR to STREAM.
VECTOR may be a vector or string, but STREAM should be in the correct mode.
Ports and applications may specialize this method to optimize data transfer rates."))

(defmethod write-vector (stream (vector vector) &optional (start 0) (end (length vector)))
  (with-fast-array-references ((v vector vector))
    (loop for idx upfrom start below end
          do (write-byte (aref v idx) stream)))
  vector)

(defmethod write-vector (stream (vector string) &optional (start 0) (end (length vector)))
  (with-fast-array-references ((v vector vector))
    (loop for idx upfrom start below end
          do (write-char (aref v idx) stream)))
  vector)

(define blank-line-p (line &optional (start 0) end)
  (declare (type string line))
  (or (= start (or end (setq end (length line))))	;null string?
      (with-fast-array-references ((line line string))
	(loop for idx upfrom start below end
	      unless (member (aref line idx) '(#\space #\tab #\Linefeed #\Return) :test #'eql)
		return nil
	      finally (return t))))) 

(define string-search= (pattern string &optional (start1 0) end1 (start2 0) end2)
  "Fast search for PATTERN in STRING. Test is EQL"
  (declare (type string pattern)
           (type string string))
  (unless end1 (setq end1 (length pattern)))
  (unless end2 (setq end2 (length string)))
  (with-fast-array-references ((pattern pattern string)
                               (string string string))
    (loop with pchar = (aref pattern start1)
          with pidx1 = (1+ (the fixnum start1))
          for idx upfrom start2 below end2
          for char = (aref string idx)
          when (eql char pchar)
            do (loop for pidx upfrom pidx1 below end1
                     for idx1 upfrom (1+ (the fixnum idx)) below end2
                     unless (eql (aref pattern pidx)
                                 (aref string idx1))
                       return nil
                     finally (when (= pidx end1)
                               (return-from string-search= idx)))
          finally (return nil))))

(define-macro %string-test (pattern string &optional (offset 0) (test 'eql))
  "Fast check to determine if PATTERN is present in STRING beinning at OFFSET.
PATTERN must be a string at compile time. TEST is the character comparison to use.
This expands into fast inline code."
  (check-type pattern string)
  (check-type test symbol)
  (let ((length (length pattern)))
    `(let ((start ,offset))
       (declare (fixnum start))
       (with-fast-array-references ((string ,string string))
	 (and (< -1 (- (the fixnum (length string)) start ,length))
	      ,.(loop for char across pattern
		      for offset upfrom 0
		      for index = (if (zerop offset)
				      'start
				      `(+ ,offset start))
		      collect `(,test ,char (aref string ,index))))))))

(define string-search (pattern string &optional (start1 0) end1 (start2 0) end2)
  "Fast search for PATTERN in STRING. Test is CHAR-EQUAL"
  (declare (type string pattern)
           (type string string))
  (unless end1 (setq end1 (length pattern)))
  (unless end2 (setq end2 (length string)))
  (with-fast-array-references ((pattern pattern string)
                               (string string string))
    (loop with pchar = (aref pattern start1)
          with pidx1 = (1+ (the fixnum start1))
          for idx upfrom start2 below end2
          for char = (aref string idx)
          when (char-equal char pchar)
            do (loop for pidx upfrom pidx1 below end1
                     for idx1 upfrom (1+ (the fixnum idx)) below end2
                     unless (char-equal (aref pattern pidx)
                                        (aref string idx1))
                       return nil
                     finally (when (= pidx end1)
                               (return-from string-search idx)))
          finally (return nil))))

(defun concatenate-lines (lines &optional (trim-predicate #'white-space-char-p))
  "Concatenates line with lines trimming leading and trailing characters satisfying trim-predicate."
  (flet ((compute-concatenation-spec (lines)
           (loop for string in lines
                 for length = (length string)
                 for end = (1+ (the fixnum (or (position-if-not trim-predicate string :start 0 :end length :from-end t) -1)))
                 for start = (or (position-if-not trim-predicate string :start 0 :end end ) 0)
                 for size = (- (the fixnum end) (the fixnum start))
                 collect `(,start ,end ,size . ,string) into subseq-specs
                 unless (zerop size)
                   sum size into total-size
                   and sum 1 into n-lines
                 finally (return (values subseq-specs (if (zerop n-lines)
                                                          total-size
                                                          (+ (the fixnum (1- (the fixnum n-lines)))
                                                             (the fixnum total-size))))))))
    (declare (inline compute-concatenation-spec))
    (cond ((cdr lines)
           (multiple-value-bind (line-specs total-size)
               (compute-concatenation-spec lines)
             (declare (dynamic-extent line-specs))
             (let ((string (make-string total-size)))
               (with-fast-array-references ((string string string))
                 (loop with start2 = 0 and end2 = 0
                       for (spec . more-specs) = line-specs then more-specs
                       for (start end size . new-line) = spec
                       do (unless (zerop size)
                            (setq end2 (+ (the fixnum start2) (the fixnum size)))
                            (copy-vector-portion new-line start end string start2 end2)
                            (cond ((and more-specs (not (zerop (caddar more-specs))))
                                   (setf (aref string end2) #\space)
                                   (setq start2 (1+ (the fixnum end2))))
                                  (t (setq start2 end2))))
                       while more-specs))
               string)))
          (t (car lines)))))

(define debug-server (&optional (debug (not *debug-server*)))
  "Toggles server debugging according to DEBUG.
DEBUG can be:

      T          Turns normal debugging on
      NIL        Turns debugging off
     :CONDITIONS Turns on debugging of unhandled HTTP conditions
     :ERRORS     Turns on normal debugging."
  (setq *debug-server* (ecase debug
                         ((nil t) (not (null debug)))
                         (:errors debug)
                         (:conditions debug)))) 

;;;------------------------------------------------------------------- 
;;;
;;; VIRTUAL HOSTS
;;;

(define-variable *virtual-host-table* (make-hash-table :test #'equalp)
                 "A table of virtual hosts served.")

(define clear-virtual-hosts ()
  "Removes all virtual hosts."
  (clrhash *virtual-host-table*))

(defun map-virtual-hosts (function)
  "Maps FUNCTION over entries for virtual hosts.
FUNCTION is called with (host port local-context)"
  (flet ((fctn (host entry)
           (loop for (port . local-context) in entry
                 do (funcall function host port local-context))))
    (declare (dynamic-extent #'fctn))
    (maphash #'fctn *virtual-host-table*)))

(define standard-http-port ()
  "Returns the standard port for HTTP connections."
  *standard-http-port*)

(defun %define-host-name-mapping (domain-name port validate-host-p entry-contructor) 
  (declare (dynamic-extent entry-contructor))
  (flet ((make-entry (contructor host port)
	   (funcall contructor (string-downcase host) port)))
    (check-type domain-name string)
    (check-type port integer)
    (when validate-host-p
      (parse-host domain-name))
    (let ((entry (gethash domain-name *virtual-host-table*)))
      (cond (entry
	     (let ((spec (assoc port entry :test #'=)))
	       (if spec
		   (setf (cdr spec) (cdr (make-entry entry-contructor domain-name port)))
		   (setf (gethash domain-name *virtual-host-table*)
			 (list* (make-entry entry-contructor domain-name port) entry)))))
	    (t (setf (gethash domain-name *virtual-host-table*)
		     (list (make-entry entry-contructor domain-name port)))))
      domain-name)))

(define add-virtual-host-nick-name (domain-name port local-context)
  "Adds a nickname for a virtual host.
This allows multiple domain names to map to the same set of URLs.
LOCAL-CONTEXT is the local context to which nickname on port refers.
Use VIRTUAL-HOST-LOCAL-CONTEXT or LOCAL-CONTEXT to obtain these."
  (flet ((make-entry (host port)
           (declare (ignore host))
           `(,port . ,local-context)))
    (declare (dynamic-extent #'make-entry))
    (%define-host-name-mapping domain-name port nil #'make-entry)))

(defun add-virtual-host (domain-name &optional (port *standard-http-port*) (protocol *standard-protocol*) (validate-host-p t))
  "Adds a virtual host named, DOMAIN-NAME, on port, PORT.

  The server will then server the vanity name, DOMAIN-NAME, as if it were an
  independent host.  URLs for the virtual host can be exported with the #u
  reader macro, but you must specify the host and port explicitly using the
  extended syntax for #u.  For example,

  (add-virtual-host \"www.ai.mit.edu\" 8000)

  (export-url #u(\"/cl-http/\" :host \"www.ai.mit.edu\" :port 8000)
              :directory
              :recursive-p t
              :pathname \"http:www              ;cl-http;\")

              Note that the virtual host should have an entry in the Internet Domain Name
              System that points at ip address of the physical server."
  (flet ((make-entry (host port)
           `(,port
             . ,(nstring-downcase
                 (case port
                   (80
                    (unless (eq protocol :http)
                      (cerror "Use HTTP as the protocol" "Protocol is ~A, but port 80 is defined to be HTTP." protocol))
                    (concatenate 'string "http://" host))
                   (443 
                    (unless (eq protocol :https)
                      (cerror "Use HTTPS as the protocol" "Protocol is ~A, but port 443 is defined to be HTTPS." protocol))
                    (concatenate 'string "https://" host))
                   (t (concatenate 'string (url::%scheme-prefix-for-protocol protocol) host ":" (write-to-string port :base 10))))))))
    (%define-host-name-mapping domain-name port validate-host-p #'make-entry)))

(define remove-virtual-host (domain-name &optional port)
  "Removes the virtual host, domain-name, on port, PORT.
When port is null, the virtual host is removed from all ports."
  (check-type domain-name string)
  (check-type port (or null integer))
  (let ((entry (gethash domain-name *virtual-host-table*)))
    (cond ((null entry) nil)
          (port
           (let ((n-entry (delete port  entry :key #'car :test #'=)))
             (if n-entry
                 (setf (gethash domain-name *virtual-host-table*) n-entry)
                 (remhash domain-name *virtual-host-table*))))
          (t (remhash domain-name *virtual-host-table*)))))

(define virtual-host-p (domain-name port)
  "Returns non-null if DOMAIN-NAME is defined as a virtual host on PORT."
  (let ((entry (gethash domain-name *virtual-host-table*)))
    (and entry (loop for (host-port) in entry
		     when (eql host-port port)
		       return t
		     finally (return nil)))))

(define virtual-host-string-p (port domain-name &optional (start 0) (end (length domain-name)))
  "Returns non-null if DOMAIN-NAME is defined as a virtual host on PORT.
This variant is provided for use when DOMAIN-NAME is contained in a larger string"
  (declare (fixnum start end))
  (cond ((and (zerop start) (= end (length domain-name)))
         (virtual-host-p domain-name port))
        (t (let ((host (subseq domain-name start end)))
             (declare (dynamic-extent host))
             (virtual-host-p host port)))))

(define virtual-host-local-context (domain-name &optional (port *standard-http-port*) (protocol *standard-protocol*) (canonicalize-p t))
  "Returns the local context for the virtual host, HOST, on port, PORT."
  (labels ((try-to-canonicalize-host (domain-name port)
             (let ((host (parse-host domain-name t))
                   primary-host-name)
               (cond ;; Doesn't parse, so give up.
                     ((null host) nil)
                     ;; Another name for the physical host
                     ((equalp (setq primary-host-name (host-domain-name host)) (local-host-domain-name))
                      (let ((local-context (%local-port-context port protocol)))
                        (when local-context
                          (add-virtual-host-nick-name domain-name port local-context)
                          local-context)))
                     ;; Another name for a virtual host
                     ((equalp primary-host-name domain-name) nil)
                     (t (let ((vh-local-context (virtual-host-local-context primary-host-name port protocol nil)))
                          (cond (vh-local-context
                                 (add-virtual-host-nick-name domain-name port vh-local-context)
                                 vh-local-context)
                                (t nil))))))))
    (let ((entry (gethash domain-name *virtual-host-table*)))
      (cond (entry
             (let ((local-context (cdr (assoc port entry :test #'=))))
               (when (and local-context (eq protocol (url:%protocol-for-url-string local-context)))
                 local-context)))
            ;; Try getting the primary DNS name to see if we know that.
            ;; If so, cache the mapping for future reference.
            (canonicalize-p (try-to-canonicalize-host domain-name port))
            (t nil)))))

(defmacro with-standard-protocol ((protocol) &body body)
  "Binds the standard protocol to PROTOCOL within the scope of BODY."
  `(let ((*standard-protocol* ,protocol))
     ,@body))

(defmacro with-local-scheme ((protocol) &body body)
  "Binds the HTTP SCHEME according to PROTOCOL within the scope of BODY."
  `(with-standard-protocol (,protocol)
     (let ((*local-scheme* ,protocol))
       ,@body)))

(define-macro with-virtual-host-local-context ((url-string) &body body)
  "Binds the local context to URL within the scope of BODY.
URL-STRING must be previously coerced into correct form, or
you should use the safer WITH-LOCAL-CONTEXT."
  `(with-local-scheme ((url:%protocol-for-url-string ,url-string))
     (let ((*local-context* ,url-string))
       ,@body)))

;;;------------------------------------------------------------------- 
;;;
;;; LOAD BALANCING CLUSTERS
;;;

(defparameter *load-balancing-clusters* nil)

(defun add-host-to-load-balancing-cluster (cluster host &optional (port *standard-http-port*) (protocol *standard-protocol*))
  "Adds HOST and PORT to the load balancing cluster, CLUSTER."
  (check-type cluster (or symbol integer))
  (check-type port integer)
  (let ((context (%make-context host port protocol))
        (entry (assoc cluster *load-balancing-clusters* :test #'eql)))
    (cond (entry 
           (pushnew context (cddr entry) :test #'string-equal)
           (setf (second entry) (length (cddr entry))))
          (t (push `(,cluster ,1 ,context) *load-balancing-clusters*)))))

(defun remove-host-from-load-balancing-cluster (cluster host &optional (port *standard-http-port*) (protocol *standard-protocol*))
  "Removes HOST and PORT from the load balancing cluster, CLUSTER."
  (check-type cluster (or symbol integer))
  (check-type port integer)
  (let ((context (%make-context host port protocol))
        (entry (assoc cluster *load-balancing-clusters* :test #'eql)))
    (declare (dynamic-extent context))
    (cond (entry
           (setf (cddr entry) (delete context (cddr entry) :test #'string-equal))
           (if (cddr entry)
               (setf (second entry) (length (cddr entry)))
             (setq *load-balancing-clusters* (delete entry *load-balancing-clusters*)))
           t)
          (t (error "Unknown load balancing cluster, ~S." cluster)))))

(defun remove-load-balancing-cluster (cluster)
  "Removes CLUSTER as a load balancing cluster."
  (let ((entry (assoc cluster *load-balancing-clusters* :test #'eql)))
    (when entry
      (setq *load-balancing-clusters* (delete entry *load-balancing-clusters*)))))

(define-macro define-load-balancing-cluster (name &body host-ports)
  "Defines a load balancing clusted named NAME that is comprised by HOST-PORTS.
HOST-PORTS are pairs of (DOMAIN-NAME PORT-NUMBER)."
  `(progn (remove-load-balancing-cluster ',name)
          ,@(loop for (host port) in host-ports
                  collect `(add-host-to-load-balancing-cluster ',name ',host ,(or port *standard-http-port*)))))

(defun %load-balanced-url (cluster relative-url)
  (let ((entry (assoc cluster *load-balancing-clusters* :test #'eql)))
    (cond (entry
           (let* ((size (second entry))
                  (idx (floor (random (the fixnum (* 100 (the fixnum size)))) 100)))
             (merge-url relative-url (nth idx (cddr entry)))))
          (t (error "Unknown load balancing cluster, ~S." cluster)))))


;;;------------------------------------------------------------------- 
;;;
;;; SERVER LOCAL CONTEXTS
;;;

(defun %local-port-context (port protocol)
  "Returns the local context for PORT if it exists, or NIL when not present."
  (let ((entry (assoc port *local-port-context-alist* :test #'eql)))
    (when (and entry
               (or (null protocol) (eq protocol (url:%protocol-for-url-string (cdr entry)))))
      (cdr entry))))

;; Called when schemes change on ports
(defun %decache-local-port-context (port)
  "Removes the cached local context for PORT."
  (let ((entry (assoc port *local-port-context-alist* :test #'eql)))
    (when entry
      (setq *local-port-context-alist* (delete entry *local-port-context-alist*))
      t)))

(define local-port-context (port &optional (protocol *standard-protocol*))
  "Returns the local context for PORT.
Creates the local context if none present using PROTOCOL."
  (let ((entry (assoc port *local-port-context-alist*)))
    (cond (entry (cdr entry))
          (t (setq entry (%make-local-context port protocol))
             (push-ordered `(,port . ,entry) *local-port-context-alist* #'< :key #'car)
             entry))))

(define map-local-port-contexts (function)
  "Maps FUNCTION over the local contexts enabled for HTTP service.
Function is called with (PORT CONTEXT)."
  (loop for (port . context) in *local-port-context-alist*
        when (http-service-enabled-p port)
        do (funcall function port context)))

(define local-port-http-service ()
  "Returns a list of ports on which local HTTP servers are running."
  (mapcar #'car *local-port-context-alist*))

(defmacro with-local-port-context ((port &optional (protocol *standard-protocol*)) &body body)
  "Binds the local context according to PORT within the scope of BODY."
  `(let ((protocol ,protocol)
         (port ,port))
     (with-local-scheme (protocol)
       (let ((*standard-http-port* port)
             (*local-context* (local-port-context port protocol)))
         ,@body))))

(defmacro with-local-port-context-if ((cond port protocol) &body body)
  "When COND evaluates non-null, this conditionally binds the local context
according to PORT within the scope of BODY."
  `(cond (,cond
	  (with-local-port-context (,port ,protocol) . ,body))
	 (t . ,body)))

(declaim (inline local-context))

(define local-context ()
  "Returns the local context against which partial URLs are merged."
  *local-context*)

(declaim (inline local-scheme))

(define local-scheme ()
  "Returns the local scheme used in merging partial URLs."
  *local-scheme*)

(defun set-local-context (url)
  (let* ((context (url:local-context-string url))
         (len (length context)))
    (cond ((string-search= "http://" context 0 6 0 len)
           (psetq *local-scheme* :http
                  *local-context* context))
          ((string-search= "https://" context 0 7 0 len)
           (psetq *local-scheme* :https
                  *local-context* context))
	  (t (error "Bad Syntax: Local context must start with either http://host.domain or https://host.domain.")))))

;; local context must be set with this method to ensure a certain measure of canonicalization.
(defsetf local-context set-local-context)

(define-macro with-local-context ((url) &body body)
  "Binds the local context to URL within the scope of BODY."
  `(let (*local-scheme* *local-context*)
     (setf (local-context) ,url)
     ,@body))

;; Implement a real merge algorithm sometime really soon.
;(define merge-url (url &optional (defaults *local-context*))
;  (check-type defaults string)
;  ;; preparse for paren syntax
;  (etypecase url
;    (string)
;    (cons
;      (destructuring-bind (url-string &key host port) url
;        (let ((vh-local-context (virtual-host-local-context host (or port *standard-http-port*))))
;          ;; look for virtual hosts to reduce consing
;          (cond (vh-local-context
;                 (setq defaults vh-local-context
;                       url url-string))
;                (host
;                 (setq defaults (if (or port (setq port (url::get-port-info defaults 7 (length defaults))))
;                                    (concatenate 'string "http://" host ":" (write-to-string port :base 10.))
;                                    (concatenate 'string "http://" host))
;                       url url-string))
;                (port
;                 (setq url (concatenate 'string ":" (write-to-string port :base 10.) 
;                                        (if (eql (aref url-string 0) #\:)
;                                            (subseq url-string (or (char-position #\/ url-string 0 (length url-string))
;                                                                   (error "No directory delimiter in ~S." url-string)))
;                                            url-string))))
;                (t (setq url url-string)))))))
;  (with-fast-array-references ((string url string))
;    (let ((l (length string))
;          ch)
;      (declare (fixnum l))
;      (flet ((merge-port (string defaults)
;               (let ((pos (char-position #\: defaults 7 (length defaults) t)))
;                 (concatenate 'string (if pos (subseq defaults 0 pos) defaults) string))))
;        (declare (inline merge-port))
;        (cond ;; no url provided
;          ((zerop l) (return-from merge-url defaults))
;          ;; default the pathname 
;          ((eql (setq ch (aref string 0)) #\/)
;           (return-from merge-url (concatenate 'string defaults url)))
;          ((eql ch #\:)
;           (loop for idx upfrom 1 to (the fixnum (1- l))
;                 while (digit-char-p (aref string idx))
;                 finally (return-from merge-url
;                           (if (< 1 idx)
;                               (merge-port string defaults)
;                               (concatenate 'string defaults (subseq string 1 l))))))
;          ;;
;          ((loop for idx upfrom 0 to (the fixnum (1- l))
;                 for char = (aref string idx)
;                 do (cond ((member char '(#\/ #\?) :test #'eql)
;                           (return nil))
;                          ((and (eql char #\:)  ;found the scheme, ergo fully specified
;; Removed per PCH bug report because it prevents merge url from working for
;; other url schemes, e.g. mailto   7/4/96 -- JCMa.
;;                                (< 2 (the fixnum (- l idx)))
;;                                (eql (aref string (the fixnum (1+ idx))) #\/)
;;                                (eql (aref string (the fixnum (+ 2 idx))) #\/)
;                                )
;                           (return-from merge-url url)))
;                 finally (return nil)))
;          ;; url name
;          (t (concatenate 'string defaults "/" url)))))))


(defun %merge-url (url defaults &optional destructive-p)
  (declare (dynamic-extent defaults))
  (check-type defaults string)
  (let ((l (length url))
	(buffer (and destructive-p
		     (array-has-fill-pointer-p url)
		     (adjustable-array-p url)
		     url))
	ch)
    (declare (fixnum l))
    (flet ((merge-port (string defaults)
	     (let* ((pos (char-position #\: defaults 7 (length defaults) t))
		    (context-less-port (if pos (subseq defaults 0 pos) defaults)))
	       (declare (dynamic-extent context-less-port))
	       (string-concatenate buffer context-less-port string))))
      (declare (inline merge-port))
      (with-fast-array-references ((string url string))
	(cond ;; no url provided
	  ((zerop l) (return-from %merge-url (string-concatenate buffer defaults)))
	  ;; default the pathname 
	  ((eql (setq ch (aref string 0)) #\/)
	   (return-from %merge-url (string-concatenate buffer defaults url)))
	  ;; merge the port number
	  ((eql ch #\:)
	   (loop for idx upfrom 1 to (the fixnum (1- l))
		 while (digit-char-p (aref string idx))
		 finally (return-from %merge-url
			   (if (< 1 idx)
			       (merge-port string defaults)
			       (string-concatenate buffer defaults (subseq string 1 l))))))
	  ;; check for url scheme
	  ((loop for idx upfrom 0 to (the fixnum (1- l))
		 for char = (aref string idx)
		 do (case char
		      (#\: (return-from %merge-url url))	;found the scheme, ergo fully specified
		      ((#\/ #\?) (return nil)))
		 finally (return nil)))
	  ;; url name and defaults ends in slash
	  ((eql #\/ (aref defaults (1- (the fixnum (length defaults)))))
	   (string-concatenate buffer defaults url))
	  ;; url name and defaults does not end in slash
	  (t (string-concatenate buffer defaults "/" url)))))))

(declaim (special url::*scheme-maximum-length*))

(define merge-url (url &optional (defaults *local-context*))
  (check-type defaults string)
  (macrolet ((with-extended-syntax-bindings ((url) &body body)
               `(destructuring-bind (url-string &key host port protocol) ,url
                  (multiple-value-bind (host port protocol)
                      (cond ((not (or host port protocol))
                             (values nil nil nil))
                            (host
                             (values host 
                                     (or port *standard-http-port*) 
                                     (or protocol 
                                         (and (eq defaults *local-context*) *local-scheme*)
                                         (url::%protocol-for-scheme defaults 0 (length defaults) nil nil)
                                         *standard-protocol*)))
                            (t (let (default-len)
                                 (multiple-value-bind (h p)
                                     (when defaults
                                       (url::get-host-port-info defaults 0 (setq default-len (length defaults)) nil))
                                   (values h 
                                           (or port p *standard-http-port*) 
                                           (or protocol 
                                               (and (eq defaults *local-context*) *local-scheme*)
                                               (url::%protocol-for-scheme defaults 0 (or default-len (setq default-len (length defaults))) nil nil)
                                               *standard-protocol*))))))
                    ,@body)))
             (with-new-scheme-for-defaults ((protocol port defaults) &body body)
               `(let ((,defaults (swap-default-scheme ,protocol ,port ,defaults)))
                  (declare (dynamic-extent ,defaults))
                  ,@body)))
    (flet ((swap-default-scheme (protocol port defaults)
             (multiple-value-bind (scheme scheme-length)
                 (url::%scheme-for-protocol protocol)
               (declare (fixnum scheme-length))
               (let* ((s1 (char-position #\: defaults 0 url::*scheme-maximum-length*)))
                 ;;; KAP the declaration is wrong, s1 could also be nil
                 ;;; ecl and sbcl both complain
                 #-(or ecl sbcl) (declare (fixnum s1))
                 (cond ((%string-equal scheme defaults 0 scheme-length 0 s1)
                        defaults)
                       (port
                        (multiple-value-bind (host old-port)
                            (url::get-host-port-info defaults 0 (length defaults) nil)
                          old-port ; ignore
                          (concatenate 'string scheme "://" host ":" (write-to-string port :base 10.))))
                       (t (let* ((len (length defaults))
                                 (size (+ (- len s1) scheme-length))
                                 new-defaults)
                            (setq new-defaults (make-string size))
                            (copy-vector-portion scheme 0 scheme-length new-defaults 0 scheme-length)
                            ;; Won't work for URLs not using ://
                            (copy-vector-portion defaults s1 len new-defaults scheme-length size)
                            new-defaults)))))))
      (etypecase url
        (string
         (%merge-url url defaults nil))
        ;; handle paren syntax
        (cons
         (with-extended-syntax-bindings
          (url)
          (let (vh-local-context)
            ;; look for virtual hosts to reduce consing
            (cond ((setq vh-local-context (and host port (virtual-host-local-context host port *standard-protocol*)))
                   (if protocol
                       (with-new-scheme-for-defaults 
                        (protocol port vh-local-context)
                        (%merge-url url-string vh-local-context nil))
                     (%merge-url url-string vh-local-context nil)))
                  (host
                   (let ((new-defaults (if (or port (setq port (url::get-port-info defaults)))
                                           (concatenate 'string (url::%scheme-prefix-for-protocol protocol) host ":" (write-to-string port :base 10.))
                                         (concatenate 'string (url::%scheme-prefix-for-protocol protocol) host))))
                     (declare (dynamic-extent new-defaults))
                     (%merge-url url-string new-defaults nil)))
                  (port
                   (let ((new-url (concatenate (list 'array (array-element-type url-string) '(*))
                                               ":" (write-to-string port :base 10.) 
                                               (if (eql (aref url-string 0) #\:)
                                                   (subseq url-string (or (char-position #\/ url-string 0 (length url-string))
                                                                          (error "No directory delimiter in ~S." url-string)))
                                                 url-string))))
                     (declare (dynamic-extent new-url))
                     (%merge-url new-url defaults nil)))
                  ((and protocol defaults)
                   (with-new-scheme-for-defaults 
                    (protocol port defaults)
                    (%merge-url url-string defaults nil)))
                  (t (%merge-url url-string defaults nil))))))))))

(defun sharp-sign-u-reader-helper (stream sub-char arg)
  (declare (ignore sub-char arg))
  (let ((url (read stream t nil t)))
    (etypecase url
      (null url)
      (symbol
       `(url:intern-url (merge-url ,url (local-context))
                        :if-does-not-exist :create))
      (string
       `(url:intern-url (merge-url ',url (local-context))
                        :if-does-not-exist :create))
      (cons
       (etypecase (car url)
         (string ;; the extended syntax
                 `(let ((spec (list ,@url)))
                    (declare (dynamic-extent spec))
                    (url:intern-url (merge-url spec (local-context)) :if-does-not-exist :create)))
         (symbol ;; evaluating a form
                 `(url:intern-url (merge-url ,url (local-context)) :if-does-not-exist :create)))))))

;; Handy dandy macro character #U"foo.html". One can also say
;; #U("foo.html" :host "www.ai.mit.edu" :port 8000)
;; We should probably have our own read table for modularity reasons.
(set-dispatch-macro-character #\# #\u  'sharp-sign-u-reader-helper)

;;;------------------------------------------------------------------- 
;;;
;;; HEADER UTILITIES
;;;

(defmacro %with-header-set-index ((header-set) &body body)
  `(let* ((index-ptr (%header-set-index ,header-set))
	  (index (car index-ptr))
	  (.headers. (cdr index-ptr)))
     ,@body))

(declaim (inline %%get-header-object))

(defun %%get-header-object (keyword index headers)
  (with-fast-array-references ((index index vector)
			       (headers headers vector))
    (loop for idx fixnum upfrom 0 below (the fixnum (fill-pointer index))
	  when (eq keyword (aref index idx))
	    return (aref headers idx)
	  finally (return nil))))

(defun %get-header-object (header-set keyword)
  (when header-set
    (%with-header-set-index (header-set)
      (%%get-header-object keyword index .headers.))))

(declaim (notinline %get-header-object))

(declaim (inline get-header-object))

(define get-header-object (header-keyword &optional (headers *headers*))
  "Returns the header object for HEADER-KEYWORD in the current headers."
  (declare (values header-object))
  (%get-header-object headers header-keyword))

(declaim (inline get-header))

(define get-header (header-keyword &optional (headers *headers*))
  "Returns the parsed value for HEADER-KEYWORD in the current headers."
  (declare (values parsed-value found-p))
  (let ((hdr (%get-header-object headers header-keyword)))
    (when hdr
      (values (header-value hdr) t))))

(define get-raw-header (header-keyword &optional (headers *headers*) durable-p)
  "Returns the raw value for HEADER-KEYWORD in the current headers.
When DURABLE-P is non-null, this returns a raw value that
persists beyond the lifetime of the current header set."
  (declare (values raw-value found-p))
  (let ((hdr (%get-header-object headers header-keyword)))
    (when hdr
      (values (header-raw-value hdr durable-p) t))))

(defmacro with-header-values (headers header-set &body body)
  "Binds HEADERS to appropriate header values from HEADER-SET.

This macro is more than GET-HEADER at accessing header values when more than
one header is accessed.

HEADERS is a list of header specs, each of which can be either a variable
symbol or a list of (VARIABLE-SYMBOL NULL-BINDING-VALUE). When supplied,
NULL-BINDING-VALUE is used as the variable value when not header named
VARIABLE-SYMBOL is present."
  (flet ((header-spec (spec)
	   (etypecase spec
	     (symbol (values spec nil))
	     (cons (values-list spec))))
	 (%make-header-binding1 (header-set keyword null-binding-value)
	   `(let ((hdr (%get-header-object ,header-set ,keyword)))
	      ,(if null-binding-value
		   `(if hdr (header-value hdr) ,null-binding-value)
		   `(and hdr (header-value hdr)))))
	(%make-header-binding2 (keyword null-binding-value)
	   `(let ((hdr (%%get-header-object ,keyword index .headers.)))
	      ,(if null-binding-value
		   `(if hdr (header-value hdr) ,null-binding-value)
		   `(and hdr (header-value hdr))))))
    (cond ((cdr headers)
	   (loop for spec in headers
		 with header and null-binding-value
		 do (multiple-value-setq (header null-binding-value)
		      (header-spec spec))
		 collect `(,header ,(%make-header-binding2 (intern (symbol-name header) *keyword-package*) null-binding-value))
		   into bindings
		 finally (return `(%with-header-set-index (,header-set)
				    (let* ,bindings ,@body)))))
	  (t (multiple-value-bind (header null-binding-value)
		 (header-spec (car headers))
	       `(let ((,header ,(%make-header-binding1 header-set (intern (symbol-name header) :keyword) null-binding-value))) ,@body))))))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

;(defun parse-user-agent (string)
;  (labels ((delimiter1-char-p (char)
;             (eql char #\/))
;           (delimiter2-char-p (char)
;             (member char '(#\( #\/ #\space) :test #'eql))
;           (parse-platform (string start end)
;             (declare (fixnum start end))
;             (loop for s = (position-if-not #'(lambda (ch) (member ch '(#\; #\/ #\( #\space) :test #'eql)) string :start start :end end)
;                         then (position-if-not #'(lambda (ch) (member ch '(#\; #\/ #\( #\space) :test #'eql)) string :start (1+ e) :end end)
;                   while s
;                   for e = (or (position-if #'(lambda (ch) (member ch '(#\; #\/ #\) #\space) :test #'eql))
;                                            string :start (1+ s)  :end end)
;                               end)
;                   for key = (symbolize (subseq string s e) *keyword-package*)
;                   collect key
;                   while (< e end))))
;    (declare (inline delimiter1-char-p delimiter2-char-p parse-platform))
;    (let* ((end (the fixnum (length string)))
;           (pos1 (position-if #'delimiter1-char-p string :start 0 :end end))
;           (pos2 (and pos1 (or (position-if #'delimiter2-char-p string :start (1+ (the fixnum pos1)) :end end) end)))
;           (user-agent (nsubstitute #\- #\space (subseq string 0 pos1)))
;           (version (and pos1 (subseq string (1+ (the fixnum pos1)) pos2)))
;           (comment (and pos2 (parse-platform string pos2 end))))
;      (values (symbolize user-agent *keyword-package*)
;              (and version (symbolize version *keyword-package*))
;              comment))))

(defun parse-user-agent (string &optional (start 0) (end (length string)))
  (declare (fixnum start end)
	   (values user-agent version comment))
  (labels ((delimiter1-char-p (char)
             (eql char #\/))
           (delimiter2-char-p (char)
             (member char '(#\( #\/ #\space) :test #'eql))
	   (delimiter3-char-p (char)
             (member char '(#\; #\/ #\( #\) #\space) :test #'eql))
           (parse-platform (string start end)
             (declare (fixnum start end))
             (loop for s = (%fast-position-if-not delimiter3-char-p string :start start :end end)
                         then (%fast-position-if-not delimiter3-char-p string :start (1+ e) :end end)
                   while s
                   for e fixnum = (or (%fast-position-if delimiter3-char-p string :start (1+ (the fixnum s)) :end end)
				      end)
                   for key = (tokenize-header-keyword string s e)
                   collect key
                   while (< e end))))
    (declare (inline delimiter1-char-p delimiter2-char-p delimiter3-char-p parse-platform))
    (let* ((pos1 (%fast-position-if delimiter1-char-p string :start start :end end))
           (pos2 (and pos1 (or (%fast-position-if delimiter2-char-p string :start (1+ (the fixnum pos1)) :end end) end)))
           (user-agent (nsubstitute #\- #\space (subseq string start pos1)))	;conses due to hyphen   7/12/99 -- JCMa.
           (version (and pos1 (tokenize-header-keyword string (1+ (the fixnum pos1)) pos2)))
           (comment (and pos2 (parse-platform string pos2 end))))
      (values (tokenize-header-keyword user-agent)
              version
              comment))))

;; "Mozilla/1.12(Macintosh; I; 68K)"
(define current-user-agent (&optional ignore)
  "Returns keywords for (USER-AGENT VERSION PLATFORM-SPECS) for the user agent currently being served.
When there is more than one user agent header, this returns the first one,
which is assumed to be the final consumer."
  (declare (values user-agent version comment)
           (ignore ignore))
  (values-list (server-user-agent *server*)))

(declaim (inline current-user-object))

(define current-user-object ()
  "Returns the authenticated user object for the current HTTP transaction,
or null if no user has been authenticated.
The following operations are available on the object:

          USER-NAME
          USER-REALM
          USER-GROUPS
          USER-PERSONAL-NAME
          USER-EMAIL-ADDRESS"
  (server-user-object *server*))

(define-macro with-user-email-address ((&key (server '*server*) no-personal-name) &body body)
  "Binds EMAIL-ADDRESS and PERSONAL-NAME based on SERVER within BODY."
  (let ((addr (intern "EMAIL-ADDRESS" *package*))
        (pname (intern "PERSONAL-NAME" *package*)))
    `(let ((.email-address. nil)
           ,@(unless no-personal-name '((.personal-name. nil)
                                        (.no-name. nil))))
       (flet ((.email-address. ()
                (or .email-address.
                    (setq .email-address. (user-email-address ,server))))
              ,@(unless no-personal-name
                  `((.personal-name. ()
                                     (cond (.personal-name.)
                                           (.no-name. nil)
                                           ((setq .personal-name. (user-personal-name ,server)))
                                           (t (setq .no-name. t) nil))))))
         (declare (inline .email-address.))
         (symbol-macrolet ((,addr (.email-address.))
                           ,@(unless no-personal-name
                               `((,pname (.personal-name.)))))
           ,@body)))))

(define make-http-search-suffix (search-terms)
  "Returns an appropriately escaped search suffix."
  (flet ((write-term (term stream)
           (with-input-from-string
             (in (etypecase term
                   (string term)
                   (symbol 
                     ;;downcase becase MAC Mosaic loses on Uppercase URLs
                     (string-downcase (symbol-name term)))))
             (escaping-special-chars in stream))))
    (declare (inline write-term))
    (with-output-to-string (string)
      (loop for terms = search-terms then (cdr terms)
            while terms
            do (write-term (car terms) string)
               (when (cadr terms)
                 (write-char #\+ string))))))

;;;------------------------------------------------------------------- 
;;;
;;; ESCAPE AND UNESCAPE SPECIAL CHARACTERS
;;;

;(parse-integer string :radix 16)
;(format t "~2,'0X" 65)

;(define escaped-character-p (char)
;  "Returns non-null if CHAR is a special character that must be escaped.
;CHAR is compared with CHAR=."
;  (loop for ch in *escaped-characters*
;        when (char= ch char)
;          return t
;        finally (return nil)))
;
;(define escaped-character-p (char)
;  "Returns non-null if CHAR is a special character that must be escaped.
;CHAR is compared with CHAR=."
;  (mask-bit-p (char-code char) #.(bitmask (mapcar #'char-code *escaped-characters*))))

(eval-when (:load-toplevel :execute :compile-toplevel)
;; the #. is here to work around a bug in the LispWorks compiler.   8/31/95 -- JCMa.
  (defconstant *escape-characters-bits*
               #.(let ((v (make-array (1+ (loop for ch in *escaped-characters*
                                                    maximizing (char-code ch)))
                                      :element-type 'bit
                                      :initial-element 0)))
                   (loop for ch in *escaped-characters*
                         do (setf (sbit v (char-code ch)) 1))
                   v))
  )

(define escaped-character-p (char)
  "Returns non-null if CHAR is a special character that must be escaped.
CHAR is compared with CHAR=."
  (let ((code (char-code char)))
    (and (< code #.(length *escape-characters-bits*))
         (eql 1 (sbit (the simple-bit-vector *escape-characters-bits*) code)))))

(declaim (inline escape-character-p))

(define escape-character-p (char)
  "Returns non-null if CHAR is a special character used for escaping.
CHAR is compared with CHAR=."
  (char= *escape-character* char))

(declaim (inline fragment-character-p))

(define fragment-character-p (char)
  "Returns non-null if CHAR is a special character used to delimite URL fragment syntax."
  (eql *fragment-delimiter-character* char))

(declaim (inline uri-reserved-or-unsafe-char-p))

(define uri-reserved-or-unsafe-char-p (char)
  "Returns non-null when char is unsafe or reserved within URL schemes."
  (member char '#.(append *uri-reserved-characters* *uri-unsafe-characters*) :test #'eql))

(define digit-chars (integer &optional (radix 10))
  "Returns a list of characters required to print the INTEGER.
No minus sign is provided for negative integers."
  (declare (integer integer)
           (fixnum radix)
           (optimize (speed 3)))
  (flet ((get-digit-char (digit radix)
           (declare (integer digit)
                    (fixnum radix))
           (digit-char (abs digit) radix)))
    (declare (inline get-digit-char))
    (loop with digits = integer
          and last-digit and chars
          do (multiple-value-setq (digits last-digit)
                 (truncate (the integer digits) radix))
          do (push (get-digit-char last-digit radix) chars)
          until (zerop digits)
          finally (return-from digit-chars chars))))

(define escape-character (char &optional (result-type 'cons))
  "Returns a sequence containing the escaped version of CHAR.
The sequence's type is controlled by RESULT-TYPE, which can be
either a CONS or a STRING."
  (declare (values result-type-sequence))
  (flet ((chars (char)
           (let ((code (char-code char)))
             (if (< (the fixnum code) 16)
                 `(,*escape-character* #\0 ,.(digit-chars code 16))
                 `(,*escape-character* ,.(digit-chars code 16))))))
    (declare (inline chars))
    (ecase result-type
      (cons (chars char))
      (string
        (let ((chars (chars char)))
          (declare (dynamic-extent chars))
          (coerce chars 'string))))))

(define-macro with-bad-escaping-resignalled ((string &key reason start end (error-class 'error)) &body body)
  `(handler-case-if (not *debug-server*)
      (progn ,@body)
     (,error-class (err)
      (error 'bad-escaping
	     :url ,(if (and start end)
		       `(subseq ,string (max ,start 0) (min ,end (length ,string)))
		       `(progn ,string))
	     ,@(when reason `(:reason ,reason))
	     :format-string (report-string err)))))

(define unescape-character (string &optional (start 0) (end (+ start 2)))
  "Returns the character whose escaped represenation appears in STRING from START to END."
  (declare (fixnum start end))
  (cond ((eql (- end start) 2)
	 (with-bad-escaping-resignalled (string :start start :end end :error-class error)
           (code-char (parse-integer string :radix 16 :start start :end end))))
        (t (error 'bad-escaping :format-string "START, ~D, and END, ~D, do not specify two characters in STRING."
                  :format-args (list start end string)))))

;; time for clients to provide the correct characters.   8/31/95 -- JCMa.
;(define read-translated-char (stream delimiter &optional (eof-errorp t) eof-value)
;  "Translates characters read from stream,
;returning NIL when the character in DELIMITER is encounted."
;  (declare (values char chars-read))
;  (flet ((decode-escaped-char (stream)
;          (let ((string (make-string 2)))
;            (declare (dynamic-extent string))
;            (www-utils:with-fast-array-references
;              ((string string))
;              (setf (aref string 0) (read-char stream)
;                    (aref string 1) (read-char stream))
;              (let ((char (code-char (parse-integer string :radix 16 :start 0 :end 2))))
;                (cond ((char-equal char (code-char #x8a)) #\Return)       ;handle weird char UNIX MOSAIC substitutes for 
;                      ((char-equal char (code-char #x8d)) #\Return)       ;macweb lossage for CR in forms.
;                      ((char-equal char (code-char #x89)) #\tab)     ;Macweb sends tabs as (code-char #x89)  10/2/94.
;                      (t char)))))))
;    (declare (inline decode-escaped-char))
;    (let ((char (read-char stream eof-errorp eof-value)))
;      (cond ((char-equal char delimiter)        ;delimiter return nil
;            (values nil 1))
;           ((char-equal char #\+)
;            (values #\space 1))
;           ((char-equal char (code-char #x8d))            ;Macweb loses with (code-char #x8d)  10/2/94.
;            (values #\Return 1))
;           ((char-equal char *escape-character*)       ;escape character
;            (values (decode-escaped-char stream) 3))
;           (t (values char 1))))))

(define-generic read-translated-char (stream delimiter &optional eof-errorp eof-value)
  (declare (values char number-of-chars-read))
  (:documentation "Translates characters read from stream,
returning NIL when the character in DELIMITER is encounted.
This automatically undoes URL escaping.
Specialize this method for higher performance on specific platforms."))

(defmethod read-translated-char (stream delimiter &optional (eof-errorp t) eof-value)
  (flet ((decode-escaped-char (stream)
           (let ((string (make-string 2)))
             (declare (dynamic-extent string))
             (www-utils:with-fast-array-references ((string string string))
               (setf (aref string 0) (read-char stream)
                     (aref string 1) (read-char stream))
               (let ((code (parse-integer string :radix 16 :start 0 :end 2)))
                 (case code
                   (13 #\newline)
                   (10 #\linefeed)
                   (t (code-char code))))))))
    (declare (inline decode-escaped-char))
    (let ((char (read-char stream eof-errorp eof-value)))
      (cond ((characterp char)
             (cond ((eql char delimiter)        ;delimiter return nil
                    (values nil 1))
                   ((eql char #\+)
                    (values #\space 1))
                   ((eql char *escape-character*)       ;escape character
                    (handler-case-if eof-errorp
                       (values (decode-escaped-char stream) 3)
                      (end-of-file () (values eof-value 1))))
                   (t (values char 1))))
            ((equal char eof-value)
             (values char 0))
            (t (error "Bad value, ~S, returned by READ-CHAR." char))))))

;; superseded by write-escaped-char   7/29/99 -- JCMa.
;(defun %write-escaped-char (char stream)
;  (let* ((code (char-code char))
;         (digit-chars (digit-chars code 16)))
;    (declare (dynamic-extent digit-chars))
;    (write-char *escape-character* stream)
;    (when (< (the fixnum code) 16)
;      (write-char *escape-character* stream))
;    (dolist (ch digit-chars)
;      (write-char ch stream))))

(define write-escaped-char (char &optional (stream *standard-output*))
  "Writes the escape code for char on STREAM."
  (let ((code (char-code char)))
    (declare (fixnum code))
    (write-char *escape-character* stream)
    (when (< code 16)
      (write-char #\0 stream))
    (print-integer code stream 16)))

(defun write-escaped-string (string escaped-chars &optional (stream *standard-output*) (start 0) (end (length string)))
  "Writes STRING to STREAM escaping ESCAPED-CHARS."
  (with-fast-array-references ((string string string))
    (loop with s = start
	  for idx fixnum upfrom 0 below end
	  for char = (char string idx)
	  when (member char escaped-chars)
	    do (unless (= s (1- idx))
		 (write-string string stream :start s :end idx)
		 (setq s (1+ idx)))
	       (write-escaped-char char stream)
	  finally (unless (= s (1- idx))
		    (write-string string stream :start s :end end)))))

(define escaping-special-chars (in-stream out-stream)
  (loop for char = (read-char in-stream nil)
        while char
        when (escaped-character-p char)
          do (write-escaped-char char out-stream)
        else do (write-char char out-stream)))  ;return char

(define unescaping-special-chars (in-stream out-stream &optional only-safe-characters-p)
  (flet ((escape-char (char out-stream)
           (let ((digit-chars (escape-character char 'cons)))
             (declare (dynamic-extent digit-chars))
             (dolist (ch digit-chars)
               (write-char ch out-stream))))
         (unescaped-char (in-stream)
           (let ((string (make-string 2)))
             (declare (dynamic-extent string))
             (setf (aref string 0) (read-char in-stream)
                   (aref string 1) (read-char in-stream))
             ;; Handle improperly formed hex encoded special charactes by
             ;; just leaving them in place. This was problem because html generator wasn't 
             ;; escaping search components of search URLs-- JCMa 5/14/1995.
             (handler-case-if  
                 (not *debug-server*)
                (let ((char-code (parse-integer string :radix 16 :start 0 :end 2)))
                  (code-char char-code))
               (error ()
                      (write-char *escape-character* out-stream)
                      (write-char (aref string 0) out-stream)
                      (aref string 1))))))      ; return the last char
    (declare (inline unescaped-char escape-char))
    (loop for char = (read-char in-stream nil)
          while char
          when (escape-character-p char)
            do (let ((nchar (unescaped-char in-stream)))
                 ;clisp did not like the following character
                 #+ignore                       ;removed kludge for mac-mosaic   8/31/95 -- JCMa.
                 (unless (char= nchar (code-char 141))  ;lose random termination char from MAC Mosaic
                   (write-char nchar out-stream))
                 (if (and only-safe-characters-p (uri-reserved-or-unsafe-char-p nchar))
                     (escape-char nchar out-stream)
                     (write-char nchar out-stream)))
          else do (write-char char out-stream))))

(define string-escape-special-chars (string &optional (start 0) (end (length string) end-supplied-p) (escape-fragments-p t))
  "When any special characters are present, this returns a string with these characters escaped.
A new string is consed only when escape characters are present.
ESCAPE-FRAGMENTS-P controls whether URL fragment syntax is also escaped."
  (declare (values escaped-string chars-escaped-p)
           (fixnum start end))
  (flet ((count-escape-chars (string start end)
           (with-fast-array-references ((string string string))
             (loop for idx upfrom start below end
                   count (escaped-character-p (aref string idx)))))
         (count-escape-chars-upto-fragment (string start end)
           (with-fast-array-references ((string string string))
             (loop for idx upfrom start below end
                   for char = (aref string idx)
                   when (fragment-character-p char)
                     do (return (values count idx))
                   count (escaped-character-p char) into count
                   finally (return count))))
         (make-escaped-string (string start end count length)
           (let* ((len2 (+ (the fixnum length) (the fixnum (* 2 (the fixnum count)))))
                  (nstring (make-string len2)))
             (with-fast-array-references ((string string string)
                                          (nstring nstring string))
               (loop with nidx = 0
                     for idx upfrom start below end
                     for char = (aref string idx)
                     when (escaped-character-p char)
                       do (let ((digit-chars (escape-character char 'cons)))
                            (declare (dynamic-extent digit-chars))
                            (dolist (ch digit-chars)
                              (setf (aref nstring nidx) ch)
                              (incf nidx)))
                     else do (setf (aref nstring nidx) char)
                             (incf nidx)
                     finally (return (values nstring nidx)))))))
    (declare (inline count-escape-chars count-escape-chars-upto-fragment make-escaped-string))
    (multiple-value-bind (count fragment-pos)
        (if escape-fragments-p
            (count-escape-chars string start end)
            (count-escape-chars-upto-fragment string start end))
      (cond ((zerop count)
             (values 
               (if (and (zerop start) (or (not end-supplied-p) (= end (length string))))
                   string
                   (subseq string start end))
               nil))
            (fragment-pos
             (multiple-value-bind (nstring nidx)
                 (make-escaped-string string start fragment-pos count (- end start))
               (with-fast-array-references ((string string string)
                                            (nstring nstring string))
                 (loop for idx1 upfrom fragment-pos below end
                       for idx2 upfrom nidx
                       do (setf (aref nstring idx2) (aref string idx1))))
               (values nstring t)))
            (t (values (make-escaped-string string start end count (- end start)) t)))))) 

;(define string-unescape-special-chars (string &optional (start 0) (end (length string) end-supplied-p))
;   "When any escaped characters are present, this returns a string with these characters unescaped.
;A new string is consed only when escaped characters are present."
;   (declare (values unescaped-string chars-unescaped-p)
;                 (fixnum start end))
;   (flet ((count-unescape-characters (string start end)
;               (with-fast-array-references ((string string string))
;                   (loop with c = 0
;                            for idx upfrom start below end
;                            when (escape-character-p (aref string idx))
;                            do (incf c)
;                            finally (return c)))))
;      (declare (inline count-unescape-characters))
;      (let ((count (count-unescape-characters string start end)))
;         (cond ((zerop count)
;                    (if (and (zerop start) (or (not end-supplied-p) (= end (length string))))
;                       string
;                       (subseq string start end)))
;                  (t (let* ((len1 (- end start))
;                                (len2 (- (the fixnum len1) (the fixnum (* 2 (the fixnum count)))))
;                                (nstring (make-array len2 :element-type (array-element-type string) :fill-pointer 0)))
;                        (with-fast-array-references ((string string string)
;                                                                       (nstring nstring string))
;                            (loop with idx = start
;                                     for nidx upfrom 0 below len2
;                                     for char = (aref string idx)
;                                     do (cond ((escape-character-p char)
;                                                    (let* ((skip (+ 3 (the fixnum idx)))
;                                                              (hex (subseq string (1+ (the fixnum idx)) skip)))
;                                                       (declare (dynamic-extent hex))
;                                                       ;; handles case of bogus escape characters appearing in URLs....
;                                                       (handler-case
;                                                          (progn (setf (aref nstring nidx) (unescape-character hex 0 2))
;                                                                     (setq idx skip))
;                                                          (error ()
;                                                                     (setf (aref nstring nidx) char)
;                                                                     (incf (the fixnum idx))))))
;                                                   (t (setf (aref nstring nidx) char)
;                                                       (incf (the fixnum idx))))
;                                     while (< (the fixnum idx) (the fixnum end))
;                                     ;; erroneous escape characters can produce a shortfall in length of nstring
;                                     finally (setf (fill-pointer nstring) (1+ nidx))
;                                     (return-from string-unescape-special-chars (values nstring t)))))))))) 

(define string-unescape-special-chars (string &optional (start 0) (end (length string) end-supplied-p)
                                              (only-safe-characters-p nil) &aux new-string)
  "When any escaped characters are present, this returns a string with these characters unescaped.
A new string is consed only when escaped characters are present.
ONLY-SAFE-CHARACTERS-P set to non-nill skips reserved or unsafe URL characters."
  (declare (values unescaped-string chars-unescaped-p new-string-returned-p)
           (fixnum start end))
  (with-fast-array-references ((string string string)
                               ;; Can't declare unless an array is provided on LispM.
                               #-Genera (new-string new-string (or null string)))
    (loop with idx fixnum = start
          and last-idx fixnum = start
          and new-idx fixnum = start
          and new-char
          while (< idx end)
          for char = (aref string idx)
          when (escape-character-p char)
            do (setf new-char (unescape-character string (1+ idx) (+ idx 3)))
               (cond  ;; Skip unescaping a char, just incf idx to skip the hex pair.
                 ((and only-safe-characters-p (uri-reserved-or-unsafe-char-p new-char))
                  (incf idx 3))
                 ;; Escape a char, we have already started a new string.
                 (new-string 
                  (let ((new-idx2 (+ new-idx (- idx last-idx))))
                    (setf new-string (replace new-string string :start1 new-idx :end1 new-idx2 :start2 last-idx :end2 idx)
                          (aref new-string new-idx2) new-char
                          new-idx (1+ (the fixnum new-idx2))
                          last-idx (incf idx 3))))
                 ;; Escape a char, need to start a new string.
                 (t (setf new-idx (- idx start)
                          new-string (replace (make-array (- end start 2) :fill-pointer t :element-type *standard-character-type*)
                                              string :start1 0 :end1 new-idx :start2 start :end2 idx)
                          (aref new-string new-idx) new-char
                          last-idx (incf idx 3))
                    (incf new-idx)))
          else
            do (incf idx)
          finally (return (cond ;; We've started a new string, now finish up
                            (new-string 
                             (let ((new-end (+ (the fixnum new-idx) (- end last-idx))))
                               (setf new-string (replace new-string string :start1 new-idx :end1 new-end :start2 last-idx :end2 end)
                                     (fill-pointer new-string) new-end))
                             (values new-string t t))
                            ;; No escaping was performed
                            ((and (zerop start) (or (not end-supplied-p) (= end (length string))))
                             (values string nil nil))
                            ;; Trim original as necessary
                            (t (values (subseq string start end) nil t)))))))

(define nstring-unescape-special-chars (string &optional (start 0) (end (length string))
					       only-safe-characters-p (pad-char #\space) set-fill-pointer-p)
  "When any escaped characters are present, this returns a string with these characters unescaped.
STRING is destructively modified when escaped characters are present.
ONLY-SAFE-CHARACTERS-P set to non-nill skips reserved or unsafe URL characters.
When SET-FILL-POINTER-P is non-null, the fill-pointer on STRING is moved backwards
by two times the number of characters unescaped. Otherwise, that number of characters
are padded on the right using PAD-CHAR"
  (declare (values unescaped-string new-end chars-unescaped-p)
           (fixnum start end))
  (with-fast-array-references ((string string string))
    (loop with read-idx fixnum = start
	  with write-idx fixnum = start
	  and new-char and chars-unescaped-p 
          while (< read-idx end)
          for char = (aref string read-idx)
	  do (incf read-idx)
          when (escape-character-p char)
            do (setf new-char (unescape-character string read-idx (+ read-idx 2)))
               (cond  ;; Skip unescaping a char, just incf idx to skip the hex pair.
                 ((and only-safe-characters-p (uri-reserved-or-unsafe-char-p new-char))
		  (cond (chars-unescaped-p
			 (setf (aref string write-idx) char
			       (aref string (incf write-idx)) (aref string read-idx)
			       (aref string (incf write-idx)) (aref string (incf read-idx)))
			 (incf write-idx)
			 (incf read-idx))
			(t (incf write-idx 3)
			   (incf read-idx 2))))
                 ;; Escape a char, we have already started a new string.
                 ((or chars-unescaped-p (setq chars-unescaped-p t))
		  (setf (aref string write-idx) new-char)
		  (incf write-idx)
		  (incf read-idx 2)))
	  else do (when chars-unescaped-p
		    (setf (aref string write-idx) char))
		  (incf write-idx)
	  finally (return (cond (chars-unescaped-p
				 (if set-fill-pointer-p
				     (setf (fill-pointer string) write-idx)
				     (loop for idx upfrom write-idx below end
					   do (setf (aref string idx) pad-char)))	;pad out the end
				 (values string write-idx chars-unescaped-p))
				(t (values string read-idx)))))))

(define nstring-translate-chars (string &optional (start 0) (end (length string)) (pad-char #\space) set-fill-pointer-p)
  "Destructively translates characters in STRING according to HTTP POST FORM rules.
All escaped characters are translated. CRLF is converted to CR and + is converted to space.
When SET-FILL-POINTER-P is non-null, the fill-pointer on STRING is moved backwards
by two times the number of characters unescaped. Otherwise, that number of characters
are padded on the right using PAD-CHAR"
  (declare (values unescaped-string new-end chars-unescaped-p)
           (fixnum start end))
  (macrolet ((insert-char (char vector index)
	       `(case ,char
		  ,@(unless (eql #\newline #\Return)
		      `((#\Return
			 (setf (aref ,vector ,index) #\newline)
			 (incf ,index))))
		  (#\LineFeed)			;drop LF
		  (t (setf (aref ,vector ,index) ,char)
		     (incf ,index)))))
    (flet ((decode-escaped-char (string start end)
	     (handler-case
	       (let ((code (parse-integer string :radix 16 :start start :end end)))
		 (case code
		   (13 #\Return)
		   (10 #\LineFeed)
		   (t (code-char code))))
	       (error () nil))))
      (declare (inline decode-escaped-char))
      (with-fast-array-references ((string string string))
	(loop with read-idx fixnum = start
	      with write-idx fixnum = start
	      and new-char and chars-shifted-p 
	      while (< read-idx end)
	      for char = (aref string read-idx)
	      do (incf read-idx)
		 (case char
		   ;;plus translated to space
		   (#\+
		    (setf (aref string write-idx) #\space)
		    (incf write-idx))
		   ;; LineFeed is ignored
		   (#\LineFeed 
		    (setq chars-shifted-p t))
		   ;; escaped characters are translated
		   (#.*escape-character*
		    (setq chars-shifted-p t)
		    (let ((escp-end (+ read-idx 2)))
		      ;; only translate when within bounds and hex code is good.
		      (cond ((and (<= escp-end end)
				  (setq new-char (decode-escaped-char string read-idx escp-end)))
			     (setq read-idx escp-end)
			     (insert-char new-char string write-idx))
			    (chars-shifted-p
			     (setf (aref string write-idx) char)
			     (incf write-idx))
			    (t (incf write-idx)))))
		   (t (when chars-shifted-p
			(setf (aref string write-idx) char))
		      (incf write-idx)))
	      finally (return (cond (chars-shifted-p
				     (if set-fill-pointer-p
					 (setf (fill-pointer string) write-idx)
					 (loop for idx upfrom write-idx below end
					       do (setf (aref string idx) pad-char)))	;pad out the end
				     (values string write-idx chars-shifted-p))
				    (t (values string read-idx)))))))))

;;(defun write-string-escaping-special-chars (string &optional (stream *standard-output*) (start 0) (end (length string)))
;;  "Writes STRING to STREAM escaping the standardly escaped characters."
;; (write-escaped-string string *escaped-characters* stream start end))

(define write-string-escaping-special-chars (string &optional (stream *standard-output*) (start 0)
                                                    (end (length string)) (escape-fragments-p t))
  "Writes STRING to STREAM being careful to escape any special characters for HTTP."
  (flet ((write-part (string stream start end)
           (unless (= start end)
             (write-string string stream :start start :end  end))))
    (declare (inline write-part))
    (with-fast-array-references ((string string string))
      (loop with scan-idx
            for idx upfrom start below end
            for char = (aref string idx)
            until (and escape-fragments-p (fragment-character-p char))
            when (escaped-character-p char)
              do (write-part string stream (or scan-idx start) idx)
                 (write-escaped-char char stream)
                 (setq scan-idx (1+ (the fixnum idx)))
            finally (if scan-idx
                        (write-part string stream scan-idx end)
                        (write-string string stream :start start :end end))))))

#|
(let ((str "http://www.foo.%6eet/%7ecvince/cgi%3fbar%3da%2bb%26baz%3dc%2bd"))
  (values str
          (string-unescape-special-chars str 0 (length str) nil)
          (string-unescape-special-chars str 0 (length str) t)))
|#

;;;-------------------------------------------------------------------
;;;
;;; 
;;;

;;; *print-pretty* is NIL in all server transactions
(define write-to-armor-plated-string (form &optional (line-length *armor-plated-string-line-length*))
  "Writes FORM to an armor plated string that travels through HTML and LISP in tact.
LINE-LENGTH controls the number of characters per line of encoded output.
In cases where RETURN causes truncation, this number should be high enough so
no line breaks occur."
  (let ((string (write-to-string form :escape t :base 10.)))
    (declare (dynamic-extent string))
    (base64:with-encoding-vector (#\$ #\! #\*)
      (base64:base64-encode-vector string :max-line-length line-length))))

;; *read-eval* is NIL in all server transactions.
(define read-from-armor-plated-string (string &optional (eof-error-p t) eof (start 0) (end (length string)))
  "Read from an armor plated string that travels through HTML in tact.
When EOF-ERROR-P is null, base 64 encoding errors and read errors are caught and the value of EOF returned."
  (declare (fixnum end))
  (handler-case-if (null eof-error-p)
     (let* ((pad-char (if (char= (schar string (1- end)) #\@) #\@ #\*))	;backward compatible with old #@ pad char 4/12/2001 -- JCMa.
	    (decoded-string (base64:with-encoding-vector (#\$ #\! pad-char)
			      (base64:base64-decode-vector string :start start :end end
							   :decoded-byte-type *standard-character-type*
							   :bogus-character-action :error))))
       (declare (dynamic-extent decoded-string))
       (read-from-string decoded-string eof-error-p eof))
    (base64:base64-decode-error () eof)))

;;;------------------------------------------------------------------- 
;;;
;;; SENDING STATUS LINES
;;;

;; this one works pratically all the time.
(declaim (inline send-line-feed))

(define send-line-feed (stream)
  (write-char #\Linefeed stream))

(declaim (inline send-cr-line-feed))

(define send-cr-line-feed (stream)
  (write-char #\Return stream)
  ;; Genera, MCL, implementation do CR->CRLF
  ;; translation on write to ASCI translating HTTP stream.
  #-(or Genera MCL)(write-char #\Linefeed stream))

(defun compute-transmitted-bytes (string &optional (start 0) (end (length string)))
  "Computes the number of bytes that would be transmitted by STRING over HTTP."
  #- (or Genera MCL)
  (declare (ignore start))
  #-(or Genera MCL)
  end
  #+(or Genera MCL)
  (+ end (count #\return string :start start :end end)))

(declaim (inline send-blank-line))

(define send-blank-line (stream)
  (send-cr-line-feed stream)
  (send-cr-line-feed stream))

(declaim (inline %send-status-line))

(define %send-status-line (stream status-code reason &optional (version *http-version*))
  (check-type status-code integer)
  (write-string version stream)
  (write-char #\space stream)
  (write-positive-fixnum status-code 10 stream)
  (write-char #\space stream)
  (etypecase reason
    (string (write-string reason stream))
    (function (funcall reason stream))
    (condition (report-status reason stream))
    (null))
  (send-cr-line-feed stream))

#+ignore
;; Matches the client http version. used in fall 1996 per Henrik's suggestion.
(define send-status-line (stream status-code reason)
  "Standard primitive for sending a status line over STREAM."
  (let* ((server *server*)
         (client-version (server-http-version-string server)))
    (%send-status-line stream status-code reason (or client-version *http-version*))
    (set-server-status server status-code)))

;; HTTP WG concluded that the server version should be sent. See HTTP Version
;; RFC Draft.   2/11/97 -- JCMa.
(define send-status-line (stream status-code reason)
  "Standard primitive for sending a status line over STREAM."
  (%send-status-line stream status-code reason *http-version*)
  (set-server-status *server* status-code))

;;;------------------------------------------------------------------- 
;;;
;;; SENDING STATUS LINES
;;;

(define-macro report-status-continue (stream)
  `(send-status-line ,stream 100 "Continue"))

(define-macro report-status-switching-protocols (stream)
  `(send-status-line ,stream 101 "Switching Protocols"))

(define-macro report-status-success (stream)
  `(send-status-line ,stream 200 "OK"))

;; Used by the connect method
(define-macro report-connection-established (stream)
  `(send-status-line ,stream 200 "Connection Established"))

(define-macro report-status-created (stream)
  `(send-status-line ,stream 201 "Created"))

(define-macro report-status-accepted (stream)
  `(send-status-line ,stream 202 "Accepted"))

(define-macro report-status-non-authoritative-information (stream)
  `(send-status-line ,stream 203 "Non-Authoritative Information"))

(define-macro report-status-no-content (stream)
  `(send-status-line ,stream 204 "No Content"))

(define-macro report-status-reset-content (stream)
  `(send-status-line ,stream 205 "Reset Content"))

(define-macro report-status-partial-content (stream)
  `(send-status-line ,stream 206 "Partial Content"))
 
(define-macro report-status-not-modified (stream)
  `(send-status-line ,stream 304 "Not Modified"))

;;;------------------------------------------------------------------- 
;;;
;;; TIME FUNCTIONS
;;;

(define write-24-hour-time (hours mins secs &optional (stream *standard-output*))
  (declare (fixnum hours mins secs))
  (when (< hours 10) (write-char #\0 stream))
  (write-positive-fixnum hours 10 stream)
  (write-char #\: stream)
  (when (< mins 10) (write-char #\0 stream))
  (write-positive-fixnum mins 10 stream)
  (write-char #\: stream)
  (when (< secs 10) (write-char #\0 stream))
  (write-positive-fixnum secs 10 stream))

(define write-time (&optional (universal-time (get-universal-time)) (stream *standard-output*)
                              (time-zone (www-utils:time-zone)) (date-separator #\space))
  "Print the full date, assuming TIME-ZONE as the timeszone."
  (multiple-value-bind (seconds minutes hours day month year weekday)
      (decode-universal-time universal-time time-zone)
    (with-string-for-null-stream (stream)
      (write-string (www-utils:day-of-the-week-string weekday :short) stream)
      (cond ((< (the fixnum day) 10.)
             (write-string ", 0" stream :end 3))
            (t (write-string ", " stream :end 2)))
      (write-positive-fixnum day 10. stream)
      (write-char date-separator stream)
      (write-string (month-string month :short) stream)
      (write-char date-separator stream)
      (write-positive-fixnum year 10. stream)
      (write-char #\space stream)
      (write-24-hour-time hours minutes seconds stream))))

(declaim (inline print-gmt-time))

(define print-gmt-time (&optional (stream *standard-output*) (universal-time (get-universal-time)) (date-separator #\space))
  "Print the full date, assuming GMT as the timeszone."
  (write-time universal-time stream 0 date-separator)
  (write-char #\space stream)
  (write-string "GMT" stream)
  universal-time)

(define write-standard-time (&optional (universal-time (get-universal-time)) (stream *standard-output*) gmt-offset-p time-zone)
  "Writes the ISO date followed by the time on STREAM."
  (declare (optimize (speed 3)))
  (flet ((gmt-offset (timezone &optional daylight-savings (stream *standard-output*))
           (declare (fixnum timezone))
           (write-char #\space stream)
           (when daylight-savings (setq timezone (the fixnum (1- timezone))))
           (multiple-value-bind (hours sec)
               (truncate (* timezone 3600) 3600)
             (declare (fixnum hours sec))
             (if (minusp hours)
                 (write-char #\+ stream)
               (write-char #\- stream))
             (when (< (abs hours) 10) (write-char #\0 stream))
             (write-positive-fixnum (abs hours) 10 stream)
             (let ((mins (abs (truncate sec 60))))
               (declare (fixnum mins))
               (when (< mins 10) (write-char #\0 stream))
               (write-positive-fixnum mins 10 stream)))))
    (declare (inline gmt-offset))
    (multiple-value-bind (secs mins hours day month year weekday daylight-savings timezone)
        (if time-zone
            (decode-universal-time universal-time time-zone)
          (decode-universal-time universal-time))
      (declare (fixnum secs mins hours day month year))
      weekday                                   ;ignore
      (with-string-for-null-stream (stream)
        (write-positive-fixnum year 10. stream)
        (write-char #\- stream)
        (when (< month 10) (write-char #\0 stream))
        (write-positive-fixnum month 10. stream)
        (write-char #\- stream)
        (when (< day 10) (write-char #\0 stream))
        (write-positive-fixnum day 10. stream)
        (write-char #\space stream)
        (write-24-hour-time hours mins secs stream)
        ;; not sure if this is really the right GMT offset computational  6/28/94 -- JCMa.
        (and gmt-offset-p (gmt-offset timezone daylight-savings stream))))))

(declaim (inline write-iso-date))

(define write-iso-date (year month day &optional stream)
  "Writes an ISO 9601 date on STREAM.
When STREAM is NIL, it return it as a string."
  (format stream "~4D-~2,'0D-~2,'0D" year month day))

(declaim (inline write-iso-time))

(define write-iso-time (hour minutes &optional seconds (time-zone (time-zone)) stream)
  "Writes an ISO 8601 time on STREAM.
When STREAM is NIL, it return it as a string."
  (declare (fixnum hour minutes)
           (rational time-zone))
  (multiple-value-bind (th ts) (truncate time-zone)
    (declare (fixnum th ts))
    (setf ts (truncate (* (abs ts) 60)))
    (format stream "~2,'0D:~2,'0D~:[~;~:*:~2,'0D~]~:[~C~2,'0D:~2,'0D~;Z~]"
            hour minutes seconds (zerop time-zone) (if (plusp th) #\+ #\-) (abs th) ts)))

(define write-iso-datime (universal-time &optional stream (time-zone (time-zone)))
  "Writes an ISO 8601 date and time on STREAM.
When STREAM is NIL, it return it as a string."
  (multiple-value-bind (seconds minutes hours day month year day-of-the-week daylight-savings-p timezone)
      (decode-universal-time universal-time time-zone)
    (declare (rational timezone)
	     (ignore day-of-the-week))
    (let ((offset (if daylight-savings-p (1- timezone) timezone)))
      (declare (rational offset))
      (multiple-value-bind (th ts) (truncate offset)
        (declare (fixnum th ts))
        (setf ts (truncate (* (abs ts) 60)))
        (format stream "~4D-~2,'0D-~2,'0DT~2,'0D:~2,'0D~:[~;~:*:~2,'0D~]~:[~C~2,'0D:~2,'0D~;Z~]"
                year month day hours minutes seconds (zerop offset) (if (plusp th) #\+ #\-) (abs th) ts)))))

(define decode-interval (seconds)
  (declare (values weeks days hours minutes seconds))
  "Decodes SECONDS into something more presentable to humans."
  (multiple-value-bind (weeks r1)
      (floor seconds #.(* 60 60 24 7))
    (multiple-value-bind (days r2)
        (floor r1 #.(* 60 60 24))
      (multiple-value-bind (hours r3)
          (floor r2 #.(* 60 60))
        (multiple-value-bind (minutes seconds)
            (floor r3 60)
          (values weeks days hours minutes seconds))))))


;; Make this faster if it is every used for something important.   3/16/97 -- JCMa.
(defun write-interval (seconds &optional (stream *standard-output*))
  "Writes the interval SECONDS to STREAM in English."
  (multiple-value-bind (weeks days hours minutes seconds)
      (decode-interval seconds)
    (format stream "~:[ ~D week~:P~;~*~]~:[ ~D day~:P~;~*~]~:[ ~D hour~:P~;~*~]~:[ ~D minute~:P~;~*~]~:[ ~D second~:P~;~*~]"
            (zerop weeks) weeks (zerop days) days (zerop hours) hours (zerop minutes) minutes (zerop seconds) seconds)))

;; Used in ftp client
(defun current-time (&optional (timezone 0))
  "Returns the current date and time values."
  (declare (values (sec min hour day month year daylight-p zone)))
  (decode-universal-time (if http::*server* (http::server-request-time http::*server*) (get-universal-time)) timezone))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(define clear-white-space (stream)
  (loop for char = (and (listen stream)
                        (peek-char nil stream nil))
        while char                              ;clear any dangling White Space due to buggy clients.
        when (member char '(#\return #\linefeed #\space #\tab) :test #'eql)
          do (read-char stream t)
        return t))
   
(define-generic http-input-data-available-p (stream &optional timeout-seconds)
  (:documentation "Returns non-null when input data is available on the HTTP STREAM within
TIMEOUT-SECONDS.  When timeout-seconds is null, data must be immediately
available. A dead HTTP connection means no data is available.
Ports can specialize this as necessary for their stream and process implementations."))

(defmethod http-input-data-available-p (stream &optional timeout-seconds)
  (labels ((data-available-p (stream)
             (loop for char = (when (listen stream)
                                (peek-char nil stream nil))
                   while char
                   when (member char '(#\return #\linefeed #\space #\tab) :test #'eql)
                     do (read-char stream t)
                   else
                     return t                   ;something still there.
                   finally (return nil)))
           (continue-p (stream)
             (or (not (www-utils:live-connection-p stream))     ;connection went dead
                 (data-available-p stream))))   ;data available
    (declare (inline data-available-p))
    (cond ((not (www-utils:live-connection-p stream)) nil)
          ((data-available-p stream) t)
          ((and timeout-seconds (not (zerop timeout-seconds)))
           ;; Block until there is reason to take action
           (process-wait-with-timeout
             "HTTP Request Wait" timeout-seconds #'continue-p stream)
           ;; Determine whether input data was available without consing.
           (and (www-utils:live-connection-p stream)
                (listen stream)))
          (t nil))))

;;;------------------------------------------------------------------- 
;;;
;;; PARSING NUMBERS
;;;

;(define write-integer-to-string (integer &optional (radix *print-radix*) (width 0) (pad-char #\0) string offset)
;  "Writes a non-negative INTEGER to a string according to RADIX.
;When string is shorter than WIDTH, it is padded with leading PAD-CHARs.
;When STRING is provided, INTEGER is written to the string started at OFFSET.
;Otherwise, a new string is created."
;  (declare (fixnum radix width)
;	   (integer integer))
;  (let* ((n-digits (cond ((< -1 integer 10) 1)
;			 ((< 9 integer 100) 2)
;			 ((< 99 integer) (ceiling (log integer radix)))	;is there a faster way to get width?
;			 (t (error "Integer is the negative number, ~D, which is not handled." integer))))
;	 (size (max n-digits width))
;	 (start 0)
;	 buffer start-num end fill-pointer-p)
;    (declare (fixnum n-digits size start))
;    (cond (string
;	   (setq buffer string
;		 fill-pointer-p (array-has-fill-pointer-p string)
;		 start (or offset (if fill-pointer-p (fill-pointer string) (error "~S has no fill pointer and OFFSET was not specified." string)))))
;	  (t (setq buffer (make-array size :element-type *standard-character-type*))))
;    (setq start-num (the fixnum (+ start width))
;	  end (the fixnum (+ start size))
;	  start-num (the fixnum (- end n-digits)))
;    (with-fast-array-references ((buffer buffer string))
;      (loop with digits = integer and last-digit
;	    for idx fixnum downfrom (1- end) to start-num
;	    do (multiple-value-setq (digits last-digit)
;		 (truncate digits radix))
;	       (setf (aref buffer idx) (digit-char last-digit radix)))
;      (unless (= size n-digits)
;	(loop for idx fixnum upfrom start below start-num
;	      do (setf (aref buffer idx) pad-char))))
;    (when fill-pointer-p
;      (setf (fill-pointer string) end))
;    buffer))

(define write-integer-to-string (integer &optional (radix *print-radix*) (width 0) (pad-char #\0) string offset)
  "Writes a non-negative INTEGER to a string according to RADIX.
When string is shorter than WIDTH, it is padded with leading PAD-CHARs.
When STRING is provided, INTEGER is written to the string started at OFFSET.
Otherwise, a new string is created."
  (declare (fixnum radix width)
	   (integer integer))
  (labels ((write-digits (integer radix string start nchars offset)
	     (declare (fixnum start nchars offset))
	     (multiple-value-bind (digits last-digit)
		 (truncate integer radix)
	       (cond ((zerop digits)
		      (let* ((offset (if (< nchars offset) (the fixnum (- offset nchars 1)) 0))
			     (idx (the fixnum (+ start offset))))
			(unless string
			  (setq string (make-array (the fixnum (+ 1 nchars offset)) :element-type *standard-character-type*)))
			(setf (aref string idx) (digit-char last-digit radix))
			(values string (1+ idx) idx)))
		     (t (multiple-value-bind (buffer idx start-num)
			    (write-digits digits radix string start (1+ nchars) offset)
			  (declare (fixnum idx))
			  (setf (aref buffer idx) (digit-char last-digit radix))
			  (values buffer (1+ idx) start-num)))))))
    (let ((start 0)
	  fill-pointer-p)
      (declare (fixnum start))
      (when string
	(setq fill-pointer-p (array-has-fill-pointer-p string)
	      start (or offset (if fill-pointer-p (fill-pointer string) (error "~S has no fill pointer and OFFSET was not specified." string)))))
      (multiple-value-bind (buffer end start-num)
	  (write-digits integer radix string start 0 width)
	(declare (fixnum end start-num))
	(unless (= start start-num)
	  (loop for idx fixnum upfrom start below start-num
		do (setf (aref buffer idx) pad-char)))
	(when fill-pointer-p
	  (setf (fill-pointer string) end))
	buffer))))

(define print-integer (integer &optional (stream *standard-output*) (radix 10))
  "Returns a list of characters required to print the INTEGER.
No minus sign is provided for negative integers."
  (let ((buffer (make-array 2 :element-type *standard-character-type* :adjustable t :fill-pointer 0)))
    (declare (dynamic-extent buffer))
    (with-fast-array-references ((buffer buffer string))
      (loop with digits = integer and last-digit
	    do (multiple-value-setq (digits last-digit)
		 (truncate digits radix))
	    do (vector-push-extend (digit-char (abs last-digit) radix) buffer)
	    until (zerop digits))
      (loop for idx downfrom (1- (the fixnum (fill-pointer buffer))) to 0
	    do (write-char (aref buffer idx) stream))))
  integer)

(define parse-float (string &optional (start 0) (end (length string)) (radix 10) junk-allowed)
  "Parses STRING and returns either an integer or a float depending on what's there."
  (declare (values integer-or-float)
           (fixnum start end radix))
  (let ((pos (char-position #\. string start end)))
    (if pos
        (let* ((p2 (the fixnum (1+ pos)))
               (last (and (< p2 end)
                          (digit-char-p (aref string p2) radix)
                          (or (position-if-not #'(lambda (ch) (digit-char-p ch radix)) string :start p2 :end end)
                              end))))
          (if last
              (the single-float
                   (+ (the integer (parse-integer string :start start :end pos :radix radix :junk-allowed junk-allowed))
                      ;; care to avoid dangerous reader macros.  Is there a better way to parse floats?
                      (the single-float (read-from-string string nil 0 :start pos :end last))))
              (the integer (parse-integer string :start start :end pos :radix radix :junk-allowed junk-allowed))))
        (the integer (parse-integer string :start start :end end :radix radix :junk-allowed junk-allowed)))))

(define-macro with-image-coordinates ((http-searchable-object) &body body)
  "Use this inside a response function for an image search to bind X and Y to image coordinates."
  (let ((x-var (intern "X"))
        (y-var (intern "Y")))
    `(macrolet ((parse-coordinate (url string start end)
                  `(handler-case-if (not *debug-server*)
                      (parse-float ,string ,start ,end)
                     (error () (error 'bad-syntax-provided :url ,url
                                      :format-string "Bad coordinate provided for image search.")))))
       (with-slots (url:search-keys) ,http-searchable-object
         (let* ((coordinate-string (car url:search-keys))
                (length (length coordinate-string))
                (pos (char-position #\, coordinate-string 0 length))
                (,x-var (and pos (parse-coordinate ,http-searchable-object coordinate-string 0 pos)))
                (,y-var (and pos (parse-coordinate ,http-searchable-object coordinate-string (the fixnum (1+ pos)) length))))
           (unless-every
             ((and ,x-var ,y-var)
              (error 'bad-syntax-provided :url ,http-searchable-object
                     :format-string "Client failed to provide both X and Y coordinates for image search."))
             ((and (numberp ,x-var) (numberp ,y-var))
              (error 'bad-syntax-provided :url ,http-searchable-object
                     :format-string "Client failed to provide numbers for both X and Y coordinates during image search.")))
           ,@body)))))

;;;------------------------------------------------------------------- 
;;;
;;; LIMITING THE NUMBER OF SIMULTANEOUS CONNECTIONS SERVED.
;;;

(define-macro with-connection-noted (&body body)
  `(unwind-protect
       (progn (atomic-incf *number-of-connections*)
              . ,body)
     (atomic-decf *number-of-connections*)))

(define-macro without-connection-overflows ((url) &body body)
  "Rejects excessive connections by signalling a server-overloaded condition.
See the variable *MAXIMUM-NUMBER-OF-CONNECTIONS*."
  `(cond ((> *number-of-connections* *maximum-number-of-connections*)
          (error 'server-overloaded :url ,url))
         (t ,@body)))

(define set-maximum-number-of-connections (number)
  "Sets the maximum number of simultaneous connections that the server will accept."
  (declare (fixnum number))
  (check-type number integer)
  (setq *maximum-number-of-connections* number
        *reject-connection-threshold* (+ number (floor (the single-float (* 0.2 number)))))
  number)

;;;------------------------------------------------------------------- 
;;;
;;; SUBNET SECURITY
;;;

(define parse-internet-address (ip-address)
  "Parses IP-ADDRESS specification."
  (declare (optimize (speed 3)))
  (if (and (stringp ip-address) (equalp ip-address "localhost"))
      (local-host)				;RFC 1738 defines this string
    (www-utils::%parse-internet-address ip-address)))

(defvar *named-subnet-table* (make-hash-table :test #'equal))

(defun get-named-subnet (name &optional error-p)
  "Returns the list of parsed IP addresses denoted by NAME."
  (declare (values ip-address-spec found-p))
  (multiple-value-bind (value found-p)
      (gethash (ensure-string name) *named-subnet-table*)
    (cond (found-p (values value found-p))
	  (error-p (error "~S is not a known named subnet." name))
	  (t nil))))

;; Don't store parsed IP addresses so that images can be saved out without complication
(defun set-named-subnet (name ip-addresses &optional flatten-p)
  "Associates NAME with the parsed IP addresses of IP-ADDRESSES."
  (declare (values ip-addresses))
  ;; make sure the addresses are good
  (labels ((check-spec (ip-addresses)
	     (loop for spec in ip-addresses
		   unless (or (multiple-value-bind (value found-p)
				  (get-named-subnet spec nil)
				value found-p)
			      (parse-internet-address spec))
		     do (error "Bad IP address specification, ~S, in IP-ADDRESSES." spec)
		   finally (return ip-addresses)))
	   (flatten-spec (ip-addresses)
	     (loop for spec in ip-addresses
		   nconc (multiple-value-bind (value found-p)
			     (get-named-subnet spec nil)
			   (cond (value (flatten-spec value))
				 (found-p nil)
				 ((parse-internet-address spec) (list spec))
				 (t (error "Bad IP address specification, ~S, in IP-ADDRESSES." spec)))))))
    (let ((entry (if flatten-p (flatten-spec ip-addresses) (check-spec ip-addresses))))
      (setf (gethash (ensure-string name) *named-subnet-table*) entry)
      entry)))

(defun remove-named-subnet (name)
  "Forgets the named subnet denoted by NAME."
  (remhash (ensure-string name) *named-subnet-table*))

(defun clear-named-subnets ()
  "Forgets all known named subnets."
  (clrhash *named-subnet-table*))

(defmacro define-subnet (name (&key flatten-p) &rest ip-addresses)
   "Defines name to denote IP-ADDRESSES.  IP-ADDRESSES are IP addresses or
symbols denoting other named subnets. Note that named subnets are resolved
when they are used, for example by a call to EXPORT-URL, and therefore,
changes to named subnets require re-export of URL referring to them."
   `(set-named-subnet ',name ',ip-addresses ,flatten-p))

(define parse-internet-addresses (ip-addresses)
  "Parses IP-ADDRESSES into a list of ip-address specifications.
Specifications can include named subnets, which are resolved
when this function is executed."
  (check-type ip-addresses list)
  (labels ((parse-address-spec (ip-address)
	     (typecase ip-address
	       ((or string symbol)
		(multiple-value-bind (expansion found-p)
		    (get-named-subnet ip-address nil)
		  (cond (expansion (parse-internet-addresses expansion))
			(found-p nil)
			(t (list (parse-internet-address ip-address))))))
	       (t (list (parse-internet-address ip-address))))))
    (loop for ip-address in ip-addresses
	  for parsed-spec = (parse-address-spec ip-address)
	  when parsed-spec
	    nconc parsed-spec)))

(declaim (special *read-subnets* *write-subnets*))

(defun update-secure-subnets ()
  (setq *secure-subnets* (union *read-subnets* *write-subnets*)))

(eval-when (:load-toplevel :execute :compile-toplevel)
(define-macro define-secure-subnet-type (name documentation &key secure-subnets-component)
  "Defines a new secure subnet type.
Provides the function parse-<name> and the macro define-<name>."
  (let* ((variable (symbolize (concatenate 'string "*" (string name) "*") :http))
	 (parse-function-name (symbolize (format nil "PARSE-~A" name) :http))
	 (define-macro-name (symbolize (format nil "DEFINE-~A" name) :http))
	 (code `(setq ,variable (parse-internet-addresses subnets))))
    `(progn (defparameter ,variable nil ,(string-capitalize documentation :start 0 :end 1))
	    (defun ,parse-function-name (subnets)
	      ,(format nil "Binds ~S to the parsed IP addresses in SUBNETS." variable)
	      ,(if secure-subnets-component
		   `(prog1 ,code
			   (update-secure-subnets))
		   code))
	    (defmacro ,define-macro-name (&body subnets)
	      ,(format nil "Define ~A" documentation)
	      `(,',parse-function-name ',subnets))))))

(define-secure-subnet-type
  read-subnets
  "the list of trusted subnets for IP-subnet level access control."
  :secure-subnets-component t)

(define-secure-subnet-type
  write-subnets
  "the subnets trusted for HTTP write methods by the HTTP server on any port."
  :secure-subnets-component t)

(defmacro define-secure-subnets (&body subnets)
  "OBSOLETE MACRO: Use DEFINE-READ-SUBNETS and DEFINE-WRITE-SUBNETS instead."
  `(parse-read-subnets ',subnets))

(define-secure-subnet-type
  disallowed-subnets
  "the list of subnets that are denied access globally.")

(define-secure-subnet-type
  proxy-subnets
  "the subnets trusted by the HTTP server on any port.
These subnets override the secure subnets define by define-secure-subnets.
However, if proxy subnets are not provided, they default to the secure subnets.")

(define-secure-subnet-type
 proxy-connect-disallowed-destination-subnets
 "The list of destination subnets for which the HTTP CONNECT is disallowed.
This can be used, for example, to prevent use of CONNECT to obtain a tunnel
to a host behind a firewall or the local host.")

(define-macro with-subnet-access-control ((ip-address secure-subnets &key rejection-form
                                                      deny-subnets require-secure-subnets) &body body)
  "Executes REJECTION-FORM whenever ip-ADDRESS is not on SECURE-SUBNETS,
Otherwise executes BODY. If REQUIRE-SECURE-SUBNETS is non-null, all accesses are
rejected whenever SECURE-SUBNETS is null. If DENY-SUBNETS is provided,
all requests from those IP addresses are rejected. DENY-SUBNETS is ignored when
require-secure-subnets is non-null."
  `(let ((subnets ,secure-subnets)
         ,.(when deny-subnets
             `((ip-addr ,ip-address)
               (deny-subnets ,deny-subnets))))
     (cond (,(cond (require-secure-subnets
                    `(and subnets (www-utils:ip-host-trusted-p ,ip-address subnets)))
                   (deny-subnets
                    `(and (or (null deny-subnets)
                              (not (www-utils:ip-host-trusted-p ip-addr deny-subnets)))
                          (or (null subnets) (www-utils:ip-host-trusted-p ip-addr subnets))))
                   (t `(or (null subnets) (www-utils:ip-host-trusted-p ,ip-address subnets))))
            ,@body)
           (t ,rejection-form))))

(define accept-remote-write-method-p (url)
  "Returns non-null if side-effecting methods like PUT and DELETE are accepted from parsed-ip-address."
  (ecase *accept-write-methods*
    (:none nil)
    (:local-host
      (let ((secure-subnets (list (local-host-parsed-ip-address))))
        (declare (dynamic-extent secure-subnets))
        (www-utils:ip-host-trusted-p (server-address *server*) secure-subnets)))
    ;; Only authenticated users on trusted subnets
    (:authenticated-users-on-write-subnets
      (and (or (url:write-subnets url) *write-subnets*)
	   (url:capabilities url)))
    ;; Anyone on a subnet trusted to write to this URL
    (:write-subnets
      (or (url:write-subnets url) *write-subnets*))
    ;; Authenticated users with the correct capabilities 
    (:authenticated-users
      (url:capabilities url))
    ;; Either authenticated users or trusted subnets
    (:access-controlled
      (or (url:capabilities url)
	  (url:write-subnets url)
	  *write-subnets*))
    ;; Anyone who can access the system
    (:remote-host t)))

(define-macro with-remote-write-control ((url &key rejection-form) &body body)
  `(cond 
     ((accept-remote-write-method-p ,url)
      ,@body)
     (t ,rejection-form)))

;;;------------------------------------------------------------------- 
;;;
;;; NEW MACRO
;;;

(defun %make-query-binding (url query query-alist &optional (null-binding-value :unbound))
  `(let ((entry (assoc ,query ,query-alist :test #'eq)))
     (if entry
         (second entry)
         ,@(case null-binding-value
             (:error `((error 'server-internal-error :url ,url
                              :format-string "Unbound Query: ~S was not returned by the client."
                              :format-args '(,query))))
             (t `(,null-binding-value))))))

;; COLLECTION-FLAG flag is non-null when the cons which is tha value resulted
;; from collecting multiple values from different input fields referring to
;; the same query identifier.  11/17/94 -- JCMa.
(declaim (inline %values-collected-p))

(defun %values-collected-p (query query-alist)
  (check-type query keyword)
  (third (assoc query query-alist :test #'eq)))

(defun %query-baggage (query query-alist)
  (check-type query keyword)
  (fourth (assoc query query-alist :test #'eq)))

(define-macro bind-query-values (queries (url query-alist) &body body)
  "Binds variables to the names of queries with the values of queries in a form.
For use in response functions, dynamic presentation accept methods, and
present methods.  Query can be a symbol or a lists of (query default-value)
If default value is :ERROR, an unbound query condition is signalled.  The
keyword :UNBOUND, used as a default so that response function can handle it.
Sometimes more than one input field will use the same query identifier.  In
these cases, the values returned for the query will be collected in a list.
The predicate VALUES-COLLECTED-P can be applied to the query keyword in order
to find out if collection has taken place. QUERY-BAGGAGE returns any
associated string that may have been packed with HTML:PACK-QUERY-NAME and
transmitted in a form by HTML:ACCEPT-INPUT."  
  (loop with query and null-binding-value
        for spec in queries
        do (etypecase spec
             (symbol (setq query spec
                           null-binding-value nil))
             (cons (setq query (car spec)
                         null-binding-value (second spec))))
        collect `(,query ,(%make-query-binding url (intern (symbol-name query) :keyword) query-alist null-binding-value))
          into bindings
        finally (return (if bindings
                            `(macrolet
                               ((values-collected-p (query)
                                  `(%values-collected-p ,query ,',query-alist))
                                (query-baggage (query)
                                  `(%query-baggage ,query ,',query-alist)))
                               (let ,bindings . ,body))
                            `(progn . ,body)))))


;; this macro is used for lisp-lisp posting.
;; export when we're happy about the interface.   6/3/96 -- JCMa.
(define-macro bind-query-values* (queries (url query-alist) &body body)
  "Binds variables to the names of queries with the values of queries in a form.
This is like BIND-QUERY-VALUES but it provides facilities for reinstatiating
lisp objects. It uses a slightly different syntax and allows readers to
entries to be specified. A query is either a symbol or a cons of the form
(query &key package reader default). In the extended form, QUERY is a symbol
reader is function that builds tyhe lisp representation from the raw query
string. If PACKAGE is provided, the reader function is applied with PACKAGE as
the current package.  DEFAULT is the default value to use when the query is
not supplied and behaves exactly like the default in BIND-QUERY-VALUES."
  (flet ((get-bindings-and-initializers (query-entry)
           (destructuring-bind (query &key package reader default) query-entry
             (values query
                     default
                     `(,query
                       ,(cond (reader
                               `(setq ,query (,reader ,query)))
                              (package
                               `(let ((*package* ,package))
                                  (setq ,query (,(or reader 'read-from-string) ,query))))
                              (t (error "Unknown query case for ~S" query-entry))))))))
    (loop with query and default and initializer
          for q in queries
          do (etypecase q
               (cons (multiple-value-setq (query default initializer)
                       (get-bindings-and-initializers q)))
               (symbol (setq query q
                             default nil
                             initializer nil)))
          when initializer
            collect initializer into initializers
          collect (if default `(,query ,default) query) into bindings
          finally (return `(bind-query-values ,bindings (,url ,query-alist)
                             ,.(when initializers
                                 `((cond-every ,. initializers)))
                             ,@body)))))

(define return-form-values (stream &rest args &key &allow-other-keys)
  "This is used to return values from a response function for a http post
so that the calling client will get the values back.
writes an alist of returned values to stream without consing.
call this with alternating pairs of keyword value."
  (declare (dynamic-extent args))
  (loop initially (write-char #\( stream)
        for (keyword value) on args by #'cddr
        do  (write-string "(" stream)
            (prin1 keyword stream)
            (write-string " " stream)
            (prin1 value stream)
            (write-string ")" stream)
        finally (write-char #\) stream)))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(define launch-demo ()
  "Starts up the demo configuration."
  (dolist (file '("http:examples;configuration.lisp"
                  "http:examples;exports"))
    (load file :verbose nil)))

(define user-agent-capability-p (capability user-agent version)
  #.(format nil "Returns non-null when VERSION of USER-AGENT has CAPABILITY. Existing capabilities are: ~S."
            (mapcar #'car *user-agent-capabilities*))
  (flet ((%user-agent-capability-p (capability user-agent version)
           (let ((entry (assoc capability *user-agent-capabilities* :test #'eq))
                 ua-entry)
             (cond (entry
                    (and (setq ua-entry (assoc user-agent (cdr entry) :test #'eq))
                         (or (null (cdr ua-entry))      ;no versions means all versions
                             (not (null (member version (cdr ua-entry) :test #'eq))))))
                   (t (error "~S is not one of the known capabilities, ~S, for user agents."
                             capability (mapcar #'car *user-agent-capabilities*)))))))
    (declare (inline %user-agent-capability-p))
    (typecase capability
      (keyword
        (%user-agent-capability-p capability user-agent version))
      (cons
        (loop for item in capability
              do (unless (%user-agent-capability-p item user-agent version)
                   (return-from user-agent-capability-p nil))
              finally (return-from user-agent-capability-p t))))))

;; remove sometime   10/8/95 -- JCMa.
;; Obsolete: Use user-agent-capability-p instead.
(defun tables-capable-user-agent-p (user-agent version)
  (user-agent-capability-p :tables user-agent version))


;;;------------------------------------------------------------------- 
;;;
;;; IMAGE SIZES
;;; 

(define gif-image-size (stream &optional (error-p t)
                               &aux color-table-flags global-color-table-p (global-color-table-size 0)
                               (nextbyte 0) (width 0) (height 0) (acceptable-headers '("GIF87a" "GIF89a")))
  "Returns WIDTH and HEIGHT for the first image encountered in a GIF data stream. 
   Assumes stream is positioned at beginning of gif data.
   For invalid GIF data, when ERROR-P it throws an error, otherwise
     it returns null values."
  (declare (values height width gif-image-p))
  (let ((startpos (file-position stream))
        (test-header (make-array 6 :element-type *standard-character-type*))
        (image-descriptor-found nil))
    (loop for idx upfrom 0 below 6
          do (setf (aref test-header idx) (code-char (read-byte stream nil nil))))
    (cond
      ((member test-header acceptable-headers :test #'equalp)
       (file-position stream (+ startpos 10))   ; set up to read color table flags
       (setf color-table-flags  (read-byte stream nil nil))
       (setf global-color-table-p (logbitp 7 color-table-flags))        ; global color table present?
       (when global-color-table-p 
         (setf global-color-table-size (* 3 (expt 2 (1+ (logand 7 color-table-flags))))))
       (file-position stream (+ startpos 13 global-color-table-size))
       ;; now we could have an AE, a CE, a PTE, a GCB, a GCE, or an Image Descriptor
       (setf nextbyte (read-byte stream nil nil))       ; is all you need
       (loop until image-descriptor-found
             do (case nextbyte
                  (#x21                         ; it's an extension
                   (setf nextbyte (read-byte stream nil nil))
                   (case nextbyte
                     (#xF9                      ; Graphic control extension
                      (file-position stream (+ 6 (file-position stream)))
                      (setf nextbyte (read-byte stream nil nil)))
                     ;; Barring a rare Plain Text extension, the next block will be our image descriptor.
                     ;;  But just to be absolutely sure, we'll do the outer loop again.
                     (#xFE                      ; Comment extension
                      (loop for byte = (read-byte stream nil nil)
                            until (eql byte 0)
                            do (file-position stream (+ byte (file-position stream))))
                      (setf nextbyte (read-byte stream nil nil)))
                     (#xFF                      ; Application extension
                      (file-position stream (+ (read-byte stream nil nil) 
                                               (file-position stream)))
                      (loop for byte = (read-byte stream nil nil)
                            until (eql byte 0)
                            do (file-position stream (+ byte (file-position stream))))
                      (setf nextbyte (read-byte stream nil nil)))
                     ;; Other extensions, like Plain Text (#x01) seem to
                     ;; be extremely rarely used and thus we aren't
                     ;; handling them for now.  Someone needs to check
                     ;; the gif spec and make this entry work.   6/4/98 JCMa.
                     ;; Punt unless someone cares -- JCMa 10/22/1998.
                     (#x01 (return-from gif-image-size (values nil nil t)))
                     (t (if error-p
                            (error "Unknown extension found, ~X, in GIF data stream, ~S. Please report the problem."
                                   nextbyte stream)
                            (return-from gif-image-size (values nil nil t))))))
                  (#x2C                         ; It's the Image Descriptor
                   (file-position stream (+ 4 (file-position stream)))
                   (setf image-descriptor-found t))
                  (t (if error-p
                         (error "Invalid GIF format. Byte header of ~X found." nextbyte)
                         (return-from gif-image-size (values nil nil t))))))
       (setf width (read-byte stream nil nil))  ; LS Byte is first
       (incf width (* 256 (read-byte stream nil nil)))
       (setf height (read-byte stream nil nil)) ; LS Byte is first
       (incf height (* 256 (read-byte stream nil nil)))
       (values width height t))
      (error-p (error "~S is not a GIF data stream." stream))
      (t (values nil nil nil))))) 

(define gif-image-dimensions (pathname &optional (error-p t))
  "Returns HEIGHT and WIDTH for the first image encountered in a GIF data stream. 
   For an invalid GIF file, when ERROR-P it throws an error, otherwise
     it returns null values."
  (declare (values width height gif-image-p))
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
    (gif-image-size stream error-p)))

#+ignore
(defun directory-gif-dimensions (pathname &optional (stream *standard-output*))
  "Prints out x y sizes of each gif file in a directory you choose."
  (loop for (path) in (directory-info pathname)
        when (equalp "gif" (pathname-type path))
          do (multiple-value-bind (x y) (gif-image-dimensions path)
               (format stream "~%~A: ~D ~D" path x y)))) 

(define jpeg-image-size (stream &optional (error-p t) &aux (file-length (file-length stream)))
  "Returns WIDTH and HEIGHT for JPEG encountered in data stream. 
   Assumes stream is positioned at beginning of JPEG data."
  (declare (values height width jpeg-image-p))
  (labels ((read-two-bytes (stream)
             (+ (ash (read-byte stream nil nil) 8)
                (read-byte stream nil nil)))
           (get-next-type (stream error-p)
             (let ((lead (read-byte stream nil nil))
                   (next (read-byte stream nil nil)))
               (cond ((and lead (= lead #xff)) next)
                     (error-p (error "~X is not a mark for JPEG stream ~A" lead stream))
                     (t (return-from jpeg-image-size (values nil nil t))))))
           (image-block-p (type)
             (and type (<= #xc0 type #xcf) (/= type #xc4) (/= type #xcc)))
           (get-size-from-image-block (stream)
             (file-position stream (+ (file-position stream) 3))
             (let ((height (read-two-bytes stream))
                   (width (read-two-bytes stream)))
               (values width height t)))
           (skip-block (stream type error-p)
             (unless (= type #xd9)
               (let* ((length (read-two-bytes stream))
                      (new-length (+ length -2 (file-position stream))))
                 (file-position stream (+ length -20 (file-position stream)))
                 (cond ((<= new-length file-length)
                        (file-position stream new-length))
                       (error-p (error "Wrong format in JPEG stream: ~S" stream))
                       (t (return-from jpeg-image-size (values nil nil t))))))))
    (let ((first-mark (get-next-type stream error-p)))
      (cond ((and first-mark (= first-mark #xd8))
             (loop for type = (get-next-type stream error-p)
                   while type
                   when (image-block-p type)
                     do (return (get-size-from-image-block stream))
                   else do (skip-block stream type error-p)))
            (error-p (error "First mark in JPEG stream ~S is ~X and not #xd8." stream first-mark))
            (t (values nil nil nil))))))

(define jpeg-image-dimensions (pathname &optional (error-p t))
  "Returns WIDTH and HEIGHT for the JPEG image. 
   For an invalid JPEG file, when ERROR-P it throws an error, otherwise
     it returns null values."
  (declare (values width height))
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
    (jpeg-image-size stream error-p)))

(define png-image-size (stream &optional (error-p t))
  "Returns WIDTH and HEIGHT for PNG encountered in data stream. 
   Assumes stream is positioned at beginning of PNG data."
  (declare (values width height))
  (labels ((read-four-bytes-as-integer (stream)
             (+ (ash (read-byte stream nil nil) 24)
                (ash (read-byte stream nil nil) 16)
                (ash (read-byte stream nil nil) 8)
                (read-byte stream nil nil)))
           (is-png-file-p (stream)
             (when (> (file-length stream) 8)
               (every (lambda (byte)
                        (= byte (read-byte stream nil nil)))
                      #(137 80 78 71 13 10 26 10)))))
    (cond ((is-png-file-p stream)
           (file-position stream (+ 8 4 4))
           (values (read-four-bytes-as-integer stream) (read-four-bytes-as-integer stream) t))
          (error-p
           (error "Stream ~a is not a PNG image." stream))
          (t (values nil nil nil)))))


(define png-image-dimensions (pathname &optional (error-p t))
  "Returns WIDTH and HEIGHT for the PNG image."
  (declare (values width height))
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
    (png-image-size stream error-p)))


;;;------------------------------------------------------------------- 
;;;
;;; PARSE QUALITY VALUES PER HTTP 1.1
;;; by Ken Anderson

;;; Here's a function that parses qvalues as in HTML 1.1, p. 26(?).  It
;;; returns 0.0 if it can't find anything like what it wants.  It uses an idea
;;; from Henry Baker which i can generalize into other tokenizers.

(defmacro with-string-parsing-operators (&body body)
  "A simple parsing language built on the ideas of:
Henry G. Baker, Pragmatic Parsing in Common Lisp; or, putting defmacro 
on Steroids, Lisp Pointers, IV, 2, 1991, p. IV-2.3 - IV-2.15.
The language is just enough to do parse-qvalue."
  `(macrolet ((%+ (a b) `(the fixnum (+ (the fixnum ,a) (the fixnum ,b))))
              (%* (a b) `(the fixnum (* (the fixnum ,a) (the fixnum ,b))))
              (advance () 
                `(setq start (%+ start 1)
                       creg (if (< start end) (sref string start) nil)))
              (sref (s i) `(aref ,s ,i))
              (is (c k f)
                `(if (eql creg,c) (progn (advance)
                                         ,k)
                     ,f))
              (opt (test &rest args)
                (let ((k (if (cdr args) (second args) (car args)))
                      (action (if (cdr args) (car args) nil)))
                  `(let ((it ,(if (consp test)
                                  `(and creg (,(car test) creg ,@(cdr test)))
                                  `(eql creg ,test))))
                     (when it 
                       ,action
                       (advance))
                     ,k))))
     (let ((creg (if (< start end) (sref string start) nil)))
       ,@body)))

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

#|(flet ((test-qvalue (string)
         (= (parse-quality-value string 0 (length string))
            (read-from-string string nil nil))))
  (map nil #'(lambda (s) (assert (test-qvalue s)))
       '("0.0" "0.1" "0.8" "1.0" "0.23" "0.493" "1.00" "1.000" ".1" ".23" ".493")))|#


;;;------------------------------------------------------------------- 
;;;
;;; FAST COMPILE-TIME FORMAT
;;;

(eval-when (:load-toplevel :execute :compile-toplevel) 

(defgeneric %princ-item (item stream)
   (:documentation "Prints item without slashification on stream.
Specialize for higher performance according platform."))

(defmethod %princ-item (item stream)
   (princ item stream))

(defmethod %princ-item ((item string) stream)
   (write-string item stream)) 

(defun %fast-format-execute-command (idx format-string stream arg-n args length &aux base-p)
    (macrolet ((pop-arg (args)
                 `(prog1 (pop ,args)
                         (incf arg-n)))
               (arg-n (n) `(nth ,n format-args)))
      (values
        (ecase (aref format-string (incf idx))
          ((#\A #\a)
           `(%princ-item,(pop-arg args) ,stream))
          ((#\D #\d)
           (setq base-p t)
           `(%princ-item ,(pop-arg args) ,stream))
          ((#\C #\c)
           `(write-char ,(pop-arg args) ,stream))
          ((#\S #\s)
           `(prin1 ,(pop-arg args) ,stream))
          ((#\Return #\Linefeed)
           (loop with s = (1+ idx)
                 for i upfrom s below length
                 while (member (aref format-string i) '(#\space #\tab #\Return #\Linefeed))
                 finally (when (< s i)
                           (setq idx (1- i))))
           nil)
          (#\&
           `(fresh-line ,stream))
          (#\%
           `(terpri ,stream))
          (#\~ `(write-char #\~ ,stream))
          ((#\I #\i)
           (pop-arg args)))
        args
        arg-n
        (1+ idx)
        base-p)))

  (defun %build-fast-format-code (stream format-string format-args)
    (check-type format-string string)
    (check-type format-args list)
    (let ((len (length format-string))
          (args (copy-list format-args))
          (arg-n 0))
      (declare (dynamic-extent args))
      (flet ((write-string-form (stream string start end)
	       (declare (fixnum start end))
	       (let ((length (- end start)))
		 (if (= 1 length)
		     `(write-char ,(aref string start) ,stream)
		     `(write-string ,(subseq string start end) ,stream :start 0 :end ,length)))))
        (loop with start = 0 and form and arg-form and base-p
              for idx = 0 then (1+ idx)
              while (< idx len)
              for char = (aref format-string idx)
              for inside-command-p = nil then start-command-p
              for start-command-p = (and (not inside-command-p) (char-equal char #\~))
              when start-command-p
                do (setq form (when (< start idx)
                                (write-string-form stream format-string start idx)))
                   (multiple-value-bind (form n-args n-arg-n n-start bind-base-p)
                       (%fast-format-execute-command idx format-string stream arg-n args len)
                     (when bind-base-p
                       (setq base-p t))
                     (setq arg-form form
                           args n-args
                           arg-n n-arg-n
                           start n-start))
              when (and start-command-p form)
                collect form into result
              when (and start-command-p arg-form)
                collect arg-form into result
              finally (return (let ((code (if (< start idx)
                                              `(,.result ,(write-string-form stream format-string start idx) nil)
                                              `(,.result nil))))
                                (if base-p
                                    `((let ((*print-base* 10.)) ,.code))
                                    code)))))))
  )

;; moved from http:server;html2.lisp due to compile order problems   7/3/96 -- JCMa.
(define-macro fast-format (stream format-string &rest format-args)
  "A simple version of FORMAT that expands into fast code at compile time.
FORMAT-STRING must be a string.
The follow format directives are supported.

     ~%         -- hard carriage return
     ~&         -- fresh line
     ~~         -- writes a ~
     ~<Return>  -- ignores a carriage return or line feed
     ~A         -- writes a lisp object with escape nil
     ~C         -- writes a character with escape nil
     ~D         -- writes a number in base 10
     ~S         -- writes a lisp object with escape t
     ~I         -- inserts a lisp form

Arguments to directives are supported only as indicated." 
  (case stream
    ((nil)
     `(with-output-to-string (stream)
        ,. (%build-fast-format-code 'stream format-string format-args)))
    ((t)
     `(let ((stream *standard-output*))
        ,.(%build-fast-format-code 'stream format-string format-args)))
    (t `(let ((stream ,stream))
          ,.(%build-fast-format-code 'stream format-string format-args)))))

#|
(fast-format foo "~&the test is ~A but you never want ~D frobs ~%~S  ~
       ~%this is the end string" "FROB" 12 "BAR")

(fast-format *standard-output* "(~A)" 12)

(defun test (n)
  (time
    (loop for idx upto n
          do (fast-format #'sys:null-stream "~&the test is ~A but you never want ~D frobs ~%~S  ~
       ~%this is the end string" "FROB" 12 "BAR"))))

|#

;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(define server-mail-address (&optional recompute-p)
  "Returns the mail address for the server maintainer."
  (cond ((and (not recompute-p) *server-mail-address*))
        (t (setq *server-mail-address* (concatenate 'string *server-maintainer* "@" *default-mailer-host*)))))

(define email-address-for-bug-reports (&optional recompute-p)
  "Returns the maintenance email address for the server."
  (cond ((and (not recompute-p) *bug-http-server*))
        (t (setq *bug-http-server* (concatenate 'string *server-bug-list* "@" *default-mailer-host*)))))

(define write-server-mail-address (stream)
  "Writes the mail address for the server maintainer on STREAM."
  (fast-format stream "mailto:~A" (server-mail-address)))


;;;------------------------------------------------------------------- 
;;;
;;; HOST NAME VALIDATION
;;;

;; This should test for legal top-level domains.   7/13/96 -- JCMa.
(define valid-domain-name-string-p (hostname &optional (start 0) (end (length hostname)) )
  "Returns non-null if HOSTNAME is a valid internet domain name."
  (flet ((illegal-char-p (char)
           (member char '(#\% #\space #\tab #\return) :test #'eql)))
    (declare (inline illegal-char-p))
    (and (char-position #\. hostname start end t)
         (not (find-if #'illegal-char-p hostname :start start :end end)))))

(define ip-address-string-p (string)
  "Returns non-null if STRING is a Internet IP address (e.g., 128.52.39.11)."
  (every #'(lambda (ch) (or (digit-char-p ch 10) (eql ch #\.))) string))

(define valid-internet-mail-address-p (address &optional (start 0) (end (length address)) &aux pos)
  "Checks address for well-formedness returning ADDRESS,
or NIL when the address is ill-formed."
  (flet ((bad-host-char-p (char)
           (member char '(#\, #\( #\) :test #'eql)))
         (bad-user-name-char-p (char)
           (member char '(#\, #\( #\) :test #'eql))))
    (declare (inline bad-host-char-p bad-user-name-char-p))
    (and (stringp address)
         (setq pos (char-position #\@ address start end))
         (= 1 (count #\@ address :test #'eql :start pos :end end))
         (char-position #\. address (1+ (the fixnum pos)) end)
         (not (find-if #'bad-host-char-p address :start pos :end end))
         (not (find-if #'bad-user-name-char-p address :start start :end pos)))))

(define qualify-domain-name (name)
  "Returns name as a fully qualified domain name based on the local host domain name."
  (let* ((local-host-name (local-host-domain-name))
         (l1 (length local-host-name))
         (pos1 (char-position #\. local-host-name 0 l1))
         (l2 (length name))
         (l3 (+ (the fixnum l2) (- (the fixnum l1) (the fixnum pos1))))
         (qualified-name (make-array l3 :element-type *standard-character-type*)))
    (nfill-array qualified-name name :start1 0 :end1 l2 :start2 0 :end2 l2)
    (nfill-array qualified-name local-host-name :start1 l2 :end1 l3 :start2 pos1 :end2 l1)
    qualified-name))


;;;------------------------------------------------------------------- 
;;;
;;; RUDIMENTARY RESPONSE FUNCTION METERING
;;;

(define-macro with-metered-response ((url &key (server '*server*)) &body body)
  "Meters the CPU and elapsed time consumed by a response function.
Wrap this form at the outter most level of your response function.
Use URL:AVERAGE-RESPONSE-TIMES to obtain the results."
  `(multiple-value-prog1
     (progn . ,body)
     (record-response-times ,server ,url)))


;;;------------------------------------------------------------------- 
;;;
;;; CAPTURING STACK BACKTRACES FOR ERROR REPORTING
;;;

(define-generic write-stack-backtrace (error stream &optional n-frames)
  (:documentation "Writes a stack backtrace for ERROR on STREAM that includes N-FRAMES stack frames.
Each port should specialize this method for best automatic bug reporting."))

(defmethod write-stack-backtrace (error stream &optional (n-frames *stack-backtrace-number-of-frames*))
  (declare (ignore error n-frames))
  (write-string "[No stack backtrace available on this port.]" stream))

(defmethod write-stack-backtrace :around (error stream &optional (n-frames *stack-backtrace-number-of-frames*))
  (handler-case
    (call-next-method error stream n-frames)
    (error () (write-string "Error generating stack backtrace." stream))))

(define-generic stack-backtrace-string (error &optional n-frames buffer)
  (:documentation "Returns a string containing a stack backtrace for ERROR that includes N-FRAMES stack frames."))

(defmethod stack-backtrace-string (error &optional (n-frames *stack-backtrace-number-of-frames*) buffer)
  (if buffer
      (with-output-to-string (stream buffer)
        (write-stack-backtrace error stream n-frames))
      (with-output-to-string (stream)
        (write-stack-backtrace error stream n-frames))))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(define print-date-time (&key (universal-time (get-universal-time)) (stream *standard-output*))
  (multiple-value-bind (seconds minutes hours days month year day-of-week daylight-savings-p timezone)
      (decode-universal-time universal-time)
    (declare (ignore seconds year daylight-savings-p timezone))
    (fast-format stream "~D:~A~D ~A EST on ~A, ~A ~D"
                 hours (if (< minutes 10) "0" "") minutes (if (< hours 12) "am" "pm")
                 (day-of-the-week-string day-of-week)
                 (month-string month)
                 days)))

(define print-date (&key (universal-time (get-universal-time)) (format :iso) (stream *standard-output*))
  "Prints the UNIVERSAL-TIME on STREAM using FORMAT.

  FORMATS: :ISO
  :US-ENGLISH
  :US-ENGLISH+WEEKDAY"
   (multiple-value-bind (seconds minutes hours day month year day-of-the-week)
                                   (decode-universal-time universal-time)
       (declare (ignore seconds minutes hours))
       (ecase format
          (:iso (format stream "~4D-~2,'0D-~2,'0D" year month day))
          (:us-english
	    (let ((month (month-string month)))
	       (format stream "~A ~D, ~D" month day year)))
          (:us-english+weekday
	    (let ((day-of-the-week (day-of-the-week-string day-of-the-week))
	            (month (month-string month)))
	       (format stream "~A, ~A ~D, ~D" day-of-the-week month day year))))))

(define-macro with-response-ensured ((email-address   
                                       &key (subject "HTTP Request Processed Successfully")
                                       (response-generator '#'response-writer-to-inform-user-via-email-of-success)
                                       (server-address 'http:*server-mail-address*)
                                       (server '*server*))
                                     &body body)
  "Ensures that even if the HTTP connection is lost, the user will be notified via e-mail that 
 the processing of their HTTP request was successful despite the lost connection."
  `(cond ((server-live-connection-p ,server)
          ,@body)
         (t (send-mail-from ,server-address ,email-address ,subject (funcall ,response-generator ,server)))))

(defun careful-notify-log-window (continuation)
  "Notifies the console being careful with special characters."
  (using-resource (string line-buffer *line-buffer-size*)
    (setf (fill-pointer string) 0)
    (with-output-to-string (stream string)
      (funcall continuation stream)
      (finish-output stream)
      (if (find #\~ string :test #'eql)
	  (www-utils:notify-log-window "~A" string)
	  (www-utils:notify-log-window string)))))

(defun enable-url-areas (&optional (on-p (not *url-areas*)) (notify-p t))
  "Enables URL areas."
  (cond ((eq (not on-p) (not *url-areas*)) *url-areas*)
	(t (when notify-p 
	     (www-utils:notify-log-window "HTTP Server ~:[disabling~;enabling~] URL areas globally." on-p))
	   (setq *url-areas* on-p))))


;;;------------------------------------------------------------------- 
;;;
;;; CLUEFUL APROPOS TO FIND THE  RIGHT SYMBOLS WITHOUT A BIG MESS
;;;

;(defun apropos* (syms &optional (package *package*) type external-only)
;  "An apropos with a clue.
;SYMS can be an atom or a list of atoms. Each atom must be either a string or
;a symbol. When more than one atom is provided, the symbols found must contain
;every atom as substrings.
;TYPE can be any of FUNCTION, VARIABLE, or CLASS.
;When EXTERNAL-ONLY is specified, only external synmbols are returned."
;  (flet ((ensure-string (item)
;			(etypecase item
;			  (string item)
;			  (symbol (symbol-name item))))
;         (display (sym stream &aux class)
;		  (fresh-line stream)
;		  (prin1 sym stream)
;		  (cond-every
;		    ((fboundp sym)
;		     (princ (cond ((macro-function sym) " [macro]: ")
;				  ((special-operator-p sym) " [special form]: ")
;				  ((functionp (symbol-function sym)) " [function]: ")
;				  (t " [???]: "))
;			    stream)
;		     (princ (arglist sym) stream))
;		    ((boundp sym)
;		     (format stream " [Variable]: ~A" (symbol-value sym)))
;		    ((setq class (find-class sym nil))
;		     (format stream " [Class]: ~A" class))))
;         (satisfies-type-p (sym type)
;           (ecase type
;             (function (fboundp sym))
;             (variable (boundp sym))		;doesn't handle unbound special variables
;             (class (find-class sym nil)))))
;    (declare (inline ensure-string))
;    (etypecase syms
;      (cons
;	(destructuring-bind (first . rest) syms
;	  (loop for sym in (apropos-list first package external-only)
;		when (and (or (null type) (satisfies-type-p sym type))
;			  (loop for key in rest
;				always (string-search (ensure-string key) (symbol-name sym))))
;		  do (display sym *standard-output*)
;		  and collect sym)))
;      (atom
;	(cond (type
;	       (loop for sym in (apropos-list syms package external-only)
;		     when (satisfies-type-p sym type)
;		       do (display sym *standard-output*)
;		       and collect sym))
;	      (t (let ((results (apropos-list syms package external-only)))
;		   (dolist (sym results)
;		     (display sym *standard-output*))
;		   results)))))))

#-(or Genera MCL)
(defun apropos* (syms &optional (package *package*) type external-only)
  "An apropos with a clue.
SYMS can be an atom or a list of atoms. Each atom must be either a string or
a symbol. When more than one atom is provided, the symbols found must contain
every atom as substrings.
TYPE can be any of FUNCTION, VARIABLE, or CLASS.
When EXTERNAL-ONLY is specified, only external synmbols are returned."
  (flet ((ensure-string (item)
			(etypecase item
			  (string item)
			  (symbol (symbol-name item))))
	 (display (sym stream &aux class)
		  (fresh-line stream)
		  (prin1 sym stream)
		  (cond-every
		    ((fboundp sym)
		     (princ (cond ((macro-function sym) " [macro]: ")
				  ((special-operator-p sym) " [special form]: ")
				  ((functionp (symbol-function sym)) " [function]: ")
				  (t " [???]: "))
			    stream)
		     (princ (arglist sym) stream))
		    ((boundp sym)
		     (format stream " [Variable]: ~A" (symbol-value sym)))
		    ((setq class (find-class sym nil))
		     (format stream " [Class]: ~A" class))))
	 (satisfies-type-p (sym type)
	   (ecase type
	     (function (fboundp sym))
	     (variable (boundp sym))		;doesn't handle unbound special variables
	     (class (find-class sym nil)))))
    (etypecase syms
      (cons
	(let ((keys (mapcar #'ensure-string syms)))
	  (flet ((collect-symbol-p (sym)
		   (and (or (null type) (satisfies-type-p sym type))
			(loop for key in keys
			      always (string-search key (symbol-name sym))))))
	    (declare (inline collect-symbol-p))
	    (if external-only
		(do-external-symbols (sym package)
		  (when (collect-symbol-p sym)
		    (display sym *standard-output*)))
		(do-symbols (sym package)
		  (when (collect-symbol-p sym)
		    (display sym *standard-output*)))))))
      (atom
	(let ((key (ensure-string syms)))
	  (flet ((collect-symbol-p (sym)
		   (and (or (null type) (satisfies-type-p sym type))
			(string-search key (symbol-name sym)))))
	    (declare (inline collect-symbol-p))
	    (if external-only
		(do-external-symbols (sym package)
		  (when (collect-symbol-p sym)
		    (display sym *standard-output*)))
		(do-symbols (sym package)
		  (when (collect-symbol-p sym)
		    (display sym *standard-output*))))))))))

(defparameter *unsafe-characters-for-pathnames* '(#\space #\. #\~ #\' #\` #\_ #\, #\? #\; #\: #\( #\) #\|
						  #+LispWorks #\No-Break-Space	;weird char from Safari
						  )
  "Characters that are unsafe in pathname components.")

(defun ensure-safe-pathname-component (string &optional (start 0) (end (length string))
                                              &aux (unsafe-chars *unsafe-characters-for-pathnames*))
  "Removes characters from STRING that  are unsafe in pathname components."
  (declare (values safe-string new-p)
           (fixnum start end))
  (flet ((unsafe-char-p (char)
           (member char unsafe-chars)))
    (declare (inline unsafe-char-p)) 
    (cond ((http::%fast-position-if unsafe-char-p string :start start :end end)
           (with-string-trim-bounds (unsafe-chars string start end)
             (with-fast-array-references ((string string string))
               (loop with prev-dash
                     for idx fixnum upfrom start below end
                     for char = (aref string idx)
                     unless (or (unsafe-char-p char)
                                (if (eql char #\-) prev-dash (and prev-dash (setq prev-dash nil))))
		       collect char into result
                     else 
		       unless prev-dash
			 collect #\- into result
			 and do (setq prev-dash t)
                     finally (return (values (coerce result 'string) t))))))
          ((and (zerop start) (= end (length string))) string)
          (t (values (subseq string start end) t)))))

;;;------------------------------------------------------------------- 
;;;
;;; SSL UTILITIES
;;;

#+CL-HTTP-SSL
(defun ssl-implementation-type (&optional recache-p &aux cache)
  "Returns a string indicating the SSL library in use by CL-HTTP."
  (cond ((and (not recache-p) (setq cache *ssl-implementation-version*))
         (first cache))
        (t (multiple-value-bind (type version)
               (ssl-implementation recache-p)
             (declare (ignore version))
             type))))

#+CL-HTTP-SSL
(defun ssl-implementation-version (&optional recache-p &aux cache)
  "Returns a string indicating the version of the SSL library in use by CL-HTTP."
  (cond ((and (not recache-p) (setq cache *ssl-implementation-version*))
         (second cache))
        (t (multiple-value-bind (type version)
               (ssl-implementation recache-p)
             (declare (ignore type))
             version))))

;;;------------------------------------------------------------------- 
;;;
;;; Misc
;;;

; Needed to write types like (AND SYMBOL (SATISFIES SYMBOL-FBOUND-P)), which are not 'filtering' in ANSI CL as in CLtL2.

(defun symbol-fbound-p (symbol)
  (and (symbolp symbol) (fboundp symbol)))


