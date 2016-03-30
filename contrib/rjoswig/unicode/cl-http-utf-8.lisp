;;; -*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-

;;; Copyright Rainer Joswig, 2009-2010, joswig@lisp.de

;;; Unicode support for CL-HTTP, currently with LispWorks

;;; Extends and modifies the CL-HTTP functionality found in http:server;utils.lisp
;;; Adds to the CL-HTTP functionality found in http:lw;server;tcp-stream.lisp

;; It adds UTF-8 support:
;;
;;  * escaping/unescaping URIs, including search URLs
;;  * escaping/unescaping form text
;;  * output of text encoded as UTF-8 to HTTP streams

;; Currently CL-HTTP encodes only a minimal set of characters for use in URIs.
;; It can be useful to be able to encode a larger character set,
;; for example in search URLs.
;; For this purpose this code adopts the UTF-8 encoding scheme for
;; URIs and in form fields.

;; See also:
;; http://en.wikipedia.org/wiki/Percent-encoding
;; http://en.wikipedia.org/wiki/UTF-16/UCS-2
;; http://en.wikipedia.org/wiki/UTF-8
;; http://en.wikipedia.org/wiki/Universal_Character_Set   (ISO 10646)
;; http://en.wikipedia.org/wiki/ISO/IEC_8859-1
;; http://www.unicode.org/faq/
;; http://tools.ietf.org/html/rfc3986 , 2.5
;;   When a new URI scheme defines a component that represents textual
;;   data consisting of characters from the Universal Character Set [UCS],
;;   the data should first be encoded as octets according to the UTF-8
;;   character encoding [STD63]; then only those octets that do not
;;   correspond to characters in the unreserved set should be percent-
;;   encoded.  For example, the character A would be represented as "A",
;;   the character LATIN CAPITAL LETTER A WITH GRAVE would be represented
;;   as "%C3%80", and the character KATAKANA LETTER A would be represented
;;   as "%E3%82%A2".
;;
;; Lisp Implementation Documentation
;;   CMUCL 20a: http://common-lisp.net/project/cmucl/doc/cmu-user/unicode.html
;;     One character type, one string type, CHAR-CODE-LIMIT is 65536, Strings are encoded in UTF-16 (!).
;;     This Unicode implementation looks ugly.
;;   LispWorks:  http://www.lispworks.com/documentation/lw51/LWUG/html/lwuser-402.htm#pgfId-885921
;;     Uses UCS-2 internally for representation of characters. Three character types, three string types.
;;   Allegro CL: http://www.franz.com/support/documentation/current/doc/iacl.htm
;;    Feature: :ics.


;; HTTP:*STANDARD-CHARACTER-TYPE*

;; This code has been tested with LispWorks 6 beta and LispWorks 5.

;; Naming: it is UTF-8, not UTF8.

;; To Do (?)
;;  porting to MCL and, possibly Scieneer CL
;;  GET of files with Unicode pathname

;; Remarks
;;
;;  HTTP:EXPORT-URL takes a keyword argument :CHARACTER-SET :UTF-8
;;  The function URL:CHARACTER-SET returns the character set with which the
;;   URL was exported.
;;  HTTP:WITH-SUCCESSFUL-RESPONSE takes a :CHARACTER-SET :UTF-8 argument.
;;   The server then sends UTF-8 as character set with the response headers.
;;   It is then not necessary to mention the character set in, say, the
;;   HTML reponse.


(in-package "HTTP")


;;; ================================================================
;;; Basic UTF-8 encoding

(in-package "WWW-UTILS")

;;; LispWorks has
;;;  EXTERNAL-FORMAT:DECODE-EXTERNAL-STRING
;;;  EXTERNAL-FORMAT:ENCODE-LISP-STRING

(defun utf-8-encode-unicode-character (character)
  (declare (type (or character fixnum) character)
           (optimize (speed 3) (safety 2) (debug 1)))
  "Returns a list of bytes encoding the character.
The list is between 1 and 4 elements long."
  (let ((code (if (numberp character) character (char-code character))))
    (cond ((< code     #x80)   (list code))
          ((< code    #x800)   (list (logior #b11000000 (ldb (byte 5  6) code))
                                     (logior #b10000000 (ldb (byte 6  0) code))))
          ((< code  #x10000)   (list (logior #b11100000 (ldb (byte 4 12) code))
                                     (logior #b10000000 (ldb (byte 6  6) code))
                                     (logior #b10000000 (ldb (byte 6  0) code))))
          ((< code #x200000)   (list (logior #b11110000 (ldb (byte 3 18) code))
                                     (logior #b10000000 (ldb (byte 6 12) code))
                                     (logior #b10000000 (ldb (byte 6  6) code))
                                     (logior #b10000000 (ldb (byte 6  0) code))))
          (t (error "Can't encode character code ~a" code)))))

(declaim (inline utf-8-encode-unicode-character-as-values))

(defun utf-8-encode-unicode-character-as-values (character)
  (declare (type (or character fixnum) character)
           (optimize (speed 3) (safety 2) (debug 1)))
  "Returns values of characters encoding the character. The first value
is the encoding length."
  (let ((code (if (numberp character) character (char-code character))))
    (declare (fixnum code))
    (cond ((< code     #x80)   (values 1
                                       (code-char code)))
          ((< code    #x800)   (values 2
                                       (code-char (logior #b11000000 (ldb (byte 5  6) code)))
                                       (code-char (logior #b10000000 (ldb (byte 6  0) code)))))
          ((< code  #x10000)   (values 3
                                       (code-char (logior #b11100000 (ldb (byte 4 12) code)))
                                       (code-char (logior #b10000000 (ldb (byte 6  6) code)))
                                       (code-char (logior #b10000000 (ldb (byte 6  0) code)))))
          ((< code #x200000)   (values 4
                                       (code-char (logior #b11110000 (ldb (byte 3 18) code)))
                                       (code-char (logior #b10000000 (ldb (byte 6 12) code)))
                                       (code-char (logior #b10000000 (ldb (byte 6  6) code)))
                                       (code-char (logior #b10000000 (ldb (byte 6  0) code)))))
          (t (error "Can't encode character code ~a" code)))))

(defun utf-8-encoding-length (character)
  "Returns the number of bytes needed to encode the character using UTF-8.
The character can be either a character object or the character code."
  (declare (type (or character fixnum) character)
           (optimize (speed 3) (safety 2) (debug 1)))
  (let ((code (if (numberp character) character (char-code character))))
    (declare (fixnum code))
    (cond ((< code     #x80)   1)
          ((< code    #x800)   2)
          ((< code  #x10000)   3)
          ((< code #x200000)   4)
          (t (error "Can't encode character code ~a" code)))))

(defun utf-8-encode-unicode-character-into-string (character string &optional (start 0))
  (declare (type (or character fixnum) character)
           (optimize (speed 3) (safety 2) (debug 1)))
  "Encodes the character into the string at position start.
Returns the string and the encoding length."
  (let* ((code (if (numberp character) character (char-code character)))
         (encoding-length (utf-8-encoding-length code)))
    (case encoding-length
      (1 (setf (aref string start)       (code-char code)))
      (2 (setf (aref string start)       (code-char (logior #b11000000 (ldb (byte 5  6) code)))
               (aref string (+ start 1)) (code-char (logior #b10000000 (ldb (byte 6  0) code)))))
      (3 (setf (aref string start)       (code-char (logior #b11100000 (ldb (byte 4 12) code)))
               (aref string (+ start 1)) (code-char (logior #b10000000 (ldb (byte 6  6) code)))
               (aref string (+ start 2)) (code-char (logior #b10000000 (ldb (byte 6  0) code)))))
      (4 (setf (aref string start)       (code-char (logior #b11110000 (ldb (byte 3 18) code)))
               (aref string (+ start 1)) (code-char (logior #b10000000 (ldb (byte 6 12) code)))
               (aref string (+ start 2)) (code-char (logior #b10000000 (ldb (byte 6  6) code)))
               (aref string (+ start 3)) (code-char (logior #b10000000 (ldb (byte 6  0) code)))))
      (t (error "Can't encode character code ~a" code)))
    (values string encoding-length)))

(defun utf-8-number-of-bytes (first-byte)
  "returns the length of the utf-8 code in number of bytes, based on the first byte.
The length can be a number between 1 and 4."
  (declare (fixnum first-byte))
  (cond ((=       0 (ldb (byte 1 7) first-byte)) 1)
        ((=   #b110 (ldb (byte 3 5) first-byte)) 2)
        ((=  #b1110 (ldb (byte 4 4) first-byte)) 3)
        ((= #b11110 (ldb (byte 5 3) first-byte)) 4)
        (t (error "unknown number of utf-8 bytes for ~a" first-byte))))

(defun utf-8-decode-unicode-character-code (sequence &key (position 0))
  "Decodes a sequence of byte values which decribes a character encode using UTF-8.
Returns the character code."
  (let* ((first-byte (elt sequence position))
         (number-of-bytes (utf-8-number-of-bytes first-byte)))
    (declare (fixnum first-byte number-of-bytes))
    (ecase number-of-bytes
      (1 (ldb (byte 7 0) first-byte))
      (2 (logior (ash (ldb (byte 5 0) first-byte) 6)
                 (ldb (byte 6 0) (elt sequence (+ position 1)))))
      (3 (logior (ash (ldb (byte 5 0) first-byte) 12)
                 (ash (ldb (byte 6 0) (elt sequence (+ position 1))) 6)
                 (ldb (byte 6 0) (elt sequence (+ position 2)))))
      (4 (logior (ash (ldb (byte 3 0) first-byte) 18)
                 (ash (ldb (byte 6 0) (elt sequence (+ position 1))) 12)
                 (ash (ldb (byte 6 0) (elt sequence (+ position 2))) 6)
                 (ldb (byte 6 0) (elt sequence (+ position 3)))))
      (t (error "wrong UTF-8 encoding for position ~a of sequence ~s" position sequence)))))

(defun escape-character-as-utf-8 (char &optional (result-type 'cons))
  "Returns a list (default) or string of characters that encode
CHAR in UTF-8. It uses 'percent' encoding.
Result type is either CONS or STRING."
  (declare (type (or character fixnum) char))
  (let* ((utf-8-bytes (utf-8-encode-unicode-character char))
         (characters (loop for byte in utf-8-bytes
                           collect http::*escape-character*
                           collect (digit-char (ldb (byte 4 4) byte) 16)
                           collect (digit-char (ldb (byte 4 0) byte) 16))))
    (ecase result-type
      (cons characters)
      (string (coerce characters 'string)))))

(declaim (inline maybe-unicode-code-char))

(defvar *unicode-replacement-character* #\?)

(defun maybe-unicode-code-char (code &optional (replacement-character *unicode-replacement-character*))
  "Returns the character for any code below char-code-limit.
Otherwise the replacement character will be returned."
  (if (< code char-code-limit)
      (code-char code)
    replacement-character))

(defun utf-8-encode-string (string &optional (start 0) (end (length string)))
  "Returns a new string with the encoded characters between start and end."
  (let ((new-length (loop for pos from start below end sum (utf-8-encoding-length (aref string pos))))
        (old-length (- end start)))
    (if (= new-length old-length)
        (if (and (= start 0) (= end (length string)))
            (values string old-length)
          (let ((new-string (subseq string start end)))
            (values new-string (length new-string))))
      (let ((new-string (make-string new-length :element-type 'character)))
        (loop with pos-new = 0
              for pos0 from start below end
              for char = (aref string pos0)
              do (multiple-value-bind (string encoding-length)
                     (utf-8-encode-unicode-character-into-string char new-string pos-new)
                   (declare (ignore string))
                   (incf pos-new encoding-length)))
        (values new-string (length new-string))))))


;;;; ================================================================
;;;; new definitions for CL-HTTP functions

;;; Escaped Characters are percent encoded
;;; %C3%A4  is  umlaut a

(in-package "HTTP")

(define escaped-character-p (char)
  "Returns non-null if CHAR is a special character that must be escaped.
CHAR is compared with CHAR=.
If the Lisp implementation supports the :UNICODE feature, then
it returns true for all characters with a character code of 256 or higher.
Characters with character codes between 128 and 255 also need to be encoded."
  (let ((code (char-code char)))
    (or (and (< code #.(length *escape-characters-bits*))
             (eql 1 (sbit (the simple-bit-vector *escape-characters-bits*) code)))
        ; 8 bit characters
        (<= 128 code 255)
        #+:unicode
        (> code 255))))

(define write-escaped-char (char &optional (stream *standard-output*))
  "Writes the UTF-8 escape code for char on STREAM.
UTF-8 is 'percent' encoded."
  (loop for char in (www-utils::escape-character-as-utf-8 char)
        do (write-char char stream)))

(define unescape-character (string &optional (start 0) end (code-p nil))
  "Returns the character whose escaped representation appears in STRING from START to END.
The start is the first character after the (first) escape character.
It unescapes 'percent encoded' UTF-8 characters.
It returns the character (or the code if code-p is non-null) and
the length of the unescaped substring."
  (declare (fixnum start))
  (flet ((byte-at-pos (pos)
           (parse-integer string :radix 16 :start pos :end (+ pos 2))))
    (let* ((first-byte (byte-at-pos start))
           (number-of-bytes (www-utils::utf-8-number-of-bytes first-byte)))
      (setf end (+ start (case number-of-bytes
                           (1 2) (2 5) (3 8) (4 11))))
      (multiple-value-bind (code length)
          (with-bad-escaping-resignalled (string :start start :end end :error-class error)
            (ecase number-of-bytes
              (1 (values (ldb (byte 7 0) first-byte)
                         2))
              (2 (values (logior (ash (ldb (byte 5 0) first-byte) 6)
                                 (ldb (byte 6 0) (byte-at-pos (+ start 3))))
                         5))
              (3 (values (logior (ash (ldb (byte 5 0) first-byte) 12)
                                 (ash (ldb (byte 6 0) (byte-at-pos (+ start 3))) 6)
                                 (ldb (byte 6 0) (byte-at-pos (+ start 6))))
                         8))
              (4 (values (logior (ash (ldb (byte 3 0) first-byte) 18)
                                 (ash (ldb (byte 6 0) (byte-at-pos (+ start 3))) 12)
                                 (ash (ldb (byte 6 0) (byte-at-pos (+ start 6))) 6)
                                 (ldb (byte 6 0) (byte-at-pos (+ start 9))))
                         11))
              (t (error "wrong UTF-8 encoding for position ~a of string ~s" start string))))
        (values (if code-p code (www-utils::maybe-unicode-code-char code)) length)))))

(defun unescape-character-from-stream (stream &optional (code-p nil))
  "Returns the character whose escaped representation appears in STREAM.
If code-p is T the code of the character is return instead.
The second value is the number of characters read from the stream.
The first escape character should have been read already."
  (flet ((read-byte-at-pos (&optional (read-escape-character-p nil))
           (when read-escape-character-p (read-char stream)) ; could do some checking here
           (+ (ash (digit-char-p (read-char stream) 16) 4)
              (digit-char-p (read-char stream) 16))))
    (let* ((first-byte (read-byte-at-pos))
           (number-of-bytes (www-utils::utf-8-number-of-bytes first-byte)))
      (multiple-value-bind (code length)
          (ecase number-of-bytes
            (1 (values (ldb (byte 7 0) first-byte)
                       2))
            (2 (values (logior (ash (ldb (byte 5 0) first-byte) 6)
                               (ldb (byte 6 0) (read-byte-at-pos t)))
                       5))
            (3 (values (logior (ash (ldb (byte 5 0) first-byte) 12)
                               (ash (ldb (byte 6 0) (read-byte-at-pos t)) 6)
                               (ldb (byte 6 0) (read-byte-at-pos t)))
                       8))
            (4 (values (logior (ash (ldb (byte 3 0) first-byte) 18)
                               (ash (ldb (byte 6 0) (read-byte-at-pos t)) 12)
                               (ash (ldb (byte 6 0) (read-byte-at-pos t)) 6)
                               (ldb (byte 6 0) (read-byte-at-pos t)))
                       11))
            (t (error "wrong UTF-8 encoding for stream ~a" stream)))
        (values (if code-p code (www-utils::maybe-unicode-code-char code)) length)))))

(defmethod read-translated-char (stream delimiter &optional (eof-errorp t) eof-value)
  (flet ((decode-escaped-char (stream)
           (multiple-value-bind (code length) (unescape-character-from-stream stream t)
             (values (case code
                       (13 #\newline)
                       (10 #\linefeed)
                       (t (www-utils::maybe-unicode-code-char code)))
                     (1+ length)))))
    (declare (inline decode-escaped-char))
    (let ((char (read-char stream eof-errorp eof-value)))
      (cond ((characterp char)
             (cond ((eql char delimiter)        ;delimiter return nil
                    (values nil 1))
                   ((eql char #\+)
                    (values #\space 1))
                   ((eql char *escape-character*)       ;escape character
                    (handler-case-if eof-errorp
                        (decode-escaped-char stream)
                      (end-of-file () (values eof-value 1))))
                   (t (values char 1))))
            ((equal char eof-value)
             (values char 0))
            (t (error "Bad value, ~S, returned by READ-CHAR." char))))))

(define unescaping-special-chars (in-stream out-stream &optional only-safe-characters-p)
  (flet ((escape-char (char out-stream)
           (write-escaped-char char out-stream))
         (unescaped-char (in-stream)
           (unescape-character-from-stream in-stream)))
    (declare (inline unescaped-char escape-char))
    (loop for char = (read-char in-stream nil)
          while char
          when (escape-character-p char)
          do (let ((nchar (unescaped-char in-stream)))
               (if (and only-safe-characters-p (uri-reserved-or-unsafe-char-p nchar))
                   (escape-char nchar out-stream)
                 (write-char nchar out-stream)))
          else do (write-char char out-stream))))

(define string-escape-special-chars (string &optional (start 0) (end (length string) end-supplied-p)
                                            (escape-fragments-p t))
  "When any special characters are present, this returns a string with these characters escaped.
A new string is consed only when escape characters are present.
ESCAPE-FRAGMENTS-P controls whether URL fragment syntax is also escaped."
  (declare (values escaped-string chars-escaped-p)
           (fixnum start end))
  (flet ((count-escape-chars (string start end)
           (with-fast-array-references ((string string string))
             (loop for idx upfrom start below end
                   for char = (aref string idx)
                   when (escaped-character-p char)
                   sum (* (www-utils::utf-8-encoding-length char) 3) into sum and count t into count
                   finally (return (- sum count)))))
         (count-escape-chars-upto-fragment (string start end)
           (with-fast-array-references ((string string string))
             (loop for idx upfrom start below end
                   for char = (aref string idx)
                   when (fragment-character-p char)
                   do (return (values (- sum count) idx))
                   when (escaped-character-p char)
                   sum (* (www-utils::utf-8-encoding-length char) 3) into sum and count t into count
                   finally (return (- sum count)))))
         (make-escaped-string (string start end count length)
           (declare (fixnum count))
           (let* ((len2 (+ (the fixnum length) count))
                  (nstring (make-string len2)))
             (with-fast-array-references ((string string string)
                                          (nstring nstring string))
               (loop with nidx = 0
                     for idx upfrom start below end
                     for char = (aref string idx)
                     when (escaped-character-p char)
                     do (let ((digit-chars (www-utils::escape-character-as-utf-8 char 'cons)))
                          (declare (dynamic-extent digit-chars))
                          (dolist (ch digit-chars)
                            (setf (aref nstring nidx) ch)
                            (incf nidx)))
                     else do (progn (setf (aref nstring nidx) char)
                               (incf nidx))
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
          and new-char and encoding-length
          while (< idx end)
          for char = (aref string idx)
          when (escape-character-p char)
            do (multiple-value-setq (new-char encoding-length)
                   (unescape-character string (1+ idx)))
               (cond  ;; Skip unescaping a char, just incf idx to skip the hex pair.
                 ((and only-safe-characters-p (uri-reserved-or-unsafe-char-p new-char))
                  (incf idx (1+ encoding-length)))
                 ;; Escape a char, we have already started a new string.
                 (new-string 
                  (let ((new-idx2 (+ new-idx (- idx last-idx))))
                    (setf new-string (replace new-string string
                                              :start1 new-idx :end1 new-idx2 :start2 last-idx :end2 idx)
                          (aref new-string new-idx2) new-char
                          new-idx (1+ (the fixnum new-idx2))
                          last-idx (incf idx (1+ encoding-length)))))
                 ;; Escape a char, need to start a new string.
                 (t (setf new-idx (- idx start)
                          new-string (replace (make-array (- end start 2)
                                                          :fill-pointer t :element-type 'character)
                                              string :start1 0 :end1 new-idx :start2 start :end2 idx)
                          (aref new-string new-idx) new-char
                          last-idx (incf idx (1+ encoding-length)))
                    (incf new-idx)
                    ; (inspect (list :new new-string idx new-idx last-idx encoding-length))
                    ))
          else
            do (incf idx)
          finally (return (cond ;; We've started a new string, now finish up
                            (new-string 
                             (let ((new-end (+ (the fixnum new-idx) (- end last-idx))))
                               (setf new-string (replace new-string string :start1 new-idx :end1
                                                         new-end :start2 last-idx :end2 end)
                                     (fill-pointer new-string) new-end))
                             (values new-string t t))
                            ;; No escaping was performed
                            ((and (zerop start) (or (not end-supplied-p) (= end (length string))))
                             (values string nil nil))
                            ;; Trim original as necessary
                            (t (values (subseq string start end) nil t)))))))

(define nstring-unescape-special-chars (string &optional (start 0) (end (length string))
					       only-safe-characters-p (pad-char #\space)
                                               set-fill-pointer-p)
  "When any escaped characters are present, this returns a string with these characters unescaped.
STRING is destructively modified when escaped characters are present.
ONLY-SAFE-CHARACTERS-P set to non-nill skips reserved or unsafe URL characters.
When SET-FILL-POINTER-P is non-null, the fill-pointer on STRING is moved backwards
by two times the number of characters unescaped. Otherwise, that number of characters
are padded on the right using PAD-CHAR.

If Unicode characters are unescaped, the string must have the correct element type."
  (declare (values unescaped-string new-end chars-unescaped-p)
           (fixnum start end))
  (with-fast-array-references ((string string string))
    (loop with read-idx fixnum = start
	  with write-idx fixnum = start
	  and new-char and chars-unescaped-p and encoding-length
          while (< read-idx end)
          for char = (aref string read-idx)
	  do (incf read-idx)
          when (escape-character-p char)
          do (multiple-value-setq (new-char encoding-length) (unescape-character string read-idx))
          (cond  ;; Skip unescaping a char, just incf idx to skip the hex pair.
                 ((and only-safe-characters-p (uri-reserved-or-unsafe-char-p new-char))
		  (cond (chars-unescaped-p
			 (setf (aref string write-idx) char)
                         (loop for read-pos from read-idx
                               for write-pos from (1+ write-idx)
                               repeat encoding-length
			       do (setf (aref string write-pos) (aref string read-pos)))
			 (incf write-idx encoding-length)
			 (incf read-idx encoding-length))
			(t (incf write-idx (1+ encoding-length))
			   (incf read-idx encoding-length))))
                 ;; Escape a char, we have already started a new string.
                 ((or chars-unescaped-p (setq chars-unescaped-p t))
		  (setf (aref string write-idx) new-char)
		  (incf write-idx)
		  (incf read-idx encoding-length)))
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

;;; ================================================================
;;; Forms

; form buffers now are of element type character, not base-char
(defun make-post-form-buffer (resource &optional (size *post-form-buffer-size*))
  (declare (ignore resource))
  (make-array size :element-type 'character :adjustable t :fill-pointer 0))

; unchanged, here for updating the definition
(defresource post-form-buffer (size)
  :constructor   make-post-form-buffer
  :deinitializer deinitialize-post-form-buffer
  :matcher       match-post-form-buffer-p)


;; form text now can also contain unicode characters

(define nstring-translate-chars (string &optional (start 0) (end (length string))
                                        (pad-char #\space) set-fill-pointer-p)
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
                 (multiple-value-bind (code length) (unescape-character string start end t)
                   (values (case code
                             (13 #\return)
                             (10 #\linefeed)
                             (t (www-utils::maybe-unicode-code-char code)))
                           (1+ length)))
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
		    (let ((escp-end (+ read-idx 2))
                          (nchars 0))
		      ;; only translate when within bounds and hex code is good.
		      (cond ((and (<= escp-end end)
				  (multiple-value-setq (new-char nchars)
                                      (decode-escaped-char string read-idx escp-end)))
			     (incf read-idx (1- nchars))
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

;;; ================================================================

(in-package "URL")

; this should also escape the string returned by CALL-NEXT-METHOD
;  since the other methods don't deal with search URLs, the escaping
;  for search URLs need to be handled here.
(defmethod coerce-url-string ((url search-mixin) &optional (escape-search-url-p *escape-search-urls*)
			      (relativize *relativize-urls*))
  (flet ((get-relative-url-based-on-context (context url)
           (if (local-context-match-p context url)
               (relative-name-string-with-unescaped-search-suffix url)
	       (name-string-with-unescaped-search-suffix url))))
    (declare (inline get-relative-url-based-on-context))
    (cond (escape-search-url-p
	   (ensure-escaped-search-url (call-next-method)))     ; ESCAPE the string!
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



;;; ================================================================
;;; Output, UTF-8 encoding of output to CL-HTTP-CHUNK-TRANSFER-SOCKET-STREAM

;;; LispWorks version

;;; http:lw;server;tcp-stream.lisp

#+lispworks
(in-package "IPC")

#+lispworks
(defclass character-encoding-stream-mixin ()
  ((output-encoding :initform nil :accessor stream-character-output-encoding)
   ; (input-decoding  :initform nil :accessor stream-character-input-decoding) ; not used yet
   ))

#+lispworks
(defmethod stream:stream-write-char :around ((stream character-encoding-stream-mixin) character)
  (declare (type (or character fixnum) character)
           (optimize (speed 3) (safety 2) (debug 1)))
  (case (stream-character-output-encoding stream)
    (:utf-8 (let ((code (if (numberp character) character (char-code character))))
              (declare (fixnum code))
              (cond ((< code     #x80)
                     (call-next-method stream (code-char code)))
                    ((< code    #x800)
                     (call-next-method stream (code-char (logior #b11000000 (ldb (byte 5  6) code))))
                     (call-next-method stream (code-char (logior #b10000000 (ldb (byte 6  0) code)))))
                    ((< code  #x10000)
                     (call-next-method stream (code-char (logior #b11100000 (ldb (byte 4 12) code))))
                     (call-next-method stream (code-char (logior #b10000000 (ldb (byte 6  6) code))))
                     (call-next-method stream (code-char (logior #b10000000 (ldb (byte 6  0) code)))))
                    ((< code #x200000)
                     (call-next-method stream (code-char (logior #b11110000 (ldb (byte 3 18) code))))
                     (call-next-method stream (code-char (logior #b10000000 (ldb (byte 6 12) code))))
                     (call-next-method stream (code-char (logior #b10000000 (ldb (byte 6  6) code))))
                     (call-next-method stream (code-char (logior #b10000000 (ldb (byte 6  0) code)))))
                    (t (error "Can't UTF-8 encode character code ~a on stream ~a" code stream)))))
    (otherwise (call-next-method))))

#+lispworks
(defun %buffered-stream-write-utf-8-string (stream string start end)
  "we are writing a string (or a part of a string) directly into the buffer of the stream.
characters are encoded as UTF-8. This routine better be fast."
  (declare (type string string)
           (type stream:buffered-stream stream)
           (fixnum start end)
           (optimize (speed 3) (safety 2) (debug 1)))
  (unless (>= start end)
    (let ((nc 0) (ic 0) c0 c1 c2 c3
          (string-index start))
      (declare (fixnum nc ic string-index)
               (type (or character null) c0 c1 c2 c3))
      (macrolet ((get-char-n (n)
                   `(case ,n
                      (0 c0)
                      (1 c1)
                      (2 c2)
                      (3 c3))))
        (flet ((get-next-encoded-char ()
                 (cond (; end of string and no more characters
                        (and (>= string-index end) (>= ic nc))
                        nil)
                       ; encode new character and get first encoded character
                       ((>= ic nc)
                        (setf ic 0)
                        (let ((char (schar string string-index)))
                          (incf string-index)
                          (case char
                            ; CR or LF is written as CR and LF
                            ((#\return #\linefeed)
                             (setf c0 #\Linefeed
                                   nc 1)
                             #\Return)
                            ; otherwise encode other characters
                            (t (multiple-value-setq (nc c0 c1 c2 c3)
                                   (www-utils::utf-8-encode-unicode-character-as-values char))
                               (setf ic 1)
                               c0))))
                       ; get next encoded character
                       (t (prog1 (get-char-n ic) (incf ic))))))
          (let ((char nil))
            (loop (loop (stream:with-stream-output-buffer (output-buffer output-index output-limit) stream
                          (when (>= output-index output-limit)
                            ; buffer is full
                            (return))
                          (setf char (get-next-encoded-char))
                          (unless char
                            ; no more characters
                            (return-from %buffered-stream-write-utf-8-string))
                          (setf (schar output-buffer output-index) char)
                          (incf output-index)))
                  ; buffer is full, flush it
                  (stream:stream-flush-buffer stream))))))))
                    
#+lispworks
(defmethod stream:stream-write-string :around ((stream character-encoding-stream-mixin) 
                                               string
                                               &optional (start 0) (end (length string)))
  (case (stream-character-output-encoding stream)
    (:utf-8 (%buffered-stream-write-utf-8-string stream string start end))
    (otherwise (call-next-method))))

#+lispworks
(defclass cl-http-chunk-transfer-socket-stream 
          (character-encoding-stream-mixin 
           chunk-transfer-encoding-output-stream
           chunk-transfer-decoding-input-stream
           extended-socket-stream)
  ())

(in-package "HTTP")

;; It should be possible to generate output on a stream that does not support encoding.
;; what should happen then? Should it be possible to encode output to a terminal or
;; file stream? It might be useful to write encoded text to static files.
;; OTOH, if one wants to generate UTF-8 encoded files, then one can use the
;; usual platform streams that do encoding.

#+lispworks
(defmacro with-character-output-encoding ((stream &optional (character-set :utf-8)) &body body)
  "Output to the CL-HTTP HTTP stream will be encoded during execution of the body.
Currently only :UTF-8 is supported and is the default."
  (let ((fn (gensym)))
    `(flet ((,fn ()
              ,@body))
       (declare (dynamic-extent (function ,fn)))
       (if (and (member ,character-set '(:utf-8))
                (typep ,stream 'ipc::character-encoding-stream-mixin))
           (unwind-protect (progn
                             (setf (ipc::stream-character-output-encoding ,stream) ,character-set)
                             (,fn))
             (setf (ipc::stream-character-output-encoding ,stream) nil))
         (,fn)))))


#+lispworks
(export 'http::with-character-output-encoding)


;;; ================================================================
;;; Directory Listings

;;  http:server;server.lisp

;; directory listings are encoded in UTF-8 when the directory is exported with
;;  (http:export-url  ... :CHARACTER-SET :UTF-8)

(in-package "HTTP")

(defun write-directory-listing-acceptable-to-client (url stream inclusion-predicate path-url-intern-function directories-p)
  (multiple-value-bind (writer data-type)
      (get-directory-writer-acceptable-to-client *server* url)
    (with-conditional-get-response (stream data-type
                                           :last-modification (file-modification-date (url::cached-pathname url))
					   :expires           (url:expiration-universal-time url) :content-location url
					   :cache-control     (url:response-cache-control-directives url)
                                           :character-set     (url:character-set url)
					   :content-language  (languages url))
      (with-character-output-encoding (stream (url:character-set url))
        (funcall writer url stream inclusion-predicate path-url-intern-function directories-p)))))

;;; ================================================================
;;; End of File

