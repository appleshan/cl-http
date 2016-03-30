;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: xml-parser; -*-


#|
<DOCUMENTATION>
 <DESCRIPTION>
  <P>
  this implementes stream readers which perform decoding/encoding in the case
  of a binary stream. the correct decoder is selected by first examining the type of
  input element which the stream will offer and the, in the case of binary streams,
  by examining the initial byte sequence.
  no attempt is made to distinguish the encoder based on the stream's class.
  the detection methods return a reader function together with its argument, the
  detected decoding and, where it is necessary to read an initial byte sequence, that
  sequence as a string to be reread.
  </P>
  </DESCRIPTION>
 <COPYRIGHT YEAR='2001' AUTHOR='james adam anderson' MARK='(C)'
            href='file://xml/sysdcl.lisp' />
 <CHRONOLOGY>
  <DELTA DATE='20010328' AUTHOR='JAA'>
   adapted from version 0.4*</DELTA>
  <DELTA DATE='20010605' AUTHOR='MS'>
   lispworks conformance</DELTA>
  <DELTA DATE='20010621'>added a method to encode UTF-8 for character streams</DELTA>
  <DELTA DATE='20010907'>
   <ul><li><code>*xml-verbose*</code></li>
       <li>fixed incorrect decoding for re-read sequence by recognized UTF-1612/21 BOM</li></ul></DELTA>
  <DELTA DATE='20010914'>provisional us-ascii encoding method</DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
|#

(in-package "XML-PARSER")

#+(or ALLEGRO LispWorks)
(defMacro lsh (x y) `(ash ,x ,y))

#+(and allegro allegro-version>= (not (version>= 6 0)))
(progn
  ;; acl 5.0 versions

  (defMethod stream-reader ((stream excl:fundamental-character-input-stream))
    (let ((methods (compute-applicable-methods #'excl:stream-read-char (list stream))))
      (values (if (= (length methods) 1)
		  (clos:method-function (first methods))
		#'(lambda (arg)
		    (setf arg (excl:stream-read-char arg))
		    (unless (eq arg :eof) arg)))
	      stream)))

  (defMethod stream-reader ((stream excl:fundamental-binary-input-stream))
    (let ((methods (compute-applicable-methods #'excl:stream-read-byte (list stream))))
      (values (if (= (length methods) 1)
		  (clos:method-function (first methods))
		#'(lambda (arg)
		    (setf arg (excl:stream-read-byte arg))
		    (unless (eq arg :eof) arg)))
	      stream)))

  (defMethod stream-writer ((stream excl:fundamental-character-output-stream))
    (let ((methods (compute-applicable-methods #'excl:stream-write-char (list stream))))
      (values (if (= (length methods) 1)
		  (clos:method-function (first methods))
		#'excl:stream-write-char)
	      stream)))

  (defMethod stream-writer ((stream excl:fundamental-binary-output-stream))
    (let ((methods (compute-applicable-methods #'excl:stream-write-byte (list stream))))
      (values (if (= (length methods) 1)
		  (clos:method-function (first methods))
		#'excl:stream-write-byte)
	      stream)))
  ;; end acl5.0 versions
  )

#+(and allegro allegro-version>= (version>= 6 0))
(progn
  ;; allegro 6.0 versoins
  (defmethod stream-reader ((stream excl:fundamental-stream))
    (values #'(lambda (stream)
		(read-byte stream nil nil))
	    stream))
  (defmethod stream-writer ((stream excl:fundamental-stream))
    (values #'(lambda (stream)
		#'write-byte)
	    stream))
  )

#+LispWorks
(defMethod stream-reader ((stream stream:fundamental-stream))
  (values (if (subtypep (stream-element-type stream) 'character)
	      #'(lambda (stream)
		  (read-char stream nil nil))
	    #'(lambda (stream)
		(read-byte stream nil nil)))
          stream))
#+LispWorks
(defMethod stream-writer ((stream stream:fundamental-stream))
  (values (if (subtypep (stream-element-type stream) 'character)
	      #'(lambda (stream char)
		  (write-char char stream))
	    #'(lambda (stream byte)
		(write-byte byte stream)))
          stream))

;; adapted from cl-http:mcl;server;tcp-ot-stream for the strictly binary case
;; the first case comes into play without cl-http. connections are made with a simple tcp stream
;; the second case takes into account, that chunking decoding extends the generic function
#+MCL
(defMethod stream-reader ((stream ccl::binary-tcp-stream))
  (values #'ccl::io-buffer-read-byte (ccl::stream-io-buffer stream)))

#+(and MCL CL-HTTP)
(defmethod stream-reader ((stream ccl::modal-ascii-or-binary-tcp-stream-mixin))
  (values (case (ccl::input-mode stream)
            (:ascii #'stream-tyi)
            ((:binary nil)
             (let ((method-combination (ccl::generic-function-method-combination #'ccl::stream-read-byte))
                   (methods (compute-applicable-methods #'ccl::stream-read-byte (list stream))))
               (ccl::compute-effective-method-function #'ccl::stream-read-byte
                                                       method-combination
                                                       methods))))
          stream))

#+MCL
(defmethod stream-writer ((stream ccl::binary-tcp-stream))
  (flet ((io-buffer-tyo-byte (io-buffer byte)
           ;; ? not sure that continuability is a good idea, but
	   (unless (typep io-buffer 'ccl::io-buffer)
	     (setq io-buffer (require-type io-buffer 'ccl::io-buffer)))
           (if (characterp byte) (setf byte (char-code byte)))
	   (ccl::%io-buffer-write-byte io-buffer byte)))
    (values #'io-buffer-tyo-byte (ccl::stream-io-buffer stream))))


#+MCL
(defMethod stream-element-type ((stream ccl::binary-tcp-stream))
  ;; mcl 4.2 yielded BASE-CHARACTER for this class and produced a character reader ...?
  'unsigned-byte)

;; determine whether the stream supports binary operations

(defGeneric is-binary-stream (stream)
  #-allegro(:method ((stream stream)) (subtypep (stream-element-type stream) 'unsigned-byte))
  ;; allegro 6 "simple streams" can always do the byte operations
  #+(and allegro allegro-version>= (version>= 6 0))
  (:method ((stream stream)) t)
  #-(and allegro allegro-version>= (version>= 6 0))
  (:method ((stream stream)) (subtypep (stream-element-type stream) 'unsigned-byte)))

(defGeneric decoding-stream-reader (stream encoding)
  (:method ((stream stream) (encoding null))
           (decoding-stream-reader stream (stream-element-type stream)))
  (:method ((stream stream) (type cons))
           (decoding-stream-reader stream (first type)))
  (:method ((stream stream) (encoding string) &aux canonical-encoding)
           (cond ((setf canonical-encoding (canonical-encoding encoding))
                  (when *xml-verbose* 
                    (warn "assuming ~a encoding for stream: ~s." canonical-encoding stream))
                  (decoding-stream-reader stream canonical-encoding))
                 (t
                  (warn "no decoding defined for operation on stream: ~s: ~s." stream encoding)
                  (values nil nil encoding))))
  (:method ((stream stream) (encoding symbol) &aux canonical-encoding)
           (cond ((subtypep encoding 'integer)
                  (decoding-stream-reader stream :autodetect))
                 ((subtypep encoding 'character)
                  (decoding-stream-reader stream *default-character-encoding*))
                 ((setf canonical-encoding (canonical-encoding encoding))
                  (when *xml-verbose* 
                    (warn "assuming ~a encoding for stream: ~s." canonical-encoding stream))
                  (decoding-stream-reader stream canonical-encoding))
                 (t
                  (warn "no decoding defined for operation on stream: ~s: ~s." stream encoding)
                  (values nil nil encoding)))))

(defGeneric encoding-stream-writer (stream encoding)
  (:method ((stream t) (encoding t))
           (warn "no decoding defined for operation on stream: ~s: ~s." encoding stream)
           (values nil nil))
  (:method ((stream stream) (encoding null))
           (encoding-stream-writer stream (stream-element-type stream)))
  (:method ((stream stream) (type cons))
           (encoding-stream-writer stream (first type)))
  (:method ((stream stream) (encoding string) &aux canonical-encoding)
           (cond ((setf canonical-encoding (canonical-encoding encoding))
                  (when *xml-verbose* 
                    (warn "assuming ~a encoding for stream: ~s." canonical-encoding stream))
                  (encoding-stream-writer stream canonical-encoding))
                 (t
                  (warn "no decoding defined for operation on stream: ~s: ~s." stream encoding)
                  (values nil nil encoding))))
  (:method ((stream stream) (encoding symbol) &aux canonical-encoding)
           (cond ((subtypep encoding 'integer)
                  (encoding-stream-writer stream :autodetect))
                 ((subtypep encoding 'character)
                  (encoding-stream-writer stream *default-character-encoding*))
                 ((setf canonical-encoding (canonical-encoding encoding))
                  (when *xml-verbose* 
                    (warn "assuming ~a encoding for stream: ~s." canonical-encoding stream))
                  (encoding-stream-writer stream canonical-encoding))
                 (t
                  (warn "no encoding defined for operation on stream: ~s: ~s." stream encoding)
                  (values nil nil encoding)))))


(defMacro funcall-function.arg (function.arg &rest args)
  `(funcall (first ,function.arg) (rest ,function.arg) ,@args))

(defun change-stream-decoding (new-encoding)
  (etypecase new-encoding
    (keyword t)
    ((or string (and symbol (not null)))
     (setf new-encoding (intern (string-upcase new-encoding) "KEYWORD"))))
  (if (or (eq new-encoding *input-encoding*)
          (and (eq new-encoding :utf-16)
               (member *input-encoding* '(:utf-16-12 :utf-16-21))))
    (values *input-encoding* nil)
    (multiple-value-bind (function arg detected-encoding to-reread)
                         (decoding-stream-reader *input-source* new-encoding)
      (unless (and (eq detected-encoding new-encoding)
                   (null to-reread))
        (error "can't change encoding: ~s: ~s -> ~s = ~s/~s."
               *input-source*
               *input-encoding* new-encoding detected-encoding to-reread))
      (unless function (error "no reader for encoding: ~s." new-encoding))
      (setf *input-reader* function
            *input-reader-arg* arg
            *input-encoding* new-encoding)
      ;; return the new encoding
      (values new-encoding t))))

;;
;;
;; UTF-8 is computed

(defMethod decoding-stream-reader
           ((stream stream) (encoding (eql :UTF-8)))
  (unless (is-binary-stream stream)
    (warn "stream type not compatible with encoding: ~s: ~s."
          (stream-element-type stream) encoding))
  (multiple-value-bind (function arg)
                       (stream-reader stream)
    (values #'(lambda (function.arg &aux byte1)
                (block read-utf-8-datum
                  (flet ((read-byte-code (&aux byte)
                           (setf byte (funcall-function.arg function.arg))
                           (if (integerp byte) byte
                               (return-from read-utf-8-datum nil))))
                    (declare (type fixnum byte1)
                             (ftype (function () fixnum) read-byte-code)
                             (optimize (speed 3) (safety 0)))
                    (setf byte1 (read-byte-code))
                    (cond ((= 0 (logand #x80 byte1))
                           byte1)
                          ((= #xc0 (logand #xe0 byte1))
                           (logior (lsh (logand byte1 #x1f) 6)
                                   (logand (read-byte-code) #x3f)))
                          ((= #xe0 (logand #xf0 byte1))
                           (logior (logior (lsh (logand byte1 #x0f) 12)
                                           (lsh (logand (read-byte-code) #x3f) 6))
                                   (logand (read-byte-code) #x3f)))
                          ((= #xf0 (logand #xf8 byte1))
                           (let ((byte2 (read-byte-code))
                                 (byte3 (read-byte-code))
                                 (byte4 (read-byte-code)))
                             (xml-error "unsupported unicode datum: ~s."
                                            (list byte1 byte2 byte3 byte4))))
                          (t
                           (xml-error "illegal UTF-8 data: x~2,'0x." byte1))))))
            (cons function arg)
            :UTF-8)))


(defMethod encoding-stream-writer
           ((stream stream) (encoding (eql :UTF-8)))
  (if (is-binary-stream stream)
    (multiple-value-bind (function arg)
                         (stream-writer stream)
      (values #'(lambda (function.arg char &aux (code (char-code char)))
                  (cond ((<= code 255)
                         (funcall-function.arg function.arg code))
                        ((<= code #x03ff)
                         (funcall-function.arg function.arg (logior #b11000000 (lsh code -6)))
                         (funcall-function.arg function.arg (logior #b10000000 (logand code #b00111111))))
                        ((<= code #xffff)
                         (funcall-function.arg function.arg (logior #b11100000 (lsh code -12)))
                         (funcall-function.arg function.arg (logior #b10000000 (logand (lsh code -6) #b00111111)))
                         (funcall-function.arg function.arg (logior #b10000000 (logand code #b00111111))))
                        (t
                         (xml-error "unsupported unicode datum: ~s." code))))
              (cons function arg)))
    (multiple-value-bind (function arg)
                         (stream-writer stream)
      (values #'(lambda (function.arg char &aux (code (char-code char)))
                  (cond ((<= code 255)
                         (funcall-function.arg function.arg (code-char code)))
                        (t
                         (let ((string (format nil "&#~d" code)))
                           (dotimes (x (length string))
                             (funcall-function.arg function.arg (char string x)))))))
              (cons function arg)))))

;;
;;
;; ISO-8859-1 is direct

(defMethod decoding-stream-reader
           ((stream stream) (encoding (eql :ISO-8859-1)))
  (multiple-value-bind (function arg)
                       (stream-reader stream)
    (if (is-binary-stream stream)
      #-MCL
      (multiple-value-bind (reader arg)
                           (stream-reader stream)
        (values #'(lambda (function.arg &aux byte)
                    (setf byte (funcall-function.arg function.arg))
                    (when (integerp byte) byte))
                (cons reader arg)
                encoding))
      #+MCL
      (values function arg encoding)
      ;; character streams must be mapped back to binary
      (values #'(lambda (function.arg &aux char)
                  (setf char (funcall-function.arg function.arg))
                  (when (characterp char)
                    ;; hardcore tracing 
                    ;; (write-char char *trace-output*)
                    (char-code char)))
              (cons function arg)
              encoding))))

(defMethod decoding-stream-reader
           ((stream stream) (encoding (eql :US-ASCII)))
  (multiple-value-bind (function arg)
                       (stream-reader stream)
    (if (is-binary-stream stream)
      (values function arg encoding)
      ;; character streams must be mapped back to binary
      (values #'(lambda (function.arg &aux char)
                  (setf char (funcall-function.arg function.arg))
                  (when (characterp char)
                    (char-code char)))
              (cons function arg)
              encoding))))

#+MCL ;; fix eol
(defMethod encoding-stream-writer
           ((stream stream) (encoding (eql :ISO-8859-1)))
  (multiple-value-bind (function arg)
                       (stream-writer stream)
    (if (is-binary-stream stream)
      (values #'(lambda (function.arg char)
                  (funcall-function.arg function.arg 
                                        (if (= (char-code char) #x0a)
                                          #x0d
                                          (char-code char))))
              (cons function arg))
      (values #'(lambda (function.arg char)
                  (funcall-function.arg function.arg 
                                        (if (= (char-code char) #x0a)
                                          #.(code-char #x0d)
                                          char)))
              (cons function arg)))))

#-MCL
(defMethod encoding-stream-writer
           ((stream stream) (encoding (eql :ISO-8859-1)))
  (if (is-binary-stream stream)
    (multiple-value-bind (function arg)
                         (stream-writer stream)
      (values #'(lambda (function.arg char)
                  (funcall-function.arg function.arg (char-code char)))
              (cons function arg))
      (stream-writer stream))))

(defMethod encoding-stream-writer
           ((stream stream) (encoding (eql :US-ASCII)))
  (multiple-value-bind (function arg)
                       (stream-writer stream)
    #+MCL
    (if (is-binary-stream stream)
      (values #'(lambda (function.arg char)
                  (funcall-function.arg function.arg 
                                        (if (= (char-code char) #x0a)
                                          #x0d
                                          (char-code char))))
              (cons function arg))
      (values #'(lambda (function.arg char)
                  (funcall-function.arg function.arg 
                                        (if (= (char-code char) #x0a)
                                          #.(code-char #x0d)
                                          char)))
              (cons function arg)))
    #-MCL
    (if (is-binary-stream stream)
      (values #'(lambda (function.arg char)
                  (funcall-function.arg function.arg (char-code char)))
              (cons function arg))
      (values function arg))))

;;
;;
;; UTF-16 distinguishes encoding order:
;; UTF-16LE is UTF16 in "little-endian" byte order. (order mark is 0xFF 0xFE)
;; UTF-16BE is UTF16 in "big-endian" byte order. (order mark is 0xFE 0xFF)

(defMethod decoding-stream-reader
           ((stream stream) (encoding (eql :UTF-16)))
  (decoding-stream-reader stream *default-utf-16-encoding*))

(defMethod encoding-stream-writer
           ((stream stream) (encoding (eql :UTF-16)))
  (encoding-stream-writer stream *default-utf-16-encoding*))


(defMethod decoding-stream-reader
           ((stream stream) (encoding (eql :UTF-16-12)))
  (unless (is-binary-stream stream)
    (warn "stream type not compatible with encoding: ~s: ~s."
          (stream-element-type stream) encoding))
  (multiple-value-bind (function arg)
                       (stream-reader stream)
    (values #'(lambda (function.arg)
                (declare (ftype (function () fixnum) read-byte-code)
                         (optimize (speed 3) (safety 0)))
                (block read-utf-16-datum
                  (flet ((read-byte-code (&aux byte)
                           (setf byte (funcall-function.arg function.arg))
                           (if (integerp byte) byte
                               (return-from read-utf-16-datum nil))))
                    (+ (lsh (read-byte-code) 8) (read-byte-code)))))
            (cons function arg)
            encoding)))

(defMethod encoding-stream-writer
           ((stream stream) (encoding (eql :UTF-16-12)))
  (unless (is-binary-stream stream)
    (warn "stream type not compatible with encoding: ~s: ~s."
          (stream-element-type stream) encoding))
  (multiple-value-bind (function arg)
                       (stream-writer stream)
    (values #'(lambda (function.arg datum)
                (setf datum (char-code datum))
                (funcall-function.arg function.arg (logand #xff (lsh datum -8)))
                (funcall-function.arg function.arg (logand #xff datum)))
            (cons function arg))))


(defMethod decoding-stream-reader
           ((stream stream) (encoding (eql :UTF-16-21)))
  (unless (is-binary-stream stream)
    (warn "stream type not compatible with encoding: ~s: ~s."
          (stream-element-type stream) encoding))
  (multiple-value-bind (function arg)
                       (stream-reader stream)
    (values #'(lambda (function.arg)
                (declare (ftype (function () fixnum) read-byte-code)
                         (optimize (speed 3) (safety 0)))
                (block read-utf-16-datum
                  (flet ((read-byte-code (&aux byte)
                           (setf byte (funcall-function.arg function.arg))
                           (if (integerp byte) byte
                               (return-from read-utf-16-datum nil))))
                    (+ (read-byte-code) (lsh (read-byte-code) 8)))))
            (cons function arg)
            encoding)))

(defMethod encoding-stream-writer
           ((stream stream) (encoding (eql :UTF-16-21)))
  (unless (is-binary-stream stream)
    (warn "stream type not compatible with encoding: ~s: ~s."
          (stream-element-type stream) encoding))
  (multiple-value-bind (function arg)
                       (stream-writer stream)
    (values #'(lambda (function.arg datum)
                (setf datum (char-code datum))
                (funcall-function.arg function.arg (logand #xff datum))
                (funcall-function.arg function.arg (logand #xff (lsh datum -8))))
            (cons function arg))))



;;
;;
;; detect encoding by reading the initial content. works for binary streams only

(defMethod decoding-stream-reader
       ((stream stream) (encoding (eql :autodetect)) &aux byte0 byte1 to-reread)
  "see PR-xml Appendix F"
  (multiple-value-bind (reader arg) (stream-reader stream)
    (flet ((next-byte () (funcall reader arg)))
      (case (setf byte0 (next-byte))
        (#x00 (case (next-byte)
                (#x00 (case (next-byte)
                        (#x3c (if (= (next-byte) #x00)
                                (setf encoding :UCS-4-2143 to-reread "<")
                                (error "markup stream corrupt: ~s." stream)))
                        (#x00 (if (= (next-byte) #x3c)
                                (setf encoding :UCS-4-1234 to-reread "<")
                                (error "markup stream corrupt: ~s." stream)))
                        (#xFE (if (= (next-byte) #xFF)
                                (setf encoding :UCS-4-1234 to-reread nil)
                                (error "markup stream corrupt: ~s." stream)))
                        (#xFF (if (= (next-byte) #xFE)
                                (setf encoding :UCS-4-2143 to-reread nil)
                                (error "markup stream corrupt: ~s." stream)))
                        (t (error "markup stream corrupt: ~s." stream))))
                (#x3c (if (= (setf byte1 (next-byte)) #x00)
                        (case (setf byte1 (next-byte))
                          (#x00 (setf encoding :UCS-4-3412 to-reread "<"))
                          (#x3f
                           (when *xml-verbose*
                             (warn "assuming UTF-16-21 encoding for stream: ~s." stream))
                           (setf encoding :UTF-16-21 to-reread "<?"))
                          (t (|XML-ERROR-WFC: Byte Order Mark| :data (list #x00 #x3c #x00 byte1))))
                        (|XML-ERROR-WFC: Byte Order Mark| :data (list #x00 #x3c byte1))))
                (t
                 (error "markup stream corrupt: ~s." stream))))
        (#x3c (case (setf byte1 (next-byte))
                (#x00 (case (setf byte1 (next-byte))
                        (#x3f (case (setf byte1 (next-byte))
                                (#x00
                                 (when *xml-verbose*
                                   (warn "assuming UTF-16-12 encoding for stream: ~s." stream))
                                 (setf encoding :UTF-16-12 to-reread "<?"))
                                (t
                                 (|XML-ERROR-WFC: Byte Order Mark| :data (list #x3c #x00 #x3f byte1)))))
                        (#x00 (if (= (next-byte) #x00)
                                (setf encoding :UCS-4-4321 to-reread "<")
                                (|XML-ERROR-WFC: Byte Order Mark| :data (list #x3c #x00 #x00 byte1))))
                        (t
                         (|XML-ERROR-WFC: Byte Order Mark| :data (list #x3c #x00 byte1)))))
                (#x3f (setf encoding *default-binary-encoding* to-reread "<?"))
                (t
                 (unless (or (xml-initial-namechar? byte1) (= byte1 #.(char-code #\!)))
                   (when *xml-verbose* 
                     (warn "assuming UTF-8 encoding for stream: ~s." stream)))
                 (setf encoding *default-binary-encoding* to-reread (list byte0 byte1)))))
        (#xEF (case (setf byte1 (next-byte))
                (#xBB (if (= (setf byte1 (next-byte)) #xBF)
                        (setf encoding :UTF-8 to-reread nil)
                        (|XML-ERROR-WFC: Byte Order Mark| :data (list #xEF #xBB byte1))))
                (t
                 (|XML-ERROR-WFC: Byte Order Mark| :data (list #xEF byte1)))))
        (#xff
         (if (= (setf byte1 (next-byte)) #xfe)
           (let ((byte2 (next-byte)) (byte3 (next-byte)))
             (if (and (= byte2 0) (= byte3 0))
               (setf encoding :UCS4-4321 to-reread nil)
               (setf encoding :UTF-16-21 to-reread (list (+ byte2 (lsh byte3 8))))))
           (|XML-ERROR-WFC: Byte Order Mark| :data (list #xff byte1))))
        (#xfe
         (if (= (setf byte1 (next-byte)) #xff)
           (let ((byte2 (next-byte)) (byte3 (next-byte)))
             (if (and (= byte2 0) (= byte3 0))
               (setf encoding :UCS4-3412 to-reread nil)
               (setf encoding :UTF-16-12 to-reread (list (+ (lsh byte2 8) byte3)))))
           (|XML-ERROR-WFC: Byte Order Mark| :data (list #xfe byte1))))
        ((nil) ;; no data in stream
         (setf encoding :UTF-8 to-reread nil))
        (t
         (when *xml-verbose* 
           (warn "assuming UTF-8 encoding for stream: ~s." stream))
         (setf encoding :UTF-8 to-reread (list byte0))))
      (multiple-value-bind (reader arg encoding) (decoding-stream-reader stream encoding)
        (values reader arg encoding to-reread)))))
    


#|

(defun utf-8-decoding (byte1 &optional (byte2 0) (byte3 0) (byte4 0))
  (cond ((= 0 (logand #x80 byte1))
         byte1)
        ((= #xc0 (logand #xe0 byte1))
         (logior (lsh (logand byte1 #x1f) 6) (logand byte2 #x3f)))
        ((= #xe0 (logand #xf0 byte1))
         (logior (logior (lsh (logand byte1 #x0f) 12) (lsh (logand byte2 #x3f) 6)) (logand byte3 #x3f)))
        ((= #xf0 (logand #xf8 byte1))
         (xml-error "unsupported unicode datum: ~s."
                    (list byte1 byte2 byte3 byte4)))
        (t
         (xml-error "illegal UTF-8 data: ~2,'0x." byte1))))
(xml-namechar? (utf-8-decoding 194 183 0 0))
(xml-space? (utf-8-decoding 239 187 191))

(defun utf-8-decodingXstring (string)
  (handler-case
    (apply #'utf-8-decoding (map 'list #'char-code string))
    (error (condition) (format nil "~s(~{~2,'0x~}) -> ~a"
                               string (map 'list #'char-code string)
                               condition))))

(format nil "~{~8,'0x~^~%~}" (mapcar #'utf-8-decodingXstring
                                    '("퟿" "氏" "" "�" "�")))

from the 3.1 unicode report

                              Table 3.1.
                                                  UTF-8 Bit Distribution
      Scalar Value      UTF-16           1st Byte   2nd Byte   3rd Byte   4th Byte
    00000000 0xxxxxxx 00000000 0xxxxxxx  0xxxxxxx

    00000yyy yyxxxxxx 00000yyy yyxxxxxx  110yyyyy   10xxxxxx

    zzzzyyyy yyxxxxxx zzzzyyyy yyxxxxxx  1110zzzz   10yyyyyy   10xxxxxx
            
    000uuuuu zzzzyyyy 110110ww wwzzzzyy  11110uuu   10uuzzzz   10yyyyyy   10xxxxxx
    yyxxxxxx          110111yy yyxxxxxx 
                    
Where uuuuu = wwww + 1 (to account for addition of 1000016 as in Section 3.7, Surrogates). 

|#
:EOF
