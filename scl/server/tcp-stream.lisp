;;;
;;; The Scieneer Common Lisp TCP/IP streams for CL-HTTP.
;;;
;;; Scieneer Common Lisp port:
;;; (C) Copyright 2006, Scieneer Pty Ltd
;;;     All Rights Reserved.
;;;

(in-package :www-utils)

(use-package :alien)
(use-package :c-call)

(pushnew :cl-http-ssl *features*)

;;;;
;;; Conditions

(define-condition http-stream-error (stream-error)
  ())

(define-condition network-error (error)
  ())

(define-condition domain-error (network-error)
  ())

(define-condition unknown-host-name (network-error)
  ((hostname :reader unknown-host-name-hostname 
	     :initarg :hostname))
  (:report (lambda (condition stream)
             (format stream "Unknown host name ~A"
                     (unknown-host-name-hostname condition)))))

(define-condition unknown-address (network-error)
  ((address :reader unknown-address-address :initarg :address))
  (:report (lambda (condition stream)
             (let ((address (unknown-address-address condition)))
               (format stream "Unknown address ~A"
                       (ext:ip-address-string address))))))

(define-condition domain-resolver-error (network-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "The domain server has returned an error"))))

(define-condition remote-network-error (network-error) ())

(define-condition bad-connection-state (remote-network-error) ())

(define-condition connection-closed (bad-connection-state) ())

(define-condition connection-lost (bad-connection-state) ())

(define-condition host-stopped-responding (bad-connection-state) ())

(define-condition connection-error (remote-network-error) ())

(define-condition ssl-connection-error (connection-error) ())

(define-condition connection-refused (connection-error) ())

(define-condition host-not-responding (remote-network-error) ())

(define-condition protocol-timeout (remote-network-error) ())

(defmacro add-default-conditions (&rest types)
  `(progn
     ,@(mapcar
	#'(lambda (sname)
	    `(deftype ,(intern sname) () 'error))
	types)))

(add-default-conditions
 "LOCAL-NETWORK-ERROR"
 "NETWORK-PARSE-ERROR"
 "NETWORK-RESOURCES-EXHAUSTED"
 )

;;;;
;;; ---------------------------------------------------------------------------
;;;; The HTTP-STREAM structure.

(eval-when (compile load eval)
(defclass http-stream (ext:encapsulating-stream ext:character-output-stream ext:character-input-stream)
  ())
) ; eval-when

(ext:defgvar *http-streams* nil)

(defun make-http-stream (binary-stream)
  "Returns a character stream which performs its operations on the
  'binary-stream. The stream input is unbuffered, but out is buffered
  and may need flushing when done."
  (let ((stream (kernel:atomic-pop *http-streams*)))
    (cond (stream
	   (setf (slot-value stream 'stream) binary-stream)
	   (setf (ext:stream-open-p stream) t)
	   stream)
	  (t
	   (make-instance 'http-stream
			  :in-buffer ""
			  :out-buffer (lisp::make-stream-buffer 'base-char)
			  :stream binary-stream)))))

(defun free-http-stream (stream &optional free-binary-stream-p)
  (declare (type http-stream stream))
  (when free-binary-stream-p
    (let ((binary-stream (slot-value stream 'stream)))
      (when (typep binary-stream 'scl-http::http-stream)
	(scl-http::free-http-stream binary-stream))))
  (setf (slot-value stream 'stream) nil)
  (setf (ext:stream-out-tail stream) 0)
  (kernel:atomic-push stream *http-streams*)
  nil)

;;; Don't free the stream buffers because the http-streams are returned to a
;;; pool.
(defmethod close ((stream http-stream) &key ((:abort abort) nil))
  (when (ext:stream-open-p stream)
    (unless abort
      (ext:flush-output stream))
    #+nil (lisp::free-stream-buffers stream)
    (setf (ext:stream-open-p stream) nil)
    (let ((binary-stream (slot-value stream 'stream)))
      (assert (not (typep binary-stream 'scl-http::chunk-encoding-stream)))
      (close binary-stream :abort abort))
    t))

(defmethod stream-element-type ((stream http-stream))
  'base-char)

(defmethod ext:stream-line-length ((stream http-stream))
  nil)

(defmethod ext:stream-line-column ((stream http-stream))
  nil)

(defmethod ext:stream-read-char ((stream http-stream))
  (declare (inline read-byte))
  (let* ((source (slot-value stream 'stream))
	 (char (read-byte source nil :eof)))
    (if (eq char :eof)
	:eof
	(code-char char))))

(defmethod ext:stream-read-char-no-hang ((stream http-stream))
  (declare (inline ext:read-byte-no-hang))
  (let* ((source (slot-value stream 'stream))
	 (char (ext:read-byte-no-hang source nil :eof)))
    (cond ((eq char :eof)
	   :eof)
	  (char
	   (code-char char)))))

(defmethod ext:stream-unread-char ((stream http-stream) character)
  (let ((source (slot-value stream 'stream)))
    (declare (inline ext:unread-byte))
    (ext:unread-byte (char-code character) source)))

(defmethod ext:stream-peek-char ((stream http-stream))
  (declare (inline ext:peek-byte))
  (let* ((source (slot-value stream 'stream))
	 (char (ext:peek-byte source nil nil :eof)))
    (if (eq char :eof)
	:eof
	(code-char char))))

(defmethod ext:stream-write-char ((stream http-stream) character)
  (declare (inline write-byte))
  (let ((target (slot-value stream 'stream)))
    (write-byte (char-code character) target))
  character)

(defmethod ext:stream-write-chars ((stream http-stream) string start end waitp)
  (declare (type simple-string string)
	   (type fixnum start end)
	   (inline write-byte)
	   (ignore waitp)
	   (optimize (speed 3) (debug 0)))
  (let* ((target (slot-value stream 'stream))
	 (buffer (ext:stream-out-buffer target))
	 (length (length buffer)))
    (declare (type (simple-array (unsigned-byte 8) (*)) buffer))
    (cond ((> length 0)
	   (let ((out-tail (ext:stream-out-tail target)))
	     (declare (type fixnum out-tail)
		      (optimize (safety 0)))
	     (flet ((%write-byte (byte)
		      (when (>= out-tail length)
			(ext:stream-write-bytes target buffer 0 length t)
			(setf (ext:stream-out-tail target) 0)
			(setf out-tail 0))
		      (setf (aref buffer out-tail) byte)
		      (incf out-tail)))
	       (do ((i start (1+ i)))
		   ((>= i end))
		 (declare (fixnum i))
		 (%write-byte (char-code (schar string i)))))
	     (setf (ext:stream-out-tail target) out-tail)))
	  (t
	   (do ((i start (1+ i)))
	       ((>= i end))
	     (declare (fixnum i))
	     (write-byte (char-code (schar string i)) target)))))
  (- end start))

(defmethod ext:stream-force-output ((stream http-stream))
 (let ((source (slot-value stream 'stream)))
   (force-output source)))

(defmethod ext:stream-finish-output ((stream http-stream))
 (let ((source (slot-value stream 'stream)))
   (finish-output source)))



;;;------------------------------------------------------------------- 
;;;
;;; STREAM HACKING
;;;

(define-macro with-binary-stream ((stream direction) &body body)
  "Turns STREAM into a binary stream within the scope of BODY.
  direction can be :OUTPUT, :INPUT, or :BOTH."
  (declare (ignore direction))
  `(let ((,stream (cond ((typep ,stream 'ext:character-stream)
			 (ext:flush-output ,stream)
			 (slot-value ,stream 'stream))
			(t
			 ;; Assume binary.
			 ,stream))))
    ,@body))

(define-macro with-text-stream ((stream direction) &body body)
  "Turns STREAM into a text stream within the scope of BODY.
  direction can be :OUTPUT, :INPUT, or :BOTH."
  (declare (ignore direction))
  `(flet ((body (,stream)
	    ,@body))
     (cond ((typep ,stream 'ext:binary-stream)
	    (let ((stream (make-http-stream ,stream)))
	      (unwind-protect
		   (body stream)
		(ext:flush-output stream)
		(free-http-stream stream))))
	   (t
	    (body ,stream)))))

;;; CRLF stream.

(defmacro with-crlf-stream ((stream &optional direction) &body body)
  (declare (ignore stream direction))
  `(progn ,@body))



(define bytes-transmitted (stream)
  "Returns the number of bytes transmitted over STREAM."
  (etypecase stream
    (http-stream
     (bytes-transmitted (slot-value stream 'stream)))
    (scl-http::http-stream
     (scl-http::http-stream-bytes-transmitted stream))
    (scl-http::chunk-encoding-stream
     (bytes-transmitted (slot-value stream 'stream)))
    (scl-http::chunk-decoding-stream
     (bytes-transmitted (slot-value stream 'stream)))
    (ssl:ssl-stream
     (ssl::ssl-stream-bytes-written stream))))

(define (setf bytes-transmitted) (new-value stream)
  (etypecase stream
    (http-stream
     (setf (bytes-transmitted (slot-value stream 'stream)) new-value))
    (scl-http::http-stream
     (setf (scl-http::http-stream-bytes-transmitted stream) new-value))
    (scl-http::chunk-encoding-stream
     (setf (bytes-transmitted (slot-value stream 'stream)) new-value))
    (scl-http::chunk-decoding-stream
     (setf (bytes-transmitted (slot-value stream 'stream)) new-value))
    (ssl:ssl-stream
     new-value)))

(define bytes-received (stream)
  "Returns the number of bytes received over STREAM."
  (etypecase stream
    (http-stream
     (bytes-received (slot-value stream 'stream)))
    (scl-http::http-stream
     (scl-http::http-stream-bytes-received stream))
    (scl-http::chunk-encoding-stream
     (bytes-received (slot-value stream 'stream)))
    (scl-http::chunk-decoding-stream
     (bytes-received (slot-value stream 'stream)))
    (ssl:ssl-stream
     (ssl::ssl-stream-bytes-read stream))))
     

(define (setf bytes-received) (new-value stream)
  (etypecase stream
    (http-stream
     (setf (bytes-received (slot-value stream 'stream)) new-value))
    (scl-http::http-stream
     (setf (scl-http::http-stream-bytes-received stream) new-value))
    (scl-http::chunk-encoding-stream
     (setf (bytes-received (slot-value stream 'stream)) new-value))
    (scl-http::chunk-decoding-stream
     (setf (bytes-received (slot-value stream 'stream)) new-value))
    (ssl:ssl-stream
     new-value)))
    


;;;------------------------------------------------------------------- 
;;;
;;; HTTP 1.1 CHUNKED TRANSFER ENCODING
;;;

#+nil
(defun chunk-transfer-encoding-mode (stream &optional function)
  (check-type function (or null function))
  function)

(defun note-first-chunk (stream)
  (etypecase stream
    (http-stream
     #+nil (format t "~&*** First chunk TEXT: ~S~%" stream)
     (ext:flush-output stream)
     (let* ((binary-stream (slot-value stream 'stream))
	    (chunk-stream (scl-http::make-chunk-encoding-stream binary-stream)))
       (setf (slot-value stream 'stream) chunk-stream))
     stream)
    (ext:binary-stream
     #+nil (format t "~&*** First chunk BINARY: ~S~%" stream)
     (scl-http::make-chunk-encoding-stream stream))))

(defun note-last-chunk (stream &optional footers-plist)
  (ext:flush-output stream)
  (etypecase stream
    (http-stream
     #+nil (format t "~&*** Last chunk TEXT: ~S~%" stream)
     (let* ((chunk-stream (slot-value stream 'stream))
	    (binary-stream (slot-value chunk-stream 'stream)))
       (close chunk-stream)
       (scl-http::free-chunk-encoding-stream chunk-stream)
       (setf (slot-value stream 'stream) binary-stream))
     (http::write-headers stream footers-plist t)
     stream)
    (ext:binary-stream
     #+nil (format t "~&*** Last chunk BINARY: ~S~%" stream)
     (let ((binary-stream (slot-value stream 'stream)))
       (close stream)
       (scl-http::free-chunk-encoding-stream stream)
       (write-sequence (ext:make-bytes-from-string
			(with-output-to-string (stream)
			  (http::write-headers stream footers-plist t))
			:iso-8859-1)
		       binary-stream)
       binary-stream))))


;;;------------------------------------------------------------------- 
;;;
;;; HTTP 1.1 CHUNKED TRANSFER DECODING
;;;

(defun chunk-transfer-decoding-mode (stream)
  (etypecase stream
    (http-stream
     #+nil (format t "~&*** First chunk TEXT: ~S~%" stream)
     (ext:flush-output stream)
     (let* ((binary-stream (slot-value stream 'stream))
	    (chunk-stream (scl-http::make-chunk-decoding-stream binary-stream)))
       (setf (slot-value stream 'stream) chunk-stream))
     stream)
    (ext:binary-stream
     #+nil (format t "~&*** First chunk BINARY: ~S~%" stream)
     (scl-http::make-chunk-decoding-stream stream))))

(defun chunk-transfer-content-length (stream)
  (file-position stream))

(defun chunk-transfer-decoding-mode-end (stream)
  (ext:flush-output stream)
  (etypecase stream
    (http-stream
     #+nil (format t "~&*** Last chunk TEXT: ~S~%" stream)
     (let* ((chunk-stream (slot-value stream 'stream))
	    (binary-stream (slot-value chunk-stream 'stream)))
       (close chunk-stream)
       (scl-http::free-chunk-decoding-stream chunk-stream)
       (setf (slot-value stream 'stream) binary-stream))
     stream)
    (ext:binary-stream
     #+nil (format t "~&*** Last chunk BINARY: ~S~%" stream)
     (let ((binary-stream (slot-value stream 'stream)))
       (close stream)
       (scl-http::free-chunk-decoding-stream stream)
       binary-stream))))



;;;; 
;;; ---------------------------------------------------------------------------
;;; Client support.

(defun open-http-stream-to-host (host port &optional (timeout 60))
  (declare (type (unsigned-byte 16) port)
	   (optimize (speed 3)))
  #+nil
  (format t "~&* Open http stream host=~s port=~s~%" host port)
  (let* ((fd (ext:connect-to-inet-socket host port
					 :kind :stream
					 :timeout timeout))
	 (binary-stream (scl-http::make-http-stream fd
						    :timeout timeout
						    :expiration (* timeout 5)))
	 (stream (make-http-stream binary-stream)))
    stream))

(defun open-ssl-stream-to-host (host port)
  (declare (type (unsigned-byte 16) port)
	   (optimize (speed 3)))
  #+nil
  (format t "~&* Open ssl stream host=~s port=~s~%" host port)
  (let ((fd (ext:connect-to-inet-socket host port
					:kind :stream
					:timeout 60)))
    (let ((binary-stream nil))
      (unwind-protect
	   (setf binary-stream
		 (ssl:make-ssl-client-stream fd
				:element-type '(unsigned-byte 8)
				:timeout 60
				:expiration 120))
	(unless binary-stream
	  (unless (unix:unix-close fd)
	    (format t "Error: close failed."))
	  (error 'www-utils:ssl-connection-error)))
      (make-http-stream binary-stream))))


;;;: Proxy support.

(defun http::splice-stream-to-host (stream host port)
  (finish-output stream)
  (let* ((binary-stream (slot-value stream 'stream))
	 (fd1 (sys:fd-stream-fd binary-stream))
	 (fd2 (ext:connect-to-inet-socket host port
					  :kind :stream
					  :timeout 60)))
    ;; Mark the stream closed.
    (setf (slot-value binary-stream 'lisp::fd) -1)
    (setf (ext:stream-open-p binary-stream) nil)
    (setf (ext:stream-open-p stream) nil)
    ;; Hand the task to a separate thread.
    (scl-http::splice-file-descriptors fd1 fd2)))


;;;; 
;;; ---------------------------------------------------------------------------
;;; SMTP support.

(defun smtp::%open-mailer-stream (host port args)
  (declare (ignore args))
  (open-http-stream-to-host (parse-host host) port))



(defmacro with-stream-timeout ((stream timeout &key error-p) &body body)
  "Executes BODY and returns the values of the last form in BODY. However, if
a stream operation on the 'stream takes longer than TIMEOUT seconds, abort it.
If :ERROR-P is not supplied or false, just return nil.  If :ERROR-P is true,
signal an error."
  (let ((old-timeout (gensym)))
    `(let ((,old-timeout (ext:stream-expiration ,stream)))
       (unwind-protect
	    (http::handler-case-if (not ,error-p)
	      (progn
		(setf (ext:stream-expiration ,stream) ,timeout)
		,@body)
	      (sys:io-timeout ()
		nil))
	 (setf (ext:stream-expiration ,stream) ,old-timeout)))))

(export 'with-stream-timeout :www-utils)



;;; Faster local version.

(defmethod read-delimited-line (stream &optional (delimiters '(#\Return #\Linefeed)) eof buffer)
  "Reads a line from stream which is delimited by DELIMITERS."
  (declare (values line eof delimiter length)
	   (special http::*line-buffer-size*)
	   (special http::line-buffer)
	   (type stream stream)
	   (type (or null string) buffer)
	   (type list delimiters)
	   (inline read-char unread-char)
	   (optimize (speed 3) (safety 1) (debug 0)))
  (let ((crlf-p (and (= (length delimiters) 2)
		     (let ((d1 (car delimiters))
			   (d2 (cadr delimiters)))
		       (declare (type base-char d1 d2))
		       (or (and (eql d1 #\return) (eql d2 #\linefeed))
			   (and (eql d1 #\linefeed) (eql d2 #\return)))))))
    (labels ((delimiter-p (char)
	       (declare (type base-char char))
	       (cond (crlf-p
		      (or (char= char #\linefeed)
			  (char= char #\return)))
		     (t
		      (dolist (delim delimiters nil)
			(declare (type base-char delim))
			(when (eql char delim)
			  (return t))))))
	     (clear-delimiter (prev-char stream)
	       (declare (type base-char prev-char)
			(type stream stream))
	       (let ((char (read-char stream nil nil)))
		 (when (and char
			    (or (eql char prev-char)
				(not (delimiter-p char))))
		   (unread-char char stream))))
	     (do-it (stream buffer)
	       (declare (type string buffer))
	       (let* ((index 0)
		      (error-p nil)
		      (delimiter nil))
		 (declare (type fixnum index))
		 (setf (fill-pointer buffer) 0)
		 (block read-line
		   (loop
		    (kernel::with-array-data ((buffer buffer) (start) (end))
		      (declare (type simple-string buffer))
		      (loop
		       (when (>= index end)
			 (let ((new-size (ceiling (* end 1.2))))
			   (setf buffer (adjust-array buffer new-size))
			   (return)))
		       (let ((char (read-char stream nil nil)))
			 (unless char
			   (setf delimiter eof)
			   (when (zerop index)
			     (setq error-p t))
			   (return-from read-line))
			 (when (delimiter-p char)
			   (setf delimiter char)
			   (clear-delimiter char stream)
			   (return-from read-line))
			 (setf (aref buffer (+ start index)) char)
			 (incf index))))))
		 (setf (fill-pointer buffer) index)
		 (if (zerop index)
		     (values (if error-p eof buffer) error-p delimiter 0)
		     (values buffer error-p delimiter index)))))
      (declare (inline delimiter-p clear-delimiter do-it))
      (if buffer
	  (do-it stream buffer)
	  (using-resource (line-buffer http::line-buffer http::*line-buffer-size*)
	    (multiple-value-bind (buf error-p delim length)
		(do-it stream line-buffer)
	      (declare (type string buffer)
		       (fixnum length))
	      (if error-p
		  (values eof error-p delim length)
		  (kernel::with-array-data ((buf buf) (start) (end))
		    (declare (type simple-string buf)
			     (ignore end))
		    (values (subseq buf start (+ start length)) error-p delim length)))))))))

(defmethod read-delimited-line ((stream http-stream) &optional (delimiters '(#\Return #\Linefeed))
				eof buffer)
  "Reads a line from stream which is delimited by DELIMITERS."
  (declare (values line eof delimiter length)
	   (special http::*line-buffer-size*)
	   (special http::line-buffer)
	   (type stream stream)
	   (type (or null string) buffer)
	   (type list delimiters)
	   (inline read-byte ext:unread-byte)
	   (optimize (speed 3) (safety 1) (debug 0)))
  (let ((crlf-p (and (= (length delimiters) 2)
		     (let ((d1 (car delimiters))
			   (d2 (cadr delimiters)))
		       (declare (type base-char d1 d2))
		       (or (and (eql d1 #\linefeed) (eql d2 #\return))
			   (and (eql d1 #\return) (eql d2 #\linefeed)))))))
    (with-binary-stream (stream :input)
      (labels ((delimiter-p (byte)
		 (declare (type (unsigned-byte 8) byte))
		 (cond (crlf-p
			(or (= byte (char-code #\linefeed))
			    (= byte (char-code #\return))))
		       (t
			(dolist (delim delimiters nil)
			  (declare (type base-char delim))
			  (when (eql byte (char-code delim))
			    (return t))))))
	       (clear-delimiter (prev-char stream)
		 (declare (type (unsigned-byte 8) prev-char)
			  (type stream stream))
		 (let ((byte (read-byte stream nil nil)))
		   (when (and byte
			      (or (eql byte prev-char)
				  (not (delimiter-p byte))))
		     (ext:unread-byte byte stream))))
	       (do-it (stream buffer)
		 (declare (type string buffer))
		 (let* ((index 0)
			(error-p nil)
			(delimiter nil))
		   (declare (type fixnum index))
		   (setf (fill-pointer buffer) 0)
		   (block read-line
		     (loop
		      (kernel::with-array-data ((buffer buffer) (start) (end))
			(declare (type simple-string buffer)
				 (inline read-byte))
			(loop
			 (when (>= index end)
			   (let ((new-size (ceiling (* end 1.2))))
			     (setf buffer (adjust-array buffer new-size))
			     (return)))
			 (let ((byte (read-byte stream nil nil)))
			   (declare (type (or null (unsigned-byte 8)) byte))
			   (unless byte
			     (setf delimiter eof)
			     (when (zerop index)
			       (setq error-p t))
			     (return-from read-line))
			   (let ((char (code-char byte)))
			     (when (delimiter-p byte)
			       (setf delimiter char)
			       (clear-delimiter byte stream)
			       (return-from read-line))
			     (setf (aref buffer (+ start index)) char)
			     (incf index)))))))
		   (setf (fill-pointer buffer) index)
		   (if (zerop index)
		       (values (if error-p eof buffer) error-p delimiter 0)
		       (values buffer error-p delimiter index)))))
	(declare (inline delimiter-p clear-delimiter do-it))
	(if buffer
	    (do-it stream buffer)
	    (using-resource (line-buffer http::line-buffer http::*line-buffer-size*)
	      (multiple-value-bind (buf error-p delim length)
		  (do-it stream line-buffer)
		(declare (type string buf)
			 (fixnum length))
		(if error-p
		    (values eof error-p delim length)
		    (kernel::with-array-data ((buf buf) (start) (end))
		      (declare (type simple-string buf)
			       (ignore end))
		      (values (subseq buf start (+ start length))
			      error-p delim length))))))))))



(declaim (special http::*ssl-implementation-version*))

(defun ssl-implementation (&optional recache-p)
  "Returns the SSL implementation and version as strings."
  (declare (values implementation version))
  (flet ((%openssl-version-string ()
	   (string-trim '(#\newline)
			(with-output-to-string (stream)
			  (ext:run-program "openssl" '("version" "-v")
					   :wait t :input nil :output stream)))))
    (cond ((and (not recache-p) http::*ssl-implementation-version*)
           (values-list http::*ssl-implementation-version*))
          (t (let* ((string (%openssl-version-string))
                    (end (length string))
                    (pos1 (position #\space string :end end))
                    (type (subseq string 0 pos1))
                    (pos2 (position-if-not #'http::white-space-char-p string :start (1+ pos1) :end end))
                    (pos3 (position #\space string :start pos2 :end end))
                    (version (subseq string pos2 pos3)))
               ;; cache values
               (setq http::*ssl-implementation-version* (list type version))
               ;; return result
               (values type version))))))


