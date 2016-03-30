
;;; Scieneer Common Lisp port:
;;; (C) Copyright 2006, Scieneer Pty Ltd
;;;     All Rights Reserved.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Patches to server/server.lisp

(in-package :http)
;;;

(defmethod record-response-times ((server basic-server-mixin) (url url:http-url))
  (let ((cpu-time (cpu-time server))
        (elapsed-time (elapsed-time server)))
    (with-slots (plist) url
      (kernel:with-atomic-getf-modification (time plist :cpu-time 0)
	(+ time cpu-time))
      (kernel:with-atomic-getf-modification (time plist :elapsed-time 0)
	(+ time elapsed-time))
      (kernel:with-atomic-getf-modification (n plist :n-requests 0)
	(1+ n)))
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Patches to server/shtml.lisp

(in-package :http)

(defmethod parse-shtml-template (pathname)
  (declare (values template-parameters))
  (labels ((char-to-ascii (char) (char-code char))      ;UNIX, Win32
           (ascii-to-char (byte) (code-char byte))      ;UNIX, Win32
           (string-to-8-bit-vector (string)
             (map '(simple-array (unsigned-byte 8) 1) #'char-to-ascii string)))
    (declare (inline char-to-ascii ascii-to-char))
    (let* ((+shtml-tag-start+ (string-to-8-bit-vector "<!--#"))
           (+shtml-tag-start-length+ (length +shtml-tag-start+))
           (+shtml-tag-end+ (string-to-8-bit-vector "-->"))
           (+shtml-tag-end-length+ (length +shtml-tag-end+)))
      (with-open-file (file-stream pathname :direction :input)
        (let* ((length (file-length file-stream))
               (data (make-array length :element-type '(unsigned-byte 8)))
               (positions (make-array length)))
          (dotimes (index length)
            (setf (svref positions index) (file-position file-stream)
                  (aref data index) (char-code (read-char file-stream))))
          (let ((end-position (file-position file-stream))
                (read-start 0)
                (template-parameters '()))
            (loop (let ((tag-start (search +shtml-tag-start+ data :start2 read-start)))
                    (unless tag-start
                      (let ((read-end end-position))
                        (unless (eql read-start read-end)
                          (push (list read-start read-end) template-parameters))
                        (return)))
                    (let ((tag-end (search +shtml-tag-end+ data :start2 (+ tag-start +shtml-tag-start-length+))))
                      (unless tag-end
                        (error "Unbalanced element at byte ~D" read-start))
                      (incf tag-end +shtml-tag-end-length+)
                      (let* ((string-length (- tag-end tag-start))
                             (string (make-string string-length)))
                        (dotimes (string-index string-length)
                          (let ((char (ascii-to-char (aref data (+ tag-start string-index)))))
                            (setf (schar string string-index)
                                  (if (member char '(#\Return #\Newline))
                                      #\Space
                                      char))))
			(multiple-value-bind (function parameter-plist)
			    (http::parse-shtml-element string)
			  (push (list* read-start (aref positions tag-start)
				       function parameter-plist)
				template-parameters)))
                      (setq read-start tag-end))))
            (nreverse template-parameters)))))))

;;; Patches to server/utils.lisp

(ext:defgvar *byte-buffers* nil)
(ext:defgvar *string-buffers* nil)

(defmethod stream-copy-until-eof (from-stream to-stream &optional (copy-mode :text))
  (declare (inline read-char write-char unread-char read-byte write-byte)
	   (optimize (speed 3)))
  (ecase copy-mode
    (:text
      (with-text-stream  (from-stream :input)
        (with-text-stream (to-stream :output)
	  ;#+nil
	  (loop
	   (let ((char (read-char from-stream nil nil)))
	     (unless char
	       (return))
	     (cond ((member char '(#\Linefeed #\Return) :test #'char=)
		    (write-char #\Linefeed to-stream)
		    (let ((next-char (read-char from-stream nil nil)))
		      (when (and next-char
				 (or (eql next-char char)
				     (not (member next-char '(#\Linefeed #\Return)
						  :test #'char=))))
			(unread-char next-char from-stream))))
		   (t
		    (write-char char to-stream)))))
	  ;; Optimized version, but without the CRLF translation.
	  #+nil
	  (let ((string (or (kernel:atomic-pop *string-buffers*)
			    (make-array 16384 :element-type 'base-char))))
	    (loop
	     (let ((count (ext:read-chars from-stream string 0 16384)))
	       (when (eql count -1)
		 (return))
	       (ext:write-chars to-stream string 0 count)))
	    (kernel:atomic-push string *string-buffers*))
	  )))
    ((:binary :crlf)
     (with-binary-stream (from-stream :input)
       (with-binary-stream (to-stream :output)
	 #+nil
         (loop for byte = (read-byte from-stream nil)
               while byte
               do (write-byte byte to-stream))
	 ;; Optimized version.
	 (let ((buffer (or (kernel:atomic-pop *byte-buffers*)
			   (make-array 16384 :element-type '(unsigned-byte 8)))))
	   (loop
	    (let ((count (ext:read-bytes from-stream buffer 0 16384)))
	      (when (eql count -1)
		(return))
	      (ext:write-bytes to-stream buffer 0 count)))
	   (kernel:atomic-push buffer *byte-buffers*))))))
  nil)

(defmethod stream-decode-crlf-bytes (from-stream to-stream n-bytes)
  (declare (inline read-byte write-char))
  (with-binary-stream (from-stream :input)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Patches to w3p/w3p-system.lisp

(in-package :www-present)

(defmethod get-presentation-type ((class standard-class) &optional (errorp t))
  "get an exact match for a presentation-type from a class"
  (get-presentation-type (class-name class) errorp))

(defmethod get-presentation-type ((class built-in-class) &optional (errorp t))
  "get an exact match for a presentation-type from a class"
  (get-presentation-type (class-name class) errorp))


