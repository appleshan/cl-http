(in-package :www-utils)

;;; make stream bivalent
(defmacro with-crlf-stream ((stream direction) &body body)
  "Turns STREAM into a CRLF stream within the scope of BODY.
direction can be :OUTPUT, :INPUT, or :BOTH."
  (declare (ignore stream direction))
  `(progn ,@body))

#-(or clisp cmu)
(defmacro with-binary-stream ((stream direction) &body body)
  (declare (ignore stream direction))
  `(progn
     ,@body))

#-clisp
(defmacro with-text-stream ((stream direction) &body body)
  "Turns STREAM into a text stream within the scope of BODY.
direction can be :OUTPUT, :INPUT, or :BOTH."
  (declare (ignore stream direction))
  `(progn ,@body))

#+clisp
(defmacro with-binary-stream ((stream direction) &body body)
  (declare (ignore direction))
  (let ((old-type (gensym)))
    `(let ((,old-type (stream-element-type ,stream)))
      (setf (stream-element-type ,stream) '(UNSIGNED-BYTE 8))
      (unwind-protect 
	   (progn
	     ,@body))
      (setf (stream-element-type ,stream) ,old-type))))

#+clisp
(defmacro with-text-stream ((stream direction) &body body)
  "Turns STREAM into a text stream within the scope of BODY.
direction can be :OUTPUT, :INPUT, or :BOTH."
  (declare (ignore direction))
  (let ((old-type (gensym)))
    `(let ((,old-type (stream-element-type ,stream)))
      (setf (stream-element-type ,stream) 'character)
      (unwind-protect 
	   (progn
	     ,@body))
      (setf (stream-element-type ,stream) ,old-type))))

;;; CMUCL enhancements written by Douglas Crosher have been
;;; placed in the public domain and are provided 'as-is'.
#+cmu
(defmacro with-binary-stream ((stream direction) &body body)
  "Turns STREAM into a binary stream within the scope of BODY.
  direction can be :OUTPUT, :INPUT, or :BOTH."
  `(let ((orig-element-type (lisp::fd-stream-element-type ,stream)))
     (unwind-protect
	 (progn
	   ,(when (or (eq direction :output) (eq direction :both))
		  `(when (eq (lisp::fd-stream-bout ,stream)
			     #'lisp::ill-bout)
			 (setf (lisp::fd-stream-bout ,stream)
			       #'lisp::output-unsigned-byte-full-buffered)))
	   ,(when (or (eq direction :input) (eq direction :both))
		  `(when (eq (lisp::fd-stream-bin ,stream)
			     #'lisp::ill-bin)
			 (setf (lisp::fd-stream-bin ,stream)
			       #'lisp::input-unsigned-8bit-byte)))
	   (setf (lisp::fd-stream-element-type ,stream) '(unsigned-byte 8))
	   ,@body)
       (setf (lisp::fd-stream-element-type ,stream) orig-element-type))))