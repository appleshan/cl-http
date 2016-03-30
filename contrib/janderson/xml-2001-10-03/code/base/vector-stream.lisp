;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: xutils; -*-

#|
<DOCUMENTATION>
 <DESCRIPTION>
  simple vector streams for use with cl-xml decoding
  </DESCRIPTION>
 <COPYRIGHT YEAR='2001' AUTHOR='james adam anderson' MARK='(C)'
            href='file://xml/sysdcl.lisp' />
 <CHRONOLOGY>
  <DELTA DATE='20010605' AUTHOR='MS'>lispworks conformance</DELTA>
  <DELTA DATE='20010702'>moved from xparser to xutils to support data url
   </DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
|#
(in-package "XML-UTILS")

(defClass vector-input-stream (#+ALLEGRO excl::fundamental-binary-input-stream
                               #+LispWorks stream:fundamental-stream
                               #+MCL ccl::input-binary-stream)
  ((position :initform 0)
   (vector :initarg :vector :reader vector-input-stream.vector))
  (:default-initargs :direction :input))

(defMethod initialize-instance :after
           ((instance vector-input-stream) &key)
  (with-slots (vector) instance
    (etypecase vector
      (string (setf vector (map 'vector #'char-code vector)))
      (list (setf vector (map 'vector #'(lambda (datum)
                                          (etypecase datum
                                            (fixnum datum)
                                            (character (char-code datum))))
                              vector)))
      (vector t))))

(defMethod stream-position
           ((stream vector-input-stream) &optional new)
  (with-slots (position) stream
    (if new
      (setf position new)
      position)))

(defMethod print-object
           ((vs vector-input-stream) (stream t)
            &aux (*print-array* t) (*print-length* 32) (*print-base* 16))
  (print-unreadable-object (vs stream :type t)
    (princ (vector-input-stream.vector vs) stream)))

(defMethod stream-eofp
           ((stream vector-input-stream))
  (with-slots (position vector) stream
    (>= position (length vector))))

(defMethod stream-tyi ((stream vector-input-stream))
  (with-slots (position vector) stream
    (when (< position (length vector))
      (prog1 (svref vector position)
        (incf position)))))

(defMethod stream-untyi ((stream vector-input-stream) (datum integer))
  (with-slots (position vector) stream
    (cond ((< position 0)
           (decf position)
           (setf (svref vector position) datum))
          (t
           (error 'end-of-file :stream stream)))))

(defMethod stream-reader ((stream vector-input-stream))
  (with-slots (vector position) stream
    (if (typep vector 'simple-vector)
      #'(lambda (ignore) (declare (ignore ignore))
         (when (< position (length vector))
           (prog1 (svref vector position)
             (incf position))))
      #'(lambda (ignore) (declare (ignore ignore))
         (when (< position (length vector))
           (prog1 (aref vector position)
             (incf position)))))))

(defMethod stream-state ((stream vector-input-stream))
  (with-slots (position) stream
    (format nil "@~s" position)))

(defMethod stream-element-type ((stream vector-input-stream))
  '(unsigned-byte 8))

#|
(inspect
(mapcar #'(lambda (vector)
            (multiple-value-list 
             (decoding-stream-reader (make-instance 'vector-input-stream :vector vector) nil)))
        '(#(#x00 #x00 #x00 #x3c)
          #(#x3c #x00 #x00 #x00)
          #(#x00 #x00 #x3c #x00)
          #(#x00 #x3c #x00 #x00)
          #(#xff #xfe #x00 #x3c)
          #(#xfe #xff #x3c #x00)
          #(#x00 #x3c #x00 #x3f)
          #(#x3c #x00 #x3f #x00)
          #(#x3c #x3f #x78 #x60)
          #(#x12 #x12 #x3c #x3f))))
;("UCS-4-1234" "UCS-4-4321" "UTF-4-2143" "UCS-4-3412" "UTF-16-21" "UTF-16-12" "UTF-16-21" "UTF-16-12" "UTF-8" "UTF-8")

(defparameter *s* (encoded-stream "<?xml ?><x>asdfgh</x>"))
(inspect *s*)
(peek-char nil *s*)
(loop (unless (princ (encoded-stream-tyi *s*)) (return)))


(with-open-file (stream (choose-file-dialog) :direction :input
                        :element-type '(unsigned-byte 8))
  (let ((c nil)
        (w (make-instance 'fred-window))
        (cs (make-instance 'decoding-stream :stream stream)))
    (print (encoded-stream.encoding cs))
    (loop (unless (setf c (stream-tyi cs)) (return))
          (write-char c w))
    (fred-update w)))
|#             


:EOF

