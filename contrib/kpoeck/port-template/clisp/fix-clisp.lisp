(in-package :www-utils)

;;; With this, clisp does not really start serving and loops in read-delimited-line
;;; But the lack of buffering could be very bad

(defun %wait-for-connection (socket)
  (let ((stream (SOCKET:SOCKET-ACCEPT socket :BUFFERED nil
				      ;&KEY :ELEMENT-TYPE :EXTERNAL-FORMAT :BUFFERED :TIMEOUT
				      )))
    stream))

(defmethod read-delimited-line (stream &optional (delimiters '(#\Return #\Linefeed)) eof buffer)
  "Reads a line from stream which is delimited by DELIMITERS."
  (declare (values line eof delimiter length)
           (special http::*line-buffer-size*)
           (special http::line-buffer))
  (flet ((do-it (stream delimiters buffer)
           (declare (type string buffer))
           (flet ((clear-delimiter (prev-char stream)
                                   (let ((char (read-char-no-hang stream nil nil)))
                                     (when (and char
                                                (or (eql char prev-char)
                                                    (not (member char delimiters :test #'char=))))
                                       (unread-char char stream)))))
             (declare (inline clear-delimiter))
             (let* ((size (array-total-size buffer))
                    (index -1)
                    error-p delimiter)
               (handler-case
                   (with-fast-array-references ((buffer buffer string))
                     (loop initially (setf (fill-pointer buffer) 0)
                         for char = (read-char-no-hang stream t eof t)
                         until (or (eql char eof) (member char delimiters :test #'char=))
                         for idx upfrom 0
                         unless (< idx size)
                         do (setq size (floor (* (the fixnum size) 1.2))
                                buffer (adjust-array buffer size :element-type http::*standard-character-type*))
                         do (setf (aref buffer idx) char)
                           (setq index idx)
                           ;; (format t "~:C|" char)
                         finally (if (and (eql char eof) (< 0 idx))
                                     (setq error-p t)
                                   (setq delimiter char))
                           (clear-delimiter char stream)))
                 (end-of-file  () (setq error-p t)))
               (if (= -1 index)
                   (values (if error-p eof buffer) error-p delimiter 0)
                 (values buffer error-p delimiter (setf (fill-pointer buffer) (1+ (the fixnum index)))))))))
    (if buffer
        (do-it stream delimiters buffer)
      (using-resource (line-buffer http::line-buffer http::*line-buffer-size*)
                      (multiple-value-bind (buf error-p delim length)
                          (do-it stream delimiters line-buffer)
                        (values (if error-p eof (subseq buf 0 length)) error-p delim length))))))