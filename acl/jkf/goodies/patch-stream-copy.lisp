(in-package :http)

(defmethod stream-copy-until-eof ((from-stream stream:fundamental-input-stream) 
                                  (to-stream socket:socket-stream-internet-active) &optional copy-mode)
  (declare (ignore copy-mode))
  (stream-buffer-copy-until-eof from-stream to-stream))

(defmethod stream-copy-until-eof ((from-stream stream:fundamental-binary-input-stream)
                                  (to-stream socket:socket-stream-internet-active) &optional copy-mode)
  (declare (ignore copy-mode))
  (with-binary-stream (from-stream :output)
    (stream-buffer-copy-until-eof from-stream to-stream)))

;;; the following fails with IE

(defmethod stream-buffer-copy-until-eof ((from-stream stream:fundamental-input-stream) 
                                         (to-stream socket:socket-stream-internet-active))
  (finish-outputs to-stream) ; Fresh start
  (loop with buffer-size = 4096
      with buffer = (make-array buffer-size :element-type (or (stream-element-type from-stream) '(unsigned-byte 8)))
      as end = (read-sequence buffer from-stream :end buffer-size)
      do (write-sequence buffer to-stream :end end)
      when (< end buffer-size)
      do (finish-outputs to-stream) ; Synchronize streams
      until (zerop end)))

(defmethod finish-outputs ((stream two-way-stream))
  (finish-output stream)
  (finish-outputs (two-way-stream-output-stream stream)))

(defmethod finish-outputs ((stream broadcast-stream))
  (finish-output stream)
  (loop for stream in (broadcast-stream-streams stream)
      do (finish-outputs stream)))

(defmethod finish-outputs ((stream socket:socket-stream-internet-active))
  (finish-output stream))