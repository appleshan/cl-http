(in-package :www-utils)

(deftype end-of-chunk-transfer-decoding () 'end-of-file)

(defmethod chunk-transfer-encoding-mode ((stream comm:socket-stream) chunk-function)
  (declare (ignore chunk-function))
  nil)

(defmethod note-first-chunk ((stream comm:socket-stream))
  (force-output stream))

(defmethod note-last-chunk ((stream comm:socket-stream) &optional footers-plist)
  (declare (ignore footers-plist))
  (force-output stream))

(defmethod chunk-transfer-decoding-mode ((stream comm:socket-stream))
  nil
  )

(defmethod end-of-chunk-transfer-decoding ((stream comm:socket-stream))
  ;; this is the same as the next method due to the way the
  ;; with-chunked-transfer-decoding macro is written.
  (force-output stream)
  )

(defmethod chunk-transfer-decoding-mode-end ((stream comm:socket-stream))
  ;; shut down input chunk decoding
  (force-output stream)
  )

(defmethod chunk-transfer-content-length ((stream comm:socket-stream))
  ;;; I don't know what to do here
  )