(in-package :www-utils)

(deftype end-of-chunk-transfer-decoding () 'end-of-file)

(defmethod chunk-transfer-encoding-mode ((stream stream) chunk-function)
  (declare (ignore chunk-function))
  nil)

(defmethod note-first-chunk ((stream stream))
  (force-output stream))

(defmethod note-last-chunk ((stream stream) &optional footers-plist)
  (declare (ignore footers-plist))
  (force-output stream))

(defmethod chunk-transfer-decoding-mode ((stream stream))
  nil
  )

(defmethod end-of-chunk-transfer-decoding ((stream stream))
  ;; this is the same as the next method due to the way the
  ;; with-chunked-transfer-decoding macro is written.
  (force-output stream)
  )

(defmethod chunk-transfer-decoding-mode-end ((stream stream))
  ;; shut down input chunk decoding
  (force-output stream)
  )

(defmethod chunk-transfer-content-length ((stream stream))
  ;;; I don't know what to do here
  )