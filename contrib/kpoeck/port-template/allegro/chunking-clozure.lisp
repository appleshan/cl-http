(in-package :www-utils)

(deftype end-of-chunk-transfer-decoding () 'end-of-file)

(defmethod chunk-transfer-encoding-mode ((stream CCL::BASIC-TCP-STREAM) chunk-function)
  (declare (ignore chunk-function))
  nil)

(defmethod note-first-chunk ((stream CCL::BASIC-TCP-STREAM))
  (force-output stream))

(defmethod note-last-chunk ((stream CCL::BASIC-TCP-STREAM) &optional footers-plist)
  (declare (ignore footers-plist))
  (force-output stream))

(defmethod chunk-transfer-decoding-mode ((stream CCL::BASIC-TCP-STREAM))
  nil
  )

(defmethod end-of-chunk-transfer-decoding ((stream CCL::BASIC-TCP-STREAM))
  ;; this is the same as the next method due to the way the
  ;; with-chunked-transfer-decoding macro is written.
  (force-output stream)
  )

(defmethod chunk-transfer-decoding-mode-end ((stream CCL::BASIC-TCP-STREAM))
  ;; shut down input chunk decoding
  (force-output stream)
  )

(defmethod chunk-transfer-content-length ((stream CCL::BASIC-TCP-STREAM))
  ;;; I don't know what to do here
  )