(in-package :www-utils)

(deftype end-of-chunk-transfer-decoding () 'end-of-file)

(defmethod chunk-transfer-encoding-mode ((stream sys:fd-stream) chunk-function)
  (declare (ignore chunk-function))
  nil)

(defmethod note-first-chunk ((stream sys:fd-stream))
  (force-output stream))

(defmethod note-last-chunk ((stream sys:fd-stream) &optional footers-plist)
  (declare (ignore footers-plist))
  (force-output stream))

(defmethod chunk-transfer-decoding-mode ((stream sys:fd-stream))
  nil
  )

(defmethod end-of-chunk-transfer-decoding ((stream sys:fd-stream))
  ;; this is the same as the next method due to the way the
  ;; with-chunked-transfer-decoding macro is written.
  (force-output stream)
  )

(defmethod chunk-transfer-decoding-mode-end ((stream sys:fd-stream))
  ;; shut down input chunk decoding
  (force-output stream)
  )

(defmethod chunk-transfer-content-length ((stream sys:fd-stream))
  ;;; I don't know what to do here
  )