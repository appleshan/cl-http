(in-package :www-utils)

(deftype end-of-chunk-transfer-decoding () 'excl:socket-chunking-end-of-file)

#+no
(define-condition end-of-chunk-transfer-decoding
                  (end-of-file)
  ()
  (:documentation "Signalled when a complete HTTP resource has been successfully transferred."))


(defmethod chunk-transfer-encoding-mode ((stream tcp-client-stream) chunk-function)
  (declare (ignore chunk-function))
  nil)

(defmethod note-first-chunk ((stream tcp-client-stream))
  (mp:with-timeout (http1:*persistent-connection-timeout*
                    (error "note-first-chunk timeout on output to the socket"))
    (force-output stream)
    (socket:socket-control stream :output-chunking t)))

(defmethod note-last-chunk ((stream tcp-client-stream) 
                            &optional footers-plist)
  (mp:with-timeout (http1:*persistent-connection-timeout*
                    (error "note-last-chunk timeout on output to the socket"))
    (socket:socket-control stream :output-chunking-eof t)
    (http::write-headers stream footers-plist t)
    (force-output stream)))
  

(defmethod chunk-transfer-decoding-mode ((stream tcp-client-stream))
  (socket:socket-control stream :input-chunking t)
  )

(defmethod end-of-chunk-transfer-decoding ((stream tcp-client-stream))
  ;; this is the same as the next method due to the way the
  ;; with-chunked-transfer-decoding macro is written.
  (socket:socket-control stream :input-chunking nil)
  )


(defmethod chunk-transfer-decoding-mode-end ((stream tcp-client-stream))
  ;; shut down input chunk decoding
  (socket:socket-control stream :input-chunking nil))

(defmethod chunk-transfer-content-length ((stream tcp-client-stream))
  ;;; I don't know what to do here
  )


(defmethod chunk-transfer-encoding-mode ((stream excl::ssl-server-stream) chunk-function)
  (declare (ignore chunk-function))
  nil)

(defmethod note-first-chunk ((stream excl::ssl-server-stream))
  (force-output stream))

(defmethod note-last-chunk ((stream excl::ssl-server-stream) 
                            &optional footers-plist)
  (mp:with-timeout (http1:*persistent-connection-timeout*
                    (error "note-last-chunk timeout on output to the socket"))
    (http::write-headers stream footers-plist t)
    (force-output stream)))
  

(defmethod chunk-transfer-decoding-mode ((stream excl::ssl-server-stream))
  nil
  )

(defmethod end-of-chunk-transfer-decoding ((stream excl::ssl-server-stream))
  ;; this is the same as the next method due to the way the
  ;; with-chunked-transfer-decoding macro is written.
  nil
  )

(defmethod chunk-transfer-decoding-mode-end ((stream excl::ssl-server-stream))
  ;; shut down input chunk decoding
  nil)

(defmethod chunk-transfer-content-length ((stream excl::ssl-server-stream))
  ;;; I don't know what to do here
  )