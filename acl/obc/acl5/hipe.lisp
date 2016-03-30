(in-package :www-utils)

;;; Only new methods for hiper stuff copied from acl;jkf;hiper-socket*.lisp
;;;

;;; KAP 2003-06-09
;;; use hiper sockets

(defmethod chunk-transfer-encoding-mode ((stream socket::hiper-socket-stream-internet-active) chunk-function)
  (declare (ignore chunk-function))
  nil)

(defmethod note-first-chunk ((stream socket::hiper-socket-stream-internet-active))
  (mp:with-timeout (http1:*persistent-connection-timeout*
                    (error "note-first-chunk timeout on output to the socket"))
    (force-output stream)
    (socket:socket-control stream :output-chunking t)))

(defmethod note-last-chunk ((stream socket::hiper-socket-stream-internet-active) 
                            &optional footers-plist)
  (mp:with-timeout (http1:*persistent-connection-timeout*
                    (error "note-last-chunk timeout on output to the socket"))
    (socket:socket-control stream :output-chunking-eof t)
    (http::write-headers stream footers-plist t)
    (force-output stream)))
  

(defmethod chunk-transfer-decoding-mode ((stream socket::hiper-socket-stream-internet-active))
  (socket:socket-control stream :input-chunking t)
  )

(defmethod end-of-chunk-transfer-decoding ((stream socket::hiper-socket-stream-internet-active))
  ;; this is the same as the next method due to the way the
  ;; with-chunked-transfer-decoding macro is written.
  (socket:socket-control stream :input-chunking nil)
  )


(defmethod chunk-transfer-decoding-mode-end ((stream socket::hiper-socket-stream-internet-active))
  ;; shut down input chunk decoding
  (socket:socket-control stream :input-chunking nil))

;; Already defined in unix.lisp
#+ignore
(define ip-address-for-parsed-ip-address (ip-number)
  "Returns an IP address as a string from, IP-NUMBER, the parsed address."
  (if (stringp  ip-number)
      ip-number
    (socket:ipaddr-to-dotted ip-number)))

(defmethod www-utils:foreign-host ((http-stream socket::hiper-socket-stream-internet-active))
   (socket:remote-host http-stream))

(defmethod www-utils:foreign-port ((http-stream socket::hiper-socket-stream-internet-active))
  (socket:remote-port http-stream))

(defmethod www-utils:local-port ((http-stream socket::hiper-socket-stream-internet-active))
  (socket:local-port http-stream))

(defmethod (setf ipc::tcp-stream-process) (process (stream socket::hiper-socket-stream-internet-active))
  process)
