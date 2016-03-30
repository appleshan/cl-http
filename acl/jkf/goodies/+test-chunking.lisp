(in-package :http)

(defun use-chunked-transfer-encoding-p (content-length)
  nil)

(defmethod write-header (header value stream)
  (%write-header header value stream)
  (%write-header header value excl:*initial-terminal-io*)
  )



(force-output sock)
(socket:socket-control sock :output-chunking t)


(socket:socket-control sock :output-chunking-eof t)

(net.aserve:start :port 800 :listeners 3 :DEBUG-STREAM *standard-output* :debug :all)

(net.aserve:shutdown)