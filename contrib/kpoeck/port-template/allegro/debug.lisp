(in-package :http)

#+no
(defun %provide-single-service (stream client-address client-host)
  (let ((server (make-server nil  stream client-host client-address)))
          (initialize-resourced-server nil server stream client-host client-address)
          
          ;; keep other processes from tampering with our TCP state.
          (let ((*server* server))
            (provide-service server))))

#+no
(defun  use-chunked-transfer-encoding-p (content-length)
  (declare (ignore content-length))
  nil)

;;; to disable caching of static files
#+no
(defun conditional-get-not-modified-p (&rest was)
  nil)

#+no
(defun client-http-version-meets-p (&rest was) nil)


;; (trace use-chunked-transfer-encoding-p provide-service WRITE-DIRECTORY-LISTING-ACCEPTABLE-TO-CLIENT)

;; ( start-examples t :single)