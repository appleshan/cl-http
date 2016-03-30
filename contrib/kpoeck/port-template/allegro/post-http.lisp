(in-package :http)

(defun ftp-copy-file-to-http-stream (from-pathname http-stream &key (port 21) data-type url additional-headers
                                                   (user-id "anonymous") (user-pw (server-mail-address)))
  "Copies the content of FROM-PATHNAME to HTTP-STREAM."
  (declare (values success-p)
           (ignore port))
  (flet ((handle-invalid-ftp-user-id-and-password (&rest ignore)
                                                  (declare (ignore ignore))
                                                  (signal 'client-unauthorized-ftp-access :url url :method :get
                                                          :authentication-realm "FTP Server" :authentication-method :basic)))
    (declare (dynamic-extent #'handle-invalid-ftp-user-id-and-password))
    (let* ((host (pathname-host from-pathname))
           (copy-mode (or (url::%content-type-copy-mode data-type nil) :binary))
           (element-type (ecase copy-mode
                           (:text 'character)
                           ((:binary :crlf) '(unsigned-byte 8)))))
      (with-automatic-login (host user-id user-pw)
        (with-open-file (ftp-stream from-pathname :direction :input :element-type element-type)
          (with-successful-response (http-stream (case data-type ((:unknown nil) :text) (t data-type))
                                                      :location url :additional-headers additional-headers)
            (case copy-mode
              (:text
               (with-text-stream (http-stream :output)
                 (stream-copy-until-eof ftp-stream http-stream :text)))
              ((:binary :crlf)
               (with-binary-stream (http-stream :output)
                 (stream-copy-until-eof ftp-stream http-stream :binary))))
            (values t)))))))