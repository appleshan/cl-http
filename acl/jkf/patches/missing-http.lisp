(in-package :http)

;;; for some reason the proxy code is not loaded
;;; Stub to avoid warning

(unless (fboundp 'clear-proxy-mappings)
  (defun clear-proxy-mappings ()
    (error "Proxy code not loaded")
    )
  )

(defun http-service-ports ()
  *http-ports*)

(defun http-service-enabled-p (&optional (ports (http-service-ports)))
  "Returns non-null when HTTP is enabled on PORTS.
PORTS is normally a list of port numbers and defaults to the currently
enabled set of HTTP ports. If any port in PORTS is not enabled for HTTP,
this returns null. PORTS may also be a single port number, in which case
this returns non-null when HTTP is enabled on it. When supplied with the
keyword :ANY, this returns non-null when HTTP is enabled on some port."
  (labels (
           (http-service-on-port-p (port)
                                   (getf *acl-tcp-servers* port))
           (get-all-ports ()
                          (remove-if-not #'numberp *acl-tcp-servers*))
           
           (any-active-http-service-p ()
                                      (dolist (port (get-all-ports) nil)
                                        (when (http-service-on-port-p port)
                                          (return t))))
           )
    (etypecase ports
      (integer (and (http-service-on-port-p ports) ports))
      (cons 
       (if (member :any ports)
           (any-active-http-service-p)
         (loop for port in ports
             unless (http-service-on-port-p port)
             return nil
             finally (return ports))))
      (null (any-active-http-service-p))
      (symbol (cond ((eql ports :any)
                     (any-active-http-service-p))
                    (t (error "Unknown keyword,~S, specified  in PORTS." ports)))))))


(defmacro with-automatic-login ((host user-id user-pw) &body body)
  "Supplies userid and PW to ensure successul FTP login to host with BODY."
  `(progn (notify-log-window "~&(WITH-AUTOMATIC-LOGIN (~S ~S ~S) - Not available on MAC"
                             ,host ,user-id ,user-pw)
          ,@body))

(defun ftp-directory-info (directory &optional (port 21) (user-id "anonymous") (user-pw (user-mail-address)))
  "Returns a list of pathname spec for directory just like DIRECTORY-INFO.
If a network error is encountered, this returns NIL."
  (declare (ignore port))
  (let* ((path (pathname directory))
         (host (pathname-host path)))
    (handler-case 
      (with-automatic-login (host user-id user-pw)
        ;; ansi CL directory fails due to :fast option  3/13/94 -- JCMa.
        (directory-info directory))
      ;; handle remote connection problems, including dead host, refused connection.
      (error () nil))))

(defun ftp-copy-file (from-pathname to-stream &key (element-type 'character)
                                     (user-id "anonymous") (user-pw (user-mail-address)))
  "Copies the content of FROM-PATHNAME to TO-STREAM. 
If a network error is encountered, this returns NIL, otherwise T.
ELEMENT-TYPE is the ANSI file openning argument."
  (let ((host (pathname-host from-pathname)))
    (handler-case 
      (with-automatic-login (host user-id user-pw)
        (with-open-file (ftp-stream from-pathname :direction :input :element-type element-type)
          (http::stream-copy-until-eof ftp-stream to-stream)
          (values t)))
      ;; handle remote connection problems, including dead host, refused connection.
      (error () nil))))

(defun ftp-copy-file-to-http-stream (from-pathname http-stream &key (port 21) data-type url additional-headers
                                                                                  (user-id "anonymous") (user-pw (server-mail-address)))
   "Copies the content of FROM-PATHNAME to HTTP-STREAM."
   (declare (values success-p)
                 (ignore port))
   (flet ((handle-invalid-ftp-user-id-and-password (&rest ignore)
               (declare (ignore ignore))
               (signal 'http::client-unauthorized-ftp-access :url url :method :get
                          :authentication-realm "FTP Server" :authentication-method :basic)))
      (declare (dynamic-extent #'handle-invalid-ftp-user-id-and-password))
      (let* ((host (pathname-host from-pathname))
                (copy-mode (or (url::%content-type-copy-mode data-type nil) :binary))
                (element-type (ecase copy-mode
                                          (:text 'character)
                                          ((:binary :crlf) '(unsigned-byte 8)))))
         (with-automatic-login (host user-id user-pw)
             (with-open-file (ftp-stream from-pathname :direction :input :element-type element-type)
                 (http:with-successful-response (http-stream (case data-type ((:unknown nil) :text) (t data-type))
                                                                                       :location url :additional-headers additional-headers)
                    (case copy-mode
                       (:text
                         (with-text-stream (http-stream :output)
                             (http::stream-copy-until-eof ftp-stream http-stream :text)))
                       ((:binary :crlf)
                         (with-binary-stream (http-stream :output)
                             (http::stream-copy-until-eof ftp-stream http-stream :binary))))
                    (values t)))))))