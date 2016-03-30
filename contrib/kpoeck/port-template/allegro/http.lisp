(in-package :http)

(defun http-service-enabled-p (&optional (ports (%http-get-all-active-ports)))
  "Returns non-null when HTTP is enabled on PORTS.
PORTS is normally a list of port numbers and defaults to the currently
enabled set of HTTP ports. If any port in PORTS is not enabled for HTTP,
this returns null. PORTS may also be a single port number, in which case
this returns non-null when HTTP is enabled on it. When supplied with the
keyword :ANY, this returns non-null when HTTP is enabled on some port."
  (labels (
           (http-service-on-port-p (port)
                                   (%http-servive-on-port-enabled port))
           (get-all-ports ()
                          (%http-get-all-active-ports))
           
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


(defun enable-http-service (&key on-ports log (new-location t) listeners)
  "Top-level method for starting up HTTP servers."
  (values on-ports log new-location listeners)
  )

(defun enable-http-stream-server (port &key log (new-location t))
  (values port log new-location)
  )

(defun disable-http-service (&key (on-ports :all))
  on-ports
  )

(defun deallocate-client-http-stream (stream)
  (declare (ignore stream)))

(defun open-http-stream-to-host (host port)
  (declare (values stream))
  (let ((stream #+ignore 
                ;; I don't think this is needed now (-jkf 2/23/00)
                (if *proxy-service*
                    
                    (find-proxyhost-connection host :port port :default-timeout *client-timeout*)
                  (www-utils::find-direct-connection host :port port))
                ;; instead do this
                (www-utils::find-direct-connection host :port port)
                ))
    (unless stream
      (error 'host-not-responding :host host :port port))
    stream))

(defun ftp-copy-file (from-pathname to-stream &key (element-type 'character)
                                    (user-id "anonymous") (user-pw (user-mail-address)))
  "Copies the content of FROM-PATHNAME to TO-STREAM. 
If a network error is encountered, this returns NIL, otherwise T.
ELEMENT-TYPE is the ANSI file openning argument."
  (let ((host (pathname-host from-pathname)))
    (handler-case 
        (with-automatic-login (host user-id user-pw)
          (with-open-file (ftp-stream from-pathname :direction :input :element-type element-type)
            (stream-copy-until-eof ftp-stream to-stream)
            (values t)))
      ;; handle remote connection problems, including dead host, refused connection.
      (error () nil))))

(defun %get-user-name+pw (url-string  &optional realm method  proxy-p (stream *standard-output*))
  (declare (ignore realm method proxy-p))
  (let* ((user-name (default-client-user-id))
         (my-password (default-client-pw))
         ) 
    (multiple-value-bind (uid pw abort-p)
        (ask-user-name+pw url-string user-name my-password stream)
      (unless abort-p
        (when uid 
          (set-default-client-user-id uid))
        (when pw 
          (set-default-client-pw pw)))
      (values uid pw abort-p))))

#+COMMON-GRAPHICS
(defun ask-user-name+pw (url-string name pwd stream)
  (declare (ignore stream))
  (multiple-value-bind
        (user my-password button)
      (cg:pop-up-strings-dialog 
       (or (cg:selected-window (cg:screen cg:*system*))
           (cg:screen cg:*system*))
       (format nil "~&Access to URL, ~a, is restricted.~%" url-string)
       "User:"
       cg:question-icon 
       name
       "~OK" 
       "~Cancel"
       "Password"
       pwd)
    (cond ((= 1 button)
           (values user my-password nil))
          (T (values nil nil t)))))

#-COMMON-GRAPHICS
(defun ask-user-name+pw (url-string name pwd stream)
  (let (nname npwd)
    (format stream "~&Access to URL, ~a, is restricted.~%" url-string)
    (if name
        (format stream "~&Login (default: ~a): " name)
      (format stream "~&Login: "))
    (setq nname (read-line stream nil nil))
    (if (equal nname "")
        (setq nname name))
    (if pwd
        (format stream "~&Password (default: ~a): " (make-string (length pwd) :initial-element #\*))
      (format stream "~&Password: "))
    (setq npwd (read-line stream nil nil))
    (if (equal npwd "")
        (setq npwd pwd))
    (values (or nname name) (or npwd pwd) (not (and nname npwd)))))

(defparameter *default-client-user-id* nil)

(defun default-client-user-id ()
  (or *default-client-user-id* ""))

(defun set-default-client-user-id (user-id)
  (when user-id
    (setq *default-client-user-id* user-id)))

(defparameter *default-client-pw* nil)

(defun default-client-pw ()
  (or *default-client-pw* ""))

(defun set-default-client-pw (pw)
  (when pw
    (setq *default-client-pw* pw)))

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

#-(or LispWorks Genera MCL)
(defun maybe-move-server-ip-address (&key (report-stream *standard-output*))
  (when (local-host-ip-address-moved-p)
    (move-server-to-new-host (%local-host-domain-name)
                             :old-host (local-host-domain-name)
                             :reinitialize-p t
                             :report-stream report-stream)))

;;;Plist port Server
(defparameter *%servers* nil)

(defun %http-servive-on-port-enabled (port)
  (getf *%servers* port))

(defun %http-get-all-active-ports ()
  (remove-if-not #'numberp *%servers*))

(defun %note-server-on-port (server port)
  (setf (getf *%servers* port) server))

(defun %forget-server-on-port (port)
  (remf *%servers* port))

(define-condition http::directory-not-found (error)
  ;; used in cl-http but won't be signalled by acl
  ())

#+(or sbcl ecl)
(defmethod host-domain-name ((host array) &optional update-object-p)
  (declare (ignore update-object-p))
  (www-utils:domain-name-for-parsed-ip-address host))



