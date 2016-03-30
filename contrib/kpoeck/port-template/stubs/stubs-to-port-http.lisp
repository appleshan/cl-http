(in-package :http)

(defun http-service-enabled-p (&optional ports)
  "Returns non-null when HTTP is enabled on PORTS.
PORTS is normally a list of port numbers and defaults to the currently
enabled set of HTTP ports. If any port in PORTS is not enabled for HTTP,
this returns null. PORTS may also be a single port number, in which case
this returns non-null when HTTP is enabled on it. When supplied with the
keyword :ANY, this returns non-null when HTTP is enabled on some port."
  ports
  )

(defun enable-http-service (&key on-ports log (new-location t) listeners)
  "Top-level method for starting up HTTP servers."
  (values on-ports log new-location listeners)
  )

(defun disable-http-service (&key (on-ports :all))
  on-ports
  )

(defun ftp-copy-file (from-pathname to-stream &key (element-type 'character)
                                    port
                                    (user-id "anonymous") (user-pw (user-mail-address)))
  "Copies the content of FROM-PATHNAME to TO-STREAM. 
If a network error is encountered, this returns NIL, otherwise T.
ELEMENT-TYPE is the ANSI file openning argument."
  (values from-pathname to-stream element-type user-id user-pw port)
  )

;;; This is incorrect in the Allegro code
(defun %get-user-name+pw (url-string &optional realm method proxy-p stream)
  (values url-string realm method proxy-p stream)
  )

(defun ftp-copy-file-to-http-stream (from-pathname http-stream &key (port 21) data-type url additional-headers
                                                   (user-id "anonymous") (user-pw (server-mail-address)))
  "Copies the content of FROM-PATHNAME to HTTP-STREAM."
  (values from-pathname http-stream port data-type url additional-headers user-id user-pw)
  )

(defun ftp-directory-info (directory &optional (port 21) (user-id "anonymous") (user-pw (user-mail-address)))
  "Returns a list of pathname spec for directory just like DIRECTORY-INFO.
If a network error is encountered, this returns NIL."
  (values directory port user-id user-pw)
  )

(defun deallocate-client-http-stream (stream)
  (declare (ignore stream)))

(defun open-http-stream-to-host (host port)
  (values host port)
  )

#-(or LispWorks Genera MCL)
(defun maybe-move-server-ip-address (&key (report-stream *standard-output*))
  (when (local-host-ip-address-moved-p)
    (move-server-to-new-host (%local-host-domain-name)
                             :old-host (local-host-domain-name)
                             :reinitialize-p t
                             :report-stream report-stream)))

(defmacro with-open-tcp-stream ((stream host port &key (timeout '*client-timeout*)) &body body)
  (declare (ignore host port timeout))
  "Opens a TCP stream to HOST on PORT with TIMEOUT within the scope of BODY."
  `(let ((,stream *standard-output*))
     (progn . ,body)))

#-(or Genera MCL LispWorks Allegro CMU clozure-common-lisp sbcl clisp)
(defun update-chunk-transfer-decoded-headers (header-set stream content-length)
  (declare (ignore header-set stream content-length)))
