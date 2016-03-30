;;; -*- Mode: lisp; Package: IPC; -*-
;;;
;;; LispWorks 4 interface to TCP/IP streams
;;;
;;; Copyright (C) 1997-2000 Xanalys Inc.  All rights reserved.
;;;
;;; Enhancements Copyright (C) 2003, 2006, John C. Mallery. All rights reserved.
;;;

(in-package :IPC)

;; Added SSL support conditionalized by #+CL-HTTP-SSL -- JCMa 3/24/2006

;; Some optimization declarations conditionalized by #+CL-HTTP-Debugging have
;; been changed to #+CL-HTTP-Untested in order to increase safety in the
;; released system pending investigation of a memory corruption bug. 12/17/2003 -- JCMa.

;;; SSL condition hierarchy introduced by LispWorks 4.4
;;;
;;; ssl-condition
;;;   ssl-closed      - The condition class ssl-closed corresponds to SSL_ERROR_ZERO_RETURN . It means the underlying socket is dead.
;;;   ssl-error       - The condition class ssl-error corresponds to SSL_ERROR_SYSCALL . It means that something got broken.
;;;   ssl-failure     - The condition class ssl-failure corresponds to SSL_ERROR_SSL . This means a failure in processing the input, 
;;;                     typically due to a mismatch between the client and the server. You get this error when trying to use a SSL 
;;;                     connection to a non-secure peer.
;;;   ssl-x509-lookup - The condition class ssl-x509-lookup corresponds to SSL_ERROR_WANT_X509_LOOKUP . It happens when a certificate 
;;;                     is rejected by a user callback.

;;; This is the condition hierarchy, based on the LispM one.
;;; network-error
;;;   domain-resolver-error
;;;   local-network-error
;;;     network-resources-exhausted
;;;     unknown-address
;;;     unknown-host-name
;;;   remote-network-error
;;;     bad-connection-state
;;;       connection-closed
;;;       connection-lost
;;;       host-stopped-responding
;;;     connection-error
;;;       connection-refused
;;;     host-not-responding
;;;     protocol-timeout
;;; network-parse-error

(defparameter *tcp-stream-safe-abort-states*
  '("Waiting for socket input"
    "Waiting for socket output")
  "Process whostates from which it is safe to abort HTTP connnections.")

(define-condition www-utils:network-error (simple-error)
  ()
  (:report (lambda (condition stream)
             (let ((control (simple-condition-format-control condition)))
               (if control
                   (apply #'format stream control (simple-condition-format-arguments condition))
                 (format stream "A network error of type ~S occurred." (type-of condition))))))
  (:default-initargs
   :format-control nil
   :format-arguments nil))

(define-condition www-utils:domain-resolver-error (www-utils:network-error)
  ((address :initarg :address))
  (:report (lambda (condition stream)
             (if (slot-boundp condition 'address)
	         (format stream "Cannot resolve IP address ~A" (ip-address-string (slot-value condition 'address)))
	       (format stream "Cannot find current domainname")))))

(define-condition www-utils:local-network-error (www-utils:network-error)
  ())

(define-condition www-utils:unknown-host-name (www-utils:local-network-error)
  ((hostname :initarg :hostname))
  (:report (lambda (condition stream)
	     (format stream "Unknown host name ~A" (slot-value condition 'hostname)))))

#+comment ; MJS 07Oct97: not used
(define-condition www-utils:unknown-address (www-utils:local-network-error)
  ((address :initarg :address))
  (:report (lambda (condition stream)
	     (let ((address (slot-value condition 'address)))
	       (format stream "Unknown address ~A" (ip-address-string address))))))

(define-condition www-utils:remote-network-error (www-utils:network-error) ())

#+CL-HTTP-SSL
(define-condition www-utils:ssl-certificate-rejected (www-utils:network-error comm:ssl-x509-lookup) ())

(define-condition www-utils:bad-connection-state (www-utils:remote-network-error) ())

(define-condition www-utils:connection-closed (www-utils:bad-connection-state) ())

#+CL-HTTP-SSL
(define-condition www-utils:ssl-connection-closed (www-utils:connection-closed comm:ssl-closed) ())

(define-condition www-utils:connection-lost (www-utils:bad-connection-state) ())

(define-condition www-utils:host-stopped-responding (www-utils:bad-connection-state) ())

(define-condition www-utils:connection-error (www-utils:remote-network-error) ())

#+CL-HTTP-SSL
(define-condition www-utils:ssl-connection-error (www-utils:connection-error comm:ssl-error) ())

(define-condition www-utils:connection-refused (www-utils:connection-error) ())

#+CL-HTTP-SSL
(define-condition www-utils:ssl-connection-refused (www-utils:connection-refused comm:ssl-failure) ())

(define-condition www-utils:host-not-responding (www-utils:remote-network-error) ())

(define-condition www-utils:protocol-timeout (www-utils:remote-network-error) ())

(define-condition www-utils:network-error-mixin
		  (www-utils:network-error)
  ()
  (:documentation "Mixin to allow ports to inherit instance variables and methods to network conditions
defined at the portable code level."))

(define-condition connection-timed-out (www-utils:protocol-timeout www-utils:bad-connection-state)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
	     (format stream "Connection timed out."))))

;;; ----------------------------------------------------------------------------------
;;; NETWORK ADDRESS CODE
;;;

;; Turn internet address into string format
(defun ip-address-string (address)
  (comm:ip-address-string address))

;; Turn internet string address format into IP number
(defun string-ip-address (address-string)
  (let ((address (comm:string-ip-address address-string)))
    #+LispWorks4.0
    (when (eql address #xffffffff)
      (setq address nil))
    address))

(defun internet-address (name)
  (or (comm:get-host-entry name :fields '(:address))
      (error 'www-utils:unknown-host-name :hostname name)))

(defun internet-addresses (name)
  (or (comm:get-host-entry name :fields '(:addresses))
      (error 'www-utils:unknown-host-name :hostname name)))

#+UNIX
(fli:define-foreign-function (c-getdomainname getdomainname)
                             ((name :pointer)
                              (namelen :int))
                             :result-type :int)

(defvar *domainname* nil)

;;; There are more than one strategies you can use
;;; based on what OS you are using. Instead of providing
;;; each one we try one.
;;;
(defun getdomainname (&optional (where "/etc/defaultdomain"))
  (or *domainname*
      #+UNIX
      (fli:with-dynamic-foreign-objects ((buffer (:ef-mb-string :external-format :ascii :limit 256)))
	(and (eql (c-getdomainname buffer 256) 0)
             (setq *domainname* (fli:convert-from-foreign-string buffer))))
      ;; Try using DNS name lookup: on some machines this 
      (let* ((self-name (comm:get-host-entry (machine-instance) :fields '(:name)))
	     (dot (and self-name (position #\. self-name))))
	(and dot (subseq self-name (1+ dot))))
      (when (probe-file where)
        (with-open-file (stream where :direction :input)
          (setq *domainname* (read-line stream))))
      (error 'www-utils:domain-resolver-error)))

(defun get-host-name-by-address (address)
  (or (comm:get-host-entry address :fields '(:name))
      (error 'www-utils:domain-resolver-error :address address)))

;;; ----------------------------------------------------------------------------------
;;; STREAM CODE

(defclass chunk-transfer-encoding-output-stream (stream:buffered-stream)
  ((chunk-start-index)
   (chunk-function)
   (chunks-transmitted :initform nil)))

(defclass chunk-transfer-decoding-input-stream (stream:buffered-stream)
  ((chunk-length-received)
   (chunk-remaining-bytes :initform nil)
   (chunk-real-buffer-limit)))

; local-protocol returns URL scheme protocol for stream, eg :http, :https -- JCMa 3/22/2006
(defclass extended-socket-stream (comm:socket-stream)
  ((local-host :initarg :local-host :accessor local-host)
   (local-port :initarg :local-port :accessor www-utils:local-port)
   (foreign-host :initarg :foreign-host :accessor www-utils:foreign-host)
   (foreign-port :initarg :foreign-port :accessor www-utils:foreign-port)
   (bytes-received :initform 0 :accessor www-utils:bytes-received)
   (bytes-transmitted :initform 0 :accessor www-utils:bytes-transmitted)
   (local-protocol :initform :http :initarg :local-protocol :accessor www-utils:local-protocol)
   #+CL-HTTP-X509
   (peer-certificate :initarg :peer-certificate :accessor %peer-certificate)))

(defclass cl-http-chunk-transfer-socket-stream 
          (chunk-transfer-encoding-output-stream
           chunk-transfer-decoding-input-stream
           extended-socket-stream)
  ())

(defmethod print-object ((socket-stream extended-socket-stream) stream)
  (with-slots (local-protocol local-host local-port foreign-host foreign-port) socket-stream
    (flet ((ip-address (thing)
             (etypecase thing
               (integer (ip-address-string thing))
               (string thing)))
           (local-host ()
             (ip-address-string (internet-address (machine-instance)))))
      (declare (inline local-host))
      (print-unreadable-object (socket-stream stream :type t :identity t)
        (when (and (slot-boundp socket-stream 'foreign-host)
                   (slot-boundp socket-stream 'foreign-port))
          (format stream "~A: ~A:~A <-> ~A:~D" local-protocol (ip-address local-host)
                  (if (slot-boundp socket-stream 'local-port) local-port "loopback") 
                  (ip-address foreign-host) foreign-port))))))

;;;------------------------------------------------------------------- 
;;;
;;; X.509 STREAM INTERFACE
;;;

#+CL-HTTP-X509
(declaim (inline %ssl-peer-certificate-verified-p))

#+CL-HTTP-X509
(defun %ssl-peer-certificate-verified-p (ssl)
  (zerop (comm::ssl-get-verify-result ssl)))

#+CL-HTTP-X509
(defgeneric www-utils:peer-certificate-verified-p (extended-socket-stream)
  (:documentation "Returns non-null when peer certificate has been verified for the SSL EXTENDED-SOCKET-STREAM.
If the stream is not in SSL mode or peer certificates are not required this returns NIL."))

#+CL-HTTP-X509
(defmethod www-utils:peer-certificate-verified-p ((stream extended-socket-stream))
  (let ((ssl (comm:socket-stream-ssl stream)))
    (when ssl
      (%ssl-peer-certificate-verified-p ssl))))

#+CL-HTTP-X509
(declaim (function (http::allocate-x509-certificate (x509-pointer))))

#+CL-HTTP-X509
(defmethod www-utils:peer-certificate ((stream extended-socket-stream))
  (flet ((get-peer-certificate (stream)
           (let ((ssl (comm:socket-stream-ssl stream)))
             (when (and ssl (%ssl-peer-certificate-verified-p ssl))
               (let ((x509-pointer (comm::ssl-get-peer-certificate ssl)))
                 (when (and (not (fli:null-pointer-p x509-pointer)) ;not valid if null
                            (comm::x509-pointer-p x509-pointer))
                   (http::allocate-x509-certificate x509-pointer)))))))
    (declare (inline get-peer-certificate))
    (cond ((slot-boundp stream 'peer-certificate)
           (%peer-certificate stream))
          (t (setf (%peer-certificate stream) (get-peer-certificate stream))))))

;;;------------------------------------------------------------------- 
;;;
;;; STREAM INTERFACE
;;;

(defmethod stream:stream-read-buffer ((stream extended-socket-stream)
                                      buffer start end)
  (declare (ignore buffer start end))
  (with-slots (bytes-received) stream
    (let ((len (call-next-method)))
      (declare (fixnum len))
      (when (> len 0)
	(incf bytes-received len))
      len)))

(defmethod stream:stream-write-buffer :after ((stream extended-socket-stream)
					      buffer start end)
  (declare (ignore buffer)
	   (fixnum start end))
  (with-slots (bytes-transmitted) stream
    (incf bytes-transmitted (the fixnum (- end start)))))

(declaim (inline make-tcp-stream))

;; Why not resource the stream objects and save some consing? -- JCMa 10/9/2003
(defun make-tcp-stream (socket-handle local-port read-timeout)
  (multiple-value-bind (foreign-host foreign-port)
      (comm:get-socket-peer-address socket-handle)
    (if foreign-host
        (multiple-value-bind (local-host local-port*)
            (comm:get-socket-address socket-handle)
          (declare (ignore local-port*))
          (make-instance 'cl-http-chunk-transfer-socket-stream
                         :socket socket-handle
                         :direction :io
                         :element-type 'base-char
                         :local-host local-host
                         :local-port local-port
                         :foreign-host foreign-host
                         :foreign-port foreign-port
                         :read-timeout read-timeout
                         :local-protocol :http))
      (error 'www-utils:network-error))))

(defun lispworks-accept-connection (fd port read-timeout function)
  (declare (optimize (speed 3)))
  (handler-case
      (let ((stream (make-tcp-stream fd port read-timeout)))
	(funcall function stream  port))
    (error (c) (abort c))))  ;announce

(defun %listen-for-connections (address port backlog read-timeout function)
  (declare (optimize (speed 3)))
  (let ((fctn-spec `(lispworks-accept-connection ,port ,read-timeout ,function)))
    (declare (dynamic-extent fctn-spec))
    (comm::listen-and-attach-stream fctn-spec port nil backlog address)))

#+CL-HTTP-SSL
(declaim (inline make-ssl-tcp-stream))

#+CL-HTTP-SSL
(defun make-ssl-tcp-stream (socket-handle local-port read-timeout ssl-ctx)
  (multiple-value-bind (foreign-host foreign-port)
      (comm:get-socket-peer-address socket-handle)
    (if foreign-host
        (multiple-value-bind (local-host local-port*)
            (comm:get-socket-address socket-handle)
          (declare (ignore local-port*))
          (make-instance 'cl-http-chunk-transfer-socket-stream
                         :socket socket-handle
                         :direction :io
                         :element-type 'base-char
                         :local-host local-host
                         :local-port local-port
                         :foreign-host foreign-host
                         :foreign-port foreign-port
                         :read-timeout read-timeout
                         :local-protocol :https
                         :ssl-ctx ssl-ctx
                         :ssl-side :server))
      (error 'www-utils:network-error))))

#+CL-HTTP-SSL
(defun lispworks-accept-ssl-connection (fd port read-timeout ssl-ctx function)
  (handler-case
      (let ((stream (make-ssl-tcp-stream fd port read-timeout ssl-ctx)))
	(funcall function stream  port))
    (error (c) (abort c))))  ;announce

#+CL-HTTP-SSL
(defun %listen-for-ssl-connections (address port backlog read-timeout ssl-ctx function)
  (declare (optimize (speed 3)))
  (let ((fctn-spec `(lispworks-accept-ssl-connection ,port ,read-timeout ,ssl-ctx ,function)))
    (declare (dynamic-extent fctn-spec))
    (comm::listen-and-attach-stream fctn-spec port nil backlog address)))

#-CL-HTTP-SSL
(defun http::%open-http-stream-to-host (host port timeout)
  (declare (optimize (speed 3)))
  (let ((socket-handle nil))
    (unwind-protect
	(progn
	  (setq socket-handle (comm:connect-to-tcp-server host port :errorp nil :timeout timeout))
	  (if socket-handle
              (multiple-value-bind (local-host local-port)
                  (comm:get-socket-address socket-handle)
                (make-instance 'cl-http-chunk-transfer-socket-stream
                               :socket (shiftf socket-handle nil)
                               :direction :io
                               :element-type 'base-char
                               :local-host local-host
                               :local-port local-port
                               :foreign-host host
                               :foreign-port port
                               :read-timeout timeout))
            (error 'www-utils:connection-error)))
      (when socket-handle
	(comm::close-socket socket-handle)))))

;; Added SSL client streams. -- JCMa 3/22/2006
#+CL-HTTP-SSL
(defun http::%open-http-stream-to-host (host port timeout &optional ssl-ctx)
  (declare (optimize (speed 3)))
  (let ((socket-handle nil))
    (unwind-protect
        (cond ((setq socket-handle (comm:connect-to-tcp-server host port :errorp nil :timeout timeout))
               (multiple-value-bind (local-host local-port)
                   (comm:get-socket-address socket-handle)
                 (make-instance 'cl-http-chunk-transfer-socket-stream
                                :socket (shiftf socket-handle nil)
                                :direction :io
                                :element-type 'base-char
                                :local-host local-host
                                :local-port local-port
                                :foreign-host host
                                :foreign-port port
                                :read-timeout timeout
                                :ssl-ctx ssl-ctx
                                :ssl-side :client)))
              (ssl-ctx
               (error 'www-utils:ssl-connection-error))
              (t (error 'www-utils:connection-error)))
      (when socket-handle
	(comm::close-socket socket-handle)))))

(defclass smtp-stream (extended-socket-stream)
  ((newline-p :initform t)
   (body-p :initform nil)))

;; When LW does not know them, add assigned protocol ports here.
(defvar *tcp-service-port-alist* '(("finger" . 79)))

(defun tcp-service-port-number-aux (service-name error-p)
  (let ((port (or (comm::get-port-for-service service-name "tcp")
                  (cdr (assoc service-name *tcp-service-port-alist* :test #'string-equal)))))
    (cond (port (comm::ntohs port))
	  (error-p (error "Unknown TCP service name ~S" service-name)))))

(defun www-utils:tcp-service-port-number (protocol &optional error-p)
  "Returns the service port number for the TCP protocol denoted by protocol.
PROTOCOL is a keyword,, but integer and string are also supported."
  (etypecase protocol
    (integer protocol)
    (keyword
     (let ((string (string-downcase protocol)))
       (declare (dynamic-extent string))
       (tcp-service-port-number-aux string error-p)))
    (string (tcp-service-port-number-aux protocol error-p))))

(declaim (inline smtp::%open-mailer-stream))

(defun smtp::%open-mailer-stream (host port args)
  (let ((socket-handle nil))
    (unwind-protect
	(progn
	  (setq socket-handle (comm:connect-to-tcp-server host port :errorp nil))
	  (if socket-handle
	      (apply 'make-instance 'smtp-stream
		     :socket (shiftf socket-handle nil)
		     :direction :io
		     :element-type 'base-char
		     :foreign-host host
		     :foreign-port port
		     args)
	    (error 'www-utils:connection-error)))
      (when socket-handle
	(comm::close-socket socket-handle)))))

;; redfines macro conditionalized to not apply to lispworks4
(defmacro smtp::with-message-body-encoding ((stream output-stream) &body body)
  `(let ((,stream ,output-stream))
     (unwind-protect
	 (progn
	   (setf (slot-value ,stream 'body-p) t)
	   . ,body)
       (setf (slot-value ,stream 'body-p) nil))))

(defmethod stream:stream-write-char ((stream smtp-stream) char)
  (with-slots (newline-p body-p) stream
    ;; Convert dot at line start to dot dot.
    (when (and newline-p body-p (eql char #\.))
      (call-next-method stream #\.))
    ;; Convert newline to CRLF.
    (when (setf newline-p (eql char #\Newline))
      (call-next-method stream #\Return))
    (call-next-method)))

(defmethod stream:stream-write-string ((stream smtp-stream) string &optional (start 0) end)
  (with-slots (newline-p) stream
    (loop (let ((break (position-if #'(lambda (char)
					(or (eql char #\Newline)
					    (eql char #\.)))
				    string
				    :start start
				    :end end)))
	    (unless break
	      (setf newline-p nil)
	      (return (call-next-method stream string start end)))
	    (when (> break start)
	      (setf newline-p nil))
	    (call-next-method stream string start break)
	    (stream:stream-write-char stream (char string break))
	    (setq start (1+ break))))))


#+MSWindows
(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant WSAENETRESET    10052)
(defconstant WSAECONNABORTED 10053)
(defconstant WSAECONNRESET   10054)
(defconstant WSAETIMEDOUT    10060)
(defconstant WSAECONNREFUSED 10061)
(defconstant WSAEHOSTDOWN    10064)
)

#+unix
(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant EPIPE 32)
(defparameter ENETRESET
   (cond ((sys:featurep :sunos4) 52)
         ((sys:featurep :svr4) 129)
         ((sys:featurep :aix) 71)
         ((sys:featurep :hp-ux) 230)
         ((sys:featurep :osf1) 52)
         ((sys:featurep :linux) 102)
         ((sys:featurep :darwin) 52)
         ((sys:featurep :freebsd) 52)
         ))
(defparameter ECONNABORTED
   (cond ((sys:featurep :sunos4) 53)
         ((sys:featurep :svr4) 130)
         ((sys:featurep :aix) 72)
         ((sys:featurep :hp-ux) 231)
         ((sys:featurep :osf1) 53)
         ((sys:featurep :linux) 103)
         ((sys:featurep :darwin) 53)
         ((sys:featurep :freebsd) 53)
         ))
(defparameter ECONNRESET
   (cond ((sys:featurep :sunos4) 54)
         ((sys:featurep :svr4) 131)
         ((sys:featurep :aix) 73)
         ((sys:featurep :hp-ux) 232)
         ((sys:featurep :osf1) 54)
         ((sys:featurep :linux) 104)
         ((sys:featurep :darwin) 54)
         ((sys:featurep :freebsd) 54)
         ))
(defparameter ETIMEDOUT
   (cond ((sys:featurep :sunos4) 60)
         ((sys:featurep :svr4) 145)
         ((sys:featurep :aix) 78)
         ((sys:featurep :hp-ux) 238)
         ((sys:featurep :osf1) 60)
         ((sys:featurep :linux) 110)
         ((sys:featurep :darwin) 60)
         ((sys:featurep :freebsd) 60)
         ))
(defparameter ECONNREFUSED
   (cond ((sys:featurep :sunos4) 61)
         ((sys:featurep :svr4) 146)
         ((sys:featurep :aix) 79)
         ((sys:featurep :hp-ux) 239)
         ((sys:featurep :osf1) 61)
         ((sys:featurep :linux) 111)
         ((sys:featurep :darwin) 61)
         ((sys:featurep :freebsd) 61)
         ))
(defparameter EHOSTDOWN
   (cond ((sys:featurep :sunos4) 64)
         ((sys:featurep :svr4) 147)
         ((sys:featurep :aix) 80)
         ((sys:featurep :hp-ux) 241)
         ((sys:featurep :osf1) 64)
         ((sys:featurep :linux) 112)
         ((sys:featurep :darwin) 64)
         ((sys:featurep :freebsd) 64)
         )))

;; Hook into error signaling
(defmethod comm::socket-error ((stream extended-socket-stream) error-code format-control &rest format-arguments)
  (let ((class #+MSWindows
               (case error-code
                 ((#.WSAECONNABORTED #.WSAECONNRESET)
		  'www-utils:connection-closed)
                 ((#.WSAETIMEDOUT)
                  'connection-timed-out)
                 ((#.WSAENETRESET)
                  'www-utils:connection-lost)
                 ((#.WSAECONNREFUSED)
                  'www-utils:connection-refused)
                 ((#.WSAEHOSTDOWN)
                  'www-utils:host-not-responding)
                 (t 'www-utils:network-error))
               #+unix
               (cond ((or (eql error-code EPIPE)
                          (eql error-code ECONNABORTED)
                          (eql error-code ECONNRESET))
                      'www-utils:connection-closed)
                     ((eql error-code ETIMEDOUT)
                      'connection-timed-out)
                     ((eql error-code ENETRESET)
                      'www-utils:connection-lost)
                     ((eql error-code ECONNREFUSED)
                      'www-utils:connection-refused)
                     ((eql error-code ECONNREFUSED) 
                      'www-utils:host-not-responding)
                     (t 'www-utils:network-error))))
    (error class :format-control "~A during socket operation: ~?" :format-arguments (list class format-control format-arguments))))

(define-condition www-utils:end-of-chunk-transfer-decoding (end-of-file)
  ()
  (:documentation "Condition signalled when a complete HTTP resource has been successfully transferred.")
  (:report (lambda (condition stream)
             (format stream "End of chunk tranfer decoding on stream ~S"
                     (stream-error-stream condition)))))

(define-condition end-of-file-while-copying (end-of-file)
  ((bytes :initarg :bytes))
  (:report (lambda (condition stream)
             (with-slots (bytes) condition
               (format stream "End of stream before n-bytes ~D copied."
                       bytes)))))

(defmethod http:stream-copy-until-eof ((from-stream stream) (to-stream stream) &optional copy-mode)
  (declare (optimize (speed 3))
           (ignore copy-mode))
  (%stream-copy-bytes from-stream to-stream nil))

(defmethod http::stream-copy-bytes ((from-stream stream) (to-stream stream) n-bytes &optional (copy-mode :binary))
  (declare (optimize (speed 3))
           (ignore copy-mode))
  (%stream-copy-bytes from-stream to-stream n-bytes))

(defmethod http::stream-copy-byte-range ((from-stream stream) (to-stream stream) start last)
  (cond ((file-position from-stream start)
	 (%stream-copy-bytes from-stream to-stream (- last start)))
	(t (error "Unable to set file position for byte range copy."))))

(defun %copy-buffer (output-buffer input-buffer output-index input-index new-output-limit)
  (declare (fixnum output-index input-index new-output-limit)
           #-CL-HTTP-Debugging (optimize (safety 0)))
  #+LispWorks4.0
  (do ((input-index input-index (1+ input-index))
       (output-index output-index (1+ output-index)))
      ((eql output-index new-output-limit))
    (setf (schar output-buffer output-index)
          (schar input-buffer input-index)))
  #-LispWorks4.0 ; post 4.0, replace is more than 2 times faster
  (replace output-buffer input-buffer
           :start1 output-index
           :start2 input-index
           :end1 new-output-limit))

(defun %binary-copy-buffer (output-buffer input-buffer output-index input-index new-output-limit)
  (declare (fixnum output-index input-index new-output-limit)
           #-CL-HTTP-Debugging (optimize (safety 0)))
  #+LispWorks4.0
  (do ((input-index input-index (1+ input-index))
       (output-index output-index (1+ output-index)))
      ((eql output-index new-output-limit))
    (setf (aref output-buffer output-index)
          (aref input-buffer input-index)))
  #-LispWorks4.0 ; post 4.0, replace is more than 2 times faster
  (replace output-buffer input-buffer
           :start1 output-index
           :start2 input-index
           :end1 new-output-limit))

(defmacro loop-over-stream-input-buffer ((input-buffer input-index input-limit input-bytes)
                                         (from-stream n-bytes)
                                         &body body)
  (rebinding (from-stream n-bytes)
    `(loop (when (eql ,n-bytes 0)
             (return))
           (stream:with-stream-input-buffer (,input-buffer ,input-index ,input-limit)
               ,from-stream
             (if (>= ,input-index ,input-limit)
                 (unless (handler-case (stream:stream-fill-buffer ,from-stream)
                           (www-utils:end-of-chunk-transfer-decoding
                            ()
                            nil))
                   (if ,n-bytes
                       (error 'end-of-file-while-copying
                              :stream ,from-stream
                              :bytes ,n-bytes)
                     (return)))
               (let* ((input-length (- ,input-limit ,input-index))
                      (,input-bytes (if (or (null ,n-bytes) (> ,n-bytes input-length))
                                        input-length
                                      ,n-bytes)))
                 (declare (fixnum input-length ,input-bytes))
                 ,@body
                 (when ,n-bytes
                   (decf ,n-bytes ,input-bytes))))))))

(defmethod %stream-copy-bytes ((from-stream stream:buffered-stream) (to-stream stream:buffered-stream) n-bytes)
  (loop-over-stream-input-buffer (input-buffer input-index input-limit input-bytes)
      (from-stream n-bytes)
    (let ((remaining-input-bytes input-bytes))
      (loop (stream:with-stream-output-buffer (output-buffer output-index output-limit)
                to-stream
              (if (>= output-index output-limit)
                  (stream:stream-flush-buffer to-stream)
                (let* ((test-output-limit (+ output-index remaining-input-bytes))
                       (new-output-limit (if (> test-output-limit output-limit)
                                             output-limit
                                           test-output-limit))
                       (output-length (- new-output-limit output-index)))
                  (%copy-buffer output-buffer input-buffer
                                output-index input-index
                                new-output-limit)
                  (incf input-index output-length)
                  (setf output-index new-output-limit)
                  (when (>= output-index output-limit)
                    (stream:stream-flush-buffer to-stream))
                  (decf remaining-input-bytes output-length)
                  (when (eql remaining-input-bytes 0)
                    (return))))))))
  nil)

(defmethod %stream-copy-bytes ((from-stream stream:buffered-stream) (to-stream stream) n-bytes)
  (declare (optimize (speed 3)))
  (loop-over-stream-input-buffer (input-buffer input-index input-limit input-bytes)
      (from-stream n-bytes)
    (let ((input-end (+ input-index input-bytes)))
      (declare (fixnum input-end) 
               #+CL-HTTP-Untested (optimize (safety 0)))
      (loop for index fixnum from input-index below input-end
	    do (write-byte (char-code (schar input-buffer index)) to-stream))
      (setf input-index input-end)))
  nil)

#+comment ;; untested
(defmethod %stream-copy-bytes ((from-stream stream) (to-stream stream:buffered-stream) n-bytes)
  (let ((remaining-input-bytes n-bytes))
    (loop (when (eql remaining-input-bytes 0)
	    (return))
	  (loop (stream:with-stream-output-buffer (output-buffer output-index output-limit)
		    to-stream
		  (if (>= output-index output-limit)
		      (stream:stream-flush-buffer to-stream)
		    (let* ((test-output-limit (if remaining-input-bytes
						  (+ output-index remaining-input-bytes)
						output-limit))
			   (new-output-limit (if (> test-output-limit output-limit)
						 output-limit
					       test-output-limit))
			   (output-length (- new-output-limit output-index)))
		      (loop for index from output-index to new-output-limit
			    for byte = (read-byte from-stream n-bytes nil)
			    when (null byte)
			    do (progn
				 (setq new-output-limit index)
				 (setq output-length (- new-output-limit output-index))
				 (setq remaining-input-bytes 0)
				 (return))
			    do (setf (schar output-buffer index)
				     (code-char byte)))
		      (setf output-index new-output-limit)
		      (when (>= output-index output-limit)
			(stream:stream-flush-buffer to-stream))
		      (when remaining-input-bytes
			(decf remaining-input-bytes output-length))
		      (when (eql remaining-input-bytes 0)
			(return))))))))
  nil)

#+LispWorks4.0   ; post 4.0, file-stream is a stream:buffered-stream
(defmethod %stream-copy-bytes ((from-stream file-stream)
                               (to-stream stream:buffered-stream)
                               n-bytes)
  (multiple-value-bind (existing-buffer existing-index existing-limit)
      (stream-grab-existing-input-buffer from-stream n-bytes)
    (loop (when (eql n-bytes 0)
            (return))
          (stream:with-stream-output-buffer (output-buffer output-index output-limit)
              to-stream
            (if (>= output-index output-limit)
                (stream:stream-flush-buffer to-stream)
              (let ((output-length
                     (if (> existing-limit existing-index)
                         (let* ((remaining-input-bytes (- existing-limit existing-index))
                                (test-output-limit (+ output-index remaining-input-bytes))
                                (new-output-limit (if (> test-output-limit output-limit)
                                                      output-limit
                                                    test-output-limit))
                                (output-length (- new-output-limit output-index)))
                           (do ((input-index existing-index (1+ input-index))
                                (output-index output-index (1+ output-index)))
                               ((eql output-index new-output-limit))
                             (setf (schar output-buffer output-index)
                                   (char existing-buffer input-index)))
                           (incf existing-index output-length)
                           (setf output-index new-output-limit)
                           output-length)
                       (let ((output-length (sys::read-binary-bytes from-stream output-buffer
                                                                    (- output-limit output-index)
                                                                    output-index)))
                         (when (eql output-length 0)
                           (if n-bytes
                               (error 'end-of-file-while-copying
                                      :strean from-stream
                                      :bytes n-bytes)
                             (return)))
                         (incf output-index output-length)
                         output-length))))
                (when n-bytes
                  (decf n-bytes output-length)))))))
  nil)

#+LispWorks4.0   ; post 4.0, file-stream is a stream:buffered-stream
(defmethod %stream-copy-bytes ((from-stream stream:buffered-stream)
                               (to-stream file-stream)
                               n-bytes)
  (force-output to-stream)		; serialize, to allow unbuffered output
  (loop-over-stream-input-buffer (input-buffer input-index input-limit input-bytes)
      (from-stream n-bytes)
    (sys::write-binary-bytes to-stream input-buffer input-bytes input-index)
    (incf input-index input-bytes))
  nil)

#+CAPI
(defmethod http:stream-copy-until-eof ((from-stream stream:buffered-stream) (to-stream editor::rubber-stream) &optional (copy-mode :text))
  (loop-over-stream-input-buffer (input-buffer input-index input-limit input-bytes)
      (from-stream nil)
    (let ((input-end (+ input-index input-bytes)))
      (declare (fixnum input-end) 
               #+CL-HTTP-Untested (optimize (safety 0)))
      (loop for index fixnum from input-index below input-end
            for char = (schar input-buffer index)
	    do (cond ((member char '(#\return #\linefeed))
                      (when (eql char #\newline)
                        (terpri to-stream)))
                     (t (write-char char to-stream))))
      (setf input-index input-end))))

#+LispWorks4.0   ; post 4.0, file-stream is a stream:buffered-stream
(defmethod stream-grab-existing-input-buffer ((stream file-stream)
					      n-bytes)
  (let* ((buffer (io::file-stream-buffer stream))
	 (index (io::file-stream-index stream))
	 (limit (length buffer))
	 (new-index (if n-bytes
			(min limit (the fixnum (+ index (the fixnum n-bytes))))
		      limit)))
    (declare (fixnum index limit new-index))
    (setf (io::file-stream-index stream) new-index)
    (values buffer
	    index
	    new-index)))

#-(or LispWorks3.2 LispWorks4)
(defmethod http::input-available-on-streams-p ((streams cons) wait-reason timeout)
  (sys::wait-for-input-streams-returning-first streams :wait-reason wait-reason :timeout timeout))

(defmethod http::stream-copy-input-buffer ((from-stream stream:buffered-stream) (to-stream stream:buffered-stream))
  (loop (stream:with-stream-input-buffer 
            (input-buffer input-index input-limit)
            from-stream
          (if (>= input-index input-limit)
              (stream:stream-fill-buffer from-stream)
            (let ((remaining-input-bytes (- input-limit input-index)))
              (loop (stream:with-stream-output-buffer 
                        (output-buffer output-index output-limit)
                        to-stream
                      (if (>= output-index output-limit)
                          (stream:stream-flush-buffer to-stream)
                        (let* ((test-output-limit (+ output-index remaining-input-bytes))
                               (new-output-limit (if (> test-output-limit output-limit) output-limit test-output-limit))
                               (output-length (- new-output-limit output-index)))
                          (%copy-buffer output-buffer input-buffer output-index input-index new-output-limit)
                          (incf input-index output-length)
                          (setf output-index new-output-limit)
                          (when (>= output-index output-limit)
                            (stream:stream-flush-buffer to-stream))
                          (decf remaining-input-bytes output-length)
                          (when (eql remaining-input-bytes 0)
                            (return))))))
              (return))))))

(defmethod http::advance-input-buffer ((stream stream:buffered-stream) &optional delta)
  (loop-over-stream-input-buffer (input-buffer input-index input-limit input-bytes)
      (stream delta)
    input-buffer ; ignore this way because a declare ignore does not work here
    (incf input-index input-bytes)))

(defmacro element-type-ecase (stream &body body)
  `(let ((element-type (stream-element-type ,stream)))
     (cond ,.(loop for (elt-type . forms) in body
                   for test = (etypecase elt-type (symbol 'eq) (cons 'equal))
                   collect `((,test element-type ',elt-type) ,@forms))
           (t (error "Unknown element type, ~S, for the stream, ~S." element-type ,stream)))))

(defun %binary-to-8bit-char-copy-buffers (output-buffer input-buffer output-index input-index new-output-limit)
  (declare (fixnum output-index input-index new-output-limit)
           (type (simple-base-string) output-buffer)
           (type (array (unsigned-byte 8) (*)) input-buffer)  ; not simple-array
           #-CL-HTTP-Debugging (optimize (safety 0)))
  (do ((input-index input-index (1+ input-index))
       (output-index output-index (1+ output-index)))
      ((eql output-index new-output-limit))
    (setf (schar output-buffer output-index) (code-char (aref input-buffer input-index)))))

(defmethod 8-bit-buffer-input-function ((stream stream:buffered-stream))
  (element-type-ecase stream
    ((unsigned-byte 8) #'%binary-to-8bit-char-copy-buffers)
    (base-char  #'%binary-to-8bit-char-copy-buffers)
    (lw:simple-char (error "Multi-byte characters are not implmented yet."))))

(defmethod 8-bit-buffer-input-function ((stream comm:socket-stream))
 (element-type-ecase stream
   (base-char #'%binary-to-8bit-char-copy-buffers)
   ((unsigned-byte 8) #'%binary-copy-buffer)))

(defmethod http::binary-stream-copy-from-8-bit-array (from-array (to-stream stream:buffered-stream) &optional (start 0) end)
  (let* ((input-index start)
         (input-limit (or end (length from-array)))
         (remaining-input-bytes (- input-limit input-index))
         (copy-buffer-function (8-bit-buffer-input-function to-stream)))
    (declare (fixnum input-index input-limit remaining-input-bytes))
    (loop doing (stream:with-stream-output-buffer (output-buffer output-index output-limit)
                    to-stream
                  (declare (fixnum output-index output-limit))
                  (cond ((>= output-index output-limit) (stream:stream-flush-buffer to-stream))
                        (t (let* ((output-length (min (- output-limit output-index) remaining-input-bytes))
                                  (new-output-limit (+ output-index output-length)))
                             (declare (fixnum output-length new-output-limit))
                             (funcall copy-buffer-function output-buffer from-array output-index input-index new-output-limit)
                             (incf input-index output-length)
                             (setf output-index new-output-limit)
                             (when (>= output-index output-limit)
                               (stream:stream-flush-buffer to-stream))
                             (decf remaining-input-bytes output-length)
                             (when (eql remaining-input-bytes 0)
                               (return)))))))))

(defun %8bit-char-to-binary-copy-buffer (output-buffer input-buffer output-index input-index new-output-limit)
  (declare (fixnum output-index input-index new-output-limit)
           #-CL-HTTP-Debugging (optimize (safety 0)))
  (do ((input-index input-index (1+ input-index))
       (output-index output-index (1+ output-index)))
      ((eql output-index new-output-limit))
    (setf (aref output-buffer output-index) (char-code (schar input-buffer input-index)))))

(defmethod 8-bit-buffer-output-function ((stream stream:buffered-stream))
  (element-type-ecase stream
    (character #'%8bit-char-to-binary-copy-buffer)
    ((unsigned-byte 8) #'%8bit-char-to-binary-copy-buffer)
    (lw:simple-char (error "Multi-byte characters are not implmented yet for ~S streams." (type-of stream)))))

(defmethod 8-bit-buffer-output-function ((stream comm:socket-stream))
  (element-type-ecase stream
    (base-char #'%8bit-char-to-binary-copy-buffer)
    ((unsigned-byte 8) #'%binary-copy-buffer)))

(defmacro handler-case-if (condition form &body clauses)
  `(flet ((execute-form () ,form))
     (declare (inline execute-form))
     (cond (,condition
            (handler-case (execute-form) ,@clauses))
           (t (execute-form)))))

(defmethod http::binary-stream-copy-into-8-bit-array ((from-stream stream:buffered-stream) n-bytes &optional (start 0) 8-bit-array
                                                      &aux (size 0))
  (declare (fixnum start size))
  (flet ((make-the-array (size fill-pointer)
           (make-array size :fill-pointer fill-pointer :adjustable t :element-type '(unsigned-byte 8)))
         (adjust-the-array (array size fill-pointer)
           (let ((new-array (adjust-array array size :fill-pointer fill-pointer :element-type '(unsigned-byte 8))))
             #+testing(unless (eq new-array array) (format t "New array in adjustment."))
             new-array))
         (new-size (size)
           (cond ((< size 64000) (* 2 size))
                 (t (truncate (* size 1.2))))))
    (declare (inline make-the-array adjust-the-array new-size))
    (multiple-value-bind (copy-buffer-function)
           (8-bit-buffer-output-function from-stream)
      (cond (n-bytes
             (locally
               (declare (fixnum n-bytes))
               (setq size (+ n-bytes start))
               (cond ((null 8-bit-array)
                      (setq 8-bit-array (make-the-array size start)))
                     ((< (array-total-size 8-bit-array) size)
                      (setq 8-bit-array (adjust-the-array 8-bit-array size start))))
               (let ((fill-pointer start)
                     output-limit)
                 (declare (fixnum fill-pointer))
                 (loop-over-stream-input-buffer (input-buffer input-index input-limit input-bytes)
                     (from-stream n-bytes)
                   (setq output-limit (+ fill-pointer input-bytes))
                   (funcall copy-buffer-function 8-bit-array input-buffer fill-pointer input-index output-limit)
                   (setf input-index (+ input-index input-bytes)
                         fill-pointer output-limit)))
               (values 8-bit-array (setf (fill-pointer 8-bit-array) (+ start n-bytes)))))
            ;; the size and growth issues are open to experimentation and better
            ;; algorithms that do less work.  7/26/95 -- JCMa.
            (t (cond ((null 8-bit-array)
                      (setq size (+ 1000 start)
                            8-bit-array (make-the-array size start)))
                     (t (setq size (array-total-size 8-bit-array))))
               (let ((fill-pointer start)
                     output-limit)
                 (declare (fixnum fill-pointer))
                 (loop with chunking-p = (chunked-transfer-decoding-p from-stream)
                       doing (stream:with-stream-input-buffer (input-buffer input-index input-limit)
                                 from-stream 
                               (cond ((>= input-index input-limit)
                                      (unless (handler-case-if chunking-p
                                                  (stream:stream-fill-buffer from-stream) ;advance input buffer
                                                (www-utils:end-of-chunk-transfer-decoding () nil)) ;catch end of chunk transfer decoding
                                        (return)))
                                     (t (setq output-limit (+ fill-pointer (the fixnum (- input-limit input-index))))
                                        (when (> output-limit size)
                                          (setq 8-bit-array (adjust-the-array 8-bit-array (setq size (new-size size)) fill-pointer)))
                                        (funcall copy-buffer-function 8-bit-array input-buffer fill-pointer input-index output-limit)
                                        (setf input-index input-limit
                                              fill-pointer output-limit)))))
                 (values 8-bit-array (setf (fill-pointer 8-bit-array) fill-pointer))))))))

;; Primitive for reading delimited lines which does not reset the fill-pointer
(defun www-utils::%buffered-stream-read-delimited-line (stream delimiters eof buffer)
  (declare (optimize (speed 3)))
  (flet ((do-it (stream delimiters eof buffer)
           (declare (type string buffer)
                    (type stream:buffered-stream stream)
                    (type cons delimiters))
           (let* ((size (array-total-size buffer))
                  (start (fill-pointer buffer))
                  (fill-pointer start)
                  eof-p delimiter)
             (declare (fixnum size start fill-pointer))
             (loop named buffer-feed
                   with chunking-p = (chunked-transfer-decoding-p stream)
                   doing (stream:with-stream-input-buffer (input-buffer input-index input-limit)
                             stream 
                           (cond ((>= input-index input-limit)
                                  (unless (handler-case-if chunking-p
                                              (stream:stream-fill-buffer stream) ;advance input buffer
                                            (www-utils:end-of-chunk-transfer-decoding () nil)) ;catch end of chunk transfer decoding
                                    (unless delimiter (setq eof-p t)) ;line is good if we found the first delimiter
                                    (return-from buffer-feed)))
                                 (t (locally
                                      #+CL-HTTP-Untested (declare (optimize (safety 0)))
                                      (loop named fill-buffer
                                            for input-idx fixnum upfrom input-index below input-limit
                                            for char = (schar input-buffer input-idx)
                                            do (cond (delimiter
                                                      (if (and (member char delimiters) (not (eql delimiter char)))
                                                          (setf input-index (1+ input-idx)) ;advance buffer index
                                                        (setf input-index input-idx));set buffer index to current char
                                                      (return-from buffer-feed))
                                                     ((member char delimiters) ;set delimiter flag
                                                      (setq delimiter char))
                                                     (t (unless (< fill-pointer size) ;grow the copy buffer
                                                          (setq size (floor (* (the fixnum size) 1.2))
                                                                buffer (adjust-array buffer size :element-type (array-element-type buffer))))
                                                        (setf (aref buffer fill-pointer) char) ;fill copy buffer
                                                        (incf fill-pointer)))  
                                            finally (setf input-index input-limit))))))) ;advance buffer index
             (if (= start fill-pointer)           ; no data read
                 (values (if eof-p eof buffer) eof-p delimiter (fill-pointer buffer))
               (values buffer eof-p delimiter (setf (fill-pointer buffer) fill-pointer))))))
    (cond (buffer
           (do-it stream delimiters eof buffer))
          (t (resources:using-resource (line-buffer http::line-buffer http::*line-buffer-size*)
               (multiple-value-bind (buf eof-p delim length)
                   (do-it stream delimiters eof line-buffer)
                 (values (if eof-p eof (subseq buf 0 length)) eof-p delim length)))))))

(defmethod www-utils:read-delimited-line ((stream stream:buffered-stream) &optional (delimiters '(#\Return #\Linefeed)) eof buffer)
  (when buffer (setf (fill-pointer buffer) 0)) ;callers depend on zero-based start
  (www-utils::%buffered-stream-read-delimited-line stream delimiters eof buffer))

(defmethod http::crlf-stream-copy-into-string ((stream stream:buffered-stream) &optional n-bytes (start 0) string)
  (flet ((make-the-string (size fill-pointer)
	   (make-array size :fill-pointer fill-pointer :adjustable t :element-type http::*standard-character-type*))
	 (adjust-the-string (string size fill-pointer)
	   (adjust-array string size :fill-pointer fill-pointer :element-type (array-element-type string)))
	 (new-size (size)
	   (cond ((< size 64000) (* 2 size))
		 (t (truncate (* size 1.2))))))
    (declare (inline make-the-string adjust-the-string new-size))
    (macrolet ((push-char (char string index delimiter)
                 `(cond ((member ,char '(#\return #\linefeed))
                         (cond ((and ,delimiter (not (eql ,char ,delimiter)))
                                (setq ,delimiter nil))
                               (t (setq ,delimiter ,char) ;update new delimiter
                                  (setf (aref ,string ,index) #\newline) ; ANSI CL line terminator
                                  (incf ,index))))
                        (t (setf (aref ,string ,index) ,char)
                           (incf ,index)))))
      (let ((fill-pointer start)
            (size 0)
            delimiter)
        (declare (fixnum fill-pointer size))
        (cond (n-bytes
               (setq size (+ n-bytes start))
               (cond ((null string)
                      (setq string (make-the-string size start)))
                     ((< (array-total-size string) size)
                      (setq string (adjust-the-string string size fill-pointer))))
               (loop-over-stream-input-buffer (input-buffer input-index input-limit input-bytes)
                   (stream n-bytes)
                 (let ((input-end (+ input-index input-bytes)))
                   (declare (fixnum input-end)
                            #+CL-HTTP-Untested (optimize (safety 0)))
                   (loop for index fixnum from input-index below input-end
                         for char = (schar input-buffer index)
                         do (push-char char string fill-pointer delimiter))
                   (setf input-index input-end))))
              ;; the size and growth issues are open to experimentation and better
              ;; algorithms that do less work.  7/26/95 -- JCMa.
              (t (cond ((null string)
                        (setq size (+ 1000 start)
                              string (make-the-string size start)))
                       (t (setq size (array-total-size string))))
                 (loop with chunking-p = (chunked-transfer-decoding-p stream)
                       doing (stream:with-stream-input-buffer (input-buffer input-index input-limit)
                                 stream 
                               (declare (fixnum input-index input-limit))
                               (cond ((>= input-index input-limit) 
                                      (unless (handler-case-if chunking-p
                                                  (stream:stream-fill-buffer stream) ;advance input buffer
                                                (www-utils:end-of-chunk-transfer-decoding () nil))
                                        (return)))
                                     (t (locally
                                          #+CL-HTTP-Untested (declare (optimize (safety 0)))
                                          (loop for index fixnum upfrom input-index below input-limit
                                                for char = (schar input-buffer index)
                                                do (when (= size fill-pointer)
                                                     (setq string (adjust-the-string string (setq size (new-size size)) fill-pointer))) 
                                                do (push-char char string fill-pointer delimiter)))
                                        (setf input-index input-limit)))))))
        ;; return the string and actual fill pointer
        (values string (setf (fill-pointer string) fill-pointer))))))

;; convenient abstraction
(defun %stream-standardize-line-breaks (from-stream to-stream line-break-char &optional n-bytes &aux delimiter)
  (declare (type stream:buffered-stream from-stream)

           (type stream:buffered-stream to-stream))
  (macrolet ((push-char (char string index delimiter)
               `(cond ((member ,char '(#\return #\linefeed))
                       (cond ((and ,delimiter (not (eql ,char ,delimiter)))
                              (setq ,delimiter nil))
                             (t (setq ,delimiter ,char) ;update new delimiter
                                (setf (aref ,string ,index) line-break-char) ;insert the line break character
                                (incf ,index))))
                      (t (setf (aref ,string ,index) ,char)
                         (incf ,index)))))
    (ipc::loop-over-stream-input-buffer (input-buffer input-index input-limit input-bytes)
        (from-stream n-bytes)
      (loop with remaining-input-bytes fixnum = input-bytes
            doing (stream:with-stream-output-buffer (output-buffer output-index output-limit)
                      to-stream
                    (cond ((>= output-index output-limit) (stream:stream-flush-buffer to-stream))
                          (t (locally
                               #+CL-HTTP-Untested (declare (optimize (safety 0)))
                               (loop with output-idx fixnum = output-index
                                     with input-size fixnum = (min (- output-limit output-index) remaining-input-bytes)
                                     with input-end fixnum = (+ input-index input-size)
                                     for index fixnum from input-index below input-end
                                     for char = (schar input-buffer index)
                                     do (push-char char output-buffer output-idx delimiter)
                                     finally (progn 
                                               (decf remaining-input-bytes input-size) 
                                               (setf input-index input-end
                                                     output-index output-idx))))
                             (when (>= output-index output-limit)
                               (stream:stream-flush-buffer to-stream))
                             (when (eql remaining-input-bytes 0)
                               (return)))))))))

(defmethod http::stream-decode-crlf-bytes ((from-stream stream:buffered-stream) (to-stream stream:buffered-stream) n-bytes)
 (%stream-standardize-line-breaks from-stream to-stream #\newline n-bytes)) ; ANSI CL line terminator

(defmethod http::stream-decode-crlf-until-eof ((from-stream stream:buffered-stream) (to-stream stream:buffered-stream))
  (%stream-standardize-line-breaks from-stream to-stream #\newline nil)) ; ANSI CL line terminator

(defmethod http::stream-standardize-line-breaks-until-eof ((from-stream stream:buffered-stream) (to-stream stream:buffered-stream) 
                                                           (line-break (eql :cr)))
  (%stream-standardize-line-breaks from-stream to-stream #\return nil))

(defmethod http::stream-standardize-line-breaks-until-eof ((from-stream stream:buffered-stream) (to-stream stream:buffered-stream)
                                                           (line-break (eql :lf)))
  (%stream-standardize-line-breaks from-stream to-stream #\linefeed nil))

(defmethod http::stream-standardize-line-breaks-until-eof ((from-stream stream:buffered-stream) (to-stream stream:buffered-stream)
                                                           (line-break (eql :crlf)))
  (http::stream-encode-crlf-until-eof from-stream to-stream))

;; This should produce a correct CRLF when the input is a CRLF stream!
;; Version by Martin 9/25/2003
(defun %buffered-stream-write-string (stream string start end)
  (declare (type string string)
           (type stream:buffered-stream stream)
           (fixnum start end))
  (macrolet ((%push-char (char buffer index)
               `(prog2 (setf (aref ,buffer ,index) ,char)
                    (incf ,index)))
             (push-char (char buffer index limit at-cr-p) ; Even when receiving CRLF input, make this transmit a CRLF stream.
               `(cond ((member ,char '(#\return #\linefeed))
                       (%push-char #\Return ,buffer ,index)
                       (cond ((< ,index ,limit)
                              (%push-char #\Linefeed ,buffer ,index))
                             (t (setq ,at-cr-p t) ;end of output buffer
                                nil)))
                      (t (%push-char ,char ,buffer ,index)))))
    (let ((input-index start)
           at-cr-p)
      (declare (fixnum input-index))
      (loop (stream:with-stream-output-buffer (output-buffer output-index output-limit)
                stream
              (cond ((>= output-index output-limit)
                     (stream:stream-flush-buffer stream))
                    (t (let ((output-idx output-index)
                             (index input-index))
                         (loop (let ((char (schar string index)))
                                 (incf index)
                                 (unless (push-char char output-buffer output-idx output-limit at-cr-p)
                                   (return))
                                 (when (or (>= index end)
                                           (>= output-idx output-limit))
                                   (return))))
                         (setf input-index index
                               output-index output-idx))
                       (when (>= output-index output-limit)
                         (stream:stream-flush-buffer stream)))))
            (when at-cr-p
              (stream:with-stream-output-buffer (output-buffer output-index output-limit)
                  stream
                output-limit ;ignore? What about zero length buffers?
                (%push-char #\Linefeed output-buffer output-index)
                (setq at-cr-p nil)))
            (when (>= input-index end)
              (return)))
      (values input-index))))

(defmethod http::stream-encode-crlf-until-eof ((from-stream stream:buffered-stream) (to-stream stream:buffered-stream))
  #+later(declare (optimize (safety 0)))
  (loop with chunking-p = (chunked-transfer-decoding-p from-stream)
        doing (stream:with-stream-input-buffer (input-buffer input-index input-limit)
                  from-stream 
                (declare (fixnum input-index input-limit))
                (cond ((>= input-index input-limit)
                       (unless (handler-case-if chunking-p
                                   (stream:stream-fill-buffer from-stream) ;advance input buffer
                                 (www-utils:end-of-chunk-transfer-decoding () nil))
                         (return)))
                      (t (let ((new-index (%buffered-stream-write-string to-stream input-buffer input-index input-limit)))
                           (declare (fixnum new-index))
                           (setf input-index new-index)))))))

(defmethod http::write-vector ((stream chunk-transfer-encoding-output-stream) (vector string) &optional (start 0) (end (length vector)))
  (%buffered-stream-write-string stream vector start end)
  vector)

(defmethod http::write-vector ((stream stream:buffered-stream) (vector vector) &optional (start 0) (end (length vector)))
  (cond ((equal `(unsigned-byte 8)(array-element-type vector))
         (http::binary-stream-copy-from-8-bit-array vector stream start end)
         vector) ;return the vector
        (t (call-next-method))))

;;;-----------------------------------------------------------------------------------
;;; 
;;;

(defun www-utils:process-wait-for-stream (wait-reason stream &optional wait-function timeout)
  (declare (optimize (speed 3)))
  (mp::process-wait-for-input-stream stream
				     :wait-function wait-function
                                     :wait-reason wait-reason
                                     :timeout timeout))

(defmethod http::http-input-data-available-p ((stream cl-http-chunk-transfer-socket-stream) &optional timeout-seconds)
  "Returns non-null when input data is available on the HTTP STREAM within
TIMEOUT-SECONDS.  When timeout-seconds is null, data must be immediately
available. A dead HTTP connection means no data is available.
Ports can specialize this as necessary for their stream and process implementations."
  (labels ((data-available-p (stream)
             (loop for char = (when (listen stream)
                                (peek-char nil stream nil))
                   while char                   ;clear any dangling White Space due to buggy clients.
                   when (member char '(#\return #\linefeed #\space #\tab) :test #'eql)
                     do (read-char stream t)
                   else
                     return t                   ;something still there.
                   finally (return nil)))
           (continue-p (stream)
             (or (not (www-utils:live-connection-p stream))     ;connection went dead
                 (data-available-p stream))))   ;data available
    (declare (inline data-available-p))
    (cond ((not (www-utils:live-connection-p stream)) nil)
          ((data-available-p stream) t)
	  #-LispWorks
          ((and timeout-seconds (not (zerop timeout-seconds)))
           ;; Block until there is reason to take action
           (process-wait-with-timeout
             "HTTP Request Wait" timeout-seconds #'continue-p stream)
           ;; Determine whether input data was available without consing.
           (and (www-utils:live-connection-p stream)
                (listen stream)))
	  #+LispWorks
	  ((and timeout-seconds (not (zerop timeout-seconds)))
           ;; Block until there is reason to take action
	   (loop (unless (www-utils:live-connection-p stream)
		   (return nil))
		 (when (data-available-p stream)
		   (return t))
		 (unless (www-utils:process-wait-for-stream
			  "HTTP Request Wait" stream
			  nil timeout-seconds)
		   ;; Timeout expired
		   (return nil))))
          (t nil)))) 

(defmacro www-utils:with-binary-stream ((stream direction) &body body)
  "Turns STREAM into a binary stream within the scope of BODY.
direction can be :OUTPUT, :INPUT, or :BOTH."
  (declare (ignore stream direction))
  `(progn ,@body))

(defmacro www-utils:with-text-stream ((stream direction) &body body)
  "Turns STREAM into a text stream within the scope of BODY.
direction can be :OUTPUT, :INPUT, or :BOTH."
  (declare (ignore stream direction))
  `(progn ,@body))

(defmacro www-utils:with-crlf-stream ((stream direction) &body body)
  "Turns STREAM into a CRLF stream within the scope of BODY.
direction can be :OUTPUT, :INPUT, or :BOTH."
  (declare (ignore stream direction))
  `(progn ,@body))

(defun www-utils:live-connection-p (http-stream)
  "Returns non-null if the TCP/IP connection over HTTP-STREAM remains alive
in that the remote host continue to respond at the TCP/IP level."
  (and (open-stream-p http-stream)
       (not (stream:stream-check-eof-no-hang http-stream))))

(declaim (inline www-utils:abort-http-stream))

(defun www-utils:abort-http-stream (http-stream)
  "Closes http-stream in abort mode.  
This will push any output in the transmit buffer and catch any network errors.
Takes care to clean up any dangling pointers."
  (handler-case 
    (close http-stream :abort t)
    (www-utils:network-error ())))

;;; ----------------------------------------------------------------------------------
;;; Server side chunking
;;;
(eval-when (:compile-toplevel :execute)
(defmacro fixed-crlf-string (count &key prefix)
  `(load-time-value
    (coerce ',(append prefix
		      (loop repeat count
			    append '(#\Return #\Linefeed)))
	    'string)))
)

(defmethod www-utils:chunk-transfer-encoding-mode ((stream chunk-transfer-encoding-output-stream)
						   &optional function)
  (with-slots (chunk-function) stream
    (check-type function (or null function))
    (setf chunk-function function)))

(defmethod www-utils:note-first-chunk ((stream chunk-transfer-encoding-output-stream))
  (with-slots (chunk-start-index chunks-transmitted) stream
    (unless chunks-transmitted
      ;; write anything not part of the chunk
      (force-output stream)
      (stream:with-stream-output-buffer (buffer index limit) stream
        ;; Insert the CRLF part of the chunk prefix.
        (%buffer-insert-crlf buffer (the fixnum (+ index 4)))
        ;; Advance the buffer past the chunk prefix.
        (incf index 6)
        ;; Leave space in the buffer for the CRLF chunk suffix.
        (decf limit 2)
        (setf chunk-start-index index)
        ;; turn on chunking
        (setf chunks-transmitted 0)))))

(defmethod www-utils:note-last-chunk ((stream chunk-transfer-encoding-output-stream)
                                      &optional footers-plist)
  (with-slots (chunks-transmitted) stream
    (when chunks-transmitted
      ;; write the last chunk
      (force-output stream)
      ;; Restore index and limit.
      (stream:with-stream-output-buffer (buffer index limit) stream
        buffer ;ignore
        (decf index 6)
        (incf limit 2))
      ;; turn off chunking and write the Chunked-Body terminator (assumes
      ;;  that :transfer-encoding :chunked was written by caller)
      (setf chunks-transmitted nil)
      (write-string (fixed-crlf-string 1 :prefix (#\0))
                    stream)
      (http::write-headers stream footers-plist t))))

;; Make note-last-chunk do nothing in future calls e.g. unwinding.
(defmethod close :after ((stream chunk-transfer-encoding-output-stream) &key abort)
  (declare (ignore abort))
  (with-slots (chunks-transmitted) stream
    (setf chunks-transmitted nil)))

(defmethod stream:stream-flush-buffer ((stream chunk-transfer-encoding-output-stream))
  (with-slots (chunks-transmitted chunk-start-index) stream
    (if chunks-transmitted ; Are we chunking?
        (stream:with-stream-output-buffer (buffer index limit) stream
          (cond ((or (< index chunk-start-index) ; sanity check
                     (> index limit)) ; e.g. closed stream
                 (let ((bad-index index)
                       (bad-limit limit))
                   (call-next-method) ;try to get system error first
                   (error "Output buffer too full to insert chunk size marker ~S index ~D limit ~D chunk-start-index ~D."
                          stream bad-index bad-limit chunk-start-index)))
                ((> index chunk-start-index) ; non-empty chunk
                 (let ((start (%buffer-insert-chunk-size buffer chunk-start-index index)))
                   (%buffer-insert-crlf buffer index)
                   (stream:stream-write-buffer stream buffer start (+ index 2)))
                 (incf chunks-transmitted)
                 (setf index chunk-start-index))))
      (call-next-method))))

(defun %buffer-insert-crlf (buffer index)
  (declare (fixnum index))
  (setf (schar buffer index) (code-char 13))
  (setf (schar buffer (the fixnum (1+ index))) (code-char 10)))

(defun %buffer-insert-chunk-size (buffer start end)
  (declare (fixnum start end))
  (let* ((size (- end start))
         (index (- start 2)))
    (declare (fixnum size index))
    (loop (decf index)
          (let ((digit (logand size 15)))
            (setf (schar buffer index)
                  (if (> digit 9)
                      (code-char (+ digit (- (char-code #\A) 10)))
                    (code-char (+ digit (char-code #\0))))))
          (setq size (ash size -4))
          (when (eql size 0)
            (return)))
    index))

;;; ----------------------------------------------------------------------------------
;;; CLIENT SIDE CHUNKING
;;;

(defvar *debug-client-chunking* nil)

(defmethod www-utils:chunk-transfer-decoding-mode ((stream chunk-transfer-decoding-input-stream))
  (with-slots (chunk-length-received chunk-remaining-bytes chunk-real-buffer-limit) stream
    (setf chunk-length-received 0
          chunk-real-buffer-limit nil
          chunk-remaining-bytes 0) ;ensure we enter stream:stream-fill-buffer on next read
    (%set-buffer-chunk-limit stream)))

(defmethod www-utils:chunk-transfer-decoding-mode-end ((stream chunk-transfer-decoding-input-stream))
  (with-slots (chunk-remaining-bytes chunk-real-buffer-limit) stream
    ;; Restore the buffer limit in case more real data follows.
    (when chunk-real-buffer-limit
      (stream:with-stream-input-buffer (buffer index limit) stream
        buffer index ;ignore
        (setf limit chunk-real-buffer-limit
              chunk-real-buffer-limit nil)))
    (setq chunk-remaining-bytes nil)))

(defmethod www-utils:chunk-transfer-content-length ((stream chunk-transfer-decoding-input-stream))
  (with-slots (chunk-remaining-bytes chunk-length-received) stream
    (if chunk-remaining-bytes
        chunk-length-received
      (error "~S is not in chunked transfer decoding mode." stream))))

;; Returns non-null when the stream is decoding chunked input -- JCMa 9/10/2003
(defmethod chunked-transfer-decoding-p ((stream chunk-transfer-decoding-input-stream))
  (with-slots (chunk-remaining-bytes) stream
    (not (null chunk-remaining-bytes))))

(defmethod chunked-transfer-decoding-p (stream)
  (declare (ignore stream))
  nil)

(defmethod stream:stream-fill-buffer ((stream chunk-transfer-decoding-input-stream))
  (with-slots (chunk-length-received chunk-remaining-bytes chunk-real-buffer-limit) stream
    (if chunk-remaining-bytes ; Are we chunking?
        (when (if (eq chunk-remaining-bytes :eof) ; end of sequence of chunks?
                  ;; Continue to signal eoc until chunk-transfer-decoding-mode-end.
                  (signal 'www-utils:end-of-chunk-transfer-decoding :stream stream)
                (if (eql chunk-remaining-bytes 0) ; end of chunk?
                    (progn
                      ;; Restore the buffer limit in case more chunk data follows.
                      (when chunk-real-buffer-limit
                        (stream:with-stream-input-buffer (buffer index limit) stream
                          buffer index ;ignore
                          (setf limit chunk-real-buffer-limit)))
                      ;; Assume another one follows.  Condition will be signaled if not.
                      (%parse-chunk-header stream
                                           #'(lambda (stream)
                                               (declare (ignore stream))
                                               (call-next-method))
                                           (not (eql chunk-length-received 0))))
                  (call-next-method)))
          (%set-buffer-chunk-limit stream)
          t)
      (call-next-method))))

(defun %set-buffer-chunk-limit (stream)
  (declare (type chunk-transfer-decoding-input-stream stream))
  (with-slots (chunk-remaining-bytes chunk-real-buffer-limit) stream
    (stream:with-stream-input-buffer (buffer index limit) stream
      buffer ;ignore
      (let ((chunk-end-index (+ index chunk-remaining-bytes)))
        (if (< chunk-end-index limit)
            ;; Chunk ends within the buffer, so record the
            ;; real limit and put an artificial limit in the
            ;; stream so stream:stream-fill-buffer is called
            ;; again at the end of the chunk.
            (setf chunk-real-buffer-limit limit
                  limit chunk-end-index
                  chunk-remaining-bytes 0)
          ;; Chunk ends after the limit so allow the whole
          ;; buffer to be consumed.
          (setf chunk-real-buffer-limit nil
                chunk-remaining-bytes (- chunk-remaining-bytes (- limit index))))
        (when *debug-client-chunking*
          (format t "~&;; Client chunk section ~D~%" (- limit index)))))))

#+comment ;MJS 19Jun98: a more pernickety version
(defun %parse-chunk-header-size (stream buffer-fill-function skip-first-crlf)
  ;; The next bytes in the stream are supposed to be a chunk header.
  (declare (type chunk-transfer-decoding-input-stream stream))
  (macrolet ((want-char (want)
               `(unless (char= ch ,want)
                  (error "Chunk decoding error: wanted ~S, got ~S in ~S ~S ~S"
                         ,want ch want-cr want-lf parsing-size)))
             (adjust-size (baseline)
               `(setq size (+ (ash size 4)
                              (- (char-code ch) ,baseline)))))
    (let ((size 0)
          (want-char (and skip-first-crlf #\Return)))
      (block found-size
        (loop (block refill-buffer
                (stream:with-stream-input-buffer (buffer index limit) stream
                  (loop (when (>= index limit)
                          (return-from refill-buffer))
                        (let ((ch (schar buffer index)))
                          (declare (character ch))
                          (incf index)
                          (cond ((eql ch want-char)
                                 (cond ((char= ch #\Return)
                                        (setq want-char #\Newline))
                                       (t  ; must have been #\Newline
                                        (if skip-first-crlf
                                            (setq skip-first-crlf nil
                                                  want-char nil)
                                          (return-from found-size)))))
                                (want-char
                                 (error "Chunk decoding error: wanted ~S, got ~S"
                                        want-char ch))
                                (t
                                 (cond ((char<= #\0 ch #\9)
                                        (adjust-size (char-code #\0)))
                                       ((char<= #\A ch #\Z)
                                        (adjust-size (- (char-code #\A) 10)))
                                       ((char<= #\a ch #\z)
                                        (adjust-size (- (char-code #\a) 10)))
                                       ((char= ch #\Return)
                                        (setq want-char #\Newline))
                                       ((char= ch #\Space)
                                        ;; Skip space, which some servers add erroneously
                                        )
                                       (t (error "Chunk decoding error: wanted size, got ~S"
                                                 ch)))))))))
              (unless (funcall buffer-fill-function stream)
                (return-from %parse-chunk-header-size nil))))
      size)))

(defun %parse-chunk-header-size (stream buffer-fill-function skip-first-crlf)
  ;; The next bytes in the stream are supposed to be a chunk header.
  (declare (type chunk-transfer-decoding-input-stream stream))
  (let ((size 0))
    (block found-size
      (loop (block refill-buffer
              (stream:with-stream-input-buffer (buffer index limit) stream
                (loop (when (>= index limit)
                        (return-from refill-buffer))
                      (let ((ch (schar buffer index)))
                        (declare (character ch))
                        (incf index)
                        (cond ((char<= #\0 ch #\9)
                               (setq size (+ (ash size 4)
                                             (- (char-code ch) (char-code #\0)))))
                              ((char<= #\A ch #\Z)
                               (setq size (+ (ash size 4)
                                             (- (char-code ch) (- (char-code #\A) 10)))))
                              ((char<= #\a ch #\z)
                               (setq size (+ (ash size 4)
                                             (- (char-code ch) (- (char-code #\a) 10)))))
                              ((char= ch #\Return)
                               ;; Skip return, assumed to be part of CRLF.
                               )
                              ((char= ch #\Newline)
                               (if skip-first-crlf
                                   (setq skip-first-crlf nil)
                                 (return-from found-size)))
                              ((char= ch #\Space)
                               ;; Skip space, which some servers add erroneously
                               )
                              (t
                               (error "Chunk decoding error got ~S"
                                      ch)))))))
            (unless (funcall buffer-fill-function stream)
              (return-from %parse-chunk-header-size nil))))
    size))

(defun %parse-chunk-header (stream buffer-fill-function skip-first-crlf)
  ;; The next bytes in the stream are supposed to be a chunk header.
  (declare (type chunk-transfer-decoding-input-stream stream))
  (let ((size (%parse-chunk-header-size stream buffer-fill-function skip-first-crlf)))
    (when size
      (when *debug-client-chunking*
        (format t "~&;; New client chunk ~D~%" size))
      (if (eql (the fixnum size) 0) ; end of sequence of chunks?
          (stream:with-stream-input-buffer (buffer index limit) stream
            buffer ;ignore
            (with-slots (chunk-remaining-bytes chunk-real-buffer-limit) stream
              ;; Put an artificial limit in the
              ;; stream so stream:stream-fill-buffer is called
              ;; again for each read.
              (setf chunk-real-buffer-limit limit
                    limit index
                    chunk-remaining-bytes :eof)
              (signal 'www-utils:end-of-chunk-transfer-decoding :stream stream)))
        (with-slots (chunk-length-received chunk-remaining-bytes) stream
          (setf chunk-remaining-bytes size)
          (incf chunk-length-received size)))
      t)))
