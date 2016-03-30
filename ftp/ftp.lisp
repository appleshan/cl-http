;;; -*- Mode: Lisp; Package: FTP; Syntax: Ansi-Common-Lisp; -*-
;;;
;;; FTP CLIENT SUBSTRATE FOR CL-HTTP PROXY, CLIENT, AND WEB WALKER
;;;
;;; Copyright 2006, John C. Mallery. 
;;; All rights reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; BASIC FTP CLIENT SUBSTRATE
;;;
;;; Documentation: 
;;;                FTP                     -- http://www.ietf.org/rfc/rfc959.txt
;;;                FTP security extensions -- http://www.ietf.org/rfc/rfc2228.txt
;;;                FTPS                    -- http://www.ietf.org/rfc/rfc4217.txt
;;;                FTP extensions          -- http://www.ietf.org/rfc/rfc2389.txt
;;;                MLST                    -- http://www.ietf.org/internet-drafts/draft-ietf-ftpext-mlst-16.txt
;;;
;;;                Index of docs           -- http://www.wu-ftpd.org/rfc/
(in-package :ftp)

;;;------------------------------------------------------------------- 
;;;
;;; VARIABLES AND UTILITIES
;;

(pushnew :cl-http-ftp-client *features*)

(defvar *ftp-standard-port* 21
  "The standard port used for the FTP control connection.")

(defvar *ftp-data-port* 20
  "The standard port used for FTP data transfer.")

(defun ftp-set-standard-port (port)
  "Set the standard FTP to PORT.
Updates the default data port as well."
  (prog1 (setq *ftp-standard-port* port)
    (setq *ftp-data-port* (1- port)))) ;RFC 959 says default data port is one below assigned port

(ftp-set-standard-port 21)

(defparameter *ftp-base-data-port* 1023
  "The alternate port used for FTP data transfer.")

(defparameter *ftp-data-port-range* 100
  "The range of ports sequentially above the alternate port that can be used for FTP data transfer.")

(defparameter *ftp-timeout* (* 60. 60. 5) ;5 minutes
  "Controls the amount of idle time before an idle FTP connection is dropped.")

(defparameter *ftp-default-user-id* "anonymous"
  "The default user-id used to login hen none is provided.")

(defparameter *ftp-default-password* nil
  "The default user-password used to login hen none is provided.")

(defun ftp-default-password ()
  "The default user-password used to login hen none is provided."
  (or *ftp-default-password* (user-mail-address)))

(defparameter *ftp-command-timeout* 1
  "The number of seconds to wait for 100 code replies from FTP servers.")

(defparameter *ftp-data-open-timeout* 30
  "The number of seconds to wait for FTP servers to open a data connection.")

(defparameter *trace-client* nil
  "Controls whether the FFTP session for client is traced on *trace-output*.")

(defparameter *debug-client* nil
  "Controls whether debugging is turned on for the FTP client.")

(define debug-client (&optional (debug (not *debug-client*)))
  "Toggles server debugging according to DEBUG.
DEBUG can be:

      T          Turns normal debugging on
      NIL        Turns debugging off"
  (setq *debug-client* (ecase debug
                         ((nil t) (not (null debug))))))

(defun element-type-for-mode (mode)
  (ecase mode
    (:binary '(unsigned-byte 8))
    (:text *standard-character-type*)))

(defun %intern-new-keyword (string &optional (start 0) (end (length string)))
  (intern (string-upcase string :start start :end end) *keyword-package*))

(tk1:define-tokenizer keyword
                      :tokenizer '%intern-new-keyword
                      :test 'char-equal
		      :size 70
                      :definer define
                      :documentation "Tokenizes FTP keywords without consing.")

(declaim (inline ftp-stream-input-data-available-p))

(defun ftp-stream-input-data-available-p (stream timeout)
  "Returns non-null when input data is available on the FTP STREAM within TIMEOUT seconds.  
When TIMEOUT is null, data must be immediately available. A dead FTP connection means no
data is available."
  (http-input-data-available-p stream timeout))

(defmacro ignore-first-line ((line start flag) &body body)
  `(cond ((and (not ,flag) (= ,start 4) (eql #\- (char ,line (1- ,start))))
          (setq ,flag t))
         (t . ,body)))

;;;------------------------------------------------------------------- 
;;;
;;; CONNECTION CLASS
;;;
 
(defclass ftp-connection
          (property-list-mixin)
  ((stream :initform nil :accessor ftp-stream :documentation "FTP control stream") 
   (host :initarg :host :accessor ftp-host :documentation "The remote host")
   (port :initarg :port :accessor ftp-port :documentation "The remote port")
   (timeout :initform *ftp-timeout* :initarg :timeout :accessor ftp-timeout :type integer
            :documentation "Stream timeout in 60ths of a second.")
   (data-open-timeout :initform *ftp-data-open-timeout* :initarg :data-open-timeout :accessor ftp-data-open-timeout :type integer)
   (user-id :initform "anonymous" :initarg :user-id :accessor ftp-user-id :documentation "The login user-id")
   (password :initform "" :initarg :password :accessor ftp-password :documentation "The login password")
   (passive-p :initform t :initarg :passive-p :accessor ftp-passive-p :documentation "Use active FTP if non-nil")
   (transfer-type :initform :text :initarg :transfer-type :accessor ftp-transfer-type :type (member :binary :text)
                  :documentation "The data connection transfer type.")
   (features :initform nil :accessor %ftp-supported-features :type (or null (cons keyword)))
   (features-checked-p :initform nil :accessor ftp-features-checked-p :type boolean)
   (state :initform 0 :accessor ftp-state :type integer))
  (:documentation "The object that holds all information for an FTP connection."))

(defmethod print-object ((connection ftp-connection) stream)
  (with-slots (host port user-id) connection
    (print-unreadable-object (connection stream)
      (format stream "FTP connection to ~A:~A user-id: ~A" host port user-id))))

(defgeneric ftp-close (stream-or-socket &key abort)
  (:documentation "Closes an FTP stream or socket.
This may also deallocate the stream when streams are resourced."))

(defmethod ftp-close (stream-or-socket &key abort)
  (close stream-or-socket :abort abort))

;;;------------------------------------------------------------------- 
;;;
;;; DATA STREAM
;;;

(defmethod choose-data-socket ((connection ftp-connection)  mode base-port port-range &key (timeout *ftp-timeout*))
  (flet ((find-data-port (base-port port-range mode timeout)
           (declare (fixnum base-port port-range))
           (loop with element-type = (element-type-for-mode mode)
                 repeat port-range
                 for port = (+ base-port (random port-range))
                 for socket = (make-passive-socket port
                                                   :element-type element-type
                                                   :read-timeout timeout
                                                   :no-error-p t)
                 when socket return (values socket port)
                 finally (error 'www-utils:network-error 
                                :format-string "No FTP Data Port Available: Unable to find a free port betweem ~D and ~D"
                                :format-args (list base-port (+ base-port port-range))))))
    (multiple-value-bind (data-socket data-port)
        (find-data-port base-port port-range mode timeout)
      (send-port-command connection (local-host-ip-address) data-port)
      (values data-socket data-port))))

(defmethod establish-data-stream ((connection ftp-connection) mode command reply-handler restart)
  (with-slots (passive-p data-open-timeout) connection
    (unless (eq mode (ftp-transfer-type connection))
      (send-transfer-type-command connection mode))
    (cond (passive-p
           (send-crlf-line connection "PASV") ;make the other end passive
           (multiple-value-bind (data-host data-port)
               (receive-pasv-reply connection)
             (when restart
               (send-restart-command connection restart))
             (send-crlf-line connection command)
             (when reply-handler
               (funcall reply-handler connection))
             ;; open stream after sending the command
             (ftp-open-stream data-host data-port :mode mode :timeout data-open-timeout)))
          (t (let (data-socket)
               (unwind-protect
                   (progn 
                     (setq data-socket (choose-data-socket connection mode *ftp-base-data-port* *ftp-data-port-range*))
                     ;; open stream before sending the command
                     (when restart
                       (send-restart-command connection restart))
                     (send-crlf-line connection command)
                     (when reply-handler
                       (funcall reply-handler connection))
                     (prog1 (accept-connection data-socket)
                       (setq data-socket nil))) ;don't deallocate when accept-connection wins
                 (when data-socket
                   (ftp-close data-socket))))))))

(defmethod call-continuation-with-data-stream ((connection ftp-connection) mode command reply-handler (continuation function) &key restart)
  "Similar to WITH-DATA-STREAM, except that function is a function which accepts a single
argument; namely the data-stream"
  (with-slots (timeout) connection
    (let (data-stream)
      (unwind-protect
          (progn
            (setq data-stream (establish-data-stream connection mode command reply-handler restart))
            (with-stream-timeout (data-stream timeout :error-p t)
              (funcall continuation data-stream)))
        (when data-stream
          (ftp-close data-stream))))))

(defmacro with-data-stream ((stream connection mode command reply-handler &key restart) &body body)
  "The FTP command, command, is executed and STREBAM is bound to the corresponding data stream within
the scope of BODY.  The MODE of stream can be either :BINARY or :TEXT. If RESTART is provided, FTP restart
command will be sent using it as a value."
  (let ((fctn (gensym "TRANSFER-FUNCTION-")))
    `(flet ((,fctn (,stream) ., body))
       (declare (dynamic-extent #',fctn))
       (call-continuation-with-data-stream ,connection ,mode ,command ,reply-handler #',fctn :restart ,restart))))

;;;------------------------------------------------------------------- 
;;;
;;; CONDITIONS
;;;

(defvar *status-code-alist*
  ;; In this case, the text is exact and not left to the particular implementation it must read: MARK yyyy = mmmm
  ;; Where yyyy is User-process data stream marker, and mmmm server's equivalent marker (note the spaces between markers and "=").
  '((110 "Restart marker reply.")
    (120 "Service ready in nnn minutes.")
    (125 "Data connection already open; transfer starting.")
    (150 "File status okay;  about to open data connection.")
    (200 "Command okay.")
    (202 "Command not implemented, superfluous at this site.")
    (211 "System status, or system help reply.")
    (212 "Directory status.")
    (213 "File status.")
    ;; On how to use the server or the meaning of a particular non-standard command.  This reply is useful only to the human user.
    (214 "Help message.")
    ;; Where NAME is an official system name from the list in the Assigned Numbers document.
    (215 "NAME system type.")
    (220 "Service ready for new user.")
    ;; Logged out if appropriate.
    (221 "Service closing control connection.")
    (225 "Data connection open; no transfer in progress.")
    ;; Requested file action successful (for example, file transfer or file abort).
    (226 "Closing data connection.")
    (227 "Entering Passive Mode (h1,h2,h3,h4,p1,p2).")
    (230 "User logged in, proceed.")
    (250 "Requested file action okay, completed.")
    (257 "PATHNAME created.")
    (331 "User name okay, need password.")
    (332 "Need account for login.")
    (350 "Requested file action pending further information.")
    ;; This may be a reply to any command if the service knows it must shut down.
    (421 "Service not available, closing control connection." ftp-service-unavailable)
    (425 "Can't open data connection." ftp-unable-to-open-data-connection)
    (426 "Connection closed; transfer aborted." ftp-data-connection-closed)
    (450 "File unavailable: Requested file action not taken." ftp-file-busy)
    (451 "Local error in processing: Requested action aborted." ftp-local-error)
    (452 "Insufficient storage space: Requested action not taken." ftp-insufficient-storage)
    (500 "Syntax error, command unrecognized, or command too long." ftp-unrecognized-command)
    (501 "Syntax error in parameters or arguments." ftp-syntax-error)
    (502 "Command not implemented." ftp-command-not-implemented)
    (503 "Bad sequence of commands." ftp-bad-command-sequence)
    (504 "Command not implemented for that parameter." ftp-parameter-not-implemented)
    (530 "Not logged in." ftp-not-logged-in)
    (532 "Need account for storing files." ftp-account-required)
    (550 "File unavailable: Requested action not taken." ftp-file-not-found);(e.g., file not found, no access)
    (551 "Page type unknown: Requested action aborted.")
    (552 "Exceeded storage allocation: Requested file action aborted." ftp-no-more-storage) ; (for current directory or dataset).
    (553 "File name not allowed: Requested action not taken." ftp-file-name-not-allowed)))

(defun get-status-code-reason (code &optional (error-p t))
  (cond ((second (assoc code *status-code-alist*)))
        (error-p
         (error "Unknown FTP status code, ~D." code))
        (t nil)))

(define get-condition-for-status-code (code &optional no-error-p)
  (cond ((third (assoc code *status-code-alist*)))
        (no-error-p nil)
        (t (error "Unknown FTP status code, ~D."code))))

;;;------------------------------------------------------------------- 
;;;
;;; CONDITIONS
;;;

(define-condition ftp-condition
    (protocol-condition condition)
  ((code :initform nil :initarg :code :reader ftp-status-code)
   (reason :initform nil :initarg :reason :reader ftp-reason)
   (description :initform "FTP Condition" :allocation :class :reader ftp-description))
  (:report (lambda (cond stream)
             (let ((code (ftp-status-code cond)))
               (format stream "~A~:[~; (~:*~D)~]:~:[~; ~:*~A~]" 
                       (ftp-description cond) code (or (ftp-reason cond) (get-status-code-reason code nil)))))))

(define-condition ftp-error 
    (ftp-condition error)
  ((description :initform "FTP Error" :allocation :class :reader ftp-description)
   (format-string :initform nil :initarg :format-string :reader format-string)
   (format-args :initform nil :initarg :format-args :reader format-args))
  (:report (lambda (cond stream)
             (let ((code (ftp-status-code cond))
                   (format-string (format-string cond)))
               (format stream "~A~:[~; (~:*~D)~]:~:[~; ~:*~A~]" 
                       (ftp-description cond) code (or (ftp-reason cond) (unless format-string (get-status-code-reason code nil))))
               (when format-string
                 (apply #'format stream format-string (format-args cond)))))))

(define-condition ftp-file-error (ftp-condition error) ())

(define-condition ftp-invalid-code 
    (ftp-error)
  ((description :initform "Invalid FTP Code" :allocation :class)
   (expected  :initarg :expected :reader ftp-expected-code))
  (:report (lambda (cond stream)
             (format stream "~A: Expected ~A received ~A" 
                     (ftp-description cond) (ftp-expected-code cond) (ftp-status-code cond)))))
  
(define-condition ftp-transient-failure 
    (ftp-error)
  ((description :initform "Transient FTP Failure" :allocation :class))
  (:documentation "Signalled when a transient error is received from the FTP server.
The input was fine, but something else went wrong.  Feel free to resend."))

(define-condition ftp-service-unavailable
    (ftp-transient-failure)
  ((code :initform 421)
   (description :initform "Service Not Available" :allocation :class)))

(define-condition data-connection-error (ftp-transient-failure) ())

(define-condition ftp-unable-to-open-data-connection
    (data-connection-error)
  ((code :initform 425)
   (description :initform "Unable to Open Data Connecton" :allocation :class)))

(define-condition ftp-data-connection-closed
    (data-connection-error)
  ((code :initform 426)
   (description :initform "Data Connecton Closed" :allocation :class)))

(define-condition ftp-file-busy
    (ftp-transient-failure ftp-file-error)
  ((code :initform 450)
   (description :initform "File Busy" :allocation :class)))

(define-condition ftp-local-error
    (ftp-transient-failure)
  ((code :initform 451)
   (description :initform "Internal Server Error" :allocation :class)))

(define-condition ftp-insufficient-storage
    (ftp-transient-failure ftp-file-error)
  ((code :initform 452)
   (description :initform "Insufficient Storage" :allocation :class)))

(define-condition ftp-permanent-failure
    (ftp-error)
  ((description :initform "Permanent FTP Failure" :allocation :class))
  (:documentation "Signalled when a permanent error is received from the FTP server.
The input was not acceptable and should not be re-sent."))

(define-condition ftp-unrecognized-command
    (ftp-permanent-failure)
  ((code :initform 500)
   (description :initform "Unkown Command" :allocation :class)))

(define-condition ftp-syntax-error
    (ftp-permanent-failure)
  ((code :initform 501)
   (description :initform "Syntax Error" :allocation :class)))

(define-condition ftp-command-not-implemented
    (ftp-permanent-failure)
  ((code :initform 502)
   (description :initform "Command Not Implemented" :allocation :class)))

(define-condition ftp-bad-command-sequence
    (ftp-permanent-failure)
  ((code :initform 503)
   (description :initform "Bad Command Sequence" :allocation :class)))

(define-condition ftp-parameter-not-implemented
    (ftp-permanent-failure)
  ((code :initform 504)
   (description :initform "Parameter Not Implemented" :allocation :class)))

(define-condition ftp-not-logged-in
    (ftp-permanent-failure)
  ((code :initform 530)
   (description :initform "Not Logged In" :allocation :class)))

(define-condition ftp-account-required
    (ftp-not-logged-in)
  ((code :initform 532)
   (description :initform "Need Account to Store Files" :allocation :class)))

(define-condition ftp-file-not-found
    (ftp-permanent-failure ftp-file-error)
  ((code :initform 550)
   (description :initform "File Not Found" :allocation :class)))

(define-condition ftp-no-more-storage
    (ftp-permanent-failure ftp-file-error)
  ((code :initform 553)
   (description :initform "No More Storage" :allocation :class)))

(define-condition ftp-file-name-not-allowed
    (ftp-permanent-failure ftp-file-error)
  ((code :initform 553)
   (description :initform "File Name Not Allowed" :allocation :class)))

(defun signal-ftp-error (status-code &key reason expected-code)
  (declare (fixnum status-code))
  (flet ((get-reason (status-code reason)
           (or reason (get-status-code-reason status-code nil))))
    (declare (inline get-reason))
    (let ((condition-class (get-condition-for-status-code status-code nil)))
      (cond (condition-class
             (signal condition-class :reason (get-reason status-code reason)))
            (expected-code
             (error 'ftp-invalid-code :code status-code :expected expected-code :reason (get-reason status-code reason)))
            ((< 399 status-code 500)
             (error 'ftp-transient-failure :code status-code :reason (get-reason status-code reason)))
            ((< 499 status-code 600)
             (error 'ftp-permanent-failure :code status-code :reason (get-reason status-code reason)))
            (t (error 'ftp-error :code status-code :reason (get-reason status-code reason)))))))

(defgeneric require-code (ftp-connection expected-code &optional collector)
  (declare (values ftp-code))
  (:documentation "Requires the FTP response code to be EXPECTED-CODE or signals an FTP condition.
EXPECTED-CODE can be an integer or a list of integers. Returns the actual FTP code received."))

(defmethod require-code ((connection ftp-connection) (expected-code integer) &optional collector)
  (flet ((require-code-collector (code line start end)
           (cond ((eql code expected-code)
                  (and collector (funcall collector code line start end)))
                 (t (signal-ftp-error code :reason (subseq line start end) :expected-code expected-code)))))
    (declare (dynamic-extent #'require-code-collector))
    (receive-reply connection #'require-code-collector t)))

(defmethod require-code ((connection ftp-connection) (expected-codes cons) &optional collector)
  (flet ((require-code-collector (code line start end)
           (cond ((member code expected-codes)
                  (and collector (funcall collector code line start end)))
                 (t (signal-ftp-error code :reason (subseq line start end) :expected-code expected-codes)))))
    (declare (dynamic-extent #'require-code-collector))
    (receive-reply connection #'require-code-collector t)))

(defmethod require-code :around (connection expected-code &optional collector)
  (when *trace-client*
    (etypecase expected-code
      (integer
       (fast-format *trace-output* "~&FTP Client (~A)>[Require code ~D]~%" (local-host-domain-name) expected-code))
      ((cons integer)
       (fast-format *trace-output* "~&FTP Client (~A)>[Require codes ~S]~%" (local-host-domain-name) expected-code))))
  (call-next-method connection expected-code collector))

(defmethod check-code ((connection ftp-connection) (expected-code integer) &optional collector)
  (flet ((check-code-collector (code line start end)
           (cond ((eql code expected-code)
                  (and collector (funcall collector code line start end)))
                 (t (signal-ftp-error code :reason (subseq line start end) :expected-code expected-code)))))
    (declare (dynamic-extent #'check-code-collector))
    (receive-reply connection #'check-code-collector *ftp-command-timeout*)))

(defmethod check-code :around (connection expected-code &optional collector)
  (let ((code (call-next-method connection expected-code collector)))
    (when *trace-client*
      (format *trace-output* "~&FTP Client (~A)>[Check code ~D:~:[ not found~; found~] ~:[(timeout: ~D secs)~;(code: ~D)~]]~%" 
              (local-host-domain-name) expected-code (eql expected-code code) code (if code code *ftp-command-timeout*)))
    code))

(defmethod clear-pending-replies ((connection ftp-connection))
  (cond (*trace-client*
         (loop for code = (receive-reply connection)
               while code
               do (fast-format *trace-output* "~&FTP Client (~A)>[Clear code ~D]~%" (local-host-domain-name) code)))
        (t (loop while (receive-reply connection)))))

;;;------------------------------------------------------------------- 
;;;
;;; TRACE FACILITY
;;;

(define trace-client (&optional (on-p (not *trace-client*)))
  "Toggles FTP session tracing for the client according to ON-P."
  (setq *trace-client* (not (null on-p))))

(defgeneric trace-session (connection case data)
  (:documentation "Logs a transcript of the FTP session to *trace-output*.
CASE is either :CLIENT or :SERVER.  
DATA can be either a STRING or a FUNCTION applied to *trace-output*."))

(defmethod trace-session ((connection ftp-connection) case (data string))
  (declare (ignore case))
  (fast-format *trace-output* "~A~%" data))

(defmethod trace-session ((connection ftp-connection) case (data function))
  (declare (ignore case))
  (funcall data *trace-output*)
  (fresh-line *trace-output*))

(defmethod trace-session :around ((connection ftp-connection) case data)
  (when *trace-client*
    (ecase case
      (:client 
       (fast-format *trace-output* "~&FTP Client (~A)>" (local-host-domain-name)))
      (:server
       (fast-format *trace-output* "~&FTP Server (~A)>" (ftp-host connection))))
    (call-next-method connection case data)))

;;;------------------------------------------------------------------- 
;;;
;;; OPERATORS FOR SENDING SESSION COMMANDS AND REPLIES
;;;

(defmethod send-crlf-line ((connection ftp-connection) line)
  (with-slots (stream) connection
    (trace-session connection :client line)
    (etypecase line
      (string (write-string line stream))
      (function (funcall line stream)))
    (send-cr-line-feed stream)
    (force-output stream)))

(defmethod send-reply-code ((connection ftp-connection) code)
  (flet ((write-reply-code (stream)
           (fast-format stream "~D ~A" code (get-status-code-reason code t))))
    (declare (dynamic-extent #'write-reply-code))
    (send-crlf-line connection #'write-reply-code)))

;;;------------------------------------------------------------------- 
;;;
;;; DEFINE FTP COMMANDS
;;;

(defmethod send-user-command (connection (user-id string) &optional collector)
  (declare (values (code)))
  (flet ((write-user-command (stream)
           (fast-format stream "USER ~A" user-id)))
    (declare (dynamic-extent #'write-user-command))
    (send-crlf-line connection #'write-user-command)
    (require-code connection '(230 331) collector)))

(defmethod send-password-command (connection (password string) &optional collector)
  (declare (values (code)))
  (flet ((write-pass-command (stream)
           (fast-format stream "PASS ~A" password)))
    (declare (dynamic-extent #'write-pass-command))
    (send-crlf-line connection #'write-pass-command)
    (require-code connection 230 collector)))

(defmethod send-port-command (connection (ip-address string) (port integer) &optional collector)
  (declare (values (code)) (fixnum port))
  (flet ((write-port-command (stream)
           (multiple-value-bind (quot rem)
               (truncate port 256)
             (fast-format stream "PORT ~I,~A,~A"
                          (loop for ch across ip-address
                                do (write-char (case ch (#\. #\,) (t ch)) stream))
                          quot rem))))
    (declare (dynamic-extent #'write-port-command))
    (send-crlf-line connection #'write-port-command)
    (require-code connection 200 collector)))

(defmethod send-abort-command (connection &optional collector)
  "Sends the FTP ABOR command to abort the previous FTP service command."
  (declare (values (code)))
  (flet ((receive-abort-reply (code line start end)
           (case code
             (226 ;FTP service successfully completed, closing data connection
              (and collector (funcall collector code line start end)))
             ;; Service terminated abnormally
             (426 (require-code connection 226 collector))
             (t (signal-ftp-error code)))))
    (declare (dynamic-extent #'receive-abort-reply))
    (send-crlf-line connection "ABOR")
    (receive-reply connection #'receive-abort-reply t)))

(defmethod send-logout-command (connection &optional collector)
  (declare (values (code data)))
  (flet ((write-quit-command (stream)
           (fast-format stream "QUIT"))
         (receive-quit-reply (code line start end)
           (case code
             (221 ;FTP session successfully closed
              (and collector (funcall collector code line start end)))
             (t (signal-ftp-error code)))))
    (declare (dynamic-extent #'write-quit-command #'receive-quit-reply))
    (when (connection-open-p connection)
      (send-crlf-line connection #'write-quit-command)
      (receive-reply connection #'receive-quit-reply t))))

(defmethod send-reinitialize-command (connection &optional collector)
  (declare (values (code data)))
  (flet ((write-rein-command (stream)
           (fast-format stream "REIN"))
         (receive-rein-reply (code line start end)
           (case code
             (220 ;FTP session ready for new user
              (and collector (funcall collector code line start end)))
             (t (signal-ftp-error code)))))
    (declare (dynamic-extent #'write-rein-command #'receive-rein-reply))
    (when (connection-open-p connection)
      (send-crlf-line connection #'write-rein-command)
      (receive-reply connection #'receive-rein-reply t))))

(defmethod send-no-op-command (connection &optional collector)
  (declare (values (code data)))
  (flet ((write-noop-command (stream)
           (fast-format stream "NOOP")))
    (declare (dynamic-extent #'write-noop-command))
    (send-crlf-line connection #'write-noop-command)
    (require-code connection 200 collector)))

(defmethod send-restart-command (connection (restart integer))
  (flet ((write-restart-command (stream)
           (fast-format stream "REST ~D" restart)))
    (declare (dynamic-extent #'write-restart-command))
    (send-crlf-line connection #'write-restart-command)))

(defmethod send-transfer-type-command (connection mode &optional collector)
  (declare (values (code)))
  (labels ((write-data-type (mode stream)
             (etypecase mode
               (keyword
                (write-char (ecase mode 
                              (:binary #\I) ;8-bit binary
                              (:text #\A) ;ascii
                              (:ebcdic #\E)) ;ebcdic
                            stream))
               (cons ;; lisp compound type
                     (destructuring-bind (type-specifier byte-size) mode
                       (ecase type-specifier
                         (unsigned (fast-format stream "~D" byte-size)))))))
           (write-type-command (stream)
             (fast-format stream "TYPE ~I" (write-data-type mode stream))))
    (declare (dynamic-extent #'write-type-command))
    (send-crlf-line connection #'write-type-command)
    (prog1 (require-code connection 200 collector)
      (setf (ftp-transfer-type connection) mode))))

(defun parse-257-reply (string collector &optional (start 0) (end (length string)))
  (loop with s
        for idx fixnum upfrom start below end
        for ch = (char string idx)
        do (case ch
             (#\" 
              (cond (s (return (funcall collector 257  string s idx)))
                    (t (setq s (1+ idx))))))
        finally (error 'ftp-error :code 501 :reason "Syntax Error: Ill-formed 257 reply.")))

(defmethod send-current-directory-command (connection &optional collector)
  "Sends the FTP PWD command."
  (declare (values (code)))
  (flet ((receive-pwd-reply (code line start end)
           (case code
             (257 ;standard reply
              (parse-257-reply line collector start end))
             (250 ;handle Genera reply
              (with-string-trim-bounds (*white-space-chars* line start end)
                (funcall collector 250 line (char-position #\space line start end t) end))))))
    (declare (dynamic-extent #'receive-pwd-reply))
    (send-crlf-line connection "PWD")
    (require-code connection '(257 250) #'receive-pwd-reply)))

(defmethod send-change-directory-command (connection (remote-dir function) &optional collector)
  "Sends the FTP CWD command, to change to the remote-directory."
  (declare (values (code)))
  (flet ((write-cwd-command (stream)
           (fast-format stream "CWD ~I" (funcall remote-dir stream))))
    (declare (dynamic-extent #'write-cwd-command))
    (send-crlf-line connection #'write-cwd-command)
    (require-code connection 250 collector)))

(defmethod send-change-directory-command (connection (remote-dir string) &optional collector)
  (declare (values (code)))
  (flet ((write-remote-dir (stream)
           (write-string remote-dir stream)))
    (declare (dynamic-extent #'write-remote-dir))
    (send-change-directory-command connection #'write-remote-dir collector)))

(defmethod send-change-to-parent-directory-command (connection &optional collector)
  "Sends the FTP CDUP command to change to the parent of the current remote directory."
  (declare (values (code)))
  (send-crlf-line connection "CDUP")
  (require-code connection 250 collector))

(defmethod send-directory-list-command (connection (stream-reader function) &optional (directory :current-directory) collector)
  "Sends the FTP LIST command and writes the reply to OUTPUT-STREAM."
  (declare (values (code)))
  (let (ready-p)
    (flet ((write-list-command (stream)
             (fast-format stream "LIST ~A" (case directory (:current-directory ".") (t directory))))
           (handle-list-reply (connection)
             (setq ready-p (check-code connection (if (ftp-passive-p connection) 150 125) collector))))                                
      (declare (dynamic-extent #'write-list-command #'handle-list-reply))
      (with-data-stream (stream connection :text #'write-list-command #'handle-list-reply)
        (funcall stream-reader stream))
      (unless ready-p
        (require-code connection (if (ftp-passive-p connection) 150 125) collector))
      (require-code connection (if (ftp-passive-p connection) 250 226) collector))))

(defmethod send-directory-list-command (connection (output-stream stream) &optional (directory :current-directory) collector)
  (flet ((copy-stream (ftp-stream)
           (stream-copy-until-eof ftp-stream output-stream :text)))
    (declare (dynamic-extent #'copy-stream))
    (send-directory-brief-list-command connection #'copy-stream directory collector)))

(defmethod send-directory-brief-list-command (connection (stream-reader function) &optional (directory :current-directory) collector)
  "Sends the FTP NLST command and writes the reply to OUTPUT-STREAM."
  (declare (values (code)))
  (let (ready-p)
    (flet ((write-nlist-command (stream)
             (fast-format stream "NLST ~A" (case directory (:current-directory ".") (t directory))))
           (handle-nlist-reply (connection)
             (setq ready-p (check-code connection (if (ftp-passive-p connection) 150 125) collector))))
      (declare (dynamic-extent #'write-nlist-command #'handle-nlist-reply))
      (with-data-stream (stream connection :text #'write-nlist-command #'handle-nlist-reply)
        (funcall stream-reader stream))
      (unless ready-p
        (require-code connection (if (ftp-passive-p connection) 150 125) collector))
      (require-code connection (if (ftp-passive-p connection) 226 250) collector))))

(defmethod send-directory-brief-list-command (connection (output-stream stream) &optional (directory :current-directory) collector)
  (declare (values (code)))
  (flet ((copy-stream (ftp-stream)
           (stream-copy-until-eof ftp-stream output-stream :text)))
    (declare (dynamic-extent #'copy-stream))
    (send-directory-brief-list-command connection #'copy-stream directory collector)))

(defmethod send-create-directory-command (connection (dir-name string) &optional collector)
  "Sends the FTP MKD command to make a remote directory, and returns directory name string,
or signals error."
  (declare (values (code)))
  (flet ((write-mkd-command (stream)
           (fast-format stream "MKD ~A" dir-name))
         (receive-mkd-reply (code line start end)
           (declare (ignore code))
           (parse-257-reply line collector start end)))
    (declare (dynamic-extent #'write-mkd-command #'receive-mkd-reply))
    (send-crlf-line connection #'write-mkd-command)
    (require-code connection 257 #'receive-mkd-reply)))

(defmethod send-delete-directory-command (connection (directory function) &optional collector)
  "Sends the FTP RMD command to delete a remote DIRECTORY.  
DIRECTORY can either an absolute or relative pathname. If it is a relative pathname, directory is
removed from the current directory."
  (declare (values (code)))
  (flet ((write-rmd-command (stream)
           (fast-format stream "RMD ~I" (funcall directory stream))))
    (declare (dynamic-extent #'write-rmd-command))
    (send-crlf-line connection #'write-rmd-command)
    (require-code connection 250 collector)))

(defmethod send-delete-directory-command (connection (directory-name string) &optional collector)
  (declare (values (code))
           (ignore collector))
  (flet ((write-dir-name (stream)
           (fast-format stream "~A" directory-name)))
    (declare (dynamic-extent #'write-dir-name))
    (send-delete-directory-command connection #'write-dir-name)))

(defmethod send-delete-file-command (connection (file function) &optional collector)
  "Sends the FTP DELE command to delete a remote FILE."
  (declare (values (code)))
  (flet ((write-dele-command (stream)
           (fast-format stream "DELE ~I" (funcall file stream))))
    (declare (dynamic-extent #'write-dele-command))
    (send-crlf-line connection #'write-dele-command)
    (require-code connection 250 collector)))

(defmethod send-delete-file-command (connection (file-name string) &optional collector)
  "Sends the FTP DELE command to delete a remote file."
  (declare (values (code)))
  (flet ((write-file-name (stream)
           (fast-format stream "~A" file-name)))
    (declare (dynamic-extent #'write-file-name))
    (send-delete-file-command connection #'write-file-name collector)))

(defmethod send-allocate-storage-command (connection (bytes integer) &optional byte-size collector)
  "Sends the FTP ALLO command to reserve storage for a file transfer.
BYTES is a decimal integer representing the number of bytes of storage
to be reserved. For files sent with record or page structure,
BYTE-SIZE is the maximum record of page size."
  (declare (values (code)))
  (flet ((write-allo-command (stream)
           (fast-format stream "ALLO ~D~I" bytes
                        (when byte-size (fast-format stream " R ~D" byte-size)))))
    (declare (dynamic-extent #'write-allo-command))
    (send-crlf-line connection #'write-allo-command)
    (require-code connection '(200 202) collector)))

(defmethod send-mount-structure-command (connection (pathname function) &optional collector)
  "Sends the FTP SMNT command to mount a remote file system structure."
  (flet ((write-smnt-command (stream)
           (fast-format stream "SMNT ~I" (funcall pathname stream))))
    (declare (dynamic-extent #'write-smnt-command))
    (send-crlf-line connection #'write-smnt-command)
    (require-code connection '(250 202) collector)))

(defgeneric send-system-command (connection)
  (declare (values operating-system-keyword))
  (:documentation "Sends the FTP SYST command and returns an operating system keyword."))

;; Operating system tokens listed here
;; http://www.iana.org/assignments/operating-system-names
(defmethod send-system-command (connection &aux operating-system)
  (declare (values operating-system-keyword))
  (flet ((write-syst-command (stream)
           (fast-format stream "SYST"))
         (receive-syst-reply (code line start end)
           (declare (ignore code))
           (unless operating-system
             (let ((end-os-token (char-position #\space line start end)))
               (cond (end-os-token
                      (setq operating-system (tokenize-keyword line start end-os-token)))
                     (t (signal-ftp-error 501 :reason (concatenate 'list "SYST reply unparsable: " line))))))))
    (declare (dynamic-extent #'write-syst-command))
    (send-crlf-line connection #'write-syst-command)
    (require-code connection 215 #'receive-syst-reply)
    operating-system))

(defgeneric send-rename-file-command (connection from-pathname to-pathname &optional collector)
  (declare (values (code)))
  (:documentation "Sends the FTP RNTO command to rename a remote FILE or directory from FROM-PATHNAME to TO-PATHNAME."))

(defmethod send-rename-file-command (connection (from-pathname function) (to-pathname function) &optional collector)
  (flet ((write-rnfr-command (stream)
           (fast-format stream "RNFR ~I" (funcall from-pathname stream)))
         (write-rnto-command (stream)
           (fast-format stream "RNTO ~I" (funcall to-pathname stream))))
    (declare (dynamic-extent #'write-rnfr-command #'write-rnto-command))
    (send-crlf-line connection #'write-rnfr-command)
    (require-code connection 350 collector)
    (send-crlf-line connection #'write-rnto-command)
    (require-code connection 250 collector)))

(defmethod send-rename-file-command (connection (from-pathname string) to-pathname &optional collector)
   (flet ((write-path-name (stream)
           (fast-format stream "~A" from-pathname)))
    (declare (dynamic-extent #'write-path-name))
    (send-rename-file-command connection #'write-path-name to-pathname collector)))

(defmethod send-rename-file-command (connection from-pathname (to-pathname string) &optional collector)
  (flet ((write-path-name (stream)
           (fast-format stream "~A" to-pathname)))
    (declare (dynamic-extent #'write-path-name))
    (send-rename-file-command connection from-pathname #'write-path-name collector)))

(defgeneric send-status-command (connection pathname &optional collector)
  (:documentation "Sends the FTP STAT command to obtain the status of a server, directory, or file."))

(defmethod send-status-command (connection (pathname null) &optional collector)
  (flet ((write-stat-command (stream)
           (fast-format stream "STAT")))
    (declare (dynamic-extent #'write-stat-command))
    (send-crlf-line connection #'write-stat-command) ;systems may just send 211
    (require-code connection '(211 212 213) collector))) ; 211 system, 212 directory, 213 file

(defmethod send-status-command (connection (pathname function) &optional collector)
  (flet ((write-stat-command (stream)
           (fast-format stream "STAT ~I" (funcall pathname stream))))
    (declare (dynamic-extent #'write-stat-command))
    (send-crlf-line connection #'write-stat-command)
    (require-code connection '(211 212 213) collector)))

(defmethod send-status-command (connection (pathname string) &optional collector)
  (flet ((write-file (stream)
           (fast-format stream "~A" pathname)))
    (declare (dynamic-extent #'write-file))
    (send-status-command connection #'write-file collector)))

(defmethod send-status-command (connection (pathname (eql :current-directory)) &optional collector)
  (let ((current-dir :root))
    (labels ((directory-delimiter-p (char)
               (member char '(#\/ #\>))) ;unix, Genera delimiters
             (get-dir-name (code string start end)
               (declare (fixnum start end))
               (let* ((e (if (directory-delimiter-p (char string (1- end))) (1- end) end))
                      (s-1 (position-if* #'directory-delimiter-p string :start start :end e :from-end t)))
                 (cond (s-1
                        (setq current-dir (subseq string (1+ (the fixnum s-1)) e)))
                       (e) ;root
                       (t (error 'ftp-error
                                 :code code
                                 :format-string "Unable to parse directory pathname.~&FTP Reply: ~S"
                                 :format-args (list (subseq string 0 end))))))))
      (declare (dynamic-extent #'get-dir-name))
      (case current-dir
        (:root
         (send-status-command connection "." collector))
        (t (send-current-directory-command connection #'get-dir-name)
           (unwind-protect
               (progn 
                 (send-change-to-parent-directory-command connection collector)
                 (send-status-command connection current-dir collector))
             (send-change-directory-command connection current-dir collector)))))))

;; heuristic - may be called inside a reply handler.
(defun %directory-p (string &optional (start 0) (end (length string)))
  (declare (values directory-p confirmed-p))
  (when (good-pathname-p string start end)
    (let ((pos (char-position #\. string start end t)))
      ;; Check for numeric versions, eg Genera
      (when pos
        (loop for idx upfrom (1+ (the fixnum pos)) below end
              always (digit-char-p (char string idx))
              finally (psetq pos (char-position #\. string start pos t)
                             end pos)))
      ;; Guess files from extensions first
      (cond ((and pos (tokenize-exported-pathname-extension string (1+ (the fixnum pos)) end))
             (values nil t))
            (t t)))))

(defun parse-unix-directory-list-line (string &optional (start 0) (end (length string)))
  (macrolet ((collect ((keyword value) result)
               `(setq ,result (list* ,keyword ,value ,result))))
    (flet ((parse-unix-date (string start end)
             (flet ((determine-year (month)
                      (multiple-value-bind (sec min hour day mon year)
                          (current-time 0)
                        (declare (ignore sec min hour day))
                        (if (<= month mon) year (1- year))))
                    (year-char-p (char)
                      (or (digit-char-p char)
                          (eql char #\:))))
               (handler-case-if (not *debug-client*)
                   (let* ((e1 (char-position #\space string start end))
                          (s2 (position-if-not #'white-space-char-p string :start (1+ e1) :end end))
                          (e2 (char-position #\space string (1+ s2) end))
                          (s3 (position-if-not #'white-space-char-p string :start (1+ e2) :end end))
                          (e3 end)
                          (y-colon (char-position #\: string s3 e3))
                          (month (www-utils::translated-month string start e1))
                          (day (parse-integer string :start s2 :end e2))
                          (year (if y-colon (determine-year month) (parse-integer string :start s3 :end e3)))
                          (hour (if y-colon (parse-integer string :start s3 :end y-colon) 0))
                          (min (if y-colon 
                                   (parse-integer string :start (1+ (the fixnum y-colon)) :end e3)
                                 0)))
                     (declare (fixnum e1 s2 e2 s3))
                     (encode-universal-time 0 min hour day month year 0)) ;gmt
                 (error () nil)))))
      (loop with e and end-date and result
            with space-p = t 
            with count = -1
            for idx fixnum downfrom (1- end) downto start
            for ch = (char string idx)
            do (case ch
                 ((#\space #\tab)
                  (cond (e
                         (incf count)
                         (case count
                           (0 (if (good-pathname-p string (1+ idx) e)
                                  (collect (:file (subseq string (1+ idx) e)) result)
                                (return-from parse-unix-directory-list-line nil))) ;bail out for bad pathnames
                           (1 (setq end-date e)) ;year
                           (2) ;day
                           (3 (let ((ut (parse-unix-date string (1+ idx) end-date)));date
                                (when ut
                                  (collect (:modify ut) result))))
                           (4 (collect (:size (parse-integer string :start (1+ idx) :end e)) result))
                           (5) ; group
                           (6 (collect (:author (subseq string (1+ idx) e)) result)))
                         (psetq e nil
                                space-p t))
                        (t (setq space-p t))))
                 (t (cond (space-p
                           (setq e (1+ idx))))
                    (setq space-p nil)))
            finally (destructuring-bind (&key file &allow-other-keys) result
                      (return (if (%directory-p file)
                                  (list* :type :directory result)
                                (list* :type :file result))))))))

(defun get-directory-list-parser (operating-systrem)
  (ecase operating-systrem
    ((:unix :unix-bsd :unix-v1at :unix-v :unix-v.1 :unix-v.2 :unix-v.3 :unix-pc)
     'parse-unix-directory-list-line)))

(defun ftp-status-directory-plist (connection pathname &aux plists)
  "Returns a directory property list for pathname based on the FTP STAT command.
If applied to a directory, this returns a list of property lists for each file or directory.  If
applied to a file, it returns a single property list."
  (declare (values plist operating-systrem))
  (let* ((operating-systrem (send-system-command connection))
         (parser (get-directory-list-parser operating-systrem))
         1st-line-p)
    (unless parser
      (error 'ftp-error :reason "Unable to build directory plist."))
    (flet ((receive-status-reply (code line start end)
             (declare (ignore code))
             (ignore-first-line (line start 1st-line-p)
               (with-string-trim-bounds (*white-space-chars* line start end)
                 (let ((plist (funcall parser line start end)))
                   (when plist
                     (push plist plists)))))))
      (declare (dynamic-extent #'receive-status-reply))
      (send-status-command connection pathname #'receive-status-reply)
      (values (if (cdr plists) (nreverse plists) plists)
              operating-systrem))))

;;;------------------------------------------------------------------- 
;;;
;;; FTP EXTENSION
;;;

(defgeneric send-features-command (connection &optional collector)
  (declare (values (features)))
  (:documentation "Sends the FTP FEAT command to obtain the supported features of the FT server.
Returns a list of keywords corresponding to the supported features."))

(defmethod send-features-command (connection &optional collector &aux features 1st-line-p)
  (declare (values (code)))
  (flet ((write-feat-command (stream)
           (fast-format stream "FEAT"))
         (read-feat-reply (code line start end)
           (and collector (funcall collector code line start end))
           (ignore-first-line (line start 1st-line-p)
             (with-string-trim-bounds (*white-space-chars* line start end)
               (let ((pos (char-position #\space line start end)))
                 (push (tokenize-keyword line start (or pos end) :create) features))))))
    (declare (dynamic-extent #'write-feat-command #'read-feat-reply))
    (send-crlf-line connection #'write-feat-command)
    (require-code connection 211 #'read-feat-reply)
    (setf (%ftp-supported-features connection) features
          (ftp-features-checked-p connection) t)
    features))

(defmethod ftp-supported-features ((connection ftp-connection))
  (with-slots (features features-checked-p) connection
    (unless features-checked-p
      ;; check the avilable extension commands
      (http::handler-case
          (send-features-command connection)
        (ftp-unrecognized-command () (setf (ftp-features-checked-p connection) t))))
    features))

(defmacro ftp-feature-case (connection &body clauses)
  "Executes clauses based on the availability of features supported by the FTP server.
Each claues is of the form (FEATURE-KEYWORD . FORMS). FEATURE-KEYWORD can be T when no test is
desired."
  `(let ((features (ftp-supported-features ,connection)))
     (cond ,.(loop for (feature . forms) in clauses
                   for test = (cond ((eql feature t) t)
                                    (t (etypecase feature
                                         (keyword `(member ,feature features))
                                         ((cons keyword)
                                          `(and ,.(loop for item in feature
                                                        collect `(member ,item features)))))))
                   collect `(,test .,forms)))))

;; extension
(defmethod send-options-command (connection (command string) &optional collector)
  "Sends the FTP OPT command to delete a remote file."
  (declare (values (code)))
  (flet ((write-opt-command (stream)
           (fast-format stream "OPT ~A" command)))
    (declare (dynamic-extent #'write-opt-command))
    (send-crlf-line connection #'write-opt-command)
    (require-code connection 211 collector)))

(defmethod send-file-size-command (connection (remote-file function) &aux size)
  "Sends the FTP SIZE command on for REMOTE-FILE.  Returns an integer size, or signals error."
  (declare (values remote-file-size success-p))
  (flet ((write-size-command (stream)
           (fast-format stream "SIZE ~I" (funcall remote-file stream)))
         (receiev-size-reply (code line start end)
           (declare (ignore code))
           (unless size
             (setq size (parse-integer line :start start :end end)))))
    (declare (dynamic-extent #'write-size-command #'receiev-size-reply))
    (with-slots (transfer-type) connection
      (ftp-feature-case connection
        (:size 
         (unless (eq :binary transfer-type)
           (send-transfer-type-command connection :binary))
         (send-crlf-line connection #'write-size-command)
         (require-code connection 213 #'receiev-size-reply)
         (values size t))
        (t nil)))))

(defmethod send-file-size-command (connection (remote-file string))
  (flet ((write-file-name (stream)
           (write-string remote-file stream)))
    (declare (dynamic-extent #'write-file-name))
    (send-file-size-command connection #'write-file-name)))

;; http://www.ietf.org/internet-drafts/draft-ietf-ftpext-mlst-16.txt
(defun ftp-parse-time (string &optional (start 0) (end (length string)))
  (declare (fixnum start end))
  (unless (<= 14 (- end start) 17)
    (error "Malformed FTP date field: ~S" (subseq string start end)))
  (let ((year-end (+ start 4))
        (month-end (+ start 6))
        (day-end (+ start 8))
        (hour-end (+ start 10))
        (min-end (+ start 12))
        (sec-end (+ start 14)))
    (encode-universal-time (parse-integer string :start min-end :end sec-end)
                           (parse-integer string :start hour-end :end min-end)
                           (parse-integer string :start day-end :end hour-end)
                           (parse-integer string :start month-end :end day-end)
                           (parse-integer string :start year-end :end month-end)
                           (parse-integer string :start start :end year-end)
                           0))) ;always GMT

(defmethod send-modification-time-command (connection (remote-file function) &aux last-modification)
  "Sends the FTP MDTM command on for REMOTE-FILE.  Returns a universal time, or signals error."
  (declare (values remote-file-mdtm success-p))
  (flet ((write-mdtm-command (stream)
           (fast-format stream "MDTM ~I" (funcall remote-file stream)))
         (receiev-mdtm-reply (code line start end)
           (declare (ignore code))
           (unless last-modification
             (setq last-modification (ftp-parse-time line start end)))))
    (declare (dynamic-extent #'write-mdtm-command #'receiev-mdtm-reply))
    (ftp-feature-case connection
      (:mdtm 
       (send-crlf-line connection #'write-mdtm-command)
       (require-code connection 213 #'receiev-mdtm-reply)
       (values last-modification t))
      (t nil))))

(defmethod send-modification-time-command (connection (remote-file string))
  (flet ((write-file-name (stream)
           (write-string remote-file stream)))
    (declare (dynamic-extent #'write-file-name))
    (send-modification-time-command connection #'write-file-name)))

(defun parse-mlist-pairs (string &optional (start 0) (end (length string)) &aux (delimiter #\;))
  (labels ((parse-mlist-delimited-pair (value-parser string &optional (start 0) (end (length string)))
             (declare (values keyword value))
             (let* ((delim-pos (char-position #\= string start end))
                    (pos (position-if-not* #'white-space-char-p string :start start :end (or delim-pos end) :from-end t)))
               (cond (delim-pos
                      (let ((keyword (%tokenize-keyword string start (1+ (the fixnum pos)))))
                        (values keyword (funcall value-parser keyword string (1+ (the fixnum delim-pos)) end))))
                     (t (error "No = delimiter found in the equal sign delimited pair, ~A" (subseq string start end))))))
           (value-getter (keyword string start end)
             (with-string-trim-bounds (*white-space-chars* string start end)
               (case keyword
                 (:type
                  (cond ((%string-equal "dir" string 0 3 start end)
                         :directory)
                        ((%string-equal "file" string 0 4 start end)
                         :file)
                        ((%string-equal "cdir" string 0 4 start end)
                         :current-directory)
                        ((%string-equal "pdir" string 0 4 start end)
                         :parent-directory)
                        (t (%tokenize-keyword string start end)))) ;OS or file system dependent type
                 (:size (parse-integer string :start start :end end :junk-allowed t))
                 ((:modify :create) (ftp-parse-time string start end))
                 (:perm 
                  (let ((key (%tokenize-keyword string start end)))
                    (or (get key 'lowercase-string) ;limit consing
                        (setf (get key 'lowercase-string) (string-downcase key)))))
                 ;; consing unique-ids and other stuff we don't know about
                 (t (subseq string start end))))))
    (loop with keyword and value 
          for s = start then (1+ (the fixnum e))
          while (< s end)
          for s1 = (position-if-not* #'white-space-char-p string :start s :end end)
	  while s1
          for e = (or (char-position delimiter string s1 end) end)
          do (multiple-value-setq (keyword value)
                 (parse-mlist-delimited-pair #'value-getter string s1 e))
          collect keyword
          collect value)))

(defmethod send-machine-list-command (connection (remote-file-or-directory function) &optional collector &aux plist 1st-line-p)
  "Sends the FTP MLST command and returns a property list of attributes when COLLECTOR is null."
  (declare (values (plist success-p)))
  (labels ((write-mlist-command (stream)
             (fast-format stream "MLST ~I" (funcall remote-file-or-directory stream)))
           (parse-mlist-reply (code line start end)
             (declare (fixnum start end) 
                      (ignore code))
             (ignore-first-line (line start 1st-line-p)
               (with-string-trim-bounds (*white-space-chars* line start end)
                 (let ((e1 (char-position #\; line start end t)))
                   (setq plist (nconc plist (parse-mlist-pairs line start (1+ (the fixnum e1))))))))))
    (declare (dynamic-extent #'write-mlist-command #'parse-mlist-reply))
    (ftp-feature-case connection
      (:mlst
       (send-crlf-line connection #'write-mlist-command)
       (require-code connection 250 (or collector #'parse-mlist-reply))
       (values plist t))
      (t nil))))

(defmethod send-machine-list-command (connection (remote-file-or-directory string) &optional collector)
  (flet ((write-file-or-directory-command (stream)
           (fast-format stream "~A" remote-file-or-directory)))
    (declare (dynamic-extent #'write-file-or-directory-command))
    (send-machine-list-command connection #'write-file-or-directory-command collector)))

(defmethod send-machine-list-command (connection (remote-file-or-directory (eql :current-directory)) &optional collector)
  (flet ((write-file-or-directory-command (stream)
           (fast-format stream ".")))
    (declare (dynamic-extent #'write-file-or-directory-command))
    (send-machine-list-command connection #'write-file-or-directory-command collector)))

;;;------------------------------------------------------------------- 
;;;
;;; TOPLEVEL INVOCATION
;;;

(defun open-ftp-connection (host port &key user-id password (timeout *ftp-timeout*) (passive-p t) report-stream)
  (check-type port (integer 0))
  (check-type user-id (or string null))
  (check-type password (or string null))
  (flet ((allocate-ftp-connection (host port timeout)
           (make-instance 'ftp-connection
                          :host host
                          :port port
                          :timeout timeout)))
    (let ((connection (allocate-ftp-connection host port timeout)))
      ;; iniialize parameters
      (cond-every
       (user-id (setf (ftp-user-id connection) user-id))
       (password (setf (ftp-password connection) password))
       (passive-p (setf (ftp-passive-p connection) passive-p)))
      ;; fire up connection
      (connect connection report-stream))))

(defmethod connect ((connection ftp-connection) &optional report-stream)
  "Connects to the FTP described by CONNECTION."
  (flet ((report-server-replies (code line start end)
           (declare (ignore code))
           (fresh-line report-stream)
           (write-string line report-stream :start start :end end)))
    (declare (dynamic-extent #'report-server-replies))
    (with-slots (stream host port timeout user-id password) connection
      (cond ((and stream
                  (cond ((live-connection-p stream)
                         (handler-case
                             (progn
                               (send-reinitialize-command connection (and report-stream #'report-server-replies))
                               t)
                           (ftp-error () nil)))
                        (t (ftp-close stream :abort t)
                           nil))))
            ((setq stream (ftp-open-stream host port :timeout timeout))
             (setf (ftp-stream connection) stream)
             (require-code connection 220 (and report-stream #'report-server-replies)))
            (t (error "Connection to ~A:~A failed" host port)))
      (when (and user-id password)
        (case (send-user-command connection user-id (and report-stream #'report-server-replies))
          (331
           (send-password-command connection password (and report-stream #'report-server-replies)))))
      connection)))

(defmethod connection-force-output ((connection ftp-connection))
  (with-slots (stream) connection
    (force-output stream)))

(defmethod connection-open-p ((connection ftp-connection))
  (with-slots (stream) connection
    (and stream
         (live-connection-p stream))))

(defmethod close-connection ((connection ftp-connection) &key abort report-stream)
  "Closes the FTP connection, CONNECTION.
Closes the stream in abort mode when ABORT is non-null."
  (flet ((report-server-replies (code line start end)
           (declare (ignore code))
           (fresh-line report-stream)
           (write-string line report-stream :start start :end end)))
    (declare (dynamic-extent #'report-server-replies))
    (with-slots (stream) connection
      (cond ((null stream))
            ((connection-open-p connection)
             (if abort
                 (send-abort-command connection (and report-stream #'report-server-replies))
               (send-logout-command connection (and report-stream #'report-server-replies)))
             (ftp-close stream :abort abort))
            (t (ftp-close stream :abort t))))))

(defmacro with-ftp-connection ((connection host port &key  (user-id '*default-user-id*) (password '(default-user-password)) (timeout '*ftp-timeout*)
                                           (passive-p t) report-stream) &body body) 
  "Opens and ensures proper close of an FTP connection.  Binds connection-variable to the
FTP-CONNECTION object in the scope of body.  Arguments are similar to that of the initargs for the
class FTP-CONNECTION."
  `(let ((,connection (open-ftp-connection ,host ,port
                                           :user-id ,user-id
                                           :password ,password
                                           :timeout ,timeout
                                           :passive-p ,passive-p
                                           :report-stream ,report-stream)))
     (unwind-protect
         (progn .,body)
       (close-connection ,connection))))

;;;------------------------------------------------------------------- 
;;;
;;; PROCESSING REPLIES
;;;

(defgeneric receive-reply (connection &optional collector block)
  (declare (values code))
  (:documentation "Receives a reply from the FTP server connected to CONNECTION.
Waits for the reply when BLOCK is non-null, otherwise if no reply is immediately available returns
NIL.  BLOCK can also be an integer, which is interpreted as the number of seconds to wait for the
reply.  Calls COLLECTOR on each line received with the arguments (LINE CODE START END)."))

(defmethod receive-reply ((connection ftp-connection) &optional collector block)
  (flet ((parse-ftp-code (string &optional (start 0) (end (length string)))
           (declare (fixnum start end))
           (let ((e1  (+ start 3)))
             (declare (fixnum e1))
             (when (< e1 end)
               (parse-integer string :start start :end e1 :junk-allowed t)))))
    (with-slots (stream) connection
      (when (or (eq block t) (ftp-stream-input-data-available-p stream block))
        (using-resource (buffer http::line-buffer http::*line-buffer-size*)
          (let (ftp-code continue-p)
            (multiple-value-bind (line eof delimiter end)
                (read-delimited-line stream '(#\Return #\linefeed) nil buffer)
              (declare (ignore eof delimiter))
              (when line
                (setq ftp-code (parse-ftp-code line 0 end)
                      continue-p (eql #\- (char line 3)))
                (setf (ftp-state connection) ftp-code) ;keep track FTP session state
                (trace-session connection :server line)
                (when collector
                  (funcall collector ftp-code line 4 end))))
            (when continue-p
              (loop with line-code and space-p
                    doing (progn 
                            (setf (fill-pointer buffer) 0) ;reset buffer index
                            (multiple-value-bind (line error-p delimiter end)
                                (read-delimited-line stream '(#\Return #\linefeed) nil buffer)
                              (declare (ignore error-p delimiter))
                              (when line
                                (setq line-code (parse-ftp-code line 0 end)
                                      space-p (and (< 3 end) (eql #\space (char line 3))))
                                ;; Read lines until line  begins with code followed by space
                                (when (and (eql ftp-code line-code) space-p)
                                  (loop-finish))
                                (trace-session connection :server line)
                                (when collector 
                                  (funcall collector ftp-code line (if (eql line-code ftp-code) 4 0) end)))))))
            (values ftp-code)))))))

(defmethod receive-pasv-reply ((connection ftp-connection) &aux host port)
  (declare (values host port))
  (flet ((parse-pasv-reply (code line start end)
           (flet ((parse-standard-pasv (line start end)
                    (let ((s (char-position #\( line start end t))
                          (e (char-position #\) line start end t)))
                      (when (and  s e)
                        (parse-comma-separated-header line (1+ (the fixnum s)) e))))
                  (parse-variant-pasv (line start end)
                    (with-string-trim-bounds (*white-space-chars* line start end)
                      (parse-comma-separated-header line start end))))
             (declare (inline parse-standard-pasv parse-variant-pasv))
             (case code
               (227
                (unless (or host port) ;only parse first line
                  (let ((numbers (or (parse-standard-pasv line start end)
                                     (parse-variant-pasv line start end))))
                    (declare (dynamic-extent numbers))
                    (destructuring-bind (h1 h2 h3 h4 p1 p2) numbers
                      (psetq host (concatenate 'string h1 "." h2 "." h3 "." h4)
                             port (+ (ash (the fixnum (parse-integer p1)) 8) (the fixnum (parse-integer p2))))))))
               (t (signal-ftp-error code :reason (subseq line start end) :expected-code 227))))))
    (declare (dynamic-extent #'parse-pasv-reply))
    (receive-reply connection #'parse-pasv-reply t)
    (values host port)))

;;;------------------------------------------------------------------- 
;;;
;;; PRIMARY FILE AND DIRECTORY OPERATIONS
;;;

;; Supresses useless file or directory names
(defun good-pathname-p (string &optional (start 0) (end (length string)))
  (declare (ignore end))
  (not (member (char string start) '(#\. #\# #\%))))

(defun ftp-map-directory (connection function &optional (directory :current-directory))
  "Maps FUNCTION over every entry in DIRECTORY.
Function is called with the arguments: (STRING START END)."
  (flet ((read-file-list (stream)
           (using-resource (buffer http::line-buffer http::*line-buffer-size*)
             (loop with start = 0 and end and line and error-p and delimiter
                   do (progn 
                        (setf (fill-pointer buffer) 0)
                        (multiple-value-setq (line error-p delimiter end)
                            (read-delimited-line stream '(#\return #\linefeed) nil buffer))
                        error-p delimiter)
                   while line
                   do (with-string-trim-bounds (*white-space-chars* line start end)
                        (when (good-pathname-p line start end)
                          (funcall function line start end)))))))
    (declare (dynamic-extent #'read-file-list))
    (send-directory-brief-list-command connection #'read-file-list directory)))

(defun ftp-directory-list (connection &optional (directory :current-directory) &aux file-list)
  "Returns a list of files for DIRECTORY."
  (flet ((read-file-list (stream)
           (using-resource (buffer http::line-buffer http::*line-buffer-size*)
             (setq file-list (loop with start = 0 and end and line and error-p and delimiter and item
                                   do (progn 
                                        (setf (fill-pointer buffer) 0)
                                        (multiple-value-setq (line error-p delimiter end)
                                            (read-delimited-line stream '(#\return #\linefeed) nil buffer))
                                        error-p delimiter)
                                   while line
                                   do (with-string-trim-bounds (*white-space-chars* line start end)
                                        (setq item (when (good-pathname-p line start end)
                                                     (subseq line start end))))
                                   when item collect item)))))
    (declare (dynamic-extent #'read-file-list))
    (send-directory-brief-list-command connection #'read-file-list directory)
    file-list))

(defun file-plist-via-mlst (connection pathname)
  (handler-case ;; can happen with links
      (send-machine-list-command connection pathname)
    (ftp-file-not-found () nil)))

(defun file-plist-via-file-info (connection pathname)
  (handler-case ;; can happen with links
      (multiple-value-bind (size last-modification)
          (ftp-file-info connection pathname)
        `(,@(if size '(:type :file) '(:type :directory))
          ,.(when size `(:size ,size))
          ,.(when last-modification `(:modify ,last-modification))))
    (ftp-file-not-found () nil)))

(defun ftp-map-directory-plist (connection function &optional (directory :current-directory))
  "Maps function over the entires of directory.
function is called with: (entry property-list)."
  (ftp-feature-case connection
    (:mlst
     (dolist (entry (ftp-directory-list connection directory))
       (funcall function entry (file-plist-via-mlst connection entry))))
    ((:size :mdtm)
     (dolist (entry (ftp-directory-list connection directory))
       (funcall function entry (file-plist-via-file-info connection entry))))
    (t (dolist (plist (ftp-status-directory-plist connection directory))
         (funcall function (getf plist :file) plist)))))

(defun ftp-directory-p (connection string &optional (start 0) (end (length string)))
  (declare (ignore connection)
           (values directory-p confirmed-p))
  (multiple-value-bind (directory-p confirmed-p)
      (%directory-p string start end)
    (when confirmed-p
      (return-from ftp-directory-p directory-p))
    ;; Then check to see if CD to it wins
 ;   (handler-case 
 ;       (send-change-directory-command connection item)
 ;     (ftp-error () (return-from ftp-directory-p nil)))
  ;  (send-change-to-parent-directory-command connection)
    (values directory-p confirmed-p)))

(defgeneric ftp-get-file (ftp-connection remote-file local-file &key mode restart collector)
  (declare (values (code data)))
  (:documentation "Copies the remote file, REMOTE-FILE, to the local file , LOCAL-FILE,
according to MODE, which is either :TEXT or :BINARY. LOCAL-FILE can be a file pathname or a
stream.  RESTART specifies an integer amount to seek into the file before retrieving it."))

(defmethod ftp-get-file ((connection ftp-connection) (remote-file function) (local-file function) &key (mode :text) restart collector)
  (let (ready-p)
    (flet ((write-retr-command (stream)
             (fast-format stream "RETR ~I" (funcall remote-file stream)))
           (handle-retr-reply (connection)
             (setq ready-p (check-code connection (if (ftp-passive-p connection) 150 125) collector))))
      (declare (dynamic-extent #'write-retr-command #'handle-retr-reply))
      (with-data-stream (stream connection mode #'write-retr-command #'handle-retr-reply :restart restart)
        (funcall local-file stream))
      (unless ready-p
        (require-code connection (if (ftp-passive-p connection) 150 125) collector))
      (require-code connection (if (ftp-passive-p connection) 226 250) collector))))

(defmethod ftp-get-file (connection remote-file (local-stream stream) &key (mode :text) restart collector)
  (flet ((copy-data (stream)
           (stream-copy-until-eof stream local-stream mode)))
    (declare (dynamic-extent #'copy-data))
    (ftp-get-file connection remote-file #'copy-data :mode mode :restart restart :collector collector)))

(defmethod ftp-get-file (connection remote-file (local-file pathname) &key (mode :text) restart collector)
  (with-open-file (stream local-file :direction :output :element-type (element-type-for-mode mode)
                          :if-does-not-exist :create :if-exists :supersede)
    (ftp-get-file connection remote-file stream :mode mode :restart restart :collector collector))
  (autotype-file local-file))

(defmethod ftp-get-file (connection (remote-file string) (local-stream stream) &key (mode :text) restart collector)
  (flet ((write-remote-file (stream)
           (write-string remote-file stream)))
    (declare (dynamic-extent #'write-remote-file))
    (ftp-get-file connection #'write-remote-file local-stream :mode mode :restart restart :collector collector)))

(defgeneric ftp-put-file (ftp-connection local-file remote-file &key mode restart collector)
  (declare (values (code data)))
  (:documentation "Copies the local file, local-file, to the remote file, remote-file, according to
MODE, which can be either :TEXT or :BINARY. When available ESTIMATED-SIZE provides the maximize
size of the file to be stored in units of BYTE-SIZE, which defaults to 8 bits."))

(defmethod ftp-put-file ((connection ftp-connection) (local-stream function) (remote-file function) &key 
                         (mode :text) estimated-size byte-size restart collector)
  (let (ready-p)
    (flet ((write-stor-command (stream)
             (fast-format stream "STOR ~I" (funcall remote-file stream)))
           (handle-stor-reply (connection)
             (setq ready-p (check-code connection (if (ftp-passive-p connection) 150 125) collector))))
      (declare (dynamic-extent #'write-stor-command #'handle-stor-reply))
      (when estimated-size
        (send-allocate-storage-command connection estimated-size byte-size collector))
      (with-data-stream (stream connection mode #'write-stor-command #'handle-stor-reply :restart restart)
        (funcall local-stream stream))
      (unless ready-p
        (require-code connection (if (ftp-passive-p connection) 150 125) collector))
      (require-code connection (if (ftp-passive-p connection) 226 250) collector))))

(defmethod ftp-put-file (connection (local-stream stream) remote-file &key (mode :text) restart collector)
  (flet ((copy-data (stream)
           (stream-copy-until-eof local-stream stream mode)))
    (declare (dynamic-extent #'copy-data))
    (ftp-put-file connection #'copy-data remote-file :mode mode :restart restart :collector collector)))

(defmethod ftp-put-file (connection (local-file pathname) remote-file &key (mode :text) restart collector)
  (with-open-file (stream local-file :direction :input :element-type (element-type-for-mode mode)
                          :if-does-not-exist :create :if-exists :supersede)
    (ftp-put-file connection stream remote-file :mode mode :restart restart :collector collector)))

(defmethod ftp-put-file (connection local-stream (remote-file string) &key (mode :text) restart collector)
  (flet ((write-file-name (stream)
           (fast-format stream "~A" remote-file)))
    (declare (dynamic-extent #'write-file-name))
    (ftp-put-file connection local-stream #'write-file-name :mode mode :restart restart :collector collector)))

(defgeneric ftp-append-file (ftp-connection local-file remote-file &key mode collector)
  (declare (values (code data)))
  (:documentation "Appends local file, LOCAL-FILE, to the remote file, REMOTE-FILE, according to
MODE, which can be either :TEXT or :BINARY. LOCAL-FILE can be a string, function, pathname, or
stream.  When available ESTIMATED-SIZE provides the maximize size of the file to be stored in units
of BYTE-SIZE, which defaults to 8 bits."))

(defmethod ftp-append-file ((connection ftp-connection) (local-stream function) (remote-file function) 
                            &key (mode :text) estimated-size byte-size collector)
  (let (ready-p)
    (flet ((write-appe-command (stream)
             (fast-format stream "APPE ~I" (funcall remote-file stream)))
           (handle-appe-reply (connection)
             (setq ready-p (check-code connection (if (ftp-passive-p connection) 150 125) collector))))
      (declare (dynamic-extent #'write-appe-command #'handle-appe-reply))
      (when estimated-size
        (send-allocate-storage-command connection estimated-size byte-size collector))
      (with-data-stream (stream connection mode #'write-appe-command #'handle-appe-reply)
        (funcall local-stream stream))
      (unless ready-p
        (require-code connection (if (ftp-passive-p connection) 150 125) collector))
      (require-code connection (if (ftp-passive-p connection) 226 250) collector))))

(defmethod ftp-append-file (connection local-stream (remote-file string) &key (mode :text) collector)
  (flet ((write-file-name (stream)
           (fast-format stream "~A" remote-file)))
    (declare (dynamic-extent #'write-file-name))
    (ftp-append-file connection local-stream #'write-file-name :mode mode :collector collector)))

(defmethod ftp-append-file (connection (local-stream stream) remote-file &key (mode :text) collector)
  (flet ((copy-data (stream)
           (stream-copy-until-eof local-stream stream mode)))
    (declare (dynamic-extent #'copy-data))
    (ftp-append-file connection #'copy-data remote-file :mode mode :collector collector)))

(defmethod ftp-append-file (connection (local-file pathname) remote-file &key (mode :text) collector)
  (with-open-file (stream local-file :direction :input :element-type (element-type-for-mode mode))
    (ftp-append-file connection stream remote-file :mode mode :collector collector)))

(defgeneric ftp-change-directory (connection directory)
  (:documentation "Changes the current directory to DIRECTORY on the FTP server connected to CONNECTION.  
DIRECTORY can be a list of directory components, a string or a function called on stream to
write the directory path."))

;; Functions or strings can lose on FTP servers not supporting TVFS.
;; Requires parsing the string and using the grovel method.
(defmethod ftp-change-directory (connection directory)
  (ftp-feature-case connection
    (:tvfs
     (send-change-directory-command connection directory))
    (t (error 'ftp-error
              :format-string  "Unable to change directory to the root directory in the file system.~&FTP host: ~A~&Port, ~D"
              :format-args (list (ftp-host connection) (ftp-port connection))))))

(defmethod ftp-change-directory (connection (directory list))
  (flet ((grovel-to-root-directory (connection &aux dir ldir)
           (flet ((get-dir (code string start end)
                    (declare (ignore code))
                    (setq dir (subseq string start end))))
             (loop repeat 10 ;never more than 10 times
                   do (send-current-directory-command connection #'get-dir)
                   do (cond ((equal ldir dir) (return))
                            (t (setq ldir dir)
                               (send-change-to-parent-directory-command connection)))
                   finally (error 'ftp-error
                                  :format-string "Unable to change directory to the root directory in the file system.~&FTP host: ~A~&Port, ~D"
                                  :format-args (list (ftp-host connection) (ftp-port connection)))))))
    (ftp-feature-case connection
      (:tvfs
       (flet ((write-directory-name (stream)
                (loop initially (fast-format stream "/")
                      for component in directory
                      do (fast-format stream "~A/" component))))
         (declare (dynamic-extent #'write-directory-name))
         (send-change-directory-command connection #'write-directory-name)))
      ;; primitive RFC 959 method
      (t (grovel-to-root-directory connection)
         (loop for component in directory
               do (send-change-directory-command connection component))))))

(defgeneric ftp-delete-directory (connection directory)
  (:documentation "Deletes DIRECTORY on the FTP server connected to CONNECTION.  
DIRECTORY can be a list of directory components, a string or a function called on stream to
write the directory path."))

(defmethod ftp-delete-directory (connection directory)
  (ftp-feature-case connection
    (:tvfs
     (send-delete-directory-command connection directory))
    ;; primitive RFC 959 method
    (t (error 'ftp-error
              :format-string "Directory deletion for RFC 959 FTP host using functions is not implemented.~&FTP host: ~A~&Port, ~D"
              :format-args (list (ftp-host connection) (ftp-port connection))))))

(defmethod ftp-delete-directory (connection (directory cons))
  (ftp-feature-case connection
    (:tvfs
     (flet ((write-directory-name (stream)
              (loop initially (fast-format stream "/")
                    for component in directory
                    do (fast-format stream "~A/" component))))
       (declare (dynamic-extent #'write-directory-name))
       (send-delete-directory-command connection #'write-directory-name)))
    ;; primitive RFC 959 method
    (t (let ((dir (car (last directory)))
             (parent (butlast directory)))
         (declare (dynamic-extent parent))
         (ftp-change-directory connection parent)
         (send-delete-directory-command connection dir)))))

(defgeneric ftp-delete-file (connection file)
  (:documentation "Deletes FILE on the FTP server connected to CONNECTION.
FILE can be a string or a function called on stream to write the file"))

(defmethod ftp-delete-file (connection file)
  (send-delete-file-command connection file))

(defgeneric ftp-rename-file (connection from-file to-file)
  (:documentation "Renames FROM-FILE on the FTP server connected to CONNECTION.
FROM-FILE can be a string or a function called on stream to write the file"))

(defmethod ftp-rename-file (connection from-file to-file)
  (send-rename-file-command connection from-file to-file))

(defgeneric ftp-file-info (connection &optional file-or-directory)
  (declare (values size last-modification unique-id))
  (:documentation "Returns information on FILE-OR-DIRECTORY depending on the extensions supported by the
FTP server.  When MLST is supported, returns SIZE, LAST-MODIFICATION and UNIQUE-ID. When SIZE is
supported, returns just the size.  Otherwise returns NIL."))

(defmethod ftp-file-info (connection &optional (file-or-directory :current-directory))
  (ftp-feature-case connection
    (:mlst
     (destructuring-bind (&key type size modify create unique &allow-other-keys) 
         (send-machine-list-command connection file-or-directory)
       (case type
         (:file
          (values size (or modify create) unique))
         ((:directory :current-directory :parent-directory)
          (values nil (or modify create) unique))
         (t (values size (or modify create) unique)))))
    ((:size :mdtm)
     (handler-case ;generates error on directories
         (values (send-file-size-command connection file-or-directory)
                 (send-modification-time-command connection file-or-directory))
       (ftp-file-not-found () nil)))
    (t nil)))

(defgeneric ftp-file-size (connection file)
  (declare (values size))
  (:documentation "Returns the size of FILE, when the ftp size command is supported, otherwise NIL."))

(defmethod ftp-file-size (connection file)
  (ftp-feature-case connection
    (:size
     (send-file-size-command connection file))
    (t nil)))

