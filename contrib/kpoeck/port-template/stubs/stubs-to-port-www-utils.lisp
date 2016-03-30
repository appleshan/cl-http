(in-package :www-utils)

;;; Unclassified
(defun arglist (function)
  "Returns the arglist for FUNCTION."
  function
  )

(defun time-zone (&optional update-p)
  update-p
  )

;;; Characters & Strings
(defun char-bits (char)
  (declare (ignore char))
  0
  )

(defun string-thin (string)
  "Strips font description"
  string)

;;; server tasks
(defun synchronize-daily-server-tasks ()
  ;; set up a timer for mighnight to start the 24 interval timer
  )

(defun synchronize-idle-http-process-scavenger () nil)

;;; logging
(defun notify-log-window (format-string &rest format-args)
  "Top-level method for writing to the HTTP log window."
  (values format-string format-args)
  )

(defun expose-log-window ())

(defun log-http-server-error (format-string &rest format-args)
  (apply #'notify-log-window  format-string format-args))

(defvar *last-condition*)

(defmethod report-condition ((condition simple-condition) stream)
  "Prints the report string for CONDITION onto STREAM."
  (values condition stream)
  (setq *last-condition* condition)
  (apply #'format stream 
         (simple-condition-format-control condition) 
         (simple-condition-format-arguments condition))
  (apply #'format t 
         (simple-condition-format-control condition) 
         (simple-condition-format-arguments condition))
  )

(defmethod report-condition ((condition condition) stream)
  "Prints the report string for CONDITION onto STREAM."
  (values condition stream)
  #+no (break)
  (setq *last-condition* condition)
  nil
  )

(defun report-string (condition)
  "Returns the report string for CONDITION."
  (with-output-to-string (stream)
    (report-condition condition stream)))

;;;; Files
(defun directory-list* (pathname predicate &rest options)
  "Accepts the options :FILES :DIRECTORIES :SORTED :PROPERTIES."
  (values pathname predicate options)
  )

(defun pathname-directory-p (pathname)
  "Returns non-null if PATHNAME denotes a directory."
  pathname
  )

(defmethod file-length-in-bytes ((stream pathname) &optional recompute-p)
  (declare (ignore recompute-p))
  (with-open-file (file-stream stream)
    (file-length file-stream))
  )

(defun file-stream-length-in-bytes (file-stream)
  "Returns the length in bytes for FILE-STREAM's source file."
  (values 23 file-stream)
  )

(defun file-version  (pathname)
  pathname
  )

(defun file-stream-version (file-stream)
  file-stream
  (get-universal-time)
  )

(defun %make-log-pathname (device directory name host)
  "Returns the pathname to which current log entries are written."
  (make-pathname
    :host host
    :device device
    :directory directory
    :name name
   :type "text"))

(defmethod file-creation-date ((pathname pathname))
  (file-write-date pathname))

(defun file-properties (pathname)
  "Returns the length in bytes  and the creation in in universal time 
for FILE-STREAM's source file."
  pathname
  )

(defun file-stream-creation-date (file-stream)
  "Returns the creation date in universal time for FILE-STREAM's source file."
  (file-write-date file-stream))

(defun file-stream-modification-date (stream)
  (file-write-date stream))

(defun create-directories-recursively (pathname)
  "Recursively create directories according to the directories present in PATHNAME."
  pathname
  )

(defun probe-directory (pathname)
  pathname
  )

(defun directory-info (pathname &key (name :wild) (type :wild) (version :newest) (sort-pathnames t)
                                 directories)
  "Returns a poperty list of information for every file in the directory PATHNAME
that matches pathnames wildcards. Directories are included when directories is non-null."
  (values pathname name type version sort-pathnames directories)
  nil
  )

;;; Processes
(defun process-idle-time (process)
  "Returns the amount of time the process has been up, in seconds."
  process
  )

(defun process-run-time (process)
  "Returns the amount of run time the process has accumulated, in microseconds."
  process
  )

(defmethod process-active-p ((process t))
  )

(defun process-disable (process)
  process
  )

(defun process-enable (process)
  process
  )

(defun process-wait (reason &rest args)
  (values reason args)
  )

(defun process-kill (process)
  process
  )

(defun current-process ()
  )

(defun make-process (name &key &allow-other-keys)
  name
  )

(defmethod process-preset ((process t) initial-function &rest initial-args)
  (values initial-function initial-args)
  )

(defmethod process-whostate ((process t))
  )

(defmethod (setf process-priority) (val (process t))
  val
  )

(defun make-lock (name &key type &allow-other-keys)
  "Returns a lock named name that is suitable for use with with-lock-held."
  (values name type)
  )

(defun process-run-function (name function &rest args)
  (values name function args))

(defun process-wait-with-timeout (whostate seconds function &rest args)
  (values whostate seconds function args)
  )

(defun process-name (process)
  "nada" 
  )

(defun all-processes ()
  nil
  )

(defmacro atomic-conditional-replacef (reference predicate new-value)
  "When PREDICATE returns non-null, this setfs REFERENCE to NEW-VALUE.
Predicate is called (OLD-VALUE NEW-VALUE). The operation
assures that predicate application and swap are atomic."
  `(when ,predicate
     (setf ,reference ,new-value))
  )

(defmacro atomic-decf (reference &optional (delta 1))
  "Atomically decrements REFERENCE by DELTA."
  `(decf ,reference ,delta)
  )

(defmacro atomic-push (item reference)
  "Atomically pushes ITEM onto REFERENCE."
  `(push ,item ,reference)
  )

(defmacro atomic-incf (reference &optional (delta 1))
  "Atomically increments REFERENCE by DELTA."
  `(incf ,reference ,delta))

(defmacro with-lock-held ((lock &optional (mode :write) (whostate "Wait for Lock")) &body body)
  "Executes BODY with LOCK held in MODE, which is one of :READ or :WRITE."
  `(progn
     (values ,lock ,mode ,whostate)
     (progn ,@body)))

(defmacro with-timeout ((timeout &key error-p) &body body)
  "Executes BODY and returns the values of the last form in BODY. However, if
the execution takes longer than TIMEOUT seconds, abort it. If :ERROR-P is
unsupplied or false, just return nil. If :ERROR-P is true, signal an error."
  `(progn ,timeout ,error-p . ,body))

(defmacro with-stream-timeout ((stream timeout &key error-p) &body body)
  "Executes BODY and returns the values of the last form in BODY. 
However, if the stream goes idle for longer than TIMEOUT seconds, the
operation is aborted.  If ERROR-P is non-null, the time out error is
signalled, otherwise NIL is returned."
  `(progn ,stream
     (with-timeout (,timeout :error-p ,error-p) . ,body)))

;;;; IP Layer
(defun host-eq (host1 host2)
  "Returns non-null if HOST1 is equal to HOST2."
  (eq host1 host2)
  )

(defun host-http-name (host)
  "Returns the internet host name for HOST."
  host
  )

(defun ip-address-for-host-domain-name (domain-name)
  "Returns the IP address string for domain-name."
  domain-name
  )

(defun ip-address-for-parsed-ip-address (ip-number)
  "Returns an IP address as a string from, IP-NUMBER, the parsed address."
  ip-number
  )

(defun domain-name-for-ip-address (address &optional (no-error-p t))
  "Given the IP address, ADDRESS, this returns the domain name or NIL."
  (if no-error-p
      address
    23)
  )

(defun domain-name-for-parsed-ip-address (ip-number &optional (no-error-p t))
  ;;
  ;;  my guess: this is supposed to return the fully qualified domain
  ;;  name for a given ip address, if on exists, else it returns the
  ;;  dotted ipaddr in a string
  ;;
  (if no-error-p
      23
    ip-number)
  )

(defun %parse-internet-address (address)
  "Returns an IP-NUMBER which is integer denoting the address of host."
  ;; address is an ipaddr, or string holding a dns name for a
  ;; machine, or a string containing a dotted ip address.
  address
  )

(defun parse-host (address &optional no-error-p)
  "Top-level method for parsing a host ADDRESS."
  (values address no-error-p)
  "localhost"
  )

(defun local-host-parsed-ip-address (&optional recache-p)
  "Returns the parsed IP address of the local host."
  recache-p)

(defun local-host-ip-address (&optional recache-p)
  "Returns the IP address of the local host."
  recache-p
  )

(defun local-host-domain-name (&optional recache-p)
  "Returns the local host domain name."
  recache-p
  "localhost"
  )

(defun local-host (&optional recache-p)
  (declare (ignore recache-p))
  "localhost"
  )

(defun host-mail-name (host)
  "The internet mail name for HOST."
  host
  )

(defun local-host-ip-address-moved-p ()
  "Returns non-null if the local host IP address has changed."
  nil)

(defun %local-host-domain-name ()
  "localhost")

(defun local-protocol (http-stream)
  (declare (ignore http-stream))
  :http)

(defun port-protocol (port)
  "Returns a keyword denoting the protocol specified for PORT,
or NIL if no protocol has been defined for PORT."
  (declare (ignore port))
  :http)

(defun ip-host-trusted-p (address secure-subnets &optional network)
  "Returns non-null if IP-address address is trusted given secure-subnets."
  (declare (ignore network))
  (labels ((split-ip (address)
             (declare (values (i1 i2 i3 i4)))
             `(,(ldb (byte 8  0) address)
               ,(ldb (byte 8  8) address)
               ,(ldb (byte 8 16) address)
               ,(ldb (byte 8 24) address)))
           (address-match-p (addr1 addr2)
             (let ((address1 (split-ip addr1))
                   (address2 (split-ip addr2)))
               (and 
                (or (= (first address2) 0) (= (first address1) (first address2)))
                (or (= (second address2) 0) (= (second address1) (second address2)))
                (or (= (third address2) 0) (= (third address1) (third address2)))
                (or (= (fourth address2) 0) (= (fourth address1) (fourth address2)))))))
    (declare (inline split-ip address-match-p))
    (cond (secure-subnets
           (member (etypecase address
                     (integer address)
                     (string (%parse-internet-address address)))
                   secure-subnets
                   :test #'address-match-p))
          (t t))))

(defun %host-log-name (address host &optional resolve-ip-address-p)
  "Returns a string for use in logging server access."
  (values address host resolve-ip-address-p))

(defmacro with-automatic-login ((host user-id user-pw) &body body)
  "Supplies userid and PW to ensure successul FTP login to host with BODY."
  `(progn (notify-log-window "~&(WITH-AUTOMATIC-LOGIN (~S ~S ~S) - Not available on MAC"
                             ,host ,user-id ,user-pw)
          ,@body))

;;; Chunking
(defmethod chunk-transfer-decoding-mode ((stream stream))
  )

(defmethod note-last-chunk ((stream stream) &optional footers-plist)
  footers-plist
  )

(defmethod note-first-chunk ((stream stream))
  )

(defmethod chunk-transfer-encoding-mode ((stream stream) chunk-function)
  chunk-function
  )

(defmethod chunk-transfer-decoding-mode-end ((stream stream))
  ;; shut down input chunk decoding
  )

(defmethod chunk-transfer-content-length ((stream stream))
  )

(define-condition end-of-chunk-transfer-decoding
                  (end-of-file)
  ()
  (:documentation "Signalled when a complete HTTP resource has been successfully transferred."))

(defun %buffered-stream-read-delimited-line (stream delimiters eof buffer)
  (values stream delimiters eof buffer))
  
;;; Server
(defun ensure-http-protocol-on-port (port)
  port
  )

(defmethod foreign-port ((http-stream stream))
  )

(defmethod local-port ((http-stream stream))
  )

(defmethod foreign-host ((http-stream stream))
  "localhost"
  )

(defun live-connection-p (http-stream)
  "Returns non-null if the TCP/IP connection over HTTP-STREAM remains alive
in that the remote host continue to respond at the TCP/IP level."
  http-stream
  )

(defmacro with-crlf-stream ((stream direction) &body body)
  "Turns STREAM into a CRLF stream within the scope of BODY.
direction can be :OUTPUT, :INPUT, or :BOTH."
  (declare (ignore stream direction))
  `(progn ,@body))

(defmacro with-binary-stream ((stream direction) &body body)
  (declare (ignore stream direction))
  `(progn
     ,@body))

(defmacro with-text-stream ((stream direction) &body body)
  "Turns STREAM into a text stream within the scope of BODY.
direction can be :OUTPUT, :INPUT, or :BOTH."
  (declare (ignore stream direction))
  `(progn ,@body))

(defmethod bytes-received ((stream t)) 0)
(defmethod bytes-transmitted ((stream t)) 0)
(defmethod (setf bytes-received) (val (stream t)) val)
(defmethod (setf bytes-transmitted) (val (stream t)) val)

;;; Mail
(defun send-mail-from (from to subject mail-writer
			    &key keywords comments file-references reply-to additional-headers
                            user
                            password)
  "Send an email message from FROM to TO with subject SUBJECT.
MESSAGE-WRITER is either a string or a function accepting a stream argument
that is used to generate the message body. KEYWORDS is a list of keywords for a keyword
header. COMMENTS is a string for a comment header. FILE-REFERENCES is a list of pathnames.
REPLY-TO is automatically added unless supplied here."
  (values from to subject mail-writer keywords comments file-references reply-to additional-headers user password)
  )

(defun report-bug  (to subject format-string &rest format-args)
  "Reports a bug to TO with SUBJECT and message body
produced by applying format to format-string and format-args.
The from field is http:*server-mail-address*."
  (values to subject format-string format-args)
  )


(deftype file-not-found () 
  "Specialization of Common Lisp File-error in which the file was not found on open."
  '(and condition file-error))

;;; Mop Stuff
#-cormanlisp
(defmethod method-specializers ((object t)))

#-cormanlisp
(defmethod generic-function-methods ((object t)))

(defvar %tcp-service-port-alist% '(("finger" . 79)))

(defun tcp-service-port-number-aux (service-name error-p)
  (let ((port (cdr (assoc service-name %tcp-service-port-alist% :test #'string-equal))))
    (cond (port port)
          (error-p (error "Unknown TCP service name ~S" service-name)))))

(defun tcp-service-port-number (protocol &optional error-p)
  "Returns the service port number for the TCP protocol denoted by protocol.
PROTOCOL is a keyword,, but integer and string are also supported."
  (etypecase protocol
    (integer protocol)
    (keyword
     (let ((string (string-downcase protocol)))
       (declare (dynamic-extent string))
       (tcp-service-port-number-aux string error-p)))
    (string (tcp-service-port-number-aux protocol error-p))))