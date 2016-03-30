(in-package :www-utils)

;;;; TODO
;;;; respect http:*shadow-host-name*
;;;;         http::*shadow-host-domain-name*

(defun vector-ip-adress-p (what)
  (and (arrayp what)
       (numberp (aref what 0))))

(defun host-eq (host1 host2)
  "Returns non-null if HOST1 is equal to HOST2."
  (cond ((and (integerp host1)(integerp host2))
         (= host1 host2))
        ((and (vector-ip-adress-p host1)(vector-ip-adress-p host2))
         (and
          (= (aref host1 0)(aref host2 0))
          (= (aref host1 1)(aref host2 1))
          (= (aref host1 2)(aref host2 2))
          (= (aref host1 3)(aref host2 3))))
        ((and (stringp host1)
              (stringp host2)
              (http:ip-address-string-p host1)
              (http:ip-address-string-p host1))
         (string= host1 host2))
        ;;; dont really like this recursion, already hitted twice and infinite loop
        (t (host-eq (%whatever-to-address host1)(%whatever-to-address host2))))
  ) 

#|

(host-eq "localhost" "127.0.0.1")
(host-eq "localhost" "127.0.0.1")
(host-eq "localhost" (%whatever-to-address "127.0.0.1"))
(Host-eq "localhost" "www.franz.com")
|#

(defun host-http-name (host)
  "Returns the internet host name for HOST."
  (%whatever-to-hostname host))

#|
(host-http-name (%dummy-hostname))
(host-http-name (%dummy-dotted))
(host-http-name (%whatever-to-address "127.0.0.1"))
|#

(defun ip-address-for-host-domain-name (host)
  (ip-address-for-parsed-ip-address host))

(defun ip-address-for-parsed-ip-address (ip-number)
  "Returns an IP address as a string from, IP-NUMBER, the parsed address."
  (if (integerp ip-number)
    (%ipaddr-to-dotted ip-number)
    (if (http:ip-address-string-p ip-number)
        ip-number
      (if (%resolve-hostnames)
        (%ipaddr-to-dotted (%lookup-hostname ip-number))
        (%dummy-dotted)))))

#|

(ip-address-for-parsed-ip-address (%dotted-to-ipaddr "123.1.1.90"))
(ip-address-for-parsed-ip-address  "localhost")

|#


(defun domain-name-for-ip-address (address &optional (no-error-p t))
  "Given the IP address, ADDRESS, this returns the domain name or NIL."
  (or
   (%whatever-to-hostname address)
   (if no-error-p
       nil
     (error "Domain Name for Ip not found ~a~%" address))))

#|

(domain-name-for-ip-address (%dotted-to-ipaddr "123.1.1.90"))
(domain-name-for-ip-address (%dotted-to-ipaddr "123.1.1.90") nil)

|#
 
(defun domain-name-for-parsed-ip-address (ip-number &optional (no-error-p t))
  ;;
  ;; my guess: this is supposed to return the fully qualified domain
  ;;  name for a given ip address, if on exists, else it returns the
  ;;  dotted ipaddr in a string
  ;;
  (declare (ignore no-error-p))
  (if (%resolve-hostnames)
      (let ((name (%ipaddr-to-hostname ip-number)))
        (or name (%ipaddr-to-dotted ip-number)))
    (%ipaddr-to-dotted ip-number)))

(defun %parse-internet-address (address)
  "Returns an IP-NUMBER which is integer denoting the address of host."
  ;; address is an ipaddr, or string holding a dns name for a
  ;; machine, or a string containing a dotted ip address.
  (%whatever-to-address address)
  )

(defun parse-host (address &optional no-error-p)
  "Top-level method for parsing a host ADDRESS."
  (if no-error-p
      (handler-case
          (%whatever-to-address address)
        (error () nil))
    (%whatever-to-address address)))

;;; this guy is the integer representation
(defun local-host-parsed-ip-address (&optional recache-p)
  "Returns the parsed IP address of the local host."
  (cond ((and (not recache-p) http:*local-host-address*))
        (t (setq http:*local-host-address* (%get-localhost-parsed-address)))))


;;; this guy is dotted
(defun local-host-ip-address (&optional recache-p)
  "Returns the dotted IP address of the local host."
  (cond ((and (not recache-p) http::*local-host-ip-address*))
        (t (setq http::*local-host-ip-address* (%get-localhost-dotted-address)))))

;;; this guy is the logical string (assume including domain)
(defun local-host-domain-name (&optional recache-p)
  "Returns the local host domain name."
  (cond ((and (not recache-p) http::*local-host-domain-name*))
        (t (setq http::*local-host-domain-name*
                 (or http:*http-host-name* (%get-localhost-hostname))))))


;;; fixme, there is a proclamation with an optional parameter
;;; check in the lispwork sources 
(defun local-host ()
  "The host object for the local host on which we are running."
  (or http::*local-host-address*
      (setq http::*local-host-address* (%get-localhost-parsed-address))))


(defun host-mail-name (host)
  "The internet mail name for HOST."
  (domain-name-for-ip-address host t))

(defun local-host-ip-address-moved-p ()
  "Returns non-null if the local host IP address has changed."
  nil)

(defun %local-host-domain-name()
  "Computes the new domain name"
  (Error "Don't know how to update the domain name~%"))

(defun split-dotted-ip (address)
  (if (string= address "127.0.0.1")
      (list 127 0 0 1)
    (progn
      (warn "Don't know how to split address~a~%" address)
      (list -1 -1 -1 -1))))
  
(defun ip-host-trusted-p (address secure-subnets &optional network)
  "Returns non-null if IP-address address is trusted given secure-subnets."
  (declare (ignore network))
  (labels ((split-ip (address)
                     (declare (values (i1 i2 i3 i4)))
                     (if (vector-ip-adress-p address)
                         (list (aref address 0)(aref address 1)(aref address 2)(aref address 3))
                       (if (stringp address)
                           (split-dotted-ip address)
                         `(,(ldb (byte 8  0) address)
                             ,(ldb (byte 8  8) address)
                             ,(ldb (byte 8 16) address)
                             ,(ldb (byte 8 24) address)))))
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


(define %host-log-name (address host &optional resolve-ip-address-p)
  "Returns a string for use in logging server access."
  (declare (ignore host))
  (if resolve-ip-address-p
      (domain-name-for-parsed-ip-address address t)
    (ip-address-for-parsed-ip-address address)))

#|

(parse-host "127.0.0.1")
(parse-host "localhost")
(parse-host "www.franz.com")
(parse-host "www.franz.com" t)
|#
  
(defmacro with-automatic-login ((host user-id user-pw) &body body)
  "Supplies userid and PW to ensure successul FTP login to host with BODY."
  `(progn (notify-log-window "~&(WITH-AUTOMATIC-LOGIN (~S ~S ~S) - Not available in this port"
                             ,host ,user-id ,user-pw)
     ,@body))

;;; Errors

(define-condition host-stopped-responding (error) 
  ; a necessary placeholder but we don't signal it yet
  ())

(define-condition network-error-mixin () 
  ; a necessary placeholder but we don't signal it yet
  ())

(define-condition unknown-host-name (simple-error)
  ((address :initform 0 :initarg :address))
  (:default-initargs :format-control "host name not found for address ~D."))

(deftype file-not-found () 
  "Specialization of Common Lisp File-error in which the file was not found on open."
  '(and condition file-error))

;;; Internals

(defparameter *%internal-hostaddress* nil)
(defparameter *%internal-hostname* nil)

(defun %get-localhost-parsed-address ()
  (or *%internal-hostaddress*
      (setq *%internal-hostaddress* (%whatever-to-address (%dummy-dotted))))
  )

(defun %get-localhost-dotted-address ()
  (%ipaddr-to-dotted (%get-localhost-parsed-address)))

(defun %get-localhost-hostname ()
  (or *%internal-hostname*
      (%whatever-to-hostname (%get-localhost-parsed-address)))
  )

(defun %resolve-hostnames ()
  http:*resolve-ip-addresses*)

(defun %dummy-dotted ()
  "127.0.0.1")

(defun %dummy-hostname ()
  "localhost.localdomain")

(defun %whatever-to-address (what)
  (if (integerp what)
      what
      ;;; In SBCL could be an array
      (if (vector-ip-adress-p what)
	  what
	  (if (stringp what)
	      (if (http:ip-address-string-p what)
		  (%dotted-to-ipaddr what)
		  (if (%resolve-hostnames)
		      (if (string= what (%dummy-hostname))
			  (%dotted-to-ipaddr (%dummy-dotted))
			  (%lookup-hostname what))
		      (%dotted-to-ipaddr (%dummy-dotted))))
	      (error "Wrong parameter~a~%" what)))))

#|
(%lookup-hostname (%dummy-hostname))
(time 
 (%whatever-to-hostname (%whatever-to-address (%dummy-hostname)))
|#

(defun %whatever-to-hostname (host)
  (cond ((integerp host)
         (if (%resolve-hostnames)
             (%ipaddr-to-hostname host)
           (%dummy-hostname)))
        ((vector-ip-adress-p host)
         (%whatever-to-hostname
           (format nil "~D.~D.~D.~D"
                   (aref host 0) (aref host 1) (aref host 2) (aref host 3))))
        ((http:ip-address-string-p host)
         (if (%resolve-hostnames)
             (%ipaddr-to-hostname (%dotted-to-ipaddr host))
           host))
        (t host)))

;;; borrowed from jkf
(defun note-hostname (given-host)
  ;; we are starting cl-http and the user has specied that host
  ;; is the host we are serving.
  ;; if it's nil the we have to figure it out for ourself
  (%note-hostname (or given-host (%guess-hostname))))

(defun %note-hostname (host)
  (cond 
   ((integerp host)
    (setq *%internal-hostaddress* host)
    ;;; try to get the domain
    (let ((domain (domain-name-for-ip-address host)))
      (if domain
          (setq *%internal-hostaddress* domain)
        (setq domain (%ipaddr-to-dotted host)))))
   ((stringp host)
    ;assume it is a string with a nice looking hostname
    ;don't really know what to do about localhost, accept for now
    (unless (or (position #\. host)(string= host "localhost"))
      (error "Address ~a is not a valid host~%" host))
    (setq 
     *%internal-hostaddress* (%whatever-to-address host)
      *%internal-hostname* host))
   (t (error "I don't understand this hostname ~a~%" host))))

#|
(%note-hostname "localhost")
(%note-hostname "localhost.localdomain")
|#

(defun %guess-hostname ()
  (let ((name (long-site-name)))
    (if (null (position #\. name))
        ; will have to see if we can find the domain
        (let* ((ipaddr (%whatever-to-address name))
               (newname (domain-name-for-parsed-ip-address ipaddr)))
          (when (null (position #\. newname))
            (error "You must specify a :host argument to start"))
          newname)
      name)))



#|
acl interface
SOCKET:LOOKUP-HOSTNAME
 -> 32 bit Address
socket:ipaddr-to-dotted
socket:dotted-to-ipaddr
socket:ipaddr-to-hostname
excl:socket-error is the error class

socket:with-pending-connect
  with-timeout
socket:accept-connection
 -> stream
socket:make-socket
socket:shutdown
socket:socket-control

|#

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

(defun find-direct-connection (host &key port)
  (%make-client-socket host :port port)
  )

(defun %open-http-stream-to-host (host port timeout-in-seconds)
  (%connect-with-timeout host port timeout-in-seconds)
  )

(defun open-tcp-stream (host port timeout)
  "Opens a TCP stream to HOST on PORT with TIMEOUT."
  (%open-http-stream-to-host host port (ceiling timeout 60.)))

(defun ensure-http-protocol-on-port (port)
  ;; make sure we're running a server on this port
  (or (http:http-service-enabled-p port) 
      (http::enable-http-stream-server port)))

(defun live-connection-p (http-stream)
  "Returns non-null if the TCP/IP connection over HTTP-STREAM remains alive
in that the remote host continue to respond at the TCP/IP level."
  (open-stream-p http-stream))


