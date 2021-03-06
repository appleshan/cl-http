;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: IPC; Base: 10 -*-

;; ACL 5.0 port for the tcp classes

(defpackage "IPC"
  (:use :socket :cl :excl)
  (:export "INTERNET-ADDRESS"
           "IP-ADDRESS-STRING"        ; this are used with ipc:: doesn't hurt to export it
           "GET-HOST-NAME-BY-ADDRESS" ; ditto
           "PROTOCOL-ERROR"
           "*domainname*"             ; ditto
           "FRANZ-HOSTNAME"
           "GETDOMAINNAME"
           "TCP-CLIENT-STREAM"        ; used as ipc:: in places
           "TCP-STREAM-PROCESS"       ; ditto
           "TCP-CLIENT-ALIVE"         ; ditto
           "TCP-SERVER-STREAM"
           "STREAM-READ"
	   
	   #:compute-host-domain-name 
	   ))

(in-package "IPC")

(eval-when (load eval)
  ;; the terms hostname and domain name are used in seemingly many 
  ;; ways in the cl-http code.
  ;; In this module the meaning is this:
  ;; given a fully qualified dns name for a machine, e.g foo.bar.com
  ;; the
  ;;   *hostname* is the part before the first .
  ;;   *domainname* is the part after the first . including the .
  ;;   *local-hostdomainname* is the whole thing.
  (defvar *hostname* nil)
  (defvar *domainname* nil)
  (defvar *local-hostdomainname* nil)
)

;; A few functions for converting between IP address representations

; DNS symbolic name -> 32-bit address
(defmethod internet-address ((name string))
   #+ignore
   (if (equal name *local-hostdomainname*)
      (lookup-hostname *local-hostdomainname*)
      (socket:lookup-hostname name))
   (socket:lookup-hostname name))

; 32-bit address -> dotted string "#.#.#.#"
(defmethod ip-address-string ((address integer))
   (socket:ipaddr-to-dotted address))

; 32-bit address -> DNS symbolic name
(defmethod get-host-name-by-address ((address integer))
   (socket:ipaddr-to-hostname address))

;; this is shadow-imported into www-utils
(deftype unknown-host-name () #-acl5 'socket::socket-error #+acl5 'excl::socket-error)

(define-condition protocol-error (error)
  ((stream :initform nil :initarg :stream)))

(defun franz-hostname ()
  (cond (*hostname*)
        ((gethostdomainname) *hostname*)))

(defun getdomainname (&optional (where t) (domwhere "HTTP:acl;acl5;defaultdomain"))
  (typecase where
    (string
     (setq *domainname* where))
    (null
     (setq *domainname* nil))
    (pathname
     (setq dowhere (merge-pathnames "defaultdomain" where))
     ;; Use different file path for ACLPC
     (gethostdomainname where)))
  (or *domainname*
      (if (probe-file domwhere)
	  (with-open-file (stream domwhere :direction :input)
	     (setq *domainname* (read-line stream nil nil))))
      (when (setq *domainname* (user::prompt-user-string "Enter domain name (without host): "))
	(with-open-file (stream domwhere :direction :output :if-exists :supersede)
	  (write-string *domainname* stream)
	  (terpri stream))
	*domainname*)))

(defun gethostdomainname (&optional (where "HTTP:acl;acl5;hostdomain"))
  (let ((domwhere (merge-pathnames "defaultdomain" where)))
    (or *local-hostdomainname*
	(if (probe-file where)
	    (with-open-file (stream where :direction :input)
	        (setq *local-hostdomainname* (read-line stream nil nil)))))
    (or *domainname*
	(if (probe-file domwhere)
	    (with-open-file (stream domwhere :direction :input)
	        (setq *domainname* (read-line stream nil nil)))))
    (if (and *local-hostdomainname* (> (length *local-hostdomainname*) 0))
	(let (domwasnone)
	  (if *domainname*
	      (let ((dopos (- (length *local-hostdomainname*) (length *domainname*))))
		(if (and (> dopos 1)
			 (char= (elt *local-hostdomainname* (1- dopos)) #\.))
		    (setq *hostname* (subseq *local-hostdomainname* 0 (1- dopos)))))
	    (setq domwasnone t))
	  ;; this is a guess we can't tell how long is the domain name really
	  (unless *hostname*
	    (let ((point (position #\. *local-hostdomainname*)))
	      (when (and point (> point 1))
		(setq *hostname* (subseq *local-hostdomainname* 0 point)
		      *domainname* (subseq *local-hostdomainname* (1+ point)))
		(if (string-equal *hostname* "www")
		    (progn (setq point (position #\. *local-hostdomainname* :start (1+ point)))
			   (setq *hostname* (subseq *local-hostdomainname* 0 point)
				 *domainname* (subseq *local-hostdomainname* (1+ point))))))))
	  (if (and domwasnone *domainname* (> (length *domainname*) 0))
	      (with-open-file (stream domwhere :direction :output :if-exists :supersede)
		  (write-string *domainname* stream)
		  (terpri stream))))
    ;; else
    (with-open-file (stream where :direction :output :if-exists :supersede)
	(setq *hostname* (user::prompt-user-string "Enter host name (without domain): ")
	      *local-hostdomainname* (concatenate 'string *hostname* "." (getdomainname t domwhere)))
	(write-string *local-hostdomainname* stream)
	(terpri stream))))
  *local-hostdomainname*)


(defun compute-host-domain-name (given-host)
  ;; we are starting cl-http and the user has specified that host
  ;; is the host we are serving.
  ;; if it's nil the we have to figure it out for ourself
  
  (if* given-host
     then ; check to see if it's a dotted ip address
	  (let ((ipaddr (dotted-to-ipaddr given-host :errorp nil)))
	    (if* ipaddr
	       then (setq given-host (ipaddr-to-hostname ipaddr)))))
  
  (if* given-host
     then (multiple-value-bind (ok whole host domain)
	      (match-regexp "\\([^.]+\\)\\(.*\\)" given-host)
	    (declare (ignore whole))
	    (if* ok
	       then
		    (setq *hostname* host
			  *domainname* domain
			  *local-hostdomainname* given-host)
	       else (error "~s isn't a valid host name" given-host)))
     else ; must compute it.
	  (let ((name (long-site-name)))
	    (if* (null (position #\. name))
	       then ; will have to see if we can find the domain
		    (let ((ipaddr (socket:lookup-hostname name)))
		      (let ((newname 
			     (socket:ipaddr-to-hostname ipaddr
							:ignore-cache t)))
			(if* (null (position #\. newname))
			   then (error 
				 "you must specify a :host argument to start"))
			(setq name newname))))
		
	    ;; ok.. fully qualified
	    (compute-host-domain-name name)
	    (format t "computed host: ~s, domain ~s~%" 
		    *hostname*
		    *domainname*))))
	           
		    
		    
			    
  
  
;;;
;;; TCP classes
;;;

(defclass tcp-client-stream (#-(version>= 6) socket::socket-stream-internet-active-bivalent
			     #+(version>= 6) socket::socket-stream-internet-active)
    ((listen-sockaddr :initform nil :initarg :listen-sockaddr)
     (process :initform nil :accessor tcp-stream-process)
     (alive   :initform t   :accessor tcp-client-alive)
     ))

#+(and acl5 (version>= 6))
(defmethod tcp-client-alive ((stream excl:socket-simple-stream)) t)

(defmethod tcp-client-alive ((stream t))
  nil)

(defmethod (setf tcp-client-alive) (value (stream t))
  value)

(defclass tcp-server-stream (socket::socket-stream-internet-passive)
    ((listen-socket-fd :initarg :fn-in)
     (listen-sockaddr :initarg :listen-sockaddr)
     (client-streams :initform nil)
     (process :initform :initializing :accessor tcp-stream-process)))

(defvar *sync-stream* T)

(defmethod stream-read ((stream tcp-server-stream))
  #-(and acl5 (version>= 6)) (change-class (socket:accept-connection stream) 'tcp-client-stream)
  #+(and acl5 (version>= 6)) (socket:accept-connection stream))

;; like listen, but for passive sockets (which aren't streams)
;; returns a client stream if a connection is pending, nil otherwise
(defmethod server-listen ((stream tcp-server-stream))
   (let ((new-connection (socket:accept-connection stream :wait nil)))
     #-(and acl5 (version>= 6))
      (if new-connection
         (change-class new-connection 'tcp-client-stream)
         nil)
      #+(and acl5 (version>= 6))
      new-connection))

(defmethod close :around ((stream tcp-server-stream) &key (abort t))
  (with-slots (process) stream
    (setq process nil))
  (clim-sys:process-yield)
  (call-next-method stream :abort abort))
