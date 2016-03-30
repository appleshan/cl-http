;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: www-utils -*-

;;; (C) Copyright 1994-1995, 2003, 2005-2007, 2009, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;; (C) Allegro enhancements Copyright 1995, OBC. All Rights Reserved.
;;;
;;; LispWorks enhancements Copyright (C) 1995-2000 Xanalys Inc.  All rights reserved.
;;;
(in-package :www-utils)
;;;------------------------------------------------------------------- 
;;;
;;; LISPWORKS SPECIFIC CODE
;;;

;; Copied cold enable definitions from http:lw;start.lisp so that they would run compiled -- JCMa 8/18/2005

#+LispWorks
(defun lw-get-cl-http-init-script-from-command-line ()
  (second (member "-cl-http-init-script" sys:*line-arguments-list* :test 'string-equal)))

#+LispWorks
(defun lw-cold-enable-http-service ()
  (push '("cl-http init" nil lw-cold-enable-http-service-mp) mp:*initial-processes*)
  (mp:initialize-multiprocessing))

#+LispWorks
(defun lw-cold-enable-http-service-mp ()
  (let ((init-script (or (lw-get-cl-http-init-script-from-command-line) cl-user::*cl-http-init-script*)))
    (when (and init-script
               (typep init-script 'string)
               (not (eql (aref init-script 0) #\-))) ;allow starting with no CL-HTTP init -- JCMa 8/17/2005
      (load init-script)
      (http:enable-http-service))))

;;;------------------------------------------------------------------- 
;;;
;;; MCL AND LISPM COMPATABILITY CODE ADAPTED FOR LISPWORKS
;;;

(define report-condition (condition stream)
  "Prints the report string for CONDITION onto STREAM."
  (handler-case (format stream "~A" condition)
    ;; Cannot guaranty all errors are printable.
    (error ()
	   (format stream "~&~A~%" condition)
	   (describe condition stream))))

(define report-string (condition)
  "Returns the report string for CONDITION."
  (with-output-to-string (stream)
    (report-condition condition stream)))

(defmethod http::write-stack-backtrace ((error condition) stream &optional n-frames)
  (declare (ignore n-frames))
  (let ((*print-pretty* t)) ;make these more readable
    (dbg::output-backtrace :bug-form stream)))

;; Define equivalence mapping to the MCL case.
(deftype file-not-found () 
  "Specialization of Common Lisp File-error in which the file was not found on open."
  '(and condition file-error))

;; Hack for proxy database.
(deftype http::directory-not-found () 'error)

#+lispworks3.2
(defun special-operator-p (symbol)
   (special-form-p symbol))

#+lispworks3.2
(export 'special-operator-p :www-utils)

; with-tcp-port-for-protocol not used !

;; should be obsolete -- JCMa 9/9/2003
(defmacro with-array-registers (bindings &body body)
  `(let ,bindings ,@body))

#+clim-sys
(define-macro atomic-incf (reference &optional (delta 1))
  "Atomically increments REFERENCE by DELTA."
  `(clim-sys:atomic-incf ,reference ,delta))

#+clim-sys
(define-macro atomic-decf (reference &optional (delta 1))
  "Atomically decrements REFERENCE by DELTA."
  `(clim-sys:atomic-decf ,reference ,delta))

#+(and clim-sys (or LispWorks4 LispWorks5))
(define-macro atomic-push (item reference)
  "Atomically pushes ITEM onto REFERENCE."
  `(clim-sys:without-scheduling
     (push ,item ,reference)))

#+(and LispWorks (not (or LispWorks4 LispWorks5)))
(define-macro atomic-push (item reference)
  "Atomically pushes ITEM onto REFERENCE."
  `(sys:atomic-push ,item ,reference))

#+(and clim-sys (or LispWorks4 LispWorks5))
(define-macro atomic-pop (reference)
  "Atomically pops an item off REFERENCE."
  `(clim-sys:without-scheduling
     (pop ,reference)))

#+(and LispWorks (not (or LispWorks4 LispWorks5)))
(define-macro atomic-pop (reference)
  "Atomically pops an item off REFERENCE."
  `(sys:atomic-pop ,reference))

; The macro ATOMIC-CONDITIONAL-REPLACEF is only used once in  http:server;data-cache.lisp .
; It will not be used by LispWorks 6.

#+(and clim-sys (or LispWorks4 LispWorks5))
(define-macro atomic-conditional-replacef (reference predicate new-value)
  "When PREDICATE returns non-null, this setfs REFERENCE to NEW-VALUE.
Predicate is called (OLD-VALUE NEW-VALUE). The operation 
assures that precicate application ande swap are atomic."
  (declare (values old-value))
  (let ((old-value (gensym))
          (new-value-var (gensym))
	  (pred (gensym)))
     `(clim-sys:without-scheduling
          (let ((,old-value ,reference)
                  (,new-value-var ,new-value)
                  (,pred ,predicate))
             (when (funcall ,pred ,old-value ,new-value-var)
                 (prog1 ,old-value
                    (setf ,reference ,new-value-var)))))))

(declaim (inline arglist))

(defun arglist (function)
  "Returns the arglist for FUNCTION."
  (declare (values (arglist values type arglist-types value-types)))
  #-lispworks4.0   ; 4.0 is an anomaly: LW package in bother newer and older versions!
  (lw:function-lambda-list function)
  #+lispworks4.0
  (hcl:function-lambda-list function))

;;;------------------------------------------------------------------- 
;;;
;;; OBTAINING THE DOMAIN NAME FOR AN IP ADDRESS
;;;

(declaim (inline %parse-internet-address))

(defun %parse-internet-address (address)
  "Returns an IP-NUMBER which is integer denoting the address of host."
  (declare (values ip-number))
  (etypecase address
    (integer address)
    (string (or (ipc:string-ip-address address)
		(ipc:internet-address address)))))

(declaim (inline ip-address-for-parsed-ip-address))

(define ip-address-for-parsed-ip-address (ip-number)
  "Returns an IP address as a string from, IP-NUMBER, the parsed address."
  (ipc:ip-address-string ip-number))

(defvar *last-domain-name* nil)

;;; There are surely better strategies to get your domain name.
;;; But they depend on your OS. Can someone shine light on
;;; getting DNS domain names portably on UNIX?
;;;
#|
;; this is a loser on cl-http.org.  Prompts for a domain name when it loses, which is every time.  -- JCMa 5/2/2007
(defun default-domain-name (&optional (where "HTTP:lw;defaultdomain"))
  (flet ((dnp (dn)
           (and (stringp dn)
		;; Need at least one of #\. or this may not be DNS!
		;; Number may vary with contries?
                (find #\. dn))))
    (let ((dn0 (ipc:getdomainname))
          dn)
      (cond ((dnp dn0)
             dn0)
            ((and
              (setq dn (if (probe-file where)
                           (with-open-file (stream where :direction :input)
                             (read-line stream))))
              (dnp dn))
             (setq *last-domain-name* dn))
            (t (www-utils::local-host-domain-name))
            (t (warn "Unexpected DNS domain name ~s." (or dn dn0))
               (format t "~&Enter you local DNS domain name 'local.institution.type': ")
               (peek-char t)
               (setq dn (read-line))
               (cond ((dnp dn)
                      (setq *last-domain-name* dn)
                      (with-open-file (stream where :direction :output
                                              :if-does-not-exist :create
                                              :if-exists :supersede)
                        (write-string dn stream)
                        (terpri stream))
                      dn)
                     (t "the.unknown.domain")))))))|#

(defparameter *prompt-for-default-local-domain* nil)

(defun default-domain-name (&optional (where "HTTP:lw;defaultdomain"))
  (flet ((dnp (dn &aux l)
           (and (stringp dn)
                (> (setq l (the fixnum (length dn))) 3)
		;; Need at least one of #\. or this may not be DNS!
		;; Number may vary with countries?
                (position #\. dn :start 0 :end l)
                (not (position #\space dn :start 0 :end l))))
         (ip-address-p (dn)
           (every #'(lambda (ch) (or (digit-char-p ch) (eql ch #\.))) dn))
         (trim-domain (string)
           (let ((pos (position #\. string)))
             (if pos
                 (subseq string (1+ (the fixnum pos)))
               string)))
         (read-domain-name (file)
           (if (probe-file file)
               (with-open-file (stream file :direction :input)
                 (read-line stream))))
         (save-domain-name (name file)
           (with-open-file (stream file :direction :output :if-does-not-exist :create :if-exists :supersede)
             (write-string name stream)
             (terpri stream)))
         (prompt-for-domain-name (&optional (stream *terminal-io*))
           (format stream "~&Enter you local DNS domain name 'local.institution.type': ")
           (peek-char stream)
           (string-trim '(#\space #\tab) (read-line stream nil))))
    (let ((dn0 (ipc:getdomainname))
          dn)
      (cond ((dnp dn0)
             dn0)
            (*last-domain-name*)
            (t (let ((local-host (www-utils::local-host-domain-name)))
                 (cond ((or (ip-address-p local-host)
                            (equalp local-host "localhost")) ;don't lose when running disconnected as 127.0.0.1 -- JCMa 11/25/2007
                        (cond ((and (setq dn (read-domain-name where))
                                    (dnp dn))
                               (setq *last-domain-name* dn))
                              ((not *prompt-for-default-local-domain*)
                               (error "No default domain name available."))
                              ;; code below here is going to hang threads so you want this switch off -- JCMa 11/25/2007
                              ((dnp (setq dn (prompt-for-domain-name)))
                               (save-domain-name dn where)
                               (setq *last-domain-name* dn))
                              (t #+MAC (machine-instance)
                                 #-MAC "the.unknown.domain")))
                       (t (setq *last-domain-name* (trim-domain local-host))))))))))

;; This could grow large and not be reclaimed. Added weak-kind argument  -- JCMa 11/25/2007
(defvar *domain-name-lookup* (make-hash-table :test #'eql :weak-kind :both))

(define domain-name-for-parsed-ip-address (ip-number &optional (no-error-p t))
  (or (gethash ip-number *domain-name-lookup*)
      (setf (gethash ip-number *domain-name-lookup*)
            (let ((host-name (and http:*resolve-ip-addresses*
                                  (if no-error-p
                                      (ignore-errors (ipc:get-host-name-by-address ip-number))
                                    (ipc:get-host-name-by-address ip-number)))))
              (cond ;; No error fall-back
                    ((or (null host-name)
                         (http::null-string-p host-name)) ;failed lookup comes back with null string in LW 5.0.3 under Mac OS X 10.4
                     (ip-address-for-parsed-ip-address ip-number))
                    ((or (position #\. host-name) ; some OSes return fully qualified names
                         (equalp host-name "localhost")) ;don't lose when running disconnected as 127.0.0.1 -- JCMa 10/3/2003
                     host-name)
                    (t (concatenate 'string host-name "." (default-domain-name))))))))

(declaim (notinline domain-name-for-ip-address))

(define domain-name-for-ip-address (address &optional (no-error-p t))
  "Given the IP address, ADDRESS, this returns the domain name or NIL."
  (cond (no-error-p
         (let ((internet-address (handler-case 
                                     (%parse-internet-address address)
                                   (unknown-host-name () nil))))
           (when internet-address
             (domain-name-for-parsed-ip-address internet-address t))))
        (t (domain-name-for-parsed-ip-address (%parse-internet-address address) nil))))

(define ip-address-for-host-domain-name (domain-name &optional no-error-p)
  "Returns the IP address string for DOMAIN-NAME.
When ERROR-P is non-null, NIL is returned when the domain lookup fails."
  (cond (no-error-p
         (handler-case 
             (ip-address-for-parsed-ip-address (http::parse-internet-address domain-name))
           (unknown-host-name () nil)))
        (t (ip-address-for-parsed-ip-address (http::parse-internet-address domain-name)))))

(define %local-host-domain-name-for-parsed-ip-address (ip-number)
  (if (zerop ip-number)
      ;; zerop means no network connection and no DNS
      (ip-address-for-parsed-ip-address ip-number)
    ;; normal case wth network and DNS
    (domain-name-for-parsed-ip-address ip-number)))

(defun %local-host-primary-parsed-ip-number ()
  "Returns the current real primary parsed IP numbwe (no caching) for the local host."
  (ipc:internet-address (machine-instance)))

(defun %local-host-primary-ip-address ()
  "Returns the current real primary IP address (no caching) for the local host."
  (ip-address-for-parsed-ip-address (%local-host-primary-parsed-ip-number)))

(define %local-host-primary-domain-name ()
  "Returns the current real primary domain name (no caching) for the local host."
  (%local-host-domain-name-for-parsed-ip-address (%local-host-primary-parsed-ip-number)))

(defvar *primary-network-host* nil
  "Overrides the default host name or IP address considered primary for network services.

Set this to a string containing the domain name or IP address that should be used as the primary
name for the local host. If set to localhost (or 127.0.0.1), then localhost will be the primary
address for your HTTP server, and will appear as the default host names for URLs. If the primary
host is localhost, then to access your server properly from an external host, you will need to add
virtual host nicknames for the external IP addresses as they change.  This option is particularly
useful for computing with changing IP address, as in the cases of mobile laptops or dynamic IP
addresses.

Alternatively, you can select an IP address other than your primary address, such as a second IP
address on the same or a different network card. This enables you to provide HTTP service on a
particular IP address without installing virtual host nicknames.")

(defun local-machine-instance ()
  "Returns the primary domain name (or IP number) for the local host."
  (or *primary-network-host* (machine-instance)))

(defun %local-host-parsed-ip-number ()
  (let ((local-machine-instance (local-machine-instance)))
    (or (ipc:string-ip-address local-machine-instance)
        (ipc:internet-address local-machine-instance))))

(define local-host (&optional recache-p)
  "The host object for the local host on which we are running.  For this port, we use the IP address."
  (local-host-parsed-ip-address recache-p))

(defun %local-host-ip-address ()
  "Returns the current local host IP address (no caching)."
  (ip-address-for-parsed-ip-address (%local-host-parsed-ip-number)))

(define local-host-ip-address (&optional recache-p)
  "Returns the IP address of the local host."
  (cond ((and (not recache-p) http::*local-host-ip-address*))
        (t (setq http::*local-host-ip-address* (%local-host-ip-address)))))

(define local-host-parsed-ip-address (&optional recache-p)
  "Returns the parsed IP address of the local host."
  (cond ((and (not recache-p) http:*local-host-address*))
        (t (setq http:*local-host-address* (%local-host-parsed-ip-number)))))

(define %local-host-domain-name ()
  (%local-host-domain-name-for-parsed-ip-address (%local-host-parsed-ip-number)))

(defun local-host-domain-name (&optional recache-p)
  "Returns the local host domain name."
  (cond ((and (not recache-p) http:*local-host-domain-name*))
        (t (setq http:*local-host-domain-name* (%local-host-domain-name)))))

(defun local-host-ip-address-moved-p ()
  "Returns non-null if the local host IP address has been changed."
  (not (equal http::*local-host-ip-address* (%local-host-ip-address))))

#+UNIX
(defun add-loopback-host-virtual-host ()
   (unless (equal (local-host-ip-address) "127.0.0.1")
      (http:add-virtual-host-nick-name "127.0.0.1" (http::standard-http-port) (http:local-context))))

;; Allow connections to the loopback host "127.0.0.1" to always map to whatever the default 
;; server address and port might be. -- 9/15/03 JCMa

#+UNIX
(add-initialization "Add Loopback Virtual Host for UNIX" '(add-loopback-host-virtual-host) 
                    '(:normal) 'http:*server-launch-initialization-list*)

;; Execute shutdown initialization list when exiting LispWorks
#-LispWorks3.2
(lw:define-action "When quitting image" "Run Shutdown initializations" 'http::run-shutdown-initializations)
;;;------------------------------------------------------------------- 
;;;
;;; HOST RELATED
;;;

(define parse-host (address &optional no-error-p)
  "Top-level method for parsing a host ADDRESS."
  (declare (optimize (speed 3)) 
           (values ip-number))
  (cond (no-error-p
         (handler-case
             (http::parse-internet-address address)
           (network-error () nil)))
        (t (http::parse-internet-address address))))

(declaim (inline host-mail-name))

(define host-mail-name (host)
  "The internet mail name for HOST."
  (domain-name-for-ip-address host t))

(define host-eq (host1 host2)
  "Returns non-null if HOST1 is equal to HOST2."
  (cond ((or (null host1) (null host2))
         nil)
        ((and (integerp host1) (integerp host2))
         (= host1 host2))
        (t (= (http::parse-internet-address host1)
              (http::parse-internet-address host2)))))

(define host-http-name (host)
  "Returns the internet host name for HOST."
  (host-mail-name host))

(declaim (inline %host-log-name))

(define %host-log-name (address host &optional resolve-ip-address-p)
  "Returns a string for use in logging server access."
  (declare (ignore host))
  (if resolve-ip-address-p
      (domain-name-for-parsed-ip-address address t)
      (ip-address-for-parsed-ip-address address))) 

;;;------------------------------------------------------------------- 
;;;
;;; AN INTERFACE CONSISTENT WITH PROPERTY LIST MIXIN FOR PATHNAMES
;;;
;;; This facility compensate for Lisps that can't implement pathname property lists
;;; directly on pathnames. Lack of EQ pathnames means that a plist slot on a pathname 
;;; is useless.
(defvar *pathname-property-list-table* nil
  "Holds the property lists for pathnames in the file system.")

(declaim (inline pathname-property-list-table))

;; pathnames are not eq in LW 4.3!!! -- JCMa 9/4/2003
;; change the hashtable test to EQ when they are fixed.

(defun pathname-property-list-table ()
  (or *pathname-property-list-table*
      (setf *pathname-property-list-table* (make-hash-table :test #'equal))))

(defun clear-pathname-property-list-table ()
   (when *pathname-property-list-table*
       (clrhash *pathname-property-list-table*)))

(declaim (inline %pathname-property-list))

(defun %pathname-property-list (pathname)
  (gethash pathname (pathname-property-list-table)))

(declaim (inline %set-pathname-property-list))

(defun %set-pathname-property-list (pathname plist)
  (setf (gethash pathname (pathname-property-list-table)) plist))

(defsetf %pathname-property-list %set-pathname-property-list)

(declaim (inline %pathname-property-list-put-value))

(defun %pathname-property-list-put-value (pathname indicator value)
   (let ((plist (%pathname-property-list pathname)))
      (prog1 (setf (getf plist indicator) value)
         (setf (%pathname-property-list pathname) plist))))

(declaim (inline %remove-property-list))

(defun %remove-property-list (pathname)
  (remhash pathname (pathname-property-list-table))) 

(defmethod http:get-value ((pathname pathname) indicator &optional default)
  (let ((value (getf (%pathname-property-list pathname) indicator :+not-found+)))
    (case value
      (:+not-found+
        (values default nil))
      (t (values value t))))) 

(defmethod http::%put-value ((pathname pathname) indicator value)
   (%pathname-property-list-put-value pathname indicator value))

(defmethod (setf http:get-value) (value (pathname pathname) indicator &optional default)
   (declare (ignore default))
   (%pathname-property-list-put-value pathname indicator value)) 

(defmethod http:remove-value ((pathname pathname) indicator)
  (let ((plist (%pathname-property-list pathname)))
    (when plist
      (prog1 (remf plist indicator)
             (if plist
                 (setf (%pathname-property-list pathname) plist)
                 (%remove-property-list pathname))))))

(defmethod http:property-list ((pathname pathname))
  (%pathname-property-list pathname))

(defmethod http:map-indicators ((pathname pathname) function)
  (loop for item in (%pathname-property-list pathname) by #'cddr
        do (funcall function item)))

(defmethod http:map-values ((pathname pathname) function)
  (loop for item in (cdr (%pathname-property-list pathname)) by #'cddr
        do (funcall function item))) 

;;;------------------------------------------------------------------- 
;;;
;;; FILE RELATED OPERATIONS
;;;

(define file-stream-creation-date (file-stream)
  "Returns the creation date in universal time for FILE-STREAM's source file."
  (file-write-date file-stream))

(declaim (inline file-stream-length-in-bytes))

(define file-stream-length-in-bytes (file-stream)
  "Returns the length in bytes for FILE-STREAM's source file."
  (file-length file-stream))

(defgeneric file-length-in-bytes (pathname-or-url &optional new-length)
   (:documentation "Returns the number of bytes in PATHNAME-OR-URL or NIL."))

(declaim (inline %file-length-in-bytes))

(defun %file-length-in-bytes (pathname)
  #+lispworks3.2
  (file-length pathname)
  #-lispworks3.2
  (sys:file-size pathname))

(defmethod file-length-in-bytes ((pathname pathname) &optional new-length)
  (declare (optimize (speed 3)) 
           (ignore new-length))
  (%file-length-in-bytes pathname))

(defmethod file-creation-date ((pathname pathname))
  (file-write-date pathname))

#-UNIX ;; This makes no sense on UNIX.
(defmethod set-file-creation-date  ((pathname pathname) (universal-time integer) &optional error-p)
  (if error-p
      (set-file-dates pathname :creation universal-time)
    (ignore-errors (set-file-dates pathname :creation universal-time)))
  universal-time)

(define file-stream-modification-date (file-stream)
  "Returns the modification date in universal time for FILE-STREAM's source file."
  (file-write-date file-stream))

(defmethod file-modification-date ((pathname pathname))
  (file-write-date pathname))

(defmethod set-file-modification-date ((pathname pathname) (universal-time integer) &optional error-p)
  (if error-p
      (set-file-dates pathname :modification universal-time)
    (ignore-errors (set-file-dates pathname :modification universal-time)))
  universal-time)

(defmethod set-file-reference-date ((pathname pathname) (universal-time integer) &optional error-p)
  (if error-p
      (set-file-dates pathname :access universal-time)
    (ignore-errors (set-file-dates pathname :access universal-time)))
  universal-time)

(declaim (inline file-stream-version))

(defun file-stream-version (file-stream)
  (file-stream-modification-date file-stream))

(declaim (inline file-version))

(defun file-version  (pathname)
  (when (probe-file pathname)
    (file-modification-date pathname)))

(define file-properties (pathname)
  "Returns the length in bytes  and the modification in in universal time 
 for FILE-STREAM's source file."
  (declare (values length-in-bytes modification-date version))
  (let ((modification-date (file-modification-date pathname)))
    (values (%file-length-in-bytes pathname)
	    modification-date
	    modification-date)))

(defmethod set-file-author ((pathname pathname) (author string) &optional error-p)
  "Sets autheor of PATHNAME to be AUTHOR."
  (declare (values success-p))
  ;; set-file-owners can also set group under UNIX -- JCMa 8/30/2003
  (cond (error-p
         (set-file-owners pathname :author author)
         author)
        (t (ignore-errors 
             (set-file-owners pathname :author author)
             author))))

(defmethod set-file-author ((pathname pathname) (author null) &optional error-p)
  (declare (values success-p))
  (cond (error-p
         (set-file-owners pathname :author author)
         t)
        (t (ignore-errors 
             (set-file-owners pathname :author author)
             t))))
  
(declaim (inline pathname-directory-p))

(defun pathname-directory-p (pathname)
  "Returns non-null if PATHNAME¬denotes a directory."
  #+lispworks3.2
  (lw:directoryp pathname)
  #-LispWorks3.2
  ;; (lw:file-directory-p pathname) ;this one touches the file system -- JCMa 10/9/2003
  (system:directory-pathname-p pathname))

;;; When it's unclear what shell is used by CL, check the shell argument SHELL
;;; -- OBC
#+lispworks3.2
(defun system (arg)
  (sys::call-system arg))

;;; OBC added
#+lispworks3.2
(defun unix-sh-test (cond path &aux (strpath (cond ((stringp path) path)
                                                   ((pathnamep path)
                                                    (namestring path)))))
  (if strpath
      (= (system (format nil "test ~a \"~a\" || exit 1"
			 cond strpath))	; This is the fix: "strpath"!!
	 0)
    nil))

#+(or LispWorks3.2 LispWorks4.0 LispWorks4.1 LispWorks4.2 LispWorks4.3)
(defun unix-directory-list* (pathname predicate &rest options)
  (declare (dynamic-extent options))
  (let ((dirs (directory pathname)))
    (unless (member :directories options)
      (setq dirs (loop for file in dirs
		       unless (pathname-directory-p file)
		       collect file)))
    (when predicate
      (setq dirs (loop for file in dirs
                       when (funcall predicate file)
                       collect file)))
    dirs))

;; More efficient version uses the TEST keyword introduced in LW 4.4 -- JCMa 9/15/2005
#-(or LispWorks3.2 LispWorks4.0 LispWorks4.1 LispWorks4.2 LispWorks4.3)
(defun unix-directory-list* (pathname predicate &rest options)
  (declare (dynamic-extent options))
  (labels ((pathname-file-p (file)
             (not (pathname-directory-p file)))
           (files-satisfying-predicate-p (file)
             (and (pathname-file-p file)
                  (funcall predicate file))))
    (declare (inline pathname-file-p)
             (dynamic-extent #'files-satisfying-predicate-p))
    (cond ((member :directories options)
           (if predicate
               (directory pathname :test predicate)
             (directory pathname)))
          (t (directory pathname :test (if predicate #'files-satisfying-predicate-p #'pathname-file-p))))))

(defun directory-list* (pathname predicate &rest options)
  "Accepts the options :FILES :DIRECTORIES :SORTED :PROPERTIES."
  (declare (dynamic-extent options))
  (flet ((get-pathname-properties (path)
           (multiple-value-bind (length modification-date)
               (file-properties path)
             `(,path 
               ,.(when length `(:length-in-bytes ,length))
               ,.(when modification-date `(:creation-date ,modification-date))
              #+cl-http-file-author
              ,.(let ((author (file-author path)))
                   (when (and author (not (zerop (length author))))
                     `(:author ,author)))))))
    (declare (inline get-pathname-properties)) 
    (let ((pathnames (apply #'unix-directory-list* pathname predicate options)))
      (when (member :sorted options)
        (setq pathnames (sort pathnames #'(lambda (x y)
                                            (and (string-lessp (pathname-name x)
                                                          (pathname-name y))
                                                 (string-lessp (pathname-type x)
                                                          (pathname-type y)))))))
      (cond ((member :properties options)
             (loop for path in pathnames
                   collect (get-pathname-properties path)))
            (t pathnames)))))

(defun directory-list (pathname &rest options)
  "Returns a lisp Machine style directory listing."
  (declare (dynamic-extent options))
  (let ((pathnames (apply #'unix-directory-list* pathname nil :directories t options)))
    (when (member :sorted options)
      (setq pathnames (sort pathnames #'(lambda (x y)
                                          (and (string-lessp (pathname-name x)
                                                        (pathname-name y))
                                               (string-lessp (pathname-type x)
                                                        (pathname-type y)))))))
    (loop with length and modification-date
          for path in pathnames
          do (multiple-value-setq (length modification-date)
                 (file-properties path))
          collect `(,path 
                    ,.(when length `(:length-in-bytes ,length))
                    ,.(when modification-date `(:creation-date ,modification-date))
		    ,.(when (pathname-directory-p path) `(:directory t))))))

(declaim (inline alphalessp))

(defun alphalessp (a b)
  (string< a b))

(define directory-info (pathname &key (name :wild) (type :wild) (version :newest) (sort-pathnames t)
                                 directories)
  "Returns a property list of information for every file in the directory PATHNAME
that matches pathnames wildcards. Directories are included when directories is non-null."
  (declare (notinline))
  (flet ((get-directory-listing (p &optional (sort-p sort-pathnames))
           (let ((args nil))
             (declare (dynamic-extent args))
             (when sort-p (push :sorted args))
             (when directories (push :directories args))
             (apply #'directory-list p :no-extra-info args)))
         (pattern (path type)
           (make-pathname :name (etypecase name
                                  (keyword
                                    (ecase name
                                      (:wild :wild)))
                                  (string name))
                          :type (etypecase type
                                  (keyword
                                    (case type
                                      (:wild :wild)
                                      (t (symbol-name type))))
                                  (string type))
                          :version (etypecase version
                                     (keyword
                                       (ecase version
                                         (:wild nil)
                                         (:newest :newest))))
			  :defaults path))
         (sorter (e1 e2)
           (let ((p1 (car e1))
                 (p2 (car e2)))
             (and (alphalessp  (pathname-name p1) (pathname-name p2))
                  (let ((t1 (pathname-type p1))
                        (t2 (pathname-type p2)))
                    (cond ((and t1 t2)
                           (alphalessp t1 t2))
                          (t1 nil)
                          (t t)))))))
    (let ((p (pathname pathname)))
      (typecase type
        (keyword
          (ecase type
            (:wild (get-directory-listing (pattern p :wild)))))
        (string
          (get-directory-listing (pattern p type)))
        (cons
          (loop for type in type
                nconc (get-directory-listing (pattern p type)) into paths
                finally (return (if sort-pathnames
                                    (sort paths #'sorter)
                                    paths))))))))

(define create-directories-recursively (pathname)
  "Recursively create directories according to the directories present in PATHNAME."
  #+LispWorks3.2
  (create-directory-recursively1 pathname)
  #-LispWorks3.2
  (ensure-directories-exist pathname))

;;; For implementations where pathname-directory does
;;; not return NIL when there is no directory in the pathname.
;;; -- OBC
#+LispWorks3.2
(defun pathname-dirs (pathname)
  (let ((dirs (pathname-directory pathname)))
    (and (consp dirs) dirs)))

;;; -- OBC
#+LispWorks3.2
(defun create-a-directory (path &optional (error-p t))
  (let ((str (namestring path))) ;;(directorystring path)
    (case (system (format nil "mkdir ~S" str))
      (0 path)
      (t (if error-p
             (if (probe-directory path)
                 (error "create-a-directory: file or directory already exists: ~a" path)
               (error "create-a-directory: failed on: ~a" path))
           path)))))

;;; Return path if you can write in it or over it.
;;; -- OBC
#+LispWorks3.2
(defun file-permit-p (path &optional (permission "w"))
  (and (unix-sh-test (concatenate 'string "-" permission) path) path))

;;; -- OBC
#+LispWorks3.2
(defun create-directory-recursively1 (path &optional (error-p t))
  (ctypecase path
    (string (setq path (translate-logical-pathname (pathname path))))
    (pathname))
  ;; most system cannot create a whole directory from scratch so
  ;; recursively create directories for path to be valid
  (let ((host (pathname-host path))
        (order-dirs (nreverse (maplist #'reverse (reverse (pathname-dirs path)))))
        lastpath result)
    (dolist (dirs order-dirs)
      (setq lastpath (make-pathname :host host
                                    :directory dirs))
      (cond ((probe-directory lastpath)
             (setq result lastpath))
            (t
             (if error-p
                 (setq result (create-a-directory lastpath error-p))
               (if (and result (file-permit-p result))
                   (setq result (create-a-directory lastpath error-p))
                 ;; quiet and early termination, don't want to bother
                 ;; user with any low system messages since ERROR-P is NIL
                 (return))))))))

(defgeneric probe-directory (pathname)
  (:documentation "Returns non-null if the directory pathname exists."))

(defmethod probe-directory ((pathname pathname))
  (probe-file
   (lw:pathname-location pathname))) ;unlike make-pathname, this preserves the type of pathname

(defmethod probe-directory ((pathname string))
  (probe-directory (pathname pathname)))
   
;; Not really relevant for non-Genera CLs, but perhaps could build a PW db for FTP access -- JCMa 4/22/2006
(define-macro with-automatic-login ((host user-id user-pw) &body body)
  "Supplies userid and PW to ensure successul FTP login to host with BODY."
  `(progn #+ignore
     (notify-log-window "~&(WITH-AUTOMATIC-LOGIN (~S ~S ~S) - Not available under LispWorks" ,host ,user-id ,user-pw)
     ,host ,user-id ,user-pw
     ,@body))

#+UNIX
(defmethod http::compress-file ((pathname pathname) mode &key delete)
  (let ((file (truename pathname)))
    (flet ((do-it ()
             (ecase mode
	       (:compress (and (zerop (sys:call-system (format nil "gzip -f ~A < /dev/null" file)))
			       (make-pathname :name (file-namestring file)
					      :type "gz"
					      :defaults file)))
	       (:decompress (and (zerop (sys:call-system (format nil "gzip -f -d ~A < /dev/null" file)))
				 (merge-pathnames (parse-namestring (pathname-name file))
						  (lw:pathname-location file)))))))
      (if delete
	  (let ((res (do-it)))
	    (if res
		(values res t)
	      (values file nil)))
	(let* ((id 0)
               saved-path
               (ptype (pathname-type file))
               (type (and (stringp ptype) ptype))
	       (done nil))
	  (loop (setq saved-path (make-pathname :type (concatenate 'string
								   type
								   (format nil "~D" id))
						:defaults file))
		(unless (probe-file saved-path)
		  (return))
		(incf id))
	  (unwind-protect
	      (progn
                ;; gzip always removes the file, so save a copy and move it
                ;; back afterwards
		(sys:call-system (format nil "cp -p ~A ~A" file saved-path))
		(let ((res (do-it)))
		  (when res
		    (rename-file saved-path file)
		    (setq done t))
		  (values (or res file)
			  nil)))
	    (unless done
	      (delete-file saved-path))))))))

(defmethod http::compressed-file-p ((pathname pathname))
  (cond ((member (pathname-type pathname) '("gz") :test #'string-equal) t)
        (t nil)))
      

;; MJS 02Oct97: This is www-utils:char-bits, which doesn't have to be
;; cltl1:char-bits becase it was probably only needed to fix some bugs on the
;; LispM version.
(declaim (inline char-bits))

;; returns font or shift bits for Genera
;; not an issue if they are not stored in characters.
(defun char-bits (char)
  (declare (ignore char))
  0)

(declaim (inline string-thin))

;; removes fonts from Lispm Fat strings.-- JCMa 12/30/1994.
(defun string-thin (string)
  "Strips font description"
  string)

;;;------------------------------------------------------------------- 
;;;
;;; SECURE SUBNETS
;;;

(define ip-host-trusted-p (address secure-subnets &optional network)
  "Returns non-null if IP-address address is trusted given secure-subnets."
  (declare (ignore network))
  (flet ((address-match-p (addr1 addr2)
           (let ((diff (logxor addr1 addr2)))
             (macrolet ((masked-test (mask)
                          `(or (not (logtest ,mask addr2))
                               (not (logtest ,mask diff)))))
               (and (masked-test #xFF000000)
                    (masked-test #x00FF0000)
                    (masked-test #x0000FF00)
                    (masked-test #x000000FF))))))
    (declare (inline address-match-p))
    (cond (secure-subnets
           (member (etypecase address
                     (integer address)
                     (string (%parse-internet-address address)))
                   secure-subnets
                   :test #'address-match-p))
          (t t))))

;;;------------------------------------------------------------------- 
;;;
;;; LOGGING EVENTS
;;; 

(defvar *log-window-output-stream* (make-synonym-stream '*trace-output*))

(defvar *log-window-output-line-count* nil)

(defvar *log-collector-pane* nil
  "The pane of the log window for logging output.
This will be set, when the window system is used.")

(defvar *log-window-output-line-limit* 1000)

#+(or lispworks4 lispworks5)
(defmacro with-editor-buffer-locked (buffer &body body)
  (declare (ignore buffer))
  `(lispworks:without-preemption
     ,@body))

#-(or lispworks4 lispworks5)
(defmacro with-editor-buffer-locked (buffer &body body)
  `(editor:with-buffer-locked (,buffer)
     ,@body))

#+(or lispworks4 lispworks5)
(define notify-log-window (format-string &rest format-args)
  "Top-level method for writing to the HTTP log window.
If the window system is not used, it uses a normal stream.
FORMAT-STRING can also be a function applied to STREAM,
in which case FORMAT-ARGS are ignored."
  (declare (dynamic-extent format-args))
  (let ((stream *log-window-output-stream*))
    (fresh-line stream)
    (write-char #\[ stream)
    (http::write-standard-time (get-universal-time) stream)
    (write-string "]  " stream)
    (etypecase format-string
      (string
       (apply #'format stream format-string format-args))
      (function (funcall format-string stream)))
    (finish-output stream)
    (when *log-collector-pane*
      (lw:without-interrupts
        (when (and *log-window-output-line-count*
                   (>= (incf *log-window-output-line-count*)
                       *log-window-output-line-limit*))
          (let ((buffer (editor:point-buffer (editor:editor-stream-point stream)))
                (keep-lines (floor *log-window-output-line-limit* -2)))
            (editor:with-point ((point (editor:buffers-end buffer)))
              (editor:line-offset point keep-lines)
              (editor:delete-between-points (editor:buffers-start buffer) point)
              (editor:insert-string 
               point (load-time-value (format nil "---older lines deleted---~%")))
              (editor:use-buffer buffer (editor:clear-undo-command))))
          (setq *log-window-output-line-count* 0))))))

#||
#+(and LispWorks (not (or lispworks4 lispworks5)))
(define notify-log-window (format-string &rest format-args)
  "Top-level method for writing to the HTTP log window.
FORMAT-STRING can also be a function applied to STREAM,
in which case FORMAT-ARGS are ignored."
  (declare (dynamic-extent format-args))
  (let ((string (with-output-to-string (stream)
                  (fresh-line stream)
                  (write-char #\[ stream)
                  (http::write-standard-time (get-universal-time) stream)
                  (write-string "]  " stream)
                  (etypecase format-string
                    (string
                     (apply #'format stream format-string format-args))
                    (function (funcall format-string stream)))))
        (point (editor:editor-stream-point *log-window-output-stream*)))
    (editor:with-point-locked (point)
      (let ((buffer (editor:point-buffer point)))
        (editor:insert-string (editor:buffers-end buffer) string)
        (when (and *log-window-output-line-count*
                   (>= (incf *log-window-output-line-count*)
                       *log-window-output-line-limit*))
          (let ((keep-lines (floor *log-window-output-line-limit* -2)))
            (editor:with-point ((point (editor:buffers-end buffer)))
              (editor:line-offset point keep-lines)
              (editor:delete-between-points (editor:buffers-start buffer) point)
              (editor:insert-string 
               point (load-time-value (format nil "---older lines deleted---~%")))
              (editor:use-buffer buffer (editor:clear-undo-command buffer))))
          (setq *log-window-output-line-count* 0))
        (finish-output *log-window-output-stream*)))))
||#

#+(and LispWorks (not (or lispworks4 lispworks5)))
(define notify-log-window (format-string &rest format-args)
  "Top-level method for writing to the HTTP log window.
If the window system is not used, it uses a normal stream.
FORMAT-STRING can also be a function applied to STREAM,
in which case FORMAT-ARGS are ignored."
  (declare (dynamic-extent format-args))
  (flet ((compact-editor-buffer-p ()
           (and *log-window-output-line-count*
                (>= (incf *log-window-output-line-count*)
                    *log-window-output-line-limit*)))
         (compact-editor-buffer (buffer)
           (let ((keep-lines (floor *log-window-output-line-limit* -2)))
             (editor:with-point ((point (editor:buffers-end buffer)))
               (editor:line-offset point keep-lines)
               (editor:delete-between-points (editor:buffers-start buffer) point)
               (editor:insert-string point (load-time-value (format nil "---older lines deleted---~%")))
               (editor:use-buffer buffer (editor:clear-undo-command buffer))))
           (setq *log-window-output-line-count* 0))
         (write-entry (stream format-string format-args)
           (write-char #\[ stream)
           (http::write-standard-time (get-universal-time) stream)
           (write-string "]  " stream)
           (etypecase format-string
             (string
              (apply #'format stream format-string format-args))
             (function (funcall format-string stream)))))
    (declare (inline compact-editor-buffer-p write-entry))
    (if *log-collector-pane*
        (using-resource (string-buffer http::line-buffer http:*line-buffer-size*)
          (with-output-to-string (entry string-buffer)
            (write-entry entry format-string format-args)) ;write string before locking buffer
          (fresh-line *log-window-output-stream*)
          (let ((point (editor:editor-stream-point *log-window-output-stream*)))
            (editor:with-point-locked (point)
              (let ((buffer (editor:point-buffer point)))
                (editor:insert-string (editor:buffers-end buffer) string-buffer)
                ;; maybe compact buffer
                (when (compact-editor-buffer-p)
                  (compact-editor-buffer buffer))
                (finish-output *log-window-output-stream*)))))
      (progn
        (fresh-line *log-window-output-stream*)
        (write-entry *log-window-output-stream* format-string format-args)
        (finish-output *log-window-output-stream*)))))

#+CAPI
(defparameter *console-log-background-color* :grey
              "The background color for the CL-HTTP console log.
A LispWorks color specification suitable for an editor buffer window pane.
See the function COLOR:APROPOS-COLOR-SPEC-NAMES.")

#+CAPI
(defparameter *console-log-text-color* :blue
  "The text color for the CL-HTTP console log.
A LispWorks color specification suitable for an editor buffer window pane.
See the function COLOR:APROPOS-COLOR-SPEC-NAMES.")

#+CAPI 
(defparameter *console-log-font* nil
  "The font used in the CL-HTTP console log.
A LispWorks font specification suitable for an editor buffer window pane.
See: (capi::prompt-for-font \"Specify Font\")")

(define expose-log-window ()
  "Exposes the Log window."
  #-CAPI nil ;ignore when CAPI not present
  #+CAPI
  (flet ((create-console-log-window ()
           ;; Need to use Monaco fixed width font until beta bug in LW5b1 fixed -- JCMa 5/25/2006
           (setq *console-log-font* (gp:make-font-description #+LispWorks5 :family #+LispWorks5 "Monaco" :size 9 :weight :regular)
                 *log-collector-pane* (capi:contain (make-instance 'capi:collector-pane
                                                                   :echo-area t
                                                                   :buffer-name "CL-HTTP-Console-Log" 
                                                                   :font *console-log-font*
                                                                   :background *console-log-background-color*
                                                                   :foreground *console-log-text-color*)
                                                    :title "CL-HTTP Console Log"
                                                    :destroy-callback #'(lambda (self)
                                                                          (declare (ignore self))
                                                                          (setq *log-collector-pane* nil))
                                                    :best-width '(character 125))
                 *log-window-output-stream* (capi:collector-pane-stream *log-collector-pane*)
                 *log-window-output-line-count* 0)))
    (when (capi:screens)
      (let ((window (and *log-collector-pane* (capi:top-level-interface *log-collector-pane*))))
        (if window
            (capi:execute-with-interface
             window
             #'(lambda ()
                 (setf (capi:top-level-interface-display-state window) :restore)
                 (capi:activate-pane window)))
          (create-console-log-window))))))

(define common-logfile-notify (server)
  "Issues a notification of server activity on a window."
  (flet ((write-log-entry (server stream)
           (http::write-common-logfile-entry server stream)
           (terpri stream)
           (finish-output stream)))
    (funcall #'write-log-entry server *log-window-output-stream*)))

(define log-http-server-error (format-string &rest format-args)
  (declare (dynamic-extent format-args))
  (apply #'notify-log-window  format-string format-args))

(define http::log-http-request (client-host method url-string case)
  #|(log-http-access client-host
                   (if accepted-p "Serving ~A ~S" "Rejected ~A ~S") method url-string)|#
  (let ((host-name (http::host-domain-name client-host)))
    (ecase case
      (:accepted
        (notify-log-window "HTTP Serving ~A: Method ~S: ~S" host-name method url-string))
      (:rejected
        (notify-log-window "HTTP Rejected ~A: Method ~S: ~S" host-name method url-string))
      (:timeout
        (notify-log-window "HTTP Timed Out Serving ~A: Method ~S: ~S" host-name method url-string)))))

;; export the logging symbols
#+ignore
(export '(log-window notify-log-window expose-log-window common-logfile-notify) :www-utils)

;;;------------------------------------------------------------------- 
;;;
;;; MAIL SENDING  
;;;


#+LispWorks3.2
;; Modern version is defined in http:smtp;mail.lisp
(define report-bug  (to subject format-string &rest format-args)
  (notify-log-window (with-output-to-string
                       (stream)
                       (format  stream "~&Report Bug:  To:~A~
                                                       ~&Subject: ~A"
                                to subject)
                       (fresh-line stream)
                       (apply #'format stream format-string format-args))))


;;;------------------------------------------------------------------- 
;;;
;;; LOG RELATED PORTABILITY
;;;

(defun make-lock (name &key (type :simple))
  "Returns a lock named name that is suitable for use with with-lock-held.
TYPE can be either :SIMPLE (the default) or :MULTIPLE-READER-SINGLE-WRITER.
A simple lock queues all operations whereas a :MULTIPLE-READER-SINGLE-WRITER lock
allows multiple read threads but only a single writer thread."
  (declare (ignore type))
  (clim-sys:make-lock name))

(defmacro with-lock-held ((lock &optional (mode :write) (whostate "Wait for Lock")) &body body)
  "Executes BODY with LOCK held in MODE, which is one of :READ or :WRITE."
  (declare (ignore mode))
  `(clim-sys:with-lock-held (,lock ,whostate)
     ,@body))

(defun %make-log-pathname (device directory name host)
  "Returns the pathname to which current log entries are written."
  (make-pathname
    :host host
    :device device
    :directory directory
    :name name
    :type "text"))


;;;------------------------------------------------------------------- 
;;;
;;; PERIODIC TIMERS
;;;

(defun create-timer-call (function arguments &key name)
  "Make a timer which will asynchronously call the given function with
arguments when the time expires."
  (mp:make-named-timer name
		       'make-process-and-run-function
		       name function arguments))

;; Portable?
#+clim-sys
(defun make-process-and-run-function (name function args)
  (clim-sys:make-process #'(lambda ()
			     (apply function args))
			 :name name))

(defun reset-timer-absolute (timer absolute-universal-time)
  "Reset the timer to expire at the given absolute-universal-time."
  (mp:schedule-timer-relative
   timer
   ;; Convert to seconds from time now
   (- absolute-universal-time
      (get-universal-time))))

(defvar *time-zone* nil
   "Time zone offset from GMT in hours.")

(define time-zone (&optional update-p)
  "Returns the timezone as an offset from GMT."
  (cond ((or update-p
             (not *time-zone*))
         (multiple-value-bind (sec min hr date mo year day daylight-p zone)
             (get-decoded-time)
           (declare (ignore sec min hr date mo year day))
           (setq *time-zone* (if daylight-p (1- zone) zone))))
        (t *time-zone*)))

;; Portable?
(defvar *daily-server-timer* nil)

;; Portable?
(defun daily-server-timer ()
  (or *daily-server-timer*
      (setq *daily-server-timer*
	    (create-timer-call 'run-daily-server-tasks '()
			       :name "Daily Server Tasks"))))

;; Portable?
(defun synchronize-daily-server-tasks ()
  (reset-timer-absolute (daily-server-timer)
			(next-3am-universal-time)))




;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

#|
genera process functions
  CLIM-SYS:PROCESS-WAIT-WITH-TIMEOUT - Function (WAIT-REASON TIMEOUT PREDICATE)
  CLIM-SYS:PROCESS-NAME - Function (PROCESS)
  CLIM-SYS:PROCESS-YIELD - Function ()
  CLIM-SYS:PROCESS-STATE - Function (PROCESS)
  CLIM-SYS:PROCESS-WAIT - Function (WAIT-REASON PREDICATE)
  CLIM-SYS:PROCESS-WHOSTATE - Function (PROCESS)
  CLIM-SYS:PROCESS-INTERRUPT - Function (PROCESS FUNCTION)
  CLIM-SYS:ENABLE-PROCESS - Function (PROCESS)
  CLIM-SYS:CURRENT-PROCESS - Function ()
  CLIM-SYS:DISABLE-PROCESS - Function (PROCESS)
  CLIM-SYS:ALL-PROCESSES - Function ()
  CLIM-SYS:MAKE-PROCESS - Function (FUNCTION &key :NAME)
  CLIM-SYS:RESTART-PROCESS - Function (PROCESS)
  CLIM-SYS:DESTROY-PROCESS - Function (PROCESS)

|#

(defun process-run-function (name function &rest args)
  (apply 'mp:process-run-function name nil function args))


(defun process-disable (process)
  (clim-sys:disable-process process))

(defun process-enable (process)
  (clim-sys:enable-process process))

(defun process-kill (process)
  (clim-sys:destroy-process process))

(defun process-reset (process)
  (clim-sys:restart-process process))

(defun current-process ()
  (clim-sys:current-process))

(export 'current-process :www-utils)

;; MJS 09Aug99: Don't use CLIM-SYS:MAKE-PROCESS because the one that
;; is shipped with CLIM doesn't take the extra args.
(define make-process (process-name &key (priority 0) quantum
				   run-reasons background-p 
				   restart-after-reset warm-boot-action &allow-other-keys)
  "Creates a process using a portable set of options."
  (declare (ignore priority quantum run-reasons background-p
                   warm-boot-action restart-after-reset))
  (mp:process-run-function process-name
                           ;; MJS 09Aug99: could pass priority is we knew what it meant.
                           (list)
                           (constantly nil)))

;; updated from acl/server/unix.lisp   7/14/96 -- JCMa.
(defun process-wait (wait-reason predicate &rest args)
  (declare (dynamic-extent args))
  (flet ((wait-function ()
	   (apply predicate args)))
    (declare (dynamic-extent #'wait-function))
    (clim-sys:process-wait wait-reason (if args #'wait-function predicate))))

(defun process-wait-with-timeout (wait-reason timeout predicate &rest args)
  (declare (dynamic-extent args))
  (flet ((wait-function ()
	   (apply predicate args)))
    (declare (dynamic-extent #'wait-function))
    (clim-sys:process-wait-with-timeout wait-reason timeout (if args #'wait-function predicate))))

(defun process-run-time (process)
  "Returns the amount of run time the process has accumulated, in microseconds."
  #+(or LispWorks3.2 LispWorks4)
  (* 1000000 (mp::process-time-slice process)) ;MJS 03Apr97: rather a lie, but better than nothing
  #-(or LispWorks3.2 LispWorks4)
  (floor (the integer (mp:process-run-time process)) #.(float (/ internal-time-units-per-second 1000000))))

(defun process-idle-time (process)
  "Returns the amount of time since the process ran last, in sixtieths of a second."
  #+(or LispWorks3.2 LispWorks4)
  (declare (ignore process))
  #+(or LispWorks3.2 LispWorks4) 60 ;MJS 03Apr97: a complete lie
  #-(or LispWorks3.2 LispWorks4)
  (floor (the integer (mp:process-idle-time process)) #.(float (/ internal-time-units-per-second 60))))

;; MJS 13Oct99: could do process-priority is we knew what it meant.
(defun process-priority (process)
  #+(or LispWorks3.2 LispWorks4)
  (declare (ignore process))
  #+(or LispWorks3.2 LispWorks4) 0
  #-(or LispWorks3.2 LispWorks4)
  (mp:process-priority process))

(defun (setf process-priority) (value process)
  #+(or LispWorks3.2 LispWorks4)
  (declare (ignore process))
  (check-type value fixnum)
  #-(or LispWorks3.2 LispWorks4)
  (mp:change-process-priority process value)
  value)

;; What happens on multi-processor machines?
(defmacro without-preemption (&body body)
  "Executes BODY without allowing the current process to be preempted.
This macro ensures atomic execution of BODY."
  `(mp:without-preemption ,@body))

(export 'without-preemption :www-utils)


;;;------------------------------------------------------------------- 
;;;
;;; TIMERS AND TIME-BOUNDED EXECUTION
;;;

(defun make-timer (resource function)
  (declare (ignore resource function))
  (mp:make-named-timer 'cl-http-timer nil))

(defun initialize-timer (resource timer function)
  (declare (ignore resource))
  (setf (slot-value timer 'mp::function) function)
  timer)

(defun deinitialize-timer (resource timer)
  (declare (ignore resource))
  (setf (slot-value timer 'mp::function) nil)
  timer)

(defresource
  timer (function)
  :constructor make-timer
  :initializer initialize-timer
  :deinitializer deinitialize-timer
  :initial-copies 0)

(defun with-timeout-internal (timeout function error-p)
  (check-type timeout (integer 0))
  (let ((catch-tag nil)
        (process mp:*current-process*))
    (labels ((timeout-throw ()
               (throw catch-tag nil))
             (timeout-action ()
               (mp:process-interrupt process #'timeout-throw)))
      (declare (dynamic-extent #'timeout-throw #'timeout-action))
      (using-resource (timer timer #'timeout-action)
        (setq catch-tag timer)
        (catch timer
          (unwind-protect
              (progn
                (mp:schedule-timer-relative timer timeout)
                (return-from with-timeout-internal
                  (funcall function)))
            (mp:unschedule-timer timer)))
        (when error-p
          (error "Timeout: body took longer than ~d second(s) to complete."
		 timeout))))))

(defmacro with-timeout ((timeout &key error-p) &body body)
  "Executes BODY and returns the values of the last form in BODY. However, if
the execution takes longer than TIMEOUT seconds, abort it. If :ERROR-P is
unsupplied or false, just return nil. If :ERROR-P is true, signal an error."
  (let ((function (gensym "function")))
    `(flet ((,function () . ,body))
       (declare (dynamic-extent #',function))
       (with-timeout-internal ,timeout #',function ,error-p))))

#-LispWorks3.2
(defun with-stream-timeout-internal (stream timeout function error-p)
  (check-type timeout (integer 0))
  (let ((old-timeout (stream:stream-read-timeout stream)))
    (unwind-protect
        (progn
          (setf (stream:stream-read-timeout stream) timeout)
          (cond (error-p
                 (funcall function))
                (t (handler-case
                       (funcall function)
                     (end-of-file () nil))))) ;read timeout is described as signalling eof by the manual
      (setf (stream:stream-read-timeout stream) old-timeout))))

#-LispWorks3.2
(defmacro with-stream-timeout ((stream timeout &key error-p) &body body)
  "Executes BODY and returns the values of the last form in BODY. 
However, if the stream goes idle for longer than TIMEOUT seconds, the
operation is aborted.  If ERROR-P is non-null, the time out error is
signalled, otherwise NIL is returned."
  (let ((function (gensym "function")))
    `(flet ((,function () . ,body))
       (declare (dynamic-extent #',function))
       (with-stream-timeout-internal ,stream ,timeout #',function ,error-p))))


;;;------------------------------------------------------------------- 
;;;
;;; CLEAN UP HANGING HTTP PROCESSES
;;;

(defvar *idle-http-process-scavenger-timer* nil)

(defun idle-http-process-scavenger-timer ()
  (or *idle-http-process-scavenger-timer*
      (setq *idle-http-process-scavenger-timer*
            (create-timer-call 'scavenge-idle-http-processes '()
                               :name "Scavenge Idle HTTP Server Processes"))))

;; set up a timer to run the scavenger every server timout interval
(defun synchronize-idle-http-process-scavenger ()
  (reset-timer-absolute (idle-http-process-scavenger-timer)
                        (+ (get-universal-time)
                           (min (* http:*server-life-time* 1000.)
                                (ceiling http:*server-timeout* 60.)))))

(defparameter *safe-server-abort-states* ipc::*tcp-stream-safe-abort-states*
  "Process whostates from which it is safe to abort HTTP connnections.")

(defun server-safe-to-abort-p (server)
  "Returns non-null when it is safe to abort the HTTP connection for SERVER."
  (let ((process (http::server-process server)))
    (and process
	 (or (null *safe-server-abort-states*)
	     (member (clim-sys:process-whostate process) *safe-server-abort-states*
                     :test #'equalp)))))

(define-variable *idle-connections-scavenged*  0
		 "The number of idle connections scavenged since boot time.")

;; Don't just kill active HTTP connections. Useful for web walker, log window
;; and other lengthly computations.   3/18/97 -- JCMa.
(define scavenge-idle-http-processes ()
  "Finds and kills any HTTP servers that have been idle for more than IDLE-TIME."
  (labels ((reason-to-die (server)
             (cond ((http::server-timeout-p server) "it has been idle too long")
                   ((http::server-life-time-expired-p server) "its lifetime expired")
                   (t nil)))
           (idle-time (server)
             (let ((idle (http::server-idle-time server)))
               (when idle
                 (ceiling idle 60.))))
           (report-connection-abort (server process reason-to-die &aux (idle-time (idle-time server)))
             (let ((report-string (format nil "~&Forcibly Aborting ~A
                                               ~&~5TState: ~A~&~5TReason: ~A
                                               ~&~10TRequest Time: ~A
                                               ~:[~;~&~10TIdle Time: ~:*~A~]~
                                               ~:[~;~&~10TURL: ~:*~A~]"
                                          server (clim-sys:process-whostate process) reason-to-die
                                          (http::write-time (http::server-request-time server) nil)
                                          (and idle-time (write-time-interval idle-time nil)) (http::server-url-string server))))
               (flet ((report-connection-scavenge (stream)
                        (write-string report-string stream)))
                 #'report-connection-scavenge)))
           (kill-http-server (server reason-to-die)
             (let ((process (http::server-process server)))
               (when process                    ;beware of fencepost error
		 (prog1 (report-connection-abort server process reason-to-die)
                   (mp:process-interrupt process #'http::abort-connection server)))))
	   (report-connection-scavenges (report)
	     (when http::*report-idle-connection-scavenges*
	       (http:report-bug http:*bug-http-server* "HTTP Connection Forcibly Aborted" report)))
           (maybe-kill-http-server (server &aux reason-to-die report)
              (when (setq reason-to-die (reason-to-die server))
                (#+(or LispWorks4 LispWorks5) clim-sys:without-scheduling	;Don't allow process to change state
                 #+(and LispWorks (not (or LispWorks4 LispWorks5))) progn       ;not sure how to do this in LispWorks6 with SMP
                 (when (server-safe-to-abort-p server) ;kill it next time or not it has returned to a useful state
                   (setq report (kill-http-server server reason-to-die)))))
	     (when report
	       (atomic-incf *idle-connections-scavenged*)
	       (report-connection-scavenges report))))
    (declare (inline reason-to-die))
    (unless http:*debug-server*
      (http::map-http-resource :server :allocated #'maybe-kill-http-server))
    (synchronize-idle-http-process-scavenger)))

;;;------------------------------------------------------------------- 
;;;
;;; ATOMIC CREATION OF CRLF FILES
;;;

(defmethod http:valid-crlf-cache-file-p ((pathname pathname) &aux source canonical c-probe-date)
  ;; Since creation uses an atomic rename-file operation, the crlf is valid if
  ;; it exists and is newer than the source.
  (cond ((and (setq source (probe-file pathname))
              (setq canonical (http:crlf-pathname source)) 
              (setq c-probe-date (file-write-date canonical)) 
              (< (file-write-date source) c-probe-date))
         (values t source (truename canonical)))
        (t (values nil source (and canonical (probe-file canonical)) canonical))))

(defmethod http:ensure-crlf-canonical-file ((pathname pathname))
  (multiple-value-bind (valid-crlf-cache-p source-pathname crlf-pathname canonical-pathname)
      (http:valid-crlf-cache-file-p pathname)
    (cond ((null source-pathname) (error 'conditions:non-existent-file-error :pathname pathname)) 
          (valid-crlf-cache-p crlf-pathname)
          ((atomic-crlf-canonicalize-file source-pathname canonical-pathname)
	   (values (http:crlf-pathname pathname) t))
	  (t (http:crlf-pathname pathname)))))

; added for LispWorks6
(defvar *atomic-crlf-canonicalize-file-lock*
  (make-lock "atomic-crlf-canonicalize-file-lock"))

(defun atomic-crlf-canonicalize-file (source-pathname canonical-pathname)
  (let ((lock-file (make-pathname :type (concatenate 'simple-string
						     (pathname-type canonical-pathname)
						     "-lock")
				  :defaults canonical-pathname)))
    ;; Our caller saw an out-of-date crlf file, so repeatedly try to create it.
    (loop (let ((lock-stream nil))
	    (unwind-protect
		(progn
		  ;; Atomically probe and create the lock file if it doesn't
		  ;; exist.
		  (with-lock-held (*atomic-crlf-canonicalize-file-lock*)
		   (setq lock-stream
			 (open lock-file :direction :output
			       :if-exists nil
			       :if-does-not-exist :create)))
		  (when lock-stream
		    ;; We have the lock, so noone else can create the file now.
		    ;; If the file is valid then it has just been create by another process.
		    (when (http:valid-crlf-cache-file-p source-pathname)
		      (return))
		    (let* ((temp-file (make-pathname
				       :type (concatenate 'simple-string
							  (pathname-type canonical-pathname)
							  "-temp")
				       :defaults canonical-pathname)))
		      ;; Make the file in a temporary place.
		      (http:crlf-canonicalize-file source-pathname temp-file)
		      ;; Unix rename is atomic, so other processes will see either
		      ;; the old out-of-date file or the new one.
		      (rename-file temp-file canonical-pathname)
		      (return t))))
	      (when lock-stream
		(close lock-stream)
		(delete-file lock-file))))
	  (clim-sys:process-wait-with-timeout
	   "CRLF Wait"
	   1
	   #'(lambda () (not (probe-file lock-file)))))))

;;;------------------------------------------------------------------- 
;;;
;;; MODIFY HASH --  EVERY LISP SHOULD HAVE ONE
;;;

#+(or LispWorks4 LispWorks5)
(defun modify-hash (table key function &aux value)
  "Combines the action of setf of gethash into one call to modify- hash. It lets
you both examine the value of key and change it. It is more efficient because
it does the lookup once instead of twice.

Finds the value associated with key in table, then calls function with key,
this value, a flag indicating whether or not the value was found.  Puts
whatever is returned by this call to function into table, associating it
with key. Returns the new value and the key of the entry. Note:  The actual
key stored in table is the one that is used on function, not the one you
supply with key."
  (declare (values new-value key))
  (without-preemption
    (multiple-value-bind (entry foundp)
        (gethash key table)
      (cond (foundp
             (setq value (funcall function key entry foundp))
             (unless (eq value entry)
               (setf (gethash key table) value))
             (values value key))
            (t (setq value (funcall function key nil nil))
               (setf (gethash key table) value)
               (values value key))))))

#+(and LispWorks (not (or LispWorks4 LispWorks5)))
(defun modify-hash (table key function)
  "Combines the action of setf of gethash into one call to modify- hash. It lets
you both examine the value of key and change it. It is more efficient because
it does the lookup once instead of twice.

Finds the value associated with key in table, then calls function with key,
this value, a flag indicating whether or not the value was found.  Puts
whatever is returned by this call to function into table, associating it
with key. Returns the new value and the key of the entry. Note:  The actual
key stored in table is the one that is used on function, not the one you
supply with key."
  (declare (values new-value key))
  (hcl:modify-hash table key function))

(export 'modify-hash :www-utils)
