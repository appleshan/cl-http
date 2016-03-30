;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: www-utils -*-

;;; (C) Copyright 1994-1995, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;; Ideas hacked from the other cl-http ports.
;;;
;;; CMUCL enhancements written by Douglas Crosher have been
;;; placed in the public domain and are provided 'as-is'.
;;;



;;;------------------------------------------------------------------- 
;;;

(in-package :www-utils)

#+mp ;; Advise CL-HTTP that the Lisp Environment is multithreaded.
(pushnew :multi-threaded *features*)

(define-condition network-error-mixin
                  (network-error)
  ()
  (:documentation "Mixin to allow ports to inherit instance variables and methods to network conditions
defined at the portable code level."))

(declaim (inline report-condition))

(define report-condition (condition stream)
  "Prints the report string for CONDITION onto STREAM."
  (handler-case (format stream "~A" condition)
    ;; Cannot guaranty all errors are printable.
    (error ()
	   (format stream "~&~A~%" condition)
	   (describe condition stream))))

(declaim (inline report-string))

(define report-string (condition)
  "Returns the report string for CONDITION."
  (with-output-to-string (stream)
    (report-condition condition stream)))

;; Define equivalence mapping to the MCL case.
(deftype file-not-found () 
  "Specialization of Common Lisp File-error in which the file was not found on open."
  '(and condition file-error))

(define-macro atomic-incf (reference &optional (delta 1))
  "Atomically increments REFERENCE by DELTA."
  `(system:without-interrupts
    (incf ,reference ,delta)))

(define-macro atomic-decf (reference &optional (delta 1))
  "Atomically decrements REFERENCE by DELTA."
  `(system:without-interrupts
    (decf ,reference ,delta)))

(define-macro atomic-push (item reference)
  "Atomically pushes ITEM onto REFERENCE."
  `(system:without-interrupts
    (push ,item ,reference)))

(define-macro atomic-pop (reference)
  "Atomically pops an item off REFERENCE."
  `(system:without-interrupts
    (pop ,reference)))

(define-macro atomic-conditional-replacef (reference predicate new-value)
  "When PREDICATE returns non-null, this setfs REFERENCE to NEW-VALUE.
  Predicate is called (OLD-VALUE NEW-VALUE). The operation
  assures that predicate application and swap are atomic."
  (let ((old-value (gensym))
	(new-value-var (gensym)))
    `(sys:without-interrupts
      (let ((,old-value ,reference)
	    (,new-value-var ,new-value))
	(when (funcall ,predicate ,old-value ,new-value-var)
	  (setf ,reference ,new-value-var)
	  (values ,old-value t))))))


;;;------------------------------------------------------------------- 
;;;

; (declaim (inline arglist))

; arglist code taken from Public Domain project SLIME / SWANK
; SLIME is an Emacs mode for Common Lisp development. 
; http://common-lisp.net/project/slime/

(defun arglist (fun)
  (cond ((symbolp fun)
	 (arglist (or (macro-function fun)
		      (symbol-function fun))))
	((functionp fun)
	 (function-arglist fun))
	(t :not-available)))

(defun function-arglist (fun)
  (let ((arglist
         (cond ((eval:interpreted-function-p fun)
                (eval:interpreted-function-arglist fun))
               ((pcl::generic-function-p fun)
                (pcl:generic-function-lambda-list fun))
               ((c::byte-function-or-closure-p fun)
                (byte-code-function-arglist fun))
               ((kernel:%function-arglist (kernel:%function-self fun))
                (handler-case (read-arglist fun)
                  (error () :not-available)))
               ;; this should work both for compiled-debug-function
               ;; and for interpreted-debug-function
               (t 
                (handler-case (debug-function-arglist 
                               (di::function-debug-function fun))
                  (di:unhandled-condition () :not-available))))))
    (check-type arglist (or list (member :not-available)))
    arglist))


(defun read-arglist (fn)
  "Parse the arglist-string of the function object FN."
  (let ((string (kernel:%function-arglist 
                 (kernel:%function-self fn)))
        (package (find-package
                  (c::compiled-debug-info-package
                   (kernel:%code-debug-info
                    (vm::find-code-object fn))))))
    (with-standard-io-syntax
      (let ((*package* (or package *package*)))
        (read-from-string string)))))

;;; A harder case: an approximate arglist is derived from available
;;; debugging information.

(defun debug-function-arglist (debug-function)
  "Derive the argument list of DEBUG-FUNCTION from debug info."
  (let ((args (di::debug-function-lambda-list debug-function))
        (required '())
        (optional '())
        (rest '())
        (key '()))
    ;; collect the names of debug-vars
    (dolist (arg args)
      (etypecase arg
        (di::debug-variable 
         (push (di::debug-variable-symbol arg) required))
        ((member :deleted)
         (push ':deleted required))
        (cons
         (ecase (car arg)
           (:keyword 
            (push (second arg) key))
           (:optional
            (push (debug-variable-symbol-or-deleted (second arg)) optional))
           (:rest 
            (push (debug-variable-symbol-or-deleted (second arg)) rest))))))
    ;; intersperse lambda keywords as needed
    (append (nreverse required)
            (if optional (cons '&optional (nreverse optional)))
            (if rest (cons '&rest (nreverse rest)))
            (if key (cons '&key (nreverse key))))))

(defun debug-variable-symbol-or-deleted (var)
  (etypecase var
    (di:debug-variable
     (di::debug-variable-symbol var))
    ((member :deleted)
     '#:deleted)))

(defun symbol-debug-function-arglist (fname)
  "Return FNAME's debug-function-arglist and %function-arglist.
A utility for debugging DEBUG-FUNCTION-ARGLIST."
  (let ((fn (fdefinition fname)))
    (values (debug-function-arglist (di::function-debug-function fn))
            (kernel:%function-arglist (kernel:%function-self fn)))))

;;; Deriving arglists for byte-compiled functions:
;;;
(defun byte-code-function-arglist (fn)
  ;; There doesn't seem to be much arglist information around for
  ;; byte-code functions.  Use the arg-count and return something like
  ;; (arg0 arg1 ...)
  (etypecase fn
    (c::simple-byte-function 
     (loop for i from 0 below (c::simple-byte-function-num-args fn)
           collect (make-arg-symbol i)))
    (c::hairy-byte-function 
     (hairy-byte-function-arglist fn))
    (c::byte-closure
     (byte-code-function-arglist (c::byte-closure-function fn)))))

(defun make-arg-symbol (i)
  (make-symbol (format nil "~A~D" (string 'arg) i)))

;;; A "hairy" byte-function is one that takes a variable number of
;;; arguments. `hairy-byte-function' is a type from the bytecode
;;; interpreter.
;;;
(defun hairy-byte-function-arglist (fn)
  (let ((counter -1))
    (flet ((next-arg () (make-arg-symbol (incf counter))))
      (macrolet ((with-struct ((conc-name &rest names) obj &body body)
                   "Like with-slots but works only for structs."
                   (flet ((reader (slot) (intern (concatenate 'string
                                                              (symbol-name conc-name)
                                                              (symbol-name slot))
                                                 (symbol-package conc-name))))
                     (let ((tmp (gensym "OO-")))
                       ` (let ((,tmp ,obj))
                           (symbol-macrolet
                               ,(loop for name in names collect 
                                      (typecase name
                                        (symbol `(,name (,(reader name) ,tmp)))
                                        (cons `(,(first name) (,(reader (second name)) ,tmp)))
                                        (t (error "Malformed syntax in WITH-STRUCT: ~A" name))))
                             ,@body))))))
        (with-struct (c::hairy-byte-function- min-args max-args rest-arg-p
                                              keywords-p keywords) fn
            (let ((arglist '())
                  (optional (- max-args min-args)))
              ;; XXX isn't there a better way to write this?
              ;; (Looks fine to me. -luke)
              (dotimes (i min-args)
                (push (next-arg) arglist))
              (when (plusp optional)
                (push '&optional arglist)
                (dotimes (i optional)
                  (push (next-arg) arglist)))
              (when rest-arg-p
                (push '&rest arglist)
                (push (next-arg) arglist))
              (when keywords-p
                (push '&key arglist)
                (loop for (key _ __) in keywords
                      do (push key arglist))
                (when (eq keywords-p :allow-others)
                  (push '&allow-other-keys arglist)))
              (nreverse arglist)))))))

;;;------------------------------------------------------------------- 
;;;
;;; OBTAINING THE DOMAIN NAME FOR AN IP ADDRESS
;;;

;;; From pw.
(defun nl-from-address (string)
  (declare (type simple-string string))
  (multiple-value-bind 
    (n1 p1)(parse-integer string :junk-allowed t)
    (when n1
      (locally (declare (fixnum p1))
	(multiple-value-bind
	  (n2 p2)(parse-integer string :start (1+ p1) :junk-allowed t)
	  (when n2
	    (locally (declare (fixnum p2))
	      (multiple-value-bind
		(n3 p3)(parse-integer string :start (1+ p2) :junk-allowed t)
		(when n3
		  (locally (declare (fixnum p3))
		    (let* ((n4 (parse-integer string :start (1+ p3))))
		      (declare (type (integer 0 255) n1 n2 n3 n4))
		      (+ (ash n1 24)
			 (ash n2 16)
			 (ash n3  8)
			 n4))))))))))))

(declaim (inline %parse-internet-address))

(defun %parse-internet-address (address)
  "Returns an IP-NUMBER which is integer denoting the address of host."
  (typecase address
    (integer address)
    (string
      (cond ;; pw-- "200.0.1.9"
	((and (= 3 (count #\. address))
	      (parse-integer address :junk-allowed t))
	 ;; handle nn.nn.nn.nn /= 127.0.0.1 for localhost alias
	 (let ((nl (nl-from-address address)))
	   (if (and nl (= nl (local-host)))
	       (local-host)
	       (let ((host (ext:lookup-host-entry nl)))
		 (if (null host)
		     (error 'unknown-host-name :hostname address)
		     (ext:host-entry-addr host))))))
	(t
	 (let ((host (ext:lookup-host-entry address)))
	   (if (null host)
	       (error 'unknown-host-name :hostname address)
	       (ext:host-entry-addr host))))))
    
    (t (error 'unknown-host-name :hostname address))))

(declaim (inline ip-address-for-parsed-ip-address))

(define ip-address-for-parsed-ip-address (ip-number)
  "Returns an IP address as a string from, IP-NUMBER, the parsed address."
  (ip-address-string ip-number))

(define domain-name-for-parsed-ip-address (ip-number &optional (no-error-p t))
  "Given the parsed IP address (an integer), IP-NUMBER, this returns the domain name or NIL.
When no-error-p is T, this returns the IP Address string when a domain error occurs."
  (check-type ip-number integer)
  (cond ;; Return cached value when available. 
    ;; Faster and used for standalone operation.
    ((and (eql ip-number (local-host-parsed-ip-address))
          http:*local-host-domain-name*)) 
    ;; DNS off case returns IP addresses only
    ((or (null http:*resolve-ip-addresses*) (zerop ip-number)) 
     (ip-address-for-parsed-ip-address ip-number))
    (t 
     (let ((host (ext:lookup-host-entry ip-number)))
       (cond ((null host)
	      (if no-error-p
		  (ip-address-for-parsed-ip-address ip-number)
		(error 'domain-error ()
		       (ip-address-for-parsed-ip-address ip-number))))
	     (t
	      (ext:host-entry-name host)))))))

(declaim (inline domain-name-for-ip-address))

(define domain-name-for-ip-address (address &optional (no-error-p t))
  "Given the IP address, ADDRESS, this returns the domain name or NIL."
  (domain-name-for-parsed-ip-address (%parse-internet-address address) no-error-p))

(declaim (inline ip-address-for-host-domain-name))

(define ip-address-for-host-domain-name (domain-name)
  "Returns the IP address string for domain-name."
  (ip-address-for-parsed-ip-address (http::parse-internet-address domain-name))) 

(declaim (notinline %local-host-parsed-ip-number))

(defun %local-host-parsed-ip-number ()
  (ext:host-entry-addr (ext:lookup-host-entry
			(or http:*http-host-name* (machine-instance)))))

(defparameter *local-host*  nil
  "The local host object, which is a parsed IP number on the MAC." )

(define local-host (&optional recache-p)
  "The host object for the local host on which we are running."
  (cond ((and (not recache-p) *local-host*))
        (t (setq *local-host* (%local-host-parsed-ip-number)))))

(define local-host-ip-address (&optional recache-p)
  "Returns the IP address of the local host."
  (cond ((and (not recache-p) http::*local-host-ip-address*))
        (t (setq http:*local-host-ip-address* (ip-address-for-parsed-ip-address 
                                                (%local-host-parsed-ip-number))))))

(define local-host-parsed-ip-address (&optional recache-p)
  "Returns the parsed IP address of the local host."
  (cond ((and (not recache-p) http:*local-host-address*))
        (t (setq http:*local-host-address* (%local-host-parsed-ip-number)))))

(define %local-host-domain-name ()
  (let ((ip-number (%local-host-parsed-ip-number)))
    (if (zerop ip-number)
        ;; zerop means no network connection and no DNS
        (ip-address-for-parsed-ip-address ip-number)
        ;; normal case
        ;; When a mac is running with only an IP address and no DNS entry,
        ;; it will get an error here.  Using the no error argument allows it
        ;; to continue operating with just it's IP address. -- JCMa 8/16/96
        (domain-name-for-parsed-ip-address ip-number t))))

(defun local-host-domain-name (&optional recache-p)
  "Returns the local host domain name."
  (cond ((and (not recache-p) http::*local-host-domain-name*))
        (t (setq http::*local-host-domain-name* (or http:*http-host-name* (%local-host-domain-name))))))

;;;------------------------------------------------------------------- 
;;;
;;; HOST RELATED
;;;

(define parse-host (address &optional no-error-p)
  "Top-level method for parsing a host ADDRESS."
  (declare (values ip-number))
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
  (cond ((or (null host1) (null host2)) nil)
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

#-mp
(defun local-port (http-stream)
  (http-stream-local-port (stream-detail http-stream)))

#-mp
(defun foreign-port (http-stream)
  (http-stream-remote-port (stream-detail http-stream)))

#-mp
(defun foreign-host (http-stream)
  (http-stream-remote-host (stream-detail http-stream)))

#+mp
(defun local-port (http-stream)
  (http-stream-local-port http-stream))

#+mp
(defun foreign-port (http-stream)
  (http-stream-remote-port http-stream))

#+mp
(defun foreign-host (http-stream)
  (http-stream-remote-host http-stream))

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

(declaim (inline file-stream-creation-date))

(define file-stream-creation-date (file-stream)
  "Returns the creation date in universal time for FILE-STREAM's source file."
  (file-write-date file-stream))

(declaim (inline file-stream-modification-date))

(define file-stream-modification-date (file-stream)
  "Returns the modification date in universal time for FILE-STREAM's source file."
  (file-write-date file-stream))

(declaim (inline file-stream-length-in-bytes))

(define file-stream-length-in-bytes (file-stream)
  "Returns the length in bytes for FILE-STREAM's source file."
  (file-length file-stream))

(defgeneric file-length-in-bytes (pathname-or-url &optional new-length)
   (:documentation "Returns the number of bytes in PATHNAME-OR-URL or NIL."))

(defmethod file-length-in-bytes ((pathname pathname) &optional new-length)
  (declare (ignore new-length))
  (with-open-file (file-stream pathname)
    (file-length file-stream)))

(defmethod file-creation-date ((pathname pathname))
  (file-write-date pathname))

(defmethod file-modification-date ((pathname pathname))
  (file-write-date pathname))

(declaim (inline file-stream-version))

(defun file-stream-version (file-stream)
  (file-stream-creation-date file-stream))

(declaim (inline file-version))

(defun file-version  (pathname)
  (when (probe-file pathname)
    (file-creation-date pathname)))

(declaim (inline file-properties))

(define file-properties (pathname)
  "Returns the length in bytes  and the creation in in universal time 
for FILE-STREAM's source file."
  (declare (values length-in-bytes creation-date version))
  (with-open-file (file-stream pathname)
    (values (file-stream-length-in-bytes file-stream)
            (file-stream-creation-date file-stream)
            (file-stream-version file-stream))))

(declaim (inline pathname-directory-p))

(defun pathname-directory-p (pathname)
  "Returns non-null if PATHNAME denotes a directory."
  (when pathname
	(let ((namestring (ext:unix-namestring pathname)))
	  (when namestring
		(eq (unix:unix-file-kind namestring) :directory)))))

(defun unix-directory-list* (pathname predicate &rest options)
  (let ((dirs (directory  pathname  :all nil :truenamep nil))) ;;; :follow-links nil)))
    (when (not (member :directories options))
      (setq dirs (loop for file in dirs
                       unless (pathname-directory-p file)
			 collect file)))
    (when predicate
      (setq dirs (loop for file in dirs
		       when (funcall predicate file)
			 collect file)))
    ;; filter -- better to merge this with the predicate
    (delete-if (lambda (pathname)
		 (some (lambda (item)
			 (search item (directory-namestring pathname) :test #'string-equal))
		       '("RCS" "CVS")))
	       dirs)))

(defun directory-list (pathname &rest options)
  "Returns a lisp Machine style directory listing."
  (let ((pathnames (apply #'unix-directory-list* (merge-pathnames pathname "*.*") nil options)))
    (when (member :sorted options)
      (setq pathnames (sort pathnames #'(lambda (x y)
                                          (and (string< (pathname-name x)
                                                        (pathname-name y))
                                               (string< (pathname-type x)
                                                        (pathname-type y)))))))
    (loop with length and creation-date
          for path in pathnames
          do (multiple-value-setq (length creation-date)
               (file-properties path))
          collect `(,path 
                    ,.(when length `(:length-in-bytes ,length))
                    ,.(when creation-date `(:creation-date ,creation-date))))))

(declaim (inline alphalessp))

(defun alphalessp (a b)
  (string< a b))

(define directory-info (pathname &key (name :wild) (type :wild) (version :newest) (sort-pathnames t)
                                 directories)
  "Returns a poperty list of information for every file in the directory PATHNAME
that matches pathnames wildcards. Directories are included when directories is non-null."
  (declare (notinline))
  (flet ((get-directory-listing (p &optional (sort-p sort-pathnames))
           (let ((args nil))
             (declare (dynamic-extent args))
             (when sort-p (push :sorted args))
             (when directories (push :directories args))
             (apply #'directory-list p :no-extra-info args)))
         (pattern (path type)
           (make-pathname :host (pathname-host path)
                          :directory (pathname-directory path)
                          :name (etypecase name
                                  (keyword
                                    (ecase name
                                      (:wild "*")))
                                  (string name))
                          :type (etypecase type
                                  (keyword
                                    (case type
                                      (:wild "*")
                                      (t (symbol-name type))))
                                  (string type))
                          :version (etypecase version
                                     (keyword
                                       (ecase version
                                         (:wild nil)
                                         (:newest :newest))))))
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
            (:wild (get-directory-listing (pattern p "*")))))
        (string
          (get-directory-listing (pattern p type)))
        (cons
          (loop for type in type
                nconc (get-directory-listing (pattern p type)) into paths
                finally (return (if sort-pathnames
                                    (sort paths #'sorter)
                                    paths))))))))

(defun directory-list* (pathname predicate &rest options)
  "Accepts the options :FILES :DIRECTORIES :SORTED :PROPERTIES."
  (let ((pathnames (apply #'unix-directory-list* (merge-pathnames pathname "*.*") predicate options)))
    (when (member :sorted options)
      (setq pathnames (sort pathnames #'(lambda (x y)
                                          (and (string< (pathname-name x)
                                                        (pathname-name y))
                                               (string< (pathname-type x)
                                                        (pathname-type y)))))))
    (cond ((member :properties options)
           (loop for path in pathnames
                 collect  (multiple-value-bind (length creation-date)
                              (file-properties path) 
                            `(,path 
                              ,.(when length `(:length-in-bytes ,length))
                              ,.(when creation-date `(:creation-date ,creation-date))))))
          (t pathnames))))

(define create-directories-recursively (pathname)
  "Recursively create directories according to the directories present in PATHNAME."
  (ensure-directories-exist pathname :verbose t))

(defgeneric probe-directory (pathname)
  (:documentation "Returns non-null if the directory pathname exists."))

(defmethod probe-directory ((pathname pathname))
  (probe-file
   (make-pathname :host (pathname-host pathname)
		  :device (pathname-device pathname)
		  :directory (pathname-directory pathname))))

(defmethod probe-directory ((pathname string))
  (probe-directory (pathname pathname)))
   
(define-macro with-automatic-login ((host user-id user-pw) &body body)
  "Supplies userid and PW to ensure successul FTP login to host with BODY."
  `(progn (notify-log-window "~&(WITH-AUTOMATIC-LOGIN (~S ~S ~S) - Not available on MAC"
                             ,host ,user-id ,user-pw)
          ,@body))

(define ftp-directory-info (host port directory &optional (user-id "anonymous")
				 (user-pw (user-mail-address)))
  "Returns a list of pathname spec for host/directory just like DIRECTORY-INFO.
  If a network error is encountered, this returns NIL."
  (handler-case 
   (with-ftp-stream (control-stream host user-id user-pw :port port)
     (with-ftp-data-stream (data-stream control-stream "LIST" :input directory)
       ;; Not yet.
       nil))
   ;; Handle remote connection problems, including dead host, refused
   ;; connection.
   (error () nil)))

(define ftp-copy-file (host port path to-stream
		         &key (element-type 'character)
		         (user-id "anonymous") (user-pw (user-mail-address)))
  "Copies the content of FROM-HOST/FROM-PATH to TO-STREAM. 
  If an network error is encountered, this returns NIL, otherwise T.
  ELEMENT-TYPE is the ANSI file opening argument."
  (declare (ignore element-type))
  (handler-case 
   (with-ftp-stream (control-stream host user-id user-pw :port port)
     (with-ftp-data-stream (data-stream control-stream "RETR" :input path)
       (http::stream-copy-until-eof data-stream to-stream)
       (values t)))
   ;; Handle remote connection problems, including dead host, refused
   ;; connection.
   (error () nil)))

(declaim (inline char-bits))

;; returns font or shift bits
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

;; Presently only does exact ip number matches.
; Needs to match partial addresses (e.g., 128.52.0.0) -- JCMa 12/30/1994.
(define ip-host-trusted-p (address secure-subnets &optional network)
  "Returns non-null if IP-address address is trusted given secure-subnets."
  (declare (ignore network))
  (flet ((address-match-p (addr1 addr2)
           (= addr1 addr2)))
    (declare (inline address-match-p))
    (cond (secure-subnets
           (member (etypecase address
                     (integer address)
		     ;; dtc: trouble here, can't be a string if tested
		     ;; with '='!
                     (string (%parse-internet-address address)))
                   secure-subnets
                   :test #'address-match-p))
          (t t))))

;;;------------------------------------------------------------------- 
;;;
;;; STREAM HACKING
;;;

;;; Currently on CMUCL all streams are character streams; not much
;;; point changing until the portable source is fixed.
(define-macro with-binary-stream ((stream direction) &body body)
  "Turns STREAM into a binary stream within the scope of BODY.
  direction can be :OUTPUT, :INPUT, or :BOTH."
  (declare (ignore stream direction))
  `(progn ,@body))

#+nil
(define-macro with-binary-stream ((stream direction) &body body)
  "Turns STREAM into a binary stream within the scope of BODY.
  direction can be :OUTPUT, :INPUT, or :BOTH."
  `(let ((orig-element-type (lisp::fd-stream-element-type stream)))
     (unwind-protect
	 (progn
	   ,(when (or (eq direction :output) (eq direction :both))
		  `(when (eq (lisp::fd-stream-bout ,stream)
			     #'lisp::ill-bout)
			 (setf (lisp::fd-stream-bout ,stream)
			       #'lisp::output-unsigned-byte-full-buffered)))
	   ,(when (or (eq direction :input) (eq direction :both))
		  `(when (eq (lisp::fd-stream-bin ,stream)
			     #'lisp::ill-bin)
			 (setf (lisp::fd-stream-bin ,stream)
			       #'lisp::input-unsigned-8bit-byte)))
	   (setf (lisp::fd-stream-element-type ,stream) '(unsigned-byte 8))
	   ,@body)
       (setf (lisp::fd-stream-element-type ,stream) orig-element-type))))

(define-macro with-text-stream ((stream direction) &body body)
  "Turns STREAM into a text stream within the scope of BODY.
  direction can be :OUTPUT, :INPUT, or :BOTH."
  (declare (ignore stream direction))
  `(progn ,@body))

;;;------------------------------------------------------------------- 
;;;
;;; ABORTING CONNECTIONS
;;; 

#+nil
(define live-connection-p (http-stream)
  (declare (optimize (speed 3)))
  "Returns non-null if the TCP/IP connection over HTTP-STREAM remains alive
  in that the remote host continue to respond at the TCP/IP level."
  (and (open-stream-p http-stream)
       (unix:unix-fstat
	#-mp (lisp::fd-stream-fd http-stream)
	#+mp (http-stream-fd http-stream))))

(define live-connection-p (http-stream)
  (declare (optimize (speed 3)))
  "Returns non-null if the TCP/IP connection over HTTP-STREAM remains alive
  in that the remote host continue to respond at the TCP/IP level."
  (and (open-stream-p http-stream)
       (handler-case
	(ext:get-peer-host-and-port
	 #-mp (lisp::fd-stream-fd http-stream)
	 #+mp (http-stream-fd http-stream))
	(error ()))))

(declaim (inline abort-http-stream))

(define abort-http-stream (http-stream)
  "Closes http-stream in abort mode.  
This will push any output in the transmit buffer and catch any network errors.
Takes care to clean up any dangling pointers."
  (handler-case 
   (close http-stream :abort t)
   (error ())))

;; these definitions should be moved into the shared code -- JCMa 12/30/1994.
(define abort-current-connection ()
  "Aborts the computation associated with the current HTTP connection."
  (signal 'http-abort))

(declaim (inline abort-if-connection-dead))

(define abort-if-connection-dead (http-stream)
  "Aborts the HTTP connection if the TCP/IP connection over HTTP-STREAM
has died, i.e. the remote host is no longer connected."
  (unless (live-connection-p http-stream)
    (abort-current-connection)))

;;;------------------------------------------------------------------- 
;;;
;;; LOGGING EVENTS
;;; 

;;; Bound to the HTTP server log window when one exists.
(defvar *log-window* nil)

(define notify-log-window (format-string &rest format-args)
  "Top-level method for writing to the HTTP log window."
  (let ((stream *trace-output*))
    (fresh-line stream)
    (write-char #\[ stream)
    (http::write-standard-time (get-universal-time) stream)
    (write-string "]  " stream)
    (apply #'format stream format-string format-args)
    ;; Flush it.
    (finish-output *trace-output*)))

(define expose-log-window ()
  "Exposes the Log window. Does nothing."
  nil)

(define common-logfile-notify (server)
  "Issues a notification of server activity on a window."
  (http::write-common-logfile-entry server *trace-output*)
  (terpri *trace-output*)
  (finish-output *trace-output*))

(define log-http-server-error (format-string &rest format-args)
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

;;;------------------------------------------------------------------- 
;;;
;;; TIME RELATED 
;;;

(defvar *time-zone* nil
   "Time zone offset from GMT in hours.")

(define time-zone (&optional update-p)
  "Returns the timezone as an offset from GMT."
  (if (or update-p (not *time-zone*))
      (setq *time-zone*
	    (multiple-value-bind (second minute hour date month year day
					 daylight timezone)
	       (get-decoded-time)
	       (declare (ignore second minute hour date month year day
				daylight))
	       timezone))
    *time-zone*))

(defvar *daily-server-timer* nil)

#+mp
(defun synchronize-daily-server-tasks ()
  (when *daily-server-timer*
    (mp:destroy-process *daily-server-timer*))
  (setq *daily-server-timer*
	(mp:make-process
	 #'(lambda ()
	     (loop
	      ;; Sleep until 3am.
	      (sleep (- (next-3am-universal-time) (get-universal-time)))
	      ;; The guts of run-daily-server-tasks, see www-utils.lisp.
	      (with-null-stream (*standard-output* *query-io*)
		(loop with week-day = (weekday)
		      for (name periodicity form) in *daily-server-tasks*
		      when (case periodicity
			     (:daily t)
			     (:weekly (eq week-day :sunday))
			     (t (eq periodicity week-day)))
		      do (handler-case
			  (apply (car form) (cdr form))
			  (error (err)
				 (log-http-server-error
				  "Error in daily server task, ~S:~&~A"
				  name (report-string err))))))))
	 :name "Daily Server Tasks")))

#-mp
(defun synchronize-daily-server-tasks ())

;;;------------------------------------------------------------------- 
;;;
;;; MAIL SENDING  
;;;

;;; Note: redefined if smtp/mail.lisp is loaded.
;;;
(define report-bug  (to subject format-string &rest format-args)
  (notify-log-window (with-output-to-string
                       (stream)
                       (format  stream "~&Report Bug:  To:~A~
                                                       ~&Subject: ~A"
                                to subject)
                       (fresh-line stream)
                       (apply #'format stream format-string format-args))))

(defun http-user-email-address ()
  "Return the login name of the user running the server."
;;  (lisp::lookup-login-name (unix:unix-getuid)))
  (unix:user-info-name (unix:unix-getpwuid (unix:unix-getuid))))

(define tcp-service-port-number (protocol &optional error-p)
  "Returns the service port number for the TCP protocol denoted by protocol.
  PROTOCOL is a keyword, but integer and string are also supported."
  (flet ((tcp-service-port (protocol error-p)
	   (cond ((eq protocol :smtp) 25)
		 (error-p (error "Unknown protocol name, ~S" protocol))
		 (t nil))))
    (etypecase protocol
      (integer protocol)
      (keyword (tcp-service-port protocol error-p))
      (string (tcp-service-port 
	       (http::symbolize protocol http::*keyword-package*) error-p)))))


;;;------------------------------------------------------------------- 
;;;
;;; LOG RELATED PORTABILITY
;;;

#+mp
(defun make-lock (name &key (type :simple))
  "Returns a lock named name that is suitable for use with with-lock-held.
  TYPE can be either :SIMPLE (the default) or :MULTIPLE-READER-SINGLE-WRITER.
  A simple lock queues all operations whereas a :MULTIPLE-READER-SINGLE-WRITER
  lock allows multiple read threads but only a single writer thread."
  (declare (ignore type))
  (mp:make-lock name))

#-mp
(defun make-lock (name &key (type :simple))
  (declare (ignore name type))
  nil)

#+mp
(defmacro with-lock-held ((lock &optional (mode :write) (whostate "Wait for Lock")) &body body)
  "Executes BODY with LOCK held in MODE, which is one of :READ or :WRITE."
  (declare (ignore mode))
  `(mp:with-lock-held (,lock ,whostate)
     ,@body))

#-mp
(defmacro with-lock-held ((lock &optional (mode :write)
				(whostate "Wait for Lock")) &body body)
  "Executes BODY with LOCK held in MODE, which is one of :READ or :WRITE."
  (declare (ignore mode whostate lock))
  `(progn
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
;;; CLIENT SUPPORT.
;;;

(declaim (inline http::deallocate-client-http-stream))

;;; No-op. Present for implementations that resource their TCP
;;; streams, e.g. mac
(defun http::deallocate-client-http-stream (stream)
  (declare (ignore stream)))


;;;------------------------------------------------------------------- 
;;;
;;; ATOMIC CREATION OF CRLF FILES
;;;


;;;------------------------------------------------------------------- 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Use CMUCL x86 multi-process support if available.
#+mp
(progn
  (defun process-run-function (name function &rest args)
    (mp:make-process #'(lambda ()
			 (apply function args))
		     :name name))
  (defun make-process (process-name &key (type :simple) &allow-other-keys)
  "Creates a process using a portable set of options.
  TYPE can be either :SIMPLE (the default) or :MULTIPLE-READER-SINGLE-WRITER.
  A simple lock queues all operations whereas a :MULTIPLE-READER-SINGLE-WRITER
  lock allows multiple read threads but only a single writer thread.
  CMUCL does not implement :MULTIPLE-READER-SINGLE-WRITER as of version 18b."
  (declare (ignore type))
    (mp:make-process nil :name process-name))

  (defun current-process ()
    mp:*current-process*)

  (defmacro process-wait (wait-reason predicate &rest args)
    (let ((bindings nil)
	  (arg-tmps nil))
      (dolist (arg args)
	(let ((arg-tmp (gensym)))
	  (push `(,arg-tmp ,arg) bindings)
	  (push arg-tmp arg-tmps)))
      `(let (,@(nreverse bindings))
	(mp:process-wait ,wait-reason
	 #'(lambda ()
	     (declare (optimize (speed 3)))
	     (funcall ,predicate ,@(nreverse arg-tmps)))))))

  (defmacro process-wait-with-timeout (wait-reason timeout predicate
						   &rest args)
    (let ((bindings nil)
	  (arg-tmps nil))
      (dolist (arg args)
	(let ((arg-tmp (gensym)))
	  (push `(,arg-tmp ,arg) bindings)
	  (push arg-tmp arg-tmps)))
      `(let (,@(nreverse bindings))
	(mp:process-wait-with-timeout ,wait-reason ,timeout
	 #'(lambda ()
	     (declare (optimize (speed 3)))
	     (funcall ,predicate ,@(nreverse arg-tmps)))))))

  (defun process-disable (process)
    (mp:disable-process process))
  (defun process-enable (process)
    (mp:enable-process process))
  (defun process-kill (process)
    (mp:destroy-process process))
  (defun process-reset (process)
    (mp:restart-process process))

  (defun process-run-time (process)
    "Returns the amount of run time the process has accumulated, in
  microseconds."
    (values (truncate (* 1000000d0 (mp::process-run-time process)))))

  (defun process-idle-time (process)
    "Returns the amount of time since the process ran last, in sixtieths of
  a second."
    (values (truncate (* 60d0 (mp::process-idle-time process)))))

  (defun with-timeout-internal (timeout function &optional error-p)
    (catch 'timer-interrupt
      (let* ((current-process mp:*current-process*)
	     (timer-process (mp:make-process
			     #'(lambda ()
				 (sleep timeout)
				 (mp:process-interrupt
				  current-process
				  #'(lambda () (throw 'timer-interrupt nil))))
			     :name "Timeout timer")))
	(unwind-protect
	     (return-from with-timeout-internal (funcall function))
	  (mp:destroy-process timer-process))))
    (when error-p
      (error "Timeout: body took longer than ~d second(s) to complete."
	     timeout)))

  (defmacro with-timeout ((timeout &key error-p) &body body)
    "Executes body and returns the values of the last form in body. However, if
  the execution takes longer than timeout seconds, abort it. If :error-p is
  unsupplied or false, just return nil. If :error-p is true, signal an error."
    `(flet ((fn () . ,body))
       (with-timeout-internal ,timeout #'fn ,error-p)))
) ; end progn

;;; Some hacks if multi-process support is not available or used.
#-mp
(progn

(defun current-process ()
  "Returns the current process."
  0)

(defun process-run-time (process)
  "Returns the amount of run time the process has accumulated, in
  microseconds."
  (declare (ignore process))
  (* (/ 1000000 internal-time-units-per-second) (get-internal-run-time)))

(defun process-idle-time (process)
  "Returns the amount of time since the process ran last, in sixtieths of
  a second."
  (declare (ignore process))
  0)

;;;;---------------------------------------------------------------------------
;;;
;;; Wait until input is available on stream.

;;; The simplest and safest thing to do is to just return nil; can do
;;; a few polled checks. It is probably best to return quickly so the
;;; connection can be closed and others accepted.
(defun process-wait-with-timeout (string timeout fun stream)
  (declare (ignore string timeout))
  (force-output stream)
  (dotimes (i 10 nil)
     (when (funcall fun stream)
	   (return t))))

;;; This version uses sleep to wait - slow.
#+nil
(defun process-wait-with-timeout (string timeout fun stream)
  (declare (ignore string))
  (force-output stream)

  ;; Wait
  (dotimes (i timeout)
     ;; Usable ?
     (when (funcall fun stream)
	   (return-from process-wait-with-timeout t))
     (sleep 1))
  ;; Time-out
  (return-from process-wait-with-timeout nil))

;;; This version calls the event server. This does not work well as
;;; other connection events may be processed!? Perhaps if the
;;; connection requests were queued?
#+nil
(defun process-wait-with-timeout (string timeout fun stream)
  (declare (ignore string))
  (force-output stream)
  
  (let ((fd (system:fd-stream-fd stream)))
    ;; Wait
    (dotimes (i 50)
       (cond ((lisp::wait-until-fd-usable fd :input timeout)
	      ;; Usable
	      (when (funcall fun stream)
		    (return-from process-wait-with-timeout t)))
	     (t
	      ;; Time-out
	      (return-from process-wait-with-timeout nil))))
    ;; Probably a closed connection??
    (close stream :abort t)
    nil))
) ; progn


;;; Process priorities are not implemented, but at least provide some
;;; dummy functions.
(defun process-priority (process)
  (declare (ignore process))
  0)

(defun (setf process-priority) (value process)
  (declare (ignore process))
  value)


;;;------------------------------------------------------------------- 
;;;
;;; Disable the SIGPIPE interrupt.
;;;

(defun disable-sigpipe ()
  (sys:ignore-interrupt unix:sigpipe))

;;; Disable now.
(disable-sigpipe)

;;; Disable when restarted.
(pushnew 'disable-sigpipe ext:*after-save-initializations*)


;;;------------------------------------------------------------------- 
;;;
;;; Save a CL-HTTP image.
;;;

(defun cl-http-initialise (switch)
  (let ((init-file (or (ext:cmd-switch-value switch)
		       (first (ext:cmd-switch-words switch))
		       "http:cmucl;examples;cl-http-init.lisp")))
    (load init-file :if-does-not-exist nil)))

(defun save-cl-http (core-file-name)
  ;; Shutdown CL-HTTP.
  (http:disable-http-service)
  (http::clear-access-logs)
  #+mp
  (when *daily-server-timer*
    (mp:destroy-process *daily-server-timer*)
    (setq *daily-server-timer* nil))

  ;; Add CL-HTTP to the CMUCL herald.
  (setf (getf ext:*herald-items* :cl-http) 
	(list (format nil "    CL-HTTP/~D.~D"
		      (first cl-user::*cl-http-server-version*)
		      (second cl-user::*cl-http-server-version*))))

  ;; Setup a CL-HTTP initialisation command line switch.
  (ext:defswitch "cl-http-init" #'cl-http-initialise)
  ;;
  (ext:save-lisp core-file-name))

;;; MOP

(defun www-utils::class-slots (class)
  (pcl:class-slots class))

(export 'WWW-UTILS::CLASS-SLOTS)

