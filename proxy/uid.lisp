;;; -*- mode: lisp; syntax: ansi-common-lisp; package: http; base: 10 -*-
;;;
;;; Copyright John C. Mallery,  1999-2000.
;;; All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;;  PORTABLE FACILITY FOR GENERATING UNIQUE IDS
;;;
;;;  The main function is UID-SERIES-ALLOCATE, which returns a unique integer.
;;;  Uniqueness is preserved across reboot by storing data on disk.
;;;   

(in-package :http) 

;;;------------------------------------------------------------------- 
;;;
;;;
;;;

;; Export parameters users may set and functions they may call.
(export '(define-uid-series uid-series-allocate uid-series-initialized-p uid-series-interval uid-series-string uid-series-width)) 

;;; KAP: sbcl used that fail with #P"http:pw;uid;", fixed in 0.9.8.14 are a bit earlier
(defstruct (uid-series (:print-function print-uid-series))
  
  (last 0 :type integer)			;last UID released  
  (chunk-limit 0 :type integer)			;limit after which a new UID file must be written
  (lock (make-lock "UID Lock"))			;thread safety
  (initialized-p nil :type boolean)		;flag indicating whether the UID facility has already been initialized
  (width 8 :type fixnum)			;width of unique ID strings in characters
  (directory #p"http:pw;uid;" :type pathname)	;directory where UIDs are kept
  (interval 100. :type integer)			;number of UIDs allocated per chunk.
  (pathname nil :type (or null pathname))	;pathname of the current UID file.
  (wild-path nil :type (or null pathname))	;wild carded pathname used for performing directory operations
  (type nil :type (or null string))		;type for the series
  (notifier 'log-event))			;function to used to log events called with (event-class format-string&rest format-args)

(defun print-uid-series (uid-series stream depth)
  (declare (ignore depth))
  (print-unreadable-object (uid-series stream :type t :identity t)
    (format stream "~:[no type~;~:*~A~]: ~D (~D)" (uid-series-type uid-series) (uid-series-last uid-series) (uid-series-interval uid-series))))

;;;------------------------------------------------------------------- 
;;;
;;;  FUNCTIONS
;;;

(defun uid-series-initialize-local-pathname (uid-series)
  "Initializes the local UID-SERIES wild pathname."
  (setf (uid-series-wild-path uid-series) (make-pathname :name :wild :type (uid-series-type uid-series)
							 :defaults (uid-series-directory uid-series))))

(defun uid-series-wild-pathname (uid-series)
  (or (uid-series-wild-path uid-series)
      (uid-series-initialize-local-pathname uid-series)))

(defun log-event (event-class format-string &rest format-args)
  (declare (ignore event-class))
  (apply #'notify-log-window format-string format-args))

(defun uid-series-log-event (uid-series event-class format-string &rest format-args)
  (declare (dynamic-extent format-args))
  (apply (uid-series-notifier uid-series) event-class format-string format-args))

(defun uid-series-string (uid-series integer)
  (write-integer-to-string integer 10. (uid-series-width uid-series) #\0))

(defun uid-series-check-limit (uid-series)
  (declare (values wrote-new-uid-series-file-p))
  (let ((chunk-limit (uid-series-chunk-limit uid-series)))
    (when (eql chunk-limit (uid-series-last uid-series))
      (let* ((new-limit (+ chunk-limit (uid-series-interval uid-series)))
	     (old-pathname (uid-series-pathname uid-series))
	     (new-pathname (make-pathname :name (uid-series-string uid-series new-limit) :defaults old-pathname)))
	(rename-file old-pathname new-pathname)
	#+ignore (uid-series-log-event uid-series :normal "Allocating new chunk (~D) on disk" new-limit)
	(setf (uid-series-pathname uid-series) new-pathname
	      (uid-series-chunk-limit uid-series) new-limit)
	t))))

(defun uid-series-create-file (uid-series pathname &optional report-p close-p)
  (let (stream)
    (unwind-protect
	(setq stream (open pathname :direction :output :if-does-not-exist :create :if-exists :error))
      (when (and close-p stream)
	(close stream)))
    (when report-p
      (uid-series-log-event uid-series :normal "Creating ~A" (truename pathname)))
    pathname)) 

(defun uid-series-initialize (uid-series)
  "Initializes the uid-series facility."
  (pathname-create-directory-if-needed (uid-series-directory uid-series))
  (let* ((uid-series-files (directory (uid-series-wild-pathname uid-series)))
	 (earlier-files (butlast uid-series-files)))
    (cond (uid-series-files
	   (uid-series-log-event uid-series :normal "Restoring uid-series state from disk")
	   (when earlier-files
	     (uid-series-log-event uid-series :problem "More than one uid-series file, deleting old files")
	     (dolist (file earlier-files)
	       (delete-file-carefully file)))
	   (let* ((current-pathname (car (last uid-series-files)))
		  (current-last (parse-integer (pathname-name current-pathname))))
	     (setf (uid-series-pathname uid-series) current-pathname
		   (uid-series-last uid-series) current-last
		   (uid-series-chunk-limit uid-series) current-last)
	     (uid-series-log-event uid-series :normal "Reading uid-series file ~A" current-pathname)
	     (uid-series-check-limit uid-series)))
	  (t (uid-series-log-event uid-series :normal "Creating new uid-series state on disk")
	     (let* ((interval (uid-series-interval uid-series))
		    (current-pathname (make-pathname :name (uid-series-string uid-series interval)
						     :type (uid-series-type uid-series) :defaults (uid-series-directory uid-series))))
	       (setf (uid-series-pathname uid-series) current-pathname)
	       (uid-series-create-file uid-series current-pathname t t)
	       (setf (uid-series-last uid-series) 0
		     (uid-series-chunk-limit uid-series) interval))))
    (setf (uid-series-initialized-p uid-series) t)))

(defun uid-series-allocate (uid-series)
  "Allocates a unique integer greater than zero.
Uniqueness is guaranteed across reboots."
  (with-lock-held ((uid-series-lock uid-series) :write "UID Allocate")
    (if (uid-series-initialized-p uid-series)
	(uid-series-check-limit uid-series)
	(uid-series-initialize uid-series))
    (incf (uid-series-last uid-series))))

(defun create-uid-series (type interval width directory notifier)
  (check-type interval integer)
  (check-type width integer)
  (check-type notifier function)
  (let ((type (delete-if-not #'(lambda (ch) (or (alpha-char-p ch) (member ch '(#\-)))) (string-downcase type))))
    (make-uid-series :type type
		     :interval interval
		     :width width
		     :directory (make-pathname :name nil :type nil :version nil :defaults directory))))

(defmacro define-uid-series (type variable &key (directory "http:pw;uid;")
			     (interval 10) (width 8) (notifier '#'log-event))
  "Defines a Unique ID Series named TYPE.
VARIABLE is a symbol that will hold the UID series object.
DIRECTORY is where persistent data is stored.
INTERVAL is the number of IDs between references to persistent storage.
WIDTH is the minimum size of an UID string."
  `(defparameter ,variable (or (and (boundp ',variable) ,variable)
			       (create-uid-series ',type ,interval ,width ,directory ,notifier))))

;;;------------------------------------------------------------------- 
;;;
;;; TESTING CODE
;;;

#|

(define-uid-series proxy-uid *proxy-uid-series*
  :directory "http:pw;uid;"
  :interval 10
  :width 8
  :notifier #'(lambda (event format-string &rest format-args)
		(declare (ignore event))
		(format t format-string format-args)))

(defun uid-series-test (n &optional (uid-series *proxy-uid-series*))
  (format t "Testing uid-series Allocation")
  (loop with warn = (/ (uid-series-interval uid-series) 10.)
	repeat n
	do (when (zerop (rem (uid-series-last uid-series) warn))
	     (format t "Allocated uid-series ~D " (uid-series-last uid-series)))
	   (print (uid-series-allocate uid-series))))

(uid-series-test 20)


|#
