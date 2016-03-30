;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-
;;;
;;; (C) Copyright 2000, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;;
;;; Earlier non-persistent draft.
;;; (C) Copyright 1996-97, Christopher R. Vincent
;;;     All Rights Reserved.
;;; (C) Enhancements 1998, John C. Mallery
;;;
;;; Completely rewritten to actually work! 5/6/2000 -- JCMa.

(in-package :http)

;;;------------------------------------------------------------------- 
;;;
;;; GENERIC DATABASE INTERFACE
;;;

(defmethod print-object ((handle database-handle) stream)
  (print-unreadable-object (handle stream :type t :identity t)
    (let ((uid (handle-uid handle)))
      (when uid 
	(format stream "~D" uid)))))

(defgeneric initialize-database (database)
  (:documentation "Initialize an instance of a database."))

(defmethod generate-database-name ((database database))
  (concatenate 'string (string-capitalize (type-of database)) "-" (write-integer-to-string (atomic-incf *database-counter*) 10)))

(defmethod initialize-instance :after ((database database) &key)
  (unless (database-name database)
    (setf (database-name database) (generate-database-name database))) 
  (initialize-database database))

(defgeneric allocate-handle (database uid type)
  (:documentation "Return a new database handle based on UID and TYPE.
UID is a unique number for the database. 
TYPE is one of :ENTITY or :metadata."))

(defmethod allocate-handle ((database database) (uid integer) type)
  (handle-initialize (make-instance (ecase type
				      (:entity (entity-handle-class database))
				      (:metadata (metadata-handle-class database)))
				    :database database
				    :uid uid)))

(defgeneric handle-initialize (handle)
  (:documentation "Initialize an instance of a HANDLE."))

(defgeneric handle-write (handle stream)
  (:documentation "Write HANDLE on STREAM."))

(defgeneric handle-object-size (handle)
  (:documentation "Returns the number of bytes associated with the object dnoted by HANDLE."))

(defgeneric handle-object-delete (handle)
  (:documentation "Deletes the object denoted by HANDLE its associated database."))

(defgeneric handle-invalidate (handle)
  (:documentation "Invalidates HANDLE so that it cannot be used to access invalid data."))

(defgeneric handle-make-valid (representation-or-handle)
  (:documentation "Makes any associated handles be valid."))

(defgeneric handle-valid-p (handle &optional verify-p)
  (:documentation "Returns non-null if HANDLE points to valid data.
When VERIFY-P is non-null, this actively checks to make sure the handle is valid."))

(defgeneric initialize-proxy-database (cache)
  (:documentation "Initializes the proxy database, CACHE for proxy service.
This assumes that proxy service is not enabled while it runs.
Specialize this for different classes of datavases."))

(define-generic note-metadata-update (representation time-stamp)
  (:documentation "Notes that the metadata of REPRESENTATION has been updated at TIME-STAMP."))

(define-generic save-persistently-unsaved-metadata (representation)
  (declare (values unsaved-metadata-p))
  (:documentation "Notes that REPRESENTATION's metadata needs to be saved persistently
if it has been modified since the last persistent save."))

(defgeneric save-metadata (entity mode)
  (:documentation "Saves the ENTITY metadata according to MODE."))

(defgeneric save-metadata-persistently (database representation mode)
  (:documentation "Saves REPRESENTATION's metadata persistently in DATABASE according to MODE.
Specialize this method for platform-specific modes or database-specific backend stores."))

(defgeneric restore-metadata (database handle mode)
  (declare (values successful-restoration-p))
  (:documentation "Restores persistently saved ENTITY metadata denoted by HANDLE to MODE.
Specialize this method for platform-specific modes or database-specific backend stores."))

(defgeneric database-storage-size (database)
  (:documentation "Returns the amount of storage used by DATABASE in bytes."))

(defmethod database-storage-size ((cache cache))
  (database-storage-size (cache-database cache)))

;; Return a useful number to avoid losing in a GC wait function.
(defmethod database-storage-size ((database null)) 0)

(defgeneric database-kill-all-processes (database)
  (:method-combination progn)
  (:documentation "Kills all processes associated with database."))

(defmethod database-kill-all-processes progn ((database database))
  nil)

(defgeneric database-allocate-representation (database cache resource &optional init-args)
  (:documentation "Create an instance of a resource representation."))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(defgeneric database-open (database handle direction)
  (declare (values stream))
  (:documentation "Opens a stream to the entity denoted by HANDLE in DATABASE.
DIRECTION is either :INPUT or :OUTPUT."))

(defgeneric database-close (stream handle direction &optional abort-p)
  (:documentation "Closes the STREAM into the entity denoted by HANDLE.
DIRECTION is either :input or :output.
When abort-p is non-null, stream is closed in abort mode."))

(defmacro with-open-database ((stream handle direction) &body body)
  "Execute body with STREAM bound to stream into the entity denoted by HANDLE.
DIRECTION is either :INPUT or :OUTPUT."
  `(let ((,stream)
	 (handle ,handle)
	 (direction ,direction)
	 (abort-p t))
     (unwind-protect
	 (multiple-value-prog1
	   (progn
	     (setq ,stream (database-open (handle-database handle) handle direction))
	     ,@body)
	   (setq abort-p nil))
       (when ,stream
	 (database-close ,stream handle direction abort-p)))))


;;;------------------------------------------------------------------- 
;;;
;;; ASYNCHRONOUS DATABASE OPERATIONS
;;;

(defmethod initialize-instance :after ((database basic-asynchronous-database-mixin) &key &allow-other-keys)
  ;; intialization some strings
  (setf (tq:task-queue-wait-whostate database) "Data Wait"
        (tq:task-queue-process-name database) (database-name database)
	(tq:task-process-priority database) *database-log-process-priority*)
  ;; start up the process
  (start-database-log database)
  database)

(defmethod database-kill-all-processes progn ((database basic-asynchronous-database-mixin))
  (database-log-process-kill database))

(define-generic database-log-process-kill (database)
  (:documentation "Stops database's log process and kills it."))

(defmethod database-log-process-kill ((database basic-asynchronous-database-mixin))
  (tq:task-process-kill database))

(define-generic asynchronous-metadata-save (database representation time-stamp)
  (:documentation "Saves the metadata of REPRESENTATION persistenty in ATABASE with asynchrony awareness.
metadata is only saved if REPRESENTATION is valid and  the integer, TIME-STAMP, denotes its current state."))

(defmethod asynchronous-metadata-save ((database basic-asynchronous-database-mixin) (representation representation) time-stamp &aux saved-p)
  (declare (optimize (speed 3)))
  (with-lock-held ((cache-object-lock representation) :read "Metadata Save")
    (when (and (representation-valid-p representation)	;make sure its coherent
	       (equal time-stamp (representation-verified-date representation)))	;skip save if time-stamp stale 
      (save-metadata-persistently database representation *metadata-storage-mode*)
      (setq saved-p t)))
  (when saved-p
    (with-lock-held ((cache-object-lock representation) :write "Metadata Saved")
      (when (representation-unsaved-metadata representation)
	(setf (representation-unsaved-metadata representation) nil)))))

(define-generic enqueue-asynchronous-metadata-save (database representation time-stamp)
  (:documentation "Enqueues REPRESENTATION for an asynchronous save of its metadata in database.
TIME-STAMP is an integer indicating the version of REPRESENTATION state to be saved persistently."))

;; This could use a data structure to reduce consing sometime 12/16/2000 -- JCMa.
(defmethod enqueue-asynchronous-metadata-save ((database basic-asynchronous-database-mixin) representation time-stamp)
  (flet ((asynchronous-save ()
	   (asynchronous-metadata-save database representation time-stamp)))
    (tq:push-task-queue database #'asynchronous-save)))

(define-generic ensure-active-database-log (database)
  (:documentation "Ensures that DATABASE's log is active and writing data updates into persistent storage."))

(defmethod ensure-active-database-log ((database basic-asynchronous-database-mixin))
  (tq:ensure-active-task database))

(define-generic database-log-process-priority (database)
  (:documentation "Returns the process priority of the Log Manager for DATABASE.
A SETF method allow the priority to be changed."))

(defmethod database-log-process-priority ((database basic-asynchronous-database-mixin))
  (tq:task-process-priority database))

(defmethod (setf database-log-process-priority) ((process-priority integer) (database basic-asynchronous-database-mixin))
  (setf (tq:task-process-priority database) process-priority))

(define-generic start-database-log (database)
  (:documentation "Starts DATABASE's log writing data updates into persistent storage."))

(defmethod start-database-log ((database basic-asynchronous-database-mixin))
  (tq:start-task database))

(define-generic stop-database-log (database)
  (:documentation "Stops DATABASE's log from writing data updates into persistent storage."))

(defmethod stop-database-log ((database basic-asynchronous-database-mixin))
  (tq:stop-task database))

(defmethod tq:task-queue-execute-pending-tasks ((database basic-asynchronous-database-mixin))
  (declare (optimize (speed 3)))
  (labels ((report-proxy-database-error (database error fatal-p)
	     (let ((error-type (type-of error))
		   (*print-readably* nil))
	       (when fatal-p
		 (log-event :critical "Proxy Database, ~A, suspended by fatal error.~&Take immediate corrective action."
			    (database-name database)))
	       (report-bug *bug-http-server* (format nil "Proxy~:[~; Fatal~] Database Error: ~S" fatal-p error-type)
			   "~:[~;~&The proxy database has been suspended. Attend to the error at once and resume the proxy database.~]~
                            ~&Database: ~S~&Error: ~S~:[~;~&Error Report: ~:*~A~]~:[~;~&Backtrace: ~:*~A~]"
			   fatal-p database error-type (report-string error)
			   (when *stack-backtraces-in-bug-reports*
			     (stack-backtrace-string error)))))
           (%handle-proxy-database-error (error)
             (typecase error
               ((or network-error file-error)
                (report-proxy-database-error database error nil)
		(let ((seconds 5))
		  (log-event :exceptional "Proxy Database, ~A, suspended for ~D seconds by transient ~A error."
			     (database-name database) seconds (type-of error))
		  (sleep 5)
		  (log-event :normal "Proxy Database, ~A, resumed after ~D seconds." (database-name database) seconds))
                t)
               (t (report-proxy-database-error database error t)
                  (stop-database-log database)
                  t))))
    (declare (dynamic-extent #'%handle-proxy-database-error))
    (handler-bind-if (not *debug-server*)
        ((error #'%handle-proxy-database-error))
      (loop for closure = (tq:pop-task-queue database)
	    while closure
	    ;; Careful not to lose entries when error occur executing tasks
	    do (unwind-protect
		   (progn (funcall closure)
                     (setq closure nil))
		 (when closure
		   (tq::task-queue-push-front database closure)))
	    while (tq:task-queue-run-p database)))))


;;;------------------------------------------------------------------- 
;;;
;;; HIERACHICAL DIRECTORY CACHES
;;;

(defun proxy-cache-initialize-uid-directory-pathnames ()
  "Initilizes the parameters required to map UIDs into cache pathnames."
  (setq *proxy-cache-uid-limit* (min most-positive-fixnum (expt *proxy-cache-uids-per-directory* (1+ *proxy-cache-directory-depth*)))
	*proxy-cache-uid-directory-base-list* (loop for level downfrom *proxy-cache-directory-depth* above 0
						    collect (expt *proxy-cache-uids-per-directory* level))))

(defun proxy-cache-uid-directory-components (uid)
  "Returns a list of directory components for use in building the cache files for UID.
This wraps around after  *proxy-cache-uid-limit* is reached."
  (declare (integer uid))
  (loop with component fixnum = (rem uid (the fixnum *proxy-cache-uid-limit*)) and dir
	for base fixnum in *proxy-cache-uid-directory-base-list*
	do (multiple-value-bind (directory remainder)
	       (floor component base)
	     (setq component remainder
		   dir directory))
	collect (write-integer-to-string dir 10)))

(defun proxy-make-cache-pathname (uid type cache-directory)
  (let ((directory (nconc (copy-list (pathname-directory cache-directory)) (proxy-cache-uid-directory-components uid)))
	(name (proxy-uid-string uid)))
    (make-pathname :directory directory :name name :type type :defaults cache-directory)))

(defun proxy-uid-from-cache-pathname (pathname)
  (parse-integer (pathname-name pathname)))

; We call www-utils::%pathname-as-directory directly because
; %pathname-directory-file-p does not work on the VLM because 
; (scl:send (pathname pathname) :get :directory) returns nil
(defun map-proxy-cache-directory (pathname function)
  "Maps FUNCTION over the directory pathnames containing cache files."
  (labels ((map-cache-directory (pathname depth function)
	     (macrolet ((mapping-directory ((pathname) &body body)
			  `(loop for idx upfrom 0 below *proxy-cache-uids-per-directory*
				 for directory = (make-pathname :directory (nconc (copy-list (pathname-directory ,pathname))
										  (list (write-integer-to-string idx 10)))
								:name :wild :type :wild #-Allegro :version #-Allegro :wild
								:defaults ,pathname)
				 do (handler-case
				      (loop for (path . plist) in (directory-info directory :sort-pathnames t)
					    while (getf plist :directory)
					    do #+Genera (let ((path (www-utils::%pathname-as-directory path))) . ,body)
					       #-Genera (progn . ,body))
				      (directory-not-found () (return nil))))))
                        
	       (let ((depth (1- depth)))
		 (if (= depth 1)
		     (mapping-directory (pathname)
					(funcall function path))
		     (mapping-directory
		       (pathname)
		       (map-cache-directory path depth function)))))))
    (map-cache-directory pathname *proxy-cache-directory-depth* function)))

(defun map-proxy-cache-pathnames (pathname function &key (file-type *metadata-file-type*))
  "Maps FUNCTION over the cache files in the proxy cache directory, PATHNAME.
FUNCTION is called on the cache file pathname of type, FILE-TYPE.
Cache files are enumerated in UID order."
  (flet ((map-cache-files (pathname)
	   (dolist (path (directory (make-pathname :name :wild
						   :type (etypecase file-type
							   (string file-type)
							   (symbol (case file-type
								     (:wild file-type)
								     (t (string file-type)))))
						   :version :newest :defaults pathname)))
	     (funcall function path))))
    (declare (dynamic-extent #'map-cache-files))
    (map-proxy-cache-directory pathname #'map-cache-files)))

(defun map-proxy-cache-pathnames* (pathname function &key (file-type *metadata-file-type*))
  "Maps FUNCTION over the cache files in the proxy cache directory, PATHNAME.
FUNCTION is called on (cache-pathname property-list). CACHE-PATHNAME is a 
cache file pathname of type, FILE-TYPE. PROPERTY-LIST is the files property list.
Cache files are enumerated in UID order."
  (flet ((map-cache-files (pathname)
	   (loop for (path . plist) in (directory-info (make-pathname :name :wild
								      :type (etypecase file-type
									      (string file-type)
									      (symbol (case file-type
											(:wild file-type)
											(t (string file-type)))))
								      :version :newest :defaults pathname)
						       :sort-pathnames t)
		 do (funcall function path plist))))
    (declare (dynamic-extent #'map-cache-files))
    (map-proxy-cache-directory pathname #'map-cache-files)))

;;;------------------------------------------------------------------- 
;;;
;;; FILESYSTEM DATABASE
;;;

(defmethod print-object ((database filesystem-database) stream)
  (print-unreadable-object (database stream :type t :identity t)
    (let ((pathname (filesystem-database-cache-directory database)))
      (when pathname
	(write pathname :stream stream :escape nil)))))

;; Make sure this works when the proxy is booted from an image, other define an initialization 5/7/2000 -- JCMa.
(defmethod initialize-instance :after ((database filesystem-database) &key)
  (let ((pathname (filesystem-database-cache-directory database)))
    (when pathname
      (pathname-create-directory-if-needed (pathname pathname)))))

(defmethod initialize-database ((database filesystem-database))
  (proxy-cache-initialize-uid-directory-pathnames)
  database)

(defmethod handle-initialize ((handle filesystem-handle))
  (let ((pathname (filesystem-database-cache-directory (handle-database handle)))
	(uid (handle-uid handle))
	(type (handle-type handle)))
    ;; (setf (filesystem-handle-pathname handle) (make-pathname :name (proxy-uid-string uid) :type type :defaults pathname))
    (setf (filesystem-handle-pathname handle) (proxy-make-cache-pathname uid type pathname))
    handle))

(defgeneric increment-storage-size (filesystem-database delta)
  (declare (values new-storage-size))
  (:documentation "Increments the storage size by delta with thread safety."))

(defmethod increment-storage-size ((database filesystem-database) delta)
  (with-slots (storage-size) database
    (atomic-incf storage-size delta)))

;; This one around method maintains the database storage byte count.
(defmethod (setf %handle-object-size) :around (new-size (handle database-handle))
  (declare (integer new-size)) 
  (let ((old-size (%handle-object-size handle))
	delta)
    (prog1 (call-next-method)
	   (unless (zerop (setq delta (if (or (null old-size) (zerop old-size))
					  new-size
					  (- new-size (the integer old-size)))))
	     (increment-storage-size (handle-database handle) delta)))))

(defmethod handle-compute-object-size ((handle filesystem-handle))
  (declare (optimize (speed 3)))
  (file-length-in-bytes (filesystem-handle-pathname handle)))

(defmethod handle-object-size ((handle database-handle))
  (or (%handle-object-size handle)
      (setf (%handle-object-size handle) (handle-compute-object-size handle))
      0))

(defmethod verify-storage-size ((handle database-handle))
  (let ((size (restart-case
		(handle-compute-object-size handle)
		(skip () :report "Skip File Probe" 0))))
    (unless (equal size (%handle-object-size handle))
      (format *trace-output* "~&Bad Handle Size: ~D (~D) ~S" (%handle-object-size handle) size handle))
    size))

(defmethod handle-object-delete ((handle filesystem-handle))
  (let ((pathname (filesystem-handle-pathname handle)))
    (when pathname
      (prog1 (handler-case			;speculative delete for better performance
	       (delete-file* pathname :expunge :queue)
	       (file-error () nil))
	     (setf (%handle-object-size handle) 0
		   (filesystem-handle-valid-p handle) nil)))))

(defmethod handle-database-hygine ((handle database-handle)))

#+Genera
(defmethod handle-database-hygine ((handle filesystem-entity-handle))
  (clean-up-contiguous-file-versions (filesystem-handle-pathname handle) 1 t))

#+Genera
(defmethod handle-database-hygine ((handle filesystem-metadata-handle))
  (clean-up-contiguous-file-versions (filesystem-handle-pathname handle) 1 nil))

(defmethod handle-write ((handle database-handle) stream)
  (write (handle-uid handle) :stream stream))

(defmethod handle-write ((handle filesystem-handle) stream)
  (write-string (namestring (filesystem-handle-pathname handle)) stream))

(defmethod handle-invalidate ((handle filesystem-handle))
  (setf (filesystem-handle-valid-p handle) nil))

(defmethod handle-make-valid ((handle database-handle))
  t)

(defmethod handle-make-valid ((handle filesystem-handle))
  (setf (filesystem-handle-valid-p handle) t))

(defmethod handle-valid-p ((handle filesystem-handle) &optional verify-p)
  (let ((valid-p (filesystem-handle-valid-p handle)))
    (cond ((not valid-p)
	   (if (and verify-p (probe-file (filesystem-handle-pathname handle)))
	       (setf (filesystem-handle-valid-p handle) t)
	       nil))
	  (verify-p
	   (or (probe-file (filesystem-handle-pathname handle))
	       (setf (filesystem-handle-valid-p handle) nil)))
	  (t valid-p))))

(defmethod database-open ((database filesystem-database) (handle filesystem-handle) (direction (eql :input)))
    (open (filesystem-handle-pathname handle) :direction :input
	  :element-type '(unsigned-byte 8) :if-does-not-exist :error))

(defmethod database-open ((database filesystem-database) (handle filesystem-handle) (direction (eql :output)))
  (declare (optimize (speed 3)))
  (let ((pathname (filesystem-handle-pathname handle)))
    (handler-case
        (open pathname :direction :output :element-type '(unsigned-byte 8)
              :if-does-not-exist :create :if-exists *file-exists-supersede-action*)
      (directory-not-found
       ()
       (create-directories-recursively pathname)
       (open pathname :direction :output :element-type '(unsigned-byte 8)
             :if-does-not-exist :create :if-exists *file-exists-supersede-action*))))) 

(defmethod database-close (stream (handle filesystem-handle) direction &optional abort-p)
  (declare (ignore direction))
  (close stream :abort abort-p))

#-Genera
(defmethod database-close :around (stream (handle filesystem-handle) (direction (eql :output)) &optional abort-p)
  (prog2 
    ;; Capture the file length while the stream is open
    (unless abort-p
      (setf (%handle-object-size handle) (file-stream-length-in-bytes stream)
	    (filesystem-handle-valid-p handle) t))
    ;; call the primary method
    (call-next-method stream handle direction abort-p)
    ;; Perform clean up when writes are aborted after the file stream closed.
    (when abort-p
      (handle-valid-p handle t)))) 

;;file-stream-length-in-bytes on a stream doesn't work in Genera   5/9/2000 -- JCMa.
#+Genera
(defmethod database-close :after (stream (handle filesystem-handle) (direction (eql :output)) &optional abort-p)
  (declare (ignore stream))
  (if abort-p
      (handle-valid-p handle t)
      (let ((pathname (filesystem-handle-pathname handle)))
	(handler-case 
	  (setf (%handle-object-size handle) (file-length-in-bytes pathname)
		(filesystem-handle-valid-p handle) t)
	  (file-error () (setf (%handle-object-size handle) (if (handle-valid-p handle t) (file-length-in-bytes pathname) 0)))))))

(defmethod database-allocate-representation ((database filesystem-database) (cache proxy-cache) (resource resource) &optional init-args)
  "Create an instance of a resource representation."
  (let* ((existing-uid (getf init-args :uid))
	 (uid (or existing-uid (allocate-proxy-uid)))
	 (metadata-handle (allocate-handle database uid :metadata))
	 (entity-handle (allocate-handle database uid :entity))
	 (representation (apply #'make-instance *representation-class*
				:cache cache
				:metadata-handle metadata-handle
				:entity-handle entity-handle
				:resource resource
				:uid uid
				init-args))
	 response-headers etag)
    ;; Perform any initializations related to data handles
    (setf (%handle-object-size entity-handle) (representation-entity-size representation))
    ;; update the meta-data handle when UID already exists
    (when existing-uid
      (let ((size (handle-compute-object-size metadata-handle)))
	(setf (%handle-object-size metadata-handle) size
	      (representation-metadata-size representation) size)))
    ;; Update slots caching header values fater headers are in place.
    (when (and (setq response-headers (getf init-args :response-headers))
	       (setq etag (getf response-headers :etag)))
      (setf (representation-etag representation) etag))
    representation))

(defgeneric representation-metadata-pathname (representation)
  (declare (values pathname))
  (:documentation "Returns the pathname where REPRESENTATION stores its metadata."))

(defmethod representation-metadata-pathname ((representation entity-persistence-mixin))
  (filesystem-handle-pathname (representation-metadata-handle representation)))

(defgeneric representation-entity-pathname (representation)
  (declare (values pathname))
  (:documentation "Returns the pathname where REPRESENTATION stores its entity."))

(defmethod representation-entity-pathname ((representation entity-persistence-mixin))
  (filesystem-handle-pathname (representation-entity-handle representation)))

;;;------------------------------------------------------------------- 
;;;
;;; PROXY CACHE DATABASE
;;;

(defun make-proxy-cache-database (&optional (pathname (proxy-cache-default-directory)))
  (fast-make-instance *proxy-database-class* (proxy-cache-database)
    :cache-directory (make-pathname :name nil :type nil :version nil :defaults pathname)))

(defmethod initialize-proxy-database ((database filesystem-database) &aux (count 0) (orphan-count 0))
  (flet ((reload-metadata (pathname)
	   (let ((metadata-pathname (make-pathname :type *metadata-file-type* :version :newest :defaults pathname)))
	     (handler-case
	       (progn 
		 (restore-metadata database metadata-pathname *metadata-storage-mode*)
		 (incf count))
	       ;; Automatically GC orphaned entity files during proxy initialization.
	       (file-not-found ()
			       (when (equalp (pathname-type pathname) *entity-data-file-type*)
				 (incf orphan-count)
				 (delete-file* pathname :expunge :queue)))))))
    (declare (dynamic-extent #'reload-metadata))
    (let ((pathname (filesystem-database-cache-directory database))
	  (*proxy-cache* (cache-object-cache database)))	;must be bound to current cache for successful reload
      (log-event :normal "Restoring Proxy Cache from ~A...." pathname)
      (map-proxy-cache-pathnames pathname #'reload-metadata :file-type *entity-data-file-type*)
      (unless (zerop orphan-count)
	(expunge-deleted-files)
	(log-event :normal "Deleted ~D orphaned entity file~:P." orphan-count))
      (log-event :normal "Restored ~D cache entr~:@P from ~A." count pathname)
      database)))

(defmethod initialize-proxy-database ((cache proxy-cache))
  (let ((database (cache-database cache)))
    (clrhash (cache-resource-table cache))
    (setf (cache-object-cache database) cache) 
    (initialize-proxy-database database)))

(defmethod gc-orphaned-entities ((database filesystem-database) &aux (count 0))
  (flet ((maybe-gc-entity (pathname)
	   (let ((metadata-pathname (make-pathname :type *metadata-file-type* :version :newest :defaults pathname)))
	     (unless (probe-file metadata-pathname)
	       (delete-file* pathname :expunge :queue)
	       (incf count)))))
    (declare (dynamic-extent #'maybe-gc-entity))
    (let ((pathname (filesystem-database-cache-directory database)))
      (log-event :normal "Proxy Cache GC orphaned entity files in ~A...." pathname)
      (map-proxy-cache-pathnames pathname #'maybe-gc-entity :file-type *entity-data-file-type*)
      (expunge-deleted-files)
      (log-event :normal "Deleted ~D orphaned entity file~:P from ~A." count pathname)
      database)))

(defmethod gc-orphaned-metadata ((database filesystem-database) &aux (count 0))
  (flet ((maybe-gc-metadata (pathname)
	   (let ((entity-pathname (make-pathname :type *entity-data-file-type* :version :newest :defaults pathname)))
	     (unless (probe-file entity-pathname)
	       (delete-file* pathname :expunge :queue)
	       (incf count)))))
    (declare (dynamic-extent #'maybe-gc-metadata))
    (let ((pathname (filesystem-database-cache-directory database)))
      (log-event :normal "Proxy Cache GC orphaned metadata files in ~A...." pathname)
      (map-proxy-cache-pathnames pathname #'maybe-gc-metadata :file-type *metadata-file-type*)
      (expunge-deleted-files)
      (log-event :normal "Deleted ~D orphaned metadata file~:P from ~A." count pathname)
      database)))


#+Genera
(defmethod clean-up-duplicate-file-versions ((database filesystem-database))
  (flet ((clean-up-directory (pathname)
	   (loop for path in (directory (make-pathname :name :wild :type :wild :version :newest :defaults pathname))
		 for type = (pathname-type path)
		 do (cond ((or (string-equal *metadata-file-type* type :end1 #.(length *metadata-file-type*))
			       (string-equal *entity-data-file-type* type :end1 #.(length *entity-data-file-type*)))
			   (clean-up-contiguous-file-versions path 1 nil))
			  (t (clean-up-contiguous-file-versions path 0 nil)))
		 finally (fs:expunge-directory pathname))))
    (let ((pathname (filesystem-database-cache-directory database)))
      (log-event :normal "Proxy Cache Clean Up Cache Files in ~A...." pathname)
      (map-proxy-cache-directory pathname #'clean-up-directory)
      (log-event :normal "Cleaned Up Cache Files in ~A." pathname)
      database)))
