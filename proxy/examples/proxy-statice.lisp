;;;   -*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-
;;;
;;; (c) Copyright  2000, John C. Mallery
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; STATICE BINDING FOR PROXY-CACHE DATABASE
;;;
;;; To configure the proxy to store meta-data in Statice, you only need to:
;;;      1. Load this file
;;;      2. Create a statice data base using the Symbolics CP command
;;;      3. In the namaspace, add to local VLM the user property 
;;;             CL-HTTP-PROXY-STATICE-DATABASE VLM-Name-DBFS:>proxy-metadata.db
;;;      3. Eval the form: (enable-proxy-cache-statice-database t)
;;;      4. Eval the form: (initialize-proxy-statice-db *proxy-statice-database*)
;;;
;; make sure statice is present
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (sct:find-system-named "statice-server" nil t)
    (sct:load-system "statice-runtime" :query nil :silent t)
    (sct:load-system "statice" :query nil :silent t)
    (sct:load-system "statice-server" :query nil :silent t)))

;;;------------------------------------------------------------------- 
;;;
;;; STATICE ABSTRACTIONS
;;;

(defmacro with-database-if ((database) &body body)
  `(if (and statice-model::*current-database* (eq statice-model::*current-database* ,database))
       (progn ,@body)
       (statice:with-database (db ,database)
	 (declare (ignore db))
	 ,@body)))

(define-macro with-transaction ((database) &body body)
  "Sets up the database and transaction context carefully to avoid nesting."
  `(with-database-if (,database)
     (statice:with-transaction-if (not statice-model::*transaction-info*) ()
       ,@body)))

(defun %deleted-entity-p (entity)
  (declare (inline))
  (eq (statice-model::handle-rid entity) :deleted))

(defvar *proxy-statice-database* nil
  "The current statice database pathname.")


;;;------------------------------------------------------------------- 
;;;
;;; SPECIALIZED CLASSES
;;;

;; Does not support file system operations.  May need some special methods. 11/10/2000 -- JCMa.
(defclass proxy-statice-metadata-handle
	  (proxy-database-handle-mixin metadata-handle)
    ((representation :initform nil :initarg :representation :accessor handle-representation)
     (persistent-store :initform nil :initarg :persistent-store :accessor %handle-persistent-metadata-store))
  (:documentation "The metadata handle class for HTTP proxy statice database caches."))

(defclass proxy-statice-entity-handle
	  (proxy-database-handle-mixin filesystem-entity-handle)
    ()
  (:documentation "The entity handle class for HTTP proxy statice database caches."))

(defclass proxy-statice-database
	  (filesystem-database)
    ((metadata-handle-class :initform 'proxy-statice-metadata-handle :allocation :class)
     (entity-handle-class :initform 'proxy-statice-entity-handle :allocation :class)
     (cache :initarg :cache :accessor cache-object-cache)	;so we can find the proxy cache
     (pathname :initform *proxy-statice-database* :accessor proxy-statice-database-pathname))	;The current statice database pathname.
  (:documentation "A proxy database implemented on the Statice and the filesystem."))

(defclass proxy-cache-statice-database
	  (proxy-statice-database)
    ()
  (:documentation "A database for an HTTP cache using Statice to store metadata."))

(defclass persistent-representation
	  (representation)
    ())


;;;------------------------------------------------------------------- 
;;;
;;; STATICE ENTITY DEFINITIONS
;;;

(statice:define-entity-type representation-store
			    ()
  ((uri persistent-uri :unique nil :no-nulls t :inverse-index t :inverse %persistent-uri-representation-stores)
   (pathname fs:pathname :unique t :no-nulls t :inverse-index t :inverse %find-representation-store-by-pathname)
   (plist t))
  (:conc-name %representation-store-)
  (:area cache-area)
  (:documentation "Persistent metadata store for representations that accessed via the persistent metadata handle."))

(statice:define-entity-type persistent-uri
			    ()
  ((string string :unique t :no-nulls t :inverse-index t :inverse %find-persistent-uri)
   (creation-date time:universal-time :no-nulls t))
  (:conc-name %persistent-uri-)
  (:area uri-area)
  (:documentation "A databased URI."))

(statice:define-schema
  proxy-cache-schema 
  (representation-store
    persistent-uri))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

;; Use this to create new statice databases for the proxy.
(define initialize-proxy-statice-db (db-pathname &key rebuild-if-necessary (schema 'proxy-cache-schema))
  "Dangerous internal function to create a fresh statice database."
  (when (or rebuild-if-necessary
	    (setq rebuild-if-necessary
		  (yes-or-no-p "This operation will destroy all the data in ~A.~
                         ~&Are you assuming full responsibility? "
			       db-pathname)))
    (let ((*package* (symbol-package schema)))
      (statice:make-database db-pathname schema
			     :if-exists (if rebuild-if-necessary :create :error)))))

(defun proxy-statice-db-pathname (host)
  (let* ((host (net:parse-host host))
	 (db-pathname (scl:send host :user-get :cl-http-proxy-statice-database)))
    (if db-pathname
	(pathname db-pathname)
	(error "The namespace object for ~S, contains no :proxy-statice-database property.~&~
                Please specify Statice database file system and pathname for the proxy to use on this machine." host))))

(defun enable-proxy-cache-statice-database (&optional (on-p t))
  (cond (on-p
	 (case *proxy-database-class*
	   (proxy-cache-database
	     (setq *representation-class* 'persistent-representation
		   *proxy-database-class* 'proxy-cache-statice-database)))
	 (setq *proxy-statice-database* (proxy-statice-db-pathname (local-host-domain-name))))
	(t (setq *representation-class* 'representation
		 *proxy-database-class* 'proxy-cache-database)))
  on-p)

;; this could be specialized for performance
(defmethod set-dynamic-slots ((representation persistent-representation) init-plist)
  (macrolet ((with-fast-slot-set ((representation keyword value) fast-slots &body default)
	       `(with-slots ,fast-slots ,representation
		  (case ,keyword
		    ,.(loop for slot in fast-slots
			   for key = (intern (symbol-name slot) *keyword-package*)
			   collect `(,key (setq ,slot ,value)))
		    (t ,@default)))))
    (loop for (keyword value) on init-plist by #'cddr
	  do (with-fast-slot-set
	       (representation keyword value)
	       (default-expiration-time entity-invalid-pentity-size expiration-time
		 http-status http-version last-modification last-reference last-update
		 metadata-size must-revalidate-p request-headers response-headers uid
		 valid-p verified-date)
	       ;; report this in mail.
	       (let ((slot (intern (symbol-name keyword) *http-package*)))
		 (setf (slot-value representation slot) value))))))

(declaim (inline %representation-store-uri-string))

(defun %representation-store-uri-string (representation-store)
  (%persistent-uri-string (%representation-store-uri representation-store)))

(defmethod %representation-store-representation-init-plist ((store representation-store))
  (let ((*package* *http-package*)		;correct package
	(*read-base* 10)			;get the base right
	(*read-eval* nil))			;turn off in case dangerous stuff slipped through a header
    (%representation-store-plist store)))

(defmethod %representation-store-representation-init-plist ((handle proxy-statice-metadata-handle))
  (%representation-store-representation-init-plist (%handle-persistent-metadata-store handle)))

;; required to canonicalize pathname so that we find them in statice.
(defun proxy-statice-canonicalize-pathname (pathname)
  (typecase pathname
    (logical-pathname
      (translated-pathname (setf (pathname-version pathname) :newest)))
    (t (setf (pathname-version (typecase pathname
				 (logical-pathname (translated-pathname pathname))
				 (t pathname)))
	     :newest))))

(defmacro with-proxy-statice-canonicalize-pathname ((pathname) &body body)
  `(let ((,pathname (proxy-statice-canonicalize-pathname ,pathname))) . ,body))

(defmethod proxy-statice-database-pathname ((handle proxy-statice-metadata-handle))
  (proxy-statice-database-pathname (representation-database (handle-representation handle))))

(define-condition no-persistent-uri-found
                  (url:unknown-uri)
  ((url-string :initarg :url-string :reader url-string))
  (:report (lambda (condition stream)
             (format stream "No interned object found for the persistent URI, ~S." (url-string condition)))))

(declaim (inline %intern-persistent-uri))

(defun %intern-persistent-uri (string create-p creation-date)
  (declare (values persistent-uri newly-created-p))
  (cond ((%find-persistent-uri string))
	(create-p
	 (values (statice:make-entity 'persistent-uri :string string :creation-date (or creation-date (get-universal-time)))
		 t))
	(t nil)))

(define-generic intern-persistent-uri (database uri-string &key if-does-not-exist creation-date)
  (declare (values persistent-uri newly-created-p))
  (:documentation "Interns URI-STRING as a persistent URI in DATABASE.
CREATION-DATE is a universal time to use when interning a new object.

IF-DOES-NOT-EXIST can be any of:

     :CREATE  - Create a new interned object if one does not already exist.
     :SOFT    - Return the existing object or NIL if none is found.
     :ERROR   - Return the existing object or signal the error 
                HTTP::NO-PERSISTENT-URI-FOUND if none is found."))

(defmethod intern-persistent-uri ((database proxy-statice-database) (name string) &key (if-does-not-exist :soft) creation-date)
  (multiple-value-bind (persistent-uri newly-created-p)
      (with-transaction ((proxy-statice-database-pathname database))
	(%intern-persistent-uri name (eql if-does-not-exist :create) creation-date))
    (cond (persistent-uri (values persistent-uri newly-created-p))
	  ((eql if-does-not-exist :error)
	   (error 'no-persistent-uri-found :uri-string name))
	  (t nil))))

(defmethod intern-persistent-uri ((cache cache-database-mixin) name &key (if-does-not-exist :soft) creation-date)
  (intern-persistent-uri (cache-database cache) name :if-does-not-exist if-does-not-exist) :creation-date creation-date)

(defmethod intern-persistent-uri (database (resource resource) &key (if-does-not-exist :soft) creation-date)
  (intern-persistent-uri database (uri-string resource) :if-does-not-exist if-does-not-exist) :creation-date creation-date)

(defmethod intern-persistent-uri (database (representation representation) &key (if-does-not-exist :soft) creation-date)
  (intern-persistent-uri database (uri-string (representation-resource representation))
			 :if-does-not-exist if-does-not-exist) :creation-date creation-date) 


;;;------------------------------------------------------------------- 
;;;
;;; SPECIALIZE METHODS FOR STATICE PERSISTENCY
;;;

;; Just return the plist and let Statice do the dump forms
(defmethod representation-make-header-plist-load-form ((representation persistent-representation) header-plist)
  header-plist)

;; no-op
(defmethod handle-initialize ((handle proxy-statice-metadata-handle))
  handle)

;; We need a way to compute the size of the representation-store as stored in statice. 11/16/2000 -- JCMa.
(defmethod handle-compute-object-size ((handle proxy-statice-metadata-handle))
  0)

(defmethod handle-object-delete ((handle proxy-statice-metadata-handle))
  (let ((store (%handle-persistent-metadata-store handle)))
    (when store
      (prog1 (with-transaction ((proxy-statice-database-pathname handle))
	       (statice:delete-entity store))
	     (setf (%handle-object-size handle) 0
		   (%handle-persistent-metadata-store handle) nil)))))

(defmethod handle-valid-p ((handle proxy-statice-metadata-handle) &optional verify-p)
  (let ((store (%handle-persistent-metadata-store handle)))
    (cond ((null store) nil)
	  (verify-p
	   (if (%deleted-entity-p store)
	       (setf (%handle-persistent-metadata-store handle) nil)
	       store))
	  (t store))))

(defmethod handle-invalidate ((handle proxy-statice-metadata-handle))
  (let ((store (%handle-persistent-metadata-store handle)))
    (cond ((null store) nil)
	  ((%deleted-entity-p store)
	   (setf (%handle-persistent-metadata-store handle) nil))
	  (t (error "Attempt to invalidate ~S, which points to a valid representation store, ~S." handle store)))))

(defmethod save-metadata-persistently ((database proxy-statice-database) (representation persistent-representation) mode)
  (declare (ignore mode))
  (let* ((handle (representation-metadata-handle representation))
	 (init-plist (persistent-slot-values representation))
	 store)
    (with-transaction ((proxy-statice-database-pathname database))
      (cond ((setq store (%handle-persistent-metadata-store handle))
	     (setf (%representation-store-plist store) init-plist))
	    (t (setq store (statice:make-entity 'representation-store
						:uri (%intern-persistent-uri (uri-string representation) t (getf init-plist :creation-date))
						:pathname (proxy-statice-canonicalize-pathname (representation-entity-pathname representation))
						:plist init-plist))
	       (setf (handle-representation handle) representation
		     (%handle-persistent-metadata-store handle) store))))))

(defmethod restore-metadata ((database proxy-statice-database) (pathname pathname) mode)
  (declare (ignore mode))
  (let (store uri init-plist)
    (with-transaction ((proxy-statice-database-pathname database))
      (cond ((setq store (%find-representation-store-by-pathname pathname))
	     (setq uri (%representation-store-uri-string store)
		   init-plist (%representation-store-representation-init-plist store)))
	    (t (return-from restore-metadata nil))))
    (let* ((cache (cache-object-cache database))
	   (representation (%restore-representation cache database uri nil init-plist))
	   (metadata-handle (representation-metadata-handle representation)))
      (setf (%handle-persistent-metadata-store metadata-handle) store
	    (handle-representation metadata-handle) representation)
      t)))

(defmethod initialize-proxy-database ((database proxy-statice-database) &aux (count 0) (orphan-count 0))
  (flet ((reload-metadata (pathname)
	   (with-proxy-statice-canonicalize-pathname (pathname)
	     (cond ((restore-metadata database pathname nil)
		    (incf count))
		   ;; Automatically GC orphaned entity files during proxy initialization.
		   ((equalp (pathname-type pathname) *entity-data-file-type*)
		    (incf orphan-count)
		    (delete-file pathname))
		   (t nil)))))			;some other file type
    (declare (dynamic-extent #'reload-metadata))
    (let ((cache-pathname (filesystem-database-cache-directory database))
	  (statice-db (proxy-statice-database-pathname database)))
      (log-event :normal "Restoring Proxy Cache from:
                         ~&~5TMetadata Database: ~A~
                         ~&~5TEntity Cache: ~A" statice-db cache-pathname)
      (map-proxy-cache-pathnames cache-pathname #'reload-metadata :file-type *entity-data-file-type*)
      (unless (zerop orphan-count)
	(log-event :normal "Deleted ~D orphaned entity file~:P." orphan-count))
      (log-event :normal "Restored ~D cache entr~:@P from ~A." count cache-pathname)
      database)))

;; need to expunge directory when files deleted. 11/10/2000 -- JCMa.
(defmethod gc-orphaned-entities ((database proxy-statice-database) &aux (count 0))
  (flet ((maybe-gc-entity (pathname)
	   (with-proxy-statice-canonicalize-pathname (pathname)
	     (unless (with-transaction ((proxy-statice-database-pathname database))
		       (%find-representation-store-by-pathname pathname))
	       (delete-file pathname)
	       (incf count)))))
    (declare (dynamic-extent #'maybe-gc-entity))
    (let ((pathname (filesystem-database-cache-directory database)))
      (log-event :normal "Proxy Cache GC orphaned entity files GC in ~A...." pathname)
      (map-proxy-cache-pathnames pathname #'maybe-gc-entity :file-type *entity-data-file-type*)
      (log-event :normal "Deleted ~D orphaned entity file~:P from ~A." count pathname)
      database)))

(defmethod gc-orphaned-metadata ((database proxy-statice-database) &aux (count 0))
  (flet ((%gc-orphaned-metadata (db-pathname)
	   (with-transaction (db-pathname)
	     (statice:for-each ((store representation-store))
	       (let ((pathname (%representation-store-pathname store)))
		 (unless (and pathname
			      (handler-case
				(probe-file pathname)
				(directory-not-found () nil)))
		   (statice:delete-entity store)
		   (incf count)))))))
    (declare (dynamic-extent #'%gc-orphaned-metadata))
    (let ((db-pathname (proxy-statice-database-pathname database)))
      (log-event :normal "Proxy Cache GC orphaned metadata representations in the Statice database ~A...." db-pathname)
      (%gc-orphaned-metadata db-pathname)
      (log-event :normal "Deleted ~D orphaned metadata representation~:P from ~A." count db-pathname)
      database)))

;;;------------------------------------------------------------------- 
;;;
;;; TESTING CODE
;;;

#|

;; make sure statice is present
(unless (sct:find-system-named "statice-server" nil t)
  (sct:load-system "statice-server"))

;; set up a new statice database
(initialize-proxy-statice-db *proxy-statice-database*)

;; enable persistent meta-data
(enable-proxy-cache-statice-database)

(initialize-proxy-cache t)

(defun test ()
  (let ((database (cache-database *proxy-cache*)))
    (flet ((do-it (pathname)
	     (prin1 #\.)
	     (let ((path (proxy-statice-canonicalize-pathname pathname)))
	       (with-transaction ((proxy-statice-database-pathname database))
		 (let ((entity (%find-representation-store-by-pathname path)))
		 (format t "~&~S: ~A" entity (%representation-store-pathname entity)))))))
      (map-proxy-cache-pathnames "http:proxy-cache;" #'do-it :file-type *entity-data-file-type*))))

(defun test ()
  (let* ((cache *proxy-cache*)
	 (database (cache-database cache))
	 (db-pathname (proxy-statice-database-pathname database)))
    (flet ((do-it (rep)
	     (when (representation-valid-p rep)
	       (save-metadata-persistently database rep nil)
	       (let* ((handle (representation-metadata-handle rep))
		      (store (%handle-persistent-metadata-store handle))
		      init-plist)
		 (with-transaction (db-pathname)
		   (setq init-plist (%representation-store-representation-init-plist store)))
		 (format t "~&-----~S-----~&~S" (uri-string rep) init-plist)))))
      (map-representations cache #'do-it nil))))

(defun test ()
  (let* ((cache *proxy-cache*)
	 (database (cache-database cache))
	 (db-pathname (proxy-statice-database-pathname database)))
    (with-transaction (db-pathname)
      (statice:for-each ((persistent-uri persistent-uri))
	(format t "~&~A~&Creation Date: ~\\time\\~&Stores: ~S" (%persistent-uri-string persistent-uri) (%persistent-uri-creation-date persistent-uri)
		(%persistent-uri-representation-stores persistent-uri))))))

|#  