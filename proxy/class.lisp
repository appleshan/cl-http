;;;   -*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-
;;;
;;; (c) Copyright  2000, John C. Mallery
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; CLASS DEFINITIONS FOR PROXY
;;;

(in-package :http)


;;;------------------------------------------------------------------- 
;;;
;;; HANDLE CLASSES
;;;

(defclass database-handle ()
    ((database :initarg :database :accessor handle-database)
     (uid :initarg :uid :accessor handle-uid)
     (size :initform nil :initarg :size :accessor %handle-object-size))
  (:documentation "A unique identifier that can be used to access a database entry."))

(defclass entity-handle (database-handle)
    ()
  (:documentation "A unique identifier that accesses entity data."))

(defclass metadata-handle (database-handle)
    ()
  (:documentation "A unique identifier that accesses metadata."))

(defclass filesystem-handle
	  (database-handle)
    ((type :reader handle-type :allocation :class)
     (pathname :initarg :pathname :accessor filesystem-handle-pathname)
     (valid-p :initform nil :initarg valid-p :accessor filesystem-handle-valid-p))
  (:documentation "Handle for a filesystem-database."))

(defclass filesystem-metadata-handle
	  (filesystem-handle metadata-handle)
    ((type :initform *metadata-file-type* :allocation :class))
  (:documentation "Metadata handle for a filesystem-database."))

(defclass filesystem-entity-handle
	  (filesystem-handle entity-handle)
    ((type :initform *entity-data-file-type* :allocation :class))
  (:documentation "Entity handle for a filesystem-database."))

(defclass proxy-database-handle-mixin () ()
  (:documentation "A mixin on proxy handles where methods independent of database should be defined."))

(defclass proxy-metadata-handle
	  (proxy-database-handle-mixin filesystem-metadata-handle)
    ()
  (:documentation "The metadata handle class for HTTP proxy database caches."))

(defclass proxy-entity-handle
	  (proxy-database-handle-mixin filesystem-entity-handle)
    ()
  (:documentation "The entity handle class for HTTP proxy database caches."))


;;;------------------------------------------------------------------- 
;;;
;;; DATABASE CLASSES
;;;

(defclass cache-database-mixin
	  ()
    ((database :initarg :database :accessor cache-database)))

#+ecl
(defclass basic-cache-mixin ()())

(defclass database (basic-cache-mixin)
    ((entity-handle-class :initform 'entity-handle :reader entity-handle-class :allocation :class)
     (metadata-handle-class :initform 'metadata-handle :reader metadata-handle-class :allocation :class)
     (name :initform nil :accessor database-name))
  (:documentation "A generic database."))

(defclass basic-asynchronous-database-mixin
	  (tq:task-queue)
    ()
  (:documentation "A mixin that provides asynchronous database save operations."))

(defclass filesystem-database
	  (basic-asynchronous-database-mixin database)
    ((metadata-handle-class :initform 'filesystem-metadata-handle :allocation :class)
     (entity-handle-class :initform 'filesystem-entity-handle :allocation :class)
     (cache-directory :initform (pathname "http:proxy-cache;") :initarg :cache-directory :accessor filesystem-database-cache-directory)
     (storage-size :initform 0 :initarg :storage-size :reader database-storage-size))
  (:documentation "A database implemented on the filesystem."))

(defclass proxy-cache-database
	  (filesystem-database)
    ((metadata-handle-class :initform 'proxy-metadata-handle :allocation :class)
     (entity-handle-class :initform 'proxy-entity-handle :allocation :class))
  (:documentation "A database for an HTTP cache."))


;;;------------------------------------------------------------------- 
;;;
;;; CACHE CLASSES
;;;

(defclass basic-gc-mixin
	  ()
    ((maximum-number-of-objects :initarg :maximum-number-of-objects :accessor cache-maximum-number-of-objects :type fixnum)
     (maximum-size :initarg :maximum-size :accessor cache-maximum-size :type integer)
     (gc-method :initform :reference-time :initarg :gc-method :accessor cache-gc-method)))

(defclass cache
	  (basic-gc-mixin cache-database-mixin)
    ((name :initform nil :initarg :name :accessor cache-name)
     (creation-time :initarg :creation-time :accessor cache-creation-time)
     (description :initarg :description :accessor cache-description))
  (:documentation "Basic cache."))

(defclass cache-reference-recency-mixin
	  ()
    ((recency :initarg :recency :accessor cache-recency))
  (:documentation "A mixin that provides an ordered list of representations in increasing order by reference time."))

(defclass incremental-cache-gc
	  (tq:background-task)
    ((cache :initarg :cache :accessor cache-gc-cache)
     (space-threshold :initform 0 :initarg :space-threshold :accessor cache-gc-space-threshold :type integer)
     (resource-threshold :initform 0 :initarg :resource-threshold :accessor cache-gc-resource-threshold :type fixnum)))

(defclass proxy-cache-incremental-gc-mixin
	  ()
    ((incremental-gc :initform nil :initarg :incremental-gc :reader proxy-cache-incremental-gc)
     (incremental-gc-method :initform :reference-time :initarg :incremental-gc-method :accessor proxy-cache-incremental-gc-method))
  (:documentation "A mixin that provides a background incremental GC for a proxy cache."))

(defclass proxy-cache
	  (proxy-cache-incremental-gc-mixin cache-reference-recency-mixin cache)
    ((resource-table :initarg :resource-table :accessor cache-resource-table)
     (resource-table-lock :initarg :resource-table-lock :accessor cache-resource-table-lock))
  (:documentation "HTTP Proxy cache."))


;;;------------------------------------------------------------------- 
;;;
;;; CACHE OBJECT CLASSES
;;;

(defclass basic-cache-mixin () ((cache :initarg :cache :accessor cache-object-cache)))

(defclass cache-object (basic-cache-mixin)
    ((lock :initarg :lock :accessor cache-object-lock)
     (creation-date :initarg :creation-date :accessor cache-object-creation-date :type (or null integer))	;cache creation time
     (size :initform 0 :initarg :size :accessor cache-object-size :type (or null integer)))
  (:documentation "Basic cache object."))

(defclass resource (cache-object)
  ((uri-string :initarg :uri-string :accessor uri-string)
   (representations  :initform nil :initarg :representations :accessor resource-representations)
   (vary :initform nil :initarg :vary :accessor resource-vary))
  (:documentation "A cached HTTP resource."))

(defclass entity-persistence-mixin
	  ()
    ((entity-handle :initarg :entity-handle :accessor representation-entity-handle)	;entity-handle handle
     (entity-size :initform 0 :initarg :entity-size :accessor representation-entity-size :type integer)	;size in bytes of the entity
     (metadata-handle :initarg :metadata-handle :accessor representation-metadata-handle)	;metadata handle
     (metadata-size :initform 0 :initarg :metadata-size :accessor representation-metadata-size :type integer)	;size in bytes of the metadata
     (uid :initarg :uid :accessor representation-uid)	;persistent unique ID
     (valid-p :initform nil :initarg :valid-p :accessor representation-valid-p)	;whether the representation contains valid data
     (entity-invalid-p :initform t :initarg :entity-invalid-p  :accessor representation-entity-invalid-p))
  (:documentation "A mixin that supplies a cached entity with perisistent storage."))

(defclass entity-metadata-mixin
	  ()
    ((etag :initform nil :initarg :etag :accessor representation-etag)	;entity tag HTTP1.1
     (expiration-time :initform nil :initarg :expiration-time	;expiration time specified by origin server
		      :accessor %representation-expiration-time :type (or null integer))
     (last-modification :initform nil :initarg :last-modification
			:accessor representation-last-modification :type (or null integer))
     (http-status :initarg :http-status :accessor representation-http-status)
     (http-version :initarg :http-version :accessor representation-http-version)
     (must-revalidate-p :initform nil :initarg :must-revalidate-p :accessor representation-must-revalidate-p)
     (request-headers :initform nil :initarg :request-headers :accessor representation-request-headers)
     (response-headers :initform nil :initarg :response-headers :accessor representation-response-headers))
  (:documentation "A mixin that supplies a cached entity with slots recording its metadata."))

(defclass representation-recency-mixin
	  ()
    ((recency-pointer :initform nil :accessor representation-recency-pointer))
  (:documentation "A mixin that allows the cache to track representations by reference time."))

(defclass entity-transaction-mixin
	  (representation-recency-mixin)
    ((default-expiration-time :initform nil :initarg :default-expiration-time :accessor %representation-default-expiration-time
			      :type (or null integer))
     (last-reference :initform nil :initarg :last-reference 	;most recent cache reference
		     :accessor representation-last-reference :type (or null integer))
     (last-update :initform nil :initarg :last-update	;last time the entity data was updated.
		  :accessor representation-last-update :type (or null integer))
     (verified-date :initform nil :initarg :verified-date :accessor representation-verified-date
		    :type (or null integer))	;latest contact w/ origin server
     (unsaved-metadata :initform nil :initarg :unsaved-metadata :accessor representation-unsaved-metadata))	;null or last reference time if unsaved
  (:documentation "A mixin that supplies a cached entity with slots recording transactions with the origin server."))

(defclass representation
	  (entity-transaction-mixin entity-metadata-mixin entity-persistence-mixin cache-object)
    ((resource :initarg :resource :accessor representation-resource))
  (:documentation "A cached representations of an HTTP resource."))


;;;------------------------------------------------------------------- 
;;;
;;; PROXY LOG CLASSES
;;;

(defclass proxy-client-log-callback-mixin
	  ()
    ((message-tag  :reader proxy-log-message-tag :allocation :class)
     (server-logs :initform nil :initarg :server-logs :accessor proxy-server-logs))
  (:documentation "This mixin provides a client request log with the ability to deliver callback data to server logs."))

(defclass proxy-log-client-callback-mixin
	  ()
    ((message-tag :reader proxy-log-message-tag :allocation :class)
     (client-log-class :reader proxy-client-log-class :allocation :class)
     (client-log :initform nil :initarg :client-log :accessor proxy-client-log))
  (:documentation "Provides server logs with the ability to receive callback data from client logs."))

(defclass proxy-extended-common-file-log-client-callback
	  (client-request-log proxy-client-log-callback-mixin)
    ((message-tag :initform :proxy-extended-client-data :allocation :class))
  (:documentation "This log class report client data back to the proxy server for logging."))

(defclass proxy-extended-common-file-format-mixin
          ()
    ()
  (:documentation "This log class records accesses according to the Extended Proxy Common File Format."))

(defclass proxy-extended-common-file-log
          (proxy-extended-common-file-format-mixin proxy-log-client-callback-mixin
						   exclusive-proxy-log-mixin compressed-file-logging-mixin
						   access-log basic-file-logging-mixin)
    ((log-file-name :initform "Ext-Common-Proxy-Log" :initarg :log-file-name :allocation :class)
     (message-tag :initform :proxy-extended-client-data :allocation :class)
     (client-log-class :initform 'proxy-extended-common-file-log-client-callback :allocation :class))
  (:documentation "This log class records proxy accesses according to an extended Common File Format without logging server requests."))
