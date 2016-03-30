;;;   -*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-
;;; (C) Copyright 1998-2001, John C. Mallery
;;;     All Rights Reserved.
;;; (C) Copyright 1996-97, Christopher R. Vincent
;;;     All Rights Reserved.

(in-package :http)

;;;------------------------------------------------------------------- 
;;;
;;; RESOURCES
;;;

(defmethod print-object ((resource resource) stream)
  (with-slots (uri-string) resource
    (print-unreadable-object (resource stream :type t :identity t)
      (when (slot-boundp resource 'uri-string)
        (write-string uri-string stream)))))

(declaim (inline cache-allocate-resource))

(defun cache-allocate-resource (cache uri creation-date)
  "Create an instance of a resource."
  (fast-make-instance *resource-class* (resource)
    :cache cache
    :uri-string uri
    :creation-date creation-date))

(declaim (inline %intern-resource))

(defun %intern-resource (cache uri &optional (if-does-not-exist :error))
  (declare (values resource newly-interned-p))
  (let ((table (cache-resource-table cache))
	(table-lock (cache-resource-table-lock cache)))
    (or (with-lock-held (table-lock :read "Cache")
	  (gethash uri table))
	(ecase if-does-not-exist
	  (:soft nil)
	  (:create
	    (with-lock-held (table-lock :write "Cache")
	      (cond ((gethash uri table))
		    (t (let ((resource (cache-allocate-resource cache uri (server-request-time *server*))))
			 (setf (gethash uri table) resource)
			 (values resource t))))))
	  (:error (error "~S does not denote an cached HTTP resource in ~S." uri (cache-name cache)))))))

(defgeneric intern-resource (cache resource &key if-does-not-exist)
  (declare (values resource newly-interned-p))
  (:documentation "Interns RESOURCE as a cached HTTP resource.
RESOURCE can be a URI string, a URL, or a resource"))

(defmethod intern-resource ((cache proxy-cache) (uri string) &key (if-does-not-exist :error))
  (%intern-resource cache uri if-does-not-exist))

(defmethod intern-resource (cache (url url) &key (if-does-not-exist :error))
  (intern-resource cache (coerce-url-string url) :if-does-not-exist if-does-not-exist))

(defmethod intern-resource ((cache proxy-cache) (resource resource) &key (if-does-not-exist :error))
  (cond ((eq cache (cache-object-cache resource)) resource)
	(t (intern-resource cache (uri-string resource) :if-does-not-exist if-does-not-exist))))

(defmethod intern-resource ((cache string) uri &key (if-does-not-exist :error))
  (intern-resource (intern-cache cache :if-does-not-exist if-does-not-exist) uri :if-does-not-exist if-does-not-exist))

(defmethod intern-resource ((server proxy-server-mixin) uri &key (if-does-not-exist :error))
  (intern-resource (server-proxy-cache server) uri :if-does-not-exist if-does-not-exist))

(defmethod intern-resource ((server proxy-server-mixin) (uri string) &key (if-does-not-exist :error))
  (%intern-resource (server-proxy-cache server) uri if-does-not-exist))

(defmethod server-proxy-cache ((server proxy-server-mixin) &optional (error-p t))
  (cond (*proxy-cache*)
	(error-p (error "Proxy cache not initialized for HTTP port ~D." (server-host-local-port server)))
	(t nil)))

(declaim (inline %unintern-resource))

(defun %unintern-resource (cache resource)
  "Primitive that uninterns the cached resource, RESOURCE, from its CACHE."
  (remhash (uri-string resource) (cache-resource-table cache)))

(defmethod unintern-resource ((resource resource))
  "Uninterns the cached resource, RESOURCE, from its CACHE."
  (let ((cache (cache-object-cache resource)))
    (with-lock-held ((cache-resource-table-lock cache) :write "Cache")
      (with-lock-held ((cache-object-lock resource) :write "Resource")
	(%unintern-resource cache resource)))))

(defmethod map-representations ((resource resource) function  &optional (mode :read))
  (if mode
      (with-lock-held ((cache-object-lock resource) mode "Map Representations")
	(dolist (representation (resource-representations resource))
	  (funcall function representation)))
      (dolist (representation (resource-representations resource))
	(funcall function representation))))

(defgeneric %remove-representation (resource representation)
  (declare (values empty-resource-p))
  (:documentation "Primitive for removing a representation from a resource without locking."))

(defmethod %remove-representation ((resource resource) (representation representation))
  (prog1 (setf (resource-representations resource) (delete representation (resource-representations resource) :test #'eq))
	 (with-lock-held ((cache-object-lock representation) :write "Representation")
	   (%representation-delete representation))))

(defmethod remove-representation ((resource resource) (representation representation))
  (with-lock-held ((cache-object-lock resource) :write "Resource")
    (%remove-representation resource representation)
    (decache-object-size resource)))

(defmethod remove-cache-object ((cache proxy-cache) (resource resource))
  (with-lock-held ((cache-object-lock resource) :write "Resource")
    (loop for representation in (resource-representations resource)
	  doing (with-lock-held ((cache-object-lock representation) :write "Representation")
		  (%representation-delete representation))))
  (unintern-resource resource))

(defmethod %remove-cache-object ((cache proxy-cache) (resource resource))
  "Primitive that removes resource from cache without grabbing the resource table lock."
  (with-lock-held ((cache-object-lock resource) :write "Resource")
    (loop for representation in (resource-representations resource)
	  doing (%representation-delete representation))
    (%unintern-resource cache resource)))

;;  "Total size, including all the representations."
(defmethod cache-object-size ((resource resource))
  (with-slots (size) resource
    (or size
	(setq size (loop for rep in (resource-representations resource)
			 sum (cache-object-size rep))))))

(defgeneric decache-object-size (cache-object)
  (:documentation "Decaches the size of object so that it will be recomputed."))

(defmethod decache-object-size ((object cache-object))
  (with-slots (size) object
    (setq size nil)))

(defmethod verify-storage-size ((resource resource))
  (with-lock-held ((cache-object-lock resource) :read "Verify Size")
    (let ((size (loop for rep in (resource-representations resource)
		      sum (verify-storage-size rep))))
      (unless (= size (cache-object-size resource))
	(format *trace-output* "~&Bad Resource Size: ~D (~D) ~S" (cache-object-size resource) size resource))
      size)))

(defmethod cache-object-last-reference-date ((resource resource))
  (or (loop for rep in (resource-representations resource)
	    maximize (cache-object-last-reference-date rep))
      (cache-object-creation-date resource)))


;;;------------------------------------------------------------------- 
;;;
;;; CREATION AND DESTRUCTION OF REPRESENTATIONS
;;;

(defgeneric variant-representation-matches-request-p (representation request-headers vary-header-keywords)
  (:documentation "Returns non-null if REPRESENTATION's response headers match REQUEST-HEADERS 
for all values indicated by VARY-HEADER-KEYWORDS."))

(defmethod variant-representation-matches-request-p ((representation representation) (request-headers header-set) vary-header-keywords)
  (loop with response-headers = (representation-response-headers representation)
	for header-keyword in vary-header-keywords
	unless (let ((variant-value (getf response-headers header-keyword :not-found)))
		 (case variant-value
		   (:not-found nil)
		   (t (multiple-value-bind (request-value request-value-found-p)
			  (get-header header-keyword request-headers)
			(and request-value-found-p (equalp variant-value request-value))))))
	  return nil
	finally (return t)))

(defmethod variant-representation-matches-request-p ((representation representation) (request-headers null)  vary-header-keywords)
   (declare (ignore vary-header-keywords))
   nil)

(define-generic intern-representation (resource request-header-set &key if-does-not-exist)
  (declare (values representation newly-created-p))
  (:documentation "Interns a cached HTTP representation of RESOURCE based on any variant constraints matched against REQUEST-HEADERS."))

(defmethod intern-representation ((resource resource) request-headers &key (if-does-not-exist :error))
  (declare (optimize (speed 3)))
  (flet ((%get-representation (resource request-headers)
	   "Find a representation of resource that is the same variant implied by request headers in request-headers"
	   (let ((representations (resource-representations resource)))
	     (when representations
	       (let ((vary (resource-vary resource)))
		 (cond ((eq vary :*) nil)
		       (vary
			(loop for variant in representations
			      when (variant-representation-matches-request-p variant request-headers vary)
                              return variant))
		       (t (car representations)))))))
	 (%create-representation (resource)
	   (let* ((cache (cache-object-cache resource))
		  (database (cache-database cache))
		  (init-args `(:creation-date ,(server-request-time *server*)))
		  (representation (database-allocate-representation database cache resource init-args)))
	     (declare (dynamic-extent init-args))
	     ;; Carefully tell the resource that this representation is back
	     (%register-representation resource representation))))
    (declare (inline %get-representation %create-representation))
    (let ((lock (cache-object-lock resource)))
      (or (with-lock-held (lock :read "Resource")
	    (%get-representation resource request-headers))
	  (ecase if-does-not-exist
	    (:soft nil)
	    (:create
             (with-lock-held (lock :write "Resource")
               (or (%get-representation resource request-headers)
                   (values (%create-representation resource) t))))
	    (:error (error (format nil "~S does not denoted a matching representation cached by ~S." 
				   request-headers (uri-string resource)))))))))

(define-generic get-representation  (resource uid &optional errorp)
  (:documentation "Find a representation using its unique uid, use for documentation, GC, etc."))

(defmethod get-representation ((resource resource) (uid integer) &optional (errorp t))
  (loop for variant in (resource-representations resource)
	when (= uid (representation-uid variant))
	  return variant
	finally (when errorp
		  (error "There is no matching representation cached for ~S."
			 (uri-string resource)))))
  
(define-generic unintern-representation (representation)
  (:documentation "Unintern a representation of a cached resource."))

(defmethod unintern-representation ((representation representation))
  (let* ((resource (representation-resource representation))
	 (resource-lock (cache-object-lock resource)))
    (unless (with-lock-held ((cache-object-lock resource) :write "Resource")
	      (prog1 (%remove-representation resource representation)
		     (decache-object-size resource)))
      ;; Carefully grab locks in correct order
      (let ((cache (cache-object-cache resource)))
	(with-lock-held ((cache-resource-table-lock cache) :write "Cache")
	  (with-lock-held (resource-lock :write "Resource")
	    (unless (resource-representations resource)
	      (%unintern-resource cache resource))))))))

(defmethod remove-representation ((cache proxy-cache) (representation representation))
  (unintern-representation representation))