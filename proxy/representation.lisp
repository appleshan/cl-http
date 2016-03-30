;;;   -*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-
;;;
;;; (C) Copyright 1998-2001, 2003, John C. Mallery
;;;     All Rights Reserved.
;;;
;;; (C) Copyright 1996-97, Christopher R. Vincent
;;;     All Rights Reserved.
;;;------------------------------------------------------------------- 
;;;
;;; REPRESENTATION
;;;
(in-package :http)

(defmethod uri-string ((representation representation))
  (uri-string (representation-resource representation)))

(defmethod print-object ((representation representation) stream)
  (print-unreadable-object (representation stream :type t :identity t)
    (when (slot-boundp representation 'resource)
      (with-slots (uri-string) (representation-resource representation)
        (write-string uri-string stream)))))

(defgeneric representation-database (representation)
  (:documentation "Returns the database object where representation is persistently stored."))

(defmethod representation-database ((representation representation))
  (cache-database (cache-object-cache representation)))

(defmethod cache-object-size ((representation representation))
  (with-slots (size metadata-size entity-size) representation 
    (or size
	(setq size (+ (the integer metadata-size) (the integer entity-size))))))

(defmethod decache-object-size :after ((representation representation))
  (decache-object-size (representation-resource representation)))

(defmethod verify-storage-size ((representation representation))
  (with-lock-held ((cache-object-lock representation) :read "Verify Size")
    (if (representation-valid-p representation)
	(let* ((entity-size (verify-storage-size (representation-entity-handle representation)))
	       (mdata-size (verify-storage-size (representation-metadata-handle representation)))
	       (size (+ mdata-size entity-size)))
	  (unless-every
	    ((= entity-size (representation-entity-size representation))
	     (format *trace-output* "~&Bad Entity Size: ~D (~D) ~S" (representation-entity-size representation) entity-size representation))
	    ((= mdata-size (representation-metadata-size representation))
	     (format *trace-output* "~&Bad Metadata Size: ~D (~D) ~S" (representation-metadata-size representation) mdata-size representation))
	    ((= size (cache-object-size representation))
	     (format *trace-output* "~&Bad Representation Size: ~D (~D) ~S" (cache-object-size representation) size representation)))
	  size)
	0)))

;; Updates the position of REPRESENTATION in its cache's recency list at runtime
(defmethod (setf representation-last-reference) :after (universal-time (representation representation-recency-mixin))
  (note-last-reference (cache-object-cache representation) representation universal-time))

;; Inserts REPRESENTATION in its cache's recency list at start up time
(defmethod register-representation :after (resource (representation representation-recency-mixin))
  (declare (ignore resource))
  (cache-recency-insert-entry (cache-object-cache representation) representation))

;; Removes representation from the cache recency list.
(defmethod %representation-delete :before ((representation representation-recency-mixin))
  (cache-recency-remove-entry (cache-object-cache representation) representation))

(defgeneric %representation-delete (representation)
  (:documentation "Low-level primitive for deleting a representation."))

(defmethod %representation-delete ((representation representation))
  (setf (representation-valid-p representation) nil)
  (handle-object-delete (representation-metadata-handle representation))
  (handle-object-delete (representation-entity-handle representation)))

(defgeneric representation-invalid-p (representation)
  (:documentation "Returns non-null if REPRESENTATION holds invalid state."))

(defmethod representation-invalid-p ((representation representation))
  (not (representation-valid-p representation)))

(defgeneric representation-entity-handle-valid-p (representation &optional verify-p)
  (:documentation "Returns non-null if the entity handle for REPRESENTATION points to valid data.
When VERIFY-p is non-null, this Actively verifies the entity handle validity."))

(defmethod representation-entity-handle-valid-p ((representation entity-persistence-mixin) &optional verify-p)
  (handle-valid-p (representation-entity-handle representation) verify-p))

(defmethod cache-object-last-reference-date ((representation entity-transaction-mixin))
  (or (representation-last-reference representation)
      (cache-object-creation-date representation)))


;;;------------------------------------------------------------------- 
;;;
;;; FILESYSTEM PERSISTENCY FOR METADATA
;;;

(defgeneric representation-make-header-plist-load-form (representation header-plist)
  (:documentation "Builds a lisp form that will reload HEADER-PLIST."))

(defmethod representation-make-header-plist-load-form ((entity entity-metadata-mixin) header-plist)
  (labels ((header-make-load-form (thing)
	     (etypecase thing
	       (keyword thing)
	       (symbol `(quote ,thing))
	       (string thing)
	       (number thing)
	       (cons (loop for list = thing then (cdr list)
			   while list
			   when (listp list)
			     collect (header-make-load-form (car list)) into result
			   else
			     collect (header-make-load-form list) into result
			     and return `(list* ,.result)
			   finally (return `(list ,.result))))
	       (standard-object (make-load-form thing))
               (structure-object (make-load-form thing)))))
    `(list ,.(loop for (key value) on header-plist by #'cddr
		   collect key
		   collect (header-make-load-form value)))))

(defgeneric persistent-slot-values (entity)
  (declare (values slot-value-plist))
  (:method-combination nconc)
  (:documentation "Returns a property list of slots and values for persistent storage."))

(defmethod persistent-slot-values nconc ((entity cache-object))
  (with-slots (creation-date size) entity
    `(:creation-date ,creation-date
      :size ,size)))

(defmethod persistent-slot-values nconc ((entity entity-persistence-mixin))
  (with-slots (entity-invalid-p entity-size metadata-size uid valid-p) entity
    (if entity-invalid-p
	`(:entity-size 0 :entity-invalid-p t :metadata-size ,metadata-size :uid ,uid :valid-p ,valid-p)
	`(:entity-size ,entity-size :entity-invalid-p nil :metadata-size ,metadata-size :uid ,uid :valid-p ,valid-p))))

(defmethod persistent-slot-values nconc ((entity entity-metadata-mixin))
  (with-slots (expiration-time last-modification http-status http-version must-revalidate-p request-headers response-headers) entity
    `(:expiration-time ,expiration-time
      :last-modification ,last-modification
      :http-status ,http-status
      :http-version ,http-version
      :must-revalidate-p ,must-revalidate-p
      :request-headers ,(representation-make-header-plist-load-form entity request-headers)
      :response-headers ,(representation-make-header-plist-load-form entity response-headers))))

(defmethod persistent-slot-values nconc ((entity entity-transaction-mixin))
  (with-slots (default-expiration-time last-reference last-update verified-date) entity
    `(:default-expiration-time ,default-expiration-time
      :last-reference ,last-reference
      :last-update ,last-update
      :verified-date ,verified-date)))

(defgeneric %register-representation (resource representation)
  (declare (values representation))
  (:documentation "Pimitive that registers representation with resource with appropriate interlocking."))

(defmethod %register-representation ((resource resource) (representation representation))
  (pushnew representation (resource-representations resource) :key #'representation-uid)
  (decache-object-size resource)
  representation)

(defgeneric register-representation (resource representation)
  (declare (values representation))
  (:documentation "Registers representation with resource with appropriate interlocking."))

(defmethod register-representation ((resource resource) (representation representation))
  (with-lock-held ((cache-object-lock resource) :write "Resource")
    (with-lock-held ((cache-object-lock representation) :write "Representation Lock")
      (%register-representation resource representation))))

(defmethod handle-make-valid ((representation entity-persistence-mixin))
  (handle-make-valid (representation-metadata-handle representation))
  (handle-make-valid (representation-entity-handle representation)))

(defun %restore-representation (cache database uri uid &optional init-args)
  (declare(values representation))
  (let* ((resource (intern-resource cache uri :if-does-not-exist :create))
	 (init-args2 (if uid
			 `(:uid ,uid ,.init-args)
			 init-args))
	 (representation (database-allocate-representation database cache resource init-args2)))
    (declare (dynamic-extent init-args2))
    (handle-make-valid representation)		;because we reload by mapping entity files
    ;; Carefully tell the resource that this representation is back
    (register-representation resource representation)))

(defun restore-representation (uri uid &rest init-args)
  (declare (dynamic-extent init-args)
	   (values representation))
  (let* ((cache *proxy-cache*)
	 (database (cache-database cache)))
    (%restore-representation cache database uri uid init-args)))

#-(or ANSI-CL Draft-ANSI-CL-2)
(defmethod make-load-form ((representation representation))
  `(restore-representation ,(uri-string representation)
			   ,(representation-uid representation)
			   ,.(persistent-slot-values representation)))

#+(or ANSI-CL Draft-ANSI-CL-2)
(defmethod make-load-form ((representation representation) &optional environment)
  (declare (ignore environment))
  `(restore-representation ,(uri-string representation)
			   ,(representation-uid representation)
			   ,.(persistent-slot-values representation)))

(defmethod save-metadata ((representation representation) mode)
  (save-metadata-persistently (representation-database representation) representation mode))

(defmethod save-metadata-persistently ((database filesystem-database) (representation representation) (mode (eql :text)))
  (let* ((*print-pretty* nil)			;turn off unless debugging
	 (reload-form (make-load-form representation))
	 (handle (representation-metadata-handle representation)))
    (declare (dynamic-extent reload-form))
    (with-open-file (file (filesystem-handle-pathname handle) :direction :output :element-type *standard-character-type*
			  :if-does-not-exist :create :if-exists *file-exists-supersede-action*)
      (prin1 reload-form file))
    (setf (filesystem-handle-valid-p handle) t)))

#+Genera
(defmethod save-metadata-persistently ((database filesystem-database) (representation representation) (mode (eql :binary)))
  (let ((reload-forms `(,(make-load-form representation)))
	(handle (representation-metadata-handle representation)))
    (declare (dynamic-extent reload-forms))
    (sys:dump-forms-to-file (filesystem-handle-pathname handle) reload-forms '(:package "HTTP"))
    (setf (filesystem-handle-valid-p handle) t)))

#+LispWorks
(defmethod save-metadata-persistently ((database filesystem-database) (representation representation) (mode (eql :binary)))
  (declare (optimize (speed 3)))
  (let ((reload-forms `(,(make-load-form representation)))
	(handle (representation-metadata-handle representation)))
    (declare (dynamic-extent reload-forms))
    (sys:dump-forms-to-file (filesystem-handle-pathname handle) reload-forms)
    (setf (filesystem-handle-valid-p handle) t)))

(defmethod save-metadata-persistently :around ((database filesystem-database) (representation representation) mode)
  (declare (optimize (speed 3)))
  (tagbody retry
	   (restart-case
               (with-lock-held ((cache-object-lock representation) :read "Save MetaData")
                 (when (representation-valid-p representation)
                   (let ((*package* *http-package*)	;relative to HTTP package
                         (*print-radix* nil)	;prevents writing integers to strings with a trailing decimal point
                         (*print-base* 10)	;always in base 10
                         (*print-escape* t)	;readable strings
                         (*print-readably* t))	;readable output
                     (call-next-method database representation mode))))
	     (retry ()
		    :report (lambda (stream) 
			      (format stream "Retry saving metadata to ~A" (representation-metadata-pathname representation)))
		    (go retry))
	     (skip ()
		   :report (lambda (stream) 
			     (format stream "Skip saving metadata to ~A" (representation-metadata-pathname representation)))
		   (return-from save-metadata-persistently nil))))
  ;; update the size of the metadata
  (let* ((handle (representation-metadata-handle representation))
	 (size (handle-compute-object-size handle)))
    (setf (%handle-object-size handle) size
	  (representation-metadata-size representation) size)
    (decache-object-size representation)
    ;; Clean up excess file versions in Genera
    #+Genera (handle-database-hygine handle)
    #+Genera (handle-database-hygine (representation-entity-handle representation)))
  representation)

(defmethod restore-metadata ((database filesystem-database) (pathname pathname) (mode (eql :text)))
  (let ((*package* *http-package*)		;correct package
	(*read-base* 10)			;get the base right
	(*read-eval* nil))			;turn off in case dangerous stuff slipped through a header
    (load pathname :verbose nil :print nil)
    t))

#+Genera
(defmethod restore-metadata ((database filesystem-database) (pathname pathname) (mode (eql :binary)))
  ;; use of common lisp version with '(cl:unsigned-byte 16) was losing on NFS streams 5/13/2000 -- JCMa.
  (si:with-open-file (stream pathname :characters nil :error t)
    (si:bin-load-file-internal stream *http-package* t))
  t)

#+LispWorks
(defmethod restore-metadata ((database filesystem-database) (pathname pathname) (mode (eql :binary)))
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
    (load stream :verbose nil))
  t)

(defmethod restore-metadata :around ((database filesystem-database) pathname mode)
  (tagbody retry
	   (restart-case
	     (return-from restore-metadata (call-next-method database pathname mode))
	     (skip ()
		   :report (lambda (stream) (format stream "Skip restoring metadata from ~A" pathname))
		   nil)
	     (retry ()
		    :report (lambda (stream) (format stream "Retry restoring metadata from ~A" pathname))
		    (go retry)))))

#|
;; Testing code

(map-representations *proxy-cache* #'(lambda (rep)
				       (print (make-load-form rep))
				       (unless (y-or-n-p "Continue? ") (break "foo"))))

(defun compare-slots (slots1 slots2)
  (flet ((report (x y)
	   (format t "~&NOT EQUAL: ~S ~S" x y)
	   nil))
    (etypecase slots1
      (atom
	(if (equal slots1 slots2)
	    t
	    (report slots1 slots2)))
      (cons (etypecase slots2
	      (atom (report slots1 slots2))
	      (cons (and (compare-slots (car slots1) (car slots2))
			 (compare-slots (cdr slots1) (cdr slots2)))))))))

(defun test-persistence (rep)
  (when (representation-valid-p rep)
    (let* ((resource (representation-resource rep))
	   (database (cache-database (cache-object-cache resource)))
	   (handle (representation-metadata-handle rep))
	   (uid (representation-uid rep))
	   (uri (uri-string rep))
	   (slots (persistent-slot-values rep))
	   (mode :binary #+ignore :text))
      (save-metadata rep mode)
      (setf (resource-representations resource)
	    (delete rep (resource-representations resource)))
      (restore-metadata database handle mode)
      (let* ((new-rep (get-representation resource uid))
	     (new-uri (uri-string new-rep))
	     (new-uid (representation-uid new-rep))
	     (new-slots (persistent-slot-values new-rep)))
	(unless (and (equal uid new-uid)
		     (equal uri new-uri)
		     (equal slots new-slots))
	  (scl:send *terminal-io* :clear-window)
	  (describe rep)
	  (describe new-rep)
	  
	  (when (y-or-n-p "Discrepency Found: Break? ")
	    (break "foo")))))))

(map-representations *proxy-cache* #'test-persistence)

|#


;;;------------------------------------------------------------------- 
;;;
;;; ASYNCHRONOUS META DATA SAVE
;;;

(defmethod note-metadata-update ((representation representation) time-stamp)
  (let ((handle (representation-metadata-handle representation)))
    (enqueue-asynchronous-metadata-save (handle-database handle) representation time-stamp)))

(defmethod save-persistently-unsaved-metadata ((representation representation))
  (when (representation-unsaved-metadata representation)
    (note-metadata-update representation (representation-verified-date representation))
    t))


;;;------------------------------------------------------------------- 
;;;
;;; WRITING ENTITY DATA
;;;

(defmacro with-entity-data-stream ((stream representation direction) &body body)
  "Execute BODY with STREAM bound to a stream into the entity data associated with REPRESENTATION.
DIRECTION is either :INPUT from the entity data or :OUTPUT into the entity data."
  (ecase direction
    (:input
      `(with-open-database (,stream (representation-entity-handle ,representation) :input)
	 ,@body))
    (:output
      `(let* ((rep ,representation)
	      (handle (representation-entity-handle rep)))
	 (prog1 (with-open-database (,stream handle :output)
		  ,@body)
		(setf (representation-entity-size rep) (or (%handle-object-size handle) 0))
		(decache-object-size rep))))))

(defun write-representation-entity (representation stream)
  "Write a entity data from REPRESENTATION to STREAM."
  (with-entity-data-stream (data-stream representation :input)
    (stream-copy-until-eof data-stream stream :binary)))

(defun write-representation-entity-byte-range (representation stream start end)
  "Write a representation of a resource to a stream."
  (with-entity-data-stream (data-stream representation :input)
    (stream-copy-byte-range data-stream stream start end)))

;;;------------------------------------------------------------------- 
;;;
;;; HTTP CACHE OPERATIONS 
;;;

(defmethod write-request-headers ((representation representation) stream &optional termination-line-p
				  modification-plist excluded-headers additional-headers)
  (write-modified-headers (representation-request-headers representation)
			  stream modification-plist excluded-headers termination-line-p additional-headers))

(defmethod write-response-headers ((representation representation) stream &optional termination-line-p
				   modification-plist excluded-headers additional-headers)
  (write-modified-headers (representation-response-headers representation)
			  stream modification-plist excluded-headers termination-line-p additional-headers))

;; these should go sometime   5/5/2000 -- JCMa.
(declaim (inline get-request-header-value))
(defun get-request-header-value (representation header-keyword)
  (get-header-value (representation-request-headers representation) header-keyword))

(declaim (inline get-response-header-value))
(defun get-response-header-value (representation header-keyword)
  (get-header-value (representation-response-headers representation) header-keyword))

(defgeneric representation-origin-server-date (representation)
  (:documentation "Returns the universal time according to the origin server that the resource was retrieved.
If this date header is not available, this defaults to the time at which the resource was last verified."))

(defmethod representation-origin-server-date ((representation representation))
  (or (getf (representation-response-headers representation) :date)
      (representation-verified-date representation)))

(define-generic representation-age (representation &optional current-time)
  (:documentation "Return the current age in seconds of representation"))

(defmethod representation-age (representation &optional (current-time (get-universal-time)))
  (declare (integer current-time))
  (- current-time (representation-verified-date representation)))

(define-generic representation-staleness (representation &optional current-time)
  (:documentation "Returns staleness in seconds if representation has expired, otherwise nil"))

(defmethod representation-staleness ((representation representation) &optional (current-time (get-universal-time)))
  (declare (integer current-time))
  (let ((staleness (- current-time (representation-expiration-time representation))))
    (and (plusp staleness) staleness)))

(define-generic representation-freshness-lifetime (representation)
  (:documentation "Return the freshness lifetime in seconds of representation."))

(defmethod representation-staleness-freshness-lifetime ((representation representation))
  (- (representation-expiration-time representation) (representation-verified-date representation)))

(define-generic compute-default-expiration-time (representation)
  (:documentation "Computes the universal time when representation becomes invalid and requires revalidation."))

(defmethod compute-default-expiration-time ((representation representation))
  (let ((headers (representation-response-headers representation))
	(verified-date (representation-verified-date representation))
	last-modified date age)
    (+ verified-date
       (cond ((and (setq last-modified (getf headers :last-modified))
		   (setq date (getf headers :date))
		   (not (minusp (setq age (- (the integer date) (the integer last-modified))))))
	      (min *proxy-cache-default-expiration-interval* (floor age 10)))	; 10 percent heuristic per HTTP 1.1 spec
	     (t *proxy-cache-default-expiration-interval*)))))

(define-generic compute-expiration-time (representation)
  (declare (values expiration-time default-expiration-time))
  (:documentation "Computes the universal time when representation becomes invalid and requires revalidation."))

(defmethod compute-expiration-time ((rep representation))
  (let ((headers (representation-response-headers rep)))
    (or ;; cache control directives override the expires header in HTTP 1.1
      (unless (member (representation-http-version rep) '(:http/0.9 :http/1.0))
	(multiple-value-bind (directive found-p)
	    (getf headers :cache-control)
	  (when found-p
	    (let ((max-age (or (getf directive :s-maxage)	;S-MAXAGE overrides MAXAGE
			       (getf directive :maxage))))
	      (and max-age (+ (min max-age *proxy-cache-maximum-expiration-time*)
			      (representation-verified-date rep)))))))
      ;; Try for an expires time and try to relativize it to avoid clock synchronization problems.
      (let ((expires (getf headers :expires))
	    date)
	(when expires
	  (cond ((setq date (getf headers :date))
		 (+ (min (- (the integer expires) (the integer date)) *proxy-cache-maximum-expiration-time*)
		    (representation-verified-date rep)))
		((< 0 (- expires (representation-verified-date rep)) *proxy-cache-maximum-expiration-time*))
		(t expires))))
      ;; default to our standard stalness interval.
      (values nil (compute-default-expiration-time rep)))))

(defmethod representation-compute-expiration-time-from-headers ((rep representation))
  (multiple-value-bind (expiration-time default-expiration-time)
      (compute-expiration-time rep)
    (setf (%representation-expiration-time rep) expiration-time
	  (%representation-default-expiration-time rep) default-expiration-time)
    (or expiration-time default-expiration-time)))

(defmethod representation-expiration-time ((rep representation))
  (or (%representation-expiration-time rep)
      (%representation-default-expiration-time rep)
      (representation-compute-expiration-time-from-headers rep)
      (error "Failed to compute an expiration time. This should never happen.")))

(defmethod stale-representation-p ((rep representation))
  (let ((expiration (representation-expiration-time rep))
	(time (or (and (boundp '*server*) (server-request-time *server*))
		  (get-universal-time))))
    (<= expiration time)))

(defgeneric metadata-validates-representation-entity-p (representation response-headers response-http-version)
  (:documentation "Returns non-null if the entity body cached for representation remains valid,
given the fresh headers, RESPONSE-HEADERS, from the origin server and prior headers cached within REPRESENTATION."))

(defmethod metadata-validates-representation-entity-p ((representation representation) (headers header-set) http-version)
  (macrolet ((test-header-value (header-keyword comparator cached-headers authoritative-p)
	       `(multiple-value-bind (value found-p)
		    (get-header ,header-keyword headers)
		  (when found-p
		    (let ((stored-value (getf ,cached-headers ,header-keyword :not-found)))
		      (unless (eq stored-value :not-found)
			,(if authoritative-p
			     `(return-from metadata-validates-representation-entity-p (,comparator value stored-value))
			     `(unless (,comparator value stored-value)
				(return-from metadata-validates-representation-entity-p nil)))))))))
    (let ((response-headers (representation-response-headers representation)))
      (when response-headers
	(case http-version
	  ((:http/1.0 :http/0.9)
	   (test-header-value :content-length equal response-headers nil)	;heuristic
	   (test-header-value :last-modified equal response-headers nil)	;heuristic
	   (test-header-value :content-md5 equalp response-headers t)	;authoritative
	   t)
	  (t (test-header-value :etag entity-tag-equal response-headers t)	;authoritative
	     (test-header-value :content-length equal response-headers nil)	;heuristic
	     (test-header-value :last-modified equal response-headers nil)	;heuristic
	     (test-header-value :content-md5 equalp response-headers t)	;authoritative
	     t))))))

(define-generic proxy-representation-p (representation request-headers http-version))

(defmethod proxy-representation-p ((rep representation) request-headers http-version)
  http-version
  (with-header-values (pragma) request-headers
    (cond (pragma (not (eql pragma :no-cache)))
	  (t t))))

(defun proxy-cacheable-server-response-p (http-status http-version headers)
  "Returns non-null when the origin server HEADERS for HTTP-VERSION indicate that the response is cacheable by a proxy.
Assumes HTTP-STATUS is one of *cacheable-response-status-codes*"
  (declare (optimize (speed 3)))
  (unless (member http-version '(:http/0.9 :http/1.0))
    (multiple-value-bind (directive found-p)
	(get-header :cache-control headers)
      (when found-p
	;; Ignore :PUBLIC t because the default is to cache.
	(loop for (key value) on directive by #'cddr
	      do (case key
		   ((:private :no-cache :no-store)
		    (when value (return-from proxy-cacheable-server-response-p nil)))))
	;; Max age directives take precedence over expires
	(let ((max-age (or (getf directive :s-maxage)	;S-MAXAGE overrides MAXAGE
			   (getf directive :maxage))))
	  (when max-age
	    (return-from proxy-cacheable-server-response-p (< 0 *proxy-cache-minimum-expiration-time* (the integer max-age))))))))
  ;; work with the the expires and last-modified headers
  (with-header-values (date expires last-modified) headers
    ;; if the date header is missing, we could use local proxy time instead,
    ;; but we might just assume that the origin server is a loser, and not
    ;; trust the rest of there headers. If this turns out to bust the cache
    ;; too much, one can always back off the strategy. 5/23/2000 -- JCMa.
    (cond (expires
	   ;; http 1.1 spec 14.9.3 backward compatibility feature for http 1.0
	   (if (zerop expires)
	       nil
             (and date (< 0 *proxy-cache-minimum-expiration-time* (- (the integer expires) (the integer date))))))
	  ;; Don't cache status codes whose default is no-cache
	  ((member http-status *response-status-codes-defaultedly-not-cached*) nil)
	  ;; this heuristic expiration time is suggested by the http 1.1 spec in 13.2.4 5/4/2000 -- JCMa.
	  (last-modified
	   (and date (< 0 *proxy-cache-minimum-expiration-time* (floor (- (the integer date) (the integer last-modified)) 10))))
	  (t t))))

(defun proxy-cacheable-client-request-p (http-version headers)
  "Returns non-null when the client HEADERS for HTTP-VERSION indicate that the response is cacheable by a proxy."
  (case http-version
    ((:http/0.9 :http/1.0) t)
    (t (multiple-value-bind (directive found-p)
	   (get-header :cache-control headers)
	 (cond ((and found-p (getf directive :no-store)) nil)
	       (t t))))))

(defun pragma-no-cache-p (request-headers)
  "Returns non-null if the pragma header contains the no-cache directive."
  (multiple-value-bind (directive found-p) 
      (get-header :pragma request-headers)
    (and found-p
	 (etypecase directive
	   (atom (eq :no-cache directive))
	   (cons (member :no-cache directive))))))

(define proxy-revalidate-cache-for-client-p (representation client-version &optional (time (get-universal-time)))
  "Returns non-null if a CACHE-CONTROL header contains the no cache directive."
  (or (case client-version
	((:http/0.9 :http/1.0)
	 #+ignore (proxy-trace "~&PROXY-REVALIDATE: Expiration: ~\\time\\ Current: ~\\time\\"
			       (representation-expiration-time representation) time)
	 (< (representation-expiration-time representation) time))	; Standard freshness check.
	;; equivalent to previous netscape lossage for http 1.1
	(t (multiple-value-bind (directive found-p)
	       (get-header :cache-control)
	     (if found-p
		 ;; check cache control parameters
		 (cond ;; forced revalidation by client. :NO-CACHE can force a partial revalidation but we don't implement it.
		   ((getf directive :no-cache) t)
		   ;; Cache expired?
		   ((< (representation-expiration-time representation) time)
		    (let ((max-stale (getf directive :max-stale)))
		      ;; Cache cannot be any staler than MAX-STALE.
		      (if (and max-stale (< (- time (representation-expiration-time representation)) max-stale))
			  nil
			  t)))
		   ;; Cache must be more recent than MAX-AGE
		   ((let ((max-age (getf directive :max-age)))
		      (and max-age
			   (or (zerop max-age)
			       (> max-age (- time (representation-verified-date representation))))))
		    t)
		   ;; Cache must retain at least as much freshness as MIN-FRESH.
		   ((let ((min-fresh (getf directive :min-fresh)))
		      (and min-fresh
			   (< min-fresh (- (representation-expiration-time representation) time))))
		    t)
		   (t nil))
		 ;; Standard freshness check.
		 (< (representation-expiration-time representation) time)))))
      ;; HTTP 1.1 clients can send pragma too.
      (pragma-no-cache-p *headers*)))

(defgeneric representation-update-response-headers (representation response-headers status)
  (:documentation "Merges any missing headers from old-headers into new-headers when recording server response headers for the proxy cache."))

(defmethod representation-update-response-headers ((representation representation) (response-headers header-set) status)
  (let ((old-headers (representation-response-headers representation))
	(exclude-headers (case status ;;Don't update key entity headers on conditional get for robustness against broken proxies or servers
			   (304 '#.(list* :content-length :content-type *hop-by-hop-headers*))
			   (t nil))))
    (flet ((update-header1 (header header-object)
	     (when (cacheable-response-header-p header)
	       (setf (getf old-headers header) (header-value header-object))))
	   (update-header2 (header header-object)
	     (unless (member header exclude-headers)
	       (setf (getf old-headers header) (header-value header-object)))))
      (declare (dynamic-extent #'update-header1 #'update-header2))
      (map-headers (if exclude-headers #'update-header2 #'update-header1) response-headers)
      (setf (representation-response-headers representation) old-headers))))

(defgeneric representation-note-transaction (representation http-status http-version verified-date &optional request-headers)
  (declare (values representation))
  (:documentation "Updates the metadata for cached representation whenever a new request is cached or validated."))

(defmethod representation-note-transaction ((representation representation) http-status http-version verified-date
					    &optional request-headers)
  (declare (optimize (speed 3)))
  (check-type verified-date integer)
  (flet ((must-revalidate-p (http-version request-headers response-headers)
	   (cond ((or (getf response-headers :set-cookie)	;always revalidate for cookies
		      (get-header :cookie request-headers))
		  (return-from must-revalidate-p t))
		 (t (with-header-values (authorization) request-headers
		      (unless (member http-version '(:http/0.9 :http/1.0))
			(multiple-value-bind (directive found-p)
			    (getf response-headers :cache-control)
			  (when found-p
			    (loop for (key value) on directive by #'cddr
				  when (and (member key '(:must-revalidate :proxy-revalidate)) value)
                                  do (return-from must-revalidate-p t))
			    (when authorization	;HTTP 1.1 spec 14.8
			      (let ((s-maxage (getf directive :s-maxage)))
				(return-from must-revalidate-p
				  (cond (s-maxage
					 (if (zerop s-maxage) t nil))
					((or (getf directive :public)
					     (not (getf directive :must-revalidate :not-found)))
					 nil)
					(t t))))))))
		      ;; revalidate any requests with AUTHORIATION, cookie
		      (not (null authorization)))))))
    (declare (inline must-revalidate-p))
    (let ((response-headers (representation-response-headers representation)))
      (when request-headers
	(setf (representation-request-headers representation) (proxy-cacheable-request-header-plist request-headers *proxy-cacheable-request-headers*)
	      ;; assume that revalidation doesn't change when no request headers.
	      (representation-must-revalidate-p representation) (must-revalidate-p http-version request-headers response-headers)))
      (setf (representation-verified-date representation) verified-date
	    (representation-etag representation) (getf response-headers :etag)
	    (representation-last-modification representation) (getf response-headers :last-modified)
	    (representation-http-version representation) http-version)
      (case http-status
	(304 ;; don't set status to 304, but be smart when the first mention is a 304
             (unless (slot-boundp representation 'http-status)
               (setf (representation-http-status representation) 200)))
	(t (setf (representation-http-status representation) http-status))))
    ;; Reset expiration times
    (representation-compute-expiration-time-from-headers representation)
    representation))				;return the representation

(defgeneric representation-revalidate-if-expired (representation &optional current-time)
  (declare (values revalidated-p))
  (:documentation "Revalidates representation if it would be expired at CURRENT-TIME."))

(defmethod representation-revalidate-if-expired ((representation representation) &optional (current-time (get-universal-time)))
  (when (cache-object-expired-p representation current-time)
    (handler-case-if (not *debug-proxy*)
       (progn (proxy-revalidate-representation representation)
	      t)
      (network-error () nil)
      (http-condition (cond)
		      (when *trace-proxy*
			(report-condition cond *trace-output*)
			nil))
      (error (err) (bug-report-error err) nil))))
