;;;   -*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-
;;;
;;; (C) Copyright 1998-2001, 2006, John C. Mallery
;;;     All Rights Reserved.
;;; (C) Copyright 1996-97, Christopher R. Vincent
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; CACHE SUBSTRATE
;;;

(in-package :http)


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(defvar *cache-table* (make-hash-table :test #'equalp)
  "Holds all known cache-objects.")

(defmethod register-cache ((cache cache))
  (declare (values cache))
  (setf (gethash (cache-name cache) *cache-table*) cache))

(defmethod unregister-cache ((cache cache))
  (declare (values removed-p))
  (remhash (cache-name cache) *cache-table*))

(declaim (inline get-cache))

(defun get-cache (name &optional (error-p t))
  (cond ((gethash name *cache-table*))
	(error-p (error "The  cache, ~S, is unknown." name))
	(t nil)))

(define intern-cache (cache &key (if-does-not-exist :error) (class *proxy-cache-class*))
  "Interns a  cache."
  (etypecase cache
    (string
      (or (gethash cache *cache-table*)
	  (ecase if-does-not-exist
	    (:soft nil)
	    (:create (values (make-instance class :name cache)
			     t))
	    (:error (error "The  cache, ~S, is unknown." cache)))))
    (cache cache)))

(defgeneric unintern-cache (cache)
  (declare (values removed-p))
  (:documentation "Uninterns a cache."))

(defmethod unintern-cache ((cache cache))
  (unregister-cache cache))

(defmethod generate-cache-name ((cache cache))
  (concatenate 'string (string-capitalize (type-of cache)) "-" (write-integer-to-string (atomic-incf *cache-counter*) 10)))

(defmethod initialize-instance :after ((cache cache) &key)
  (unless (cache-name cache)
    (setf (cache-name cache) (generate-cache-name cache)))
  (register-cache cache))  

(defmethod print-object ((cache cache) stream)
  (with-slots (name) cache
    (print-unreadable-object (cache stream :type t :identity t)
      (when (slot-boundp cache 'name)
        (write-string name stream)))))

(defmethod initialize-instance :after ((cache-object cache-object) &key)
  (setf (cache-object-lock cache-object) (make-lock (symbol-name (type-of cache-object))
						    :type :multiple-reader-single-writer))
  cache-object)

(defgeneric remove-cache-object (cache resource)
   (:documentation "Removes resource from CACHE."))

(defgeneric map-cache (cache function &optional mode)
  (:documentation "Call FUNCTION on all the cache-objects in CACHE.
Mode is either :READ or :WRITE, depending on whether cache-objects
will be added or deleted by FUNCTION."))

(defgeneric cache-size (cache)
  (:documentation "Returns the total size of a cache."))

; Specialize this for efficiency.
(defmethod cache-size ((cache cache) &aux (total 0))
  (flet ((accum (object)
           (incf total (cache-object-size object))))
    (map-cache cache #'accum :read)
    total))

(defgeneric clear-cache (cache)
  (:documentation "Remove all objects from CACHE."))

(defmethod clear-cache ((cache cache))
  (flet ((lose-it (object)
	   (remove-cache-object cache object)))
    (declare (dynamic-extent #'lose-it)) 
    (map-cache cache #'lose-it :write)))

(defgeneric expunge-cache (cache space)
  (:documentation "Clear SPACE bytes of data from the cache."))

; Specialize this for more intelligent replacement.
(defmethod expunge-cache ((cache cache) (space integer) &aux (removed nil))
  (flet ((do-it (cache-object)
           (decf space (cache-object-size cache-object))
           (remove-cache-object cache cache-object)
           (push cache-object removed)
           (if (<= space 0)
             (return-from expunge-cache removed))))
    (declare (inline do-it))
    (map-cache cache #'do-it :write)
    removed))

(defgeneric cache-last-reference (cache)
  (:documentation "Returns the universal when the cache was last referenced."))

(defgeneric uri-string (cache-object)
  (:documentation "Returns the URI string for CACHE-OBJECT."))

(defgeneric cache-kill-all-processes (cache)
  (:method-combination progn)
  (:documentation "Kills all processes associated with CACHE."))

(defgeneric cache-object-last-reference-date (cache-object)
  (declare (values universal-time))
  (:documentation "Returns the universal time when CACHE-OBJECT was last referenced."))


;;;------------------------------------------------------------------- 
;;;
;;; REPERESENTATIONS BY REFERENCE TIME
;;;

(defstruct (recency
	     (:print-function print-recency))
  (lock nil :read-only t)
  (pointer nil)
  (list nil))

(declaim (inline recency-last-reference))

(defun recency-last-reference (recency)
  (car (recency-list recency)))

(defun print-recency (recency stream depth)
  (declare (ignore depth))
  (print-unreadable-object (recency stream :type t :identity t)
    (let ((last-reference (recency-last-reference recency)))
      (when last-reference
	(write-standard-time last-reference stream t)))))

(defmethod cache-last-reference ((cache cache-reference-recency-mixin))
  (let* ((recency (cache-recency cache))
	 (lock (recency-lock recency)))
    (or (with-lock-held (lock :read "Last Reference")
	  (recency-last-reference recency))
	(cache-creation-time cache))))

(defmethod initialize-instance :after ((cache cache-reference-recency-mixin) &key)
  (setf (cache-recency cache) (make-recency :lock (make-lock "Recency Lock" :type :multiple-reader-single-writer)
					    :list (list (cache-creation-time cache))
					    :pointer nil)))

(declaim (inline %recency-splice-out-representation))

(defun %recency-splice-out-representation (recency representation update-recency-pointer-p)
  (declare (values removed-p))
  (flet ((signal-misaligned-representation-recency-pointer (representation)
	   (destructuring-bind (&optional previous current next) (representation-recency-pointer representation)
	     (error "Proxy Cache: Inconsistent recency pointer in ~S.~
                 ~&Representation: ~S~&Current: ~S~&Next: ~S~&Previous: ~S" 
		    (cache-object-cache representation) representation current previous next))))
    (let ((old-ptr (representation-recency-pointer representation)))
      (when old-ptr ;; Splice out the old entry
        (let* ((entry (cdr old-ptr))
               (next-cdr (cdr entry)))
          (unless (eq representation (car entry))
            (signal-misaligned-representation-recency-pointer representation))
          (tagbody
           (with-lock-held ((recency-lock recency) :write "Last Reference")
             (cond (next-cdr ;; Update the next in line to his new superior
                             (setf (representation-recency-pointer (car next-cdr)) old-ptr))
                   ;; Otherwise it's the last representation
                   (update-recency-pointer-p
                    (let ((ptr (recency-pointer recency))
                          (latest-representation (car old-ptr)))
                      (if (eq ptr (cdr old-ptr))
                          (setf (recency-pointer recency) old-ptr
                                (car (recency-list recency)) (or (representation-last-reference latest-representation)
                                                                 (cache-object-creation-date representation)
                                                                 (go no-last-reference-time)))
                        (go signal-inconsistent-toplevel-recency)))))
             ;; After the pointer comparison, finalize pointers
             (setf (cdr old-ptr) next-cdr			;splice out middle cons
                   (representation-recency-pointer representation) nil	; Flush old pointer for safety & GC
                   (cdr entry) nil)				;set middle cons cdr to NIL for safety
             (return-from %recency-splice-out-representation entry))	;return middle cons
           ;; signal errors outside the preemption context
           signal-inconsistent-toplevel-recency
           (error "Proxy Cache: Inconsistent recency pointer in ~S.~&Top-level recency pointer does not point to ~S."
                  (cache-object-cache representation) representation)
           no-last-reference-time
           (error "Proxy Cache: Can't determine reference time for now latest representation, ~S" representation)))))))

(defgeneric note-last-reference (cache representation universal-time)
  (:documentation "Updates REPRESENTATION's position in the CACHE reference recency list based on UNIVERSAL-TIME."))

;; This is blazingly fast at runtime.
(defmethod note-last-reference ((cache cache-reference-recency-mixin) (representation representation-recency-mixin) universal-time)
  (let ((recency (cache-recency cache))
	ptr)
    (with-lock-held ((recency-lock recency) :write "Last Reference")
      (if (eq representation (car (setq ptr (recency-pointer recency))))	;optimize for repeated access case
	  (setf (car (recency-list recency)) universal-time)	;Update the last entry time
        (let ((entry (or (%recency-splice-out-representation recency representation nil)	;recyle cons when present
                         (list representation))))
          (with-lock-held ((cache-object-lock representation) :write "Last Reference")
            ;; Push the lastest referenced representation onto the back of the list
            (if ptr
                ;; Normal case
                (setf (cdr ptr) entry
                      (representation-recency-pointer representation) ptr)
              ;; First entry case
              (setf (cdr (recency-list recency)) entry
                    (representation-recency-pointer representation) (recency-list recency)))
            ;; Make the pointer point at the last entry
            (setf (recency-pointer recency) entry)
            ;; Keep track of the last time in the list
            (setf (car (recency-list recency)) universal-time)))))))

(defgeneric cache-recency-remove-entry (cache representation)
  (declare (values removed-p))
  (:documentation "Removes REPRESENTATION from CACHE's recency list."))

(defmethod cache-recency-remove-entry ((cache cache-reference-recency-mixin) (representation representation-recency-mixin))
  (let ((recency (cache-recency cache)))
    (with-lock-held ((recency-lock recency) :write "Last Reference")
      (not (null (%recency-splice-out-representation recency representation t))))))

(defgeneric cache-recency-insert-entry (cache representation)
  (:documentation "Inserts REPRESENTATION into CACHE's recency list while maintaining temporal order."))

;; This does linear search and needs some enhancements for large scale to
;; index the structure with enough granularity to make reloading the proxy
;; tolerable.   5/10/2000 -- JCMa.
(defmethod cache-recency-insert-entry ((cache cache-reference-recency-mixin) (representation representation-recency-mixin))
  (let ((recency (cache-recency cache))
	(last-reference (representation-last-reference representation)))
    (unless last-reference
      (error "No last reference for ~S, which is not allowed." representation))
    (with-lock-held ((recency-lock recency) :write "Last Reference")
      (let ((recency-list (recency-list recency))
	    (entry (or (%recency-splice-out-representation recency representation nil)
		       (list representation))))
	;; Place most recent representations directly at the end of the list
	(cond ((<= (car recency-list) last-reference)
	       (let ((ptr (recency-pointer recency)))
		 ;; Update list
		 (with-lock-held ((cache-object-lock representation) :write "Last Reference")
		   (if ptr
		       (setf (cdr ptr) entry
			     (representation-recency-pointer representation) ptr)
                     (setf (cdr recency-list) entry
                           (representation-recency-pointer representation) recency-list))
		   ;; Update pointer
		   (setf (recency-pointer recency) entry)
		   ;; Keep track of the last time in the list
		   (setf (car recency-list) last-reference))))
	      ;; Otherwise linear scan down the list to the insertion position
	      (t (loop for prev = recency-list then scan
		       for scan = (cdr prev)
		       while (and scan (< (representation-last-reference (car scan)) last-reference))
		       finally (with-lock-held ((cache-object-lock representation) :write "Last Reference")
				 (setf (cdr entry) scan		;point to subsequent
				       (cdr prev) entry		;Previous point to entry
				       (representation-recency-pointer representation) prev)	;remember it
				 (if scan ;; Update the next in line to his new superior
				     (setf (representation-recency-pointer (car scan)) entry)
                                   ;; We're at the end so update the highest time entry
                                   (setf (recency-pointer recency) entry	;update pointer
                                         (car recency-list) last-reference))))))))))	;update last time in the list

(defgeneric cache-recency-map-representations (cache function)
  (:documentation "Maps over representations in CACHE in ascending order of reference time.
FUNCTION is called on each successive representation."))

;; dolist loses the next cdr on some implementations (e.g. LispWorks) when performing reference-time
;; proxy GC beause they don't keep the pointer to the next cons outside the body. -- JCMa 5/29/2001.
(defmethod cache-recency-map-representations ((cache cache-reference-recency-mixin) function)
  (let* ((recency (cache-recency cache))
	 (representations (cdr (recency-list recency))))
    (when representations
      (loop for (rep . next) = representations then next
	    do (funcall function rep)
	    while next))))

(defgeneric cache-recency-map-resources (cache function)
  (:documentation "Maps over resources in CACHE in ascending order of reference time.
FUNCTION is called on each successive resource."))

;; dolist loses the next cdr on some implementations (e.g. LispWorks) when performing reference-time
;; proxy GC beause they don't keep the pointer to the next cons outside the body. -- JCMa 5/29/2001.
;; This can call function on resource multiple times when a resource has multiple representations.
(defmethod cache-recency-map-resources ((cache cache-reference-recency-mixin) function)
  (let* ((recency (cache-recency cache))
	 (representations (cdr (recency-list recency))))
    (when representations
      (loop for (rep . next) = representations then next
	    do (funcall function (representation-resource rep))
	    while next))))

(defgeneric cache-verify-recency (cache)
  (:documentation "Verifies the last reference recency of cache.
It ensures that representations appear only once and that their pointers are correct."))

(defmethod cache-verify-recency ((cache cache-reference-recency-mixin))
  (let ((recency (cache-recency cache))
	(win-p t))
    (with-lock-held ((recency-lock recency) :read "Verify Recency") 
      (let ((table (make-hash-table :test #'eq :size (resources-count cache))))
	(declare (dynamic-extent table))
	(clrhash table)
	(loop with last-reference = (car (recency-list recency))
	      and time-hysterisis = (floor *proxy-server-life-time* 1000)
	      and misaligned-prts = 0 and misaligned-dates = 0 and duplicates = 0
	      for previous = (recency-list recency) then current
	      for current = (cdr previous) then (cdr current)
	      for count upfrom 0
	      while current
	      for (representation) = current
	      for pointer = (representation-recency-pointer representation)
	      for idx upfrom 0
	      do (unless (eq previous pointer)
		   (format *trace-output* "~&~D: Misaligned Pointer ~D: index ~D (~:[un~;sound~]) ~S"
			   count (incf misaligned-prts) idx (eq representation (second pointer)) representation)
		   (setq win-p nil))
		 (etypecase (car previous)
		   (representation
		     (unless (<= (representation-last-reference (car previous))
				 (+ (representation-last-reference representation) time-hysterisis))
		       (fast-format *trace-output* "~&~D: Misaligned Date ~D: index ~D ~I (should later than ~I) ~S"
				    count
				    (incf misaligned-dates) idx
				    (write-standard-time (representation-last-reference representation) *trace-output*)
				    (write-standard-time (representation-last-reference (car previous)) *trace-output*)
				    representation)
		       (setq win-p nil)))
		   (integer))			;universal time
		 (multiple-value-bind (entry found-p)
		     (gethash representation table)
		   (cond (found-p
			  (format *trace-output* "~&~D: Duplicate Entry ~D: index ~D (~{~D~^, ~}) ~S"
				  count (incf duplicates) idx (cdr entry) representation)
			  (push idx (cdr entry))
			  (setq win-p nil))
			 (t (setf (gethash representation table) `(,representation ,idx)))))
	      finally (unless-every
			((= last-reference (representation-last-reference (car previous)))
			 (fast-format *trace-output* "~&~D: Bad Last Reference: ~I (~I)"
				      count (write-standard-time last-reference *trace-output*)
				      (write-standard-time (representation-last-reference (car previous)) *trace-output*))
			 (setq win-p nil))
			((eq (recency-pointer recency) previous)
			 (fast-format *trace-output* "~&~D: Bad Recency Pointer: should be ~S but is ~S"
				      count previous (recency-pointer recency))
			 (setq win-p nil))))))
    win-p))

(defgeneric cache-nth-earliest-referenced-representation (cache n)
  (:documentation "Returns the representation with the Nth earliest reference time.
This is 0 based and performs linear search upfrom 0 to find the result."))

(defmethod cache-nth-earliest-referenced-representation ((cache cache-reference-recency-mixin) n)
  (let ((recency (cache-recency cache)))
    (with-lock-held ((recency-lock recency) :read "Last Reference")
      (nth n (cdr (recency-list recency))))))

(defgeneric cache-earliest-reference-time (cache)
  (:documentation "Returns the eariler reference time for a cached representation."))

(defmethod cache-earliest-reference-time ((cache cache-reference-recency-mixin))
  (let ((earliest-rep (cache-nth-earliest-referenced-representation cache 0)))
    (if earliest-rep
	(representation-last-reference earliest-rep)
	(cache-creation-time cache))))

(defgeneric cache-rebuild-recency (cache)
  (declare (values rebuilt-recency))
  (:documentation "Rebuilds the CACHE recency datastructure from the resource table."))

(defmethod cache-rebuild-recency ((cache cache-reference-recency-mixin) &aux representations invalid-representations recency)
  (flet ((collect (rep)
	   (if (representation-valid-p rep)
	       (push rep representations)
	       (push rep invalid-representations)))
	 (lose-invalid-representation (rep)
	   (setf (representation-recency-pointer rep) nil)	;now meaningless, avoid error
	   (unintern-representation rep)))    
    (declare (dynamic-extent #'collect))
    (with-lock-held ((cache-resource-table-lock cache) :write "Rebuild Recency")
      (map-representations cache #'collect nil)
      ;; update a consistent set of representations
      (setq recency (cache-recency cache))
      (with-lock-held ((recency-lock recency) :write "Verify Recency") 
	(let ((new-recency (list* nil (sort representations #'<= :key #'representation-last-reference))))
	  (loop for previous = new-recency then current
		for current = (cdr previous) then (cdr current)
		while current
		for (representation) = current
		do (setf (representation-recency-pointer representation) previous)
		finally (setf (car new-recency) (representation-last-reference representation)
			      (recency-list recency) new-recency
			      (recency-pointer recency) previous)))))
    ;; lose any invalid representations found along the way
    (mapc #'lose-invalid-representation invalid-representations)
    recency))


;;;------------------------------------------------------------------- 
;;;
;;; PROXY-CACHE
;;;

(defgeneric map-resources (proxy-cache function &optional mode)
  (:documentation "Maps FUNCTION over PROXY-CACHE.
FUNCTION accepts two args, URI-STRING and RESOURCE.
MODE is either :READ or :WRITE. A mode of :WRITE is required
when resources are added or deleted by FUNCTION."))

(defmethod map-resources ((cache proxy-cache) function &optional (mode :read))
  (if mode
      (with-lock-held ((cache-resource-table-lock cache) mode "Map Resources")
	(maphash function (cache-resource-table cache)))
      (maphash function (cache-resource-table cache))))

(defgeneric resources-count (proxy-cache)
  (:documentation "Returns the number of HTTP resources in CACHE."))

(defmethod resources-count ((cache proxy-cache))
  (hash-table-count (cache-resource-table cache)))

;; return a useful number to avoid losing in a GC wait function.
(defmethod resources-count ((cache null)) 0)

(defgeneric map-representations (proxy-cache-or-resource function &optional mode)
  (:documentation "Maps FUNCTION over the representations in PROXY-CACHE-OR-RESOURCE
FUNCTION accepts one argument, REPRESENTATION.
mode is either :read or :write, depending on whether resources will be deleted or not."))

(defmethod map-representations ((cache proxy-cache) function &optional (mode :read))
  (flet ((fctn (uri-string resource)
	   (declare (ignore uri-string))
	   (dolist (representation (resource-representations resource))
	     (funcall function representation))))
    (declare (dynamic-extent #'fctn))
    (map-resources cache #'fctn mode)))

(defmethod clear-cache ((cache proxy-cache))
  (flet ((%clear-resource (uri res)
	   (declare (ignore uri))
	   (%remove-cache-object cache res)))
    (map-resources cache #'%clear-resource :write)))

(defmethod map-cache ((cache proxy-cache) (function function) &optional (mode :read))
  (flet ((do-it (uri res)
           (declare (ignore uri))
           (loop for rep in (resource-representations res)
                 doing (funcall function rep))
           (funcall function res)))
    (declare (inline do-it))
    (map-resources cache #'do-it mode)))


;;;------------------------------------------------------------------- 
;;;
;;; INITIALIZATION
;;;

(defun make-proxy-cache (&key database description (maximum-size *proxy-cache-maximum-size*)
			      (maximum-number-of-objects *proxy-cache-maximum-resources*)
			      initial-size name)
  "Create an instance of an HTTP cache."
  ;; We're not likely to perform well, if at all, when the hashtable must grow
  ;; above the fixnum limit.
  (when (< most-positive-fixnum maximum-number-of-objects)
    (error "The proxy cache is not design to handle more than ~D resources." most-positive-fixnum))
  (fast-make-instance *proxy-cache-class* (proxy-cache)
    :name name
    :creation-time (get-universal-time)
    :database (or database (make-proxy-cache-database))
    :maximum-size maximum-size
    :maximum-number-of-objects maximum-number-of-objects
    :description (or description "An HTTP resource cache.")
    :resource-table (if initial-size
			(make-hash-table :test #'equal :size initial-size)
			(make-hash-table :test #'equal))
    :resource-table-lock (make-lock "Cache Resource Table" :type :multiple-reader-single-writer))) 

(defun proxy-cache-default-directory ()
   "Returns the default pathname to use as the cache directory."
   (pathname "http:proxy-cache;"))

(declaim (ftype (function (&optional pathname) t) make-proxy-cache-database))

(defun initialize-proxy-cache (&optional redo-p)
  "The primary means to initialize the proxy cache, including restoring data from persistent storage."
  (cond ((and (not redo-p) *proxy-cache*))
	(t (log-event :normal "Initializing Proxy Cache....")
	   (let* ((database (make-proxy-cache-database (proxy-cache-default-directory)))
		  (cache (make-proxy-cache :database database)))
	     (initialize-proxy-database cache)
	     (log-event :normal "Proxy Cache Initialized.")
	     (cache-start-incremental-gc cache)
	     (setq *proxy-cache* cache)))))

(define set-standard-http-proxy-port (&optional (port *standard-proxy-port*))
  "Primary method for setting the standard port for HTTP proxy connections."
  (check-type port integer)
  (setq *standard-proxy-port* port))

(define enable-proxy-service (&optional (port *standard-proxy-port*))
  "Enables proxy service on a running server.
PORT is either a port number or a list of port numbers."
  (flet ((enable-proxy-on-port (port)
	   (check-type port integer)
	   (pushnew port *proxy-service*) ;allow enable-http-service to detect proxy on this port
	   (enable-http-service :on-ports port)
           (multiple-value-bind (host retricted-p)
               (port-address port)
             (log-event :normal "~:[~;~:*~A ~]Proxy Service Enabled on port ~D ~:[~;exclusively ~]for ~A." (port-protocol port) port retricted-p host))))
    (etypecase port
      (atom
       (enable-proxy-on-port port))
      (cons
       (dolist (item port)
         (enable-proxy-on-port item))))))

(define disable-proxy-service (&optional (port :all))
  "Disables proxy service on a running server.
PORT is either a port number, a list of port numbers, or :ALL."
  (flet ((disable-proxy-on-port (port)
	   (check-type port integer)
	   (disable-http-service :on-ports port) 
           ;;allow disable-http-service to detect proxy on this port
	   (setq *proxy-service* (delete port *proxy-service*))
	   (log-event :normal "~:[~;~:*~A ~]Proxy Service Disabled on port ~D for ~A." (port-protocol port) port (port-address port))))
    (etypecase port
      (integer
       (disable-proxy-on-port port))
      (cons
       (dolist (item port)
         (disable-proxy-on-port item)))
      (symbol
       (ecase port
         (:all
          (let ((ports *proxy-service*))
            (when ports
              (dolist (port ports)
                (disable-proxy-on-port port))
              (setq *proxy-service* nil)))))))))

(define initialize-proxy (&optional redo-p)
  "Initializes proxy service but does not enable it.
Called on server initialization list."
  (expose-log-window)
  (initialize-client-substrate redo-p)
  (initialize-proxy-cache redo-p))

(add-initialization
  "Initialize Proxy"
  '(initialize-proxy)
  '(:normal)
  '*server-launch-initialization-list*)


;;;------------------------------------------------------------------- 
;;;
;;; PROXY USER ACCESS CONTROL
;;;

(define-variable *proxy-port-access-control-alist* nil
		 "Holds an alist mapping proxy ports to access controls.")

(declaim (inline proxy-access-control))

(defun proxy-access-control (port)
  "Returns the access control for the proxy operating on PORT."
  (cdr (assoc port *proxy-port-access-control-alist* :test #'eql)))

(defgeneric set-proxy-access-control (port access-control)
  (:documentation "Sets user-based access control for the proxy running on port, PORT, to use ACCESS-CONTROL.
ACCESS-CONTROL is either a named access control or NIL. When NIL, user-based access control
is turned off on PORT. When it is a specification suitable for interning the realm and
access control, the syntax for ACCESS-CONTROL is:

     ((REALM-NAME &REST REALM-ARGS) ACCESS-CONTROL-NAME &REST ACCESS-CONTROL-ARGS)

Where REALM-ARGS are arguments to INTERN-REALM and ACCESS-CONTROL-ARGS are arguments
to INTERN-ACCESS-CONTROL, excluding the realm."))

(defmethod set-proxy-access-control ((port integer) (access-control access-control))
  (let ((entry (assoc port *proxy-port-access-control-alist* :test #'eql)))
    (if entry
	(setf (cdr entry) access-control)
	(push (list* port access-control) *proxy-port-access-control-alist*)))
  access-control)

(defmethod set-proxy-access-control ((port integer) (access-control null))
  (setq *proxy-port-access-control-alist* (delete port *proxy-port-access-control-alist* :test #'eql :key #'car))
  nil)

(defmethod set-proxy-access-control (port (access-control-spec cons))
  (destructuring-bind ((realm-name &rest realm-args) access-control-name &rest access-control-args)
      access-control-spec
    (declare (dynamic-extent realm-args access-control-args))
    (let* ((realm-object (apply #'intern-realm realm-name realm-args))
	   (access-control-object (apply #'intern-access-control realm-object access-control-name access-control-args)))
      (set-proxy-access-control port access-control-object))))

(defgeneric realm-get-client-proxy-credentials (realm server)
  (:documentation "Returns the client proxy authentication credentials the current request, 
given the authentication REALM for the requested URL and SERVER."))

(defmethod realm-get-client-proxy-credentials ((realm realm-http-authentication-mixin) (server server-authentication-mixin))
  (get-header :proxy-authorization (server-headers server)))

#+CL-HTTP-SSL-CLIENT
(defmethod realm-get-proxy-client-credentials ((realm realm-certificate-authentication-mixin) (server server-ssl-mixin))
  (declare (ignore realm))
  (server-ssl-client-credentials server))

(define-macro with-proxy-authentication-access-control ((proxy-access-control method &key rejection-form (server '*server*)) &body body)
  "Executes REJECTION-FORM whenever CREDENTIALS does not qualify for capabilities under PROXY-ACCESS-CONTROL.
Otherwise executes BODY."
  `(let (.access-control.)
     (cond ((setq .access-control. ,proxy-access-control)
	    (let ((realm (access-control-realm .access-control.))
		  credentials user)
	      (handler-case
		(cond ((and (setq credentials (realm-get-client-proxy-credentials realm ,server)) 
			    (setq user (authenticate-user realm credentials ,method :proxy-access))
			    (progn (setf (server-user-object ,server) user)
                              (allow-user-access-p .access-control. user ,method)))
		       ,@body)
		      (t ,rejection-form))
		(unknown-authentication-method () ,rejection-form))))
	   (t ,@body))))


;;;------------------------------------------------------------------- 
;;;
;;; VERIFYING STORAGE SIZE
;;;

(defgeneric verify-storage-size (cache-object)
  (declare (values actual-size))
  (:documentation "Verifies that CACHE-OBJECT holds consistent storage sizes.
This compares the actual physical storage used by CACHE-OBJECT to its in-memory values.
This locks up a number of things while it runs.  For best results, proxy service should be
turned off."))

(defmethod verify-storage-size ((cache proxy-cache) &aux (size 0))
  (flet ((test (uri resource)
	   (declare (ignore uri))
	   (incf size (verify-storage-size resource))))
    (with-lock-held ((cache-resource-table-lock cache) :read "Verify Size")
      (map-resources cache #'test nil)
      (unless (= size (database-storage-size (cache-database cache)))
	(format *trace-output* "~&Bad Database Size: ~D (~D) ~S" (database-storage-size (cache-database cache)) size cache))
      size)))


;;;------------------------------------------------------------------- 
;;;
;;; GC PREDICATES
;;;
(defgeneric cache-object-expiration-time (cached-object)
  (:documentation "Returns the expiration time for RESOURCE using origin-server or local-cache criteria."))

(defmethod cache-object-expiration-time ((resource resource))
  (loop for rep in (resource-representations resource)
	minimize (representation-expiration-time rep)))

(defmethod cache-object-expiration-time ((representation representation))
  (representation-expiration-time representation))

(defgeneric cache-object-expired-p (cached-object &optional current-time)
  (:documentation "Returns non-null when RESOURCE has expired."))

(defmethod cache-object-expired-p ((resource resource) &optional (current-time (get-universal-time)))
  (< (cache-object-expiration-time resource) current-time))

(defmethod cache-object-expired-p ((representation representation) &optional (current-time (get-universal-time)))
  (< (cache-object-expiration-time representation) current-time))

(defgeneric last-reference-after-p (resource universal-time)
  (:documentation "Returns non-null if RESOURCE has been referenced after UNIVERSAL-TIME."))

(defmethod last-reference-after-p ((resource resource) universal-time)
  (loop for representation in (resource-representations resource)
	when (last-reference-after-p representation universal-time)
	  return t
	finally (return nil)))

(defmethod last-reference-after-p ((representation representation) universal-time)
  (< universal-time (representation-last-reference representation)))


;;;------------------------------------------------------------------- 
;;;
;;; GC SWEEP
;;;

(defgeneric sweep-cache-resources-for-garbage (proxy-cache gc-predicate &key enumeration
							   reclaim-bytes reclaim-objects)
  (declare (values resources-decached bytes-reclaimed))
  (:documentation "Maps proxy-cache and applies GC-PREDICATE to every RESOURCE.
ENUMERATION is either :OLDEST-REFERENCE or :RANDOM. When enumeration is :RANDOM,
the resource lock is grabbed in write mode, and consequently, HTTP threads wishing
to intern or unintern a resource or representation will block.
GC-PREDICATE is called with (RESOURCE) to deterime whether a resource should be GCed.
When RECLAIM-BYTES and/or RECLAIM-OBJECTS are supplied, garbage collection stops once
these limits are reached."))

(defmethod sweep-cache-resources-for-garbage ((cache proxy-cache) gc-predicate &key (enumeration :oldest-reference)
					      reclaim-bytes reclaim-objects
					      &aux (bytes-reclaimed 0) (resources-decached 0) fctn resource-deleter)
  (declare (integer bytes-reclaimed resources-decached))
  (labels ((conditional-gc (resource)
	     (when (funcall gc-predicate resource)
	       (let ((size (cache-object-size resource)))
		 (funcall resource-deleter cache resource)
		 (incf resources-decached)
		 (incf bytes-reclaimed size)
		 t)))
	   (bounded-gc (resource)
	     (when (conditional-gc resource)
	       (when (or (and reclaim-bytes (>= bytes-reclaimed reclaim-bytes))
			 (and reclaim-objects (= resources-decached reclaim-objects)))
		 (return-from sweep-cache-resources-for-garbage (values resources-decached bytes-reclaimed)))))
	   (map-variant (uri resource)
	     (declare (ignore uri))
	     (funcall fctn resource)))
    (declare (dynamic-extent #'conditional-gc #'bounded-gc #'map-variant))
    ;; preamble
    (cond ((or reclaim-bytes reclaim-objects)
	   (unless (or (and reclaim-bytes (< 0 reclaim-bytes))
		       (and reclaim-objects (< 0 reclaim-objects)))
	     (return-from sweep-cache-resources-for-garbage (values 0 0)))
	   (setq fctn #'bounded-gc))
	  (t (setq fctn #'conditional-gc)))
    ;; Apply the sweep method
    (ecase enumeration
      (:oldest-reference
	(setq resource-deleter #'remove-cache-object)	;delete grabbing table lock
	(cache-recency-map-resources cache fctn))
      (:random
	(setq resource-deleter #'%remove-cache-object)	;delete without grabbing table lock
	(map-resources cache #'map-variant :write)))
    ;; report the results
    (values resources-decached bytes-reclaimed)))

(defgeneric sweep-cache-representations-for-garbage (proxy-cache gc-predicate &key enumeration
							   reclaim-bytes reclaim-objects)
  (declare (values representations-decached resources-decached bytes-reclaimed))
  (:documentation "Maps PROXY-CACHE and applies GC-PREDICATE to every REPRESENTATION.
ENUMERATION is either :OLDEST-REFERENCE or :RANDOM.
GC-PREDICATE is called with (REPRESENTATION) to deterime whether a representation should be GCed.
When RECLAIM-BYTES and/or RECLAIM-OBJECTS are supplied, garbage collection stops once
these limits are reached."))

(defmethod sweep-cache-representations-for-garbage ((cache proxy-cache) gc-predicate &key (enumeration :oldest-reference)
						    reclaim-bytes reclaim-objects
						    &aux (bytes-reclaimed 0) (representations-decached 0) (resources-decached 0) fctn
						    resource-deleter)
  (declare (integer bytes-reclaimed representations-decached resources-decached))
  (labels ((delete-resource-without-lock (cache resource)
	     (%unintern-resource cache resource))
	   (delete-resource-with-lock (cache resource)
	     (with-lock-held ((cache-resource-table-lock cache) :write "Cache")
	       (with-lock-held ((cache-object-lock resource) :write "Resource")
		 (%unintern-resource cache resource))))
	   (conditional-gc (resource)
	     (loop with representations-decached-p
		   for representation in (resource-representations resource)
		   do (when (funcall gc-predicate representation)
			(let ((size (cache-object-size representation)))
			  (remove-representation resource representation)
			  (incf representations-decached)
			  (incf bytes-reclaimed size)
			  (setq representations-decached-p t)))
		   finally (unless (resource-representations resource)
			     (with-lock-held ((cache-object-lock resource) :write "Resource")
			       (unless (resource-representations resource)
				 (funcall resource-deleter cache resource)
				 (incf resources-decached))))
			   (return representations-decached-p)))
	   (bounded-gc (resource)
	     (when (conditional-gc resource)
	       (when (or (and reclaim-bytes (>= bytes-reclaimed reclaim-bytes))
			 (and reclaim-objects (= resources-decached reclaim-objects)))
		 (return-from sweep-cache-representations-for-garbage (values representations-decached resources-decached bytes-reclaimed)))))
	   (map-variant (uri resource)
	     (declare (ignore uri))
	     (funcall fctn resource)))
    (declare (dynamic-extent #'conditional-gc #'bounded-gc #'map-variant))
    ;; preamble
    (cond ((or reclaim-bytes reclaim-objects)
	   (unless (or (and reclaim-bytes (< 0 reclaim-bytes))
		       (and reclaim-objects (< 0 reclaim-objects)))
	     (return-from sweep-cache-representations-for-garbage (values 0 0 0)))
	   (setq fctn #'bounded-gc))
	  (t (setq fctn #'conditional-gc)))
    ;; Apply the sweep method
    (ecase enumeration
      (:oldest-reference
	(setq resource-deleter #'delete-resource-with-lock)
	(cache-recency-map-resources cache fctn))
      (:random
	(setq resource-deleter #'delete-resource-without-lock)
	(map-resources cache #'map-variant :write)))
    ;; report the results
    (values representations-decached resources-decached bytes-reclaimed)))

(defgeneric cache-garbage-collection-thresholds (cache free-space-ratio)
  (declare (values space-threshold resource-threshold))
  (:documentation "Returns thresholds that trip garbage collection for CACHE, given FREE-SPACE-RATIO."))

(defmethod cache-garbage-collection-thresholds ((cache proxy-cache) free-space-ratio)
  (declare (float free-space-ratio))
  (let ((space-threshold (floor (the float (* (the float (- 1 free-space-ratio))
					      (the integer (cache-maximum-size cache))))))
	(resource-threshold (floor (the float (* (the float (- 1 free-space-ratio))
						 (the integer (cache-maximum-number-of-objects cache)))))))
    (declare (integer space-threshold) (fixnum resource-threshold))
    (values space-threshold resource-threshold)))

(defgeneric cache-garbage-collection-targets (cache free-space-ratio)
  (declare (values reclaim-space reclaim-resources))
  (:documentation "Returns values for garbage collection in CACHE to achieve to meet FREE-SPACE-RATIO.
the first value is the amount of storage space to reclaim.
The second value is the number of resources to reclaim.
The amount of storage space and the number of resources that garbage collection."))

(defmethod cache-garbage-collection-targets ((cache proxy-cache) free-space-ratio)
  (multiple-value-bind (space-threshold resource-threshold)
      (cache-garbage-collection-thresholds cache free-space-ratio)
    (declare (integer space-threshold) (fixnum resource-threshold))
    (let ((delta-space (- space-threshold (the integer (database-storage-size (cache-database cache)))))
	  (delta-objects (- resource-threshold (the fixnum (resources-count cache)))))
      (declare (integer delta-space) (fixnum delta-objects))
      (values (if (minusp delta-space) (- delta-space) 0)
	      (if (minusp delta-objects) (- delta-objects) 0)))))


;;;------------------------------------------------------------------- 
;;;
;;; GARBAGE COLLECTION METHODS
;;;

(defgeneric gc-orphaned-entities (database-or-cache)
  (:documentation "Garbage collects all entities in DATABASE-OR-CACHE for which there is no corresponding metadata."))

(defmethod gc-orphaned-entities ((cache proxy-cache))
 (gc-orphaned-entities (cache-database cache)))

(defgeneric gc-orphaned-metadata (database-or-cache)
  (:documentation "Garbage collects all metadata in database-or-cache for which there is no corresponding metadata."))

(defmethod gc-orphaned-metadata ((cache proxy-cache))
 (gc-orphaned-metadata (cache-database cache)))

(defgeneric expiration-garbage-collect (proxy-cache &key reclaim-bytes reclaim-objects current-time gc-type)
  (:documentation "Expunges expired resources from PROXY-CACHE.
When RECLAIM-BYTES or RECLAIM-OBJECTS are specified, garabage collection stops once these limits are reached."))

(defmethod expiration-garbage-collect ((cache proxy-cache) &key reclaim-bytes reclaim-objects (current-time (get-universal-time)) gc-type)
  (flet ((garbage-p (resource)
	   (cache-object-expired-p resource current-time)))
    (declare (dynamic-extent #'garbage-p))
    (let ((total-count (resources-count cache))
	  (gc-type-string (case gc-type (:full "Full GC") (:incremental "Incremental GC") (t gc-type))))
      (log-event :normal "Proxy Cache~:[~;~:* ~A~]: Beginning expiration garbage collection ~D resources..." gc-type-string total-count)
      (multiple-value-bind (resources-decached bytes-reclaimed)
	  (sweep-cache-resources-for-garbage cache #'garbage-p :enumeration :oldest-reference
					     :reclaim-bytes reclaim-bytes :reclaim-objects reclaim-objects)
	(log-event :normal "Proxy Cache~:[~;~:* ~A~]: Finished expiration garbage collection.~&~D out of ~D resources expunged and ~D bytes reclaimed."
		   gc-type-string resources-decached total-count bytes-reclaimed))
      cache)))

(defgeneric reference-time-garbage-collect (proxy-cache universal-time &key reclaim-bytes reclaim-objects gc-type)
  (declare (values resources-decached bytes-reclaimed))
  (:documentation "Expunges all resources that have not been referenced since UNIVERSAL-TIME.
When RECLAIM-BYTES or RECLAIM-OBJECTS are specified, garabage collection stops once these limits are reached.
GC-TYPE can be either :FULL or :INCREMENTAL."))

(defmethod reference-time-garbage-collect ((cache proxy-cache) (universal-time integer) &key reclaim-bytes reclaim-objects gc-type)
  (macrolet ((gc-value (reclamation)
	       `(and ,reclamation (not (zerop ,reclamation)) ,reclamation)))
    (flet ((garbage-p (resource)
	     (not (last-reference-after-p resource universal-time))))
      (declare (dynamic-extent #'garbage-p))
      (let ((total-count (resources-count cache))
	    (bounded-p (or (gc-value reclaim-bytes) (gc-value reclaim-objects)))
	    (multiple-bound-p (and (gc-value reclaim-bytes) (gc-value reclaim-objects)))
	    (gc-type-string (case gc-type (:full "Full GC") (:incremental "Incremental GC") (t gc-type))))
	(log-event :normal "Proxy Cache~:[~;~:* ~A~]: Beginning reference time garbage collection of ~:D resources.~&~
                            Scavenging resources referenced between ~A and ~A~
                            ~:[~;~&to reclaim ~:[~;~:*~:D bytes~]~:[~; or ~]~:[~;~:*~:D resources~]~]...." 
		   gc-type-string total-count (write-standard-time (cache-earliest-reference-time cache) nil) (write-standard-time universal-time nil)
		   bounded-p (gc-value reclaim-bytes) multiple-bound-p (gc-value reclaim-objects))
	(multiple-value-bind (resources-decached bytes-reclaimed)
	    (sweep-cache-resources-for-garbage cache #'garbage-p :enumeration :oldest-reference
					       :reclaim-bytes reclaim-bytes :reclaim-objects reclaim-objects)
	  (log-event :normal "Proxy Cache~:[~;~:* ~A~]: Finished reference time garbage collection.~
                              ~&~:D out of ~:D resources expunged and ~:D bytes reclaimed."
		     gc-type-string resources-decached total-count bytes-reclaimed)
	  (values resources-decached bytes-reclaimed))))))

(defgeneric revalidate-expired-representations (proxy-cache &key current-time)
  (:documentation "Revalidates all cached representations in PROXY-CACHE that are expired at CURRENT-TIME."))

(defmethod revalidate-expired-representations ((cache proxy-cache) &key (current-time (get-universal-time)))
  (flet ((revalidate-if-expired (representation)
	   (when (< (representation-last-reference representation) current-time)
	     (representation-revalidate-if-expired representation current-time))))
    (declare (dynamic-extent #'revalidate-if-expired))
    (cache-recency-map-representations cache #'revalidate-if-expired)))

(defgeneric garbage-collect-invalid-representations (proxy-cache &key gc-type)
  (:documentation "Expunges expired resources from PROXY-CACHE.
When RECLAIM-BYTES or RECLAIM-OBJECTS are specified, garabage collection stops once these limits are reached."))

;; Grabs a write lock on the resource table. Be prepared to have http threads block
(defmethod garbage-collect-invalid-representations ((cache proxy-cache) &key gc-type)
  (flet ((invalid-representation-p (representation)
	   (or (representation-invalid-p representation)
	       (representation-entity-invalid-p representation))))
    (let ((total-count (resources-count cache))
	  (gc-type-string (case gc-type (:full "Full GC") (:incremental "Incremental GC") (t gc-type))))
      (log-event :normal "Proxy Cache~:[~;~:* ~A~]: Beginning invalid representation garbage collection of ~D resources..." gc-type-string total-count)
      (multiple-value-bind (representations-decached resources-decached bytes-reclaimed)
	  (sweep-cache-representations-for-garbage cache #'invalid-representation-p :enumeration :random)
	(log-event :normal "Proxy Cache~:[~;~:* ~A~]: Finished invalid representation garbage collection.~
                            ~&~D representations expunged, ~D out of ~D resources expunged, and ~D bytes reclaimed."
		   gc-type-string representations-decached resources-decached total-count bytes-reclaimed))
      cache)))

(defmethod proxy-cache-save-unsaved-metadata ((cache proxy-cache) &aux (count 0))
  (flet ((maybe-save-metadata (representation)
	   (when (save-persistently-unsaved-metadata representation)
	     (incf count))))
    (let ((total-count (resources-count cache)))
      (log-event :normal "Proxy Cache: Scanning ~D resources to save unsaved metadata..." total-count)			     
      (map-representations *proxy-cache* #'maybe-save-metadata :read)
      (log-event :normal "Proxy Cache: ~D resources with unsaved metadata found and saved to stable storage." count)
      cache)))

#|(proxy-cache-save-unsaved-metadata *proxy-cache*)|#

(defgeneric proxy-cache-verify-entity-validity (proxy-cache &key active-check-p)
  (:documentation "Verifies that all putatively valid representations point to valid entity data.
Representations with invalid entity data are marked to have invalid data.
When ACTIVE-CHECK-P is non-null, the actual storage media is checked to confirm 
the existence of a valid entity."))

(defmethod proxy-cache-verify-entity-validity ((cache proxy-cache) &key active-check-p &aux (count 0))
  (flet ((verify-entity-validity (representation)
	   (with-lock-held ((cache-object-lock representation) :write "Verify Entity Validity")
	     (when (and (representation-valid-p representation)
			(not (representation-entity-invalid-p representation))
			(not (representation-entity-handle-valid-p representation active-check-p)))
	       (setf (representation-entity-invalid-p representation) t)
	       (incf count)))))
    (declare (dynamic-extent #'verify-entity-validity))
    (let ((total-count (resources-count cache)))
      (log-event :normal "Proxy Cache: Scanning ~D resources ~:[in memory~;on storage media~] to verify entity validity..."
		 total-count active-check-p)
      (map-representations cache #'verify-entity-validity :read)
      (if (zerop count)
	  (log-event :normal "Proxy Cache: No representations found with invalid entities.")
	  (log-event :normal "Proxy Cache: ~D representations found with invalid entities and invalidated." count))))
  cache)


;;;------------------------------------------------------------------- 
;;;
;;; CACHE GC OPERATIONS
;;;

(defgeneric proxy-cache-invoke-full-gc-method (proxy-cache gc-method reclaim-bytes reclaim-objects)
  (declare (values resources-decached bytes-reclaimed))
  (:documentation "Runs  full GC on PROXY-CACHE using GC-METHOD.
Specialize this method to provide new GC methods."))

(defmethod proxy-cache-invoke-full-gc-method ((cache proxy-cache) (gc-method (eql :reference-time)) reclaim-bytes reclaim-objects)
  (reference-time-garbage-collect cache (cache-last-reference cache) :gc-type :full
				  :reclaim-bytes reclaim-bytes :reclaim-objects reclaim-objects))

(defgeneric proxy-cache-full-garbage-collect (proxy-cache)
  (:documentation "Garbage collects cached resources with low probably of reuse.
This is intended to free enough space to avoid tripping the incremental GC during
loaded operation. Typically, this GC will run at night when the load is lighest."))

(defmethod proxy-cache-full-garbage-collect ((cache proxy-cache))
  (with-slots (gc-method) cache
    ;; clean out any invalid representations
    (garbage-collect-invalid-representations cache :gc-type :full)
    ;; perform a reference time GC
    (multiple-value-bind (reclaim-bytes reclaim-objects)
	(cache-garbage-collection-targets cache *proxy-cache-full-gc-free-space-ratio*)
      (proxy-cache-invoke-full-gc-method cache gc-method reclaim-bytes reclaim-objects))
    ;; Make sure all references are current
    (proxy-cache-save-unsaved-metadata cache)))

(defgeneric proxy-cache-daily-full-gc-exclusively (proxy-cache)
  (:documentation "Performs a full GC after arranging for quiesence of the incremental GC."))

(defmethod proxy-cache-daily-full-gc-exclusively ((cache proxy-cache))
  (labels ((report-proxy-gc-error (cache error fatal-p)
	     (let ((error-type (type-of error)))
	       (when fatal-p
		 (log-event :critical "Proxy Full GC, ~A, suspended by fatal error.~&Take corrective action."
			    (cache-name cache)))
	       (report-bug *bug-http-server* (format nil "Proxy~:[~; Fatal~] Full GC Error: ~S" fatal-p error-type)
			   "~:[~;~&The proxy Full GC has been suspended. Attend to the error and resume the proxy GC.~]~
                            ~&Cache: ~S~&Error: ~S~:[~;~&Error Report: ~:*~A~]~:[~;~&Backtrace: ~:*~A~]"
			   fatal-p cache error-type (report-string error)
			   (when *stack-backtraces-in-bug-reports*
			     (stack-backtrace-string error)))))
	   (%handle-proxy-gc-error (error)
	     (report-proxy-gc-error cache error t)))
    (declare (dynamic-extent #'%handle-proxy-gc-error))
    (handler-bind-if (not *debug-server*)
       ((error #'%handle-proxy-gc-error))
      (let ((old-incremental-gc-value *proxy-cache-incremental-gc*))
	(unwind-protect 
	    (progn (setq *proxy-cache-incremental-gc* nil)
		   (sleep 60)
		   (proxy-cache-full-garbage-collect cache))
	  (setq *proxy-cache-incremental-gc* old-incremental-gc-value))))))

(defun daily-proxy-cache-gc ()
  "The daily garbage collection of the proxy cache late at night."
  (let ((proxy-cache *proxy-cache*))
    (when proxy-cache
      (process-run-function "Proxy Cache Full GC" #'proxy-cache-daily-full-gc-exclusively proxy-cache))))

(add-periodic-task "Proxy Cache Daily GC" :daily '(daily-proxy-cache-gc))

(defgeneric proxy-cache-invoke-incremental-gc-method (proxy-cache gc-method reclaim-bytes reclaim-objects)
  (declare (values resources-decached bytes-reclaimed))
  (:documentation "Runs an incremental GC on proxy-cache using gc-method.
Specialize this method to provide new GC methods."))

(defmethod proxy-cache-invoke-incremental-gc-method ((cache proxy-cache) (gc-method (eql :reference-time)) reclaim-bytes reclaim-objects)
  (reference-time-garbage-collect cache (- (cache-last-reference cache) (floor *proxy-server-life-time* 1000))
				  :gc-type :incremental :reclaim-bytes reclaim-bytes :reclaim-objects reclaim-objects))

(defgeneric proxy-cache-incremental-garbage-collect (proxy-cache)
  (:documentation "Incrementally garbage collects cached resources with low probably of reuse.
This is intended for use to reduce resource consumption while the proxy is under load."))

(defmethod proxy-cache-incremental-garbage-collect ((cache proxy-cache))
  (with-slots (incremental-gc-method) cache
    (multiple-value-bind (reclaim-bytes reclaim-objects)
	(cache-garbage-collection-targets cache *proxy-cache-incremental-gc-free-space-ratio*)
      (multiple-value-bind (resources-decached bytes-reclaimed)
	  (proxy-cache-invoke-incremental-gc-method cache incremental-gc-method reclaim-bytes reclaim-objects)
	;; Avoid continuous incremental GCs when the GC fails to meet objectives.
	(unless (and (<= reclaim-bytes bytes-reclaimed)
		     (<= reclaim-objects resources-decached))
	  (error "Incremental Garbage Collection failed to reclaim ~D bytes and ~D objects.~
                ~&Actual reclamation was ~D bytes and ~D objects."
		 reclaim-bytes reclaim-objects bytes-reclaimed resources-decached))))))


;;;------------------------------------------------------------------- 
;;;
;;; INCREMENTAL GC PROCESS
;;;

(defmethod initialize-instance :after ((proxy-cache proxy-cache-incremental-gc-mixin) &key)
  (with-slots (incremental-gc) proxy-cache
    (setf incremental-gc (make-instance 'incremental-cache-gc
					:cache proxy-cache
					:process-name (concatenate 'string "GC-" (cache-name proxy-cache))
					:process-priority *proxy-cache-incremental-gc-process-priority*
					:wait-whostate "GC Wait"))))

(defmethod cache-kill-all-processes progn ((proxy-cache proxy-cache-incremental-gc-mixin))
  (cache-kill-incremental-gc-process proxy-cache))

(defmethod (setf cache-gc-process-priority) (priority (proxy-cache proxy-cache-incremental-gc-mixin))
  (with-slots (incremental-gc) proxy-cache
    (setf (tq:task-process-priority incremental-gc) priority)))

(defgeneric cache-recompute-incremental-gc-thresholds (cache)
  (:documentation "Recomputes the threshold for tripping the increament cache GC."))

(defmethod cache-recompute-incremental-gc-thresholds ((task incremental-cache-gc))
  (with-slots (cache resource-threshold space-threshold) task
    (multiple-value-setq (space-threshold resource-threshold)
      (cache-garbage-collection-thresholds cache *proxy-cache-incremental-gc-free-space-trigger-ratio*))))

(defmethod cache-recompute-incremental-gc-thresholds ((proxy-cache proxy-cache-incremental-gc-mixin))
  (with-slots (incremental-gc) proxy-cache
    (cache-recompute-incremental-gc-thresholds incremental-gc)))

;; Make the GC respond to changes in cache limits.
(defmethod (setf cache-maximum-size) :around ((size integer) (proxy-cache proxy-cache-incremental-gc-mixin))
  (unless (plusp size)
    (error "Bad size, ~S, for the maximum size of ~S." size proxy-cache))
  (call-next-method size proxy-cache)
  (cache-recompute-incremental-gc-thresholds proxy-cache)
  size)

;; Make the GC respond to changes in cache limits.
(defmethod (setf cache-maximum-number-of-objects) :around ((size integer) (proxy-cache proxy-cache-incremental-gc-mixin))
  (unless (and (plusp size) (typep size 'fixnum))
    (error "Bad size, ~S, for the maximum number of objects in ~S." size proxy-cache))
  (call-next-method size proxy-cache)
  (cache-recompute-incremental-gc-thresholds proxy-cache)
  size)

(defmethod tq:task-work-p ((task incremental-cache-gc))
  (with-slots (cache resource-threshold tq::run-p space-threshold) task
    (and tq::run-p
	 *proxy-cache-incremental-gc*
	 (or (< space-threshold (database-storage-size cache))	;guaranteed to return an integer
	     (< resource-threshold (resources-count cache))))))	;guaranteed to return a fixnum

(defmethod tq:task-main-loop :before ((task incremental-cache-gc))
  (cache-recompute-incremental-gc-thresholds task))

(defmethod tq:task-execute ((task incremental-cache-gc))
  (with-slots (cache) task
    (labels ((report-proxy-gc-error (cache error fatal-p)
	       (let ((error-type (type-of error)))
		 (when fatal-p
		   (log-event :critical "Proxy GC, ~A, suspended by fatal error.~&Take corrective action."
			      (cache-name cache)))
		 (report-bug *bug-http-server* (format nil "Proxy~:[~; Fatal~] GC Error: ~S" fatal-p error-type)
			     "~:[~;~&The proxy GC has been suspended. Attend to the error and resume the proxy GC.~]~
                              ~&Cache: ~S~&Error: ~S~:[~;~&Error Report: ~:*~A~]~:[~;~&Backtrace: ~:*~A~]"
			     fatal-p cache error-type (report-string error)
			     (when *stack-backtraces-in-bug-reports*
			       (stack-backtrace-string error)))))
	     (%handle-proxy-gc-error (error)
	       (typecase error
		 ((or network-error file-error)
		  (report-proxy-gc-error cache error nil)
		  (let ((seconds 5))
		    (log-event :exceptional "Proxy Incremental GC, ~A, suspended for ~D seconds by transient ~A error."
			       (cache-name cache) seconds (type-of error))
		    (sleep seconds)
		    (log-event :normal "Proxy Incremental GC, ~A, resumed after ~D seconds." (cache-name cache) seconds))
		  t)
		 (t (report-proxy-gc-error cache error t)
		    (cache-stop-incremental-gc cache)
		    t))))
      (declare (dynamic-extent #'%handle-proxy-gc-error))
      (handler-bind-if (not *debug-server*)
	 ((error #'%handle-proxy-gc-error))
	(proxy-cache-incremental-garbage-collect cache)))))

(defgeneric cache-ensure-incremental-gc-active (proxy-cache)
  (:documentation "Ensure that the incremental cache GC is active."))

(defmethod cache-ensure-incremental-gc-active ((proxy-cache proxy-cache-incremental-gc-mixin))
  (with-slots (incremental-gc) proxy-cache
    (tq:ensure-active-task incremental-gc)))

(defgeneric cache-kill-incremental-gc-process (proxy-cache)
  (:documentation "Kills the process in which the incremental GC runs."))

(defmethod cache-kill-incremental-gc-process ((proxy-cache proxy-cache-incremental-gc-mixin))
  (with-slots (incremental-gc) proxy-cache
    (tq:task-process-kill incremental-gc)))

(defgeneric cache-start-incremental-gc (proxy-cache)
  (:documentation "Start the incremental cache GC."))

(defmethod cache-start-incremental-gc ((proxy-cache proxy-cache-incremental-gc-mixin))
  (with-slots (incremental-gc) proxy-cache
    (tq:start-task incremental-gc)))

(defgeneric cache-stop-incremental-gc (proxy-cache)
  (:documentation "Stop the incremental cache GC."))

(defmethod cache-stop-incremental-gc ((proxy-cache proxy-cache-incremental-gc-mixin))
  (with-slots (incremental-gc) proxy-cache
    (tq:stop-task incremental-gc)))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

#|
(defun test ()
  (let ((*debug-proxy* t)
	(*trace-proxy* t))
    (revalidate-expired-representations *proxy-cache* :current-time (get-universal-time))))

(database-storage-size *proxy-cache*)

(setf (database-log-process-priority (cache-database *proxy-cache*)) 1)

(database-storage-size *proxy-cache*)

(setf (cache-maximum-size *proxy-cache*) 5000000)

(verify-storage-size *proxy-cache*)

(cache-garbage-collection-targets *proxy-cache* *proxy-cache-full-gc-free-space-ratio*)

(proxy-cache-full-garbage-collect *proxy-cache*)

(verify-storage-size *proxy-cache*)

(log-notifications-on (current-access-logs) nil)

|#

