;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: w4; -*-

;;; Copyright John C. Mallery,  1995-96, 2000, 2005.
;;; All rights reserved.

;;;------------------------------------------------------------------- 
;;;
;;; CONSTRAINT-BASED WEB WALKER
;;;
(in-package :w4) 

;;;------------------------------------------------------------------- 
;;;
;;; INITIALIZE CONSTRAINT TYPES 
;;;

(define %make-constraint-function-name (name-string)
  (symbolize (concatenate 'string name-string "-CONSTRAINT-FUNCTION")))

(define %make-constraint-arglist (lambda-list)
  (loop with arg = (copy-list lambda-list)
        with built-in = (mapcar #'intern '("CONSTRAINT" "ACTIVITY" "URL"))
	for item in built-in
        do (setq arg (delete item arg))
        finally (return `(constraint activity url ,.arg))))

(defun %process-declarations (new-declarations function-body)
  (declare (values declarations body))
  (cond ((and (consp (car function-body))
              (eql 'declare (caar function-body)))
         (destructuring-bind (declarations &rest body) function-body
           (values `((declare ,@new-declarations ,@(cdr declarations)))
                   body)))
        (new-declarations
         (values `((declare ,@new-declarations))
                 function-body))
        (t (values nil function-body))))

(define %define-constraint-function (name lambda-list constraint-function)
  (unless constraint-function
    (error "No code was provided for the body of CONSTRAINT-FUNCTION."))
  (multiple-value-bind (declarations body)
      (%process-declarations
        #+Genera `((sys:function-parent define-constraint-type ,name))
        #-Genera nil
        constraint-function)
    `(define ,(%make-constraint-function-name (symbol-name name)) ,lambda-list
       ,@declarations ,@body)))

(define %make-allocation-function-name (name-string)
  (symbolize (concatenate 'string name-string "-ALLOCATION-FUNCTION")))

(define %define-allocation-function (name allocation-function)
  (unless allocation-function
    (error "No code was provided for the body of ALLOCATION-FUNCTION."))
  (destructuring-bind (lambda-list &body body)
      allocation-function
    (multiple-value-bind (declarations body)
        (%process-declarations
          #+Genera `((sys:function-parent define-constraint-type ,name))
          #-Genera nil
          body)
      `(define ,(%make-allocation-function-name (symbol-name name)) (constraint-type arguments)
         ,@declarations
         (destructuring-bind ,lambda-list arguments
           ,@body)))))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(defmethod update-constraint-function ((constraint-type constraint-type) &optional (arglist nil arglist-supplied-p))
  (with-slots (name lambda-list function) constraint-type
    (let ((fctn-name (%make-constraint-function-name name)))
      (setq function (fdefinition fctn-name))
      (setq lambda-list (if arglist-supplied-p
                            arglist
                            (cddr (arglist fctn-name)))))))

(define-generic update-allocation-function (constraint-type))

(defmethod update-allocation-function ((constraint-type constraint-type))
  (with-slots (name allocator) constraint-type
    (let ((fctn-name (%make-allocation-function-name name)))
      (setq allocator (fdefinition fctn-name)))))

(define-generic initialize-constraint-type (constraint-type &rest init-args))

(defmethod initialize-constraint-type ((constraint-type constraint-type) &rest init-args)
  (declare (ignore init-args))
  (with-slots (name) constraint-type
    (%register-constraint-type name constraint-type)
    constraint-type))

(defvar *constraint-type-table* (make-hash-table :test #'equalp)
  "Table mapping constraint type names to constraint type objects.")

(define %register-constraint-type (name object)
  (setf (gethash name *constraint-type-table*) object))

(declaim (inline %get-constraint-type))

(define %get-constraint-type (name)
  (gethash name *constraint-type-table*))

(define find-constraint-type (name &optional (error-p t))
  (cond ((%get-constraint-type name))
        (error-p (error "No constraint named ~S was found." name))
        (t nil)))

(define map-constraint-types (function)
  "Maps FUNCTION over all constraint-types
Function is call with the arguments NAME and CONSTRAINT-TYPE."
  (maphash function *constraint-type-table*))

(define intern-constraint-type (name &key (if-does-not-exist :error) (class 'constraint-type))
  "Returns the interned constraint type named NAME according to IF-DOES-NOT-EXIST."
  (declare (values constraint-type newly-create-p))
  (or (etypecase name
        (string (%get-constraint-type name))
        (symbol (%get-constraint-type (symbol-name name)))
        (constraint-type name))
      (ecase if-does-not-exist
        (:soft nil)
        (:create
          (values (initialize-constraint-type (make-instance class :name (string name)))
                  t))
        (:error
          (error "~S does not denote an existing constraint." name)))))

(define-macro define-constraint-type (name (type &key (class 'constraint-type) documentation
                                                 allocator) lambda-list &body body)
  #.(format nil "Defines a new type of constraint named NAME whose type is TYPE.
TYPE can be any of: ~{~A~^, ~}
LAMBDA-LIST is the set of arguments passed to the constraint function.
BODY is the body of the constraint function. The lexical variables CONSTRAINT and URL
are available within BODY." (constraint-types))
  (declare (values constraint-type newly-created-p))
  (let ((namestring (symbol-name name))
        (arglist (%make-constraint-arglist lambda-list)))
    `(progn
       ,(%define-constraint-function name arglist body)
       ,.(when allocator `(,(%define-allocation-function name allocator)))
       (multiple-value-bind (constraint-type new-p)
           (intern-constraint-type ,namestring :if-does-not-exist :create :class ',class)
         (setf (constraint-type-instance-class constraint-type) (%constraint-class-for-type ,type))
         (setf (documentation-string constraint-type) (or ,documentation "Undocumented."))
         (update-constraint-function constraint-type ',arglist)
         ,@(when allocator `((update-allocation-function constraint-type)))
         (values constraint-type new-p)))))


;;;------------------------------------------------------------------- 
;;;
;;; ALLOCATION AND DEALLOCATION
;;;

(define allocate-constraint-set (constraints &optional (class 'constraint-set))
  (case class
    (constraint-set
      (make-instance 'constraint-set :constraints constraints))
    (t (funcall #'make-instance class :constraints constraints))))

(define-generic allocate-constraint (constraint-type arguments))

(defmethod allocate-constraint ((constraint-type constraint-type) arguments)
  (with-slots (instance-class) constraint-type
    (case instance-class
      (constraint
        (make-instance 'constraint :constraint-type constraint-type :arguments arguments))
      (t (make-instance instance-class
                        :constraint-type constraint-type :arguments arguments)))))

(define-generic allocate-constraint-structure (constraint-spec)
  (:documentation "Primary method for allocating constraint structure from a textual specification."))

(defmethod allocate-constraint-structure ((constraint-spec t))
  constraint-spec)

(defmethod allocate-constraint-structure ((constraint-spec cons))
  (etypecase (car constraint-spec)
    (symbol
      (allocate-constraint (intern-constraint-type (first constraint-spec) :if-does-not-exist :error)
                           (cdr constraint-spec)))
    (cons
      (loop with constraint-set-p = t
            for item in constraint-spec
            for structure = (allocate-constraint-structure item)
            collect structure into result
            unless (typep structure 'constraint)
              do (setq constraint-set-p nil)
            finally (return (if constraint-set-p
                                (allocate-constraint-set result)
                                result))))))

(defmethod allocate-constraint ((constraint-type circumstance-constraint-type) arguments)
  (with-slots (allocator instance-class) constraint-type
    (let ((args (funcall allocator constraint-type arguments)))
      (case instance-class
        (circumstance-constraint
          (make-instance 'circumstance-constraint :constraint-type constraint-type
                         :arguments args))
        (t (make-instance instance-class
                          :constraint-type constraint-type :arguments args))))))
      
(define-generic deallocate-constraint-structure (constraint-spec)
  (:documentation "Primary method for deallocating constraint structure."))

(defmethod deallocate-constraint-structure ((constraint-spec t))
  constraint-spec)

(defmethod deallocate-constraint-structure ((constraint-spec cons))
  (mapc #'deallocate-constraint-structure constraint-spec))

(define-macro let-constraint-structure (variable-bindings &body body)
  "Binding form for temporarily allocating constraint structure.
VARIABLE-BINDINGS are a list of (variable-name constraint-spec).
Whenever VARIABLE-NAME is non-null after execution of BODY,
any constraint structure bound to VARIABLE-NAME is deallocated."
  (loop for (var value) in variable-bindings
        collect `(,var (allocate-constraint-structure ,value)) into bindings
        collect `(,var (deallocate-constraint-structure ,var)) into dereferencings
        finally (return (cond (bindings
                               `(let ,bindings
                                  (unwind-protect (progn . ,body)
                                    (cond-every ,.dereferencings))))
                              (t `(progn . ,body))))))

(define read-constraints-from-string (string &optional (start 0) (end (length string)))
  (when string
    (let ((*package* (find-package :w4)))
      #+Genera ;; convert circle-cross and delta to spaces to clean up lossage
      (nsubstitute-if #\space #'(lambda (x) (member x `#.(mapcar #'code-char '(13 10)) :test #'eql))
                      string :start start :end end)
      (loop with s = start
            for constraint = (multiple-value-bind (form idx)
                                 (read-from-string string nil nil :start s :end end)
                               (setq s idx)
                               form)
            when constraint
              collect constraint into constraints
            while (> end s)
            finally (return-from read-constraints-from-string
                      (etypecase (car constraints)
                        (null nil)
                        (symbol (list constraints))
                        (list (typecase (caar constraints)
                                (cons (car constraints))
                                (t constraints)))))))))


;;;------------------------------------------------------------------- 
;;;
;;; ACTION OPERATIONS
;;;

(define %make-action-function-name (name-string)
  (symbolize (concatenate 'string name-string "-ACTION-FUNCTION")))

(define %make-action-arglist (lambda-list)
  (loop with arg = (copy-list lambda-list)
	with built-in = (mapcar #'intern '("ACTION" "ACTIVITY" "URL"))
        for item in built-in 
        do (setq arg (delete item arg))
        finally (return (nconc built-in arg))))

(define %define-action-function (name class lambda-list action-function)
  (unless action-function
    (error "No code was provided for the body of ACTION-FUNCTION."))
  (multiple-value-bind (declarations body)
      (%process-declarations
        #+Genera `((sys:function-parent ,name define-action-type))
        #-Genera nil
        action-function)
    (cond ((subtypep class 'encapsulating-action-type)
           `(define ,(%make-action-function-name (symbol-name name)) ,lambda-list
              ,@declarations
              (macrolet ((call-next-action (&optional (action 'action) (url 'url) (activity 'activity))
                           `(with-slots (inferiors) ,action
                              (dolist (inf inferiors)
                                (perform-action ,url inf ,activity)))))
                ,@body)))
          (t `(define ,(%make-action-function-name (symbol-name name)) ,lambda-list
                ,@declarations
                ,@body)))))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(defmethod update-action-function ((action-type action-type) &optional (arglist nil arglist-supplied-p))
  (with-slots (name lambda-list function) action-type
    (let ((fctn-name (%make-action-function-name name)))
      (setq function (fdefinition fctn-name))
      (setq lambda-list (if arglist-supplied-p
                            arglist
                            (cdddr (arglist fctn-name)))))))

(defvar *action-type-table* (make-hash-table :test #'equalp)
  "Table mapping action names to action objects.")

(define %register-action-type (name object)
  (setf (gethash name *action-type-table*) object))

(declaim (inline %get-action-type))

(define %get-action-type (name)
  (gethash name *action-type-table*))

(define find-action-type (name &optional (error-p t))
  (cond ((%get-action-type name))
        (error-p
         (error "No constraint named ~S was found." name))
        (t nil)))

(define map-action-types (function)
  "Maps FUNCTION over all action-types.
Function is call with the arguments NAME and ACTION-TYPE."
  (maphash function *action-type-table*))

(declaim (inline %unregister-action-type))

(define %unregister-action-type (action-type-name)
  (check-type action-type-name string)
  (remhash action-type-name *action-type-table*))

(define-generic unintern-action-type (action-type))

(defmethod unintern-action-type ((action-type action-type))
  (with-slots (name) action-type
    (%unregister-action-type name)))

(defmethod unintern-action-type ((action-type string))
  (%unregister-action-type action-type))

(defmethod unintern-action-type ((action-type symbol))
  (%unregister-action-type (symbol-name action-type)))

(define-generic initialize-action-type (action-type &rest init-args))

(defmethod initialize-action-type ((action-type action-type) &rest init-args)
  (declare (ignore init-args))
  (with-slots (name) action-type
    (%register-action-type name action-type)
    action-type))

(define intern-action-type (name &key (if-does-not-exist :error) (class 'action-type))
  (declare (values action-type newly-create-p))
  (or (etypecase name
        (string (%get-action-type name))
        (symbol (%get-action-type (symbol-name name)))
        (action-type name))
      (ecase if-does-not-exist
        (:soft nil)
        (:create
          (values (initialize-action-type (make-instance class :name (string name))) t))
        (:error
          (error "~S does not denote an existing action-type." name)))))

(define-macro define-action-type (name (type &key (class 'action-type) documentation) lambda-list &body body)
  #.(format nil "Defines a new type of action named NAME whose type is TYPE.
TYPE can be any of: ~{~A~^, ~}
LAMBDA-LIST is the set of arguments passed to the action function.
The lexical variables ACTION ACTIVITY URL are available within BODY.
When CLASS is a subtype of encapsulating-action-type, the continuation
that executes the encapsulated actions is invoked within the
action function with (call-next-action &optional action url activity).
Additionally, the first argument after the standard action arguments must be
the inferior actions during the allocation process." (action-types))
  (let ((namestring (symbol-name name))
        (arglist (%make-action-arglist lambda-list)))
    `(progn
       ,(%define-action-function name class arglist body)
       (multiple-value-bind (action-type new-p)
           (intern-action-type ,namestring :if-does-not-exist :create :class ',class)
         (unless (eq (type-of action-type) ',class)
           (change-class action-type ',class))
         (setf (action-type-instance-class action-type) (%action-class-for-type ',type))
         (setf (documentation-string action-type) (or ,documentation "Undocumented."))
         (update-action-function action-type ',arglist)
         (values action-type new-p)))))


;;;------------------------------------------------------------------- 
;;;
;;; ALLOCATION ACTION STRUCTURES
;;;

(define-generic allocate-action (action-type arguments))

(defmethod allocate-action ((action-type action-type) arguments)
  (with-slots (instance-class) action-type
    (case instance-class
      (action
        (make-instance 'action :action-type action-type :arguments arguments))
      (t (make-instance instance-class
                        :action-type action-type :arguments arguments)))))

(defmethod allocate-action ((action-type encapsulating-action-type) arguments)
  (with-slots (instance-class) action-type
    (destructuring-bind (inferior-specs &rest args) arguments
      (case instance-class
        (encapsulating-action
          (make-instance 'encapsulating-action
                         :action-type action-type
                         :inferiors (allocate-action-structure inferior-specs)
                         :arguments args))
        (t (make-instance instance-class
                          :action-type action-type
                          :inferiors (allocate-action-structure inferior-specs)
                          :arguments args))))))

(define-generic allocate-action-structure (action-spec)
  (:documentation "Primary method for allocating action structure from a textual specification."))

(defmethod allocate-action-structure ((action t)) action)

(defmethod allocate-action-structure ((action action)) action)

(defmethod allocate-action-structure ((action-spec cons))
  (typecase (car action-spec)
    (cons
      (loop for item in action-spec
            collect (allocate-action-structure item)))
    (action
      (loop for item in action-spec
            collect (allocate-action-structure item)))
    (t (allocate-action (intern-action-type (car action-spec)
                                            :if-does-not-exist :error)
                        (cdr action-spec)))))

(define-generic deallocate-action-structure (action-spec)
  (:documentation "Primary method for deallocating action structure."))

(defmethod deallocate-action-structure ((action-spec t))
  action-spec)

(defmethod deallocate-action-structure ((action-spec cons))
  (mapc #'deallocate-action-structure action-spec))

(defmethod deallocate-action-structure ((action encapsulating-action))
  (with-slots (inferiors) action
    (mapc #'deallocate-action-structure inferiors)))

(define-macro let-action-structure (variable-bindings &body body)
  "Binding form for temporarily allocating action structure.
VARIABLE-BINDINGS are a list of (variable-name action-spec).
Whenever VARIABLE-NAME is non-null after execution of BODY,
any action structure bound to VARIABLE-NAME is deallocated."
  (loop for (var value) in variable-bindings
        collect `(,var (allocate-action-structure ,value)) into bindings
        collect `(,var (deallocate-action-structure ,var)) into dereferencings
        finally (return (cond (bindings
                               `(let ,bindings
                                  (unwind-protect (progn . ,body)
                                    (cond-every ,.dereferencings))))
                              (t `(progn . ,body))))))

;;;------------------------------------------------------------------- 
;;;
;;; INITIALIZE ACTIVITY 
;;;

(defvar *activity-table* (make-hash-table :test #'equalp)
  "Table mapping activity names to activity objects.")

(define %register-activity (name object)
  (check-type name string)
  (setf (gethash name *activity-table*) object))

(declaim (inline %get-activity))

(define %get-activity (name)
  (check-type name string)
  (gethash name *activity-table*))

(define find-activity (name &optional (error-p t))
  (cond ((%get-activity name))
        (error-p
         (error "No constraint named ~S was found." name))
        (t nil)))

(define map-activities (function)
  "Maps FUNCTION over all activities.
Function is call with the arguments NAME and ACTIVITY."
  (maphash function *activity-table*))

(declaim (inline %unregister-activity))

(define %unregister-activity (activity-name)
  (check-type activity-name string)
  (remhash activity-name *activity-table*))

(define-generic unintern-activity (activity))

(defmethod unintern-activity ((activity activity))
  (with-slots (name) activity
    (%unregister-activity name)))

(defmethod unintern-activity ((activity string))
  (%unregister-activity activity))

(defmethod unintern-activity ((activity symbol))
  (%unregister-activity (symbol-name activity)))

(define-generic initialize-activity (activity &rest init-args))

(defmethod initialize-activity ((activity activity) &rest init-args)
  (with-slots (name) activity
    (destructuring-bind (&key uninterned) init-args
      (unless uninterned
        (%register-activity name activity))
      activity)))

(define-generic deinitialize-activity (activity))

(defmethod deinitialize-activity ((activity activity))
  (deinitialize-queue activity (activity-queue activity))
  activity)

(define intern-activity (name &key (if-does-not-exist :error) (if-exists :overwrite) (class 'activity))
  (declare (values activity newly-create-p))
  (macrolet ((check-overwrite (activity)
               `(let ((activity ,activity))
                  (when activity
                    (ecase if-exists
                      (:error (error "An activity named, ~A, already." (activity-name activity)))
                      (:overwrite nil)))
                  activity))
             (handling-intern-status (&body body)
               `(case if-does-not-exist
                  (:uninterned
                    (values (initialize-activity (make-instance class :name (string name)) :uninterned t) :uninterned))
                  (t ,@body))))
    (handling-intern-status
      (or (etypecase name
            (string
              (check-overwrite (%get-activity name)))
            (symbol
              (check-overwrite (%get-activity (symbol-name name))))
            (activity name))
          ;; Doesn't already exist.
          (ecase if-does-not-exist
            (:soft nil)
            (:create (values (initialize-activity (make-instance class :name (string name))) :interned))
            (:error (error "~S does not denote an existing activity." name)))))))

(define %define-activity (name constraint-set actions
			       &key unsatisfied-actions documentation report-stream
			       user-agent operator connection-timeout life-time
			       (url-host-name-resolution :never)
			       (search-method :depth-first)
			       (threads 1)
			       predicate
			       (proxy (local-proxy))
			       proxy-port
			       uri-universe
			       (if-does-not-exist :uninterned)
			       (if-exists :error)
			       (class 'activity))
  "Program level interface for defining activities."
  (flet ((parse-operator (operator)
	   (etypecase operator
	     (string (list (parse-internet-mail-address operator)))
	     (cons (mapcar #'parse-internet-mail-address operator))
	     (mail-address (list operator))
	     (null (when http:*server-mail-address*
		     (parse-internet-mail-address http:*server-mail-address*))))))
    (let ((activity (intern-activity name
				     :if-does-not-exist if-does-not-exist
				     :if-exists if-exists
				     :class class)))
      (unless (member url-host-name-resolution '(:always :never :preferred))
	(error "URL-HOST-NAME-RESOLUTION is not one of the valid values, ~S"
	       '(:always :never :preferred)))
      (unless (assoc search-method *queue-class-alist*)
	(error "SEARCH-METHOD, ~S, is not one of the known search methods, ~{~S~^, ~}."
               search-method
	       (mapcar #'car *queue-class-alist*)))
      (setf (activity-constraint-set activity) (allocate-constraint-structure constraint-set)
	    (activity-actions activity) (allocate-action-structure actions)
	    (activity-unsatisfied-actions activity) (allocate-action-structure unsatisfied-actions)
	    (activity-url-host-name-resolution activity) url-host-name-resolution
	    (activity-queue-type activity) search-method
	    (activity-best-first-predicate activity) predicate
	    (activity-number-of-threads activity) threads
	    (activity-proxy activity) (etypecase proxy
					(null nil)
					(http::proxy proxy)
					(string (http::intern-proxy proxy (or proxy-port 80) :if-does-not-exist :create)))
	    (activity-uri-universe activity) (or uri-universe name)
	    (activity-report-stream activity) (or report-stream (with-null-stream (stream) stream))
	    (activity-user-agent activity) (or user-agent (robot-version))
	    (activity-operator activity) (parse-operator operator)
	    (activity-connection-timeout activity) (if connection-timeout (* 60. connection-timeout) http:*client-timeout*)
	    (activity-life-time activity) (if life-time (* 60. life-time) http:*server-timeout*)
	    (documentation-string activity) (or documentation "Undocumented."))
      activity)))

(define-macro define-activity (name &key constraint-set actions unsatisfied-actions
                                    (search-method :depth-first)
                                    user-agent operator connection-timeout life-time
				    (url-host-name-resolution :never)
                                    documentation report-stream 
                                    (class 'activity))
  "Top-level method for compile time definition of an activity for Web walking.
CONNECTION-TIMEOUT is the number of seconds to wait for a server to respond.
LIFE-TIME is the maximum number of seconds to allocate for the web walk."
  `(%define-activity ',(symbol-name name) ',constraint-set ',actions
                     :unsatisfied-actions ',unsatisfied-actions
                     :search-method ',search-method
                     :if-does-not-exist :create
                     :if-exists :overwrite
                     :user-agent ,user-agent
                     :operator ,operator
		     :connection-timeout ,connection-timeout
		     :life-time ,life-time
                     :url-host-name-resolution ,url-host-name-resolution
                     :documentation ,documentation
                     :report-stream ',report-stream
                     :class ',class))

(define-macro with-activity ((name (&key (activity-var (intern "ACTIVITY" *package*))
					 (search-method :depth-first)
					 (threads 1)
					 predicate
					 (proxy '(local-proxy)) 
					 proxy-port
					 (if-does-not-exist :uninterned)
					 (if-exists :error)
					 user-agent operator connection-timeout life-time
					 (url-host-name-resolution :never)
					 uri-universe
					 documentation report-stream
					 (class ''activity))
				   &key constraints actions unsatisfied-actions)
			     &body body)
  "Top-level method for runtime definition of an activity for Web walking.
See the macros LET-CONSTRAINT-STRUCTURE and LET-ACTION-STRUCTURE
CONNECTION-TIMEOUT is the number of seconds to wait for a server to respond.
LIFE-TIME is the maximum number of seconds to allocate for the web walk.
PROXY and PROXY-PORT specify a local proxy to use when fetching web resources
to achieve local caching of resources walked.
Note that DEFINE-CLIENT-PROXY-MAPPING provides a more general interface for
specifying proxies for use by the cient."
  (let ((code `(let-constraint-structure ((constraints ,constraints))
		 (let-action-structure ((actions ,actions))
		   (let ((,activity-var (%define-activity ,name constraints actions
							  ,.(when unsatisfied-actions
							      `(:unsatisfied-actions unsatisfied-actions))
							  :if-does-not-exist ,if-does-not-exist
							  :if-exists ,if-exists
							  :documentation ,documentation
							  :user-agent ,user-agent
							  :operator ,operator
							  :connection-timeout ,connection-timeout
							  :life-time ,life-time
							  :url-host-name-resolution ,url-host-name-resolution
							  :search-method ,search-method
							  :threads ,threads
							  :predicate ,predicate
							  :proxy ,proxy
							  :proxy-port ,proxy-port
							  :uri-universe ,uri-universe
							  :report-stream ,report-stream
							  :class ,class)))
		     (unwind-protect 
			 (progn (initialize-queue ,activity-var (activity-queue-type ,activity-var))
				,@body)
		       (deinitialize-activity ,activity-var)))))))
    (if unsatisfied-actions
	`(let-action-structure ((unsatisfied-actions ,unsatisfied-actions)) ,code)
	code)))
     
(defmethod report-stream ((activity activity))
  (with-slots (report-stream) activity
    (etypecase report-stream
      (symbol (if (boundp report-stream)
		  (symbol-value report-stream)
		  *standard-output*))
      (cons (apply (first report-stream) (rest report-stream)))
      (function report-stream)
      (stream report-stream)
      #+CLIM(clim:sheet report-stream)
      #+Genera(tv:sheet report-stream))))


;;;------------------------------------------------------------------- 
;;;
;;; DOCUMENTATION CONSTRAINTS, ACTIONS, AND ACTIVITIES.
;;;

(defmethod pretty-name ((constraint-type constraint-type))
  (string-capitalize (constraint-type-name constraint-type)))

(defmethod lambda-list ((constraint-type constraint-type))
  (with-slots (lambda-list) constraint-type
    (cdddr lambda-list)))

(defmethod pretty-name ((action-type action-type))
  (string-capitalize (action-type-name action-type)))

(defmethod lambda-list ((action-type action-type))
  (with-slots (lambda-list) action-type
    (cdddr lambda-list)))

(defmethod type-string ((object walker-documentation-mixin))
  (string-capitalize (type-of object)))

(defmethod type-string ((constraint-type constraint-type))
  (with-slots (instance-class) constraint-type
    (string-capitalize instance-class)))

(defmethod type-string ((action-type action-type))
  (with-slots (instance-class) action-type
    (string-capitalize instance-class)))

(defmethod html-describe-object ((object walker-documentation-mixin) &optional (stream html:*output-stream*))
  (let ((name-string (pretty-name object))
        (constraint-type (type-string object))
        (arguments (lambda-list object))
        (documentation (documentation-string object)))
    (fresh-line stream)
    (html:with-paragraph (:stream stream)
      (html:with-rendition (:bold :stream stream)
        (write-string name-string stream))
      (write-string " [" stream)
      (write-string constraint-type stream)
      (write-string "]: " stream)
      (if arguments
          (write arguments :escape nil :base 10. :stream stream)
          (write-string "()" stream))
      (fresh-line stream)
      (html:break-line :stream stream)
      (write-string (or documentation "Undocumented.") stream))
    object))

(define html-find-constraint-types (&key substring (stream html:*output-stream*)
                                         &aux constraint-types)
  (let ((title (format nil "W4 Constraints~:[~; matching ~:*~S~]" substring )))
    (flet ((collect (key object)
             (when (or (null substring) (search substring key :test #'equalp))
               (http::push-ordered object constraint-types #'string< :key #'constraint-type-name))))
      (map-constraint-types #'collect)
      (html:with-html-document (:stream stream)
        (html:with-document-preamble (:stream stream)
          (html:declare-title title :stream stream))
        (html:with-standard-document-body (:stream stream)
          (html:with-section-heading (title :stream stream)
            (html:horizontal-line :stream stream)
            (dolist (item constraint-types)
              (html-describe-object item stream))
            (html:horizontal-line :stream stream)
            (cl-http-signature stream)))))))

(define html-find-action-types (&key substring (stream html:*output-stream*)
                                     &aux action-types)
  (let ((title (format nil "W4 Actions~:[~; matching ~:*~S~]" substring )))
    (flet ((collect (key object)
             (when (or (null substring) (search substring key :test #'equalp))
               (http::push-ordered object action-types #'string< :key #'action-type-name))))
      (map-action-types #'collect)
      (html:with-html-document (:stream stream)
        (html:with-document-preamble (:stream stream)
          (html:declare-title title :stream stream))
        (html:with-standard-document-body (:stream stream)
          (html:with-section-heading (title :stream stream)
            (html:horizontal-line :stream stream)
            (dolist (item action-types)
              (html-describe-object item stream))
            (html:horizontal-line :stream stream)
            (cl-http-signature stream)))))))


;;;------------------------------------------------------------------- 
;;;
;;; SORTING CONSTRAINTS BY EFFICIENCY CLASSES
;;;

(eval-when (compile eval load)
  (defconstant *constraint-efficiency-class-ordering*
               '(:context-constraint :url-constraint :dns-constraint :header-constraint :resource-constraint))
  (define-macro efficiency-class-position (keyword)
    `(or (position ,keyword *constraint-efficiency-class-ordering*
                   :start 0 :end (length *constraint-efficiency-class-ordering*))
         (error "Unknown constraint efficiency class, ~S." ,keyword)))
  )

(define-generic constraint-efficiency-class (constraint)
  (declare (values efficiency-class ranking))
  (:documentation "Returns a keyword denoting the efficiency class to which the constraint belongs."))

;; default to the worst case
(defmethod constraint-efficiency-class ((constraint constraint))
  (values :resource-constraint #.(efficiency-class-position :resource-constraint)))

(defmethod constraint-efficiency-class ((constraint context-constraint))
  (values :context-constraint #.(efficiency-class-position :context-constraint)))

(defmethod constraint-efficiency-class ((constraint url-constraint))
  (values :url-constraint #.(efficiency-class-position :url-constraint)))

(defmethod constraint-efficiency-class ((constraint dns-constraint))
  (values :dns-constraint #.(efficiency-class-position :dns-constraint)))

(defmethod constraint-efficiency-class ((constraint header-constraint))
  (values :header-constraint #.(efficiency-class-position :header-constraint)))

(defmethod constraint-efficiency-class ((constraint resource-constraint))
  (values :resource-constraint #.(efficiency-class-position :resource-constraint)))

(defmethod constraint-efficiency-class ((constraint circumstance-constraint))
  (with-slots (arguments) constraint
    (highest-efficiency-class arguments)))

(define-generic highest-efficiency-class (constraint-or-constraint-set)
  (declare (values efficiency-class ranking))
  (:documentation "Returns a keyword denoting least efficient efficiency class to which the constraint belongs."))

(defmethod highest-efficiency-class ((thing t))
  (values :context-constraint #.(efficiency-class-position :context-constraint)))

(defmethod highest-efficiency-class ((list list))
  (loop with efficiency-class = :context-constraint
        with ranking = 0
        for item in list
        do (multiple-value-bind (class rank)
               (highest-efficiency-class item)
             (cond ((< ranking rank)
                    (setq efficiency-class class
                          ranking rank))
                   (t nil)))
        finally (return (values efficiency-class ranking))))

(defmethod highest-efficiency-class ((constraint constraint))
  (constraint-efficiency-class constraint))

(defmethod highest-efficiency-class ((constraint circumstance-constraint))
  (with-slots (arguments) constraint
    (highest-efficiency-class arguments)))

(defmethod highest-efficiency-class ((constraint-set constraint-set))
  (with-slots (context-constraints dns-constraints header-constraints
                                   resource-constraints url-constraints sorted-p)
              constraint-set
    ;; ensure that constraints are always sorted.
    (unless sorted-p (sort-constraints constraint-set))
    (cond
      (resource-constraints (highest-efficiency-class resource-constraints))
      (header-constraints (highest-efficiency-class header-constraints))
      (dns-constraints (highest-efficiency-class dns-constraints))
      (url-constraints (highest-efficiency-class url-constraints))
      (context-constraints (highest-efficiency-class context-constraints))
      (t (error "No constraints found.")))))

(define-generic sort-constraints (constraint-set)
  (:documentation "Sorts constraints in CONSTRAINT-SET for application."))

(defmethod sort-constraints ((constraint-set constraint-set))
  (with-slots (constraints context-constraints dns-constraints 
                           header-constraints resource-constraints
                           url-constraints sorted-p) constraint-set
    (loop for constraint in constraints
          for efficiency-class = (highest-efficiency-class constraint)
          when (eq efficiency-class :context-constraint)
            collect constraint into c-constraints
          else
            when (eq efficiency-class :url-constraint)
              collect constraint into u-constraints
          else
            when (eq efficiency-class :dns-constraint)
              collect constraint into d-constraints
          else
            when (eq efficiency-class :header-constraint)
              collect constraint into h-constraints
          else
            when (eq efficiency-class :resource-constraint)
              collect constraint into r-constraints
          else do (error "Unknown constraint efficiency class ~S for ~S." efficiency-class constraint)
          finally (setf context-constraints c-constraints
                        url-constraints u-constraints
                        dns-constraints d-constraints
                        header-constraints h-constraints
                        resource-constraints r-constraints
                        sorted-p t))))


;;;------------------------------------------------------------------- 
;;;
;;; APPLYING CONSTRAINTS TO URLs
;;;

(define-generic satisfies-p (url activity constraint-or-constraint-set)
  (:documentation "Returns non-null if URL and ACTIVITY satisfies CONSTRAINT-OR-CONSTRAINT-SET."))

(defmethod satisfies-p (url activity (constraint (eql t)))
  (declare (ignore url activity))
  t)

(defmethod satisfies-p (url activity (constraint (eql nil)))
  (declare (ignore url activity))
  nil)

(declaim (inline %satisfies-contraint-p))

(defun %satisfies-contraint-p (url activity constraint)
  (let (type function result trace)
    (cond ((setq type (constraint-type constraint))
	   (unless (setq function (constraint-type-function type))
	     (error "No constraint function available for ~S." type))
	   (setq result (handler-case
			  (apply function constraint activity url (constraint-arguments constraint))
			  ;; This should log the problem for possible retry 
			  (unknown-host-name () nil)
			  (network-error () nil)))
	   (when (and (setq trace *trace-constraints*)
		      (or (eql trace t)
			  (member (constraint-type-name type) trace :test #'string-equal)))
	     (trace-report (report-stream activity) "(~A~{ ~A~}) => ~S ~40T~S"
			   (constraint-type-name type) (constraint-arguments constraint) result (name-string url)))
	   result)
	  (t (error "No constraint type available for ~S." constraint)))))

(defmethod satisfies-p (url (activity activity) (constraint context-constraint))
  (%satisfies-contraint-p url activity constraint))

(defmethod satisfies-p ((url url:url) (activity activity) (constraint constraint))
  (%satisfies-contraint-p url activity constraint))

(defmethod satisfies-p ((url url:http-url) (activity activity) (constraint-set constraint-set))
  (with-slots (dns-constraints header-constraints resource-constraints
                               url-constraints sorted-p) constraint-set
    (flet ((%satisfies-constraint-list-p (url activity c-list)
             (loop for constraint in c-list
                   unless (satisfies-p url activity constraint)
                   do (return-from satisfies-p nil)
                   finally (return t))))
      (declare (inline %satisfies-constraint-list-p))
      ;; ensure that constraints are always sorted.
      (unless sorted-p (sort-constraints constraint-set))
      ;; check them in fixed order of efficiency
      (and (%satisfies-constraint-list-p url activity url-constraints)
           (%satisfies-constraint-list-p url activity dns-constraints)
           (%satisfies-constraint-list-p url activity header-constraints)
           (%satisfies-constraint-list-p url activity resource-constraints)))))

(define-generic satisfies-context-constraints-p (url activity))

(defmethod satisfies-context-constraints-p (url (activity activity))
  (with-slots (constraint-set) activity
    (with-slots (context-constraints sorted-p) constraint-set
      ;; ensure that constraints are always sorted.
      (unless sorted-p (sort-constraints constraint-set))
      (loop for constraint in context-constraints
            unless (satisfies-p url activity constraint)
              do (return-from satisfies-context-constraints-p nil)
            finally (return-from satisfies-context-constraints-p t)))))

(define-generic satisfies-activity-p (url activity)
  (:documentation "Returns non-null if URL satisfies ACTIVITY."))

(defmethod satisfies-activity-p ((url url:http-url) (activity activity))
  (satisfies-p url activity (activity-constraint-set activity)))

(define-generic satisfies-constraints-p (url activity constraint-set))

(defmethod satisfies-constraints-p ((url url:http-url) (activity activity) (constraint-set constraint-set))
  (with-slots (context-constraints sorted-p) constraint-set
    ;; ensure that constraints are always sorted.
    (unless sorted-p (sort-constraints constraint-set))
    (loop for constraint in context-constraints
          unless (and (satisfies-p url activity constraint)
                      (satisfies-p url activity constraint-set))
            do (return-from satisfies-constraints-p nil)
          finally (return-from satisfies-constraints-p t))))


;;;------------------------------------------------------------------- 
;;;
;;; CACHE OPERAITONS
;;;

(define-generic clear-walker-cache (url)
  (:documentation "Clears any data cached on URL by the Web Walker."))

(defmethod clear-walker-cache ((url url))
  ;; deallocate any retained headers
  (multiple-value-bind (headers found-p)
      (get-value url :headers)
    (when (and found-p headers)
      (deallocate-resource 'http::header-set headers)))
  (multiple-value-bind (array found-p)
      (get-value url :content)
    (when (and found-p array)
      (http::deallocate-web-walker-cache-array array)))
  ;;clear URL cache.
  (dolist (key *cache-indicators*)
    (remove-value url key))
  url)

(define-generic clear-cache (activity url &optional activity-cache-p))

(defmethod clear-cache ((activity activity) (url url) &optional activity-cache-p)
  (clear-walker-cache url)
  (when activity-cache-p
    (remove-value url activity))
  activity)

(define clear-url-caches ()
  "Clears all cached data on urls in the url table."
  (flet ((clear-url-cache (string url)
           (declare (ignore string))
	   (clear-walker-cache url)))
    (map-url-table #'clear-url-cache)))

(define-generic get-resource-time-stamp (activity url &optional recompute-p)
  (:documentation "Returns the universal time when the resource was last visited,
computing one if none is present."))

(defmethod get-resource-time-stamp ((activity activity) (url http-url) &optional recompute-p)
  (with-value-cached (url activity :recompute-p recompute-p)
    (get-universal-time)))

(define-generic %get-resource-time-stamp (activity url)
  (:documentation "Returns the universal time when the resource was last visited."))

(defmethod %get-resource-time-stamp ((activity activity) (url http-url))
  (get-value url activity))

(define-generic get-url-notes (activity location note-type)
  (:documentation "Returns a list of notes for LOCATION written during ACTIVITY.
NOTE-TYPE is a keyword indicating the type of the note."))

(defmethod get-url-notes ((activity activity-note-tracking-mixin) (location url) note-type)
  (declare (values notes foundp))
  (let ((note-table (activity-note-table activity))
	entry)
    (with-lock-held ((activity-note-lock activity) :read "Read Activity Note")
      (if (setq entry (gethash location note-table))
	  (values (getf entry note-type) t)
	  (values nil nil)))))

(define-generic record-url-note (activity location note-type note)
		(:documentation "Records NOTE about LOCATION as NOTE-TYPE for ACTIVITY.
NOTE is a string. NOTE-TYPE is a keyword indicating the type of the note."))

(defmethod record-url-note ((activity activity-note-tracking-mixin) (location url) note-type note)
  (let ((note-table (activity-note-table activity))
	entry)
    (with-lock-held ((activity-note-lock activity) :write "Write Activity Note")
      (cond ((setq entry (gethash location note-table))
	     (let ((val (getf entry note-type)))
	       (if val
		   (push note (getf entry note-type))
		   (setf (getf entry note-type) (list note)))))
	    (t (setf (gethash location note-table) (list note-type note)))))))

(define-generic clear-url-notes (activity &optional note-type)
		(:documentation "Clears notes associated with ACTIVITY.
NOTE-TYPE defaults to :ALL and removes all notes. If NOTE-TYPE is a keyword,
only those note types are removed."))

(defmethod clear-url-notes ((activity activity-note-tracking-mixin) &optional (note-type :all))
  (flet ((clear-note-type (url plist)
	   (declare (ignore url))
	   (remf plist note-type)))
    (declare (dynamic-extent #'clear-note-type))
    (let ((note-table (activity-note-table activity)))
      (case note-type
	(:all (with-lock-held ((activity-note-lock activity) :write "Write Activity Note")
		(clrhash note-table)))
	(t (with-lock-held ((activity-note-lock activity) :write "Write Activity Note")
	     (maphash #'clear-note-type note-table)))))))


;;;------------------------------------------------------------------- 
;;;
;;; ACCESSING RESOURCES
;;;

(defmethod fetch-resource-headers ((activity activity) (url http-url) headers report-stream)
  (let ((http::*standard-proxy* (activity-proxy activity))
	(outgoing-headers `(,@*standard-head-robot-headers* ,@(robot-headers activity) ,.headers)))
    (declare (dynamic-extent outgoing-headers))
    (handler-case-if (not *debug-walker*) 
       (multiple-value-bind (cached-headers status-code redirection http-version)
	   (http:get-url-headers url outgoing-headers report-stream)
	 (setf (get-value url :headers) cached-headers
	       (get-value url :header-status-code) status-code
	       (get-value url :http-version) http-version)
	 (when redirection
	   (setf (get-value url :redirection) redirection))
	 (values cached-headers status-code redirection nil http-version))
      (host-not-responding (err)
			   (record-url-note activity url :error-getting-headers (report-string err))
			   (values nil 504))
      (http::reportable-condition
	(cond)
	(record-url-note activity url :http-condition-getting-headers (report-string cond))
	(values nil (http::status-code cond)))
      (url::url-condition
	(cond)
	(record-url-note activity url :url-condition-getting-headers (report-string cond))
	(values nil 400))
      (error (err)
	     (record-url-note activity url :error-getting-headers (report-string err))
	     (values nil 500)))))

(define-generic get-resource-headers (activity url &key refetch-p headers report-stream)
  (declare (values headers status-code redirection retrieved-from-cache-p http-version)))

(defmethod get-resource-headers ((activity activity) (url http-url) &key refetch-p headers (report-stream *report-stream*)
				 &aux cached-headers)
  (if (and (not refetch-p) (setq cached-headers (get-value url :headers)) *cache-url-data*)
      (values cached-headers
	      (get-value url :header-status-code)
	      (get-value url :redirection)
	      t
	      (get-value url :http-version))
      (with-network-operation-retries ((format nil "(get-resource-content ~S ~S)" activity url)
				       (activity-retries-on-network-error activity)
				       :wait-interval (activity-wait-interval-before-retry activity)
				       :error-values-form (values nil nil (typecase error
									    (unknown-host-name 504)
									    (host-not-responding 504)
									    (t 502)))
				       :stream report-stream)
	(fetch-resource-headers activity url headers report-stream))))

(defmethod fetch-resource-content ((activity activity) (url http-url) headers report-stream)
  (declare (values content headers status-code redirection retrieved-from-cache-p))
  (macrolet ((maybe-dellocate-content-array (content)
	       `(when ,content
		  (http::deallocate-web-walker-cache-array ,content)
		  (setq ,content nil))))
    (let ((http::*standard-proxy* (activity-proxy activity))
	  (outgoing-headers `(,@*standard-get-robot-headers* ,@(robot-headers activity) ,.headers)))
      (declare (dynamic-extent outgoing-headers))
      (handler-case-if (not *debug-walker*) 
	 (multiple-value-bind (body headers status-code redirection http-version)
	     (http:get-url-headers-and-body url outgoing-headers report-stream)
	   (cond ((null status-code)
		  (setf (get-value url :content-status-code) 500)
		  (maybe-dellocate-content-array body)
		  (abort-activity-on-resource))
		 ((< 199 status-code 300)
		  (setf (get-value url :content) body
			(get-value url :headers) headers
			(get-value url :content-status-code) status-code
			(get-value url :http-version) http-version)
		  (when redirection
		    (setf (get-value url :redirection) redirection)))
		 (t (setf (get-value url :content-status-code) status-code)
		    (maybe-dellocate-content-array body)
		    (abort-activity-on-resource)))
	   ;; return values
	   (values body headers status-code redirection nil http-version))
	(host-not-responding (err)
			     (record-url-note activity url :error-getting-content (report-string err))
			     (values nil nil 504))
	(http::reportable-condition
	  (cond)
	  (record-url-note activity url :http-condition-getting-content (report-string cond))
	  (values nil nil (http::status-code cond)))
	(url::url-condition
	  (cond)
	  (record-url-note activity url :url-condition-getting-headers (report-string cond))
	  (values nil 400))
	(error (err)
	       (record-url-note activity url :error-getting-content (report-string err))
	       (values nil nil 500))))))

(define-generic get-resource-content (activity url &key refetch-p headers report-stream)
  (declare (values content headers status-code redirection retrieved-from-cache-p http-version)))

(defmethod get-resource-content ((activity activity) (url http-url) &key refetch-p headers (report-stream *report-stream*)
				 &aux content)
  (declare (values content headers status-code redirection retrieved-from-cache-p))
  (if (and (not refetch-p) (setq content (get-value url :content)) *cache-url-data*)
      (values content
	      (get-value url :headers)
	      (get-value url :content-status-code)
	      (get-value url :redirection)
	      t
	      (get-value url :http-version))
      (with-network-operation-retries ((format nil "(get-resource-content ~S ~S)" activity url)
				       (activity-retries-on-network-error activity)
				       :wait-interval (activity-wait-interval-before-retry activity)
				       :error-values-form (values nil nil (typecase error
									    (unknown-host-name 504)
									    (host-not-responding 504)
									    (t 502)))
				       :stream report-stream)
	(fetch-resource-content activity url headers report-stream))))

;;;------------------------------------------------------------------- 
;;;
;;; ROBOT EXCLUSION
;;;

(defconstant *robot-exclusion-url* "/robots.txt")

(define make-exclusion-url-string (host &optional port)
  (if (and port (not (eql port 80.)))
      (concatenate 'string "http://" host ":" (write-to-string port :base 10) *robot-exclusion-url*)
      (concatenate 'string "http://" host  *robot-exclusion-url*)))

(defmethod robot-exclusion-url ((url http-url))
  (let ((host (host-string url))
        (port (url:port url)))
    (url:intern-url (make-exclusion-url-string host port))))

(defmethod robot-exclusion-table ((activity activity))
  (with-activity-value-cached (activity :robot-exclusion-table)
    (make-hash-table :test #'equalp)))

(defmethod note-robot-exclusion-status ((activity activity) host status)
  (let ((exclusion-table (robot-exclusion-table activity)))
    (setf (gethash host exclusion-table) status)
    status))

(defmethod robot-exclusion-status ((activity activity) (url http-url))
  (let ((exclusion-table (robot-exclusion-table activity))
        (host (url:host-object url)))
    (or (gethash host exclusion-table)
        :unknown)))

;; this needs to be smarter as the robot exclusion standard allows the robots
;; file to specify which URL hierarchies are allowed and disallowed.   7/17/95 -- JCMa.
(defmethod robot-excluded-p ((activity activity) (url http-url))
  (case (robot-exclusion-status activity url)
    (:excluded nil)
    (:allowed t)
    (:unknown
      (let ((exclusion-url (robot-exclusion-url url)))
        (multiple-value-bind (headers status-code)
            (get-resource-headers activity exclusion-url)
          (declare (ignore headers))
          (case status-code
            (404 (note-robot-exclusion-status activity (host-object url) :allowed)
                 t)
            (t (note-robot-exclusion-status activity (host-object url) :excluded)
               nil)))))))

(defmethod robot-headers ((activity activity) &optional recompute-p)
  (with-slots (operator user-agent) activity
    (with-activity-value-cached (activity :robot-headers :recompute-p recompute-p)
      `(,.(when operator
            `(:from ,(ensure-list operator)))   ; from field is always a list -- JCMa 4/2/2000.
        ,.(when user-agent
            `(:user-agent ,user-agent))))))


;;;------------------------------------------------------------------- 
;;;
;;; SCANNING HTML
;;;

(defconstant *open-element-delimiter* #\<)
(defconstant *close-element-delimiter* #\>)
(defvar *standard-delimiters* '(#\space #\tab #\Return #\Linefeed #\= #\< #\>))
(defvar *ignore-chars-in-element* '(#\space #\tab #\Return #\Linefeed))

(declaim (inline white-space-char-in-element-p))

(define white-space-char-in-element-p (char)
  (member char *ignore-chars-in-element* :test #'char=))

(define clear-white-space (stream)
  (loop for char = (peek-char nil stream nil t)
        while (white-space-char-in-element-p char)
        do (read-char stream t nil t)))

(declaim (inline forward-slash-p))

(define forward-slash-p (char)
  (char-equal char #\/))

;;optimize?
;(define merge-walker-url (url &optional (default *local-context*))
;  (check-type default string)
;  (check-type url string)
;;;;catch improperly specified contexts
;  (unless (char-equal #\/ (char default (1- (length default))))
;    (setq default (concatenate 'string default "/")))
;  (with-fast-array-references ((string url string))
;    (cond ;; no url provided
;      ((zerop (length string)) (return-from merge-walker-url default))
;      ;; default the pathname
;      ((eql (aref string 0) #\/)  ;;url refers to root in default
;       (let* ((start-root (+ 2 (search "//" default :test #'char-equal)))
;              (root (subseq default 0 (or (position-if #'forward-slash-p default :start start-root) 
;                                          (length default)))))
;         (return-from merge-walker-url (concatenate 'string root url))))
;      ((let ((l (length string)))
;         (loop for idx upfrom 0 to (the fixnum (1- l))
;               for char = (aref string idx)
;               do (cond ((member char '(#\/ #\?) :test #'eql)
;                         (return nil))
;                        ((and (eql char #\:)    ;found the scheme, ergo fully specified
;                              (< 2 (the fixnum (- l idx)))
;                              (eql (aref string (the fixnum (1+ idx))) #\/)
;                              (eql (aref string (the fixnum (+ 2 idx))) #\/))
;                         (unless (position-if #'forward-slash-p string :start (+ 3 idx))
;                           (setf url (concatenate 'string url "/")))
;                         (return-from merge-walker-url url)))
;               finally (return nil))))
;      ;; url name
;      (t (let ((base (subseq default 0 (1+ (position-if #'forward-slash-p default :from-end t)))))
;           (return-from merge-walker-url (concatenate 'string base url)))))))

;; Fix the test cases below from draft-uri-syntax-03.text   8/11/98 -- Martin
;; Specifically
;;              * "." is handled now.
;;              * ".." is handled at the end of the url.
;;              * A change of authority component is handled.
;;              * It doesn't try to remove .. from things like http://a/b/c/g../z.

(define merge-walker-url (url &optional (default *local-context*) (len-default (length default)))
  (declare (fixnum len-default))
  (check-type default string)
  (check-type url string)
  ;;catch improperly specified contexts
  (unless (char-equal #\/ (char default (1- len-default)))
    (setq default (concatenate 'string default "/")
	  len-default (1+ len-default)))
  (let* ((len (length url)))
    (declare (fixnum len))
    (with-fast-array-references ((url url string))
      (cond ;; no url provided
        ((zerop len)
	 (return-from merge-walker-url (values default default)))
        ;; default the pathname
        ((eql (aref url 0) #\/)  ; URL refers to root in default
	 (let* ((start-root (+ 2 (the fixnum (http::string-search= "//" default 0 2 0 len-default))))
		(new-authority-p (and (> len 1) (eql (aref url 1) #\/)))	; new authority?
		(end-default (if new-authority-p
				 start-root
				 (or (position-if #'forward-slash-p default :start start-root :end len-default) len-default)))
		(start-url (if new-authority-p 2 0))
		(abs-url-size (+ (the fixnum end-default) (- len (the fixnum start-url))))
                (abs-url (make-array abs-url-size :element-type http::*standard-character-type* :fill-pointer t)))
	   (nfill-array abs-url default :start1 0 :end1 end-default :start2 0 :end2 end-default)
	   (nfill-array abs-url url :start1 end-default :end1 abs-url-size :start2 start-url :end2 len)
	   (setf (fill-pointer abs-url) abs-url-size)
           (return-from merge-walker-url (values abs-url abs-url))))
        ((loop for idx fixnum upfrom 0 to (1- len)
               for char = (aref url idx)
               do (case char
		    ((#\/ #\?) (return nil))
		    (#\: ;; Found the scheme, ergo fully specified
		     (when (and (< 2 (- len idx)) (eql (aref url (1+ idx)) #\/) (eql (aref url (+ 2 idx)) #\/))
		       (unless (position-if #'forward-slash-p url :start (+ 3 idx) :end len)
			 (setf url (concatenate 'string url "/")))
		       (return-from merge-walker-url (values url url)))))
	       finally (return nil)))
        ;; url name
        (t (let* ((dir-end (1+ (position-if #'forward-slash-p default :start 0 :end len-default :from-end t)))
		  (abs-url-size (+ dir-end len))
		  (abs-url (make-array abs-url-size :element-type http::*standard-character-type* :fill-pointer t)))
	     (nfill-array abs-url default :start1 0 :end1 dir-end :start2 0 :end2 dir-end)
	     (nfill-array abs-url url :start1 dir-end :end1 abs-url-size :start2 0 :end2 len)
	     (setf (fill-pointer abs-url) abs-url-size)
             (return-from merge-walker-url
               (values (handle-unix-superior-directory-reference abs-url) abs-url))))))))

#|
(defvar *unix-url-tests*
	'(("g" "http://a/b/c/g")
	  ("./g" "http://a/b/c/g")
	  ("g/" "http://a/b/c/g/")
	  ("/g" "http://a/g")
	  ("//g" "http://g")
	  ("?y" "http://a/b/c/?y")
	  ("g?y" "http://a/b/c/g?y")
	  ("#s" "http://a/b/c/#s")
	  ("g#s" "http://a/b/c/g#s")
	  ("g?y#s" "http://a/b/c/g?y#s")
	  (";x" "http://a/b/c/;x")
	  ("g;x" "http://a/b/c/g;x")
	  ("g;x?y#s" "http://a/b/c/g;x?y#s")
	  ("." "http://a/b/c/")
	  ("./" "http://a/b/c/")
	  (".." "http://a/b/")
	  ("../" "http://a/b/")
	  ("../g" "http://a/b/g")
	  ("../.." "http://a/")
	  ("../../" "http://a/")
	  ("../../g" "http://a/g")
	  ("../../../g" "http://a/b/c/../../../g")
	  ("/./g" "http://a/./g")
	  ("/../g" "http://a/../g")
	  ("g." "http://a/b/c/g.")
	  (".g" "http://a/b/c/.g")
	  ("g.." "http://a/b/c/g..")
	  ("..g" "http://a/b/c/..g")
	  ("g./z" "http://a/b/c/g./z")
	  (".g/z" "http://a/b/c/.g/z")
	  ("g../z" "http://a/b/c/g../z")
	  ("..g/z" "http://a/b/c/..g/z")))

(defun test-merge-walker-url ()
  (loop for (string res) in *unix-url-tests*
	do (multiple-value-bind (http-p relative-p)
	       (url:http-url-string-p string)
	     (when (or http-p relative-p)
	       (let ((merged-url-string (w4::merge-walker-url string "http://a/b/c/")))
		 (format t "~&~:[LOSE~;WIN~]:~6T~S ~40T+ ~S ~55T=> ~S" (equal merged-url-string res) res string merged-url-string))))))
|#


;;;------------------------------------------------------------------- 
;;;
;;; STREAM-ORIENTED HYPERLINK EXTRACTION
;;;

(eval-when (compile eval load)
  (define make-stream-matcher (patterns reader match)
    (loop with comparitor = (ecase match
                              (:exact 'char=)
                              (:case-insensitive 'char-equal))
          for pattern in (ensure-list patterns)
          for p = (coerce pattern 'list)
          for key = (car p)
          collect `((,comparitor ,key char)
                    ,(loop for char in (cdr p)
                           collect `(,comparitor ,char ,reader) into test
                           finally (return `(and ,.test))))
            into clauses
          finally (return `(let ((char ,reader))
                             (cond ,.clauses (t nil)))))))

(define-macro match-stream-pattern-p (stream pattern &key (match :exact))
  (make-stream-matcher pattern `(read-char ,stream nil #\Return t) match))

;(defun %handle-unix-superior-directory-reference (url-string &optional (start 0) end)
;  (let* ((len nil)
;         (end1 (or end (setq len (length url-string))))
;         pos)
;    (declare (fixnum start end1))
;    (cond ((setq pos (http::string-search=  "/../" url-string 0 4 start end1))
;           (let* ((pos1 (position #\/ url-string :test #'eql :start start :end pos :from-end t))
;                  (size (+ (- pos1 start) (- end1 (+ pos 4)) 1))
;                  (n-string (make-string size)))
;             (declare (fixnum pos1 pos))
;             (with-fast-array-references ((url-string url-string string)
;                                          (n-string n-string string))
;               (loop for idx1 upfrom start upto pos1
;                     for idx2 upfrom 0 below size
;                     do (setf (aref n-string idx2) (aref url-string idx1)))
;               (loop for idx1 upfrom (+ pos 4) below end1
;                     for idx2 upfrom (1+ (- pos1 start)) below size
;                     do (setf (aref n-string idx2) (aref url-string idx1))))
;             (values n-string t t)))
;          ((and (zerop start) (or (null end) (null len)))
;           url-string)
;          (t (values (subseq url-string start end1) t)))))

;; Brain dead approach. rewrite this sometime to do it directly without all
;; this consing. barf.   8/19/96 -- JCMa.
;(defun handle-unix-superior-directory-reference (url-string &optional (start 0) end)
;  "Handles the /../ directory component in URLs.
;This drops the immediately higher directory and returns a new url-string."
;  (declare (values string new-string-p))
;  (multiple-value-bind (nstring new-p ref-trimmed-p)
;      (%handle-unix-superior-directory-reference url-string start end)
;    (cond (ref-trimmed-p
;           (loop doing (multiple-value-setq (nstring new-p ref-trimmed-p)
;                         (%handle-unix-superior-directory-reference nstring))
;                 unless ref-trimmed-p
;                   return (values nstring new-p t)))
;          (t (values nstring new-p)))))

;(defun handle-unix-superior-directory-reference (url-string &optional (start 0) end)
;  "Handles the /../ directory component in URLs.
;This drops the immediately higher directory and returns a new url-string."
;  (declare (values string new-string-p))
;  (let ((str (subseq url-string start end)))
;    (declare (dynamic-extent str))
;    (loop with dots-found-p = nil and dst = 0 and dot-count = 0 and slash-count = 0
;	  for idx from 0 below (length str)
;	  for char = (char str idx)
;	  doing (case char
;		  (#\/ 
;		   (incf slash-count)
;		   (when (eq dot-count 2)
;		     (setf dots-found-p t)
;		     (setf dot-count 0)
;		     (when (< slash-count 5)
;		       (return (values url-string nil)))
;		     (decf slash-count 2)
;		     (loop with up-count = 0
;			   for i from (1- dst) downto 0
;			   as c = (char str i)
;			   doing (when (eq c #\/) (incf up-count))
;				 (when (eq up-count 2)
;				   (setf dst i)
;				   (return)))))
;		  (#\. (incf dot-count))
;		  (t (setf dot-count 0)))
;		(setf (char str dst) char)
;		(incf dst)
;		#+ignore (print (list idx dst c slash-count dot-count (subseq str 0 dst)))
;      	  finally (return (if dots-found-p
;			      (values (subseq str 0 dst) t)
;			      (values url-string nil))))))

;; Definition revised   8/11/98 -- Martin
(defun handle-unix-superior-directory-reference (url-string &optional (start 0) end)
  "Handles the /../ directory component in URLs.
This drops the immediately higher directory and returns a new url-string."
  (declare (values string new-string-p))
  (let ((str (subseq url-string start end)))
    (declare (dynamic-extent str))
    (macrolet ((elide-dots
		  (final-slash-p)
		 `(case dot-count
		    (1 (setf dots-found-p t)
		       (setf dot-count 0)
		       (when ,(if final-slash-p
				  `(and (>= slash-count 4) (decf slash-count 1))
				  `(>= slash-count 3))
			 (decf dst ,(if final-slash-p 2 1))))
		    (2 (setf dots-found-p t)
		       (setf dot-count 0)
		       (decf slash-count ,(if final-slash-p 2 1))
		       (when (< slash-count 3)
			 (return (values url-string nil)))
		       (loop with up-count = 0
			     for i from (1- dst) downto 0
			     as c = (char str i)
			     doing (when (eq c #\/) (incf up-count))
				   (when (eq up-count 2)
				     (setf dst ,(if final-slash-p `i `(1+ i)))
				     (return)))))))
      (loop with dots-found-p = nil and dst = 0 and dot-count = 0 and slash-count = 0 and slash-before-dot-p = nil
	    for idx from 0 below (length str)
	    for char = (char str idx)
	    doing (case char
		    (#\/ 
		     (incf slash-count)
                     (elide-dots t)
                     (setq slash-before-dot-p t))
		    (#\. (when slash-before-dot-p (incf dot-count)))
		    (t (setf dot-count 0)
                       (setq slash-before-dot-p nil)))
		  (setf (char str dst) char)
		  (incf dst)
		  #+ignore (print (list idx dst c slash-count dot-count (subseq str 0 dst)))
      	    finally (elide-dots nil)
		    (return (if dots-found-p (values (subseq str 0 dst) t) (values url-string nil)))))))


;(define intern-url-referenced-url (string)
;  (multiple-value-bind (http-p relative-p)
;      (url:http-url-string-p string)
;    (cond ((or http-p relative-p)
;           (let ((url (merge-walker-url string)))
;             (setq url (handle-unix-superior-directory-reference url))
;             (handler-case-if (not *debug-walker*)
;                (let ((url:*escape-search-urls* nil))
;                  (url:intern-url url :if-does-not-exist :create))
;               (url::bad-host-port-specification
;                 ()
;                 (record-url-note *activity* (current-url) :bad-url-syntax (merge-walker-url string))
;                 nil)
;               (url::host-parsing-error ()
;                                        (record-url-note *activity* (current-url) :bad-url-syntax (merge-walker-url string))
;                                        nil)
;               (url::no-parser-for-scheme ()
;                                          (record-url-note *activity* (current-url) :bad-url-syntax (merge-walker-url string))
;                                          nil))))
;          (t nil))))


(define web-walkable-url-string-p (url-string &optional (start 0) (end (length url-string)))
  "Returns non-null if url-string is a fully qualified URL string."
  (declare (fixnum start end))
  (with-fast-array-references ((url-string url-string string))
    (let* ((colon (http::char-position #\: url-string start end)))
      (cond ((null colon)
             (if (< start end)    ;; it's potentially a relative HTTP URL if it starts with / or a legal url character.
                 (values nil t)
               (values nil nil)))
	    ((and (< (+ 2 (the fixnum colon)) end)
		  (eql (char url-string (1+ (the fixnum colon))) #\/)
		  (eql (char url-string (+ 2 (the fixnum colon))) #\/))
             (cond ((string-equal "http" url-string :start1 0 :end1 4 :start2 start :end2 colon)
                    (values :http nil))
                   #+CL-HTTP-SSL-Client
                   ((string-equal "https" url-string :start1 0 :end1 5 :start2 start :end2 colon)
                    (values :https nil))
                   #+ignore
                   ((string-equal "ftp" url-string :start1 0 :end1 3 :start2 start :end2 colon)
                    (values :ftp nil))
                   (t nil)))
	    (t nil)))))

(define intern-url-referenced-url (string)
  (flet ((signal-bad-syntax (url-string)
	   (record-url-note *activity* (current-url) :bad-url-syntax url-string)
	   nil))
    (multiple-value-bind (http-p relative-p)
	(web-walkable-url-string-p string)
      (when (or http-p relative-p)
	(let ((url-string (merge-walker-url string)))
	  (multiple-value-bind (n-url-string munged-p)
	      (handle-unix-superior-directory-reference url-string)
	    (handler-case-if (not *debug-walker*)
	       (let ((url:*escape-search-urls* nil)
		     (url:*search-url-signal-bad-form-encoding* nil))
		 (url:intern-url (if munged-p n-url-string url-string) :if-does-not-exist :create))
	      (url:search-url-bad-form-encoding () (signal-bad-syntax url-string))
	      (url:bad-host-port-specification () (signal-bad-syntax url-string))
	      (url:host-parsing-error () (signal-bad-syntax url-string))
	      (url:no-parser-for-scheme () (signal-bad-syntax url-string))
	      (http::bad-escaping () (signal-bad-syntax url-string)))))))))

(define get-url-string (stream)
  (when (eql #\" (peek-char nil stream nil  #\return t))
    (read-char stream nil nil t))
  (clear-white-space stream)
  (let ((chars (loop for char = (read-char stream nil nil t)
                     while char
                     until (member char '(#\space #\tab #\" #\> #\< #\#) :test #'eql)
                     collect char)))
    (declare (dynamic-extent chars))
    (if chars (coerce chars 'string) nil)))

(define read-url (stream)
  (declare (values url newly-interned-p))
  (let ((string (get-url-string stream)))
    (when string
      (intern-url-referenced-url string))))

(define get-referenced-urls-from-html (stream)
  (loop for char = (read-char stream nil nil t)
        until (or (null char) (char= char *close-element-delimiter*))
        when (and (member char *ignore-chars-in-element*)
		  (prog2 (clear-white-space stream)
			 (match-stream-pattern-p stream ("href=" "src=") :match :case-insensitive)))
          do (clear-white-space stream)
             (return-from get-referenced-urls-from-html (read-url stream))
        finally (return nil)))

(define read-token (stream &optional (delimiters *standard-delimiters*))
  (flet ((delimiter-p (char)
           (when (member char delimiters :test #'char-equal)
             (unread-char char stream)
             t)))
    (declare (inline delimiter-p))
    (loop for char = (read-char stream nil (first delimiters) t)
          until (delimiter-p char)
          collect (char-upcase char) into chars
          finally (return (intern (coerce chars 'string) *keyword-package*)))))

(define-generic collect-hyperlinks (url resource)
  (:documentation "Scans RESOURCE for HTML hyperlinks and returns a list of them.
URL is the base against which relative URLs are merged."))

(defmethod collect-hyperlinks ((url http-url) stream)
  (loop for char = (read-char stream nil :eof t)
        until (eq char :eof)
        for url = (when (and (char= *open-element-delimiter* char)      ;find open html element
                             (not (char= #\/ (peek-char nil stream t nil t)))   ;skip close elements
                             (member (read-token stream *standard-delimiters*)
                                     '(:a :img :isindex :area :frame :script :embed)
                                     :test #'eq))
                    (get-referenced-urls-from-html stream))
        when (and url (not (member url hyperlinks :test #'eq)))
          collect url into hyperlinks
        finally (return hyperlinks)))

(defmethod collect-hyperlinks ((url http-url) (pathname pathname))
  (with-open-file (file-stream pathname :direction :input :if-does-not-exist :error 
                               :element-type http::*standard-character-type*)
    (collect-hyperlinks url file-stream)))

(defmethod collect-hyperlinks :around ((url http-url) stream)
  (let ((local-context (url::directory-context-string url)))
    (unwind-protect 
        (with-local-context (local-context)
          (call-next-method url stream))
      (remove-value url :directory-context-string))))


;;;------------------------------------------------------------------- 
;;;
;;; STRING-ORIENTED HYPERLINK COLLECTION
;;;

(define read-token-from-string (string &optional (start 0) (end (length string)) (delimiters *standard-delimiters*))
  (declare (values token next-index))
  (loop for idx upfrom (position-if-not #'(lambda (x) (member x delimiters :test #'eql)) string :start start :end end)
                below end
        for char = (aref string idx)
        until (member char delimiters :test #'eql)
        collect (char-upcase char) into chars
        finally (return (values (intern (coerce chars 'string) *keyword-package*)
                                idx))))

(define read-url-from-string (string &optional (start 0) (end (length string)))
  (declare (values url newly-interned-p next-index))
  (flet ((copy-url-string (string start end)
	   (declare (fixnum start end))
	   (let ((nstring (make-array (- end start) :fill-pointer t :element-type http::*standard-character-type*)))
	     (with-fast-array-references ((string string string)
					  (nstring nstring string))
	       (loop with fill-pointer = 0
		     for idx1 upfrom start below end
		     for char = (aref string idx1)
		     unless (member char '(#\return #\Linefeed))
		       do (setf (aref nstring fill-pointer) char)
			  (incf (the fixnum fill-pointer))
		     finally (setf (fill-pointer nstring) fill-pointer)
			     (return nstring))))))
    (declare (inline copy-url-string))
    (loop with s = (or (position-if-not #'(lambda (x)
					    (member x '(#\space #\" #\tab #\return #\Linefeed) :test #'eql))
					string :start start :end end)
		       end)
	  for idx upfrom s below end
	  for char = (aref string idx)
	  until (member char '(#\space #\tab #\" #\> #\< #\#) :test #'eql)
	  finally (let ((string (copy-url-string string s idx)))
		    (multiple-value-bind (url newly-interned-p)
			(intern-url-referenced-url string)
		      (if url
			  (return (values url newly-interned-p (1+ idx)))
			  (return (values nil nil (1+ idx)))))))))

(define get-referenced-urls-from-html-string (string &optional (start 0) (end (length string)))
  (declare (values url next-index))
  (macrolet ((match-key (keys string index)
	       (loop for key in keys
		     for length = (length key)
		     collect `(and (< (+ ,length ,index) end)	; MCL loses if end2 is greater than (length string)
				   (string-equal ,key ,string :start1 0 :end1 ,length
						 :start2 ,index :end2 (+ ,length ,index))
				   (incf ,index ,length))
		       into clauses
		     finally (return `(or ,.clauses))))
	     (string-clear-whitespace (string idx)	;skip any amount of white space when white space encountered
	       `(when (member char *ignore-chars-in-element*)
		  (incf ,idx)
		  (loop while (member (aref ,string ,idx) *ignore-chars-in-element*)
			do (incf ,idx))
		  t)))
    (loop for idx upfrom start below end
	  for char = (aref string idx)
	  until (or (null char) (eql char *close-element-delimiter*))
	  when (and (string-clear-whitespace string idx)
		    (match-key ("href=" "src=") string idx))
	    do (multiple-value-bind (url newly-interned-p next-index)
		   (read-url-from-string string idx end)
		 (declare (ignore newly-interned-p))
		 (when url
		   (return-from get-referenced-urls-from-html-string
		     (values url next-index))))
	  finally (return (values nil (1+ idx))))))

(define collect-hyperlinks-from-string (string &optional (start 0) (end (length string)))
  (using-resource (duplicate-table w4-hash-table)
    (flet ((duplicate-p (url duplicate-table)
             (cond ((gethash url duplicate-table) t)
                   (t (setf (gethash url duplicate-table) t)
                      nil))))
      (declare (inline duplicate-p))
      (loop with url and token
            for idx upfrom start below (1- end) ;no go off the end in the conditional
            for char = (aref string idx)
            when (and (eql char *open-element-delimiter*)       ;find open html element
                      (not (eql  #\/ (aref string (1+ idx)))))  ;skip close elements
              do (multiple-value-setq (token idx)
                   (read-token-from-string string (1+ idx) end *standard-delimiters*))
                 (when (member token '(:a :img :isindex :area :frame :script :embed) :test #'eq)
                   (multiple-value-setq (url idx)
                     (get-referenced-urls-from-html-string string idx end)))
            when url
              unless (duplicate-p url duplicate-table)
                collect url into hyperlinks
                  end
                and do (setq url nil)
            finally (return (values hyperlinks (1+ idx)))))))

(defmethod collect-hyperlinks ((url http-url) (string string))
  (collect-hyperlinks-from-string string))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(define scan-for-url-inferiors (activity url)
  (macrolet
    ((do-html (&body body)
       `(multiple-value-bind (headers status-code)
            (get-resource-headers activity url)
          (case status-code
            (200
              (destructuring-bind (&optional major minor &rest plist)
                  (get-header :content-type headers)
                (declare (ignore plist))
                (cond ((and (eq major :text)
                            (eq minor :html))
                       ,@body)
                      (t nil))))
            (t (when *debug-walker*
                 (format *error-output* "~&HEAD Status ~D for ~A in W4::SCAN-FOR-URL-INFERIORS"
                         status-code url))
               nil)))))
    (do-html 
      (multiple-value-bind (document *headers* status-code)
          (get-resource-content activity url)
        (cond ((null status-code)
               (error "A status code should always be returned. Check the code."))
              ((<= 200 status-code 300)
               (collect-hyperlinks url document)))))))

(define-generic url-inferiors (activity url &optional recompute-p)
  (declare (values urls retrieved-from-cache-p))
  (:documentation "Returns the URL inferiors found in HTML documents."))

(defmethod url-inferiors ((activity activity) (url url:http-object) &optional recompute-p)
  (with-value-cached (url :url-inferiors :recompute-p (or recompute-p (not *cache-url-data*)))
    (scan-for-url-inferiors activity url)))

(defmethod url-inferiors ((activity activity) (url url:http-path) &optional recompute-p)
  (with-value-cached (url :url-inferiors :recompute-p (or recompute-p (not *cache-url-data*)))
    (scan-for-url-inferiors activity url)))

(defmethod url-inferiors ((activity activity) (url url:http-search) &optional recompute-p)
  (with-value-cached (url :url-inferiors :recompute-p (or recompute-p (not *cache-url-data*)))
    (scan-for-url-inferiors activity url)))

(defmethod activity-report-condition ((activity activity) (http-url url:http-url) cond)
  (with-slots (report-stream) activity
    (www-utils:report-condition cond report-stream)))

(define-generic perform-action (url action activity))

(defmethod perform-action ((url url:http-url) (action action) (activity activity))
  (with-slots (type arguments) action
    (cond (type
           (with-slots (function) type
             (apply (or function
                        (error "No action function available for ~S." type))
                    action activity url arguments)))
          (t (error "No action type available for ~S." action))))) 

(defmethod perform-action ((url url:http-url) (action open-http-action) (activity activity))
  (with-slots (function arguments follow-redirects-p method) action
    (let ((outgoing-headers `(:host (,(url:host-string url) ,(url:host-port url))
			      ,.(activity-outgoing-headers action))))
      (declare (dynamic-extent outgoing-headers))
      (handler-case-if 
	  *debug-walker*
	 (if follow-redirects-p
	     (handling-redirects (url)
	       (with-http-request (url method)
		 outgoing-headers
		 (apply function action activity url http::remote-stream arguments)))
	     (with-http-request (url method)
	       outgoing-headers
	       (apply function action activity url http::remote-stream arguments)))
	(client-condition (cond) (activity-report-condition activity url cond))))))

(define-generic url-inferiors-satisfying-activity (url activity &optional context-constraints-applied-p))

(defmethod url-inferiors-satisfying-activity ((url url:http-url) (activity activity) &optional context-constraints-applied-p)
  (when (or context-constraints-applied-p (satisfies-context-constraints-p t activity))
    (loop with constraint-set = (activity-constraint-set activity)
	  for inferior in (url-inferiors activity url)
	  when (satisfies-p inferior activity constraint-set)
	    collect inferior)))

(define-generic walk (url activity)
  (:documentation "Top-level function for walking a structure."))

(defmethod walk (url (activity string))
  (walk url (intern-activity activity)))

(defmethod walk ((url url:http-url) activity)
  (walk (url:name-string url) activity))

(defmethod walk ((url string) (activity activity))
  (flet ((doit (activity url)
	   (flet ((clear-activity-caches (key value)
		    (declare (ignore key))
		    (clear-cache activity value t)))
	     (declare (dynamic-extent #'clear-activity-caches))
	     (unwind-protect
		 (when (and (satisfies-context-constraints-p url activity)
			    (satisfies-activity-p url activity))
		   (walk-using-queue activity (activity-queue activity) url)
		   (map-url-table #'clear-activity-caches)
		   (setf (activity-uri-universe-object activity) nil))))))
    (declare (inline doit))
    ;; initialize activity
    (setf (activity-aborted-p activity) nil
	  (property-list activity) nil)
    (clear-url-notes activity :all)
    ;; Allocate time for the web walk. Prevent server connection scavenger from killing us.
    (when http:*server*
      (setf (http:server-timeout http:*server*) (activity-life-time activity)))
    ;; primary method to perform the walk
    (flet ((primary-method ()
	     (let ((uri-universe (activity-uri-universe activity)))
	       (typecase uri-universe
		 (uri-universe
		   (url:with-uri-universe (uri-universe)
		     (setf (activity-uri-universe-object activity) uri-universe)
		     (doit activity (url:intern-url url))))
		 (string
		   (using-resource (uri-universe-object uri-universe uri-universe)
		     (setf (activity-uri-universe-object activity) uri-universe-object)
		     (url:with-uri-universe (uri-universe-object)
		       (doit activity (url:intern-url url)))))
		 (null (doit activity (url:intern-url url)))))))
      (cond ((activity-report-stream activity)
	     (let ((*report-stream* (report-stream activity)))
	       (primary-method)))
	    (t (with-null-stream (*report-stream*)
		 (setf (activity-report-stream activity) '*report-stream*)
		 (unwind-protect
		     (primary-method)
		   (setf (activity-report-stream activity) nil))))))
    activity))

(define-generic walk-using-queue (activity queue url)
  (:documentation "Main method for initiating a walk using a work queue."))

(defmethod walk-using-queue ((activity activity) (queue single-threaded-queue) (url url:http-url))
  (clear-queue activity)
  (push-queue url queue activity *depth* '() t :pending)
  (execute-pending-tasks queue)
  activity)

(defmethod walk-using-queue ((activity activity) (queue single-activity-multi-threaded-queue) (url url:http-url))
  (push-queue url queue activity *depth* '() t :pending)
  (process-wait "Primary Queue Wait" #'(lambda () (or (activity-aborted-p activity)
						      (queue-exhausted-p queue))))
  (unless (retry-queue-exhausted-p queue)
    (process-wait "Retry Queue Wait" #'(lambda () (or (activity-aborted-p activity)
						      (retry-queue-exhausted-p queue)))))
  activity)

(defmethod walk-using-queue ((activity activity) (queue shared-multi-threaded-queue) (url url:http-url))
  (error "Shared Multi-threaded queues are not yet implemented.")
  activity)

(defgeneric abort-activity (activity)
  (:documentation "Aborts ACTIVITY, causing it to return from its web walk."))

(defmethod abort-activity ((activity activity))
  (etypecase (activity-queue activity)
    (shared-multi-threaded-queue
      (error "Aborting shared queue muilti-theaded activities is not implemented."))
    (multi-threaded-queue
      (setf (activity-aborted-p activity) t))
    (single-threaded-queue
      (error "Aborting single theaded foreground activities is not implemented."))))


;;;------------------------------------------------------------------- 
;;;
;;; QUEUE
;;;

(defun %make-queue-entry (resource url activity depth parent-stack &optional satisfies-constraints-p (state :pending))
  (declare (ignore resource))
  (make-instance 'queue-entry :url url
		 :activity activity
                 :depth depth
                 :parent-stack parent-stack
                 :satisfies-constraints-p satisfies-constraints-p
                 :state state))

(defun %initialize-queue-entry (resource queue-entry url activity depth parent-stack satisfies-constraints-p state)
  (declare (ignore resource))
  (setf (qe-url queue-entry) url
	(qe-activity queue-entry) activity
        (qe-depth queue-entry) depth
        (qe-parent-stack queue-entry) parent-stack
        (qe-satisfies-constraints-p queue-entry) satisfies-constraints-p
        (qe-state queue-entry) state)
  queue-entry)

(defun match-w4-queue-entry-p (resource queue-entry url activity depth parent-stack &optional satisfies-constraints-p (state :pending))
  (declare (ignore resource queue-entry url activity depth parent-stack satisfies-constraints-p state))
  t)

(defresource w4-queue-entry (url activity depth parent-stack &optional satisfies-constraints-p (state :pending))
  :constructor %make-queue-entry
  :matcher match-w4-queue-entry-p
  :initializer %initialize-queue-entry)

(define clear-queue-entry-resource ()
  "Clears the resource of HTTP queue-entry objects."
  (clear-resource 'w4-queue-entry)) 

(defun allocate-queue-entry (url activity depth parent-stack &optional (satisfies-constraints-p nil) (state :pending))
  (allocate-resource 'w4-queue-entry url activity depth parent-stack satisfies-constraints-p state))

(define-generic deallocate-queue-entry (queue-entry)
  (declare (values queue-entry))
  (:documentation  "Deallocates queue-entry to the queue-entry resoure."))

(defmethod deallocate-queue-entry ((queue-entry queue-entry))
  (setf (qe-url queue-entry) nil
        (qe-depth queue-entry) 0
        (qe-parent-stack queue-entry) nil
        (qe-satisfies-constraints-p queue-entry) nil
        (qe-state queue-entry) :deallocated
        (qe-retries queue-entry) 0)
  (deallocate-resource 'w4-queue-entry queue-entry)
  queue-entry)

(defmethod initialize-instance :after ((queue multi-threaded-queue) &key)
  (setf (queue queue) (make-instance 'primary-queue)
	(retry-queue queue) (make-instance 'retry-queue)))

(defun %make-queue (resource activity class)
   (declare (ignore resource activity))
   (make-instance class))

(defun match-w4-queue-p (resource queue activity class)
   (declare (ignore resource activity))
   (eq (type-of queue) class))

(defmethod %initialize-queue (resource (queue basic-queue) activity class)
   (declare (ignore resource class))
   (setf (queue-activity queue) activity)
   queue)

(defmethod %initialize-queue (resource (queue multi-threaded-queue) activity class)
  (declare (ignore resource class))
  (setf (queue-activity queue) activity
	(tq::task-queue-thread-number (queue queue)) (activity-number-of-threads activity))
  (tq:start-task (queue queue))
  (tq:start-task (retry-queue queue))
  queue)

(defmethod %initialize-queue :before (resource (queue best-first-queue) activity class)
  (declare (ignore resource class))
  (unless (setf (queue-predicate queue) (activity-best-first-predicate activity))
    (error "~S contains no predicate for the best-first queue ~S." activity queue)))

(defresource w4-queue (activity class)
  :constructor %make-queue
  :matcher match-w4-queue-p
  :initializer %initialize-queue)

(define clear-queue-resource ()
  "Clears the resource of W4 queues"
  (clear-resource 'w4-queue))

(defvar *shared-queue-alist* nil
   "Holds the currently active shared queues.") 

(declaim (inline %sharing-queue-p))

(defun %sharing-queue-p (queue)
   "Returns non-null if QUEUE is available for shared use by multiple activities."
   (not (null (rassoc queue *shared-queue-alist*))))

(declaim (inline %get-shared-queue))

(defun %get-shared-queue (class)
   "Returns the shared queue of CLASS."
   (cdr (assoc class *shared-queue-alist*))) 

(defun allocate-shared-queue (activity class)
   "Returns or creates the shared queue of CLASS for ACTIVITY."
   (or (%get-shared-queue class)
         (let ((queue (allocate-resource 'w4-queue activity class)))
            (push (list* class queue) *shared-queue-alist*)
            queue))) 

(defun allocate-queue (activity type)
  (let ((class (%queue-class-for-type type)))
    (if (subtypep class 'shared-multi-threaded-queue)
	(allocate-shared-queue activity class)
	(allocate-resource 'w4-queue activity class))))

(define-generic deallocate-queue (queue)
  (declare (values queue-entry))
  (:documentation  "Deallocates QUEUE to the w4-queue resoure."))

(defmethod deallocate-queue ((queue basic-queue))
   ;; Remove ay pending work entries
   (clear-queue queue)
   ;; Finally deallocate the queue object
   (deallocate-resource 'w4-queue queue)
   queue)

(defmethod deallocate-queue :before ((queue best-first-queue))
  (setf (queue-predicate queue) nil)) 

(defmethod deallocate-queue ((queue multi-threaded-queue))
  (let ((primary (queue queue))
	(retry-queue (retry-queue queue)))
    ;; Stop the queues in an orderly fashion
    (stop-queue queue)
    ;; Kill off the processes so that we don't consume process resources in the idle resources.
    (tq:task-process-kill primary)
    (tq:task-process-kill retry-queue)
    ;; Clean up the task queues
    (tq:clear-task-queue primary)
    (tq:clear-task-queue retry-queue)
    ;; Finally deallocate the queue object
    (deallocate-resource 'w4-queue queue)
    queue))

(defmethod deallocate-queue ((queue shared-multi-threaded-queue))
   (cond ((%sharing-queue-p queue)
              queue)
            (t (call-next-method))))

(defmethod initialize-queue ((activity activity) type)
  (setf (activity-queue activity) (allocate-queue activity type)))

(defmethod deinitialize-queue ((activity activity) (queue single-activity-queue))
  (with-slots (queue) activity
    (when queue
      (deallocate-queue queue)
      (setq queue nil))))

;; this need to flush any pending tasks for the activity calling it
(defmethod deinitialize-queue ((activity activity) (queue shared-multi-threaded-queue))
   (with-slots (queue) activity
       (when queue
           ;;(deallocate-queue queue)
           (setq queue nil))))

(define-generic clear-queue (queue-or-activity)
  (:documentation "Clears all pending entries in QUEUE-OR-ACTIVITY."))

(defmethod clear-queue ((activity activity))
  (with-slots (queue) activity
    (clear-queue queue)))

(defmethod clear-queue ((queue basic-queue))
  (let ((primary (queue queue))
	(retry-queue (retry-queue queue)))
    (cond-every
      (primary 
	(mapc #'deallocate-queue-entry primary)
	(setf (queue queue) nil
	      (pointer queue) nil))
      (retry-queue
	(mapc #'deallocate-queue-entry retry-queue)
	(setf (retry-queue queue) nil)))
    queue))

(defmethod clear-queue ((queue multi-threaded-queue))
  (let ((primary (queue queue))
	(retry-queue (retry-queue queue)))
    (mapc #'deallocate-queue-entry (tq:clear-task-queue primary))
    (mapc #'deallocate-queue-entry (tq:clear-task-queue retry-queue))
    queue))

(defgeneric stop-queue (multi-threaded-queue)
  (:documentation "Stops all threads associated with the queue, MULTI-THREADED-QUEUE."))

(defmethod stop-queue ((queue multi-threaded-queue))
  (let ((primary (queue queue))
	(retry-queue (retry-queue queue)))
    (tq:stop-task primary)
    (tq:stop-task retry-queue)
    queue))

(defgeneric start-queue (multi-threaded-queue)
  (:documentation "Starts all threads associated with the stropped queue, multi-threaded-queue."))

(defmethod start-queue ((queue multi-threaded-queue))
  (let ((primary (queue queue))
	(retry-queue (retry-queue queue)))
    (tq:start-task primary)
    (tq:start-task retry-queue)
    queue))

;; these will need counters eventually.
(define-generic number-of-queue-entries (q)
  (:documentation "Returns the number of entries in QUEUE."))

(defmethod number-of-queue-entries ((q multi-threaded-queue))
  (with-slots (queue) q
    (tq::task-queue-pending-tasks queue)))

(define-generic number-of-queue-retry-entries (q)
  (:documentation "Returns the number of entries awaiting rerty in QUEUE."))

(defmethod number-of-queue-retry-entries ((q multi-threaded-queue))
  (with-slots (retry-queue) q
    (tq::task-queue-pending-tasks retry-queue)))

(defmacro %push-queue (entry queue)
  `(with-slots (lock queue pointer number-of-queue-entries) ,queue
     (with-lock-held (lock :write "Push Queue")
       (push ,entry queue)
       (unless pointer
	 (setq pointer queue))
       (incf number-of-queue-entries))))

(define-generic push-queue (url queue activity depth parent-stack &optional satisfies-constraints-p state)
  (:documentation "Pushes all pending entries in QUEUE."))

(defmethod push-queue ((url url) (q single-thread-depth-first-queue) activity depth parent-stack
		       &optional satisfies-constraints-p (state :pending))
  (%push-queue (allocate-queue-entry url activity depth parent-stack satisfies-constraints-p state) q)
  url)

(defmethod push-queue ((url url) (q single-thread-breadth-first-queue) activity depth parent-stack &optional satisfies-constraints-p state)
  (with-slots (lock queue pointer number-of-queue-entries) q
    (with-lock-held (lock :write "Push Queue")
      (let* ((entry (allocate-queue-entry url activity depth parent-stack satisfies-constraints-p state))
	     (n-pointer (list entry)))
	(if pointer
	    (nconc pointer n-pointer)
	    (setq queue n-pointer))
	(setq pointer n-pointer)
	(incf number-of-queue-entries)
	url))))

(defmethod push-queue ((url url) (q single-thread-best-first-queue) activity depth parent-stack &optional satisfies-constraints-p state)
  (with-slots (lock queue pointer predicate number-of-queue-entries) q
    (with-lock-held (lock :write "Push Queue")
      (push-ordered (allocate-queue-entry url activity depth parent-stack satisfies-constraints-p state)
		    queue predicate)
      (unless pointer
	(setq pointer queue))
      (incf number-of-queue-entries))
    url))

(defmethod push-queue ((url url) (q multi-threaded-depth-first-queue) activity depth parent-stack &optional satisfies-constraints-p (state :pending))
  (with-slots (queue) q
    (let ((task (allocate-queue-entry url activity depth parent-stack satisfies-constraints-p state)))
      (tq:task-queue-push-front queue task))
    url))

(defmethod push-queue ((url url) (q multi-threaded-breadth-first-queue) activity depth parent-stack &optional satisfies-constraints-p state)
  (with-slots (queue) q
    (let ((task (allocate-queue-entry url activity depth parent-stack satisfies-constraints-p state)))
      (tq:push-task-queue queue task)
      url)))

(defmethod push-queue ((url url) (q multi-threaded-best-first-queue) activity depth parent-stack &optional satisfies-constraints-p state)
  (with-slots (queue predicate) q
    (let ((task (allocate-queue-entry url activity depth parent-stack satisfies-constraints-p state)))
      (tq:task-queue-push-ordered queue task predicate))
    url))

(defmethod push-retry-queue ((queue-entry queue-entry) (queue basic-queue))
  (with-slots (lock retry-queue number-of-queue-retry-entries) queue
    (with-lock-held (lock :write "Push Retry Queue")
      (push queue-entry retry-queue)
      (incf number-of-queue-retry-entries))))

(defmethod push-retry-queue ((queue-entry queue-entry) (queue multi-threaded-queue))
  (with-slots (retry-queue) queue 
    (tq:push-task-queue retry-queue queue-entry)))

(define-generic pop-queue (queue)
  (:documentation "Pops the next entry off QUEUE."))

(defmethod pop-queue ((queue basic-queue))
  (with-slots (lock queue pointer number-of-queue-entries) queue
    (with-lock-held (lock :write "Pop Queue")
      (destructuring-bind (&optional first . rest) queue
	(setf queue rest)
	(unless rest
	  (setf pointer nil))
	(decf number-of-queue-entries)
	first))))

(defmethod pop-queue ((queue single-thread-depth-first-queue))
  (with-slots (lock queue pointer number-of-queue-entries) queue
    (with-lock-held (lock :write "Pop Queue")
      (destructuring-bind (&optional first . rest) queue
	(when (and first (= (qe-depth first) (depth)))	;performs depth cutoff
	  (setf queue rest)
	  (unless rest
	    (setf pointer nil))
	  (decf number-of-queue-entries)
	  first)))))

(defmethod pop-queue ((queue multi-threaded-queue))
  (tq:pop-task-queue (queue queue)))

(defgeneric queue-exhausted-p (queue)
   (:documentation "Retutrns non-null when there are no more work entries in QUEUE."))

(defmethod queue-exhausted-p ((queue basic-queue))
  (null (queue queue)))

(defmethod queue-exhausted-p ((queue multi-threaded-queue))
  (tq:task-queue-exhausted-p (queue queue)))

(defgeneric retry-queue-exhausted-p (queue)
   (:documentation "Retutrns non-null when there are no more work entries in QUEUE."))

(defmethod retry-queue-exhausted-p ((queue basic-queue))
  (null (retry-queue queue)))

(defmethod retry-queue-exhausted-p ((queue multi-threaded-queue))
  (tq:task-queue-exhausted-p (retry-queue queue)))

(defgeneric activity-complete-p (activity &optional require-retry-completion-p)
  (:documentation "Returns non-null when ACTIVITY has completed all its work.
Useful for asynchronous work queues operating in separate threads."))

(defmethod activity-complete-p ((activity activity) &optional require-retry-completion-p)
  (let ((queue (activity-queue activity)))
    (and (queue-exhausted-p queue)
	 (or (null require-retry-completion-p)
	     (retry-queue-exhausted-p queue)))))

(define-generic parent-stack (queue)
  (:documentation "Returns the current url stack.
It copies the stack when required by queue search characteristics."))

(defmethod parent-stack ((queue queue)) *url-stack*)

(defmethod parent-stack ((queue best-first-queue))
  (copy-list *url-stack*))

(defmethod perform-action ((url url:http-url) (action generator) (activity activity))
  (flet ((generate-candidates (url action activity &aux fctn)
           (with-slots (type arguments) action
             (if (setq fctn (action-type-function (or type (error "No action type available for ~S." action))))
                 (apply fctn action activity url arguments)
                 (error "No action function available for ~S." type)))))
    (declare (inline generate-candidates))
    (multiple-value-bind (candidates constraints-applied-p)
        (generate-candidates url action activity)
      (when candidates
        (let* ((queue (activity-queue activity))
               (parent-stack (parent-stack queue))
               (depth *depth*))
          (if constraints-applied-p
              (loop for item in candidates
                    do (push-queue item queue activity depth parent-stack t :pending))
              (let ((constraint-set (activity-constraint-set activity)))
		(when (satisfies-context-constraints-p t activity)
		  (dolist (item candidates)
		    (when (satisfies-p item activity constraint-set)
		      (push-queue item queue activity depth parent-stack t :pending))))))
          ;; execution of the queue here replicates the earlier depth-first enumeration.
          (typecase queue
            (single-thread-depth-first-queue
	      (execute-pending-tasks queue))))))))

(define-generic perform-activity (url activity)
  (:documentation "Primary method for applying ACTIVITY to URL."))

(defmethod perform-activity ((url url:http-url) (activity activity) &aux (status :retry))
  (get-resource-time-stamp activity url t)
  (handling-activity-aborts ()
    (handler-case-if *debug-walker*
       (loop for action in (activity-actions activity)
	     do (perform-action url action activity)
		;; clear the cache after completing the actions.
	     finally (setq status :success)
		     (clear-cache activity url nil))
      (unknown-host-name
	(err)
	(let ((message (format nil "~&Unknown Host Name: Can't resolve ~A~&Error: ~S~&Report: ~A"
			       (name-string url) (type-of err) (report-string err))))
	  (record-url-note activity url :unknown-host-name message)))))
  status)

(defmethod retry-queue ((queue-entry queue-entry))
  (retry-queue (activity-queue (qe-activity queue-entry))))

(defgeneric qe-queue (queue-entry)
  (:documentation "Returns the work queue associated with QUEUE-ENTRY."))

(defmethod qe-queue ((queue-entry queue-entry))
  (activity-queue (qe-activity queue-entry)))

(defun %execute-task (queue-entry)
  (let ((activity (qe-activity queue-entry))
	(url (qe-url queue-entry)))
    (with-activity-bindings (activity url (qe-depth queue-entry) (qe-parent-stack queue-entry))
      (perform-activity url activity))
    :success))

(defun execute-queue-entry (queue-entry)
  (flet ((note-task-error (queue-entry error)
	   (let* ((url (qe-url queue-entry))
		  (activity (qe-activity queue-entry))
		  (message (format nil "~&Error Executing Task for ~A~&Error: ~S~&Report: ~A"
				   (name-string url) (type-of error) (report-string error))))
	     (record-url-note activity message :error message))))
    (let ((status :abort))
      (cond (*debug-walker*
	     (prog ()
		retry
		   (restart-case
		     (setq status (%execute-task queue-entry))
		     (retry ()
			    :report (lambda (stream)
				      (format stream "Retry W4 Task on ~A" (qe-url queue-entry)))
			    :test (lambda (condition) condition)
			    (go retry))
		     (skip ()
			   :report (lambda (stream)
				     (format stream "Skip W4 Task on ~A" (qe-url queue-entry)))
			   :test (lambda (condition) condition)
			   (return (setq status :abort))))))
	    (t (setq status (handler-case
			      (%execute-task queue-entry)
			      (network-error () :retry)
			      (condition () :retry)
			      (error (err) (note-task-error queue-entry err) :error)))))
	
      status)))

(defmethod tq:task-queue-execute-task ((queue w4-multi-threaded-task-queue) (queue-entry queue-entry))
  (restart-case
    (ecase (execute-queue-entry queue-entry)
      ((:success :error)
       (deallocate-queue-entry queue-entry))
      (:retry
	(push-retry-queue queue-entry (qe-queue queue-entry)))
      ;; Caller pushes queue-entry back on the front of queue via unwind-protect so no resources lost
      (:abort))
    (abort ()
	   :report (lambda (stream)
		     (format stream "Abort W4 Activity, ~A" (activity-name (qe-activity queue-entry))))
	   (abort-activity (qe-activity queue-entry)))))

(defmethod execute-task ((queue single-threaded-queue) &aux entry (status :error))
  (unwind-protect
      (cond ((setq entry (pop-queue queue))
	     (setq status (execute-queue-entry entry)))
	    (t (return-from execute-task nil)))
    (when entry
      (ecase status
	((:success :error :abort)
	 (deallocate-queue-entry entry))
	(:retry
	  (push-retry-queue entry (qe-queue entry)))))))

(defgeneric execute-pending-tasks (queue))

(defmethod execute-pending-tasks ((queue single-threaded-queue))
  (loop for status = (execute-task queue)
        while status
        finally (return :complete)))

