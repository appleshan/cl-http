;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-
;;;
;;; (C) Copyright 1995-2001, 2003, 2005, John C. Mallery and Christopher R. Vincent.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; CL-HTTP AUTHENTICATION
;;; 
;;; Branched off from authentication.lisp.79 and modularized for database storage.   1/24/96 -- JCMa.
;;;

(in-package :http)

;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(defmethod print-object ((realm realm) stream)
  (print-unreadable-object (realm stream :type t :identity t)
    (write-string (realm-name realm) stream)))

(defmethod print-object ((user user) stream)
  (print-unreadable-object (user stream :type t :identity t)
    (write-string (user-name user) stream)))

(defmethod print-object ((group group) stream)
  (print-unreadable-object (group stream :type t :identity t)
    (write-string (group-name group) stream)))

(defmethod print-object ((access-control access-control) stream)
  (print-unreadable-object (access-control stream :type t :identity t)
    (write-string (access-control-name access-control) stream)))

(define-macro with-realm-write-lock ((realm) &body body)
  "Provides concurrency control for shared resources associated with realms."
  `(www-utils:with-lock-held ((realm-lock ,realm) :write "Realm Wait") ,@body))


;;;------------------------------------------------------------------- 
;;;
;;; REALM OPERATIONS
;;;

(define-generic realm-scheme (realm)
  (:documentation "Returns the token for a realm's authentication scheme."))

(define-generic realm-name (realm-or-string)
  (:documentation "Returns the name string for REALM-OR-STRING."))

(defmethod realm-name ((realm string))
  realm)

(define-variable *realm-table* nil
                 "The hash table pointing to authentication realms on the server.")

(declaim (inline realm-table))

(defun realm-table ()
  "Returns a hash table pointing to authentication realms on the server."
  (or *realm-table* (setq *realm-table* (make-hash-table :test #'equalp :size 10))))

(defun clear-realm-table ()
  "Clears all known realms."
  (if *realm-table*
      (clrhash *realm-table*)
      (realm-table)))

(declaim (inline %get-realm))

(defun %get-realm (realm-keyword &optional (realm-table (realm-table)))
  (gethash realm-keyword realm-table)) 

(defparameter *realm-scheme-class-alist* '((:basic . basic-realm)
                                           (:digest . digest-realm)
                                           (:digest-sha . digest-sha-realm)
                                           (:certificate . certificate-realm))
  "Maps the realm authentication scheme to the realm class.
When applications specialize the realm class for a security scheme,
substitute the specialized realm class  for the default.")

(defun %get-realm-class (scheme)
  (or (cdr (assoc scheme *realm-scheme-class-alist* :test #'eq))
      (error "The authentication scheme, ~S, is unknown." scheme)))

(define add-realm-scheme (scheme realm-class)
  "Adds or replaces the realm class used for the authentication scheme, SCHEME.
REALM-CLASS must be a realm. SCHEME must be a keyword.
If you replace a default authentication schemes, you must ensure that the proper
super classes are inherited."
  (declare (values realm-class new-scheme-p))
  (check-type scheme keyword)
  (unless (and (symbolp realm-class)
               (subtypep realm-class 'realm))
    (error "~S is not a realm." realm-class))
  (let ((entry (assoc scheme *realm-scheme-class-alist*)))
    (cond (entry
           (setf (cdr entry) realm-class)
           (values realm-class nil))
          (t (push `(,scheme . ,realm-class) *realm-scheme-class-alist*)
             (values realm-class t)))))

(define remove-realm-scheme (scheme)
  "Removes knowledge of a realm scheme.
Dangerous.  Use with caution."
  (setq *realm-scheme-class-alist* (delete scheme *realm-scheme-class-alist* :key #'car)))

(define map-realms (function &optional (realm-table (realm-table)))
  "Maps FUNCTION over the groups in REALM.
FUNCTION is called with the arguments (NAME-STRING GROUP-OBJECT)."
  (maphash function realm-table))

(defgeneric make-realm (name class))

(defmethod make-realm ((name string) (class symbol))
  (make-instance class :name name))

(define intern-realm (realm &key (if-does-not-exist :error) (scheme :basic))
  (declare (values interned-realm newly-created-p))
  (flet ((do-it (realm)
           (let ((realm-table (realm-table)))
             (cond ((%get-realm realm realm-table))
                   (t (ecase if-does-not-exist
                        (:soft nil)
                        (:create
                          (let ((object (make-realm realm (%get-realm-class scheme))))
                            (setf (realm-lock object) (make-lock (format nil "~A Realm Lock" realm)
                                                                 :type :multiple-reader-single-writer))
                            (setf (gethash realm realm-table) object)
                            (values object t)))
                        (:error (error "~S is not a known realm." realm))))))))
    (declare (inline do-it))
    (etypecase realm
      (string (do-it realm))
      (symbol (do-it (symbol-name realm)))
      (realm realm))))

(define-generic unintern-realm (realm)
  (:documentation "Uninterns a realm."))

(defmethod unintern-realm ((realm realm))
  (remhash (realm-name realm) (realm-table)))

(defmethod unintern-realm ((realm string))
  (remhash realm (realm-table)))

#+CL-HTTP-SSL-CLIENT
(defun %define-certificate-realm (realm-name certificate-authorities &key (key-size 1024.) (cyphers '#.*ssl-cipher-sets*) (ssl-versions '#.*ssl-versions*) 
                                             (algorithms '(:rsa :dsa)) (verify-modes '(:always :once)) (scheme :certificate))
  (multiple-value-bind (realm newly-created-p)
      (intern-realm realm-name :scheme scheme :if-does-not-exist :create)
    (unless newly-created-p
      (let ((class (%get-realm-class scheme)))
        (unless (eq class (type-of realm))
          (change-class realm class))))
    (check-type certificate-authorities (cons keyword))
    (check-type algorithms (cons keyword))
    (check-type key-size (integer 512))
    (check-type verify-modes (cons keyword))
    (check-type ssl-versions (cons keyword))
    (setf (certificate-realm-algorithms realm) algorithms
          (certificate-realm-key-size realm) key-size
          (certificate-realm-authorities realm) certificate-authorities
          (certificate-realm-cyphers realm) cyphers
          (certificate-realm-verify-modes realm) verify-modes
          (certificate-realm-ssl-versions realm) ssl-versions)
    (values realm newly-created-p)))

#+CL-HTTP-SSL-CLIENT
(define-macro define-certificate-realm (realm-name (&key (key-size 1024)  (cyphers '#.*ssl-cipher-sets*) (ssl-versions '#.*ssl-versions*)
                                                         (algorithms '(:rsa :dsa)) (verify-modes '(:always :once)) (scheme :certificate))
                                                   &rest certificate-authorities)
  "Defines a certificate authentication realm named REALM-NAME.

Certificate realms authenticate users based on the X.509 certificates provided by clients to an SSL
server configured to require client certificates. client certificates are only acceptable for user
authentication to a realm when the certificate authority issuing the certificate is a member of
CERTIFICATE-AUTHORITIES, a list of keywords denoting certificate authorities. Other parameters must
also be satisfied for successful user authentication.

     KEY-SIZE     - an integer which is the lowest acceptable public key size. 
     CYPHERS      - a list of keywords denoting acceptable cypher groups. 
     SSL-VERSIONS - a list of keywords denoting acceptable versions of SSL used in the connection.
     ALGORITHMS   - a list of keywords denoting acceptable public key algorithms for the user's public key.
     VERIFY-MODES - a list of keywords indicating the frequency of certificate verification,
                    selected from (:always :once)."
  `(%define-certificate-realm ',realm-name ',certificate-authorities :key-size ,key-size :algorithms ',algorithms :cyphers ',cyphers
                              :verify-modes ',verify-modes :ssl-versions ',ssl-versions  :scheme ',scheme))

(declaim (inline add-realm))

(define add-realm (realm-name scheme)
  "Adds a realm object of type scheme to realm-table."
  (intern-realm realm-name :if-does-not-exist :create :scheme scheme))

(declaim (inline add-group))

(define add-group (realm group)
  "Adds GROUP to realm, creating if necessary."
  (intern-group (intern-realm realm :if-does-not-exist :error)
                group
                :if-does-not-exist :create))

(declaim (inline add-groups))

(define add-groups (realm &rest groups)
  "Adds GROUPs to realm, creating if necessary."
  (loop with realm-object = (intern-realm realm :if-does-not-exist :error)
        for group in groups
        do (intern-group realm-object group :if-does-not-exist :create)))

(defmethod  authentication-scheme ((url http-url))
  (realm-scheme (intern-realm (url:authentication-realm url) :if-does-not-exist :error)))

(define-generic make-user-password-digest (realm username password)
  (declare (values user-password-digest))
  (:documentation "Returns a digest of USERNAME and PASSWORD specific to REALM."))

(defmethod make-user-password-digest (realm (user user) password)
  (make-user-password-digest realm (user-name user) password))

(define-generic update-user (user &key personal-name email-address password groups &allow-other-keys)
  (declare (values user))
  (:documentation "Updates USER according to the arguments."))

(defmethod update-user ((user user) &key (personal-name nil personal-name-supplied-p)
                        (email-address nil email-address-supplied-p) (password nil password-supplied-p)
                        (groups nil groups-supplied-p) &allow-other-keys
                        &aux realm)
  (flet ((get-user-realm ()
           (or realm (setq realm (user-realm user)))))
    (declare (inline get-user-realm))
    (cond-every
      (email-address-supplied-p ;; change the users's email address
        (setf (user-email-address user) email-address))
      (personal-name-supplied-p ;; Change the user's personal name.
        (setf (user-personal-name user) (or personal-name "")))
      (password-supplied-p ;;change the user's password
        (setf (user-password-digest user) (when password (make-user-password-digest (get-user-realm) user password))))
      (groups-supplied-p 
       (let* ((current-groups (user-groups user))
              (n-groups (loop for g in groups
                              collect (intern-group (get-user-realm) g)))
              (add-groups (set-difference n-groups current-groups))
              (del-groups (set-difference current-groups n-groups)))
         (declare (dynamic-extent n-groups add-groups del-groups))
         (dolist (g add-groups)
           (group-add-user g user))
         (dolist (g del-groups)
           (group-remove-user g user)))))
    user))

(defgeneric %realm-get-user (realm user-name)
  (:documentation "Returns the user object for USER-NAME in REALM or null."))

(defmethod %realm-get-user ((realm standard-realm-user-mixin) user-name)
  (with-slots (user-table) realm
    (gethash user-name user-table)))

(defmethod %realm-set-get-user ((realm standard-realm-user-mixin) user-name user-object)
  (with-slots (user-table) realm
    (setf (gethash user-name user-table) user-object)))

(defmethod (setf %realm-get-user) ((user-object user)  (realm standard-realm-user-mixin) user-name)
  (with-slots (user-table) realm
    (setf (gethash user-name user-table) user-object)))

(defgeneric make-user (realm user-name &optional password email-address personal-name))

(defmethod make-user ((realm standard-realm-user-mixin) user-name &optional password email-address personal-name)
  (make-instance (realm-user-class realm)
                 :username user-name
                 :realm realm
                 :password-digest (and password (make-user-password-digest realm user-name password))
                 :personal-name personal-name
                 :email-address email-address))

(define-generic intern-user (realm user &key if-does-not-exist password groups
                                   personal-name email-address &allow-other-keys)
  (declare (values user-object newly-created-p))
  (:documentation "Primary method for defining a REALM."))

(defmethod intern-user ((realm standard-realm-user-mixin) (user string) 
                        &key (if-does-not-exist :error) password groups personal-name email-address 
                        &allow-other-keys
                        &aux user-object)
  (declare (values interned-user newly-created-p))
  (with-slots (user-table) realm
    (tagbody
     start
     (cond ((setq user-object (gethash user user-table))
            (cond-every
             (password (setf (user-password-digest user-object) (make-user-password-digest realm user password)))
             (groups
              (dolist (group groups)
                (group-add-user (intern-group realm group :if-does-not-exist :create) user-object)))
             (email-address (setf (user-email-address user-object) email-address))
             (personal-name (setf (user-personal-name user-object) personal-name)))
            (return-from intern-user user-object))
           (t (ecase if-does-not-exist
                (:soft (return-from intern-user nil))
                (:create 
                 (setq user-object (make-user realm user password email-address personal-name))
                 (with-realm-write-lock (realm)
                   (cond ((gethash user user-table) (go start))	;someone else created the user
                         (t (setf (gethash user user-table) user-object)
                            (when email-address ;index the email address too
                              (setf (gethash email-address user-table) user-object)))))
                 (dolist (group groups)
                   (group-add-user (intern-group realm group :if-does-not-exist :create) user-object))
                 (return-from intern-user (values user-object t)))
                (:error (error "There is no user, ~S, in the realm, ~S." user (realm-name realm)))))))))

(defmethod intern-user ((realm symbol) user &key (if-does-not-exist :error) password groups personal-name
                        email-address &allow-other-keys)
  (intern-user (symbol-name realm) user
               :if-does-not-exist if-does-not-exist
               :password password
               :groups groups
               :personal-name personal-name
               :email-address email-address))

(defmethod intern-user ((realm string) user &key (if-does-not-exist :error) password groups personal-name
                        email-address &allow-other-keys)
  (intern-user (intern-realm realm :if-does-not-exist :error) user
               :if-does-not-exist if-does-not-exist
               :password password
               :groups groups
               :personal-name personal-name
               :email-address email-address))

(define-generic unintern-user (realm user)
  (:documentation "Uninterns USER from REALM.
REALM can be an interned realm or a realm keyword."))

(defmethod unintern-user ((realm standard-realm-user-mixin) (user user))
  (remhash (user-name user) (realm-user-table realm)))

(defmethod unintern-user :after ((realm standard-realm-user-mixin) (user user-email-address-mixin))
  (let ((email-address (user-email-address user)))
    (when email-address
      (remhash email-address (realm-user-table realm)))))

(defmethod unintern-user :around ((realm standard-realm-user-mixin) (user user-email-address-mixin))
  (with-realm-write-lock (realm)
    (call-next-method realm user)))

(defmethod unintern-user :before ((realm realm) (user user))
  (dolist (group (user-groups user))
    (group-remove-user group user)))

(defmethod unintern-user ((realm realm) user)
  (let ((object (intern-user realm user :if-does-not-exist :soft)))
    (when object
      (unintern-user realm object))))

(defmethod unintern-user ((realm symbol) user) 
  (unintern-user (intern-realm realm :if-does-not-exist :error) user))

;; must be called inside a with-realm-write-lock
(defmethod %rename-user ((realm standard-realm-user-mixin) (user user) (old-name string) (new-name string))
  (let ((user-table (realm-user-table realm))
        existing-user)
    (cond ((and (setq existing-user (gethash new-name user-table))
                (not (eq user existing-user)))
           (values existing-user nil))
          (t (setf (gethash new-name user-table) user)
             (setf (user-name user) new-name)
             (remhash old-name user-table)
             (user-clear-name-caches user)
             (values user t)))))

(define-generic rename-user (realm user new-name &optional if-exists)
  (declare (values user-object user-renamed-p))
  (:documentation "Renames USER to NEW-NAME in REALM.
If a user named new-name already exists in realm, IF-EXISTS controls
the action. IF-EXISTS can be:

   :SOFT Return the values (USER-OBJECT-WITH-NEW-NAME NIL)
   :ERROR  Signal an error."))

(defmethod rename-user ((realm standard-realm-user-mixin) (user user) (new-name string) &optional (if-exists :soft))
  (multiple-value-bind (existing-user renamed-p)
      (with-realm-write-lock (realm)
        (%rename-user realm user (user-name user) new-name))
    (cond (existing-user
           (ecase if-exists
             (:soft (values existing-user renamed-p))
             (:error (error "A user named, ~S, already exists, in the realm, ~S." new-name (realm-name realm)))))
          (t (values user renamed-p)))))

(define parse-authentication-object (string intern-function &key (if-does-not-exist :soft))
  (declare (values authentication-object newly-created-p))
  (let* ((len (1+ (position-if-not #'http:white-space-char-p string :start 0 :end (length string) :from-end t)))
         (pos1 (position-if-not #'http:white-space-char-p string :start 0 :end len))
         (pos2 (or (position #\| string :start pos1 :end len) len))
         (realm-name (subseq string pos1 pos2))
         (object-name (subseq string (1+ pos2) len))
         (realm (intern-realm realm-name :if-does-not-exist (case if-does-not-exist
                                                              (:error if-does-not-exist)
                                                              (t :soft)))))
    (when realm
      (funcall intern-function realm-name object-name :if-does-not-exist if-does-not-exist))))

(define-generic map-users (realm function)
  (:documentation "Maps FUNCTION over the users in REALM.
FUNCTION is called with the arguments (NAME-STRING USER-OBJECT)."))

(defmethod map-users ((realm standard-realm-user-mixin) function)
  (flet ((fctn (key user)
           (when (equal key (user-name user)) ;only apply function when key is userid
             (funcall function key user))))
    (declare (dynamic-extent #'fctn))
    (with-slots (user-table) realm
      (maphash #'fctn user-table))))

(define-generic map-groups (realm function)
  (:documentation "Maps FUNCTION over the groups in REALM.
FUNCTION is called with the arguments (NAME-STRING GROUP-OBJECT)."))

(defmethod map-groups ((realm standard-realm-group-mixin) function)
  (with-slots (group-table) realm
    (maphash function group-table)))

(define-generic map-access-controls (realm function)
  (:documentation "Maps FUNCTION over the access-controls in REALM.
FUNCTION is called with the arguments (NAME-STRING ACCESS-CONTROL-OBJECT)."))

(defmethod map-access-controls ((realm standard-realm-access-control-mixin) function)
  (with-slots (access-control-table) realm
    (maphash function access-control-table)))

(define sorted-realms (&key (predicate #'string<) (key #'realm-name))
  "Returns all known realm sorted by PREDICATE on the value of KEY."
  (let ((realms nil))
    (declare (dynamic-extent realms))
    (flet ((collect (name realm)
             (declare (ignore name))
             (push realm realms)))
      (map-realms #'collect (realm-table))
      (sort realms predicate :key key))))

(define-generic sorted-users (realm &key predicate key)
  (:documentation "Returns all users in REALM sorted according to PREDICATE on the value of KEY."))

(defmethod sorted-users ((realm realm) &key (predicate #'string<) (key #'user-name))
  (let ((users nil))
    (declare (dynamic-extent users))
    (flet ((collect (name user)
             (declare (ignore name))
             (push user users)))
      (declare (dynamic-extent #'collect))
      (map-users realm #'collect)
      (sort users predicate :key key))))

(define-generic sorted-groups (realm &key predicate key)
  (:documentation "Returns all groups in REALM sorted according to PREDICATE on the value of KEY."))

(defmethod sorted-groups ((realm realm) &key (predicate #'string<) (key #'group-name))
  (let ((groups nil))
    (declare (dynamic-extent groups))
    (flet ((collect (name group)
             (declare (ignore name))
             (push group groups)))
      (declare (dynamic-extent #'collect))
      (map-groups realm #'collect)
      (sort groups predicate :key key))))

(define-generic sorted-access-controls (realm &key predicate key)
  (:documentation "Returns all access-controlss in REALM sorted according to PREDICATE on the value of KEY."))

(defmethod sorted-access-controls ((realm realm) &key (predicate #'string<) (key #'access-control-name))
  (let ((access-controls nil))
    (declare (dynamic-extent access-controls))
    (flet ((collect (name access-control)
             (declare (ignore name))
             (push access-control access-controls)))
      (map-access-controls realm #'collect)
      (sort access-controls predicate :key key))))

;; Cliches for the user accessibility.
(declaim (inline add-user))

(define add-user (user realm &rest args)
  "Add a user object with its appropriate slots to a realm."
  (declare (dynamic-extent args))
  (apply #'intern-user (intern-realm realm :if-does-not-exist :error) user
         :if-does-not-exist :create args))

(declaim (inline delete-user))

(defun delete-user (user realm )
  (unintern-user realm user))

(defgeneric authenticate-user (realm credentials &optional http-method case)
  (declare (values authenticated-user-or-null))
  (:documentation "Authenticate user in a REALM based on CREDENTIALS.
Returns an authenicated user object or NIL if authentication not possible.
CASE is either :CLIENT-ACCESS or :PROXY-ACCESS"))

(defmethod authenticate-user (realm credentials &optional method case)
  (declare (ignore realm credentials method case))
  nil)

(defmethod authenticate-user ((realm string) credentials &optional http-method (case :client-access))
  (let ((realm (intern-realm realm :if-does-not-exist :soft)))
    (when realm
      (authenticate-user realm credentials http-method case))))

(define-generic authenticate-user-p (user credentials http-method case)
  (declare (values user-object-or-null))
  (:documentation "Returns the user object if the CREDENTIALS match the USER for HTTP-METHOD, otherwise returns NIL.
CASE is either :CLIENT-ACCESS or :PROXY-ACCESS"))

(define-generic www-authenticate-header-spec (realm authentication-method &rest keyword-value-pairs)
  (:documentation "Returns a WWW-AUTHENTICATE plist suitable for writing the header over HTTP."))

(define-generic proxy-authenticate-header-spec (realm authentication-method &rest keyword-value-pairs)
  (:documentation "Returns a PROXY-AUTHENTICATE plist suitable for writing the header over HTTP."))

;;;------------------------------------------------------------------- 
;;;
;;; GROUPS
;;;

(defun %intern-group-list (realm list &optional (if-does-not-exist :soft))
  (loop for name in list
        for user = (and (stringp name)
                        (intern-user realm name :if-does-not-exist if-does-not-exist))
        for group = (intern-group realm name :if-does-not-exist if-does-not-exist)
        when user
          collect user into users
        else when group
               collect group into groups
        else do (error "Unknown user or group for the realm, ~S." realm)
        finally (return (values users groups))))

(declaim (inline %group-add-user))

(defun %group-add-user (group user)
  (with-slots (users) group
    (with-slots (groups) user
      (unless (member user users :test #'eq)
        (atomic-push user users)
        (atomic-push group groups)))))

(declaim (inline %group-delete-user))

(defun %group-remove-user (group user)
  (with-slots (users) group
    (when (member user users :test #'eq)
      (setf users (delete user users)
            (user-groups user) (delete group (user-groups user))))))

(declaim (inline %group-add-inferior))

(defun %group-add-inferior (group inferior)
  (with-slots (inferiors) group
    (with-slots (superiors) inferior
      (unless (member inferior inferiors :test #'eq)
        (atomic-push inferior inferiors)
        (atomic-push group superiors)))))

(declaim (inline %group-delete-inferior))

(defun %group-remove-inferior (group inferior)
  (with-slots (inferiors) group
    (when (member inferior inferiors :test #'eq)
      (setf inferiors (delete inferior inferiors)
            (group-superiors inferior) (delete group (group-superiors inferior))))))

(define-generic group-add-user (group user)
  (:documentation "Adds USER to GROUP."))

(defmethod group-add-user ((group standard-group) (user authenticated-user))
  (%group-add-user group user))

(define-generic user-add-group (user group)
  (:documentation "Adds GROUP to USER."))

(defmethod user-add-group ((user user) (group group))
  (group-add-user group user))

(define-generic group-remove-user (group user)
  (:documentation "Remove USER from GROUP."))

(defmethod group-remove-user ((group standard-group) (user authenticated-user))
  (%group-remove-user group user))

(define-generic user-remove-group (user group)
  (:documentation "Removes GROUP from USER."))

(defmethod user-remove-group ((user user) (group group))
  (group-remove-user group user))

(define-generic group-add-inferior (superior inferior)
  (:documentation "Adds an inferior group, INFERIOR, to a superior group, SUPERIOR."))

(defmethod group-add-inferior ((superior standard-group) (inferior standard-group))
  (%group-add-inferior superior inferior))

(define-generic group-remove-inferior (superior inferior)
  (:documentation "Removes an inferior group, INFERIOR, from a superior group, SUPERIOR."))

(defmethod group-remove-inferior ((superior standard-group) (inferior standard-group))
  (%group-remove-inferior superior inferior))

(define-generic inherits-from-group-p (user-or-group group)
  (:documentation "Returns non-null if the USER-OR-GROUP inherits from GROUP."))

;; if performance becomes an issue, then go through the hair of caching/decaching the results. -- JCMa 3/15/2010
(defmethod inherits-from-group-p ((group group) (superior group))
  (or (eq group superior) ;if the same, return immediately
      (let ((my-superiors (group-superiors group)))
        (when my-superiors
          ;; Breadth first check
          (loop for sup in my-superiors
                do (when (eq superior sup)
                     (return-from inherits-from-group-p t)))
          (loop for sup in my-superiors
                when (inherits-from-group-p sup superior)
                return t
                finally (return nil))))))

(defmethod inherits-from-group-p ((user user) (superior group))
  (let ((my-groups (user-groups user)))
    (when my-groups
      (loop for sup in my-groups
            do (when (eq superior sup)
                 (return-from inherits-from-group-p t)))
      (loop for sup in my-groups
            when (inherits-from-group-p sup superior)
            return t
            finally (return nil)))))

(defgeneric udpate-users-and-groups (group users-and-groups &optional if-does-not-exist))

(defmethod udpate-users-and-groups ((group group) users-and-groups &optional (if-does-not-exist :soft) &aux realm)
  (macrolet ((realm ()
               `(or realm (setq realm (group-realm group)))))
    (destructuring-bind (users groups) users-and-groups
      (cond-every
       (users
        (dolist (user users)
          (group-add-user group (intern-user (realm) user :if-does-not-exist if-does-not-exist))))
       (groups
        (dolist (inferior groups)
          (group-add-inferior group (intern-group (realm) inferior :if-does-not-exist if-does-not-exist))))))))

(defgeneric make-group (realm group-name))

(defmethod make-group ((realm standard-realm-group-mixin) group-name)
  (make-instance (realm-group-class realm)
                 :name group-name
                 :realm realm))

(define-generic intern-group (realm group &key if-does-not-exist members)
  (declare (values group-object newly-create-p))
  (:documentation "Interns the group named, NAME, in the realm, REALM.
When supplied, members are set to MEMBERS.
Interning of MEMBERS respects the value of IF-DOES-NOT-EXIST."))

(defmethod intern-group ((realm standard-realm-group-mixin) (name string) &key (if-does-not-exist :error)
                         (members nil members-supplied-p) &aux group-object)
  (declare (values interned-group newly-created-p))
  (with-slots (group-table) realm
    (cond ((setq group-object (gethash name group-table))
           (when members-supplied-p
             (let ((spec `(nil ,members)))
               (declare (dynamic-extent spec))
               (udpate-users-and-groups group-object spec if-does-not-exist)))
           group-object)
          (t (ecase if-does-not-exist
               (:soft nil)
               (:create
                (setq group-object (make-group realm name))
                (with-realm-write-lock (realm)
                  (setf (gethash name group-table) group-object))
                (when members-supplied-p
                  (let ((spec `(nil ,members)))
                    (declare (dynamic-extent spec))
                    (udpate-users-and-groups group-object spec if-does-not-exist)))
                (values group-object t))
               (:error (error "There is no group named, ~A, in the realm, ~A." name (realm-name realm))))))))

(defmethod intern-group ((realm realm) (name symbol) &key (if-does-not-exist :error)
                         (members nil members-supplied-p))
  (cond (members-supplied-p
         (intern-group realm (symbol-name name) :if-does-not-exist if-does-not-exist
                       :members members))
        (t (intern-group realm (symbol-name name) :if-does-not-exist if-does-not-exist))))

(defmethod intern-group ((realm string) name &key (if-does-not-exist :error)
                         (members nil members-supplied-p))
  (cond (members-supplied-p
         (intern-group (intern-realm realm :if-does-not-exist :error) name
                       :if-does-not-exist if-does-not-exist
                       :members members))
        (t (intern-group (intern-realm realm :if-does-not-exist :error) name
                         :if-does-not-exist if-does-not-exist))))

(defmethod intern-group ((realm realm) (group group) &key if-does-not-exist
                         (members nil members-supplied-p))
  (declare (ignore if-does-not-exist))
  (unless (typep group (realm-group-class realm))
    (error "Group, ~S, is the wrong class for REALM, ~S." group realm))
  (when members-supplied-p
    (udpate-users-and-groups group members))
  group)

(defmethod unintern-group ((realm standard-realm-group-mixin) (group group))
  (remhash (group-name group) (realm-group-table realm)))

(defmethod unintern-group :before ((realm realm) (group group))
  (dolist (user (group-users group))
    (user-remove-group user group))
  (dolist (g (group-inferiors group))
    (group-remove-inferior group g))
  (dolist (g (group-superiors group))
    (group-remove-inferior g group)))

(defmethod unintern-group ((realm realm) (group string))
  (let ((object (intern-group realm group :if-does-not-exist :soft)))
    (when object
      (unintern-group realm object))))

(defmethod unintern-group ((realm string) group) 
  (unintern-group (intern-realm realm :if-does-not-exist :error) group)) 

(define-generic group-keyword (group)
  (:documentation "Returns a keyword denoting the group."))

(defmethod group-keyword ((group group))
  (intern (group-name group) *keyword-package*))

(define-generic qualified-name (authentication-object &optional recompute-p)
  (:documentation "Returns the name of authentication-object qualified by realm [e.g., \"my-realm|my-name|\"]"))

(defmethod qualified-name ((group group) &optional recompute-p)
  (with-value-cached (group :qualified-name :recompute-p recompute-p)
    (concatenate 'string (realm-name (group-realm group)) "|" (group-name group))))

;;;------------------------------------------------------------------- 
;;;
;;; URL ACCESS CONTROL
;;; 

;; add new keywords here as new methods are desired.
(defparameter *url-capability-methods* '(:default :get :head :post :put :delete :trace :options)
  "The set of allowed URL capabilities.")

(declaim (inline check-url-capability-method))

(defun check-url-capability-methods (alist)
  (loop for (method) in alist
        unless (member method *url-capability-methods* :test #'eq)
          do (error "The URL capability method, ~S, is not one of the known set, ~S.
Perhaps you need to update http:*url-capability-methods*." method *url-capability-methods*)))

;; returns an alist of (method groups users)
(defun %intern-access-control-alist (realm alist)
  (declare (values access-alist default-groups default-users))
  (flet ((parse-entry (entry)
           (destructuring-bind (method &rest capabilities) entry
             (loop for name in capabilities
                   for user = (and (stringp name)
                                   (intern-user realm name :if-does-not-exist :soft))
                   for group = (and (null user)
                                    (intern-group realm name :if-does-not-exist :soft))
                   when user
                     collect user into users
                   else when group
                          collect group into groups
                   else do (error "Unknown user or group, ~S for the realm, ~S." name realm)
                   finally (return (when method
                                     (list method groups users)))))))
    (check-url-capability-methods alist)
    (let ((default (assoc :default alist :test #'eq)))
      (destructuring-bind (&optional default-groups default-users)
          (and default (cdr (parse-entry default)))
        (loop for method in *url-capability-methods*
              for entry = (unless (member method '(:default) :test #'eq)
                            (assoc method alist :test #'eq))
              when entry 
                collect (parse-entry entry) into access-alist
              finally (return (values access-alist default-groups default-users)))))))

(defmethod update-access-control ((access-control standard-access-control) capability-alist)
  (with-slots (realm) access-control
    (multiple-value-bind (access-alist default-groups default-users)
        (%intern-access-control-alist realm capability-alist)
      (setf (access-control-alist access-control) access-alist
            (access-control-default-groups access-control) default-groups
            (access-control-default-users access-control) default-users))))

(define-generic capability-alist (access-control)
  (:documentation "Returns a capability alist for ACCESS-CONTROL suitable for intern-access-control."))

(defmethod capability-alist ((access-control standard-access-control))
  (with-slots (alist default-groups default-users) access-control
    (flet ((make-entry (method groups users)
             `(,method ,.(mapcar #'group-keyword groups)
               ,.(mapcar #'user-name users))))
      (loop for (method groups users) in alist
            collect (make-entry method groups users)
              into capability-alist
            finally (return (cond ((or default-groups default-users)
                                   `(,.capability-alist
                                     ,(make-entry :default default-groups default-users)))
                                  (t capability-alist))))))) 

(defgeneric make-access-control (realm name))

(defmethod make-access-control ((realm standard-realm-access-control-mixin) name)
  (make-instance (realm-access-control-class realm)
                 :name name
                 :realm realm))

(defgeneric make-url-access-control (realm name))

(defmethod make-url-access-control ((realm standard-realm-access-control-mixin) name)
  (make-instance (realm-url-access-control-class realm)
                 :name name
                 :realm realm))

(define-generic intern-access-control (realm name &key if-does-not-exist capability-alist)
  (declare (values access-control-object newly-interned-p))
  (:documentation "Interns a URL capabilities object named, NAME, in the realm, REALM.
capabilities-alist can be an alist of (METHOD &rest users-and-groups)."))

(defmethod intern-access-control ((realm standard-realm-access-control-mixin) (name string) 
                                  &key (if-does-not-exist :error)
                                  (capability-alist nil capability-alist-supplied-p)
                                  &aux access-control-object)
  (declare (values interned-access-control newly-created-p))
  (with-slots (access-control-table) realm
    (cond ((setq access-control-object (gethash name access-control-table))
           (when capability-alist-supplied-p
             (update-access-control access-control-object capability-alist))
           access-control-object)
          (t (ecase if-does-not-exist
               (:soft nil)
               ((:create :url-create)
                (setq access-control-object (ecase if-does-not-exist
                                              (:create (make-access-control realm name))
                                              (:url-create (make-url-access-control realm name))))
                (update-access-control access-control-object capability-alist)
                (with-realm-write-lock (realm)
                  (setf (gethash name access-control-table) access-control-object))
                (values access-control-object t))
               (:error (error "There is no access-control named, ~A, in the realm, ~A."
                              name (realm-name realm))))))))

(declaim (inline %intern-access-control-aux))

(defun %intern-access-control-aux (realm name if-does-not-exist capability-alist capability-alist-supplied-p)
  (cond (capability-alist-supplied-p
         (intern-access-control realm name
                                :if-does-not-exist if-does-not-exist
                                :capability-alist capability-alist))
        (t (intern-access-control realm name :if-does-not-exist if-does-not-exist))))

(defmethod intern-access-control (realm (name symbol) &key (if-does-not-exist :error)
                                        (capability-alist nil capability-alist-supplied-p))
  (%intern-access-control-aux realm (symbol-name name) if-does-not-exist capability-alist capability-alist-supplied-p))

(defmethod intern-access-control ((realm string) name &key (if-does-not-exist :error)
                                  (capability-alist nil capability-alist-supplied-p))
  (%intern-access-control-aux (intern-realm realm :if-does-not-exist :error)
                              name if-does-not-exist capability-alist capability-alist-supplied-p))

(defmethod intern-access-control ((realm symbol) name &key (if-does-not-exist :error)
                                  (capability-alist nil capability-alist-supplied-p))
  (%intern-access-control-aux (intern-realm realm :if-does-not-exist :error)
                              name if-does-not-exist capability-alist capability-alist-supplied-p))

(defmethod intern-access-control (realm (url url:http-url) &key (if-does-not-exist :error)
                                        (capability-alist nil capability-alist-supplied-p))
  (%intern-access-control-aux realm (url:name-string url) if-does-not-exist capability-alist capability-alist-supplied-p))

(defmethod intern-access-control ((realm realm) (access-control access-control) &key if-does-not-exist
                                  (capability-alist nil capability-alist-supplied-p))
  (declare (ignore if-does-not-exist))
  (unless (typep access-control (realm-access-control-class realm))
    (error "ACCESS-CONTROL, ~S, is the wrong class for REALM, ~S." access-control realm))
  (when capability-alist-supplied-p
    (update-access-control access-control capability-alist))
  access-control)

(define-generic unintern-access-control (realm access-control)
  (:documentation "Unintern access-control from REALM."))

(defmethod unintern-access-control ((realm standard-realm-access-control-mixin) (access-control string))
  (with-slots (access-control-table) realm
    (remhash access-control access-control-table)))

(defmethod unintern-access-control ((realm realm) (access-control access-control))
  (unintern-access-control realm (access-control-name access-control)))

(defmethod unintern-access-control ((realm symbol) access-control) 
  (unintern-access-control (intern-realm realm :if-does-not-exist :error) access-control))

(define-generic add-access-control-group (name-or-url realm &key capabilities)
  (declare (values access-control newly-created-p))
  (:documentation "Interns access-control, named NAME-OR-URL, in REALM with method CAPABILITIES.


CAPABILITIES is a capability alist relating server methods to groups and
users, like ((METHOD . GROUPS-AND-USERS)) METHOD can be any valid HTTP METHOD
keyword (:GET, :POST, :PUT, :DELETE, :OPTIONS, :TRACE) or :DEFAULT. The
GROUPS-AND-USERS for :DEFAULT are used whenever no specific method is
supplied. If GROUPS-AND-USERS is null for a METHOD, then no access is allowed.
GROUPS-AND-USERS is a sequence of group keywords or user strings in REALM.

A typcial value for CAPABILITIES is '((:default :users :wizards))."))

(DEFMETHOD add-access-control-group (name (realm realm) &key capabilities)
  (intern-access-control realm name :if-does-not-exist :create :capability-alist capabilities))

(defmethod add-access-control-group (name (realm symbol) &key capabilities)
  (intern-access-control (intern-realm realm :if-does-not-exist :error)
                         name :capability-alist capabilities
                         :if-does-not-exist :create))

(defmethod add-access-control-group ((url url:http-url) (realm realm) &key capabilities)
  (intern-access-control realm (url:name-string url)
                         :capability-alist capabilities
                         :if-does-not-exist :url-create))

(defmethod url:initialize-authentication ((url url:authentication-mixin) realm capabilities
                                          &aux realm-obj access-control)
  (macrolet ((require-realm (realm-obj)
               `(unless ,realm-obj
                  (error "Authentication cannot be initialized without a realm."))))
    ;; intern the objects
    (setq realm-obj (typecase realm
                      (null nil)
                      (t (intern-realm realm :if-does-not-exist :error))))
    (setq access-control (typecase capabilities
                           (null nil)
                           (cons
                             (require-realm realm-obj)
                             (add-access-control-group url realm-obj :capabilities capabilities))
                           (access-control
                             (cond
                               (realm-obj
                                (unless (eq realm-obj (access-control-realm capabilities))
                                  (error "REALM, ~S, for the URL, ~S, is not the same as the authentication realm for CAPABILITIES, ~S.
They must be the same." realm-obj url capabilities)))
                               (t (setq realm-obj (access-control-realm capabilities))
                                  (require-realm realm-obj)))
                             capabilities)
                           (t (require-realm realm-obj)
                              (intern-access-control realm-obj capabilities :if-does-not-exist :error))))
    ;; set the instance variables.
    (with-slots (url:authentication-realm url:capabilities) url
      (setq url:authentication-realm realm-obj
            url:capabilities access-control))))

(define-generic allow-user-access-p (url-or-access-control user-or-server http-method)
  (:documentation "Decides whether USER-OR-SERVER should get access to URL-OR-ACCESS-CONTROL via HTTP-METHOD."))

;; modified to allow access based on group inheritence. -- JCMa 3/15/2010
(defmethod allow-user-access-p ((access-control standard-access-control) (user user) (http-method symbol))
  (with-slots (alist default-groups default-users) access-control

    (labels ((any-intersection (l1 l2)
               (and l1 l2
                    (loop for item in l1
                          when (member item l2 :test #'eq)
                          return t
                          finally (return nil))))
             (any-member (item l2)
               (and l2 (member item l2 :test #'eq)))
             (any-group-intersection (user-groups allowed-groups)
               (cond ((and user-groups allowed-groups)
                      (when (any-intersection user-groups allowed-groups)
                        (return-from any-group-intersection t))
                      (loop for u-group in user-groups
                            do (loop for a-group in allowed-groups
                                     do (when (inherits-from-group-p u-group a-group)
                                          (return-from any-group-intersection t)))
                            finally (return nil)))
                     (t nil))))
      (declare (inline any-intersection any-member))
      (let ((entry (assoc http-method alist :test #'eq)))
        (cond (entry
               (destructuring-bind (&optional groups users) (cdr entry)
                 (or (any-member user users)
                     (any-group-intersection (user-groups user) groups))))
              ((or (any-member user default-users)
                   (any-group-intersection (user-groups user) default-groups)))
              ((not (or alist default-groups default-users))
               t)
              (t nil))))))

(defmethod allow-user-access-p ((url url) (user user) http-method)
  (let ((access-control (url:capabilities url)))
    (if access-control
        (allow-user-access-p access-control user http-method)
        t)))

(defmethod allow-user-access-p ((url url) user http-method)
  (declare (ignore user http-method))
  (null (url:capabilities url)))

;; These are not cached because plist mixin was not added to them.
(defmethod qualified-name ((access-control access-control) &optional recompute-p)
  (declare (ignore recompute-p))
  (concatenate 'string (realm-name (access-control-realm access-control)) "|" (access-control-name access-control)))

;;;------------------------------------------------------------------- 
;;;
;;; AUTHENTICATION UTILITIES
;;; 

(define define-realm-table (token-scheme-a-list)
  "Initializes and defined realms authenticated the server from an a-list of name tokens and schemes."
  (loop initially (clear-realm-table)
        for (name . scheme) in token-scheme-a-list
        collect (intern-realm name :if-does-not-exist :create :scheme scheme)))

(define-macro with-authentication-access-control ((url method credentials realm &key rejection-form
                                                       require-capabilities) &body body)
  "Executes REJECTION-FORM whenever CREDENTIALS does not qualify for CAPABILITIES under REALM,
Otherwise executes BODY. If REQUIRE-CAPABILITIES is non-null, all accesses are
rejected whenever CAPABILITIES is null"
  `(cond (,realm
          (handler-case
            (let ((user (authenticate-user ,realm ,credentials ,method :client-access)))
              (cond ((and user
                          (let ((capabilities (url:capabilities ,url)))
                            (setf (server-user-object *server*) user)
                            ,(if require-capabilities
                                 `(and capabilities (allow-user-access-p capabilities user ,method))
                                 `(or (null capabilities) (allow-user-access-p capabilities user ,method)))))
                     ,@body)
                    (t ,rejection-form)))
            (unknown-authentication-method () ,rejection-form)))
         (t ,@body)))

(defgeneric signal-unauthorized-client-access (realm url method)
  (:documentation "Signals unathorized client access according to REALM."))

(defmethod signal-unauthorized-client-access ((realm realm-http-authentication-mixin) url method)
  (error 'recoverable-unauthorized-client-access :method method :url url :authentication-method (realm-scheme realm) :authentication-realm realm))

(defmethod signal-unauthorized-client-access ((realm realm-certificate-authentication-mixin) url method)
  (error 'access-forbidden :method method :url url))

(define-macro with-access-control ((url method server secure-subnets &key require-secure-subnets require-capabilities
                                        deny-subnets write-method-p) &body body)
  (let ((code `(with-subnet-access-control ((server-address ,server)
                                            ,secure-subnets
                                            :deny-subnets ,deny-subnets
                                            :require-secure-subnets ,require-secure-subnets
                                            :rejection-form (error 'access-forbidden :method ,method :url ,url))
                 (let ((.realm. (url:authentication-realm ,url)))
                   (with-authentication-access-control
                       (,url ,method
                             (realm-get-client-credentials .realm. ,server) 
                             .realm.
                             :require-capabilities ,require-capabilities
                             :rejection-form (signal-unauthorized-client-access .realm. ,url ,method))
                     ,@body)))))
    (if write-method-p
        `(with-remote-write-control (,url :rejection-form (error 'access-forbidden :method ,method :url ,url))
           ,code)
      code)))


;;;------------------------------------------------------------------- 
;;;
;;; SAVING AUTHENTICATION DATA
;;;

(define-generic write-lisp-source (realm authentication-object stream)
  (declare (values authenication-object))
  (:documentation "Writes Lisp source code on STREAM for reloading AUTHENTICATION-OBJECT in REALM.
AUTHENTICATION-OBJECT can be a user, group, access-control."))

(defmethod write-lisp-source ((realm standard-realm) (group standard-group) stream)
  (with-slots (inferiors) group
    (let ((infs (mapcar #'group-keyword inferiors)))
      (fresh-line stream)
      (write `(intern-group ,(realm-name realm) ,(group-name group) 
                            ,.(when infs `(:members ',infs))
                            :if-does-not-exist :create)
             :stream stream)
      (fresh-line stream)
      group)))

(defmethod write-lisp-source :around ((realm standard-realm) (user standard-user) stream)
  (with-slots (groups) user
    (let ((user-code `(intern-user ,(realm-name realm) ,(user-name user)
                                   :if-does-not-exist :create
                                   :email-address ,(user-email-address user)
                                   :personal-name ,(user-personal-name user)
                                   :groups ',(mapcar #'group-name groups))))
      (declare (dynamic-extent user-code))
      (fresh-line stream)
      (cond ((%write-lisp-source-specialized-info-p user)
             (write-string "(LET " stream)
             (write `((user ,user-code)) :stream stream) 
             (call-next-method)
             (write-char #\) stream))           ; close let
            (t (write user-code :stream stream)))
      (fresh-line stream)
      user)))

(defgeneric %write-lisp-source-specialized-info-p (object)
  (:documentation "Returns non-null when additional slots will be set on OBJECT
by combined methods for write-lisp-source.")
  (:method-combination or))

(defmethod %write-lisp-source-specialized-info-p or (object)
  (declare (ignore object)) nil)

(defmethod %write-lisp-source-specialized-info-p or ((user user-password-mixin))
  (with-slots (password-digest) user
    password-digest)) 

;; each type of authenticated user may need to define this method.
;; This one covers any user classes employing user-password-mixin
(defmethod write-lisp-source ((realm standard-realm) (user user-password-mixin) stream)
  (with-slots (password-digest) user
    (when password-digest
      (fresh-line stream)
      (let ((*print-base* 8))
        (write `(setf (user-password-digest user) ',password-digest) :stream stream)))
    user)) 

(defmethod write-lisp-source ((realm standard-realm-access-control-mixin) (access-control standard-access-control) stream)
  (fresh-line stream)
  (write `(intern-access-control ,(realm-name realm) ,(access-control-name access-control)
                                 :capability-alist ',(capability-alist access-control)
                                 :if-does-not-exist :create)
         :stream stream)
  (fresh-line stream)
  access-control)

;; Access controls linked to URLs are not written out because they are created when the URL is exported.
(defmethod write-lisp-source ((realm standard-realm) (access-control url-access-control) stream) 
  (declare (ignore stream)) 
  access-control)

(defgeneric realm-slot-setter-forms (realm)
  (:documentation "Returns a list of forms that set slow values for REALM.")
  (:method-combination nconc))

#+CL-HTTP-SSL-CLIENT
(defmethod realm-slot-setter-forms nconc ((realm certificate-realm-cryptography-mixin))
  `((setf (certificate-realm-algorithms realm) ',(certificate-realm-algorithms realm)
          (certificate-realm-key-size realm) ',(certificate-realm-key-size realm))))

#+CL-HTTP-SSL-CLIENT
(defmethod realm-slot-setter-forms nconc ((realm certificate-realm-security-mixin))
  `((setf (certificate-realm-authorities realm) ',(certificate-realm-authorities realm)
          (certificate-realm-cyphers realm) ',(certificate-realm-cyphers realm)
          (certificate-realm-ssl-versions realm) ',(certificate-realm-ssl-versions realm)
          (certificate-realm-verify-modes realm) ',(certificate-realm-verify-modes realm))))

#+CL-HTTP-SSL-CLIENT
(defmethod %write-lisp-source-specialized-info-p or ((realm certificate-realm-cryptography-mixin)) t) 

#+CL-HTTP-SSL-CLIENT
(defmethod %write-lisp-source-specialized-info-p or ((realm certificate-realm-security-mixin)) t)

(defgeneric intern-realm-form (realm)
  (:documentation "Returns a form that interns and instantiates REALM."))

(defmethod intern-realm-form ((realm standard-realm))
  (let ((realm-form `(intern-realm ,(realm-name realm)
                             :if-does-not-exist :create
                             :scheme ,(realm-scheme realm))))
  (cond ((%write-lisp-source-specialized-info-p realm)
          `(let ((realm ,realm-form))
             ,@(realm-slot-setter-forms realm)
             realm))
        (t realm-form))))

(defmethod write-lisp-source ((realm standard-realm) (ignore null) stream)
  (flet ((write-item (key value)
           (declare (ignore key))
           (write-lisp-source realm value stream)))
    (declare (dynamic-extent #'write-item))
    (with-realm-write-lock (realm) 
      ;; Create the realm.
      (format stream ";;;-------------------------------------------------------------------~&;;;
~&;;; ~:(~A~) Realm~&;;;~2%" (realm-name realm))
      (write (intern-realm-form realm) :stream stream)
      (terpri stream)
      ;; create all the groups within the realm.
      (map-groups realm #'write-item)
      ;; Create all users and link them to groups.
      (map-users realm #'write-item)
      ;; Create all access control objects and link them to groups and users.
      (map-access-controls realm #'write-item)
      (terpri stream)
      realm))) 

(define save-authentication-data (&key (pathname *authentication-data-pathname*) if-exists)
  "Top-level function to write a LISP file containing all current realm, group, and user password data.
IF-EXISTS can be either :NEW-VERSION, :SUPERSEDE, :OVERWRITE."
  ;; Make sure the directory exists
  (pathname-create-directory-if-needed pathname)
  (with-open-file (stream pathname :direction :output :if-does-not-exist :create
                          :if-exists (or if-exists #+Genera :new-version #-Genera :supersede))
    (format stream ";;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-
~&;;;~&;;; AUTHENTICATION DATA -- ")
(print-gmt-time stream)
(format stream "~&;;;~2%(in-package :http)~2%")
(flet ((write-realm (name realm)
         (declare (ignore name))
         (write-lisp-source realm nil stream)))
  (declare (dynamic-extent #'write-realm))
  (let ((*print-pretty* t)
        (*print-readably* t))
    (map-realms #'write-realm (realm-table)))
  (format stream "~&;;; End Complete Authentication Data Save
~&;;; Begin Incremental Authentication Data Saves~2%")
  pathname)))

(define-generic save-authentication-object (authentication-object &key pathname &allow-other-keys)
  (declare (values authentication-object))
  (:documentation "Writes AUTHENTICATION-OBJECT to an existing authentication data PATHNAME.
Use this method for incrementally updating this file, and periodically dump a new version of
the pathname when you want to eliminate duplicate entries."))

(defmethod save-authentication-object ((user standard-user) &key (pathname *authentication-data-pathname*))
  (with-open-file (stream pathname :direction :output :if-does-not-exist :error
                          :if-exists :append)
    (write-lisp-source (user-realm user) user stream)))

(defmethod save-authentication-object ((group standard-group) &key (pathname *authentication-data-pathname*))
  (with-open-file (stream pathname :direction :output :if-does-not-exist :error
                          :if-exists :append)
    (write-lisp-source (group-realm group) group stream)))

(defmethod save-authentication-object ((access-control standard-access-control)
                                       &key (pathname *authentication-data-pathname*))
  (with-open-file (stream pathname :direction :output :if-does-not-exist :error
                          :if-exists :append)
    (write-lisp-source (access-control-realm access-control) access-control stream)))

(define restore-authentication-data (&key (pathname *authentication-data-pathname*)
                                          (if-does-not-exist :error) clear-stale-data)
  "Top-level function for restoring authentication information from PATHNAME.
IF-DOES-NOT-EXIST can be :error or NIL. When CLEAR-STALE-DATA is non-null,
all authentication data in dynamic memory is purged."
  (cond ((or (eq if-does-not-exist :error)
             (and (probe-directory pathname)
                  (probe-file pathname)))
         (let ((*read-base* 8))
           ;; clear all previsous data
           (when clear-stale-data (clear-realm-table))
           (load pathname :verbose nil :if-does-not-exist (ecase if-does-not-exist
                                                            ((:error nil) if-does-not-exist)
                                                            (:soft nil))))
         t)
        (t nil)))

(define initialize-server-authentication (&optional (pathname *authentication-data-pathname*))
  "Ensures that password data is loaded.
If no data exists, this creates the SERVER realm using digest scheme and
creates a Webmaster user in the :webmasters group and the Webmasters access control."
  (cond ((restore-authentication-data :pathname pathname :if-does-not-exist :soft))
        ;; define the webmaster realm
        (t (add-realm :server :digest)
           ;; define the group of webmasters in the
           (add-groups :server :webmasters)
           ;; Define a set of capabilities giving the :elite-members group basic access
           (add-access-control-group "Webmasters"
                                     :server
                                     :capabilities '((:default :webmasters)))
           ;; Set up two users, assigning realms and groups.
           (intern-user :server "Webmaster"
                        :groups '(:webmasters)
                        :personal-name "Webmaster"
                        :email-address *bug-http-server*
                        :if-does-not-exist :create)
           (pathname-create-directory-if-needed pathname)
           (save-authentication-data :pathname pathname))))

(add-initialization
  "Initialize Server Authentication"
  '(initialize-server-authentication)
  '(:normal)
  '*server-launch-initialization-list*)


;;;------------------------------------------------------------------- 
;;;
;;; USER METHODS
;;;

(define-generic user-name (user-or-server)
  (:documentation "Returns the unqualified user name."))

(define-generic user-personal-name (user-or-server)
  (:documentation "Returns the unqualified user name."))

(define-generic user-email-address (user-or-server)
  (:documentation "Returns the unqualified user name."))

;; Enable look of users by their email address
(defmethod (setf user-email-address) :around ((email-address string) (user user-email-address-mixin))
  (let ((old-email-address (user-email-address user))
        (realm (user-realm user)))
    (with-slots (user-table) realm
      (prog1 (call-next-method email-address user)
        (with-realm-write-lock (realm)
          (when old-email-address
            (remhash old-email-address user-table))
          (setf (gethash email-address user-table) user))))))

(defmethod (setf user-email-address) :around ((email-address null) (user user-email-address-mixin))
  (let ((old-email-address (user-email-address user))
        (realm (user-realm user)))
    (with-slots (user-table) realm
      (prog1 (call-next-method email-address user)
        (with-realm-write-lock (realm)
          (when old-email-address
            (remhash old-email-address user-table))
          (remhash email-address user-table))))))

(define-generic user-realm (user-or-server)
  (:documentation "Returns the unqualified user name."))

(define-generic user-groups (user-or-server)
  (:documentation "Returns the unqualified user name."))

(defvar *user-name-cache-keys* '(:qualified-name)
  "Holds the cache keys for any user caches containing the user name.")

(define-generic user-clear-name-caches  (user)
  (:documentation "Clears all caches containing the user name for  USER."))

(defmethod user-clear-name-caches ((user user))
  (clear-cached-values user *user-name-cache-keys*))

(define note-user-name-cache-key (key)
  "Adds a new key to the known keys for user name caches."
  (pushnew key *user-name-cache-keys*))

(define-generic user-qualified-name (user-or-server &optional recompute-p)
  (:documentation "Returns the username for user qualified by realm [e.g., \"my-realm|my-username|\"]"))

(defmacro %user-qualified-name (user recompute-p)
  `(with-value-cached (,user :qualified-name :recompute-p ,recompute-p)
     (concatenate 'string (realm-name (user-realm ,user)) "|" (user-name ,user))))

(defmethod user-qualified-name ((user user) &optional recompute-p)
  (%user-qualified-name user recompute-p))

(defmethod qualified-name ((user user) &optional recompute-p)
  (%user-qualified-name user recompute-p))

(define-generic send-mail-to-user (user-or-server subject message-writer &key from keywords
                                                  comments file-references reply-to)
  (declare (values authenticated-user))
  (:documentation "Sends an email message to AUTHENTICATED-USER with SUBJECT from FROM.
MESSAGE-WRITER is either a string or a function accepting a single stream argument."))

(defmethod send-mail-to-user ((user user) subject message-writer
                              &key (from http:*server-mail-address*) keywords comments file-references reply-to)
  (send-mail-from from (user-email-address user) subject message-writer
                  :reply-to reply-to
                  :keywords keywords
                  :comments comments
                  :file-references file-references)
  user)

;;;------------------------------------------------------------------- 
;;;
;;; GROUP METHODS
;;; 

(define-generic group-map-users (group function &optional do-inferiors-p)
  (:documentation "Maps FUNCTION over the users in GROUP.
FUNCTION is applied to the USER-OBJECT.  When DO-INFERIORS-P is
non-null, all users belonging to inferior groups are included."))

(defmethod group-map-users ((group group) function &optional (do-inferiors-p t))
  (loop for user in (group-users group)
        do (funcall function user))
  (when do-inferiors-p
    (loop for group in (group-inferiors group)
          do (group-map-users group function do-inferiors-p))))
  
(define-generic group-user-email-addresses (group &optional do-inferiors-p)
  (:documentation "Returns a list of email addresses for the users in GROUP.
Users with no email address are ignored.  When DO-INFERIORS-P is
non-null, email addresses for all users belonging to inferior groups
are included."))

(defmethod group-user-email-addresses ((group group) &optional (do-inferiors-p t) &aux email-addresses)
  (flet ((fctn (user)
           (let ((email (user-email-address user)))
             (when email
               (push email email-addresses)))))
    (declare (dynamic-extent #'fctn))
    (group-map-users group #'fctn do-inferiors-p)
    (nreverse email-addresses)))

(define-generic send-mail-to-group (group subject message-writer &key from keywords
                                                  comments file-references reply-to hide-recipients-p do-inferiors-p)
  (declare (values authenticated-user))
  (:documentation "Sends an email message to all the users in GROUP with SUBJECT from FROM.  
When DO-INFERIORS-P is non-null, email all users belonging to inferior
groups are included.  MESSAGE-WRITER is either a string or a function
accepting a single stream argument."))

(defmethod send-mail-to-group ((group group) subject message-writer
                               &key (from http:*server-mail-address*) keywords comments file-references reply-to hide-recipients-p do-inferiors-p)
  (let* ((recipients (group-user-email-addresses group do-inferiors-p))
         (to (if hide-recipients-p from recipients))
         (additional-headers (when hide-recipients-p (list :bcc recipients))))
    (send-mail-from from to subject message-writer
                    :additional-headers additional-headers
                    :reply-to reply-to
                    :keywords keywords
                    :comments comments
                    :file-references file-references)
    group))

;;;------------------------------------------------------------------- 
;;;
;;; HTTP/1.0 BASIC AUTHENTICATION SCHEME
;;; 

;; MD5 digesting of passwords could be upgraded to SHA sometime. However, that
;; would break everyone's passwords. Given the lack of security on basic, it
;; is hardly worth it. 3/15/97 -- JCMa.
(defmethod make-user-password-digest ((realm basic-realm-mixin) (username string) (password string))
  (let ((string (concatenate 'string username ":" password)))
    (declare (dynamic-extent string))
    (md5:md5-digest-hexadecimal-string string)))

(defmethod authenticate-user-p ((user basic-user-mixin) credentials method case)
  (declare (values user-or-null)
           (ignore method case))
  (if (md5:md5-with-temporary-digest-string
	(equalp (md5:md5-digest-hexadecimal-string credentials) (user-password-digest user)))
      user
    nil))

(defmethod authenticate-user ((realm basic-realm-mixin) credentials &optional method case)
  "Checks the BASIC authentication CREDENTIALS and returns the user object if valid."
  (declare (values user-object-or-null))
  (when credentials
    (destructuring-bind (authentication-method cookie &rest args) credentials
      (declare (ignore args))
      (case authentication-method
        (:basic
         (let* ((decoded-credentials (base64:base64-decode-vector cookie :decoded-byte-type *standard-character-type*))
                (username (subseq decoded-credentials 0 (char-position #\: decoded-credentials)))
                (user (%realm-get-user realm username)))
           (declare (dynamic-extent decoded-credentials username))
           (when user
             (authenticate-user-p user decoded-credentials method case))))
        (t nil)))))

;; backwards compatibility to digest lists in the first implementation  11/15/95 -- JCMa.
(defmethod (setf user-password-digest) :around ((password-digest cons) (user basic-user-mixin))
  (call-next-method (apply #'md5::md5-hexadecimal-encode password-digest) user))

(defmethod www-authenticate-header-spec ((realm basic-realm-mixin) (method symbol) &rest args)
  (declare (ignore args))
  `(:www-authenticate (,method ,(realm-name realm))))

(defmethod proxy-authenticate-header-spec ((realm basic-realm-mixin) (method symbol) &rest args)
  (declare (ignore args))
  `(:proxy-authenticate (,method ,(realm-name realm))))


;;;-------------------------------------------------------------------
;;;
;;; DIGEST AUTHENTICATION SCHEME
;;;

(defvar *digest-authentication-random-seed* nil)

;; This could use more sources of ergativity.  One good start would be to
;; capture 60 characters from the user at start up.
(defun make-random-seed (&optional old-seed)
  (sha:sha-digest-hexadecimal-string
    (with-output-to-string (stream)
      (write (get-internal-real-time) :stream stream)
      (cond (old-seed (write old-seed :stream stream))
            (t (write (machine-instance) :stream stream)
               (write (get-internal-real-time) :stream stream)
               (write (random 99999999999999999999999999999999999) :stream stream)
               (write (get-internal-run-time) :stream stream)
               (write (software-version) :stream stream)
               ;; toss in a bunch of environment variables
               (dolist (sym '(*gensym-counter* *features* * ** *** + ++ +++ *error-output* *standard-output* *print-level*
                                               *package* *load-pathname* *terminal-io* *query-io* *readtable*
                                               *default-pathname-defaults*))
                 (write (symbol-value sym) :stream stream :escape nil :base 10 :readably nil))))
      (write (get-universal-time) :stream stream)
      (write (get-internal-run-time) :stream stream))))

(defun save-random-seed (&key (pathname *random-seed-pathname*))
  (pathname-create-directory-if-needed pathname)
  (with-open-file (file pathname :if-does-not-exist :create
                        :if-exists #+Genera :overwrite #-Genera :supersede :direction :output)
    (write-string *digest-authentication-random-seed* file)))

;; Capture some new randomness every time the random seed is restored from
;; network and disk latency involved in reading the seed.
(defun restore-random-seed (&key (pathname *random-seed-pathname*))
  (let* ((start (fixnum-microsecond-time))
         (file-p (probe-file pathname))
         (end (fixnum-microsecond-time)))
    (when file-p
      (with-open-file (file pathname :direction :input)
        (let* ((old (read-line file))
               (new-randomness (write-to-string (- end start)))
               (new (concatenate 'string old new-randomness)))
          (declare (dynamic-extent old new-randomness new))
          (sha:sha-digest-hexadecimal-string new))))))

(define digest-authentication-random-seed (&optional force-new-p)
  "Returns the random seed for Digest authentication.
This gets more randomness each time the first digest authentication is
performed for each cold boot of the server.  If you're worried about the
randomness of the seed, you can invoke this function manually with
force-new-p sixteen times."
  (flet ((initialize-random-seed ()
           (let ((old-seed (restore-random-seed)))
             (prog1 (setq *digest-authentication-random-seed* (make-random-seed old-seed))
                    (save-random-seed)))))
    (cond (force-new-p (initialize-random-seed))
          (*digest-authentication-random-seed*)
          (t (initialize-random-seed)))))

;; Must follow server authentication initialization.
(add-initialization
  "Initialize Random Seed"
  '(digest-authentication-random-seed t)
  '(:normal)
  '*server-launch-initialization-list*)

#|
;; old method retired 7/10/2006 -- JCMa
(defun generate-digest-nonce (opaque random-seed &optional (life-time *digest-authentication-nonce-life-time*))
  (flet ((time-window-for-nconce ()
           (write-to-string (truncate (server-request-time *server*) life-time))))
    (declare (inline time-window-for-nconce))
    (let* ((window (time-window-for-nconce))    ;life time for nonces
           (client-address (write-to-string (server-address *server*) :base 8.))        ;prevent replay attack
           (hash (concatenate 'string opaque window client-address random-seed))
           (digest (sha:sha-digest-vector hash))
           (integer-chars (with-fast-array-references ((array digest vector))
                            (loop for i fixnum from 0 to 15 by 2
                                  collect (write-to-string (aref array i))))))
      (declare (dynamic-extent window client-address hash digest integer-chars))
      (apply #'concatenate 'string integer-chars))))
|#

;; faster but conses a little more   4/13/2001 -- JCMa.
(defun generate-digest-nonce (opaque random-seed &optional (life-time *digest-authentication-nonce-life-time*))
  (declare (optimize (speed 3)))
  (flet ((time-window-for-nconce ()
           (write-to-string (truncate (server-request-time *server*) life-time))))
    (declare (inline time-window-for-nconce))
    (let* ((window (time-window-for-nconce))    ;life time for nonces
           (client-address (write-to-string (server-address *server*) :base 8.))        ;prevent replay attack
           (hash (concatenate 'string opaque window client-address random-seed))
           (digest (sha:sha-digest-vector hash))
           (integer-chars (with-fast-array-references ((array digest vector))
                            (loop for i fixnum from 0 to 15 by 2
                                  nconc (digit-chars (aref array i) 8)))))
      (declare (dynamic-extent window client-address hash digest integer-chars))
      (coerce integer-chars 'string))))

(define digest-nonce-opaque-pair ()
  "Returns a dotted list of the nonce and opaque component."
  (declare (values nonce opaque)
           (optimize (speed 3)))
  (let* ((time-factor (write-to-string (get-internal-run-time) :base 10.))
         (opaque (sha:sha-digest-hexadecimal-string time-factor))
         (nonce (generate-digest-nonce opaque (digest-authentication-random-seed))))
    (declare (dynamic-extent time-factor))
    `(,nonce . ,opaque)))

(defun random-cut-string (string &optional (start 0) (end (length string)) (mode :any))
  "Randomly picks an index in the string and flips the starting sequences to follow the ending sequence."
  (declare (fixnum start end))
  (flet ((random-index (range mode)
           (declare (fixnum range))
           (macrolet ((assure-predicate (predicate value)
                        `(if (,predicate ,value)
                             ,value
                           (error "Failed to find an index satisfying ~S." ',predicate))))
             (unless (< 4 range)
               (error "String is too small for a random cut."))
             (let ((state *random-state*)
                   (rounds 20))
               (ecase mode
                 (:any 
                  (random range state))
                 (:even
                  (loop repeat rounds
                        for idx fixnum  = (random range state)
                        until (evenp idx)
                        finally (return (assure-predicate evenp idx))))
                 (:odd
                  (loop repeat rounds
                        for idx fixnum = (random range state)
                        until (oddp idx)
                        finally (return (assure-predicate oddp idx)))))))))
    (let ((pos (the fixnum (random-index (max (- end start) (- end 4)) mode)))
          (new-string (make-array (- end start) :element-type *standard-character-type* :fill-pointer t)))
      (loop for idx fixnum upfrom start below pos
            for idx2 fixnum upfrom (- end pos)
            do (setf (aref new-string idx2) (aref string idx)))
      (loop for idx fixnum upfrom pos below end
            for idx2 fixnum upfrom start
            do (setf (aref new-string idx2) (aref string idx)))
      new-string)))

(define generate-user-password (user-id &optional (n-chars 12) (life-time *digest-authentication-nonce-life-time*))
  "Generates a password for USER-ID that is N-CHARS long.
This is the recommended way to generate new passwords.
Do not specify LIFE-TIME unless you understand what you are doing.
LIFE-TIME is the time window during which the same arguments for 
USER-ID and N-CHARS will yield the same password."
  (labels ((vowel-p (char)
             (find (char-downcase char) "aeiouy" :test #'eql :start 0 :end 6))
           (consonant-p (char)
             (let ((ch (char-downcase char)))
               (unless (find ch "aeiouy" :test #'eql :start 0 :end 6)
                 ch)))
           (next-consonant (seq start end)
             (with-fast-array-references ((seq seq string))
               (loop for idx upfrom start below end
                     for char = (consonant-p (aref seq idx))
                     when char
                     return (values (char-downcase char) (1+ idx))
                     finally (return (next-consonant seq 0 start)))))
           (next-vowel (seq start end)
             (with-fast-array-references ((seq seq string))
               (loop for idx upfrom start below end
                     for char = (vowel-p (aref seq idx)) 
                     when char
                     return (values (char-downcase char) (1+ idx))
                     finally (return (next-vowel seq 0 start))))))
    (declare (inline consonant-p vowel-p))
    (let* ((nonce (generate-digest-nonce user-id (digest-authentication-random-seed) life-time))
           (chars (base64:base64-encode-vector nonce))
           (l (length chars))
           (pw-l (min n-chars l))
           (pw (make-array pw-l :element-type *standard-character-type* :fill-pointer t)))
      (declare (dynamic-extent nonce chars pw)
               (fixnum l n-chars))
      (loop with pos fixnum = 0 and vowel-index fixnum = 0 and consonant-index fixnum = 0 and char
            for idx fixnum upfrom 0 below l
            when (evenp idx)
            do  (multiple-value-setq (char consonant-index)
                    (next-consonant chars consonant-index l))
            else
            do (multiple-value-setq (char vowel-index)
                   (next-vowel chars vowel-index l))
            do (setf (char pw pos) char
                     pos (1+ pos))
            while (< pos n-chars)
            finally (progn (setf (fill-pointer pw) pos)
                      (return (random-cut-string pw 0 pos :even)))))))

(defmethod make-user-password-digest ((realm digest-realm-mixin) (username string) (password string))
  (let ((string (concatenate 'string username ":" (realm-name realm) ":" password)))
    (declare (dynamic-extent string))
    (funcall (digest-realm-digest-function realm) string)))

(declaim (inline accept-nonce-value-p))

(define accept-nonce-value-p (nonce opaque)
  "Returns non-null unless NONCE is stale or invalid, given OPAQUE."
  (let ((local-nonce (generate-digest-nonce opaque (digest-authentication-random-seed))))
    (declare (dynamic-extent local-nonce))
    (equalp nonce local-nonce)))

(defvar *proxy-digest-authentication-allow-relative-uri* nil
  "When non-null, PROXY-AUTHORIZATION headers may use a relative URI, rather than the absolute
request URI required by RFC 2617. This compromises security somewhat by enabling replay attacks to
alternate hosts.")

(defmethod authenticate-user-p ((user digest-user-mixin) credentials http-method case)
  (destructuring-bind (&key nonce opaque response uri &allow-other-keys) (cdr credentials)
    (let ((request-uri (server-raw-request-url-string *server*)))
      ;; Error checking for the multitude of buggy clients
      (unless (equal request-uri uri)  ;According RFC 2617, MUST be equal to URI or digest authentication fails -- JCMa 6/19/2006
        (if (and (eql case :proxy-access) 
                 *proxy-digest-authentication-allow-relative-uri*
                 (equal uri (server-raw-request-relative-url-string *server*)))
            (setq request-uri uri)
          (error 'inconsistent-digest-authorization-uri :authorization credentials :proxy-p (eql case :proxy-access) 
                 :method http-method :url request-uri)))
      ;; Get down to business
      (cond 
       ((accept-nonce-value-p nonce opaque)
        (let* ((realm (user-realm user))
               (digest-fctn (digest-realm-digest-function realm))
               (uri-method (concatenate 'string (symbol-name http-method) ":" request-uri))
               (uri-method-digest (funcall digest-fctn uri-method))
               (user-nonce-uri-method (concatenate 'string (user-password-digest user) ":" nonce ":" uri-method-digest))
               (user-nonce-uri-method-digest (funcall digest-fctn user-nonce-uri-method)))
          (declare (dynamic-extent uri-method uri-method-digest user-nonce-uri-method user-nonce-uri-method-digest))
          (if (equalp response user-nonce-uri-method-digest)
              user
            nil)))
       (t (let ((realm (user-realm user)))
            (error (ecase case
                     (:client-access 'client-access-with-stale-nonce)
                     (:proxy-access 'proxy-access-with-stale-nonce))
                   :method http-method :url request-uri
                   :authentication-method  (realm-scheme realm)
                   :authentication-realm realm)))))))

(defmethod authenticate-user ((realm digest-realm-mixin) credentials &optional http-method case)
  "Checks the digest authentication CREDENTIALS and returns the user object if valid."
  (declare (values user-object-or-null))
  (and credentials
       (destructuring-bind (authentication-method . plist) credentials
         (case authentication-method
           (:digest
            (let* ((username (getf plist :username))
                   (user (%realm-get-user realm username)))
              (and user
                   (authenticate-user-p user credentials http-method case))))
           (t nil)))))

(defmethod www-authenticate-header-spec ((realm digest-realm-mixin) (method symbol) &rest args)
  `(:www-authenticate (,method ,(realm-name realm) :algorithm ,(digest-realm-algorithm realm) ,.args)))

(defmethod proxy-authenticate-header-spec ((realm digest-realm-mixin) (method symbol) &rest args)
  `(:proxy-authenticate (,method ,(realm-name realm) :algorithm ,(digest-realm-algorithm realm) ,.args)))

;;;------------------------------------------------------------------- 
;;;
;;; SSL CERTIFICATE AUTHENTICATION SCHEME
;;; 

#+CL-HTTP-SSL-CLIENT
(defmethod authenticate-user-p ((user certificate-user-mixin) certificate method case)
  (declare (values user-or-null)
           (ignore method case))
  (if (%string-equal (user-personal-name user) (x509-common-name certificate)) ;make sure the names match for extra confidence
      user
    nil))

#+CL-HTTP-SSL-CLIENT
(defmethod acceptable-security-level-p ((realm certificate-realm-security-mixin) peer-certificate service)
  (flet ((acceptable-key-size-p (realm key-bits signature-bits)
           (let ((min-key-size (certificate-realm-key-size realm)))
             (and (<= min-key-size key-bits)
                  (<= min-key-size signature-bits))))
         (acceptable-algorithms-p (realm key-algorithm signature-algorithm)
           (let ((algorithms (certificate-realm-algorithms realm)))
             (and (loop for algorithm in algorithms
                        thereis (string-search (string algorithm) key-algorithm))
                  (loop for algorithm in algorithms
                        thereis (string-search (string algorithm) signature-algorithm))))))
    (when (and (member (x509-issuer-common-name peer-certificate) (certificate-realm-authorities realm) :test #'string-equal)
               (acceptable-key-size-p realm (x509-public-key-bits peer-certificate) (x509-signature-bits peer-certificate))
               (acceptable-algorithms-p realm (x509-public-key-algorithm peer-certificate) (x509-signature-algorithm peer-certificate)))
      (destructuring-bind (ssl-version ciphers verify-mode)
          (ssl-security-parameters service)
        (and (member ssl-version (certificate-realm-ssl-versions realm))
             (member ciphers (certificate-realm-cyphers realm))
             (member verify-mode (certificate-realm-verify-modes realm)))))))

#+CL-HTTP-SSL-CLIENT
(defmethod authenticate-user ((realm certificate-realm-mixin) credentials &optional method case)
  "Checks the certificate authentication credentials and returns the user object if valid."
  (declare (values user-object-or-null))
  (when credentials
    (destructuring-bind (authentication-method peer-certificate &optional https-service &rest args) credentials
      (declare (ignore args))
      (case authentication-method
        (:certificate
         (when (and (case case
                      ((:client-access :proxy-access) (x509-purpose-valid-p peer-certificate :ssl-client))
                      (t nil))
                    (acceptable-security-level-p realm peer-certificate https-service))
           (let* ((username (x509-email-address peer-certificate)) ;lookup by email address
                  (user (%realm-get-user realm username)))
             (when user
               (authenticate-user-p user peer-certificate method case)))))
        (t nil)))))

;;;------------------------------------------------------------------- 
;;;
;;; INTERFACE FOR CREATING ACCOUNTS AND NOTIFYING NEW USERS
;;;

(defgeneric user-personal-name+email (user &optional recompute-p)
  (:documentation "Returns a string suitable for sending email that includes the personal name and the email address."))

(defmethod user-personal-name+email ((user user) &optional recompute-p)
  (with-value-cached (user :name+email :recompute-p recompute-p)
    (let ((email-address (user-email-address user))
          (personal-name (user-personal-name user)))
      (cond ((and personal-name email-address)
             (format nil "~A <~A>" personal-name email-address))
            (email-address email-address)
            (t (error "No Email Address: The user, ~S, has no email address." user))))))

(defgeneric user-first-name (user &optional recompute-p)
  (:documentation "Returns the first name of the user or NIL."))

(defmethod user-first-name ((user user) &optional recompute-p)
  (with-value-cached (user :first-name :recompute-p recompute-p)
    (let ((personal-name (user-personal-name user)))
      (when personal-name
        (let ((pos (char-position #\space personal-name)))
          (subseq personal-name 0 pos))))))

(defclass user-account-info-message
          ()
  ((admin-email :initarg :admin-email :reader uaim-admin-email)
   (admin-name :initarg :admin-name :reader uaim-admin-name)
   (admin-name+email :initarg :admin-name+email :reader uaim-admin-name+email)
   (subject :initarg :subject :reader uaim-subject)
   (url :initarg :url :reader uaim-url)
   (leading-message :initarg :leading-message :reader uaim-leading-message)
   (trailing-message :initarg :trailing-message :reader uaim-trailing-message)))

(defvar *user-account-info-message* nil
  "Holds an object containing the information needed to send users account information.")

(defun %define-user-account-info-message (admin-email admin-name subject url leading-message trailing-message)
  (setq *user-account-info-message* (make-instance 'user-account-info-message 
                                                   :admin-name admin-name
                                                   :admin-email admin-email
                                                   :admin-name+email (format nil "~A <~A>" admin-name admin-email)
                                                   :subject subject
                                                   :url url
                                                   :leading-message leading-message
                                                   :trailing-message trailing-message)))

(defmacro define-user-account-info-message (&key email name subject url leading-message trailing-message)
  "Defines the parameters for sending users account information."
  `(%define-user-account-info-message ',email ',name ,subject ,url ,leading-message ,trailing-message))

(defmethod send-user-account-info ((msg-parameters user-account-info-message) to-user &optional pw (report-stream *standard-output*))
  (with-slots (admin-name+email subject url leading-message trailing-message) msg-parameters
    (flet ((write-message (stream)
             (format stream "~&~A,~2%~A~2%~5T~A~%~10T UID= ~A~% ~:[~15T Use your existing password.~;~10T PW= ~:*~A~]~2%~:[~;~:*~A~]" 
                     (or (user-first-name to-user) (user-personal-name to-user)) leading-message url (user-name to-user) pw trailing-message)))
      (cond (report-stream
             (fast-format report-stream "~&User account info sent to: ~A" (user-personal-name+email to-user)))
            (t (notify-log-window "~&User account info sent to: ~A" (user-personal-name+email to-user))))
      #| (format report-stream "~&from: ~A~%to: ~A~%Subject: ~A~2%" admin-name+email (user-personal-name+email to-user) subject)
      (write-message report-stream)|#
      (send-mail-from admin-name+email #|(user-personal-name+email to-user)|# (user-email-address to-user) subject #'write-message))))

#|(send-user-account-info *user-account-info-message* (intern-user :stanley "JCMallery") "foo-pw" nil) |#

(defgeneric create-user-account+notify (realm group  user-name &optional email-address personal-name report-stream)
  (:documentation "Creates a new user account withing GROUP in REALM with USER-NAME, EMAIL-ADDRESS and PERSONAL-NAME.
PERSONAL-NAME should be first name first."))

(defmethod create-user-account+notify ((realm realm) (groups cons) user-name &optional email-address personal-name  (report-stream *standard-output*))
    (let ((pw (generate-user-password email-address 12 1))
          (groups (mapcar #'(lambda (g) (intern-group realm g)) groups)))
    (declare (dynamic-extent groups))
    (multiple-value-bind (user new-p)
        (intern-user realm user-name
                     :groups groups
                     :password pw
                     :email-address email-address
                     :personal-name personal-name
                     :if-does-not-exist :create)
      (declare (ignore new-p))
      (send-user-account-info *user-account-info-message* user pw report-stream))))

(defmethod create-user-account+notify ((realm realm) (group group) user-name &optional email-address personal-name (report-stream *standard-output*))
  (let ((groups (list group)))
    (create-user-account+notify realm groups user-name email-address personal-name report-stream)))

(defmethod create-user-account+notify ((realm string) group user-name &optional email-address personal-name (report-stream *standard-output*))
  (create-user-account+notify (intern-realm realm) group user-name email-address personal-name report-stream))

(defmethod create-user-account+notify ((realm realm) (group string) user-name &optional email-address personal-name (report-stream *standard-output*))
  (create-user-account+notify realm (intern-group realm group) user-name email-address personal-name report-stream))



