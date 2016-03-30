;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http; -*-
;;;
;;; Copyright 2004-2005, John C. Mallery.
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;; DATABASED AUTHENTICATION
;;;
;;; An interface to relational databases for CL-HTTP USERS, GROUPS, AND REALMS
;;; based on the LispWorks Common SQL facility.
;;;
;;; Developed and tested on LispWorks 4.3.6 under Mac OS X 10.3.3 and MySQL 4.0.18 standard
;;; running in ANSI mode with MyOBDC 3.51.06
;;;
;;; Instructions:
;;;
;;;   1. Set up your database so Lisp can talk to it
;;;   2. If you are running MySQL, perform the following before compiling and loading this file:
;;;
;;;         a. Evaluate (pushnew :MySQL-Database *features*)
;;;         b. If you want transactionable tables, evaluate (pushnew :MySQL-InnoDB *features*)
;;;         c. Compile & load this file.
;;;
;;;   2. Evaluate (CONNECT-TO-AUTHENTICATION-DATABASE <connection-spec> <authentication-database-name>)
;;;
;;;   3. Initialize table structure by evaluating (INITIALIZE-AUTHENTICATION-DATABASE-TABLES)
;;;   
;;;   4. If you don't have a supported database (eg, MySQL), follow the instructions 
;;;      in (DOCUMENTATION 'INITIALIZE-AUTHENTICATION-DATABASE-TABLES 'FUNCTION) to tune your table structure.
;;;
;;;   5. Evaluate (ENABLE-DATABASED-REALMS t) to start creating persistent authentication structures.
;;;
;;;   6. Evaluate (SAVE-AUTHENTICATION-DATA) to tell CL-HTTP to remember these persistent realms.
;;;
;;;   7. Always load this file into your CL-HTTP and add the form (ENABLE-DATABASED-REALMS t) to your
;;;      launch configuration.lisp file before any authentication operations.
;;;
;;; Example:
;;;       ;; this assumes there  a database named authentication
;;;       (connect-to-authentication-database "mysql/root/mysql" "authentication")
;;;       ;; initialize new table structure
;;;       (initialize-authentication-database-tables)
;;;       ;; enable databased realms
;;;       (enable-databased-realms t)
;;;
;;;------------------------------------------------------------------- 
(in-package :http)

(eval-when (:compile-toplevel :execute :load-toplevel)
  #+LispWorks4
  (require "odbc")
  #-LispWorks4
  (cond ((intersection *features* '(:macosx :linux :solaris :windows))
         (push :mysql-native *features*)
         (require "mysql"))
        (t (require "odbc"))))

(export ;Persistent classes
 '(db-basic-group 
   db-basic-realm
   db-basic-user
   db-digest-group
   db-digest-realm
   db-digest-sha-group
   db-digest-sha-realm
   db-digest-sha-user
   db-digest-user
   db-group
   db-realm
   db-user
   db-user-info
   ;; Variables
   *authentication-database*
   user-id
   ;; Functions
   connect-to-authentication-database 
   disconnect-from-authentication-database
   enable-databased-realms
   initialize-authentication-database-tables
   intern-databased-realm
   reset-authentication-database-connection
   user-info-user
   with-databased-realms)
 :http)

;; Enable Common SQL [ ] syntax
(eval-when (:compile-toplevel)
  (sql:enable-sql-reader-syntax))

;;;------------------------------------------------------------------- 
;;;
;;; VARIABLES AND MACROS
;;;

(defvar *authentication-database-type* #+mysql-native :mysql #-mysql-native :obdc)

;; Don't reorder these as %get-databased-realm-class-for-scheme-id on their position 
(defconstant *database-realm-scheme-class-alist* '((:basic . db-basic-realm)
                                                   (:digest . db-digest-realm)
                                                   (:digest-sha . db-digest-sha-realm)))

;; Don't reorder these as %get-databased-realm-class-for-scheme-id on their position 
(defconstant *database-user-scheme-class-alist* '((:basic . db-basic-user)
                                                  (:digest . db-digest-user)
                                                  (:digest-sha . db-digest-sha-user)))

(defun %get-databased-realm-class (scheme)
  (or (cdr (assoc scheme *database-realm-scheme-class-alist* :test #'eq))
      (error "The authentication scheme, ~S, is unknown." scheme)))

(defun %get-databased-realm-class-for-scheme-id (scheme-id)
  (declare (fixnum scheme-id))
  (or (cdr (nth (1- scheme-id) *database-user-scheme-class-alist*))
      (error "The authentication scheme-id, ~S, is unknown." scheme-id)))

(defun %get-databased-realm-scheme-for-scheme-id (scheme-id)
  (declare (fixnum scheme-id))
  (or (car (nth (1- scheme-id) *database-realm-scheme-class-alist*))
      (error "The authentication scheme-id, ~S, is unknown." scheme-id)))

(defun %get-databased-user-class-for-scheme-id (scheme-id)
  (declare (fixnum scheme-id))
  (or (cdr (nth (1- scheme-id) *database-user-scheme-class-alist*))
      (error "The authentication scheme-id, ~S, is unknown." scheme-id)))

(defmacro with-databased-realms (() &body body)
  "Enables databased realm creation within body."
  `(let ((*realm-scheme-class-alist* *database-realm-scheme-class-alist*))
     ,@body))

(defun enable-databased-realms (&optional (on-p t))
  "Globally turns on and off the creation of databased realms."
  (let ((databased-realms-p (eq *realm-scheme-class-alist* *database-realm-scheme-class-alist*)))
    (cond (on-p
           (if databased-realms-p
               *realm-scheme-class-alist*
             ;; Remember default realm schemes and classes
             (setf (get'*realm-scheme-class-alist* :standard-value) *realm-scheme-class-alist*
                   *realm-scheme-class-alist* *database-realm-scheme-class-alist*)))
          (databased-realms-p
           (setq *realm-scheme-class-alist* (get'*realm-scheme-class-alist* :standard-value)))
          (t *realm-scheme-class-alist*))))

(defvar *authentication-database* sql:*default-database*
  "The database to use for HTTP authentication data.")

(defun connect-to-authentication-database (connection-spec &optional (database-name "authentication"))
  "Establishes the connetion to the authentication database CONNECTION-SPEC.
If DATABASE-NAME is non-null, an SQL use command is issued on the connection."
  (prog1 (setq *authentication-database* (sql:connect connection-spec :database-type *authentication-database-type* :if-exists :warn-new))
    (when database-name
      (let ((command (format nil "use ~A;" database-name)))
        (declare (dynamic-extent command))
        (sql:execute-command command :database *authentication-database*)
        (setf (get '*authentication-database* 'database-name) database-name)))
    (setf (get '*authentication-database* 'connection-spec) connection-spec)))

(defun disconnect-from-authentication-database ()
  "Disconnects from the authentication database."
  (when *authentication-database*
    (prog1 (sql:disconnect :database *authentication-database* :error nil)
      (setq *authentication-database* nil))))

(defun reset-authentication-database-connection ()
  "Resets the connection to the authenication database."
  (let ((connection-spec (get '*authentication-database* 'connection-spec))
        (database-name (get '*authentication-database* 'database-name)))
    (cond ((and *authentication-database* connection-spec)
           (disconnect-from-authentication-database)
           (connect-to-authentication-database connection-spec database-name))
          (t (error "Not currently connected to the authentication database.")))))

;; Handles return values correctly unlike sql:with-transaction in LW 4.3.6
(defmacro with-db-transaction ((&key (database '*authentication-database*)) &body body)
  (let ((normal-exit-p-var (gensym))
        (database-var (gensym)))
    `(let ((,normal-exit-p-var nil)
           (,database-var ,database))
       (sql::db-begin-transaction ,database-var)
       (unwind-protect
           (multiple-value-prog1 
               (progn ,@body)
             (setq ,normal-exit-p-var t))
         (if ,normal-exit-p-var (sql:commit :database ,database-var) (sql:rollback :database ,database-var))))))

;; Flag to avoid creating nested transactions
(defvar *inside-authentication-db-transaction* nil)

(defmacro with-authentication-db-transaction ((&key (database '*authentication-database*)) &body body)
  "Provides a transaction environment within the scope of BODY with the database bound to DATABASE."
  `(flet ((do-it () ,@body))
     (declare (dynamic-extent #'do-it))
     (cond (*inside-authentication-db-transaction* (do-it))
           (t (let ((sql:*default-database* ,database)
                    (*inside-authentication-db-transaction* t))
                (with-db-transaction (:database ,database)
                  (do-it)))))))

;; Work-around case-sensitivity in Common-SQL
(defmacro sql-table (table)
  (typecase table
    (string
     (sql:sql-expression :table (make-symbol table)))
    ((or symbol cons)
     (let ((var (gensym "TABLE-")))
       `(let ((,var ,table))
          (sql:sql-expression :table (etypecase ,var
                                       (string (make-symbol ,var))
                                       (symbol ,var))))))))

(defmacro with-class-base-table ((class &key base-table-var) &body body)
  `(let* ((class (find-class ,class))
          (,(or base-table-var (intern "BASE-TABLE" *package*)) (sql::db-class-base-table class)))
     ,@body))

(defgeneric sql-execute-commands (sql-source database &optional inside-transaction-p)
  (:documentation "Executes a series of SQL expressions in SQL-SOURCE on DATABASE.
SQL-SOURCE can be any of a SQL string, SQL stream, or SQL pathname."))

;; Doesn't blow out like LW 4.3.6 sql:execute-command.
(defmethod sql-execute-commands ((sql-expression string) database &optional (inside-transaction-p t))
  (labels ((white-space-char-p (char)
             (member char '(#\space #\tab #\linefeed #\return)))
           (do-expression (string database)
             (loop with end fixnum = (length string)
                   for s = 0 then (fast-position-if-not #'white-space-char-p string (1+ (the fixnum e)) end nil)
                   for e = (and s (char-position #\; string s end))
                   while (and s e)
                   do (let ((sql-exp (subseq string s (1+ (the fixnum e)))))
                        (declare (dynamic-extent sql-exp))
                        (sql:execute-command sql-exp :database database)))))
    (if inside-transaction-p
        (with-authentication-db-transaction (:database database) 
          (do-expression sql-expression database))
      (do-expression sql-expression database))))

(defmethod sql-execute-commands ((sql-stream stream) database &optional (inside-transaction-p t))
  (flet ((do-sql-stream (sql-stream database)
           (using-resource (buffer line-buffer *line-buffer-size*)
             (loop doing (multiple-value-bind (sql-expression error-p delimiter end)
                             (read-delimited-line sql-stream '(#\;) nil buffer)
                           (declare (ignore error-p))
                           (unless sql-expression
                             (return))
                           (unless (blank-line-p buffer 0 end)
                             (vector-push delimiter sql-expression)
                             (sql-execute-commands sql-expression database nil)))))))
    (if inside-transaction-p
        (with-authentication-db-transaction (:database database) 
          (do-sql-stream sql-stream database))
      (do-sql-stream sql-stream database))))

(defmethod sql-execute-commands ((sql-file pathname) database &optional (inside-transaction-p t))
  (with-open-file (file sql-file :direction :input :element-type *standard-character-type* :if-does-not-exist :error)
    (sql-execute-commands file database inside-transaction-p)))

;; Cliche for invalidating a join slots in LW Common SQL
(defmethod invalidate-join-slot ((db-object sql:standard-db-object) (slot symbol))
  (slot-makunbound db-object slot))

(defmethod invalidate-join-slot ((db-object sql:standard-db-object) (slots cons))
  (dolist (slot slots)
    (slot-makunbound db-object slot)))

(defmacro defjoin-method ((reader class) &key home-key foreign-key join-slot join-class-reader (set-p t) target-reader)
  "Defines the join method, READER, on CLASS according to the parameters.
     HOME-KEY is local slot used in the join.
     FOREIGN-key is the foreign slot used in the  join.
     JOIN-SLOT is a slot on CLASS where results are cached.
     JOIN-CLASS-READER is a method on CLASS that returns the join class (a symbol).
     SET-P determines whether the value of join-slot is a list of or an atom.
     TARGET-READER accesses a value from the join class for collection instead of the join instance."
  (let ((collector (if target-reader `(,target-reader join-instance) 'join-instance)))
    `(defmethod ,reader ((,class ,class))
       (with-slots (,home-key ,join-slot) ,class
         (cond ((slot-boundp ,class ',join-slot) ,join-slot)
               (t (loop with join-class = (,join-class-reader ,class)
                        for (join-instance) being each record of [select join-class
                                                                         :where [= [slot-value join-class ',foreign-key] ,home-key]
                                                                         :refresh t]
                        ,@(if set-p
                              `(collect ,collector  into result)
                            `(return (setf ,join-slot ,collector)))
                        ,@(if set-p
                              `(finally (return (setf ,join-slot result)))
                            '(finally (return nil))))))))))

(defmacro define-view-class-accessor (external-accessor internal-accessor slot-name 
                                                         view-class &key provide-setter (value-equality-operator 'equal))
  "Defines accessor methods, EXTERNAL-ACCESSOR, to access and set SLOT-NAME on
VIEW-CLASS using INTERNAL-ACCESSOR for database refresh. When PROVIDE-SETTER
is non-null, a setter is also defined."
  (let ((object view-class))
    `(progn
       (defmethod ,external-accessor ((,object ,view-class))
	 (declare (values ,slot-name newly-refreshed-p))
         (cond ((slot-boundp ,object ',slot-name)
                (,internal-accessor ,object))
               (t (with-authentication-db-transaction ()
                    (values (,internal-accessor ,object) t)))))
       ,.(when provide-setter
           `((defmethod (setf ,external-accessor) (new-value (,object ,view-class))
               (with-authentication-db-transaction ()
                 (let ((old-value (,internal-accessor ,object)))
                   (cond ((,value-equality-operator new-value old-value) new-value)
                         (t (prog1 (setf (,internal-accessor ,object) new-value)
                              (sql:update-record-from-slot ,object ',slot-name :database *authentication-database*)))))))))
       ',external-accessor)))

(defmacro define-user-info-accessor ((method class) &key internal-accessor slot (value-equality-operator 'equal))
  `(progn
     (defmethod ,method ((,class ,class))
       (,internal-accessor (user-info ,class)))
     (defmethod (setf ,method) (string (,class ,class))
       (with-authentication-db-transaction ()
         (let ((user-info (%user-info ,class)))
           (cond ((,value-equality-operator string (,internal-accessor user-info)) string)
                 (t (prog1 (setf (,internal-accessor user-info) string)
                      (sql:update-record-from-slot user-info ',slot)))))))
     ',method))

;;;------------------------------------------------------------------- 
;;;
;;; VIEW CLASS DEFINITIONS
;;;

(sql:def-view-class db-realm
          (standard-lock-mixin
           standard-realm-user-mixin
           standard-realm-group-mixin
           standard-realm-access-control-mixin
           realm sql:standard-db-object)
  ((realm-id :db-kind :key :column |realm_id| :type fixnum :reader realm-id :initarg :realm-id)
   (name :db-kind :key :column |name| :type (string 50) :reader realm-name :initarg :name)
   (groups :db-kind :virtual) ;our own join slot
   (users :db-kind :virtual)) ;our own join slot
  (:documentation "A persistent realm object."))

(sql:def-view-class db-basic-realm
          (db-realm basic-realm-mixin)
  ((scheme :db-kind :virtual :allocation :class :initform :basic :reader realm-scheme)
   (scheme-id :db-kind :virtual :allocation :class :initform 1 :type fixnum :reader scheme-id)
   (group-class :db-kind :virtual :allocation :class :initform 'db-basic-group)
   (user-class :db-kind :virtual :allocation :class :initform 'db-basic-user)
   (access-control-class :db-kind :virtual :allocation :class :initform 'basic-access-control)
   (url-access-control-class :db-kind :virtual :allocation :class :initform 'basic-url-access-control))
  (:base-table |authentication_basic_realms|))

(sql:def-view-class db-digest-realm
          (db-realm md5-algorithm-mixin digest-realm-mixin)
  ((scheme :db-kind :virtual :allocation :class :initform :digest :reader realm-scheme)
   (scheme-id :db-kind :virtual :allocation :class :initform 2 :type fixnum :reader scheme-id)
   (group-class :db-kind :virtual :allocation :class :initform 'db-digest-group)
   (user-class :db-kind :virtual :allocation :class :initform 'db-digest-user)
   (access-control-class :db-kind :virtual :allocation :class :initform 'digest-access-control)
   (url-access-control-class :db-kind :virtual :allocation :class :initform 'digest-url-access-control))
  (:base-table |authentication_digest_realms|))

(sql:def-view-class db-digest-sha-realm
          (db-realm sha-algorithm-mixin digest-realm-mixin)
  ((scheme :db-kind :virtual :allocation :class :initform :digest-sha :reader realm-scheme)
   (scheme-id :db-kind :virtual :allocation :class :initform 3 :type fixnum :reader scheme-id)
   (group-class :db-kind :virtual :allocation :class :initform 'db-digest-sha-group)
   (user-class :db-kind :virtual :allocation :class :initform 'db-digest-sha-user)
   (access-control-class :db-kind :virtual :allocation :class :initform 'digest-access-control)
   (url-access-control-class :db-kind :virtual :allocation :class :initform 'digest-url-access-control))
  (:base-table |authentication_sha_digest_realms|))

;; Map inferior groups to groups via intermediate view class
(sql:def-view-class db-group-inferior-map
          (sql:standard-db-object)
  ((superior-id :db-kind :key :column |superior_id| :type fixnum :reader superior-id :initarg :superior-id)
   (inferior-id :db-kind :key :column |inferior_id| :type fixnum :reader inferior-id :initarg :inferior-id)))

;; A bug in LW 4.3.6 prevents these inferior map classes from workin unless one of them does not require immediate retrieval.
;; Uncomment the work around when Xanalys fixes the bug -- JCMa 04/14/2004
(sql:def-view-class db-basic-group-inferior-map
          (db-group-inferior-map)
  ((superior :db-kind :join 
             :db-info (:home-key superior-id :foreign-key group-id :set nil :join-class db-basic-group #| :retrieval :immediate |#)
             :reader group-inferior-map-superior)
   (inferior :db-kind :join 
             :db-info (:home-key inferior-id :foreign-key group-id :set nil :join-class db-basic-group :retrieval :immediate)
             :reader group-inferior-map-inferior))
  (:base-table |authentication_basic_group_inferior_map|))

(sql:def-view-class db-digest-group-inferior-map
          (db-group-inferior-map)
  ((superior :db-kind :join 
             :db-info (:home-key superior-id :foreign-key group-id :set nil :join-class db-digest-group #| :retrieval :immediate |#)
             :reader group-inferior-map-superior)
   (inferior :db-kind :join 
             :db-info (:home-key inferior-id :foreign-key group-id :set nil :join-class db-digest-group :retrieval :immediate)
             :reader group-inferior-map-inferior))
  (:base-table |authentication_digest_group_inferior_map|))

(sql:def-view-class db-digest-sha-group-inferior-map
          (db-group-inferior-map)
  ((superior :db-kind :join 
             :db-info (:home-key superior-id :foreign-key group-id :set nil :join-class db-digest-sha-group #| :retrieval :immediate |#)
             :reader group-inferior-map-superior)
   (inferior :db-kind :join 
             :db-info (:home-key inferior-id :foreign-key group-id :set nil :join-class db-digest-sha-group :retrieval :immediate)
             :reader group-inferior-map-inferior))
  (:base-table |authentication_sha_digest_group_inferior_map|))

;; Map users into groups via intermediate view class
(sql:def-view-class db-group-user-map
          (sql:standard-db-object)
  ((user-id :db-kind :key :column |user_id| :type fixnum :reader user-id :initarg :user-id)
   (group-id :db-kind :key :column |group_id| :type fixnum :reader group-id :initarg :group-id)))

(sql:def-view-class db-basic-group-user-map
          (db-group-user-map)
  ((user :db-kind :join :db-info (:home-key user-id :foreign-key user-id :set nil :join-class db-basic-user :retrieval :immediate)
         :reader group-user-map-user)
   (group :db-kind :join :db-info (:home-key group-id :foreign-key group-id :set nil :join-class db-basic-group :retrieval :immediate)
          :reader group-user-map-group))
  (:base-table |authentication_basic_group_user_map|))

(sql:def-view-class db-digest-group-user-map
          (db-group-user-map)
  ((user :db-kind :join :db-info (:home-key user-id :foreign-key user-id :set nil :join-class db-digest-user :retrieval :immediate)
         :reader group-user-map-user)
   (group :db-kind :join :db-info (:home-key group-id :foreign-key group-id :set nil :join-class db-digest-group :retrieval :immediate)
          :reader group-user-map-group))
  (:base-table |authentication_digest_group_user_map|))

(sql:def-view-class db-digest-sha-group-user-map
          (db-group-user-map)
  ((user :db-kind :join :db-info (:home-key user-id :foreign-key user-id :set nil :join-class db-digest-sha-user :retrieval :immediate)
         :reader group-user-map-user)
   (group :db-kind :join 
          :db-info (:home-key group-id :foreign-key group-id :set nil :join-class db-digest-sha-group :retrieval :immediate)
          :reader group-user-map-group))
  (:base-table |authentication_sha_digest_group_user_map|))

(sql:def-view-class db-group
          (group sql:standard-db-object)
  ((group-id :db-kind :key :column |group_id| :type fixnum :reader group-id)
   (name :db-kind :key :column |name| :type (string 50) :reader group-name)
   (realm-id :db-kind :base :column |realm_id| :type fixnum :reader realm-id)
   (users :db-kind :virtual) ;our own join slot
   (inferiors :db-kind :virtual) ;our own join slot
   (superiors :db-kind :virtual))) ;our own join slot

(sql:def-view-class db-basic-group 
          (db-group basic-group-mixin)
  ((user-map-class :db-kind :virtual :allocation :class :initform 'db-basic-group-user-map :reader group-user-map-class)
   (inferior-map-class :db-kind :virtual :allocation :class :initform 'db-basic-group-inferior-map :reader group-inferior-map-class)
   (realm :db-kind :join :db-info (:home-key realm-id :foreign-key realm-id :set nil :join-class db-basic-realm) :reader %group-realm))
  (:base-table |authentication_basic_groups|))

(sql:def-view-class db-digest-group
          (db-group digest-group-mixin)
  ((user-map-class :db-kind :virtual :allocation :class :initform 'db-digest-group-user-map :reader group-user-map-class)
   (inferior-map-class :db-kind :virtual :allocation :class :initform 'db-digest-group-inferior-map :reader group-inferior-map-class)
   (realm :db-kind :join :db-info (:home-key realm-id :foreign-key realm-id :set nil :join-class db-digest-realm) :reader %group-realm))
  (:base-table |authentication_digest_groups|))

(sql:def-view-class db-digest-sha-group
          (db-group digest-group-mixin)
  ((user-map-class :db-kind :virtual :allocation :class :initform 'db-digest-sha-group-user-map :reader group-user-map-class)
   (inferior-map-class :db-kind :virtual :allocation :class :initform 'db-digest-sha-group-inferior-map :reader group-inferior-map-class)
   (realm :db-kind :join :db-info (:home-key realm-id :foreign-key realm-id :set nil :join-class db-digest-realm) :reader %group-realm))
  (:base-table |authentication_sha_digest_groups|))

(sql:def-view-class db-user-info
          (sql:standard-db-object)
  ((user-id :db-kind :key :column |user_id| :type fixnum :reader %user-id)
   (realm-id :db-kind :base :column |realm_id| :type fixnum :reader %realm-id)
   (scheme-id :db-kind :base :column |scheme_id| :type fixnum :reader %scheme-id)
   ;; Names are case-sensitive in HTTP
   (name :db-kind :base :column |name| :type (string 25) :accessor %user-name)
   (personal-name :db-kind :base :column |personal_name| :type (string 255) :accessor %user-personal-name)
   (email-address :db-kind :base :column |email| :type (string 255) :accessor %user-email-address)
   (user :db-kind :virtual)) ;our own join slot
  (:base-table |authentication_user_info|))

(sql:def-view-class db-user
          (authenticated-user user-password-mixin user-email-address-mixin user sql:standard-db-object)
  ((user-id :db-kind :key :column |user_id| :type fixnum :reader user-id :initarg :user-id)
   (realm-id :db-kind :base :column |realm_id| :type fixnum :reader realm-id :initarg :realm-id)
   ;; Names are case-sensitive in HTTP
   (name :db-kind :base :column |name| :type (string 25) :accessor %user-name :initarg :username)
   (user-info :db-kind :join :db-info (:home-key user-id :foreign-key user-id :set nil :join-class db-user-info)
              :accessor %user-info :initarg :user-info)
   (groups :db-kind :virtual))) ;our own join slot

(sql:def-view-class db-basic-user 
          (db-user basic-user-mixin)
  ((group-map-class :db-kind :virtual :allocation :class :initform 'db-basic-group-user-map :reader group-user-map-class)
   (password :db-kind :base :column |password| :type (string 32) :accessor %user-password :initarg :password)
   (realm :db-kind :join :db-info (:home-key realm-id :foreign-key realm-id :set nil :join-class db-basic-realm) :accessor %user-realm))
  (:base-table |authentication_basic_users|))

(sql:def-view-class db-digest-user
          (db-user digest-user-mixin)
  ((group-map-class :db-kind :virtual :allocation :class :initform 'db-digest-group-user-map :reader group-user-map-class)
   (password :db-kind :base :column |password| :type (string 32) :accessor %user-password :initarg :password)
   (realm :db-kind :join :db-info (:home-key realm-id :foreign-key realm-id :set nil :join-class db-digest-realm) :accessor %user-realm))
  (:base-table |authentication_digest_users|))

(sql:def-view-class db-digest-sha-user
          (db-user digest-user-mixin)
  ((group-map-class :db-kind :virtual :allocation :class :initform 'db-digest-sha-group-user-map :reader group-user-map-class)
   (password :db-kind :base :column |password| :type (string 40) :accessor %user-password :initarg :password)
   (realm :db-kind :join :db-info (:home-key realm-id :foreign-key realm-id :set nil :join-class db-digest-sha-realm)
          :accessor %user-realm))
  (:base-table |authentication_sha_digest_users|))

;;;------------------------------------------------------------------- 
;;;
;;; TABLE INITIALIZATION
;;;

;; 
#-MySQL-Database
(defun initialize-authentication-database-tables (&key (database *authentication-database*))
  "Intializes the database table structure required for databased authentication.
This definition is an approximation and requires manual tuning of the table and index specifications.

In particular, tables associated with following classes need to automatically increment unique ids:

     DB-REALM: realm_id
     DB-GROUP: group_id
     DB-USER-INFO: user_id

User names need to use case-sensitive comparisons  for compliance with HTTP standards:

DB-USER-INFO, DB-USER: name

All id integers can be made unsiged.

All character specifications can be varchar.

Please add any more items that I may have omitted."
  (flet ((create-realm-structure (class)
           (with-class-base-table (class)
             (sql:create-view-from-class class)
             (sql:create-index [realm_id_index] :on (sql-table base-table) :unique t :attributes '([|realm_id|]))
             (sql:create-index [name_index] :on (sql-table base-table) :unique t :attributes '([|name|]))))
         (create-group-inferior-map-structure (class)
           (with-class-base-table (class)
             (sql:create-view-from-class class)
             (sql:create-index [superior_id_index] :on (sql-table base-table) :attributes '([|superior_id|]))
             (sql:create-index [inferior_id_index] :on (sql-table base-table)  :attributes '([|inferior_id|]))))
         (create-group-user-map-structure (class)
           (with-class-base-table (class)
             (sql:create-view-from-class class)
             (sql:create-index [group_id_index] :on (sql-table base-table) :attributes '([|group_id|]))
             (sql:create-index [user_id_index] :on (sql-table base-table) :attributes '([|user_id|]))))
         (create-group-structure (class)
           (with-class-base-table (class)
             (sql:create-view-from-class class)
             (sql:create-index [group_id_index] :on (sql-table base-table) :unique t :attributes '([|group_id|]))
             (sql:create-index [name_index] :on (sql-table base-table) :attributes '([|name|]))
             (sql:create-index [realm_id_index] :on (sql-table base-table) :attributes '([|realm_id|]))))
         (create-user-info-structure (class)
           (with-class-base-table (class)
             (sql:create-view-from-class class)
             (sql:create-index [user_id_index] :on (sql-table base-table) :unique t :attributes '([|user_id|]))
             (sql:create-index [realm_id_index] :on (sql-table base-table) :attributes '([|realm_id|]))
             (sql:create-index [scheme_id_index] :on (sql-table base-table) :attributes '([|scheme_id|]))
             (sql:create-index [name_index] :on (sql-table base-table) :attributes '([|name|]))))
         (create-user-structure (class)
           (with-class-base-table (class)
             (sql:create-view-from-class class)
             (sql:create-index [user_id_index] :on (sql-table base-table) :attributes '([|user_id|]))
             (sql:create-index [realm_id_index] :on (sql-table base-table) :attributes '([|realm_id|]))
             (sql:create-index [name_index] :on (sql-table base-table) :attributes '([|name|])))))
    (with-authentication-db-transaction (:database database)
      ;; Create realm tables
      (create-realm-structure 'db-basic-realm)
      (create-realm-structure 'db-digest-realm)
      (create-realm-structure 'db-digest-sha-realm)
      ;; Create group inferior map tables
      (create-group-inferior-map-structure 'db-basic-group-inferior-map)
      (create-group-inferior-map-structure 'db-digest-group-inferior-map)
      (create-group-inferior-map-structure 'db-digest-sha-group-inferior-map)
      ;; Create group user map tables
      (create-group-user-map-structure 'db-basic-group-user-map)
      (create-group-user-map-structure 'db-digest-group-user-map)
      (create-group-user-map-structure 'db-digest-sha-group-user-map)
      ;; Create the group tables
      (create-group-structure 'db-basic-group)
      (create-group-structure 'db-digest-group)
      (create-group-structure 'db-digest-sha-group)
      ;; Create user info table
      (create-user-info-structure 'db-user-info)
      ;; Create user tables
      (create-user-structure 'db-basic-user)
      (create-user-structure 'db-digest-user)
      (create-user-structure 'db-digest-sha-user)
      t)))

;; Direct SQL execution because we can't say auto-increment, unsigned, binary string, and primary key via view class interface. 
#+(and MySQL-Database (not MySQL-InnoDB))
(defparameter *sql-to-initialize-authentication-database-tables*
"
CREATE TABLE `authentication_basic_group_inferior_map` (
  `superior_id` int(11) unsigned NOT NULL default '0',
  `inferior_id` int(11) unsigned NOT NULL default '0',
  KEY `SUPERIOR_ID_INDEX` (`superior_id`),
  KEY `INFERIOR_ID_INDEX` (`inferior_id`)
) TYPE=MyISAM;

CREATE TABLE `authentication_basic_group_user_map` (
  `user_id` int(11) unsigned NOT NULL default '0',
  `group_id` int(11) unsigned NOT NULL default '0',
  KEY `GROUP_ID_INDEX` (`group_id`),
  KEY `USER_ID_INDEX` (`user_id`)
) TYPE=MyISAM;

CREATE TABLE `authentication_basic_groups` (
  `group_id` int(11) unsigned NOT NULL auto_increment,
  `name` varchar(50) NOT NULL default '',
  `realm_id` int(11) unsigned default NULL,
  PRIMARY KEY  (`group_id`),
  KEY `REALM_ID_INDEX` (`realm_id`),
  KEY `NAME_INDEX` (`name`)
) TYPE=MyISAM;

CREATE TABLE `authentication_basic_realms` (
  `realm_id` int(11) unsigned NOT NULL auto_increment,
  `name` varchar(50) NOT NULL default '',
  PRIMARY KEY  (`realm_id`),
  UNIQUE KEY `NAME_INDEX` (`name`)
) TYPE=MyISAM;

CREATE TABLE `authentication_basic_users` (
  `user_id` int(11) unsigned NOT NULL default '0',
  `realm_id` int(11) NOT NULL default '0',
  `name` varchar(25) NOT NULL default '',
  `password` varchar(32) default NULL,
  KEY `REALM_ID_INDEX` (`realm_id`),
  KEY `NAME_INDEX` (`name`),
  KEY `USER_ID_INDEX` (`user_id`)
) TYPE=MyISAM;

CREATE TABLE `authentication_digest_group_inferior_map` (
  `superior_id` int(11) unsigned NOT NULL default '0',
  `inferior_id` int(11) unsigned NOT NULL default '0',
  KEY `SUPERIOR_ID_INDEX` (`superior_id`),
  KEY `INFERIOR_ID_INDEX` (`inferior_id`)
) TYPE=MyISAM;

CREATE TABLE `authentication_digest_group_user_map` (
  `user_id` int(11) unsigned NOT NULL default '0',
  `group_id` int(11) unsigned NOT NULL default '0',
  KEY `GROUP_ID_INDEX` (`group_id`),
  KEY `USER_ID_INDEX` (`user_id`)
) TYPE=MyISAM;

CREATE TABLE `authentication_digest_groups` (
  `group_id` int(11) unsigned NOT NULL auto_increment,
  `name` varchar(50) NOT NULL default '',
  `realm_id` int(11) unsigned NOT NULL default '0',
  PRIMARY KEY  (`group_id`),
  KEY `REALM_ID_INDEX` (`realm_id`),
  KEY `NAME_INDEX` (`name`)
) TYPE=MyISAM;

CREATE TABLE `authentication_digest_realms` (
  `realm_id` int(11) unsigned NOT NULL auto_increment,
  `name` varchar(50) NOT NULL default '',
  PRIMARY KEY  (`realm_id`),
  UNIQUE KEY `NAME_INDEX` (`name`)
) TYPE=MyISAM;

CREATE TABLE `authentication_digest_users` (
  `user_id` int(11) unsigned NOT NULL default '0',
  `realm_id` int(11) unsigned NOT NULL default '0',
  `name` varchar(25) binary NOT NULL default '',
  `password` varchar(32) default NULL,
  KEY `REALM_ID_INDEX` (`realm_id`),
  KEY `USER_ID_INDEX` (`user_id`),
  KEY `NAME_INDEX` (`name`)
) TYPE=MyISAM;

CREATE TABLE `authentication_sha_digest_group_inferior_map` (
  `superior_id` int(11) unsigned NOT NULL default '0',
  `inferior_id` int(11) unsigned NOT NULL default '0',
  KEY `SUPERIOR_ID_INDEX` (`superior_id`),
  KEY `INFERIOR_ID_INDEX` (`inferior_id`)
) TYPE=MyISAM;

CREATE TABLE `authentication_sha_digest_group_user_map` (
  `user_id` int(11) unsigned NOT NULL default '0',
  `group_id` int(11) unsigned NOT NULL default '0',
  KEY `GROUP_ID_INDEX` (`group_id`),
  KEY `USER_ID_INDEX` (`user_id`)
) TYPE=MyISAM;

CREATE TABLE `authentication_sha_digest_groups` (
  `group_id` int(11) unsigned NOT NULL auto_increment,
  `name` char(50) NOT NULL default '',
  `realm_id` int(11) unsigned default NULL,
  PRIMARY KEY  (`group_id`),
  KEY `REALM_ID_INDEX` (`realm_id`),
  KEY `NAME_INDEX` (`name`)
) TYPE=MyISAM;


CREATE TABLE `authentication_sha_digest_realms` (
  `realm_id` int(11) unsigned NOT NULL auto_increment,
  `name` char(50) NOT NULL default '',
  PRIMARY KEY  (`realm_id`),
  UNIQUE KEY `NAME_INDEX` (`name`)
) TYPE=MyISAM;

CREATE TABLE `authentication_sha_digest_users` (
  `user_id` int(11) unsigned NOT NULL default '0',
  `realm_id` int(11) unsigned NOT NULL default '0',
  `name` char(25) binary NOT NULL default '',
  `password` char(40) default NULL,
  KEY `REALM_ID_INDEX` (`realm_id`),
  KEY `USER_ID_INDEX` (`user_id`),
  KEY `NAME_INDEX` (`name`)
) TYPE=MyISAM;

CREATE TABLE `authentication_user_info` (
  `user_id` int(11) unsigned NOT NULL auto_increment,
  `realm_id` int(11) unsigned NOT NULL default '0',
  `scheme_id` int(11) unsigned NOT NULL default '0',
  `name` char(25) binary NOT NULL default '',
  `personal_name` char(255) default NULL,
  `email` char(255) default NULL,
  PRIMARY KEY  (`user_id`),
  KEY `REALM_ID_INDEX` (`realm_id`),
  KEY `NAME_INDEX` (`name`),
  KEY `SCHEME_ID_INDEX` (`scheme_id`)
) TYPE=MyISAM;")

#+(and MySQL-Database MySQL-InnoDB)
(defparameter *sql-to-initialize-authentication-database-tables*
  "CREATE TABLE `authentication_basic_realms` (
  `realm_id` tinyint(3) unsigned NOT NULL auto_increment,
  `name` varchar(50) NOT NULL default '',
  PRIMARY KEY  (`realm_id`),
  UNIQUE KEY `NAME_INDEX` (`name`)
) TYPE=InnoDB;

CREATE TABLE `authentication_digest_realms` (
  `realm_id` tinyint(3) unsigned NOT NULL auto_increment,
  `name` varchar(50) NOT NULL default '',
  PRIMARY KEY  (`realm_id`),
  UNIQUE KEY `NAME_INDEX` (`name`)
) TYPE=InnoDB;

CREATE TABLE `authentication_sha_digest_realms` (
  `realm_id` tinyint(3) unsigned NOT NULL auto_increment,
  `name` char(50) NOT NULL default '',
  PRIMARY KEY  (`realm_id`),
  UNIQUE KEY `NAME_INDEX` (`name`)
) TYPE=InnoDB;

CREATE TABLE `authentication_basic_groups` (
  `group_id` smallint(5) unsigned NOT NULL auto_increment,
  `name` varchar(50) NOT NULL default '',
  `realm_id` tinyint(3) unsigned default NULL,
  PRIMARY KEY  (`group_id`),
  KEY `REALM_ID_INDEX` (`realm_id`),
  KEY `NAME_INDEX` (`name`),
  CONSTRAINT `authentication_basic_groups_ibfk_1` FOREIGN KEY (`realm_id`) REFERENCES `authentication_basic_realms` (`realm_id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) TYPE=InnoDB;

CREATE TABLE `authentication_digest_groups` (
  `group_id` smallint(5) unsigned NOT NULL auto_increment,
  `name` varchar(50) NOT NULL default '',
  `realm_id` tinyint(3) unsigned NOT NULL default '0',
  PRIMARY KEY  (`group_id`),
  KEY `REALM_ID_INDEX` (`realm_id`),
  KEY `NAME_INDEX` (`name`),
  CONSTRAINT `authentication_digest_groups_ibfk_1` FOREIGN KEY (`realm_id`) REFERENCES `authentication_digest_realms` (`realm_id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) TYPE=InnoDB;

CREATE TABLE `authentication_sha_digest_groups` (
  `group_id` smallint(5) unsigned NOT NULL auto_increment,
  `name` char(50) NOT NULL default '',
  `realm_id` tinyint(3) unsigned default NULL,
  PRIMARY KEY  (`group_id`),
  KEY `REALM_ID_INDEX` (`realm_id`),
  KEY `NAME_INDEX` (`name`),
  CONSTRAINT `authentication_sha_digest_groups_ibfk_1` FOREIGN KEY (`realm_id`) REFERENCES `authentication_sha_digest_realms` (`realm_id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) TYPE=InnoDB;

CREATE TABLE `authentication_user_info` (
  `user_id` mediumint(8) unsigned NOT NULL auto_increment,
  `realm_id` tinyint(3) unsigned NOT NULL default '0',
  `scheme_id` tinyint(3) unsigned NOT NULL default '0',
  `name` char(25) binary NOT NULL default '',
  `personal_name` char(255) default NULL,
  `email` char(255) default NULL,
  PRIMARY KEY  (`user_id`),
  KEY `REALM_ID_INDEX` (`realm_id`),
  KEY `NAME_INDEX` (`name`),
  KEY `SCHEME_ID_INDEX` (`scheme_id`)
) TYPE=InnoDB;

CREATE TABLE `authentication_basic_users` (
  `user_id` mediumint(8) unsigned NOT NULL default '0',
  `realm_id` tinyint(3) unsigned NOT NULL default '0',
  `name` varchar(25) NOT NULL default '',
  `password` varchar(32) default NULL,
  KEY `REALM_ID_INDEX` (`realm_id`),
  KEY `NAME_INDEX` (`name`),
  KEY `USER_ID_INDEX` (`user_id`),
  CONSTRAINT `authentication_basic_users_ibfk_2` FOREIGN KEY (`realm_id`) REFERENCES `authentication_basic_realms` (`realm_id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `authentication_basic_users_ibfk_1` FOREIGN KEY (`user_id`) REFERENCES `authentication_user_info` (`user_id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) TYPE=InnoDB;

CREATE TABLE `authentication_digest_users` (
  `user_id` mediumint(8) unsigned NOT NULL default '0',
  `realm_id` tinyint(3) unsigned NOT NULL default '0',
  `name` varchar(25) binary NOT NULL default '',
  `password` varchar(32) default NULL,
  KEY `REALM_ID_INDEX` (`realm_id`),
  KEY `USER_ID_INDEX` (`user_id`),
  KEY `NAME_INDEX` (`name`),
  CONSTRAINT `authentication_digest_users_ibfk_1` FOREIGN KEY (`realm_id`) REFERENCES `authentication_digest_realms` (`realm_id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `authentication_digest_users_ibfk_2` FOREIGN KEY (`user_id`) REFERENCES `authentication_user_info` (`user_id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) TYPE=InnoDB;

CREATE TABLE `authentication_sha_digest_users` (
  `user_id` mediumint(8) unsigned NOT NULL default '0',
  `realm_id` tinyint(3) unsigned NOT NULL default '0',
  `name` char(25) binary NOT NULL default '',
  `password` char(40) default NULL,
  KEY `REALM_ID_INDEX` (`realm_id`),
  KEY `USER_ID_INDEX` (`user_id`),
  KEY `NAME_INDEX` (`name`),
  CONSTRAINT `authentication_sha_digest_users_ibfk_3` FOREIGN KEY (`realm_id`) REFERENCES `authentication_sha_digest_realms` (`realm_id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `authentication_sha_digest_users_ibfk_2` FOREIGN KEY (`user_id`) REFERENCES `authentication_user_info` (`user_id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) TYPE=InnoDB;

CREATE TABLE `authentication_basic_group_inferior_map` (
  `superior_id` smallint(5) unsigned NOT NULL default '0',
  `inferior_id` smallint(5) unsigned NOT NULL default '0',
  KEY `SUPERIOR_ID_INDEX` (`superior_id`),
  KEY `INFERIOR_ID_INDEX` (`inferior_id`),
  CONSTRAINT `authentication_basic_group_inferior_map_ibfk_1` FOREIGN KEY (`superior_id`) REFERENCES `authentication_basic_groups` (`group_id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `authentication_basic_group_inferior_map_ibfk_2` FOREIGN KEY (`inferior_id`) REFERENCES `authentication_basic_groups` (`group_id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) TYPE=InnoDB;

CREATE TABLE `authentication_digest_group_inferior_map` (
  `superior_id` smallint(5) unsigned NOT NULL default '0',
  `inferior_id` smallint(5) unsigned NOT NULL default '0',
  KEY `SUPERIOR_ID_INDEX` (`superior_id`),
  KEY `INFERIOR_ID_INDEX` (`inferior_id`),
  CONSTRAINT `authentication_digest_group_inferior_map_ibfk_1` FOREIGN KEY (`superior_id`) REFERENCES `authentication_digest_groups` (`group_id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `authentication_digest_group_inferior_map_ibfk_2` FOREIGN KEY (`inferior_id`) REFERENCES `authentication_digest_groups` (`group_id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) TYPE=InnoDB;

CREATE TABLE `authentication_sha_digest_group_inferior_map` (
  `superior_id` smallint(5) unsigned NOT NULL default '0',
  `inferior_id` smallint(5) unsigned NOT NULL default '0',
  KEY `SUPERIOR_ID_INDEX` (`superior_id`),
  KEY `INFERIOR_ID_INDEX` (`inferior_id`),
  CONSTRAINT `authentication_sha_digest_group_inferior_map_ibfk_1` FOREIGN KEY (`superior_id`) REFERENCES `authentication_sha_digest_groups` (`group_id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `authentication_sha_digest_group_inferior_map_ibfk_2` FOREIGN KEY (`inferior_id`) REFERENCES `authentication_sha_digest_groups` (`group_id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) TYPE=InnoDB;

CREATE TABLE `authentication_basic_group_user_map` (
  `user_id` mediumint(8) unsigned NOT NULL default '0',
  `group_id` smallint(5) unsigned NOT NULL default '0',
  KEY `GROUP_ID_INDEX` (`group_id`),
  KEY `USER_ID_INDEX` (`user_id`),
  CONSTRAINT `authentication_basic_group_user_map_ibfk_1` FOREIGN KEY (`user_id`) REFERENCES `authentication_basic_users` (`user_id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `authentication_basic_group_user_map_ibfk_2` FOREIGN KEY (`group_id`) REFERENCES `authentication_basic_groups` (`group_id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) TYPE=InnoDB;

CREATE TABLE `authentication_digest_group_user_map` (
  `user_id` mediumint(8) unsigned NOT NULL default '0',
  `group_id` smallint(5) unsigned NOT NULL default '0',
  KEY `GROUP_ID_INDEX` (`group_id`),
  KEY `USER_ID_INDEX` (`user_id`),
  CONSTRAINT `authentication_digest_group_user_map_ibfk_1` FOREIGN KEY (`user_id`) REFERENCES `authentication_digest_users` (`user_id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `authentication_digest_group_user_map_ibfk_2` FOREIGN KEY (`group_id`) REFERENCES `authentication_digest_groups` (`group_id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) TYPE=InnoDB;

CREATE TABLE `authentication_sha_digest_group_user_map` (
  `user_id` mediumint(8) unsigned NOT NULL default '0',
  `group_id` smallint(5) unsigned NOT NULL default '0',
  KEY `GROUP_ID_INDEX` (`group_id`),
  KEY `USER_ID_INDEX` (`user_id`),
  CONSTRAINT `authentication_sha_digest_group_user_map_ibfk_1` FOREIGN KEY (`user_id`) REFERENCES `authentication_sha_digest_users` (`user_id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `authentication_sha_digest_group_user_map_ibfk_2` FOREIGN KEY (`group_id`) REFERENCES `authentication_sha_digest_groups` (`group_id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) TYPE=InnoDB;")

#+MySQL-Database
(defun initialize-authentication-database-tables (&key (database *authentication-database*))
  "Intializes the database table structure required for databased authentication.
This utilized SQL that has already been tuned for the  database."
  (sql-execute-commands *sql-to-initialize-authentication-database-tables* database))

;;;------------------------------------------------------------------- 
;;;
;;; MAPPING FUNCTIONS
;;;

(defmethod map-db-group-inferior-map ((group db-group) function)
  (let ((group-inferior-map-class (group-inferior-map-class group))
        (group-id (group-id group)))
    (sql:do-query ((group-map) [select group-inferior-map-class
                                       :where [= [slot-value group-inferior-map-class 'superior-id] group-id]
                                       :refresh t])
                  (funcall function group-map))))

(defmethod map-db-group-superior-map ((group db-group) function)
  (let ((group-inferior-map-class (group-inferior-map-class group))
        (group-id (group-id group)))
    (sql:do-query ((group-map) [select group-inferior-map-class
                                       :where [= [slot-value group-inferior-map-class 'inferior-id] group-id]
                                       :refresh t])
                  (funcall function group-map))))

(defmethod map-db-group-user-map ((group db-group) function)
  (let ((group-user-map-class (group-user-map-class group)))
    (sql:do-query ((group-user-map) [select group-user-map-class
                                            :where [= [slot-value group-user-map-class 'group-id] (group-id group)]
                                            :refresh t])
                  (funcall function group-user-map))))

(defmethod map-db-user-group-map ((user db-user) function)
  (let ((group-user-map-class (group-user-map-class user)))
    (sql:do-query ((group-user-map) [select group-user-map-class
                                            :where [= [slot-value group-user-map-class 'user-id] (user-id user)]
                                            :refresh t]
                   :get-all t)
                  (funcall function group-user-map))))

(defmethod map-users ((group db-group) function)
  (flet ((fctn (map)
           (let ((user (group-user-map-user map)))
             (funcall function (user-name group) user))))
    (map-db-group-user-map group #'fctn)))

(defmethod map-groups ((user db-user) function)
  (flet ((fctn (map)
           (let ((group (group-user-map-group map)))
             (funcall function (group-name group) group))))
    (map-db-user-group-map user #'fctn)))

(defmethod map-users ((realm db-realm) function)
  (with-slots (realm-id) realm
    (let ((db-user (realm-user-class realm)))
      (sql:do-query ((user) [select db-user :where [= [slot-value db-user 'realm-id] realm-id] :refresh t])
                    (funcall function (user-name user) user)))))

(defmethod map-groups ((realm db-realm) function)
  (with-slots (realm-id) realm
    (let ((db-group (realm-group-class realm)))
      (sql:do-query ((group) [select db-group :where [= [slot-value db-group 'realm-id] realm-id] :refresh t])
                    (funcall function (group-name group) group)))))

;;;------------------------------------------------------------------- 
;;;
;;; REALM SUPPORT CODE
;;;

(defjoin-method (%realm-groups db-realm)
                :home-key realm-id
                :foreign-key realm-id
                :join-slot groups
                :join-class-reader realm-group-class)

(defjoin-method (%realm-users db-realm)
                :home-key realm-id
                :foreign-key realm-id
                :join-slot users
                :join-class-reader realm-user-class)

(defun %find-realm-named (scheme realm-name)
  (let ((realm-class (%get-databased-realm-class scheme)))
    (sql:do-query ((realm) [select realm-class :where [= [slot-value realm-class 'name] realm-name] :refresh t])
                  (return-from %find-realm-named realm)))
  nil)

(defgeneric %initialize-realm (db-realm)
  (:documentation "Initializes all the dynamic caches for a databased realm."))

(defmethod %initialize-realm ((realm db-realm))
  (let ((user-table (realm-user-table realm))
	(group-table (realm-group-table realm))
	(groups (%realm-groups realm))
	(users (%realm-users realm)))
    (dolist (group groups)
      (setf (gethash (group-name group) group-table) group))
    (dolist (user users)
      (setf (gethash (user-name user) user-table) user)))
  realm)

(defun %make-db-realm (scheme name)
  (flet ((make-the-realm (scheme name)
           (let ((realm-class (%get-databased-realm-class scheme))
                 (values (list name)))
             (declare (dynamic-extent values))
             ;; insert the specialized records
             (with-class-base-table (realm-class)
               (sql:insert-records :into (sql-table base-table) :attributes '(|name|) :values values))
             ;; Return the specialized realm class
             (sql:do-query ((realm) [select realm-class :where [= [slot-value realm-class 'name] name] :refresh t])
                           (return-from make-the-realm realm)))
           (error "Failed to create the new realm. This should never happen.")))
    (with-authentication-db-transaction ()
      (let ((realm (%find-realm-named scheme name)))
        (cond (realm (%initialize-realm realm)) ;initialize existing realms fetched from database
              (t (make-the-realm scheme name)))))))

;;;------------------------------------------------------------------- 
;;;
;;; USER SUPPORT CODE
;;;

(defjoin-method (%user-groups db-user)
                :home-key user-id
                :foreign-key user-id
                :join-slot groups
                :join-class-reader group-user-map-class
                :target-reader group-user-map-group)

(define-view-class-accessor user-info %user-info user-info db-user)

(define-user-info-accessor (user-personal-name db-user)
                           :internal-accessor %user-personal-name
                           :slot personal-name)

(define-user-info-accessor (user-email-address db-user)
                           :internal-accessor %user-email-address
                           :slot email-address)

(defun %find-user-info-named (realm-id scheme-id user-name)
  (sql:do-query ((user-info) [select 'db-user-info ;; User-names are case-sensitive in HTTP 
                                     :where [and [like [slot-value 'db-user-info 'name] user-name] ;;[binary user-name]]
                                                 [= [slot-value 'db-user-info 'realm-id] realm-id]
                                                 [= [slot-value 'db-user-info 'scheme-id] scheme-id]]])
                (when (string= (%user-name user-info) user-name) ;case-sensitive comparison in LISP until we find out SQL way
                  (return-from %find-user-info-named user-info)))
  nil)

(defmethod user-realm ((user-info db-user-info))
  (user-realm (user-info-user user-info)))

(defmethod print-object ((user-info db-user-info) stream)
  (with-slots (scheme-id name) user-info 
    (print-unreadable-object (user-info stream :type t :identity t)
      (when (slot-boundp user-info 'name)
        (format stream "~A (~A|~A)" name (%get-databased-realm-scheme-for-scheme-id scheme-id)
                (realm-name (user-realm user-info)))))))

(defgeneric user-info-user (db-user-info)
  (:documentation "Returns the databased user object associated with DB-USER-INFO."))

(defmethod user-info-user ((user-info db-user-info))
  (with-slots (scheme-id user-id user) user-info
    (cond ((slot-boundp user-info 'user) user)
          (t (with-authentication-db-transaction ()
               (loop with user-class = (%get-databased-user-class-for-scheme-id scheme-id)
                     for (db-user) being each record of [select user-class
                                                                :where [= [slot-value user-class 'user-id] user-id]
                                                                :refresh t]
                     return (setf user db-user)))))))

(defmethod %find-user-named ((realm db-realm) (user-name string))
  (let ((db-user-class (realm-user-class realm)))
    (sql:do-query 
     ((user) [select db-user-class ;; User-names are case-sensitive in HTTP 
                     :where [and [like [slot-value db-user-class 'name] user-name] ;;[binary user-name]]
                                 [= [slot-value db-user-class 'realm-id] (realm-id realm)]]
                     :refresh t])
     (when (string= (user-name user) user-name) ;case-sensitive comparison in LISP until we find out SQL way
       (return-from %find-user-named user)))
    nil))

(defmethod %find-user-with-user-id ((realm db-realm) (user-id fixnum))
  (let ((db-user-class (realm-user-class realm)))
    (sql:do-query 
     ((user-id) [select db-user-class ;; User-names are case-sensitive in HTTP 
                        :where [and [like [slot-value db-user-class 'user-id] user-id]
                                    [= [slot-value db-user-class 'realm-id] (realm-id realm)]]
                        :refresh t])
     (return-from %find-user-with-user-id user-id))
    nil))

(defmethod intern-user ((realm db-realm) (user integer) &key (if-does-not-exist :error) password groups personal-name email-address 
                        &allow-other-keys &aux user-object)
  (declare (values interned-user newly-created-p))
  (flet ((get-user (realm id)
           (with-authentication-db-transaction ()
             (http::%find-user-with-user-id realm id))))
    (setq user-object (ecase if-does-not-exist
                        (:soft (get-user realm user))
                        (:error
                         (or (http::%find-user-with-user-id realm user)
                             (error "There is no user, ~S, in the realm, ~S." user (realm-name realm))))
                        (:create (error "The :CREATE option for :IF-DOES-NOT-EXIST makes no sense for ~S, in the realm, ~S." user (realm-name realm)))))
    (cond-every
     (password (setf (user-password-digest user-object) (make-user-password-digest realm user password)))
     (groups
      (dolist (group groups)
        (group-add-user (intern-group realm group :if-does-not-exist :create) user-object)))
     (email-address (setf (user-email-address user-object) email-address))
     (personal-name (setf (user-personal-name user-object) personal-name)))
    (values user-object)))

;;;------------------------------------------------------------------- 
;;;
;;; GROUP SUPPORT CODE
;;;

(defjoin-method (%group-inferiors db-group)
                :home-key group-id
                :foreign-key superior-id
                :join-slot inferiors
                :join-class-reader group-inferior-map-class
                :target-reader group-inferior-map-inferior)

(defjoin-method (%group-superiors db-group)
                :home-key group-id
                :foreign-key inferior-id
                :join-slot superiors
                :join-class-reader group-inferior-map-class
                :target-reader group-inferior-map-superior)

(defjoin-method (%group-users db-group)
                :home-key group-id
                :foreign-key group-id
                :join-slot users
                :join-class-reader group-user-map-class
                :target-reader group-user-map-user)

(defmethod %find-group-named ((realm db-realm) group-name)
  (let ((db-group-class (realm-group-class realm)))
    (sql:do-query ((group) [select db-group-class :where [and [= [slot-value db-group-class 'name] group-name]
                                                              [= [slot-value db-group-class 'realm-id] (realm-id realm)]]])
                  (return-from %find-group-named group))
    nil))

;;;------------------------------------------------------------------- 
;;;
;;; REALM API
;;;

(define-view-class-accessor realm-groups %realm-groups groups db-realm)
(define-view-class-accessor realm-users %realm-users users db-realm)

(defun intern-databased-realm (realm &key (if-does-not-exist :error) (scheme :basic))
  "Used to create databased realms with databased users."
  (declare (values interned-realm newly-created-p))
  (case if-does-not-exist
    (:create
     (with-databased-realms ()
       (intern-realm realm :if-does-not-exist if-does-not-exist :scheme scheme)))
    (t (intern-realm realm :if-does-not-exist if-does-not-exist))))

(defmethod intern-realm-form ((realm db-realm))
  `(intern-databased-realm ,(realm-name realm)
			   :if-does-not-exist :create
			   :scheme ,(realm-scheme realm)))

(defmethod make-realm ((name string) (class (eql 'db-basic-realm)))
  (%make-db-realm :basic name))

(defmethod make-realm ((name string) (class (eql 'db-digest-realm)))
  (%make-db-realm :digest name))

(defmethod make-realm ((name string) (class (eql 'db-digest-sha-realm)))
  (%make-db-realm :digest-sha name))

;; Safer to use database mapping methods
(defmethod unintern-realm :before ((realm db-realm))
  (flet ((delete-group (name group)
           (declare (ignore name))
           (unintern-group realm group))
         (delete-user (name user)
           (declare (ignore name))
           (unintern-user realm user)))
    (declare (dynamic-extent #'delete-group #'delete-user))
    (map-groups realm #'delete-group)
    (map-users realm #'delete-user)))

(defmethod unintern-realm :after ((realm db-realm))
  (sql:delete-instance-records realm))

(defmethod unintern-realm :around ((realm db-realm))
  (with-authentication-db-transaction ()
    (call-next-method realm)))

(defmethod write-lisp-source ((realm db-realm) (ignore null) stream)
  (flet ((write-item (key value)
           (declare (ignore key))
           (write-lisp-source realm value stream)))
    (declare (dynamic-extent #'write-item))
    ;; Create the realm.
    (format stream ";;;-------------------------------------------------------------------~&;;; ~
                  ~&;;; ~:(~A~) Realm (Databased Groups & Users)~&;;;~2%" (realm-name realm))
    (write (intern-realm-form realm) :stream stream)
    (terpri stream)
    ;; Create all access control objects and link them to groups and users.
    (map-access-controls realm #'write-item)
    (terpri stream)
    realm))

;;;------------------------------------------------------------------- 
;;;
;;; DATABASE USER API
;;;

(define-view-class-accessor user-groups %user-groups groups db-user)
(define-view-class-accessor user-name %user-name name db-user :provide-setter t)
(define-view-class-accessor user-password-digest %user-password password db-user :provide-setter t)
(define-view-class-accessor user-realm %user-realm realm db-user)

(defmethod update-user :around ((user db-user) &key (inside-transaction-p t) &allow-other-keys)
  (if inside-transaction-p
      (with-authentication-db-transaction ()
        (call-next-method))
    (call-next-method)))

(defmethod make-user ((realm db-realm) user-name &optional password email-address personal-name)
  (declare (values user-object newly-created-p))
  (flet ((make-db-user (user-class realm-id user-name password-digest user-info)
           (let ((user (make-instance user-class
                                      :user-id (%user-id user-info)
                                      :realm-id realm-id
                                      :username user-name
                                      :password password-digest
                                      :user-info user-info)))
             (sql:update-records-from-instance user :database *authentication-database*)
             (invalidate-join-slot realm 'users)
             user)))
    (with-authentication-db-transaction ()
      (let ((user (%find-user-named realm user-name)))
        (cond (user
               (update-user user :personal-name personal-name :email-address email-address :password password :inside-transaction-p nil)
               user)
              (t (let* ((realm-id (realm-id realm))
                        (scheme-id (scheme-id realm))
                        (user-info (%find-user-info-named realm-id scheme-id user-name))
                        (user-class (realm-user-class realm))
                        (password-digest (and password (make-user-password-digest realm user-name password))))
                   ;; catch partial user creation and continue
                   (cond (user-info 
                          (values (make-db-user user-class realm-id user-name password-digest user-info) t))
                         (t (let ((values (list realm-id scheme-id user-name personal-name email-address)))
                              (declare (dynamic-extent values))
                              (sql:insert-records :into (sql-table "authentication_user_info")
                                                  :attributes '(|realm_id| |scheme_id| |name| |personal_name| |email|)
                                                  :values values)
                              ;; fetch the user-info with unique user-id
                              (unless (setq user-info (%find-user-info-named realm-id scheme-id user-name))
                                (error "No user info created. This should never happen."))
                              (values (make-db-user user-class realm-id user-name password-digest user-info) t)))))))))))

(defmethod unintern-user :before ((realm db-realm) (user db-user))
  (flet ((delete-group-memberships (map)
           (let ((group (group-user-map-group map)))
             (invalidate-join-slot group 'users)
             (sql:delete-instance-records map))))
    ;; delete any group memberships
    (map-db-user-group-map user #'delete-group-memberships)))

(defmethod unintern-user :after ((realm db-realm) (user db-user))
  (let ((user-info (user-info user)))
    (sql:delete-instance-records user)
    (invalidate-join-slot realm 'users)
    ;; delete the user info record last because of foreign key constraints on user-id of user
    (sql:delete-instance-records user-info)))

(defmethod unintern-user :around ((realm db-realm) (user db-user))
  (with-authentication-db-transaction ()
    (call-next-method realm user)))

(defmethod %rename-user :around ((realm standard-realm-user-mixin) (user user) (old-name string) (new-name string))
  (let ((database *authentication-database*)
        existing-user)
    (with-authentication-db-transaction (:database database)
      (unless (setq existing-user (%find-user-named realm new-name))
        (let ((user-info (user-info user)))
          (setf (%user-name user) new-name)
          (sql:update-record-from-slot user 'name :database database)
          (setf (%user-name user-info) new-name)
          (sql:update-record-from-slot user-info 'name :database database))))
    (cond (existing-user
           (values existing-user nil))
          (t (call-next-method)
             (values user t)))))

(defmethod save-authentication-object ((user db-user) &key (pathname *authentication-data-pathname*))
  (declare (ignore pathname))
  user)

(defmethod write-lisp-source ((realm db-realm) (user db-user) stream)
  (declare (ignore stream))
  user)

;;;------------------------------------------------------------------- 
;;;
;;; DATABASE GROUP API
;;;

(define-view-class-accessor group-realm %group-realm realm db-group)
(define-view-class-accessor group-inferiors %group-inferiors inferiors db-group)
(define-view-class-accessor group-superiors %group-superiors superiors db-group)
(define-view-class-accessor group-users %group-users users db-group)

(defmethod make-group ((realm db-realm) group-name)
  (declare (values group newly-created-p))
  (flet ((make-the-group (realm group-name)
           (let* ((realm-id (realm-id realm))
                  (db-group-class (realm-group-class realm))
                  (values (list group-name realm-id))
                  group)
             (declare (dynamic-extent values))
             (with-class-base-table (db-group-class :base-table-var group-table)
               (sql:insert-records :into (sql-table group-table) :attributes '(|name| |realm_id|) :values values))
             ;; fetch the group object
             (sql:do-query ((db-group) [select db-group-class :where [and [= [name] group-name] [= [realm_id] realm-id]] :refresh t])
                           (setq group db-group)
                           (invalidate-join-slot realm 'groups)
                           (return))
             (values group t))))
    (declare (inline make-the-group))
    (with-authentication-db-transaction ()
      (cond ((%find-group-named realm group-name))
            (t (make-the-group realm group-name))))))

(defmethod unintern-group :before ((realm db-realm) (group db-group))
  (flet ((delete-user-memberships (map)
           (let ((user (group-user-map-user map)))
             (invalidate-join-slot user 'groups)
             (sql:delete-instance-records map)))
         (delete-inferior-registries (map)
           (let ((inferior (group-inferior-map-inferior map)))
             (invalidate-join-slot inferior 'superiors)
             (sql:delete-instance-records map)))
         (delete-superior-registries (map)
           (let ((superior (group-inferior-map-superior map)))
             (invalidate-join-slot superior 'inferiors)
             (sql:delete-instance-records map))))
    ;; delete any group memberships
    (map-db-group-user-map group #'delete-user-memberships)
    (map-db-group-inferior-map group #'delete-inferior-registries)
    (map-db-group-superior-map group #'delete-superior-registries)))

(defmethod unintern-group :after ((realm db-realm) (group db-group))
  (sql:delete-instance-records group)
  (invalidate-join-slot realm 'groups))

(defmethod unintern-group :around ((realm db-realm) (group db-group))
  (with-authentication-db-transaction ()
    (call-next-method realm group)))

(defmethod group-add-inferior ((superior db-group) (inferior db-group))
  (flet ((%find-group-inferior-map (db-group-inferior-map-class superior-id inferior-id)
           (sql:do-query ((map) [select db-group-inferior-map-class
                                        :where [and [= [slot-value db-group-inferior-map-class 'superior-id] superior-id]
                                                    [= [slot-value db-group-inferior-map-class 'inferior-id] inferior-id]]
                                        :refresh t]
                          :get-all t)
                         (return-from %find-group-inferior-map map))
           nil))
    (declare (inline %find-group-inferior-map))
    (let ((db-group-inferior-map-class (group-inferior-map-class superior))
          (superior-id (group-id superior))
          (inferior-id (group-id inferior)))
      (with-authentication-db-transaction ()
        (unless (%find-group-inferior-map db-group-inferior-map-class superior-id inferior-id)
          (let ((map (make-instance db-group-inferior-map-class :superior-id superior-id :inferior-id inferior-id)))
            (sql:update-records-from-instance map :database *authentication-database*)
            ;; trigger transient slot refresh on next access
            (invalidate-join-slot superior 'inferiors)
            (invalidate-join-slot inferior 'superiors)))))))

(defmethod group-remove-inferior ((superior db-group) (inferior db-group))
  (let ((db-group-inferior-map-class (group-inferior-map-class superior))
        (superior-id (group-id superior))
        (inferior-id (group-id inferior))
        deleted-p)
    (with-authentication-db-transaction ()
      (sql:do-query ((map) [select db-group-inferior-map-class 
                                   :where [and [= [slot-value db-group-inferior-map-class 'superior-id] superior-id]
                                               [= [slot-value db-group-inferior-map-class 'inferior-id] inferior-id]]
                                   :refresh t]
                     :get-all t)
                    (sql:delete-instance-records map)
                    ;; trigger transient slot refresh on next access
                    (invalidate-join-slot superior 'inferiors)
                    (invalidate-join-slot inferior 'superiors)
                    (setq deleted-p t)))
    deleted-p))

(defmethod group-add-user ((group db-group) (user db-user))
  (flet ((%find-group-user-map (db-group-user-map-class group-id user-id)
           (sql:do-query ((map) [select db-group-user-map-class 
                                        :where [and [= [slot-value db-group-user-map-class 'group-id] group-id]
                                                    [= [slot-value db-group-user-map-class 'user-id] user-id]]
                                        :refresh t]
                          :get-all t)
                         (return-from %find-group-user-map map))
           nil))
    (declare (inline %find-group-user-map))
    (let ((db-group-user-map-class (group-user-map-class group))
          (group-id (group-id group))
          (user-id (user-id user)))
      (with-authentication-db-transaction ()
        (unless (%find-group-user-map db-group-user-map-class group-id user-id)
          (let ((map (make-instance db-group-user-map-class :group-id group-id :user-id user-id)))
            (sql:update-records-from-instance map :database *authentication-database*)
            ;; trigger transient slot refresh on next access
            (invalidate-join-slot group 'users)
            (invalidate-join-slot user 'groups)))))))

(defmethod group-remove-user ((group db-group) (user db-user))
  (let ((db-group-user-map-class (group-user-map-class group))
        (group-id (group-id group))
        (user-id (user-id user))
        deleted-p)
    (with-authentication-db-transaction ()
      (sql:do-query ((map) [select db-group-user-map-class :where [and [= [slot-value db-group-user-map-class 'group-id] group-id]
                                                                       [= [slot-value db-group-user-map-class 'user-id] user-id]]
                                   :refresh t]
                     :get-all t)
                    (sql:delete-instance-records map)
                    ;; trigger transient slot refresh on next access
                    (invalidate-join-slot group 'users)
                    (invalidate-join-slot user 'groups)
                    (setq deleted-p t))
      deleted-p)))

(defmethod save-authentication-object ((group db-group) &key (pathname *authentication-data-pathname*))
  (declare (ignore pathname))
  group)

(defmethod write-lisp-source ((realm db-realm) (group db-group) stream)
  (declare (ignore stream))
  group)

;;;------------------------------------------------------------------- 
;;;
;;; TESTING
;;;

;; Disable Common SQL [ ] syntax
(eval-when (:compile-toplevel)
  (sql:disable-sql-reader-syntax))

;;;------------------------------------------------------------------- 
;;;
;;; TESTING
;;;

#|

;; (reset-authentication-database-connection)
;; (setq r (intern-realm "digest-test1" :scheme :digest :if-does-not-exist :create))

;; (intern-user r "jcma1" :if-does-not-exist :create)

;; (setq g (intern-group r "digest-group1" :if-does-not-exist :create))

(defun test-add-group-inferior (realm &optional (stream *standard-output*))
  (let ((group1 (intern-group realm "test-group1" :if-does-not-exist :create))
        (group2 (intern-group realm "test-group2" :if-does-not-exist :create))
        (success-p t))
    (format stream "~&Making ~S superior to ~S." group1 group2)
    (group-add-inferior group1 group2)
    (unless-every
     ((member group1 (group-superiors group2))
      (format stream "~&XX Failed: ~S is not a member of (group-superiors ~S)." group1 group2)
      (setq success-p nil))
     ((member group2 (group-inferiors group1))
      (format stream "~&XX Failed: ~S is not a member of (group-inferiors ~S)." group2 group1)
      (setq success-p nil)))
    (when success-p
      (format stream "~&Removing inferior ~S from ~S." group2 group1)
      (group-remove-inferior group1 group2)
      (cond-every
       ((member group1 (group-superiors group2))
        (format stream "~&XX Failed: ~S is still a member of (group-superiors ~S)." group1 group2)
        (setq success-p nil))
       ((member group2 (group-inferiors group1))
        (format stream "~&XX Failed: ~S is still a member of (group-inferiors ~S)." group2 group1)
        (setq success-p nil))))
    success-p))

(defun test-user-passwords (realm &optional (stream *standard-output*))
  (let* ((user (intern-user realm "test-user-password" :if-does-not-exist :create))
         (password "foobarbaz")
         (password-digest (make-user-password-digest (user-realm user) user password)))
    (unwind-protect
        (progn
          (with-authentication-db-transaction ()
            (update-user user :password password))
          (format stream "~&Testing passwords for ~S." user)
          (unless (equal (user-password-digest user) password-digest)
            (format stream "~&XX Failed: Dynamically stored password digest, ~S, is not equal to ~S)."
                    (user-password-digest user) password-digest)
            (return-from test-user-passwords nil))
          (with-authentication-db-transaction ()
            (setf (%user-password user) "test")
            (format stream "~&Setting ~S password to ~A." user (%user-password user))
            (sql:update-slot-from-record user 'password)
            (format stream "~&Reverting ~S password to ~A." user (%user-password user)))
          (unless (equal (user-password-digest user) password-digest)
            (format stream "~&XX Failed: Persistantly stored password digest, ~S, is not equal to ~S)."
                    (user-password-digest user) password-digest)
            (return-from test-user-passwords nil)))
      (unintern-user realm user)
      (when (intern-user realm "test-user-password" :if-does-not-exist :soft)
        (format stream "~&XX Failed: User, ~S was not successfully uninterned." user)
        (return-from test-user-passwords nil)))
    t))

;; Testing users and groups
(defun test-add-users (realm &optional (stream *standard-output*))
  (let ((user (intern-user realm "test-user" :if-does-not-exist :create))
        (group (intern-group realm "test-group" :if-does-not-exist :create))
        (success-p t))
    (format stream "~&Adding ~S to ~S." user group)
    (group-add-user group user)
    (unless-every
     ((member group (user-groups user))
      (format stream "~&XX Failed: ~S is not a member of (user-groups ~S)." group user)
      (setq success-p nil))
     ((member user (group-users group))
      (format stream "~&XX Failed: ~S is not a member of (groups-user ~S)." user group)
      (setq success-p nil)))
    (when success-p
      (format stream "~&Removing ~S from ~S." user group)
      (group-remove-user group user)
      (cond-every
       ((member group (user-groups user))
        (format stream "~&XX Failed: ~S remains a member of (user-groups ~S)." group user)
        (setq success-p nil))
       ((member user (group-users group))
        (format stream "~&XX Failed: ~S remains a member of (groups-user ~S)." user group)
        (setq success-p nil))))
    success-p))

(defun test-realm-reinstatment-and-deletion (realm-name scheme &optional (stream *standard-output*))
  (flet ((remove-realm-from-dynamic-memory (realm)
           (remhash (realm-name realm) (realm-table))))
    (let ((realm (intern-realm realm-name :scheme scheme :if-does-not-exist :soft))
          (success-p t))
      (cond (realm
             (format stream "~&~S was found as ~S in dynamic memory." realm-name realm))
            (t (format stream "~&XX Failed: ~S was not found in dynamic memory." realm-name)))
      ;; test reload from database
      (remove-realm-from-dynamic-memory realm)
      (cond ((setq realm (%find-realm-named scheme realm-name))
             (format stream "~&~S was found by (%find-realm-named ~S ~S) in the database." realm scheme realm-name))
            (t (format stream "~&XX Failed: ~S was not found by (%find-realm-named ~S ~S) in the database." realm-name scheme realm-name)
               (setq success-p nil)))
      (setq realm (intern-realm realm-name :scheme scheme :if-does-not-exist :create))
      ;; unintern realms
      (unintern-realm realm)
      (cond ((intern-realm realm-name :scheme scheme :if-does-not-exist :soft)
             (format stream "~&XX Failed: ~S was not deleted from dynamic memory." realm)
             (setq success-p nil))
            ((%find-realm-named scheme realm-name)
             (format stream "~&XX Failed: ~S was not deleted from the database." realm)
             (setq success-p nil))
            (t (format stream "~&~A|~A was successfully deleted from dynamic memory and the database."  scheme realm-name)))
      success-p)))

(defun test-realm-variants (&optional (stream *standard-output*) &aux (success-p t))
  (macrolet ((noting-failure (&body test-forms)
               `(unless-every
                 ,.(loop for test in test-forms
                         collect `(,test
                                   (setq success-p nil))))))
    (let ((basic-realm (intern-realm "basic-test-realm" :scheme :basic :if-does-not-exist :create))
          (digest-realm (intern-realm "digest-test-realm" :scheme :digest :if-does-not-exist :create))
          (digest-sha-realm (intern-realm "digest-sha-test-realm" :scheme :digest-sha :if-does-not-exist :create)))
      (when (setq success-p (and basic-realm digest-realm digest-sha-realm))
        (noting-failure
          (test-user-passwords basic-realm stream)
          (test-user-passwords digest-realm stream)
          (test-user-passwords digest-sha-realm stream)
          (test-add-users basic-realm stream)
          (test-add-users digest-realm stream)
          (test-add-users digest-sha-realm stream)
          (test-add-group-inferior basic-realm stream)
          (test-add-group-inferior digest-realm stream)
          (test-add-group-inferior digest-sha-realm stream)
          (test-realm-reinstatment-and-deletion (realm-name basic-realm) (realm-scheme basic-realm) stream)
          (test-realm-reinstatment-and-deletion (realm-name digest-realm) (realm-scheme digest-realm) stream)
          (test-realm-reinstatment-and-deletion (realm-name digest-sha-realm) (realm-scheme digest-sha-realm) stream)))
      success-p)))


|#
