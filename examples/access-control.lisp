;;;   -*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-
;;;
;;; (c) Copyright  1995-96, 2003, 2005, John C. Mallery
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; DISPLAYING & EDITING AUTHENTICATION INFORMATION
;;;

(in-package :http)

(defparameter *describe-realm-url* "/cl-http/describe-realm?"
  "Holds the relative URL used to describe realms.")

(defparameter *describe-group-url* "/cl-http/describe-group?"
  "Holds the relative URL used to describe groups within realms.")

(defparameter *describe-access-control-url* "/cl-http/describe-access-control?"
  "Holds the relative URL used to describe access controls within relams*.")

(defparameter *describe-user-url* "/cl-http/describe-user?"
  "Holds the relative URL used to describe users privileges.")

(defparameter *edit-user-url* "/cl-http/edit-user?"
  "Holds the relative URL used to edit users privileges.")

(defparameter *edit-user-form-url* "/cl-http/edit-user.html"
  "Holds the relative URL used to create or edit users privileges.")

(defparameter *create-account-form-url* "/cl-http/create-user.html"
  "Holds the relative URL used to create users with privileges and automatically notify them.")

(defvar *create-account-standard-realm* nil
  "The realm where account creation occurs.")

(defvar *access-control-document-look* (create-document-look :style-url "http://www.cl-http.org:8001/cl-http/css/base.css"
                                                             :style-type "text/css"
                                                             :class 'cl-http-look))

(define-macro with-html-access-control-document ((&key (declare-dtd-version-p :transitional) (stream '*output-stream*)) &body body)
  "Replaces WITH-HTML-DOCUMENT and establishes *ACCESS-CONTROL-DOCUMENT-LOOK* as the standard document look."
  `(with-document-look-html (:document-look *access-control-document-look*
                             :declare-dtd-version-p ,declare-dtd-version-p
                             :language :en 
                             :direction :left-to-right
                             :stream ,stream)
     ()
     ,@body))

(defmethod describe-realm-url ((realm realm) &optional recompute-p)
  (with-value-cached (realm :describe-realm-url :recompute-p recompute-p)
    (let ((url-string (concatenate 'string *describe-realm-url* (realm-name realm))))
      #u url-string)))

(defmethod describe-group-url ((group group) &optional recompute-p)
  (with-value-cached (group :describe-group-url :recompute-p recompute-p)
    (let ((url-string (concatenate 'string *describe-group-url* (qualified-name group))))
      #u url-string)))

(defmethod describe-access-control-url ((access-control access-control) &optional recompute-p)
  (with-value-cached (access-control :describe-access-control-url :recompute-p recompute-p)
    (let ((url-string (concatenate 'string *describe-access-control-url* (qualified-name access-control))))
      url-string)))

(defmethod describe-user-url ((user user) &optional recompute-p)
  (with-value-cached (user :describe-user-url :recompute-p recompute-p)
    (let ((url-string (concatenate 'string *describe-user-url* (qualified-name user))))
      url-string)))

(note-user-name-cache-key :describe-user-url)

(defmethod edit-user-url ((user user) &optional recompute-p)
  (with-value-cached (user :edit-user-url :recompute-p recompute-p)
    (let ((url-string (concatenate 'string *edit-user-url* (qualified-name user))))
      url-string)))

(note-user-name-cache-key :edit-user-url)

(defmacro with-user-description-url ((&key (user '(server-user-object *server*)) (function-var 'url-fctn)) &body body)
  `(let ((,function-var (if (allow-user-access-p #u *edit-user-url* ,user :get) #'edit-user-url #'describe-user-url)))
     ,@body))


;;;------------------------------------------------------------------- 
;;;
;;; URL
;;;

(defmethod display-url-authentication-status ((url url:authentication-mixin) stream)
  (with-successful-response (stream :html :expires (url:expiration-universal-time url)
                                    :cache-control (url:response-cache-control-directives url)
                                    :content-language (languages url))
    (let ((title "Access Control Status"))
      (with-html-access-control-document (:declare-dtd-version-p t :stream stream)
        (with-document-look-preamble (:stream stream)
          (declare-title title :stream stream)
          (declare-base-reference url :stream stream))
        (with-document-look-body (:stream stream)
          (with-section-heading (title :stream stream)
            (horizontal-line :stream stream)
            (let ((realm (url:authentication-realm url))
                  (capabilities (url:capabilities url))
                  (authorization (get-raw-header :authorization))
                  (user (current-user-object)))
              (with-section-heading ("Current Resource" :stream stream)
                (with-rendition (:bold :stream stream)
                  (note-anchor (url:name-string url) :reference url :stream stream))
                (cond (realm
                       (with-paragraph (:stream stream)
                         (write-string "This resource is part of the " stream)
                         (with-rendition (:bold :stream stream)
                           (write-string (realm-name realm) stream))
                         (write-string " authentication realm." stream))
                       (with-paragraph (:stream stream)
                         (cond (capabilities
                                (write-string "Access capabilities are granted for: " stream)
                                (display-access-control capabilities stream))
                               (t (write-string "Access is granted to any user in the above realm." stream)))))
                      (t (with-paragraph (:stream stream)
                           (write-string "This resource is not assigned to an authentication realm." stream))))
                (cond
                 (authorization
                  (with-paragraph (:stream stream)
                    (write-string "The client's request for the document included the authorization header:" stream))
                  (with-paragraph (:stream stream)
                    (with-rendition (:bold :stream stream)
                      (write-string "Authorization " stream))
                    (with-enumeration (stream :plain)
                      (dolist (item authorization)
                        (enumerating-item (stream)
                          (write-string-quoting-specials item stream))))))
                 (t (with-paragraph (:stream stream)
                      (write-string "There was no authorization header field included with the client request for this document."
                                    stream)))))
              (when user 
                (with-section-heading ("Current User" :stream stream)
                  (describe-user user stream))))))))))

(export 'display-url-authentication-status :http)


;;;------------------------------------------------------------------- 
;;;
;;; GROUPS
;;;

(defmethod write-user-field-label (user (field (eql :personal-name)) stream)
  (declare (ignorable user stream))
  (fast-format stream "Personal Name: "))

(defmethod write-user-field-label (user (field (eql :email-address)) stream)
  (declare (ignorable user stream))
  (fast-format stream "Email Address: "))

(defmethod write-user-field-label :around (user field stream)
  (with-rendition (:italic :stream stream)
    (call-next-method)))

(defmethod write-user-field (user (field (eql :personal-name)) stream)
  (let ((personal-name (user-personal-name user)))
    (when personal-name
      (break-line :stream stream)
      (write-user-field-label user field stream)
      (write-string personal-name stream))))

(defmethod write-user-field (user (field (eql :email-address)) stream)
  (let ((email-address (user-email-address user)))
    (when email-address
      (break-line :stream stream)
      (write-user-field-label user field stream)
      (flet ((write-mailto (stream)
               (fast-format stream "mailto:~A" email-address)))
        (declare (dynamic-extent #'write-mailto))
        (note-anchor email-address :reference #'write-mailto :stream stream)))))

(defmethod display-group-users ((group group) &optional (stream *output-stream*))
  (let ((email-addresses (group-user-email-addresses group)))
    (labels ((write-mailto-url (type stream)
               (loop initially (fast-format stream "\"mailto:~I"
                                            (ecase type
                                              (:to)
                                              (:bcc (write-string "?bcc=" stream))))
                     for (email-address . more) = email-addresses then more
                     do (write-string email-address stream)
                     while more
                     do (write-char #\, stream)
                     finally (write-char #\" stream)))
             (write-mail-bcc-url (stream)
               (write-mailto-url :bcc stream))
             (write-mail-to-url (stream)
               (write-mailto-url :to stream)))
      (declare (dynamic-extent #'write-mailto-url))
      (with-section-heading ("Users" :stream stream)
        (when email-addresses
          (with-emphasis (:quotation :stream stream)
            (with-rendition (:italic :stream stream)
              (fast-format stream "Group Email:")
              (with-enumeration (stream :itemize)
                (enumerating-item (stream)
                  (note-anchor "Send TO" :reference #'write-mail-to-url :stream stream))
                (enumerating-item (stream)
                  (note-anchor "Send BCC" :reference #'write-mail-bcc-url :stream stream))))))
        (with-enumeration (stream :enumerate)
          (with-user-description-url (:function-var url-fctn)
            (loop for user in (sort (group-users group) #'string< :key #'user-name)
                  for user-name = (user-name user)
                  for url = (funcall url-fctn user)
                  do (enumerating-item (stream)
                       (note-anchor user-name :reference url :stream stream)
                       (write-user-field user :personal-name stream)
                       (write-user-field user :email-address stream)))))))))

(defmethod display-group-superiors ((group group) &optional (stream *output-stream*))
  (let ((superiors (group-superiors group)))
    (when superiors
      (with-section-heading ("Superior Groups" :stream stream)
        (with-enumeration (stream :enumerate)
          (loop for group in superiors
                do (enumerating-item (stream)
                     (note-anchor (group-name group) :reference (describe-group-url group) :stream stream))))))))

(defmethod display-group-inferiors ((group group) &optional (stream *output-stream*))
  (let ((inferiors (group-inferiors group)))
    (when inferiors
      (with-section-heading ("Inferior Groups" :stream stream)
        (with-enumeration (stream :enumerate)
          (loop for group in inferiors
                do (enumerating-item (stream)
                     (note-anchor (group-name group) :reference (describe-group-url group) :stream stream))))))))

(defmethod describe-group ((group group) &optional (stream *output-stream*))
  (let ((realm (group-realm group)))
    (with-section-heading ("Description" :stream stream)
      (with-enumeration (stream :itemize)
        (enumerating-item (stream)
          (with-rendition (:italic :stream stream)
            (write-string "Realm: " stream))
          (note-anchor (realm-name realm) :reference (describe-realm-url realm) :stream stream))
        (enumerating-item (stream)
          (with-rendition (:italic :stream stream)
            (write-string "Authentication: " stream))
          (write (realm-scheme realm) :stream stream :escape nil))))
    (display-group-superiors group stream)
    (display-group-inferiors group stream)
    (display-group-users group stream)))


;;;------------------------------------------------------------------- 
;;;
;;; ACCESS CONTROLS
;;;

(defmethod display-access-control ((access-control access-control) &optional (stream *output-stream*))
  "Displays groups and users able to access the URL access group, ACCESS-CONTROL, on STREAM."
  (let ((alist (access-control-alist access-control))
        (default-groups (access-control-default-groups access-control))
        (default-users (access-control-default-users access-control)))
    (with-user-description-url (:function-var url-fctn)
      (macrolet ((write-cognoscienti ((set) &body form)
                   `(if ,set
                        (loop for (item . more) = ,set then more
                              while item
                              do ,@form
                                 (when more (write-string ", " stream))
                              finally (write-char #\. stream))
                        (write-string "none." stream))))
        (flet ((enumerate-method (method groups users)
                 (enumerating-item (stream)
                   (with-rendition (:bold :stream stream)
                     (write method :escape nil :stream stream))
                   (with-enumeration (stream :itemize)
                     (enumerating-item (stream)
                       (with-rendition (:bold :stream stream)
                         (write-string "Groups: " stream))
                       (write-cognoscienti (groups)
                                           (note-anchor (group-name item) :reference (describe-group-url item)
                                                             :stream stream)))
                     (enumerating-item (stream)
                       (with-rendition (:bold :stream stream)
                         (write-string "Users: " stream))
                       (write-cognoscienti (users)
                                           (note-anchor (user-name item) :reference (funcall url-fctn item) :stream stream)))))))
          (declare (dynamic-extent #'enumerate-method))
          (with-enumeration (stream :plain)
            (loop for (method groups users) in alist
                  do (enumerate-method method groups users))
            (when (or default-groups default-users)
              (enumerate-method :default default-groups default-users))))))))

(defmethod describe-access-control ((access-control access-control) &optional (stream *output-stream*))
  (let ((realm (access-control-realm access-control)))
    (with-section-heading ("Description" :stream stream)
      (with-enumeration (stream :itemize)
        (enumerating-item (stream)
          (with-rendition (:italic :stream stream)
            (write-string "Realm: " stream))
          (note-anchor (realm-name realm) :reference (describe-realm-url realm) :stream stream))
        (enumerating-item (stream)
          (with-rendition (:italic :stream stream)
            (write-string "Authentication: " stream))
          (write (realm-scheme realm) :stream stream :escape nil))))
    (with-section-heading ("Methods" :stream stream)
      (display-access-control access-control stream))))


;;;------------------------------------------------------------------- 
;;;
;;; USERS
;;;

(defmethod describe-user ((user user) &optional (stream *output-stream*))
  "Describes USER's capabilities in HTML on STREAM."
  (let ((realm (user-realm user))
        (groups (user-groups user)))
    (flet ((write-entry (heading string stream)
             (with-rendition (:italic :stream stream)
               (write-string heading stream)
               (write-char #\: stream)
               (write-char #\space stream))
             (write-string string stream)
             (break-line :stream stream)))
      (with-paragraph (:stream stream)
        (write-entry "Username" (user-name user) stream)
        (write-user-field user :personal-name stream)
        (write-user-field user :email-address stream)
        (write-entry "Realm"
                     (note-anchor (realm-name realm) :reference (describe-realm-url realm) :stream nil)
                     stream)
        (with-rendition (:bold :stream stream)
          (write-string "Groups: " stream))
        (cond (groups
               (loop for (group . more) = groups then more
                     do (note-anchor (group-name group) :reference (describe-group-url group) :stream stream)
                     (if more
                         (write-string ", " stream)
                       (write-char #\. stream))
                     while more))
              (t (write-string "none." stream)))))))


;;;------------------------------------------------------------------- 
;;;
;;; REALMS
;;;

(defmethod display-realm-users ((realm realm) &optional (stream *output-stream*))
  (macrolet ((write-user-property (property value stream)
               `(when ,value
                  (break-line :stream ,stream)
                  (with-rendition (:italic :stream ,stream)
                    (write-string ,property ,stream))
                  (write-string ,value ,stream))))
    (let ((users (sorted-users realm :predicate #'string<)))
      (declare (dynamic-extent users))
      (with-section-heading ("Users" :stream stream)
        (with-enumeration (stream :enumerate)
          (with-user-description-url (:function-var url-fctn)
            (loop for user in users
                  for user-name = (user-name user)
                  for url = (funcall url-fctn user)
                  for name = (user-personal-name user)
                  for email = (user-email-address user)
                  do (enumerating-item (stream)
                       (with-rendition (:bold :stream stream)
                         (note-anchor user-name :reference url :stream stream))
                       (cond-every
                        (name (write-user-property "Name: " name stream))
                        (email
                         (let ((mailto (note-anchor email :reference (concatenate 'string "mailto:" email)  :stream nil)))
                           (declare (dynamic-extent mailto))
                           (write-user-property "Email: " mailto stream))))))))))))

(defmethod display-realm-groups ((realm realm) &optional (stream *output-stream*))
  (let ((groups (sorted-groups realm :predicate #'string<)))
    (declare (dynamic-extent groups))
    (with-section-heading ("Groups" :stream stream)
      (with-enumeration (stream :enumerate)
        (with-user-description-url (:function-var url-fctn)
          (loop for group in groups
                for group-name = (group-name group)
                for group-url = (describe-group-url group)
                do (enumerating-item (stream)
                     (with-rendition (:bold :stream stream)
                       (note-anchor group-name :reference group-url :stream stream))
                     (break-line :stream stream)
                     (with-rendition (:italic :stream stream)
                       (write-string "Users: " stream))                   
                     (loop for users = (sort (group-users group) #'string< :key #'user-name) then (cdr users)
                           while users
                           as (user) = users
                           as name = (user-name user)
                           as url = (funcall url-fctn user)
                           do (note-anchor name :reference url :stream stream)
                              (if (cdr users)
                                  (write-string ", " stream)
                                  (write-char #\. stream))))))))))

(defmethod display-realm-access-controls ((realm realm) &optional (stream *output-stream*))
  (let ((access-controls (sorted-access-controls realm :predicate #'string<)))
    (declare (dynamic-extent access-controls))
    (with-section-heading ("Access Controls" :stream stream)
      (with-enumeration (stream :enumerate)
        (loop for access-control in access-controls
              for access-control-name = (access-control-name access-control)
              for access-control-name-url = (describe-access-control-url access-control)
              do (enumerating-item (stream)
                   (with-rendition (:bold :stream stream)
                     (note-anchor access-control-name 
                                       :reference access-control-name-url
                                       :stream stream))
                   (display-access-control access-control stream)))))))

(defmethod describe-realm ((realm realm) &optional (stream *output-stream*))
  (with-section-heading ("Description" :stream stream)
    (with-enumeration (stream :itemize)
      (enumerating-item (stream)
        (with-rendition (:italic :stream stream)
          (write-string "Authentication: " stream))
        (write (realm-scheme realm) :stream stream :escape nil))))
  (display-realm-groups realm stream)
  (display-realm-access-controls realm stream)
  (display-realm-users realm stream))

;;;------------------------------------------------------------------- 
;;;
;;; RESPONSE FUNCTIONS
;;;

(defmethod respond-to-describe-realm ((url url:http-search) stream)
  (flet ((get-realm (realm-string)
           (when realm-string
             (let ((realm-name (string-trim *white-space-chars* realm-string)))
               (declare (dynamic-extent realm-name))
               (unless (null-string-p realm-name)
                 (intern-realm realm-name :if-does-not-exist :soft))))))
    (declare (inline get-realm))
    (with-slots (url:search-keys) url
      (destructuring-bind (&optional realm-string) url:search-keys
        (with-successful-response (stream :html :cache-control (url:response-cache-control-directives url)
                                          :content-language (languages url))
          (with-html-access-control-document (:declare-dtd-version-p t :stream stream)
            (let ((realm (get-realm realm-string)))
              (cond
               (realm
                (let ((title (format nil "Realm: ~A " (realm-name realm)))
                      (heading (format nil "Realm: ~A " (note-anchor (realm-name realm) :reference (describe-realm-url realm) :stream nil))))
                  (declare (dynamic-extent title heading))
                  (with-document-look-preamble (:stream stream)
                    (declare-title title :stream stream)
                    (declare-base-reference url :stream stream))
                  (with-document-look-body (:stream stream)
                    (with-section-heading (heading :stream stream)
                      (horizontal-line :stream stream)
                      (describe-realm realm stream)))))
               (t (let ((heading (format nil "~A (~D) Access Control Realms" (local-host-domain-name) *standard-http-port*)))
                    (with-document-look-preamble (:stream stream)
                      (declare-title heading :stream stream)
                      (declare-base-reference url :stream stream))
                    (with-document-look-body (:stream stream)
                      (with-section-heading (heading :stream stream)
                        (horizontal-line :stream stream)
                        (dolist (realm (sorted-realms :predicate #'string<))
                          (let ((heading (concatenate 'string "Realm: "
                                                      (note-anchor (realm-name realm)
                                                                   :reference (describe-realm-url realm) :stream nil))))
                            (declare (dynamic-extent heading))
                            (with-section-heading (heading :stream stream)
                              (describe-realm realm stream))))))))))))))))

(defmacro %respond-to-describe-acl-unit (url stream var-name type-string intern-function describe-function)
  `(with-slots (url:search-keys) ,url
     (destructuring-bind (&optional ,var-name &rest extra) url:search-keys
       (with-successful-response (,stream :html :cache-control (url:response-cache-control-directives url)
                                          :content-language (languages url))
         (let ((title (format nil ,(format nil "~:(~A~): ~~:(~~A~~)" (substitute #\space #\- type-string :test 'eql)) ,var-name)))
           (with-html-access-control-document (:declare-dtd-version-p t :stream ,stream)
             (with-document-look-preamble (:stream ,stream)
               (declare-title title :stream ,stream)
               (declare-base-reference url :stream stream))
             (with-document-look-body (:stream ,stream)
               (with-section-heading (title :stream ,stream)
                 (horizontal-line :stream stream)
                 (cond
                  ((null ,var-name)
                   (write-string
                    ,(format nil "No ~A and realm was specified. You need to pass in realm|~:*~A-name." type-string)
                    ,stream))
                  (extra
                   (format nil "The extraneous arguments, ~S, were provided. You need to pass in realm|~Aname." extra ,type-string))
                  (t (let* ((len (1+ (position-if-not #'white-space-char-p ,var-name :start 0 :end (length ,var-name)
                                                      :from-end t)))
                            (pos1 (position-if-not #'white-space-char-p ,var-name :start 0 :end len))
                            (pos2 (or (position #\| ,var-name :start pos1 :end len) len))
                            (realm-name (subseq ,var-name pos1 pos2))
                            (unit-name (subseq ,var-name (1+ pos2) len))
                            (realm (intern-realm realm-name :if-does-not-exist :soft))
                            (unit nil))
                       (declare (dynamic-extent realm unit-name))
                       (cond ((and realm (setq unit (,intern-function realm unit-name :if-does-not-exist :soft)))
                              (,describe-function unit ,stream))
                             (realm
                              (format ,stream "No ~A named, ~A, exists in the realm, ~A." ',type-string unit-name realm-name))
                             (t (format ,stream "No realm named, ~A, exists on the server." realm-name))))))))))))))

(defmethod respond-to-describe-group ((url url:http-search) stream)
  (%respond-to-describe-acl-unit url stream realm+group "group" intern-group describe-group))

(defmethod respond-to-describe-access-control ((url url:http-search) stream)
  (%respond-to-describe-acl-unit url stream realm+access-control "access-control" intern-access-control describe-access-control))

(defmethod respond-to-describe-user ((url url:http-search) stream)
  (%respond-to-describe-acl-unit url stream realm+user "user" intern-user describe-user))


;;;------------------------------------------------------------------- 
;;;
;;; EDITING USERS
;;;

(defparameter *group-selection-box-maximum-size* 10)

(defun %write-edit-user-form (url stream &optional user-id password email-address groups personal-name realm)
  (macrolet ((write-prompt (prompt stream)
               `(with-table-cell
                    (:horizontal-alignment :right :stream stream)
                  (with-rendition (:bold :stream ,stream)
                    (write-string ,prompt ,stream))))
             (accept-radio-button (stream prompt query choices &optional default)
               `(with-table-row (:stream ,stream)
                  (write-prompt ,prompt ,stream)
                  (with-table-cell (:horizontal-alignment :center :vertical-alignment :middle :stream ,stream)
                    (accept-input 'radio-button ,query :choices ,choices :default ,default :linebreaks nil :stream ,stream))))
             (accept-simple-field (input-type prompt query default stream)
               `(with-table-row (:stream ,stream)
                  (write-prompt ,prompt ,stream)
                  (with-table-cell
                      (:horizontal-alignment :center :vertical-alignment :middle :stream ,stream)
                    (accept-input ,input-type ,query :default ,default :stream ,stream
                                  :size (max 25 (length ,default))))))
             (accept-selection (stream prompt query choices &optional default)
               `(with-table-row (:stream ,stream)
                  (write-prompt ,prompt ,stream)
                  (with-table-cell (:horizontal-alignment :center :vertical-alignment :middle :stream ,stream)
                    (let ((n-choices (length ,choices)))
                      (if (zerop n-choices)
                          (with-rendition (:italic :stream stream) (write-string "None" ,stream))
                        (accept-input 'select-choices ,query :choices ,choices :default ,default
                                      :sequence-p t :size (min *group-selection-box-maximum-size* n-choices) :stream ,stream)))))))
    (with-paragraph (:stream stream)
      (with-fillout-form (:post url :stream stream)
        ;; hidden fields so that we know where we started
        (cond-every
         (realm
          (accept-input 'hidden "ORIGINAL-REALM" :default (write-to-armor-plated-string (realm-name realm)) :stream stream))
         (user-id
          (accept-input 'hidden "ORIGINAL-USER-ID" :default (write-to-armor-plated-string user-id) :stream stream)))
        (with-table (:stream stream)
          (accept-simple-field 'string "User-ID (required): " "USER-NAME" user-id stream)
          (when user-id
            (accept-simple-field 'string "Personal Name: " "PERSONAL-NAME" personal-name stream)
            (accept-simple-field 'string "Email Address: " "EMAIL-ADDRESS" email-address stream))
          (cond (realm
                 (with-table-row (:stream stream)
                   (write-prompt "Realm (required): " stream)
                   (with-table-cell (:horizontal-alignment :center :vertical-alignment :middle :stream stream)
                     (note-anchor (realm-name realm) :reference (describe-realm-url realm) :stream stream)))
                 (let ((realm-groups (mapcar #'group-name (sorted-groups realm)))
                       (group-names (mapcar #'group-name groups)))
                   (declare (dynamic-extent group-names))
                   (accept-selection stream "Groups: " "GROUPS" realm-groups group-names)))
                (t (accept-selection stream "Realm (required): " "REALM" (mapcar #'realm-name (sorted-realms)))))
          (when user-id
            (accept-simple-field 'password "New Password: " "PASSWORD1" nil stream)
            (accept-simple-field 'password "Confirm Password: " "PASSWORD2" nil stream)
            (when password
              (accept-radio-button stream "Remove Password: " "REMOVE-PASSWORD" '(("Yes" . "YES") ("No" . "NO")) "NO"))
            (accept-radio-button stream "Delete User: " "DELETE-USER" '(("Yes" . "YES") ("No" . "NO")) "NO"))
          (with-table-row (:stream stream)
            (write-prompt "Mode: " stream)
            (with-table-cell (:horizontal-alignment :center :vertical-alignment :middle :stream stream)
              (if user-id
                  (accept-input 'select-choices "MODE" :choices '(("View" :value :view) ("Update" :value :update)) :default :view :size :pull-down-menu :stream stream)
                (accept-input 'select-choices "MODE" :choices '(("View" :value :view) ("Create" :value :create))
                :default :view :size :pull-down-menu :stream stream))))
          (with-table-row (:stream stream)
            (write-prompt "Action: " stream)
            (with-table-cell (:horizontal-alignment :center :stream stream)
              (with-verbatim-text (:fresh-line nil :width 30 :stream stream)
                (accept-input 'reset-button "Reset" :stream stream)
                (write-string "     " stream)
                (accept-input 'submit-button "Submit" :stream stream)))))))))

(defmethod write-edit-user-form ((user user) url &optional (stream *output-stream*))
  (let ((personal-name (user-personal-name user))
        (email-address (user-email-address user))
        (realm (user-realm user))
        (groups (user-groups user))
        (username (user-name user))
        (password-digest (user-password-digest user)))
    (%write-edit-user-form url stream username (not (null password-digest))
                           email-address groups personal-name realm)))

(defmethod respond-to-edit-user (url stream)
  (with-slots (url:search-keys) url
    (let ((user-spec (first url:search-keys)))
      (cond (user-spec
             (let* ((user (parse-authentication-object user-spec #'intern-user))
                    (title (if user
                               (format nil "Edit User: ~A" (user-name user))
                             (format nil "Unknown User: ~A" (first url:search-keys)))))
               (with-successful-response (stream :html :cache-control (url:response-cache-control-directives url)
                                                 :content-language (languages url))
                 (with-html-access-control-document (:declare-dtd-version-p t :stream stream)
                   (with-document-look-preamble (:stream stream)
                     (declare-title title :stream stream)
                     (declare-base-reference url :stream stream))
                   (with-document-look-body (:stream stream)
                     (with-section-heading (title :stream stream)
                       (horizontal-line :stream stream)
                       (cond (user
                              (write-edit-user-form user *edit-user-form-url* stream))
                             (t (format stream "There is no user named, ~A, on this server.~
                                       You need to pass in realm-name|user-name."
                                        (first url:search-keys))))))))))
            (t (%compute-edit-user-form url stream))))))

(defun %compute-edit-user-form (url stream)
  (with-successful-response (stream :html :expires (url:expiration-universal-time url)
                                    :cache-control (url:response-cache-control-directives url)
                                    :content-language (languages url))
    (with-html-access-control-document (:declare-dtd-version-p t :stream stream)
      (with-document-look-preamble (:stream stream)
        (declare-title "Edit User" :stream stream)
        (declare-base-reference url :stream stream))
      (with-document-look-body (:stream stream)
        (with-section-heading ("Edit User" :stream stream)
          (horizontal-line :stream stream)
          (%write-edit-user-form *edit-user-form-url* stream))))))

(defmethod compute-edit-user-form ((url url:http-form) stream)
  (%compute-edit-user-form url stream))

(defmethod respond-to-edit-user-form ((url url:http-form) stream query-alist)
  (flet ((clean-up (item)
           (and item (not (null-string-p (setq item (string-trim '(#\space #\tab #\return #\Linefeed) item)))) item))
         (name-change-p (user user-name)
           (not (equal (user-name user) user-name))))
    (macrolet ((clean-up-vars (&rest vars)
                 `(progn ,.(loop for var in vars collect `(setq ,var (clean-up ,var)))))
               (report-result (stream format-string &rest format-args)
                 `(enumerating-item (stream)
                    ,(if format-args
                         `(format ,stream ,format-string ,@format-args)
                       `(write-string ,format-string ,stream))))
               (check-user-and-realm (stream user realm)
                 `(unless-every
                   (,realm 
                    (report-result ,stream "No Realm supplied.")
                    (return-from work-zone))
                   (,user
                    (fast-format ,stream "User not found in the ~I realm." 
                                 (note-anchor (realm-name ,realm) :reference (describe-realm-url ,realm)  :stream ,stream))
                    (return-from work-zone)))))
      (bind-query-values (original-realm original-user-id user-name personal-name email-address
                                         groups new-group password1 password2 realm remove-password delete-user mode)
          (url query-alist)
        (clean-up-vars user-name personal-name email-address new-group password1 password2
                       realm remove-password delete-user mode)
        (let* ((realm (intern-realm (if original-realm (read-from-armor-plated-string original-realm) realm)
                                    :if-does-not-exist :soft))
	       (user-id (or (and original-user-id (read-from-armor-plated-string original-user-id)) user-name))
               (mode (or (cdr (assoc mode '(("VIEW" . :view) ("UPDATE" . :update) ("CREATE" . :create)) :test #'equalp)) :view))
               title)
          (multiple-value-bind (user newly-created-user-p)
              (and realm user-id (intern-user realm user-id 
                                              :if-does-not-exist (case mode (:create :create) (t :soft))))
            (setq title (format nil "Edit User: ~A" (if user (user-qualified-name user) (or user-id "none"))))
            (with-successful-response (stream :html :cache-control (url:response-cache-control-directives url)
                                              :content-language (languages url))
              (with-html-access-control-document (:declare-dtd-version-p t :stream stream)
                (with-document-look-preamble (:stream stream)
                  (declare-title title :stream stream)
                  (declare-base-reference url :stream stream))
                (with-document-look-body (:stream stream)
                  (with-section-heading (title :stream stream)
                    (horizontal-line :stream stream)
                    (ecase mode
                      ((:update :create)
                       (with-section-heading ("Results" :stream stream)
                         (with-paragraph (:stream stream)
                           (with-enumeration (stream :itemize)
                             (block work-zone
                               (check-user-and-realm stream user realm)
                               ;; we have a user and realm now
                               (ecase mode
                                 (:create 
                                  (cond (newly-created-user-p
                                         (enumerating-item (stream)
                                           (fast-format stream "New user created in the ~I realm."
                                                        (note-anchor (realm-name realm) :reference (describe-realm-url realm)  :stream stream))))
                                        (t (enumerating-item (stream)
                                             (fast-format stream "A user named, ~I, already exists in the ~I realm."
                                                          (note-anchor (user-name user) :reference (describe-user-url user)  :stream stream)
                                                          (note-anchor (realm-name realm) :reference (describe-realm-url realm)  :stream stream)))
                                           (setq user nil)
                                           (return-from work-zone))))
                                 (:update
                                  (cond ((equalp delete-user "YES")        ;delete user object
                                         (unintern-user realm user)
                                         (report-result stream "User deleted.")
                                         (save-authentication-data)
                                         (report-result stream "Authentication data saved.")
                                         (setq user nil)
                                         (return-from work-zone))
                                        (t (cond-every
                                            ((name-change-p user user-name)    ;rename user
                                             (multiple-value-bind (new-user renamed-p)
                                                 (rename-user realm user user-name :soft)
                                               (unless renamed-p
                                                 (enumerating-item (stream)
                                                   (fast-format stream "A user named, ~I, already exists in the ~I realm."
                                                                (note-anchor (user-name new-user) :reference (describe-user-url new-user)  :stream stream)
                                                                (note-anchor (realm-name realm) :reference (describe-realm-url realm)  :stream stream)))
                                                 (return-from work-zone)))
                                             (report-result stream "User renamed."))
                                            ((equalp remove-password "YES")       ;remove password
                                             (setf (user-password-digest user) nil)
                                             (report-result stream "Password Removed.")))))))
                               (let ((i-groups (loop for g in (ensure-list groups)
                                                     collect (intern-group realm g)))
                                     pw)
                                 (when password1 ;change the user's password
                                   (cond ((null password1))
                                         ((and password2 (string= password1 password2))
                                          (report-result stream "Password updated.")
                                          (setq pw password1))
                                         (t (report-result stream "Password not updated; the two passwords do not match."))))
                                 (let ((args (when pw `(:password ,pw))))
                                   (declare (dynamic-extent args))
                                   (apply #'update-user user
                                          :personal-name personal-name
                                          :email-address email-address
                                          :groups i-groups
                                          args))
                                 (save-authentication-data)
                                 (report-result stream "Authentication data saved."))))
                           (when user
                             (horizontal-line :stream stream)))))
                      (:view
                       (unless (and realm user)
                         (with-section-heading ("Results" :stream stream)
                           (with-paragraph (:stream stream)
                             (with-enumeration (stream :itemize)
                               (block work-zone
                                 (check-user-and-realm stream user realm))))))))
                    (when user
                      (unless (equal user-id user-name)
                        (flet ((write-heading (stream)
                                 (format stream "Current User: ~A" (user-qualified-name user))))
                          (declare (dynamic-extent #'write-heading))
                          (with-section-heading (#'write-heading :level 2 :stream stream))))
                      (with-paragraph (:stream stream)
                        (write-edit-user-form user *edit-user-form-url* stream)))))))))))))

;;;------------------------------------------------------------------- 
;;;
;;; CREATE USER ACCOUNT AND notify
;;;



(defun %write-create-account-form (url stream realm)
  (macrolet ((write-prompt (prompt stream)
               `(with-table-cell
                    (:horizontal-alignment :right :stream stream)
                  (with-rendition (:bold :stream ,stream)
                    (write-string ,prompt ,stream))))
             (accept-radio-button (stream prompt query choices &optional default)
               `(with-table-row (:stream ,stream)
                  (write-prompt ,prompt ,stream)
                  (with-table-cell (:horizontal-alignment :center :vertical-alignment :middle :stream ,stream)
                    (accept-input 'radio-button ,query :choices ,choices :default ,default :linebreaks nil :stream ,stream))))
             (accept-simple-field (input-type prompt query default stream)
               `(with-table-row (:stream ,stream)
                  (write-prompt ,prompt ,stream)
                  (with-table-cell
                      (:horizontal-alignment :center :vertical-alignment :middle :stream ,stream)
                    (accept-input ,input-type ,query :default ,default :stream ,stream
                                  :size (max 25 (length ,default))))))
             (accept-selection (stream prompt query choices &optional default)
               `(with-table-row (:stream ,stream)
                  (write-prompt ,prompt ,stream)
                  (with-table-cell (:horizontal-alignment :center :vertical-alignment :middle :stream ,stream)
                    (let ((n-choices (length ,choices)))
                      (if (zerop n-choices)
                          (with-rendition (:italic :stream stream) (write-string "None" ,stream))
                        (accept-input 'select-choices ,query :choices ,choices :default ,default
                                      :sequence-p t :size (min *group-selection-box-maximum-size* n-choices) :stream ,stream)))))))
    (with-paragraph (:stream stream)
      (with-fillout-form (:post url :stream stream)
        ;; hidden fields so that we know where we started
        (cond-every
         (realm
          (accept-input 'hidden "ORIGINAL-REALM" :default (write-to-armor-plated-string (realm-name realm)) :stream stream)))
        (with-table (:stream stream)
          (accept-simple-field 'string "User-ID (required): " "USER-NAME" nil stream)
          (accept-simple-field 'string "Personal Name: " "PERSONAL-NAME"  nil stream)
          (accept-simple-field 'string "Email Address: " "EMAIL-ADDRESS" nil stream)
          (with-table-row (:stream stream)
            (write-prompt "Realm: " stream)
            (with-table-cell (:horizontal-alignment :center :vertical-alignment :middle :stream stream)
              (note-anchor (realm-name realm) :reference (describe-realm-url realm) :stream stream)))
          (let ((realm-groups (mapcar #'group-name (sorted-groups realm))))
            (declare (dynamic-extent realm-groups))
            (accept-selection stream "Groups: " "GROUPS" realm-groups nil))
          (with-table-row (:stream stream)
            (write-prompt "Action: " stream)
            (with-table-cell (:horizontal-alignment :center :stream stream)
              (with-verbatim-text (:fresh-line nil :width 30 :stream stream)
                (accept-input 'reset-button "Reset" :stream stream)
                (write-string "     " stream)
                (accept-input 'submit-button "Create" :stream stream)))))))))

(defmethod write-create-account-form (url &optional (stream *output-stream*))
  (let* ((realm (intern-realm *create-account-standard-realm*)))
    (%write-create-account-form url stream realm)))

(defmethod compute-create-account-form ((url url:http-form) stream)
  (with-successful-response (stream :html :expires (url:expiration-universal-time url)
                                    :cache-control (url:response-cache-control-directives url)
                                    :content-language (languages url))
    (with-html-access-control-document (:declare-dtd-version-p t :stream stream)
      (with-document-look-preamble (:stream stream)
        (declare-title "Create Account" :stream stream)
        (declare-base-reference url :stream stream))
      (with-document-look-body (:stream stream)
        (with-section-heading ("Create Account" :stream stream)
          (horizontal-line :stream stream)
          (write-create-account-form *create-account-form-url* stream))))))

(defmethod respond-to-create-account-form ((url url:http-form) stream query-alist)
  (flet ((clean-up (item)
           (and item (not (null-string-p (setq item (string-trim '(#\space #\tab #\return #\Linefeed) item)))) item)))
    (macrolet ((clean-up-vars (&rest vars)
                 `(progn ,.(loop for var in vars collect `(setq ,var (clean-up ,var))))))
      (bind-query-values (original-realm user-name personal-name email-address groups)
          (url query-alist)
        (clean-up-vars user-name personal-name email-address)
        (let* ((realm-name (read-from-armor-plated-string original-realm))
               (realm (intern-realm realm-name :if-does-not-exist :soft      ))
               (old-user (and realm user-name (intern-user realm user-name :if-does-not-exist :soft)))
               (groups (etypecase groups
                         (atom (clean-up groups))
                         (list (mapcar #'clean-up groups)))))
          (cond (old-user
                 (let ((title (format nil "Create User: User Exists")))
                   (with-successful-response (stream :html :cache-control (url:response-cache-control-directives url)
                                                     :content-language (languages url))
                     (with-html-access-control-document (:declare-dtd-version-p t :stream stream)
                       (with-document-look-preamble (:stream stream)
                         (declare-title title :stream stream)
                         (declare-base-reference url :stream stream))
                       (with-document-look-body (:stream stream)
                         (with-section-heading (title :stream stream)
                           (horizontal-line :stream stream)
                           (with-paragraph (:stream stream)
                             (with-enumeration (stream :itemize)
                               (enumerating-item (stream)
                                 (fast-format stream "A user named, ~I, already exists in the ~I realm."
                                              (note-anchor (user-name old-user) :reference (describe-user-url old-user)  :stream stream)
                                              (note-anchor (realm-name realm) :reference (describe-realm-url realm)  :stream stream)))))))))))
                (t (let ((target (concatenate 'string (local-context) *edit-user-url* realm-name "|" user-name)))
                     (declare (dynamic-extent target))
                      (create-user-account+notify realm groups user-name email-address personal-name nil)
                     (redirect-request *server* target)))))))))
            
;;;------------------------------------------------------------------- 
;;;
;;; EXPORT WEB INTERFACE
;;;

(defun export-access-control-interface (&rest args &key (host (local-host-domain-name)) (port *standard-http-port*) (protocol *standard-protocol*)
                                              (authentication-realm :server) (capabilities :webmasters) &allow-other-keys)
  (flet ((intern-acl-url (string)
           (let ((args (list string :host host :port port :protocol protocol)))
             (declare (dynamic-extent args))
             (intern-url (merge-url args (local-context)) :if-does-not-exist :create))))
    (declare (dynamic-extent #'intern-acl-url))
    (let ((export-args `(:authentication-realm ,authentication-realm
                         :capabilities ,capabilities
                         :private t
                         :proxy-revalidate t
                         :language :en
                         :keywords (:cl-http :site :maintenance :access-control)
                         ,@args)))
      (declare (dynamic-extent export-args))
      (apply #'export-url (intern-acl-url *describe-realm-url*)
             :search
             :response-function #'respond-to-describe-realm
             :documentation "Describe an access control realm."
             export-args)
      (apply #'export-url (intern-acl-url *describe-group-url*)
             :search
             :response-function #'respond-to-describe-group
             :documentation "Describe a particular access control group in a realm.
The syntax is realm-name|group-name."
             export-args)
      (apply #'export-url (intern-acl-url *describe-access-control-url*)
             :search
             :response-function #'respond-to-describe-access-control
             :documentation "Describe a particular access control unit in a realm.
The syntax is realm-name|access-control-name."
             export-args)
      (apply #'export-url (intern-acl-url *describe-user-url*)
             :search
             :response-function #'respond-to-describe-user
             :documentation "Describe the access capabilities for a particular user in a realm.
The syntax is realm-name|user-name."
             export-args)
      (apply #'export-url (intern-acl-url *edit-user-url*)
             :search
             :response-function #'respond-to-edit-user
             :documentation "Edit the access capabilities for a particular user in a realm.
The syntax is realm-name|user-name."
             export-args)
      (apply #'export-url (intern-acl-url *edit-user-form-url*)
             :html-computed-form
             :form-function #'compute-edit-user-form
             :response-function #'respond-to-edit-user-form
             :documentation "A form to edit user access control."
             export-args)
      (apply #'export-url (intern-acl-url *create-account-form-url*)
             :html-computed-form
             :form-function #'compute-create-account-form
             :response-function #'respond-to-create-account-form
             :documentation "A form to create a user account with privileges."
             export-args))))

(eval-when (:load-toplevel :execute)
  (export '(*describe-realm-url* *describe-group-url* *describe-access-control-url* *describe-user-url* *edit-user-url* *edit-user-form-url*
                                 export-access-control-interface
                                 *access-control-document-look* *create-account-standard-realm* *create-account-form-url*)
          :http)) 

#|

(export-access-control-interface :port 8443 :protocol :https)

|#
