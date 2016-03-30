;;;   -*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-

;;;
;;; (c) Copyright  1995, 2005-2006, John C. Mallery
;;;     All Rights Reserved.
;;;


;;;------------------------------------------------------------------- 
;;;
;;; SERVER SELF DOCUMENTATION
;;;

(in-package :http)

(defun submit-and-reset-buttons (&optional (stream *output-stream*))
  (multiple-value-bind (user-agent version)
      (current-user-agent)
    (cond ;; use tables when possible.
      ((and user-agent (user-agent-capability-p :tables user-agent version))
       (with-table (:cell-spacing 1 :cell-padding 5 :stream stream)
         (with-table-row (:stream stream)
           (with-table-cell (:horizontal-alignment :left :stream stream)
             (with-rendition (:bold :stream stream)
               (write-string "Action:" stream)))
           (with-table-cell (:horizontal-alignment :center :stream stream)
             (accept-input 'reset-button "Reset" :stream stream))
           (with-table-cell (:horizontal-alignment :center :stream stream))
           (with-table-cell (:horizontal-alignment :center :stream stream)
             (accept-input 'submit-button "Submit" :stream stream)))))
      ;; otherwise create a similar effect with preformatted text
      (t (with-paragraph (:stream stream)
           (with-verbatim-text (:fresh-line nil :stream stream)
             (with-rendition (:bold :stream stream)
               (write-string "Action:  " stream))
             (accept-input 'reset-button "Reset" :stream stream)
             (write-string "          " stream)
             (accept-input 'submit-button "Submit" :stream stream)))))))

(export 'submit-and-reset-buttons :http)

;;;------------------------------------------------------------------- 
;;;
;;;  FILL-OUT FORM EXAMPLE THAT FINDS DOCUMENTATION ABOUT CL-HTTP
;;;

(defun variable-p (sym)
  (boundp sym))

(defun function-p (sym)
  (and (fboundp sym)                            ; should subsume special-form-p
       (not (macro-function sym))))

(defun class-p (sym)
  (find-class sym nil)) 

(defparameter *lisp-type-predicate-alist*
              '((:all nil "All") (:class class-p "Class") (:function function-p "Function") (:macro macro-function "Macro") 
                (:variable variable-p "Variable"))) 

(defun get-lisp-type-predicate (lisp-type &optional (type :predicate))
  (let ((entry (assoc lisp-type *lisp-type-predicate-alist*)))
    (cond (entry
           (ecase type
             (:predicate 
               (and (second entry) (fdefinition (second entry))))
             (:documentation (third entry))))
          (t (error "~S is not one of the known lisp predicates, ~S." 
                    lisp-type (mapcar #'car *lisp-type-predicate-alist*)))))) 

;; Make this use a hashtable resource for the multiple package sometime.
(defun find-symbols (substring &key (module :http-user) sort-p (external-p t) 
                               (lisp-type :all) &aux symbols pred)
  (flet ((find-syms-in-package (pkg)
           (flet ((collect-matching (sym)
                    (when (and (search substring (symbol-name sym) :test #'char-equal)
                               (or (null pred) (funcall pred sym)))
                      (push sym symbols))))
             (declare (inline collect-matching))
             (let ((package (typecase pkg
                              (null nil)
                              (package pkg)
                              (t (or (find-package (string-upcase pkg))
                                     (return-from find-symbols nil))))))
               (cond ((and package external-p)
                      (do-external-symbols (sym package)
                        (collect-matching sym)))
                     (package
                      (do-symbols (sym package)
                        (collect-matching sym)))
                     (t (do-all-symbols (sym)
                          (collect-matching sym))))))))
    (declare (dynamic-extent #'find-syms-in-package))
    ;; set the predicate for constrained searches
    (setq pred (get-lisp-type-predicate lisp-type :predicate))
    (etypecase module
      (atom (find-syms-in-package module))
      (cons
        (mapc #'find-syms-in-package module)
        (setq symbols (delete-duplicates symbols :test #'eq))))
    (if sort-p 
        (sort symbols #'string< :key #'symbol-name)
        symbols))) 

(defun write-lisp-expression (thing &optional (stream *output-stream*))
  (let* ((*package* (find-package :common-lisp))
         (*print-readably* nil)
         (string (write-to-string thing :base 10. :circle t)))
    (declare (dynamic-extent string))
    (write-string-quoting-specials string stream)))

(defun describe-symbol (sym &key reference  documentation-p (stream *output-stream*) 
                            &aux describe-handled-p)
  (flet ((note-symbol (symbol reference)
           (let* ((*package* (find-package :common-lisp))) 
             (break-line :stream stream)
             (with-rendition (:bold :stream stream)
               (if reference
		   (let ((string (write-to-string symbol :case :upcase)))
                     (declare (dynamic-extent string))
		     (flet ((write-url-string (stream)
			      (write-string reference stream)
			      (write-string-escaping-special-chars string stream)))
		       (declare (dynamic-extent #'write-url-string))
                     (note-anchor string :reference #'write-url-string :stream stream)))
		   (write symbol :stream stream :readably t :case :upcase)))))
         (note-documentation (symbol doc-type)
           (let ((docs (documentation symbol doc-type)))
             (break-line :stream stream)
             (cond  ((and docs (or (position #\newline docs :test #'eql) (position #\linefeed docs :test #'eql)))
                     (with-verbatim-text (:fresh-line nil :stream stream)
                       (write-string-quoting-specials docs stream)))
                    (docs
                     (write-string-quoting-specials docs stream))
                    (t (write-string "[Undocumented]" stream))))))
    (declare (inline note-symbol note-documentation))
    (macrolet ((%describe-symbol ((symbol reference doc-type lisp-type-string) &body body)
                 `(progn
                    (setq describe-handled-p t)
                    (note-symbol ,symbol ,reference)
                    (write-string ,lisp-type-string stream)
                    ,@body
                    (when documentation-p
                      (note-documentation ,symbol ,doc-type)))))
      (cond-every 
        ((fboundp sym)
         (%describe-symbol
           (sym reference 'function
                (cond ((macro-function sym) " [macro]: ")
                      ((special-operator-p sym) " [special form]: ")
                      ((functionp (symbol-function sym)) " [function]: ")
                      (t " [???]: ")))
           (write (or (arglist sym) " ()") :stream stream :escape nil)))
        ((boundp sym)
         (%describe-symbol
           (sym reference 'variable
                " [variable] : ")
           (if (boundp sym)
               (write-lisp-expression (symbol-value sym) stream)
               (write-string "Unbound" stream))))
        ((find-class sym nil)
         (%describe-symbol (sym reference 'type " [class] : ")))
        ;; handle more classes of lisp objects like methods....
        ((not describe-handled-p) (%describe-symbol (sym reference 'function " [random] : ")))))))

(defun find-documentation (url substring module external-p lisp-type documentation-p stream)
  (let* ((wild-p  (or (null substring) (null-string-p substring)))
         (lisp-type-doc (unless (eq lisp-type :all) (get-lisp-type-predicate lisp-type :documentation)))
         (title (format nil "~:[~;~:*~A~]~:[~; ~:*~A~]  Documentation~:[ matching ~S~;~]" 
                        module lisp-type-doc wild-p (or substring "")))
         (candidates (find-symbols substring :module module :sort-p t :external-p external-p
                                   :lisp-type lisp-type)))
    (with-html-document (:declare-dtd-version-p t :stream stream)
      (with-document-preamble (:stream stream)
        (declare-title title :stream stream)
        (declare-base-reference url :stream stream)
        (declare-link :reference "/cl-http/css/base.css" 
                      :relation "stylesheet" :media-type "text/css" :stream stream))
      (with-document-body (:stream stream)
        (with-section-heading (title :stream stream)
          (horizontal-line :stream stream)
          (with-paragraph (:stream stream)
            (note-anchor "Find Other Documentation"
                         :reference "/cl-http/find-documentation.html"
                         :stream stream))
          (with-paragraph (:stream stream)
            (cond (candidates
                   (loop with reference = "/cl-http/show-documentation?"
                         for item in candidates
                         do (describe-symbol item
                                             :reference reference
                                             :documentation-p documentation-p
                                             :stream stream)))
                  (t (write-string "No candidates found." stream))))
          (horizontal-line :stream stream)
          (cl-http-signature stream))))))

(defmethod respond-to-find-documentation ((url http-form) stream query-alist)
  (bind-query-values (substring module external lisp-type documentation)
      (url query-alist)
    (with-successful-response (stream :html :expires (url:expiration-universal-time url)
                                      :cache-control (url:response-cache-control-directives url)
                                      :content-language (languages url)
                                      :additional-headers (ns4.0:client-target-window-http-headers))
      (find-documentation url substring 
                          module 
                          (and  external (equalp external "YES"))
                          (find-symbol lisp-type *keyword-package*)
                          (and documentation (equalp documentation "YES"))
                          stream))))

(defparameter *modules-for-find-documentation*
              '("HTTP" "HTML2" "HTML3.2" "HTML4.0" "RSS2.0" "URL" "W3P" "XHTML1.0"
		;; utilities second
		"WWW-UTILS" "BASE64" "MD5" "SHA" "TK1"
		;; Platform packages third
		#-Genera"SMTP" #-Genera"COMMON-LISP" #+CL-HTTP-MENU "CL-HTTP-MENU"
		;; obsolete generation tools last
		"NETSCAPE1.1" "NETSCAPE2.0" "NETSCAPE3.0" "NETSCAPE4.0"  "VRML1.0"))

(defun add-module-for-find-documentation (module)
  (let ((pkg (find-package module))
	name)
    (cond (pkg
	   (setq name (package-name pkg))
	   (unless (member name *modules-for-find-documentation* :test #'equalp)
	     (push-ordered name *modules-for-find-documentation* #'string-lessp)))
	  (t (error "There is no module (package) named, ~S." module)))))

(export 'add-module-for-find-documentation)

(defparameter *lisp-types-for-find-documentation* 
              (loop for item in *lisp-type-predicate-alist*
                    for string = (third item)
                    collect (list* string (string-upcase string)))) 

(defun %write-find-documentation-form (url stream modules default-modules title description)
  (macrolet ((with-query-environment ((label) &body body)
               `(with-paragraph (:stream stream)
                  (with-rendition (:bold :stream stream)
                    (fresh-line stream)
                    (write-string ,label stream)
                    ,@body))))
    (with-html-document (:declare-dtd-version-p t :stream stream)
      (with-document-preamble (:stream stream)
        (declare-title title :stream stream)
        (declare-base-reference url :stream stream)
        (declare-link :reference "/cl-http/css/base.css" 
                      :relation "stylesheet" :media-type "text/css" :stream stream))
      (with-document-body (:stream stream)
        (with-section-heading (title :stream stream)
          (horizontal-line :stream stream)
          (with-paragraph (:stream stream)
            (write-string description stream))
          (with-fillout-form (:post url :stream stream)
            (with-query-environment
             ("Find Documentation (Substring Match): ")
             (accept-input 'string "SUBSTRING" :size 30 :stream stream)) 
            (with-query-environment
             ("Module:")
             (fresh-line stream)
             (accept-input 'select-choices "MODULE" :choices modules :size (length modules)
                           :default default-modules :sequence-p t :stream stream))
            (with-query-environment
             ("Lisp Type:")
             (accept-input 'radio-button "LISP-TYPE"
                           :choices *lisp-types-for-find-documentation*
                           :default "ALL" :linebreaks nil :stream stream))
            (with-query-environment
             ("Show Documentation: ")
             (accept-input 'radio-button "DOCUMENTATION" :choices '(("Yes" . "YES") ("No" . "NO"))
                           :default "NO" :linebreaks nil :stream stream))
            (with-query-environment
             ("External Interface: ")
             (accept-input 'radio-button "EXTERNAL" :choices '(("Yes" . "YES") ("No" . "NO"))
                           :default "YES" :linebreaks nil :stream stream))
            (submit-and-reset-buttons stream))
          (horizontal-line :stream stream)
          (cl-http-signature stream))))))

;; Instead of using a static form, gain cross platform flexibility by using a computed form.
(defmethod compute-find-documentation-form ((url url:http-form) stream)
  (with-conditional-get-response (stream :html :expires (url:expiration-universal-time url)
                                         :cache-control (url:response-cache-control-directives url)
                                         :content-language (languages url))
    (%write-find-documentation-form url
                                    stream
                                    *modules-for-find-documentation*
                                    '("HTML")
                                    "Find CL-HTTP Documentation"
                                    "Search for functions and variables in the Common Lisp Hypermedia Server.")))

(defun %respond-to-find-documentation-search (url stream default-module)
  (with-slots (url:search-keys) url
    (let* ((spec (and url:search-keys (string-trim '(#\space #\tab) (first url:search-keys))))
           (length (length spec))
           (pos (unless (zerop length) (position #\: spec :test #'eql)))
           (lisp-type :all)
           (documentation-p nil))
      (declare (dynamic-extent spec))
      (cond 
       (pos (let* ((external (< (count #\: spec :test #'eql :start pos) 2))
                   (module (if (zerop pos) default-module (string-upcase (subseq spec 0 pos))))
                   (pos2 (position-if-not  #'(lambda (x) (eql x #\:)) spec :start pos :end length))
                   (substring (if pos2 (subseq spec pos2 length) "")))
              (declare (dynamic-extent substring module))
              (find-documentation url substring module external lisp-type documentation-p stream)))
       (t (find-documentation url "" default-module t lisp-type documentation-p stream))))))

(defmethod respond-to-find-documentation-search ((url http-search) stream)
  (with-conditional-get-response (stream :html :expires (url:expiration-universal-time url)
                                         :cache-control (url:response-cache-control-directives url)
                                         :content-language (languages url)
                                         :additional-headers (ns4.0:client-target-window-http-headers))
    (%respond-to-find-documentation-search url stream "HTML")))

;;;------------------------------------------------------------------- 
;;;
;;;  SEARCH URL THAT DISPLAYS DOCUMENTATION 
;;;

(defun get-symbol (pkg-string &optional (start 0) (end (length pkg-string)))
  (let* ((pos1 (position #\: pkg-string :start start :end end))
         (pkg (if pos1 (subseq pkg-string 0 pos1) "HTTP-USER"))
         (pname (if pos1
		    (unless (= (1+ pos1) end)
		      (subseq pkg-string (position-if-not #'(lambda (x) (eql x #\:)) pkg-string
							  :start (the fixnum (1+ pos1)) :end end)))
		    pkg-string)))
    (when pname
      (find-symbol pname (if (null-string-p pkg) *keyword-package* (or (find-package pkg) :http))))))

(defmethod respond-to-show-documentation  ((url url:http-search) stream)
  (with-slots (url:search-keys) url
    (with-conditional-get-response (stream :html :expires (url:expiration-universal-time url)
                                           :cache-control (url:response-cache-control-directives url)
                                           :content-language (languages url))
      (let* ((*print-case* :upcase)
	     (title (format nil "Documentation for ~{~A~^~}" url:search-keys)))
        (with-html-document (:declare-dtd-version-p t :stream stream)
          (with-document-preamble (:stream stream)
            (declare-title title :stream stream)
            (declare-base-reference url :stream stream)
            (declare-link :reference "/cl-http/css/base.css" 
                          :relation "stylesheet" :media-type "text/css" :stream stream))
          (with-document-body (:stream stream)
            (with-section-heading (title :stream stream)
              (horizontal-line :stream stream)
              (loop for item in url:search-keys
                    for sym = (get-symbol item)
		    when sym
                    do (describe-symbol sym :documentation-p t :stream stream))
              (horizontal-line :stream stream)
              (cl-http-signature stream))))))))


;;;------------------------------------------------------------------- 
;;;
;;; DESCRIBE URL
;;;

#+Genera
(defun fixed-describe (object &optional stream)
  (let ((*standard-output* stream))
    (describe object stream)))

(defgeneric describe-url (url translation &optional stream)
  (:documentation "Describes URL on STREAM using TRANSLATION."))

;; Inefficient. Define this by writing a describe for HTML rather than using this kludge.  This
;; requires portable access to certain operations in the CLOS meta-protocol.
(defmethod describe-url ((url url:url) (translation (eql :html)) &optional (stream *output-stream*))
  (let ((*package* (find-package :common-lisp))
        (description (with-output-to-string (string)
		       #+Genera(fixed-describe url string)
		       #-Genera(describe url string))))
    (declare (dynamic-extent description))
    (fresh-line stream)
    (with-verbatim-text (:stream stream)
      (fresh-line stream)
      (write-string-quoting-specials description stream))))

(defmethod respond-to-describe-url ((url url:http-search) stream)
  (flet ((describe-the-url (url raw-url-string stream)
           (let ((title (format nil "Describe: ~A" raw-url-string)))
             (with-document-preamble (:stream stream)
               (declare-title title :stream stream)
               (declare-base-reference url :stream stream)
               (declare-link :reference "/cl-http/css/base.css" 
                             :relation "stylesheet" :media-type "text/css" :stream stream))
             (with-document-body (:stream stream)
               (with-section-heading (title :stream stream) 
                 (horizontal-line :stream stream)
                 (cond (url
                        (describe-url url :html stream))
                       (t (format stream "~&The URL, ~S, was not found on the server." raw-url-string)))
                 (horizontal-line :stream stream)
                 (cl-http-signature stream)))))
         (report-no-url-found-for-describe-url (url url-string stream)
           (let ((title (if url-string "URL Not Found" "No URL to Describe")))
             (with-document-preamble (:stream stream)
               (declare-title title :stream stream)
               (declare-base-reference url :stream stream)
               (declare-link :reference "/cl-http/css/base.css" 
                             :relation "stylesheet" :media-type "text/css" :stream stream))
             (with-document-body (:stream stream)
               (with-section-heading (title :stream stream) 
                 (horizontal-line :stream stream)
                 (cond (url-string
                        (with-paragraph (:stream stream)
                          (fast-format stream "~&Unknown URL: ~A" url-string)
                          (break-line :stream stream)
                          (fast-format stream "Please try again.")))
                       (t (with-paragraph (:stream stream)
                            (with-paragraph (:stream stream)
                              (format stream "~&You need to supply a URL to describe.")
                              (break-line :stream stream)
                              (format stream "The URL specification should follow the ?. Please try again.")))))
                 (horizontal-line :stream stream)
                 (cl-http-signature stream))))))
    (with-slots (url:search-keys) url
      (with-successful-response (stream :html :expires (url:expiration-universal-time url)
                                        :cache-control (url:response-cache-control-directives url)
                                        :content-language (languages url)) 
        (with-html-document (:declare-dtd-version-p t :stream stream)
          (cond ((car url:search-keys)
                 (let*  ((raw-url-string (car url:search-keys))
                         (url-string (if (scheme-prefixed-url-p raw-url-string)
                                         raw-url-string
                                       (merge-url raw-url-string (local-context))))
                         (url-object (url:intern-url url-string ;; catch any search URLs that appear.
                                                     :if-does-not-exist (if (url:valid-search-url-p url-string) 
                                                                            *search-url-intern-mode*
                                                                          :soft))))
                   (declare (dynamic-extent url-string))
                   (cond ((and url-object (eq (port url-object) (local-port stream))) ;don't cross port boundaries -- 6/19/2006 JCMa
                          (describe-the-url url-object raw-url-string stream))
                         (t (report-no-url-found-for-describe-url url raw-url-string stream)))))
                (t (report-no-url-found-for-describe-url url nil stream))))))))

(pushnew :cl-http-documentation-facility *features*)


;;;------------------------------------------------------------------- 
;;;
;;; EXPORT DOCUMENTATION URLS
;;;

#|
;; move to http:example-exports.lisp 1/8/2006 -- JCMa

(export-url #u"/cl-http/find-documentation.html"
            :html-computed-form
            :form-function #'compute-find-documentation-form
            :response-function #'respond-to-find-documentation
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation)
            :documentation "A form interface for looking up documentation for CL-HTTP.")

(export-url #u"/cl-http/find-documentation?"
            :search
            :response-function #'respond-to-find-documentation-search
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation)
            :documentation "A search URL interfaced into the same functions that respond to the find-documentation form.
To use, provide a symbol with a package prefix after the ?, e.g., [package]:[substring].
The search for [substring] will be performed in the package, [package]. 
A single colon will cause only external symbols to be returned, whereas a double colon
will retrieve both internal and external symbols.")

(export-url #u"/cl-http/show-documentation?"
            :search
            :response-function #'respond-to-show-documentation
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation)
            :documentation "Shows the documentation for a Lisp symbol in CL-HTTP.
To use, provide a package-prefixed name of a symbol in uppercase (unless slashified versions are desired).")

(export-url #u"/cl-http/find-url?"
            :search 
            :response-function #'respond-to-find-url
            :public t
            :language :en
            :keywords '(:cl-http :documentation)
            :documentation "Search for URLs containing a particular substring.")

(export-url #u"/cl-http/find-url.html"
            :html-computed-form
            :form-function #'compute-find-url-form
            :response-function #'respond-to-find-url-form
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation)
            :documentation "A form interface to search for URLs containing a particular substring.")

(export-url #u"/cl-http/describe-url?"
            :search
            :response-function #'respond-to-describe-url
            :keywords '(:cl-http :documentation)
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :documentation "Describe for a specific URL on the server.")|#
