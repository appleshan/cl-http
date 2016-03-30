;;; -*- Mode: LISP; Package: (lispdoc :use (html www-utils future-common-lisp); BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-
;;;
;;; (c) Copyright 2005-2006, 2010, Rainer Joswig & John C. Mallery
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; LispDoc -- AN ANSI COMMON LISP PACKAGE-LEVEL DOCUMENTATION SYSTEM
;;;

;;; Simple Sketch of a Documentation Browser for CL-HTTP
;;; Copyright Rainer Joswig, joswig@lisp.de, 2005

;;; Ideas
;
; * simple markup language for documentation strings
; ** links to mentioned symbols (classes, functions, ...)
; * feedback form to send updates, questions or comments for the documentations
; * documentation
; ** general: source code, files where defined, tags (labels, categories)
; ** functions: who-calls
; ** classes: superclasses, subclasses, slots, slot-documentations, meta-classes
; ** macros: 
; ** packages: use-list, number of symbols, nicknames
; ** generic-functions: method-combination, methods
; ** variables: constants
; * coloring: brighter for non-exported symbols
; * search field to search the packages, the documentations strings, the files
; * file content browser
; ** colored lines of definition headers
; ** Constraint box that reduces the number of symbols shown in left column
; ** Cache the sorted package symbols and use the number of symbols in package as an invalidation bit.
; ** boldify current symbol in index using a script run from the documentation pane on load that fools the element id
; ** Offer a view where index symbols are sorted by containing system and containing file.

;;; To Do
; 
; * differentiate between classes and types
; * differentiate between variables and constants

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defpackage lispdoc
    (:nicknames ld)
    (:use html www-utils future-common-lisp)
    (:import-from "URL"
                  "%SEARCH-PARENT")
    (:import-from "HTTP"
                  "*KEYWORD-PACKAGE*"
                  "*WHITE-SPACE-CHARS*"
                  "ESCAPED-CHARACTER-P"
                  "GET-VALUE"
                  "NSTRING-UNESCAPE-SPECIAL-CHARS"
                  "NULL-STRING-P"
                  "STRING-ESCAPE-SPECIAL-CHARS"
                  "STRING-SEARCH"
                  "WHITE-SPACE-CHAR-P"
                  "WITH-STRING-TRIM-BOUNDS")
    #+(and :mit-values)(:shadowing-import-from :mit-values "VALUES")
    #+(or clisp ecl) (:shadowing-import-from :mit-defgeneric "DEFGENERIC")
    (:export
     "ADD-DOCUMENTATION-MODULE"
     "DEFINE-DOCUMENTATION"
     "EXPORT-LISPDOC-INTERFACE"
     "FIND-MANUAL"
     "INTERN-MANUAL"
     "REMOVE-DOCUMENTATION-MODULE"
     "UNEXPORT-LISPDOC-INTERFACE"))
  ) ;close eval-when

(in-package :lispdoc)

;; Provide a feature so other modules can add their packages
(pushnew :lispdoc *features*)

;;;------------------------------------------------------------------- 
;;;
;;; VARIABLES
;;;

(defparameter *base-search-urlname* "lispdoc?")

(defparameter *documentation-search-urlname* "lispdoc-documentation?")

(defparameter *index-search-urlname* "lispdoc-index?")

(defparameter *lispdoc-http-cache-expiration* (* 15. 60.) ;15 minutes
  "Controls the number of seconds that a LispDoc Web page can be cached before needing an HTTP refresh.")

(defvar *package-symbol-cache* (make-hash-table :test #'equal ))

(defparameter *style-sheet-urlname* "lispdoc.css")

(defparameter *lispdoc-css* 
"/* CL-HTTP LispDoc Style Sheet
Copyright 2005, John C. Mallery. All rights RESERVED. */

/* General CSS styling */
body {display: block; background-color: white; 
      background-image: none; 
      font-family: helvetica, sans-serif, serif, times;}

H1, H2, H3, H4, H5, H6 {color: #800000; 
                        font-family: arial, helvetica, sans-serif, times;
                        font-weight: bolder; font-style: italic;}

a {font-family: helvetica, sans-serif, times;}
a:link {color: #000080;}
a:visited {color:#003399}
a:active {color: yellow}
a:hover {color: white; background:  #000080;}

hr {border: 1px solid #000080; background-color: #000080;}

/* Specific CSS for the various components. */

div.LispDoc-header {text-align: center; color: #800000; font-size: 200%; font-weight: bolder; font-style: italic;
                    font-family: arial, helvetica, sans-serif, times;}

table.LispDoc {align:center;}

/* Toolbar for selecting packages. */
div.LispDoc-toobar {text-align: center; color: white; background-color: #000080; font-size: smaller;
                    padding-left: 3px; padding-right: 3px; padding-top: 3px; padding-bottom: 3px;
                    font-family: arial, helvetica, sans-serif, serif;}

a.LispDoc-package-current {text-decoration:none; color: white; font-weight: bold; font-size: larger;}
a.LispDoc-package-current:hover {text-decoration:none; color: #000080 ; background: white;}
a.LispDoc-package {text-decoration:none; color: white;}
a.LispDoc-package:hover {text-decoration:none; color: #000080 ; background: white;}

/* Index object case. */
object.LispDoc-index {height: 690px; width: 250px; color: #000080; background-color: #CDCDCD; border: 1px solid #000080; 
                      margin-left: 0px; margin-right: 0px; margin-top: 5px; padding-bottom: 5px; padding-left: 0px; padding-right: 0px;}

body.LispDoc-index {color: #000080; background-color: #CDCDCD; font-size: small;}

/* Index no object case. */
div.LispDoc-index {width:250px; border: 1px solid #000080; margin-top: 5px; margin-left: 3px; margin-right 3px; 
                   padding-bottom: 10px; padding-left: 3px; padding-right: 3px;
                   color: #000080; background-color: #CDCDCD; font-size: small;}

h2.LispDoc-index {text-align: center;}

h3.LispDoc-index-section {text-indent: 5%;}

a.LispDoc-symbol-current {text-decoration:none; font-weight: bold; font-size: normal;}
a.LispDoc-symbol {text-decoration:none; font-size: smaller;}
a.LispDoc-symbol-current-undocumented {text-decoration:none; font-weight: bold; font-size: normal;}
a.LispDoc-symbol-current-undocumented:hover {background: #800000;}
a.LispDoc-symbol-undocumented {text-decoration:none; font-size: smaller;}
a.LispDoc-symbol-undocumented:hover {background: #800000;}


/* Documentation  object case. */
object.LispDoc-documentation {height: 690px; width: 850px; border: 1px solid #800000;
                              margin-left: 0px; margin-right: 0px; margin-top: 5px; padding-bottom: 5px; padding-left: 0px; padding-right: 0px;}

/* Documentation no object case. */
div.LispDoc-documentation {margin-top: 5px;  margin-left: 3px; margin-right: 3px; width: 850px;
                           border: 1px solid #800000; font-size: normal;}

div.LispDoc-Signature {color: white; background-color: #800000; font-family: monospace;
                       padding-left: 3px; padding-right: 3px; padding-top: 3px; padding-bottom: 3px;}

span.LispDoc-symbol {font-weight: bolder; font-size: larger; font-family: monospace;}

span.lisp-type {font-style: italic;}

div.class-info {color: #000080; background-color: #CDCDCD; font-family: monospace;
                padding-left: 20px; padding-right: 3px; padding-top: 3px; padding-bottom: 3px;}

div.LispDoc-description {color: #000080; font-style: italic; padding-left: 3px; padding-right: 3px;}

span.index-form-label {font-style: italic; font-weight: bolder;}

"
  "The CSS style sheet for documentation.")

;;;------------------------------------------------------------------- 
;;;
;;; CLASS DEFINITION
;;;

(defclass basic-manual
          ()
  ((name :initarg :name :accessor manual-name :type string)
   (title :initarg :title :accessor manual-title :type (or string function)
          :documentation "The title for the manual page. This can be either a string or a function called on stream.")
   (index-undocumented-symbols :initform t :initarg :index-undocumented-symbols :accessor manual-index-undocumented-symbols
                               :documentation "Controls whether or not undocumented symbols are displayed.")
   (documented-package-names :initarg :documented-package-names :accessor manual-documented-package-names
                             :documentation "All package to be included by LispDoc.")
   (internally-documented-package-names :initarg :internally-documented-package-names :accessor manual-internally-documented-package-names
                                        :documentation "All package to be included by LispDoc.")
   (style-sheet :initform nil :initarg :style-sheet :accessor manual-style-sheet)
   (style-sheet-url :initarg :style-sheet-url :accessor manual-style-sheet-url)
   (base-search-url :initarg :base-search-url :accessor manual-base-search-url)
   (index-search-url :initarg :index-search-url :accessor manual-index-search-url)
   (documentation-search-url :initarg :documentation-search-url :accessor manual-documentation-search-url)
   (display-type :initform :scrolling :initarg :display-type :accessor manual-display-type :type (and keyword #-MCL(member :scrolling :full-display))      ; member broken in MCL 7/4/2006
                 :documentation "Controls whether the display uses scrolling boxes for the symbol index and documentation.
Values: :SCROLLING or :FULL-DISPLAY.")
   
   (documented-packages :initform nil :initarg :documented-packages :accessor manual-documented-packages
                        :documentation "For these packages external symbols are browsable, too.")
   (internally-documented-packages :initform nil :initarg :internally-documented-packages :accessor manual-internally-documented-packages
                                   :documentation "For these packages internal symbols are browsable, too.")))

(defmethod print-object ((manual basic-manual) stream)
  (with-slots (name) manual
    (print-unreadable-object (manual stream :type t :identity t)
      (format stream "~A" name)
      manual)))

(defvar *manuals* nil
  "A list of defined manuals.")

(defmethod register-manual ((manual basic-manual))
  (unless (member manual *manuals*)
    (push manual *manuals*)))

(defmethod  unregister-manual ((manual basic-manual))
  (unless (member manual *manuals*)
    (push manual *manuals*)))

(defvar *default-manual-class* 'basic-manual
  "The default class for LispDoc manuals.")

(defun find-manual (name &optional (error-p t))
  "Finds a LispDoc manual object named, NAME."
  (or (etypecase name
        (string
         (find name *manuals* :test #'equalp :key #'manual-name))
        (basic-manual
         (find name *manuals*)))
      (when error-p
        (error "No manual named, ~S, was found." name))))

(defun intern-manual (name &key (if-does-not-exist :error) (class *default-manual-class*))
  "Interns a LispDoc manual object named, NAME."
  (let ((manual (find-manual name (eq if-does-not-exist :error))))
    (cond (manual)
          (t (ecase if-does-not-exist
               (:soft nil)
               (:create
                (let ((manual (make-instance class :name name)))
                  (register-manual manual)
                  (values manual t))))))))

;;;------------------------------------------------------------------- 
;;;
;;; COMMON LISP UTILITY FUNCTIONS
;;;

(defun symbol-external-p (symbol package)
  "Is a symbol exported from its package?"
  (check-type package package)
  (when (and (symbolp symbol) (symbol-package symbol))
    (multiple-value-bind (sym status)
        (find-symbol (symbol-name symbol) package)
      (and (eq sym symbol)
           (eq status :external)))))

(defun get-documentation-type-from-string (string)
  "Returns a symbol representing the documentation type."
  (cond ((equalp string "VARIABLE") 'cl:variable)
        ((equalp string "FUNCTION") 'cl:function)
        ((equalp string "TYPE") 'cl:type)
        (t (error "Unknown documentation type ~a" string))))

(defun class-superclasses (class &aux classes)
  "The list of of all superclasses."
  (labels ((class-superclasses-1 (class)
             (let ((dc (class-direct-superclasses class)))
               (when dc
                 (mapc #'class-superclasses-1 dc)))
             (loop for class in (class-direct-superclasses class)
                   do (pushnew class classes))))
    (declare (dynamic-extent #'class-superclasses-1))
    (class-superclasses-1 class)
    classes))

;;;------------------------------------------------------------------- 
;;;
;;; SUBSTRING CONSTRAINTS
;;;

(defun make-constraint-list (string  &aux (start 0) (end (length string)))
  (declare (fixnum start end))
  (with-string-trim-bounds (*white-space-chars* string start end)
    (loop with s = start and prev-white-space-p = t and result
          for idx fixnum upfrom start below end
          for ch = (char string idx)
          do (cond ((member ch *white-space-chars* :test #'eql)
                    (unless prev-white-space-p
                      (push (subseq string s idx) result)
                      (setq prev-white-space-p t))
                    (setq s (1+ idx)))
                   (t (setq prev-white-space-p nil)))
          finally (if (or prev-white-space-p (not (< s end)))
                      (return (nreverse result))
                    (return (nreverse (push (subseq string s idx) result)))))))

(defun apply-constraints (symbol constraint-list)
  (let* ((string (symbol-name symbol))
         (end (length string)))
    (loop for constraint in constraint-list
          always (string-search constraint string 0 (length constraint) 0 end))))

;;;------------------------------------------------------------------- 
;;;
;;; CACHING DEFINITION SYMBOLS
;;;

(declaim (inline make-cache-package-key))

(defun make-cache-package-key (package include-internal-symbols-p sort-predicate)
  (list package include-internal-symbols-p sort-predicate))

(declaim (inline cache-key-for-package-p))

(defun cache-key-for-package-p (package key)
  (equal package (car key)))

(defun cache-package-symbols (symbols count total package include-internal-symbols-p sort-predicate)
  (let ((key (make-cache-package-key package include-internal-symbols-p sort-predicate))
        (entry (list symbols count total)))
    (setf (gethash key *package-symbol-cache*) entry)))

(defun decache-cached-package-symbols (&optional (package :all))
  (case package
    (:all
     (clrhash *package-symbol-cache*))
    (t (let (package-key)
         (flet ((find-key (key entry)
                  (declare (ignore entry))
                  (when (cache-key-for-package-p package key)
                    (setq package-key key))))
           (declare (dynamic-extent #'find-key))
           (maphash #'find-key *package-symbol-cache*)
           (when package-key
             (remhash package-key *package-symbol-cache*)))))))

(defun get-cached-package-symbols (package include-internal-symbols-p sort-predicate constraints)
  (let* ((key (make-cache-package-key package include-internal-symbols-p sort-predicate))
         (entry (gethash key *package-symbol-cache*)))
    (declare (dynamic-extent key))
    (when entry
      (destructuring-bind (symbols count total) entry
        (declare (fixnum count))
        (cond (constraints
               (let ((constraint-list (make-constraint-list constraints)))
                 (declare (dynamic-extent constraint-list)) ;
                 (loop for sym in symbols
                       when (apply-constraints sym constraint-list)
                       collect sym into new-symbols 
                       else do (decf count)
                       finally (return (values new-symbols count total)))))
              (t (values symbols count total)))))))

;;;------------------------------------------------------------------- 
;;;
;;; RETREIVING DEFINITIONS FROM PACKAGES
;;;

(defun package-definitions (package constraints &optional (sort-predicate #'string<) include-internal-symbols-p)
  "Returns a list of all symbols that have a defintion for either a class, a function, a macro or a variable.
The list is sorted by sort-PREDICATE. If INCLUDE-INTERNAL-SYMBOLS-P is
non-null then also internal symbols will be returned. The returned values are: (SYMBOLS MATCH-COUNT DEFINITION-COUNT)."
  (declare (values symbols match-count definition-count))
  (check-type package package)
  (flet ((relevant-symbol-p (symbol)
           (or (fboundp symbol)
               (boundp symbol)
               (macro-function symbol)
               (find-class symbol nil))))
    (declare (inline relevant-symbol-p))
    (multiple-value-bind (symbols count total)
        (get-cached-package-symbols package include-internal-symbols-p sort-predicate constraints)
      (when symbols
        (return-from package-definitions (values symbols count total))))
    ;; compute the package lists
    (let ((constraint-list (make-constraint-list constraints))
          (count 0)
          (total 0)
          symbols)
      (declare (dynamic-extent constraint-list)
               (fixnum count total))
      (cond (include-internal-symbols-p
             (do-symbols (symbol package)
               (when (and (or (eq (symbol-package symbol) package) (symbol-external-p symbol package))
                          (relevant-symbol-p symbol))
                 (incf total)
                 (when (or (null constraints) (apply-constraints symbol constraint-list))) 
                 (push symbol symbols)
                 (incf count))))
            (t (do-external-symbols (symbol package)
                 (when (relevant-symbol-p symbol)
                   (incf total)
                   (when (or (null constraints) (apply-constraints symbol constraint-list))
                     (push symbol symbols)
                     (incf count))))))
      (when symbols 
        (setq symbols (sort symbols sort-predicate :key #'symbol-name)))
      (unless constraints ;; cache only sorted full-package lists, and compute subsets on the fly
        (cache-package-symbols symbols count total package include-internal-symbols-p sort-predicate))
      (values symbols count total))))

;;;------------------------------------------------------------------- 
;;;
;;;  UTILITY FUNCTIONS
;;;

(defun collect-sorted-packages (packages)
  (loop for name in packages
        for package = (find-package name)
        when package
        collect package into pkgs
        finally (return (sort pkgs #'string-lessp :key #'package-name))))

(defmethod documented-packages ((manual basic-manual) &optional recache-p)
  (let ((documented-packages (manual-documented-packages manual)))
    (cond ((or recache-P (null documented-packages))
           (setf (manual-documented-packages manual) (collect-sorted-packages (manual-documented-package-names manual))))
          (t documented-packages))))

(defgeneric documented-package-p (manual package)
  (:documentation "Returns non-null when PACKAGE is documented by MANUAL."))

(defmethod documented-package-p ((manual basic-manual) (package package))
  (member package (documented-packages manual)))

(defmethod documented-package-p ((manual basic-manual) (package string))
  (documented-package-p manual (find-package package)))

(defmethod documented-package-p (manual (package null))
  (declare (ignore manual))
  nil)

(defmethod internally-documented-packages ((manual basic-manual) &optional recache-p)
  (let ((internally-documented-packages (manual-internally-documented-packages manual)))
    (cond ((or recache-p (null internally-documented-packages))
           (setf (manual-internally-documented-packages manual) (collect-sorted-packages (manual-internally-documented-package-names manual))))
          (t internally-documented-packages))))

(defgeneric internally-documented-package-p (manual package)
  (:documentation "Returns non-null when PACKAGE is internally documented by MANUAL."))

(defmethod internally-documented-package-p ((manual basic-manual) (package package))
  (member package (internally-documented-packages manual)))

(defmethod internally-documented-package-p ((manual basic-manual) (package string))
  (internally-documented-package-p manual (find-package package)))

(defmethod internally-documented-package-p (manual (package null))
  (declare (ignore manual))
  nil)

(defmethod (setf manual-documented-package-names) :after (new-names (manual basic-manual))
  (declare (ignorable new-names))
  (setf (manual-documented-packages manual) nil));decache

(defmethod (setf manual-internally-documented-package-names) :after (new-names (manual basic-manual))
  (declare (ignorable new-names))
  (setf (manual-internally-documented-packages manual) nil)) ;decache

;;;------------------------------------------------------------------- 
;;;
;;; DEFINING DOCUMENTATION MANUALS
;;;

(defgeneric remove-documentation-module (manual package)
  (:documentation "Removes a module for documentation by LispDoc."))

(defmethod remove-documentation-module ((manual basic-manual) (package string))
  (setf (manual-internally-documented-package-names manual) (delete package (manual-internally-documented-package-names manual) :test #'equalp)
        (manual-documented-package-names manual) (delete package (manual-documented-package-names manual) :test #'equalp)))

(defmethod remove-documentation-module ((manual basic-manual) (package package))
  (remove-documentation-module manual (package-name package)))

(defgeneric add-documentation-module (manual package &optional document-internals-p)
  (:documentation "Adds a new module for documentation by LispDoc."))

(defmethod add-documentation-module ((manual basic-manual) (package string) &optional document-internals-p)
  (unless (member package (manual-documented-package-names manual) :test #'equalp)
    (push package  (manual-documented-package-names manual)))
  (if (member package (manual-internally-documented-package-names manual) :test #'equalp)
      (unless document-internals-p
        (setf (manual-internally-documented-package-names manual) (delete package (manual-internally-documented-package-names manual) :test #'equalp)))
    (when document-internals-p
      (push package (manual-internally-documented-package-names manual)))))

(defmethod add-documentation-module ((manual basic-manual) (package package) &optional document-internals-p)
  (add-documentation-module manual (package-name package) document-internals-p ))

(defmethod add-documentation-module ((manual basic-manual) (package symbol) &optional document-internals-p)
  (add-documentation-module manual (symbol-name package) document-internals-p))

(defun %define-documentation (name directory title style-sheet display-type only-documented packages)
  (flet ((make-url (path urlname default)
           (cond (path
                  (concatenate 'string path urlname))
                 (t (let ((npath (string-downcase default)))
                      (declare (dynamic-extent npath))
                      (concatenate 'string "/" npath "/" urlname))))))
    (let ((manual (intern-manual name :if-does-not-exist :create)))
      ;; reset all state
      (setf (manual-documented-package-names manual) nil
            (manual-documented-packages manual) nil
            (manual-internally-documented-package-names manual) nil
            (manual-internally-documented-packages manual) nil)
      ;; get down to business
      (check-type display-type (and keyword #-MCL(member :scrolling :full-display)))
      (check-type style-sheet (or null string function))
      (check-type title (or string function))
      (setf (manual-title manual) title
            (manual-style-sheet manual) style-sheet
            (manual-display-type manual) display-type
            (manual-index-undocumented-symbols manual) (not only-documented)
            (manual-style-sheet-url manual) (make-url directory *style-sheet-urlname* name)
            (manual-base-search-url manual) (make-url directory *base-search-urlname* name)
            (manual-index-search-url manual) (make-url directory *index-search-urlname* name)
            (manual-documentation-search-url manual) (make-url directory *documentation-search-urlname* name))
      (dolist (entry packages)
        (etypecase entry
          (cons
           (destructuring-bind (package &key internals)
               entry
             (add-documentation-module manual package internals)))
          (atom
           (add-documentation-module manual entry))))
      ;; recache any changes for internal documentation -- JCMa 2/21/2010
     (internally-documented-packages manual t)
      manual)))

;; Toplevel form for specified a LispDoc configuration
(defmacro define-documentation ((name &key title directory style-sheet display-type only-documented) &body packages)
  "Defines the set of packages to be documented by LispDoc.
NAME is the name of the manual set.
TITLE is a string or function which displays the title of the manual
DIRECTORY is the URL path for LispDoc URLs. If DIRECTORY is not supplied, the path defaults to NAME,
which must not include unsafe characters.
STYLE-SHEET can be CSS style sheet as a string or function that writes it to a stream.
DISPLAY-TYPE is either :SCOLLING or :FULL-DISPLAY, which controls the layout of the documentation
pane.
ONLY-DOCUMENTED controls whether undocumented definitions are shown.
PACKAGES is a set of packages to document. Entries can either be a package name or (PACKAGE &KEY
INTERNALS). Using the extended syntax, INTERNALS controls whether the internal definitions are
documented in addition to the exported definitions. "
  `(%define-documentation ,name ,directory ,title ,style-sheet ,display-type ,only-documented ',packages))

;;;------------------------------------------------------------------- 
;;;
;;; MAKING URLS
;;;

(defun safe-constraints (string)
  (string-escape-special-chars (nsubstitute #\+ #\space string)))

;; Returns an URL escaped string when necessary
(defun safe-symbol-name (symbol)
  (flet ((escape-symbol-name-p (name)
           (loop for ch across name
                 thereis (escaped-character-p ch))))
    (declare (inline escape-symbol-name-p))
    (let ((name (symbol-name symbol)))
      (cond ((escape-symbol-name-p name)
             (or (get symbol 'url-escaped-name)
                 (setf (get symbol 'url-escaped-name) (string-escape-special-chars name))))
            (t name)))))

(declaim (inline make-package-documentation-link))

(defun make-package-documentation-link (manual package constraints)
  "Returns a string that represents a link to the http documentation for a package."
  (check-type package package)
  (let ((args (when constraints (list "&apropos=" (safe-constraints constraints)))))
    (declare (dynamic-extent args))
    (apply #'concatenate 'string (manual-base-search-url manual) "package=" (package-name package) args)))

(declaim (inline make-symbol-documentation-link))

(defun make-symbol-documentation-link (manual package symbol type constraints)
  "Generates a string that represents a link to the http documentation for a symbol/package/type combination."
  (let ((args (nconc (when package (list "package=" (package-name package)))
                     (when symbol (list "&symbol=" (safe-symbol-name symbol)))
                     (when type (list "&type=" (symbol-name type)))
                     (when constraints (list "&apropos=" (safe-constraints constraints))))))
    (declare (dynamic-extent args))
    (apply #'concatenate 'string (manual-base-search-url manual) args)))

(declaim (inline make-object-index-link))

(defun make-object-index-link (manual package symbol type constraints)
  "Returns a string that represents a link to the index for a package."
  (let ((args (nconc (when package (list "package=" (package-name package)))
                     (when symbol (list "&symbol=" (safe-symbol-name symbol)))
                     (when type (list "&type=" (symbol-name type)))
                     (when constraints (list "&apropos=" (safe-constraints constraints))))))
    (declare (dynamic-extent args))
    (apply #'concatenate 'string (manual-index-search-url manual) args)))

(declaim (inline make-object-documentation-link))

(defun make-object-documentation-link (manual package symbol type)
  "Returns a string that represents a link to the documentation for a package."
  (let ((args (nconc (when package (list "package=" (package-name package)))
                     (when symbol (list "&symbol=" (safe-symbol-name symbol)))
                     (when type (list "&type=" (symbol-name type))))))
    (declare (dynamic-extent args))
    (apply #'concatenate 'string (manual-documentation-search-url manual) args)))

;;;------------------------------------------------------------------- 
;;;
;;;  HTML RENDERING
;;;

(defmethod display-component-toolbar ((manual basic-manual) stream current-package constraints)
  "Renders a list of all documented packages"
  (flet ((note-package (package class stream)
           (let ((url:*escape-search-urls* nil)
                 (url (make-package-documentation-link manual package constraints)))
             (declare (dynamic-extent url))
             (note-anchor (package-name package) :reference url :class class :stream stream))))
    (declare (inline note-package))
    (with-division (:class "LispDoc-toobar" :stream stream)
      (loop for (pkg . more) = (documented-packages manual) then more
            for class = (if (eq pkg current-package) "LispDoc-package-current" "LispDoc-package")
            do (note-package pkg class stream)
            while more
            do (fast-format stream " ")))))

(defmethod display-index-symbols ((manual basic-manual) stream package current-symbol current-type constraints 
                                  &aux (documentation-required-p (not (manual-index-undocumented-symbols manual))))
  "Renders a list of all (usually external) symbols in package."
  (check-type package package)
  (labels ((current-symbol-p (symbol type current-symbol current-type)
             (and (eq symbol current-symbol) (eq type current-type)))
           (documentation-p (symbol doc-type)
             (or (not documentation-required-p) (documentation symbol doc-type)))
           (documented-structure-p (symbol &aux class)
             (and (setq class (find-class symbol nil))
                  (typep class 'structure-class)
                  (documentation-p symbol 'type)))
           (documented-classp-p (symbol &aux class)
             (and (setq class (find-class symbol nil))
                  (not (typep class 'structure-class))
                  (documentation-p symbol 'type)))
           (documented-variable-p (symbol)
             (and (boundp symbol)
                  (documentation-p symbol 'variable)))
           (documented-generic-function-p (symbol)
             (and (fboundp symbol)
                  (ignore-errors (symbol-function symbol)) ;why is the ignore errors here? -- 9/22/2005 JCMa
                  (not (macro-function symbol))
                  (typep (symbol-function symbol) 'generic-function)
                  (documentation-p symbol 'function)))
           (documented-function-p (symbol)
             (and (fboundp symbol)
                  (ignore-errors (symbol-function symbol))
                  (not (macro-function symbol))
                  (not (typep (symbol-function symbol) 'generic-function))
                  (documentation-p symbol 'function)))
           (documented-macro-p (symbol)
             (and (fboundp symbol)
                  (ignore-errors (symbol-function symbol))
                  (macro-function symbol)
                  (documentation-p symbol 'function)))
           (note-symbol (manual symbol package type class constraints stream)
             (let ((url:*escape-search-urls* nil)
                   (url (make-symbol-documentation-link manual package symbol type constraints)))
               (declare (dynamic-extent url))
               (note-anchor (symbol-name symbol) :reference url :class class :stream stream)))
           (determine-symbol-class (symbol doc-type)
             (cond ((or documentation-required-p (documentation symbol doc-type))
                    (if (current-symbol-p symbol doc-type current-symbol current-type) "LispDoc-symbol-current" "LispDoc-symbol"))
                   (t (if (current-symbol-p symbol doc-type current-symbol current-type) 
                          "LispDoc-symbol-current-undocumented"
                        "LispDoc-symbol-undocumented"))))
           (write-symbol-kind-section (manual stream symbols label lisp-type constraints test)
             (loop with label-printed-p
                   for symbol in symbols
                   when (funcall test symbol)
                   do (when (funcall test symbol)
                        (unless label-printed-p ;; print lavel when category present
                          (with-section-heading (label :class "LispDoc-index-section" :stream stream))
                          (setq label-printed-p t))
                        (note-symbol manual symbol package lisp-type (determine-symbol-class symbol lisp-type) constraints stream)
                        (break-line :stream stream)))))
    (declare (inline current-symbol-p)
             (dynamic-extent #'documentation-p #' determine-symbol-class #'write-symbol-kind-section))
    (let ((include-internal-symbols-p (internally-documented-package-p manual package)))
      (multiple-value-bind (symbols match-count definition-total)
          (package-definitions package constraints #'string< include-internal-symbols-p)
        (declare (fixnum match-count definition-total))
        (flet ((write-index-heading (stream)
                 (fast-format stream "~A" (package-name package))
                 (break-line :stream stream)
                 (with-division (:inline-p t :style "font-size: smaller;" :stream stream)
                   (fast-format stream  "(~A~I definitions)" 
                                definition-total 
                                (unless include-internal-symbols-p (fast-format stream " external"))))))
          (declare (dynamic-extent #'write-index-heading))
          (with-fillout-form (:get (manual-base-search-url manual) :class "index-form" :stream stream)
            (with-division (:inline-p t :class "index-form-label" :stream stream)
              (fast-format stream "Apropos:"))
            (cond-every
             (package 
              (accept-input 'hidden "package" :default (package-name package) :stream stream))
             (current-symbol
              (accept-input 'hidden "symbol" :default (safe-symbol-name current-symbol) :stream stream))
             (current-type
              (accept-input 'hidden "type" :default (symbol-name current-type) :stream stream)))
            (accept-input 'string "apropos" :size 20 :default constraints :stream stream))
          (unless (or (= match-count definition-total) (zerop match-count))
            (with-emphasis (:emphasis :stream stream)
              (fast-format stream "Found ~D ~A." match-count (if (eql match-count 1) "match" "matches"))))
          (with-section-heading (#'write-index-heading :level 2 :class "LispDoc-index" :stream stream)
            (cond (symbols
                   (write-symbol-kind-section manual stream symbols "Classes" 'cl:type constraints #'documented-classp-p)
                   (write-symbol-kind-section manual stream symbols "Structures" 'cl:type constraints #'documented-structure-p)
                   (write-symbol-kind-section manual stream symbols "Variables" 'cl:variable constraints #'documented-variable-p)
                   (write-symbol-kind-section manual stream symbols "Generic Functions" 'cl:function constraints #'documented-generic-function-p)
                   (write-symbol-kind-section manual stream symbols "Functions" 'cl:function constraints #'documented-function-p)
                   (write-symbol-kind-section manual stream symbols "Macros"'cl:function constraints #'documented-macro-p))
                  (t (with-emphasis (:emphasis :stream stream)
                       (fast-format stream "~&No matching symbols."))))))))))

(defmethod display-documentation ((manual basic-manual) stream package symbol type)
  "Renders the documentation for one symbol given a certain type (variable, type, function, ...)."
  (check-type package package)
  (labels ((write-lisp-symbol (symbol stream)
             (with-division (:class "LispDoc-symbol" :inline-p t :stream stream)
               (cond ((symbol-external-p symbol package)
                      (fast-format stream "~A:~A"(package-name package) (symbol-name symbol)))
                     ((eq package (symbol-package symbol))
                      (fast-format stream "~A::~A" (package-name package) (symbol-name symbol)))
                     (t (let ((*package* *keyword-package*)) ;forces package prefix to be printed under LispWorks 4.4.5
                          (fast-format stream "~S" symbol))))))
           (write-lisp-type (type-string stream)
             (with-division (:inline-p t :class "lisp-type" :stream stream)
               (fast-format stream "[~A]" type-string)))
           (display-function-docmentation (symbol stream)
             (flet ((function-type-string (symbol)
                      (cond ((macro-function symbol) "Macro")
                            ((typep (symbol-function symbol) 'generic-function) "Generic Function")
                            (t "Function"))))
               (with-division (:class "LispDoc-Signature"  :stream stream)
                 (write-lisp-symbol symbol stream)
                 (fast-format stream " ~I: " (write-lisp-type (function-type-string symbol) stream))
                 (with-division (:class "arguments" :inline-p t :stream stream)
                   (fast-format stream "~A" (www-utils:arglist symbol))))))
           (display-variable-documentation (symbol stream)
             (with-division (:class "LispDoc-Signature"  :stream stream)
               (write-lisp-symbol symbol stream)
               (fast-format stream " ~I: " (write-lisp-type "variable" stream))
               (if (boundp symbol)
                   (let ((value-string (write-to-string (symbol-value symbol) :escape t)))
                     (declare (dynamic-extent value-string))
                     (with-division (:class "value" :inline-p t :stream stream)
                       (write-string-quoting-specials value-string stream)))
                 (fast-format stream "[unbound]"))))
           (display-class-documentation (symbol package stream)
             (flet ((write-field (label items name-key stream)
                      (with-rendition (:italic :stream stream)
                        (fast-format stream "~A:" label))
                      (loop initially (fast-format stream " (")
                            for item in items
                            for sym = (funcall name-key item)
                            do (when (symbol-external-p sym package)
                                 (unless (eq item (car items)) (fast-format stream " "))
                                 (with-division (:class "symbol" :inline-p t :stream stream)
                                   (fast-format stream "~S" sym)))
                            finally (fast-format stream ")"))))
               (let* ((class (find-class symbol))
                      (structure-p (typep class 'structure-class))
                      superclasses slots)
                 (with-division (:class "LispDoc-Signature"  :stream stream)
                   (write-lisp-symbol symbol stream)
                   (fast-format stream " ~I " (write-lisp-type (if structure-p "structure" "class") stream)))
                 (unless structure-p
                   (with-division (:class "class-info" :stream stream)
                     (cond-every
                      ((setq superclasses (class-superclasses class))
                       (write-field "Superclasses" superclasses #'class-name stream))
                      ((setq slots (class-slots class))
                       (when superclasses (break-line :stream stream))
                       (write-field "Slots" slots #'slot-definition-name stream)))))))))
    (declare (dynamic-extent #'write-lisp-symbol))
    (ecase type
      (cl:function
       (display-function-docmentation symbol stream))
      (cl:type
       (display-class-documentation symbol package stream))
      (cl:variable
       (display-variable-documentation symbol stream)))
    (with-division (:class "LispDoc-description"  :stream stream)
      (let ((doc-string  (documentation symbol type)))
        (cond (doc-string
               (with-verbatim-text (:width nil :stream stream)
                 (write-string-quoting-specials (documentation symbol type) stream)))
              (t (fast-format stream "[Undocumented]")))))))

(defmacro with-index-and-documentation-link-writers ((manual) &body body)
  `(flet ((write-index-link (stream)
            (let ((url:*escape-search-urls* nil)
                  (index-url (make-object-index-link ,manual package symbol type constraints)))
              (declare (dynamic-extent index-url))
              (fast-format stream "~S" index-url)))
          (write-documentation-link (stream)
            (let ((url:*escape-search-urls* nil)
                  (documentation-url (make-object-documentation-link ,manual package symbol type)))
              (declare (dynamic-extent documentation-url))
              (fast-format stream "~S" documentation-url))))
     (declare (dynamic-extent #'write-index-link #'write-documentation-link))
     ,@body))

(defmethod layout-manual-page ((manual basic-manual) (layout (eql :scrolling)) stream package symbol type constraints)
  (with-index-and-documentation-link-writers (manual)
    (with-table-row-group (:body :stream stream)
      (with-table-row (:class "LispDoc" :stream stream)
        (with-table-cell (:vertical-alignment :top :stream stream)
          (with-object (:name "index-pane" :data #'write-index-link :data-type "text/html" 
                        :class "LispDoc-index" :stream stream)
            (with-division (:class "LispDoc-index" :stream stream)
              (display-index-symbols manual stream package symbol type constraints))))
        (with-table-cell (:vertical-alignment :top :horizontal-alignment :left :stream stream)
          (when (and symbol type)
            (with-object (:name "documentation-pane" :data #'write-documentation-link :data-type "text/html" 
                          :class "LispDoc-documentation" :stream stream)
              (with-division (:class "LispDoc-documentation"  :stream stream)
                (display-documentation manual stream package symbol type)))))))))

(defmethod layout-manual-page ((manual basic-manual) (layout (eql :full-display)) stream package symbol type constraints)
  (with-index-and-documentation-link-writers (manual)
    (with-table-row-group (:body :stream stream)
      (with-table-row (:class "LispDoc" :stream stream)
        (with-table-cell (:vertical-alignment :top :stream stream)
          (with-division (:class "LispDoc-index" :stream stream)
            (display-index-symbols manual stream package symbol type constraints)))
        (with-table-cell (:vertical-alignment :top :horizontal-alignment :left :stream stream)
          (when (and symbol type)
            (display-documentation manual stream package symbol type)))))))

(defmethod display-manual-page ((manual basic-manual) stream package symbol type constraints)
  "Renders a table with cells for all packages, package symbols and symbol documentation."
  (flet ((write-banner (title stream)
           (with-division (:class "LispDoc-header" :stream stream)
             (etypecase title
               (string
                (fast-format stream "~A" title))
               (function
                (funcall title stream))))))
    (declare (inline write-banner))
    (with-table (:class "LispDoc" :stream stream)
      (with-table-row-group (:header :stream stream)
        (with-table-row (:stream stream)
          (with-table-cell (:column-span 2 :stream stream)  
            (write-banner (manual-title manual) stream)))
        (with-table-row (:stream stream)
          (with-table-cell (:column-span 2 :stream stream)  
            (display-component-toolbar manual stream package constraints))))
      (when package
        (layout-manual-page manual (manual-display-type manual) stream package symbol type constraints)))))

;;;------------------------------------------------------------------- 
;;;
;;;  RESPONSE FUNCTIONS
;;;

(defmethod write-style-sheet ((manual basic-manual) stream)
  (let ((style-sheet (or (manual-style-sheet manual) *lispdoc-css*)))
    (etypecase style-sheet
      (string
       (fast-format stream "~&~A~&" style-sheet))
      (function
       (funcall style-sheet stream)))))

(defun lispdoc-style-sheet-response (url stream)
  "The response function for the documentation browser."
  (let ((manual (or (http:get-value url 'manual) (error "No LispDoc manual found."))))
    (http:with-conditional-get-response (stream :html :expires (url:expiration-universal-time url)
                                                :cache-control (url:response-cache-control-directives url)
                                                :content-language (url:languages url))
      (write-style-sheet manual stream))))

(defmethod manual-for-url ((url url:http-url))
  (or (get-value url 'manual) (error "No LispDoc manual found.")))

(defmethod manual-for-url ((url url:http-search))
  (or (get-value (%search-parent url) 'manual) (error "No LispDoc manual found.")))

(defmethod (setf manual-for-url) ((manual basic-manual) (url url:http-url))
  (setf (get-value url 'manual) manual))

;; replace simple value parsing with w3p presentation types -- 9/19/2005 JCMa
(defun lispdoc-arguments-from-url (url)
  (declare (values manual package symbol type constraints))
  (flet ((white-space-p (char)
           (or (white-space-char-p char) (eql char #\+))))
    (declare (inline white-space-p))
    (http:bind-query-values (package symbol type apropos)
        (url (url:search-keys url))
      (let ((manual (manual-for-url url))
            pkg sym lisp-type constraints)
        (cond-every
         (package
          (cond ((setq pkg (find-package package))
                 ;; Prevents access to undocumented packages via LispDoc.
                 (unless (documented-package-p manual pkg)
                   (error "The package, ~A, is not a package documented by the LispDoc manual, ~A." package (manual-name manual))))
                (t (error "~A, is an unknown package for LispDoc." package))))
         (symbol
          (setq sym (find-symbol symbol package)))
         (type
          (setq lisp-type (get-documentation-type-from-string type)))
         (apropos
          (setq constraints (unless (or (null apropos)
                                        (null-string-p apropos)
                                        (every #'white-space-p apropos))
                              (nsubstitute #\space #\+ (nstring-unescape-special-chars apropos))))))
        (values manual pkg sym lisp-type constraints)))))

(defun lispdoc-response (url stream)
  "The response function for the documentation browser."
  (multiple-value-bind (manual package symbol type constraints)
      (lispdoc-arguments-from-url url)
    (let ((title (manual-title manual)))
      (http:with-conditional-get-response (stream :html :expires (url:expiration-universal-time url)
                                                  :cache-control (url:response-cache-control-directives url)
                                                  :content-language (url:languages url))
        (with-html-document (:declare-dtd-version-p :strict :language :en :direction :left-to-right :stream stream)
          (with-document-preamble (:stream stream)
            (declare-base-reference url :target :self :stream stream)
            (declare-document-style "text/css" stream)
            #+ignore(declare-link :relation "stylesheet" :reference (manual-style-sheet-url manual) 
                                  :media-type "text/css" :media :screen :stream stream)
            (with-document-style-declared ("text/css" :stream stream)
              (write-style-sheet manual stream))
            (declare-title title :stream stream))
          (with-document-body (:stream stream)
            (display-manual-page manual stream package symbol type constraints)))))))

(defun lispdoc-index-response (url stream)
  "The response function for the documentation browser."
  (multiple-value-bind (manual package symbol type constraints)
      (lispdoc-arguments-from-url url)
    (http:with-conditional-get-response (stream :html :expires (url:expiration-universal-time url)
                                                :cache-control (url:response-cache-control-directives url)
                                                :content-language (url:languages url))
      (with-html-document (:declare-dtd-version-p :strict :language :en :direction :left-to-right :stream stream)
        (with-document-preamble (:stream stream)
          (declare-title "LispDoc Index" :stream stream)
          (declare-base-reference url :target :parent :stream stream)
          (declare-document-style "text/css" stream)
          #+ignore (declare-link :relation "stylesheet" :reference (manual-style-sheet-url manual) 
                                 :media-type "text/css" :media :screen :stream stream)
          ;; using the css link above loses in safari as the index pane background does not display properly -- 9/22/2005 JCMa
          (with-document-style-declared ("text/css" :stream stream)
            (write-style-sheet manual stream)))
        (with-document-body (:class "LispDoc-index" :stream stream)
          (display-index-symbols manual stream package symbol type constraints))))))

(defun lispdoc-documentation-response (url stream)
  "The response function for the documentation browser."
  (multiple-value-bind (manual package symbol type)
      (lispdoc-arguments-from-url url)
    (cond ((and package symbol type) ;required arguments
           (http:with-conditional-get-response (stream :html :expires (url:expiration-universal-time url)
                                                       :cache-control (url:response-cache-control-directives url)
                                                       :content-language (url:languages url))
             (with-html-document (:declare-dtd-version-p :strict :language :en :direction :left-to-right :stream stream)
               (with-document-preamble (:stream stream)
                 (declare-base-reference url :target :self :stream stream)
                 (declare-document-style "text/css" stream)
                 #+ignore(declare-link :relation "stylesheet" :reference (manual-style-sheet-url manual) 
                                       :media-type "text/css" :media :screen :stream stream)
                 (with-document-style-declared ("text/css" :stream stream)
                   (write-style-sheet manual stream))
                 (declare-title "LispDoc Documentation" :stream stream))
               (with-document-body (:class "LispDoc" :stream stream)
                 (display-documentation manual stream package symbol type)))))
          (t (error 'bad-syntax-provided :format-string "PACKAGE, SYMBOL and TYPE were not provided." :url url)))))

;;;------------------------------------------------------------------- 
;;;
;;;  EXPORT URLs
;;;

(defgeneric manual-interned-urls (manual &key host port protocol)
  (declare (values style-sheet-url base-search-url index-search-url documentation-search-url))
  (:documentation "Returns the interned URLs comprising the interface to MANUAL on HOST at PORT over PROTOCOL."))

(defmethod manual-interned-urls ((manual basic-manual) &key (host (local-host-domain-name)) (port http:*standard-http-port*)
                                 (protocol http:*standard-protocol*))
  (declare (values style-sheet-url base-search-url index-search-url documentation-search-url))
  (flet ((inten-manual-url (string)
           (let ((args (list string :host host :port port :protocol protocol)))
             (declare (dynamic-extent args))
             (url:intern-url (http:merge-url args (http:local-context)) :if-does-not-exist :create))))
    (declare (dynamic-extent #'inten-manual-url))
    (values (inten-manual-url (manual-style-sheet-url manual))
            (inten-manual-url (manual-base-search-url manual))
            (inten-manual-url (manual-index-search-url manual))
            (inten-manual-url (manual-documentation-search-url manual)))))

(defgeneric export-lispdoc-interface (manual &rest export-arguments &key host port protocol &allow-other-keys)
  (:documentation "Exports the Web interface to LispDoc.
Additional keyword export arguments may be passed to EXPORT-URL via EXPORT-ARGUMENTS.
PORT, PROTOCOL, HOST control the corresponding components of the exported URL."))

(defmethod export-lispdoc-interface ((manual string) &rest export-arguments &key &allow-other-keys)
  (declare (dynamic-extent export-arguments))
  (apply #'export-lispdoc-interface (intern-manual manual) export-arguments))

(defmethod export-lispdoc-interface  ((manual basic-manual) &rest export-arguments &key &allow-other-keys)
  (declare (dynamic-extent export-arguments))
  (destructuring-bind (&key (host (local-host-domain-name)) (port http:*standard-http-port*) 
                            (protocol http:*standard-protocol*) &allow-other-keys) export-arguments
    (let* ((additional-arguments (loop for (key value) on export-arguments by #'cddr
                                       unless (member key '(:host :port :protocol))
                                       collect key
                                       and collect value))
           (export-args `(:max-age ,*lispdoc-http-cache-expiration*
                          :must-revalidate t
                          :public t
                          :language :en
                          :keywords (:cl-http :documentation :lispdoc)
                          ,.additional-arguments)))
      (declare (dynamic-extent export-args))
      (multiple-value-bind (style-sheet-url base-search-url index-search-url documentation-search-url)
          (manual-interned-urls manual :host host :port port :protocol protocol)
        ;; install the LispDoc manual object
        (setf (manual-for-url style-sheet-url) manual
              (manual-for-url base-search-url) manual
              (manual-for-url index-search-url) manual
              (manual-for-url documentation-search-url) manual)
        ;; Export the various URLs
        (apply #'http:export-url style-sheet-url
               :computed
               :response-function 'lispdoc-style-sheet-response
               :documentation "Displays CSS style sheet for LispDoc documentation."
               export-args)
        (apply #'http:export-url base-search-url
               :search
               :response-function 'lispdoc-response
               :search-parser #'url:parse-search-info-as-query-alist
               :search-writer #'url:write-search-info-as-query-alist
               :documentation "Displays reference documentation for LispDoc."
               export-args)
        (apply #'http:export-url index-search-url
               :search
               :response-function 'lispdoc-index-response
               :search-parser #'url:parse-search-info-as-query-alist
               :search-writer #'url:write-search-info-as-query-alist
               :documentation "Displays the index pane for LispDoc."
               export-args)
        (apply #'http:export-url documentation-search-url
               :search
               :response-function 'lispdoc-documentation-response
               :search-parser #'url:parse-search-info-as-query-alist
               :search-writer #'url:write-search-info-as-query-alist
               :documentation "Displays the documentation pane for LispDoc."
               export-args)))))

(defgeneric unexport-lispdoc-interface (manual &key host port protocol)
  (:documentation "Unexports the URLs comprising the MANUAL interface on HOST at PORT for PROTOCOL."))

(defmethod unexport-lispdoc-interface  ((manual basic-manual) &key (host (local-host-domain-name)) (port http:*standard-http-port*)
                                        (protocol http:*standard-protocol*))
  (multiple-value-bind (style-sheet-url base-search-url index-search-url documentation-search-url)
      (manual-interned-urls manual :host host :port port :protocol protocol)
    (url:unintern-url style-sheet-url)
    (url:unintern-url base-search-url)
    (url:unintern-url index-search-url)
    (url:unintern-url documentation-search-url)))
  
;;;------------------------------------------------------------------- 
;;;
;;;  DEFINE CL-HTTP-DOCUMENTATION
;;;

(define-documentation 
    ("CL-HTTP"
     :title "CL-HTTP Reference Manual"
     :display-type :scrolling)
  ("HTTP" :internals t) "WWW-UTILS" "W3P" "TK1" "CLIM-SYS" "MD5" ("URL" :internals t) #+CL-HTTP-FTP-CLIENT "FTP" "SMTP"
  "BASE64" "TASK-QUEUE" "RSS2.0" "HTML4.0" "SHA" "W4" "XHTML1.0" "HTML3.2" ("LAMBDA-IR" :internals t) "NETSCAPE4.0")

;;; End of File
