;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (html4 :use (future-common-lisp www-utils url html-parser)); Base: 10 -*-

;;;
;;; HTML 4
;;;
;;; Copyright (c) 2000-2001, 2003 Conrad Bookout. All Rights Reserved.
;;;
;;; Other packages copyrighted by their respective authors.
;;;

(in-package :cl-user)

;;;
;;;  Ensure html-parser available and initialized
;;;

#-html-parser (error "Missing prerequisite: HTML-PARSER.")

(eval-when (load compile eval)
  (when (not html-parser:*current-dtd*)
    (error "HTML-PARSER not initialized.")))

;;;
;;; Define HTML4 package
;;;

(eval-when (load compile eval)

  #+Genera (if (find-package :html4)
	       (scl:pkg-kill :html4))

  (defpackage html4
    (:use future-common-lisp www-utils url html-parser)
    #+Genera
    (:import-from scl
     "STRING-SEARCH"
     "SUBSTRING")
    (:import-from http
     "FAST-FORMAT")
    (:import-from html2
     "*OUTPUT-STREAM*"
     "*SECTION-LEVEL*"
     "%WRITE-COMMAND-KEY-ARG"
     "%ISSUE-COMMAND"
     "%WITH-ENVIRONMENT")
    (:import-from html-parser
     "*DEFAULT-DTD*"
     "*HTML-DTD-LIST*"
     "ALLOWED-VALUES")
    (:export
      "*FRESH-LINE*"
      "*TRAILING-LINE*"
      "DECLARE-DOCUMENT-TYPE" 
      "DECLARE-ELEMENT"
      "DECLARE-TITLE"
      "WITH-COMMENT"
      "WITH-ELEMENT"
      "WITH-FONTSTYLE"
      "WITH-HEADING"
      "WITH-PHRASE"
      "WITH-SECTION-HEADING"
      ))

;;;
;;; Change the HTML nickname to point to HTML4
;;;

  #+ignore
  (progn
    (rename-package (find-package :html3.2) "HTML3.2")
    (rename-package (find-package :html4) "HTML4" '("HTML")))

  )

(in-package :html4)

#+cl-http-documentation-facility 
(http:add-module-for-find-documentation "HTML4")

;;;
;;; Global variables
;;;

(defparameter *fresh-line* t)

(defparameter *trailing-line* t)

;;;
;;; Utilities
;;;

(defun symbolize (parameter)
    (cond 
      ((or (typep parameter 'keyword)
	   (typep parameter 'symbol)) (intern (symbol-name parameter) :html4))
      ((typep parameter 'string) (intern (string-upcase parameter) :html4))
      (t (error "Unable to symbolize ~A." parameter))))

;;;
;;; Attribute validation
;;;

(defun required-attributes (html-tag)
  (loop for attribute in (html-parser::attributes (tag-definition html-tag))
	if (eq :required (html-parser::default-value attribute))
	  collect (html-parser::name attribute)))

(defun attribute-ok? (attribute-tag value)
  ;; verification is rather loose...
  ;;   for example: pixels are verified as cdata,
  ;;     due to the way the parser distills attributes.
  (let ((true-value (case (type-of value)
		      (string (fast-format nil "~A" value))
		      (symbol (fast-format nil "~A" (string-downcase (symbol-name value))))
		      (url (url:coerce-url-string value))
		      (otherwise
			(fast-format nil "~A" value))))
	(allowed-values (allowed-values attribute-tag)))
    (case (type-of allowed-values)
      (cons (if (member true-value (mapcar #'token-name allowed-values) :test #'string-equal)
		(values true-value nil)
		(values nil (mapcar #'token-name allowed-values))))
      (html-name-token
	(let ((type (intern (token-name allowed-values) :html4)))
	  (case type
	    (number (if (numberp value) (values value nil) (values nil "number")))
	    (otherwise (values true-value nil)))))
      (otherwise (values true-value nil)))))

(defun %build-attribute-list (element &rest rest &key attributes &allow-other-keys)
  "Returns a string of valid attribute=value pairs and a list of missing required attributes."
  (let ((attributes (if attributes attributes rest))
	(required-attributes (required-attributes element))
	(stream (make-string-output-stream)))
    (loop for attribute in attributes by #'cddr
	  for value in (cdr attributes) by #'cddr
	  do
      (let ((tag (tag-attribute-definition (symbol-name attribute) (tag-definition element))))
	(if tag
	    (multiple-value-bind (value error) (attribute-ok? tag value)
	      (when error
		(error "~A:~A must be ~A ~A." element (token-name (name tag)) (if (consp error) "one of" "a") error))
	      (setf required-attributes (remove (name tag) required-attributes))
	      (%write-command-key-arg stream (string-downcase (symbol-name attribute)) value))
	    (error "~A is an invalid attribute for ~A." attribute element))))
    (values (get-output-stream-string stream) required-attributes)))

;;;
;;; Declare functions
;;;

(defun declare-element (element-name &rest rest &key attributes (fresh-line *fresh-line*) (trailing-line *trailing-line*) (stream *output-stream*) &allow-other-keys)
  "Issues commands for elements with empty content: for example, META data."
  (declare (special fresh-line trailing-line))
  (let ((element-name (typecase element-name
			((or keyword symbol) (symbol-name element-name))
			(string element-name)
			(otherwise (error "The declare-element element-name parameter ~S must be a keyword, symbol, or string." element-name)))))
    (when (tag-definition element-name)
      (multiple-value-bind (attribute-string missing-attributes) 
	  (%build-attribute-list element-name :attributes (if attributes attributes rest))
	(if missing-attributes 
	    (error "~A requires attributes ~A." element-name (mapcar #'token-name missing-attributes))
	    (%issue-command ((string-downcase element-name) stream :fresh-line fresh-line :trailing-line trailing-line)
	      (fast-format stream "~A" attribute-string)))))))

(eval-when (load compile eval)
  (defun define-declare-function (element &optional alias)
    "Define declare function to invoke declare-element"
    (let ((function-name (intern (if alias
				     (fast-format nil "~A" alias)
				     (fast-format nil "DECLARE-~A" element)) :html4))
	  (documentation 
	    (fast-format nil "Element: ~A~%~%Attributes: ~A" 
			 (if alias alias element)
			 (loop for attribute in (html-parser::attributes (tag-definition element)) 
			       collect (token-name (name attribute)) into attribute-list
			       finally (return (sort attribute-list #'string-lessp))))))
      (eval `(defun ,function-name (&rest attributes &key (element ,element) (fresh-line *fresh-line*) (trailing-line *trailing-line*) (stream *output-stream*) &allow-other-keys)
	       ,documentation
	       (declare-element element :attributes attributes :fresh-line fresh-line :trailing-line trailing-line :stream stream)))
      (export function-name))))

;;;
;;; With macros
;;;

(defmacro with-element ((element-name &rest attributes &key (fresh-line '*fresh-line*) (stream '*output-stream*) &allow-other-keys) &body body)
  "Executes BODY within an HTML element environment."
  `(let ((element-name (typecase ,element-name
			 ((or keyword symbol) (symbol-name ,element-name))
			 (string ,element-name)
			 (otherwise (error "The with-element element-name parameter ~S must be a keyword, symbol, or string." ,element-name)))))
     (when (tag-definition element-name)
       (multiple-value-bind (attribute-string missing-attributes) 
	   (%build-attribute-list element-name ,.attributes)
	 (if missing-attributes 
	     (error "~A requires attributes ~A." element-name (mapcar #'token-name missing-attributes))
	     (%with-environment ((string-downcase element-name) :fresh-line ,fresh-line :stream ,stream)
				(fast-format ,stream "~A" attribute-string)
	       ,@body))))))

(eval-when (load compile eval)
  (defun define-with-macro (element &optional alias)
    "Define with macro to invoke with-element"
    (let ((macro-name (intern (if alias
				  (fast-format nil "~A" alias)
				  (fast-format nil "WITH-~A" element)) :html4))
	  (documentation 
	    (fast-format nil "Element: ~A~%~%Attributes: ~A" 
			 (if alias alias element)
			 (loop for attribute in (html-parser::attributes (tag-definition element)) 
			       collect (token-name (name attribute)) into attribute-list
			       finally (return (sort attribute-list #'string-lessp))))))
      (eval `(defmacro ,macro-name ((&rest attributes &key (element ,element) (fresh-line '*fresh-line*) (stream '*output-stream*) &allow-other-keys) &body body)
	       ,documentation
	       `(with-element (,element :fresh-line ,fresh-line :stream ,stream ,.attributes)
			      ,@body)))
      (export macro-name))))

;;;
;;; Generate the functions/macros...
;;;

(eval-when (load compile eval)

  (defparameter *tags-without-content* '(area base basefont br col frame hr img input isindex link meta param))

  (defparameter *tags-to-ignore* '())		;for manually written code

  (defun generate-code ()
    (dolist (tag *dtd-tags*) 
      (let* ((tag-name (token-name (html-parser:name (tag-definition tag))))
	     (tag-symbol (intern tag-name :html4)))
	(cond ((member tag-symbol *tags-to-ignore*) nil)
	      ((member tag-symbol *tags-without-content*) (define-declare-function tag-name))
	      (t (define-with-macro tag-name))))))

  (export 'generate-code)

  (generate-code))

;;;
;;; Special declare functions
;;;

(defun declare-document-type (&key (fresh-line *fresh-line*) (trailing-line *trailing-line*) (stream *output-stream*))
  "Declares the document type as the current HTML generation DTD.
All HTML must declare the document type."
  (declare (special fresh-line trailing-line))
  (let* ((dtd (intern (string-upcase (cadr (assoc *default-dtd* *html-dtd-list* :test #'string=))) :html4))
	 (dtd-version *default-dtd*)
	 (dtd-reference 
	   (case dtd
	     (html-4-0-strict "http://www.w3.org/TR/html4/strict.dtd")
	     (html-4-0-frameset "http://www.w3.org/TR/1999/REC-html401-19991224/frameset.dtd")
	     (html-4-0-transitional "http://www.w3.org/TR/1999/REC-html401-19991224/loose.dtd"))))
    (%issue-command ("!DOCTYPE HTML PUBLIC" stream :fresh-line fresh-line :trailing-line trailing-line)
      (fast-format stream " ~S ~S" dtd-version dtd-reference))))

(defun declare-title (title &rest attributes &key (fresh-line *fresh-line*) (trailing-line *trailing-line*) (stream *output-stream*) &allow-other-keys)
  #.(fast-format nil "Element: ~A~%~%Attributes: ~A" :title 
		 (loop for attribute in (html-parser::attributes (tag-definition "TITLE")) 
		       collect (token-name (name attribute)) into attribute-list
		       finally (return (sort attribute-list #'string-lessp))))
  (declare (ignore trailing-line))
  (with-element (:title :attributes attributes :fresh-line fresh-line :stream stream)
    (etypecase title
      (string (write-string title stream))
      (function (funcall title stream)))))

;;;
;;; Special with macros
;;;

(defmacro with-comment ((&key (fresh-line 'html4:*fresh-line*) (stream 'html2:*output-stream*)) &body body)
  "Establishes comment environment around BODY.

  FRESH-LINE ::= T | nil

    FRESH-LINE == T

      <!--
      BODY
      -->

    FRESH-LINE == nil

      <!-- BODY -->"
  `(multiple-value-prog1 
     (progn (fresh-line ,stream)
            (write-string "<!-- " ,stream)
	    (when ,fresh-line (fresh-line ,stream))
            ,@body)
     (progn (when ,fresh-line (fresh-line ,stream))
	    (write-string " -->" ,stream)
	    (when ,fresh-line (fresh-line ,stream)))))

(defmacro with-fontstyle ((fontstyle-element &rest attributes &key (fresh-line '*fresh-line*) (stream '*output-stream*) &allow-other-keys) &body body)
  #.(fast-format nil "Element: FONTSTYLE

FONTSTYLE-ELEMENT ::= :TT | :I | :B | :BIG | :SMALL

Parameters: ~A" (loop for attribute in (html-parser::attributes (tag-definition "TT")) 
		  collect (token-name (name attribute)) into attribute-list
		  finally (return (sort attribute-list #'string-lessp))))

  (if (member (symbolize fontstyle-element) '(tt i b big small))
      `(with-element (,fontstyle-element :fresh-line ,fresh-line :stream ,stream ,.attributes)
		     ,@body)))

(defmacro with-phrase ((phrase-element &rest attributes &key (fresh-line '*fresh-line*) (stream '*output-stream*) &allow-other-keys) &body body)
  #.(fast-format nil "Element: PHRASE

PHRASE-ELEMENT ::= :EM | :STRONG | :DFN | :CODE | :SAMP |
                   :KBD | :VAR | :CITE | :ABBR | :ACRONYM
Parameters: ~A" (loop for attribute in (html-parser::attributes (tag-definition "EM")) 
		  collect (token-name (name attribute)) into attribute-list
		  finally (return (sort attribute-list #'string-lessp))))

  (if (member (symbolize phrase-element) '(em strong dfn code samp kbd var cite abbr acronym))
      `(with-element (,phrase-element :fresh-line ,fresh-line :stream ,stream ,.attributes)
		     ,@body)))

(defun %heading (&key heading attributes (level *section-level*) (fresh-line *fresh-line*) (stream *output-stream*) &aux element-name)
  (declare (special fresh-line))
  (when (and (integerp level) (>= level 1) (<= level 6))
    (setq element-name (fast-format nil "h~S" level))
    (when (tag-definition element-name)
      (multiple-value-bind (attribute-string missing-attributes) 
	  (%build-attribute-list element-name :attributes attributes)
	(if missing-attributes 
	    (error "~A is missing the required attributes ~A" element-name (mapcar #'token-name missing-attributes))
	    (%with-environment ((string-downcase element-name) :fresh-line fresh-line :stream stream)
			       (fast-format stream "~A" attribute-string)
			       (etypecase heading
				 (string (write-string heading stream))
				 (function (funcall heading stream)))))))))

(defmacro with-heading ((heading &rest attributes &key (level '*section-level*) (fresh-line 'html4:*fresh-line*) (stream 'html2:*output-stream*) &allow-other-keys) &body body)
  "Excutes BODY within a section heading of depth LEVEL.
HEADING can be a string or a function called with (stream)."
  (if body
      `(progn
	 (%heading :heading ,heading :attributes ',attributes :level ,level :fresh-line ,fresh-line :stream ,stream)
	 (let ((*section-level* (the fixnum (1+ ,level))))
	   ,@body))
      `(%heading :heading ,heading :attributes ',attributes :level ,level :fresh-line ,fresh-line :stream ,stream)))

(defmacro with-section-heading ((heading &rest attributes &key (level '*section-level*) (fresh-line '*fresh-line*) (stream '*output-stream*) &allow-other-keys) &body body)
  "Excutes BODY within a section heading of depth LEVEL.
HEADING can be a string or a function called with (stream)."
  (if body
      `(progn
	 (%heading :heading ,heading :attributes ',attributes :level ,level :fresh-line ,fresh-line :stream ,stream)
	 (let ((*section-level* (the fixnum (1+ ,level))))
	   ,@body))
      `(%heading :heading ,heading :attributes ',attributes :level ,level :fresh-line ,fresh-line :stream ,stream)))

;;;
;;; Add a few aliases...
;;;

(define-declare-function :base :declare-base-reference)
(define-declare-function :br :break-line)
(define-declare-function :br :line-break)
(define-declare-function :hr :horizontal-line)
(define-declare-function :hr :horizontal-rule)
(define-declare-function :img :image)
(define-declare-function :input :accept-input)

(define-with-macro :html :with-html-document)
(define-with-macro :head :with-document-preamble)
(define-with-macro :body :with-standard-document-body)
(define-with-macro :form :with-fillout-form)

(define-with-macro :a :with-anchor)
(define-with-macro :a :with-anchor-noted)
(define-with-macro :colgroup :with-column-group)
(define-with-macro :div :with-division)
(define-with-macro :em :with-emphasis)
(define-with-macro :optgroup :with-option-group)
(define-with-macro :p :with-paragraph)
(define-with-macro :pre :with-preformatted-text)
(define-with-macro :tr :with-table-row)
(define-with-macro :td :with-table-cell)
(define-with-macro :td :with-table-data)
(define-with-macro :th :with-table-header)
(define-with-macro :thead :with-table-head)
(define-with-macro :tbody :with-table-body)
(define-with-macro :tfoot :with-table-foot)
(define-with-macro :li :with-list-item)
(define-with-macro :ol :with-ordered-list)
(define-with-macro :ul :with-unordered-list)

;;;
;;; that's all, folks!
;;;

