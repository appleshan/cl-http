;;;   -*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-
;;;
;;; (c) Copyright 1999, 2005, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; HTML 4.01 GENERATION
;;;
;;; Specification available at: http://www/w3.org/TR/REC-html40/
;;;
;;; This facility is a largely complete and accurate HTML 4.01 implementation.
;;; Please be on the lookout for any missing or incorrect functionality
;;; and report it right away. 9/12/2005 -- JCMa.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; PACKAGE DEFINITION
;;;

(in-package :html4.0)

;;;------------------------------------------------------------------- 
;;;
;;;  DOCUMENT LEVEL OPERATIONS
;;;

(define-condition html4-generation-error
		  (html-generation-error)
  ()
  (:documentation "Signalled when HTML 4.01 generation code detects an HTML error."))

(defvar *xhtml-generation* nil
  "Non-null when generating XHTML, other null when generating HTML")

(defvar *dtd-version* :transitional
  "Holds the keyword describing the current DTD declaration.")

(eval-when (:compile-toplevel :execute :load-toplevel)
(defvar *strict-dtd* nil
  "Non-null when generating strict HTML 4.")

(defmacro %issue-command ((command stream &key fresh-line trailing-line) &body argument-body)
  `(progn
     ,.(when fresh-line
         `((fresh-line ,stream)))
     (write-char #\< ,stream)
     (write-string ,command ,stream)
     ,@argument-body
     (when *xhtml-generation*
       (write-char #\/ ,stream))
     (write-char #\> ,stream)
     ,.(when trailing-line
         `((fresh-line ,stream)))))
) ; eval-when

(define declare-html-version (&optional (stream *output-stream*) (dtd-version :frameset))
  "Declares the document type as the current HTML generation DTD.
All HTML 4.0 must declare the document type definition version.

   DTD-VERSION can be any of:

      :STRICT       - includes all elements that have not been deprecated and do not appear in
                      frameset documents.

      :TRANSITIONAL - includes everything is the STRICT DTD plus deprecated elements and attributes.

      :FRAMESET     - includes everything in TRANSITIONAL plus frames."
  (when dtd-version
    (%issue-command ("!DOCTYPE HTML PUBLIC" stream :fresh-line t :trailing-line t)
      (ecase dtd-version
        ((:frameset t)
         (fast-format stream " ~S ~S" "-//W3C//DTD HTML 4.01 Frameset//EN" "http://www.w3.org/TR/html4/frameset.dtd"))
        (:strict
	  (fast-format stream " ~S ~S" "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd"))
        (:transitional
	  (fast-format stream " ~S ~S" "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd"))))))

(eval-when (:compile-toplevel :execute :load-toplevel)
(defun %make-with-html-document-env (stream declaration-function xml-declaration xml-args declare-dtd-version-p 
                                            language language-supplied-p direction direction-supplied-p body)
  (let* ((args (nconc xml-args
                      (when language-supplied-p
                        `((%write-language-argument ,stream ,language)))
                      (when direction-supplied-p
                        `((%write-direction-argument ,stream ,direction)))))
         (version (case declare-dtd-version-p
                    ((nil) nil)
                    ((t :frameset) :frameset)
                    (:transitional :transitional)
                    (:strict :strict)
                    (t declare-dtd-version-p)))
         (version-declaration (etypecase version
                                (null nil)
                                (keyword
                                 (ecase version
                                   (:frameset `(,declaration-function ,stream :frameset))
                                   (:transitional `(,declaration-function ,stream :transitional))
                                   (:strict `(,declaration-function ,stream :strict))))
                                ((or symbol cons)
                                 `(,declaration-function ,stream *dtd-version*))))
         (xml-p (not (null xml-args))))
    (if version-declaration
	`(let* ((*dtd-version* ,version)
                (*strict-dtd* (eq :strict *dtd-version*))
                (*xhtml-generation* ,xml-p))
           ,.xml-declaration
           ,version-declaration
	   (%with-environment ("html" :stream ,stream)
               ,(when args (cons 'progn args))
	     . ,body))
      `(let ((*xhtml-generation* ,xml-p))
         (%with-environment ("html" :stream ,stream)
             ,(when args (cons 'progn args))
           . ,body)))))
) ;close eval-when

(define-macro with-html-document ((&key (stream '*output-stream*) (declare-dtd-version-p :transitional)
					(language nil language-supplied-p) (direction nil direction-supplied-p))
				  &body body)
  "Asserts the contents of BODY is an HTML document.

DECLARE-DTD-VERSION-P declares the version of the DTD corresponding to the mark up generated within
BODY and can be any of :TRANSITIONAL, :FRAMESET, or :STRICT. 

     :TRANSITIONAL is the default and supports various deprecated features compatible with earlier
     versions of HTML.

     :FRAMESET must be used when specifying an frame.

     :STRICT should be used when the generated HTML uses no deprecated features, and thus strictly
     adheres to the HTML standard.

Whenever extension tags or features outside these specifications are used, DECLARE-DTD-VERSION-P
should always be NIL.

LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)

DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT."
  (%make-with-html-document-env stream 'declare-html-version nil nil declare-dtd-version-p
                                language language-supplied-p direction direction-supplied-p
                                body))

#|
(with-html-document (:declare-dtd-version-p t :language :en :direction :left-to-right)
  (print 'foo))

|#

(eval-when (:compile-toplevel :execute :load-toplevel)

(defun %get-deprecation-checking-strings ()
  (declare (values continue-string markup-string))
  (if *xhtml-generation*
      (values "Continue XHTML Generation" "XHTML 1.0")
    (values "Continue HTML Generation" "HTML 4.01")))

;; Use this to detect generation of deprecated arguments when generating strict HTML 4.01
(defmacro %with-deprecation-checking ((function condition &optional argument value value-supplied-p 
						format-string format-args) &body body)
  `(progn
     (when ,condition
       ,(cond (argument
	       `(multiple-value-bind (continue-string markup-string)
                    (%get-deprecation-checking-strings)
                  (cerror continue-string
                          "In ~A, for ~S the argument, ~S, ~:[~; with value, ~S,~] is deprecated.~:[~;~&~:*~?~]"
                          markup-string ,function ,argument ,value-supplied-p ,value ,format-string ,format-args)))
	      (function
	       `(multiple-value-bind (continue-string markup-string)
                    (%get-deprecation-checking-strings)
                  (cerror continue-string
                          "In ~A, ~S is deprecated.~:[~;~&~:*~?~]" ,function ,format-string ,format-args)))
	      (t `(multiple-value-bind (continue-string markup-string)
                      (%get-deprecation-checking-strings)
                    (cerror continue-string (concatenate 'string "In " markup-string ", " ,format-string) ,@format-args)))))
     ,@body))

(defmacro with-deprecation-checking ((function &key argument (value nil value-supplied-p) (condition '*strict-dtd*) (compile-condition t)
						   format-string format-args) &body body)
  (if compile-condition 
      `(%with-deprecation-checking (',function ,condition ',argument ,value ,value-supplied-p ,format-string ,@format-args)
	 ,@body)
      `(progn ,@body)))

;; DTD-VERSION can be any of :frameset, :transitional, :strict
(defmacro with-dtd-version-checking ((dtd-version operator) &body body)
  `(progn
     (unless ,(etypecase dtd-version 
		(symbol
		  (ecase dtd-version
		    ((:frameset :transitional :strict)
		     `(eq *dtd-version* ,dtd-version))))
		(cons
		  (assert (every #'(lambda (x) (member x '(:frameset :transitional :strict))) dtd-version))
		  `(member *dtd-version* ',dtd-version)))
       (error "The declared HTML DTD is ~A, which is not compatible with ~S." *dtd-version* ',operator))
     ,@body))
)

;;;------------------------------------------------------------------- 
;;;
;;; EVENT HANDLERS
;;;

(define-parameter *event-handlers* 
    '((:java-script ;; events defined by HTML 4.01 spec
       (:input-defocus . "onblur")
       (:input-focus . "onfocus")
       (:key-down . "onkeydown")
       (:key-press . "onkeypress")
       (:key-up . "onkeyup")
       (:load . "onload")
       (:mouse-click . "onclick")
       (:mouse-double-click . "ondblclick")
       (:mouse-down . "onmousedown")
       (:mouse-move . "onmousemove")
       (:mouse-out . "onmouseout")
       (:mouse-over . "onmouseover")
       (:mouse-up . "onmouseup")
       (:new-value . "onchange")
       (:reset . "onreset")
       (:select . "onselect")
       (:submit . "onsubmit")
       (:unload . "onunload")
       (:form-reset . "onreset"))) ;backward compatibility to pre-html 4.01
  "An alist of (SCRIPTING-LANGUAGE (EVENT-KEYWORD . EVENT-TAG)).
This variable describes all known client-side scripting languages.")

(defun back-translate-event-handler (event &optional (language :java-script))
  "Back translates EVENT for the scripting language, LANGUAGE, to the HTML 4.01 keyword."
  (flet ((event-equal (x y)
           (etypecase x
             (string
              (etypecase y
                (string (string-equal x y))
                (symbol (string-equal x (symbol-name y)))))
             (symbol
              (etypecase y
                (string (string-equal (symbol-name x) y))
                (symbol (if (eq (symbol-package x) (symbol-package y))
                            (eq x y)
                          (string-equal (symbol-name x) (symbol-name y)))))))))
    (let ((events-alist (or (cdr (assoc language *event-handlers*))
                            (error "Unknown scripting language, ~S." language))))
      (or (car (rassoc event events-alist :test #'EVENT-equal))
          (error "Unknown event, ~S, for the scripting language, ~S." event language)))))

(defun back-translate-html-event-string (string)
  (flet ((translate (str start end)
           (let ((event (subseq str start end)))
             (back-translate-event-handler event :java-script))))
    (http::parse-comma-separated-header string 0 (length string) #'translate)))

(defun sorted-back-translated-html-event-string (string)
  (sort (back-translate-html-event-string string) #'string<))
 
(defun %get-event-tag (event language)
  (let ((entry (assoc language *event-handlers* :test #'eq)))
    (if entry
        (or (cdr (assoc event (cdr entry) :test #'eq))
            (error "~S is an unknown event for ~S." event language))
      (error "~S is not one of the known event languages: ~S" language (mapcar #'car *event-handlers*)))))

;; Primary function for writing events in HTML markup
(defun %write-events-argument (stream events)
  (etypecase events
    ;; when function are pass in, just call them because they were compiled
    ;; with the with-event-handlers macro.
    (function (funcall events stream))
    ;; otherwise run interpreted code.
    (cons
     (dolist (event events)
       (etypecase event
         (function (funcall event stream))
         (cons
          (destructuring-bind ((event . language) command) event
            (fast-format stream " ~A=" (%get-event-tag event language))
            (etypecase command
              (string
               (fast-format stream "~S"command))
              (function 
               (funcall command stream))
              ((and symbol (satisfies fbound-symbol-p))
               (funcall command stream))))))))))

(defun %collect-events (event-clauses)
  (loop with fnames2 and fbodies2
        for (language . clause) in event-clauses
        do (loop for (event command) on clause by #'cddr
                 for tag = (%get-event-tag event language)
                 for function-name = (gensym (format nil "~A-EVENT-HANDLER-" event))
                 for key-arg-code = `(write-string ,(concatenate 'string " " tag "=") stream)
                 collect `(function ,function-name) into fnames
                 collect (typecase command
                           (string
                            `(,function-name (stream)
                                             ,key-arg-code
                                             (fast-format stream "~S" ,command)))
                           (t `(,function-name (stream)
                                               (let ((command ,command))
                                                 (declare (dynamic-extent command))
                                                 ,key-arg-code
                                                 (etypecase command
                                                   (function
                                                    (funcall command stream))
                                                   (string 
                                                    (fast-format stream "~S" command))
                                                   ((and symbol (satisfies fbound-symbol-p))
                                                    (funcall command stream)))))))
                 into fbodies
                 finally (psetq fnames2 fnames
                                fbodies2 fbodies))
        nconc fnames2 into function-names
        nconc fbodies2 into function-bodies
        finally (return (values function-names function-bodies))))

(define-macro with-event-handlers ((variable &rest clauses) &body body)
  "Binds VARIABLE within scope of BODY to client-side event handlers specified by CLAUSES .

CLAUSES is list of languages and event-clauses of the form:

     (SCRIPTING-LANGUAGE &rest EVENTS).

EVENTS are a sequence of EVENT COMMAND pairs of the form:

     (EVENT1 COMMAND1 EVENT2 COMMAND2 ...)

EVENT can be any event keyword for SCRIPTING-LANGUAGE and COMMAND is either a string,
an s-expression returning a string or a function. If it returns a function, the
function will be called on STREAM to emit the script calling string.

Normal usage involves using DEFINE-SCRIPT to define a script object which is then reused via the
EVENT-CALLER method.  EVENT-CALLER returns a function that writes the correct invocation string.

Any number of EVENT and COMMAND pairs can be bound or one or more scripting languages.

*EVENT-HANDLERS* holds the valid events available for each known scripting languages.

Valid HTML 4.01 events

   :LOAD                The onload event occurs when the user agent finishes loading a window or all
                        frames within a FRAMESET. This attribute may be used with BODY and FRAMESET
                        elements.

   :UNLOAD              The onunload event occurs when the user agent removes a document from a
                        window or frame. This attribute may be used with BODY and FRAMESET elements.

   :MOUSE-CLICK         The onclick event occurs when the pointing device button is clicked over an
                        element. This attribute may be used with most elements.

   :MOUSE-DOUBLE-CLICK  The ondblclick event occurs when the pointing device button is double
                        clicked over an element. This attribute may be used with most elements.

   :MOUSE-DOWN          The onmousedown event occurs when the pointing device button is pressed over
                        an element. This attribute may be used with most elements.

   :MOUSE-UP            The onmouseup event occurs when the pointing device button is released over
                        an element. This attribute may be used with most elements.

   :MOUSE-OVER          The onmouseover event occurs when the pointing device is moved onto an
                        element. This attribute may be used with most elements.

   :MOUSE-MOVE          The onmousemove event occurs when the pointing device is moved while it is
                        over an element. This attribute may be used with most elements.

   :MOUSE-OUT           The onmouseout event occurs when the pointing device is moved away from
                        an element. This attribute may be used with most elements.

   :INPUT-FOCUS         The onfocus event occurs when an element receives focus either by the
                        pointing device or by tabbing navigation. This attribute may be used with
                        the following elements: A, AREA, LABEL, INPUT, SELECT, TEXTAREA, and BUTTON.

   :INPUT-DEFOCUS       The onblur event occurs when an element loses focus either by the pointing
                        deviceor by tabbing navigation. It may be used with the same elements as
                        onfocus.

   :KEY-PRESS           The onkeypress event occurs when a key is pressed and released over an
                        element.This attribute may be used with most elements.

   :KEY-DOWN            The onkeydown event occurs when a key is pressed down over an element.
                        This attribute may be used with most elements.

   :KEY-UP              The onkeyup event occurs when a key is released over an element. This
                        attribute may be used with most elements.

   :SUBMIT              The onsubmit event occurs when a form is submitted. It only applies to the
                        FORM element.

   :RESET               The onreset event occurs when a form is reset. It only applies to the
                        FORM element.

   :SELECT              The onselect event occurs when a user selects some text in a text field.
                        This attribute may be used with the INPUT and TEXTAREA elements.

   :NEW-VALUE           The onchange event occurs when a control loses the input focus and its value
                        has been modified since gaining focus. This attribute applies to the
                        followingelements: INPUT, SELECT, and TEXTAREA.

Here is an example of how to use this macro.

  (with-document-preamble (:stream stream)
    (declare-script script stream))
  (with-document-body (:stream stream)
    (with-event-handlers
      (events
        (:java-script
          :new-value (event-caller script 10 20)))
      (accept-input 'string query-name
                    :events events
                    :stream stream)))"
  (multiple-value-bind (function-names function-bodies)
      (%collect-events clauses)
    `(flet ,function-bodies
       (declare (dynamic-extent ,@function-names))
       (let ((,variable (list ,@function-names)))
         (declare (dynamic-extent ,variable))
         ,@body))))

#|
(with-event-handlers
    (events
     (:java-script
      :new-value (event-caller script 10 20)
      :mouse-over (event-caller script 5 10)))
  (accept-input 'string query-name
                :events events
                :stream stream)))

(with-event-handlers
      (events
        (:java-script
          :new-value "if (!checkNum(this.value, 1, 10)) 
        {this.focus();this.select();} else {thanks()}"))
  (accept-input 'string "num" :default "0" :events events :stream *standard-output*))

|#


;;;------------------------------------------------------------------- 
;;;
;;; GENERATION UTILITIES
;;;

;; Revised version of html2 function for html 4 and xhtml 1.0
(defun %write-command-key-arg (stream option &optional (value :no-value))
  (flet ((get-symbol-tag (symbol) ;downcase symbols for XHTML
           (or (get symbol 'token-string)
               (let ((pname (symbol-name symbol)))
                 ;; downcase unslashified symbols
                 (setf (get symbol 'token-string) (if (every #'upper-case-p pname) (string-downcase pname) pname)))))
         (write-char-handling-special-tokens (value stream)
           (let ((token-string (cdr (assoc value *special-character-translation-alist*))))
             (if token-string
                 (write-string token-string stream)
               (write-char value stream))))
         (write-unknown-type (value stream)
           (let ((string (write-to-string value :escape nil :base 10)))
             (declare (dynamic-extent string))
             (write-string-quoting-specials string stream))))
    (declare (inline get-symbol-tag write-char-handling-special-tokens write-unknown-type))
    (cond ((eq value :no-value)
           (if *xhtml-generation*
               (fast-format stream " ~A=~S" option option)
             (fast-format stream " ~A" option)))
          ;; Avoid line breaks and multiple white space characters within attribute values. 
          ;; These are handled inconsistently by user agents (XHTML 1.0).
          (t (fast-format stream " ~A=" option)
             (cond #|((and (numberp value) (not *xhtml-generation*)) (write value :stream stream :base 10.))|#
                   ((functionp value)
                    (funcall value stream))
                   (t (write-char #\" stream)
                      (typecase value
                        (string
                         (write-string-quoting-specials value stream))
                        (symbol
                         (write-string-quoting-specials (get-symbol-tag value) stream))
                        (number
                         (write value :stream stream :base 10.))
                        (character
                         (write-char-handling-special-tokens value stream))
                        (t (write-unknown-type value stream)))
                      (write-char #\" stream)))))))

(defmacro maybe-coerce-uri (uri &optional (relativize-url-p 'url:*relativize-urls*))
  `(typecase ,uri
     (function ,uri)
     (t (coerce-url-string ,uri url:*escape-search-urls* ,relativize-url-p))))

(defconstant *horizontal-alignment-values* '(:left :center :right :justify))

(defun horizontal-alignment-value (alignment)
  (unless (member alignment *horizontal-alignment-values*)
    (error "Unknown alignment, ~S, for a paragraph, division, or inline frame." alignment))
  (symbol-name alignment))

(defun %write-alignment-argument (stream alignment)
  (%write-command-key-arg stream "align" (horizontal-alignment-value alignment)))

;; check the percentage spec to make sure it remains the same   3/23/99 -- JCMa.
(defun %write-length-argument (tag length stream)
  (if (<= 0 length 1)
      (fast-format stream " ~A=~D%" tag (floor (* length 100)))
      (%write-command-key-arg stream tag length)))

(declaim (inline %write-integer-argument))

(defun %write-integer-argument (stream option value)
  (check-type value integer)
  (%write-command-key-arg stream option value))

;;; HTML 4.0 only allows strings matching the pattern: [A-Za-z][A-Za-z0-9:_.-]*
(defun check-identifier-string (string &optional (start 0) (end (length string)) &aux error character)
  "Signals an error if string contains characters that are not compatible with XHTML and HTML."
  (declare (fixnum start end))
  (flet ((valid-char-p (char)
           (let ((char-code (char-code char)))
             (declare (fixnum char-code))
             (or (<= #.(char-code #\a) char-code #.(char-code #\z))
                 (<= #.(char-code #\0) char-code #.(char-code #\9))
                 (<= #.(char-code #\A) char-code #.(char-code #\Z))
                 (member char-code '#.(mapcar #'char-code '(#\* #\- #\. #\: #\[ #\] #\_)) :test #'=)))))
    (declare (inline valid-char-p))
    (with-fast-array-references ((string string string))
      (cond ((alpha-char-p (aref string start))
             (loop for idx fixnum upfrom (1+ start) below end
                   for char = (char string idx)
                   do (unless (valid-char-p char)
                        (psetq error :rest-chars
                               character char)
                        (return))
                   finally (return-from check-identifier-string t)))
            (t (psetq error :first-char
                      character (aref string start)))))
    ;; don't get errors inside the checking code
    (ecase error
      (:first-char
       (error "The character, ~C, is not an alphanetcial as required for the first character in element ID or NAME for HTML 4.01." character))
      (:rest-chars
       (error "The character, ~C, is not valid as element ID or NAME for HTML 4.01." character)))))

(defparameter *xhtml-backward-compatible-identifiers* t
  "Controls whether XHTML element IDs are assured to be backward compatible with HTML 4.0.")

(defparameter *check-identifier-validity* t
  "Controls whether HTML element names and IDs are checked for validity.
Turning this off can improve performance marginally in tested production systems.")

(defun %write-id-argument (stream id &optional (backward-compatible-xml-generation-p (and *xhtml-generation* *xhtml-backward-compatible-identifiers*)))
  (cond ((or backward-compatible-xml-generation-p *check-identifier-validity*)
         (let ((id-string (etypecase id
                            (string id)
                            (symbol (symbol-name id))
                            (function (with-output-to-string (str)
                                        (funcall id str))))))
           (declare (dynamic-extent id-string))
           (check-identifier-string id-string)
           (%write-command-key-arg stream "id" id-string)))
        (t (%write-command-key-arg stream "id" id))))

(defun %write-xhtml-id-argument-backward-compatibly (stream id)
  (let ((id-string (cond (*check-identifier-validity*
                          (etypecase id
                            (string id)
                            (symbol (symbol-name id))
                            (function (with-output-to-string (str)
                                        (funcall id str)))))
                         (t id))))
    (declare (dynamic-extent id-string))
    (check-identifier-string id-string)
    (%write-command-key-arg stream "name" id-string)
    (%write-command-key-arg stream "id" id-string)))

(declaim (inline %write-id-argument-handling-xhtml-backward-compatibly))

(defun %write-id-argument-handling-xhtml-backward-compatibly (stream id)
  (cond ((and *xhtml-generation* *xhtml-backward-compatible-identifiers*)
         (%write-xhtml-id-argument-backward-compatibly stream id))
        (t (%write-id-argument stream id nil))))

(defun %write-class-argument (stream class)
  (typecase class
    (cons
      (fast-format stream " class=\"~I\""
		   (loop for items = class then (cdr items)
			 while (cdr items)
			 do (fast-format stream "~A " (car items))
			 finally (fast-format stream "~A" (car items)))))
    (t (%write-command-key-arg stream "class" class))))

(defun %write-style-argument (stream style)
  (%write-command-key-arg stream "style" style))

(defun %write-name-argument (stream name)
  (cond ((if *xhtml-generation* *xhtml-backward-compatible-identifiers* *check-identifier-validity*)
         (let ((name-string (etypecase name
                            (string name)
                            (symbol (symbol-name name))
                            (function (with-output-to-string (str)
                                        (funcall name str))))))
           (check-identifier-string name-string)
           (%write-command-key-arg stream "name" name-string)))
        (t (%write-command-key-arg stream "name" name))))

;; define the standard link types and provide mechanisms for adding new ones via profile. -- JCMa 8/28/2005
(defun %get-link-type (link-type &optional (error-p *strict-dtd*))
  (declare (ignore error-p))
  (etypecase link-type
    (symbol (symbol-name link-type))
    (string link-type)))

;; define the languages sometime.   3/23/99 -- JCMa.
(defun %language-iso-code (language &optional (error-p t))
  (declare (ignore error-p))
  language)

(defun %write-language-argument (stream language)
  (let ((iso-code  (%language-iso-code language t)))
    (when *xhtml-generation* ;emit both xml NS lang and lang for compatibility
      (%write-command-key-arg stream "xml:lang" iso-code))
    (%write-command-key-arg stream "lang" iso-code)))

(defun %write-media-type (media-type-spec stream)
  (etypecase media-type-spec
    (string
     (fast-format stream "~S" media-type-spec))
    (cons
     (http::write-mime-content-type-as-string media-type-spec stream))
    (keyword
     (write-mime-content-type-keyword-as-string media-type-spec stream))))

(defun %write-media-type-argument (stream tag media-type-spec)
  (typecase media-type-spec
    ((or keyword cons)
     (flet ((writer (stream)
              (%write-media-type media-type-spec stream)))
       (declare (dynamic-extent #'writer))
       (%write-command-key-arg stream tag #'writer)))
    (t (%write-command-key-arg stream tag media-type-spec))))

(defun %write-charset-argument (stream charset)
  (%write-command-key-arg stream "charset" charset))

(defun %write-accept-charset-argument (stream charset)
  (flet ((write-accept-charset-value (stream)
	   (loop initially (write-char #\" stream)
		 for (chrst . more) = charset then more
		 do (etypecase chrst
		      (string (write-string chrst stream))
		      (symbol (write-string (symbol-name chrst) stream))
		      (function (funcall chrst stream)))
		 while more
		 do (write-string ", " stream)	;delimit with both comma and space
		 finally (write-char #\" stream))))
    (declare (dynamic-extent #'write-accept-charset-value))
    (%write-command-key-arg stream "accept-charset" (etypecase charset
						      (atom charset)
						      (cons #'write-accept-charset-value)))))

(defun %write-accept-media-type-argument (stream media-type)
  (flet ((write-accept-media-type-value (stream)
	   (etypecase media-type
	     (string 
              (fast-format stream "~S" media-type))
	     (keyword
              (write-mime-content-type-keyword-as-string media-type stream))
	     (cons
              (etypecase (car media-type)
                (string 
                 (fast-format stream "~S" media-type))
                (keyword
                 (http::write-mime-content-type-as-string media-type stream))
                (cons
                 (loop initially (write-char #\" stream)
                       for (mt . more) = media-type then more
                       do (http::write-mime-content-type mt stream)
                       while more
                       do (write-char #\, stream)	;delimit with comma
                       finally (write-char #\" stream))))))))
    (declare (dynamic-extent #'write-accept-media-type-value))
    (%write-command-key-arg stream "accept" (typecase media-type
					      (function media-type)
					      (symbol
                                               (case media-type
                                                 (:unknown "unknown")	;special token denoting source content type.
                                                 (t #'write-accept-media-type-value)))
					      (t #'write-accept-media-type-value)))))

(defun %write-media-argument (stream media)
  (%write-command-key-arg stream "media" (ecase media
					   (:screen "screen")
                                           (:tty "tty")
                                           (:tv "tv")
                                           (:projection "projection")
                                           (:handheld "handheld")
                                           (:print "print")
                                           (:braile "braile")
                                           (:aural "aural")
                                           (:all "all"))))

(defun %write-direction-argument (stream direction)
  (%write-command-key-arg stream "dir" (ecase direction
					 (:left-to-right "ltr")
					 (:right-to-left "rtl"))))

(declaim (inline %write-title-argument))

(defun %write-title-argument (stream title)
  (%write-command-key-arg stream "title" title))

(defun %write-target-argument (stream target)
  (%write-command-key-arg stream "target" (etypecase target
					    (string target)
					    (keyword (special-target-window-name target)))))

(declaim (inline %write-tab-index-argument))

(defun %write-tab-index-argument (stream tab-index)
  (%write-integer-argument stream "tabindex" tab-index))

(defun %write-access-key-argument (stream access-key)
  (check-type access-key #.http:*standard-character-type*)
  (%write-command-key-arg stream "accesskey" access-key))

(defun %write-language-and-direction-arguments (stream language direction)
  (cond-every
    (language
      (%write-language-argument stream language))
    (direction
      (%write-direction-argument stream direction))))

;; inline version
(defmacro %%write-basic-arguments (stream id class language direction title style)
  `(cond-every 
    (,id 
     (%write-id-argument ,stream ,id))
    (,class 
     (%write-class-argument ,stream ,class))
    (,language
     (%write-language-argument ,stream ,language))
    (,direction
     (%write-direction-argument ,stream ,direction))
    (,title
     (%write-title-argument ,stream ,title))
    (,style 
     (%write-style-argument ,stream ,style))))

(defun %write-basic-arguments (stream id class language direction title style)
  (%%write-basic-arguments stream id class language direction title style))

;; inline version
(defmacro %%write-standard-arguments (stream id class language direction title style events)
  `(cond-every 
    (,id 
     (%write-id-argument ,stream ,id))
    (,class 
     (%write-class-argument ,stream ,class))
    (,language
     (%write-language-argument ,stream ,language))
    (,direction
     (%write-direction-argument ,stream ,direction))
    (,title
     (%write-title-argument ,stream ,title))
    (,style 
     (%write-style-argument ,stream ,style))
    (,events 
     (%write-events-argument ,stream ,events))))

(defun %write-standard-arguments (stream id class language direction title style events)
  (%%write-standard-arguments stream id class language direction title style events))


;;;------------------------------------------------------------------- 
;;;
;;; DOCUMENT PREAMBLE AND BODY
;;;

(defun %write-profile-arguments (stream profile language direction)
  (cond-every
    (profile
      (fast-format stream " profile=~I"
		   (etypecase profile
		     (atom
		       (prin1 (url:coerce-url-string profile) stream))
		     (cons
		       (loop initially (write-char #\" stream)
			     for (url . more) = profile then more
			     do (write-string (url:coerce-url-string url) stream)
			     while more
			     do (write-char #\space stream)
			     finally (write-char #\" stream))))))
    ((or language direction)
     (%write-language-and-direction-arguments stream language direction))))

(define-macro with-document-preamble ((&key profile language direction (stream '*output-stream*)) &body body)
  "Asserts the contents of BODY are heading declarations for the document (see HEAD).
PROFILE is a URI or a list of URIs thatspecifies the location of one or more metadata profiles.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT."
  `(%with-environment 
     ("head" :stream ,stream)
     ,(when (or profile language direction)
	`(%write-profile-arguments ,stream ,profile ,language ,direction))
     ,@body))

(defun %write-body-arguments (stream class id title style language direction events
                                     background-url background foreground link visited-link active-link)
  (%%write-standard-arguments stream id class language direction title style events)
  ;; write deprecated parameters
  (cond-every
   (background-url
    (with-deprecation-checking (with-document-body :argument background-url)
      (%write-command-key-arg stream "background" (typecase background-url
                                                    (keyword (background-url background-url))
                                                    (t (maybe-coerce-uri background-url))))))
   (background
    (with-deprecation-checking (with-document-body :argument background)
      (%write-command-key-arg stream "bgcolor" (color-mapping background))))
   (foreground
    (with-deprecation-checking (with-document-body :argument foreground)
      (%write-command-key-arg stream "text" (color-mapping foreground))))
   (link
    (with-deprecation-checking (with-document-body :argument link)
      (%write-command-key-arg stream "link" (color-mapping link))))
   (visited-link
    (with-deprecation-checking (with-document-body :argument visited-link)
      (%write-command-key-arg stream "vlink" (color-mapping visited-link))))
   (active-link
    (with-deprecation-checking (with-document-body :argument active-link)
      (%write-command-key-arg stream "alink" (color-mapping active-link))))))

(define-macro with-document-body ((&key class id title style language direction events
					background-url background foreground link visited-link active-link
                                        (stream '*output-stream*)) &body body)
  "Asserts the contents of BODY is the body of the document (see BODY).

CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or
:RIGHT-TO-LEFT.

EVENTS can be any of the intrinsic events: :LOAD, :UNLOAD, :KEY-DOWN, :KEY-PRESS, :KEY-UP,
:MOUSE-CLICK, :MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT, :MOUSE-OVER, :MOUSE-UP.

  Deprecated arguments are superseded by style sheets in HTML 4.01.

  BACKGROUND-URL   -- an image URL to use as the background 
  BACKGROUND       -- color for the background.
  FOREGROUND       -- color for the foreground.
  LINK             -- color for links
  VISITED-LINK     -- color for visited links.
  ACTIVE-LINK      -- color for active links.

  See the variable *BUILT-IN-CLIENT-COLORS* for a complete list of colors 
  built into the client. For information on how to use these, 
  see: http://home.netscape.com/assist/net_sites/bg/index.html
  The variable *built-in-backgrounds* contains a list of backgrounds
  and please use BACKGROUND-URL to map the keyword into the URL. 
  Note that Background URLs must be specified as either URL strings or 
  interned URLS because keywords are interpreted as referring to built-in 
  colors. For a sampling of backgrounds, 
  see: http://home.netscape.com/assist/net_sites/bg/backgrounds.html"
  `(%with-environment 
     ("body" :stream ,stream)
      (%write-body-arguments ,stream ,class ,id ,title ,style ,language ,direction ,events
					     ,background-url ,background ,foreground ,link ,visited-link ,active-link)
     ,@body))

(defun %standard-document-body-unpack-color-scheme (color-scheme)
  (when color-scheme
    (check-type color-scheme color-scheme)
    (values (color-scheme-background-url color-scheme)
            (color-scheme-background color-scheme)
            (color-scheme-foreground color-scheme)
            (color-scheme-link color-scheme)
            (color-scheme-visited-link color-scheme)
            (color-scheme-active-link color-scheme))))

(define-macro with-standard-document-body ((&key (color-scheme '*standard-color-scheme*) class id title style language direction events
                                                 (stream '*output-stream*)) &body body)
  "Asserts the contents of BODY are the body of the document open STREAM. (deprecated)
The page is displayed with the standard color scheme from *STANDARD-COLOR-SCHEME*."
  `(multiple-value-bind (background-url background foreground link visited-link active-link)
       (%standard-document-body-unpack-color-scheme ,color-scheme)
     (with-document-body (:class ,class :id ,id :title ,title :style ,style :language ,language :direction ,direction :events ,events :stream ,stream
                          :background-url background-url
                          :background background
                          :foreground foreground
                          :link link
                          :visited-link visited-link
                          :active-link active-link)
       ,@body)))


;;;------------------------------------------------------------------- 
;;;
;;; DECLARE OPERATORS USED WITHIN THE DOCUMENT PREAMBLE
;;;

(declaim (inline %write-link-arguments))

(defun %write-link-arguments (stream reference local-reference charset link-language media-type relation inverse media 
                                     id class language direction title style events target)
  (cond-every
   (relation ;; "The relationship the anchored document has to this one."
             (%write-command-key-arg stream "rel" (%get-link-type relation)))
   (media-type
    (%write-media-type-argument stream "type" media-type)))
  (cond ((and local-reference reference)
	 (html2::%write-href-with-local-reference stream reference local-reference))
        (reference ;; URL for a anchored document retrieved by the anchor.
                   ;; ensure generated URLs are escaped.
                   (%write-command-key-arg stream "href" (maybe-coerce-uri reference)))
        (local-reference ;; tags for a position in the current document.
                         (fast-format stream " href=#~A" local-reference)))
  (cond-every 
   (inverse ;; "The reverse relationship type, and the inverse of REL."
            (%write-command-key-arg stream "rev" (%get-link-type inverse)))
   (charset
    (%write-charset-argument stream charset))
   (link-language
    (%write-command-key-arg stream "hreflang" (%language-iso-code language t)))
   (media
    (%write-media-argument stream media))
   (target
    (%write-target-argument stream target)))
  (%%write-standard-arguments stream id class language direction title style events))

(defun %declare-link (stream reference local-reference charset link-language media-type relation inverse media
			     id class language direction title style events target)
  (%issue-command ("link" stream :fresh-line t :trailing-line t)
    (%write-link-arguments stream reference local-reference charset link-language media-type relation inverse media 
				   id class language direction title style events target)))

(declaim (inline declare-link))

(define declare-link (&key reference local-reference charset link-language media-type relation inverse target media 
			   class id language text-direction title style events (stream *output-stream*))
  "Declares a link between the current document and other documents or resources.

REFERENCE is a URL (object or string).
LOCAL-REFERENCE is reference to a tag local to the current page.
TAG is a string denoting a named point in the document which can be referenced
with a local reference (#name).
RELATION is the link type from the referring document to the the anchored
document.
INVERSE is the inverse relationship back from the anchored document to the
referring document.

MEDIA is one of:

     :SCREEN     - Non-paged computer screens.
     :TTY        - Media using a fixed-picth character grid (eg, teletypes, terminals,)
     :TV         - Television devises (low resolution, color, limited scrollability)
     :PROJECTION - Projectors.
     :HANDHELD   - Handheld devices (small screen, monochrome, bitmapped graphics, low bandwidth).
     :PRINT      - Paged, opaque material and documents viewed on screen in print preview.
     :BRAILE     - Braille tactile feedback devices.
     :AURAL      - Speech synthesizers.
     :ALL        - Suitable for all devices.

MEDIA-TYPE is a MIME content type for the linked URL.
LINK-LANGUAGE is the two-digit language code for the displayed content (see RFC 1766).
CHARSET is the character encoding of the resource designated by the link (see ISO 10646).
TAB-INDEX is an integer denoting position in the tabbing order.
ACCESS-KEY is a single accessibility character from ISO 10646.

SHAPE can be: 

     :RECTANGLE  -- accepts the COORDINATES (left top right bottom)
     :CIRCLE     -- accepts the COORDINATES (center-x center-y radius-x)
     :POLYGON    -- accepts COORDINATES as a series of pairs, (x1 y1 x2 y2 ...)

COORDINATES uses a zero origin and are in pixels of the image displayed.

TARGET specified the name of a frame where a document is to be opened. The value
is a string or a special keyword:

     :BLANK  - Always load this link into a new, unnamed window
     :SELF   - Always load this link over yourself
     :PARENT - Always load this link over your parent (becomes self if your at the top)
     :TOP    - Always load this link at top level (becomes self if your at the top)

CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)

DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or
:RIGHT-TO-LEFT.

EVENTS can be any of the intrinsic events: :KEY-DOWN, :KEY-PRESS, :KEY-UP, :MOUSE-CLICK,
:MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT, :MOUSE-OVER, :MOUSE-UP."
  (%declare-link stream reference local-reference charset link-language media-type relation inverse media
		 id class language text-direction title style events target))

(define declare-document-style (media-type &optional (stream *output-stream*))
  "Declares the default style sheet for a document to be MEDIA-TYPE.  Use only inside
the document preamble. In the absense of this declaration, the default scripting
language can be set by a CONTENT-STYLE-TYPE HTTP header."
  (flet ((writer (stream)
	   (%write-media-type media-type stream)))
    (declare (dynamic-extent #'writer))
    (declare-meta-info #'writer :header :content-style-type :stream stream)))

(defun %write-style-arguments (stream media-type media title language direction)
  (cond-every
    (media-type
      (%write-media-type-argument stream "type" media-type))
    (media
      (%write-media-argument stream media))
    (title
      (%write-title-argument stream title))
    ((or language direction)
     (%write-language-and-direction-arguments stream language direction))))

(define declare-title (title &key language direction (stream *output-stream*))
  "Declares the title for a document.
Every HTML document must declare exactly one TITLE element in the preamble.

  TITLE can be either a string or a function which is called with (STREAM).
  LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
  DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT."
  (%with-environment 
    ("title" :stream stream)
    (when (or language direction)
      (%write-language-and-direction-arguments stream language direction))
    (etypecase title
      (string (write-string title stream))
      (function (funcall title stream)))))

(define declare-base-reference (reference &key target (stream *output-stream*))
  
  "Declares REFERENCE as the base URI against which all relative URIs are merged in an HTML document.  
REFERENCE must be an absolute URI.  This should appear once in the headers section of a document.
When REFERENCE is a URL, the URL path is automatically computed.

TARGET provide the default frame target window for any hyperlink in the current
document. This is useful when many hyperlinks designate the same target."
  (flet ((write-base (stream)
           (etypecase reference
             (string
              (fast-format stream "\"~I\"" (http::write-url-remapping-context reference stream)))
             (url
              (fast-format stream "\"~I\""
                           (http::write-url-remapping-context (url:directory-name-string reference) stream))))))
    (declare (dynamic-extent #'write-base))
    (%issue-command ("base" stream :fresh-line t :trailing-line t)
      (%write-command-key-arg stream "href" #'write-base)
      (when target
        (%write-target-argument stream target)))))

(define declare-meta-info (value &key name header scheme language direction (stream *output-stream*))
  "Declares meta information within an HTML document.
NAME and/or HEADER specify the type of meta information.
VALUE is the content of the declaration.
HEADER is used only for defined HTTP response header information.
NAME is used in other cases.
SCHEME specifies how VALUE is interpreted based on the document profile.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
  DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT."
  (unless (or name header)
    (error "At least one of NAME or HEADER must be provided."))
  (%issue-command ("meta" stream :fresh-line t :trailing-line t)
    (cond-every
      (name 
       (%write-name-argument stream name))
      (header
        (check-type header keyword)
        (%write-command-key-arg stream "http-equiv" (or (http::%header-print-name header) header)))
      (scheme
	(%write-command-key-arg stream "scheme" scheme))
      ((or language direction)
       (%write-language-and-direction-arguments stream language direction)))
    (%write-command-key-arg stream "content" value)))

(define-macro with-document-style-declared ((media-type &key media title language direction (stream *output-stream*))
					    &body body)
  "Provides a preamble ennvironment for declaring style characteristics of a document.
  Code with BODY must write style sheet parameters to STREAM,

  MEDIA-TYPE is a MIME content type for style sheet in use.
  MEDIA is a descriptor for the target display. 
  Choices are: :SCREEN, :TTY, :TV, :PROJECTION, :HANDHELD, :PRINT, :BRAILE, :AURAL, OR :ALL.
  TITLE is a string used as the element title
  LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
  DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT."
  `(%with-environment 
     ("style" :stream ,stream)
     (%write-style-arguments ,stream ,media-type ,media ,title ,language ,direction)
     ,@body))

;;;------------------------------------------------------------------- 
;;;
;;; HYPERLINKS 
;;;

(defun %write-anchor-arguments (stream reference local-reference tag relation inverse target
                                       media-type link-language charset tab-index access-key shape coordinates
                                       id class language direction title style events)
  ;; write anchor arguments
  (cond ((and local-reference reference)
	 (html2::%write-href-with-local-reference stream reference local-reference))
        (reference
         ;; URL for a anchored document retrieved by the anchor. ensure generated URLs are escaped.
         (%write-command-key-arg stream "href" (maybe-coerce-uri reference)))
        (local-reference 
         ;; tags for a position in the current document.
         (etypecase local-reference
           (string
            (fast-format stream " href=#~A" local-reference))
           (function (fast-format stream " href=#~I" (funcall local-reference stream))))))
  (cond-every 
   (tag ; "A tag anchor." 
    (with-deprecation-checking (with-anchor-noted :argument tag :condition *xhtml-generation*
                                 :format-string "Use the ID argument for this purpose.")
      (%write-name-argument stream tag)))
   (relation ; "The relationship the anchored document has to this one."
    (%write-command-key-arg stream "rel" (%get-link-type relation)) )
   (inverse ; "The reverse relationship type, and the inverse of REL."
    (%write-command-key-arg stream "rev" (%get-link-type inverse)))
   (target
    (%write-target-argument stream target))
   (media-type
    (%write-media-type-argument stream "type" media-type))
   (link-language
    (%write-command-key-arg stream "hreflang" (%language-iso-code language t)))
   (charset
    (%write-charset-argument stream charset))
   (tab-index
    (%write-tab-index-argument stream tab-index))
   (access-key
    (%write-access-key-argument stream access-key))
   ((or shape coordinates)
    (unless (and shape coordinates)
      (error "Both SHAPE and COORDINATES must be provided for image map operations."))
    (%write-command-key-arg stream "shape" (%im-get-shape-arg shape coordinates))
    (%write-command-key-arg stream "coords" (%im-get-coordinate-arg coordinates)))
   (id
    (%write-id-argument-handling-xhtml-backward-compatibly stream id))
   (class 
    (%write-class-argument stream class))
   (language
    (%write-language-argument stream language))
   (direction
    (%write-direction-argument stream direction))
   (title
    (%write-title-argument stream title))
   (style 
    (%write-style-argument stream style))
   (events 
    (%write-events-argument stream events))))

(define-macro with-anchor-noted ((&key reference local-reference tag relation inverse target
				       media-type link-language charset tab-index access-key shape coordinates
				       id class language direction title style events (stream '*output-stream*))
                                 &body body)
  "Like NOTE-ANCHOR except forms in BODY can compute and write the anchored text on STREAM.
REFERENCE is a URL (object or string).
LOCAL-REFERENCE is reference to a tag local to the current page.

TAG (deprecated in XHTML 1.0) is a string denoting a named point in the document which can be
referenced with a local reference (#name). In XHTML, use ID for this purpose.

RELATION is the link type from the referring document to the the anchored
document.
INVERSE is the inverse relationship back from the anchored document to the
referring document.
TARGET is a string that designates the target browser window in which to open the linked
resource.

MEDIA-TYPE is a MIME content type for the linked URL.
LINK-LANGUAGE is the two-digit language code for the displayed content (see RFC 1766).
CHARSET is the character encoding of the resource designated by the link (see ISO 10646).
TAB-INDEX is an integer denoting position in the tabbing order.
ACCESS-KEY is a single accessibility character from ISO 10646.

SHAPE can be: 

     :RECTANGLE  -- accepts the COORDINATES (left top right bottom)
     :CIRCLE     -- accepts the COORDINATES (center-x center-y radius-x)
     :POLYGON    -- accepts COORDINATES as a series of pairs, (x1 y1 x2 y2 ...)

COORDINATES uses a zero origin and are in pixels of the image displayed.

CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or
:RIGHT-TO-LEFT.

EVENTS can be any of the intrinsic events: :INPUT-DEFOCUS, :INPUT-FOCUS, :KEY-DOWN, :KEY-PRESS,
:KEY-UP, :MOUSE-CLICK, :MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT, :MOUSE-OVER,
:MOUSE-UP."
  `(%with-environment
       ("a" :string-for-null-stream :inline :stream ,stream :fresh-line nil)
       (%write-anchor-arguments ,stream ,reference ,local-reference ,tag ,relation ,inverse ,target
                                ,media-type ,link-language ,charset ,tab-index ,access-key ,shape ,coordinates
                                ,id ,class ,language ,direction ,title ,style ,events)
     ,@body))

(define note-anchor (text &key reference local-reference tag relation inverse target
			  media-type link-language charset tab-index access-key shape coordinates
			  class id language direction title style events (stream *output-stream*))
  "Notes a hypertext anchor for TEXT on STREAM.
TEXT can be a string or a function.

REFERENCE is a URL (object or string).
LOCAL-REFERENCE is reference to a tag local to the current page.

TAG (deprecated in XHTML 1.0) is a string denoting a named point in the document which can be
referenced with a local reference (#name). In XHTML, use ID for this purpose.

RELATION is the link type from the referring document to the the anchored
document.
INVERSE is the inverse relationship back from the anchored document to the
referring document.

TARGET specifies the name of a frame where a document is to be opened. The value
is a string or a special keyword:

     :BLANK  - Always load this link into a new, unnamed window
     :SELF   - Always load this link over yourself
     :PARENT - Always load this link over your parent (becomes self if your at the top)
     :TOP    - Always load this link at top level (becomes self if your at the top)

MEDIA-TYPE is a MIME content type for the linked URL.
LINK-LANGUAGE is the two-digit language code for the displayed content (see RFC 1766).
CHARSET is the character encoding of the resource designated by the link (see ISO 10646).
TAB-INDEX is an integer denoting position in the tabbing order.
ACCESS-KEY is a single accessibility character from ISO 10646.

SHAPE can be: 

     :RECTANGLE  -- accepts the COORDINATES (left top right bottom)
     :CIRCLE     -- accepts the COORDINATES (center-x center-y radius-x)
     :POLYGON    -- accepts COORDINATES as a series of pairs, (x1 y1 x2 y2 ...)

COORDINATES uses a zero origin and are in pixels of the image displayed.

CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or
:RIGHT-TO-LEFT.

EVENTS can be any of the intrinsic events: EVENTS can be any of the intrinsic events:
:INPUT-DEFOCUS, :INPUT-FOCUS, :KEY-DOWN, :KEY-PRESS, :KEY-UP, :MOUSE-CLICK, :MOUSE-DOUBLE-CLICK,
:MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT, :MOUSE-OVER, :MOUSE-UP."
  (with-anchor-noted (:reference reference :local-reference local-reference :tag tag :relation relation :inverse inverse
                      :target target :media-type media-type :link-language link-language :charset charset 
                      :tab-index tab-index :access-key access-key :shape shape :coordinates coordinates
                      :id id :class class :language language :direction direction :title title :style style
                      :events events :stream stream)
    (etypecase text
      (string (write-string text stream))
      (function (funcall text stream)))))

(defun write-string-anchoring-urls (string stream &optional (start 0) (end (length string))
                                           &key target class id language direction style events)
  "Writes STRING to STREAM spotting any URLS and anchoring them 
while being careful to translated any special characters for HTML. See
NOTE-ANCHOR for documentation on the keyword arguments, which are
applied to all URL which are anchored."
  (loop with s = start
        doing (multiple-value-bind (url-start url-end)
                  (url::url-position string s end)
                (cond ((and url-start url-end)
                       (unless (= s url-start)
                         (write-string-quoting-specials string stream s url-start))
                       (let ((url (subseq string url-start url-end)))
                         (declare (dynamic-extent url))
                         (with-anchor-noted (:reference url :target target :class class :id id 
                                             :language language :direction direction 
                                             :style style :events events :stream stream)
                           (write-string-quoting-specials string stream url-start url-end)))
                       (if (< url-end end)
                           (setq s url-end)
                         (return-from write-string-anchoring-urls)))
                      (t (write-string-quoting-specials string stream s end)
                         (return-from write-string-anchoring-urls))))))

;;;------------------------------------------------------------------- 
;;;
;;; IMAGE EXTENSIONS
;;;

(defconstant *image-alignment-values* '(:top :middle :bottom :left :right))

;; html spec says to always provide alternative text
(defun %note-image (stream image-url alternative-text alignment accept-coordinates-at-url
                           client-side-image-map
                           border vertical-space horizontal-space width height name
			   description class id title style language direction events)
  (flet ((write-element (stream image-url alignment alternative-text accept-coordinates-at-url events)
           (flet ((alignment-value (alignment)
                    (unless (member alignment *image-alignment-values*)
                      (error "Unknown alignment, ~S, for an image." alignment))
                    (symbol-name alignment)))
             (declare (inline alignment-value))
	     ;; Automagically insert image sizes when algorithms available.
             (when (and image-url (not (or width height)) http:*image-sizes-default-automatically*)
	       (multiple-value-setq (width height)
                   (url:image-size image-url)))
             (%issue-command ("img" stream)
               (cond-every
                (image-url
                 (%write-command-key-arg stream "src" image-url))
                (alignment
                 (with-deprecation-checking (image :argument alignment)
                   (%write-command-key-arg stream "align" (alignment-value alignment))))
                (alternative-text
                 (check-type alternative-text string)
                 (%write-command-key-arg stream "alt" alternative-text))
                (name
                 (with-deprecation-checking (note-image :argument name :condition *xhtml-generation*
                                                        :format-string "Use the ID argument for this purpose.")
                   (%write-name-argument stream name)))
                (description
                 (check-type description string)
                 (%write-command-key-arg stream "longdesc" description))
                (accept-coordinates-at-url (%write-command-key-arg stream "ismap"))
                (client-side-image-map
                 (%write-command-key-arg
                  stream "usemap" (url:name-string-without-search-suffix client-side-image-map nil)))
                (border
                 (with-deprecation-checking (image :argument border)
                   (%write-integer-argument stream "border" border)))
                (vertical-space 
                 (with-deprecation-checking (image :argument vertical-space)
                   (%write-integer-argument stream "vspace" vertical-space)))
                (horizontal-space 
                 (with-deprecation-checking (image :argument horizontal-space)
                   (%write-integer-argument stream "hspace" horizontal-space)))
                (width (%write-integer-argument stream "width" width))
                (height (%write-integer-argument stream "height" height))
                (id 
                 (%write-id-argument-handling-xhtml-backward-compatibly stream id))
                (class 
                 (%write-class-argument stream class))
                (language
                 (%write-language-argument stream language))
                (direction
                 (%write-direction-argument stream direction))
                (title
                 (%write-title-argument stream title))
                (style 
                 (%write-style-argument stream style))
                (events 
                 (%write-events-argument stream events)))))))
    (let ((url-string (url:name-string-without-search-suffix image-url nil)))
      (declare (dynamic-extent url-string))
      (case accept-coordinates-at-url
        ((nil :no-url)
         (write-element stream url-string alignment alternative-text accept-coordinates-at-url events))
        (t (with-anchor-noted (:reference (if (eq accept-coordinates-at-url t)
                                              url-string
                                            (url:name-string-without-search-suffix accept-coordinates-at-url nil))
                               :stream stream)
             (write-element stream url-string alignment alternative-text accept-coordinates-at-url events)))))))

(declaim (inline image))

(define image (image-url alternative-text
                         &key alignment accept-coordinates-at-url
                         client-side-image-map
                         border vertical-space horizontal-space width height name
			 description class id title style language direction events
                         (stream *output-stream*))
  (%note-image stream image-url alternative-text alignment accept-coordinates-at-url
               client-side-image-map
               border vertical-space horizontal-space width height name 
	       description class id title style language direction events))

(setf (documentation 'image 'function)
      "Inserts an image into HTML.

IMAGE-URL is the URL for an image.

ALTERNATIVE-TEXT is the text to display when the image is not loaded.

ACCEPT-COORDINATES-URL can be:
   
     * URL to which coordinates will be returned when the user clicks on the image.

     * T, indicating that returned coordinates should go to a search URL version of
     IMAGE-URL
                
     * :NO-URL, indicating not to emit an anchor for accepting the returns but to
     mark the IMG as a coordinate search.

CLIENT-SIDE-IMAGE-MAP indicates the client side image map to use.  Normally, this is a named URL
(/url.html#map-name) and often, all client side image maps are served from a single url. The
function WRITE-CLIENT-SIDE-IMAGE-MAP writes a client side image map from a server-side image map URL
(CERN or NCSA formats).

Allow browsers to layout the display before the image has loaded and thus eliminate the delay for
the user otherwise incurred.

WIDTH is width of the image in pixels.

HEIGHT is the height of the image in pixels.

NAME (deprecated in XHTML 1.0) is names the element so that it may be referred to from style sheets
or scripts. This argument has been included for backwards compatibility, and HTML 4.01 applications
should use the ID. In XHTML, application must ID for this purpose.

DESCRIPTION provides a longer description of the image which supplements the short one in
alternative-text.

CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or
:RIGHT-TO-LEFT.

EVENTS are a set of image related events. They can be any of the intrinsic events: :KEY-DOWN,
:KEY-PRESS, :KEY-UP, :MOUSE-CLICK, :MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT,
:MOUSE-OVER, :MOUSE-UP.

ALIGNMENT (deprecated) can be:

     HTML2 Arugments

          :TOP    -- align with the tallest item on the line.
          :MIDDLE -- align with the baseline with the middle of the image.
          :BOTTOM -- align with the baseline of the current line with the image.
        
     Text Flow Options

          :LEFT -- float down and over to the next available space on :the left
          margin, and subsequent text wraps around the right side of the image.

          :RIGHT -- float down and over to the next available space on the right
          margin, and subsequent text wraps around the left side of the image.

BORDER (deprecated) is an integer indicating the thickness of the border with which to surround the
image.

VERTICAL-SPACE (deprecated) is an integer indicating the amount of vertical space above and below a
floating image.

HORIZONTAL-SPACE (deprecated) is an integer indicating the amount of horizontal space above and
below a floating image.")


;;;------------------------------------------------------------------- 
;;;
;;; EMBEDDED OBJECTS
;;;

(defun %write-archive-argument (stream archive)
  (etypecase archive
    (atom (%write-command-key-arg stream "archive" (maybe-coerce-uri archive)))
    (cons (loop initially (write-string "archive=" stream)
		for items = archive then (cdr items)
		while (cdr items)
		do (fast-format stream "~A " (maybe-coerce-uri (car items)))
		finally (fast-format stream "~A" (maybe-coerce-uri (car items)))))))

(define note-parameter (name value &key (interpretation :data) data-type id (stream *output-stream*))
  "Notes a runtime parameter for an applet or object named NAME with VALUE on STREAM.
This function must be used within the scope of a WITH-APPLET or WITH-OBJECT macro.

  NAME is a string or symbol specifying the attribute name. 
  VALUE is a string holding the parameter value or a 
  function that writes the value as an escaped string on STREAM.
  INTERPRETATION specifies how to interpret VALUE, and can be any of :DATA, :REFERENCE, or :OBJECT.
  DATA-TYPE specifies the media type of VALUE only when INTERPRETATION is :REFERENCE
  and can be a content type list, a string, or a function.
  ID is an element identifier."
  (%issue-command ("param" stream :fresh-line t :trailing-line t)
    (%write-name-argument stream name)
    (%write-command-key-arg stream "value" value)
    (%write-command-key-arg stream "value-type" (ecase interpretation
						  (:data "data")
						  (:reference "ref")
						  (:object "object")))
    (cond-every
      (data-type (%write-media-type-argument stream "type" data-type))
      (id (%write-id-argument stream id)))))

(defun %write-object-arguments (stream declaration-p name data code resources base data-type code-type
				       height width horizontal-space vertical-space border alignment
				       client-side-image-map title progress-message tab-index
				       class id language direction style events)
  (cond-every
   (declaration-p
    (%write-command-key-arg stream "declare" :no-value))
   (name (%write-name-argument stream name))
   (data (%write-command-key-arg stream "data" (maybe-coerce-uri data)))
   (code (%write-command-key-arg stream "classid" (maybe-coerce-uri code)))
   (resources (%write-archive-argument stream resources))
   (base (%write-command-key-arg stream "codebase" (maybe-coerce-uri base)))
   (data-type (%write-media-type-argument stream "type" data-type))
   (code-type (%write-media-type-argument stream "codetype" code-type))
   (height 
    (%write-integer-argument  stream "height" height))
   (width 
    (%write-integer-argument stream "width" width))
   (horizontal-space
    (%write-integer-argument stream "hspace" horizontal-space))
   (vertical-space
    (%write-integer-argument stream "vspace" vertical-space))
   (border
    (with-deprecation-checking (with-object :argument border)
      (%write-integer-argument stream "border" border)))
   (alignment
    (with-deprecation-checking (with-object :argument alignment)
      (unless (member alignment *image-alignment-values*)
        (error "Alignment, ~S, is not one of the known values, ~S."
               alignment *image-alignment-values*))
      (%write-command-key-arg stream "align" (symbol-name alignment))))
   (client-side-image-map
    (%write-command-key-arg stream "usemap" (url:name-string-without-search-suffix client-side-image-map nil)))
   (title (%write-command-key-arg stream "title" title))
   (progress-message (%write-command-key-arg stream "standby" progress-message))
   (language (%write-language-argument stream language))
   (direction (%write-direction-argument stream direction))
   (style (%write-style-argument stream style))
   (class (%write-class-argument stream class))
   (id (%write-id-argument stream id))
   (tab-index
    (%write-tab-index-argument stream tab-index))
   (events
    (%write-events-argument stream events))))

(defun %write-alterative-text (stream alternative-text)
  (etypecase alternative-text
    (null)
    (string (write-string alternative-text stream))
    (function (funcall alternative-text stream))))

(define-macro with-object ((&key name title code data resources base code-type data-type 
				 height width horizontal-space vertical-space border alignment
				 progress-message tab-index client-side-image-map
				 (alternative-text nil alternative-text-supplied-p)
				 declaration-p class id language direction style events 
				 (stream *output-stream*)) &body body)
  "Allows embedding of inline objects within HTML documents.

This generalization of inline images and inline applets provides a powerful mechanism to include
data or program output. This operator declares the object and associates it with NAME. Objects may
appear in the document preamble only when they do not render data, for example, in the case of a
program object.  In other cases, for example, when only DATA is specified, objects must appear in
the document body. When the object is a program requiring parameters, the function NOTE-PARAMETER
should be called with BODY for each parameter.
         
DECLARATION-P is a boolean indicating whether the current definition is a declaration only. If so
the object must be instantiated by a subsequent object definition referring to this one.

NAME is a string denoting the name for the element.

CODE may a URI specifying the location of the program that performs the rendering.

DATA may a URI specifying the location of data to be rendered.
RESOURCES can be a list of URIs containing resources relevant to this object.
CODE-TYPE is the media type of CODE, which can be content type list, a string, or a function.
DATA-TYPE is the media type of DATA, which can be content type list, a string, or a function.
BASE is a base URI against which to merge any relative URIs appearing in CODE, DATA, or resources.

HEIGHT is either the number of pixels or a fraction of the window.
WIDTH is either the number of pixels or fraction of the window.
HORIZONTAL-SPACE is an integer specifying the margin on each side of the object.
VERTICAL-SPACE is an integer specifying the margin above and below the object.

BORDER (deprecated) can be aninteger specifying the width in pixels of the border
around the object.

ALIGNMENT (deprecated) can be:

   :TOP    -- align with the tallest item on the line.
   :MIDDLE -- align with the baseline with the middle of the image.
   :BOTTOM -- align with the baseline of the current line with the image.

   :LEFT -- float down and over to the next available space on
   the left margin, and subsequent text wraps around the right
   side of the image.

   RIGHT -- float down and over to the next available space on
   the right margin, and subsequent text wraps around the left
   side of the image.

PROGRESS-MESSAGE is a string or function that provides a message to the user while the object loads.

TAB-INDEX is a number that specifies the position of the element in the current document.

CLIENT-SIDE-IMAGE-MAP indicates the client side image map to use.  Normally, this is a named URI
(/url.html#map-name) and often, all client side image maps are served from a single url. The
function WRITE-CLIENT-SIDE-IMAGE-MAP writes a client side image map from a server-side image map URL
(CERN or NCSA formats).

ALTERNATIVE-TEXT is a string or function that provides a message for clients unable to display the
object.

CLASS is the class for the element
ID is an element identifier.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or
:RIGHT-TO-LEFT.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.

EVENTS can be any of the intrinsic events: :KEY-DOWN, :KEY-PRESS, :KEY-UP, :MOUSE-CLICK,
:MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT, :MOUSE-OVER, :MOUSE-UP."
  `(%with-environment
       ("object" :stream ,stream)
       (%write-object-arguments ,stream ,declaration-p ,name ,data ,code ,resources ,base ,data-type ,code-type
                                ,height ,width ,horizontal-space ,vertical-space ,border ,alignment
                                ,client-side-image-map ,title ,progress-message ,tab-index
                                ,class ,id ,language ,direction ,style ,events)
     ,@body
     ,@(when alternative-text-supplied-p
	 `((%write-alterative-text ,stream ,alternative-text)))))


;;;------------------------------------------------------------------- 
;;;
;;; JAVA APPLET SUPPORT (DEPRECATED)
;;;

(define note-java-parameter (name value &optional (stream *output-stream*))
  "Writes a parameter for WITH-JAVA-APPLET. (Deprecated)

NAME is a string or symbol giving the attribute name. 
VALUE is a string holding the value of the parameter or a function that
writes the parameter value as an escaped string on STREAM."
  (with-deprecation-checking (note-java-parameter)
    (note-parameter name value :interpretation :data :stream stream)))

(defun %write-java-applet-args (stream code width height alignment name base-code-url
                                       horizontal-space vertical-space alternate-text
				       archive object id class title style)
  (with-deprecation-checking (with-java-applet)
    (unless (member alignment *image-alignment-values*)
      (error "Alignment, ~S, is not one of the known values, ~S."
	     alignment *image-alignment-values*))
    (unless (or code object)
      (error "Either CODE or OBJECT MUST be provided."))
    (%write-integer-argument stream "width" width)
    (%write-integer-argument stream "height" height)
    (%write-command-key-arg stream "align" (symbol-name alignment))    
    (cond-every
     (code
      (%write-command-key-arg stream "code" code))
     (name
      (with-deprecation-checking (with-java-applet :argument name :condition *xhtml-generation*
                                   :format-string "Use the ID argument for this purpose.")
        (%write-name-argument stream name)))
     (base-code-url
      (%write-command-key-arg stream "codebase" (maybe-coerce-uri base-code-url)))
     (horizontal-space
      (%write-integer-argument stream "hspace" horizontal-space))
     (vertical-space
      (%write-integer-argument stream "vspace" vertical-space))
     (alternate-text
      (%write-command-key-arg stream "alt" alternate-text))
     (archive
      (flet ((write-archive (stream)
               (write-char #\" stream)
               (etypecase archive
                 (url:uri
                  (write-string (coerce-url-string archive) stream))
                 (cons
                  (loop for (uri . more) = archive then more
                        do (write-string (coerce-url-string uri) stream)
                        while more
                        do (write-char #\, stream))))
               (write-char #\" stream)))
        (declare (dynamic-extent #'write-archive))
        (%write-command-key-arg stream "archive" #'write-archive)))
     (object
      (%write-command-key-arg stream "object" object))
     (id
      (%write-id-argument-handling-xhtml-backward-compatibly stream id))
     (class 
      (%write-class-argument stream class))
     (title
      (%write-title-argument stream title))
     (style (%write-style-argument stream style)))))

(define-macro with-java-applet ((code width height alignment
                                      &key parameters name base-code-url archive object 
				      horizontal-space vertical-space alternate-text
				      id class title style (stream '*output-stream*)) &body body)
  `(%with-environment
     ("applet" :stream ,stream)
     (%write-java-applet-args ,stream ,code ,width ,height ,alignment ,name ,base-code-url
			      ,horizontal-space ,vertical-space ,alternate-text
			      ,archive ,object ,id ,class ,title ,style)
     ,.(when parameters
	 `((loop for (param value) in ,parameters
		 do (note-java-parameter param value ,stream))))
     ,@body))

(setf (documentation 'with-java-applet 'function)
      "Writes invocation of a Java applet of code, CODE, on STREAM.  

This macro is intended for display-oriented applets embedded in HTML documents. BODY may emit HTML
for display on browser not supporting Java.

The Java applet must be exported for a client to invoke it with this form.  The export type
:JAVA-BINARY exports byte compiled Java binaries.  :JAVA-FILE may be used to provide access to the
Java source code.

Applet Parameters 

     PARAMETERS is an alist of (name value). When supplied, these are automatically transmitted to
     the applet.  Alternatively,

     NOTE-JAVA-PARAMETER can be called within BODY.

Required Arguments

     CODE is an appletFile giving the name of the file that contains the applets compiled Applet
     subclass.  This file is relative to the base URL of the applet.  It cannot be absolute.

     WIDTH is the initial width in pixels of the applet display area, not counting any windows or
     dialogs that the applet brings up.

     HEIGHT is the corresponding height in pixels.

     ALIGNMENT can be:

                :TOP    -- align with the tallest item on the line.
                :MIDDLE -- align with the baseline with the middle of the image.
                :BOTTOM -- align with the baseline of the current line with the image.

                :LEFT -- float down and over to the next available space on
                the left margin, and subsequent text wraps around the right
                side of the image.

                RIGHT -- float down and over to the next available space on
                :the right margin, and subsequent text wraps around the left
                side of the image.

        
Optional Arguments

     BASE-CODE-URL is a URI specifying the base URI of the applet -- the directory that contains the
     applets code.  If this attribute is not specified, then the documents URI is used.

     ARCHIVE is a URI or a list of URIs containing classes and other resources that will be
     preloaded. Relative URIs are interpreted with respect to BASE-CODE-URL

     OBJECT names a resource containing a serializzed representation of the applet's state.

     NAME (deprecated in XHTML 1.0) is a string specifying a name for the applet instance, which
     makes it possible for applets on the same page to find (and communicate with) each other. In
     XHTML, use ID for this purpose.

     HORIZONTAL-SPACE is an integer specifying the margin on each side of the applet.

     VERTICAL-SPACE is an integer specifying the margin above and below the applet.

     ALTERNATE-TEXT is a string specifying any text that should be displayed if the browser
     understands the APPLET tag but can't run applets written in the Java(tm) Programming Language.

     CLASS is the class for the element
     ID is an element identifier.
     TITLE is a string used as an element title.
     STYLE specifies inline parameters to use in the default style sheet language.")


;;;------------------------------------------------------------------- 
;;;
;;; DIVSION ENVIRONMENT ABSTRACTION OVER DIV AND SPAN
;;;

(defun %write-division-arguments (stream alignment class id title style language direction events)
  (when alignment
    (%write-alignment-argument stream alignment))
  (%%write-standard-arguments stream id class language direction title style events))

(defun section-heading (heading level stream &optional alignment class id 
				title style language direction events &aux command)
  (setq command (case level
                  (1 "h1")
                  (2 "h2")
                  (3 "h3")
                  (4 "h4")
                  (5 "h5")
                  (6 "h6")
                  (t (cond ((< 6 level) (error "Exceeded the maximun section heading depth of 6."))
                           ((< level 1) (error "SECTION-HEADING called outside of WITH-SECTION-HEADING."))))))
  (%with-environment (command :stream stream :fresh-line t)
		     (%write-division-arguments stream alignment class id title style language direction events)
    (etypecase heading
      (string (write-string heading stream))
      (function (funcall heading stream)))))

(define-macro with-section-heading ((heading &key class id title style language direction events
					     (alignment nil alignment-supplied-p) (level '*section-level*)
                                             (stream '*output-stream*)) &body body)
  "Excutes BODY within a section heading of depth LEVEL.
HEADING can be a string or a function called with (STREAM).

ALIGNMENT (deprecated) can be any of :LEFT, :CENTER, RIGHT, or :JUSTIFY.

CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or
:RIGHT-TO-LEFT.

EVENTS can be any of the intrinsic events: :KEY-DOWN, :KEY-PRESS, :KEY-UP, :MOUSE-CLICK, 
:MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT, :MOUSE-OVER, :MOUSE-UP."
  `(with-deprecation-checking (with-section-heading :argument alignment :condition (and *strict-dtd* ,alignment)
                                :compile-condition ,alignment-supplied-p)
     ,(if body
          `(progn
             (section-heading ,heading ,level ,stream ,alignment ,class ,id ,title ,style ,language ,direction ,events)
             (let ((*section-level* (the fixnum (1+ ,level))))
               ,@body))
        `(section-heading ,heading ,level ,stream ,alignment ,class ,id ,title ,style ,language ,direction ,events))))

(define-macro %with-division ((&key inline-p class id title style language direction events 
				   (alignment nil alignment-supplied-p) (stream '*output-stream*))
			     &body body)
  `(with-deprecation-checking (with-division :argument alignment :compile-condition ,alignment-supplied-p)
     (%with-environment 
       (,(case inline-p
	   ((t) "span")
	   ((nil) "div")
	   (t `(if ,inline-p "span" "div")))
	:stream ,stream)
       (%write-division-arguments ,stream ,alignment ,class ,id ,title ,style ,language ,direction ,events)
       ,@body)))

(define-macro with-division ((&rest args) &body body)
  #-(or ecl sbcl scl)
  (declare (#+Genera scl:arglist 
            #+LispWorks hcl:lambda-list
            #+MCL ccl:arglist
            #-(or Genera LispWorks MCL) arglist
            (&key inline-p class id title style language direction events alignment stream) &body body))
  "Establishes a block-level division or inline span environment according to INLINE-P.

ALIGNMENT (deprecated) can be any of :LEFT, :CENTER, RIGHT, or :JUSTIFY.

CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either
:LEFT-TO-RIGHT or :RIGHT-TO-LEFT.

EVENTS can be any of the intrinsic events: :KEY-DOWN, :KEY-PRESS, :KEY-UP, :MOUSE-CLICK,
:MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT, :MOUSE-OVER, :MOUSE-UP."
  (case (car args)
    ((:left :right :center) ;backward compatible for html3.2 version
     (destructuring-bind (alignment &key (stream '*output-stream*)) args
       `(%with-division (:alignment ,alignment :stream ,stream)
          ,@body)))
    (t `(%with-division (,@args)
          ,@body))))

(define-macro with-centering ((&key (stream '*output-stream*)) &body body)
  "Centers the contents of BODY. (deprecated)"
  `(%with-division (:alignment :center :stream ,stream)
     ,@body))

(define-macro with-paragraph ((&key class id title style language direction events 
				    (fresh-line t) (alignment nil alignment-supplied-p) (stream '*output-stream*))
			      &body body)
  "Establishes a paragraph environment.

ALIGNMENT (deprecated) can be any of :LEFT, :CENTER, RIGHT, or :JUSTIFY.

CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or
:RIGHT-TO-LEFT.

EVENTS can be any of the intrinsic events: :KEY-DOWN, :KEY-PRESS, :KEY-UP, :MOUSE-CLICK,
:MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT, :MOUSE-OVER, :MOUSE-UP."
  `(with-deprecation-checking (with-paragraph :argument alignment :compile-condition ,alignment-supplied-p)
     (%with-environment 
       ("p" :stream ,stream :fresh-line ,fresh-line)
       (%write-division-arguments ,stream ,alignment ,class ,id ,title ,style ,language ,direction ,events)
       ,@body)))

(defun %write-text-block-style-arguments (stream paragraph-style cite class id title style language direction events)
  (when (and cite (member paragraph-style '(:quotation)))
    (%write-command-key-arg stream "cite" (maybe-coerce-uri cite)))
  (%%write-standard-arguments stream id class language direction title style events))

(defun block-style-command (style)
  (case style
    (:quotation "blockquote")
    (:address "address")
    (:citation "cite")
    (:code "code")
    (t (error "Unknown paragraph style, ~A." style))))

(define-macro with-text-block-style ((block-style &key cite class id title style language direction events
						  (fresh-line t) (stream '*output-stream*)) &body body)
 "Renders the enclosed text block as BLOCK-STYLE, which can be any of: :QUOTATION, :ADDRESS, :CITATION, :CODE.
 
CITE is a URI designating the source of the block (only available when BLOCK-STYLE is :QUOTATION). 

CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or
:RIGHT-TO-LEFT.

EVENTS can be any of the intrinsic events: :KEY-DOWN, :KEY-PRESS, :KEY-UP, :MOUSE-CLICK,
:MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT, :MOUSE-OVER, :MOUSE-UP."
  (typecase block-style 
    (keyword
      `(%with-environment
	 (,(block-style-command block-style) :stream ,stream :fresh-line ,fresh-line)
	 ,(case block-style
	    (:quotation
	      `(%write-text-block-style-arguments
		 ,stream ,block-style ,cite ,class ,id ,title ,style ,language ,direction ,events))
	    (t `(%write-standard-arguments ,stream ,id ,class ,language ,direction ,title ,style ,events)))
	 ,@body))
    (t `(let ((block-style ,block-style))
	  (%with-environment
	    ((block-style-command block-style) :stream ,stream :fresh-line ,fresh-line)
	    (%write-text-block-style-arguments
	      ,stream block-style ,cite ,class ,id ,title ,style ,language ,direction ,events)
	    ,@body)))))

(define-macro with-paragraph-style ((paragraph-style &key alignment cite class id title style language direction events
						     (fresh-line t) (stream '*output-stream*)) &body body)
  "Renders the enclosed text block as PARAGRAPH-STYLE, which can be any of: :QUOTATION, :ADDRESS, :CITATION, :CODE.

ALIGNMENT (deprecated) can be any of :LEFT, :CENTER, RIGHT, or :JUSTIFY.

CITE is a URI designating the source of the block (only available when BLOCK-STYLE is :QUOTATION). 

CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or
:RIGHT-TO-LEFT.

EVENTS can be any of the intrinsic events: :KEY-DOWN, :KEY-PRESS, :KEY-UP, :MOUSE-CLICK,
:MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT, :MOUSE-OVER, :MOUSE-UP."
  `(with-paragraph (:alignment ,alignment :fresh-line ,fresh-line :stream ,stream)
     (with-text-block-style (,paragraph-style :cite ,cite :class ,class :id ,id :title ,title
			     :style ,style :language ,language :direction ,direction :events ,events
			     :fresh-line ,fresh-line :stream ,stream)
       ,@body)))

;;;------------------------------------------------------------------- 
;;;
;;; FONTS
;;; 

(define declare-default-font (&key (size 3) color class id title style language direction (stream *output-stream*))
  "Declares the base font size from which all relative font size changes are determined. (deprecated)
The default size is 3."
  (declaim (inline %write-make-font-args))
  (with-deprecation-checking (with-font)
    (%issue-command ("basefont" stream)
      (cond-every
       (size
        (check-type size integer)
        (unless (< 0 size 8)
          (error "SIZE, ~S, is not an integer from 1 through 7." size))
        (%write-command-key-arg stream "SIZE" size))
       (color
        (%write-command-key-arg stream "COLOR" (color-mapping color t))))
      (%%write-basic-arguments stream id class language direction title style))))

(defun %write-make-font-args (stream size relative-p color class id language direction title style)
  (with-deprecation-checking (with-font)
    (cond-every
     (size
      (check-type size integer)
      (unless (if relative-p (< -8 size 8) (< 0 size 8))
        (error "SIZE, ~S, is not an integer from 1 through 7." size))
      (cond (relative-p
             (check-type size integer)
             (if (plusp size)
                 (fast-format stream " size=+~D" size)
               (fast-format stream " size=-~D" (- size))))
            (t (%write-integer-argument stream "size" size))))
     (color
      (%write-command-key-arg stream " color" (color-mapping color t))))
    (%%write-basic-arguments stream id class language direction title style)))

(define-macro with-font ((&key (size 3) relative-size-p color 
			       class id title style language direction
			       (stream '*output-stream*)) &body body)
  "Declares the current font size to be SIZE and color to COLOR within BODY. (deprecated)
SIZE is an absolute size denoted by an integer from 1 to 7, or a relative size.
When RELATIVE-SIZE-P is non-null, SIZE is interpreted as a relative size. Negative
integers indicate reduction in font size whereas positive integers denote an increase.

  CLASS is the class for the element
  ID is an element identifier.
  TITLE is a string used as an element title.
  STYLE specifies inline parameters to use in the default style sheet language.
  LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
  DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT."

  `(%with-environment ("font" :fresh-line nil :stream ,stream)
       (%write-make-font-args ,stream ,size ,relative-size-p ,color ,class ,id ,language ,direction ,title ,style)
     ,@body))

;;;------------------------------------------------------------------- 
;;;
;;; EMPHASIS
;;; 

(eval-when (:load-toplevel :execute :compile-toplevel)

(defun emphasis-command (emphasis)
  (case emphasis
    (:emphasis "em")
    (:strong "strong")
    (:address "address")
    (:quotation "blockquote")
    (:quote "q")
    (:definition "dfn")
    (:citation "cite")
    (:keyboard "kbd")
    (:variable "var")
    (:code "code")
    (:sample "samp")
    (:abbreviation "abbr")
    (:acronym "acronym")
    (t (error "Unknown type of emphasis, ~S." emphasis))))

(defun %write-emphasis-arguments (stream emphasis cite class id title style language direction events)
  (when (and cite (member emphasis '(:quotation :quote)))
    (%write-command-key-arg stream "cite" (maybe-coerce-uri cite)))
  (%%write-standard-arguments stream id class language direction title style events))

(define-macro with-emphasis ((emphasis &key cite class id title style language direction events
				       (stream '*output-stream*)) &body body)
  "Renders output within BODY on STREAM according to EMPHASIS. 

EMPHASIS can be any of:

     :ABBREVIATION -- Abbreviation text.
     :ACRONYM      -- Acronym text.
     :ADDRESS      -- Address block style.
     :CITATION     -- Used for citations or references to other sources.
     :CODE         -- Used for extracts from program code. 
     :DEFINITION   -- Defining instance of the enclosed term.
     :EMPHASIS     -- Basic emphasis typically rendered in an italic font.
     :KEYBOARD     -- Used for text to be typed by the user. 
     :QUOTATION    -- Block quotation.
     :QUOTE        -- Surrounding quoation marks. 
     :SAMPLE       -- Used for sample output from programs, and scripts etc. 
     :STRONG       -- Strong emphasis typically rendered in a bold font.
     :VARIABLE     -- Used for variables or arguments to commands.

CITE is a URI designating the source of the block (only available when BLOCK-STYLE is :QUOTATION). 

CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or
:RIGHT-TO-LEFT.

EVENTS can be any of the intrinsic events: :KEY-DOWN, :KEY-PRESS, :KEY-UP, :MOUSE-CLICK,
:MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT, :MOUSE-OVER, :MOUSE-UP."
  (typecase emphasis 
    (keyword
      `(%with-environment
	 (,(emphasis-command emphasis) :stream ,stream :fresh-line nil)
	 ,(case emphasis
	    ((:quotation :quote)
	     `(%write-emphasis-arguments
		,stream ,emphasis ,cite ,class ,id ,title ,style ,language ,direction ,events))
	    (t `(%write-standard-arguments ,stream ,id ,class ,language ,direction ,title ,style ,events)))
	 ,@body))
    (t `(let ((emphasis ,emphasis))
	  (%with-environment
	    ((emphasis-command emphasis) :stream ,stream :fresh-line nil)
	    (%write-emphasis-arguments
	      ,stream emphasis ,cite ,class ,id ,title ,style ,language ,direction ,events)
	    ,@body)))))

) ;close eval-when

(eval-when (:load-toplevel :execute :compile-toplevel)

(defun rendition-command (rendition)
  (with-deprecation-checking (with-rendition :condition (and *strict-dtd* (member rendition '(:subscript :underline))))
    (case rendition
      (:bold "b")
      (:italic "i") 
      (:large "big")
      (:small "small")
      (:strike "s")
      (:subscript "sub")
      (:superscript "sup")
      (:teletype "tt")
      (:underline "u")
      (t (error "Unknown type of rendition, ~S." rendition)))))

(define-macro with-rendition ((rendition &key class id title style language direction events 
					 (stream '*output-stream*)) &body body)
  "Text output within BODY on STREAM is rendered according to RENDITION.

RENDITION can be any of:

     :ITALIC         -- Italic text style.
     :BOLD           -- Bold text style.
     :LARGE          -- Places text in a large font.
     :SMALL          -- Places text in a small font.
     :STRIKE         -- Strike-through text style. (Deprecated)
     :SUBSCRIPT      -- Places text in subscript style. 
     :SUPERSCRIPT    -- Places text in superscript style.
     :TELETYPE       -- Teletype or monospaced text.
     :UNDERLINE      -- Underlined text style. (Deprecated)

CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or
:RIGHT-TO-LEFT.

EVENTS can be any of the intrinsic events: :KEY-DOWN, :KEY-PRESS, :KEY-UP, :MOUSE-CLICK,
:MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT, :MOUSE-OVER, :MOUSE-UP."
  `(%with-environment
     (,(typecase rendition
	 (keyword (rendition-command rendition))
	 (t `(rendition-command ,rendition)))
      :stream ,stream :fresh-line nil)
     (%write-standard-arguments ,stream ,id ,class ,language ,direction ,title ,style ,events)
     ,@body))

) ;close eval-when

;;;------------------------------------------------------------------- 
;;;
;;; LITERAL ENVIRONMENTS
;;;

(defun %write-verbatim-text-arguments (stream width class id title style language direction events)
  (when width
    (with-deprecation-checking (with-verbatim-text :argument width)
      (%write-command-key-arg stream "width" width)))
  (%%write-standard-arguments stream id class language direction title style events))

(define-macro with-verbatim-text ((&key class id title style language direction events
					width (fresh-line t) (stream '*output-stream*)) &body body)
  "Formats text within BODY in a fixed width font exactly as printed,
as might be used for computer output or plain text files.

CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or
:RIGHT-TO-LEFT.

EVENTS can be any of the intrinsic events: :KEY-DOWN, :KEY-PRESS, :KEY-UP, :MOUSE-CLICK,
:MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT, :MOUSE-OVER, :MOUSE-UP.

WIDTH (deprecated) controls how wide the preformated text.  User agents support widths of 40, 80
(the default), 132, with upward rounding.

Character styles can be controlled via the WITH-RENDITION macro.

Line and paragraph breaks functions as CR. ASCII tab is interpreted as spaces in multiples of 8, but
its use is not recommended"
  `(%with-environment
       ("pre" :fresh-line ,fresh-line :stream ,stream)
       (%write-verbatim-text-arguments ,stream ,width ,class ,id ,title ,style ,language ,direction ,events)
     ,@body))

;;;------------------------------------------------------------------- 
;;;
;;; LINES
;;;

(define horizontal-line (&key class id title style language direction events fresh-line (stream *output-stream*)
			      size width alignment (shade nil shade-arg-supplied-p))
  "Writes a horizontal line across the output on STREAM.

CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or
:RIGHT-TO-LEFT.

EVENTS can be any of the intrinsic events: :KEY-DOWN, :KEY-PRESS, :KEY-UP, :MOUSE-CLICK,
:MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT, :MOUSE-OVER, :MOUSE-UP.

These arguments are deprecated. Use style sheets instead.

SIZE (deprecated) is an integer indicating how thick the line should be.

WIDTH (deprecated) is an integer specifying the absolute length of the line in pixels or a float
between zero and one indicating the percent of the window with to occupy.
ALIGNMENT (deprecated) is one of :LEFT, :RIGHT or :CENTER.
SHADE (deprecated) turns on shading."
  (when fresh-line (fresh-line stream))
  (%issue-command ("hr" stream)
    (cond-every
     (size 
      (with-deprecation-checking (horizontal-line :argument size)
        (%write-command-key-arg stream "size" size)))
     (width
      (with-deprecation-checking (horizontal-line :argument width)
        (%write-width-argument width stream)))
     (alignment
      (with-deprecation-checking (horizontal-line :argument alignment)
        (unless (member alignment ns1.1::*line-alignments*)
          (error "~S is not one of the known alignments, ~S" alignment ns1.1::*line-alignments*))
        (%write-command-key-arg stream "align" alignment)))
     ((and shade-arg-supplied-p (not shade))
      (with-deprecation-checking (horizontal-line :argument shade :value shade)
        (%write-command-key-arg stream "noshade"))))
    (%%write-standard-arguments stream id class language direction title style events)))

(define break-line (&key clear class id title style (stream *output-stream*))
  "Issues a line break on STREAM.
CLEAR (deprecated) can be any of:  
     :LEFT  -- move down lines until the left margin is clear of images.
     :RIGHT -- move down lines until the right margin is clear of images.
     :ALL   -- move down lines until the right margin is clear of images.
CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language."
  (%issue-command ("br" stream)
    (cond-every
      (clear
	(with-deprecation-checking (break-line :argument clear)
	  (unless (member clear ns1.1::*break-line-clear-options*)
	    (error "~S is not one of the known options, ~S." clear ns1.1::*break-line-clear-options*)))
	(%write-command-key-arg stream "clear" clear))
      (id (%write-id-argument stream id))
      (class (%write-class-argument stream class))
      (title
	(%write-title-argument stream title))
      (style (%write-style-argument stream style)))))

;;;------------------------------------------------------------------- 
;;;
;;; ENUMERATION 
;;; 

(defun enumerate-itemized-item (stream continuation icon-url type id class language direction title style events)
  (declare (ignore icon-url type))
  (%with-environment ("li" :stream stream :fresh-line t)
      (%%write-standard-arguments stream id class language direction title style events)
    (funcall continuation stream)))

(defun enumerate-enumerated-item (stream continuation icon-url type id class language direction title style events)
  (declare (ignore icon-url type))
  (%with-environment ("li" :stream stream :fresh-line t)
      (%%write-standard-arguments stream id class language direction title style events)
    (funcall continuation stream)))

(defun enumerate-normal-item (stream continuation icon-url head id class language direction title style events)
  (declare (ignore head))
  (%with-environment ("li" :stream stream :fresh-line t)
      (%%write-standard-arguments stream id class language direction title style events)
    (when icon-url
      (image icon-url "o" :stream stream)
      (write-char #\space stream)) 
    (funcall continuation stream)))

;; Does not provide standard html 4 parameters for DD -- 8/25/05 JCMa
(defun enumerate-definition-item (stream continuation icon-url head id class language direction title style events)
  (flet ((write-dd (stream)
           (issue-command "dd" stream nil)
           (write-char #\space stream)))
    (declare (inline write-dd))
    (%with-environment ("dt" :stream stream :fresh-line t)
        (%%write-standard-arguments stream id class language direction title style events)
      (when icon-url
	(image icon-url "o" :stream stream)
	(write-char #\space stream))
      (etypecase head
	(null nil)
	(string
         (write-string head stream)
         (write-dd stream))
	(cons
         (dolist (item head)
           (write item :stream stream))
         (write-dd stream))
	(function
         (funcall head stream)
         (write-dd stream)))
      (funcall continuation stream))))

;; Does not implement the VALUE parameter -- 8/25/05 JCMa
(define-macro enumerating-item ((stream &key icon-url head type id class language direction title style events) &body body)
  "Enumerates an item on STREAM according to the enclosing enumeration style.
BODY generates the body of the item whereas icon-url head type control the item's header.

TYPE (deprecated) can be provided for the styles :ENUMERATE and :ITEMIZE to override the default
bullet given by the enclosing WITH-ENUMERATION.  For the style ENUMERATE, TYPE can be any of
:CAPITAL-LETTERS, :SMALL-LETTERS, :LARGE-ROMAN, :SMALL-ROMAN, or :ARABIC (the default).  For the
style :ITEMIZE, TYPE can be any of :SOLID-DISC, :CIRCLE, or :SQUARE.

HEAD specifies the heading for an item in the :DEFINITION style.  Head can be a string, a list of
strings or a function called on STREAM.

ICON-URL is available for the styles :DEFINITION, :DIRECTORY (deprecated), and :menu (deprecated).
when provided, an image is emitted at the start of the item using the url supplied as the value of
icon-url.

CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.
LANGUAGE is the two-digit language code for the displayed content (see rfc 1766)
DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or
:RIGHT-TO-LEFT.

EVENTS can be any of the intrinsic events: :KEY-DOWN, :KEY-PRESS, :KEY-UP, :MOUSE-CLICK,
:MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT, :MOUSE-OVER, :MOUSE-UP."
  `(flet ((continuation (,stream) ,@body))
     (declare (dynamic-extent #'continuation))
     (funcall *enumeration-function* ,stream #'continuation ,icon-url ,(or head type)
	      ,id ,class ,language ,direction ,title ,style ,events)))

(defun %enumeration-style-parameters (style type)
  (declare (values function tag type))
  (case style
    (:itemize
      (values #'enumerate-itemized-item "ul" (html3.2::%get-bullet-style type html3.2::*itemize-bullet-styles*)))
    (:enumerate
      (values #'enumerate-enumerated-item "ol" (html3.2::%get-bullet-style type html3.2::*enumerate-bullet-styles*)))
    (:definition
      (values #'enumerate-definition-item "dl"))
    (:directory
      (with-deprecation-checking (with-enumeration :argument style :value style)
	(values #'enumerate-normal-item "dir")))
    ((:menu :plain)
     (with-deprecation-checking (with-enumeration :argument style :value style)
       (values #'enumerate-normal-item "menu")))
    (t (error "Unknown enumeration style, ~A." style)))) 

(define-macro with-enumeration ((stream enumeration-style &key compact type start 
					id class language direction title style events) &body body)
  "Establishes an enumeration environment using the style, STYLE, on STREAM. 

Within this environment, each item is emitted by generation code executed with the ENUMERATING-ITEM
macro.  ENUMERATION-STYLE can be :DEFINITION, :DIRECTORY (deprecated), :ENUMERATE :ITEMIZE :MENU
(deprecated).

You must use the ENUMERATING-ITEM from the same package for reliable results.

TYPE (deprecated) allows the default styles of enumeration to be overridden for some styles. For the
style :ENUMERATE, TYPE can be any of: :CAPITAL-LETTERS, :SMALL-LETTERS, :LARGE-ROMAN, :SMALL-ROMAN,
or :ARABIC (the default). For the style :ITEMIZE, TYPE can be any of :SOLID-DISC, :CIRCLE, :SQUARE

COMPACT (deprecated) advises the client to render lists in a more compact style. 

For the :ENUMERATE style, START (deprecated) can cause the enumeration to begin at a number other
than the default 1.

CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or
:RIGHT-TO-LEFT.

EVENTS can be any of the intrinsic events: :KEY-DOWN, :KEY-PRESS, :KEY-UP, :MOUSE-CLICK,
:MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT, :MOUSE-OVER, :MOUSE-UP."
  (flet ((enumeration-arguments (compact start type)
           (let ((args nil))
             (cond-every
              (compact
               (push `(,compact
                       (with-deprecation-checking (with-enumeration :argument compact)
                         (%write-command-key-arg ,stream "compact")))
                     args))
              (type
               (push `(,type
                       (with-deprecation-checking (with-enumeration :argument type)
                         (%write-command-key-arg ,stream "type" ,type)))
                     args))
              (start
               (push `(,start
                       (with-deprecation-checking (with-enumeration :argument start)
                         (%write-integer-argument ,stream "start" ,start)))
                     args)))
             (when args 
	       `(cond-every ,.args)))))
    `(multiple-value-bind (*enumeration-function* tag enumeration-type)
         (%enumeration-style-parameters ,enumeration-style ,type)
       enumeration-type                         ;no compiler warning
       (%with-environment (tag :fresh-line t :stream ,stream)
           (progn
             ,(when (or id class language direction title style events)
                `(%write-standard-arguments
                  ,stream ,id ,class ,language ,direction ,title ,style ,events))
             ,(enumeration-arguments compact start type))
         ,@body))))

(define enumerate-item-list (item-list &key (enumeration-style :itemize) type compact 
				       id class language direction title style events (stream *output-stream*))
  "Enumerates the elements of ITEM-LIST in STYLE on STREAM."
  (with-enumeration (stream enumeration-style :compact compact :type type 
			    :id id :class class :language language :direction direction :title title :style style
			    :events events)
    (dolist (item item-list)
      (enumerating-item (stream)
        (write item :stream stream :escape nil)))))

;;;------------------------------------------------------------------- 
;;;
;;; CLIENT-SIDE IMAGE MAPS 
;;;

(define-macro with-client-image-map ((name &key (stream '*output-stream*)) &body body)
  "Establishes a client-side image map environment named, NAME, on STREAM.

Use CLIENT-IMAGE-AREA to specify image area and mappings to URLs.  Whenever successive
calls to CLIENT-IMAGE-AREA declare regions with overlapping coordinates, the first one
takes precedence.  Regions not covered by an area are interpreted as dead areas. For
accessibility, image maps must provide alternate text for each call to
client-image-area using the alternative-text argument.

The client-side image map specified in this way may be associated with elements
generated by IMAGE, WITH-OBJECT, or ACCEPT-INPUT via the CLIENT-SIDE-IMAGE-MAP
argument. Alternatively, client-side image map may be used without an associated image
for general navigation mechanisms.

The client-side image map model allows authors to:

     * Specify mouse-sensitive areas using calls to CLIENT-IMAGE-AREA.

     * Provide block-level content using calls to NOTE-ANCHOR that include the SHAPE
     and COORDINATES arguments.

Authors should use the block-level method to create more accessible documents.  When
both block-level and mouse-sensitive areas are intermixed, clients will ignore the
mouse-sensitive areas specified with CLIENT-IMAGE-AREA."
  `(%with-environment ("map" :stream ,stream)
       (if *xhtml-generation*
           (%write-id-argument stream ,name)
         (%write-name-argument ,stream ,name))
     ,@body))

(define-generic write-client-side-area (region stream &optional target))

(defmethod write-client-side-area ((region ns2.0::region) stream &optional target)
  (with-slots (bounding-shape destination name tab-index access-key
			      id class language direction title style events) region
    (let ((coordinates (ns2.0::client-side-area-coordinates bounding-shape))
          (keyword (ns2.0::client-side-area-shape bounding-shape)))
      (declare (dynamic-extent coordinates))
      (client-image-area keyword coordinates :reference destination :target target
			 :name name :tab-index tab-index :access-key access-key 
			 :id id :class class :language language :direction direction :title title :style style
			 :events events :stream stream))))

(define-generic write-client-side-image-map (image-map-or-url name &key target width height stream)
  (:documentation "Writes a client-side image map from a server side image map or a URL serving such a map."))

;; This should pass the TARGET argument to fully exploit client-image-area. -- JCMa 8/29/2005
(defmethod write-client-side-image-map ((image-map ns2.0::image-map-data) name &key target width height (stream *output-stream*) )
  (with-slots (url-default region-list tab-index-default access-key-default 
			   id-default class-default language-default direction-default title-default style-default
			   events-default) image-map
    (with-string-for-null-stream (stream)
      (with-client-image-map (name :stream stream)
        ;; enumerate the regions
        (dolist (region region-list)
          (write-client-side-area region stream target))
        ;; write the default area
        (when (and url-default width height)
          (let ((coordinates (list 0 0 (1- (the fixnum width)) (1- (the fixnum height)))))
            (declare (dynamic-extent))
            (client-image-area :rectangle coordinates :reference url-default :target target
			       :tab-index tab-index-default :access-key access-key-default 
			       :id id-default :class class-default :language language-default :direction direction-default
			       :title title-default :style style-default :events events-default :stream stream)))))))

(defmethod write-client-side-image-map ((url url:http-searchable-object) name &key target width height (stream *output-stream*))
  (write-client-side-image-map
   (url:search-database url) name
   :target target :width width :height height :stream stream ))

(defmethod write-client-side-image-map (thing name &key target width height stream)
  (declare (ignore name target width height stream))
  (error "No method is implemented for generating a client side image map from a ~S like ~S."
         (type-of thing) thing))

;; Does not handle the :default arg for shape attribute in HTML 4.01 -- JCMa 8/28/2005
(defun %im-get-shape-arg (shape coordinates)
  (let ((entry (assoc shape ns2.0::*client-side-image-map-shapes-alist*)))
    (cond (entry
	   (destructuring-bind (shape-arg &optional coordinate-num) (cdr entry)
	     (unless (or (eq :polygon shape)
			 (= (length coordinates) coordinate-num))
	       (error "Wrong number of coordinates: ~A requires ~D, but coordinates is ~S."
		      shape coordinate-num coordinates))
	     (unless (every #'integerp coordinates)
	       (error "Non-numeric coordinates provided in ~S." coordinates))
	     shape-arg))
	  (t (error "Unknown shape, ~S, for a client side area." shape)))))

(defun %im-get-coordinate-arg (coordinates)
  (loop for (idx . more) = coordinates then more
	while idx
	collect (write-to-string idx :base 10.) into result
	when more
	  collect "," into result
	finally (return (apply #'concatenate 'string result))))

(define client-image-area (shape coordinates &key reference alternative-text tab-index access-key name target
				 id class language direction title style events
				 (stream *output-stream*))
  "Asserts a hot region in a client-side image map on STREAM.
This should be used with the environment of a WITH-CLIENT-IMAGE-MAP.

SHAPE can be: 

     :RECTANGLE  -- accepts the COORDINATES (left top right bottom)
     :CIRCLE     -- accepts the COORDINATES (center-x center-y radius-x)
     :POLYGON    -- accepts COORDINATES as a series of pairs, (x1 y1 x2 y2 ...)

Coordinates use a zero origin and are in pixels of the image displayed.

REFERENCE is the URL associated with the area or null for dead areas.  This argument can be a string
or a URL objects. URL object are fully specified. Relative URLs can be provided as strings. They are
defaulted by the client according to the document (URL) in which the client-side image map is
specified.  When a base declaration is present in the that document (URL), it is used for defaulting
relative URLs.

TAB-INDEX is an integer denoting position in the tabbing order.

ACCESS-KEY is a single accessibility character from ISO 10646.

ALTERNATIVE-TEXT is the text to display when the image is not loaded.

NAME is the control name in form submission.

TARGET specified the name of a frame where a document is to be opened. The value is a string or a
special keyword:

     :BLANK  - Always load this link into a new, unnamed window
     :SELF   - Always load this link over yourself
     :PARENT - Always load this link over your parent (becomes self if your at the top)
     :TOP    - Always load this link at top level (becomes self if your at the top)

CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or
:RIGHT-TO-LEFT.

EVENTS can be any of the intrinsic events: :INPUT-DEFOCUS, :INPUT-FOCUS, :KEY-DOWN, :KEY-PRESS,
:KEY-UP, :MOUSE-CLICK, :MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT, :MOUSE-OVER,
:MOUSE-UP."
  (%issue-command ("area" stream :fresh-line t)
    (%write-command-key-arg stream "shape" (%im-get-shape-arg shape coordinates))
    (%write-command-key-arg stream "coords" (%im-get-coordinate-arg coordinates))
    (if reference
	(%write-command-key-arg stream "href" (maybe-coerce-uri reference))
	(%write-command-key-arg stream "nohref"))
    (cond-every
      (alternative-text
	(%write-command-key-arg stream "alt" alternative-text))
      (tab-index
       (%write-tab-index-argument stream tab-index))
      (access-key
	(check-type access-key #.http:*standard-character-type*)
	(%write-command-key-arg stream "accesskey" access-key))
      (name
	(%write-name-argument stream name))
      (target
	(%write-target-argument stream target)))
    (%%write-standard-arguments stream id class language direction title style events)))


;;;------------------------------------------------------------------- 
;;;
;;; DOCUMENT FRAMESETS, FRAMES AND INLINE FRAMES
;;;

(defun write-frameset-arguments (stream rows columns class id title style events)
  (flet ((make-frame-arg (spec)
           (flet ((get-elt-string (element)
                    (etypecase element
                      (keyword (ecase element (:wild "*")))
                      (cons
                        (destructuring-bind (type number) element
                          (ecase type
                            (:percentage
                              (unless (and (integerp number) (< 0 number 101))
                                (error "The percentage, ~A, was not an integer between 0 and 100." type))
                              (concatenate 'string (write-to-string number :base 10.) "%"))
                            (:constraint
                              (unless (and (integerp number) (< 0 number))
                                (error "The fraction, ~A, was not an integer greater than 0." type))
                              (cond ((< number 2) "*")
                                    (t (concatenate 'string (write-to-string number :base 10.) "*"))))
                            (:pixel
                              (check-type number integer)
                              (write-to-string number :base 10.))))))))
             (declare (inline get-elt-string))
             (let ((result (loop for entry = spec then (cdr entry)
                                 while entry
                                 collect (get-elt-string (first entry))
                                 when (cdr entry)
                                   collect ",")))
               (declare (dynamic-extent result))
               (apply #'concatenate 'string result)))))
    (with-dtd-version-checking (:frameset with-document-frameset)
      (cond-every
	(rows
	  (%write-command-key-arg stream "rows" (make-frame-arg rows)))
	(columns
	  (%write-command-key-arg stream "cols" (make-frame-arg columns)))
	(class (%write-class-argument stream class))
	(id (%write-id-argument stream id))
	(title
	  (%write-title-argument stream title))
	(style (%write-style-argument stream style))
	(events (%write-events-argument stream events))))))

(define-macro with-document-frameset ((&key rows columns class id title style events
					    (stream '*output-stream*)) &body body)
  "Asserts the contents of BODY are a frameset.  This MUST be called within

WITH-HTML-DOCUMENT where DECLARE-DTD-VERSION-P is :FRAMESET.  The HTML document may include a
preamble before the frameset but it MUST not include a document body, (eg via WITH-DOCUMENT-BODY).

Framesets allow the client to display multiple URLs that are simultaneously visible in different
window frames. A frameset document has no document BODY, and only document preamable markup may
appear before the frameset, or the frameset will be ignored by the client.

NOTE-DOCUMENT-FRAME is used to emit a frame specification in a cell of a frameset specification.

Framesets can be nested inside other framesets, in which case an entire frameset appears in the
space of an element of the superior frame.

Data may be shared among several frames by including it using WITH-OBJECT inside the document
preamble and naming it with an ID argument.

The dimensions of a frame are analogous to a table. It dimensions are implicit in the number of
elements in the specifications of ROWS and COLUMNS. Omission of an element is interpreted as :WILD.

ROWS and COLUMNS are lists of values characterized the dimension.  Each value can be:

    * (:PERCENTAGE integer) An integer between 0 and 100 denoting a percentage.

    * (:CONSTRAINT integer) An integer greater than 0 denoting denoting the
      fraction of relative space to use.

    * :WILD any remaining space.

    * (:PIXEL integer) An integer denoting fixed size in pixels. This should be
      used only in rare circumstances because screen sizes on clients vary so
      much.

CLASS is the class for the element
ID is the identifier for the element.
STYLE specifies inline parameters to use in the default style sheet language.

EVENTS can be any of the intrinsic events: :LOAD, :UNLOAD."
  `(%with-environment
     ("frameset" :stream ,stream)
     (write-frameset-arguments ,stream ,rows ,columns ,class ,id ,title ,style ,events)
     ,@body))

(define-macro without-frame-capability ((&key class id language direction title style events
					      (stream '*output-stream*)) &body body)
  "Emits alternate HTML within BODY for clients not supporting frames,
but is ignored by frame-capable clients. This may be used with the transitional DTD, as declared by
WITH-HTML-DOCUMENT.

CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or
:RIGHT-TO-LEFT.

EVENTS can be any of the intrinsic events: :KEY-DOWN, :KEY-PRESS, :KEY-UP, :MOUSE-CLICK,
:MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT, :MOUSE-OVER, :MOUSE-UP."
  `(with-dtd-version-checking ((:frameset :transitional) with-document-frameset)
     (%with-environment 
       ("noframes" :stream ,stream)
       (%write-standard-arguments ,stream ,id ,class ,language ,direction ,title ,style ,events))
     ,@body))

(defun %frame-write-default-alternate-text (stream reference title)
  (with-emphasis (:quotation :stream stream)
    (with-rendition (:italic :stream stream)
      (fast-format stream "View content which your client cannot display inline at ~I."
		   (note-anchor (or title "inline document") :reference reference :stream stream)))))

(define note-document-frame (&key name reference height width 
				  (frame-border t) (scrolling :auto) (resizable-p t)
				  margin-width margin-height description-uri alternative-text
				  class id title style (stream *output-stream*))
  "Notes a standard document frame on STREAM.

The frame loads its own URL independently of the containing HTML. It can be targeted by regular
frames using its NAME. It can resize itself dynamically in response to changes in the size of the
visible client area.  The function must be called within the scope of a WITH-DOCUMENT-FRAMESET.

NAME (deprecated in XHTML 1.0) is a string denoting the name for targetting output and MUST begin
with an alphabetic character. In XHTML, use ID for this purpose. Frames can be targeted by supplying
the frame NAME (or ID in XHTML) as the TARGET argument to NOTE-ANCHOR, DECLARE-LINE,
CLIENT-IMAGE-AREA, and WITH-FILLOUT-FORM.

REFERENCE is a URL or url-string.

HEIGHT is either the number of pixels or a fraction of the window.

WIDTH is either the number of pixels or fraction of the window.

FRAME-BORDER is a boolean argument that controls whether the frame has borders or not.

SCROLLING can be :AUTO, T, or NIL

RESIZABLE-P controls whether the user can change the size of the frame.

MARGIN-WIDTH is a size in pixels.

MARGIN-HEIGHT is a size in pixels.

TITLE is a string used as the frame title.

DESCRIPTION-URI is a URI that provides a long description of the frame, supplementing the short
description in TITLE

ALTERNATIVE-TEXT is a string or function to inform users of HTML clients earlier than version 4.0
where to find the inline URL.

CLASS is the class for the element
ID is the identifier for the element.
STYLE specifies inline parameters to use in the default style sheet language."
  (with-dtd-version-checking (:frameset note-document-frame)
    (%with-environment
        ("frame" :stream stream)
        (cond-every
         (name
          (with-deprecation-checking (note-document-frame :argument name :condition *xhtml-generation*
                                                          :format-string "Use the ID argument for this purpose.")
            (%write-name-argument stream name)))
         (reference
	  (%write-command-key-arg stream "src" (maybe-coerce-uri reference)))
         (width
	  (%write-integer-argument stream "width" width))
         (height
	  (%write-integer-argument  stream "height" height))
         ((not resizable-p)
          (%write-command-key-arg stream "noresize"))
         ((null frame-border)
          (%write-command-key-arg stream "frameborder" 0))
         (margin-height
	  (%write-integer-argument stream "marginheight" margin-height))
         (margin-width
	  (%write-integer-argument stream "marginwidth" margin-width))
         (scrolling
	  (%write-command-key-arg stream "scrolling" (ecase scrolling
						       (:auto "auto")
						       ((t) "yes")
						       ((nil) "no"))))
         (description-uri
	  (%write-command-key-arg stream "longdesc" (maybe-coerce-uri description-uri)))
         (id
          (%write-id-argument-handling-xhtml-backward-compatibly stream id))
         (class (%write-class-argument stream class))
         (title
	  (%write-title-argument stream title))
         (style (%write-style-argument stream style)))
      (etypecase alternative-text
	(null
         (%frame-write-default-alternate-text stream reference title))
	(string (write-string alternative-text stream))
	(function (funcall alternative-text stream))))))
    
(define note-inline-frame (&key name reference height width
				(scrolling :auto) (frame-border t)
				margin-width margin-height description-uri alternative-text
				class id title style alignment (stream *output-stream*))
  "Notes an inline frame on STREAM.

Inline frames allow a frame to be inserted within a block of text, much like
inserting an object using WITH-OBJECT. This may be used with the transitional DTD,
as declared by WITH-HTML-DOCUMENT.

The frame loads its own URL independently of the containing HTML. It can be targeted by regular
frames using its NAME. It can resize itself dynamically in response to changes in the size of the
visible client area.  The function must be called within the scope of a WITH-DOCUMENT-FRAMESET.

NAME (deprecated in XHTML 1.0) is a string denoting the name for targetting output and MUST begin
with an alphabetic character. Frames can be targeted by supplied the frame name as the TARGET
argument to NOTE-ANCHOR, DECLARE-LINE, CLIENT-IMAGE-AREA, and WITH-FILLOUT-FORM.

REFERENCE is a URL or url-string.
ALIGNMENT (deprecated) is one of :LEFT, :CENTER, :RIGHT, or :JUSTIFY.
HEIGHT is either the number of pixels or a fraction of the window.
WIDTH is either the number of pixels or fraction of the window.

SCROLLING can be :AUTO, T, or NIL
FRAME-BORDER is a boolean argument that controls whether the frame has
borders or not.
MARGIN-WIDTH is a size in pixels.
MARGIN-HEIGHT is a size in pixels.
TITLE is a string used as the frame title.

DESCRIPTION-URI is a URI that provides a long description of the frame,
supplementing the short description in TITLE

ALTERNATIVE-TEXT is a string or function to inform users of HTML clients earlier
than version 4.0 where to find the inline URL.

CLASS is the class for the element
ID is the identifier for the element.
STYLE specifies inline parameters to use in the default style sheet language."
  (with-dtd-version-checking (:transitional note-inline-frame)
    (%with-environment
        ("iframe" :stream stream)
        (cond-every
         (name
          (with-deprecation-checking (note-inline-frame :argument name :condition *xhtml-generation*
                                                        :format-string "Use the ID argument for this purpose.")
            (%write-name-argument stream name)))
         (reference
	  (%write-command-key-arg stream "src" (maybe-coerce-uri reference)))
         (width
          (%write-integer-argument stream "width" width))
         (height
	  (%write-integer-argument stream "height" height))
         (alignment
	  (with-deprecation-checking (note-inline-frame :argument alignment)
	    (%write-alignment-argument stream alignment)))
         ((null frame-border)
          (%write-command-key-arg stream "frameborder" 0))
         (margin-height
	  (%write-integer-argument stream "marginheight" margin-height))
         (margin-width
	  (%write-integer-argument stream "marginwidth" margin-width))
         (scrolling
	  (%write-command-key-arg stream "scrolling" (ecase scrolling
						       (:auto "auto")
						       ((t) "yes")
						       ((nil) "no"))))
         (description-uri
	  (%write-command-key-arg stream "longdesc" (maybe-coerce-uri description-uri)))
         (id
          (%write-id-argument-handling-xhtml-backward-compatibly stream id))
         (class (%write-class-argument stream class))
         (title
	  (%write-title-argument stream title))
         (style (%write-style-argument stream style)))
      (etypecase alternative-text
	(null
         (%frame-write-default-alternate-text stream reference title))
	(string (write-string alternative-text stream))
	(function (funcall alternative-text stream))))))

;;;------------------------------------------------------------------- 
;;;
;;;  TABLES
;;; 

(defconstant *table-alignment-values* '(:left :center :right))

(defun table-alignment-value (alignment)
  (unless (member alignment *table-alignment-values*)
    (error "Unknown alignment, ~S, for a table." alignment))
  (symbol-name alignment))

(defconstant *table-cell-alignment-values* '(:left :center :right :justify :char))

(defun table-cell-alignment-value (alignment)
  (unless (member alignment *table-cell-alignment-values*)
    (error "~S is not one of the possible table-cell alignments, ~S" alignment *table-cell-alignment-values*))
  (symbol-name alignment))

(defconstant *table-cell-vertical-alignment-values* '(:top :middle :bottom :baseline))

(defun table-cell-vertical-alignment-value (vertical-alignment)
  (unless (member vertical-alignment *table-cell-vertical-alignment-values*)
    (error "~S is not one of the possible table-cell vertical-alignments, ~S" 
	   vertical-alignment *table-cell-vertical-alignment-values*))
  (symbol-name vertical-alignment))

(defconstant *table-rules-values* '(:none :groups :rows :cols :all))

(defun table-rules-value (rules)
  (unless (member rules *table-rules-values*)
    (error "Unknown rules, ~S, for a table." rules))
  (symbol-name rules))

(defconstant *table-frame-values* '(:void :above :below :hsides :lhs :rhs :vsides :box :border))

(defun table-frame-value (frame)
  (unless (member frame *table-frame-values*)
    (error "Unknown frame, ~S, for a table." frame))
  (symbol-name frame))

(defconstant *caption-alignment-values* '(:top :bottom :left :right))

(defun caption-alignment-value (alignment)
  (unless (member alignment *caption-alignment-values*)
    (error "Unknown alignment, ~S, for a caption." alignment))
  (symbol-name alignment))

(defconstant *table-cell-scope-values* '(:row :col :rowgroup :colgroup))

(defun table-cell-scope-value (cell-scope)
  (unless (member cell-scope *table-cell-scope-values*)
    (error "Unknown cell-scope, ~S, for a table." cell-scope))
  (symbol-name cell-scope))

(defun %write-table-cell-arguments (stream header-p vertical-alignment horizontal-alignment 
                                           horizontal-alignment-char horizontal-alignment-char-offset 
                                           column-span row-span height width background no-wrap
                                           abbreviation axis headers scope
                                           class id language direction title style events)
  (flet ((write-delimited-sequence (stream tag delimiter seq)
	   (flet ((writer (stream)
		    (loop for (hdr . more) = seq then more
			  do (write-string hdr stream)
			  while more
			  do (write-char delimiter stream))))
	     (declare (dynamic-extent #'writer))
	     (%write-command-key-arg stream tag #'writer))))
    (cond-every
     (horizontal-alignment
      (%write-command-key-arg stream "align"  (table-cell-alignment-value horizontal-alignment)))
     (horizontal-alignment-char
      (check-type horizontal-alignment-char #.*standard-character-type*)
      (%write-command-key-arg stream "char" horizontal-alignment-char))
     (horizontal-alignment-char-offset
      (check-type horizontal-alignment-char-offset integer)
      (%write-command-key-arg stream "charoff" horizontal-alignment-char-offset))
     (vertical-alignment
      (%write-command-key-arg stream "valign" (table-cell-vertical-alignment-value vertical-alignment)))
     (column-span
      (%write-integer-argument stream "colspan" column-span))
     (row-span
      (%write-integer-argument stream "rowspan" row-span))
     (height 
      (with-deprecation-checking (with-table-cell :argument height)
        (%write-command-key-arg stream "height" height)))
     (width
      (with-deprecation-checking (with-table-cell :argument width)
        (%write-command-key-arg stream "width" width)))
     (no-wrap
      (with-deprecation-checking (with-table-cell :argument no-wrap)
        (%write-command-key-arg stream "nowrap")))
     (background
      (with-deprecation-checking (with-table-cell :argument background)
        (%write-command-key-arg stream "bgcolor" (color-mapping background))))
     (abbreviation
      (check-type abbreviation string)
      (%write-command-key-arg stream "abbr" abbreviation))
     (axis
      (etypecase axis
        (cons
         (write-delimited-sequence stream "axis" #\, axis))
        ((or string function)
         (%write-command-key-arg stream "axis" axis)))
      (%write-command-key-arg stream "axis" axis))
     (scope
      (unless header-p
        (error "The SCOPE argument,~S, does not make sense for table data cells.~&It is intended for header cells." scope))
      (%write-command-key-arg stream "scope" (table-cell-scope-value scope)))
     (headers
      (when header-p
        (error "The HEADER argument,~S, does not make sense for table header cells.~&It is intended for data cells." headers))
      (etypecase headers
        (cons
         (write-delimited-sequence stream "headers" #\space headers))
        ((or string function)
         (%write-command-key-arg stream "headers" headers)))))
    (%%write-standard-arguments stream id class language direction title style events)))

(defun %write-table-row-arguments (stream vertical-alignment horizontal-alignment 
                                          horizontal-alignment-char horizontal-alignment-char-offset background 
                                          class id language direction title style events)
  (cond-every
   (horizontal-alignment
    (%write-command-key-arg stream "align"  (table-cell-alignment-value horizontal-alignment)))
   (horizontal-alignment-char
    (check-type horizontal-alignment-char #.*standard-character-type*)
    (%write-command-key-arg stream "char" horizontal-alignment-char))
   (horizontal-alignment-char-offset
    (check-type horizontal-alignment-char-offset integer)
    (%write-command-key-arg stream "charoff" horizontal-alignment-char-offset))
   (vertical-alignment
    (%write-command-key-arg stream "valign" (table-cell-vertical-alignment-value vertical-alignment)))
   (background
    (with-deprecation-checking (with-table :argument background)
      (%write-command-key-arg stream "bgcolor" (color-mapping background)))))
  (%%write-standard-arguments stream id class language direction title style events))

(defun %write-multi-length-argument (stream attribute value)
  (flet ((write-percentage-value (stream)
	   (declare (float value))
	   (fast-format stream "\"~D%\"" (floor (* value 100))))
	 (write-relative-value (stream)
	   (declare (rational value))
	   (fast-format stream "\"~D*\"" (numerator value))))
    (declare (dynamic-extent #'write-relative-value #'write-percentage-value))
    (etypecase value
      (integer
	(%write-command-key-arg stream attribute value))
      (rational
	(%write-command-key-arg stream attribute #'write-relative-value))
      (float
	(if (< 0 value 1)
	    (%write-command-key-arg stream attribute #'write-percentage-value)
	    (error "Table VALUE is ~S, which is not a float between 0 and 1 as required by HTML 4.01" value))))))

(defun %write-table-column-group-arguments (stream span width vertical-alignment horizontal-alignment 
						   horizontal-alignment-char horizontal-alignment-char-offset 
						   class id language direction title style events)
  (cond-every
   (span
    (%write-integer-argument stream "span" span))
   (width
    (typecase width
      (symbol
       (ecase width
         (:fit (%write-command-key-arg stream "width" "0*"))))
      (t (%write-multi-length-argument stream "width" width))))
   ((or vertical-alignment horizontal-alignment 
        horizontal-alignment-char horizontal-alignment-char-offset 
        class id language direction title style events)
    (%write-table-row-arguments stream vertical-alignment horizontal-alignment 
                                horizontal-alignment-char horizontal-alignment-char-offset nil 
                                class id language direction title style events))))

(defun %write-caption-arguments (stream alignment id class language direction title style events)
  (when alignment
    (with-deprecation-checking (with-caption :argument alignment)
      (%write-command-key-arg stream "align" (caption-alignment-value alignment))))
  (%write-standard-arguments stream id class language direction title style events))

(defun %write-table-arguments (stream width cell-spacing cell-padding border frame rules summary alignment background
				      id class language direction title style events)
  (flet ((write-width-argument (width stream)
	   (if (<= 0 width 1)
	       (flet ((write-val (stream)
			(declare (float width))
			(fast-format stream "\"~D%\"" (floor (* width 100)))))
		 (declare (dynamic-extent #'write-val))
		 (%write-command-key-arg stream "width" #'write-val))
             (error "Table WIDTH is ~S, which is not a float between 0 and 1 as required by HTML 4.01" width))))
    (declare (inline %write-width-argument))
    (cond-every
     (width (write-width-argument width stream))
     (cell-spacing
      (%write-command-key-arg stream "cellspacing" cell-spacing))
     (cell-padding
      (%write-command-key-arg stream "cellpadding" cell-padding))
     (border
      (%write-command-key-arg stream "border" (if (integerp border) border 1.)))
     (frame
      (%write-command-key-arg stream "frame" (table-frame-value frame)))
     (rules
      (%write-command-key-arg stream "rules" (table-rules-value rules)))
     (summary
      (check-type summary string)
      (%write-command-key-arg stream "summary" summary))
     (alignment
      (with-deprecation-checking (with-table :argument alignment)
        (%write-command-key-arg stream "align"  (table-alignment-value alignment))))
     (background
      (with-deprecation-checking (with-table :argument background)
        (%write-command-key-arg stream "bgcolor" (color-mapping background)))))
    (%%write-standard-arguments stream id class language direction title style events)))

(define-macro with-table-cell ((&key header-p horizontal-alignment vertical-alignment column-span row-span 
				     horizontal-alignment-char horizontal-alignment-char-offset
				     abbreviation axis scope headers height width background 
				     class id language direction title style events
				     break-lines-p (stream '*output-stream*))
			       &body body)
  "Asserts that the contents of BODY is a cell within a table environment
HEADER-P controls whether the cell is a data cell or a header cell.

HORIZONTAL-ALIGNMENT can be any of:

     :LEFT    - Flush left, left justify text.
     :CENTER  - Center, center justify text.
     :RIGHT   - Right flush, right justify text.
     :JUSTIFY - Double justify text.
     :CHAR    - Align text around a specific character, if supported by user agent.

VERTICAL-ALIGNMENT specifies vertical alignment for cells and can be any of:

     :TOP      - Flush with top.
     :MIDDLE   - Centered vertically.
     :BOTTOM   - Flush with bottom.
     :BASELINE - First line constrained to align vertically with first line of 
	         preceding cells in row.

COLUMN-SPAN is an integer that controls the number of columns a cell spans.  ROW-SPAN is an integer
that controls the number of columns a cell spans.

HORIZONTAL-ALIGNMENT-CHAR specifies a single character to act as an axis for horizontal alignment
HORIZONTAL-ALIGNMENT-CHAR-OFFSET specifies the offset to the first occurence of the alignment
character.

HEIGHT (deprecated) is an integer specifying the absolute length of the line in pixels.
WIDTH (deprecated) is an integer specifying the absolute length of the line in pixels.
BREAK-LINES-P (deprecated) prevents lines from being broken to fit the width of a cell.
BACKGROUND (deprecated) specifies the color for the background.

ABBREVIATION is a short string describing the cell's content in abbreviated form (useful for UA like
speech synthesizers).

AXIS is a list of string denoting conceptual categories to which the cell belongs.

The SCOPE and HEADERS paramter help non-visual user agents present
header information.

SCOPE Specifies the set of data cells for which the current header cell provides information. Values
can be:

     :ROW      - Rest of the cells in the current row.
     :COL      - Rest of the column that contains it.
     :ROWGROUP - Rest of the row group that contains it.
     :COLGROUP - Rest of the column group that contains it.

HEADERS a list of header cell IDs that provide information for the
current data cell.

CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or
:RIGHT-TO-LEFT.

EVENTS can be any of the intrinsic events: :KEY-DOWN, :KEY-PRESS, :KEY-UP, :MOUSE-CLICK,
:MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT, :MOUSE-OVER, :MOUSE-UP."
  `(let ((header-p ,header-p))
     (%with-environment
       (,(case header-p
	   ((t) "th")
	   ((nil) "td")
	   (t `(if header-p "th" "td")))
	:fresh-line nil :stream ,stream)
       (%write-table-cell-arguments ,stream header-p ,vertical-alignment ,horizontal-alignment 
					    ,horizontal-alignment-char ,horizontal-alignment-char-offset 
					    ,column-span ,row-span ,height ,width ,background ,break-lines-p
					    ,abbreviation ,axis ,headers ,scope
					    ,class ,id ,language ,direction ,title ,style ,events)
       ,@body)))

(define-macro with-table-row ((&key horizontal-alignment vertical-alignment 
				    horizontal-alignment-char horizontal-alignment-char-offset background 
				    class id language direction title style events (stream '*output-stream*))
			      &body body)
  "Asserts that the contents of BODY is a row in a table.

HORIZONTAL-ALIGNMENT can be any of:

     :LEFT    - Flush left, left justify text.
     :CENTER  - Center, center justify text.
     :RIGHT   - Right flush, right justify text.
     :JUSTIFY - Double justify text.
     :CHAR    - Align text around a specific character, if supported by user agent.

VERTICAL-ALIGNMENT specifies vertical alignment for cells and can be any of:

     :TOP      - Flush with top.
     :MIDDLE   - Centered vertically.
     :BOTTOM   - Flush with bottom.
     :BASELINE - First line constrained to align vertically with first line of 
	         preceding cells in row.

HORIZONTAL-ALIGNMENT-CHAR specifies a single character to act as an axis for horizonatl alignment
HORIZONTAL-ALIGNMENT-CHAR-OFFSET specifies the offset to the first occurence of the alignment
character.

BACKGROUND (deprecated) specifies the color for the background.

CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or
:RIGHT-TO-LEFT.

EVENTS can be any of the intrinsic events: :KEY-DOWN, :KEY-PRESS, :KEY-UP, :MOUSE-CLICK,
:MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT, :MOUSE-OVER, :MOUSE-UP."
  `(%with-environment
     ("tr" :fresh-line t :stream ,stream)
     (%write-table-row-arguments ,stream ,vertical-alignment ,horizontal-alignment 
					 ,horizontal-alignment-char ,horizontal-alignment-char-offset ,background 
					 ,class ,id ,language ,direction ,title ,style ,events) 
     ,@body))

(define-macro with-table-row-group ((group-type &key horizontal-alignment vertical-alignment 
				    horizontal-alignment-char horizontal-alignment-char-offset 
				    class id language direction title style events (stream '*output-stream*))
			      &body body)
  "Asserts that the contents of BODY is a grouping of rows in a table.

Table groups are used to help user agents display large tables, for example by maintain the header
and footer row in view while the body rows are scrolled.

GROUP-TYPE can be any of :HEADER, :FOOTER or :BODY.

Header and footer groups MUST appear before the table body group in generated HTML.  A table body
group MUST appear whenever either a header or footer group are present. Any table group MUST contain
at least one table row. All rows within any table group MUST contain the same number of columns.

HORIZONTAL-ALIGNMENT can be any of:

     :LEFT    - Flush left, left justify text.
     :CENTER  - Center, center justify text.
     :RIGHT   - Right flush, right justify text.
     :JUSTIFY - Double justify text.
     :CHAR    - Align text around a specific character, if supported by user agent.

VERTICAL-ALIGNMENT specifies vertical alignment for cells and can be any of:

     :TOP      - Flush with top.
     :MIDDLE   - Centered vertically.
     :BOTTOM   - Flush with bottom.
     :BASELINE - First line constrained to align vertically with first line of 
	         preceding cells in row.

HORIZONTAL-ALIGNMENT-CHAR specifies a single character to act as an axis for horizonatl alignment
HORIZONTAL-ALIGNMENT-CHAR-OFFSET specifies the offset to the first occurence of the alignment
character.

CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)

DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or
:RIGHT-TO-LEFT.

EVENTS can be any of the intrinsic events: :KEY-DOWN, :KEY-PRESS, :KEY-UP, :MOUSE-CLICK,
:MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT, :MOUSE-OVER, :MOUSE-UP."
  `(%with-environment
     (,(case group-type
	 (:header "thead")
	 (:footer "tfoot")
	 (:body "tbody")
	 (t `(ecase ,group-type
	       (:header "thead")
	       (:footer "tfoot")
	       (:body "tbody"))))
      :fresh-line t :stream ,stream)
     (%write-table-row-arguments ,stream ,vertical-alignment ,horizontal-alignment 
					 ,horizontal-alignment-char ,horizontal-alignment-char-offset nil 
					 ,class ,id ,language ,direction ,title ,style ,events) 
     ,@body))

(declaim (inline table-column))

(define table-column-group (&key span width horizontal-alignment vertical-alignment 
				 horizontal-alignment-char horizontal-alignment-char-offset 
				 class id language direction title style events (stream '*output-stream*))
  "Asserts the specific attributes of a column group within a WITH-TABLE-COLUMN-GROUP environment 

Use this TABLE-COLUMN to override default column attributes established
by WITH-TABLE-COLUMN-GROUP.

SPAN is an integer greater than zero that specifies the number of
columns in the group. When omitted, the column group defaults to a
single column.

WIDTH specifies the default width for every column in the group. If the
value is an integer, it is interpreted as the absolute width in pixels.
If the value is a float between 0 and 1, it is interpreted as percentage
of the table width. If the value is a rational, it is interpreted as
relative to the remaining table width, not already consumed by earlier
absolution column widths. The denominator MUST be the same for all such
relative column widths within a table. If the value is :FIT, the width
will be the minimum needed to hold the column's content. Note that the
:FIT option will prevent user agents from rendering the table
incrementally.

HORIZONTAL-ALIGNMENT can be any of:

     :LEFT    - Flush left, left justify text.
     :CENTER  - Center, center justify text.
     :RIGHT   - Right flush, right justify text.
     :JUSTIFY - Double justify text.
     :CHAR    - Align text around a specific character, if supported by user agent.

VERTICAL-ALIGNMENT specifies vertical alignment for cells and can be any of:

     :TOP      - Flush with top.
     :MIDDLE   - Centered vertically.
     :BOTTOM   - Flush with bottom.
     :BASELINE - First line constrained to align vertically with first line of 
                 preceding cells in row.

HORIZONTAL-ALIGNMENT-CHAR specifies a single character to act as an axis for
horizonatl alignment
HORIZONTAL-ALIGNMENT-CHAR-OFFSET specifies the offset to the first occurence
of the alignment character.

CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT."
  (%issue-command ("col" stream :fresh-line t)
    (%write-table-column-group-arguments stream span width vertical-alignment horizontal-alignment 
						   horizontal-alignment-char horizontal-alignment-char-offset 
						   class id language direction title style events)))

(define-macro with-table-column-group ((&key span width horizontal-alignment vertical-alignment 
					     horizontal-alignment-char horizontal-alignment-char-offset 
					     class id language direction title style events (stream '*output-stream*))
				       &body body)
  "Asserts that the contents of BODY is a grouping of rows in a table.

Column groups allow the creation of structural divisions within a table, which may be highlighted
with style sheets or HTML attributes like rules for the table.

WITH-TABLE-COLUMN-GROUP establishes an environment for specifying column group characteristics. It
provides the default attributes for all scoped columns. Use this TABLE-COLUMN-GROUP to override
default column attributes established by WITH-TABLE-COLUMN-GROUP.

The number of columns ina group may be specified in two mutually exclusive ways:

     * Specifies the number of columns with SPAN.
     * Use TABLE-COLUMN-GROUP to assert the number of columns.

SPAN is an integer greater than zero that specifies the number of columns in the group. When
omitted, the column group defaults to a single column.

WIDTH specifies the default width for every column in the group. If the value is an integer, it is
interpreted as the absolute width in pixels.  If the value is a float between 0 and 1, it is
interpreted as percentage of the table width. If the value is a rational, it is interpreted as
relative to the remaining table width, not already consumed by earlier absolution column widths. The
denominator MUST be the same for all such relative column widths within a table. If the value is
:FIT, the width will be the minimum needed to hold the column's content. Note that the :FIT option
will prevent user agents from rendering the table incrementally.

HORIZONTAL-ALIGNMENT can be any of:

     :LEFT    - Flush left, left justify text.
     :CENTER  - Center, center justify text.
     :RIGHT   - Right flush, right justify text.
     :JUSTIFY - Double justify text.
     :CHAR    - Align text around a specific character, if supported by user agent.

VERTICAL-ALIGNMENT specifies vertical alignment for cells and can be any of:

     :TOP      - Flush with top.
     :MIDDLE   - Centered vertically.
     :BOTTOM   - Flush with bottom.
     :BASELINE - First line constrained to align vertically with first line of 
                 preceding cells in row.

HORIZONTAL-ALIGNMENT-CHAR specifies a single character to act as an axis for horizonatl alignment
HORIZONTAL-ALIGNMENT-CHAR-OFFSET specifies the offset to the first occurence of the alignment
character.

CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or
:RIGHT-TO-LEFT.

EVENTS can be any of the intrinsic events: :KEY-DOWN, :KEY-PRESS, :KEY-UP, :MOUSE-CLICK,
:MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT, :MOUSE-OVER, :MOUSE-UP."
  `(%with-environment
     ("colgroup"
      :fresh-line t :stream ,stream)
     (%write-table-column-group-arguments ,stream ,span ,width ,vertical-alignment ,horizontal-alignment 
						   ,horizontal-alignment-char ,horizontal-alignment-char-offset 
						   ,class ,id ,language ,direction ,title ,style ,events) 
     ,@body))

(define-macro with-caption ((&key alignment id class language direction title style events (stream '*output-stream*))
			    &body body)
  "Asserts the contents of BODY is a caption within a table environment.
ALIGNMENT (deprecated) can be any of: :TOP, :BOTTOM, :LEFT, :RIGHT.
CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or
:RIGHT-TO-LEFT.

EVENTS can be any of the intrinsic events: :KEY-DOWN, :KEY-PRESS, :KEY-UP, :MOUSE-CLICK,
:MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT, :MOUSE-OVER, :MOUSE-UP."
  `(%with-environment
     ("caption" :stream ,stream)
     (%write-caption-arguments ,stream ,alignment ,id ,class ,language ,direction ,title ,style ,events)
     ,@body))

(define-macro with-table ((&key width cell-spacing cell-padding  border frame rules summary alignment background
				class id language direction title style events (stream '*output-stream*)) &body body)
  "Establishes a table environment with BODY within which WITH-TABLE-ROW and WITH-TABLE-CELL can be used.
A caption can be specified using the macro WITH-CAPTION.

WIDTH is a float between zero and one indicating the percent of the window with to occupy, or the
absolute number of pixels.

CELL-SPACING is an integer, defaulting to 2, that controls the space between cells.

CELL-PADDING is an integer, defaulting to 1 that controls the space between text contained in a cell
and the wall.

BORDER is NIL or an integer specifying the width in pixels of the frame around  a table.

FRAME specifies which sides of the frame surrounding the table are visiable and can be one of:

     :VOID   - No sides (default value)
     :ABOVE  - Top side only.
     :BELOW  - Bottom side only
     :HSIDES - Top and bottom sides only.
     :VSIDES - Right and left sides only.
     :LHS    - Left hand sode only.
     :RHS    - Right hand side only.
     :BOX    - All four sides.
     :BORDER - All four sides.

RULES specifies the grid lines separating cells within a table and can be one of:

     :NONE   - No rules (default value)
     :GROUPS - Rules appear between row and column groups.
     :ROWS   - Rules appear between rows only.
     :COLS   - Rules appear between columns only.
     :ALL    - Rules appear between all rows and columns.

SUMMARY provides a desciprion of the table's purpose and structure useful for non visual rendering
(eg, speech, braille).

ALIGNMENT (deprecated) specifies the position of the table with respect to the document. Possible
values are: :LEFT, :CENTER or :RIGHT.

BACKGROUND (deprecated) specifies the color for the background.

CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or
:RIGHT-TO-LEFT.

EVENTS can be any of the intrinsic events: :KEY-DOWN, :KEY-PRESS, :KEY-UP, :MOUSE-CLICK,
:MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT, :MOUSE-OVER, :MOUSE-UP."
  `(%with-environment ("table" :fresh-line t :stream ,stream)
       (%write-table-arguments ,stream ,width ,cell-spacing ,cell-padding ,border 
                               ,frame ,rules ,summary ,alignment ,background
                               ,id ,class ,language ,direction ,title ,style ,events) 
     ,@body))

;;;------------------------------------------------------------------- 
;;;
;;;  RANDOM
;;; 

(define-macro with-language-direction ((&key language direction (stream '*output-stream*)) &body body)
  "Overrides the default language direction within BODY.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT."
  `(%with-environment ("bdo" :fresh-line t :stream ,stream)
		      (%write-language-and-direction-arguments ,stream ,language ,direction) 
     ,@body))

(defun %write-editing-change-arguments (stream universal-time reason id class language direction title style events)
  (cond-every
   (universal-time
    (flet ((write-iso-datetime (stream)
             (http::write-iso-datime universal-time stream)))
      (declare (dynamic-extent #'write-iso-datetime))
      (%write-command-key-arg stream "datetime" #'write-iso-datetime)))
   (reason
    (%write-command-key-arg stream "cite" (maybe-coerce-uri reason))))
  (%%write-standard-arguments stream id class language direction title style events))

(define-macro with-editing-change ((change-type &key universal-time reason 
						class id language direction title style events 
						(stream '*output-stream*)) &body body)
  "Establishes an environment that flags the content generated with BODY as changed.

CHANGE-TYPE can be either :INSERTION or :DELETION.
UNIVERSAL-TIME is the universal time when the change occured.
REASON is a URI pointing to to a source document or message explaining why the change was made.

CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or
:RIGHT-TO-LEFT.

EVENTS can be any of the intrinsic events: :KEY-DOWN, :KEY-PRESS, :KEY-UP, :MOUSE-CLICK,
:MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT, :MOUSE-OVER, :MOUSE-UP."
  `(%with-environment (,(case change-type
			  (:insertion "ins")
			  (:deletion "del")
			  (t `(case ,change-type
				(:insertion "ins")
				(:deletion "del"))))
		       :fresh-line t :stream ,stream)
		      (%write-editing-change-arguments ,stream ,universal-time ,reason 
						       ,id ,class ,language ,direction ,title ,style ,events)
     ,@body))


;;;------------------------------------------------------------------- 
;;;
;;; FILLOUT FORMS
;;;

;; Really deprecated operator
(define declare-search-index (&key prompt url class id language direction title style (stream *output-stream*))
  "Declares that the current document is searchable (deprecated).
Easily modernized with a form using the input type STRING"
  (with-deprecation-checking (declare-search-index :condition (or *strict-dtd* *xhtml-generation*))
    (%issue-command ("isindex" stream :fresh-line t :trailing-line t)
      (cond-every
       (url ;; not in the html 4.01 standard, BUT present in HTML2 and NS 1.1 implementations -- JCMa 9/4/2005 
            (%write-command-key-arg stream "href" (maybe-coerce-uri url)))
       (prompt
        (%write-command-key-arg stream "prompt" prompt)))
      (%%write-basic-arguments stream id class language direction title style))))

(defun %write-form-command-args (stream action destination encoding-type name target
                                        accept-media-types accept-charsets 
                                        id class language direction title style events)
  (flet ((write-coerced-email-address ()
	   (unless (http::char-position #\@ destination)
	     (error "Ill-formed email address, ~S." destination))
	   (cond ((string-equal "mailto:" destination :start1 0 :end1 7 :start2 0)
		  (write destination :stream stream :escape t))
		 (t (fast-format stream "\"mailto:~A\"" destination))))
	 (write-encoding (stream)
	   (%write-media-type encoding-type stream)))
    (declare (dynamic-extent #'write-coerced-email-address #'write-encoding))
    (ecase action
      (:post
       (%write-command-key-arg stream "action" (maybe-coerce-uri destination))
       (%write-command-key-arg stream "method" "post")
       (%write-command-key-arg stream "enctype" #'write-encoding))
      (:get
       (%write-command-key-arg stream "action" (maybe-coerce-uri destination))
       (%write-command-key-arg stream "method" "get")
       (%write-command-key-arg stream "enctype" #'write-encoding))
      (:mail
       (%write-command-key-arg stream "action" (etypecase destination
                                                 (string #'write-coerced-email-address)
                                                 (uri (coerce-url-string destination))))
       (%write-command-key-arg stream "enctype" #'write-encoding))
      (:none))
    (cond-every
     (name
      (with-deprecation-checking (with-fillout-form :argument name :condition *xhtml-generation*
                                   :format-string "Use the ID argument for this purpose.")
        (%write-name-argument stream name)))
     (target
      (%write-target-argument stream target))
     (accept-media-types
      (%write-accept-media-type-argument stream accept-media-types))
     (accept-charsets
      (%write-accept-charset-argument stream accept-charsets))
     (id
      (%write-id-argument-handling-xhtml-backward-compatibly stream id))
     (class 
      (%write-class-argument stream class))
     (language
      (%write-language-argument stream language))
     (direction
      (%write-direction-argument stream direction))
     (title
      (%write-title-argument stream title))
     (style 
      (%write-style-argument stream style))
     (events 
      (%write-events-argument stream events)))))

(define-macro with-fillout-form ((action destination &key (encoding-type ''(:application :x-www-form-urlencoded)) 
                                         name target accept-media-types
                                         accept-charsets  id class language
                                         direction title style events (stream '*output-stream*))
				 &body body)
  "Establishes an fillout-form environment around BODY.

Calls to ACCEPT-INPUT within BODY will create form fields within the enclosing fill-out form.  forms
must not be nested. Form fields can be grouped within body using WITH-FORM-FIELD-SET.

ACTION is either :POST, :MAIL, or :GET, or :NONE. The :GET action is not only deprecated, and for
use only in exceptional circumstances, but it also is limited to 1024 characters including the rest
of the the DESTINATION URI.

DESTINATION is the URI to which the form values are returned. It must be an HTTP URL for :POST or
:GET and a MAILTO URI for :MAIL. DESTINATION can be NIL only when If ACTION is :NONE. DESTINATION
may be an interned URI or a string.

ENCODING-TYPE is MIME content type to use when return the form values to DESTINATION.  ENCODING-TYPE
defaults to '(:APPLICATION :X-WWW-FORM-URLENCODED).  ENCODING-TYPE should be '(:MULTIPART
:FORM-DATA) when usd in conjunction with the input type FILE-UPLOAD.

NAME (deprecated in XHTML 1.0) is a name identifying the form element for style sheets. In XHTML,
use ID for this purpose.

TARGET specifies the name of a frame where a document is to be opened. The value is a string or a
special keyword:

   :BLANK  - Always load this link into a new, unnamed window
   :SELF   - Always load this link over yourself
   :PARENT - Always load this link over your parent (becomes self if your at the top)
   :TOP    - Always load this link at top level (becomes self if your at the top)

ACCEPT-MEDIA-TYPE is a MIME content type or a list of content types acceptable to the DESTINATION.
The default value is :UNKNOWN, establishing the expectation in the client that the destination
accepts the media type of document containing the form.

ACCEPT-CHARSET is character encoding or a list of character encodings (see ISO 10646) acceptable to
the DESTINATION.

CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or
:RIGHT-TO-LEFT.

EVENTS can be any of the intrinsic events: :KEY-DOWN, :KEY-PRESS, :KEY-UP, :MOUSE-CLICK,
:MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT, :MOUSE-OVER, :MOUSE-UP, :RESET, :SUBMIT."
  `(cond (*open-form*
          (error "HTML does not allow nesting of forms."))
         (t (let ((*open-form* ,encoding-type))
	      (%with-environment ("form" :fresh-line t :stream ,stream)
                  (%write-form-command-args ,stream ,action ,destination *open-form* ,name
                                            ,target ,accept-media-types ,accept-charsets 
                                            ,id ,class ,language ,direction ,title ,style ,events)
                ,@body)))))

;;;------------------------------------------------------------------- 
;;;
;;; ACCEPT-INPUT
;;;

(define-generic accept-input (input-type query-name &rest args &key stream &allow-other-keys)
  #-scl
  (declare (#+Genera scl:arglist #+LispWorks hcl:lambda-list #-(or Genera LispWorks) arglist
            input-type query-name &key default max-length size stream
            align choices columns compact display-string image-url linebreaks preamble rows sequence-p
	    id class language direction title style events))
  #-ecl
  (:documentation
    "The primary interface for accepting input from users via the WWW fill-out form
facility.

That really means presenting the choices to the user.  The returns are
accepted by the response function to an exported URL. See EXPORT-URL and the
macro BIND-QUERY-VALUES used within response functions.

Required Arguments: 

   INPUT-TYPE -- the symbol denoting an defined input type.

   QUERY-NAME -- a unique name for the value returned by form submission.

Standard Keyword Arguments:  

   DEFAULT -- the default value to use in lieu of user input (available for most
   input types).

   DISABLED -- disables the form control for user input (boolean).

   READ-ONLY -- prohibits changes to the form control (boolean).

   LABEL -- provides the label for a form control. This can be a string for all
   input types, except CHECKBOX and RADIO-BUTTON. For FILE, IMAGE,
   MULTI-LINE-TEXT, PASSWORD, SELECT-CHOICES and STRING, it can be a destructed
   argument list of the form: (LABEL-SPEC POSITION &KEY ACCESS-KEY ID CLASS
   LANGUAGE DIRECTION TITLE STYLE EVENTS), where POSITION can be either :BEFORE or
   :AFTER.

   CLASS -- the class for the element (string, function).

   ID -- an element identifier (string, function).

   STYLE -- specifies inline parameters to use in the default style sheet language
   (string, function).

   TAB-INDEX -- an integer denoting position in the tabbing order.

   ACCESS-KEY -- a single accessibility character from ISO 10646.

   TITLE -- a string used as an element title (string, function).

   LANGUAGE -- the two-digit language code for the displayed content (see RFC
   1766) (string, function).

   DIRECTION -- the base directionality of neutral text and can be either
   :LEFT-TO-RIGHT or :RIGHT-TO-LEFT.

   EVENTS -- If a browser supports client-side events on form input types, they can be passed in via
   this argument. EVENTS can be any of the intrinsic events: :INPUT-DEFOCUS, :INPUT-FOCUS,
   :KEY-DOWN, :KEY-PRESS, :KEY-UP, :MOUSE-CLICK, :MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE,
   :MOUSE-OUT, :MOUSE-OVER, :MOUSE-UP, :NEW-VALUE, :SELECT. Use the macro WITH-EVENT-HANDLERS to
   establish the binding for input events. Note that form submission events are supplied via the
   macro WITH-FILLOUT-FORM.

   PREAMBLE -- A function or string that is written before the input-type.  
   If PREAMBLE is a function, it is called with (INPUT-TYPE STREAM ARGS) 
   A typical use is to emit a JavaScript invoked by an input type.

   STREAM -- the stream on which to write raw HTML generated by the input-type.

Additional Arguments on a per input-type basis.

Input Type          Keyword Arguments

CHECKBOX         -- Allows the user to select a sequence of values by checking
                    boxes.

                    CHOICES is a list of choice values, a compact dotted alist
                    (<Choice-string> . <choice-value>), or an alist of
                    (<choice-string> :VALUE <choice>). Each choice displayed
                    according to its position in the list.

                    Extended arguments to for the choice use the destructuring
                    argument list format:  <choice-spec> ::  (CHOICE-STRING &key
                    VALUE DISABLED ID CLASS LANGUAGE DIRECTION TITLE STYLE EVENTS
                    TAB-INDEX ACCESS-KEY). Extended arguments apply to the
                    individual choice.

                    The three choice formats may be intermixed. When the extended
                    format does not supply a more specific value, choices default
                    their extended parameters to following arguments for the
                    toplevel input-type: DISABLED, CLASS, LANGUAGE, DIRECTION,
                    CHOICE-STYLE, CHOICE-EVENTS.

                    When ID is supplied to the toplevel input type, indivial
                    choice IDs are automatically defaulted to
                    \"<id><choice-position>\", where <choice-position> is an
                    integer with zero origin corresponding to the position of the
                    choice.

                    [Optional] DEFAULT is a single value which is the default. It
                    is compared to the values in CHOICES with EQUAL.
                    
                    [Optional] LAYOUT controls the layout of buttons and can be
                    any of: :ENUMERATION, :DIVISION, :INLINE, or :NONE.
                                 
                    [Optional] (ENUMERATION :PLAIN) is any valid argument to
                    WITH-ENUMERATION and only takes effect when LAYOUT is
                    :ENUMERATION.

                    [Optional] ITEM-STYLE is a style argument to ENUMERATING-ITEM.

                    [Optional] ITEM-EVENTS is an events argument to
                    ENUMERATING-ITEM.

                    [Additional] PREAMBLE, DISABLED, ID, CLASS, LANGUAGE,
                    DIRECTION, TITLE, STYLE, EVENTS supply parameters to toplevel
                    element for enumeration or division, as well as enumeration
                    items (except ID, PREAMBLE).

CLIENT-SIDE-BUTTON  Creates a button on the client that may perform some
                    client-side action based on associated client-side scripts.

                    [Optional] (LABEL \"Button\") is the label displayed by the
                    client.

                    [Optional] IMAGE-URL is a URI for an icon to display as the
                    button.

                    [Optional] alternative-text supplies a short description when
                    IMAGE-URL is not loaded (string, function).

                    [Additional] ID, CLASS, LANGUAGE, DIRECTION, TITLE, STYLE,
                    EVENTS, TAB-INDEX, ACCESS-KEY.

FILE                Accepts file data in form submission. The user is prompted
                    for a file on the client machine. The file data is then 
                    transmitted as the value of the query in a mime multipart 
                    form value return, which requires special handling by user code.

                    [Optional] DEFAULT should be a string or null.

                    [Optional] (SIZE 72) is an integer specifying the size to
                    make the visible input field.

                    [Optional] (MAX-LENGTH 240) is an integer specifying the
                    maximum size which the input field may reach but not top
                    exceed 1024 characters.

                    [Optional] DIRECTORY is the directory where uploaded filed
                    should be deposited on the server. It defaults to
                    *file-upload-default-directory*.

                    [Optional] ACCEPT-MEDIA-TYPES is a list of acceptable media
                    types.

                    [Additional] DISABLED, ID, CLASS, LANGUAGE, DIRECTION, TITLE,
                    STYLE, EVENTS, TAB-INDEX, ACCESS-KEY.

HIDDEN           -- Allows data passed through forms and back to the server.
                    These are known as hidden fields because they are invisible to
                    the user.

                    [Required] :DEFAULT should be a string or number.

                    HTTP:WRITE-TO-ARMOR-PLATED-STRING and HTTP:READ-FROM-ARMOR-PLATED-FORM
                    can help avoid damage in transit.

IMAGE            -- Creates a graphical submit button and returns image
                    coordinates. When a pointer is used to click on the image, X-Y
                    coordinartes can be handled by the client or the server
                    depending on CLIENT-SIDE-IMAGE-MAP and
                    ACCEPT-COORDINATES-AT-URL.

                    In the form returns, client returns two query values whose
                    names are constructed by appending .X and .Y to QUERY-NAME.
                    The bindings for X and Y are in pixels using the upper left
                    corner of the image as the origin.  The effect is just like
                    the SUBMIT input type in that a user mouse click on this field
                    causes the form to be sent immediately.

                    [Required] IMAGE-URL is a URI for an image to display as the
                    button.

                    [Optional] CLIENT-SIDE-IMAGE-MAP is a URI where a client-side
                    image map is located.

                    [Optional] ACCEPT-COORDINATES-AT-URL is a URI where a server
                    will accept X-Y coordinates.

                    [Optional] ALTERNATIVE-TEXT supplies a short description when
                    IMAGE-URL is not loaded (string, function).
                      
                    [Optional] ALIGNMENT (deprecated) can be any of :LEFT,
                    :CENTER, :RIGHT.

                    [Additional] LABEL, PREAMBLE, DISABLED, ID, CLASS, LANGUAGE,
                    DIRECTION, TITLE, STYLE, EVENTS, TAB-INDEX, ACCESS-KEY.


MULTI-LINE-TEXT  -- Allows the user to type in or edit multi line text in a
                    scrollable input box.

                    [Optional] DEFAULT should be a string, null, or a function
                    which is called on STREAM.

                    [Optional] (COLUMNS 72.) is an integer specifying the
                    width of the visible input field.

                    [Optional] (ROWS 5.) is an integer specifying the height
                    of the visible input field.

                    [Additional] LABEL, PREAMBLE, DISABLED, READ-ONLY, ID, CLASS,
                    LANGUAGE, DIRECTION, TITLE, STYLE, EVENTS, TAB-INDEX,
                    ACCESS-KEY.

PASSWORD         -- Just like the STRING input type except the client does not
                    display the input.

                    [Optional] (SIZE 10) is an integer specifying the size to make
                    the visible input field.

                    [Additional] LABEL, PREAMBLE, DISABLED, READ-ONLY, ID, CLASS,
                    LANGUAGE, DIRECTION, TITLE, STYLE, EVENTS, TAB-INDEX,
                    ACCESS-KEY.

RADIO-BUTTON     -- Allows the user to select a single value by checking a box.

                    CHOICES is a list of choice values, a compact dotted alist
                    (<Choice-string> . <choice-value>), or an alist of
                    (<choice-string> :VALUE <choice>). Each choice displayed
                    according to its position in the list.

                    Extended arguments to for the choice use the destructuring
                    argument list format:  <choice-spec> ::  (CHOICE-STRING &key
                    VALUE DISABLED ID CLASS LANGUAGE DIRECTION TITLE STYLE EVENTS
                    TAB-INDEX ACCESS-KEY). Extended arguments apply to the
                    individual choice.

                    The three choice formats may be intermixed. When the extended
                    format does not supply a more specific value, choices default
                    their extended parameters to following arguments for the
                    toplevel input-type: DISABLED, CLASS, LANGUAGE, DIRECTION,
                    CHOICE-STYLE, CHOICE-EVENTS.

                    When ID is supplied to the toplevel input type, indivial
                    choice IDs are automatically defaulted to
                    \"<id><choice-position>\", where <choice-position> is an
                    integer with zero origin corresponding to the position of the
                    choice.

                    [Optional] DEFAULT is a single value which is the default. It
                    is compared to the values in CHOICES with EQUAL.
                    
                    [Optional] LAYOUT controls the layout of buttons and can be
                    any of: :ENUMERATION, :DIVISION, :INLINE, or :NONE.
                                 
                    [Optional] ENUMERATION is any valid argument to
                    WITH-ENUMERATION and only takes effect when LAYOUT is
                    :ENUMERATION.

                    [Optional] ITEM-STYLE is a style argument to ENUMERATING-ITEM.

                    [Optional] ITEM-EVENTS is an events argument to
                    ENUMERATING-ITEM.

                    [Additional] PREAMBLE, DISABLED, ID, CLASS, LANGUAGE,
                    DIRECTION, TITLE, STYLE, EVENTS supply parameters to toplevel
                    element for enumeration or division, as well as enumeration
                    items (except ID, PREAMBLE).

RESET-BUTTON     -- Reset the values of the form to the default when pressed by the user.

                    [Optional] (LABEL \"Reset\") is the label displayed by the
                    client.

                    [Optional] IMAGE-URL is a URI for an icon to display as the
                    button.

                    [Optional] ALTERNATIVE-TEXT supplies a short description when
                    IMAGE-URL is not loaded (string, function).

                    [Additional] ID, CLASS, LANGUAGE, DIRECTION, TITLE, STYLE,
                    EVENTS, TAB-INDEX, ACCESS-KEY.

SELECT-CHOICES   -- Allows the user to select either a single or multiple choices.

                    [Required] CHOICES is a list of choice values or an alist of
                    (<choice-string> :VALUE <choice>). Each choice displayed
                    according to its position in the list.

                    Extended arguments for the choice option use the destructuring
                    argument list format: <choice-spec> :: (CHOICE-STRING &key VALUE
                    LABEL DISABLED ID CLASS LANGUAGE DIRECTION TITLE STYLE EVENTS
                    TAB-INDEX). Extended arguments apply to the individual choice.

                    The three choice formats may be intermixed. When the extended
                    format is not used, choice share the following additional
                    arguments for the toplevel input-type: CLASS, LANGUAGE,
                    DIRECTION, STYLE.
 
                    Choices can be grouped by supplying entries in the following
                    format: <choice-group> :: ((&key LABEL DISABLED ID CLASS
                    LANGUAGE DIRECTION TITLE STYLE EVENTS) <choice-spec1>
                    <choice-spec2> ...) Group arguments are all provided in the
                    first entry. The rest of the group entry consists of options
                    in the group.

                    [Optional] DEFAULT is either a single value or a list of
                    values, which are compared to choices with EQUALP.

                    [Optional] SEQUENCE-P specifies whether a sequence of values
                    should be returned.

                    [Optional] SIZE is an integer specifying the visible number
                    of rows visible in a scrolling inset choice box.  When
                    :SEQUENCE-P is null, you can specify SIZE to be
                    :PULL-DOWN-MENU to have the choices rendered as  a pull down
                    menu. When size is unspecified, it is defaulted to a heuristic
                    value.

                    [Additional] LABEL, PREAMBLE, DISABLED, ID, CLASS, LANGUAGE,
                    DIRECTION, TITLE, STYLE, EVENTS, TAB-INDEX.

SUBMIT-BUTTON    -- Submits the form when pressed by the user.

                    [Optional] (LABEL \"Submit\") is the label displayed by the
                    client.

                    [Optional] IMAGE-URL is a URI for an icon to display as the
                    button.

                    [Optional] ALTERNATIVE-TEXT supplies a short description when
                    IMAGE-URL is not loaded (string, function).

                    [Additional] ID, CLASS, LANGUAGE, DIRECTION, TITLE, STYLE,
                    EVENTS, TAB-INDEX, ACCESS-KEY.

STRING           -- Allow the user to type in a string on a single line.

                    [Optional] DEFAULT should be a string or null.

                    [Optional] (SIZE 72) is an integer specifying the size to
                    make the visible input field.

                    [Optional] (MAX-LENGTH 240) is an integer specifying the
                    maximum size which the input field may reach but not top
                    exceed 1024 characters.

                    [Additional] LABEL, PREAMBLE, DISABLED, READ-ONLY, ID, CLASS,
                    LANGUAGE, DIRECTION, TITLE, STYLE, EVENTS, TAB-INDEX,
                    ACCESS-KEY."))

;;;------------------------------------------------------------------- 
;;;
;;; DEFINE INPUT TYPES
;;;

(defun %accept-input-write-preamble (preamble input-type stream args)
  (etypecase preamble
    (function (funcall preamble input-type stream args))
    (string (write-string preamble stream))))

(define-macro write-standard-input-type-args ((stream query-name input-type args
						      &key (bindings '(default disabled read-only alignment
									       id class language direction
									       title style events tab-index access-key)))
                                              &body body)
  "Writes standard aurgments to input types."
  (flet ((get-command-key-arg (input-type argument stream)
	   (ecase argument
	     (default
	       `(default
		  (check-value-type ,input-type default)
		  (%write-command-key-arg ,stream "value" default)))
	     (disabled 
	       `(disabled (%write-command-key-arg ,stream "disabled")))
	     (alignment
	       `(alignment 
		  (with-deprecation-checking (accept-input :argument alignment)
		    (%write-alignment-argument stream alignment))))
	     (read-only
	       `(read-only (%write-command-key-arg ,stream "readonly")))
	     (id
	       `(id (%write-id-argument ,stream id)))
	     (class
	       `(class (%write-class-argument ,stream class)))
	     (language
	       `(language (%write-language-argument ,stream language)))
	     (direction
	       `(direction (%write-direction-argument ,stream direction)))
	     (title 
	       `(title (%write-title-argument ,stream title)))
	     (style
	       `(style (%write-style-argument ,stream style)))
	     (events
	       `(events (%write-events-argument ,stream events)))
	     (tab-index
	       `(tab-index (%write-tab-index-argument ,stream tab-index)))
	     (access-key
	       `(access-key (%write-access-key-argument ,stream access-key))))))
    `(,@(if args
            `(destructuring-bind (&key ,@bindings &allow-other-keys) ,args)
          `(progn))
       ,@(etypecase input-type
	   (null nil)
	   (symbol
	     `((with-slots (type-arg) ,input-type
		 (%write-command-key-arg ,stream "type" type-arg))))
	   (string
	     `((%write-command-key-arg ,stream "type" ,input-type))))
       ,@(etypecase query-name
	   (null nil)
	   (symbol
	     `((cond (,query-name
		      (verify-query-name ,query-name)
		      (%write-name-argument ,stream ,query-name))
		     (t (error "No QUERY-NAME provided for input type."))))))
       (prog1 (progn ,@body)
	      ,@(loop for arg in bindings
		      collect (get-command-key-arg input-type arg stream) into clauses
		      finally (when clauses
				(return `((cond-every ,.clauses)))))))))

(defun %write-form-button-arguments (stream type name value disabled class id language direction title style tab-index access-key events)
  (write-standard-input-type-args (stream nil nil nil :bindings (disabled id class language direction title style events tab-index access-key))
    (%write-command-key-arg stream "type" (ecase type
                                            (:button "button")
                                            (:submit "submit")
                                            (:reset "reset")))
    (cond-every
     ((not (eq value :no-value))
      (%write-command-key-arg stream "value" value))
     (name
      (%write-name-argument stream name)))))

(define-macro with-form-button ((type &key name (value :no-value) disabled 
                                      class id language direction title style tab-index access-key events
                                      (stream *output-stream*)) &body body)
  "Generates a form button that allows extended content to be emitted within BODY on STREAM.

Normally, buttons are generated using ACCEPT-INPUT with the input types CLIENT-SIDE-BUTTON,
SUBMIT-BUTTON or RESET-BUTTON.

This facility is available for use when more control over button content is required.  So, for
example, text and graphics may be combined within BODY.

   (with-form-button (:submit :name \"submit\" :value \"submit\" :steam stream)
     (fast-format stream \"~&Send ~I\" (image \"/icons/wow.gif\" :stream stream)))

Alternatively, a script tied to the button can be emitted within BODY.

TYPE is required and must be one of: :BUTTON, :SUBMIT or :RESET.

VALUE is the initial value of the button. It is always transmitted except when its value is the
keyword :NO-VALUE

NAME is a string naming the button (recommended).

DISABLED -- disables the form control for user input (boolean).
CLASS -- the class for the element (string, function).
ID -- an element identifier (string, function).
STYLE -- specifies inline parameters to use in the default style sheet language (string, function).
TAB-INDEX -- an integer denoting position in the tabbing order.
ACCESS-KEY -- a single accessibility character from ISO 10646.
TITLE -- a string used as an element title (string, function).
LANGUAGE -- the two-digit language code for the displayed content (see RFC 1766) (string, function).
DIRECTION -- the base directionality of neutral text and can be either :LEFT-TO-RIGHT or
:RIGHT-TO-LEFT.

EVENTS can be any of the intrinsic events: :INPUT-DEFOCUS, :INPUT-FOCUS, :KEY-DOWN, :KEY-PRESS,
:KEY-UP, :MOUSE-CLICK, :MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT, :MOUSE-OVER,
:MOUSE-UP."
  `(%with-environment
       ("button" :stream ,stream)
       (%write-form-button-arguments ,stream ,type ,name ,value ,disabled ,class ,id ,language ,direction ,title ,style ,tab-index ,access-key ,events)
     ,@body))

(defun %with-form-field-set-legend (stream access-key id class language direction title style events alignment)
  (cond-every
   (alignment
    (with-deprecation-checking (with-form-field-set :argument alignment)
      (%write-command-key-arg stream "align" (caption-alignment-value alignment))))
   (access-key
    (%write-access-key-argument stream access-key)))
  (%%write-standard-arguments stream id class language direction title style events))

(define-macro with-form-field-legend ((&key access-key class id language direction title alignment
					     style events (stream '*output-stream*)) &body body)
  "Asserts the contents of BODY is a legend within a form field set environment.
See: WITH-FORM-FIELD-SET

ALIGNMENT (deprecated) can be any of: :TOP, :BOTTOM, :LEFT, :RIGHT.
ACCESS-KEY is a single accessibility character from ISO 10646.
CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either
:LEFT-TO-RIGHT or :RIGHT-TO-LEFT.

EVENTS can be any of the intrinsic events: :KEY-DOWN, :KEY-PRESS, :KEY-UP, :MOUSE-CLICK,
:MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT, :MOUSE-OVER, :MOUSE-UP."
  `(%with-environment
     ("legend" :stream ,stream)
     (%with-form-field-set-legend
       ,stream ,access-key ,id ,class ,language ,direction ,title ,style ,events ,alignment)
     ,@body))

(defun %with-form-field-set-arguments (stream access-key id class language direction title style events)
  (when access-key
    (%write-access-key-argument stream access-key))
  (%%write-standard-arguments stream id class language direction title style events))

(define-macro with-form-field-set ((&key access-key class id language direction title
					     style events (stream '*output-stream*)) &body body)
  "Groups all calls to ACCEPT-INPUT within BODY into a form field set.
A legend may be specified using WITH-FORM-FIELD-LEGEND within BODY.

ACCESS-KEY is a single accessibility character from ISO 10646.
CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either
:LEFT-TO-RIGHT or :RIGHT-TO-LEFT.

EVENTS can be any of the intrinsic events: :KEY-DOWN, :KEY-PRESS, :KEY-UP, :MOUSE-CLICK,
:MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT, :MOUSE-OVER, :MOUSE-UP."
  `(%with-environment
     ("fieldset" :stream ,stream)
     (%with-form-field-set-arguments
       ,stream ,access-key ,id ,class ,language ,direction ,title ,style ,events)
     ,@body))

(defun %with-form-control-label-arguments (stream control-id access-key id class language direction title style events )
  (cond-every
   (control-id
    (%write-command-key-arg stream "for" control-id))
   (access-key
    (%write-access-key-argument stream access-key)))
  (%%write-standard-arguments stream id class language direction title style events))

(define-macro with-form-control-label ((&key control-id access-key class id language direction title
					     style events (stream '*output-stream*)) &body body)
  "Establishes an environment so that BODY can emit a label associated with a form control.

Use this macro to associate a label with a form control (a call to ACCEPT-INPUT) in either
of two ways:

  * Provide a unique HTML id to a single call to ACCEPT-INPUT and supply it as
  the value of CONTROL-ID.

  * Include a single call to ACCEPT-INPUT inside BODY and provide no CONTROL-ID
  argument.

This facility provides greater control over the label that the LABEL argument for the
input-types FILE, IMAGE, MULTI-LINE-TEXT, PASSWORD, SELECT-CHOICES, STRING.  It not
relevant for the input types that have implicit labels, specifically CHECKBOX,
CLIENT-SIDE-BUTTON, HIDDEN, RADIO-BUTTON, RESET-BUTTON, SUBMIT-BUTTON

ACCESS-KEY is a single accessibility character from ISO 10646.

CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or
:RIGHT-TO-LEFT.

EVENTS can be any of the intrinsic events: :INPUT-DEFOCUS, :INPUT-FOCUS, :KEY-DOWN, :KEY-PRESS,
:KEY-UP, :MOUSE-CLICK, :MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT, :MOUSE-OVER,
:MOUSE-UP."
  `(%with-environment
     ("label" :stream ,stream)
     (%with-form-control-label-arguments
       ,stream ,control-id ,access-key ,id ,class ,language ,direction ,title ,style ,events)
     ,@body))

(eval-when (:compile-toplevel :execute :load-toplevel)

(defun make-input-type-code (class name type-arg lisp-type nicknames)
  `(initialize-input-type
    (make-instance ',class :name ',name :type-arg ',type-arg :lisp-type ,lisp-type :nicknames ',nicknames)))
)

(define-macro define-input-type (name &key (superclass 'input-type) type-arg lisp-type parameters nicknames abstract)
  "Top-level form for defining new HTML 4.01 input types."
  `(progn
     (defclass ,name
               (,superclass)
         ,parameters)
     ,(unless abstract
        (make-input-type-code name name type-arg lisp-type nicknames))))

(define-input-type input-type :superclass html2::input-type :abstract t)

(define-input-type implicit-label-input-type :superclass input-type :abstract t)

(define-input-type explicit-label-input-type :superclass input-type :abstract t)

(defvar *input-types* (make-hash-table :size 13.))

(defmethod unregister ((input-type input-type))
  (with-slots (name nicknames) input-type
    (remhash name *input-types*)
    (dolist (item nicknames)
      (remhash item *input-types*))))

(defmethod register ((input-type input-type))
  (with-slots (name nicknames) input-type
    (setf (gethash name *input-types*) input-type)
    (dolist (item nicknames)
      (setf (gethash item *input-types*) input-type))
    input-type))

(defmethod initialize-input-type ((input-type input-type))
  (register input-type))

(declaim (inline get-input-type))

(defun get-input-type (name &optional (error-p t))
  "Returns the HTML 4.01 input type named NAME"
  (typecase name
    (symbol
     (cond ((gethash name *input-types*))
           (error-p (error "The HTML 4.01 input type, ~S, is undefined." name))
           (t nil)))
    (input-type name)))

(defmethod accept-input ((input-type symbol) query-name &rest args &key &allow-other-keys)
  (apply #'accept-input (get-input-type input-type) query-name args))

(declaim (notinline get-input-type))

;; Provides automatic labels for EXPLICIT-LABEL-INPUT-TYPE
(defmethod accept-input :around ((input-type explicit-label-input-type) query-name &rest args 
				 &key (stream *output-stream*) &allow-other-keys)
  (declare (dynamic-extent args)
	   (ignore query-name))
  (flet ((write-label (stream label)
	   (etypecase label
	     (string
	       (write-string label stream))
	     (function
	       (funcall label stream)))))
    (declare (inline write-label))
    (destructuring-bind (&key label preamble &allow-other-keys) args
	(when preamble
	  (%accept-input-write-preamble preamble input-type stream args))
      (etypecase label
	(null (call-next-method))
	(atom
	  (with-form-control-label (:stream stream)
	    (write-label stream label)
	    (fresh-line stream)
	    (call-next-method)))
	(cons
	  (destructuring-bind (string position &key access-key id class language direction title style events) label
	    (with-form-control-label (:access-key access-key :id id :class class :language language :direction direction
						  :title title :style style :events events :stream stream)
	      (ecase position
		(:before
		  (write-label stream string)
		  (call-next-method))
		(:after
		  (call-next-method)
		  (write-label stream string))))))))))

(define-input-type
  hidden
  :type-arg "hidden"
  :lisp-type '(or string number)
  :superclass input-type)

;; may be possible to inherit from html 2
(defmethod accept-input ((hidden hidden) query-name &rest args &key (stream *output-stream*) &allow-other-keys)
  (declare (dynamic-extent args))
  (flet ((issue-hidden-input-command (hidden query-name value stream)
	   (declare (type hidden hidden))
	   (with-slots (type-arg) hidden
	     (%issue-command ("input" stream)
	       (%write-command-key-arg stream "type" type-arg)
	       (%write-name-argument stream query-name)
	       (check-value-type hidden value)
	       (%write-command-key-arg stream "value" value)))))
    (declare (inline issue-hidden-input-command))
    ;; Ignore args (disabled error events) because they don't make sense for hidden fields
    (destructuring-bind (&key default &allow-other-keys) args
      (if query-name
          (verify-query-name query-name)
	  (error "No QUERY-NAME provided for input type."))
      (etypecase default
        (null (error "No default value provided for a HIDDEN input type."))
        (atom (issue-hidden-input-command hidden query-name default stream))
        (cons (dolist (value default)
		(issue-hidden-input-command hidden query-name value stream)))))))

(define-input-type
  string+
  :type-arg "text"
  :lisp-type 'string
  :superclass explicit-label-input-type
  :parameters ((default-size :initform 72 :reader default-size)
               (default-max-size :initform 240 :reader default-max-size))
  :nicknames (string))

(defmethod accept-input ((string+ string+) query-name &rest args &key (stream *output-stream*) &allow-other-keys)
  (declare (dynamic-extent args))
  (with-slots (default-size default-max-size) string+
    (%issue-command ("input" stream)
      (write-standard-input-type-args (stream query-name string+ args 
					      :bindings (default disabled read-only id class language direction
								 title style events tab-index access-key))
	(destructuring-bind (&key size max-length &allow-other-keys) args
	  (let ((local-size (or size default-size))
		(max (or max-length default-max-size)))
	    (cond-every
	      (local-size
		(%write-command-key-arg stream "size" local-size))
	      (max
		(unless (<= max 1024.)
		  (error "String fields cannot exceed 1024 characters in HMTL 4.01"))
		(%write-command-key-arg stream "maxlength" max)))))))))

(define-input-type
  password
  :type-arg "password"
  :superclass string+
  :lisp-type 'string
  :parameters ((default-size :initform 10)))

(define-input-type
  multi-line-text
  :type-arg "textarea"
  :superclass explicit-label-input-type
  :lisp-type 'string)

(defmethod accept-input ((multi-line-text multi-line-text) query-name &rest args &key (stream *output-stream*))
  (declare (dynamic-extent args))
  (with-slots (type-arg) multi-line-text
    (destructuring-bind (&key (rows 5.) (columns 72.) default &allow-other-keys) args
      (%with-environment (type-arg :stream stream :fresh-line t)
          (write-standard-input-type-args (stream query-name multi-line-text args 
                                                  :bindings (disabled read-only id class language direction
                                                                      title style events tab-index access-key))

            (%write-integer-argument stream "rows" rows) 
            (%write-integer-argument stream "cols" columns))
        (etypecase default
          (null)
          (string (write-string default stream))
          (function (funcall default stream)))))))

(define-input-type
  image
  :type-arg "image"
  :superclass explicit-label-input-type
  :lisp-type 'null)

(defmethod accept-input ((image image) query-name &rest args &key (stream *output-stream*) &allow-other-keys)
  (declare (dynamic-extent args))
  (destructuring-bind (&key image-url alternative-text accept-coordinates-at-url client-side-image-map
			    align &allow-other-keys) args
    (%issue-command ("input" stream)
      (write-standard-input-type-args (stream query-name image args 
					      :bindings (disabled alignment id class language direction
								  title style events tab-index access-key))
	(unless (and align (null alignment))	;obsolete -- 9/5/2005 JCMa
	  (setq alignment align))
        (%write-command-key-arg stream "src" (maybe-coerce-uri image-url))
	(cond-every
	  (alternative-text
	    (%write-command-key-arg stream "alt" alternative-text))
	  (accept-coordinates-at-url 
	    (%write-command-key-arg stream "ismap"))
	  (client-side-image-map
	    (%write-command-key-arg stream "usemap"
				    (url:name-string-without-search-suffix client-side-image-map nil))))
	;;backwards compatibility
	(when (and align (not alignment)) (setq alignment align))))))

;;;------------------------------------------------------------------- 
;;;
;;; CHOICES
;;;

(defmacro %form-button-make-standard-arg-list (&rest args)
  (loop for item in args
	collect `(and ,item (list ,(symbolize item :keyword) ,item)) into clauses
	finally (return `(nconc ,.clauses))))

(define-input-type
  radio-button
  :type-arg "radio"
  :superclass implicit-label-input-type
  :lisp-type 'atom)

;; enumerates from 0 to n
(defmethod accept-input ((radio-button radio-button) query-name &rest args &key (stream *output-stream*) &allow-other-keys)
  (declare (dynamic-extent args))
  (macrolet ((with-decoded-choice ((choice default count id-string standard-args) &body body)
	       `(let (value choice-string check-p element-args)
		  (etypecase ,choice
		    (atom
		      (setq value (princ-to-string ,count) choice-string ,choice check-p (equal ,choice ,default)))
		    (cons
		      (if (consp (cdr ,choice))
			  (let ((val (getf (cdr ,choice) :value :+no-value+)))
			    (when (eq val :+no-value+)
			      (error "No VALUE argument was supplied for the choice: ~S" choice))
			    (setq value val choice-string (car ,choice) check-p (equal value ,default) element-args (cdr choice)))
			  (setq value (cdr ,choice) choice-string (car ,choice) check-p (equal (cdr ,choice) ,default)))))
		  (flet ((wite-choice-id (stream) (fast-format stream "\"~A~D\"" ,id-string ,count)))
		    (declare (dynamic-extent #'wite-choice-id))
		    (let ((choice-args (if (or element-args ,id-string)
					   (nconc
					     (and element-args (copy-list element-args))
					     (and ,id-string (list :id #'wite-choice-id))
					     ,standard-args)
					   ,standard-args)))
		      (declare (dynamic-extent choice-args))
		      ,@body)))))
    (flet ((write-element (stream choice-string value check-p choice-args)
	     (%issue-command ("input" stream :fresh-line t)
	       (write-standard-input-type-args (stream query-name radio-button choice-args 
						       :bindings (disabled id class language direction
									   title style events tab-index access-key))
		 (%write-command-key-arg stream "value" value)
		 (when check-p
		   (%write-command-key-arg stream "checked"))))
	     (write-string choice-string stream)))
      (declare (dynamic-extent #'write-element))
      (destructuring-bind (&key choices layout (enumeration :plain) default selected-choice
				preamble disabled id class title language direction style events
				choice-style choice-events item-style item-events
				compact (linebreaks t)	;compact and linebreaks are obsolete -- 9/6/2005 JCMa
				&allow-other-keys) args
	(when preamble
	  (%accept-input-write-preamble preamble radio-button stream args))
	(let ((default-value (or default selected-choice))	;selected-choice is obsolete  2/26/95 -- JCMa.
	      (standard-args (%form-button-make-standard-arg-list
			       disabled class language direction choice-style choice-events)))
	  (declare (dynamic-extent standard-args))
	  (ecase (or layout (if linebreaks :enumeration :none))
	    (:enumeration
	      (with-enumeration (stream enumeration :compact compact :class class :language language :direction direction 
					:id id :title title :style style :events events)
		(loop for choice in choices
		      for count fixnum upfrom 0
		      do (enumerating-item (stream  :class class :language language :direction direction 
						    :style item-style :events item-events)
			   (with-decoded-choice (choice default-value count id standard-args)
						(write-element stream choice-string value check-p choice-args))))))
	    (:none
	      (loop for choice in choices
		    for count  fixnum upfrom 0
		    do (with-decoded-choice (choice default-value count id standard-args)
					    (write-element stream choice-string value check-p choice-args))))
	    ((:division :inline)
	     (with-division (:stream stream :inline-p (eq layout :inline) :style style :events events
				       :class class :id id :title title :language language :direction direction)
	       (loop for choice in choices
		     for count fixnum upfrom 0
		     do (with-decoded-choice (choice default-value count id standard-args)
					     (write-element stream choice-string value check-p choice-args)))))))))))

(define-input-type
  checkbox
  :type-arg "checkbox"
  :superclass implicit-label-input-type
  :lisp-type '(or atom list))

;; enumerates from 0 to (1- n)
(defmethod accept-input ((checkbox checkbox) query-name &rest args &key (stream *output-stream*) &allow-other-keys)
  (declare (dynamic-extent args))
 (macrolet ((with-decoded-choice ((choice default count id-string standard-args) &body body)
	       `(let (value choice-string check-p element-args)
		  (etypecase ,choice
		    (atom
		      (setq value (princ-to-string ,count) choice-string ,choice 
			    check-p (member ,choice ,default :test #'equal)))
		    (cons
		      (if (consp (cdr ,choice))
			  (let ((val (getf (cdr ,choice) :value :+no-value+)))
			    (when (eq val :+no-value+)
			      (error "No VALUE argument was supplied for the choice: ~S" choice))
			    (setq value val choice-string (car ,choice) 
				  check-p (member value ,default :test #'equal) element-args (cdr choice)))
			  (setq value (cdr ,choice) choice-string (car ,choice)
				check-p (member (cdr ,choice) ,default :test #'equal)))))
		  (flet ((wite-choice-id (stream) (fast-format stream "\"~A~D\"" ,id-string ,count)))
		    (declare (dynamic-extent #'wite-choice-id))
		    (let ((choice-args (if (or element-args ,id-string)
					   (nconc
					     (and element-args (copy-list element-args))
					     (and ,id-string (list :id #'wite-choice-id))
					     ,standard-args)
					   ,standard-args)))
		      (declare (dynamic-extent choice-args))
		      ,@body)))))
    (flet ((write-element (stream choice-string value check-p choice-args)
	     (%issue-command ("input" stream :fresh-line t)
	       (write-standard-input-type-args (stream query-name checkbox choice-args 
						       :bindings (disabled id class language direction
									   title style events tab-index access-key))
		 (%write-command-key-arg stream "value" value)
		 (when check-p
		   (%write-command-key-arg stream "checked"))))
	     (write-string choice-string stream)))
      (declare (dynamic-extent #'write-element))
      (destructuring-bind (&key choices layout (enumeration :plain) default selected-choice
				preamble disabled id class title language direction style events
				choice-style choice-events item-style item-events
				compact (linebreaks t)	;compact and linebreaks are obsolete -- 9/6/2005 JCMa
				&allow-other-keys) args
	(check-type default list)
	(when preamble
	  (%accept-input-write-preamble preamble checkbox stream args))
	(let ((default-value (or default selected-choice))	;selected-choice is obsolete  2/26/95 -- JCMa.
	      (standard-args (%form-button-make-standard-arg-list
			       disabled class language direction choice-style choice-events)))
	  (declare (dynamic-extent standard-args))
	  (ecase (or layout (if linebreaks :enumeration :none))
	    (:enumeration
	      (with-enumeration (stream enumeration :compact compact :class class :language language :direction direction 
					:id id :title title :style style :events events)
		(loop for choice in choices
		      for count fixnum upfrom 0
		      do (enumerating-item (stream  :class class :language language :direction direction 
						    :style item-style :events item-events)
			   (with-decoded-choice (choice default-value count id standard-args)
						(write-element stream choice-string value check-p choice-args))))))
	    (:none
	      (loop for choice in choices
		    for count fixnum upfrom 0
		    do (with-decoded-choice (choice default-value count id standard-args)
					    (write-element stream choice-string value check-p choice-args))))
	    ((:division :inline)
	     (with-division (:inline-p (eq layout :inline) :style style :events events
				       :class class :id id :title title :language language :direction direction)
	       (loop for choice in choices
		     for count fixnum upfrom 0
		     do (with-decoded-choice (choice default-value count id standard-args)
					     (write-element stream choice-string value check-p choice-args)))))))))))

(define-input-type
  select-choices
  :type-arg "select"
  :superclass explicit-label-input-type
  :lisp-type 'cons)

;; if the size argument is present, the choices appear in a scrollable
;; indented choice window.
(defmethod accept-input ((select-choices select-choices) query-name &rest args &key (stream *output-stream*)
			 &allow-other-keys)
  (declare (dynamic-extent args))
  (macrolet ((make-standard-arg-list (&rest args)
	       `(nconc ,.(loop for item in args
			       collect `(and ,item (list ,(symbolize item :keyword) ,item))))))
    (labels ((heuristic-size-default (choices)
	       (let ((n (length choices))
		     (default-size *select-choices-max-default-size*))
		 (declare (fixnum n default-size))
		 (cond ((> n default-size)
			(if (> n (* 5 default-size)) (* 2 default-size) default-size))
		       (t n))))
	     (emit-item (stream choice value default value-provided-p sequence-p option-args)
	       (%with-environment 
		 ("option" :stream stream :fresh-line t)
		 (write-standard-input-type-args (stream nil nil option-args
							 :bindings (disabled id class language direction title style
									     events tab-index))
		   (destructuring-bind (&key label &allow-other-keys) option-args
		     (cond-every
		       ((and value-provided-p value)
			(%write-command-key-arg stream "value" value))
		       ((if sequence-p
			    (member value default :test #'equalp)
			    (equalp value default))
			(%write-command-key-arg stream "selected"))
		       (label
			 (%write-command-key-arg stream "label" label)))))
		 (write choice :escape nil :base 10. :stream stream)))
	     (emit-option-group (stream group-args group-choices default sequence-p standard-args)
	       (destructuring-bind (&optional label &rest opt-group-args) group-args
		 (declare (dynamic-extent opt-group-args))
		 (%with-environment 
		   ("optgroup" :stream stream :fresh-line t)
		   (write-standard-input-type-args 
		     (stream nil nil opt-group-args
			     :bindings (disabled id class language direction title style events))
		     (when label (%write-command-key-arg stream "label" label)))
		   (dolist (opt group-choices )
		     (etypecase opt
		       (cons
			 (destructuring-bind (choice &key value &allow-other-keys) opt
			   (emit-item stream choice value default t sequence-p (cdr opt))))
		       (atom
			 (emit-item stream opt opt default nil sequence-p standard-args))))))))
      (declare (dynamic-extent #'emit-item #'heuristic-size-default))
      (with-slots (type-arg) select-choices
	(destructuring-bind (&key choices default size (sequence-p (consp default)) 
				  class language direction style &allow-other-keys) args
	  (cond ((null default))
		(sequence-p
		 (unless (listp default)
		   (error "Default, ~S, is not a list, which is required when sequence-p is not null." default)))
		(t (unless (atom default)
		     (error "Default, ~S, is not an atom, which is required when sequence-p is null." default))))
	  (%with-environment 
	    (type-arg :stream stream :fresh-line t)
	    (write-standard-input-type-args (stream query-name nil args :bindings (disabled id class language direction
											    title style events tab-index))
	      (cond (sequence-p
		     (%write-command-key-arg stream "size" (or size (heuristic-size-default choices)))
		     (%write-command-key-arg stream "multiple"))
		    ((eq size :pull-down-menu)
		     (%write-command-key-arg stream "size" 1))
		    (size
		     ;; Scrollable inset choices don't make sense for fewer than two choices.
		     (when (< 1 size)
		       (%write-command-key-arg stream "size" size)))
		    (t (%write-command-key-arg stream "size" (heuristic-size-default choices)))))
	    (let ((standard-args (make-standard-arg-list class language direction style)))
	      (declare (dynamic-extent standard-args))
	      (dolist (item choices)
		(etypecase item
		  (cons
		    (etypecase (car item)
		      (list ;; option group
			(emit-option-group stream (car item) (cdr item) default sequence-p standard-args))
		      (atom ;;normal choice
			(destructuring-bind (choice &key value &allow-other-keys) item
			  (emit-item stream choice value default t sequence-p (cdr item))))))
		  (atom
		    (emit-item stream item item default nil sequence-p standard-args)))))))))))

;;;------------------------------------------------------------------- 
;;;
;;; META CHOICES
;;;

(define-input-type
  reset-button
  :type-arg "reset"
  :superclass implicit-label-input-type
  :lisp-type 'null)

(defmethod accept-input ((reset-button reset-button) query-name &rest args &key (stream *output-stream*) &allow-other-keys)
  (declare (dynamic-extent args))
  (destructuring-bind (&key image-url label alternative-text display-string &allow-other-keys) args
    (%issue-command ("input" stream)
      (write-standard-input-type-args (stream query-name reset-button args 
					      :bindings (id class language direction title style events tab-index access-key))
	(%write-command-key-arg stream "value" (or label display-string "Reset"));display-string obsolete -- JCMa 9/4/2005 
	(cond-every
	  (image-url
	    (%write-command-key-arg stream "src" (maybe-coerce-uri image-url)))
	  (alternative-text
	    (%write-command-key-arg stream "alt" alternative-text)))))))

(define-input-type
  submit-button
  :type-arg "submit"
  :superclass implicit-label-input-type
  :lisp-type 'null)

(defmethod accept-input ((submit-button submit-button) query-name &rest args &key (stream *output-stream*) &allow-other-keys)
  (declare (dynamic-extent args))
  (destructuring-bind (&key image-url label alternative-text display-string &allow-other-keys) args
    (%issue-command ("input" stream)
      (write-standard-input-type-args (stream query-name submit-button args 
					      :bindings (id class language direction title style events tab-index access-key))
        (%write-command-key-arg stream "value" (or label display-string "Submit"));display-string obsolete -- JCMa 9/4/2005  
	(cond-every
	  (image-url
	    (%write-command-key-arg stream "src" (maybe-coerce-uri image-url)))
	  (alternative-text
	    (%write-command-key-arg stream "alt" alternative-text)))))))


;;;------------------------------------------------------------------- 
;;;
;;; BUTTON
;;;

(define-input-type
  client-side-button
  :type-arg "button"
  :superclass implicit-label-input-type
  :lisp-type 'null)

(defmethod accept-input ((client-side-button client-side-button) query-name &rest args 
			 &key (stream *output-stream*) &allow-other-keys)
  (declare (dynamic-extent args))
  (destructuring-bind (&key image-url label display-string alternative-text &allow-other-keys) args
    (%issue-command ("input" stream)
      (write-standard-input-type-args (stream query-name client-side-button args 
					      :bindings (id class language direction title style events tab-index access-key))
        (%write-command-key-arg stream "value" (or label display-string "Button")) ;display-string obsolete -- JCMa 9/4/2005 
	(cond-every
	  (image-url
	    (%write-command-key-arg stream "src" (maybe-coerce-uri image-url)))
	  (alternative-text
	    (%write-command-key-arg stream "alt" alternative-text)))))))

;;;------------------------------------------------------------------- 
;;;
;;; FILE UPLOAD
;;;

(define-input-type
  file
  :type-arg "file"
  :superclass string+
  :lisp-type 'string)

;;; extra error checking AND encoding the directory into the query-name.
(defmethod accept-input ((file file) query-name &rest args &key (stream *output-stream*) &allow-other-keys)
  (declare (dynamic-extent args))
  (cond ((equal *open-form* '(:multipart :form-data)))
	((null *open-form*)
	 (error "~S ~S should be within a ~S." 'accept-input 'file 'with-fillout-form))
	(t (error "~S ~S requires the :ENCODING-TYPE of ~S to be ~S."
		  'accept-input 'file 'with-fillout-form '(:multipart :form-data) )))
  (with-slots (default-size default-max-size) file
    (destructuring-bind (&key size max-length accept-media-types (directory *file-upload-default-directory*)
			      content-type &allow-other-keys) args
      (let ((directory-query-name (file-upload-make-query query-name directory)))
	(declare (dynamic-extent directory-query-name))
	(when (and content-type (null accept-media-types));display-string obsolete -- JCMa 9/5/2005 
	  (setq accept-media-types content-type))
	(%issue-command ("input" stream)
	  (write-standard-input-type-args (stream directory-query-name file args 
						  :bindings (default disabled id class language direction
								     title style events tab-index access-key))
	    (let ((local-size (or size default-size))
		  (max (or max-length default-max-size)))
	      (cond-every
		(local-size
		  (%write-command-key-arg stream "size" local-size))
		(max
		  (unless (< max 1024.)
		    (error "String fields cannot exceed 1024 characters in HMTL 4.01"))
		  (%write-command-key-arg stream "maxlength" max))
		(accept-media-types
		  (%write-accept-media-type-argument stream accept-media-types))))))))))

;;;------------------------------------------------------------------- 
;;;
;;; SCRIPTING
;;;

(define-macro without-script-capability ((&key (stream '*output-stream*)) &body body)
  "Enables alternate HTML to be supplied within BODY for browsers not executing scripts.
This is useful when clients are configured not to execute scripts or the browsers do
not support the scripting language."
  `(%with-environment ("noscript" :stream ,stream) () ,@body))

(define-macro with-javascript-hidden ((&key (stream '*output-stream*)) &body body)
  "Wraps an HTML comment around a JavaScript emitted within BODY to STREAM
in order to hide it from old browsers that might be confused."
  `(with-comment (:stream ,stream)
     (write-string "hide script contents from old browsers" ,stream)
     (terpri ,stream)
     ,@body
     (fresh-line ,stream)
     (write-string "// end hiding contents from old browsers" ,stream)))

(defun %write-with-script-arguments (stream media-type script-location no-content charset language)
  (%write-media-type-argument stream "type" media-type)
  (cond-every
   (script-location
    (%write-command-key-arg stream "src" (maybe-coerce-uri script-location)))
   (no-content
    (%write-command-key-arg stream "defer"))
   (charset
    (%write-charset-argument stream charset))
   (language
    (with-deprecation-checking (with-script :argument language)
      (%write-command-key-arg stream "language" (etypecase language
                                                  (keyword (ns2.0::get-script-language language))
                                                  (string language)))))))

;; Renamed from with-embedded-script in Netscape 2
;; By default SCRIPT content is no longer hidden in comments. All modern browsers (2009, RJ) understand the SCRIPT tag.
(define-macro with-script ((media-type &key script-location hide-javascript no-content charset language (stream '*output-stream*)) &body body)
  "Provides an environment for emitting a client-side script with MEDIA-TYPE on STREAM.

MEDIA-TYPE is required and must be the content type for the scripting language, like:

     (:text :javascript)  -- JavaScript script
     (:text :tcl)         -- TCL script
     (:text :vbscript)    -- Visual Basic script

MEDIA-TYPE overrides any default scripting language declarations in the document preamble.

BODY executes code that emit the script the source on STREAM, or alternatively
SCRIPT-LOCATION may used to designate a URI that loads the script text into the client.
HIDE-JAVASCRIPT (T or NIL (default)) puts the script content in comments. Useful only for old browsers
who don't understand the script tag.

NO-CONTENT is a boolean that informs the browser that the script emitted within BODY or
loaded from SCRIPT-LOCATION will not display content, and so, the browser may handle
this asynchronously and continue rendering the document.

CHARSET is the character encoding of the script text (see ISO 10646).

LANGUAGE (deprecated) is keyword or string identifying the scripting language."
  `(%with-environment
       ("script" :stream ,stream)
       (%write-with-script-arguments ,stream ,media-type ,script-location ,no-content ,charset ,language)
     (flet ((%script-writer (stream) (declare (ignorable stream)) ,@body))
       (declare (dynamic-extent #'%script-writer))
       (if ,hide-javascript
           (progn
             (with-javascript-hidden (:stream ,stream)
               (%script-writer ,stream))
             (terpri ,stream))
         (%script-writer ,stream)))))

(define declare-document-script-type (media-type &optional (stream *output-stream*))
  "Declares the default scripting language type for a document to be MEDIA-TYPE.  Use
only inside the document preamble. In the absense of this declaration, the default
scripting language can be set by a CONTENT-SCRIPT-TYPE HTTP header."
  (flet ((writer (stream)
	   (%write-media-type media-type stream)))
    (declare (dynamic-extent #'writer))
    (declare-meta-info #'writer :header :content-script-type :stream stream)))

;;;------------------------------------------------------------------- 
;;;
;;; ISO 8859/1 and ISO 10646 CHARACTERS
;;;

(defstruct (iso-10646-entry (:print-function print-iso-10646-entry))
  (char-code 0 :type integer :read-only t)
  (char nil :type (or null character) :read-only t)
  (name nil :type string :read-only t) ; name string
  (keywords nil :type (cons keyword) :read-only t) ; keyword
  (descriptions nil :type (cons string) :read-only t)) ;description

(defun print-iso-10646-entry (entry stream depth)
  (declare (ignore depth))
  (print-unreadable-object (entry stream :type t :identity t)
    (write (iso-10646-entry-keywords entry) :stream stream :escape nil)))

(define-variable *iso-10646-readtable* nil
                 "Holds the readtable for ISO 10646 characters.")

(define-variable *iso-10646-vector* nil
                 "Holds a vector for ISO 10646 characters sorted by char-code.")

(defun %error-on-new-intern-10646-character (string &optional (start 0) (end (length string)))
  (declare (fixnum start end))
  (error "The iso-10646-character-name tokenizer is locked: ~A cannot be added." (subseq string start end)))

(tk1:define-tokenizer iso-10646-character-name
                      :tokenizer '%error-on-new-intern-10646-character
                      :test 'char-equal
                      :definer define
                      :documentation "Tokenizes ISO 10646 character names to char-codes.")

(defun %define-iso-10646-readtable (spec &aux (keyword-package http::*keyword-package*))
  (flet ((initialize-entry (readtable tokenizer char-code char name descriptions)
           (let* ((keywords (loop for desc in descriptions
                                  collect (html2::%keyword-for-iso-character-description desc keyword-package)))
                  (entry (make-iso-10646-entry :char-code char-code :char char :name name :keywords keywords :descriptions descriptions)))
             (loop for key in keywords
                   do (setf (get key :iso-character-index) char-code))
             (setf (gethash char-code readtable) entry)
             (when name
               (tk1:insert-token tokenizer entry name))
             entry)))
    (declare (dynamic-extent #'initialize-entry))
    (loop with size = (length spec)
          with iso-readtable = (or *iso-10646-readtable*
                                   (and *iso-10646-readtable* (clrhash *iso-10646-readtable*))
                                   (setq *iso-10646-readtable* (make-hash-table :test #'eql :size size)))
          with iso-vector = (or *iso-10646-vector*
                                (if (and *iso-10646-vector* (= (length *iso-10646-vector*) size))
                                    *iso-10646-vector*
                                  (setq *iso-10646-vector* (make-array size :initial-element nil))))
          with iso-name-tokenizer = (prog1 *iso-10646-character-name-tokenizer*
                                      (tk1:clear-tokenizer *iso-10646-character-name-tokenizer*))
          for (indices . descriptions) in spec
          for idx fixnum upfrom 0
          for entry = (etypecase indices
                        (integer
                         (initialize-entry iso-readtable iso-name-tokenizer indices nil nil descriptions))
                        (cons
                         (destructuring-bind (idx &rest names) indices
                           (initialize-entry 
                            iso-readtable 
                            iso-name-tokenizer
                            idx 
                            (find-if #'characterp names)
                            (find-if #'stringp names)
                            descriptions))))
          do (setf (aref iso-vector idx) entry)
          finally (return iso-readtable))))

(define-macro define-iso-10646-readtable (&body entries)
  "Toplevel method for defining the ISO 10646 readtable."
  `(%define-iso-10646-readtable ',entries))

(declaim (notinline valid-iso-10646-character-code-p))

(defun valid-iso-10646-character-code-p (char-code)
  (and (integerp char-code)
       (gethash char-code *iso-10646-readtable*)))

(defun valid-iso-character-code-p (code)
  (or (html2::valid-iso-character-code-p code)
      (valid-iso-10646-character-code-p code)))

(declaim (inline check-iso-character-code))

(define check-iso-character-code (char-code)
  (unless (valid-iso-character-code-p char-code)
    (error "CHAR-CODE, ~S, is not a valid char-code for an ISO character." char-code)))

(defmacro with-iso-8859-1-dispatch ((char-code) iso-8859-1-form &body body)
  `(cond ((html2::valid-iso-character-code-p ,char-code)
          ,iso-8859-1-form)
         (t . ,body)))

(defmacro with-iso-10646-entry ((char-code &optional (entry-var 'entry)) &body body)
  `(let ((,entry-var (gethash ,char-code *iso-10646-readtable*)))
     (cond (,entry-var . ,body)
           (t (error "Unknown character code for ISO 10646 , ~D." ,char-code)))))

(define %iso-code-char (char-code &optional (error-p t) (iso-readtable html2::*iso-readtable*))
  (cond ((with-iso-8859-1-dispatch (char-code)
             (html2::iso-code-char char-code iso-readtable)
           (with-iso-10646-entry (char-code)
             (iso-10646-entry-char entry))))
        (error-p (error "No Lisp character for ISO character code, ~D." char-code))
        (t nil)))

(declaim (notinline iso-code-char))

(define iso-code-char (char-code &optional (iso-readtable html2::*iso-readtable*))
  (or (%iso-code-char char-code nil iso-readtable)
      (error "No ISO 10646 character for the character code, ~D." char-code)))

(define iso-character-name (char-code &optional (iso-readtable html2::*iso-readtable*))
  (with-iso-8859-1-dispatch (char-code)
      (html2::iso-character-name char-code iso-readtable)
    (with-iso-10646-entry (char-code)
      (iso-10646-entry-name entry))))

(define iso-character-keyword (char-code &optional (iso-readtable html2::*iso-readtable*))
  (with-iso-8859-1-dispatch (char-code)
      (html2::iso-character-keyword char-code iso-readtable)
    (with-iso-10646-entry (char-code)
      (car (iso-10646-entry-keywords entry)))))

(define iso-character-description (char-code &optional (iso-readtable html2::*iso-readtable*))
  (with-iso-8859-1-dispatch (char-code)
      (html2::iso-character-description char-code iso-readtable)
    (with-iso-10646-entry (char-code)
      (car (iso-10646-entry-descriptions entry)))))

(define iso-character-code-from-character (char &optional (iso-readtable html2::*iso-readtable*))
  (let ((char-code (char-code char)))
    (with-iso-8859-1-dispatch (char-code)
        (html2::iso-character-code-from-character char iso-readtable)
      (let ((entry (gethash char-code *iso-10646-readtable*)))
        (cond ((and entry (iso-10646-entry-char-code entry)))
              (entry (error "No ISO 10646 character code or the character, ~S." char))
              (t (error "The character, ~S (~D) is not an ISO 10646 character." char char-code)))))))

(define iso-character-code-from-name (string &optional (start 0) (end (length string)) error-p)
  (let ((entry (tk1:get-token *iso-10646-character-name-tokenizer* string start end)))
    (cond (entry
           (iso-10646-entry-char-code entry))
          (error-p (error "NAME, ~S, is not an ISO character name." (subseq string start end)))
          (t nil))))

(define write-iso-character (char-code &optional (stream *standard-output*))
  "Writes the ISO character denoted by CHAR-CODE on STREAM."
  (let ((name (iso-character-name char-code)))
    (write-char #\& stream)
    (cond (name
           (write-string name stream))
          (t (write-char #\# stream)
             (write char-code :stream stream :escape nil :base 10.)))
    (write-char #\; stream)))

(declaim (notinline write-iso-character-for-name))

(define write-iso-character-for-name (name &optional (stream *standard-output*))
  "Writes the ISO character denoted by NAME on STREAM."
  (write-iso-character (iso-character-code-from-name name 0 (length name) t) stream))

(declaim (inline write-iso-character-for-keyword))

(define write-iso-character-for-keyword (keyword &optional (stream *standard-output*))
  "Writes the ISO character denoted by KEYWORD on STREAM."
  (write-iso-character (iso-character-code-from-keyword keyword t) stream))

(define parse-iso-character (string &optional (start 0) (end (length string)))
  "Parses STRING as the textual representation of an ISO character and returns an ISO character code.
It assumes that the character name or code are delimited by & and ;."
  (declare (fixnum start end)
           (values char-code))
  (flet ((get-char-code (start end)
           (declare (fixnum start end))
           (let ((s1 (1+ start)))
             (case (aref string s1)
               (#\#
                (let ((code (parse-integer string :start (+ 2 start) :end end :radix 10.)))
                  (when (valid-iso-character-code-p code)
                    code)))
               (t (iso-character-code-from-name string s1 end t))))))
    ;;   (declare (inline get-char-code))
    (let ((amp-pos (http::char-position #\& string start end))
          (semi-pos (http::char-position #\; string start end)))
      (cond ((and amp-pos semi-pos)
             (or (get-char-code amp-pos semi-pos)
                 (error "Parse ISO Character: ~S, cannot be parsed into an ISO character code." (subseq string start end))))
            (amp-pos
             (error "Parse ISO Character: No & prefix found in, ~S." (subseq string start end)))
            (semi-pos
             (error "Parse ISO Character: No ; suffix found in, ~S." (subseq string start end)))
            (t (error "Parse ISO Character: No character delimiter found in, ~S." (subseq string start end)))))))

(define apropos-iso-character (substring &optional (type :keyword))
  "Searches for the names of ISO characters containing substring.
Type can be:
              :NAME defined names for characters
              :keyword keyword standing for characters.
              :DESCRIPTION the description strings for characters."
  (flet ((check-name (substring entry)
           (let ((name (iso-10646-entry-name entry)))
             (when (http::string-search substring name)
               (list name))))
         (check-keywords (substring entry)
           (loop for key in (iso-10646-entry-keywords entry)
                 when (http::string-search substring (symbol-name key))
                 collect key))
         (check-descriptions (substring entry)
           (loop for desc in (iso-10646-entry-descriptions entry)
                 when (http::string-search substring desc)
                 collect desc)))
    (nconc (html2::apropos-iso-character substring type)
           (let ((vector *iso-10646-vector*))
             (with-fast-array-references ((vector vector vector))
               (loop with fctn = (ecase type
                                   (:name #'check-name)                                 
                                   (:keyword #'check-keywords)
                                   (:description #'check-descriptions))
                     for idx fixnum upfrom 0 below (length vector)
                     for item = (let ((entry (aref vector idx)))
                                  (funcall fctn substring entry))
                     when item
                     nconc item))))))

(defun describe-iso-character-entry (entry stream)
  (let ((char-code (iso-10646-entry-char-code entry))
        (name (iso-10646-entry-name entry))
        (keywords (iso-10646-entry-keywords entry))
        (descriptions (iso-10646-entry-descriptions entry))
        (char (iso-10646-entry-char entry)))
    (format stream "~&~3,,D~:[~;~:*~5T~C~]~:[~;~:*~8T~A~]~20T~{~S~^, ~}~80T~{~A~^, ~}"
            char-code char name keywords descriptions)))

(define describe-iso-character (char-code &optional (stream *standard-output*) (iso-readtable html2::*iso-readtable*))
  (with-iso-8859-1-dispatch (char-code)
      (html2::describe-iso-character char-code stream iso-readtable)
    (with-iso-10646-entry (char-code)
      (describe-iso-character-entry entry stream))))

(define describe-iso-readtable (&optional (stream *standard-output*))
  (html2::describe-iso-readtable stream)
  (let ((vector *iso-10646-vector*))
    (with-fast-array-references ((vector vector vector))
      (loop for idx fixnum upfrom 0 below (length vector)
            for entry = (aref vector idx)
            do (describe-iso-character-entry entry stream)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun %%iso-code-char (code)
  #+lispworks6(code-char code)
  #-lispworks6 nil))

;;;------------------------------------------------------------------- 
;;;
;;; NUMERICAL CHARACTER REFERENCES FOR ISO 10646 CHARACTER ENTITIES
;;;
;;;

#|
(define-iso-10646-readtable
 ;; C0 Controls and Basic Latin
 ((34 "quot")   "quotation mark" "APL quote") ;U+0022 ISOnum
 ((38 "amp")   "ampersand") ;U+0026 ISOnum
 ((60 "lt")   "less-than sign") ;U+003C ISOnum
 ((62 "gt")   "greater-than sign") ;U+003E ISOnum
 ;; Latin Extended-B
 ((402 "fnof")  "latin small f with hook" "function" "florin") ;U+0192 ISOtech
 ;; Latin Extended-A
 ((338 "OElig")  "latin capital ligature OE") ;U+0152 ISOlat2
 ((339 "oelig")  "latin small ligature oe") ;U+0153 ISOlat2
 ;; ligature is a misnomer, this is a separate character in some languages
 ((352 "Scaron")  "latin capital letter S with caron") ;U+0160 ISOlat2
 ((353 "scaron")  "latin small letter s with caron") ;U+0161 ISOlat2
 ((376 "Yuml")  "latin capital letter Y with diaeresis") ;U+0178 ISOlat2
 ;; Spacing Modifier Letters
 ((710 "circ")  "modifier letter circumflex accent") ;U+02C6 ISOpub
 ((732 "tilde")  "small tilde") ;U+02DC ISOdia
 ;; Greek
 ((913 "Alpha")  "greek capital letter alpha") ;U+0391
 ((914 "Beta")   "greek capital letter beta") ;U+0392
 ((915 "Gamma")  "greek capital letter gamma") ;U+0393 ISOgrk3
 ((916 "Delta")  "greek capital letter delta") ;U+0394 ISOgrk3
 ((917 "Epsilon")"greek capital letter epsilon") ;U+0395
 ((918 "Zeta")   "greek capital letter zeta") ;U+0396
 ((919 "Eta")    "greek capital letter eta") ;U+0397
 ((920 "Theta")  "greek capital letter theta") ;U+0398 ISOgrk3
 ((921 "Iota")   "greek capital letter iota") ;U+0399
 ((922 "Kappa")  "greek capital letter kappa") ;U+039A
 ((923 "Lambda") "greek capital letter lambda") ;U+039B ISOgrk3
 ((924 "Mu")     "greek capital letter mu") ;U+039C
 ((925 "Nu")     "greek capital letter nu") ;U+039D
 ((926 "Xi")     "greek capital letter xi") ;U+039E ISOgrk3
 ((927 "Omicron")"greek capital letter omicron") ;U+039F
 ((928 "Pi")     "greek capital letter pi") ;U+03A0 ISOgrk3
 ((929 "Rho")    "greek capital letter rho") ;U+03A1
 ;; there is no Sigmaf, and no U+03A2 character either
 ((931 "Sigma")  "greek capital letter sigma") ;U+03A3 ISOgrk3
 ((932 "Tau")    "greek capital letter tau") ;U+03A4
 ((933 "Upsilon")"greek capital letter upsilon") ;U+03A5 ISOgrk3
 ((934 "Phi")    "greek capital letter phi") ;U+03A6 ISOgrk3
 ((935 "Chi")    "greek capital letter chi") ;U+03A7
 ((936 "Psi")    "greek capital letter psi") ;U+03A8 ISOgrk3
 ((937 "Omega")  "greek capital letter omega") ;U+03A9 ISOgrk3
 ((945 "alpha")   "greek small letter alpha") ;U+03B1 ISOgrk3
 ((946 "beta")    "greek small letter beta") ;U+03B2 ISOgrk3
 ((947 "gamma")   "greek small letter gamma") ;U+03B3 ISOgrk3
 ((948 "delta")   "greek small letter delta") ;U+03B4 ISOgrk3
 ((949 "epsilon") "greek small letter epsilon") ;U+03B5 ISOgrk3
 ((950 "zeta")    "greek small letter zeta") ;U+03B6 ISOgrk3
 ((951 "eta")     "greek small letter eta") ;U+03B7 ISOgrk3
 ((952 "theta")   "greek small letter theta") ;U+03B8 ISOgrk3
 ((953 "iota")    "greek small letter iota") ;U+03B9 ISOgrk3
 ((954 "kappa")   "greek small letter kappa") ;U+03BA ISOgrk3
 ((955 "lambda")  "greek small letter lambda") ;U+03BB ISOgrk3
 ((956 "mu")      "greek small letter mu") ;U+03BC ISOgrk3
 ((957 "nu")      "greek small letter nu") ;U+03BD ISOgrk3
 ((958 "xi")      "greek small letter xi") ;U+03BE ISOgrk3
 ((959 "omicron") "greek small letter omicron") ;U+03BF NEW
 ((960 "pi")      "greek small letter pi") ;U+03C0 ISOgrk3
 ((961 "rho")     "greek small letter rho") ;U+03C1 ISOgrk3
 ((962 "sigmaf")  "greek small letter final sigma") ;U+03C2 ISOgrk3
 ((963 "sigma")   "greek small letter sigma") ;U+03C3 ISOgrk3
 ((964 "tau")     "greek small letter tau") ;U+03C4 ISOgrk3
 ((965 "upsilon") "greek small letter upsilon") ;U+03C5 ISOgrk3
 ((966 "phi")     "greek small letter phi") ;U+03C6 ISOgrk3
 ((967 "chi")     "greek small letter chi") ;U+03C7 ISOgrk3
 ((968 "psi")     "greek small letter psi") ;U+03C8 ISOgrk3
 ((969 "omega")   "greek small letter omega") ;U+03C9 ISOgrk3
 ((977 "thetasym")"greek small letter theta symbol") ;U+03D1 NEW
 ((978 "upsih")   "greek upsilon with hook symbol") ;U+03D2 NEW
 ((982 "piv")     "greek pi symbol") ;U+03D6 ISOgrk3
 ;; General Punctuation
 ((8194 "ensp")   "en space") ;U+2002 ISOpub
 ((8195 "emsp")   "em space") ;U+2003 ISOpub
 ;; General Punctuation
 ((8201 "thinsp")  "thin space") ;U+2009 ISOpub
 ((8204 "zwnj")  "zero width non-joiner") ;U+200C NEW RFC 2070
 ((8205 "zwj")  "zero width joiner") ;U+200D NEW RFC 2070
 ((8206 "lrm")  "left-to-right mark") ;U+200E NEW RFC 2070
 ((8207 "rlm")  "right-to-left mark") ;U+200F NEW RFC 2070
 ((8211 "ndash")  "en dash") ;U+2013 ISOpub
 ((8212 "mdash")  "em dash") ;U+2014 ISOpub
 ((8216 "lsquo")  "left single quotation mark") ;U+2018 ISOnum
 ((8217 "rsquo")  "right single quotation mark") ;U+2019 ISOnum
 ((8218 "sbquo")  "single low-9 quotation mark") ;U+201A NEW
 ((8220 "ldquo")  "left double quotation mark") ;U+201C ISOnum
 ((8221 "rdquo")  "right double quotation mark") ;U+201D ISOnum
 ((8222 "bdquo")  "double low-9 quotation mark") ;U+201E NEW
 ((8224 "dagger")  "dagger") ;U+2020 ISOpub
 ((8225 "Dagger")  "double dagger") ;U+2021 ISOpub
 ((8226 "bull")  "bullet" "black small circle") ;U+2022 ISOpub
 ;; bullet is NOT the same as bullet operator, U+2219
 ((8230 "hellip")  "horizontal ellipsis" "three dot leader") ;U+2026 ISOpub
 ((8240 "permil")  "per mille sign") ;U+2030 ISOtech
 ((8242 "prime")  "prime" "minutes" "feet") ;U+2032 ISOtech
 ((8243 "Prime")  "double prime" "seconds" "inches") ;U+2033 ISOtech
 ((8249 "lsaquo")  "single left-pointing angle quotation mark") ;U+2039 ISO proposed
 ;; lsaquo is proposed but not yet ISO standardized
 ((8250 "rsaquo")  "single right-pointing angle quotation mark") ;U+203A ISO proposed
 ((8254 "oline")  "overline" "spacing overscore") ;U+203E NEW
 ((8260 "frasl")  "fraction slash") ;U+2044 NEW
 ;; rsaquo is proposed but not yet ISO standardized
 ((8364 "euro")  "euro sign") ;U+20AC NEW
 ;; Letterlike Symbols
 ((8472 "weierp")  "script capital P" "power" "Weierstrass p") ;U+2118 ISOamso
 ((8465 "image")  "blackletter capital I" "imaginary part") ;U+2111 ISOamso
 ((8476 "real")  "blackletter capital R" "real part symbol") ;U+211C ISOamso
 ((8482 "trade")  "trade mark sign") ;U+2122 ISOnum
 ((8501 "alefsym")  "alef symbol" "first transfinite cardinal") ;U+2135 NEW
 ;; alef symbol is NOT the same as hebrew letter alef, U+05D0 although the same glyph could be used to depict both characters
 ;; Arrows
 ((8592 "larr")  "leftwards arrow") ;U+2190 ISOnum
 ((8593 "uarr")  "upwards arrow") ;U+2191 ISOnum
 ((8594 "rarr")  "rightwards arrow") ;U+2192 ISOnum
 ((8595 "darr")  "downwards arrow") ;U+2193 ISOnum
 ((8596 "harr")  "left right arrow") ;U+2194 ISOamsa
 ((8629 "crarr")  "downwards arrow with corner leftwards" "carriage return") ;U+21B5 NEW
 ((8656 "lArr")  "leftwards double arrow") ;U+21D0 ISOtech
 ;; ISO 10646 does not say that lArr is the same as the 'is implied by' arrow but also does not have 
 ;; any other character for that function. So ? lArr can be used for 'is implied by' as ISOtech suggests
 ((8657 "uArr")  "upwards double arrow") ;U+21D1 ISOamsa
 ((8658 "rArr")  "rightwards double arrow") ;U+21D2 ISOtech
 ;; ISO 10646 does not say this is the 'implies' character but does not have
 ;; another character with this function so ? rArr can be used for 'implies' as ISOtech suggests
 ((8659 "dArr")  "downwards double arrow") ;U+21D3 ISOamsa
 ((8660 "hArr")  "left right double arrow") ;U+21D4 ISOamsa

 ;; Mathematical Operators
 ((8704 "forall")  "for all") ;U+2200 ISOtech
 ((8706 "part")  "partial differential") ;U+2202 ISOtech
 ((8707 "exist")  "there exists") ;U+2203 ISOtech
 ((8709 "empty")  "empty set" "null set" "diameter") ;U+2205 ISOamso
 ((8711 "nabla")  "nabla" "backward difference") ;U+2207 ISOtech
 ((8712 "isin")  "element of") ;U+2208 ISOtech
 ((8713 "notin")  "not an element of") ;U+2209 ISOtech
 ((8715 "ni")  "contains as member") ;U+220B ISOtech
 ;; should there be a more memorable name than 'ni'?
 ((8719 "prod")  "n-ary product" "product sign") ;U+220F ISOamsb
 ;; prod is NOT the same character as U+03A0 'greek capital letter pi' though the same glyph might be used for both
 ((8721 "sum")  "n-ary sumation") ;U+2211 ISOamsb
 ;; sum is NOT the same character as U+03A3 'greek capital letter sigma' though the same glyph might be used for both
 ((8722 "minus")  "minus sign") ;U+2212 ISOtech
 ((8727 "lowast")  "asterisk operator") ;U+2217 ISOtech
 ((8730 "radic")  "square root" "radical sign") ;U+221A ISOtech
 ((8733 "prop")  "proportional to") ;U+221D ISOtech
 ((8734 "infin")  "infinity") ;U+221E ISOtech
 ((8736 "ang")  "angle") ;U+2220 ISOamso
 ((8743 "and")  "logical and" "wedge") ;U+2227 ISOtech
 ((8744 "or")  "logical or" "vee") ;U+2228 ISOtech
 ((8745 "cap")  "intersection" "cap") ;U+2229 ISOtech
 ((8746 "cup")  "union" "cup") ;U+222A ISOtech
 ((8747 "int")  "integral") ;U+222B ISOtech
 ((8756 "there4")  "therefore") ;U+2234 ISOtech
 ((8764 "sim")  "tilde operator" "varies with" "similar to") ;U+223C ISOtech
 ;; tilde operator is NOT the same character as the tilde, 
 ;; U+007E, although the same glyph might be used to represent both
 ((8773 "cong")  "approximately equal to") ;U+2245 ISOtech
 ((8776 "asymp")  "almost equal to" "asymptotic to") ;U+2248 ISOamsr
 ((8800 "ne")  "not equal to") ;U+2260 ISOtech
 ((8801 "equiv")  "identical to") ;U+2261 ISOtech
 ((8804 "le")  "less-than or equal to") ;U+2264 ISOtech
 ((8805 "ge")  "greater-than or equal to") ;U+2265 ISOtech
 ((8834 "sub")  "subset of") ;U+2282 ISOtech
 ((8835 "sup")  "superset of") ;U+2283 ISOtech
 ;; note that nsup, 'not a superset of, U+2283' is not covered by the Symbol
 ;; font encoding and is not included. Should it be, for symmetry? It is in ISOamsn
 ((8836 "nsub")  "not a subset of") ;U+2284 ISOamsn
 ((8838 "sube")  "subset of or equal to") ;U+2286 ISOtech
 ((8839 "supe")  "superset of or equal to") ;U+2287 ISOtech
 ((8853 "oplus")  "circled plus" "direct sum") ;U+2295 ISOamsb
 ((8855 "otimes")  "circled times" "vector product") ;U+2297 ISOamsb
 ((8869 "perp")  "up tack" "orthogonal to" "perpendicular") ;U+22A5 ISOtech
 ((8901 "sdot")  "dot operator") ;U+22C5 ISOamsb
 ;; dot operator is NOT the same character as U+00B7 middle dot
 ;; Miscellaneous Technical
 ((8968 "lceil")  "left ceiling" "apl upstile") ;U+2308 ISOamsc
 ((8969 "rceil")  "right ceiling") ;U+2309 ISOamsc
 ((8970 "lfloor")  "left floor" "apl downstile") ;U+230A ISOamsc
 ((8971 "rfloor")  "right floor") ;U+230B ISOamsc
 ((9001 "lang")  "left-pointing angle bracket" "bra") ;U+2329 ISOtech
 ;; lang is NOT the same character as U+003C 'less than' or U+2039 'single left-pointing angle quotation mark'
 ((9002 "rang")  "right-pointing angle bracket" "ket") ;U+232A ISOtech
 ;; rang is NOT the same character as U+003E 'greater than' or U+203A 'single right-pointing angle quotation mark'
 ;; Geometric Shapes
 ((9674 "loz")  "lozenge") ;U+25CA ISOpub
 ;; Miscellaneous Symbols
 ((9824 "spades")  "black spade suit") ;U+2660 ISOpub
 ;; black here seems to mean filled as opposed to hollow
 ((9827 "clubs")  "black club suit" "shamrock") ;U+2663 ISOpub
 ((9829 "hearts")  "black heart suit" "valentine") ;U+2665 ISOpub
 ((9830 "diams")  "black diamond suit")) ;U+2666 ISOpub |#

(define-iso-10646-readtable
  ;; C0 Controls and Basic Latin
  ((34 #\" "quot")   "quotation mark" "APL quote") ;U+0022 ISOnum
  ((38 #\& "amp")   "ampersand") ;U+0026 ISOnum
  ((60 #\< "lt")   "less-than sign") ;U+003C ISOnum
  ((62 #\> "gt")   "greater-than sign") ;U+003E ISOnum

  ;; Latin Extended-A
  ((338  #.(%%iso-code-char 338)  "OElig") "latin capital ligature OE")
  ((339  #.(%%iso-code-char 339)  "oelig") "latin small ligature oe")
  ;; ligature is a misnomer, this is a separate character in some languages
  ((352  #.(%%iso-code-char 352)  "Scaron") "latin capital letter S with caron")
  ((353  #.(%%iso-code-char 353)  "scaron") "latin small letter s with caron")
  ((376  #.(%%iso-code-char 376)  "Yuml") "latin capital letter Y with diaeresis")
  ;; add additional characters from HTML 4.0.1 spec -- JCMa 12/3/2010
  ;; Latin Extended-B
  ((402  #.(%%iso-code-char 402)  "fnof") "latin small f with hook") ; = function = florin
  ;; Spacing Modifier Letters
  ((710  #.(%%iso-code-char 710)  "circ") "modifier letter circumflex accent")
  ((732  #.(%%iso-code-char 732)  "tilde") "small tilde")
  ;; Greek
  ((913  #.(%%iso-code-char 913)  "Alpha") "greek capital letter alpha")
  ((914  #.(%%iso-code-char 914)  "Beta") "greek capital letter beta")
  ((915  #.(%%iso-code-char 915)  "Gamma") "greek capital letter gamma")
  ((916  #.(%%iso-code-char 916)  "Delta") "greek capital letter delta")
  ((917  #.(%%iso-code-char 917)  "Epsilon") "greek capital letter epsilon")
  ((918  #.(%%iso-code-char 918)  "Zeta") "greek capital letter zeta")
  ((919  #.(%%iso-code-char 919)  "Eta") "greek capital letter eta")
  ((920  #.(%%iso-code-char 920)  "Theta") "greek capital letter theta")
  ((921  #.(%%iso-code-char 921)  "Iota") "greek capital letter iota")
  ((922  #.(%%iso-code-char 922)  "Kappa") "greek capital letter kappa")
  ((923  #.(%%iso-code-char 923)  "Lambda") "greek capital letter lambda")
  ((924  #.(%%iso-code-char 924)  "Mu") "greek capital letter mu")
  ((925  #.(%%iso-code-char 925)  "Nu") "greek capital letter nu")
  ((926  #.(%%iso-code-char 926)  "Xi") "greek capital letter xi")
  ((927  #.(%%iso-code-char 927)  "Omicron") "greek capital letter omicron")
  ((928  #.(%%iso-code-char 928)  "Pi") "greek capital letter pi")
  ((929  #.(%%iso-code-char 929)  "Rho") "greek capital letter rho")
  ;; there is no Sigmaf, and no U+03A2 character either
  ((931  #.(%%iso-code-char 931)  "Sigma") "greek capital letter sigma")
  ((932  #.(%%iso-code-char 932)  "Tau") "greek capital letter tau")
  ((933  #.(%%iso-code-char 933)  "Upsilon") "greek capital letter upsilon")
  ((934  #.(%%iso-code-char 934)  "Phi") "greek capital letter phi")
  ((935  #.(%%iso-code-char 935)  "Chi") "greek capital letter chi")
  ((936  #.(%%iso-code-char 936)  "Psi") "greek capital letter psi")
  ((937  #.(%%iso-code-char 937)  "Omega") "greek capital letter omega")
  ((945  #.(%%iso-code-char 945)  "alpha") "greek small letter alpha")
  ((946  #.(%%iso-code-char 946)  "beta") "greek small letter beta")
  ((947  #.(%%iso-code-char 947)  "gamma") "greek small letter gamma")
  ((948  #.(%%iso-code-char 948)  "delta") "greek small letter delta")
  ((949  #.(%%iso-code-char 949)  "epsilon") "greek small letter epsilon")
  ((950  #.(%%iso-code-char 950)  "zeta") "greek small letter zeta")
  ((951  #.(%%iso-code-char 951)  "eta") "greek small letter eta")
  ((952  #.(%%iso-code-char 952)  "theta") "greek small letter theta")
  ((953  #.(%%iso-code-char 953)  "iota") "greek small letter iota")
  ((954  #.(%%iso-code-char 954)  "kappa") "greek small letter kappa")
  ((955  #.(%%iso-code-char 955)  "lambda") "greek small letter lambda")
  ((956  #.(%%iso-code-char 956)  "mu") "greek small letter mu")
  ((957  #.(%%iso-code-char 957)  "nu") "greek small letter nu")
  ((958  #.(%%iso-code-char 958)  "xi") "greek small letter xi")
  ((959  #.(%%iso-code-char 959)  "omicron") "greek small letter omicron")
  ((960  #.(%%iso-code-char 960)  "pi") "greek small letter pi")
  ((961  #.(%%iso-code-char 961)  "rho") "greek small letter rho")
  ((962  #.(%%iso-code-char 962)  "sigmaf") "greek small letter final sigma")
  ((963  #.(%%iso-code-char 963)  "sigma") "greek small letter sigma")
  ((964  #.(%%iso-code-char 964)  "tau") "greek small letter tau")
  ((965  #.(%%iso-code-char 965)  "upsilon") "greek small letter upsilon")
  ((966  #.(%%iso-code-char 966)  "phi") "greek small letter phi")
  ((967  #.(%%iso-code-char 967)  "chi") "greek small letter chi")
  ((968  #.(%%iso-code-char 968)  "psi") "greek small letter psi")
  ((969  #.(%%iso-code-char 969)  "omega") "greek small letter omega")
  ((977  #.(%%iso-code-char 977)  "thetasym") "greek small letter theta symbol")
  ((978  #.(%%iso-code-char 978)  "upsih") "greek upsilon with hook symbol")
  ((982  #.(%%iso-code-char 982)  "piv") "greek pi symbol")
  ;; General Punctuation
  ((8194                    "ensp") "en space")
  ((8195                    "emsp") "em space")
  ((8201                    "thinsp") "thin space")
  ((8204                    "zwnj") "zero width non-joiner")
  ;; U+200C NEW RFC 2070
  ((8205      "zwj") "zero width joiner")
  ((8206      "lrm") "left-to-right mark")
  ((8207      "rlm") "right-to-left mark")
  ((8211  #.(%%iso-code-char 8211)  "ndash") "en dash")
  ((8212  #.(%%iso-code-char 8212)  "mdash") "em dash")
  ((8216  #.(%%iso-code-char 8216)  "lsquo") "left single quotation mark")
  ((8217  #.(%%iso-code-char 8217)  "rsquo") "right single quotation mark")
  ((8218  #.(%%iso-code-char 8218)  "sbquo") "single low-9 quotation mark")
  ((8220  #.(%%iso-code-char 8220)  "ldquo") "left double quotation mark")
  ((8221  #.(%%iso-code-char 8221)  "rdquo") "right double quotation mark")
  ((8222  #.(%%iso-code-char 8222)  "bdquo") "double low-9 quotation mark")
  ((8224  #.(%%iso-code-char 8224)  "dagger") "dagger")
  ((8225  #.(%%iso-code-char 8225)  "Dagger") "double dagger")
  ;; General Punctuation
  ((8226  #.(%%iso-code-char 8226)  "bull") "bullet = black small circle")
  ;; bullet is NOT the same as bullet operator, U+2219
  ((8230  #.(%%iso-code-char 8230)  "hellip") "horizontal ellipsis = three dot leader")
  ((8240  #.(%%iso-code-char 8240)  "permil") "per mille sign")
  ((8242  #.(%%iso-code-char 8242)  "prime") "prime = minutes = feet")
  ((8243  #.(%%iso-code-char 8243)  "Prime") "double prime = seconds = inches")
  ((8249  #.(%%iso-code-char 8249)  "lsaquo") "single left-pointing angle quotation mark")
  ;; U+2039 ISO proposed
  ;; lsaquo is proposed but not yet ISO standardized
  ((8250  #.(%%iso-code-char 8250)  "rsaquo") "single right-pointing angle quotation mark")
  ((8254  #.(%%iso-code-char 8254)  "oline") "overline = spacing overscore")
  ((8260  #.(%%iso-code-char 8260)  "frasl") "fraction slash")
  ;; U+203A ISO proposed
  ;; rsaquo is proposed but not yet ISO standardized
  ((8364  #.(%%iso-code-char 8364)  "euro") "euro sign")
  ;; Letterlike Symbols
  ((8465      "image") "blackletter capital I")
  ((8472      "weierp") "script capital P")
  ((8476      "real") "blackletter capital R")
  ((8482  #.(%%iso-code-char 8482)  "trade") "trade mark sign")
  ((8501      "alefsym") "alef symbol")
  ;; alef symbol is NOT the same as hebrew letter alef,
  ;; U+05D0 although the same glyph could be used to depict both characters -->
  ;; Arrows
  ((8592  #.(%%iso-code-char 8592)  "larr") "leftwards arrow")
  ((8593  #.(%%iso-code-char 8593)  "uarr") "upwards arrow")
  ((8594  #.(%%iso-code-char 8594)  "rarr") "rightwards arrow")
  ((8595  #.(%%iso-code-char 8595)  "darr") "downwards arrow")
  ((8596      "harr") "left right arrow")
  ((8629  #.(%%iso-code-char 8629)  "crarr") "downwards arrow with corner leftwards")
  ((8656      "lArr") "leftwards double arrow")
  ;; ISO 10646 does not say that lArr is the same as the 'is implied by' arrow
  ;; but also does not have any other character for that function. So ? lArr can
  ;; be used for 'is implied by' as ISOtech suggests
  ((8657      "uArr") "upwards double arrow")
  ((8658      "rArr") "rightwards double arrow")
  ;; ISO 10646 does not say this is the 'implies' character but does not have another character with this function so ?
  ;; rArr can be used for 'implies' as ISOtech suggests
  ((8659      "dArr") "downwards double arrow")
  ((8660      "hArr") "left right double arrow")
  ;; Mathematical Operators
  ((8704      "forall") "for all")
  ((8706  #.(%%iso-code-char 8706)  "part") "partial differential")
  ((8707      "exist") "there exists")
  ((8709  #.(%%iso-code-char 8709)  "empty") "empty set")
  ((8711  #.(%%iso-code-char 8711)  "nabla") "nabla")
  ((8712  #.(%%iso-code-char 8712)  "isin") "element of")
  ((8713  #.(%%iso-code-char 8713)  "notin") "not an element of")
  ((8715  #.(%%iso-code-char 8715)  "ni") "contains as member")
  ;; should there be a more memorable name than 'ni'?
  ((8719  #.(%%iso-code-char 8719)  "prod") "n-ary product")
  ;; prod is NOT the same character as U+03A0 'greek capital letter pi' though
  ;;  the same glyph might be used for both
  ((8721  #.(%%iso-code-char 8721)  "sum") "n-ary sumation")
  ((8722  #.(%%iso-code-char 8722)  "minus") "minus sign")
  ((8727  #.(%%iso-code-char 8727)  "lowast") "asterisk operator")
  ((8730  #.(%%iso-code-char 8730)  "radic") "square root = radical sign")
  ((8733  #.(%%iso-code-char 8733)  "prop") "proportional to")
  ((8734  #.(%%iso-code-char 8734)  "infin") "infinity")
  ((8736      "ang") "angle")
  ((8743  #.(%%iso-code-char 8743)  "and") "logical and")
  ((8744  #.(%%iso-code-char 8744)  "or") "logical or")
  ((8745  #.(%%iso-code-char 8745)  "cap") "intersection")
  ((8746  #.(%%iso-code-char 8746)  "cup") "union")
  ((8747  #.(%%iso-code-char 8747)  "int") "integral")
  ((8756  #.(%%iso-code-char 8756)  "there4") "therefore")
  ((8764  #.(%%iso-code-char 8764)  "sim") "tilde operator")
  ;; tilde operator is NOT the same character as the tilde, U+007E, although the same glyph might be used to represent both
  ((8773  #.(%%iso-code-char 8773)  "cong") "approximately equal to")
  ((8776  #.(%%iso-code-char 8776)  "asymp") "almost equal to")
  ((8800  #.(%%iso-code-char 8800)  "ne") "not equal to")
  ((8801  #.(%%iso-code-char 8801)  "equiv") "identical to")
  ((8804  #.(%%iso-code-char 8804)  "le") "less-than or equal to")
  ((8805  #.(%%iso-code-char 8805)  "ge") "greater-than or equal to")
  ((8834  #.(%%iso-code-char 8834)  "sub") "subset of")
  ((8835  #.(%%iso-code-char 8835)  "sup") "superset of")
  ;; note that nsup, 'not a superset of, U+2283' is not covered by the Symbol font encoding 
  ;; and is not included. Should it be, for symmetry It is in ISOamsn
  ((8836  #.(%%iso-code-char 8836)  "nsub") "not a subset of")
  ((8838  #.(%%iso-code-char 8838)  "sube") "subset of or equal to")
  ((8839  #.(%%iso-code-char 8839)  "supe") "superset of or equal to")
  ((8853  #.(%%iso-code-char 8853)  "oplus") "circled plus")
  ((8855  #.(%%iso-code-char 8855)  "otimes") "circled times")
  ((8869  #.(%%iso-code-char 8869)  "perp") "up tack")
  ((8901  #.(%%iso-code-char 8901)  "sdot") "dot operator")
  ;; dot operator is NOT the same character as U+00B7 middle dot
  ;; Miscellaneous Technical 
  ((8968      "lceil") "left ceiling")
  ((8969      "rceil") "right ceiling")
  ((8970      "lfloor") "left floor")
  ((8971      "rfloor") "right floor")
  ((9001      "lang") "left-pointing angle bracket")
  ;; lang is NOT the same character as U+003C 'less than' or U+2039 'single left-pointing angle quotation mark'
  ((9002      "rang") "right-pointing angle bracket")
  ;; rang is NOT the same character as U+003E 'greater than' or U+203A 'single right-pointing angle quotation mark'
  ;; Geometric Shapes
  ((9674  #.(%%iso-code-char 9674)  "loz") "lozenge")
  ;; Miscellaneous Symbols
  ((9824      "spades") "black spade suit")
  ;; black here seems to mean filled as opposed to hollow
  ((9827      "clubs") "black club suit")
  ((9829      "hearts") "black heart suit"))

;;;------------------------------------------------------------------- 
;;;
;;; DYNAMIC DOCUMENT LOOKS
;;;

(defclass document-look (http:property-list-mixin) 
  ()
  (:documentation "A document look for page generation."))

(defclass document-look-default (document-look) ())

(eval-when (:load-toplevel :compile-toplevel :execute)
(define-macro create-document-look (&rest args &key (class 'document-look-default)  &allow-other-keys)
  "Creates a document look with document class, CLASS.
Any additional keyword arguments, like STYLE-TYPE and STYLE-URL, are passed in."
  (flet ((clean-args (arglist)
           (loop for (key value) on arglist by #'cddr
                 unless (member key '(:class))
                 collect key and collect value)))
    `(make-instance ,class ,.(clean-args args)))))

(defvar *standard-document-look* (create-document-look :class 'document-look-default))

(define-generic user-document-look (user-or-server)
  (:documentation "Returns the document look for page generation to USER-OR-SERVER."))

;; Specialize this method for dynamic looks for specific users
(defmethod user-document-look (user) ;default method returns default look
  (declare (ignore user))
  *standard-document-look*)

(defmethod user-document-look ((server http::server-authentication-mixin))
  (user-document-look (http:server-user-object server)))

(define-generic write-document-preamble (look writer stream &rest args)
    (declare (dynamic-extent args))
  (:documentation "Emits the HTML document preamble carried by WRITER according the
document look, LOOK, on STREAM.  WRITER is a function that is called
on STREAM.  Specialize this for specific document looks."))

(defmethod write-document-preamble (look writer stream &rest args)
  (declare (dynamic-extent args)
           (ignore look args))
  (funcall writer stream))

(defmethod write-document-preamble :around (look writer stream &rest args)
  (declare (dynamic-extent args)
           (ignore look writer))
  (destructuring-bind (&key profile language direction &allow-other-keys) args
    (with-document-preamble (:profile profile :language language :direction direction :stream stream)
      (call-next-method))))

(defgeneric write-document-body (look writer stream &rest args)
  (declare (dynamic-extent args))
  (:documentation "Emits the HTML document body carried by WRITER according the document
look, LOOK, on STREAM.  WRITER is a function that is called on
STREAM.  Specialize this for specific document looks."))

(defmethod write-document-body (look writer stream &rest args)
  (declare (dynamic-extent args)
           (ignore look args))
  (funcall writer stream))

(defmethod write-document-body :around (look writer stream &rest args)
  (declare (dynamic-extent args)
           (ignore look writer))
  (destructuring-bind (&key class id title style language direction events &allow-other-keys) args
    (with-document-body (:class class :id id :title title :style style :language language 
                         :direction direction :events events :stream stream)
      (call-next-method))))

(define-generic write-document-html (look writer stream &rest args)
  (declare (dynamic-extent args))
  (:documentation "Emits the HTML document body carried by WRITER according the document
look, LOOK, on STREAM.  WRITER is a function that is called on
STREAM. DECLARE-DTD-VERSION-P controls whether the DTD version is
explicitly declared. Specialize this for specific document looks."))

(defmethod write-document-html (look writer stream &rest args)
  (declare (dynamic-extent args)
           (ignore look args))
  (funcall writer stream))

(defmethod write-document-html :around (look writer stream &rest args)
  (declare (dynamic-extent args)
           (ignore look writer))
  (destructuring-bind (&key declare-dtd-version-p language direction &allow-other-keys) args
    (with-html-document (:declare-dtd-version-p declare-dtd-version-p :language language 
                         :direction direction :stream stream)
      (call-next-method))))

(define-macro with-document-look ((document-look) &body body)
  "Binds the standard document look to LOOK within BODY.
This is found in *STANDARD-DOCUMENT-LOOK*.

Document looks are created by defining a CLOS class that inherits from
DOCUMENT-LOOK.  By specializing the methods WRITE-DOCUMENT-HTML,
WRITE-DOCUMENT-PREAMBLE, and WRITE-DOCUMENT-BODY, the look and feel of
pages generated with the HTML 4.0 generation tools can be conveniently
standardized.  

With a WITH-DOCUMENT-LOOK, the standard HTML macros are replaced with
following macros: WITH-DOCUMENT-LOOK-HTML, WITH-DOCUMENT-LOOK-PREAMBLE
and WITH-DOCUMENT-LOOK-BODY. 

To personalize the look and feel by use, the method (:method USER-DOCUMENT-LOOK USER) can be
specialized to return an appropriate document look. It can be invoked by passing in:

          (user-document-look http:*server*)

as the value for DOCUMENT-LOOK."
  `(let ((*standard-document-look* ,document-look))
     ,@body))

(eval-when (:compile-toplevel :execute :load-toplevel)
(defun collect-look-args (full-args &rest keyword-bindings)
  (loop for (keyword value) on keyword-bindings by #'cddr
        when value
        nconc `(,keyword ,value) into standard-args
        collect keyword into standard-keys
        finally (loop for (keyword value) on full-args by #'cddr
                      when (and value (not (member keyword standard-keys)))
                      nconc `(,keyword ,value) into arglist
                      finally (return-from collect-look-args (nconc standard-args arglist))))))

(define-macro with-document-look-preamble ((&rest args &key (document-look '*standard-document-look*) profile language direction
                                                  (stream '*standard-output*)) &body body)
  "Asserts the contents of BODY are heading declarations for the document (see HEAD).

DOCUMENT-LOOK is the document look object to use. This macro emits the
document preamble using the method WRITE-DOCUMENT-PREAMBLE specialized
on DOCUMENT-LOOK. See also: WITH-DOCUMENT-LOOK.

PROFILE is a URI or a list of URIs thatspecifies the location of one or more metadata profiles.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT."
(declare (dynamic-extent args))
  `(flet ((write-preamble (stream) ,@body))
     (declare (dynamic-extent #'write-preamble))
     (write-document-preamble ,document-look #'write-preamble ,stream
                              ,.(collect-look-args args :profile profile :language language :direction direction))))

(define-macro with-document-look-body ((&rest args &key (document-look '*standard-document-look*)
                                              class id title style language direction events
                                              (stream '*standard-output*) &allow-other-keys) &body body)
  "Asserts the contents of BODY is the body of the document (see BODY).

DOCUMENT-LOOK is the document look object to us. This macro is only
neede when the method, WRITE-DOCUMENT-HTML, has been specialized, for
example, to use a specific DDT. See also: WITH-DOCUMENT-LOOK.

CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE specifies inline parameters to use in the default style sheet language.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or
:RIGHT-TO-LEFT.

EVENTS can be any of the intrinsic events: :LOAD, :UNLOAD, :KEY-DOWN, :KEY-PRESS, :KEY-UP,
:MOUSE-CLICK, :MOUSE-DOUBLE-CLICK, :MOUSE-DOWN, :MOUSE-MOVE, :MOUSE-OUT, :MOUSE-OVER, :MOUSE-UP."
  (declare (dynamic-extent args))
  `(flet ((write-body (stream) ,@body))
     (declare (dynamic-extent #'write-body))
     (write-document-body ,document-look #'write-body ,stream 
                          ,.(collect-look-args args :class class  :id id  :title title  :style style  
                                               :language language  :direction direction :events events))))

(define-macro with-document-look-html ((&rest args &key (document-look '*standard-document-look*)
                                              (declare-dtd-version-p :transitional) language (direction :left-to-right)
                                              (stream '*output-stream*) &allow-other-keys) &body body)
  "Provides the toplevel environment for generating HTML according to a DOCUMENT-LOOK.

This macro is only neede when the method, WRITE-DOCUMENT-HTML, has been specialized, for example, to
use a specific DDT. See also: WITH-DOCUMENT-LOOK. But, it is a good idea to use it in case a future
document look specializes the method.

This macro is just like WRITE-DOCUMENT-HTML, except that it binds the document look using
WITH-DOCUMENT-LOOK."
  (declare (dynamic-extent args))
  `(flet ((write-document (stream) ,@body))
     (declare (dynamic-extent #'write-document))
     (with-document-look (,document-look)
       (write-document-html *standard-document-look* #'write-document ,stream 
                            ,.(collect-look-args args :declare-dtd-version-p declare-dtd-version-p :language language :direction direction)))))

;;;------------------------------------------------------------------- 
;;;
;;; CL-HTTP DOCUMENT LOOK
;;;

(defclass cl-http-look
          (document-look)
  ((style-type :initarg :style-type :reader look-style-type :initform "text/css" :allocation :class)
   (style-url :initarg :style-url :accessor look-style-url :initform "http://www.cl-http.org:8001/cl-http/css/base.css"))
  (:documentation "This document look define a CL-HTTP page style."))

(defmethod write-document-preamble :before ((look cl-http-look) writer stream &rest args)
  (declare (dynamic-extent args)
           (ignore writer args))
  (with-slots (style-url style-type) look
    (labels ((write-absolute-url (stream)
               (fast-format stream "~A~A" http::*local-context* style-url))
             (absolutize-proxy-style-reference (url)
               (cond ((http::server-proxy-request-p http::*server*)
                      (etypecase url
                        (string
                         (multiple-value-bind (scheme-match scheme-prefixed-p)
                             (url::scheme-prefix-match-p url '("http" "https"))
                           (if (or scheme-match scheme-prefixed-p)
                               url
                             #'write-absolute-url)))
                        (url (name-string url))))
                     (t url))))
      (declare (dynamic-extent #'write-absolute-url)
               (inline absolutize-proxy-style-reference))
      (declare-document-style style-type stream)
      (declare-link :reference (absolutize-proxy-style-reference style-url) :relation "stylesheet" :media-type style-type :stream stream))))

(defmethod write-document-body ((look cl-http-look) writer stream &rest args)
  (declare (dynamic-extent args)
           (ignore look writer))
  (destructuring-bind (&key header-class body-class footer-class heading heading-class heading-style heading-events
                            header-index header-index-class header-style header-events
                            image image-alternative-text image-class image-style image-events &allow-other-keys) args
    (with-division (:class header-class :stream stream)
      (cond-every
       (image
        (image image image-alternative-text
               :class image-class :style image-style :events image-events :stream stream))
       (heading 
        (with-section-heading (heading :class heading-class :style heading-style :events heading-events :stream stream)))
       (header-index
        (with-division (:inline-p t :class header-index-class :stream stream)
          (loop for ((display tag) . more) = header-index then more
                do (note-anchor display :local-reference tag :class header-class :style header-style :events header-events :stream stream)
                while more
                do (fast-format stream " | ")))))
      (horizontal-line :stream stream))
    (let ((*section-level* 2.)) ;start subheadings at 2
      (multiple-value-prog1
          (with-division (:class body-class :stream stream)
            (call-next-method))
        (with-division (:class footer-class :stream stream)
          (horizontal-line :stream stream)
          (http::cl-http-signature stream))))))

(defvar *cl-http-document-look* (create-document-look :style-url "http://www.cl-http.org:8001/cl-http/css/base.css"
                                                      :style-type "text/css"
                                                      :class 'cl-http-look))

(define-macro with-cl-http-html-document ((&key (declare-dtd-version-p :transitional) (stream '*output-stream*)) &body body)
  "Replaces WITH-HTML-DOCUMENT and establishes *CL-HTTP-DOCUMENT-LOOK* as the standard document look.
Defaults to use *STANDARD-DOCUMENT-LOOK* when *CL-HTTP-DOCUMENT-LOOK* is null."
  `(let ((doc-look (or *cl-http-document-look* *standard-document-look*)))
     (with-document-look-html (:document-look doc-look
                               :declare-dtd-version-p ,declare-dtd-version-p
                               :language :en 
                               :direction :left-to-right
                               :stream ,stream)
       ()
       ,@body)))

#|(export '(*standard-document-look* 
            ;; Classes
            document-look
            document-look-default
            ;; macros
            with-document-look
            with-document-look-preamble
            with-document-look-body
            with-document-look-html
            ;; methods
            user-document-look
            write-document-body
            write-document-preamble
            write-document-html)
          :html4.0)|#

#|

;; http://10.0.1.3:8000/cl-http/headers.html
(defmethod http-user::compute-headers-page ((url url:http-computed-url) stream)
  (http:with-successful-response (stream :html :expires (url:expiration-universal-time url)
                                         :cache-control (url:response-cache-control-directives url)
                                         :content-location url :content-language (languages url))
    (with-cl-http-html-document (:declare-dtd-version-p :transitional :stream stream)
      (let ((heading (format nil "Client Headers for ~A (~A)"
                           (http:server-host http:*server*) (http:server-http-version http:*server*))))
        (with-document-look-preamble (:stream stream)
          (declare-base-reference url :stream stream)
          (declare-title heading :stream stream))
        (with-document-look-body (:heading heading :stream stream)
          (http-user::write-headers-as-html stream)
          (with-paragraph (:stream stream)
            (fast-format  stream "Get the headers again?  ~I" (note-anchor "Yes" :reference url :stream stream))))))))))
|#
