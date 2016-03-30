;;;   -*- Mode: lisp; Package: (netscape4.0 :use (future-common-lisp ns3.0 www-utils url)); BASE: 10; Syntax: ansi-common-lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-

;;;
;;; (c) Copyright 1997-2001, John C. Mallery
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; INTERFACE FOR AUTHORING HTML USING NETSCAPE 4.0 EXTENSIONS
;;;
;;; Issues:
;;;
;;; 1. Conditional comments (based on Javascript predicate evaluation) not implemented.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; PACKAGE DEFINITION
;;;

(eval-when (load eval compile)

  ;; reworked to eliminate redefinition warning messages.

  #+Genera (if (find-package :netscape4.0)
	       (scl:pkg-kill :netscape4.0))			

  (defpackage :netscape4.0
    (:use future-common-lisp ns3.0 www-utils url)
    (:nicknames ns4.0)
    (:shadow 
      "%WRITE-MAKE-FONT-ARGS"
      "WITH-COMMENT"
      "WITH-DIVISION"
      "WITH-FONT"
      "WITH-PARAGRAPH"
      "WITH-SECTION-HEADING"
      "WITH-VERBATIM-TEXT")
    (:import-from html2
     "*SECTION-LEVEL*"
     "%ISSUE-COMMAND"
     "%WITH-ENVIRONMENT"
     "%WRITE-COMMAND-KEY-ARG"
     "ISSUE-COMMAND"
     "ISSUE-COMMAND*"
     "WITH-ENVIRONMENT")
    (:import-from ns2.0
     "JAVA-SCRIPT1.2")
    (:import-from html3.2
     "*HORIZONTAL-ALIGNMENT-VALUES*"
     "HORIZONTAL-ALIGNMENT-VALUE")
    (:shadowing-import-from html3.2
     "*STANDARD-COLOR-SCHEME*"
     "COLOR-SCHEME"
     "CREATE-COLOR-SCHEME"
     "DECLARE-LINK"
     "ENUMERATE-ITEM-LIST"
     "ENUMERATING-ITEM"
     "WITH-CENTERING"
     "WITH-ENUMERATION"
     "WITH-PARAGRAPH-STYLE"
     "WITH-STANDARD-DOCUMENT-BODY")
    (:import-from http
     "FAST-FORMAT")
    ;; only "new" stuff needs exporting here!
    (:export
      "*FRESH-LINE*"
      "*STANDARD-COLOR-SCHEME*"
      "*STYLE-SHEET*" 
      "COLOR-SCHEME"
      "CREATE-COLOR-SCHEME"
      "DECLARE-LINK"
      "ENUMERATE-ITEM-LIST"
      "ENUMERATING-ITEM"
      "JAVA-SCRIPT1.2"
      "SECTION-HEADING"
      "STYLE"
      "WITH-ADDRESS"
      "WITH-BLOCKQUOTE"
      "WITH-DEFINITION-DESCRIPTION"
      "WITH-DEFINITION-LIST"
      "WITH-DEFINITION-TERM"
      "WITH-DIRECTORY-LIST"
      "WITH-LAYER"
      "WITH-LIST-ITEM"
      "WITH-MENU"
      "WITH-ORDERED-LIST"
      "WITH-SPAN"
      "WITH-STYLE"
      "WITH-UNORDERED-LIST"
      "WITH-XMP"
      "WITHOUT-LAYERS-CAPABILITY"))
   
  (let ((html-pkg (find-package :ns3.0))
        (netscape-pkg (find-package :netscape4.0)))
    (do-external-symbols (sym html-pkg)
      (export (intern (symbol-name sym) netscape-pkg) netscape-pkg)))
  )								;close eval-when

(in-package :netscape4.0)

(defvar *fresh-line* t "Default.")

(defvar *style-sheet* :css "Default style sheet type.")

;;;------------------------------------------------------------------- 
;;;
;;; LAYERS
;;;
;;; http://developer.netscape.com/library/documentation/communicator/layers/index.htm

(defun %get-layer-class (arg)
  (ecase arg
    (:fixed "LAYER")
    (:in-flow "ILAYER")))

(defun %write-pixel-or-percent-argument (stream option value)
  (if (<= 0 value 1)
      (let ((w (concatenate 'string
                            (write-to-string (floor (* value 100)) :base 10. :escape nil)
                            "%")))
        (declare (dynamic-extent w))
        (%write-command-key-arg stream option w))
      (%write-command-key-arg stream option value t)))

(defun %make-layer-clip-args (clip-x1 clip-y1 clip-x2 clip-y2)
  (flet ((prepare-arg (value)
           (cond ((<= 0 value 1)
                  (let ((val (write-to-string (floor (* value 100)) :base 10. :escape nil)))
                    (declare (dynamic-extent val))
                    (concatenate 'string val "%")))
                 (t (check-type value integer)
                    value))))
    (values (prepare-arg clip-x1)
            (prepare-arg clip-y1)
            (prepare-arg clip-x2)
            (prepare-arg clip-y2))))

(defun %write-layer-arguments (stream name reference visibility background background-url
                                      x-page-origin y-page-origin x-origin y-origin width height z-index parent
                                      clip-x1 clip-y1 clip-x2 clip-y2 events)
  (cond-every
    (name (%write-command-key-arg stream "NAME" name))
    (x-page-origin (%write-pixel-or-percent-argument stream "PAGEX" x-page-origin))
    (y-page-origin (%write-pixel-or-percent-argument stream "PAGEY" y-page-origin))
    (x-origin (%write-pixel-or-percent-argument stream "LEFT" x-origin))
    (y-origin (%write-pixel-or-percent-argument stream "TOP" y-origin))
    (background
      (%write-command-key-arg stream "BGCOLOR" (color-mapping background t)))
    (background-url
      (%write-command-key-arg stream "BACKGROUND" (url:coerce-url-string background-url)))
    (reference
      (%write-command-key-arg stream "SRC" (url:coerce-url-string reference t)))
    (width (%write-command-key-arg stream "WIDTH" width t))
    (height (%write-command-key-arg stream "height" height t))
    (z-index
      (typecase z-index
        (integer (%write-command-key-arg stream "Z-INDEX" z-index t))
        (t (check-type parent string)
           (unless parent
             (error "When z-index is ~S, PARENT must be specified." z-index parent))
           (%write-command-key-arg stream
                                   (ecase z-index
                                     (:above "ABOVE")
                                     (:below "BELOW"))
                                   parent))))
    (visibility
      (unless (member visibility '(:show :hide :inherit))
        (error "visibility is ~S, which is not one of :SHOW, :HIDE or :INHERIT." visibility))
      (%write-command-key-arg stream "VISIBILITY" visibility))
    ((or clip-x2 clip-y2)
     (cond ((and clip-x2 clip-y2 clip-x1 clip-y1)
            (multiple-value-bind (x1 y1 x2 y2)
                (%make-layer-clip-args clip-x1 clip-y1 clip-x2 clip-y2)
              (declare (dynamic-extent x1 y1 x2 y2))
              (fast-format stream " CLIP=~D,~D,~D,~D" x1 y1 x2 y2)))
           (t (error "WITH-LAYER: Incomplete arguments for clipping box:~
                      CLIP-X1: ~S~&CLIP-Y1: ~S~&CLIP-X2: ~S~&CLIP-Y2:~S"
                     clip-x1 clip-y1 clip-x2 clip-y2))))
    (events
      (dolist (event events)
        (html2::%write-input-type-event-arg stream event)))))

(define-macro with-layer ((&key name (class :fixed) reference visibility background background-url
                                x-page-origin y-page-origin x-origin y-origin z-index parent width height
                                (clip-x1 0) (clip-y1 0) clip-x2 clip-y2 events (stream '*output-stream*)) &body body)
  "Establishes a layer in the HTML output on STREAM.
Layers provide idependent control of different regions of an HTML page and allow
overlay operations. Javascript operations make it possible to manipulate layers
dynamically on the client side. 
See: http://developer.netscape.com/library/documentation/communicator/layers/index.htm

NAME (optional) is a string identifying the layer.
CLASS (optional) is either :FIXED or :IN-FLOW.
REFERENCE (optional) is a URL containing HTML for display in the layer.

VISIBILITY (optional) is one of :SHOW, :HIDE or :INHERIT. The default is to
inherit the visibility of the parent.

BACKGROUND  (optional) is a color for the background.
BACKGROUND-URL (optional) is  an image URL to use as the background. 
Layers are transparent so that lower layers show through transparent areas.
These attributes will make a layer opaque.

X-PAGE-ORIGIN (optional) is the x-origin in pixels relative to the enclosing document. 
Y-PAGE-ORIGIN (optional) is the y-origin in pixels relative to the enclosing document.

X-ORIGIN (optional) is the X-origin in pixels relative to the enclosing layer. 
Y-ORIGIN (optional) is the Y-origin in pixels relative to the enclosing layer.

For positioned layers, the origin is the the upper-left hand corner of layer
containing the layer. Coordinates increase downwards and rightwards.  For
in-flow layers, the origin is the current position in the broswer rendering
when the layer is encountered. X and Y provides offset from the default
positioning.

WIDTH (optional) is the number of pixels on the horizontal axis before
wrapping begins.  If elements like images extend beyond width, the layer
will be extended horizontally.

HEIGHT (optional) is the number of pixels on the vertical axis.  If the
contents do not fit within HEIGHT, the layer will be extended vertically.
HEIGHT is intended to provide a reference height for percentage attributes of
inferior layers. HEIGHT defaults to the minimum height that contains all the
contents.

Z-INDEX (optional) is the position of the layer relative to other layers. The
default behavior is to stack new layers above previous layers. Z-INDEX can be
a number indicating the stacking of the layers.  Positive values place the
layer above its parent whereas negative values place it below. If Z-INDEX is
either :ABOVE or :BELOW, the PARENT argument must supply the name of an
earlier layer.

The clipping box is the visible area within the layer. CLIP-X1, CLIP-Y1 is the
origin and CLIP-X2, CLIP-Y2 is the extent of the box. All values are in
pixels.

Coordinate parameters may be specified as proportions relative to the
corresponding component of the enclosing layer or document. A proportion is a
float between zero and 1. Parameters accepting floats are: X-PAGE-ORIGIN,
Y-PAGE-ORIGIN, X-ORIGIN, Y-ORIGIN, WIDTH, HEIGHT, CLIP-X1, CLIP-Y1, CLIP-X2,
CLIP-Y2.

EVENTS (optional) is a list of client-side events processed when the form is
submitted."
  `(%with-environment (,(typecase class
                          (keyword
                            (%get-layer-class class))
                          (t `(%get-layer-class ,class)))
                       :stream ,stream)
                      (%write-layer-arguments
                        ,stream ,name ,reference ,visibility ,background ,background-url
                        ,x-page-origin ,y-page-origin ,x-origin ,y-origin ,width ,height ,z-index
                        ,parent ,clip-x1 ,clip-y1 ,clip-x2 ,clip-y2 ,events)
     ,@body))

(define-macro without-layers-capability ((&key (stream '*output-stream*)) &body body)
  "Any HTML emitted within BODY is ignored by layers capable clients but can be used
to provide alternate displays for clients without layers capability."
  `(with-environment ("NOLAYER" :stream ,stream)
     ,@body))

;;;---------------------------------------------------------------------------
;;;
;;; WITH-FONT
;;;
;;; Reference: http://developer.netscape.com/docs/manuals/htmlguid/index.htm
;;;
;;;---------------------------------------------------------------------------

(declaim (inline %write-make-font-args))

(defun %write-make-font-args (stream color face point-size size weight)
  (cond-every
    (color
      (%write-command-key-arg stream "COLOR" (color-mapping color t)))
    (face
      (typecase face
        (atom (%write-command-key-arg stream "FACE" face))
        (cons
          (%write-command-key-arg stream "FACE" (car face))
          (loop for item in (cdr face)
                do (write-char #\, stream)
                   (write (etypecase item
                            (keyword (symbol-name item))
                            (string item))
                          :escape t :stream stream)))))    
    (point-size
      (%write-command-key-arg stream "POINT-SIZE" point-size t))
    (size
      (unless (< 0 size 8)
        (error "SIZE, ~S, is not an integer from 1 through 7." size))
      (%write-command-key-arg stream "SIZE" size t))
    (weight
      (unless (< 99 weight 901)
        (error "WEIGHT, ~S, is not an integer from 100 through 900." weight))
      (%write-command-key-arg stream "WEIGHT" weight t))
    ))

(define-macro with-font ((&key color face point-size size weight (stream '*output-stream*)) &body body)
  "Declares the current font size to be SIZE or POINT-SIZE, the font face to be FACE, the weight or boldness to be WEIGHT, and the color to be COLOR within BODY."
  `(%with-environment ("FONT" :fresh-line nil :stream ,stream)
                      (%write-make-font-args ,stream ,color ,face ,point-size ,size ,weight)
     ,@body))

;;;-----------------------------------------------------------------------------------------
;;;
;;;  Netscape 4.0 Style Sheet Enhancements
;;;
;;;  Reference: http://developer.netscape.com/docs/manuals/index.html?content=dynhtml.html 
;;;
;;;  conrad bookout
;;;
;;;-----------------------------------------------------------------------------------------

(define-macro with-address ((&key class id lang style (fresh-line '*fresh-line*) (stream '*output-stream*)) &body body)
  "Establishes an ADDRESS environment.

  CLASS      ::= StyleClass
  ID         ::= NamedPlaceOrStyle
  LANG       ::= ISO
  STYLE      ::= Style
  FRESH-LINE ::= T | nil

Refer: with-style, style"
  `(%with-environment 
     ("address" :fresh-line ,fresh-line :stream ,stream)
     (progn
       (when ,class
	 (%write-command-key-arg ,stream "class" ,class))
       (when ,id
	 (%write-command-key-arg ,stream "id" ,id))
       (when ,lang
	 (%write-command-key-arg ,stream "lang" ,lang))
       (when ,style
	 (%write-command-key-arg ,stream "style" ,style)))
     ,@body))

(define-macro with-blockquote ((&key class id lang style (fresh-line '*fresh-line*) (stream '*output-stream*)) &body body)
  "Establishes a BLOCKQUOTE environment.

  CLASS      ::= StyleClass
  ID         ::= NamedPlaceOrStyle
  LANG       ::= ISO
  STYLE      ::= Style
  FRESH-LINE ::= T | nil

Refer: with-style, style"
  `(%with-environment 
     ("blockquote" :fresh-line ,fresh-line :stream ,stream)
     (progn
       (when ,class
	 (%write-command-key-arg ,stream "class" ,class))
       (when ,id
	 (%write-command-key-arg ,stream "id" ,id))
       (when ,lang
	 (%write-command-key-arg ,stream "lang" ,lang))
       (when ,style
	 (%write-command-key-arg ,stream "style" ,style)))
     ,@body))

(define-macro with-comment ((&key (fresh-line '*fresh-line*) (stream '*output-stream*)) &body body)
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

(define-macro with-definition-description ((&key class id lang style (fresh-line '*fresh-line*) (stream '*output-stream*)) &body body)
  "Establishes a Definition Description (DD) environment.

  CLASS      ::= StyleClass
  ID         ::= NamedPlaceOrStyle
  LANG       ::= ISO
  STYLE      ::= Style
  FRESH-LINE ::= T | nil

Refer: with-style, style"
  `(%with-environment 
     ("dd" :fresh-line ,fresh-line :stream ,stream)
     (progn
       (when ,class
	 (%write-command-key-arg ,stream "class" ,class))
       (when ,id
	 (%write-command-key-arg ,stream "id" ,id))
       (when ,lang
	 (%write-command-key-arg ,stream "lang" ,lang))
       (when ,style
	 (%write-command-key-arg ,stream "style" ,style)))
     ,@body))

(define-macro with-definition-list ((&key compact class id lang style (fresh-line '*fresh-line*) (stream '*output-stream*)) &body body)
  "Establishes a Definition List (DL) environment.

  COMPACT    ::= COMPACT
  CLASS      ::= StyleClass
  ID         ::= NamedPlaceOrStyle
  LANG       ::= ISO
  STYLE      ::= Style
  FRESH-LINE ::= T | nil

Refer: with-style, style"
  `(%with-environment 
     ("dl" :fresh-line ,fresh-line :stream ,stream)
     (progn
       (when ,compact
	 (%write-command-key-arg ,stream "compact"))
       (when ,class
	 (%write-command-key-arg ,stream "class" ,class))
       (when ,id
	 (%write-command-key-arg ,stream "id" ,id))
       (when ,lang
	 (%write-command-key-arg ,stream "lang" ,lang))
       (when ,style
	 (%write-command-key-arg ,stream "style" ,style)))
     ,@body))

(define-macro with-definition-term ((&key class id lang style (fresh-line '*fresh-line*) (stream '*output-stream*)) &body body)
  "Establishes a Definition Term (DT) environment.

  CLASS      ::= StyleClass
  ID         ::= NamedPlaceOrStyle
  LANG       ::= ISO
  STYLE      ::= Style
  FRESH-LINE ::= T | nil

Refer: with-style, style"
  `(%with-environment 
     ("dt" :fresh-line ,fresh-line :stream ,stream)
     (progn
       (when ,class
	 (%write-command-key-arg ,stream "class" ,class))
       (when ,id
	 (%write-command-key-arg ,stream "id" ,id))
       (when ,lang
	 (%write-command-key-arg ,stream "lang" ,lang))
       (when ,style
	 (%write-command-key-arg ,stream "style" ,style)))
     ,@body))

(define-macro with-directory-list ((&key class id lang style (fresh-line '*fresh-line*) (stream '*output-stream*)) &body body)
  "Establishes a DIRectory environment.

  CLASS      ::= StyleClass
  ID         ::= NamedPlaceOrStyle
  LANG       ::= ISO
  STYLE      ::= Style
  FRESH-LINE ::= T | nil

Refer: with-style, style"
  `(%with-environment 
     ("dir" :fresh-line ,fresh-line :stream ,stream)
     (progn
       (when ,class
	 (%write-command-key-arg ,stream "class" ,class))
       (when ,id
	 (%write-command-key-arg ,stream "id" ,id))
       (when ,lang
	 (%write-command-key-arg ,stream "lang" ,lang))
       (when ,style
	 (%write-command-key-arg ,stream "style" ,style)))
     ,@body))

(define-macro with-division ((&key (alignment :left) class id lang style (stream '*output-stream*)) &body body)
  "Establishes a DIVision environment.

  ALIGNMENT ::= :LEFT | :RIGHT | :CENTER
  CLASS     ::= StyleClass
  ID        ::= NamedPlaceOrStyle
  LANG      ::= ISO
  STYLE     ::= Style

Refer: with-style, style"
  `(%with-environment 
     ("div" :fresh-line t :stream ,stream)
     (progn
       (%write-command-key-arg ,stream "align" (horizontal-alignment-value ,alignment))
       (when ,class
	 (%write-command-key-arg ,stream "class" ,class))
       (when ,id
	 (%write-command-key-arg ,stream "id" ,id))
       (when ,lang
	 (%write-command-key-arg ,stream "lang" ,lang))
       (when ,style
	 (%write-command-key-arg ,stream "style" ,style)))
     ,@body))

(define-macro with-list-item ((&key type value class id lang style (fresh-line '*fresh-line*) (stream '*output-stream*)) &body body)
  "Establishes a List Item (LI) environment.

  TYPE       ::= :CIRCLE | :DISC| :SQUARE | A | a | I | i | 1
  VALUE      ::= number
  CLASS      ::= StyleClass
  ID         ::= NamedPlaceOrStyle
  LANG       ::= ISO
  STYLE      ::= Style
  FRESH-LINE ::= T | nil

Refer: with-style, style"
  `(%with-environment 
     ("li" :fresh-line ,fresh-line :stream ,stream)
     (progn
       (when ,type
	 (%write-command-key-arg ,stream "type" ,type))  
       (when ,value
	 (%write-command-key-arg ,stream "value" ,value))
       (when ,class
	 (%write-command-key-arg ,stream "class" ,class))
       (when ,id
	 (%write-command-key-arg ,stream "id" ,id))
       (when ,lang
	 (%write-command-key-arg ,stream "lang" ,lang))
       (when ,style
	 (%write-command-key-arg ,stream "style" ,style)))
     ,@body))

(define-macro with-menu ((&key class id lang style (fresh-line '*fresh-line*) (stream '*output-stream*)) &body body)
  "Establishes a MENU environment.

  CLASS      ::= StyleClass
  ID         ::= NamedPlaceOrStyle
  LANG       ::= ISO
  STYLE      ::= Style
  FRESH-LINE ::= T | nil

Refer: with-style, style"
  `(%with-environment 
     ("menu" :fresh-line ,fresh-line :stream ,stream)
     (progn
       (when ,class
	 (%write-command-key-arg ,stream "class" ,class))
       (when ,id
	 (%write-command-key-arg ,stream "id" ,id))
       (when ,lang
	 (%write-command-key-arg ,stream "lang" ,lang))
       (when ,style
	 (%write-command-key-arg ,stream "style" ,style)))
     ,@body))

(define-macro with-ordered-list ((&key start type class id lang style (fresh-line '*fresh-line*) (stream '*output-stream*)) &body body)
  "Establishes an Ordered List (OL) environment.

  START      ::= number
  TYPE       ::= A | a | I | i | 1
  CLASS      ::= StyleClass
  ID         ::= NamedPlaceOrStyle
  LANG       ::= ISO
  STYLE      ::= Style
  FRESH-LINE ::= T | nil

Refer: with-style, style"
  `(%with-environment 
     ("ol" :fresh-line ,fresh-line :stream ,stream)
     (progn
       (when ,start
	 (%write-command-key-arg ,stream "start" ,start))
       (when ,type
	 (%write-command-key-arg ,stream "type" ,type))
       (when ,class
	 (%write-command-key-arg ,stream "class" ,class))
       (when ,id
	 (%write-command-key-arg ,stream "id" ,id))
       (when ,lang
	 (%write-command-key-arg ,stream "lang" ,lang))
       (when ,style
	 (%write-command-key-arg ,stream "style" ,style)))
     ,@body))

(define-macro with-paragraph ((&key alignment class id lang style (fresh-line '*fresh-line*) (stream '*output-stream*)) &body body)
  "Establishes a Paragraph environment.

  ALIGNMENT  ::= :LEFT | :RIGHT | :CENTER
  CLASS      ::= StyleClass
  ID         ::= NamedPlaceOrStyle
  LANG       ::= ISO
  STYLE      ::= Style
  FRESH-LINE ::= T | nil

Refer: with-style, style"
  `(%with-environment 
     ("p" :fresh-line ,fresh-line :stream ,stream)
     (progn
       (when ,alignment 
	 (%write-command-key-arg ,stream "align" (horizontal-alignment-value ,alignment)))
       (when ,class
	 (%write-command-key-arg ,stream "class" ,class))
       (when ,id
	 (%write-command-key-arg ,stream "id" ,id))
       (when ,lang
	 (%write-command-key-arg ,stream "lang" ,lang))
       (when ,style
	 (%write-command-key-arg ,stream "style" ,style)))
     ,@body))

(defun section-heading (heading &optional alignment 
				(stream *output-stream*) 
				(fresh-line *fresh-line*)
				(level *section-level*) 
				class id lang style
                                &aux command)
  (setq command (case level
                  (1 "h1")
                  (2 "h2")
                  (3 "h3")
                  (4 "h4")
                  (5 "h5")
                  (6 "h6")
                  (t (cond ((< 6 level) (error "Exceeded the maximun section heading depth of 6."))
                           ((< level 1) (error "SECTION-HEADING called outside of WITH-SECTION-HEADING."))))))
  (%with-environment (command :stream stream :fresh-line fresh-line)
		     (progn
		       (when alignment 
			 (%write-command-key-arg stream "align" (horizontal-alignment-value alignment)))
		       (when class
			 (%write-command-key-arg stream "class" class))
		       (when id
			 (%write-command-key-arg stream "id" id))
		       (when lang
			 (%write-command-key-arg stream "lang" lang))
		       (when style
			 (%write-command-key-arg stream "style" style))
		       )
    (etypecase heading
      (string (write-string heading stream))
      (function (funcall heading stream)))))

(define-macro with-section-heading ((heading &key (level '*section-level*) 
					     alignment class id lang style 
					     (fresh-line '*fresh-line*) 
					     (stream '*output-stream*)) &body body)
  "Excutes BODY within a section heading of depth LEVEL.

  HEADING   ::=  string | function (called with stream)
  ALIGNMENT ::= :LEFT | :RIGHT | :CENTER
  CLASS     ::= StyleClass
  ID        ::= NamedPlaceOrStyle
  LANG      ::= ISO
  STYLE     ::= Style

Refer: with-style, style"
  (if body
      `(progn
	 (section-heading ,heading ,alignment ,stream ,fresh-line ,level ,class ,id ,lang ,style)
	 (let ((*section-level* (the fixnum (1+ ,level))))
	   ,@body))
      `(section-heading ,heading ,alignment ,stream ,fresh-line ,level ,class ,id ,lang ,style)))

(define-macro with-span ((&key class id style (stream '*output-stream*)) &body body)
  "Establishes a SPAN environment.

  CLASS ::= StyleClass
  ID    ::= NamedPlaceOrStyle
  STYLE ::= Style

Refer: with-style, style"
  `(%with-environment 
     ("span" :stream ,stream)
     (progn
       (when ,class
	 (%write-command-key-arg ,stream "class" ,class))
       (when ,id
	 (%write-command-key-arg ,stream "id" ,id))
       (when ,style
	 (%write-command-key-arg ,stream "style" ,style)))
     ,@body))

(define-macro with-style ((&key (type '*style-sheet*) (fresh-line '*fresh-line*) (stream '*output-stream*)) &body body)
  "Emit the HTML HEAD STYLE element.

  TYPE       ::= :CSS | :JAVASCRIPT
  FRESH-LINE ::= T | nil

Reference: http://developer.netscape.com/docs/manuals/index.html?content=dynhtml.html"
  `(let ((type (%clean-up ,type 'downcase)))
     (if (member type '("css" "javascript") :test #'equal)	; validate type
	 (%with-environment ("style" :fresh-line ,fresh-line :stream ,stream)
			    (format ,stream " type=\"text/~A\"" type)
	   ,@body))))

(define-macro with-unordered-list ((&key type class id lang style (fresh-line '*fresh-line*) (stream '*output-stream*)) &body body)
  "Establishes an Unordered List (UL) environment.

  TYPE       ::= :CIRCLE | :DISC | :SQUARE
  CLASS      ::= StyleClass
  ID         ::= NamedPlaceOrStyle
  LANG       ::= ISO
  STYLE      ::= Style
  FRESH-LINE ::= T | nil

Refer: with-style, style"
  `(%with-environment 
     ("ul" :fresh-line ,fresh-line :stream ,stream)
     (progn
       (when ,type
	 (%write-command-key-arg ,stream "type" ,type))
       (when ,class
	 (%write-command-key-arg ,stream "class" ,class))
       (when ,id
	 (%write-command-key-arg ,stream "id" ,id))
       (when ,lang
	 (%write-command-key-arg ,stream "lang" ,lang))
       (when ,style
	 (%write-command-key-arg ,stream "style" ,style)))
     ,@body))

(define-macro with-verbatim-text ((&key cols wrap class id lang style (fresh-line '*fresh-line*) (stream '*output-stream*)) &body body)
  "Establishes a PREformatted text environment.

  COLS       ::= Columns
  WRAP       ::= T | nil
  CLASS      ::= StyleClass
  ID         ::= NamedPlaceOrStyle
  LANG       ::= ISO
  STYLE      ::= Style
  FRESH-LINE ::= T | nil

Refer: with-style, style"
  `(%with-environment 
     ("pre" :fresh-line ,fresh-line :stream ,stream)
     (progn
       (when ,cols
	 (%write-command-key-arg ,stream "cols"
				 (etypecase ,cols
				   (string ,cols)		; string or
				   (integer (prin1-to-string ,cols)))))	; integer ok
       (when ,wrap
	 (fast-format ,stream " wrap"))
       (when ,class
	 (%write-command-key-arg ,stream "class" ,class))
       (when ,id
	 (%write-command-key-arg ,stream "id" ,id))
       (when ,lang
	 (%write-command-key-arg ,stream "lang" ,lang))
       (when ,style
	 (%write-command-key-arg ,stream "style" ,style)))
     ,@body))

(define-macro with-xmp ((&key class id lang style (stream '*output-stream*)) &body body)
  "Establishes an XMP environment.

  CLASS      ::= StyleClass
  ID         ::= NamedPlaceOrStyle
  LANG       ::= ISO
  STYLE      ::= Style

Refer: with-style, style"
  `(%with-environment 
     ("xmp" :fresh-line T :stream ,stream)		    
     (progn
       (when ,class
	 (%write-command-key-arg ,stream "class" ,class))
       (when ,id
	 (%write-command-key-arg ,stream "id" ,id))
       (when ,lang
	 (%write-command-key-arg ,stream "lang" ,lang))
       (when ,style
	 (%write-command-key-arg ,stream "style" ,style))
       (fresh-line ,stream))
     ,@body))     

(defun %clean-up (value &optional (conversion 'downcase))
  ;; convert everything to strings
  (etypecase value
    (symbol (case conversion
	      (downcase (string-downcase (symbol-name value)))
	      (upcase (string-upcase (symbol-name value)))
	      (otherwise (symbol-name value))))
    (string (case conversion
	      (downcase (string-downcase value))
	      (upcase (string-upcase value))
	      (otherwise value)))
    (integer (prin1-to-string value))))

(defun %css-style (tag 
		    stream fresh-line 
		    font-size font-family font-weight font-style line-height 
		    text-decoration text-transform text-align text-indent
		    margin margin-top margin-right margin-bottom margin-left
		    padding padding-top padding-right padding-bottom padding-left
		    border-width border-top-width border-right-width border-bottom-width 
		    border-left-width border-style border-color width float clear color
		    background-image background-color display list-style-type white-space)
  "Emit CSS stylistic attributes.

  CSS attributes may be emitted: 1) within the with-style body; or 2) contextually within block-level elements using the :style parameter."
  (let ((*stream* (if tag stream (make-string-output-stream)))
	(*white-space* (if tag (if fresh-line #\newline #\space) #\space)))
    (flet ((%format (tag name value &optional (conversion 'downcase))
	     (if tag
		 ;; within with-style body (style tag...
		 (if fresh-line 
		     (format *stream* "  ~A: ~A;~C" name 
			     (%clean-up value conversion) *white-space*)
		     (format *stream* "~A: ~A;~C" name 
			     (%clean-up value conversion) *white-space*))
		 ;; contextual (style nil...
		 (format *stream* "~A: ~A;~C" name 
			 (%clean-up value conversion) *white-space*))))
      (when tag (format *stream* "~%~A {~C" (%clean-up tag 'nil) *white-space*))
      (when font-size (%format tag "font-size" font-size))
      (when font-family (%format tag "font-family" font-family 'nil))
      (when font-weight (%format tag "font-weight" font-weight))
      (when font-style (%format tag "font-style" font-style))
      (when line-height (%format tag "line-height" line-height))
      (when text-decoration (%format tag "text-decoration" text-decoration))
      (when text-transform  (%format tag "text-transform " text-transform))
      (when text-align (%format tag "text-align" text-align))
      (when text-indent (%format tag "text-indent" font-weight))
      (when margin (%format tag "margin" margin))
      (when margin-top (%format tag "margin-top" margin-top))
      (when margin-right (%format tag "margin-right" margin-right))
      (when margin-bottom (%format tag "margin-bottom" margin-bottom))
      (when margin-left (%format tag "margin-left" margin-left))
      (when padding (%format tag "padding" padding))
      (when padding-top (%format tag "padding-top" padding-top))
      (when padding-right (%format tag "padding-right" padding-right))
      (when padding-bottom (%format tag "padding-bottom" padding-bottom))
      (when padding-left (%format tag "padding-left" padding-left))
      (when border-width (%format tag "border-width" border-width))
      (when border-top-width (%format tag "border-top-width" border-top-width))
      (when border-right-width (%format tag "border-right-width" border-right-width))
      (when border-bottom-width (%format tag "border-bottom-width" border-bottom-width))
      (when border-left-width (%format tag "border-left-width" border-left-width))
      (when border-style (%format tag "border-style" border-style))
      (when border-color (%format tag "border-color" border-color))
      (when width (%format tag "width" width))
      (when float (%format tag "float" float))
      (when clear (%format tag "clear" clear))
      (when color (%format tag "color" color))
      (when background-image (%format tag "background-image" background-image 'nil))
      (when background-color (%format tag "background-color" background-color))
      (when display (%format tag "display" display))
      (when list-style-type (%format tag "list-style-type" list-style-type))
      (when white-space (%format tag "white-space" white-space))
      (if tag 
	  (format *stream* "}~%")
	  #-Genera (get-output-stream-string *stream*)
	  #+Genera (let ((result (get-output-stream-string *stream*)))
		     (if (> (length result) 0)
			 (scl:substring result 0 (- (length result) 1))))	; eliminate trailing space
	  ))))

(defun %javascript-style (tag 
			   stream 
			   font-size font-family font-weight font-style line-height 
			   text-decoration text-transform text-align text-indent
			   margin margin-top margin-right margin-bottom margin-left
			   padding padding-top padding-right padding-bottom padding-left
			   border-width border-top-width border-right-width border-bottom-width 
			   border-left-width border-style border-color width float clear color
			   background-image background-color display list-style-type white-space)
  "Emit JavaScript stylistic attributes.

  JavaScript attributes may only be emitted within the with-style body."
  (let ((tag (%clean-up tag 'nil)))
    (flet ((%format (name value &optional (conversion 'downcase))
	     (format stream "  ~A.~A = ~S;~%" tag name (%clean-up value conversion))))
      (format stream "~%")
      (when font-size (%format "fontSize" font-size))
      (when font-family (%format "fontFamily" font-family 'nil))
      (when font-weight (%format "fontWeight" font-weight))
      (when font-style (%format "fontStyle" font-style))
      (when line-height (%format "lineHeight" line-height))
      (when text-decoration (%format "textDecoration" text-decoration))
      (when text-transform (%format "textTransform" text-transform))
      (when text-align (%format "textAlign" text-align))
      (when text-indent (%format "textIndent" text-indent))
      (when margin (%format "margins" margin))
      (when margin-top (%format "marginTop" margin-top))
      (when margin-right (%format "marginRight" margin-right)) 
      (when margin-bottom (%format "marginBottom" margin-bottom))
      (when margin-left (%format "marginLeft" margin-left))
      (when padding (%format "paddings" padding))
      (when padding-top (%format "paddingTop" padding-top))
      (when padding-right (%format "paddingRight" padding-right))
      (when padding-bottom (%format "paddingBottom" padding-bottom))
      (when padding-left (%format "paddingLeft" padding-left))
      (when border-width (%format "borderWidths" border-width))
      (when border-top-width (%format "borderTopWidth" border-top-width))
      (when border-right-width (%format "borderRightWidth" border-right-width))
      (when border-bottom-width (%format "borderBottomWidth" border-bottom-width))
      (when border-left-width (%format "borderLeftWidth" border-left-width))
      (when border-style (%format "borderStyle" border-style))
      (when border-color (%format "borderColor" border-color)) 
      (when width (%format "width" width))
      (when float (%format "align" float))
      (when clear (%format "clear" clear))
      (when color (%format "color" color))
      (when background-image (%format "backgroundImage" background-image 'nil))
      (when background-color (%format "backgroundColor" background-color))
      (when display (%format "display" display))
      (when list-style-type (%format "listStyleType" list-style-type))
      (when white-space (%format "whiteSpace" white-space)))))

(defun style (tag &key (type *style-sheet*) (fresh-line *fresh-line*) (stream *standard-output*)
		  font-size 
		  font-family 
		  font-weight 
		  font-style
		  line-height
		  text-decoration 
		  text-transform 
		  text-align 
		  text-indent
		  margin					; margins (javascript)
		  margin-top 
		  margin-right 
		  margin-bottom 
		  margin-left
		  padding					; paddings (javascript)
		  padding-top 
		  padding-right 
		  padding-bottom 
		  padding-left
		  border-width					; borderWidths (javascript)
		  border-top-width 
		  border-right-width 
		  border-bottom-width 
		  border-left-width
		  border-style
		  border-color
		  width
		  float						; align (javascript)
		  clear
		  color
		  background-image
		  background-color
		  display
		  list-style-type
		  white-space)
  "Emit stylistic attributes.

  TAG  ::= markup tag | nil (CSS only)
  TYPE ::= :CSS | :JAVASCRIPT

This function may be called within the body of with-style to generate styles within
the STYLE heading tag. If the TYPE == CSS (with TAG == nil), this function may also
be used to generate in-line stylistic attributes for other tags. CSS keywords are
used for both types of style sheets.

Reference: http://developer.netscape.com/docs/manuals/index.html?content=dynhtml.html"
  (if (equal (%clean-up type 'downcase) "css")
      (%css-style tag stream fresh-line 
		  font-size font-family font-weight font-style line-height 
		  text-decoration text-transform text-align text-indent
		  margin margin-top margin-right margin-bottom margin-left
		  padding padding-top padding-right padding-bottom padding-left
		  border-width border-top-width border-right-width border-bottom-width 
		  border-left-width border-style border-color width float clear color
		  background-image background-color display list-style-type white-space)
      (%javascript-style tag stream 
			 font-size font-family font-weight font-style line-height 
			 text-decoration text-transform text-align text-indent
			 margin margin-top margin-right margin-bottom margin-left
			 padding padding-top padding-right padding-bottom padding-left
			 border-width border-top-width border-right-width border-bottom-width 
			 border-left-width border-style border-color width float clear color
			 background-image background-color display list-style-type white-space)))
