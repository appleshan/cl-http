;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (css2 :use (future-common-lisp html3.2 www-utils url)); Base: 10 -*-

;;;
;;; CSS2 - Cascading Style Sheets, level 2
;;;
;;; (c) Copyleft 2000-2001 Conrad Bookout
;;;     All Rights Reserved.
;;;
;;; To do:
;;;  1) validate parameters?!?
;;;

(in-package :cl-user)

(eval-when (load eval compile)
  #+Genera (if (find-package :css2)
	       (scl:pkg-kill :css2))			
  (defpackage :css2
    (:use future-common-lisp html3.2 www-utils url)
    (:nicknames css)
    (:import-from html2
     "%ISSUE-COMMAND"
     "%WITH-ENVIRONMENT"
     "%WRITE-COMMAND-KEY-ARG")
    (:import-from http
     "FAST-FORMAT")
    (:export
      "*MEDIA-TYPES*"
      "@CHARSET"
      "@FONT-FACE"
      "@IMPORT"
      "@PAGE"
      "STYLE"
      "WITH-MEDIA")))

(in-package :css2)

(eval-when (load eval compile)
  (define-constant *reference* "http://www.w3.org/TR/REC-CSS2"))

#+cl-http-documentation-facility
(http:add-module-for-find-documentation "CSS")

(define-variable *fresh-line* t "If T, emit pretty fresh lines.")

;;;
;;; The property lists are used for code generation.
;;;
;;; To add a new property: 
;;;  1) update the appropriate property list(s)
;;;  2) reload this file
;;;

(eval-when (load eval compile)
  (define-constant *properties* '(ascent
				   azimuth	;aural
				   background
				   background-attachment
				   background-color
				   background-image
				   background-position
				   background-repeat
				   baseline
				   bbox
				   border
				   border-collapse
				   border-color
				   border-spacing
				   border-style
				   border-top
				   border-right
				   border-bottom
				   border-left
				   border-top-color
				   border-right-color
				   border-bottom-color
				   border-left-color
				   border-top-style
				   border-right-style
				   border-bottom-style
				   border-left-style
				   border-top-width
				   border-right-width
				   border-bottom-width
				   border-left-width
				   border-width
				   bottom
				   cap-height
				   caption-side
				   centerline
				   clear
				   clip
				   color
				   content
				   counter-increment
				   counter-reset
				   cue		;aural
				   cue-after	;aural
				   cue-before	;aural
				   cursor
				   definition-src
				   descent
				   direction
				   display
				   elevation	;aural
				   empty-cells
				   float
				   font
				   font-family
				   font-size
				   font-size-adjust
				   font-stretch
				   font-style
				   font-variant
				   font-weight
				   height
				   left
				   letter-spacing
				   line-height
				   list-style
				   list-style-image
				   list-style-position
				   list-style-type
				   margin
				   margin-top
				   margin-right
				   margin-bottom
				   margin-left
				   marker-offset
				   marks
				   mathline
				   max-height
				   max-width
				   min-height
				   min-width
				   orphans
				   outline
				   outline-color
				   outline-style
				   outline-width
				   overflow
				   padding
				   padding-top
				   padding-right
				   padding-bottom
				   padding-left
				   page
				   page-break-after
				   page-break-before
				   page-break-inside
				   panose-1
				   pause	;aural
				   pause-after	;aural
				   pause-before	;aural
				   pitch	;aural
				   pitch-range	;aural
				   play-during	;aural
				   position
				   quotes
				   richness	;aural
				   lright
				   size
				   slope
				   speak	;aural
				   speak-header	;aural
				   speak-numeral	;aural
				   speak-punctuation	;aural
				   speech-rate	;aural
				   src
				   stemv
				   stemh
				   stress	;aural
				   table-layout
				   text-align
				   text-decoration
				   text-indent
				   text-shadow
				   text-transform
				   top
				   topline
				   unicode-bidi
				   unicode-range
				   units-per-em
				   vertical-align
				   visibility
				   voice-family	;aural
				   volume	;aural
				   white-space
				   widows
				   width
				   widths
				   word-spacing
				   x-height
				   z-index)
		   "The comprehensive list of CSS properties.")
  (dolist (item *properties*) (intern (symbol-name item)))

  (define-constant *font-face-properties* '(ascent
					     baseline
					     bbox
					     cap-height
					     centerline
					     definition-src
					     descent
					     font-family
					     font-size
					     font-stretch
					     font-style
					     font-variant
					     font-weight
					     mathline
					     panose-1
					     slope
					     src
					     stemh
					     stemv
					     topline
					     unicode-range
					     units-per-em
					     x-height)
		   "Font face properties.")

  (define-constant *page-properties* '(display
					margin
					margin-top
					margin-right
					margin-bottom
					margin-left 
					marks
					orphans
					page
					page-break-after
					page-break-before
					page-break-inside
					size
					visibility
					widows)
		   "Page properties.")

  (define-constant *media-types* '(:all :aural :braille :embossed :handheld :print :projection :screen :tty :tv)
		   "Media types.")

  (define %clean-up (data &optional (separator ", "))
    "Convert data to string."
    (etypecase data
      (symbol (string-downcase (symbol-name data)))
      (string data)
      (list (let ((*stream* (make-string-output-stream)))
	      (dolist (item data) (fast-format *stream* "~A~A" (%clean-up item) separator))
	      (let ((result (get-output-stream-string *stream*)))
		(if (> (length result) 0)
		    (subseq result 0 (- (length result) (length separator)))
		    result))))
      (integer (prin1-to-string data))))

  (define %pipe (list)
    "(A B C) -> A | B | C"
    (%clean-up list " | ")))

(define @charset ()
  #.(format nil "@charset only applies to external style sheets!~%~%Reference:  ~A" *reference*)
  nil)

(define @font-face #.(append '(&key) *font-face-properties* '((fresh-line *fresh-line*) (stream *standard-output*)))
       #.(format nil "Emit @font-face rule.~%~%This function must be called within the body of with-style to generate @font-face rules.~%~%Reference:  ~A" *reference*)
       #.(append '(%style "@font-face" *font-face-properties* fresh-line stream) *font-face-properties*))

(define @import (url &optional media &key (fresh-line *fresh-line*) (stream *output-stream*))
  #.(format nil "Import style rules from external style sheets.

URL         ::= URI of style sheet to include
MEDIA       ::= list of media-types
media-types ::= ~A

Reference:  ~A" (string-downcase (%pipe *media-types*)) *reference*)
  (progn 
    (if fresh-line (fresh-line stream))
    (if media
	(fast-format stream "@import url(~S) ~A;~%" url (%clean-up media))
	(fast-format stream "@import url(~S);~%" url))))

(define @page #.(append '(selector &key) *page-properties* '((fresh-line *fresh-line*) (stream *standard-output*)))
	#.(format nil "Emit @page rule.

SELECTOR ::= string | nil

This function must be called within the body of with-style to generate @page rules.

Reference:  ~A" *reference*)
       (let ((selector (if selector (concatenate 'string "@page " (%clean-up selector)) "@page")))
	 #.(append '(%style selector *page-properties* fresh-line stream) *page-properties*)))

(define-macro with-media ((types &key (fresh-line '*fresh-line*) (stream '*output-stream*)) &body body)
  #.(format nil "Establish @media block.

TYPES       ::= list of media-types
media-types ::= ~A

This function must be called within the body of with-style to generate @media rules.

Reference:  ~A" (string-downcase (%pipe *media-types*)) *reference*)
  `(progn 
     (if ,fresh-line (fresh-line ,stream))
     (fast-format ,stream "@media ~A {~%" (%clean-up ,types))
     ,@body
     (fast-format ,stream "}~%")))

(define %style (selector properties fresh-line stream &rest rest)
  (let ((rest #+Genera (sys:copy-if-necessary rest)
	      #-Genera (copy-list rest))
	(*stream* (if selector stream (make-string-output-stream)))
	(*white-space* (if (and selector fresh-line) #\newline #\space)))
    (flet ((%format (selector descriptor value)
	     (if selector
		 ;; within with-style body -> (style selector...
		 (if fresh-line 
		     (fast-format *stream* "    ~A: ~A;~C" descriptor (%clean-up value) *white-space*)
		     (fast-format *stream* "~A: ~A;~C" descriptor (%clean-up value) *white-space*))
		 ;; contextual -> (style nil...
		 (fast-format *stream* "~A: ~A;~C" descriptor (%clean-up value) *white-space*))))
      (if (and selector fresh-line) (fresh-line stream))
      (when selector (fast-format *stream* "  ~A {~C" (%clean-up selector) *white-space*))
      (loop for i from 0 to (- (length rest) 1)
	    do (if (nth i rest) (%format selector (%clean-up (nth i properties)) (nth i rest))))
      (if selector 
	  (fast-format *stream* "  }~%")
	  (let ((result (get-output-stream-string *stream*)))
	    (if (> (length result) 0)
		(subseq result 0 (- (length result) 1))
		result))))))

#-IMACH (define style #.(append '(selector &key) *properties* '((fresh-line *fresh-line*) (stream *standard-output*)))
		#.(format nil "Emit stylistic attributes.

SELECTOR ::= string | nil

This function may be called within the body of with-style to generate styles within
the style heading tag. If SELECTOR == nil, this function may also be used to generate 
in-line stylistic attributes for other tags.

Reference:  ~A" *reference*)
       #.(append '(%style selector *properties* fresh-line stream) *properties*))

;;;
;;; Circumvent Ivory stack frame limit that prevents a successful compile by creating
;;; an interpreted function.
;;;

#+IMACH (eval '(define style #.(append '(selector &key) *properties* '((fresh-line *fresh-line*) (stream *standard-output*)))
		       #.(format nil "Emit stylistic attributes.

SELECTOR ::= string | nil

This function may be called within the body of with-style to generate styles within
the style heading tag. If SELECTOR == nil, this function may also be used to generate 
in-line stylistic attributes for other tags.

Reference:  ~A" *reference*)
       #.(append '(%style selector *properties* fresh-line stream) *properties*)))

