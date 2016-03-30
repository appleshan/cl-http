;;;   -*- Mode: LISP; Package: HTTP-UI; BASE: 10; Syntax: ANSI-Common-Lisp; -*-

;;;
;;; (c) Copyright  2000, Rainer Joswig, joswig@corporate-world.lisp.de
;;;     All Rights Reserved.
;;;


;;; ------------------------------------------------------------------- 
;;;; New Presentation Types for CLIM


(eval-when (compile load eval)
  (assert (member :clim-2 *features*) ()
	  "CLIM 2 is needed to run or compile this code.
Load CLIM, then retry assertion."))


(in-package "HTTP-UI")


;;; ------------------------------------------------------------------- 
;;; Translating W3P Presentation Types to CLIM Presentation Types

(defparameter *w3p-to-clim-presentation-types*
  '((w3p:member-sequence clim:member-sequence nil t)
    (w3p:pathname clim:pathname nil t)
    (w3p:bounded-string clim:string nil nil)
    (w3p:basic-string clim:string nil nil)
    (w3p:string clim:string nil t)
    (w3p:null clim:null nil t)
    (w3p:integer clim:integer nil t)
    (w3p:boolean clim:boolean nil t)
    (w3p:null-or-type clim:null-or-type t t)
    (w3p:sequence clim:sequence t t)
    (w3p:existing-pathname clim:pathname nil t)
    (w3p:and clim:and t t)
    (w3p:or clim:or t t))
  "A list of (w3p-presentation-type clim-presentation-type recursive-p process-options-p)")


(defun convert-w3p-to-clim-presentation-type (w3p-presentation-type)
  (cond ((atom w3p-presentation-type)
	 (first (rest (assoc w3p-presentation-type *w3p-to-clim-presentation-types*))))
	((consp w3p-presentation-type)
	 (let ((new-presentation-type-description (rest (assoc (first w3p-presentation-type)
							       *w3p-to-clim-presentation-types*))))
	   (if new-presentation-type-description
	       (destructuring-bind (new-presentation-type recursive-p process-options-p)
		   new-presentation-type-description
		 (if recursive-p
		     (if process-options-p
			 (cons new-presentation-type (mapcar #'convert-w3p-to-clim-presentation-type
							     (rest w3p-presentation-type)))
			 new-presentation-type)
		     (if process-options-p
			 (cons new-presentation-type (rest w3p-presentation-type))
			 new-presentation-type)))
	       t)))
	(t t)))

    
;;; ------------------------------------------------------------------- 
;;; Presentation Types

; This has to be expanded a lot. There will be presentation translators
; and different presentation methods using these presentation types.
; I also have to find out how to use the completion facilities
; for simplified access to the objects.

(clim:define-presentation-type url:url ())

(clim:define-presentation-method clim:present (url
						(type url:url)
						stream
						(view clim:textual-view)
						&key)
  (write-string (url:name-string url) stream))


(clim:define-presentation-type http::preference-type ())
(clim:define-presentation-type http::resource ())

; (clim:define-presentation-method present (url (type url:url) stream view &key))

;;; ----------------------------------------------------------------
;;; IP Number

#||
(defun clim-parse-ip-number (stream)
  (clim:accept '()))
||#

;;; -------------------------------------------------------------------
;;; End Of File presentation-types.lisp
