;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: HTML-PARSER -*-
;;
;; Copyright (c) 1996-98,2000 Sunil Mishra <smishra@everest.com>,
;; all rights reserved.
;;
;; $Id: //depot/cl-http/html-parser-11/html-3-2.lisp#1 $
;;
;; This library is free software; you can redistribute it and/or modify it
;; under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or (at
;; your option) any later version.
;;
;; This library is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
;; General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this library; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(in-package :html-parser)

;; This file contains a transcription of the HTML 3.2 DTD
;; All entities beginning with a % are special SGML declarations

;;   :set	one or more of these is allowed, in any order
;;   :sequence	all of these are required, in the given order
;;   :or	one of these is allowed
;;   :and	all of these are required, in any order

;;   +		one or more
;;   *		zero or more
;;   ?		zero or one

;; All entity definitions MUST precede element definitions, otherwise
;; writing the code to process the definitions becomes too much of a
;; hassle

;;; Entities

(define-html-entity #e"%html.content"
  (:sequence head
	     body))

(define-html-entity #e"%head.content"
  (:and title
	(? isindex)
	(? base)))

(define-html-entity #e"%head.misc"
  (:or script
       style
       meta
       link))

(define-html-entity #e"%body.content"
  (:set (* (:or #e"%heading"
		#e"%text"
		#e"%block"
		address))))

(define-html-entity #e"%color"
  cdata)

(define-html-entity #e"%body-color-attrs"
  ((bgcolor #e"%color" :implied)
   (text #e"%color" :implied)
   (link #e"%color" :implied)
   (vlink #e"%color" :implied)
   (alink #e"%color" :implied)))

(define-html-entity #e"%address.content"
  (* (:or #e"%text" p)))

(define-html-entity #e"%content-type"
  cdata)

(define-html-entity #e"%http-method"
  (:or get post))

(define-html-entity #e"%url"
  cdata)

(define-html-entity #e"%heading"
  (:or h1 h2 h3 h4 h5 h6))

(define-html-entity #e"%list"
  (:or ul ol dir menu))

(define-html-entity #e"%preformatted"
  pre)

(define-html-entity #e"%font"
  (:or tt i b u strike big small sub sup))

(define-html-entity #e"%phrase"
  (:or em strong dfn code samp kbd var cite))

(define-html-entity #e"%special"
  (:or a img applet font br script map))

(define-html-entity #e"%form"
  (:or input select textarea))

(define-html-entity #e"%text"
  (:or pcdata #e"%font" #e"%phrase" #e"%special" #e"%form"))

(define-html-entity #e"%block"
  (:or p #e"%list" #e"%preformatted" dl div center blockquote form
       isindex hr table))

(define-html-entity #e"%flow"
  (* (:or #e"%text" #e"%block")))

(define-html-entity #e"%shape"
  (:or rect circle poly default))

(define-html-entity #e"%coords"
  cdata)

(define-html-entity #e"%types"
  cdata)

(define-html-entity #e"%length"
  cdata)

(define-html-entity #e"%pixels"
  cdata)

(define-html-entity #e"%ialign"
  (:or top middle bottom left right))

(define-html-entity #e"%pre.exclusion"
  (:or img big small sub sup font))

(define-html-entity #e"%olstyle"
  cdata)

(define-html-entity #e"%ulstyle"
  (:or disc square circle))

(define-html-entity #e"%listyle"
  cdata)

(define-html-entity #e"%inputType"
  (:or text password checkbox radio submit reset file hidden image))

(define-html-entity #e"%where"
  (:or left center right))

(define-html-entity #e"%cell.halign"
  ((align (:or left center right) :implied)))

(define-html-entity #e"%cell.valign"
  ((valign (:or top middle bottom baseline) :implied)))

;;; Elements

;; I have left out the attributes #e"%version.attr" because it cannot be
;; set by the user; it is of no consequence to parsing.

(define-html-element html
    :start-optional t
    :end-optional t
    :content #e"%html.content")

(define-html-element head
    :start-optional t
    :end-optional t
    :content #e"%head.content"
    :inclusions #e"%head.misc")

(define-html-element title
    :content (* pcdata)
    :exclusions #e"%head.misc")

(define-html-element isindex
    :end-optional t
    :attributes ((prompt cdata :implied)))

(define-html-element base
    :end-optional t
    :attributes ((href #e"%url" :required)))

(define-html-element meta
    :end-optional t
    :attributes ((http-equiv name :implied)
		 (name name :implied)
		 (content cdata :required)))

(define-html-element style
    :content (* pcdata)
    :exclusions #e"%head.misc")

(define-html-element script
    :content (* pcdata)
    :exclusions #e"%head.misc")

(define-html-element body
    :start-optional t
    :end-optional t
    :content #e"%body.content"
    :attributes ((background #e"%url" :implied)
		 #e"%body-color-attrs"))

(define-html-element address
    :content #e"%address.content")

(define-html-element div
    :content #e"%body.content"
    :attributes ((align (:or left center right) left)))

(define-html-element center
    :content #e"%body.content")

(define-html-element (:or #e"%font" #e"%phrase")
    :content (* #e"%text"))

(define-html-element font
    :content (* #e"%text")
    :attributes ((size cdata :implied)
		 (color cdata :implied)))

(define-html-element br
    :end-optional t
    :attributes ((clear (:or left all right none) none)))

(define-html-element a
    :content (* #e"%text")
    :exclusions a
    :attributes ((name cdata :implied)
		 (href #e"%url" :implied)
		 (rel cdata :implied)
		 (rev cdata :implied)
		 (title cdata :implied)))

(define-html-element map
    :content (* area)
    :attributes ((name cdata :implied)))

(define-html-element area
    :end-optional t
    :attributes ((shape #e"%shape" rect)
		 (coords #e"%coords" :implied)
		 (href #e"%url" :implied)
		 (nohref (:or nohref) :implied)
		 (alt cdata :required)))

(define-html-element link
    :end-optional t
    :attributes ((id id :implied)
		 (href #e"%url" :implied)
		 (rel #e"%types" :implied)
		 (rev #e"%types" :implied)
		 (title cdata :implied)))

(define-html-element img
    :end-optional t
    :attributes ((src #e"%url" :required)
		 (alt cdata :implied)
		 (align #e"%ialign" :implied)
		 (height #e"%pixels" :implied)
		 (width #e"%pixels" :implied)
		 (border #e"%pixels" :implied)
		 (hspace #e"%pixels" :implied)
		 (vspace #e"%pixels" :implied)
		 (usemap #e"%url" :implied)
		 (ismap (:or ismap) :implied)))

(define-html-element applet
    :content (:sequence (* param)
			textflow)
    :attributes ((codebase #e"%url" :implied)
		 (code cdata :required)
		 (name cdata :implied)
		 (alt cdata :implied)
		 (align #e"%ialign" :implied)
		 (height #e"%pixels" :required)
		 (width #e"%pixels" :required)
		 (hspace #e"%pixels" :implied)
		 (vspace #e"%pixels" :implied)))

(define-html-element param
    :end-optional t
    :attributes ((name name :required)
		 (value cdata :implied)))

(define-html-element textflow
    :start-optional t
    :end-optional t
    :content (* #e"%text"))

(define-html-element hr
    :end-optional t
    :attributes ((align (:or left right center) :implied)
		 (noshade (:or noshade) :implied)
		 (size #e"%pixels" :implied)
		 (width #e"%length" :implied)))

(define-html-element p
    :end-optional t
    :content (* #e"%text")
    :attributes ((align (:or left right center) :implied)))

(define-html-element #e"%heading"
  :content (* #e"%text")
  :attributes ((align (:or left right center) :implied)))

(define-html-element pre
    :content (* #e"%text")
    :exclusions #e"%pre.exclusion"
    :attributes ((width number :implied)))

(define-html-element blockquote
    :content #e"%body.content")

(define-html-element dl
    :content (* (:or dt dd))
    :attributes ((compact (:or compact) :implied)))

(define-html-element dt
    :end-optional t
    :content (* #e"%text"))

(define-html-element dd
    :end-optional t
    :content #e"%flow")

(define-html-element ol
    :content (* li)
    :attributes ((type #e"%olstyle" :implied)
		 (start number :implied)
		 (compact (:or compact) :implied)))

(define-html-element ul
    :content (* li)
    :attributes ((type #e"%ulstyle" :implied)
		 (compact (:or compact) :implied)))

(define-html-element (:or dir menu)
    :content (* li)
    :exclusions #e"%block"
    :attributes ((compact (:or compact) :implied)))

(define-html-element li
    :end-optional t
    :content #e"%flow"
    :attributes ((type #e"%listyle" :implied)
		 (value number :implied)))

(define-html-element form
    :content #e"%body.content"
    :exclusions form
    :attributes ((action #e"%url" :required)
		 (method #e"%http-method" get)
		 (enctype #e"%content-type"
			  "application/x-www-form-urlencoded")))

(define-html-element input
    :end-optional t
    :attributes ((type #e"%inputtype" text)
		 (name cdata :implied)
		 (value cdata :implied)
		 (checked (:or ckecked) :implied)
		 (size cdata :implied)
		 (maxlength number :implied)
		 (src #e"%url" :implied)
		 (align (:or top middle bottom left right) top)))

(define-html-element select
    :content (+ option)
    :attributes ((name cdata :required)
		 (size number :implied)
		 (multiple (:or multiple) :implied)))

(define-html-element option
    :end-optional t
    :content (* pcdata)
    :attributes ((selected (:or selected) :implied)
		 (value cdata :implied)))

(define-html-element textarea
    :content (* pcdata)
    :attributes ((name cdata :required)
		 (rows number :required)
		 (cols number :required)))

(define-html-element table
    :content (:sequence (? caption)
			(+ tr))
    :attributes ((align #e"%where" :implied)
		 (width #e"%length" :implied)
		 (border #e"%pixels" :implied)
		 (dummy (:or border) :implied)
		 (cellspacing #e"%pixels" :implied)
		 (cellpadding #e"%pixels" :implied)))

(define-html-element caption
    :content (* #e"%text")
    :attributes ((align (:or top bottom) :implied)))

(define-html-element tr
    :end-optional t
    :content (* (:or th td))
    :attributes (#e"%cell.halign"
		   #e"%cell.valign"))

(define-html-element (:or th td)
    :end-optional t
    :content #e"%body.content"
    :attributes ((nowrap (:or nowrap) :implied)
		 (rowspan number 1)
		 (colspan number 1)
		 #e"%cell.halign"
		 #e"%cell.valign"))

;;; Characters

(define-html-entity #e"copy"
  "copy")

(define-html-entity #e"reg"
  "reg")

(define-html-entity #e"amp"
  "amp")

(define-html-entity #e"gt"
  "gt")

(define-html-entity #e"lt"
  "lt")

(define-html-entity #e"quot"
  "quot")

(define-html-entity #e"nbsp"
  "nbsp")

(define-html-entity #e"AElig"
  "AElig")

(define-html-entity #e"Aacute"
  "Aacute")

(define-html-entity #e"Acirc"
  "Acirc")

(define-html-entity #e"Agrave"
  "Agrave")

(define-html-entity #e"Aring"
  "Aring")

(define-html-entity #e"Atilde"
  "Atilde")

(define-html-entity #e"Auml"
  "Auml")

(define-html-entity #e"Ccedil"
  "Ccedil")

(define-html-entity #e"ETH"
  "ETH")

(define-html-entity #e"Eacute"
  "Eacute")

(define-html-entity #e"Ecirc"
  "Ecirc")

(define-html-entity #e"Egrave"
  "Egrave")

(define-html-entity #e"Euml"
  "Euml")

(define-html-entity #e"Iacute"
  "Iacute")

(define-html-entity #e"Icirc"
  "Icirc")

(define-html-entity #e"Igrave"
  "Igrave")

(define-html-entity #e"Iuml"
  "Iuml")

(define-html-entity #e"Ntilde"
  "Ntilde")

(define-html-entity #e"Oacute"
  "Oacute")

(define-html-entity #e"Ocirc"
  "Ocirc")

(define-html-entity #e"Ograve"
  "Ograve")

(define-html-entity #e"Oslash"
  "Oslash")

(define-html-entity #e"Otilde"
  "Otilde")

(define-html-entity #e"Ouml"
  "Ouml")

(define-html-entity #e"THORN"
  "THORN")

(define-html-entity #e"Uacute"
  "Uacute")

(define-html-entity #e"Ucirc"
  "Ucirc")

(define-html-entity #e"Ugrave"
  "Ugrave")

(define-html-entity #e"Uuml"
  "Uuml")

(define-html-entity #e"Yacute"
  "Yacute")

(define-html-entity #e"aacute"
  "aacute")

(define-html-entity #e"acirc"
  "acirc")

(define-html-entity #e"aelig"
  "aelig")

(define-html-entity #e"agrave"
  "agrave")

(define-html-entity #e"aring"
  "aring")

(define-html-entity #e"atilde"
  "atilde")

(define-html-entity #e"auml"
  "auml")

(define-html-entity #e"ccedil"
  "ccedil")

(define-html-entity #e"eacute"
  "eacute")

(define-html-entity #e"edirc"
  "edirc")

(define-html-entity #e"egrave"
  "egrave")

(define-html-entity #e"eth"
  "eth")

(define-html-entity #e"euml"
  "euml")

(define-html-entity #e"iacute"
  "iacute")

(define-html-entity #e"icirc"
  "icirc")

(define-html-entity #e"igrave"
  "igrave")

(define-html-entity #e"iuml"
  "iuml")

(define-html-entity #e"ntilde"
  "ntilde")

(define-html-entity #e"oacute"
  "oacute")

(define-html-entity #e"ocirc"
  "ocirc")

(define-html-entity #e"ograve"
  "ograve")

(define-html-entity #e"oslash"
  "oslash")

(define-html-entity #e"otilde"
  "otilde")

(define-html-entity #e"ouml"
  "ouml")

(define-html-entity #e"quot"
  "quot")

(define-html-entity #e"szlig"
  "szlig")

(define-html-entity #e"thorn"
  "thorn")

(define-html-entity #e"uacute"
  "uacute")

(define-html-entity #e"ucirc"
  "ucirc")

(define-html-entity #e"ugrave"
  "ugrave")

(define-html-entity #e"uuml"
  "uuml")

(define-html-entity #e"yacute"
  "yacute")

(define-html-entity #e"yuml"
  "yuml")

;;; EOF
