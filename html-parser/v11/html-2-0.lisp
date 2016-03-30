;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: HTML-PARSER -*-
;;
;; Copyright (c) 1996-98,2000 Sunil Mishra <smishra@everest.com>,
;; all rights reserved.
;;
;; $Id: //depot/cl-http/html-parser-11/html-2-0.lisp#1 $
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

;; This file contains a transcription of the HTML 2.0 DTD, STRICT.
;; I have chosen this simply because of convenience.

;; All entities beginning with a % are special SGML declarations
;; All fixed attributes have been ignored.

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

(define-html-entity #e"%content-type"
    cdata)

(define-html-entity #e"%http-method"
    (:or get post))

(define-html-entity #e"%heading"
    (:or h1 h2 h3 h4 h5 h6))

(define-html-entity #e"%list"
    (:or ul ol dir menu))

(define-html-entity #e"%font"
    (:or tt b i))

(define-html-entity #e"%phrase"
    (:or em strong code samp kbd var cite))

(define-html-entity #e"%text"
    (:or pcdata a img br #e"%phrase" #e"%font"))

(define-html-entity #e"%pre.content"
    (:or pcdata a hr br #e"%font" #e"%phrase"))

(define-html-entity #e"%text"
    (:or pcdata a img br))

(define-html-entity #e"%linktype"
    names)

(define-html-entity #e"%linkExtraAttributes"
    ((rel #e"%linktype" :implied)
     (rev #e"%linktype" :implied)
     (urn cdata :implied)
     (title cdata :implied)
     (methods names :implied)))

(define-html-entity #e"%a.content"
    (* #e"%text"))

(define-html-entity #e"%block.forms"
    (:or blockquote form isindex))

(define-html-entity #e"%preformatted"
    pre)

(define-html-entity #e"%block"
    (:or p #e"%list" dl #e"%preformatted" #e"%block.forms"))

(define-html-entity #e"%flow"
    (* (:or #e"%text" #e"%block")))

(define-html-entity #e"%body.content"
    (:or #e"%heading" #e"%block" hr address img))

(define-html-entity #e"%inputType"
    (:or text password checkbox radio submit reset image hidden))

(define-html-entity #e"%head.extra"
    nil)

(define-html-entity #e"%head.content"
    (:and title (? isindex) (? base) #e"%head.extra"))

(define-html-entity #e"%html.content"
    (:sequence head body))

;;; Elements

(define-html-element (:or #e"%font" #e"%phrase")
    :content (* #e"%text"))

(define-html-element br
    :end-optional t)

(define-html-element a
    :content #e"%a.content"
    :exclusions a
    :attributes ((href cdata :implied)
		 (name cdata :implied)
		 #e"%linkExtraAttributes"))

(define-html-element img
    :end-optional t
    :attributes ((src cdata :required)
		 (alt cdata :implied)
		 (align (:or top middle bottom) :implied)
		 (ismap (:or ismap) :implied)))

(define-html-element p
    :end-optional t
    :content (* #e"%text"))

(define-html-element hr
    :end-optional t)

(define-html-element #e"%heading"
    :content (* #e"%text"))

(define-html-element pre
    :content (* #e"%pre.content")
    :attributes ((width number :implied)))

(define-html-element dl
    :content (+ (:or dt dd))
    :attributes ((compact (:or compact) :implied)))

(define-html-element dt
    :end-optional t
    :content (* #e"%text"))

(define-html-element dd
    :end-optional t
    :content #e"%flow")

(define-html-element (:or ol ul)
    :content (+ li)
    :attributes ((compact (:or compact) :implied)))

(define-html-element (:or dir menu)
    :content (+ li)
    :exclusions #e"%block"
    :attributes ((compact (:or compact) :implied)))

(define-html-element li
    :end-optional t
    :content #e"%flow")

(define-html-element body
    :start-optional t
    :end-optional t
    :content #e"%body.content")

(define-html-element blockquote
    :content #e"%body.content")

(define-html-element address
    :content (* (:or #e"%text" p)))

(define-html-element form
    :content #e"%body.content"
    :exclusions form
    :inclusions (:or input select textarea)
    :attributes ((action cdata :implied)
		 (method #e"%http-method" get)
		 (enctype #e"%content-type" "application/x-www-form-urlencoded")))

(define-html-element input
    :end-optional t
    :attributes ((type #e"%inputType" text)
		 (name cdata :implied)
		 (value cdata :implied)
		 (src cdata :implied)
		 (checked (:or checked) :implied)
		 (size cdata :implied)
		 (maxlength number :implied)
		 (align (:or top middle bottom) :implied)))

(define-html-element select
    :content (+ option)
    :exclusions (:or input select textarea)
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
    :exclusions (:or input select textarea)
    :attributes ((name cdata :required)
		 (rows number :required)
		 (cols number :required)))

(define-html-element head
    :start-optional t
    :end-optional t
    :content #e"%head.content"
    :inclusions (:or meta link))

(define-html-element title
    :content (* pcdata)
    :exclusions (:or meta link))

(define-html-element link
    :end-optional t
    :attributes ((href cdata :required)
		 #e"%linkExtraAttributes"))

(define-html-element isindex
    :end-optional t)

(define-html-element base
    :end-optional t
    :attributes ((href cdata :required)))

(define-html-element meta
    :end-optional t
    :attributes ((http-equiv name :implied)
		 (name name :implied)
		 (content cdata :required)))

(define-html-element html
    :start-optional t
    :end-optional t
    :content #e"%html.content")

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
