;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;
;; Copyright (c) 1996-97,2000 Sunil Mishra <smishra@everest.com>,
;; portions copyright (c) 1999 Kaelin Colclasure <kaelin@acm.org>,
;; all rights reserved.
;;
;; $Id: //depot/cl-http/html-parser-11/packages.lisp#2 $
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

(in-package :cl-user)

(defpackage :html-parser
  (:use #-Genera :common-lisp
	#+Genera :future-common-lisp
	#+lispworks :clos #+lispworks :defsystem
	#+mcl :ccl
	#-cl-http :tokenizer
	#-cl-http :property-list
	#+cl-http :tk1)
  #+cl-http
  (:import-from :http
		"GET-VALUE"
		"REMOVE-VALUE"
		"PROPERTY-LIST-MIXIN"
		"MAP-VALUES"
		"PROPERTY-LIST")
  (:export
   ;; Classes
   "ABSTRACT-TAG-INSTANCE"
   "HTML-TAG-INSTANCE"
   "UNKNOWN-TAG-INSTANCE"
   "HTML-PARSER-TOKEN"
   "HTML-NAME-TOKEN"
   "HTML-ENTITY-TOKEN"

   ;; Primary functions
   "INITIALIZE-PARSER"
   "DEFINE-HTML-PARSER-CONTEXT"
   "DEFINE-HTML-PARSER"
   "WITHOUT-EXITS"

   ;; Secondary functions
   "HTML-WHITESPACE-P"
   "INTERN-NAME-TOKEN"
   "INTERN-ENTITY-TOKEN"
   "TOKEN-NAME"
   "NAME"
   "ATTR-VALUES"
   "ATTR-NAME"
   "ATTR-VAL"
   "HTML-FRAGMENT"
   "INSTANCE-OF"
   "PARTS"
   "PART-OF"
   "FILE->STRING"
   "STREAM->STRING"
   "SUBREFERENCE"
   "PARSER-INPUT"
   "PARSER-STACK"
   ;; "PARSE-CDATA" -- doesn't exist

   ;; Definition macros & DTD access - probably not for the general user
   "DEFINE-HTML-ELEMENT"		;
   "DEFINE-HTML-ENTITY"			;
   "TAG-ATTRIBUTE-DEFINITION"
   "TAG-DEFINITION"
   "ENTITY-DEFINITION"
   "CONTAINS"
   "MODIFY-DTD-LIST"

   ;; Variables & constants
   "*DTD-TAGS*"
   "*DTD-TOPLEVEL-TAGS*"
   "*HTML-CHARACTERS*"
   "*HTML-DTD-LIST*"
   "*CURRENT-DTD*"

   ;; Other symbols
   "PCDATA"
   "CDATA"				;
   ))
