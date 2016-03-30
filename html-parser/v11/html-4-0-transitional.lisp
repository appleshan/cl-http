;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: HTML-PARSER -*-
;;
;; Copyright (c) 1996-97,2000 Sunil Mishra <smishra@everest.com>,
;; all rights reserved.
;;
;; $Id: //depot/cl-http/html-parser-11/html-4-0-transitional.lisp#1 $
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

;; This file contains a lispified transcription of the
;;    HTML 4.0 Transitional DTD. It does not do frames.
;; Based on the DTD at
;;    http://www.w3.org/TR/1998/REC-html40-19980424/loose.dtd
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

;; All comments beginning with *** are taken from the DTD.

;;; Entities

;; ***Imported names

(define-html-entity #e"%ContentType"
  cdata)

(define-html-entity #e"%ContentTypes"
  cdata)

(define-html-entity #e"%Charset"
  cdata)

(define-html-entity #e"%Charsets"
  cdata)

(define-html-entity #e"%LanguageCode"
  name)

(define-html-entity #e"%Character"
  cdata)

(define-html-entity #e"%LinkTypes"
  cdata)

(define-html-entity #e"%MediaDesc"
  cdata)

(define-html-entity #e"%URI"
  cdata)

(define-html-entity #e"%Datetime"
  cdata)

(define-html-entity #e"%Script"
  cdata)

(define-html-entity #e"%StyleSheet"
  cdata)

(define-html-entity #e"%FrameTarget"
  cdata)

(define-html-entity #e"%Text"
  cdata)

;; ***Parameter Entities

(define-html-entity #e"%head.misc"
  (:or script style meta link object))	; Repeatable head elements

(define-html-entity #e"%heading"
  (:or h1 h2 h3 h4 h5 h6))

(define-html-entity #e"%list"
  (:or ul ol dir menu))

(define-html-entity #e"%preformatted"
  pre)

(define-html-entity #e"%Color"		; A color using sRGB:
  cdata)				; #RRGGBB as Hex values

(define-html-entity #e"%bodycolors"
  ((bgcolor #e"%Color" :implied)	; Document background color
   (text #e"%Color" :implied)		; Document text color
   (link #e"%Color" :implied)		; Color of links
   (vlink #e"%Color" :implied)		; Color of visited links
   (alink #e"%Color" :implied)))	; Color of selected links

;; ***Character mnemonic entities

;; !!!Skipped!!!

;;<!ENTITY % HTMLlat1 PUBLIC
;;   "-//W3C//ENTITIES Latin1//EN//HTML"
;;   "http://www.w3.org/TR/1998/REC-html40-19980424/HTMLlat1.ent">
;;%HTMLlat1;
;;
;;<!ENTITY % HTMLsymbol PUBLIC
;;   "-//W3C//ENTITIES Symbols//EN//HTML"
;;   "http://www.w3.org/TR/1998/REC-html40-19980424/HTMLsymbol.ent">
;;%HTMLsymbol;
;;
;;<!ENTITY % HTMLspecial PUBLIC
;;   "-//W3C//ENTITIES Special//EN//HTML"
;;   "http://www.w3.org/TR/1998/REC-html40-19980424/HTMLspecial.ent">
;;%HTMLspecial;

;; ***Generic attributes

(define-html-entity #e"%coreattrs"
  ((id id :implied)			; Document-wide unique id
   (class cdata :implied)		; Space separated list of classes
   (style #e"%StyleSheet" :implied)	; Associated style info
   (title #e"%Text" :implied)		; advisory title/amplification
   ))

(define-html-entity #e"%i18n"
  ((lang #e"%LanguageCode" :implied)	; language code
   (dir (:or ltr rtl) :implied)		; direction for weak/neutral text
   ))

(define-html-entity #e"%events"
  ((onclick #e"%Script" :implied)	; a pointer button was clicked
   (ondblclick #e"%Script" :implied)	; a pointer button was double clicked
   (onmousedown #e"%Script" :implied)	; a pointer button was pressed down
   (onmouseup #e"%Script" :implied)	; a pointer button was released
   (onmouseover #e"%Script" :implied)	; a pointer was moved onto
   (onmousemove #e"%Script" :implied)	; a pointer was moved within
   (onmouseout #e"%Script" :implied)	; a pointer was moved away
   (onkeypress #e"%Script" :implied)	; a key was pressed and released
   (onkeydown #e"%Script" :implied)	; a key was pressed down
   (onkeyup #e"%Script" :implied)	; a key was released
   ))

(define-html-entity #e"%attrs"
  (#e"%coreattrs" #e"%i18n" #e"%events"))

(define-html-entity #e"%align"		; default is left for ltr paragraphs, right for rtl
  ((align (:or left center right justify) :implied)))

;; ***Text Markup

(define-html-entity #e"%fontstyle"
  (:or tt i b u s strike big small))

(define-html-entity #e"%phrase"
  (:or em strong dfn code samp kbd var cite abbr acronym))

(define-html-entity #e"%special"
  (:or a img applet object font basefont br script
       map q sub sup span bdo iframe))

(define-html-entity #e"%formctrl"
  (:or input select textarea label button))

(define-html-entity #e"%inline"
  (:or pcdata #e"%fontstyle" #e"%phrase" #e"%special" #e"%formctrl"))

;; ***HTML content models

(define-html-entity #e"%block"
  (:or p #e"%heading" #e"%list" #e"%preformatted" dl div center
       noscript noframes blockquote form isindex hr
       table fieldset address))

(define-html-entity #e"%flow"
  (:or #e"%block" #e"%inline"))

;; ***The Anchor Element

(define-html-entity #e"%Shape"
  (:or rect circle poly default))

(define-html-entity #e"%Coords"
  cdata)				; comma separated list of lengths

;; ***Images

(define-html-entity #e"%Length"
  cdata)				; nn for pixels or nn% for percentage length

(define-html-entity #e"%MultiLength"
  cdata)				; pixel, percentage, or relative

(define-html-entity #e"%MultiLengths"
  cdata)				; comma-separated list of MultiLength

(define-html-entity #e"%Pixels"
  cdata)				; integer representing length in pixels

(define-html-entity #e"%IAlign"
  (:or top middle bottom left right))	; center?

;; ***Preformatted text

(define-html-entity #e"%pre.exclusion"
  (:or img object applet big small sub sup font basefont))

;; ***Lists

(define-html-entity #e"%OLStyle"
  cdata)				; constrained to: "(1|a|A|i|I)"

(define-html-entity #e"%ULStyle"
  (:or disc square circle))

(define-html-entity #e"%LIStyle"
  cdata)				; constrained to: (%ULStyle;|%OLStyle;)

;; ***Forms

(define-html-entity #e"%InputType"
  (:or text password checkbox radio submit reset
       file hidden image button))

(define-html-entity #e"%LAlign"
  (:or top bottom left right))

;; ***IETF HTML table standard, see [RFC1942]

(define-html-entity #e"%TFrame"
  (:or void above below hsides lhs rhs vsides box border))

(define-html-entity #e"%TRules"
  (:or none groups rows cols all))

(define-html-entity #e"%TAlign"
  (:or left center right))

(define-html-entity #e"%cellhalign"
  ((align (:or left center right justify char) :implied)
   (char #e"%Character" :implied)	; alignment char, e.g. char=':'
   (charoff #e"%Length" :implied)	; offset for alignment char
   ))

(define-html-entity #e"%cellvalign"
  ((valign (:or top middle bottom baseline) :implied)))

(define-html-entity #e"%CAlign"
  (:or top bottom left right))

(define-html-entity #e"%Scope"
  (:or row col rowgroup colgroup))

;; ***Document Frames

;; Since this DTD does not include frames...

(define-html-entity #e"%noframes.content"
  (* #e"%flow"))

;; ***Document Head

(define-html-entity #e"%head.content"
  (:and title
	(? isindex)
	(? base)))

;; ***Document Structure

;; Skipping version

;;<!ENTITY #e"%" version "version CDATA #FIXED #e"'%HTML.Version";'">

(define-html-entity #e"%html.content"
  (:sequence head body))

;;; Elements

;; I have left out the attributes #e"%version.attr" because it cannot be
;; set by the user; it is of no consequence to parsing.

;; ***Text Markup

(define-html-element (:or #e"%fontstyle" #e"%phrase")
    :content (* #e"%inline")
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   ))

(define-html-element (:or sub sup)	; subscript, superscript
    :content (* #e"%inline")
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   ))

(define-html-element span		; generic language/style container
    :content (* #e"%inline")
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   ))

(define-html-element bdo
    :content (* #e"%inline")		; I18N BiDi over-ride
    :attributes (#e"%coreattrs"		; id, class, style, title
		   (lang #e"%LanguageCode" :implied) ; language code
		   (dir (:or ltr rtl) :required) ;  directionality
		   ))

(define-html-element basefont		; base font size
    :end-optional t
    :attributes ((id id :implied)	; document-wide unique id
		 (size cdata :required)	; base font size for FONT elements
		 (color #e"%Color" :implied) ; text color
		 (face cdata :implied)	; comma separated list of font names
		 ))

(define-html-element font		; local change to font
    :content (* #e"%inline")
    :attributes (#e"%coreattrs"		; id, class, style, title
		   #e"%i18n"		; lang, dir
		   (size cdata :implied) ; [+|-]nn e.g. size="+1", size="4"
		   (color cdata :implied) ; text color
		   (face cdata :implied) ; comma separated list of font names
		   ))

(define-html-element br			; forced line break
    :end-optional t
    :attributes (#e"%coreattrs"		; id, class, style, title
		   (clear (:or left all right none) none) ; control of text flow
		   ))

;; ***Document Body

(define-html-element body		; document body
    :start-optional t
    :end-optional t
    :content (* #e"%flow")
    :inclusions (:or ins del)
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   (onload #e"%Script" :implied) ; the document has been loaded
		   (onunload #e"%Script" :implied) ; the document has been removed
		   (background #e"%URI" :implied) ; texture tile for document background
		   #e"%bodycolors"	; bgcolor, text, link, vlink, alink
		   ))

(define-html-element address
    :content (* (:or #e"%inline" p))	; information on author
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   ))

(define-html-element div		; generic language/style container
    :content (* #e"%flow")
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   #e"%align"		; align, text alignment
		   ))

(define-html-element center		; shorthand for DIV align=center
    :content (* #e"%flow")
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   ))

;; ***The Anchor Element

(define-html-element a			; anchor
    :content (* #e"%inline")
    :exclusions a
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   (charset #e"%Charset" :implied) ; char encoding of linked resource
		   (type #e"%ContentType" :implied) ; advisory content type
		   (name cdata :implied) ; named link end
		   (href #e"%uri" :implied) ; URI for linked resource
		   (hreflang #e"%LanguageCode" :implied) ; language code
		   (target #e"%FrameTarget" :implied) ; render in this frame
		   (rel #e"%LinkTypes" :implied) ; forward link types
		   (rev #e"%LinkTypes" :implied) ; reverse link types
		   (accesskey #e"%Character" :implied) ; accessibility key character
		   (shape #e"%Shape" rect) ; for use with client-side image maps
		   (coords #e"%Coords" :implied) ; for use with client-side image maps
		   (tabindex NUMBER :implied) ; position in tabbing order
		   (onfocus #e"%Script" :implied) ; the element got the focus
		   (onblur #e"%Script" :implied) ; the element lost the focus
		   ))

;; ***Client-side image maps

(define-html-element map		; client-side image map
    :content (:or (+ #e"%block") (+ area))
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   (name cdata :required) ; for reference by usemap
		   ))

(define-html-element area		; client-side image map area
    :end-optional t
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   (shape #e"%Shape" rect) ; controls interpretation of coords
		   (coords #e"%Coords" :implied) ; comma separated list of lengths
		   (href #e"%uri" :implied) ; URI for linked resource
		   (target #e"%FrameTarget" :implied) ; render in this frame
		   (nohref (:or nohref) :implied) ; this region has no action
		   (alt #e"%Text" :required) ; short description
		   (tabindex number :implied) ; position in tabbing order
		   (accesskey #e"%Character" :implied) ; accessibility key character
		   (onfocus #e"%Script" :implied) ; the element got the focus
		   (onblur #e"%Script" :implied) ; the element lost the focus
		   ))

;; ***The LINK Element

(define-html-element link		; a media-independent link
    :end-optional t
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events 
		   (charset #e"%Charset" :IMPLIED) ; char encoding of linked resource
		   (href #e"%URI" :IMPLIED) ; URI for linked resource
		   (hreflang #e"%LanguageCode" :IMPLIED) ; language code
		   (type #e"%ContentType" :IMPLIED) ; advisory content type
		   (rel #e"%LinkTypes" :IMPLIED) ; forward link types
		   (rev #e"%LinkTypes" :IMPLIED) ; reverse link types
		   (media #e"%MediaDesc" :IMPLIED) ; for rendering on these media
		   (target #e"%FrameTarget" :IMPLIED) ; render in this frame
		   ))

;; ***Images

(define-html-element img		; Embedded image
    :end-optional t
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   (src #e"%URI" :REQUIRED) ; URI of image to embed
		   (alt #e"%Text" :REQUIRED) ; short description
		   (longdesc #e"%URI" :IMPLIED) ; link to long description (complements alt)
		   (height #e"%Length" :IMPLIED) ; override height
		   (width #e"%Length" :IMPLIED) ; override width
		   (usemap #e"%URI" :IMPLIED) ; use client-side image map
		   (ismap (:or ismap) :IMPLIED) ; use server-side image map
		   (align #e"%IAlign" :IMPLIED) ; vertical or horizontal alignment
		   (border #e"%Length" :IMPLIED) ; link border width
		   (hspace #e"%Pixels" :IMPLIED) ; horizontal gutter
		   (vspace #e"%Pixels" :IMPLIED) ; vertical gutter
		   ))

;; ***Object

(define-html-element object
    :content (* (:or param #e"%flow"))
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   (declare (:or declare) :implied) ; declare but don't instantiate flag
		   (classid #e"%URI" :implied) ; identifies an implementation
		   (codebase #e"%URI" :implied) ; base URI for classid, data, archive
		   (data #e"%URI" :implied) ; reference to object's data
		   (type #e"%ContentType" :implied) ; content type for data
		   (codetype #e"%ContentType" :implied) ; content type for code
		   (archive #e"%URI" :implied) ; space separated archive list
		   (standby #e"%Text" :implied) ; message to show while loading
		   (height #e"%Length" :implied) ; override height
		   (width #e"%Length" :implied) ; override width
		   (usemap #e"%URI" :implied) ; use client-side image map
		   (name CDATA :implied) ; submit as part of form
		   (tabindex NUMBER :implied) ; position in tabbing order
		   (align #e"%IAlign" :implied) ; vertical or horizontal alignment
		   (border #e"%Length" :implied) ; link border width
		   (hspace #e"%Pixels" :implied) ; horizontal gutter
		   (vspace #e"%Pixels" :implied) ; vertical gutter
		   ))

(define-html-element param
    :end-optional t
    :attributes ((id ID :implied)	; document-wide unique id
		 (name CDATA :required)	; property name
		 (value CDATA :implied)	; property value
		 (valuetype (:or DATA REF OBJECT) data) ; How to interpret value
		 (type #e"%ContentType" :implied) ; content type for value when valuetype=ref
		 ))

;; *** Java APPLET

(define-html-element applet
    :content (* (:or param #e"%flow"))
    :attributes (#e"%coreattrs"		; id, class, style, title
		   (codebase #e"%URI" :implied) ; optional base URI for applet
		   (archive CDATA :implied) ; comma separated archive list
		   (code CDATA :implied) ; applet class file
		   (object CDATA :implied) ; serialized applet file
		   (alt #e"%Text" :implied) ; short description
		   (name CDATA :implied) ; allows applets to find each other
		   (width #e"%Length" :required) ; initial width
		   (height #e"%Length" :required) ; initial height
		   (align #e"%IAlign" :implied) ; vertical or horizontal alignment
		   (hspace #e"%Pixels" :implied) ; horizontal gutter
		   (vspace #e"%Pixels" :implied) ; vertical gutter))
		   ))

(define-html-element hr			; horizontal rule
    :end-optional t
    :attributes (#e"%coreattrs"		; id, class, style, title
		   #e"%events"
		   (align (:or left center right) :implied)
		   (noshade (:or noshade) :implied)
		   (size #e"%Pixels" :implied)
		   (width #e"%Length" :implied)))

;; ***Paragraphs

(define-html-element p			; paragraph
    :end-optional t
    :content (* #e"%inline")
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   #e"%align"		; align, text alignment
		   ))

;; ***Headings

(define-html-element #e"%heading"	; heading
  :content (* #e"%inline")
  :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		 #e"%align"		; align, text alignment
		 ))

;; ***Preformatted text

(define-html-element pre		; preformatted text
    :content (* #e"%inline")
    :exclusions #e"%pre.exclusion"
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   (width number :implied)))

;; ***Inline quotes

(define-html-element Q			; short inline quotation
    :content (* #e"%inline")
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   (cite #e"%URI" :implied) ; URI for source document or msg
		   ))

;; ***Block-like Quotes

(define-html-element blockquote		; long quotation
    :content (* #e"%flow")
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   (cite #e"%URI" :implied) ; URI for source document or msg
		   ))

;; ***Inserted/Deleted Text

(define-html-element (:or ins del)	; inserted text, deleted text
    :content (* #e"%flow")
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   (cite #e"%URI" :implied) ; info on reason for change
		   (datetime #e"%Datetime" :implied) ; date and time of change
		   ))

;; ***Lists

;; definition lists - DT for term, DD for its definition

(define-html-element DL			; definition list
    :content (+ (:or DT DD))
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   (compact (:or compact) :implied) ; reduced interitem spacing
		   ))
(define-html-element DT			; definition term
    :end-optional t
    :content (* #e"%inline")
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   ))

(define-html-element DD			; definition description
    :end-optional t
    :content (* #e"%flow")
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   ))

;; Ordered lists (OL) Numbering style
;;
;;    1   arablic numbers     1, 2, 3, ...
;;    a   lower alpha         a, b, c, ...
;;    A   upper alpha         A, B, C, ...
;;    i   lower roman         i, ii, iii, ...
;;    I   upper roman         I, II, III, ...
;;
;;    The style is applied to the sequence number which by default
;;    is reset to 1 for the first list item in an ordered list.
;;
;;    This can't be expressed directly in SGML due to case folding.

(define-html-element OL			; ordered list
    :content (+ LI)
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   (type #e"%OLStyle" :implied) ; numbering style
		   (compact (:or compact) :implied) ; reduced interitem spacing
		   (start NUMBER :implied) ; starting sequence number
		   ))

(define-html-element UL			; unordered list
    :content (+ LI)
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   (type #e"%ULStyle" :implied) ; bullet style
		   (compact (:or compact) :implied) ; reduced interitem spacing
		   ))

(define-html-element DIR		; directory list, menu list
    :content (+ LI)
    :exclusions #e"%block"
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   (compact (:or compact) :implied)))

(define-html-element MENU		; directory list, menu list
    :content (+ LI)
    :exclusions #e"%block"
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   (compact (:or compact) :implied)))

(define-html-element LI			; list item
    :end-optional t
    :content (* #e"%flow")
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   (type #e"%LIStyle" :implied) ; list item style
		   (value NUMBER :implied) ; reset sequence number
		   ))

;; ***Forms

(define-html-element FORM		; interactive form
    :content (* #e"%flow")
    :exclusions FORM
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   (action #e"%URI" :required) ; server-side form handler
		   (method (:or GET POST) get) ; HTTP method used to submit the for
		   (enctype #e"%ContentType" "application/x-www-form-urlencoded")
		   (onsubmit #e"%Script" :implied) ; the form was submitted
		   (onreset #e"%Script" :implied) ; the form was reset
		   (target #e"%FrameTarget" :implied) ; render in this frame
		   (accept-charset #e"%charsets" :implied) ; list of supported charsets
		   ))

;; Each label must not contain more than ONE field

(define-html-element LABEL		; form field label text
    :content (* #e"%inline")
    :exclusions LABEL
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   (for IDREF :implied)	; matches field ID value
		   (accesskey #e"%Character" :implied) ; accessibility key character
		   (onfocus #e"%Script" :implied) ; the element got the focus
		   (onblur #e"%Script" :implied) ; the element lost the focus
		   ))

;; attribute name required for all but submit & reset

(define-html-element INPUT		; form control
    :end-optional t
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   (type #e"%InputType" text) ; what kind of widget is needed
		   (name CDATA :implied) ; submit as part of form
		   (value CDATA :implied) ; required for radio and checkboxes
		   (checked (:or checked) :implied) ; for radio buttons and check boxes
		   (disabled (:or disabled) :implied) ; unavailable in this context
		   (readonly (:or readonly) :implied) ; for text and passwd
		   (size CDATA :implied) ; specific to each type of field
		   (maxlength NUMBER :implied) ; max chars for text fields
		   (src #e"%URI" :implied) ; for fields with images
		   (alt CDATA :implied)	; short description
		   (usemap #e"%URI" :implied) ; use client-side image map
		   (tabindex NUMBER :implied) ; position in tabbing order
		   (accesskey #e"%Character" :implied) ; accessibility key character
		   (onfocus #e"%Script" :implied) ; the element got the focus
		   (onblur #e"%Script" :implied) ; the element lost the focus
		   (onselect #e"%Script" :implied) ; some text was selected
		   (onchange #e"%Script" :implied) ; the element value was changed
		   (accept #e"%ContentTypes" :implied) ; list of MIME types for file upload
		   (align #e"%IAlign" :implied) ; vertical or horizontal alignment
		   ))

(define-html-element SELECT		; option selector
    :content (+ (:or OPTGROUP OPTION))
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   (name CDATA :implied) ; field name
		   (size NUMBER :implied) ; rows visible
		   (multiple (:or multiple) :implied) ; default is single selection
		   (disabled (:or disabled) :implied) ; unavailable in this context
		   (tabindex NUMBER :implied) ; position in tabbing order
		   (onfocus #e"%Script" :implied) ; the element got the focus
		   (onblur #e"%Script" :implied) ; the element lost the focus
		   (onchange #e"%Script" :implied) ; the element value was changed
		   ))

(define-html-element OPTGROUP		; option group
    :content (+ OPTION)
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   (disabled (:or disabled) :implied) ; unavailable in this context
		   (label #e"%Text" :required) ; for use in hierarchical menus
		   ))

(define-html-element OPTION		; selectable choice
    :end-optional t
    :content PCDATA
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   (selected (:or selected) :implied)
		   (disabled (:or disabled) :implied) ; unavailable in this context
		   (label #e"%Text" :implied) ; for use in hierarchical menus
		   (value CDATA :implied) ; defaults to element content
		   ))

(define-html-element TEXTAREA		; multi-line text field
    :content PCDATA
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   (name CDATA :implied) 
		   (rows NUMBER :required)
		   (cols NUMBER :required)
		   (disabled (:or disabled) :implied) ; unavailable in this context
		   (readonly (:or readonly) :implied)
		   (tabindex NUMBER :implied) ; position in tabbing order
		   (accesskey #e"%Character" :implied) ; accessibility key character
		   (onfocus #e"%Script" :implied) ; the element got the focus
		   (onblur #e"%Script" :implied) ; the element lost the focus
		   (onselect #e"%Script" :implied) ; some text was selected
		   (onchange #e"%Script" :implied) ; the element value was changed
		   ))

;;  PCDATA is to solve the mixed content problem,
;;  per specification only whitespace is allowed there!

(define-html-element FIELDSET		; form control group
    :content (:sequence (PCDATA LEGEND (* #e"%flow")))
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   ))

(define-html-element LEGEND		; fieldset legend
    :content (* #e"%inline")
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   (accesskey #e"%Character" :implied) ; accessibility key character
		   (align #e"%LAlign" :implied) ; relative to fieldset
		   ))

(define-html-element BUTTON		; push button
    :content (* #e"%flow")
    :exclusions (:or A #e"%formctrl" FORM ISINDEX FIELDSET IFRAME)
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   (name CDATA :implied)
		   (value CDATA :implied) ; sent to server when submitted
		   (type (:or button submit reset) submit) ; for use as form button
		   (disabled (:or disabled) :implied) ; unavailable in this context
		   (tabindex NUMBER :implied) ; position in tabbing order
		   (accesskey #e"%Character" :implied) ; accessibility key character
		   (onfocus #e"%Script" :implied) ; the element got the focus
		   (onblur #e"%Script" :implied) ; the element lost the focus
		   ))

;; ***Tables

;; IETF HTML table standard, see [RFC1942]
;;
;; The BORDER attribute sets the thickness of the frame around the
;; table. The default units are screen pixels.
;;
;; The FRAME attribute specifies which parts of the frame around
;; the table should be rendered. The values are not the same as
;; CALS to avoid a name clash with the VALIGN attribute.
;;
;; The value "border" is included for backwards compatibility with
;; <TABLE BORDER> which yields frame=border and border=implied
;; For <TABLE BORDER=1> you get border=1 and frame=implied. In this
;; case, it is appropriate to treat this as frame=border for backwards
;; compatibility with deployed browsers.
;;
;; The RULES attribute defines which rules to draw between cells:
;;
;; If RULES is absent then assume:
;;     "none" if BORDER is absent or BORDER=0 otherwise "all"

(define-html-element TABLE
    :content (:sequence (? CAPTION) (:or (* COL) (* COLGROUP)) (? THEAD) (? TFOOT) (+ TBODY))
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   (summary #e"%Text" :implied) ; purpose/structure for speech outpu
		   (width #e"%Length" :implied) ; table width
		   (border #e"%Pixels" :implied) ; controls frame width around table
		   (frame #e"%TFrame" :implied) ; which parts of frame to render
		   (rules #e"%TRules" :implied) ; rulings between rows and cols
		   (cellspacing #e"%Length" :implied) ; spacing between cells
		   (cellpadding #e"%Length" :implied) ; spacing within cells
		   (align #e"%TAlign" :implied) ; table position relative to window
		   (bgcolor #e"%Color" :implied) ; background color for cells
		   ))

(define-html-element CAPTION		; table caption
    :content (* #e"%inline")
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   (align #e"%CAlign" :implied) ; relative to table
		   ))

;;    Use THEAD to duplicate headers when breaking table
;;    across page boundaries, or for static headers when
;;    TBODY sections are rendered in scrolling panel.

(define-html-element THEAD		; table header
    :end-optional t
    :content(+ TR)
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   #e"%cellhalign"	; horizontal alignment in cells
		   #e"%cellvalign"	; vertical alignment in cells
		   ))

;;    Use TFOOT to duplicate footers when breaking table
;;    across page boundaries, or for static footers when
;;    TBODY sections are rendered in scrolling panel.

(define-html-element TFOOT		; table footer
    :end-optional t
    :content (+ TR)
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   #e"%cellhalign"	; horizontal alignment in cells
		   #e"%cellvalign"	; vertical alignment in cells
		   ))

;;    Use multiple TBODY sections when rules are needed
;;    between groups of table rows.

(define-html-element TBODY		; table body
    :start-optional t
    :end-optional t
    :content (+ TR)
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   #e"%cellhalign"	; horizontal alignment in cells
		   #e"%cellvalign"	; vertical alignment in cells
		   ))

;;COLGROUP groups a set of COL elements. It allows you to group
;;several semantically related columns together.

(define-html-element COLGROUP		; table column group
    :end-optional t
    :content (* col)
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   (span NUMBER 1)	; default number of columns in group
		   (width #e"%MultiLength" :implied) ; default width for enclosed COLs
		   #e"%cellhalign"	; horizontal alignment in cells
		   #e"%cellvalign"	; vertical alignment in cells
		   ))

;; COL elements define the alignment properties for cells in
;; one or more columns.
;;
;; The WIDTH attribute specifies the width of the columns, e.g.
;;
;;     width=64        width in screen pixels
;;     width=0.5*      relative width of 0.5
;;
;; The SPAN attribute causes the attributes of one
;; COL element to apply to more than one column.

(define-html-element COL		; table column
    :end-optional t
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   (span NUMBER 1)	; COL attributes affect N columns
		   (width #e"%MultiLength" :implied) ; column width specification
		   #e"%cellhalign"	; horizontal alignment in cells
		   #e"%cellvalign"	; vertical alignment in cells
		   ))

(define-html-element TR			; table row
    :end-optional t
    :content (+ (:or TH TD))
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   #e"%cellhalign"	; horizontal alignment in cells
		   #e"%cellvalign"	; vertical alignment in cells
		   (bgcolor #e"%Color" :implied) ; background color for row
		   ))

(define-html-element (:or TH TD)	; table header cell, table data cell
    :end-optional t
    :content (* #e"%flow")
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   (abbr #e"%Text" :implied) ; abbreviation for header cell
		   (axis CDATA :implied) ; names groups of related header
		   (headers IDREFS :implied) ; list of id's for header cells
		   (scope #e"%Scope" :implied) ; scope covered by header cells
		   (rowspan NUMBER 1)	; number of rows spanned by cell
		   (colspan NUMBER 1)	; number of cols spanned by cell
		   #e"%cellhalign"	; horizontal alignment in cells
		   #e"%cellvalign"	; vertical alignment in cells
		   (nowrap (:or nowrap) :implied) ; suppress word wrap
		   (bgcolor #e"%Color" :implied) ; cell background color
		   (width #e"%Pixels" :implied) ; width for cell
		   (height #e"%Pixels" :implied) ; height for cell
		   ))

;; ***Document Frames

;;  The content model for HTML documents depends on whether the HEAD is
;;  followed by a FRAMESET or BODY element. The widespread omission of
;;  the BODY start tag makes it impractical to define the content model
;;  without the use of a marked section.

(define-html-element IFRAME		; inline subwindow
    :content (* #e"%flow")
    :attributes (#e"%coreattrs"		; id, class, style, title
		   (longdesc #e"%URI" :implied) ; link to long description (complements title)
		   (name CDATA :implied) ; name of frame for targetting
		   (src #e"%URI" :implied) ; source of frame content
		   (frameborder (:or 1 0) 1) ; request frame borders?
		   (marginwidth #e"%Pixels" :implied) ; margin widths in pixels
		   (marginheight #e"%Pixels" :implied) ; margin height in pixels
		   (scrolling (:or yes no auto) auto) ; scrollbar or none
		   (align #e"%IAlign" :implied) ; vertical or horizontal alignment
		   (height #e"%Length" :implied) ; frame height
		   (width #e"%Length" :implied) ; frame width
		   ))

(define-html-element NOFRAMES		; alternate content container for non frame-based rendering
    :content #e"%noframes.content"
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   ))

;; ***Document Head

(define-html-element HEAD		; document head
    :start-optional t
    :end-optional t
    :content #e"%head.content"
    :inclusions #e"%head.misc"
    :attributes (#e"%i18n"		; lang, dir
		   (profile #e"%URI" :implied) ; named dictionary of meta info
		   ))

;; The TITLE element is not considered part of the flow of text.
;; It should be displayed, for example as the page header or
;; window title. Exactly one title is required per document.

(define-html-element TITLE		; document title
    :content PCDATA
    :exclusions #e"%head.misc"
    :attributes (#e"%i18n"))

(define-html-element ISINDEX		; single line prompt
    :end-optional t
    :attributes (#e"%coreattrs"		; id, class, style, title
		   #e"%i18n"		; lang, dir
		   (prompt #e"%Text" :implied) ; prompt message
		   ))

(define-html-element BASE		; document base URI
    :end-optional t
    :attributes ((href #e"%URI" :implied) ; URI that acts as base URI
		 (target #e"%FrameTarget" :implied) ; render in this frame
		 ))

(define-html-element META		; generic metainformation
    :end-optional t
    :attributes (#e"%i18n"		; lang, dir, for use with content
		   (http-equiv NAME :implied) ; HTTP response header name
		   (name NAME :implied)	; metainformation name
		   (content CDATA :required) ; associated information
		   (scheme CDATA :implied) ; select form of content
		   ))

(define-html-element STYLE		; style info
    :content #e"%StyleSheet"
    :attributes (#e"%i18n"		; lang, dir, for use with content
		   (type #e"%ContentType" :required) ; content type of style language
		   (media #e"%MediaDesc" :implied) ; designed for use with these media
		   (title #e"%Text" :implied) ; advisory title
		   ))

(define-html-element SCRIPT		; script statements
    :content #e"%Script"
    :attributes ((charset #e"%Charset" :implied) ; char encoding of linked resource
		 (type #e"%ContentType" :required) ; content type of script language
		 (language CDATA :implied) ; predefined script language name
		 (src #e"%URI" :implied) ; URI for an external script
		 (defer (:or defer) :implied) ; UA may defer execution of script
		 ))

(define-html-element NOSCRIPT		; alternate content container for non script-based rendering
    :content (* #e"%flow")
    :attributes (#e"%attrs"		; %coreattrs, %i18n, %events
		   ))

;; ***Document Structure

(define-html-element HTML		; document root element
    :start-optional t
    :end-optional t
    :content #e"%html.content"
    :attributes (#e"%i18n"		; lang, dir
		   ))

;;; Characters

;; We should have the corresponding character instead

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
