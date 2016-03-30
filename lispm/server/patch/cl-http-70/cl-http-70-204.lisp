;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: HTML4.0; Base: 10; Patch-File: T -*-
;;; Patch file for CL-HTTP version 70.204
;;; Reason: Don't inherit html3.2:declare-link
;;; 
;;; Function HTML4.0::DECLARE-LINK:  Define HTML 4 version.
;;; 
;;; Function HTML4.0::%LANGUAGE-ISO-CODE:  -
;;; Function HTML4.0::%WRITE-LANGUAGE-ARGUMENT:  update.
;;; Function HTML4.0::%WRITE-OBJECT-ARGUMENTS:  -
;;; Function HTML4.0::%WRITE-MEDIA-TYPE-ARGUMENT:  -
;;; Function HTML4.0:WITH-HTML-DOCUMENT:  -
;;; Variable HTML4.0:*DTD-VERSION*:  -
;;; Function HTML4.0:DECLARE-HTML-VERSION:  -
;;; Function HTML4.0::%WRITE-OBJECT-ARGUMENTS:  =
;;; Function HTML4.0::WITH-OBJECT:  -
;;; Function HTML4.0::%DECLARE-LINK:  -
;;; 
;;; Export HTML4.0::DECLARE-LINK.
;;; Written by jcma, 8/20/05 23:12:46
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/Plus-CL-HTTP-A-CSAIL-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, Metering 444.0,
;;; Metering Substrate 444.1, Conversion Tools 436.0, Hacks 440.0, CLIM 72.0,
;;; Genera CLIM 72.0, CLX CLIM 72.0, PostScript CLIM 72.0, CLIM Demo 72.0,
;;; CLIM Documentation 72.0, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.40, Genera 8 5 Macivory Support Patches 1.0,
;;; Genera 8 5 Metering Patches 1.0, Genera 8 5 Joshua Patches 1.0,
;;; Genera 8 5 Jericho Patches 1.0, Genera 8 5 Joshua Doc Patches 1.0,
;;; Genera 8 5 Joshua Metering Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.3,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clx Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Clim Demo Patches 1.0, Genera 8 5 Color Patches 1.1,
;;; Genera 8 5 Images Patches 1.0, Genera 8 5 Color Demo Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; Genera 8 5 Concordia Patches 1.2, Genera 8 5 Concordia Doc Patches 1.0,
;;; Genera 8 5 C Patches 1.0, Genera 8 5 Pascal Patches 1.0,
;;; Genera 8 5 Fortran Patches 1.0, MAC 415.2, MacIvory Support 447.0,
;;; MacIvory Development 434.0, Color 427.1, Graphics Support 431.0,
;;; Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Color System Documentation 10.0, SGD Book Design 10.0, Color Demo 422.0,
;;; Images 431.2, Image Substrate 440.4, Statice Runtime 466.1, Statice 466.0,
;;; Statice Browser 466.0, Statice Server 466.2, Statice Documentation 426.0,
;;; Symbolics Concordia 444.0, Graphic Editor 440.0, Graphic Editing 441.0,
;;; Bitmap Editor 441.0, Graphic Editing Documentation 432.0, Postscript 436.0,
;;; Concordia Documentation 432.0, Joshua 237.6, Joshua Documentation 216.0,
;;; Joshua Metering 206.0, Jericho 237.0, C 440.0, Lexer Runtime 438.0,
;;; Lexer Package 438.0, Minimal Lexer Runtime 439.0, Lalr 1 434.0,
;;; Context Free Grammar 439.0, Context Free Grammar Package 439.0, C Runtime 438.0,
;;; Compiler Tools Package 434.0, Compiler Tools Runtime 434.0, C Packages 436.0,
;;; Syntax Editor Runtime 434.0, C Library Headers 434,
;;; Compiler Tools Development 435.0, Compiler Tools Debugger 434.0,
;;; C Documentation 426.0, Syntax Editor Support 434.0, LL-1 support system 438.0,
;;; Fortran 434.0, Fortran Runtime 434.0, Fortran Package 434.0, Fortran Doc 427.0,
;;; Pascal 433.0, Pascal Runtime 434.0, Pascal Package 434.0, Pascal Doc 427.0,
;;; HTTP Server 70.203, Showable Procedures 36.3, Binary Tree 34.0,
;;; Experimental W3 Presentation System 8.2, CL-HTTP Server Interface 54.0,
;;; HTTP Proxy Server 6.34, HTTP Client Substrate 4.23,
;;; W4 Constraint-Guide Web Walker 45.13, HTTP Client 51.9, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.18, DEC OSF/1 V4.0 (Rev. 110),
;;; 1728x1062 24-bit TRUE-COLOR X Screen FUJI:1.0 with 224 Genera fonts (The Olivetti & Oracle Research Laboratory R3323),
;;; Machine serial number -2142637960,
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7),
;;; Get Xauthority pathname from user namespace object. (from W:>jcma>fixes>xauthority-pathname.lisp.2),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;HTML4.LISP.16"
  "HTTP:SERVER;HTML4.LISP.18")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.16")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (load eval compile)
(unexport 'html3.2:declare-link :html4.0)
(shadow 'html3.2:declare-link :html4.0)
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.16")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;; define the languages sometime.   3/23/99 -- JCMa.
(defun %language-iso-code (language &optional (error-p t))
  (declare (ignore error-p))
  language)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.16")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun %write-language-argument (stream language)
  (html2::%write-command-key-arg stream "LANG" (%language-iso-code language t)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.18")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(PROGN

(defun %write-charset-argument (stream charset)
  (html2::%write-command-key-arg stream "CHARSET" charset))

(defun %write-media-argument (stream media)
  (html2::%write-command-key-arg stream "MEDIA" media))

(defun %write-direction-argument (stream direction)
  (html2::%write-command-key-arg stream "DIR" (ecase direction
					 (:left-to-right "LTR")
					 (:right-to-left "RTL"))))

(declaim (inline %write-title-argument))

(defun %write-title-argument (stream title)
  (html2::%write-command-key-arg stream "TITLE" title))

(declaim (inline %write-target-argument))

(defun %write-target-argument (stream target)
  (html2::%write-command-key-arg stream "TARGET" target))
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.18")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;;;------------------------------------------------------------------- 
;;;
;;; EMBEDDED OBJECTS
;;;

(defun %write-media-type-argument (stream tag media-type-spec)
  (typecase media-type-spec
    (cons
      (flet ((writer (stream)
	       (write-char #\" stream)
	       (http::write-mime-content-type media-type-spec stream)
	       (write-char #\" stream)))
	(declare (dynamic-extent #'writer))
	(html2::%write-command-key-arg stream tag #'writer)))
    (t (html2::%write-command-key-arg stream tag media-type-spec))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.18")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;;;------------------------------------------------------------------- 
;;;
;;;  DOCUMENT LEVEL OPERATIONS
;;;

(lisp:compiler-let ((SYS:INHIBIT-FDEFINE-WARNINGS nil))
(defconstant *dtd-version* "-//W3C//DTD HTML 4.01//EN")
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.18")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define declare-html-version (&optional (stream *output-stream*) (dtd-version :frameset))
  "Declares the document type as the current HTML generation DTD.
All HTML 4.0 must declare the document type definition version.

   DTD-VERSION can be any of:

      :STRICT       - includes all elements that have not been deprecated and do not appear in frameset documents.
      :TRANSITIONAL - includes everything is the STRICT DTD plus deprecated elements and attributes.
      :FRAMESET     - includes everything in TRANSITIONAL plus frames."
  (when dtd-version
    (%issue-command ("!DOCTYPE HTML PUBLIC" stream :fresh-line t :trailing-line t)
      (ecase dtd-version
        ((:frameset t)
         (fast-format stream " ~S ~S" "-//W3C//DTD HTML 4.0 Frameset//EN" "http://www.w3.org/TR/REC-html40/frameset.dtd"))
        (:strict
	  (fast-format stream " ~S ~S" *dtd-version* "http://www.w3.org/TR/REC-html40/strict.dtd"))
        (:transitional
	  (fast-format stream " ~S ~S" "-//W3C//DTD HTML 4.0 Transitional//EN" "http://www.w3.org/TR/REC-html40/loose.dtd"))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.18")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun %write-object-arguments (stream args)
  (destructuring-bind (name data code resources base data-type code-type height width
			    client-side-image-map title progress-message
			    language direction
			    style class id tab-position events)
      args
    (cond-every
      (name (%write-name-argument stream name))
      (data (html2::%write-command-key-arg stream "DATA" (coerce-url-string data)))
      (code (html2::%write-command-key-arg stream "CLASSID" (coerce-url-string code)))
      (resources (%write-archive-argument stream resources))
      (base (html2::%write-command-key-arg stream "CODEBASE" (coerce-url-string base)))
      (data-type (%write-media-type-argument stream "TYPE" data-type))
      (code-type (%write-media-type-argument stream "CODETYPE" code-type))
      (height (%write-length-argument "HEIGHT" height stream))
      (width (%write-length-argument "WIDTH" width stream))
      (client-side-image-map
	(html2::%write-command-key-arg stream "USEMAP" (url:name-string-without-search-suffix client-side-image-map nil)))
      (title (html2::%write-command-key-arg stream "TITLE" title))
      (progress-message (html2::%write-command-key-arg stream "STANDBY" progress-message))
      (language (%write-language-argument stream language))
      (direction (%write-direction-argument stream direction))
      (style (%write-style-argument stream style))
      (class (%write-class-argument stream class))
      (id (%write-id-argument stream id))
      (tab-position (%write-tab-position-argument stream tab-position))
      (events
	(dolist (event events)
	  (html2::%write-input-type-event-arg stream event))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.18")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-macro with-object ((&key name title code data resources base code-type data-type client-side-image-map
				 height width progress-message (alternative-text nil alternative-text-supplied-p)
				 language direction events
				 style class id tab-position (stream *output-stream*)) &body body)
  "Allows embedding of inline objects within HTML documents.
This generalization of inline images and inline applets provides a powerful
mechanism to include data or program output. This operator declares the object
and associates it with NAME. Objects may appear in the document preamble only
when they do not render data, for example, in the case of a program object.
In other cases, for example, when only DATA is specified, objects must appear
in the document body. When the object is a program requiring parameters, the
function NOTE-PARAMETER should be called with BODY for each parameter.
         
  NAME is a string denoting the name for the element.
  TITLE is a string used as a caption.
  CODE may a URI specifying the location of the program that performs the rendering.
  DATA may a URI specifying the location of data to be rendered.
  RESOURCES can be a list of URIs containing resources relevant to this object.
  CODE-TYPE is the media type of CODE, which can be content type list, a string, or a function.
  DATA-TYPE is the media type of DATA, which can be content type list, a string, or a function.
  BASE is a base URI against which to merge any relative URIs appearing in CODE, DATA, or resources.
  CLIENT-SIDE-IMAGE-MAP indicates the client side image map to use.
   Normally, this is a named URI (/url.html#map-name) and often, all
   client side image maps are served from a single url. The function
   WRITE-CLIENT-SIDE-IMAGE-MAP writes a client side image
   map from a server-side image map URL (CERN or NCSA formats).
  CLIENT-SIDE-IMAGE-MAP
  HEIGHT is either the number of pixels or a fraction of the window.
  WIDTH is either the number of pixels or fraction of the window.
  PROGRESS-MESSAGE is a string or function that provides a message to the user while the object loads.
  ALTERNATIVE-TEXT is a string or function that provides a message for clients unable to display the object.
  LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
  DIRECTION is ht base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT.
  STYLE denotes the style sheet to use.
  CLASS is the class for the element
  ID is an element identifier.
  TAB-POSITION is a number that specifies the position of the element in the current document.
  The number must be between 0 and 32767."
  `(%with-environment ("OBJECT" :stream ,stream)
		      (let ((args (list ,name ,data ,code ,resources ,base ,data-type ,code-type ,height ,width
					,client-side-image-map ,title ,progress-message
					,language ,direction
					,style ,class ,id ,tab-position ,events)))
			(declare (dynamic-extent args))
			(%write-object-arguments ,stream args))
		     
     ,@body
     ,@(when alternative-text-supplied-p
	 `((%write-alterative-text ,stream ,alternative-text)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.18")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-macro with-html-document ((&key (stream '*output-stream*) (declare-dtd-version-p t)
					(language nil language-supplied-p) (direction nil direction-supplied-p))
				  &body body)
  "Asserts the contents of BODY is an HTML document.

  DECLARE-DTD-VERSION-P will declare the version of the DTD implemented by the current generation
  package. This should be T whenever generation strictly conforms to the HTML version
  associated with the macro WITH-HTML-DOCUMENT. HTML 4.0 offers three DTD versions.
  Consequently, DECLARE-DTD-VERSION-P can be any of :FRAMESET, :TRANSITIONAL, or :STRICT.
  A value of T is interpreted as :FRAMESET. DECLARE-DTD-VERSION-P should always be NIL,
  whenever extension tags or features outside these specifications are used.

  LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)

  DIRECTION is ht base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT."
  (let ((args `(,.(when language-supplied-p
		    `((%write-language-argument ,stream ,language)))
		,.(when direction-supplied-p
		    `((%write-direction-argument ,stream ,direction)))))
	(version-declaration (case declare-dtd-version-p
                               ((nil) nil)
                               ((t) `(declare-html-version ,stream :frameset))
                               (:transitional `(declare-html-version ,stream :transitional))
                               (:strict `(declare-html-version ,stream :strict))
                               (t `(declare-html-version ,stream ,declare-dtd-version-p)))))
    (if version-declaration
	`(progn ,version-declaration
		(%with-environment ("HTML" :stream ,stream)
				   ,(when args (cons 'progn args))
		  . ,body))
	`(%with-environment ("HTML" :stream ,stream)
			    ,(when args (cons 'progn args))
	   . ,body))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.18")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(declaim (inline %write-link-command-arguments))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.18")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(declaim (inline declare-link))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.18")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun %write-link-command-arguments (stream reference local-reference charset link-language media-type relation inverse media 
					     id class language direction title style events target)
  (cond ((and local-reference reference)
	 (html2::%write-href-with-local-reference stream reference local-reference))
        (reference ;; URL for a anchored document retrieved by the anchor.
         ;; ensure generated URLs are escaped.
         (html2::%write-command-key-arg stream "HREF" (typecase reference
                                                 (function reference)
                                                 (t (coerce-url-string reference url:*escape-search-urls*)))))
        (local-reference ;; tags for a position in the current document.
         (fast-format stream " HREF=#~A" local-reference)))

  (cond-every 
    (charset
      (%write-charset-argument stream charset))
    (link-language
      (html2::%write-command-key-arg stream "HREFLANG" (%language-iso-code language t)))
    (media-type
      (%write-media-type-argument stream "TYPE" media-type))
    (relation ;; "The relationship the anchored document has to this one."
      (html2::%write-command-key-arg stream "REL" relation)) 
    (inverse ;; "The reverse relationship type, and the inverse of REL."
      (html2::%write-command-key-arg stream "REV" inverse))
    (media
      (%write-media-argument stream media))
    ;; inherited attributes
    (id (%write-id-argument stream id))
    (class (%write-class-argument stream class))
    (language
      (%write-language-argument stream language))
    (direction
      (%write-direction-argument stream direction))
    (title ;;  "Title to use for otherwise untitled anchored document."
      (%write-title-argument stream title))
    (style (%write-style-argument stream style))
    (events (html2::%write-input-type-event-arg stream events))
    (target
      (%write-target-argument stream target))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.18")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun %declare-link (stream reference local-reference charset link-language media-type relation inverse media
			     id class language text-direction title style events target)
  (%issue-command ("LINK" stream :fresh-line t :trailing-line t)
    (%write-link-command-arguments stream reference local-reference charset link-language media-type relation inverse media 
				   id class language text-direction title style events target)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.18")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define declare-link (&key reference local-reference charset link-language media-type relation inverse media 
			   id class language text-direction title style events target (stream *output-stream*)) 
  (%declare-link stream reference local-reference charset link-language media-type relation inverse media
		 id class language text-direction title style events target))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.18")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(setf (documentation 'declare-link 'function)
      "Declares a link between the current document and other documents or resources.
   REFERENCE is a URL (object or string) denoting the target resource.
   LOCAL-REFERENCE is a string denoting a named point in the document which can be referenced with a local reference (#name).
   RELATION is the link type from the referring document to the the target document.
   INVERSE is the inverse relationship back from the anchored document to the referring document.
   MEDIA-TYPE is a MIME content type for the linked URL.
   TITLE is title to use for otherwise untitled anchored document.

LINK elements can be used
     a. for document specific navigation toolbars or menus 
     b. for controlling how collections of HTML files are rendered into printed documents 
     c. for linking associated resources such as style sheets and scripts 
     d. for providing alternative forms of the current document 

Some proposed values for RELATION:

TOP       -- The link references the top of a hierarchy, e.g. the first or cover page in a collection. 
CONTENTS  -- The link references a document serving as a table of contents. 
INDEX     -- The link references a document providing an index for the current document. 
GLOSSARY  -- The link references a document providing a glossary of terms that are relevant to 
             the current document. 
COPYRIGHT -- The link references a copyright statement for the current document. 
NEXT      -- The link references the next document to visit in a guided tour. It can be used, for example, 
             to preload the next page. 
PREVIOUS  -- The link references the previous document in a guided tour. 
HELP      -- The link references a document offering help, e.g. describing the wider context and offering 
             further links to relevant documents. This is aimed at reorienting users who have lost their way. 
SEARCH    -- The link references a page for searching material related to a collection of pages.

     Examples:

     (declare-link :RELATION \"Contents\" :reference \"toc.html\")
     (declare-link :RELATION \"Previous\" :reference \"doc31.html\")
     (declare-link :RELATION \"Next\" :reference \"doc33.html\")
     (declare-link :RELATION \"Chapter\" :inverse \"Contents\" :reference \"Chapter2.html\")")

(eval-when (load)
  (export (intern "DECLARE-LINK" :html4.0)))