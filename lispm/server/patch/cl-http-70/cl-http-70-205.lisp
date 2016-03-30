;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: HTML4.0; Base: 10; Patch-File: T -*-
;;; Patch file for CL-HTTP version 70.205
;;; Reason: Define some basic HTML 4.0.1 Generation Macros
;;; 
;;; Function HTML4.0::WITH-DOCUMENT-PREAMBLE:  new
;;; Function HTML4.0::DECLARE-TITLE:  new
;;; Function HTML4.0::DECLARE-META-INFO:  new
;;; Function HTML4.0::DECLARE-CONTENT-STYLE-TYPE:  new
;;; Function HTML4.0::WITH-DOCUMENT-STYLE-DECLARED:  new
;;; Function HTML4.0::WITH-DOCUMENT-BODY:  new
;;; Function HTML4.0::WITH-SECTION-HEADING:  new
;;; Function HTML4.0::WITH-DIVISION:  new
;;; Function HTML4.0::WITH-TEXT-BLOCK-STYLE:  new abstraction.
;;; Function HTML4.0::WITH-PARAGRAPH:  new
;;; Function HTML4.0::WITH-PARAGRAPH-STYLE:  new.
;;; Function HTML4.0::WITH-EMPHASIS:  new
;;; Function HTML4.0::WITH-RENDITION:  new
;;; Function HTML4.0::WITH-VERBATIM-TEXT:  new
;;; Function HTML4.0::HORIZONTAL-LINE:  new
;;; Function HTML4.0::BREAK-LINE:  new
;;; 
;;; DEFINE-HEADER :CONTENT-STYLE-TYPE:  new HTML 4.01 header.
;;; DEFINE-HEADER :CONTENT-TYPE:  correct print string case.
;;; 
;;; Function HTML4.0::%DECLARE-LINK:  rename arg.
;;; Function HTML4.0::%WRITE-STANDARD-COMMAND-ARGUMENTS:  new
;;; Function HTML4.0::%WRITE-LINK-COMMAND-ARGUMENTS:  use %write-standard-command-arguments
;;; Function HTML4.0::%DECLARE-LINK:  recompile.
;;; Function HTML4.0::%WRITE-DIVISION-COMMAND-ARGUMENTS:  -
;;; Function HTTP::WRITE-MIME-CONTENT-TYPE-AS-STRING:  new.
;;; Function HTML4.0::%WRITE-MEDIA-ARGUMENT:  add choices.
;;; Function HTML4.0::%WRITE-STYLE-COMMAND-ARGUMENTS:  -
;;; Function HTML4.0::%WRITE-BODY-COMMAND-ARGUMENTS:  -
;;; Function HTML4.0::%WRITE-BODY-COMMAND-ARGUMENTS:  -
;;; Function HTML4.0::%WRITE-PROFILE-COMMAND-ARGUMENTS:  -
;;; Function HTML4.0::%WRITE-LANGUAGE-AND-DIRECTION-COMMAND-ARGUMENTS:  -
;;; Function HTML4.0::SECTION-HEADING:  -
;;; Function HTML4.0::%WRITE-TEXT-BLOCK-STYLE-ARGUMENTS:  -
;;; Written by jcma, 8/21/05 11:43:29
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
;;; HTTP Server 70.204, Showable Procedures 36.3, Binary Tree 34.0,
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
  "HTTP:SERVER;HTML4.LISP.23"
  "HTTP:SERVER;HEADERS.LISP.539"
  "HTTP:SERVER;HEADERS.LISP.541"
  "HTTP:SERVER;HTML4.LISP.24"
  "HTTP:SERVER;HTML4.LISP.25"
  "HTTP:SERVER;HTML4.LISP.26"
  "HTTP:SERVER;HTML4.LISP.29"
  "HTTP:SERVER;HTML4.LISP.30"
  "HTTP:SERVER;HTML4.LISP.32"
  "HTTP:SERVER;HTML4.LISP.33"
  "HTTP:SERVER;HTML4.LISP.34"
  "HTTP:SERVER;HTML4.LISP.36"
  "HTTP:SERVER;HTML4.LISP.37"
  "HTTP:SERVER;HTML4.LISP.38"
  "HTTP:SERVER;HTML4.LISP.39"
  "HTTP:SERVER;HTML4.LISP.40"
  "HTTP:SERVER;HTML4.LISP.41"
  "HTTP:SERVER;HTML4.LISP.42")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.23")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (load eval compile)
(unexport 'html3.2:with-division :html4.0)
(shadow 'html3.2:with-division :html4.0))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.23")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun %write-link-command-arguments (stream reference local-reference charset link-language media-type relation inverse media 
					     id class language direction title style events target)
  (cond ((and local-reference reference)
	 (html2::%write-href-with-local-reference stream reference local-reference))
        (reference ;; URL for a anchored document retrieved by the anchor.
         ;; ensure generated URLs are escaped.
         (%write-command-key-arg stream "HREF" (typecase reference
                                                 (function reference)
                                                 (t (coerce-url-string reference url:*escape-search-urls*)))))
        (local-reference ;; tags for a position in the current document.
         (fast-format stream " HREF=#~A" local-reference)))
  (cond-every 
    (relation ;; "The relationship the anchored document has to this one."
      (%write-command-key-arg stream "REL" relation)) 
    (inverse ;; "The reverse relationship type, and the inverse of REL."
      (%write-command-key-arg stream "REV" inverse))
    (charset
      (%write-charset-argument stream charset))
    (link-language
      (%write-command-key-arg stream "HREFLANG" (%language-iso-code language t)))
    (media-type
      (%write-media-type-argument stream "TYPE" media-type))
    (media
      (%write-media-argument stream media))
    (target
      (%write-target-argument stream target)))

  (%write-standard-command-arguments stream id class language direction title style events))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.23")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun %declare-link (stream reference local-reference charset link-language media-type relation inverse media
			     id class language direction title style events target)
  (%issue-command ("LINK" stream :fresh-line t :trailing-line t)
    (%write-link-command-arguments stream reference local-reference charset link-language media-type relation inverse media 
				   id class language direction title style events target)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.23")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (load)
(export (intern "WITH-DIVISION" :html4.0) :html4.0))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.539")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;;;------------------------------------------------------------------- 
;;;
;;; HTML 4.01 EXTENSION HEADERS
;;;

(define-header :content-style-type
               (:content-type-header :entity)
  :print-string "Content-Style-Type")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.539")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :content-type
               (:content-type-header :entity)
  :print-string "Content-Type")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.541")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun write-mime-content-type-as-string (content-type &optional stream)
  (write-string #\" stream)
  (print-mime-content-type-header content-type stream)
  (write-string #\" stream))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.24")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define declare-content-style-type (media-type &optional (stream *output-stream*))
  "Declares the default style sheet for a document to be MEDIA-TYPE.
Use only inside the document preamble."
  (flet ((writer (stream)
	   (HTTP::write-mime-content-type-as-string media-type stream)))
    (declare (dynamic-extent #'writer))
    (declare-meta-info #'writer :header :content-style-type :stream stream)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.24")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (load)
(export (intern "DECLARE-CONTENT-STYLE-TYPE" :html4.0) :html4.0))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.25")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun %write-media-type-argument (stream tag media-type-spec)
  (typecase media-type-spec
    (cons
      (flet ((writer (stream)
	       (http::write-mime-content-type-as-string media-type-spec stream)))
	(declare (dynamic-extent #'writer))
	(%write-command-key-arg stream tag #'writer)))
    (t (%write-command-key-arg stream tag media-type-spec))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.26")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun %write-media-argument (stream media)
  (%write-command-key-arg stream "MEDIA" (ecase media
					   ((:screen :tty :tv :projection :handheld :print :braile :aural :all)
					    media))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.29")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-macro with-document-style-declared ((media-type &key media title language direction (stream *output-stream*))
					    &body body)
  "Provides a preamble ennvironment for declaring style characteristics of a document.
  Code with BODY must write style sheet parameters to STREAM,

  MEDIA-TYPE is a MIME content type for style sheet in use.
  MEDIA is a descriptor for the target display. 
  Choices are: :SCREEN, :TTY, :TV, :PROJECTION, :HANDHELD, :PRINT, :BRAILE, :AURAL, OR :ALL.
  TITLE is a string used as the element title
  LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
  DIRECTION is ht base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT."
  `(%with-environment 
     ("STYLE" :stream ,stream)
     (%write-style-command-arguments ,stream ,media-type ,media ,title ,language ,direction)
     ,@body))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.30")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(PROGN
(defconstant *horizontal-alignment-values* '(:left :center :right :justify))

(defun horizontal-alignment-value (alignment)
  (unless (member alignment *horizontal-alignment-values*)
    (error "Unknown alignment, ~S, for a paragraph or division." alignment))
  (symbol-name alignment))
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.30")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun %write-division-command-arguments (stream alignment class id title style language direction events)
  (when alignment 
    (%write-command-key-arg stream "ALIGN" (horizontal-alignment-value alignment)))
  (%write-standard-command-arguments stream id class language direction title style events))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.30")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-macro with-division ((&key inline-p alignment class id title style language direction events (stream '*output-stream*))
			     &body body)
  "Establishes a block-level division or inline span environment according to INLINE-P.
  ALIGNMENT can be any of :LEFT, :CENTER, RIGHT, or :JUSTIFY.
  CLASS is the class for the element
  ID is an element identifier.
  TITLE is a string used as a caption.
  STYLE denotes the style sheet to use.
  LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
  DIRECTION is ht base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT."
  `(%with-environment 
     (,(case inline-p
	 ((t) "SPAN")
	 ((nil) "DIV")
	 (t `(if ,inline-p "SPAN" "DIV")))
      :stream ,stream)
     (%write-division-command-arguments ,stream ,alignment ,class ,id ,title ,style ,language ,direction ,events)
     ,@body))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.30")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (eval compile load)
  (unexport 'html3.2::with-paragraph :html4.0)
  (shadow (intern "WITH-PARAGRAPH" :html4.0) :html4.0))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.30")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (load)
  (export (intern "WITH-PARAGRAPH" :html4.0) :html4.0))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.32")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun %write-body-command-arguments (stream class id title style language direction events
					     background-url background foreground link visited-link active-link)
  (%write-standard-command-arguments stream id class language direction title style events)
  ;; write deprecated parameters
  (cond-every
    (background-url
      (%write-command-key-arg stream "BACKGROUND" (typecase background-url
						    (keyword (background-url background-url))
						    (t (url:coerce-url-string background-url nil)))))
    (background 
      (%write-command-key-arg stream "BGCOLOR" (color-mapping background)))
    (foreground 
      (%write-command-key-arg stream "TEXT" (color-mapping foreground)))
    (link 
      (%write-command-key-arg stream "LINK" (color-mapping link)))
    (visited-link 
      (%write-command-key-arg stream "VLINK" (color-mapping visited-link)))
    (active-link 
      (%write-command-key-arg stream "ALINK" (color-mapping active-link)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.32")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;;;------------------------------------------------------------------- 
;;;
;;; DOCUMENT PREAMBLE AND BODY
;;;


(define-macro with-document-body ((&key class id title style language direction events
					background-url background foreground link visited-link active-link
                                        (stream '*output-stream*)) &body body)
  "Asserts the contents of BODY are the body of the document (see BODY).

  CLASS is the class for the element
  ID is an element identifier.
  TITLE is a string used as a caption.
  STYLE denotes the style sheet to use.
  LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
  DIRECTION is ht base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT.

  Deprecated arguments are superseded by style sheets in HTML 4.0.1.

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
     ("BODY" :stream ,stream)
      (%write-body-command-arguments ,stream ,class ,id ,title ,style ,language ,direction ,events
					     ,background-url ,background ,foreground ,link ,visited-link ,active-link)
     ,@body))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.32")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (load)
  (export (intern "WITH-DOCUMENT-BODY" :html4.0) :html4.0))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.32")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (eval compile load)
  (unexport 'html3.2::with-document-preamble :html4.0)
  (shadow (intern "WITH-DOCUMENT-PREAMBLE" :html4.0) :html4.0))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.32")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-macro with-document-preamble ((&key profile language direction (stream '*output-stream*)) &body body)
  "Asserts the contents of BODY are headings for the document (see HEAD).
  PROFILE is a URI or a list of URIs thatspecifies the location of one or more meta data profiles.
  LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
  DIRECTION is ht base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT."
  `(%with-environment 
     ("HEAD" :stream ,stream)
     ,(when (or profile language direction)
	`(%write-profile-command-arguments ,stream ,profile ,language ,direction))
     ,@body))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.32")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (load)
  (export (intern "WITH-DOCUMENT-PREAMBLE" :html4.0) :html4.0))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.33")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (eval compile load)
  (unexport 'html3.2::declare-title :html4.0)
  (shadow (intern "DECLARE-TITLE" :html4.0) :html4.0))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.33")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun %write-language-and-direction-command-arguments (stream language direction)
  (cond-every
    (language
      (%write-language-argument stream language))
    (direction
      (%write-direction-argument stream direction))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.33")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun %write-profile-command-arguments (stream profile language direction)
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
     (%write-language-and-direction-command-arguments stream language direction))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.33")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun %write-style-command-arguments (stream media-type media title language direction)
  (cond-every
    (media-type
      (%write-media-type-argument stream "TYPE" media-type))
    (media
      (%write-media-argument stream media))
    (title
      (%write-title-argument stream title))
    ((or language direction)
     (%write-language-and-direction-command-arguments stream language direction))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.33")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define declare-title (title &key language direction (stream *output-stream*))
  "Declares the title for a document.
Every HTML document must declare exactly one TITLE element in the preamble.

  TITLE can be either a string or a function which is called with (STREAM).
  LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
  DIRECTION is ht base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT."
  (%with-environment 
    ("TITLE" :stream stream)
    (when (or language direction)
      (%write-language-and-direction-command-arguments stream language direction))
    (etypecase title
      (string (write-string title stream))
      (function (funcall title stream)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.33")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (load)
  (export (intern "DECLARE-TITLE" :html4.0) :html4.0))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.34")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (eval compile load)
  (unexport 'html3.2::declare-meta-info :html4.0)
  (shadow (intern "DECLARE-META-INFO" :html4.0) :html4.0))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.34")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define declare-meta-info (value &key name header scheme language direction (stream *output-stream*))
  "Declares meta information within an HTML document.
NAME and/or HEADER specify the type of meta information.
VALUE is the content of the declaration.
HEADER is used only for defined HTTP response header information.
NAME is used in other cases.
SCHEME specifies how VALUE is interpreted based on the document profile.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
  DIRECTION is ht base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT."
  (unless (or name header)
    (error "At least one of NAME or HEADER must be provided."))
  (%issue-command ("META" stream :fresh-line t :trailing-line t)
    (cond-every
      (name (%write-command-key-arg stream "NAME" name))
      (header
        (check-type header keyword)
        (%write-command-key-arg stream "HTTP-EQUIV" (symbol-name header)))
      (scheme
	(%write-command-key-arg stream "SCHEME" scheme))
      ((or language direction)
       (%write-language-and-direction-command-arguments stream language direction)))
    (%write-command-key-arg stream "CONTENT" value)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.36")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (eval compile load)
  (unexport 'html3.2::with-section-heading :html4.0)
  (shadow (intern "WITH-SECTION-HEADING" :html4.0) :html4.0))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.36")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun section-heading (heading level stream &optional alignment class id 
				title style language direction events &aux command)
  (setq command (case level
                  (1 "H1")
                  (2 "H2")
                  (3 "H3")
                  (4 "H4")
                  (5 "H5")
                  (6 "H6")
                  (t (cond ((< 6 level) (error "Exceeded the maximun section heading depth of 6."))
                           ((< level 1) (error "SECTION-HEADING called outside of WITH-SECTION-HEADING."))))))
  (%with-environment (command :stream stream :fresh-line t)
		     (%write-division-command-arguments stream alignment class id title style language direction events)
    (etypecase heading
      (string (write-string heading stream))
      (function (funcall heading stream)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.36")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-macro with-section-heading ((heading &key  alignment class id title style language direction events
					     (level '*section-level*)
                                             (stream '*output-stream*)) &body body)
  "Excutes BODY within a section heading of depth LEVEL.
HEADING can be a string or a function called with (STREAM).
  ALIGNMENT can be any of :LEFT, :CENTER, RIGHT, or :JUSTIFY.
  CLASS is the class for the element
  ID is an element identifier.
  TITLE is a string used as a caption.
  STYLE denotes the style sheet to use.
  LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
  DIRECTION is ht base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT."
  (if body
      `(progn
	 (section-heading ,heading ,level ,stream ,alignment ,class ,id ,title ,style ,language ,direction ,events)
         (let ((*section-level* (the fixnum (1+ ,level))))
           ,@body))
      `(section-heading ,heading ,level ,stream ,alignment ,class ,id ,title ,style ,language ,direction ,events)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.36")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (load)
  (export (intern "WITH-SECTION-HEADING" :html4.0) :html4.0))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.37")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun %write-text-block-style-arguments (stream paragraph-style cite class id title style language direction events)
  (when (and cite (member paragraph-style '(:quotation)))
    (%write-command-key-arg stream "CITE" (url:coerce-url-string cite)))
  (%write-standard-command-arguments stream id class language direction title style events))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.37")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-macro with-text-block-style ((block-style &key cite class id title style language direction events
						  (fresh-line t) (stream '*output-stream*)) &body body)
  #.(format nil "Renders the enclosed text block as BLOCK-STYLE, which can be any of: ~{~S~^, ~}.
  CITE is a URI designating the source of the block (only available when BLOCK-STYLE is :QUOTATION). 
  CLASS is the class for the element
  ID is an element identifier.
  TITLE is a string used as an element title.
  STYLE denotes the style sheet to use.
  LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
  DIRECTION is ht base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT."
	    (mapcar #'car html3.2::*paragraph-styles*))
  (typecase block-style 
    (keyword
      `(%with-environment
	 (,(html3.2::paragraph-style-command block-style) :stream ,stream :fresh-line ,fresh-line)
	 ,(case block-style
	    (:quotation
	      `(%write-text-block-style-arguments
		 ,stream ,block-style ,cite ,class ,id ,title ,style ,language ,direction events))
	    (t `(%write-standard-command-arguments ,stream ,id ,class ,language ,direction ,title ,style ,events)))
	 ,@body))
    (t `(let ((block-style ,block-style))
	  (%with-environment
	    ((block-style-command block-style) :stream ,stream :fresh-line ,fresh-line)
	    (%write-text-block-style-arguments
	      ,stream block-style ,cite ,class ,id ,title ,style ,language ,direction events)
	    ,@body)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.37")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (load)
  (export (intern "WITH-TEXT-BLOCK-STYLE" :html4.0) :html4.0))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.37")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-macro with-paragraph ((&key alignment class id title style language direction events 
				    (fresh-line t) (stream '*output-stream*))
			      &body body)
  "Establishes a paragraph environment.
  ALIGNMENT can be any of :LEFT, :CENTER, RIGHT, or :JUSTIFY.
  CLASS is the class for the element
  ID is an element identifier.
  TITLE is a string used as an element title.
  STYLE denotes the style sheet to use.
  LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
  DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT."
  `(%with-environment 
     ("P" :stream ,stream :fresh-line ,fresh-line)
     (%write-division-command-arguments ,stream ,alignment ,class ,id ,title ,style ,language ,direction ,events)
     ,@body))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.37")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (eval compile load)
  (unexport 'html3.2::with-paragraph-style :html4.0)
  (shadow (intern "WITH-PARAGRAPH-STYLE" :html4.0) :html4.0))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.37")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-macro with-paragraph-style ((paragraph-style &key alignment cite class id title style language direction events
						     (fresh-line t) (stream '*output-stream*)) &body body)
  #.(format nil "Renders the enclosed text block as PARAGRAPH-STYLE, which can be any of: ~{~S~^, ~}.
  ALIGNMENT can be any of :LEFT, :CENTER, RIGHT, or :JUSTIFY.
  CITE is a URI designating the source of the block (only available when BLOCK-STYLE is :QUOTATION). 
  CLASS is the class for the element
  ID is an element identifier.
  TITLE is a string used as an element title.
  STYLE denotes the style sheet to use.
  LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
  DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT."
	    (mapcar #'car html3.2::*paragraph-styles*))
  `(with-paragraph (:alignment ,alignment :fresh-line ,fresh-line :stream ,stream)
     (with-text-block-style (,paragraph-style :cite ,cite :class ,class :id ,id :title ,title
			     :style ,style :language ,language :direction ,direction :events ,events
			     :fresh-line ,fresh-line :stream ,stream)
       ,@body)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.37")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (load)
  (export (intern "WITH-PARAGRAPH-STYLE" :html4.0) :html4.0))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.38")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (eval compile load)
  (unexport 'html3.2::with-emphasis :html4.0)
  (shadow (intern "WITH-EMPHASIS" :html4.0) :html4.0))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.38")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(PROGN
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defconstant *emphasis*
               `((:emphasis . "EM")
                 (:strong . "STRONG")
                 (:address . "ADDRESS")
                 (:quotation . "BLOCKQUOTE")
		 (:quote . "Q")
                 (:definition . "DFN")
                 (:citation . "CITE")
                 (:keyboard . "KBD")
                 (:variable . "VAR")
                 (:code . "CODE")
                 (:sample . "SAMP")
		 (:abbreviation . "ABBR")
		 (:acronym . "ACRONYM"))))

(defun emphasis-command (emphasis)
  (or (cdr (assoc emphasis *emphasis*))
      (error "Unknown type of emphasis, ~S." emphasis)))

(defun %write-emphasis-arguments (stream emphasis cite class id title style language direction events)
  (when (and cite (member emphasis '(:quotation :quote)))
    (%write-command-key-arg stream "CITE" (url:coerce-url-string cite)))
  (%write-standard-command-arguments stream id class language direction title style events))
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.38")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

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
     :EMPHASIS     -- basic emphasis typically rendered in an italic font.
     :KEYBOARD     -- Used for text to be typed by the user. 
     :QUOTATION    -- Block quotation.
     :QUOTE        -- Surrounding quoation marks. 
     :SAMPLE       -- Used for sample output from programs, and scripts etc. 
     :STRONG       -- strong emphasis typically rendered in a bold font.
     :VARIABLE     -- Used for variables or arguments to commands.

  CITE is a URI designating the source of the block (only available when BLOCK-STYLE is :QUOTATION). 
  CLASS is the class for the element
  ID is an element identifier.
  TITLE is a string used as an element title.
  STYLE denotes the style sheet to use.
  LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
  DIRECTION is ht base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT."
  (typecase emphasis 
    (keyword
      `(%with-environment
	 (,(emphasis-command emphasis) :stream ,stream :fresh-line nil)
	 ,(case emphasis
	    ((:quotation :quote)
	     `(%write-emphasis-arguments
		,stream ,emphasis ,cite ,class ,id ,title ,style ,language ,direction events))
	    (t `(%write-standard-command-arguments ,stream ,id ,class ,language ,direction ,title ,style ,events)))
	 ,@body))
    (t `(let ((emphasis ,emphasis))
	  (%with-environment
	    ((emphasis-command emphasis) :stream ,stream :fresh-line nil)
	    (%write-emphasis-arguments
	      ,stream emphasis ,cite ,class ,id ,title ,style ,language ,direction events)
	    ,@body)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.38")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (load)
  (export (intern "WITH-EMPHASIS" :html4.0) :html4.0))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.39")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (eval compile load)
  (unexport 'html3.2::with-rendition :html4.0)
  (shadow (intern "WITH-RENDITION" :html4.0) :html4.0))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.39")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(PROGN
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defconstant *rendition* '((:bold . "B")
                             (:italic . "I") 
                             (:large . "BIG")
                             (:small . "SMALL")
                             (:strike . "S")
                             (:subscript . "SUB")
                             (:superscript . "SUP")
                             (:teletype . "TT")
                             (:underline . "U")))) 

(defun rendition-command (rendition)
  (or (cdr (assoc rendition *rendition*))
      (error "Unknown type of rendition, ~S." rendition)))
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.39")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

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
  STYLE denotes the style sheet to use.
  LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
  DIRECTION is ht base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT."
  `(%with-environment
     (,(typecase rendition
	 (keyword (rendition-command rendition))
	 (t `(rendition-command ,rendition)))
      :stream ,stream :fresh-line nil)
     (%write-standard-command-arguments ,stream ,id ,class ,language ,direction ,title ,style ,events)
     ,@body))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.39")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (load)
  (export (intern "WITH-RENDITION" :html4.0) :html4.0))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.40")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (eval compile load)
  (unexport 'html3.2::horizontal-line :html4.0)
  (shadow (intern "HORIZONTAL-LINE" :html4.0) :html4.0))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.40")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define horizontal-line (&key class id title style language direction events fresh-line (stream *output-stream*)
			      size width alignment (shade t))
  "Writes a horizontal line across the output on STREAM.

  CLASS is the class for the element
  ID is an element identifier.
  TITLE is a string used as an element title.
  STYLE denotes the style sheet to use.
  LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
  DIRECTION is ht base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT.

  These arguments are deprecated. Use style sheets instead.

  SIZE is an integer indicating how thick the line should be.
  WIDTH is an integer specifying the absolute length of the line in pixels
  or a float between zero and one indicating the percent of the window with to occupy.
  ALIGNMENT is one of :LEFT, :RIGHT or :CENTER.
  SHADE turns on shading."
  (when fresh-line (fresh-line stream))
  (%issue-command ("HR" stream)
    (cond-every
      ((or id class language direction title style events)
       (%write-standard-command-arguments stream id class language direction title style events))
      (size (%write-command-key-arg stream "SIZE" size t))
      (width
        (%write-width-argument width stream))
      (alignment
        (unless (member alignment ns1.1::*line-alignments*)
          (error "~S is not one of the known alignments, ~S" alignment ns1.1::*line-alignments*))
        (%write-command-key-arg stream "ALIGN" (symbol-name alignment)))
      ((not shade)
       (%write-command-key-arg stream "NOSHADE")))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.40")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (load)
  (export (intern "HORIZONTAL-LINE" :html4.0) :html4.0))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.41")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun %write-standard-command-arguments (stream id class language direction title style events)
  (cond-every 
    (id (%write-id-argument stream id))
    (class (%write-class-argument stream class))
    ((or language direction)
     (%write-language-and-direction-command-arguments stream language direction))
    (title
      (%write-title-argument stream title))
    (style (%write-style-argument stream style))
    (events (html2::%write-input-type-event-arg stream events))))



;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.41")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (eval compile load)
  (unexport 'html3.2::break-line :html4.0)
  (shadow (intern "BREAK-LINE" :html4.0) :html4.0))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.41")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define break-line (&key clear class id title style (stream *output-stream*))
  "Issues a line break on STREAM.
  CLEAR can be any of:  
     :LEFT  -- move down lines until the left margin is clear of images.
     :RIGHT -- move down lines until the right margin is clear of images.
     :ALL   -- move down lines until the right margin is clear of images.
  CLASS is the class for the element
  ID is an element identifier.
  TITLE is a string used as an element title.
  STYLE denotes the style sheet to use."
  (%issue-command ("BR" stream)
    (cond-every
      (clear
	(unless (member clear ns1.1::*break-line-clear-options*)
	  (error "~S is not one of the known options, ~S." clear ns1.1::*break-line-clear-options*))
	(%write-command-key-arg stream "CLEAR" clear))
      (id (%write-id-argument stream id))
      (class (%write-class-argument stream class))
      (title
	(%write-title-argument stream title))
      (style (%write-style-argument stream style)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.42")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (eval compile load)
  (unexport 'html3.2::with-verbatim-text :html4.0)
  (shadow (intern "WITH-VERBATIM-TEXT" :html4.0) :html4.0))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.42")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;;;------------------------------------------------------------------- 
;;;
;;; LITERAL ENVIRONMENTS
;;;

(define-macro with-verbatim-text ((&key class id title style language direction events
					(width 80.)(fresh-line t) (stream '*output-stream*)) &body body)
  "Formats text within BODY in a fixed width font exactly as printed,
as might be used for computer output or plain text files.

  CLASS is the class for the element
  ID is an element identifier.
  TITLE is a string used as an element title.
  STYLE denotes the style sheet to use.
  LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
  DIRECTION is ht base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT.

  WIDTH (deprecated) controls how wide the preformated text.  User agents support widths of
  40, 80 (the default), 132, with upward rounding.
  Character styles can be controlled via the WITH-RENDITION macro.
  Line and paragraph breaks functions as CR. ASCII tab is interpreted
as spaces in multiples of 8, but its use is not recommended"
  `(%with-environment
     ("PRE" :fresh-line ,fresh-line :stream ,stream)
     (cond-every
       (,width
	(%write-command-key-arg ,stream "WIDTH" ,width nil))
       ((or class id title style language direction)
	(%write-standard-command-arguments ,stream ,id ,class ,language ,direction ,title ,style ,events)))
     ,@body))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.42")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (load)
  (export (intern "WITH-VERBATIM-TEXT" :html4.0) :html4.0))
