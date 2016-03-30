;;;   -*- Mode: LISP; Package: xhtml1.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-
;;;
;;; (c) Copyright 2005, 2007, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; XHTML 1.0 GENERATION
;;;
;;; Specification available at: http://www.w3.org/TR/xhtml1/
;;;
;;; This facility is a largely complete and accurate XHTML 1.0 implementation.
;;; Please be on the lookout for any missing or incorrect functionality
;;; and report it right away. 9/26/2005 -- JCMa.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; PACKAGE DEFINITION
;;;

(in-package :xhtml1.0)

;;;------------------------------------------------------------------- 
;;;
;;;  DOCUMENT LEVEL OPERATIONS
;;;

(defconstant *xml-version* "1.0")

(define declare-xml-version (&key (character-encoding :utf-8) (stream *output-stream*))
  "Declares the current document to be an XML 1.0 document on STREAM.

CHARACTER-ENCODING in XML defaults to :UTF-8 or :UTF-16. For character encoding other than these,
applications MUST supply CHARACTER-ENCODING."
  (fast-format stream "<?xml~I~I?>"
               (%write-command-key-arg stream "version" *xml-version*)
               (when character-encoding
                 (%write-command-key-arg stream "encoding" character-encoding))))

(declaim (inline %write-xml-namespace-arg))

(defun %write-xml-namespace-arg (stream namespace-url)
  (%write-command-key-arg stream "xmlns" namespace-url))

(define declare-html-version (&optional (stream *output-stream*) (dtd-version :frameset)
                              &aux (html4.0::*xhtml-generation* nil))
  "Declares the document type as the current HTML generation DTD.
All XHTML 1.0 document must declare the document type definition version.

   DTD-VERSION can be any of:

      :STRICT       - includes all elements that have not been deprecated and do not appear in
                      frameset documents.

      :TRANSITIONAL - includes everything is the STRICT DTD plus deprecated elements and attributes.

      :FRAMESET     - includes everything in TRANSITIONAL plus frames."
  (when dtd-version
    (%issue-command ("!DOCTYPE html PUBLIC" stream :fresh-line t :trailing-line t)
      (ecase dtd-version
        ((:frameset t)
         (fast-format stream " ~S ~S" "-//W3C//DTD XHTML 1.0 Frameset//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd"))
        (:strict
	  (fast-format stream " ~S ~S" "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"))
        (:transitional
	  (fast-format stream " ~S ~S" "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"))))))

(define-macro with-html-document ((&key (declare-dtd-version-p :transitional) (character-encoding :utf-8)
					(language nil language-supplied-p) (direction nil direction-supplied-p)
                                        (stream '*output-stream*)) &body body)
  "Asserts the contents of BODY is an HTML document.

CHARACTER-ENCODING in XML defaults to :UTF-8 or :UTF-16. For character encoding other than these,
applications MUST supply CHARACTER-ENCODING.

DECLARE-DTD-VERSION-P will declare the version of the DTD implemented by the current generation
package. This should be :STRICT whenever generation strictly conforms to the HTML version associated
with the macro WITH-HTML-DOCUMENT. XHTML 1.0 offers three DTD versions.  Consequently,
DECLARE-DTD-VERSION-P can be any of :FRAMESET, :TRANSITIONAL, or :STRICT.  A value of T is
interpreted as :FRAMESET. DECLARE-DTD-VERSION-P should always be NIL, whenever extension tags or
features outside these specifications are used.

LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)

DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT."
  (let ((xml-declaration `((declare-xml-version ,.(when character-encoding `(:character-encoding ,character-encoding)) :stream ,stream)))
        (xml-args `((%write-xml-namespace-arg ,stream "http://www.w3.org/1999/xhtml"))))
    (%make-with-html-document-env stream 'declare-html-version xml-declaration xml-args declare-dtd-version-p
                                  language language-supplied-p direction direction-supplied-p
                                  body)))

;;;------------------------------------------------------------------- 
;;;
;;;  REVISED DEFINITIONS
;;;

(define-macro with-comment ((&key (stream '*output-stream*)) &body body)
  "Establishes comment environment around BODY."
  `(multiple-value-prog1 
     (progn (fresh-line ,stream)
            (write-string "<![CDATA[ " ,stream)
            ,@body)
     (write-string " ]]>" ,stream)))

(define comment (string &key (stream *output-stream*))
  "Writes STRING as a comment on STREAM."
  (with-comment (:stream stream)
    (write-string string stream)))

(define-macro with-document-style-declared ((media-type &key media title language direction (stream *output-stream*))
					    &body body)
  "Provides a preamble ennvironment for declaring style characteristics of a document.
Code with BODY must write style sheet parameters to STREAM.

MEDIA-TYPE is a MIME content type for style sheet in use.  MEDIA is a descriptor for the target
display.  Choices are: :SCREEN, :TTY, :TV, :PROJECTION, :HANDHELD, :PRINT, :BRAILE, :AURAL, OR :ALL.

TITLE is a string used as the element title LANGUAGE is the two-digit language code for the
displayed content (see RFC 1766) DIRECTION is the base directionality of neutral text and can be
either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT.

The Cascading Style Sheets level 2 Recommendation [CSS2] defines style properties which are applied
to the parse tree of the HTML or XML documents. Differences in parsing will produce different visual
or aural results, depending on the selectors used. The following hints will reduce this effect for
documents which are served without modification as both media types:

CSS style sheets for XHTML should use lower case element and attribute names.  In tables, the tbody
element will be inferred by the parser of an HTML user agent, but not by the parser of an XML user
agent. Therefore you should always explicitly add a tbody element if it is referred to in a CSS
selector.  Within the XHTML namespace, user agents are expected to recognize the `id' attribute as
an attribute of type ID. Therefore, style sheets should be able to continue using the shorthand `#'
selector syntax even if the user agent does not read the DTD.  Within the XHTML namespace, user
agents are expected to recognize the `class' attribute. Therefore, style sheets should be able to
continue using the shorthand `.' selector syntax.  CSS defines different conformance rules for HTML
and XML documents; be aware that the HTML rules apply to XHTML documents delivered as HTML and the
XML rules apply to XHTML documents delivered as XML

In HTML 4 and XHTML, the style element can be used to define document-internal style rules. In XML,
an XML stylesheet declaration is used to define style rules. In order to be compatible with this
convention, style elements should have their fragment identifier set using the id attribute, and an
XML stylesheet declaration should reference this fragment.

Text emitted within BODY is wrapped in an XHTML comment so that script text does not need to be
escaped for XML.

Use external style sheets if your style sheet uses < or & or ]]> or --.  Note that XML parsers are
permitted to silently remove the contents of comments. Therefore, the historical practice of hiding
scripts and style sheets within comments to make the documents backward compatible is likely to not
work as expected in XML-based user agents."
  `(%with-environment 
    ("style" :stream ,stream)
    (html4.0::%write-style-arguments ,stream ,media-type ,media ,title ,language ,direction)
    (with-comment (:stream ,stream)
      ,@body)))

;; Renamed from with-embedded-script in Netscape 2
(define-macro with-script ((media-type &key script-location no-content charset language (stream '*output-stream*)) &body body)
  "Provides an environment for emitting a client-side script from BODY with MEDIA-TYPE on STREAM.

Text emitted within BODY is wrapped in an XHTML comment so that script text does not need to be
escaped for XML.

Use external scripts if your script uses < or & or ]]> or --. Note that XML parsers are permitted to
silently remove the contents of comments. Therefore, the historical practice of hiding scripts and
style sheets within comments to make the documents backward compatible is likely to not work as
expected in XML-based user agents.

MEDIA-TYPE is required and must be the content type for the scripting language, like:

     (:text :javascript)  -- JavaScript script
     (:text :tcl)         -- TCL script
     (:text :vbscript)    -- Visual Basic script

MEDIA-TYPE overrides any default scripting language declarations in the document preamble.

BODY executes code that emit the script the source on STREAM, or alternatively
SCRIPT-LOCATION may used to designate a URI that loads the script text into the client.

NO-CONTENT is a boolean that informs the browser that the script emitted within BODY or
loaded from SCRIPT-LOCATION will not display content, and so, the browser may handle
this asynchronously and continue rendering the document.

CHARSET is the character encoding of the script text (see ISO 10646).

LANGUAGE (deprecated) is keyword or string identifying the scripting language."
  `(%with-environment
    ("script" :stream ,stream)
    (html4.0::%write-with-script-arguments ,stream ,media-type ,script-location ,no-content ,charset ,language)
    (with-comment (:stream ,stream)
      ,@body)))

(defun %write-client-image-map-args (stream id class style title)
  (%write-id-argument-handling-xhtml-backward-compatibly stream id)
  (cond-every
   (class 
    (%write-class-argument stream class))
   (title
    (%write-title-argument stream title))
   (style 
    (%write-style-argument stream style))))

(define-macro with-client-image-map ((id &key class style title (stream '*output-stream*)) &body body)
  "Establishes a client-side image map environment identified by ID on STREAM.

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
       (%write-client-image-map-args ,stream ,id ,class ,style ,title)
     ,@body))