;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Patch-File: T -*-
;;; Patch file for CL-HTTP version 70.206
;;; Reason: Make STANDARDIZE-LINE-BREAKS work on the lisp machine.
;;; 
;;; Function (FLAVOR:METHOD TCP::ASCII-OUTPUT-MODE SI:BUFFERED-OUTPUT-CHARACTER-STREAM):  Required for standardize line
;;; breaks to work on the lisp machine.
;;; Function (CLOS:METHOD HTTP::STANDARDIZE-LINE-BREAKS (FS:LMFS-PATHNAME) :AROUND):  expunge LMFS directories.
;;; Function (CLOS:METHOD HTTP::STANDARDIZE-LINE-BREAKS (FS:LOGICAL-PATHNAME) :AROUND):  Expunge LMFS directories.
;;; Function WWW-UTILS::%PATHNAME-DIRECTORY-P:  -
;;; Function WWW-UTILS:PATHNAME-DIRECTORY-P:  update
;;; Function HTML4.0::%NOTE-IMAGE:  -
;;; Function HTML4.0::IMAGE:  new
;;; Variable HTML4.0:*DTD-VERSION*:  update.
;;; Function HTML4.0:DECLARE-HTML-VERSION:  update.
;;; Variable HTML4.0::*STRICT-DTD*:  new
;;; Function HTML4.0:WITH-HTML-DOCUMENT:  new
;;; Function HTML4.0::WITH-DEPRECATION-CHECKING:  new
;;; Function HTML4.0::ENUMERATING-ITEM:  new
;;; Function HTML4.0::WITH-ENUMERATION:  new
;;; Function HTML4.0::ENUMERATE-ITEM-LIST:  new
;;; Written by jcma, 8/23/05 18:57:00
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
;;; HTTP Server 70.205, Showable Procedures 36.3, Binary Tree 34.0,
;;; Experimental W3 Presentation System 8.2, CL-HTTP Server Interface 54.0,
;;; HTTP Proxy Server 6.34, HTTP Client Substrate 4.23,
;;; W4 Constraint-Guide Web Walker 45.14, HTTP Client 51.9, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.18, DEC OSF/1 V4.0 (Rev. 110),
;;; 1728x1062 24-bit TRUE-COLOR X Screen FUJI:1.0 with 224 Genera fonts (The Olivetti & Oracle Research Laboratory R3323),
;;; Machine serial number -2142637960,
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7),
;;; Get Xauthority pathname from user namespace object. (from W:>jcma>fixes>xauthority-pathname.lisp.2),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing).

;;; Patch file for CL-HTTP version 70.206
;;; Written by jcma, 8/23/05 21:19:03
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
;;; HTTP Server 70.205, Showable Procedures 36.3, Binary Tree 34.0,
;;; Experimental W3 Presentation System 8.2, CL-HTTP Server Interface 54.0,
;;; HTTP Proxy Server 6.34, HTTP Client Substrate 4.23,
;;; W4 Constraint-Guide Web Walker 45.14, HTTP Client 51.9, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.18, DEC OSF/1 V4.0 (Rev. 110),
;;; 1728x1062 24-bit TRUE-COLOR X Screen FUJI:1.0 with 224 Genera fonts (The Olivetti & Oracle Research Laboratory R3323),
;;; Machine serial number -2142637960,
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7),
;;; Get Xauthority pathname from user namespace object. (from W:>jcma>fixes>xauthority-pathname.lisp.2),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing).


;;; Patch file for CL-HTTP version 70.206
;;; Written by jcma, 8/23/05 18:59:41
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
;;; HTTP Server 70.205, Showable Procedures 36.3, Binary Tree 34.0,
;;; Experimental W3 Presentation System 8.2, CL-HTTP Server Interface 54.0,
;;; HTTP Proxy Server 6.34, HTTP Client Substrate 4.23,
;;; W4 Constraint-Guide Web Walker 45.14, HTTP Client 51.9, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.18, DEC OSF/1 V4.0 (Rev. 110),
;;; 1728x1062 24-bit TRUE-COLOR X Screen FUJI:1.0 with 224 Genera fonts (The Olivetti & Oracle Research Laboratory R3323),
;;; Machine serial number -2142637960,
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7),
;;; Get Xauthority pathname from user namespace object. (from W:>jcma>fixes>xauthority-pathname.lisp.2),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing).




(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:LISPM;SERVER;LISPM.LISP.522"
  "HTTP:LISPM;SERVER;LISPM.LISP.523"
  "HTTP:LISPM;SERVER;LISPM.LISP.526"
  "HTTP:SERVER;HTML4.LISP.43"
  "HTTP:SERVER;HTML4.LISP.44"
  "HTTP:SERVER;HTML4.LISP.46"
  "HTTP:SERVER;HTML4.LISP.49"
  "HTTP:SERVER;HTML4.LISP.50")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.522")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

;; Enables standardize line breaks to work on the lisp machine
(scl:defmethod (tcp::ascii-output-mode si:buffered-output-character-stream) ()
  scl:self)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.523")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defmethod http::standardize-line-breaks :around ((pathname fs:lmfs-pathname) &optional standard-line-break stream)
  (call-next-method pathname standard-line-break stream)
  (if (pathname-directory-p pathname)
      (fs:expunge-directory pathname)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.523")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defmethod http::standardize-line-breaks :around ((pathname logical-pathname) &optional standard-line-break stream)
  (call-next-method pathname standard-line-break stream)
  (if (and (pathname-directory-p pathname) (typep (http::translated-pathname pathname) 'fs:lmfs-pathname))
      (fs:expunge-directory pathname)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.526")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defun %pathname-directory-p (pathname)
  (let ((p (pathname pathname)))
    (and (null (pathname-name p))
	 (null (pathname-type p))
	 (null (pathname-version p)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.526")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defun pathname-directory-p (pathname)
  (let ((path (pathname pathname)))
    (or (%pathname-directory-p path)
        (%pathname-directory-file-p path))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.43")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (load eval compile)
(unexport 'html3.2:image :html4.0)
(shadow 'html3.2:image :html4.0)
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.43")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(declaim (inline image))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.43")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define image (image-url alternative-text
                         &key alignment accept-coordinates-at-url
                         client-side-image-map
                         border vertical-space horizontal-space width height
			 description class id title style language direction events
                         (stream *output-stream*))
  (%note-image stream image-url alternative-text alignment accept-coordinates-at-url
               client-side-image-map
               border vertical-space horizontal-space width height 
	       description class id title style language direction events))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.43")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(setf (documentation 'image 'function)
      "IMAGE-URL is the URL for an image and ALTERNATIVE-TEXT is the text to display when the image
   is not loaded.

  ACCEPT-COORDINATES-URL can be:
   
                * URL to which coordinates will be returned when the user
                clicks on the image.

                * T, indicating that returned coordinates should go to a
                search URL version of IMAGE-URL
                
                * :NO-URL, indicating not to emit an anchor for accepting the
                returns but to mark the IMG as a coordinate search.

   CLIENT-SIDE-IMAGE-MAP indicates the client side image map to use.
   Normally, this is a named URL (/url.html#map-name) and often, all
   client side image maps are served from a single url. The function
   WRITE-CLIENT-SIDE-IMAGE-MAP writes a client side image
   map from a server-side image map URL (CERN or NCSA formats).

   
   Allow browsers to layout the display before the image has loaded and thus
   eliminate the delay for the user otherwise incurred.

     WIDTH is width of the image in pixels.
     HEIGHT is the height of the image in pixels.

   DESCRIPTION provides a longer description of the image which supplements the short one in alternative-text.

   CLASS is the class for the element
   ID is an element identifier.
   TITLE is a string used as an element title.
   STYLE denotes the style sheet to use.
   LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
   DIRECTION is ht base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT.
   EVENTS are a set of image related events.

   ALIGNMENT (deprecated) can be:

        HTML2 Arugments

                TOP    -- align with the tallest item on the line.
                MIDDLE -- align with the baseline with the middle of the image.
                BOTTOM -- align with the baseline of the current line with the image.
        
        Text Flow Options

                LEFT -- float down and over to the next available space on
                the left margin, and subsequent text wraps around the right
                side of the image.

                RIGHT -- float down and over to the next available space on
                the right margin, and subsequent text wraps around the left
                side of the image.

        Semi-Random Options

                TEXTTOP -- align the image top with the top of the current
                line.

                ABSMIDDLE -- aling the middle of the image with the middle of
                the current line.

                ABSBOTTOM -- align the image bottom with the bottom of the
                current line.

    BORDER (deprecated) is an integer indicating the thickness of the border with which to
    surround the image.

    VERTICAL-SPACE (deprecated) is an integer indicating the amount of vertical space
    above and below a floating image.

    HORIZONTAL-SPACE (deprecated) is an integer indicating the amount of horizontal space
    above and below a floating image.")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.43")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (load)
  (export (intern "IMAGE" :html4.0)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.44")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;;;------------------------------------------------------------------- 
;;;
;;;  DOCUMENT LEVEL OPERATIONS
;;;

(defconstant *dtd-version* "-//W3C//DTD HTML 4.01//EN")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.44")
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
         (fast-format stream " ~S ~S" "-//W3C//DTD HTML 4.0.1 Frameset//EN" "http://www.w3.org/TR/html4/frameset.dtd"))
        (:strict
	  (fast-format stream " ~S ~S" *dtd-version* "http://www.w3.org/TR/html4/strict.dtd"))
        (:transitional
	  (fast-format stream " ~S ~S" "-//W3C//DTD HTML 4.0.1 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd"))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.44")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;;;------------------------------------------------------------------- 
;;;
;;;  DOCUMENT LEVEL OPERATIONS
;;;

(defvar *strict-dtd* nil
  "Non-null when generating strict HTML 4.")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.44")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-macro with-html-document ((&key (stream '*output-stream*) (declare-dtd-version-p :frameset)
					(language nil language-supplied-p) (direction nil direction-supplied-p))
				  &body body)
  "Asserts the contents of BODY is an HTML document.

  DECLARE-DTD-VERSION-P will declare the version of the DTD implemented by the current generation
  package. This should be :STRICT whenever generation strictly conforms to the HTML version
  associated with the macro WITH-HTML-DOCUMENT. HTML 4.0.1 offers three DTD versions.
  Consequently, DECLARE-DTD-VERSION-P can be any of :FRAMESET, :TRANSITIONAL, or :STRICT.
  A value of T is interpreted as :FRAMESET. DECLARE-DTD-VERSION-P should always be NIL,
  whenever extension tags or features outside these specifications are used.

  LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)

  DIRECTION is ht base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT."
  (let* ((args `(,.(when language-supplied-p
		    `((%write-language-argument ,stream ,language)))
		,.(when direction-supplied-p
		    `((%write-direction-argument ,stream ,direction)))))
	(version (ecase declare-dtd-version-p
		   ((nil) nil)
		   ((t :frameset) :frameset)
		   (:transitional :transitional)
		   (:strict :strict)))
	(version-declaration (ecase version
                               ((nil) nil)
                               (:frameset `(declare-html-version ,stream :frameset))
                               (:transitional `(declare-html-version ,stream :transitional))
                               (:strict `(declare-html-version ,stream :strict)))))
    (if version-declaration
	`(let ((*strict-dtd* (eq :strict ,version)))
	   ,version-declaration
	   (%with-environment ("HTML" :stream ,stream)
			      ,(when args (cons 'progn args))
	     . ,body))
	`(%with-environment ("HTML" :stream ,stream)
			    ,(when args (cons 'progn args))
	   . ,body))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.46")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmacro %with-deprecation-checking ((function condition &optional argument value value-supplied-p 
						format-string format-args) &body body)
  `(progn
     (when ,(if condition `(and *strict-dtd* ,condition) '*strict-dtd*)
       ,(cond (argument
	       `(cerror "Continue HTML Generation" 
			"In HTML 4.0.1, for ~S the argument, ~S, ~:[~; with value, ~S,~] is deprecated.~:[~;~&~:*~?~]"
			,function ,argument ,value-supplied-p ,value ,format-string ,format-args))
	      (function
	       `(cerror "Continue HTML Generation" 
			"In HTML 4.0.1, ~S is deprecated.~:[~;~&~:*~?~]" ,function ,format-string ,format-args))
	      (t `(cerror "Continue HTML Generation" ,format-string ,@format-args))))
     ,@body))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.46")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-macro with-deprecation-checking ((function &key argument (value nil value-supplied-p) condition
						   format-string format-args) &body body)
  `(%with-deprecation-checking (',function ,condition ',argument ,value ,value-supplied-p ,format-string ,@format-args)
     ,@body))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.49")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (load eval compile)
  (mapc #'(lambda (x)
	    (unexport x :html4.0) 
	    (shadow x :html4.0))
	'(html3.2:with-enumeration html3.2:enumerating-item html3.2:enumerate-item-list)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.49")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(PROGN
(defun enumerate-itemized-item (stream continuation icon-url type id class language direction title style events)
  (declare (ignore icon-url))
  (%issue-command ("LI" stream :fresh-line t)
    (html3.2::%get-bullet-style type html3.2::*itemize-bullet-styles*)
    (when (or id class language direction title style events)
      (%write-standard-command-arguments stream id class language direction title style events)))
  (multiple-value-prog1
    (funcall continuation stream)
    (fresh-line stream)))

(defun enumerate-enumerated-item (stream continuation icon-url type id class language direction title style events)
  (declare (ignore icon-url))
  (%issue-command ("LI" stream :fresh-line t)
    (html3.2::%get-bullet-style type html3.2::*enumerate-bullet-styles*)
    (when (or id class language direction title style events)
      (%write-standard-command-arguments stream id class language direction title style events)))
  (multiple-value-prog1
    (funcall continuation stream)
    (fresh-line stream)))

(defun enumerate-normal-item (stream continuation icon-url head id class language direction title style events)
  (declare (ignore head))
  (%issue-command ("LI" stream :fresh-line t)
    (when (or id class language direction title style events)
      (%write-standard-command-arguments stream id class language direction title style events)))
  (when icon-url
    (image icon-url "o" :stream stream)
    (write-char #\space stream)) 
  (funcall continuation stream)
  (fresh-line stream)) 

;; Does not provide standard html 4 parameters for DD -- 8/25/05 JCMa
(defun enumerate-definition-item (stream continuation icon-url head id class language direction title style events)
  (flet ((write-dd (stream)
           (issue-command "DD" stream nil)
           (write-char #\space stream)))
    (declare (inline write-dd))
    (fresh-line stream)
    (%issue-command ("DT" stream :fresh-line t)
    (when (or id class language direction title style events)
      (%write-standard-command-arguments stream id class language direction title style events)))
    (when icon-url
      (image icon-url "o" :stream stream)
      (write-char #\space stream))
    (etypecase head
      (null nil)
      (string
        (write-string head stream)
        (write-dd stream))
      (cons
        (dolist (item head)
          (write item :stream stream))
        (write-dd stream))
      (function
        (funcall head stream)
        (write-dd stream)))
    (funcall continuation stream)))
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.49")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;; Does not implement the VALUE parameter -- 8/25/05 JCMa
(define-macro enumerating-item ((stream &key icon-url head type id class language direction title style events) &body body)
  "Enumerates an item on STREAM according to the enclosing enumeration style.
BODY generates the body of the item whereas icon-url head type control the item's header.

TYPE (deprecated) can be provided for the styles :ENUMERATE and :ITEMIZE
to override the default bullet given by the enclosing WITH-ENUMERATION.
For the style ENUMERATE, TYPE can be any of :CAPITAL-LETTERS,
:SMALL-LETTERS, :LARGE-ROMAN, :SMALL-ROMAN, or :ARABIC (the default).
For the style :ITEMIZE, TYPE can be any of :SOLID-DISC, :CIRCLE, or
:SQUARE.

HEAD specifies the heading for an item in the :DEFINITION style.  Head
can be a string, a list of strings or a function called on STREAM.

ICON-URL is available for the styles :DEFINITION, :DIRECTORY
(deprecated), and :MENU (deprecated).  When provided, an image is
emitted at the start of the item using the URL supplied as the value of
ICON-URL.

CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE denotes the style sheet to use.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is ht base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT."
  `(flet ((continuation (,stream) ,@body))
     (declare (dynamic-extent #'continuation))
     (funcall *enumeration-function* ,stream #'continuation ,icon-url ,(or head type)
	      ,id ,class ,language ,direction ,title ,style ,events)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.49")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun %enumeration-style-parameters (style type)
  (declare (values function tag type))
  (case style
    (:itemize
      (values #'enumerate-itemized-item "UL" (html3.2::%get-bullet-style type html3.2::*itemize-bullet-styles*)))
    (:enumerate
      (values #'enumerate-enumerated-item "OL" (html3.2::%get-bullet-style type html3.2::*enumerate-bullet-styles*)))
    (:definition
      (values #'enumerate-definition-item "DL"))
    (:directory
      (with-deprecation-checking (with-enumeration :argument style :value style)
	(values #'enumerate-normal-item "DIR")))
    ((:menu :plain)
     (with-deprecation-checking (with-enumeration :argument style :value style)
       (values #'enumerate-normal-item "MENU")))
    (t (error "Unknown enumeration style, ~A." style)))) 

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.49")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-macro with-enumeration ((stream enumeration-style &key compact type start 
					id class language direction title style events) &body body)
  "Establishes an enumeration environment using the style, STYLE, on STREAM. 
Within this environment, each item is emitted by generation code
executed with the ENUMERATING-ITEM macro.  ENUMERATION-STYLE can be :DEFINITION,
:DIRECTORY (deprecated), :ENUMERATE :ITEMIZE :MENU (deprecated).

You must use the ENUMERATING-ITEM from the same package for reliable
results.

TYPE (deprecated) allows the default styles of enumeration to be
overridden for some styles. For the style :ENUMERATE, TYPE can be any
of: :CAPITAL-LETTERS, :SMALL-LETTERS, :LARGE-ROMAN, :SMALL-ROMAN, or
:ARABIC (the default). For the style :ITEMIZE, TYPE can be any of
:SOLID-DISC, :CIRCLE, :SQUARE

COMPACT (deprecated) advises the client to render lists in a more compact style. 

For the :ENUMERATE style, START (deprecated) can cause the enumeration
to begin at a number other than the default 1.

CLASS is the class for the element
ID is an element identifier.
TITLE is a string used as an element title.
STYLE denotes the style sheet to use.
LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
DIRECTION is ht base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT."
  (flet ((enumeration-arguments (compact start type)
           (let ((args nil))
             (cond-every
               (compact
		 (push `(,compact
			 (with-deprecation-checking (with-enumeration :argument compact)
			   (write-string " COMPACT" ,stream)))
		       args))
               (type
		 (push `(,type
			  (with-deprecation-checking (with-enumeration :argument type)
			    (fast-format stream " TYPE=~A" ,type)))
		       args))
               (start
		 (push `(,start
			 (with-deprecation-checking (with-enumeration :argument start)
			   (check-type ,start integer)
			   (fast-format ,stream " START=~D" ,start)))
		       args)))
             (when args 
	       `(cond-every ,.args)))))
    `(multiple-value-bind (*enumeration-function* tag enumeration-type)
         (%enumeration-style-parameters ,enumeration-style ,type)
       enumeration-type                         ;no compiler warning
       (%with-environment (tag :fresh-line t :stream ,stream)
                          (progn
			    ,(when (or id class language direction title style events)
			       `(%write-standard-command-arguments
				  ,stream ,id ,class ,language ,direction ,title ,style ,events))
			    ,(enumeration-arguments compact start type))
         ,@body))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.49")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define enumerate-item-list (item-list &key (enumeration-style :itemize) type compact 
				       id class language direction title style events (stream *output-stream*))
  "Enumerates the elements of ITEM-LIST in STYLE on STREAM."
  (with-enumeration (stream enumeration-style :compact compact :type type 
			    :id id :class class :language language :direction direction :title title :style style
			    :events events)
    (dolist (item item-list)
      (enumerating-item (stream)
        (write item :stream stream :escape nil)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.49")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (load)
  (mapc #'(lambda (x)
	    (export (intern x :html4.0)))
	'("WITH-ENUMERATION" "ENUMERATING-ITEM" "ENUMERATE-ITEM-LIST")))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.50")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: html4.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;; html spec says to always provide alternative text
(defun %note-image (stream image-url alternative-text alignment accept-coordinates-at-url
                           client-side-image-map
                           border vertical-space horizontal-space width height 
			   description class id title style language direction events)
  (flet ((write-element (stream image-url alignment alternative-text accept-coordinates-at-url events)
           (flet ((alignment-value (alignment)
                    (unless (member alignment *image-alignment-values*)
                      (error "Unknown alignment, ~S, for an image." alignment))
                    (symbol-name alignment))
                  (write-integer-arg (stream option value)
                    (check-type value integer)
                    (%write-command-key-arg stream option value t)))
             (declare (inline alignment-value write-integer-arg))
	     ;; Automagically insert image sizes when algorithms available.
             (when (and image-url (not (or width height)) http:*image-sizes-default-automatically*)
	       (multiple-value-setq (width height)
		 (url:image-size image-url)))
             (%issue-command ("IMG" stream)
               (cond-every
                 (image-url
                   (%write-command-key-arg stream "SRC" image-url))
                 (alignment
		   (with-deprecation-checking (image :argument alignment)
		     (%write-command-key-arg stream "ALIGN" (alignment-value alignment))))
                 (alternative-text
                   (check-type alternative-text string)
                   (%write-command-key-arg stream "ALT" alternative-text))
		 (description
                   (check-type description string)
                   (%write-command-key-arg stream "LONGDESC" description))
                 (accept-coordinates-at-url (%write-command-key-arg stream "ISMAP"))
                 (client-side-image-map
                   (%write-command-key-arg
                     stream "USEMAP" (url:name-string-without-search-suffix client-side-image-map nil)))
                 (border
		   (with-deprecation-checking (image :argument border)
		     (write-integer-arg stream "BORDER" border)))
                 (vertical-space 
		   (with-deprecation-checking (image :argument vertical-space)
		     (write-integer-arg stream "VSPACE" vertical-space)))
                 (horizontal-space 
		   (with-deprecation-checking (image :argument horizontal-space)
		     (write-integer-arg stream "HSPACE" horizontal-space)))
                 (width (write-integer-arg stream "WIDTH" width))
                 (height (write-integer-arg stream "HEIGHT" height))
		 ((or id class language direction title style events)
		  (%write-standard-command-arguments stream id class language direction title style events)))))))
    (let ((url-string (url:name-string-without-search-suffix image-url nil)))
      (declare (dynamic-extent url-string))
      (case accept-coordinates-at-url
        ((nil :no-url)
         (write-element stream url-string alignment alternative-text accept-coordinates-at-url events))
        (t (with-anchor-noted (:reference (if (eq accept-coordinates-at-url t)
                                              url-string
                                              (url:name-string-without-search-suffix accept-coordinates-at-url nil))
                               :stream stream)
             (write-element stream url-string alignment alternative-text accept-coordinates-at-url events)))))))

