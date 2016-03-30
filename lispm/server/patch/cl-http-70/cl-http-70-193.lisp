;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.193
;;; Reason: Function TK1::RESIZE-TOKENIZER:  Resizes TOKENIZER up or down to NEW-SIZE.
;;; Export it.
;;; Function HTML4.0:DECLARE-HTML-VERSION: handle version arugments of T and NIL
;;; Function HTML4.0:WITH-HTML-DOCUMENT:  handle dynamic version arguments.
;;; Written by jcma, 7/10/05 22:02:06
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
;;; HTTP Server 70.192, Showable Procedures 36.3, Binary Tree 34.0,
;;; Experimental W3 Presentation System 8.1, CL-HTTP Server Interface 54.0,
;;; HTTP Proxy Server 6.34, HTTP Client Substrate 4.23,
;;; W4 Constraint-Guide Web Walker 45.13, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.18, DEC OSF/1 V4.0 (Rev. 110),
;;; 1728x1003 24-bit TRUE-COLOR X Screen FUJI:1.0 with 224 Genera fonts (The Olivetti & Oracle Research Laboratory R3323),
;;; Machine serial number -2142637960,
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7),
;;; Get Xauthority pathname from user namespace object. (from W:>jcma>fixes>xauthority-pathname.lisp.2),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;TOKENIZER.LISP.66"
  "HTTP:SERVER;HTML4.LISP.14")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TOKENIZER.LISP.66")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ansi-common-lisp; Base: 10; Package: (tk1 :use (future-common-lisp)); -*-")

(defun resize-tokenizer (tokenizer new-size)
  "Resizes TOKENIZER up or down to NEW-SIZE.
Runs faster if there are no entries in TOKENIZER."
  (multiple-value-bind (modulus new-rehash-size)
      (select-table-size new-size)
    (declare (ignore modulus))
    (when (= new-rehash-size (tokenizer-rehash-size tokenizer))
      (rehash-tokenizer tokenizer new-size))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TOKENIZER.LISP.66")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ansi-common-lisp; Base: 10; Package: (tk1 :use (future-common-lisp)); -*-")

(eval-when (:load-toplevel)
(export (intern "RESIZE-TOKENIZER" :tk1) :tk1))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.14")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (html4.0 :use (future-common-lisp html3.2 www-utils url)); BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;; handle version arugments of T  and  NIL
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
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML4.LISP.14")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (html4.0 :use (future-common-lisp html3.2 www-utils url)); BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;; handle dynamic version arguments.
(define-macro with-html-document ((&key (stream '*output-stream*) (declare-dtd-version-p t)
					(language nil language-supplied-p) (text-directionality nil text-directionality-supplied-p))
				  &body body)
  "Asserts the contents of BODY is an HTML document.

  DECLARE-DTD-VERSION-P will declare the version of the DTD implemented by the current generation
  package. This should be T whenever generation strictly conforms to the HTML version
  associated with the macro WITH-HTML-DOCUMENT. HTML 4.0 offers three DTD versions.
  Consequently, DECLARE-DTD-VERSION-P can be any of :FRAMESET, :TRANSITIONAL, or :STRICT.
  A value of T is interpreted as :FRAMESET. DECLARE-DTD-VERSION-P should always be NIL,
  whenever extension tags or features outside these specifications are used.

  LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)

  TEXT-DIRECTIONALITY is ht base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT."
  (let ((args `(,.(when language-supplied-p
		    `((%write-language-argument ,stream ,language)))
		,.(when text-directionality-supplied-p
		    `((%write-text-directionality-argument ,stream ,text-directionality)))))
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

