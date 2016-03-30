;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for CL-HTTP version 70.207
;;; Reason: CLOS class HTTP::HTML-4-REGION-MIXIN:  new
;;; CLOS class HTTP::REGION:  mixin html-4-region-mixin.
;;; CLOS class HTTP::HTML-4-IMAGE-MAP-MIXIN:  new
;;; CLOS class HTTP::IMAGE-MAP-DATA:  mixin html-4-image-map-mixin.
;;; Function NS11::%WRITE-WIDTH-ARGUMENT:  less consing.
;;; Function HTML:WITH-CAPTION:  fix bug.
;;; Written by jcma, 8/28/05 00:03:47
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
;;; HTTP Server 70.206, Showable Procedures 36.3, Binary Tree 34.0,
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
  "HTTP:SERVER;HTML2.LISP.306"
  "HTTP:SERVER;CLASS.LISP.60"
  "HTTP:SERVER;NETSCAPE-1-1.LISP.133"
  "HTTP:SERVER;HTML-3-2.LISP.42")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.306")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

(defun %write-command-key-arg (stream option &optional (value :no-value) no-string)
  (write-char #\space stream)
  (write-string option stream)
  (unless (eq value :no-value)
    (write-char #\= stream)
    (cond (no-string
           (write value :stream stream :base 10. :escape nil))
          ((numberp value)
           (write value :stream stream :base 10))
          ((functionp value)
           (funcall value stream))
          (t (write-char #\" stream)
             (typecase value
               (string
                 (write-string-quoting-specials value stream))
               (symbol
                 (write-string-quoting-specials (symbol-name value) stream))
	       (character
		 (let ((token-string (cdr (assoc value html2::*special-character-translation-alist*))))
		   (if token-string
		       (write-string token-string stream)
		       (write-char value stream))))
               (t (let ((string (write-to-string value :escape nil :base 10)))
		    (declare (dynamic-extent string))
                    (write-string-quoting-specials string stream))))
             (write-char #\" stream)))))



;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.60")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defclass html-4-region-mixin
	  ()
    ;; html 4.0.1 parameters
    ((name :initarg :name :initform nil :accessor region-name) 
     (tab-index :initarg :tab-index :initform nil :accessor region-tab-index)
     (access-key :initarg :access-key :initform nil :accessor region-access-key)
     (id :initarg :id :initform nil :accessor region-id)
     (class :initarg :class :initform nil :accessor region-class)
     (language :initarg :language :initform nil :accessor region-language)
     (direction :initarg :direction :initform nil :accessor region-direction)
     (title :initarg :title :initform nil :accessor region-title)
     (style :initarg :style :initform nil :accessor region-style)
     (events :initarg :events :initform nil :accessor region-events)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.60")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defclass region
          (html-4-region-mixin)
    ((destination :initarg :destination
                  :initform nil
		  :accessor region-destination
                  :accessor destination)	;backward compatible -- JCMa 8/26/2005
     (bounding-shape :initarg :bounding-shape
                     :type (or shape point)     ; parse-ncsa-point passes in a single point -- JCMa 8/5/2005
		     :accessor region-bounding-shape
                     :accessor bounding-shape))	;backward compatible -- JCMa 8/26/2005
  (:documentation "Records information for a clickable region of an
image-map."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.60")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

;;;------------------------------------------------------------------- 
;;;
;;; IMAGE-MAPS
;;;
;;; (C) Copyright 1995, Christopher R. Vincent
;;;     All Rights Reserved.

(defclass html-4-image-map-mixin
	  ()
    ;; html 4.0.1 parameters
    ((tab-index-default :initarg :tab-index-default :initform nil :accessor im-tab-index-default)
     (access-key-default :initarg :access-key-default :initform nil :accessor im-access-key-default)
     (id-default :initarg :id-default :initform nil :accessor im-id-default)
     (class-default :initarg :class-default :initform nil :accessor im-class-default)
     (language-default :initarg :language-default :initform nil :accessor im-language-default)
     (direction-default :initarg :direction-default :initform nil :accessor im-direction-default)
     (title-default :initarg :title-default :initform nil :accessor im-title-default)
     (style-default :initarg :style-default :initform nil :accessor im-style-default)
     (events-default :initarg :events-default :initform nil :accessor im-events-default)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.60")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defclass image-map-data
          (html-4-image-map-mixin)
    ((url-default :initarg :url-default
                  :initform nil
                  :accessor im-url-default)
     (region-list :initarg :region-list
                  :initform nil
                  :accessor im-region-list))
  (:documentation "General image-map information."))


;========================

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;NETSCAPE-1-1.LISP.133")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: netscape1.1; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun %write-width-argument (width stream)
  (if (<= 0 width 1)
      (flet ((write-val (stream)
	       (declare (float width))
	       (fast-format stream "\"~D%\"" (floor (* width 100)))))
	(declare (dynamic-extent #'write-val))
	(%write-command-key-arg stream "WIDTH" #'write-val))
      (%write-command-key-arg stream "WIDTH" width t)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML-3-2.LISP.42")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: html3.2; -*-")

(define-macro with-caption ((&key (alignment :top) (stream '*output-stream*)) &body body)
  "Asserts the contents of BODY is a caption within a table environment.
:ALIGNMENT can be :TOP or :BOTTOM."
  `(%with-environment
     ("CAPTION" :stream ,stream)
     (%write-command-key-arg ,stream "ALIGN" (ecase ,alignment
                                               (:top "TOP")
                                               (:bottom "BOTTOM")))
     ,@body))

