;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for CL-HTTP version 70.173
;;; Reason: SETF function HTTP::%HEADER-SUPERCLASS:  fix paren bug.
;;; Function HTTP::%DEFINE-HEADER:  update.
;;; Function %HEADER-SUPERCLASS: new primitive.
;;; Function HTTP::ENTITY-HEADER-P:  -
;;; Function (CLOS:METHOD HTTP::ENTITY-HEADER-P (HTTP::HEADER)):  -
;;; Function HTTP::ENTITY-HEADER-PLIST:  -
;;; Function (CLOS:METHOD HTTP::ENTITY-HEADER-PLIST (HTTP::HEADER-SET)):  -
;;; DEFINE-HEADER :ACCEPT:  -
;;; DEFINE-HEADER :ACCEPT:  -
;;; DEFINE-HEADER :ACCEPT:  -
;;; DEFINE-HEADER :ACCEPT-CHARSET:  -
;;; DEFINE-HEADER :ACCEPT-ENCODING:  -
;;; DEFINE-HEADER :ACCEPT-LANGUAGE:  -
;;; DEFINE-HEADER :ACCEPT-RANGES:  -
;;; DEFINE-HEADER :ALLOW:  -
;;; DEFINE-HEADER :AUTHORIZATION:  -
;;; DEFINE-HEADER :AUTHENTICATION-INFO:  -
;;; DEFINE-HEADER :CACHE-CONTROL:  -
;;; DEFINE-HEADER :CONNECTION:  -
;;; DEFINE-HEADER :CONTENT-BASE:  -
;;; DEFINE-HEADER :CONTENT-ENCODING:  -
;;; DEFINE-HEADER :CONTENT-DISPOSITION:  -
;;; DEFINE-HEADER :CONTENT-ID:  -
;;; DEFINE-HEADER :CONTENT-LANGUAGE:  -
;;; DEFINE-HEADER :CONTENT-LENGTH:  -
;;; DEFINE-HEADER :CONTENT-LENGTH:  -
;;; DEFINE-HEADER :CONTENT-LOCATION:  -
;;; DEFINE-HEADER :CONTENT-MD5:  -
;;; DEFINE-HEADER :CONTENT-RANGE:  -
;;; DEFINE-HEADER :CONTENT-TRANSFER-ENCODING:  -
;;; DEFINE-HEADER :CONTENT-TYPE:  -
;;; DEFINE-HEADER :CONTENT-VERSION:  -
;;; DEFINE-HEADER :DATE:  -
;;; DEFINE-HEADER :DERIVED-FROM:  -
;;; DEFINE-HEADER :ETAG:  -
;;; DEFINE-HEADER :EXPECT:  -
;;; DEFINE-HEADER :EXPIRES:  -
;;; DEFINE-HEADER :FORWARDED:  -
;;; DEFINE-HEADER :FROM:  -
;;; DEFINE-HEADER :HOST:  -
;;; DEFINE-HEADER :IF-MATCH:  -
;;; DEFINE-HEADER :IF-MODIFIED-SINCE:  -
;;; DEFINE-HEADER :IF-NONE-MATCH:  -
;;; DEFINE-HEADER :IF-UNMODIFIED-SINCE:  -
;;; DEFINE-HEADER :KEEP-ALIVE:  -
;;; DEFINE-HEADER :LAST-MODIFIED:  -
;;; DEFINE-HEADER :LOCATION:  -
;;; DEFINE-HEADER :MAX-FORWARDS:  -
;;; DEFINE-HEADER :METHOD:  -
;;; DEFINE-HEADER :MIME-VERSION:  -
;;; DEFINE-HEADER :PRAGMA:  -
;;; DEFINE-HEADER :PROXY-AUTHENTICATE:  -
;;; DEFINE-HEADER :PROXY-AUTHORIZATION:  -
;;; DEFINE-HEADER :PROXY-CONNECTION:  -
;;; DEFINE-HEADER :PUBLIC:  -
;;; DEFINE-HEADER :RANGE:  -
;;; DEFINE-HEADER :REFERER:  -
;;; DEFINE-HEADER :SERVER:  -
;;; DEFINE-HEADER :TE:  -
;;; DEFINE-HEADER :TRAILER:  -
;;; DEFINE-HEADER :TRANSFER-ENCODING:  -
;;; DEFINE-HEADER :UPGRADE:  -
;;; DEFINE-HEADER :URI:  -
;;; DEFINE-HEADER :USER-AGENT:  -
;;; DEFINE-HEADER :VERSION:  -
;;; DEFINE-HEADER :VIA:  -
;;; DEFINE-HEADER :WWW-AUTHENTICATE:  -
;;; DEFINE-HEADER :WINDOW-TARGET:  -
;;; DEFINE-HEADER :COOKIE:  -
;;; DEFINE-HEADER :SET-COOKIE:  -
;;; DEFINE-HEADER :MESSAGE-ID:  -
;;; DEFINE-HEADER :IN-REPLY-TO:  -
;;; DEFINE-HEADER :REFERENCES:  -
;;; DEFINE-HEADER :KEYWORDS:  -
;;; Function HTTP::WITH-COPY-FILE-ENVIRONMENT:  abstraction.
;;; Function (CLOS:METHOD HTTP:CONDITIONAL-COPY-FILE (STRING STRING)):  don't lose args when calling other methods.
;;; Function (CLOS:METHOD HTTP:CONDITIONAL-COPY-FILE (T T)):  ditto.
;;; Function (CLOS:METHOD HTTP:COPY-FILE (T STRING)):  ditto.
;;; Function (CLOS:METHOD HTTP:COPY-FILE (STRING T)):  ditto.
;;; Function (CLOS:METHOD HTTP:COPY-FILE (T T)):  -
;;; Written by JCMa, 10/06/03 08:57:13
;;; while running on FUJI-3 from FUJI:/usr/lib/symbolics/vlmlmfs2.vlod
;;; with Open Genera 2.0, Genera 8.5, Logical Pathnames Translation Files NEWEST,
;;; Lock Simple 437.0, Color Demo 422.0, Color 427.1, Graphics Support 431.0,
;;; Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Color System Documentation 10.0, SGD Book Design 10.0, Images 431.2,
;;; Image Substrate 440.4, CLIM 72.0, Genera CLIM 72.0, CLX CLIM 72.0,
;;; PostScript CLIM 72.0, CLIM Demo 72.0, CLIM Documentation 72.0,
;;; Statice Runtime 466.1, Statice 466.0, Statice Browser 466.0,
;;; Statice Server 466.2, Statice Documentation 426.0, Metering 444.0,
;;; Metering Substrate 444.1, Symbolics Concordia 444.0, Graphic Editor 440.0,
;;; Graphic Editing 441.0, Bitmap Editor 441.0, Graphic Editing Documentation 432.0,
;;; Postscript 436.0, Concordia Documentation 432.0, Joshua 237.6,
;;; Joshua Documentation 216.0, Joshua Metering 206.0, Jericho 237.0, C 440.0,
;;; Lexer Runtime 438.0, Lexer Package 438.0, Minimal Lexer Runtime 439.0,
;;; Lalr 1 434.0, Context Free Grammar 439.0, Context Free Grammar Package 439.0,
;;; C Runtime 438.0, Compiler Tools Package 434.0, Compiler Tools Runtime 434.0,
;;; C Packages 436.0, Syntax Editor Runtime 434.0, C Library Headers 434,
;;; Compiler Tools Development 435.0, Compiler Tools Debugger 434.0,
;;; Experimental C Documentation 427.0, Syntax Editor Support 434.0,
;;; LL-1 support system 438.0, Fortran 434.0, Fortran Runtime 434.0,
;;; Fortran Package 434.0, Experimental Fortran Doc 428.0, Pascal 433.0,
;;; Pascal Runtime 434.0, Pascal Package 434.0, Pascal Doc 427.0,
;;; MacIvory Support 447.0, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.41, Genera 8 5 Macivory Support Patches 1.0,
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
;;; Genera 8 5 Fortran Patches 1.0, Binary Tree 34.0, Showable Procedures 36.3,
;;; HTTP Server 70.172, W3 Presentation System 8.1, HTTP Client Substrate 4.23,
;;; HTTP Client 51.4, CL-HTTP Server Interface 54.0, HTTP Proxy Server 6.32,
;;; CL-HTTP Documentation 3.0, Experimental CL-HTTP CLIM User Interface 1.1,
;;; MAC 414.0, LMFS 442.1, W4 Constraint-Guide Web Walker 45.12, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.18, DEC OSF/1 V4.0 (Rev. 110),
;;; 1152x678 24-bit TRUE-COLOR X Screen FUJI:2.0 with 224 Genera fonts (The Olivetti & Oracle Research Laboratory R3323),
;;; Machine serial number 6288682,
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add support for Apple's Gestalt and Speech Managers. (from SYS:MAC;MACIVORY-SPEECH-SUPPORT.LISP.1),
;;; Vlm lmfs patch (from W:>reti>vlm-lmfs-patch.lisp.18),
;;; Pht debugging patch (from W:>Reti>pht-debugging-patch.lisp.4),
;;; Domain ad host patch (from W:>Reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing),
;;; Cname level patch (from W:>reti>cname-level-patch),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).

;;; Patch file for CL-HTTP version 70.173
;;; Written by JCMa, 10/07/03 09:30:52
;;; while running on FUJI-3 from FUJI:/usr/lib/symbolics/vlmlmfs2.vlod
;;; with Open Genera 2.0, Genera 8.5, Logical Pathnames Translation Files NEWEST,
;;; Lock Simple 437.0, Color Demo 422.0, Color 427.1, Graphics Support 431.0,
;;; Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Color System Documentation 10.0, SGD Book Design 10.0, Images 431.2,
;;; Image Substrate 440.4, CLIM 72.0, Genera CLIM 72.0, CLX CLIM 72.0,
;;; PostScript CLIM 72.0, CLIM Demo 72.0, CLIM Documentation 72.0,
;;; Statice Runtime 466.1, Statice 466.0, Statice Browser 466.0,
;;; Statice Server 466.2, Statice Documentation 426.0, Metering 444.0,
;;; Metering Substrate 444.1, Symbolics Concordia 444.0, Graphic Editor 440.0,
;;; Graphic Editing 441.0, Bitmap Editor 441.0, Graphic Editing Documentation 432.0,
;;; Postscript 436.0, Concordia Documentation 432.0, Joshua 237.6,
;;; Joshua Documentation 216.0, Joshua Metering 206.0, Jericho 237.0, C 440.0,
;;; Lexer Runtime 438.0, Lexer Package 438.0, Minimal Lexer Runtime 439.0,
;;; Lalr 1 434.0, Context Free Grammar 439.0, Context Free Grammar Package 439.0,
;;; C Runtime 438.0, Compiler Tools Package 434.0, Compiler Tools Runtime 434.0,
;;; C Packages 436.0, Syntax Editor Runtime 434.0, C Library Headers 434,
;;; Compiler Tools Development 435.0, Compiler Tools Debugger 434.0,
;;; Experimental C Documentation 427.0, Syntax Editor Support 434.0,
;;; LL-1 support system 438.0, Fortran 434.0, Fortran Runtime 434.0,
;;; Fortran Package 434.0, Experimental Fortran Doc 428.0, Pascal 433.0,
;;; Pascal Runtime 434.0, Pascal Package 434.0, Pascal Doc 427.0,
;;; MacIvory Support 447.0, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.41, Genera 8 5 Macivory Support Patches 1.0,
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
;;; Genera 8 5 Fortran Patches 1.0, Binary Tree 34.0, Showable Procedures 36.3,
;;; HTTP Server 70.173, W3 Presentation System 8.1, HTTP Client Substrate 4.23,
;;; HTTP Client 51.4, CL-HTTP Server Interface 54.0, HTTP Proxy Server 6.32,
;;; CL-HTTP Documentation 3.0, Experimental CL-HTTP CLIM User Interface 1.1,
;;; MAC 414.0, LMFS 442.1, W4 Constraint-Guide Web Walker 45.12, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.18, DEC OSF/1 V4.0 (Rev. 110),
;;; 1152x678 24-bit TRUE-COLOR X Screen FUJI:2.0 with 224 Genera fonts (The Olivetti & Oracle Research Laboratory R3323),
;;; Machine serial number 6288682,
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add support for Apple's Gestalt and Speech Managers. (from SYS:MAC;MACIVORY-SPEECH-SUPPORT.LISP.1),
;;; Vlm lmfs patch (from W:>reti>vlm-lmfs-patch.lisp.18),
;;; Pht debugging patch (from W:>Reti>pht-debugging-patch.lisp.4),
;;; Domain ad host patch (from W:>Reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing),
;;; Cname level patch (from W:>reti>cname-level-patch),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).




(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;HEADERS.LISP.527"
  "HTTP:SERVER;UTILS.LISP.524"
  "HTTP:SERVER;UTILS.LISP.525")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (eval compile load)

(defsetf %header-superclass (header) (superclass)
  `(progn
     (unless (member ,superclass *header-superclasses*)
       (error "~S is not one of the known header super-classes, ~S."
              ,superclass *header-superclasses*))
     (setf (get ,header 'header-superclass) ,superclass)))

(defun %define-header (header header-type header-class &optional print-name default parse-function print-function
			      print-series-predicate atomic-value-reducer (collapsable-p t))
  (check-type header keyword)
  (check-type header-type keyword)
  (macrolet ((update-header-function (local-arg accessor path)
	       `(let ((function (if ,local-arg (fdefinition ,local-arg) (%inherit-header-function header ',path nil))))
		  (if function
		      (setf (,accessor header) function)
		      (remprop header ',path)))))
    (let ((psym (intern (or print-name (symbol-name header)) *keyword-package*)))
      (setf (%header-supertype header) header-type
	    (%header-print-name header) psym
	    (%header-keyword psym) header
	    (%header-collapsable-p psym) collapsable-p
	    (%header-parse-function header) (if parse-function
						(fdefinition parse-function) 
						(%inherit-header-function header 'header-parse-function))
	    (%header-print-function header) (if print-function
						(fdefinition print-function)
						(%inherit-header-function header 'header-print-function))
	    (%header-superclass header) (or header-class :unknown))
      (update-header-function print-series-predicate %header-print-series-predicate header-print-series-predicate)
      (update-header-function atomic-value-reducer %header-atomic-value-reducer header-atomic-value-reducer)
      (if default
	  (setf (%header-default-value header) default)
	  (remprop header 'header-default))
      (%tokenize-header (symbol-name psym))	;update the tokenizer
      header)))
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(PROGN
(declaim (inline %header-class-p))

(define %header-class-p (header class)
  "Returns non-null if header belongs to the HTTP Class CLASS.
Class can be any member of *header-superclasses*."
  (eq (%header-superclass header) class))
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defgeneric entity-header-p (header)
  (:documentation "Returns non-null if HEADER is an entity header."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod entity-header-p ((header header))
  (%header-class-p (header-keyword header) :entity))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defgeneric entity-header-plist (header-set &optional excluded-headers)
  (:documentation "Returns a header PLIST for all entity headers in HEADER-SET,
excluding any headers mentioned in EXCLUDED-HEADERS."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod entity-header-plist ((header-set header-set) &optional excluded-headers &aux plist)
  (flet ((maybe-collect-header (header)
           (let ((keyword (header-keyword header)))
             (when (%header-class-p keyword :entity)
               (unless (member keyword excluded-headers)
                 (push (header-value header) plist)
                 (push keyword plist))))))
    (declare (dynamic-extent #'maybe-collect-header))
    (map-header-objects header-set #'maybe-collect-header)
    plist))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;;;------------------------------------------------------------------- 
;;;
;;; HTTP HEADERS
;;;

(define-header :accept
               (:content-type-sequence-header
                :request)
  :print-string "Accept")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :accept-charset
               (:quality-pair-sequence-header :request)
  :print-string "Accept-Charset")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :accept-encoding
               (:keyword-sequence-header :request)
  :print-string "Accept-Encoding")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :accept-language
               (:quality-pair-sequence-header :request)
  :print-string "Accept-Language")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :accept-ranges
               (:keyword-header :request)       ;missing from 1.1 spec p29
  :print-string "Accept-Ranges")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :allow
               (:keyword-sequence-header :entity)
  :print-string "Allow")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :authorization                   ;RFC 2069
               (:authentication-header :request)
  :print-string "Authorization"
  :parse-function 'parse-authorization-header
  :print-function 'print-authorization-header)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :authentication-info             ;RFC 2069
               (:authentication-header :response)
  :print-string "Authentication-Info"
  :parse-function 'parse-authentication-info-header
  :print-function 'print-authentication-info-header)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :cache-control
               (:cache-control-header :general)
  :print-string "Cache-Control")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :connection
               (:keyword-sequence-header :general)
  :print-string "Connection")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :content-base
               (:header :entity)
  :print-string "Content-Base")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :content-encoding
               (:keyword-header :entity)
  :print-string "Content-Encoding")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :content-disposition             ;not in 1.1
               (:header :entity)
  :print-string "Content-Disposition"
  :parse-function 'parse-mime-content-disposition-header
  :print-function 'print-mime-content-disposition-header)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :content-id                      ;not in 1.1
               (:header :entity)
  :print-string "Content-ID")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :content-language
               (:keyword-sequence-header :entity)
  :print-string "Content-Language")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :content-length
	       (:integer-header :entity)
  :print-string "Content-length"
  :atomic-value-reducer 'header-value-max)	; handles mulitple content-length headers


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :content-location
               (:header :entity)
  :print-string "Content-Location"
  :parse-function 'parse-location-header
  :print-function 'print-location-header)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :content-md5
               (:header :entity)
  :print-string "Content-MD5")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :content-range
               (:header :entity)
  :print-string "Content-Range"
  :parse-function 'parse-content-range-header
  :print-function 'print-content-range-header)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;; This is the MIME version, not the HTTP 1.1 Transfer-Encoding
(define-header :content-transfer-encoding
               (:keyword-header :entity)        ;not in 1.1
  :print-string "Content-Transfer-Encoding")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :content-type
               (:content-type-header :entity)
  :print-string "Content-type")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :content-version
               (:version-header :entity)        ;not in 1.1 spec p,31
  :print-string "Content-Version")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :date
               (:date-header :general)
  :print-string "Date"
  :atomic-value-reducer 'header-value-max
  :default 'gmt-time)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :derived-from
               (:version-header :entity)
  :print-string "Derived-From")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :etag
               (:entity-tag-header :entity)
  :print-string "ETag")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :expect
               (:header :request)
  :print-string "Expect"
  :print-function 'print-expect-header
  :parse-function 'parse-expect-header)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :expires
               (:date-header :entity)
  :print-string "Expires"
  :parse-function 'parse-expires-header
  :print-function 'print-expires-header
  :atomic-value-reducer 'header-value-min)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;; value:: (origin proxy &optional port proxy-product)
(define-header :forwarded
               (:header :response)
  :print-string "Forwarded"
  :parse-function 'parse-forwarded-header
  :print-function 'print-forwarded-header)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :from
               (:mail-address-sequence-header :request)
  :print-string "From")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :host
               (:header :request)
  :print-string "Host"
  :parse-function 'parse-host-header
  :print-function 'print-host-header)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :if-match
               (:entity-tag-sequence-header :request)
  :print-string "If-Match")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :if-modified-since
               (:date-header :request)
  :print-string "If-Modified-Since"
  :atomic-value-reducer 'header-value-min)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :if-none-match
               (:entity-tag-sequence-header :request)
  :print-string "If-None-Match")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :if-unmodified-since
               (:date-header :request)
  :print-string "If-Unmodified-Since"
  :atomic-value-reducer 'header-value-min)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;; deprecated 1.0 extension header  6/29/96 -- JCMa.
(define-header :keep-alive
               (:comma-separated-header :request)
  :print-string "Keep-Alive"
  :parse-function 'parse-keep-alive-header
  :print-function 'print-keep-alive-header)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :last-modified
               (:date-header :entity)
  :print-string "Last-Modified"
  :atomic-value-reducer 'header-value-min)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :location (:header :response)
  :print-string "Location"
  :parse-function 'parse-location-header
  :print-function 'print-location-header)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :max-forwards
               (:integer-header :request)
  :print-string "Max-Forwards")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :method
               (:comma-separated-header :unknown)
  :print-string "Method")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :mime-version
               (:header :entity)
  :print-string "MIME-version")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :pragma
               (:keyword-header :general)
  :print-string "Pragma")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :proxy-authenticate                ;RFC 2069
               (:authentication-header :response)
  :print-string "Proxy-Authenticate"
  :parse-function 'parse-www-authenticate-header
  :print-function 'print-www-authenticate-header)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :proxy-authorization                   ;RFC 2069
               (:authentication-header :request)
  :print-string "Proxy-Authorization"
  :parse-function 'parse-authorization-header
  :print-function 'print-authorization-header)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :proxy-connection		;deprecated 1.0 Extension
               (:keyword-sequence-header :general)
  :print-string "Proxy-Connection")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :public
               (:keyword-sequence-header :response)
  :print-string "Public")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :range
               (:header :request)
  :print-string "Range"
  :parse-function 'parse-range-header
  :print-function 'print-range-header)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :referer
               (:header :request)
  :print-string "Referer")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :Server 
               (:header :response)
  :print-string "Server"
  :default *server-version*)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :te
               (:header :request)
  :print-string "TE"
  :print-function 'print-comma-separated-quality-pair-or-tokens
  :parse-function 'parse-comma-separated-quality-pairs)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :trailer
               (:keyword-sequence-header :general)
  :print-string "Trailer")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :transfer-encoding
               (:keyword-header :general)
  :print-string "Transfer-Encoding")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :upgrade
               (:keyword-header :unknown)
  :print-string "Upgrade")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;; deprecated in HTTP 1.1
(define-header :uri (:header :entity)
  :print-string "URI"
  :parse-function 'parse-uri-header
  :print-function 'print-uri-header)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :User-Agent (:header :request)
  :print-string "User-Agent") 


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;; old name replaced by content-version for 1.1
(define-header :version
               (:integer-header :entity)
  :print-string "Version")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :via
               (:via-header :general)
  :print-string "Via")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :www-authenticate                ;RFC 2069
               (:authentication-header :response)
  :print-string "WWW-Authenticate"
  :parse-function 'parse-www-authenticate-header
  :print-function 'print-www-authenticate-header)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;;;------------------------------------------------------------------- 
;;;
;;; NETSCAPE EXTENSION HEADERS
;;;

;; http://home.netscape.com/eng/mozilla/2.0/relnotes/demo/target.html
;; this header can be sent to netscape with the name of the target window.
(define-header :window-target 
               (:header :response)
  :print-string "Window-Target")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :cookie (:header :request)
  :print-string "Cookie"
  :parse-function 'parse-cookie-header
  :print-function 'print-cookie-header)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :set-cookie (:header :response)
  :print-string "Set-Cookie"
  :parse-function 'parse-set-cookie-header
  :print-function 'print-set-cookie-header
  :collapsable-p nil)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :message-id (:header :email)
  :print-string "Message-ID"
  :parse-function 'parse-message-id-header)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :in-reply-to  (:comma-separated-header :email)
  :print-string "In-Reply-To"
  :parse-function 'parse-comma-separated-message-id-header)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :references (:comma-separated-header :email)
  :print-string "References"
  :parse-function 'parse-comma-separated-message-id-header)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.527")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :keywords
               (:keyword-sequence-header :email)
  :print-string "Keywords")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.524")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

;;;------------------------------------------------------------------- 
;;;
;;; CONDITIONAL FILE COPY
;;;

(defmacro with-copy-file-environment ((from to stream &optional (block-name 'copy-file)) &body body)
  `(tagbody
     retry
	(when ,stream
	  (format ,stream "~&Copying ~A to ~A . . ." (name-string ,from) (name-string ,to)))
	(restart-case
	  (return-from ,block-name
	    (multiple-value-prog1 (progn ,@body)
	      (when ,stream
		(format ,stream "~&Copied ~A to ~A." (name-string ,from) (name-string ,to)))))
	  (retry ()
		 :report (lambda (stream)
			   (format stream "Retry copying ~A" (name-string ,from)))
		 (go retry))
	  (skip ()
		:report (lambda (stream)
			  (format stream "Skip copying ~A" (name-string ,from)))
		(when ,stream
		  (format ,stream "~A not copied." (name-string ,from)))
		(return-from ,block-name nil)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.524")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmethod conditional-copy-file ((from-pathname string) (to-pathname string) &rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (apply #'conditional-copy-file (pathname from-pathname) (pathname to-pathname) args))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.524")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmethod conditional-copy-file (from-pathname to-pathname &rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (let ((from-path (probe-file from-pathname))
        (to-path (probe-file to-pathname))
        (to-directory (probe-directory to-pathname)))
    (when (and from-path
               to-directory
               (or (null to-path)
                   (> (file-write-date from-path)
                      (file-write-date to-path))))
      (apply #'copy-file from-pathname to-pathname args))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.524")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmethod copy-file (from-pathname (to-pathname string) &rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (apply #' copy-file from-pathname 
         (if (url:url-string-p to-pathname)
             (url:intern-url to-pathname :if-does-not-exist :uninterned)
           (pathname to-pathname)) args))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.524")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmethod copy-file ((from-pathname string) to-pathname &rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (apply #'copy-file (if (url:url-string-p from-pathname)
                         (url:intern-url from-pathname :if-does-not-exist :uninterned)
                       (pathname from-pathname))
         to-pathname args))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.525")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

;; Ports may optionally specialize this method to pathnames to use platform specific file copying and/or property setting.
(defmethod copy-file (from-pathname to-pathname &key (copy-mode :text) (copy-creation-date t) report-stream &allow-other-keys)
  (with-copy-file-environment (from-pathname to-pathname report-stream)
    (let ((element-type (copy-mode-element-type copy-mode))
	  modification-date #-UNIX creation-date #+CL-HTTP-FILE-AUTHOR author)
      (with-open-file (from from-pathname :direction :input :element-type element-type :if-does-not-exist :error)
	(with-open-file (to to-pathname :direction :output :element-type element-type :if-exists :supersede
			    :if-does-not-exist :create)
	  (stream-copy-until-eof from to copy-mode))
	(when copy-creation-date
	  (cond-every
	    ((setq modification-date (file-stream-modification-date from))
	     (setf (file-modification-date to-pathname) modification-date)) 
	    #-UNIX ;; creation date is not available under unix
	    ((setq creation-date (file-stream-creation-date from))
	     (setf (file-creation-date to-pathname) creation-date)))))
      #+CL-HTTP-File-Author			;can we get the file author from the stream?
      (when (and copy-creation-date (setq author (file-author from-pathname)))
	(set-file-author to-pathname author nil))
      to-pathname)))
