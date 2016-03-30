;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for CL-HTTP version 70.208
;;; Reason: Function HTTP::WRITE-ISO-DATE:  update doc string.
;;; Function HTTP::WRITE-ISO-TIME:  new.
;;; Function HTTP::WRITE-ISO-DATIME:  new.
;;; Function NS2.0::%GET-SCRIPT-LANGUAGE-CLASS:  fix error call.
;;; Function (CLOS:METHOD URL:INITIALIZE-AUTHENTICATION (URL:AUTHENTICATION-MIXIN T T)):  fix error call.
;;; Written by jcma, 8/28/05 15:45:45
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
;;; HTTP Server 70.207, Showable Procedures 36.3, Binary Tree 34.0,
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
  "HTTP:SERVER;UTILS.LISP.555"
  "HTTP:SERVER;SCRIPTS.LISP.38"
  "HTTP:SERVER;AUTHENTICATION.LISP.160")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.555")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(define write-iso-date (year month day &optional stream)
  "Writes an ISO 9601 date on STREAM.
When STREAM is NIL, it return it as a string."
  (format stream "~4D-~2,'0D-~2,'0D" year month day))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.555")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(define write-iso-time (hour minutes &optional seconds (time-zone (time-zone)) stream)
  "Writes an ISO 8601 time on STREAM.
When STREAM is NIL, it return it as a string."
  (declare (fixnum hour minute time-zone))
  (format stream "~4D-~2,'0D-~2,'0DT~2,'0D:~2,'0D~:[~;~:*:~2,'0D~]~:[~C~2,'0D~;Z~]"
	  hour minutes seconds (zerop time-zone) (if (plusp time-zone) #\+ #\-) (abs time-zone)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.555")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(declaim (inline write-iso-time))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.555")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(define write-iso-datime (universal-time &optional stream (time-zone (time-zone)))
  "Writes an ISO 8601 date and time on STREAM.
When STREAM is NIL, it return it as a string."
  (multiple-value-bind (seconds minutes hours day month year day-of-the-week daylight-savings-p timezone)
      (decode-universal-time universal-time time-zone)
    (declare (fixnum timezone)
	     (ignore day-of-the-week))
    (let ((offset (if daylight-savings-p (1- timezone) timezone)))
      (declare (fixnum offset))
      (format stream "~2,'0D:~2,'0D~:[~;~:*:~2,'0D~]~:[~C~2,'0D~;Z~]"
	      year month day hours minutes seconds (zerop offset) (if (plusp offset) #\+ #\-) (abs offset)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SCRIPTS.LISP.38")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: :netscape2.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun %get-script-language-class (language)
  (or (get language 'class)
      (error "~S is not one of the known lanuages: (~:{~S~^ ~}."
             language *script-language-table-alist*)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;AUTHENTICATION.LISP.160")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod url:initialize-authentication ((url url:authentication-mixin) realm capabilities
                                          &aux realm-obj access-control)
  (macrolet ((require-realm (realm-obj)
               `(unless ,realm-obj
                  (error "Authentication cannot be initialized without a realm."))))
    ;; intern the objects
    (setq realm-obj (typecase realm
                      (null nil)
                      (t (intern-realm realm :if-does-not-exist :error))))
    (setq access-control (typecase capabilities
                           (null nil)
                           (cons
                             (require-realm realm-obj)
                             (add-access-control-group url realm-obj :capabilities capabilities))
                           (access-control
                             (cond
                               (realm-obj
                                (unless (eq realm-obj (access-control-realm capabilities))
                                  (error "REALM, ~S, for the URL, ~S, is not the same as the authentication realm for CAPABILITIES, ~S.
They must be the same." realm-obj url capabilities)))
                               (t (setq realm-obj (access-control-realm capabilities))
                                  (require-realm realm-obj)))
                             capabilities)
                           (t (require-realm realm-obj)
                              (intern-access-control realm-obj capabilities :if-does-not-exist :error))))
    ;; set the instance variables.
    (with-slots (url:authentication-realm url:capabilities) url
      (setq url:authentication-realm realm-obj
            url:capabilities access-control))))

