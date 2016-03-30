;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.198
;;; Reason: Function HTTP::%ABSOLUTIZE-RELATIVE-URL:  new
;;; Function HTTP::ABSOLUTIZE-RELATIVE-URL:  use it.
;;; Function HTTP::SET-URL-CONTEXT-REMAPPING:  update.
;;; Function HTTP::%DEFINE-URL-CONTEXT-REMAPPINGS:  update
;;; Function HTTP:DEFINE-URL-CONTEXT-REMAPPINGS:  update.
;;; Written by jcma, 7/11/05 13:02:12
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
;;; HTTP Server 70.197, Showable Procedures 36.3, Binary Tree 34.0,
;;; Experimental W3 Presentation System 8.1, CL-HTTP Server Interface 54.0,
;;; HTTP Proxy Server 6.34, HTTP Client Substrate 4.23,
;;; W4 Constraint-Guide Web Walker 45.13, HTTP Client 51.8, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.18, DEC OSF/1 V4.0 (Rev. 110),
;;; 1728x1003 24-bit TRUE-COLOR X Screen FUJI:1.0 with 224 Genera fonts (The Olivetti & Oracle Research Laboratory R3323),
;;; Machine serial number -2142637960,
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7),
;;; Get Xauthority pathname from user namespace object. (from W:>jcma>fixes>xauthority-pathname.lisp.2),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;UTILS.LISP.552")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.552")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defun %absolutize-relative-url (scheme host port &optional relative-url canonicalize-host-p)
  (let ((domain-name (string-downcase (if canonicalize-host-p (host-domain-name host) host)))
        (scheme-string (ecase scheme
                         (:http "http://")
                         (:https "https://")
                         (:ftp "ftp://"))))
    (declare (dynamic-extent domain-name))
    (case port
      (80 (apply #'concatenate 'string scheme-string domain-name relative-url))
      (t (apply #'concatenate 'string  scheme-string domain-name ":" (write-to-string port :base 10.) relative-url)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.552")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defun absolutize-relative-url (scheme host port &rest relative-url)
  (declare (dynamic-extent relative-url))
  (%absolutize-relative-url scheme host port relative-url t))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.552")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defun set-url-context-remapping (scheme host port new-host &optional (new-port port) (new-scheme scheme))
  (let* ((old-context (absolutize-relative-url scheme host port))
	 (new-context (%absolutize-relative-url new-scheme new-host new-port nil nil))
	 (entry (assoc old-context *url-context-remapping-alist* :test #'equalp))) 
    (if entry
	(setf (cdr entry) new-context)
      (setq *url-context-remapping-alist* (nconc *url-context-remapping-alist* `((,old-context . ,new-context)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.552")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defun %define-url-context-remappings (specs)
  (loop initially (clear-url-context-remapping)
	for entry in specs
	do (destructuring-bind (scheme host port new-host &optional (new-port port) (new-scheme scheme)) entry
	     (set-url-context-remapping scheme host port new-host new-port new-scheme)))
  *url-context-remapping-alist*)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.552")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmacro define-url-context-remappings (&rest specs)
  "Defines remappings from local contexts handled by the server to remote context,
such as the external name when operating through a reverse proxy.

SPECS is a list of remapping specifications of the form:
    (SCHEME HOST PORT NEW-HOST &optional (NEW-PORT port) (NEW-SCHEME scheme))

NEW-HOST is not canonicalized by a reverse DNS lookup."
  `(%define-url-context-remappings ',specs))

