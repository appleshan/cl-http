;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Patch-File: T -*-
;;; Patch file for HTTP-PROXY version 6.20
;;; Reason: Function HTTP::PROXY-CACHEABLE-RESPONSE-HEADER-PLIST:  use safe-header-value.
;;; Function HTTP::PROXY-CACHEABLE-REQUEST-HEADER-PLIST:  ditto.
;;; Written by JCMa, 4/22/01 22:23:34
;;; while running on FUJI-3 from FUJI:/usr/lib/symbolics/Inc-CL-HTTP-70-90-LMFS.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, LMFS 442.1, Color 427.1,
;;; Graphics Support 431.0, Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Color System Documentation 10.0, SGD Book Design 10.0, Images 431.2,
;;; Image Substrate 440.4, CLIM 72.0, Genera CLIM 72.0, CLX CLIM 72.0,
;;; PostScript CLIM 72.0, CLIM Demo 72.0, CLIM Documentation 72.0,
;;; Statice Runtime 466.1, Statice 466.0, Statice Browser 466.0,
;;; Statice Server 466.2, Statice Documentation 426.0, Metering 444.0,
;;; Metering Substrate 444.1, Symbolics Concordia 444.0, Graphic Editor 440.0,
;;; Graphic Editing 441.0, Bitmap Editor 441.0, Graphic Editing Documentation 432.0,
;;; Postscript 436.0, Concordia Documentation 432.0, Joshua 237.4,
;;; Joshua Documentation 216.0, Joshua Metering 206.0, Jericho 237.0, C 440.0,
;;; Lexer Runtime 438.0, Lexer Package 438.0, Minimal Lexer Runtime 439.0,
;;; Lalr 1 434.0, Context Free Grammar 439.0, Context Free Grammar Package 439.0,
;;; C Runtime 438.0, Compiler Tools Package 434.0, Compiler Tools Runtime 434.0,
;;; C Packages 436.0, Syntax Editor Runtime 434.0, C Library Headers 434,
;;; Compiler Tools Development 435.0, Compiler Tools Debugger 434.0,
;;; C Documentation 426.0, Syntax Editor Support 434.0, LL-1 support system 438.0,
;;; Pascal 433.0, Pascal Runtime 434.0, Pascal Package 434.0, Pascal Doc 427.0,
;;; Fortran 434.0, Fortran Runtime 434.0, Fortran Package 434.0, Fortran Doc 427.0,
;;; HTTP Proxy Server 6.19, HTTP Server 70.121, Showable Procedures 36.3,
;;; Binary Tree 34.0, W3 Presentation System 8.1, HTTP Client Substrate 4.9,
;;; HTTP Client 50.6, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.26, Genera 8 5 Metering Patches 1.0,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Jericho Patches 1.0,
;;; Genera 8 5 Joshua Doc Patches 1.0, Genera 8 5 Joshua Metering Patches 1.0,
;;; Genera 8 5 Statice Runtime Patches 1.0, Genera 8 5 Statice Patches 1.0,
;;; Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clx Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Clim Demo Patches 1.0, Genera 8 5 Color Patches 1.1,
;;; Genera 8 5 Images Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Genera 8 5 Concordia Patches 1.0,
;;; Genera 8 5 Concordia Doc Patches 1.0, Genera 8 5 C Patches 1.0,
;;; Genera 8 5 Pascal Patches 1.0, Genera 8 5 Fortran Patches 1.0, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x976 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number -2141189522,
;;; Vlm lmfs patch (from W:>Reti>vlm-lmfs-patch.lisp.12),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>Reti>background-dns-refreshing.lisp.11),
;;; Pht debugging patch (from W:>Reti>pht-debugging-patch.lisp.4),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:PROXY;UTILS.LISP.75")


(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL)
  (SCT:REQUIRE-PATCH-LEVEL-FOR-PATCH '(CL-HTTP 70. 122.)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;UTILS.LISP.75")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: HTTP; Base: 10 -*-")

(defun proxy-cacheable-response-header-plist (header-set &optional include-headers exclude-headers)
  "Converts HEADER-SET into a property list of (keyword value).
Removes connection level headers"
  (flet ((collect-p (header-keyword header include-headers exclude-headers)
	   (and (cacheable-response-header-p header-keyword)
		(or (member header-keyword include-headers)
		    (not (member exclude-headers exclude-headers))
		    (not (%header-suppress-p header))))))
    (declare (inline collect-p))
    (%with-header-set-index (header-set)
      (with-fast-array-references ((headers headers vector)
				   (index index vector))
	(loop with header-value and parsing-error-p
	      for idx fixnum upfrom 0 below (fill-pointer headers)
	      for header = (aref headers idx)
	      for header-keyword = (aref index idx)
	      when (collect-p header-keyword header include-headers exclude-headers)
		do (multiple-value-setq (header-value parsing-error-p) (safe-header-value header))
		and 
	      unless parsing-error-p
		collect header-keyword
		and collect header-value)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;UTILS.LISP.75")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: HTTP; Base: 10 -*-")

(defun proxy-cacheable-request-header-plist (header-set &optional include-headers exclude-headers )
  "Converts HEADER-SET into a property list of (keyword value).
Removes all connection-level, request-specific and user-specific headers.
EXCLUDE-HEADERS is a list of additional headers to suppress.
INCLUDE-HEADERS is a list of headers not to suppress, which overrides any other suppression mode."
  (flet ((collect-p (header-keyword header include-headers exclude-headers)
	   (and (cacheable-request-header-p header-keyword)
		(or (member header-keyword include-headers)
		    (not (or (member exclude-headers *user-specific-request-headers*)
			     (member exclude-headers exclude-headers)
			     (%header-suppress-p header)))))))
    (declare (inline collect-p))
    (%with-header-set-index (header-set)
      (with-fast-array-references ((headers headers vector)
				   (index index vector))
	(loop with header-value and parsing-error-p
	      for idx fixnum upfrom 0 below (fill-pointer headers)
	      for header = (aref headers idx)
	      for header-keyword = (aref index idx)
	      when (collect-p header-keyword header include-headers exclude-headers)
		do (multiple-value-setq (header-value parsing-error-p) (safe-header-value header))
		and unless parsing-error-p
		      collect header-keyword
		      and collect header-value)))))

