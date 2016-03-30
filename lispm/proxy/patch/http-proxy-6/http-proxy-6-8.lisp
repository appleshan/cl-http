;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for HTTP-PROXY version 6.8
;;; Reason: Function (CLOS:METHOD HTTP::REPRESENTATION-NOTE-TRANSACTION (HTTP::REPRESENTATION T T T)):  handle unbound http-status slot in case of 304.
;;; Written by JCMa, 12/06/00 15:05:06
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
;;; HTTP Proxy Server 6.7, HTTP Server 70.93, Showable Procedures 36.3,
;;; Binary Tree 34.0, W3 Presentation System 8.1, HTTP Client Substrate 4.1,
;;; HTTP Client 50.0, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Metering Patches 1.0,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Jericho Patches 1.0,
;;; Genera 8 5 Joshua Doc Patches 1.0, Genera 8 5 Joshua Metering Patches 1.0,
;;; Genera 8 5 Statice Runtime Patches 1.0, Genera 8 5 Statice Patches 1.0,
;;; Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.0,
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing),
;;; Pht debugging patch (from W:>reti>pht-debugging-patch.lisp.4),
;;; Cname level patch (from W:>reti>cname-level-patch).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:PROXY;REPRESENTATION.LISP.67")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;REPRESENTATION.LISP.67")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod representation-note-transaction ((representation representation) http-status http-version verified-date
					    &optional request-headers)
  (check-type verified-date integer)
  (flet ((must-revalidate-p (http-version request-headers response-headers)
	   (cond ((or (getf response-headers :set-cookie)	;always revalidate for cookies
		      (get-header :cookie request-headers))
		  (return-from must-revalidate-p t))
		 (t (with-header-values (authorization) request-headers
		      (unless (member http-version '(:http/0.9 :http/1.0))
			(multiple-value-bind (directive found-p)
			    (getf response-headers :cache-control)
			  (when found-p
			    (loop for (key value) on directive by #'cddr
				  when (and (member key '(:must-revalidate :proxy-revalidate)) value)
				    do (return-from must-revalidate-p t))
			    (when authorization	;HTTP 1.1 spec 14.8
			      (let ((s-maxage (getf directive :s-maxage)))
				(return-from must-revalidate-p
				  (cond (s-maxage
					 (if (zerop s-maxage) t nil))
					((or (getf directive :public)
					     (not (getf directive :must-revalidate :not-found)))
					 nil)
					(t t))))))))
		      ;; revalidate any requests with AUTHORIATION, cookie
		      (not (null authorization)))))))
    (declare (inline must-revalidate-p))
    (let ((response-headers (representation-response-headers representation)))
      (when request-headers
	(setf (representation-request-headers representation) (proxy-header-plist request-headers)
	      ;; assume that revalidation doesn't change when no request headers.
	      (representation-must-revalidate-p representation) (must-revalidate-p http-version request-headers response-headers)))
      (setf (representation-verified-date representation) verified-date
	    (representation-etag representation) (getf response-headers :etag)
	    (representation-last-modification representation) (getf response-headers :last-modified)
	    (representation-http-version representation) http-version)
      (case http-status
	(304
	  (unless (slot-boundp representation 'http-status)
	    (setf (representation-http-status representation) 200)))
	(t (setf (representation-http-status representation) http-status))))
    ;; Reset expiration times
    (representation-compute-expiration-time-from-headers representation)
    representation))				;return the representation

