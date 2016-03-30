;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.180
;;; Reason: Function (CLOS:METHOD HTTP::%PARSE-AUTHORIZATION-HEADER (T (EQL :DIGEST))):  fix inline declaration.
;;; Function HTTP::WITH-PARSED-CERN-LINE:  fix declarations and use faster operators.
;;; Function HTTP::PARSE-CERN-CIRCLE:  -
;;; Function HTTP::PARSE-CERN-POLYGON:  -
;;; Function HTTP::PARSE-CERN-RECTANGLE:  -
;;; Written by JCMa, 10/10/03 19:50:24
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5-inc1.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.6,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.179,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.4, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.5, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.41, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Documentation Patches 1.0,
;;; Genera 8 5 Clim Patches 1.3, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; Jcma 44, HTTP Proxy Server 6.34, HTTP Client Substrate 4.23, HTTP Client 51.6,
;;; Image Substrate 440.4, Essential Image Substrate 433.0, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.18, DEC OSF/1 V4.0 (Rev. 110),
;;; 1585x1133 1-bit STATIC-GRAY X Screen RELATUS:0.0 with 224 Genera fonts (eXodus 8.0  (c) 2001 Powerlan USA,
;;; Inc. R7100),
;;; 1560x1120 24-bit TRUE-COLOR X Screen FUJI:1.0 with 224 Genera fonts (The Olivetti & Oracle Research Laboratory R3323),
;;; Machine serial number -2141194968,
;;; Domain Fixes (from CML:MAILER;DOMAIN-FIXES.LISP.33),
;;; Don't force in the mail-x host (from CML:MAILER;MAILBOX-FORMAT.LISP.24),
;;; Make Mailer More Robust (from CML:MAILER;MAILER-FIXES.LISP.15),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add CLIM presentation and text style format directives. (from FV:SCLC;FORMAT.LISP.20),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; Make update schema work on set-value attributes with accessor names (from CML:LISPM;STATICE-SET-VALUED-UPDATE.LISP.1),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.107),
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;HEADERS.LISP.530"
  "HTTP:SERVER;IMAGE-MAPS.LISP.32")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.530")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod %parse-authorization-header (string (method (eql :digest)) &optional (start 0) (end (length string)))
  (flet ((value-getter (string start end)
           (triming-substring '(#\" #\space #\tab) string start end))
	 (value-end-index (string start end)
	   (declare (fixnum start end))
	   (let* ((p1 (char-position #\= string start end))
		  (p2 (position-if-not* #'white-space-char-p string :start (1+ (the fixnum p1)) :end end))
		  p3)
	     (if (eql (aref string p2) #\")
		 (and (setq p3 (char-position #\" string (1+ (the fixnum p2)) end))
		      (char-position #\, string (1+ (the fixnum p3)) end))
		 (char-position #\, string p1 end)))))
    (declare (inline value-end-index))
    (loop with keyword and value
	  for s = start then (1+ (the fixnum e))
	  while (< s end)
	  for s1 = (position-if-not* #'white-space-char-p string :start s :end end)
	  while s1
	  for e = (or (value-end-index string s1 end) end)
	  do (multiple-value-setq (keyword value)
	       (parse-equal-sign-delimited-pair string s1 e #'value-getter t))
	  collect keyword into plist
	  collect (case keyword
		    (:uri (string-unescape-special-chars value))
		    (t value))
	    into plist
	  finally (return `(,method . ,plist)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;IMAGE-MAPS.LISP.32")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmacro with-parsed-cern-line ((key line &key (line-offset 0) (line-end nil line-end-supplied-p)
				      (url-var 'url-string) (raw-coordinates-var 'raw-coordinates))
                                 &body body)
  `(let* ((end-key ,(typecase key (string `(length ,key))(t `(length ,key))))
	  (end ,(if line-end-supplied-p line-end `(length ,line)))
          (start (string-search ,key ,line 0 end-key ,line-offset end))
          (end-url (fast-position-if-not #'white-space-char-p ,line start end t))
          (end-coord (fast-position-if #'white-space-char-p ,line start end-url t))
          (,url-var (subseq ,line (1+ end-coord) (1+ end-url)))
          (,raw-coordinates-var (subseq ,line start end-coord)))
     (declare (fixnum end-key end start end-url end-coord))
     ,@body))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;IMAGE-MAPS.LISP.32")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun parse-cern-circle (line &optional (start 0) (end (length line)))
  (with-parsed-cern-line ("circ" line :line-offset start :line-end end)
    (let ((start-radius (1+ (the fixnum (char-position #\) line start end)))))
      (make-instance 'region
                     :destination (parse-destination url-string)
                     :bounding-shape (make-circle (car (parse-cern-coords raw-coordinates))
                                                  (parse-integer line :start start-radius :junk-allowed t))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;IMAGE-MAPS.LISP.32")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun parse-cern-polygon (line &optional (start 0) (end (length line)))
  (with-parsed-cern-line ("poly" line :line-offset start :line-end end)
    (let* ((point-list (parse-cern-coords raw-coordinates))
           (last (last point-list)))
      (unless (point-equal-p (first point-list) (first last))
        (setf last `(,.last ,(first point-list))))
      (make-instance 'region
                     :destination (parse-destination url-string)
                     :bounding-shape (make-polygon point-list)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;IMAGE-MAPS.LISP.32")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun parse-cern-rectangle (line &optional (start 0) (end (length line)))
  (with-parsed-cern-line ("rect" line :line-offset start :line-end end)
    (destructuring-bind (first . rest) (parse-cern-coords raw-coordinates)
      (make-instance 'region
                     :destination (parse-destination url-string)
                     :bounding-shape (make-rectangle first (car rest))))))

