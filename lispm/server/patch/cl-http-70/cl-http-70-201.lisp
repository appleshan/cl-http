;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for CL-HTTP version 70.201
;;; Reason: Function HTTP::PARSE-MIME-HEADER-PARAMETERS:  Robustify.
;;; Function HTTP::%PARSE-EXTENDED-COMMON-LOG-FORMAT-LOG-ENTRY:  add time-parser optional argument.
;;; Written by jcma, 8/01/05 15:33:01
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
;;; HTTP Server 70.200, Showable Procedures 36.3, Binary Tree 34.0,
;;; Experimental W3 Presentation System 8.1, CL-HTTP Server Interface 54.0,
;;; HTTP Proxy Server 6.34, HTTP Client Substrate 4.23,
;;; W4 Constraint-Guide Web Walker 45.13, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.18, DEC OSF/1 V4.0 (Rev. 110),
;;; 1728x985 24-bit TRUE-COLOR X Screen FUJI:1.0 with 224 Genera fonts (The Olivetti & Oracle Research Laboratory R3323),
;;; Machine serial number -2142637960,
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7),
;;; Get Xauthority pathname from user namespace object. (from W:>jcma>fixes>xauthority-pathname.lisp.2),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;HEADERS.LISP.537"
  "HTTP:SERVER;LOG.LISP.223")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.537")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun parse-mime-header-parameters (string &optional (start 0) (end (length string)))
  (flet ((delimiter-p (char)
           (member char '(#\; #\space #\tab) :test #'eql))
	 (intern-value (key string s e)
	   (declare (fixnum s e))
	   (case key
	     ((:boundary :name :filename)	;parameters used in content-disposition for file upload 4/13/2001 -- JCMa.
	      (subseq string s e))		;some parameters are strings
	     (t (with-fast-array-references ((string string string))
		  (loop with quality-value-p
			for idx fixnum upfrom s below e
			for ch = (aref string idx)
			unless (or (digit-char-p ch)
				   (setq quality-value-p (eql ch #\.)))
			  return (%tokenize-header-keyword string s e)
			finally (return (parse-number string s e quality-value-p))))))))
    (declare (inline intern-value))
    (loop with param-key and param-value
	  for s = (position-if-not* #'delimiter-p string :start start :end end) then (the fixnum (1+ idx))
	  while (and s (< s end))
	  for idx fixnum = (or (char-position #\; string (1+ s) end) end)
          for delim of-type (or null fixnum) = (char-position #\= string (1+ s) idx)	; if this returns NIL, there is an error
	  for p1 = (and delim (position-valid-mime-char string (1+ delim) idx))
	  for p2 = (and delim (position-if* #'mime-valid-char-for-token-p string :start p1 :end idx :from-end t))
	  when p2
	    do (setq param-key (%tokenize-header-keyword string (position-valid-mime-char string s delim) delim))
	       (setq param-value (intern-value param-key string p1 (1+ (the fixnum p2))))
	    and collect param-key
	    and collect param-value)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.223")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

;; Consider tokenizing BROWSER, REFERER and USER.
(defun %parse-extended-common-log-format-log-entry (line &optional (delimiter #\tab) (time-parser #'parse-gmt-time)
                                                         &aux (end (length line)))
  (declare (values url host method date server-version status bytes user-name browser referer)
           (fixnum end))
  (labels ((field-delimiter-char-p (x)
             (char= x delimiter))
           (white-space-p (x)
             (char= x #\space))
           (parse-exact-request (exact-request start end)
             (declare (fixnum end))
             (unless (eql start end)
               (let* ((r1 (%fast-position-if white-space-p exact-request :start start :end end))
                      (r2 (%fast-position-if white-space-p exact-request :start (1+ r1) :end end))
                      (method (%tokenize-header-keyword exact-request start r1))
                      (url (subseq exact-request (1+ r1) (or r2 end)))
                      (server-version (if r2
                                          (%tokenize-header-keyword exact-request (the fixnum (1+ r2)) end)
                                          :HTTP/0.9)))	; http 0.9 or telnet hackers don't send version
                 (declare (fixnum r1)
                          (type (or null fixnum) r2))
                 (values method url server-version))))
           (parse-field (line start end bounding-delimiter)
             (declare (fixnum start))
             (if (eql #\- (aref line start))
                 nil
                 (let ((s1 (char-position bounding-delimiter line start end))
                       (s2 (char-position bounding-delimiter line start end t)))
                   (when (and s1 s2)
                     (subseq line (1+ (the fixnum s1)) s2))))))
    (declare (inline field-delimiter-char-p parse-exact-request parse-field))
    (let (p1 p2 p3 p4 p5 p6 p7 p8 p9 p10)
      p3
      (cond
        ((and (setq p1 (%fast-position-if field-delimiter-char-p line :start 0 :end end))
              (setq p2 (%fast-position-if field-delimiter-char-p line :start (1+ (the fixnum p1)) :end end))
              (setq p3 (%fast-position-if field-delimiter-char-p line :start (1+ (the fixnum p2)) :end end))
              (setq p4 (char-position #\[ line (1+ (the fixnum p3)) end))
              (setq p5 (char-position #\] line (1+ (the fixnum p4)) end))
              (setq p6 (char-position #\" line (1+ (the fixnum p5)) end))
              (setq p7 (char-position #\" line (1+ (the fixnum p6)) end))
              (< (1+ (the fixnum p6)) p7)       ;empty request string is a bad entry  7/20/95 -- JCMa.
              (setq p8 (%fast-position-if field-delimiter-char-p line :start (+ 2 (the fixnum p7)) :end end))
              (setq p9 (%fast-position-if field-delimiter-char-p line :start (1+ (the fixnum p8)) :end end))
              (setq p10 (%fast-position-if field-delimiter-char-p line :start (1+ (the fixnum p9)) :end end)))
         (locally
           (declare (fixnum p1 p2 p3 p4 p5 p6 p7 p8))
           (multiple-value-bind (method url server-version)
               (parse-exact-request line (1+ p6) p7) 
             (let ((host (subseq line 0 p1))
                   (date (funcall time-parser line (1+ p4) p5))
                   (status (parse-integer line :start (+ 2 p7) :end p8))
                   (bytes (parse-integer line :start (1+ p8) :end p9))
                   (user (parse-field line (1+ p2) p4 #\"))
                   (browser (parse-field line p9 p10 #\"))
                   (referer (parse-field line p10 end #\")))
               (values url host method date server-version status bytes user browser referer)))))
        (t (values nil nil :bad-record (list p1 p2 p3 p4 p5 p6 p7 p8 p9 p10)))))))

