;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.131
;;; Reason: Update inline callers of %tokenize-* functions.
;;; Function (CLOS:METHOD HTTP::REPORT-STATUS (HTTP::HTTP-CONDITION T) :AROUND):  get into text mode before reporting error.
;;; Function HTTP::PARSE-VIA-HEADER:  -
;;; Written by JCMa, 5/10/01 19:05:08
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.129,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.4, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.26, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Documentation Patches 1.0,
;;; Genera 8 5 Clim Patches 1.2, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Jcma 43, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x994 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number -2141189585,
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.11).

;;; Patch file for CL-HTTP version 70.131
;;; Written by JCMa, 5/12/01 00:39:25
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.132,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.4, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.26, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Lock Simple Patches 1.0, Jcma 43,
;;; HTTP Proxy Server 6.22, HTTP Client Substrate 4.12, Statice Server 466.2,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.16,
;;; DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x994 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number -2141189585,
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.11),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1).


;;; Patch file for CL-HTTP version 70.131
;;; Written by JCMa, 5/10/01 20:29:39
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.131,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.4, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.26, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Lock Simple Patches 1.0, Jcma 43,
;;; HTTP Proxy Server 6.21, HTTP Client Substrate 4.10, Statice Server 466.2,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.16,
;;; DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x994 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number -2141189585,
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.11),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1).




(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;SHTML.LISP.25"
  "HTTP:SERVER;LOG.LISP.215"
  "HTTP:SERVER;URL.LISP.426"
  "HTTP:SERVER;IMAGE-MAPS.LISP.31"
  "HTTP:SERVER;SERVER.LISP.908"
  "HTTP:SERVER;HEADERS.LISP.502"
  "HTTP:SERVER;HEADERS.LISP.506"
  "HTTP:SERVER;HEADERS.LISP.501"
  "HTTP:SERVER;REPORT.LISP.185"
  "HTTP:SERVER;HTTP-CONDITIONS.LISP.197")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.501")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun %define-header (header header-type header-class &optional print-name default parse-function print-function
			      print-series-predicate atomic-value-reducer)
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

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.501")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun parse-authorization-header (string &optional (start 0) (end (length string)))
  (with-string-trim-bounds (*white-space-chars* string start end)
    (unless (= start end)
      (let* ((pos (position-if* #'white-space-char-p string :start start :end end))
	     (method (%tokenize-header-keyword string start (or pos pos))))
	(%parse-authorization-header string method (if pos (the fixnum (1+ pos)) end) end)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.501")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun parse-content-range-header (string &optional (start 0) (end (length string)))
  (with-string-trim-bounds (*white-space-chars* string start end)
    (let (pos1 start-pos pos2 last-pos pos3 length-pos)
      (cond ((and (setq pos1 (char-position #\space string start end))
		  (setq start-pos (1+ (the fixnum pos1)))
		  (setq pos2 (char-position #\- string start-pos end))
		  (setq last-pos (1+ (the fixnum pos2)))
		  (setq pos3 (char-position #\/ string last-pos end))
		  (setq length-pos (1+ (the fixnum pos3))))
	     `(,(%tokenize-header-keyword string start pos1)
	       ,(parse-integer string :start start-pos :end pos2 :radix 10)	;start postion
	       ,(parse-integer string :start last-pos :end pos3 :radix 10)	;last position
	       ,(parse-integer string :start length-pos :end end :radix 10)))	;entity length
	    (t (error 'bad-range-header-value :format-string "Bad value for Content-Range header: ~S"
		      :format-args (list (subseq string start end))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.501")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun parse-equal-sign-delimited-pair (string &optional (start 0) (end (length string)) (header-value-parser #'subseq)
                                               (require-value-p t))
  (declare (values keyword value))
  (flet ((%get-value (string delimiter end)
           (loop with v-pos = (1+ (the fixnum delimiter))
                 for idx upfrom v-pos below end
                 for char = (aref string idx)
                 unless (or (digit-char-p char)
                            (white-space-char-p char))
                   do (return-from %get-value (funcall header-value-parser string v-pos end))
                 finally (return-from %get-value (parse-integer string :start v-pos :end end :junk-allowed t)))))
    (declare (inline %get-value))
    (let* ((delim-pos (char-position #\= string start end))
           (pos (position-if-not* #'white-space-char-p string :start start :end (or delim-pos end) :from-end t)))
      (cond (delim-pos
             (values (%tokenize-header-keyword string start (1+ (the fixnum pos)))
                     (%get-value string delim-pos end)))
            (require-value-p
             (error "No = delimiter found in the equal sign delimited pair, ~A" (subseq string start end)))
            (t (values (%tokenize-header-keyword string start (1+ (the fixnum pos))) t))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.501")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun parse-mime-content-disposition-header (string &optional (start 0) (end (length string))
                                                     &aux parameters)
  (with-string-trim-bounds (*white-space-chars* string start end)
    (let* ((parameter-index (mime-header-parameter-index string start end))
	   (disposition (%tokenize-header-keyword string start (or parameter-index end))))
      (when parameter-index
	(setq parameters (parse-mime-header-parameters string (the fixnum (1+ (the fixnum parameter-index))) end)))
      (list* disposition parameters))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.501")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun parse-mime-content-type (string &optional (start 0) (end (length string)))
  (let* ((subtype-index (char-position #\/ string start end))
	 (start-index (position-valid-mime-char string start (or subtype-index end)))
	 (major-type (and (or (null subtype-index) (< start-index subtype-index))
			  (%tokenize-header-keyword string start-index (or subtype-index end)))))
    (cond ;; Content types normally have a major and minor type.
      (subtype-index
       (let* ((subtype-start (the fixnum (1+ subtype-index)))
	      (subtype-end (position-invalid-mime-char string subtype-start end)))
	 (values major-type (and (< subtype-start subtype-end)
				 (%tokenize-header-keyword string subtype-start subtype-end)))))
      (t (values major-type nil)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.501")
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
	  for delim fixnum = (char-position #\= string (1+ s) idx)
	  for p1 = (position-valid-mime-char string (1+ delim) idx)
	  for p2 = (position-if* #'mime-valid-char-for-token-p string :start p1 :end idx :from-end t)
	  when p2
	    do (setq param-key (%tokenize-header-keyword string (position-valid-mime-char string s delim) delim))
	       (setq param-value (intern-value param-key string p1 (1+ (the fixnum p2))))
	    and collect param-key
	    and collect param-value)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.501")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;; RFC 2616 says a an invalid range specs requires the whole header to be ignored. 10/5/99 -- JCMa.
(defun parse-range-header (string &optional (start 0) (end (length string)))
  (flet ((parse-range-entry (string s e)
           (let ((pos (char-position #\- string s e))
                 pos2 range-start range-end)
             (cond (pos
                    (setq pos2 (1+ (the fixnum pos)))
		    (handler-case-if (not *debug-server*) 
		       (unless-every
			 ((= pos s)
			  (setq range-start (parse-integer string :start s :end pos :radix 10)))
			 ((= pos2 e)
			  (setq range-end (parse-integer string :start pos2 :end e :radix 10))))
		      (error () (return-from parse-range-header nil)))
		    (when (and range-start range-end)
		      (unless (<= range-start range-end))
		      (return-from parse-range-header nil))
		    (list range-start range-end))
                   (t (return-from parse-range-header nil))))))
    (declare (inline parse-range-entry))
    (with-string-trim-bounds (*white-space-chars* string start end)
      (unless (= start end)
	(loop with pos = (char-position #\= string start end)
	      for s = (1+ (the fixnum pos)) then (1+ (the fixnum e))
	      for e = (or (char-position #\, string s end) end)
	      collect (parse-range-entry string s e) into ranges
	      until (= e end)
	      finally (return (cons (%tokenize-header-keyword string start pos) ranges)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.501")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun parse-uri-header (string &optional (start 0) (end (length string)))
  (labels ((signal-error (problem string start end)
	     (error 'bad-syntax-provided :url (server-url *server*)
		    :format-string "Bad syntax in URI header: ~A in ~S."
		    :format-args (list problem (subseq string start end))))
	   (parse-vary (string start end &aux pos1)
             (when (and (string-search= ";vary=" string 0 6 start end)
                        (setq pos1 (char-position #\" string (+ start 6) end)))
               (let ((pos2 (char-position #\" string (1+ pos1) end t)))
                 (cond (pos2
                        (loop for s1 = (1+ pos1) then (1+ e1)
                              for e1 = (or (char-position #\, string s1 pos2) pos2)
                              collect (%tokenize-header-keyword string s1 e1)
                              until (eql e1 pos2)))
                       (t (signal-error "Missing closing \"" string start end))))))
           (parse-entry (string s1 e1)
             (let ((e2 (char-position #\> string s1 e1)))
               (cond (e2
                      `(,(subseq string (1+ s1) e2) ,.(parse-vary string (1+ e2) e1)))
                     (t (signal-error "Missing closing >" string s1 e1))))))
    (declare (inline parse-entry parse-vary))
    (with-string-trim-bounds (*white-space-chars* string start end)
      (let ((s1 (char-position #\< string start end)))
	(cond (s1
	       (loop for s = s1 then e
		     for e = (char-position #\< string (1+ s) end)
		     collect (parse-entry string s (or e end))
		     while e))
	      ((or (char-position #\> string start end)
		   (string-search= ";vary=" string 0 6 start end))
	       (signal-error "Missing openning <" string start end))
	      ;; handle losers robustly
	      (t (list (subseq string start end))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.501")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;; Not so efficient or slick.  Fix sometime.   7/13/96 -- JCMa.
(defun parse-www-authenticate-header (string &optional (start 0) (end (length string)))
  (flet ((parse-domain (string &optional (start 0) (end (length string)))
           (flet ((get-uri (string start end)
                    (let ((s (position-if-not* #'(lambda (ch) (member ch '(#\< #\space #\") :test #'eql))
                                              string :start start :end end))
                          (e (position-if-not* #'(lambda (ch) (member ch '(#\> #\space #\") :test #'eql))
                                              string :start start :end end :from-end t)))
                      (subseq string s e))))
             (parse-comma-separated-header string start end #'get-uri))))
    (declare (inline parse-domain))
    (with-string-trim-bounds (*white-space-chars* string start end)
      (let* ((pos (position-if* #'white-space-char-p string :start start :end end))
	     (method (%tokenize-header-keyword string start pos))
	     (args (parse-equal-sign-delimited-pairs string (the fixnum (1+ pos)) end #\,)))
	(loop with realm
	      for (keyword value) on args by #'cddr
	      when (eq keyword :realm)
		do (setq realm (string-trim '(#\") value))
	      else
		collect keyword into plist
		and
	      collect (case keyword
			(:stale (equalp value "TRUE"))
			(:algorithm
			  (let ((s 0)
				(e (length value)))
			    (with-string-trim-bounds ('(#\") value s e) 
			      (%tokenize-header-keyword value s e))))
			(:domain (parse-domain value))                  
			(t (string-trim '(#\") value)))
		into plist
	      finally (return `(,method ,realm ,.plist)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.215")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

; (%parse-common-log-format-log-entry "GATOR-MAC-9.AI.MIT.EDU - - [1994-09-28 00:51:21] \"GET /homepage HTTP/1.0\" 200 3856")

(defun %parse-common-log-format-log-entry (line &aux (end (length line)))
  (declare (values url host method date server-version status bytes user-name)
	   (fixnum end))
  (labels ((white-space-p (x)
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
	   (parse-user (line start end)
	     (declare (fixnum start))
	     (if (and (eql #\- (aref line start)) (eql #\space (aref line (1+ start))))
		 nil
		 (subseq line start (1+ (the fixnum (%fast-position-if-not white-space-p line :start start :end end :from-end t)))))))
    (declare (inline white-space-p parse-exact-request parse-user))
    (let (p1 p2 p3 p4 p5 p6 p7 p8)
      p3
      (cond
	((and (setq p1 (%fast-position-if white-space-p line :start 0 :end end))
	      (setq p2 (%fast-position-if white-space-p line :start (1+ (the fixnum p1)) :end end))
	      (setq p3 (%fast-position-if white-space-p line :start (1+ (the fixnum p2)) :end end))
	      (setq p4 (char-position #\[ line (1+ (the fixnum p3)) end))
	      (setq p5 (char-position #\] line (1+ (the fixnum p4)) end))
	      (setq p6 (char-position #\" line (1+ (the fixnum p5)) end))
	      (setq p7 (char-position #\" line (1+ (the fixnum p6)) end))
	      (< (1+ (the fixnum p6)) p7)       ;empty request string is a bad entry  7/20/95 -- JCMa.
	      (setq p8 (%fast-position-if white-space-p line :start (+ 2 (the fixnum p7)) :end end)))
	 (locally
	   (declare (fixnum p1 p2 p3 p4 p5 p6 p7 p8))
	   (multiple-value-bind (method url server-version)
	       (parse-exact-request line (1+ p6) p7)
	     (let ((host (subseq line 0 p1))
		   (date (parse-gmt-time line (1+ p4) p5))
		   (status (parse-integer line :start (+ 2 p7) :end p8))
		   (bytes (parse-integer line :start (1+ p8) :end end))
		   (user (parse-user line (1+ p2) p4)))
	       (values url host method date server-version status bytes user)))))
	(t (values nil nil :bad-record))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.215")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

;; Consider tokenizing BROWSER, REFERER and USER.
(defun %parse-extended-common-log-format-log-entry (line &optional (delimiter #\tab) &aux (end (length line)))
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
                   (date (parse-gmt-time line (1+ p4) p5))
                   (status (parse-integer line :start (+ 2 p7) :end p8))
                   (bytes (parse-integer line :start (1+ p8) :end p9))
                   (user (parse-field line (1+ p2) p4 #\"))
                   (browser (parse-field line p9 p10 #\"))
                   (referer (parse-field line p10 end #\")))
               (values url host method date server-version status bytes user browser referer)))))
        (t (values nil nil :bad-record))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.215")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(PROGN
#|gnoscere.ai.mit.edu  - [1999-07-12 20:37:28] "POST /cl-http/find-documentation.html HTTP/1.1" 200 560 3577 "Mozilla/4.0 (compatible; MSIE 4.5; Mac_PowerPC)" "http://fuji-vlm.ai.mit.edu/cl-http/find-documentation.html" ((:SUBSTRING "flog") (:MODULE "HTTP") (:LISP-TYPE "ALL") (:DOCUMENTATION "YES") (:EXTERNAL "NO") (:SUBMIT "Submit"))|#

(defun %parse-http-post-log-entry (line &optional (start 0) (end (length line)))
  (declare (values url host method date http-version status bytes-received bytes-transmitted user referer
		   form-alist user-agent ua-version ua-comment)
	   (fixnum start end))
  (labels ((delimiter-char-p (x)
	     (char= x #\tab))
	   (white-space-p (x)
	     (char= x #\space))
	   (null-entry-p (string start end)
	     (declare (fixnum start end))
	     (and (= (1+ start) end) (eql #\- (aref string start))))
	   (parse-exact-request (exact-request start end)
	     (declare (fixnum start end))
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
	   (parse-user (line start end)
	     (declare (fixnum start end))
	     (subseq line start (1+ (the fixnum (%fast-position-if-not delimiter-char-p line :start start :end end :from-end t))))))
    (declare (inline delimiter-char-p parse-exact-request parse-user))
    (let (p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13)
      p3
      (cond
	((and (setq p1 (%fast-position-if-not delimiter-char-p line :start start :end end))
	      (setq p2 (%fast-position-if delimiter-char-p line :start (1+ (the fixnum p1)) :end end))
	      (setq p3 (%fast-position-if delimiter-char-p line :start (1+ (the fixnum p2)) :end end))
	      (setq p4 (char-position #\[ line (1+ (the fixnum p3)) end))
	      (setq p5 (char-position #\] line (1+ (the fixnum p4)) end))
	      (setq p6 (char-position #\" line (1+ (the fixnum p5)) end))
	      (setq p7 (char-position #\" line (1+ (the fixnum p6)) end))
	      (< (1+ (the fixnum p6)) p7)	;empty request string is a bad entry  7/20/95 -- JCMa.
	      (setq p8 (%fast-position-if delimiter-char-p line :start (1+ (the fixnum p7)) :end end))
	      (setq p9 (%fast-position-if delimiter-char-p line :start (1+ (the fixnum p8)) :end end))
	      (setq p10 (%fast-position-if delimiter-char-p line :start (1+ (the fixnum p9)) :end end))
	      (setq p11 (%fast-position-if delimiter-char-p line :start (1+ (the fixnum p10)) :end end))
	      (setq p12 (%fast-position-if delimiter-char-p line :start (1+ (the fixnum p11)) :end end))
	      (setq p13 (%fast-position-if delimiter-char-p line :start (1+ (the fixnum p12)) :end end)))
	 (locally
	   (declare (fixnum p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13))
	   (multiple-value-bind (method url http-version)
	       (parse-exact-request line (1+ p6) p7)
	     method
	     (let ((host (subseq line p1 p2))
		   (user (unless (null-entry-p line (1+ p2) p3)
			   (parse-user line (1+ p2) p3)))
		   (date (parse-gmt-time line (1+ p4) p5))
		   (status (parse-integer line :start (1+ p8) :end p9))
		   (bytes-received (parse-integer line :start (1+ p9) :end p10))
		   (bytes-transmitted (parse-integer line :start (1+ p10) :end p11))
		   (referer (unless (null-entry-p line (1+ p12) p13)
			      (subseq line (+ 2 p12) (1- p13))))
		   (form-alist (parse-form-alist-log-entry line (1+ p13) end)))
	       (multiple-value-bind (user-agent ua-version ua-comment)
		   (unless (null-entry-p line (1+ p11) p12)
		     (parse-user-agent line (+ 2 p11) (1- p12)))
		 (values url host method date http-version status bytes-received bytes-transmitted user referer
			 form-alist user-agent ua-version ua-comment))))))
	(t (values nil nil :bad-record))))))

)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SHTML.LISP.25")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(define parse-shtml-element (element &optional (start 0) (end (length element)))
  "Parses a full SHTML element and returns a funcallable function or errors."
  (declare (values function parameter-plist)
           (fixnum end))
  (let* ((end1 (- end #.(length *shtml-tag-close*)))
         (pos (position #\# element :start start :end end1))
         (start1 (1+ (the fixnum pos)))
         (pos1 (and pos (position-if #'white-space-char-p element :start start1 :end end1)))
         (method (and pos1 (%tokenize-header-keyword element start1 pos1)))
         (plist (parse-equal-sign-delimited-pairs element (1+ (the fixnum pos1)) end1 #\space t)))
    (get-shtml-operation method plist)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;IMAGE-MAPS.LISP.31")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun parse-ncsa-image-map-file (pathname)
  "Parses an NCSA format file from pathname and returns an image map object."
  (flet ((tag-keyword (line start end)
	   (declare (fixnum start end))
	   (let ((e (+ start 3)))
	     (cond ((or (= start end)		;blank line
			(eql #\# (aref line start))	;comment line
			(> e end))		;short line
		    nil)
		   (t (nstring-upcase line :start start :end e)	;don't lose on case
		      (%TOKENIZE-FORM-QUERY-KEYWORD line start e)))))
	 (get-line (file-stream line-buffer &aux (start 0))
	   (multiple-value-bind (line error-p delimiter end)
	       (read-delimited-line file-stream '(#\Return #\Linefeed) nil line-buffer)
	     (declare (ignore delimiter))
	     (cond ((and line (not error-p))
		    (WITH-STRING-TRIM-BOUNDS (*white-space-chars* line start end)
		      (setf (fill-pointer line) end)
		      (values line start end)))
		   (t nil))))
	 (tag-value (tag line start end)
	   (case tag
	     (:def (parse-default line start end))
	     (:rec (parse-ncsa-rectangle line start end))
	     (:cir (parse-ncsa-circle line start end))
	     (:pol (parse-ncsa-polygon line start end))
	     (:ova (parse-ncsa-oval line start end))
	     (:poi (parse-ncsa-point line start end))
	     (t nil))))
    (declare (inline tag-keyword tag-value get-line))
    (using-resource (line-buffer LINE-BUFFER HTTP:*LINE-BUFFER-SIZE*)
      (with-open-file (file-stream pathname :direction :input)
	(loop with default and line and start and end and tag and value
	      doing (multiple-value-setq (line start end)
		      (get-line file-stream line-buffer))
	      while line
	      do (setq tag (tag-keyword line start end)
		       value (tag-value tag line start end))
		 #+ignore(format t "~&~S~&Tag: ~S ~&Value: ~S" line tag value)
	      when (eq tag :def)
		do (setq default value)
	      else unless (or (null value) (eq tag :poi))
		     collect value into regions
	      else unless (null value)
		     collect value into points
	      finally (return (make-image-map :ncsa
					      :url-default (error-checked-default default points pathname)
					      :region-list regions
					      :proximity-list points)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.215")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defun parse-form-alist-log-entry (string &optional (start 0) (end (length string)))
  (flet ((get-keyword (string &optional (start 0) (end (length string)))
	   (declare (fixnum start end))
	   (multiple-value-bind (string unescaped-p new-string-p)
	       (string-unescape-special-chars string start end)
	     unescaped-p
	     (if new-string-p
		 (%tokenize-form-query-keyword string)
		 (%tokenize-form-query-keyword string start end)))))
    (declare (inline get-keyword))
    (unless (= start end)
      (with-fast-array-references ((string string string))
        (loop for s1 = start then (1+ (the fixnum e2))
              while (< s1 end)
              for e1 = (or (char-position #\= string s1 end)
                           (error "ill-formed query alist encoding in ~s" (subseq string start end)))
              for s2 = (1+ (the fixnum e1))
              for e2 = (or (char-position #\& string s2 end) end)
              for keyword = (get-keyword string s1 e1)
              for value = (unless (= s2 e2)
                            (string-unescape-special-chars string s2 e2))
              collect `(,keyword ,value))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.908")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(define parse-form-raw-values (stream buffer bytes &optional (durable-values-p *durable-form-values*))
  "Function that parses bytes with of form values from STREAM.
  It returns an ALIST of (QUERY-ID VALUE COLLECTION-FLAG QUERY-BAGGAGE).  When
  multiple values for QUERY-ID are collected into VALUE, COLLECTION-FLAG
  is non-null.  Otherwise, there is no third value.
  QUERY-BAGGAGE is an overloading field on the query-id that can carry additional information.
  When present it is an atom when COLLECTION-FLAG is null and a list when COLLECTION-FLAG is
  non-null."
  (declare (values raw-query-value-alist))
  (labels ((get-raw-string (buffer start end)
	     (declare (fixnum start end))
	     (multiple-value-bind (string new-end)
		 (nstring-translate-chars buffer start end #\space)
	       (cond ((= start new-end) nil)
		     (durable-values-p (subseq string start new-end))
		     (t (make-array (- (the fixnum new-end) start) :element-type (array-element-type string)
				    :displaced-to string :displaced-index-offset start)))))
	   (unpacked-query-indices (string start end)
	     (declare (fixnum start end))
	     (let ((pos2 (1- end))
		   pos1)
	       (if (and (eql (aref string pos2) #\))
			(setq pos1 (char-position #\( string start pos2)))
		   (values pos1 (1+ (the fixnum pos1)) pos2)
		   (values end))))
	   (get-keyword (buffer start end)
	     (multiple-value-bind (string new-end)
		 (nstring-translate-chars buffer start end #\space)
	       (multiple-value-bind (key-end baggage-start baggage-end)
		   (unpacked-query-indices string start new-end)
		 (values (%tokenize-form-query-keyword string start key-end)
			 (when (and baggage-start baggage-end)
			   (get-raw-string string baggage-start baggage-end)))))))
    (declare (inline unpacked-query-indices get-raw-string get-keyword maybe-push-entry))
    ;; collect the form data over the HTTP connection by reading bytes of characters.
    (multiple-value-bind (string end)
	(crlf-stream-copy-into-string stream bytes 0 buffer)
      (loop with query and baggage and value and alist and ptr 
	    with start fixnum = 0
	    while (< start end)
	    for key-end fixnum = (or (char-position #\= string start end)
				     (error 'bad-form-data-posted
					    :format-string "Bad Form Data Posted: No Query delimiter found in ~S."
					    :format-args (list (subseq string start end))))
	    do (multiple-value-setq (query baggage)
		 (get-keyword string start key-end))
	       ;; get the value
	       (let* ((val-start (1+ key-end))
		      (val-end (or (char-position #\& string val-start end) end)))
		 (declare (fixnum val-start val-end))
		 (setq value (get-raw-string string val-start val-end)	;get the query value
		       start (1+ val-end)))
	    when (and query value)		;maintain query order
	      do (setq ptr (%parse-form-collect-form-value query value baggage alist ptr))
		 (or alist (setq alist ptr))
		 #+ignore (format t "~&QUERY: ~S~:[~;~&Baggage: ~:*~S~]~&VALUE: ~S" query baggage value)
	    finally (return alist)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.908")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun parse-mime-multipart-form-data-block (mime-stream boundary &optional buffer)
  "Parses and returns a mime multipart form data block from MIME-STREAM upto BOUNDARY.
BUFFER is a line buffer."
  (declare (values query-name value baggage last-block-p))
  (with-header-values (content-disposition content-type) *headers*
    (destructuring-bind (&key name filename &allow-other-keys)
	(cdr content-disposition)
      (cond ((null name)
	     (if content-disposition
		 (error 'bad-multipart-form-data-posted  :url (server-url *server*)
			:format-string "Multipart Form: No name provided for the form element.")
		 (error 'bad-multipart-form-data-posted :url (server-url *server*)
			:format-string "Multipart Form: No Content-Disposition header.")))
	    ((or (null filename) (null-string-p filename))
	     (multiple-value-bind (raw-value last-block-p)
		 (mime-stream-decode-into-string-until-boundary mime-stream boundary nil buffer)
	       (values (%tokenize-form-query-keyword name) raw-value nil last-block-p)))
	    (t (multiple-value-bind (query-name directory)
		   (html2::file-upload-unpack-query-baggage name) 
		 (multiple-value-bind (destination copy-mode)
		     (file-upload-parse-filename filename directory content-type)
		   (multiple-value-bind (pathname last-block-p)
		       (mime-stream-decode-into-file-until-boundary mime-stream destination boundary copy-mode buffer)
		     ;; Record the pathname used to save the file as the value
		     ;; of the input element.  Include the original filename as
		     ;; an additional keyword value pair.
		     (values (%tokenize-form-query-keyword query-name)
			     pathname
			     `(:upload-filename ,filename :content-type ,content-type :copy-mode ,copy-mode )
			     last-block-p)))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.426")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defun parse-search-info-as-query-alist (string &optional (start 0) (end (length string)))
  "Parses the search component of a search URL according to URL encoding rules
for use with form submission via the GET method."
  (declare (values search-alist))
  (flet ((handle-bad-form-encoding (string start end)
	   (if *search-url-signal-bad-form-encoding*
	       (error 'search-url-bad-form-encoding :search-info (subseq string start end)
		      :url-string (default-url-string))
	       (return-from parse-search-info-as-query-alist (parse-search-info string start end)))))
    (declare (inline handle-bad-form-encoding))
    (unless (= start end)
      (with-fast-array-references ((string string string))
	(loop for s1 = start then (1+ (the fixnum e2))
	      while (< s1 end)
	      for e1 = (or (char-position #\= string s1 end)
			   (handle-bad-form-encoding string start end))
	      for s2 = (1+ (the fixnum e1))
	      for e2 = (or (char-position #\& string s2 end) end)
	      for keyword = (http::%tokenize-form-query-keyword string s1 e1)
	      for value = (unless (= s2 e2)
			    (string-unescape-special-chars string s2 e2))
	      collect `(,keyword ,value))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;IMAGE-MAPS.LISP.31")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun parse-cern-image-map-file (pathname)
  "Parses an CERN format file from pathname and returns an image map object."
  (flet ((tag-keyword (line start end)
	   (declare (fixnum start end))
	   (let ((e (+ start 3)))
	     (cond ((or (= start end)		;blank line
			(eql #\# (aref line start))	;comment line
			(> e end))		;short line
		    nil)
		   (t (nstring-upcase line :start start :end e)	;don't lose on case
		      (%TOKENIZE-FORM-QUERY-KEYWORD line start e)))))
	 (get-line (file-stream line-buffer &aux (start 0))
	   (multiple-value-bind (line error-p delimiter end)
	       (read-delimited-line file-stream '(#\Return #\Linefeed) nil line-buffer)
	     (declare (ignore delimiter))
	     (cond ((and line (not error-p))
		    (WITH-STRING-TRIM-BOUNDS (*white-space-chars* line start end)
		      (setf (fill-pointer line) end)
		      (values line start end)))
		   (t nil))))
	 (tag-value (tag line start end)
	   (case tag
	     (:def (parse-default line start end))
	     (:rec (parse-cern-rectangle line))
	     (:cir (parse-cern-circle line))
	     (:pol (parse-cern-polygon line))
	     (otherwise nil))))
    ;;(declare (inline tag-keyword tag-value get-line))
    (using-resource (line-buffer LINE-BUFFER HTTP:*LINE-BUFFER-SIZE*)
      (with-open-file (file-stream pathname :direction :input)
	(loop with default and line and start and end and tag and value
	      doing (multiple-value-setq (line start end)
		      (get-line file-stream line-buffer))
	      while line
	      do (setq tag (tag-keyword line start end)
		       value (tag-value tag line start end))
		 #+ignore (format t "~&~S~&Tag: ~S ~&Value: ~S" line tag value)
	      when (eq tag :def)
		do (setq default value)
	      else when value collect value into regions
	      finally (return (make-image-map :cern
					      :url-default (error-checked-default default nil pathname)
					      :region-list regions)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.501")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;; Old header reader
(define read-headers (stream &aux current-header line delimiter length error-p header-delimiter header-key multi-line-buffer)
  "Reads and assembles headers from STREAM."
  (declare (values header-alist))
  (labels ((clear-multi-line-buffer (header)
             (flet ((trim-p (char)
                      (member char '(#\space #\tab #\return #\linefeed) :test #'eql)))
	       (when (and header multi-line-buffer)
		 (let* ((val (%header-raw-value header))
			(lines `(,@val ,.(nreverse multi-line-buffer))))
		   (declare (dynamic-extent lines))
		   (setf (car (last val)) (concatenate-lines lines #'trim-p)
			 multi-line-buffer nil)))))
           (push-header-multi-line-string (string)
             (push string multi-line-buffer))
           (push-header-string (header string)
             (setf (%header-raw-value header) (nconc (%header-raw-value header) (list string))))
           (push-new-header-string (header string alist)
             (let ((hdr (assoc header alist)))
               (cond (hdr
                      (push-header-string (cdr hdr) string)
                      (values (cdr hdr) nil))
                     (t (values (allocate-header header-key (list string)) t)))))
           (get-header-value (string start end) ;handles no whitespace case and null value (losers)
             (let ((s (position-if-not* #'white-space-char-p string :start start :end end)))
               (if s (subseq string s length) ""))))
    (declare (inline clear-multi-line-buffer push-header-multi-line-string push-header-string
                     push-new-header-string get-header-value))
    delimiter                                   ;ignore
    (using-resource (line-buffer line-buffer *line-buffer-size*)
      (loop do (multiple-value-setq (line error-p delimiter length)
		 (read-delimited-line stream '(#\Linefeed #\return) nil line-buffer))
	    until (or error-p (blank-line-p line))
	    when (and (setq header-delimiter (if (white-space-char-p (aref line 0))
						 nil
						 (char-position #\: line 0 length)))
		      (setq header-key (%tokenize-header line 0 header-delimiter)))
	      do (clear-multi-line-buffer current-header)
	      and
	    when (multiple-value-bind (header new-p)
		     (push-new-header-string
		       header-key
		       (get-header-value line (1+ (the fixnum header-delimiter)) length)
		       header-alist)
		   (setq current-header header)
		   new-p)
	      collect (list* header-key current-header) into header-alist
		end
	    else do (push-header-multi-line-string (subseq line 0 length))
	    finally (clear-multi-line-buffer current-header)
		    (return-from read-headers header-alist)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.501")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod parse-header-buffer ((header-set header-set) &optional (start 0) (start-line-ends 0))
  (flet ((push-multi-line (header start end)
	   (let ((raw-value-position (%header-raw-value-position header)))
	     (header-position-push raw-value-position start end))))
    (declare (inline push-multi-line))
    (let ((buffer (%header-set-buffer header-set))
	  (line-ends (%header-set-line-ends header-set)))
      (%with-header-set-index (header-set)
	(with-fast-array-references ((buffer buffer string) (line-ends line-ends vector)
				     (index index vector) (headers headers vector))
	  (let ((index-fill-pointer (fill-pointer index))
		(index-size (array-total-size index))
		keyword current-header delim-pos)
	    (declare (type fixnum index-fill-pointer index-size))
	    (loop with room-p
		  for idx fixnum upfrom start-line-ends below (fill-pointer line-ends)
		  for s fixnum = start then e
		  for e fixnum = (aref line-ends idx)
		  ;;(format t "~&Parse: ") (write-string buffer t :start s :end e)
		  do (cond ((white-space-char-p (aref buffer s))
			    (when current-header
			      (push-multi-line current-header s e)))
			   ((and (setq delim-pos (char-position #\: buffer s e))
				 (setq keyword (%tokenize-header buffer s delim-pos)))
			    (setq s (1+ (the fixnum delim-pos)))	;advance pointer
			    (cond ((setq current-header (%%get-header-object keyword index headers))
				   (push-multi-line current-header s e))
				  ;; push a new header
				  (t (setq room-p (< index-fill-pointer index-size)
					   current-header (instantiate-buffered-header (and room-p index-fill-pointer) headers
										       keyword buffer s e))
				     ;; possibly grow indices
				     (unless room-p
				       (let ((n-size (floor (* (the fixnum index-size) *header-set-growth-factor*))))
					 (setf index (adjust-array index n-size :element-type t :initial-element nil)
					       headers (adjust-array headers n-size :element-type t :initial-element nil)
					       index-size (array-total-size index)
					       ;; update the stored pointer to these structures
					       (car index-ptr) index
					       (cdr index-ptr) headers)))
				     ;; push the new header
				     (setf (aref index index-fill-pointer) keyword
					   (aref headers index-fill-pointer) current-header
					   (fill-pointer index) (incf index-fill-pointer)
					   (fill-pointer headers) index-fill-pointer))))
			   (t nil)))))))))	;ignore undelimited headers

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.908")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(define parse-request (string start end url-buffer)
  "Parses an HTTP request string."
  (declare (values method url-string http-version-keyword)
           (fixnum end)
           (optimize (speed 3)))
  (let* ((e3 (if (and (not (zerop end)) (white-space-char-p (aref string (1- end))))
                 (%fast-position-if-not white-space-char-p string :start start :end (1- end) :from-end t)
                 end))
         (s1 (and (not (zerop e3)) (char-position #\space string start e3)))
         (s2 (and s1 (%fast-position-if-not white-space-char-p string :start s1 :end e3)))
         (e2 (and s2 (or (%fast-position-if white-space-char-p string :start s2 :end e3) e3)))
         (s3 (and e2 (1+ (the fixnum (or (%fast-position-if white-space-char-p string :start e2 :end e3 :from-end t) e2)))))
         (method (and s1 (%tokenize-header-keyword string 0 s1)))
         (version (and s3 (< s3 e3) (%tokenize-header-keyword string s3 e3)))
	 (url-length (and s2 e2 (- (the fixnum e2) (the fixnum s2))))
	 url-string)
    (when url-length
      ;; Ensure URL fits in buffer
      (when (> (the fixnum url-length) (the fixnum (array-total-size url-buffer)))
	(setq url-buffer (adjust-array url-buffer url-length :fill-pointer 0 :element-type *standard-character-type*)))
      ;; Copy the url into the URL buffer
      (copy-vector-portion string s2 e2  url-buffer 0 url-length)
      (setf (fill-pointer url-buffer) url-length)
      (setq url-string (url:canonicalize-url :http url-buffer 0 url-length t)))
    ;; Ensure that URLs arrive in canonical form.
    (values method url-string version)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.502")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun safe-header-value (header)
  "Returns the parsed value of HEADER signalling any errors parsing the header raw value.
Errors are automatically bug reported."
  (declare (values value error-p))
  (handler-case-if (not *debug-server*)
    (header-value header)
    (error (error)
	   (let ((error-type (type-of error)))
	     (report-bug *bug-http-server*
			 (format nil "HTTP Header Parsing Error: ~S" error-type)
			 "~&Error: ~S~&Header: ~A~&Header Raw Value: ~S~:[~;~&Error Report: ~:*~A~]"
			 error-type (header-keyword header) (header-raw-value header t)
			 (report-string error))
	     (return-from safe-header-value (values nil t))))))
;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.502")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;;;------------------------------------------------------------------- 
;;;
;;; PARSING & PRINTING CACHE CONTROL HEADERS
;;;

(defun parse-cache-control-header (string &optional (start 0) (end (length string))) 
  (labels ((parse-quoted-keyword-sequence (string &optional (start 0) (end (length string)))
	     (let ((open-pos (char-position #\" string start end))
		   (close-pos (char-position #\" string start end t)))
	       (parse-comma-separated-keywords string (1+ (the fixnum open-pos)) close-pos)))
           (parse-directive (string &optional (start 0) (end (length string)))
	     (with-string-trim-bounds (*white-space-chars* string start end)
	       (let* ((pos (%fast-position-if header-special-character-p string :start start :end end))
		      (directive (%tokenize-header-keyword string start (or pos end))))
		 (case directive
		   ((:no-store :only-if-cached :public :no-transform :must-revalidate :proxy-revalidate)
		    (list directive t))
		   ((:max-age :min-fresh)
		    (multiple-value-list
		      (parse-equal-sign-delimited-pair string start end #'parse-integer-header)))
		   (:max-stale
		     (if (and pos (char-position #\= string pos end))
			 (multiple-value-list
			   (parse-equal-sign-delimited-pair string start end #'parse-integer-header))
			 (list directive t)))
		   ((:no-cache :private)
		    (if (and pos (char-position #\= string pos end))
			(multiple-value-list
			  (parse-equal-sign-delimited-pair string start end #'parse-quoted-keyword-sequence))
			(list directive t)))
		   (t (if (and pos (char-position #\= string pos end))
			  (let ((open-pos (char-position #\" string start end)))
			    (if open-pos 
				(list directive (subseq string (1+ (the fixnum open-pos))
							(char-position #\" string start end t)))
				(multiple-value-list
				  (parse-equal-sign-delimited-pair string start end #'parse-keyword-header))))
			  (list directive t))))))))
    (with-fast-array-references ((string string string))
      (loop with escape and first = start
	    for idx fixnum upfrom start below end
	    for char = (aref string idx)
	    do (case char
		 (#\" (setq escape (not escape))))
	    when (and (not escape) (eql char #\,))
	      nconc (prog1 (parse-directive string first idx)
			   (setq first (1+ idx)))
		into result
	    finally (return (nconc result (parse-directive string first end)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.502")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun parse-keyword-header (string &optional (start 0) (end (length string)))
  (with-string-trim-bounds (*white-space-chars* string start end)
    (multiple-value-bind (keyword)
	(%tokenize-header-keyword string start end)
      keyword)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.502")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;; Consider changing the representation of qualities from floats to integers
;; if it makes sense for efficiency.  7/3/96 -- JCMa.
(defun parse-comma-separated-quality-pairs (string &optional (start 0) (end (length string)))
  (flet ((intern-quality-pair (string &optional (start 0) (end (length string)))
	   (let* ((pos (char-position #\; string start end t))
		  (q-pos (and pos (char-position #\= string (1+ (the fixnum pos)) end t)))
		  (key (%tokenize-header-keyword string start (or pos end)))
		  (val (if q-pos (parse-quality-value string (1+ (the fixnum q-pos)) end) 1)))
	     (list* key val))))
    (parse-comma-separated-header string start end #'intern-quality-pair)))



;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.506")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;;;------------------------------------------------------------------- 
;;;
;;; PARSING THE VIA HEADER
;;;

(defun parse-via-header (string &optional (start 0) (end (length string)))
  (labels ((protocol-defaulted-p (string start end)
	     (with-fast-array-references ((string string string))
	       (loop for idx upfrom start below end
		     for char = (aref string idx)
		     when (alpha-char-p char) 
		       return nil
		     when (and (eql #\/ char)	;handle numeric protocols
			       (< start idx))
		       return nil
		     finally (return t))))
	   (tokenize-defaulted-protocol (string start end)
	     (declare (fixnum start end))
	     (let* ((length (the fixnum (+ 5 (- end start))))
		    (temp (make-array length :element-type  *standard-character-type*)))
	       (declare (dynamic-extent temp))
	       (copy-vector-portion "HTTP/" 0 5 temp 0 5)
	       (copy-vector-portion string start end temp 5 length)
	       (%tokenize-header-keyword temp 0 length)))
	   (parse-host-spec (string start end)
	     (let ((colon (char-position #\: string start end t)))
	       (if colon
		   (list (subseq string start colon) (parse-integer string :start (1+ colon) :end end))
		   (list (subseq string start end)))))
	   (parse-comment (string start end)
	     (let ((s1 (char-position #\( string start end))
		   (e1 (char-position #\) string start end t)))
	       (list (subseq string (if s1 (1+ (the fixnum s1)) start) (or e1 end)))))
	   (parse-entry (string &optional (start 0) (end (length string)))
	     (with-string-trim-bounds (*white-space-chars* string start end)
	       (let* ((e1 (position-if* #'white-space-char-p string :start start :end end))
		      (s2 (position-if-not* #'white-space-char-p string :start (1+ (the fixnum e1)) :end end))
		      (e2 (or (position-if* #'white-space-char-p string :start s2 :end end) end))
		      (s3 (when (and e2 (< e2 end))
			    (position-if-not* #'white-space-char-p string :start (1+ (the fixnum e2)) :end end))))
		 (list* (if (protocol-defaulted-p string start e1)
			   (tokenize-defaulted-protocol string start e1)
			   (%tokenize-header-keyword string start e1))
		       (parse-host-spec string s2 e2) 
		       (when s3 (parse-comment string s3 end)))))))
    (declare (inline protocol-defaulted-p parse-host-spec parse-comment))
    (parse-comma-separated-header string start end #'parse-entry)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.501")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header-keywords "bytes" "none")

(define-header-keywords "realm" "nonce" "username" "uri" "response" "digest" "algorithm" "opaque"
			"basic" "digest")

(define-header-keywords "nextnonce" "digest")

(define-header-keywords "no-cache" "no-store" "max-age" "max-stale" "min-fresh" "only-if-cached" "public" 
                        "private" "no-transform" "must-revalidate" "proxy-revalidate")

(define-header-keywords "close" "Keep-Alive")

(define-header-keywords "100-continue")

(define-header-keywords "no-cache")

(define-header-keywords  "delete" "get" "head" "options" "post" "put" "trace")

(define-header-keywords "chunked" "identity" "deflate")

(define-header-keywords "realm" "domain" "nonce" "opaque" "stale" "algorithm")

(define-header-keywords "md5" "sha")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.501")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header-type :header (:header)
  :parse-function parse-standard-header
  :print-function print-standard-header)

(define-header-type :keyword-header (:header)
  :parse-function parse-keyword-header
  :print-function print-keyword-header) 

(define-header-type :integer-header (:header)
  :parse-function parse-integer-header
  :print-function print-integer-header)

(define-header-type :version-header (:header)
  :parse-function parse-version-header
  :print-function print-version-header)

(define-header-type :date-header (:integer-header)
  :parse-function parse-date-header
  :print-function print-date-header
  :atomic-value-reducer first)

(define-header-type :authentication-header (:header))

(define-header-type :comma-separated-header (:header)
  :parse-function parse-comma-separated-header
  :print-function print-comma-separated-header
  :print-series-predicate atomic-valued-header-series-value-p)

(define-header-type :content-type-header (:header)
  :parse-function parse-mime-content-type-header
  :print-function print-mime-content-type-header)

;; need to add singleton parsing option for sequence type headers.
(define-header-type :content-type-sequence-header (:header)
  :parse-function parse-mime-content-type-sequence-header
  :print-function print-mime-content-type-sequence-header
  :print-series-predicate list-valued-header-series-value-p)

(define-header-type :keyword-sequence-header (:comma-separated-header)
  :parse-function parse-comma-separated-keywords
  :print-function print-comma-separated-keywords)

(define-header-type :mail-address-sequence-header (:comma-separated-header)
  :parse-function parse-mail-addresses
  :print-function print-mail-addresses)

(define-header-type :quality-pair-sequence-header (:comma-separated-header)
  :parse-function parse-comma-separated-quality-pairs
  :print-function print-comma-separated-quality-pairs
  :print-series-predicate list-valued-header-series-value-p)

(define-header-type :via-header (:header)
  :parse-function parse-via-header
  :print-function print-via-header)

(define-header-type :cache-control-header (:header)
  :parse-function parse-cache-control-header
  :print-function print-cache-control-header
  :print-series-predicate atomic-valued-header-series-value-p)	; allow for multiple cache-control headers

(define-header-type :entity-tag-header (:header)
  :parse-function parse-entity-tag-header
  :print-function print-entity-tag-header)

(define-header-type :entity-tag-sequence-header (:entity-tag-header)
  :parse-function parse-entity-tag-sequence-header
  :print-function print-entity-tag-sequence-header
  :print-series-predicate atomic-valued-header-series-value-p)

(define-header :accept
               (:content-type-sequence-header
                :request)
  :print-string "Accept")

(define-header :accept-charset
               (:quality-pair-sequence-header :request)
  :print-string "Accept-Charset")

(define-header :accept-encoding
               (:keyword-sequence-header :request)
  :print-string "Accept-Encoding")

(define-header :accept-language
               (:quality-pair-sequence-header :request)
  :print-string "Accept-Language")

(define-header :accept-ranges
               (:keyword-header :request)       ;missing from 1.1 spec p29
  :print-string "Accept-Ranges")

(define-header :allow
               (:keyword-sequence-header :entity)
  :print-string "Allow")

(define-header :authorization                   ;RFC 2069
               (:authentication-header :request)
  :print-string "Authorization"
  :parse-function 'parse-authorization-header
  :print-function 'print-authorization-header)

(define-header :authentication-info             ;RFC 2069
               (:authentication-header :response)
  :print-string "Authentication-Info"
  :parse-function 'parse-authentication-info-header
  :print-function 'print-authentication-info-header)

(define-header :cache-control
               (:cache-control-header :general)
  :print-string "Cache-Control")

(define-header :connection
               (:keyword-sequence-header :general)
  :print-string "Connection")

(define-header :content-base
               (:header :entity)
  :print-string "Content-Base")

(define-header :content-encoding
               (:keyword-header :entity)
  :print-string "Content-Encoding")

(define-header :content-disposition             ;not in 1.1
               (:header :entity)
  :print-string "Content-Disposition"
  :parse-function 'parse-mime-content-disposition-header
  :print-function 'print-mime-content-disposition-header)

(define-header :content-id                      ;not in 1.1
               (:header :entity)
  :print-string "Content-ID")

(define-header :content-language
               (:keyword-sequence-header :entity)
  :print-string "Content-Language")

(define-header :content-length
               (:integer-header :entity)
  :print-string "Content-length")

(define-header :content-location
               (:header :entity)
  :print-string "Content-Location"
  :parse-function 'parse-location-header
  :print-function 'print-location-header)

(define-header :content-md5
               (:header :entity)
  :print-string "Content-MD5")

(define-header :content-range
               (:header :entity)
  :print-string "Content-Range"
  :parse-function 'parse-content-range-header
  :print-function 'print-content-range-header)

;; This is the MIME version, not the HTTP 1.1 Transfer-Encoding
(define-header :content-transfer-encoding
               (:keyword-header :entity)        ;not in 1.1
  :print-string "Content-Transfer-Encoding")

(define-header :content-type
               (:content-type-header :entity)
  :print-string "Content-type")

(define-header :content-version
               (:version-header :entity)        ;not in 1.1 spec p,31
  :print-string "Content-Version")

(define-header :date
               (:date-header :general)
  :print-string "Date"
  :atomic-value-reducer 'header-value-max
  :default 'gmt-time)

(define-header :derived-from
               (:version-header :entity)
  :print-string "Derived-From")

(define-header :etag
               (:entity-tag-header :entity)
  :print-string "ETag")

(define-header :expect
               (:header :request)
  :print-string "Expect"
  :print-function 'print-expect-header
  :parse-function 'parse-expect-header)

(define-header :expires
               (:date-header :entity)
  :print-string "Expires"
  :parse-function 'parse-expires-header
  :print-function 'print-expires-header
  :atomic-value-reducer 'header-value-min)

;; value:: (origin proxy &optional port proxy-product)
(define-header :forwarded
               (:header :response)
  :print-string "Forwarded"
  :parse-function 'parse-forwarded-header
  :print-function 'print-forwarded-header)

(define-header :from
               (:mail-address-sequence-header :request)
  :print-string "From")

(define-header :host
               (:header :request)
  :print-string "Host"
  :parse-function 'parse-host-header
  :print-function 'print-host-header)

(define-header :if-match
               (:entity-tag-sequence-header :request)
  :print-string "If-Match")

(define-header :if-modified-since
               (:date-header :request)
  :print-string "If-Modified-Since"
  :atomic-value-reducer 'header-value-min)

(define-header :if-none-match
               (:entity-tag-sequence-header :request)
  :print-string "If-None-Match")

(define-header :if-unmodified-since
               (:date-header :request)
  :print-string "If-Unmodified-Since"
  :atomic-value-reducer 'header-value-min)

;; deprecated 1.0 extension header  6/29/96 -- JCMa.
(define-header :keep-alive
               (:comma-separated-header :request)
  :print-string "Keep-Alive"
  :parse-function 'parse-keep-alive-header
  :print-function 'print-keep-alive-header)

(define-header :last-modified
               (:date-header :entity)
  :print-string "Last-Modified"
  :atomic-value-reducer 'header-value-min)

(define-header :location (:header :response)
  :print-string "Location"
  :parse-function 'parse-location-header
  :print-function 'print-location-header)

(define-header :max-forwards
               (:integer-header :request)
  :print-string "Max-Forwards")

(define-header :method
               (:comma-separated-header :unknown)
  :print-string "Method")

(define-header :mime-version
               (:header :entity)
  :print-string "MIME-version")

(define-header :pragma
               (:keyword-header :general)
  :print-string "Pragma")

(define-header-keywords "no-cache")

(define-header :proxy-authenticate                ;RFC 2069
               (:authentication-header :response)
  :print-string "Proxy-Authenticate"
  :parse-function 'parse-www-authenticate-header
  :print-function 'print-www-authenticate-header)

(define-header :proxy-authorization                   ;RFC 2069
               (:authentication-header :request)
  :print-string "Proxy-Authorization"
  :parse-function 'parse-authorization-header
  :print-function 'print-authorization-header)

(define-header :proxy-connection		;deprecated 1.0 Extension
               (:keyword-sequence-header :general)
  :print-string "Proxy-Connection")

(define-header :public
               (:keyword-sequence-header :response)
  :print-string "Public")

(define-header :range
               (:header :request)
  :print-string "Range"
  :parse-function 'parse-range-header
  :print-function 'print-range-header)

(define-header :referer
               (:header :request)
  :print-string "Referer")

(define-header :Server 
               (:header :response)
  :print-string "Server"
  :default *server-version*)

(define-header :te
               (:header :request)
  :print-string "TE"
  :print-function 'print-comma-separated-quality-pair-or-tokens
  :parse-function 'parse-comma-separated-quality-pairs)

(define-header-keywords "chunked" "identity" "deflate")

(define-header :trailer
               (:keyword-sequence-header :general)
  :print-string "Trailer")

(define-header :transfer-encoding
               (:keyword-header :general)
  :print-string "Transfer-Encoding")

(define-header :upgrade
               (:keyword-header :unknown)
  :print-string "Upgrade")

;; deprecated in HTTP 1.1
(define-header :uri (:header :entity)
  :print-string "URI"
  :parse-function 'parse-uri-header
  :print-function 'print-uri-header)

(define-header :User-Agent (:header :request)
  :print-string "User-Agent") 

;; old name replaced by content-version for 1.1
(define-header :version
               (:integer-header :entity)
  :print-string "Version")

(define-header :via
               (:via-header :general)
  :print-string "Via")

(define-header :www-authenticate                ;RFC 2069
               (:authentication-header :response)
  :print-string "WWW-Authenticate"
  :parse-function 'parse-www-authenticate-header
  :print-function 'print-www-authenticate-header)

(define-header :window-target 
               (:header :response)
  :print-string "Window-Target")

(define-header :cookie (:header :request)
  :print-string "Cookie"
  :parse-function 'parse-cookie-header
  :print-function 'print-cookie-header)

(define-header :set-cookie (:header :response)
  :print-string "Set-Cookie"
  :parse-function 'parse-set-cookie-header
  :print-function 'print-set-cookie-header)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;REPORT.LISP.185")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

;; Catch any errors reporting errors.  Errors that re-enter this code
;; are sure losers attempting to resignal within the process of
;; signalling the original error.  These cases are detected and handled
;; in a safe manner. 9/22/99 -- JCMa.
(defmethod report-status :around ((condition http-condition) stream)
  (flet ((report-rentrant-error (primary-error secondary-error)
           (let ((secondary-error-type (type-of secondary-error)))
             (report-bug *bug-http-server*
                         (format nil "REPORT-STATUS Re-entrant Error: ~S" secondary-error-type)
                         "~:[~;~&Log: ~:*~S~]~&Primary Error: ~S~:[~;~&Primary Error Report: ~:*~A~]~
                          ~&Secondary Error: ~S~:[~;~&Secondary Error Report: ~:*~A~]~:[~;~&Backtrace: ~:*~A~]"
                         (when http:*server* (write-extended-common-logfile-entry http:*server* nil))
                         (type-of primary-error) (report-string primary-error) (type-of secondary-error) (report-string secondary-error)
                         (when *stack-backtraces-in-bug-reports* (stack-backtrace-string condition))))))
    (cond (*report-status-condition*
           (report-rentrant-error *report-status-condition* condition))
          (t (let ((*report-status-condition* condition))
               (handler-case-if (not *debug-server*)
		  (with-text-stream (stream :output)	;in case we're in binary mode
		    (call-next-method condition stream))
                 (network-error () nil)         ;no need to report errors telling the user he lost
                 (error (error) (bug-report-error error))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.197")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

;; this is signalled when a data source provides less data than expected by content-length
(define-condition insufficient-data
                  (bad-gateway end-of-file)
  ((reason :initform "Insuffient Data")))

