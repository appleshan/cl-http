;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.124
;;; Reason: Function (CLOS:METHOD HTTP:EXPORT-URL (URL:HTTP-MINIMUM-OBJECT (EQL :SHTML-FILE))):  fix parent problem.
;;; Function (CLOS:METHOD HTTP::%AUTHENTICATE-USER (T T T T)):  method congruency.
;;; Function (CLOS:METHOD HTTP::MIME-STREAM-DECODE-UNTIL-BOUNDARY (T T T (EQL :BINARY))):  remove compiler warning.
;;; 
;;; Improve efficiency parsing CERN and NCSA image map files on export.
;;; 
;;; Function HTTP::PARSE-DESTINATION:  revert
;;; Function HTTP::WITH-PARSED-NCSA-LINE:  -
;;; Function HTTP::PARSE-NCSA-POINT:  -
;;; Function HTTP::PARSE-NCSA-RECTANGLE:  -
;;; Function HTTP::PARSE-NCSA-CIRCLE:  -
;;; Function HTTP::PARSE-NCSA-OVAL:  -
;;; Function HTTP::PARSE-NCSA-POLYGON:  -
;;; Function HTTP::PARSE-DEFAULT:  -
;;; Function HTTP::PARSE-NCSA-IMAGE-MAP-FILE:  -
;;; Function HTTP::PARSE-NCSA-IMAGE-MAP-FILE:  -
;;; Function HTTP::PARSE-CERN-RECTANGLE:  -
;;; Function HTTP::PARSE-CERN-CIRCLE:  -
;;; Function HTTP::PARSE-CERN-POLYGON:  -
;;; Written by JCMa, 4/23/01 17:41:46
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.123,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 54.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.26, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Images Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; HTTP Proxy Server 6.20, HTTP Client Substrate 4.9, Statice Server 466.2,
;;; Images 431.2, Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Image Substrate 440.4, Jcma 43, HTTP Client 50.6,
;;; W4 Constraint-Guide Web Walker 45.10, W4 Examples 15.0,
;;; Experimental CL-HTTP CLIM User Interface 1.0, CL-HTTP Documentation 3.0,
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
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).

(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;SERVER.LISP.907"
  "HTTP:SERVER;AUTHENTICATION.LISP.154"
  "HTTP:SERVER;UTILS.LISP.493"
  "HTTP:SERVER;IMAGE-MAPS.LISP.30"
  "HTTP:SERVER;IMAGE-MAPS.LISP.31")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.907")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod export-url ((url url:http-minimum-object) (translation (eql :shtml-file)) &rest args)
  (apply #'export-url (url:initialize-specialization url 'url:http-template-object args) translation args))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;AUTHENTICATION.LISP.154")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod %authenticate-user (realm authorization method case)
  (declare (ignore realm authorization method case))
  nil)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.493")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmethod mime-stream-decode-until-boundary (mime-stream to-stream boundary (mode (eql :binary)) &optional buffer
							  &aux (max-bytes *mime-multipart-block-maximum-size*) (total-bytes 0))
  (declare (values last-block-p)
	   (ignore buffer)
	   (integer total-bytes))
  (macrolet ((binary-char-code (char)
	       (typecase char
		 (#.*standard-character-type* #+Genera (si:ascii-code char) #-Genera (char-code char))
		 (t #+Genera `(si:ascii-code ,char) #-Genera `(char-code ,char)))))
    (labels ((fill-boundary-array (array boundary boundary-length)
	       (with-fast-array-references ((array array vector)
					    (boundary boundary string))
		 (loop initially (setf (aref array 0) (binary-char-code #\Return)
				       (aref array 1) (binary-char-code #\Linefeed) )
		       for i fixnum below boundary-length
		       do (setf (aref array (+ 4 i)) (char-code (aref boundary i))))))
	     (protected-write-byte (byte stream)
	       (and max-bytes
		    (> (incf total-bytes) (the integer max-bytes))
		    (signal-mime-multipart-block-maximum-size-exceeded total-bytes max-bytes))
	       (write-byte byte stream))
	     (write-array-portion (to-stream array n &rest extra-bytes)
	       (declare (dynamic-extent extra-bytes))
	       (with-fast-array-references ((array array vector))
		 (loop for idx fixnum upfrom 0 below n
		       do (protected-write-byte (aref array idx) to-stream)))
	       (dolist (byte extra-bytes)
		 (protected-write-byte byte to-stream)))
	     (check-for-mime-boundry (mime-stream to-stream boundary-array boundary-array-length)
	       (let ((last 1))
		 ;; start with 1 because the CR matched to get us here!
		 (with-fast-array-references ((boundary-array boundary-array vector))
		   (loop for idx fixnum upfrom 1 below boundary-array-length
			 for next-byte = (read-byte mime-stream)
			 while (eql next-byte (aref boundary-array idx))
			 do (setq last idx)
			 finally (when (< last (1- boundary-array-length))
				   (write-array-portion to-stream boundary-array last next-byte)
				   (return-from check-for-mime-boundry nil))))
		 (let ((byte1 (read-byte mime-stream))
		       (byte2 (read-byte mime-stream)) )
		   (cond ((and (= byte1 (binary-char-code #\-)) (= byte2 (binary-char-code #\-)))
			  (setq byte1 (read-byte mime-stream)
				byte2 (read-byte mime-stream))
			  (cond ((and (eql byte1 (binary-char-code #\Return)) (eql byte2 (binary-char-code #\Linefeed)))
				 (return-from mime-stream-decode-until-boundary t))
				(t (write-array-portion to-stream boundary-array boundary-array-length
							(binary-char-code #\-) (binary-char-code #\-) byte1 byte2))))
			 ((and (eql byte1 (binary-char-code #\Return)) (eql byte2 (binary-char-code #\Linefeed)))
			  (return-from mime-stream-decode-until-boundary nil))
			 (t (write-array-portion to-stream boundary-array boundary-array-length byte1 byte2)))))))
      (declare (inline fill-boundary-array)
	       (dynamic-extent #'protected-write-byte))
      (let* ((boundary-length (length boundary) )
	     (boundary-array-length (+ 4 boundary-length)) ;; len + CR + LF + 2 #\-'s
	     (boundary-array  (make-array boundary-array-length :initial-element #.(char-code #\-))))
	(declare (dynamic-extent boundary-array)
		 (fixnum boundary-length boundary-array-length))
	(with-binary-stream (mime-stream :input)
	  (loop initially (fill-boundary-array boundary-array boundary boundary-length)
		for byte = (read-byte mime-stream)
		do (if (eql byte (binary-char-code #\Return))
		       (check-for-mime-boundry mime-stream to-stream boundary-array boundary-array-length)
		       (protected-write-byte byte to-stream))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;IMAGE-MAPS.LISP.30")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;;;------------------------------------------------------------------- 
;;;
;;; PARSING UTILITIES
;;;

;; This needs to return an exported url because it is too messy to
;; arrange on the-fly exports from an image map object.   9/2/95 -- JCMa.
(defun parse-destination (raw-text)
  (flet ((intern-exported-url (destination)
           (let* ((url-string (%merge-url destination (local-context)))
                  (url (intern-url url-string :if-does-not-exist :create)))
             (cond
               ((not (url:local-url-p url)))    ;remote url. 
               ((and url (translation-method url)))     ; Proceed if already exported.
               ((and *auto-export* (setq url (auto-export-pathname-url url-string))))
               (t (error "The destination URL, ~A, was not already exported and could not be auto-exported."
                         url-string)))
             url)))
    (declare (inline intern-exported-url))
    (let ((destination (string-left-trim "0123456789,() " raw-text)))
      (if (null-string-p destination)
          nil
          (intern-exported-url destination)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;IMAGE-MAPS.LISP.31")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun parse-default (line  &optional (start 0) (end (length line)))
  (flet ((trim-default (line start end)
           (let* ((start-key (string-search "def" line 0 3 start end))
                  (pos1 (or (position-if #'white-space-char-p line :start start-key :end end) start)))
             (cond ((= pos1 start)
		    (subseq line pos1 end))
		   (t (let ((pos2 (position-if-not #'white-space-char-p line :start pos1 :end end)))
			(subseq line pos2 end)))))))
    (declare (dynamic-extent #'trim-default))
    (let ((destination (trim-default line start end)))
      (parse-destination destination))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;IMAGE-MAPS.LISP.31")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmacro with-parsed-cern-line ((key line &key (line-offset 0) (line-end nil line-end-supplied-p)
				      (url-var 'url-string) (raw-coordinates-var 'raw-coordinates))
                                 &body body)
  `(let* ((end-key ,(typecase key (string `(length ,key))(t `(length ,key))))
	  (end ,(if line-end-supplied-p line-end `(length ,line)))
          (start (string-search ,key ,line 0 end-key ,line-offset end))
          (end-url (position-if-not #'white-space-char-p ,line :start start :end end :from-end t))
          (end-coord (position-if #'white-space-char-p ,line :start start :end end-url :from-end t))
          (,url-var (subseq ,line (1+ end-coord) (1+ end-url)))
          (,raw-coordinates-var (subseq ,line start end-coord)))
     (declare (fixnum key-len line-len start end-url end-coord))
     ,@body))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;IMAGE-MAPS.LISP.31")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun parse-cern-rectangle (line &optional (start 0) (end (length line)))
  (with-parsed-cern-line ("rect" line :line-offset start :line-end end)
    (destructuring-bind (first . rest) (parse-cern-coords raw-coordinates)
      (make-instance 'region
                     :destination (parse-destination url-string)
                     :bounding-shape (make-rectangle first (car rest))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;IMAGE-MAPS.LISP.31")
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
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;IMAGE-MAPS.LISP.31")
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
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;IMAGE-MAPS.LISP.31")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmacro with-parsed-ncsa-line ((key line &key (line-offset 0) (line-end nil line-end-supplied-p)
				      (url-var 'url-string) (raw-coordinates-var 'raw-coordinates))
                                 &body body)
  `(let* ((end-key ,(typecase key (string (length key))(t `(length ,key))))
          (line-len ,(if line-end-supplied-p line-end `(length ,line)))
          (start (string-search ,key ,line 0 end-key ,line-offset line-len))
          (end-tag (position-if #'white-space-char-p ,line :start (the fixnum (+ start end-key)) :start start :end line-len))
          (start-url (position-if-not #'white-space-char-p ,line :start end-tag :end line-len))
          (end-url (position-if #'white-space-char-p ,line :start start-url :end line-len))
          (,url-var (subseq ,line start-url end-url))
          (,raw-coordinates-var (subseq ,line end-url line-len)))
     (declare (fixnum line-len start end-tag start-url end-url end-key))
     ,@body))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;IMAGE-MAPS.LISP.31")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(PROGN
(defun parse-ncsa-point (line &optional (start 0) (end (length line)))
  (with-parsed-ncsa-line ("point" line :line-offset start :line-end end)
    (make-instance 'region
                   :destination (parse-destination url-string)
                   :bounding-shape (car (parse-ncsa-coords raw-coordinates)))))

(defun parse-ncsa-rectangle (line &optional (start 0) (end (length line)))
  (with-parsed-ncsa-line ("rect" line :line-offset start :line-end end)
    (destructuring-bind (first . rest) (parse-ncsa-coords raw-coordinates)
      (make-instance 'region
                     :destination (parse-destination url-string)
                     :bounding-shape (make-rectangle first (car rest))))))

(defun parse-ncsa-circle (line &optional (start 0) (end (length line)))
  (with-parsed-ncsa-line ("circ" line :line-offset start :line-end end)
    (destructuring-bind (first . rest) (parse-ncsa-coords raw-coordinates)
      (make-instance 'region
                     :destination (parse-destination url-string)
                     :bounding-shape (make-circle first (round (distance-between first (car rest))))))))

(defun parse-ncsa-oval (line &optional (start 0) (end (length line)))
  (with-parsed-ncsa-line ("oval" line :line-offset start :line-end end)
    (destructuring-bind (first . rest) (parse-ncsa-coords raw-coordinates)
      (make-instance 'region
                     :destination (parse-destination url-string)
                     :bounding-shape (make-oval first (car rest))))))

(defun parse-ncsa-polygon (line &optional (start 0) (end (length line)))
  (with-parsed-ncsa-line ("poly" line :line-offset start :line-end end)
    (let* ((point-list (parse-ncsa-coords raw-coordinates))
           (last (last point-list)))
      (unless (point-equal-p (first point-list) (first last))
        (setf last `(,.last ,(first point-list))))
      (make-instance 'region
                     :destination (parse-destination url-string)
                     :bounding-shape (make-polygon point-list)))))
)

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

