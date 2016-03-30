;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.196
;;; Reason: DEFINE-CONDITION HTTP::FILE-UPLOAD-MAXIMUM-SIZE-EXCEEDED: http;server;http-conditions.lisp
;;; Export http::query-baggage.
;;; Function HTML2::FILE-UPLOAD-CLEAR-DIRECTORY-INDEX:  New.
;;; Function (CLOS:METHOD MD5::MD5-ENCODE-STREAM (T T)):  -
;;; Function MD5:MAKE-MD5-BUFFER-FILLER-FROM-READER-FUNCTION:  -
;;; Variable HTTP::*UNSAFE-CHARACTERS-FOR-PATHNAMES*:  -
;;; Function HTTP::ENSURE-SAFE-PATHNAME-COMPONENT:  new
;;; Function HTTP::FILE-UPLOAD-PARSE-FILENAME:  increase robustness.
;;; Written by jcma, 7/11/05 14:42:10
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
;;; HTTP Server 70.195, Showable Procedures 36.3, Binary Tree 34.0,
;;; Experimental W3 Presentation System 8.1, CL-HTTP Server Interface 54.0,
;;; HTTP Proxy Server 6.34, HTTP Client Substrate 4.23,
;;; W4 Constraint-Guide Web Walker 45.13, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.18, DEC OSF/1 V4.0 (Rev. 110),
;;; 1728x1003 24-bit TRUE-COLOR X Screen FUJI:1.0 with 224 Genera fonts (The Olivetti & Oracle Research Laboratory R3323),
;;; Machine serial number -2142637960,
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7),
;;; Get Xauthority pathname from user namespace object. (from W:>jcma>fixes>xauthority-pathname.lisp.2),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;HTTP-CONDITIONS.LISP.207"
  "HTTP:SERVER;PACKAGE.LISP.488"
  "HTTP:SERVER;HTML2.LISP.305"
  "HTTP:SERVER;MD5.LISP.67"
  "HTTP:SERVER;UTILS.LISP.551"
  "HTTP:SERVER;SERVER.LISP.935")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.207")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition file-upload-maximum-size-exceeded
                  (request-entity-too-large bad-multipart-form-data-posted)
  ((reason :initform "Maximum Upload File Size Exceeded")))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;PACKAGE.LISP.488")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: CL-USER; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (:load-toplevel)
(export (intern "QUERY-BAGGAGE" :http) :http))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.305")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

(defun file-upload-clear-directory-index ()
  (www-utils:with-lock-held (*file-upload-directory-index-lock* :write "File Upload index")
    (clrhash *file-upload-directory-index-table*)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;MD5.LISP.67")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ansi-common-lisp; Package: (md5 use future-common-lisp :colon-mode :external); Base: 10 -*-")

;; this should be specialized by different platforms to perform this operation
;; in a buffer oriented manner.   9/30/96 -- JCMa.
(defmethod md5-encode-stream (stream element-type &optional start limit)
  (when start (file-position stream start))
  (let ((bytes-to-read (when (and limit start) (- (the fixnum limit) (the fixnum start))))
	(bytes-already-read 0))
    (declare (fixnum bytes-already-read))
    (flet ((get-byte-from-char ()
	     (let ((char (read-char stream nil nil)))
	       (and char (character-to-byte char))))
	   (get-byte ()
	     (read-byte stream nil nil))
	   (limited-get-byte-from-char ()
	     (let ((char (when (< bytes-already-read (the fixnum bytes-to-read))
			   (prog1
			     (read-char stream nil nil)
			     (incf bytes-already-read)))))
	       (and char (character-to-byte char))))
	   (limited-get-byte ()
	     (when (< bytes-already-read (the fixnum bytes-to-read))
	       (prog1
		 (read-byte stream nil nil)
		 (incf bytes-already-read)))))
      (declare (dynamic-extent #'get-byte-from-char #'get-byte #'limited-get-byte-from-char #'limited-get-byte))
      (let ((filler-fctn (make-md5-buffer-filler-from-reader-function
			   (if (subtypep element-type 'character) 
			       (if bytes-to-read
				   #'limited-get-byte-from-char
				   #'get-byte-from-char)
			       (if bytes-to-read
				   #'limited-get-byte
				   #'get-byte)))))
	(declare (dynamic-extent filler-fctn))
	(md5-encode filler-fctn)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;MD5.LISP.67")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ansi-common-lisp; Package: (md5 use future-common-lisp :colon-mode :external); Base: 10 -*-")

(defun make-md5-buffer-filler-from-reader-function (reader-function)
  "Returns a function which can be used as the GET-DATA-FUNCTION argument to MD5-ENCODE.
The argument is a function of no arguments, which returns sucessive 8 bit bytes from 
the source to be encoded, or NIL when the source is exhausted"
  (let ((state :data)
        (count 0)
        ;; the following is used to indicate if the first (#80) pad byte was
        ;; written when the index i was 13:
        (flag13 nil))
    (declare (fixnum count))
    (flet ((fill-md5-buffer (buffer)
             (dotimes (i 16 t)
               (declare (fixnum i))
               ;; I is which 32 bit word of buffer to write to.
               (flet ((gb ()
                        (ecase state
                          (:done (return-from fill-md5-buffer nil))
                          (:data
                            (let ((byte (funcall reader-function)))
                              (if byte
                                  (progn (incf count)
                                         byte)
                                  (progn (setq state :must-pad)
                                         (when (= i 13)
                                           (setq flag13 1))
                                         #x80))))       ;first pad byte
                          ;; If we start writing the padding during the 14th
                          ;; word, we must pad the entire buffer and write
                          ;; the length in the next one.
                          (:must-pad
                            ;; this takes care of case when #x80 is the
                            ;; last byte written when i=13, and the next
                            ;; byte is first byte of i=14, in which case
                            ;; length should be written, not another full 
                            ;; buffer of zeroes
                            (when (and (= i 14) flag13)
                              (setq state :pad))
                            (unless (= i 14)
                              (setq state :pad)
                              (setq flag13 nil))
                            0)
                          (:pad
                            (if (= i 14)
                                (multiple-value-bind (md5-length64-hi md5-length64-lo)
                                    (md5-length64 (* 8 count))
                                  (setf (aref buffer 14) md5-length64-lo)
                                  (setf (aref buffer 15) md5-length64-hi)
                                  (setq state :done)
                                  (return-from fill-md5-buffer t))
                                0)))))
                 ;; RFC1211:  "a sequence of bytes can be interpreted as a sequence
                 ;; of 32-bit words, where each consecutive group of four bytes is
                 ;; interpreted as a word with the low-order (least significant)
                 ;; byte given first."
                 (let ((b0 (gb))
                       (b1 (gb))
                       (b2 (gb))
                       (b3 (gb)))
                   (setf (aref buffer i) (#+genera si:%logdpb #-genera dpb b3 (byte 8 24)
					  (dpb b2 (byte 8 16)
					       (dpb b1 (byte 8 8)
						    b0)))))))))
      #'fill-md5-buffer)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.551")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defparameter *unsafe-characters-for-pathnames* '(#\space #\. #\~ #\' #\` #\_ #\, #\? #\; #\: #\( #\) #\|
						  #+LispWorks #\No-Break-Space	;weird char from Safari
						  )
  "Characters that are unsafe in pathname components.")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.551")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defun ensure-safe-pathname-component (string &optional (start 0) (end (length string))
                                              &aux (unsafe-chars *unsafe-characters-for-pathnames*))
  "Removes characters from STRING that  are unsafe in pathname components."
  (declare (values safe-string new-p)
           (fixnum start end))
  (flet ((unsafe-char-p (char)
           (member char unsafe-chars)))
    (declare (inline unsafe-char-p)) 
    (cond ((http::%fast-position-if unsafe-char-p string :start start :end end)
           (with-string-trim-bounds (unsafe-chars string start end)
             (with-fast-array-references ((string string string))
               (loop with prev-dash
                     for idx fixnum upfrom start below end
                     for char = (aref string idx)
                     unless (or (unsafe-char-p char)
                                (if (eql char #\-) prev-dash (and prev-dash (setq prev-dash nil))))
		       collect char into result
                     else 
		       unless prev-dash
			 collect #\- into result
			 and do (setq prev-dash t)
                     finally (return (values (coerce result 'string) t))))))
          ((and (zerop start) (= end (length string))) string)
          (t (values (subseq string start end) t)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.935")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

;; coerce _ to - due to error in LW 4.3.7
;; (make-pathname :defaults #p"http:uploads;" :name "roctrip_millauF" :TYPE "mov")
(defun file-upload-parse-filename (filename directory content-type)
  (flet ((strip-directory-info (string)
	   (loop with end fixnum = (length string)
		 with last-idx fixnum = (1- end)
		 for idx fixnum downfrom last-idx to 0
		 when (char-position (aref string idx) "\\/:>" 0 4)	;directory delimiters
		   return (if (= idx last-idx)
			      (error 'bad-multipart-form-data-posted
				     :format-string "File Upload: Bad file name, ~S."
				     :format-args (list string) :url (server-url *server*))
			      (subseq filename (1+ idx) end))
		 finally (return filename)))
	 (trim-characters (string trim-chars)
           (flet ((massage-chars (string)
                    (when string (nsubstitute #\- #\_ string))))	;lw 4.3 chokes on _
             (declare (inline massage-chars))
             (when string
               (let* ((start 0)
                      (end (length string))
                      (chars (- end start))
                      n-chars)
                 (declare (fixnum start end))
                 (unless (zerop chars)
                   (massage-chars
		     ;; removes version numbers and random emacs ~'s
		     (with-string-trim-bounds (trim-chars string start end)
		       (setq n-chars (- end start))
		       (cond ((= chars (setq n-chars (- end start)))
			      string)
			     ((zerop n-chars) nil)
			     (t (subseq string start end)))))))))))
    (let* ((path (pathname (strip-directory-info filename)))
           (path-name (pathname-name path))
	   (p-name (or (ensure-safe-pathname-component path-name 0 (min (length path-name) *file-upload-default-pathname-name-maximum-size*))
		       (file-upload-generate-file-name)))
	   (p-type (trim-characters (pathname-type path) '#.(coerce "~0123456789" 'list))))
      (cond (p-type
	     (let ((content-type-from-p-type (mime-content-type-spec-for-pathname-type p-type nil)))
	       (cond ((and content-type content-type-from-p-type)
		      (unless (mime-content-type-spec-equal-p content-type content-type-from-p-type nil)
			(setq content-type content-type-from-p-type)))
		     (content-type-from-p-type
		      (setq content-type content-type-from-p-type))
		     (t (setq content-type '(:application :octet-stream))))))	;default binary
	    (content-type
	     (setq p-type (or (mime-content-type-primary-extension-string content-type nil)
			      (and (second content-type) (string-downcase (symbol-name (second content-type))))
			      "rdm")))		;random extension
	    (t (setq content-type '(:application :octet-stream))))	;default binary
      (values (make-pathname :defaults directory :name p-name :type p-type)
	      (if content-type (mime-content-type-copy-mode content-type) :binary)))))

