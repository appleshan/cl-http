;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.135
;;; Reason: DEFINE-CONDITION HTTP::PROXY-ACCESS-FORBIDDEN:  always close connection.
;;; Function HTTP::PATHNAME-COPY-MODE:  -
;;; Function (CLOS:METHOD HTTP::PATHNAME-COPY-MODE (CL:PATHNAME)):  -
;;; Function (CLOS:METHOD HTTP::PATHNAME-COPY-MODE (STRING)):  -
;;; Function HTTP:COPY-FILE:  -
;;; Function HTTP::NSTRING-TRANSLATE-CHARS:  handle platforms where newline is not return.
;;; Function (CLOS:METHOD HTTP::READ-TRANSLATED-CHAR (T T)):  return newline rather than return to handle certain platforms.
;;; Function (CLOS:METHOD HTTP::REPORT-HTTP-HEADERS (HTTP::METHOD-NOT-ALLOWED T) :AROUND):  close for post as well.
;;; Function (CLOS:METHOD HTTP::REPORT-STATUS (HTTP::SERVER-OVERLOADED T)):  -
;;; Function (CLOS:METHOD HTTP::REPORT-STATUS (HTTP::RECOVERABLE-UNAUTHORIZED-ACCESS T)):  -
;;; Written by JCMa, 5/22/01 14:03:07
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.134,
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
;;; HTTP Proxy Server 6.22, HTTP Client Substrate 4.14, Statice Server 466.2,
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
  "HTTP:SERVER;HTTP-CONDITIONS.LISP.198"
  "HTTP:SERVER;HEADERS.LISP.507"
  "HTTP:SERVER;UTILS.LISP.496"
  "HTTP:SERVER;UTILS.LISP.497"
  "HTTP:SERVER;REPORT.LISP.187")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.198")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition proxy-access-forbidden
                  (access-forbidden)
  ((reason :initform "Proxy Access Forbidden")
   (reporter :initform 'report-proxy-access-forbidden)
   (close-connection-p :initform t)))		;always close when access forbidden


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.507")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defgeneric pathname-copy-mode (pathname &optional error-p)
  (declare (values copy-mode))
  (:documentation "Returns the copy mode keyword for pathname."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.507")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod pathname-copy-mode ((pathname pathname) &optional (error-p t))
  (let ((type (pathname-type pathname))
	content-type)
    (cond (type
	   (when (setq content-type (mime-content-type-spec-for-pathname-type type error-p))
	     (mime-content-type-copy-mode content-type)))
	  (error-p (error "Can't determine the copy mode because ~A has no pathname type." type))
	  (t nil))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.507")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod pathname-copy-mode ((string string) &optional (error-p t))
  (let ((pathname (handler-case-if (not error-p) (pathname string) (error () nil))))
    (when pathname
      (pathname-copy-mode pathname error-p))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.496")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

;;;------------------------------------------------------------------- 
;;;
;;; CONDITIONAL FILE COPY
;;;

(define-generic copy-file (from-pathname to-pathname &key copy-mode report-stream &allow-other-keys)
  (:documentation "A portable file copy.
COPY-MODE is one of :TEXT, CRLF, or binary.
When REPORT-STREAM is non-null, COPY-FILE reports its activities."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.496")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmethod copy-file ((from-pathname string) to-pathname &key (copy-mode :text) report-stream &allow-other-keys)
  (copy-file (if (url:url-string-p from-pathname)
		 (url:intern-url from-pathname :if-does-not-exist :uninterned)
		 (pathname from-pathname))
	     to-pathname :copy-mode copy-mode :report-stream report-stream))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.496")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmethod copy-file (from-pathname (to-pathname string) &key (copy-mode :text) report-stream &allow-other-keys)
  (copy-file from-pathname 
	     (if (url:url-string-p to-pathname)
		 (url:intern-url to-pathname :if-does-not-exist :uninterned)
		 (pathname to-pathname))
	     :copy-mode copy-mode :report-stream report-stream))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.497")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(define nstring-translate-chars (string &optional (start 0) (end (length string)) (pad-char #\space) set-fill-pointer-p)
  "Destructively translates characters in STRING according to HTTP POST FORM rules.
All escaped characters are translated. CRLF is converted to CR and + is converted to space.
When SET-FILL-POINTER-P is non-null, the fill-pointer on STRING is moved backwards
by two times the number of characters unescaped. Otherwise, that number of characters
are padded on the right using PAD-CHAR"
  (declare (values unescaped-string new-end chars-unescaped-p)
           (fixnum start end))
  (macrolet ((insert-char (char vector index)
	       `(case ,char
		  ,@(unless (eql #\newline #\Return)
		      `((#\Return
			 (setf (aref ,vector ,index) #\newline)
			 (incf ,index))))
		  (#\LineFeed)			;drop LF
		  (t (setf (aref ,vector ,index) ,char)
		     (incf ,index)))))
    (flet ((decode-escaped-char (string start end)
	     (handler-case
	       (let ((code (parse-integer string :radix 16 :start start :end end)))
		 (case code
		   (13 #\Return)
		   (10 #\LineFeed)
		   (t (code-char code))))
	       (error () nil))))
      (declare (inline decode-escaped-char))
      (with-fast-array-references ((string string string))
	(loop with read-idx fixnum = start
	      with write-idx fixnum = start
	      and new-char and chars-shifted-p 
	      while (< read-idx end)
	      for char = (aref string read-idx)
	      do (incf read-idx)
		 (case char
		   ;;plus translated to space
		   (#\+
		    (setf (aref string write-idx) #\space)
		    (incf write-idx))
		   ;; LineFeed is ignored
		   (#\LineFeed 
		    (setq chars-shifted-p t))
		   ;; escaped characters are translated
		   (#.*escape-character*
		    (setq chars-shifted-p t)
		    (let ((escp-end (+ read-idx 2)))
		      ;; only translate when within bounds and hex code is good.
		      (cond ((and (<= escp-end end)
				  (setq new-char (decode-escaped-char string read-idx escp-end)))
			     (setq read-idx escp-end)
			     (insert-char new-char string write-idx))
			    (chars-shifted-p
			     (setf (aref string write-idx) char)
			     (incf write-idx))
			    (t (incf write-idx)))))
		   (t (when chars-shifted-p
			(setf (aref string write-idx) char))
		      (incf write-idx)))
	      finally (return (cond (chars-shifted-p
				     (if set-fill-pointer-p
					 (setf (fill-pointer string) write-idx)
					 (loop for idx upfrom write-idx below end
					       do (setf (aref string idx) pad-char)))	;pad out the end
				     (values string write-idx chars-shifted-p))
				    (t (values string read-idx)))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.497")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmethod read-translated-char (stream delimiter &optional (eof-errorp t) eof-value)
  (flet ((decode-escaped-char (stream)
           (let ((string (make-string 2)))
             (declare (dynamic-extent string))
             (www-utils:with-fast-array-references ((string string string))
               (setf (aref string 0) (read-char stream)
                     (aref string 1) (read-char stream))
               (let ((code (parse-integer string :radix 16 :start 0 :end 2)))
                 (case code
                   (13 #\newline)
                   (10 #\linefeed)
                   (t (code-char code))))))))
    (declare (inline decode-escaped-char))
    (let ((char (read-char stream eof-errorp eof-value)))
      (cond ((characterp char)
             (cond ((eql char delimiter)        ;delimiter return nil
                    (values nil 1))
                   ((eql char #\+)
                    (values #\space 1))
                   ((eql char *escape-character*)       ;escape character
                    (handler-case-if eof-errorp
                       (values (decode-escaped-char stream) 3)
                      (end-of-file () (values eof-value 1))))
                   (t (values char 1))))
            ((equal char eof-value)
             (values char 0))
            (t (error "Bad value, ~S, returned by READ-CHAR." char))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;REPORT.LISP.187")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

;; This is quite inline so that it doesn't waste time when the server is on the edge.
(defmethod report-status ((condition server-overloaded) stream)
  (flet ((report-the-message (stream reason)
           (with-html-document (:declare-dtd-version-p t :stream stream)
             (with-document-preamble (:stream stream)
               (declare-title reason :stream stream))
             (with-standard-document-body (:stream stream)
               (with-section-heading (reason :stream stream)
                 (horizontal-line :stream stream)
                 (with-paragraph (:stream stream)
                   (etypecase *overload-message*
                     (null
                       (write-string "This server is currently operating at capacity and cannot accept your request. Please try again later."
                                     stream))
                     (string (write-string *overload-message* stream))
                     (function (funcall *overload-message* condition stream))
                     (symbol (funcall (fdefinition *overload-message*) condition stream))))
                 (horizontal-line :stream stream)
                 (cl-http-signature stream))))))
    (let* ((status-code (status-code condition))
           (reason (or (http-reason condition)
                       (get-string-for-status-code status-code)))
           (server *server*)
           (method (or (server-method server) (http-method condition))))
      (case method
        (:head ;; head redirects never send a body
          (send-status-line stream status-code reason)
         (report-http-headers condition stream t))
        (t (cond ((client-http-version-meets-p server :http/1.1)
                  (%with-chunked-transfer-encoding
                    (stream)
                    (progn (send-status-line stream status-code reason)
                           (report-http-headers condition stream nil))
                    (report-the-message stream reason)))
                 (t (send-status-line stream status-code reason)
                    (report-http-headers condition stream t)
                    (report-the-message stream reason))))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;REPORT.LISP.187")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(defmethod report-status ((condition recoverable-unauthorized-access) stream)
  (let* ((more-headers (authentication-header-spec condition))
         (server *server*)
         (method (or (server-method server) (http-method condition))))
    (declare (dynamic-extent more-headers))
    (case method
      (:head ;; head redirects never send a body
        (report-status-line condition stream)
       (report-http-headers condition stream t more-headers nil))
      (t (case method				;close connection for 1.0 puts, or unsynchronizable posts
	   ((:post :put) (setf (server-close-connection-p server) t)))
	 (cond ((client-http-version-meets-p server :http/1.1)
		(%with-chunked-transfer-encoding
		  (stream)
		  (progn (report-status-line condition stream)
			 (report-http-headers condition stream nil more-headers))
		  (report-status-message condition stream)))
	       (t (case method			;close connection for 1.0 puts
		    ((:post :put) (setf (server-close-connection-p server) t)))
		  (report-status-line condition stream)
		  (report-http-headers condition stream t more-headers)
		  (report-status-message condition stream)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;REPORT.LISP.187")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(defmethod report-status ((condition access-control-condition) stream)
  (let* ((server *server*)
         (method (or (server-method server) (http-method condition))))
    (case method
      (:head ;; head redirects never send a body
        (report-status-line condition stream)
       (report-http-headers condition stream t nil nil))
      (t (cond ((client-http-version-meets-p server :http/1.1)
                (%with-chunked-transfer-encoding
                  (stream)
                  (progn (report-status-line condition stream)
                         (report-http-headers condition stream nil))
                  (report-status-message condition stream)))
               (t (case method			;close connection for 1.0 puts, or unsynchronizable posts
		    ((:post :put) (setf (server-close-connection-p server) t)))
		  (report-status-line condition stream)
                  (report-http-headers condition stream t)
                  (report-status-message condition stream)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;REPORT.LISP.187")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(defmethod report-http-headers :around ((condition method-not-allowed) stream
                                        &optional (termination-line-p t) header-plist content-type)
  (let* ((url (http-url condition))
         (server *server*)
         ;; If the url is an uninterned search URL, we just default to the
         ;; standard methods on the server  8/7/96 -- JCMa.
         (more-headers `(:allow ,(http-methods (or (and url (intern-url url :if-does-not-exist :soft))
                                                   server)
                                               (server-http-version server))
                         ,@header-plist)))
    (declare (dynamic-extent more-headers))
    (case (http-method condition)
      ((:post :put) ;; close connection because the 1.0 servers will try to blast put data at us, and we don't know that the connection is syncrhonizable
       (unless (client-http-version-meets-p server :http/1.1)
	 (setf (server-close-connection-p server) t))))
    (call-next-method condition stream termination-line-p more-headers content-type)))

