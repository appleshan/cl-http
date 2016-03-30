;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.125
;;; Reason: DEFINE-CONDITION HTTP::INSUFFICIENT-DATA:  new condition.
;;; Function HTTP::%MAKE-LOCAL-STREAM-FUNCTION-FOR-WITH-TRANSFER-DECODING:  handle eof errors from source stream
;;; DEFINE-CONDITION HTTP::REQUEST-ENTITY-TOO-LARGE:  fix typo in reason and close connection as default
;;; DEFINE-CONDITION HTTP::BAD-FORM-DATA:  -
;;; DEFINE-CONDITION HTTP::BAD-FORM-DATA-POSTED:  -
;;; DEFINE-CONDITION HTTP::BAD-MULTIPART-FORM-DATA:  -
;;; DEFINE-CONDITION HTTP::FILE-UPLOAD-MAXIMUM-SIZE-EXCEEDED:  relocate as 413
;;; DEFINE-CONDITION HTTP::SERVER-INTERNAL-ERROR:  Default is to close connection
;;; Written by JCMa, 4/27/01 17:00:15
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.124,
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
;;; Machine serial number 6294063,
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
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7),
;;; Compute serial from network addresses (from W:>reti>compute-serial-from-network-addresses.lisp.2).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;HTTP-CONDITIONS.LISP.194"
  "HTTP:SERVER;HEADERS.LISP.499"
  "HTTP:SERVER;HTTP-CONDITIONS.LISP.195")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.194")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition insufficient-data
                  (expectation-failed)
  ((reason :initform "Insuffient Data")))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.499")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun %make-local-stream-function-for-with-transfer-decoding (functions content-length headers default-copy-mode body &aux fctns)
  (cond-every
    ((member 'stream-copy-until-eof functions)
     (push `(stream-copy-until-eof
	      (from-stream to-stream &optional mode)
	      (let ((content-length ,(if content-length content-length
					 `(get-header :content-length ,headers)))
		    (copy-mode (or mode ,default-copy-mode)))
		(if content-length
		    (handler-case
		      (stream-copy-bytes from-stream to-stream content-length copy-mode)
		      (end-of-file ()		;data source delivered less bytes than content-length 4/27/2001 -- JCMa.
				   (error 'insufficient-data
					  :format-string "Insufficient Data: Source provides less data than the ~D bytes expected."
					  :format-args (list content-length)
					  :url (server-url *server*)
					  :close-connection t)))
		    (stream-copy-until-eof from-stream to-stream copy-mode))))
	   fctns)))
  (if fctns
      `(flet ,fctns
	 ,@(loop for fctn in functions
		 collect `(function ,fctn) into fspecs
		 finally (return (when fspecs
				   `((declare (dynamic-extent . ,fspecs))
				     ,@fspecs))))       ;no compiler warnings
	 ,@body)
      `(progn . ,body)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.499")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-macro with-transfer-decoding* ((stream url http-version &key
                                               (headers '*headers*) (copy-mode :binary)
                                               (stream-functions '(stream-copy-until-eof))) &body body)
  `(case ,http-version
     ((:http/1.0 :http/0.9)
      (let ((content-length (get-header :content-length ,headers)))
        (cond (content-length
               ,(%make-local-stream-function-for-with-transfer-decoding
                  stream-functions nil headers copy-mode body))
              (t ,@body))))
     (t (with-transfer-decoding (,stream ,url :headers ,headers
                                 :stream-functions ,stream-functions :copy-mode ,copy-mode)
          ,@body))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.195")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition bad-form-data
                  ()
  ((reason :initform "Unparsable form data.")))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.195")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition bad-form-data-posted
                  (bad-form-data bad-syntax-provided)
  ((reason :initform "Bad Form Data Posted: Client provided unparsable URL-encoded data.")))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.195")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition bad-multipart-form-data
                  ()
  ((reason :initform "Unparsable MIME multipart form data.")))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.195")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition bad-multipart-form-data-posted
                  (bad-multipart-form-data bad-form-data-posted)
  ((reason :initform "Bad Form Data Posted: Client provided unparsable MIME Multipart encoded data.")))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.195")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition request-entity-too-large
                  (client-reportable-condition)
  ((status-code :initform 413)
   (reason :initform "Request Entity Too Large")
   (close-connection-p :initform t)
   (retry-after :initform nil)))                ; should send a retry after header when non-null


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.195")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition file-upload-maximum-size-exceeded
                  (request-entity-too-large bad-multipart-form-data-posted)
  ((reason :initform "File Upload: Maximum Upload File Size Exceeded")))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.195")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition server-internal-error
                  (server-reportable-condition)
  ((status-code :initform 500)
   (close-connection-p :initform t)
   (server-error :initform nil :initarg :server-error :reader server-error)))

