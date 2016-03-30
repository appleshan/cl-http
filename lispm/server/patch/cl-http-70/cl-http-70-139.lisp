;;; -*- Mode: lisp; Syntax: common-lisp; Package: user; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.139
;;; Reason: Function (CLOS:METHOD URL:COPY-MODE (URL::OBJECT-MIXIN)):  handle variant extensions correctly
;;; Function HTTP::%WITH-HEADER-SET-INDEX:  -
;;; Function HTTP::%GET-HEADER-OBJECT:  -
;;; Function HTTP::WITH-HEADER-VALUES:  -
;;; Function (CLOS:METHOD HTTP::PARSE-HEADER-BUFFER (HTTP::HEADER-SET)):  -
;;; Function HTTP::%SERVER-WRITE-CONSOLE-WINDOW-NOTIFICATION:  -
;;; Function HTTP::%SERVER-WRITE-EXTENDED-COMMON-LOGFILE-ENTRY:  -
;;; Function HTTP::PARSE-MIME-MULTIPART-FORM-DATA-BLOCK:  -
;;; Function (CLOS:METHOD HTTP::LOG-WRITE-ENTRY (HTTP::EXTENDED-COMMON-FILE-FORMAT-MIXIN HTTP::SERVER-LOGGING-MIXIN T)):  -
;;; Function (CLOS:METHOD HTTP::PUT-DOCUMENT (URL:HTTP-OBJECT T)):  -
;;; Function (CLOS:METHOD HTTP::SNAPSHOT-LOG-ENTRY (HTTP::EXTENDED-COMMON-FILE-FORMAT-MIXIN HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Function (CLOS:METHOD HTTP::SNAPSHOT-LOG-ENTRY (HTTP::HTTP-POST-FILE-FORMAT-MIXIN HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Function (CLOS:METHOD HTTP::SNAPSHOT-LOG-ENTRY (HTTP::NOTIFICATION-LOG-FORMAT-MIXIN HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Function (CLOS:METHOD HTTP::SNAPSHOT-LOG-NOTIFICATION (HTTP::ASYNCHRONOUS-LOG-NOTIFICATION-MIXIN HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Function (CLOS:METHOD HTTP::WRITE-CONSOLE-WINDOW-NOTIFICATION (HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Function (CLOS:METHOD HTTP::WRITE-DOCUMENT-POST (HTTP::BASIC-SERVER-MIXIN URL:HTTP-URL T)):  -
;;; Function (CLOS:METHOD HTTP::WRITE-EXTENDED-COMMON-LOGFILE-ENTRY (HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Function HTTP::%SERVER-NOTIFY-CONSOLE-WINDOW:  -
;;; Function (CLOS:METHOD HTTP::DEALLOCATE-HEADER-OBJECTS (HTTP::HEADER-SET)):  -
;;; Function (CLOS:METHOD HTTP::CLEAR-HEADER-SET (HTTP::HEADER-SET)):  -
;;; Function (CLOS:METHOD HTTP::NULL-HEADER-SET-P (HTTP::HEADER-SET)):  -
;;; Function (CLOS:METHOD HTTP::HEADER-SET-COUNT (HTTP::HEADER-SET)):  -
;;; Function (CLOS:METHOD HTTP::MAP-HEADER-OBJECTS (T T)):  -
;;; Function (CLOS:METHOD HTTP::HEADER-SET-HEADER-PLIST (T)):  -
;;; Function (CLOS:METHOD HTTP::PUSH-HEADER (HTTP::HEADER-SET T T)):  -
;;; Function (CLOS:METHOD HTTP::WRITE-MODIFIED-HEADERS (HTTP::HEADER-SET T)):  -
;;; Written by JCMa, 7/28/01 23:15:30
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.6,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.138,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.4, Jcma 44,
;;; Experimental Genera 8 5 Patches 1.0, Genera 8 5 System Patches 1.34,
;;; Genera 8 5 Mailer Patches 1.1, Genera 8 5 Joshua Patches 1.0,
;;; Genera 8 5 Statice Runtime Patches 1.0, Genera 8 5 Statice Patches 1.0,
;;; Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, HTTP Proxy Server 6.23,
;;; HTTP Client Substrate 4.15, Statice Server 466.2, Wilbur 1.2, HTTP Client 50.9,
;;; Image Substrate 440.4, Essential Image Substrate 433.0, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
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

;;; Patch file for CL-HTTP version 70.139
;;; Written by JCMa, 7/29/01 03:05:54
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.6,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.139,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.4, Jcma 44,
;;; Experimental Genera 8 5 Patches 1.0, Genera 8 5 System Patches 1.34,
;;; Genera 8 5 Mailer Patches 1.1, Genera 8 5 Joshua Patches 1.0,
;;; Genera 8 5 Statice Runtime Patches 1.0, Genera 8 5 Statice Patches 1.0,
;;; Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, HTTP Proxy Server 6.23,
;;; HTTP Client Substrate 4.15, Statice Server 466.2, Wilbur 1.2, HTTP Client 50.9,
;;; Image Substrate 440.4, Essential Image Substrate 433.0, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
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
  "HTTP:SERVER;URL.LISP.428"
  "HTTP:SERVER;UTILS.LISP.498"
  "HTTP:SERVER;HEADERS.LISP.508"
  "HTTP:SERVER;SERVER.LISP.910"
  "HTTP:SERVER;SERVER.LISP.911"
  "HTTP:SERVER;HEADERS.LISP.509")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.428")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

;; extension should be stored in an IV extension-keyword for faster evaluation if it turns out
;; we call this predicate much. Right now, it is only used by the PUT method. -- JCMa 5/19/1995.
(defmethod copy-mode ((url object-mixin))
  (with-slots (extension) url
    (if extension
        (%content-type-copy-mode (http:primary-pathname-extension (http:symbolize extension http:*keyword-package*)) t)
        :text)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.498")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

;;;------------------------------------------------------------------- 
;;;
;;; HEADER UTILITIES
;;;

(defmacro %with-header-set-index ((header-set) &body body)
  `(let* ((index-ptr (%header-set-index ,header-set))
	  (index (car index-ptr))
	  (.headers. (cdr index-ptr)))
     ,@body))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.498")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defun %get-header-object (header-set keyword)
  (when header-set
    (%with-header-set-index (header-set)
      (%%get-header-object keyword index .headers.))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.498")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmacro with-header-values (headers header-set &body body)
  "Binds HEADERS to appropriate header values from HEADER-SET.

This macro is more than GET-HEADER at accessing header values when more than
one header is accessed.

HEADERS is a list of header specs, each of which can be either a variable
symbol or a list of (VARIABLE-SYMBOL NULL-BINDING-VALUE). When supplied,
NULL-BINDING-VALUE is used as the variable value when not header named
VARIABLE-SYMBOL is present."
  (flet ((header-spec (spec)
	   (etypecase spec
	     (symbol (values spec nil))
	     (cons (values-list spec))))
	 (%make-header-binding1 (header-set keyword null-binding-value)
	   `(let ((hdr (%get-header-object ,header-set ,keyword)))
	      ,(if null-binding-value
		   `(if hdr (header-value hdr) ,null-binding-value)
		   `(and hdr (header-value hdr)))))
	(%make-header-binding2 (keyword null-binding-value)
	   `(let ((hdr (%%get-header-object ,keyword index .headers.)))
	      ,(if null-binding-value
		   `(if hdr (header-value hdr) ,null-binding-value)
		   `(and hdr (header-value hdr))))))
    (cond ((cdr headers)
	   (loop for spec in headers
		 with header and null-binding-value
		 do (multiple-value-setq (header null-binding-value)
		      (header-spec spec))
		 collect `(,header ,(%make-header-binding2 (intern (symbol-name header) *keyword-package*) null-binding-value))
		   into bindings
		 finally (return `(%with-header-set-index (,header-set)
				    (let* ,bindings ,@body)))))
	  (t (multiple-value-bind (header null-binding-value)
		 (header-spec (car headers))
	       `(let ((,header ,(%make-header-binding1 header-set (intern (symbol-name header) :keyword) null-binding-value))) ,@body))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.508")
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
				     (index index vector) (.headers. .headers. vector))
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
			    (cond ((setq current-header (%%get-header-object keyword index .headers.))
				   (push-multi-line current-header s e))
				  ;; push a new header
				  (t (setq room-p (< index-fill-pointer index-size)
					   current-header (instantiate-buffered-header (and room-p index-fill-pointer) .headers.
										       keyword buffer s e))
				     ;; possibly grow indices
				     (unless room-p
				       (let ((n-size (floor (* (the fixnum index-size) *header-set-growth-factor*))))
					 (setf index (adjust-array index n-size :element-type t :initial-element nil)
					       .headers. (adjust-array .headers. n-size :element-type t :initial-element nil)
					       index-size (array-total-size index)
					       ;; update the stored pointer to these structures
					       (car index-ptr) index
					       (cdr index-ptr) .headers.)))
				     ;; push the new header
				     (setf (aref index index-fill-pointer) keyword
					   (aref .headers. index-fill-pointer) current-header
					   (fill-pointer index) (incf index-fill-pointer)
					   (fill-pointer .headers.) index-fill-pointer))))
			   (t nil)))))))))	;ignore undelimited headers


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.910")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun %server-write-console-window-notification (server log-stream)
  (let* ((requests (server-requests-completed server))
	 (host-name (host-log-name server))
	 (user-name (%server-user-qualified-name server))
	 (request (server-request server t))
	 (request-time nil)
	 (status (server-status server))
	 (port *standard-http-port*)		;stream data may no longer be valid
	 (bytes (bytes-transmitted (server-stream server)))	;total bytes (not number of bytes in a document)
	 (proxy-p (server-proxy-request-p server))
	 header-set cpu-time elapsed-time user-agent-val referer-val)
    (unless proxy-p
      (when (setq header-set (server-headers server)))
      (with-header-values (user-agent referer) header-set
	(setq user-agent-val user-agent
	      referer-val referer)))
    (psetq cpu-time (cpu-time server)
	   elapsed-time (elapsed-time server))
    (%write-console-window-notification
      host-name port proxy-p requests request request-time status bytes user-name user-agent-val referer-val
      cpu-time elapsed-time log-stream)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.910")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun %server-write-extended-common-logfile-entry (server log-stream gmt-p delimiter)
  (with-header-values (user-agent referer) (server-headers server)
    (%write-extended-common-logfile-entry
      (host-log-name server)
      (server-request server t)
      (server-request-time server)
      (server-status server)
      (server-bytes-transmitted server)
      (%server-user-qualified-name server)
      user-agent 
      referer
      gmt-p log-stream delimiter)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.910")
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
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.910")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod log-write-entry ((log extended-common-file-format-mixin) (agent server-logging-mixin) stream)
  (%server-write-extended-common-logfile-entry agent stream (log-times-in-gmt-p log) #\tab))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.911")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod put-document ((url url:http-object) stream &optional newly-created-url-p (check-versions-p t))
  (declare (values url exported))
  (flet ((download-crlf-file (stream pathname bytes copy-mode)
	   (let ((crlf-pathname (crlf-pathname pathname)))
	     (stream-copy-bytes stream crlf-pathname bytes copy-mode)
	     (decode-crlf-file crlf-pathname)
	     (delete-file-carefully crlf-pathname)))
	 (update-file-properties (pathname server modification-date)
	   (when modification-date
	     (set-file-modification-date pathname modification-date nil))
	   (set-file-author pathname server nil)
	   (autotype-file pathname nil)))
    (let ((pathname (url:translated-pathname url))
          (server *server*)
          (headers *headers*))
      ;; throw out of http transaction if a conflict is detected
      (when check-versions-p
        (let ((version (document-version pathname)))
          (case (server-http-version server)
            ((:http/0.9 :http/1.0)
             (check-derived-from-version url version))
            (t (check-if-match-precondition version t :put headers) 
               (check-if-unmodified-since-precondition version :put headers)))))
      (with-header-values (content-length content-range content-type last-modified transfer-encoding) headers
	;; check for byte ranges in 1.1
	(when content-range
	  (error 'server-not-implemented :url url :method :put
		 :format-string "Putting byte ranges is not implemented."))
	(let ((copy-mode (if content-type (mime-content-type-copy-mode content-type) (url:copy-mode url)))
	      (modification-date (or last-modified (server-request-time server))))
	  (cond (transfer-encoding 
		 (case transfer-encoding
		   #+(or Genera MCL LispWorks)
		   (:chunked
		     (with-successful-put-response (server stream)
		       (with-chunked-transfer-decoding (stream :headers headers)
			 (case copy-mode
			   (:crlf (stream-decode-crlf-until-eof stream pathname))
			   (t (stream-copy-until-eof stream pathname copy-mode))))
		       (update-file-properties pathname server modification-date)
		       (values url (if newly-created-url-p :created :modified) modification-date)))
		   (t (error 'server-not-implemented :close-connection t :url url :method :put
			     :format-string "The HTTP transfer encoding, ~A, is not implemented."
			     :format-args (list transfer-encoding)))))
		(content-length
		 (handler-case-if (not *debug-server*) 
		    (with-successful-put-response (server stream)
		      (case copy-mode
			(:crlf (download-crlf-file stream pathname content-length copy-mode))
			(t (stream-copy-bytes stream pathname content-length copy-mode)))
		      (update-file-properties pathname server modification-date)
		      (values url (if newly-created-url-p :created :modified) modification-date))
		   (error (err)
			  (error 'error-handling-put-method :url url :method :put :server-error err :headers (header-plist)
				 :format-string "Error executing PUT method for ~A."
				 :format-args (list (url:name-string url))))))
		(t (error 'content-length-required :url url :method :put
			  :format-string "no content-length header provided for ~A."
			  :format-args (list (url:name-string url))))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.911")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod snapshot-log-entry ((log extended-common-file-format-mixin) (server server-logging-mixin))
  (let ((host-name (host-log-name server))
	(user-name (%server-user-qualified-name server))
	(request (server-request server t))	; don't lose when transaction reset
	(request-time (server-request-time server))
	(status (server-status server))
	(bytes-transmitted (server-bytes-transmitted server))	;total bytes-transmitted (not number of bytes-transmitted in a document)
	(header-set (server-headers server))
	user-agent-val referer-val)
    (when header-set				;may be null when client drops connection (408)
      (with-header-values (user-agent referer) (server-headers server)
	(setq user-agent-val user-agent
	      referer-val referer)))
    (allocate-extended-common-log-entry log host-name request request-time status bytes-transmitted user-name user-agent-val referer-val)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.911")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod snapshot-log-entry ((log http-post-file-format-mixin) (server server-logging-mixin) &aux form-alist)
  (when (and (eq :post (server-method server))
	     (setq form-alist (server-form-alist server)))
    (let ((host-name (host-log-name server))
	  (user-name (%server-user-qualified-name server))
	  (request-time (server-request-time server))
	  (request (server-request server t))
	  (status (server-status server))
	  (bytes-transmitted (server-bytes-transmitted server))
	  (bytes-received (server-bytes-received server))
	  (headers (server-headers server)))
      (with-header-values (user-agent referer) headers
	(allocate-http-post-log-entry log host-name request request-time status bytes-transmitted user-name
				      user-agent referer bytes-received form-alist)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.911")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod snapshot-log-entry ((log notification-log-format-mixin) (server server-logging-mixin))
  (let ((requests-completed (server-requests-completed server))
	(host-name (host-log-name server))
	(user-name (%server-user-qualified-name server))
	(request (server-request server t))
	(request-time (server-request-time server))
	(status (server-status server))
	(port *standard-http-port*)		;stream data may no longer be valid
	(bytes-transmitted (bytes-transmitted (server-stream server)))	;total bytes (not number of bytes in a document)
	(proxy-p (server-proxy-request-p server))
	header-set cpu-time elapsed-time user-agent-val referer-val)
    (unless proxy-p
      (when (setq header-set (server-headers server)))
      (with-header-values (user-agent referer) header-set
	(setq user-agent-val user-agent
	      referer-val referer)))
    (psetq cpu-time (cpu-time server)
	   elapsed-time (elapsed-time server))
    (allocate-notification-log-entry log host-name port request request-time status bytes-transmitted user-name
				     user-agent-val referer-val requests-completed cpu-time elapsed-time proxy-p)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.911")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod snapshot-log-notification ((log asynchronous-log-notification-mixin) (server server-logging-mixin))
  (let ((requests-completed (server-requests-completed server))
	(host-name (host-log-name server))
	(user-name (%server-user-qualified-name server))
	(request (server-request server t))
	(request-time (server-request-time server))
	(status (server-status server))
	(port *standard-http-port*)		;stream data may no longer be valid
	(bytes-transmitted (bytes-transmitted (server-stream server)))	;total bytes (not number of bytes in a document)
	(proxy-p (server-proxy-request-p server))
	header-set cpu-time elapsed-time user-agent-val referer-val)
    (unless proxy-p
      (when (setq header-set (server-headers server)))
      (with-header-values (user-agent referer) header-set
	(setq user-agent-val user-agent
	      referer-val referer)))
    (psetq cpu-time (cpu-time server)
	   elapsed-time (elapsed-time server))
    (allocate-console-notification-log-entry log host-name port request request-time status bytes-transmitted user-name
					     user-agent-val referer-val requests-completed cpu-time elapsed-time proxy-p)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.911")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod write-console-window-notification ((server server-logging-mixin) &optional (log-stream *standard-output*))
  (with-string-for-null-stream (log-stream)
    (%server-write-console-window-notification server log-stream)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.911")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod write-document-post ((server basic-server-mixin) (url http-url) http-version)
  (with-slots (stream) server
    (case http-version
      ((:http/0.9 :http/1.0))
      ;; alert HTTP 1.1 or greater clients that we are ready
      (t (report-status-continue stream)
	 (send-cr-line-feed stream)
	 (force-output stream)
	 (setf (server-status server) 100.)))
    ;; process the POST
    (with-header-values (content-type transfer-encoding) *headers*
      (case transfer-encoding
	((nil :chunked))
	(t (error 'server-not-implemented :close-connection t :url url :method :post
		  :format-string "The HTTP transfer encoding, ~A, is not implemented."
		  :format-args (list transfer-encoding))))
      ;; Do the work
      (destructuring-bind (&optional doc-type doc-subtype &rest args) content-type
	(declare (ignore args))
	(post-document url doc-type doc-subtype stream)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.911")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod write-extended-common-logfile-entry ((server server-logging-mixin) &optional (log-stream *standard-output*)
                                                (gmt-p *log-times-in-gmt*) (delimiter #\space))
  (with-string-for-null-stream (log-stream)
    (%server-write-extended-common-logfile-entry server log-stream gmt-p delimiter)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.911")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun %server-notify-console-window (server)
  (flet ((notify-the-console (stream)
	   (%server-write-console-window-notification server stream)))
    (declare (dynamic-extent #'notify-the-console))
    (careful-notify-log-window #'notify-the-console)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.509")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod deallocate-header-objects ((header-set header-set))
  ;; the header objects could be left in place to reduce latency on the front and back ends.  4/7/99 -- JCMa.
  (%with-header-set-index (header-set)
    (with-fast-array-references ((.headers. .headers. vector)
				 (index index vector))
      (loop for idx fixnum upfrom 0 below (the fixnum (array-total-size .headers.))
	    for hdr = (aref .headers. idx)
	    while hdr				;headers always in the front
	    do (deallocate-header hdr)
	       (setf (aref .headers. idx) nil
		     (aref index idx) nil)
	    finally (setf (fill-pointer .headers.) 0
			  (fill-pointer index) 0)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.509")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod clear-header-set ((header-set header-set) &optional deallocate-surplus-headers-p)
  ;; reset other indices
  (setf (fill-pointer (%header-set-buffer header-set)) 0
	(fill-pointer (%header-set-line-ends header-set)) 0)
  ;; reset header objects
  (%with-header-set-index (header-set)
    (with-fast-array-references ((.headers. .headers. vector)
				 (index index vector))
      
      (let ((fill-pointer (fill-pointer .headers.)))
	(cond ((and deallocate-surplus-headers-p (< *header-set-index-size* fill-pointer))
	       (loop for idx fixnum upfrom 0 below *header-set-index-size*
		     do (clear-header (aref .headers. idx)))
	       (loop for idx fixnum upfrom *header-set-index-size* below fill-pointer
		     do (deallocate-header (aref .headers. idx))
			(setf (aref .headers. idx) nil
			      (aref index idx) nil)))
	      (t (loop for idx fixnum upfrom 0 below fill-pointer
		       do (clear-header (aref .headers. idx))))))
      (setf (fill-pointer .headers.) 0
	    (fill-pointer index) 0))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.509")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod null-header-set-p ((header-set header-set))
  (%with-header-set-index (header-set)
    .headers.
    (zerop (fill-pointer index))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.509")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod header-set-count ((header-set header-set))
  (%with-header-set-index (header-set)
    .headers.
    (fill-pointer index)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.509")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod map-header-objects (header-set function)
  (%with-header-set-index (header-set)
    index					;ignore
    (with-fast-array-references ((.headers. .headers. vector))
      (loop for idx fixnum upfrom 0 below (fill-pointer .headers.)
	    do (funcall function (aref .headers. idx))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.509")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod header-set-header-plist (header-set &optional (value-key #'header-value) predicate)
  (%with-header-set-index (header-set)
    (with-fast-array-references ((index index vector)
				 (.headers. .headers. vector))
      (if predicate
	  (loop for idx fixnum upfrom 0 below (fill-pointer index)
		for key = (aref index idx)
		for value = (funcall value-key (aref .headers. idx))
		when (funcall predicate key value)
		  collect key
		  and
		collect value)
	  (loop for idx fixnum upfrom 0 below (fill-pointer index)
		collect (aref index idx)
		collect (funcall value-key (aref .headers. idx)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.509")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod push-header ((header-set header-set) keyword value)
  (check-type keyword keyword)
  (macrolet ((ensure-room-vector (vector index element-type update-form)
	       `(let ((size (array-total-size ,vector)))
		  (unless (< ,index size)
		    (setf size (floor (* (the fixnum size) *header-set-growth-factor*))
			  ,vector (adjust-array ,vector size :element-type ,element-type)
			  ,update-form ,vector)
		    t))))
    (when (%header-print-series-predicate keyword)
      (error "Don't know to handle series valued headers at this time."))
    (%with-header-set-index (header-set) 
      (let* ((buffer (%header-set-buffer header-set))
	     (start (fill-pointer buffer))
	     (line-ends (%header-set-line-ends header-set))
	     (line-ends-fill-pointer (fill-pointer line-ends))
	     (pname (symbol-name (or (%header-print-name keyword) keyword)))
	     (pname-length (length pname))
	     (print-fctn (%header-print-function keyword))
	     (raw-value (with-output-to-string (stream)
			  (funcall print-fctn value stream)))
	     (raw-value-length (length raw-value))
	     (pos1 (+ start pname-length))
	     (pos2 (+ pos1 2))
	     (pos3 (+ pos2 raw-value-length))
	     (index-fill-pointer (fill-pointer index))
	     header header-array-grown-p)
	(declare (dynamic-extent raw-value))
	(ensure-room-vector buffer pos3 *standard-character-type* (%header-set-buffer header-set))
	(ensure-room-vector line-ends line-ends-fill-pointer 'fixnum (%header-set-line-ends header-set))
	(setq header-array-grown-p (ensure-room-vector index index-fill-pointer t (car index-ptr)))
	(ensure-room-vector index index-fill-pointer t (cdr index-ptr))
	;; copy raw values into buffer
	(copy-vector-portion pname 0 pname-length buffer start pos1)
	(copy-vector-portion ": " 0 2 buffer pos1 pos2)
	(copy-vector-portion raw-value 0 (length raw-value) buffer pos2 pos3)
	;; update data associated data structures
	(setf (fill-pointer buffer) pos3	;extend buffer pointer
	      (aref line-ends line-ends-fill-pointer) pos3	;assume a single line header for simplicity  4/7/99 -- JCMa.
	      (fill-pointer line-ends) (1+ line-ends-fill-pointer)	;extend line ends pointer
	      header (instantiate-buffered-header (and (not header-array-grown-p) index-fill-pointer) .headers. keyword buffer pos2 pos3)
	      (aref index index-fill-pointer) keyword	;update index vector
	      (aref .headers. index-fill-pointer) header	;update data vector
	      (fill-pointer index) (incf index-fill-pointer)	;extend index pointer
	      (fill-pointer .headers.) index-fill-pointer)	;extend data pointer
	header))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.509")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod write-modified-headers ((header-set header-set) stream &optional modification-plist excluded-headers termination-line-p additional-headers)
  (declare (ignore termination-line-p additional-headers))
  (%with-header-set-index (header-set)
    (with-fast-array-references ((index index vector)
				 (.headers. .headers. vector))
      (loop with suppressed-headers = *suppressed-headers* and header-object
	    for idx fixnum upfrom 0 below (fill-pointer .headers.)
	    for header-name = (aref index idx)
	    unless (or (member header-name excluded-headers)
		       (member header-name suppressed-headers))
	      do (multiple-value-bind (new-value found-p)
		     (%get-modification modification-plist header-name)
		   (cond (found-p
			  (%write-header header-name new-value stream))
			 ((%header-suppress-p (setq header-object (aref .headers. idx))))
			 (t (write-header header-name header-object stream)))))))
  ;; send any remaining headers
  (loop for (header-name header-value) on modification-plist by #'cddr
	do (%write-header header-name header-value stream)))

