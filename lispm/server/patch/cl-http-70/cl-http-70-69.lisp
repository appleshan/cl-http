;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.69
;;; Reason: This patch allows application code to :READ-INPUT-BUFFER and
;;; :ADVANCE-INPUT-BUFFER to be called on an HTTP stream after all chunked data
;;; has been read by continuously signalling end of chunking when EOF is T. After
;;; HTTP::CHUNK-TRANSFER-DECODING-MODE-FINISH executes, the stream is again
;;; available for reading operations.
;;; 
;;; D,#TD1PsT[Begin using 006 escapes](1 0 (NIL 0) (NIL :BOLD NIL) "CPTFONTCB")Note that all functions calling the macro HTTP::WITH-CHUNKED-TRANSFER-DECODING
0;;; 1must be recompiled.
0;;; 
;;; Function (FLAVOR:NCWHOPPER :READ-INPUT-BUFFER TCP::CHUNK-TRANSFER-DECODING-INPUT-STREAM-MIXIN):  
;;; Restore whopper being careful not to allow reading past end of chunked data until chunking cleared.
;;; Function (FLAVOR:NCWHOPPER :ADVANCE-INPUT-BUFFER TCP::CHUNK-TRANSFER-DECODING-INPUT-STREAM-MIXIN):  
;;; Don't allow user code to advance past end of chunked data until chunked decoding cleared.
;;; Function (FLAVOR:NCWHOPPER :NEXT-INPUT-BUFFER TCP::CHUNK-TRANSFER-DECODING-INPUT-STREAM-MIXIN):  
;;; Detect and signal dropped TCP connection.
;;; 
;;; Function HTTP::WITH-CHUNKED-TRANSFER-DECODING:  end chunked decoding before reading the headers.
;;; Function HTTP::CHUNK-TRANSFER-DECODING-MODE-FINISH:  new.
;;; Function HTTP::UPDATE-CHUNK-TRANSFER-DECODED-HEADERS:  add content-length arg.
;;; Function HTTP::WITH-TRANSFER-DECODING:  -
;;; Function HTTP::WITH-TRANSFER-DECODING*:  -
;;; Function (CLOS:METHOD HTTP::PUT-DOCUMENT (URL:HTTP-OBJECT T)):  -
;;; Written by JCMa, 9/06/00 20:11:14
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.68,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42, HTTP Proxy Server 5.20,
;;; HTTP Client Substrate 3.15, HTTP Client 49.8, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, W4 Constraint-Guide Web Walker 45.3,
;;; W4 Examples 15.0, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Documentation Patches 1.0,
;;; Genera 8 5 Clim Patches 1.0, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.11,
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
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).

;;; Patch file for CL-HTTP version 70.69
;;; Written by JCMa, 9/06/00 23:49:31
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.69,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42, HTTP Proxy Server 5.21,
;;; HTTP Client Substrate 3.16, HTTP Client 49.9, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, W4 Constraint-Guide Web Walker 45.4,
;;; W4 Examples 15.0, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Documentation Patches 1.0,
;;; Genera 8 5 Clim Patches 1.0, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.11,
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
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).




(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.193"
  "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.194"
  "HTTP:SERVER;HEADERS.LISP.473"
  "HTTP:SERVER;HEADERS.LISP.472"
  "HTTP:SERVER;SERVER.LISP.851")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.193")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

(scl:defwhopper (:read-input-buffer tcp::chunk-transfer-decoding-input-stream-mixin) (&optional eof no-hang-p)
  (cond ((not chunked-input) (continue-whopper eof no-hang-p))
	((zerop input-chunk-size)		;Zero sized chunk signals the end of chunk-encoded data
	 (if eof				;We continue to signal eof until chunked-input is cleared.
	     (signal 'end-of-chunk-transfer-decoding :stream self)
	     (values nil si:stream-input-index si:stream-input-limit)))
        (t (condition-case-if (not eof)  ()
                (continue-whopper eof no-hang-p)
              (end-of-chunk-transfer-decoding
                (values nil si:stream-input-index si:stream-input-limit))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.193")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

(defwhopper (:advance-input-buffer chunk-transfer-decoding-input-stream-mixin) (&optional new-index)
  (cond ((not chunked-input) (continue-whopper new-index))
	((zerop input-chunk-size)		;Zero-sized chunk signals the end of chunk-encoded data
	 si:stream-input-index)			;input index does not move forward until EOF cleared
        ((or (null new-index) (= new-index input-scan-end si:stream-input-limit))
         (setq si:stream-input-index input-scan-end))
        (t (continue-whopper new-index))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.194")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

;;; Because ASCII translation occurs in a :next-input-buffer whopper, chunking
;;; must provide the correct window before that happens lest translation
;;; change a CR delimiting the end of the chunk. This pathology occurs only
;;; when chunks cross packet boundaries, and is especially frequent when text
;;; is shipped with only LF boundaries.   4/22/97 -- JCMa.
(defwhopper (:next-input-buffer chunk-transfer-decoding-input-stream-mixin) (&optional no-wait-p &aux at-eof)
  (flet ((maybe-signal-tcp-eof (at-eof tcb)
	   (when at-eof
	     (if tcb
		 (error 'tcp-connection-no-more-data :connection tcb :reason "closing during an HTTP chunked read")
		 (error 'tcp-stream-closed :attempt "HTTP chunked read from" :stream self)))))
    (declare (inline maybe-signal-tcp-eof))
    (cond ((null chunked-input)
	   (continue-whopper no-wait-p))
	  (input-chunk-crosses-buffer-p
	   (multiple-value-setq (si:stream-input-buffer si:stream-input-index si:stream-input-limit at-eof)
	     (continue-whopper nil))		;always wait for data, or synchronization will be lost.   5/28/99 -- JCMa.
	   (maybe-signal-tcp-eof at-eof tcb)
	   (unless si:stream-input-buffer
	     (error "No data available while reading HTTP chunked input."))
	   (%note-chunk-continue)
	   ;; Update by withheld buffer window
	   (incf (bytes-received self) (- input-buffer-limit input-scan-end))
	   ;; Always return the binary buffer because :SETUP-NEXT-INPUT-BUFFER binds it.
	   (values si:stream-input-buffer si:stream-input-index input-scan-end at-eof))
	  (t (multiple-value-setq (si:stream-input-buffer si:stream-input-index si:stream-input-limit at-eof)
	       (continue-whopper nil))		;always wait for data, or synchronization will be lost.   5/28/99 -- JCMa.
	     (maybe-signal-tcp-eof at-eof tcb)
	     ;; Always return the binary buffer because :SETUP-NEXT-INPUT-BUFFER binds it.
	     (values si:stream-input-buffer si:stream-input-index si:stream-input-limit at-eof)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.473")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun update-chunk-transfer-decoded-headers (header-set stream content-length)
  (declare (values header-set))
  ;; add a content length header now that we know it
  (when content-length
    (push-header header-set :content-length content-length))
  ;; suppress the transfer encoding
  (suppress-header header-set :transfer-encoding t t)
  ;; read any trailers on the chunk
  (with-text-stream (stream :input)		;make sure stream in in text mode
    (resourced-read-headers *headers* stream))
  header-set)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.473")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun chunk-transfer-decoding-mode-finish (stream headers)
  (declare (values headers))
  (cond (headers
	 (let ((content-length (chunk-transfer-content-length stream)))
	   (www-utils:chunk-transfer-decoding-mode-end stream)
	   (update-chunk-transfer-decoded-headers headers stream content-length)))
	(t (www-utils:chunk-transfer-decoding-mode-end stream)
	   (read-headers stream)		;read and toss headers
	   nil)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.472")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-macro with-chunked-transfer-decoding ((stream &key (headers '*headers*)) &body body)
  "Automatically decodes chunked content transfer encodings.
On exit from BODY, HEADERS is extended to include content-length and any footers transmitted with
the chunked transfer. HEADERS must be amenable to generalized value setting with SETF." 
  `(let ((values nil))
     (unwind-protect
         (handler-case
           (progn (www-utils:chunk-transfer-decoding-mode ,stream)
                  (setq values (multiple-value-list (progn ,@body))))
           (www-utils:end-of-chunk-transfer-decoding ()))
       ,(if headers
	    `(setf ,headers (chunk-transfer-decoding-mode-finish ,stream ,headers))
	    `(chunk-transfer-decoding-mode-finish ,stream ,headers)))
     (values-list values)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.472")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-macro with-transfer-decoding ((stream url &key (headers '*headers*) content-length (copy-mode :binary)
                                              (stream-functions '(stream-copy-until-eof) )) &body body)
  "Automatically decodes transfer encodings." 
  `(let ((transfer-encoding (or (get-header :transfer-encoding ,headers) :fixed-length)))
     (case transfer-encoding
       (:fixed-length
         ,(%make-local-stream-function-for-with-transfer-decoding
            stream-functions content-length headers copy-mode body))
       (:chunked 
         (with-chunked-transfer-decoding (,stream :headers ,headers)
           ,@body))
       (t (error 'server-not-implemented :close-connection t :url ,url
                 :format-string "The HTTP transfer decoding, ~A, is not implemented."
                 :format-args (list transfer-encoding))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.472")
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
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.851")
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
      (with-header-values ( content-length content-range content-type last-modified transfer-encoding) headers
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

