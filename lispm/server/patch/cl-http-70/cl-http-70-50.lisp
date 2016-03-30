;;; -*- Mode: lisp; Syntax: common-lisp; Package: user; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.50
;;; Reason: Function HTTP::PRINT-VIA-HEADER:  -
;;; Function HTML::%WRITE-MAKE-FONT-ARGS:  handle negative relative sizez.
;;; Function HTTP::%EXECUTE-REQUEST:  don't escape URLs and finese broken search URLS.
;;; Function WWW-UTILS:CLEAN-UP-CONTIGUOUS-FILE-VERSIONS:  don't error if there is no pathname there.
;;; Written by JCMa, 5/23/00 19:29:23
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.49,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42, HTTP Proxy Server 5.12,
;;; HTTP Client Substrate 3.11, W4 Constraint-Guide Web Walker 41.3,
;;; Symbolics Concordia 444.0, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, Graphic Editor 440.0, Graphic Editing 441.0,
;;; Bitmap Editor 441.0, Graphic Editing Documentation 432.0, Postscript 436.0,
;;; Concordia Documentation 432.0, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.19, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Documentation Patches 1.0,
;;; Genera 8 5 Clim Patches 1.0, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; Genera 8 5 Concordia Patches 1.0, Genera 8 5 Concordia Doc Patches 1.0,
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
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7),
;;; Add clos to mx list methods (from W:>Reti>add-clos-to-mx-list-methods.lisp.1),
;;; Change random host default (from W:>Reti>change-random-host-default.lisp.3).

;;; Patch file for CL-HTTP version 70.50
;;; Written by JCMa, 5/23/00 21:55:56
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.49,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42, HTTP Proxy Server 5.12,
;;; HTTP Client Substrate 3.12, W4 Constraint-Guide Web Walker 41.4,
;;; Symbolics Concordia 444.0, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, Graphic Editor 440.0, Graphic Editing 441.0,
;;; Bitmap Editor 441.0, Graphic Editing Documentation 432.0, Postscript 436.0,
;;; Concordia Documentation 432.0, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.19, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Documentation Patches 1.0,
;;; Genera 8 5 Clim Patches 1.0, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; Genera 8 5 Concordia Patches 1.0, Genera 8 5 Concordia Doc Patches 1.0,
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
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7),
;;; Add clos to mx list methods (from W:>Reti>add-clos-to-mx-list-methods.lisp.1),
;;; Change random host default (from W:>Reti>change-random-host-default.lisp.3).




(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;HEADERS.LISP.460"
  "HTTP:SERVER;HTML-3-2.LISP.35"
  "HTTP:SERVER;SERVER.LISP.837"
  "HTTP:LISPM;SERVER;LISPM.LISP.465")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.460")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun print-via-header (header-value-plist stream)
  (using-resource (string line-buffer *line-buffer-size*)
    (flet ((entry-string (value)
	     (destructuring-bind (protocol (received-by &optional port) &optional comment) value
	       (let* ((protocol-string (symbol-name protocol))
		      (e1 (length protocol-string))
		      (s1 (if (and *elide-http-version-in-via-header*
				   (< 5 e1)
				   (string-equal "HTTP/" protocol-string :start1 0 :end1 5 :start2 0 :end2 5))
			      5
			      0))
		      (l1 (- e1 s1))
		      (s2 (1+ l1))
		      (l2 (length received-by))
		      (e2 (+ s2 l2))
		      (port-string (and port (write-to-string port :base 10 :escape nil))) 
		      s3 e3 l3 s4 e4 l4)
		 (declare (fixnum s1 l1 e1 s2 l2 e2)
			  (dynamic-extent port-string))
		 (cond-every
		   (port (setq s3 (the fixnum (1+ e2))
			       l3 (length port-string)
			       e3 (the fixnum (+ s3 l3))))
		   (comment
		     (setq l4 (length comment)
			   s4 (the fixnum (+ 2 (or e3 e2)))
			   e4 (the fixnum (+ s4 (the fixnum l4))))))
		 (setf (fill-pointer string) (if e4 (1+ e4) (or e4 e3 e2)))
		 (copy-vector-portion protocol-string s1 e1 string 0 l1)
		 (setf (aref string l1) #\space)
		 (copy-vector-portion received-by 0 l2 string s2 e2)
		 (cond-every
		   (port
		     (setf (aref string e2) #\:)
		     (copy-vector-portion port-string 0 l3 string s3 e3))
		   (l4				;comment
		     (setf (aref string (the fixnum (- s4 2))) #\space
			   (aref string (the fixnum (1- s4))) #\()
		     (copy-vector-portion comment 0 l4 string s4 e4)
		     (setf (aref string e4) #\))))
		 string))))
      (declare (dynamic-extent #'entry-string))
      (print-comma-separated-header header-value-plist stream #'entry-string)))) 


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML-3-2.LISP.35")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: html3.2; -*-")

(defun %write-make-font-args (stream size relative-p color)
  (declare (fixnum size))
  (cond-every
    (size
      (unless (if relative-p (< -8 size 8) (< 0 size 8))
        (error "SIZE, ~S, is not an integer from 1 through 7." size))
      (cond (relative-p
	     (if (plusp size)
		 (fast-format stream " SIZE=+~D" size)
		 (fast-format stream " SIZE=-~D" (- size))))
	    (t (%write-command-key-arg stream "SIZE" size t))))
    (color
      (%write-command-key-arg stream " COLOR" (color-mapping color t)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.837")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun %execute-request (server request stream)
  (declare (values persistent-connection-p)
	   (optimize (speed 3)))
  (flet ((invoke-local-service (server url-string method http-version)
           (multiple-value-bind (request-context bind-context-p)
               (request-local-context url-string http-version *headers*)
             (setf (server-url-string server) (%merge-url url-string request-context t))
             (if bind-context-p
                 (with-virtual-host-local-context (request-context)
                   (invoke-server-method server method http-version))
                 (invoke-server-method server method http-version)))))
    (declare (inline invoke-local-service))
    ;; parse the request
    (multiple-value-bind (method url-string http-version)
	(parse-request request 0 (length request) (server-url-buffer server))
      (unless (and method url-string http-version)
	(error 'bad-syntax-provided :method method :url url-string
	       :format-string "Bad HTTP Request: ~S"
	       :format-args (list request)))
      (unless (member http-version '(:http/1.1 :http/1.0 :http/0.9))
	(error 'http-version-not-supported :url url-string
	       :format-string "The server does not support ~A."
	       :format-args (list http-version)))
      (setf (server-method server) method
	    (server-http-version server) http-version
	    (server-url-string server) url-string
	    (server-status server) 200)		;anything other than 200 must reset the status value.
      (without-connection-overflows (url-string)
	(let ((*headers* (resourced-read-headers (server-headers server) stream)))
	  (cond ;; scheme prefixed URL Proxy path
	    ((url:scheme-prefixed-url-p url-string)
	     (setf (server-url-string server) url-string)
	     (let ((url:*escape-search-urls* nil)	;pass through escaping
		   (url:*search-url-signal-bad-form-encoding* nil)	;pass through broken URL- form encoded URLs.
		   (uri (intern-url url-string :if-does-not-exist *proxy-intern-url-status*)))
	       (cond  ;; Actually a local reference, start over as 
		 ((url:local-url-p uri)
		  (invoke-local-service server url-string method http-version))
		 (*proxy-service*
		  (setf (server-url server) uri)
		  (invoke-proxy-service server uri method http-version))
		 (t (error 'access-forbidden 
			   :format-string "HTTP Proxy service is currently unavailable on ~A (~D)." 
			   :format-args (list (local-host-domain-name) (server-host-local-port server))
			   :method method :url uri)))))
	    ;; Standard path, call the primary server method.
	    (t (invoke-local-service server url-string method http-version))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.465")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(define clean-up-contiguous-file-versions (pathname &optional (keep-versions 1) (expunge-p t))
  "Deletes all contiguous earlier versions PATHNAME keeping the KEEP-VERSIONS most recent."
  (let ((latest (scl:send (scl:send pathname :new-version :newest) :truename)))
    (when latest
      (loop with files-deleted-p 
	    for version downfrom (- (scl:send latest :version) keep-versions) above 0
	    for file = (scl:send pathname :new-version version)
	    do (handler-case
		 (progn (scl:send file :delete)
			(setq files-deleted-p t))
		 (fs:file-not-found () (loop-finish)))
	    finally (when (and files-deleted-p expunge-p)
		      (scl:send pathname :expunge))))))

