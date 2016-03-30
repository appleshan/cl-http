;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for HTTP-BASE-CLIENT version 51.7
;;; Reason: Function HTTP::%PUSH-ENTRY-URL-ENCODED-VECTOR: escape special characters so as not to lose posting forms.
;;; Function HTTP::%URL-ENCODED-VECTOR:  allow vector to be passed in.
;;; Function HTTP::URL-ENCODED-VECTOR-FROM-ALIST:  add vector arg.
;;; Function HTTP::URL-ENCODED-VECTOR-FROM-PLIST:  ditto.
;;; Function HTTP::WITH-URL-ENCODED-VECTOR:  new macro enables resourced form posting and reduced consing.
;;; Function (CLOS:METHOD HTTP:POST-URL (T CONS)):  use with-url-encoded-vector
;;; Function (CLOS:METHOD HTTP:POST-FORM-VALUES (T)):  call the resourced method.
;;; Written by JCMa, 11/18/03 20:29:24
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5-inc1.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.6,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.185,
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
;;; Jcma 44, HTTP Client 51.6, HTTP Client Substrate 4.23, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.18, DEC OSF/1 V4.0 (Rev. 110),
;;; 1585x1133 1-bit STATIC-GRAY X Screen RELATUS:0.0 with 224 Genera fonts (eXodus 8.0  (c) 2001 Powerlan USA,
;;; Inc. R7100),
;;; 1152x696 24-bit TRUE-COLOR X Screen FUJI:2.0 with 224 Genera fonts (The Olivetti & Oracle Research Laboratory R3323),
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
  "HTTP:CLIENT;SEXP-BROWSER.LISP.108")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.108")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;;;------------------------------------------------------------------- 
;;;
;;; POST URL 
;;; 

(defun %push-entry-url-encoded-vector (vector key value)
  (labels ((vector-safe-push-extend (char vector)
             (cond ((escaped-character-p char)
                    (let ((digit-chars (escape-character char 'cons)))
                      (declare (dynamic-extent digit-chars))
                      (dolist (ch digit-chars)
                        (vector-push-extend (char-code ch) vector))))
                   (t (vector-push-extend (char-code char) vector))))
           (push-key (key vector)
             (with-fast-array-references ((key key string))
               (loop for idx upfrom 0 below (length key)
                     for char = (aref key idx)
                     do (vector-safe-push-extend char vector)
                     finally (vector-push-extend #.(char-code #\=) vector))))
           (push-value (value vector)
             (with-fast-array-references ((value value string))
               (loop for idx upfrom 0 below (length value)
                     for char = (aref value idx)
                     do (case char
                          (#\newline ;; CRLF encode
			   (vector-push-extend #.(char-code #\Return) vector)
			   (vector-push-extend #.(char-code #\Linefeed) vector))
                          (#.(if (eql #\newline #\Return) #\Linefeed #\Return)) ;ignore other delimiter
                          (t (vector-safe-push-extend char vector)))
                     finally (vector-push-extend #.(char-code #\&) vector)))))
    (declare (inline vector-safe-push-extend push-key))
    ;; push the key
    (push-key (etypecase key
		(symbol (symbol-name key))
		(string key))
	      vector)
    ;; push the value
    (typecase value
      (string (push-value value vector))
      (pathname
	(with-open-file (file value :direction :input :element-type *standard-character-type* :if-does-not-exist :error)
	  (let ((size (file-stream-length-in-bytes file)))
	    (using-resource (buffer post-form-buffer size)
	      (setq buffer (crlf-stream-copy-into-string file size 0 buffer))
	      (push-value buffer vector)))))
      (t (let ((val-string (write-to-string value :base 10. :escape nil)))
           (declare (dynamic-extent val-string)) 
           (push-value val-string vector))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.108")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmacro %url-encoded-vector (source type vector vector-size)
  `(loop with vector = (or ,vector 
                           (make-array ,vector-size :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))
	   ,@(ecase type
	       (:plist `(for (key value) on ,source by (function cddr)))
	       (:alist `(for (key value) in ,source)))
	 do (%push-entry-url-encoded-vector vector key value)
	 finally (return vector)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.108")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun url-encoded-vector-from-alist (alist &optional vector (size *post-form-buffer-size*))
  (%url-encoded-vector alist :alist vector size))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.108")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun url-encoded-vector-from-plist (plist &optional vector (size *post-form-buffer-size*))
  (%url-encoded-vector plist :plist vector size))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.108")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmacro with-url-encoded-vector ((vector spec &optional (size '*post-form-buffer-size*)) &body body)
  `(let ((spec ,spec))
     (using-resource (,vector data-cache-array ,size) 
       (etypecase (car spec)
         (atom (url-encoded-vector-from-plist spec))
         (cons (url-encoded-vector-from-alist spec)))
       ,@body)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.108")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod post-url (url (list cons) &key headers (stream *standard-output*))
  (with-url-encoded-vector (vector list)
    (post-url url vector :headers headers :stream stream)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.108")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod post-form-values (url &rest args &key headers (stream *standard-output*) &allow-other-keys)
  (declare (dynamic-extent args))
  (let ((plist (loop for (key val) on args by #'cddr
                     unless (member key '(:headers :stream))
		       collect key
		       and collect val)))
    (declare (dynamic-extent plist))
    (post-url url plist :headers headers :stream stream)))

