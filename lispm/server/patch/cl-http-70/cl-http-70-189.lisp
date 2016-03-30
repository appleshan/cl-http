;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.189
;;; Reason: Function (CLOS:METHOD HTML2:ACCEPT-INPUT (HTML2:HIDDEN T)):  allow list-valued hidden fields.
;;; Function (CLOS:METHOD HTML2:ACCEPT-INPUT (HTML2:STRING+ T)):  dynamic extent on &rest args.
;;; Function (CLOS:METHOD HTML2:ACCEPT-INPUT (HTML2:MULTI-LINE-TEXT T)):  ditto
;;; Function (CLOS:METHOD HTML2:ACCEPT-INPUT (HTML2:IMAGE T)):  ditto
;;; Function (CLOS:METHOD HTML2:ACCEPT-INPUT (HTML2:RADIO-BUTTON T)):  ditto
;;; Function (CLOS:METHOD HTML2:ACCEPT-INPUT (HTML2:CHECKBOX T)):  ditto
;;; Function (CLOS:METHOD HTML2:ACCEPT-INPUT (HTML2:SELECT-CHOICES T)):  ditto
;;; Function (CLOS:METHOD HTML2:ACCEPT-INPUT (HTML2:SUBMIT-BUTTON T)):  -
;;; Function (CLOS:METHOD HTML2:ACCEPT-INPUT (HTML2:CLIENT-SIDE-BUTTON T)):  -
;;; Function (CLOS:METHOD HTML2:ACCEPT-INPUT (HTML2:FILE T)):  -
;;; Written by JCMa, 12/10/03 19:07:52
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5-inc1.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.6,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.188,
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
;;; Jcma 44, HTTP Client 51.7, HTTP Client Substrate 4.23, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, HTTP Proxy Server 6.34,
;;; W4 Constraint-Guide Web Walker 45.13, W4 Examples 15.0,
;;; Experimental CL-HTTP CLIM User Interface 1.1, CL-HTTP Documentation 3.0,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.18,
;;; DEC OSF/1 V4.0 (Rev. 110),
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;HTML2.LISP.304")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.304")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

;(defmethod accept-input ((hidden hidden) query-name &rest args &key (stream *output-stream*) &allow-other-keys)
;  (unless (getf args :default)
;    (error "No default value provided for a HIDDEN input type."))
;  (%issue-command ("INPUT" stream)
;    (write-standard-input-type-args (stream query-name hidden args :bind-default t))))

(defmethod accept-input ((hidden hidden) query-name &rest args &key (stream *output-stream*) &allow-other-keys)
  (declare (dynamic-extent args))
  (flet ((issue-hidden-input-command (hidden query-name value stream)
	   (declare (type hidden hidden))
	   (with-slots (type-arg) hidden
	     (%issue-command ("INPUT" stream)
               (%write-command-key-arg stream "TYPE" type-arg)
               (%write-command-key-arg stream "NAME" query-name)
               (check-value-type hidden value)
               (%write-command-key-arg stream "VALUE" value)))))
    (declare (inline issue-hidden-input-command))
    ;; Ignore args (disabled error events) because they don't make sense for hidden fields
    (destructuring-bind (&key default &allow-other-keys) args
      (if query-name
          (verify-query-name query-name)
	  (error "No QUERY-NAME provided for input type."))
      (etypecase default
        (null (error "No default value provided for a HIDDEN input type."))
        (atom (issue-hidden-input-command hidden query-name default stream))
        (cons (dolist (value default)
		(issue-hidden-input-command hidden query-name value stream)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.304")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

(defmethod accept-input ((string+ string+) query-name &rest args &key preamble (stream *output-stream*) &allow-other-keys)
  (declare (dynamic-extent args))
  (with-slots (default-size default-max-size) string+
    (%accept-input-write-preamble preamble string+ stream args)
    (%issue-command ("INPUT" stream)
      (write-standard-input-type-args (stream query-name string+ args :bind-default t)
	(destructuring-bind (&key size max-length &allow-other-keys) args
	  (let ((local-size (or size default-size))
		(max (or max-length default-max-size)))
	    (cond-every
	      (local-size
		(%write-command-key-arg stream "SIZE" local-size))
	      (max
		(unless (<= max 1024.)
		  (error "String fields cannot exceed 1024 characters in HMTL 2.0"))
		(%write-command-key-arg stream "MAXLENGTH" max t)))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.304")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

(defmethod accept-input ((multi-line-text multi-line-text) query-name &rest args &key (stream *output-stream*))
  (declare (dynamic-extent args))
  (with-slots (type-arg) multi-line-text
    (destructuring-bind (&key (rows 5.) (columns 72.) default events &allow-other-keys) args
      (check-type rows integer)
      (check-type columns integer)
      (%with-environment (type-arg :stream stream :fresh-line t)
                         (progn
                           (%write-command-key-arg stream "NAME" query-name)
                           (%write-command-key-arg stream "ROWS" rows t)
                           (%write-command-key-arg stream "COLS" columns t)
                           (dolist (event events)
                             (%write-input-type-event-arg stream event)))
        (etypecase default
          (null)
          (string (write-string default stream))
          (function (funcall default stream)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.304")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

(defmethod accept-input ((image image) query-name &rest args &key (stream *output-stream*) &allow-other-keys)
  (declare (dynamic-extent args))
  (%issue-command ("INPUT" stream)
    (write-standard-input-type-args (stream query-name image args :bind-default nil)
      (destructuring-bind (&key image-url align &allow-other-keys) args
        (unless (member align *image-alignment-values*)
          (error "Unknown image alignment, ~S, was specified." align))
        (%write-command-key-arg stream "SRC" (coerce-url-string image-url))
        (when align
          (%write-command-key-arg stream "ALIGN" align))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.304")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

;; enumerates from 0 to n
(defmethod accept-input ((radio-button radio-button) query-name &rest args &key (stream *output-stream*) &allow-other-keys)
  (declare (dynamic-extent args))
  (macrolet ((with-decoded-choice ((choice default values-provided-p count) &body body)
               `(multiple-value-bind (value choice-string check-p)
                    (if ,values-provided-p 
                        (values (cdr ,choice) (car ,choice) (equal (cdr ,choice) ,default))
                        (values (princ-to-string ,count) ,choice (equal ,choice ,default)))
                  ,@body)))
    (flet ((write-element (stream choice-string value check-p standard-args-writer)
             (%issue-command ("INPUT" stream :fresh-line t)
               (funcall standard-args-writer stream)
               (%write-command-key-arg stream "VALUE" value)
               (when check-p
                 (%write-command-key-arg stream "CHECKED")))
             (write-string choice-string stream)))
      (declare (inline write-element))
      (let ((standard-args-writer (standard-input-type-args-writer radio-button query-name args)))
        (declare (dynamic-extent standard-args-writer))
        (destructuring-bind (&key choices (linebreaks t) (enumeration :plain) compact default selected-choice
                                  &allow-other-keys) args
          (let ((values-provided-p (consp (first choices)))
                (default-value (or default selected-choice)))   ;selected-choice is obsolete  2/26/95 -- JCMa.
            (if linebreaks
                (with-enumeration (stream enumeration :compact compact)
                  (loop for choice in choices
                        for count upfrom 0
                        do (with-decoded-choice
                             (choice default-value values-provided-p count)
                             (enumerating-item (stream)
                               (write-element stream choice-string value check-p standard-args-writer)
                               (break-line :stream stream)))))
                (loop for choice in choices
                      for count upfrom 0
                      do (with-decoded-choice
                           (choice default-value values-provided-p count)
                           (write-element stream choice-string value check-p standard-args-writer))))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.304")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

;; enumerates from 0 to (1- n)
(defmethod accept-input ((checkbox checkbox) query-name &rest args &key (stream *output-stream*) &allow-other-keys)
  (declare (dynamic-extent args))
  (macrolet ((with-decoded-choice ((choice default values-provided-p count) &body body)
               `(multiple-value-bind (value choice-string check-p)
                    (if ,values-provided-p 
                        (values (cdr ,choice) (car ,choice) (member (cdr ,choice) ,default :test #'equal))
                        (values (princ-to-string ,count) ,choice (member ,choice ,default :test #'equal)))
                  ,@body)))
    (flet ((write-element (stream choice-string value check-p standard-args-writer)
             (%issue-command ("INPUT" stream :fresh-line t)
               (funcall standard-args-writer stream)
               (%write-command-key-arg stream "VALUE" value)
               (when check-p
                 (%write-command-key-arg stream "CHECKED")))
             (write-string choice-string stream)))
      (declare (inline write-element))
      (let ((standard-args-writer (standard-input-type-args-writer checkbox query-name args)))
        (declare (dynamic-extent standard-args-writer))
        (destructuring-bind (&key choices (linebreaks t) (enumeration :plain) compact default selected-choice
                                  &allow-other-keys) args
          (let ((values-provided-p (consp (first choices)))
                (default-value (or default selected-choice)))   ;selected-choice is obsolete  2/26/95 -- JCMa.
            (if linebreaks
                (with-enumeration (stream enumeration :compact compact)
                  (loop for choice in choices
                        for count upfrom 0
                        do (with-decoded-choice
                             (choice default-value values-provided-p count)
                             (enumerating-item (stream)
                               (write-element stream choice-string value check-p standard-args-writer)
                               (break-line :stream stream)))))
                (loop for choice in choices
                      for count upfrom 0
                      do (with-decoded-choice
                           (choice default-value values-provided-p count)
                           (write-element stream choice-string value check-p standard-args-writer))))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.304")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

;; if the size argument is present, the choices appear in a scrollable
;; indented choice window.
(defmethod accept-input ((select-choices select-choices) query-name &rest args &key (stream *output-stream*) &allow-other-keys)
  (declare (dynamic-extent args))
  (flet ((heuristic-size-default (choices)
           (let ((n (length choices))
                 (default-size *select-choices-max-default-size*))
             (declare (fixnum n default-size))
             (cond ((> n default-size)
                    (if (> n (* 5 default-size))
                        (* 2 default-size)
                        default-size))
                   ;;((< n 2) (error "Scrollable inset choices don't make sense for fewer than two choices."))
                   (t n))))
         (emit-item (stream choice value disabled default value-provided-p sequence-p events)
           (%issue-command ("OPTION" stream :fresh-line t)
             (cond-every
               ((and value-provided-p value)
                (%write-command-key-arg stream "VALUE" value))
               ((if sequence-p
                    (member value default :test #'equalp)
                    (equalp value default))
                (%write-command-key-arg stream "SELECTED"))
               (disabled
                 (%write-command-key-arg stream "DISABLED"))
               (events
                 (dolist (event events)
                   (%write-input-type-event-arg stream event)))))
           (write choice :escape nil :base 10. :stream stream)))
    (declare (dynamic-extent #'emit-item #'heuristic-size-default))
    (with-slots (type-arg) select-choices
      (destructuring-bind (&key choices default size (sequence-p (consp default)) events &allow-other-keys) args
        (cond ((null default))
              (sequence-p
               (unless (listp default)
                 (error "Default, ~S, is not a list, which is required when sequence-p is not null." default)))
              (t (unless (atom default)
                   (error "Default, ~S, is not an atom, which is required when sequence-p is null." default))))
        (%issue-command (type-arg stream :fresh-line t :trailing-line t)
          (if query-name
              (%write-command-key-arg stream "NAME" query-name)
              (error "No QUERY-NAME provided for input type."))
          (cond (sequence-p  ;; html spec says use "SEVERAL" but Mosaic uses "MULTIPLE"
                 (%write-command-key-arg stream "SIZE" (or size (heuristic-size-default choices)) t)
                 (%write-command-key-arg stream "MULTIPLE"))
                ((eq size :pull-down-menu)
                 (%write-command-key-arg stream "SIZE" 1 t))
                (size
                 ;; Scrollable inset choices don't make sense for fewer than two choices.
                 (when (< 1 size)
                   (%write-command-key-arg stream "SIZE" size t)))
                (t (%write-command-key-arg stream "SIZE" (heuristic-size-default choices) t)))
          (dolist (event events)
            (%write-input-type-event-arg stream event)))
        (loop for item in choices
              do (etypecase item
                   (cons 
                     (destructuring-bind (choice &key value disabled events &allow-other-keys) item
                       (emit-item stream choice value disabled default t sequence-p events)))
                   (atom (emit-item stream item item nil default nil sequence-p nil))))
        (issue-command type-arg stream t t))))) 


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.304")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

(defmethod accept-input ((reset-button reset-button) query-name &rest args &key (stream *output-stream*) &allow-other-keys)
  (declare (dynamic-extent args))
  (destructuring-bind (&key display-string image-url &allow-other-keys) args
    (%issue-command ("INPUT" stream)
      (write-standard-input-type-args (stream query-name reset-button args :bind-default nil)
        (%write-command-key-arg stream "VALUE" (or display-string "Reset"))
        (when image-url
          (%write-command-key-arg stream "SRC" (coerce-url-string image-url)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.304")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

(defmethod accept-input ((submit-button submit-button) query-name &rest args &key (stream *output-stream*) &allow-other-keys)
  (declare (dynamic-extent args))
  (destructuring-bind (&key display-string image-url &allow-other-keys) args
    (%issue-command ("INPUT" stream)
      (write-standard-input-type-args (stream query-name submit-button args :bind-default nil)
        (%write-command-key-arg stream "VALUE" (or display-string "Submit"))
        (when image-url
          (%write-command-key-arg stream "SRC" (coerce-url-string image-url)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.304")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

(defmethod accept-input ((client-side-button client-side-button) query-name &rest args &key (stream *output-stream*) &allow-other-keys)
  (declare (dynamic-extent args))
  (destructuring-bind (&key display-string image-url &allow-other-keys) args
    (%issue-command ("INPUT" stream)
      (write-standard-input-type-args (stream query-name client-side-button args :bind-default nil)
        (%write-command-key-arg stream "VALUE" (or display-string "Submit"))
        (when image-url
          (%write-command-key-arg stream "SRC" (coerce-url-string image-url)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.304")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

;;; extra error checking AND encoding the directory into the query-name.
(defmethod accept-input ((file file) query-name &rest args &key (stream *output-stream*) &allow-other-keys)
  (declare (dynamic-extent args))
  (cond ((equal *open-form* '(:multipart :form-data)))
	((null *open-form*)
	 (error "~S ~S should be within a ~S." 'accept-input 'file 'with-fillout-form))
	(t (error "~S ~S requires the :ENCODING-TYPE of ~S to be ~S."
		  'accept-input 'file 'with-fillout-form '(:multipart :form-data) )))
  (with-slots (default-size default-max-size) file
    (destructuring-bind (&key size max-length content-type (directory *file-upload-default-directory*) &allow-other-keys) args
      (let ((directory-query-name (file-upload-make-query query-name directory)))
	(declare (dynamic-extent directory-query-name))
	(%issue-command ("INPUT" stream)
	  (html2::write-standard-input-type-args (stream directory-query-name file args :bind-default t)
	    (let ((local-size (or size default-size))
		  (max (or max-length default-max-size)))
	      (cond-every
		(local-size
		  (%write-command-key-arg stream "SIZE" local-size))
		(max
		  (unless (< max 1024.)
		    (error "String fields cannot exceed 1024 characters in HMTL 2.0"))
		  (%write-command-key-arg stream "MAXLENGTH" max t))
		(content-type
		  (fast-format stream " ACCEPT=")
		  (write-char #\" stream)
		  (etypecase content-type
		    (cons
		      (etypecase (car content-type)
			(keyword (http::print-mime-content-type-header content-type stream))
			(cons (http::print-mime-content-type-sequence-header content-type stream))))
		    (keyword
		      (http::print-mime-content-type-header (http::%mime-content-type-spec content-type) stream))
		    (string
		      (write-string content-type stream)))
		  (write-char #\" stream)))))))))) 

