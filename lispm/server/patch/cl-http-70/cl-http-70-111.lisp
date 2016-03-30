;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.111
;;; Reason: Function HTML2:WITH-FILLOUT-FORM:  fix default ENCODING-TYPE to be :x-www-form-urlencoded per HTML 3.2 and 4.0.1 specs.
;;; Function HTML3.2:WITH-FILLOUT-FORM:  ditto.
;;; Function HTTP::%COMPUTE-CONFIGURE-SERVER:  update.
;;; Function (CLOS:METHOD HTTP:POST-DOCUMENT (URL:FORM-PROCESSING-MIXIN (EQL :APPLICATION) (EQL :WWW-FORM-URLENCODED) T)):  handle non-x version.
;;; Written by JCMa, 2/28/01 14:38:52
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.110,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.25, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.1,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Lock Simple Patches 1.0, Jcma 42,
;;; HTTP Proxy Server 6.14, HTTP Client Substrate 4.7, Statice Server 466.2,
;;; W4 Constraint-Guide Web Walker 45.9, Ivory Revision 5, VLM Debugger 329,
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.7),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1),
;;; Ansi common lisp as synonym patch (from W:>reti>ansi-common-lisp-as-synonym-patch.lisp.9),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;HTML2.LISP.293"
  "HTTP:SERVER;HTML-3-2.LISP.38"
  "HTTP:SERVER;WEB-CONFIGURATION.LISP.41"
  "HTTP:SERVER;SERVER.LISP.886")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.293")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

(eval-when (:compile-toplevel :load-toplevel :execute)

(define-macro with-fillout-form ((action target &key name (stream '*output-stream*)
                                         (encoding-type ''(:application :x-www-form-urlencoded))
                                         events) &body body)
  "Establishes an fillout-form environment.  
ACTION is either :POST, :MAIL, or :GET, or :NONE.
NAME is a name identifying the form element.
TARGET is the URL to which the form values are returned.
If ACTION is :NONE, TARGET can be NIL.
EVENTS is a list of client-side events processed when the form is submitted.

ENCODING-TYPE is MIME content type to use when return the form values to TARGET.
ENCODING-TYPE defaults to application/x-www-form-urlencoded.
See ACCEPT-INPUT for documentation on client-side events.             
:GET should only be used in exceptional circumstances as not only is
it considered obsolete but it also is limited to 1024 characters 
including the rest of the the Target URL."
  `(cond (*open-form*
          (error "HTML does not allow nesting of forms."))
         (t (%with-environment ("FORM" :fresh-line t :stream ,stream)
                               (write-form-command-args ,stream ,action ,target ,encoding-type
                                                        ,name ,events)
              (let ((*open-form* t))
                ,@body)))))
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML-3-2.LISP.38")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: html3.2; -*-")

;;;------------------------------------------------------------------- 
;;;
;;; FILLOUT FORMS
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)

(define-macro with-fillout-form ((action target &key name (stream '*output-stream*)
                                         (encoding-type ''(:application :x-www-form-urlencoded))) &body body)
  "Establishes an fillout-form environment.  
ACTION is either :POST, :MAIL, or :GET, or :NONE.
NAME is a name identifying the form element.
TARGET is the URL to which the form values are returned.
If ACTION is :NONE, TARGET can be NIL.

ENCODING-TYPE is MIME content type to use when return the form values to TARGET.
ENCODING-TYPE defaults to application/x-www-form-urlencoded.
See ACCEPT-INPUT for documentation on client-side events.             
:GET should only be used in exceptional circumstances as not only is
it considered obsolete but it also is limited to 1024 characters 
including the rest of the the Target URL."
  `(html2:with-fillout-form (,action ,target :name ,name :stream ,stream :encoding-type ,encoding-type)
     ,@body))
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;WEB-CONFIGURATION.LISP.41")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun %compute-configure-server (url stream)
  (flet ((write-heading (stream)
           (write-string "Configure CL-HTTP Server" stream)
           (break-line :stream stream)
           (write-string (local-context) stream)))
    (with-html-document (:declare-dtd-version-p t :stream stream)
      (with-document-preamble (:stream stream)
        (declare-base-reference url :stream stream)
        (declare-title (concatenate 'string "Configure CL-HTTP Server" ) :stream stream))
      (with-standard-document-body (:stream stream) 
        (with-section-heading (#'write-heading :stream stream)
          (write-string "This form allows maintainers to perform server configuration over the Web."
                        stream)
          (horizontal-line :stream stream)
          (with-paragraph (:stream stream)
            (let ((types (preference-types)))
              (if types
                  (with-fillout-form (:post url :stream stream)
                    (show-preferences stream types)
                    (horizontal-line :stream stream)
                    (with-paragraph (:stream stream)
                      (with-verbatim-text (:fresh-line nil :stream stream)
                        (with-rendition (:bold :stream stream)
                          (write-string "Action:  " stream))
                        (accept-input 'reset-button "RESET" :display-string "Reset" :stream stream)
                        (write-string "  " stream)
                        (accept-input 'submit-button "SUBMIT" :display-string "Configure" :stream stream))))
                  (write-string "No preference information available" stream))))
          (horizontal-line :stream stream)
          (cl-http-signature stream))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.886")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod post-document ((url url:form-processing-mixin) (type (eql :application)) (subtype (eql :www-form-urlencoded)) stream)
  (%post-document-handle-url-coded-form url stream))

