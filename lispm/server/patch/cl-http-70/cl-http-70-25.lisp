;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.25
;;; Reason: Function (CLOS:METHOD HTML2:ACCEPT-INPUT (HTML2:IMAGE T)):  fix argument calls to %write-command-key-arg.
;;; Function HTTP:WITH-MIME-MULTIPART-BLOCK:  be sure to call boundary-writer with argument of T on last block.
;;; Function NS11:WITH-SERVER-PUSH-RESPONSE:  update.
;;; Remove function (CLOS:METHOD HTTP:EXPORT-URL (URL:HTTP-MINIMUM-OBJECT (EQL :SHTML-FILE)) :AROUND): undefine.
;;; Function (CLOS:METHOD HTTP:EXPORT-URL (URL:HTTP-MINIMUM-OBJECT (EQL :SHTML-FILE))):  should be standard method.
;;; Written by JCMa, 2/26/00 02:27:33
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.15, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.24,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 41, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.11, DEC OSF/1 V4.0 (Rev. 110),
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
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;HTML2.LISP.291"
  "HTTP:SERVER;HEADERS.LISP.446"
  "HTTP:SERVER;NETSCAPE-1-1.LISP.128"
  "HTTP:SERVER;SERVER.LISP.812"
  "HTTP:SERVER;SERVER.LISP.813")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.291")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

(defmethod accept-input ((image image) query-name &rest args &key (stream *output-stream*) &allow-other-keys)
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
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.446")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmacro with-mime-multipart-block (multipart-keyword (stream &key last-block-p (force-output t) sleep-interval 
                                                               content-type content-length content-location last-modified
                                                               expires boundary) &body body)
  "Use this macro when BODY is writing blocks within a mime multipart document.
LAST-BLOCK-P should be non-null when writing the last block of a multipart mime message.
When FORCE-OUTPUT is non-null, output is forced on STREAM
after executing BODY. The HTTP 1.0 specification recommends including a CONTENT-LOCATION within multipart blocks.
Beware that CONTENT-LENGTH, CONTENT-LOCATION, LAST-MODIFIED, expires, and CONTENT-LOCATION can be multiply evaluated.
When SLEEP-INTERVAL is supplied, the process sleeps for SLEEP-INTERVAL seconds after executing the
body and forcing output if FORCE-OUTPUT is non-null."
  (let ((boundary-writer (if boundary
                             `(write-mime-multipart-boundary ,boundary ,stream)
                             `(write-mime-multipart-boundary 
                                (mime-multipart-boundary-string ,multipart-keyword) ,stream))))
    `(let ((headers `(:content-type ,,(typecase content-type
                                        (keyword `(quote ,(%mime-content-type-spec content-type)))
                                        (t `(%mime-content-type-spec ,content-type)))
                      ,,.(when content-length `(:content-length ,content-length))
                      ,,.(when content-location `(:content-location ,content-location))
                      ,,.(when last-modified `(:last-modified ,last-modified))
                      ,,.(when expires `(:expires ,expires)))))
       (declare (dynamic-extent headers))
       ,boundary-writer
       (write-headers ,stream headers t)
       (multiple-value-prog1
         (progn . ,body)
         ,.(when last-block-p `((,boundary-writer t)))
         ,.(when force-output `((force-output ,stream)))
         ,.(when (and sleep-interval (not last-block-p)) `((sleep ,sleep-interval)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;NETSCAPE-1-1.LISP.128")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: netscape1.1; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmacro with-server-push-response ((stream) &body body)
  "Use this macro to refresh the client display under the server control
from any response function. Each block must be written within a local macro WITH-BLOCK.
Typically an application, will call WITH-BLOCK in a loop that sleeps for
an interval between each iteration. WITH-BLOCK takes the arguments:
  (stream &key content-type content-length content-length content-location (force-output t))

    CONTENT-TYPE is required should be a keyword denoting a mime content type
                 (see: HTTP:MIME-CONTENT-TYPE-KEYWORDS)
    FORCE-OUTPUT should be non-null to transmit output to the client for display
                 after executing BODY.
    SLEEP-INTERVAL is the number of seconds to sleep after executing a block, but before
                 executing the next.

The other header arguments are optional, but recommended. WITH-SERVER-PUSH-RESPONSE
replaces any calls to HTTP:WITH-SUCCESSFUL-RESPONSE or variants."
  (let ((boundary (http::mime-multipart-boundary-string :multipart-mixed-replace)))
    `(macrolet ((with-block ((stream &key last-block-p (force-output t) sleep-interval
                                     content-type content-length content-location) &body body)
                  `(http::with-mime-multipart-block :multipart-mixed-replace
                                                    (,stream
                                                     :last-block-p ,last-block-p
                                                     :boundary ,,boundary
                                                     :force-output ,force-output
                                                     :sleep-interval ,sleep-interval
                                                     :content-type ,content-type
                                                     :content-length ,content-length
                                                     :content-location ,content-location)
                     ,@body)))
       (http:with-successful-response (,stream :multipart-mixed-replace
                                       :location (http:server-url http:*server*))
         (handler-case 
           (progn ,@body)
           (bad-connection-state () t))))))     ;exit via client abort


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.812")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(SCL:FUNDEFINE '(METHOD EXPORT-URL (HTTP-MINIMUM-OBJECT (EQL :SHTML-FILE)) :AROUND))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.813")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod export-url ((url url:http-minimum-object) (translation (eql :shtml-file)) &rest args)
  (apply #'export-url (url:initialize-specialization url 'url:http-template-object args)) translation args)

