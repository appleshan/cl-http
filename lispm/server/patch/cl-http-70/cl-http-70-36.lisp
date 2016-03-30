;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.36
;;; Reason: Function (FLAVOR:METHOD TCP::NOTE-LAST-CHUNK TCP::CHUNK-TRANSFER-ENCODING-OUTPUT-STREAM-MIXIN):  
;;; Ensure that HTTP stream is in ascii output mode before attempting to write footers.
;;; Written by JCMa, 5/01/00 22:04:53
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.35,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 41, HTTP Client Substrate 3.6,
;;; HTTP Proxy Server 4.6, HTTP Client 49.7, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, W4 Constraint-Guide Web Walker 41.3,
;;; W4 Examples 13.0, Ivory Revision 5, VLM Debugger 329, Genera program 8.11,
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
  "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.164")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.164")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

#+Genera
(defmethod (note-last-chunk chunk-transfer-encoding-output-stream-mixin) (&optional footers-plist)
  ;; Make sure that there is an output buffer to avoid fencepost errors  1/23/97 -- JCMa.
  (unless si:stream-output-buffer (send self :setup-new-output-buffer))
  ;; Prevent chunked output buffer setup from repeating and prevent send output from reinserting chunk args
  (setq chunk-output nil)
  (cond ;; if no data written to body yet, convert to end header
    ((= chunk-body-start si:stream-output-index)
     (unless (< 3 (- si:stream-output-limit si:stream-output-index)) (send self :setup-new-output-buffer))
     (setf (aref si:stream-output-buffer si:stream-output-index) #.(si:ascii-code #\0))
     (write-8-bit-crlf si:stream-output-buffer (incf si:stream-output-index 1) (incf si:stream-output-index 2)))
    (t (multiple-value-setq (si:stream-output-index)
         (%note-body-end si:stream-output-buffer si:stream-output-index))
       (setf (aref si:stream-output-buffer si:stream-output-index) #.(si:ascii-code #\0))
       (incf si:stream-output-index)
       (write-8-bit-crlf si:stream-output-buffer si:stream-output-index (incf si:stream-output-index 2))))
  ;; ensure that we're in ascii mode before writing footers
  (ascii-output-mode self)
  ;; write the footers
  (http::write-headers self footers-plist t))

