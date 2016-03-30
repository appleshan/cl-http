;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.127
;;; Reason: Function HTTP:WITH-MIME-MULTIPART-BLOCK:  -
;;; Written by JCMa, 5/01/01 14:17:22
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.126,
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
  "HTTP:SERVER;HEADERS.LISP.500")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.500")
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
         ,.(when last-block-p `((,@boundary-writer t)))
         ,.(when force-output `((force-output ,stream)))
         ,.(when (and sleep-interval (not last-block-p)) `((sleep ,sleep-interval)))))))

