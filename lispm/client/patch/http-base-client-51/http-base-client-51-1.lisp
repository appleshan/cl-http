;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Patch-File: T -*-
;;; Patch file for HTTP-BASE-CLIENT version 51.1
;;; Reason: Function (CLOS:METHOD HTTP::GET-USER-NAME+PW (URL:HTTP-URL STRING T T T) :AROUND):  -
;;; Variable HTTP::*PROMPT-USER-FOR-PASSWORDS*:  -
;;; Function HTTP::WITH-PASSWORD-PROMPTING:  -
;;; Function HTTP::WITHOUT-PASSWORD-PROMPTING:  -
;;; Written by JCMa, 10/20/01 16:41:23
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.6,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.152,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.4, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.4, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.38, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.3,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, HTTP Proxy Server 6.26,
;;; HTTP Client Substrate 4.19, Statice Server 466.2, HTTP Client 51.0,
;;; Image Substrate 440.4, Essential Image Substrate 433.0, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x976 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
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

;;; Patch file for HTTP-BASE-CLIENT version 51.1
;;; Written by JCMa, 11/01/01 20:32:47
;;; while running on Lispm Machine Thomas Jefferson from FEP0:>Server-CL-HTTP-70-16-C-MIT-8-5.ilod.1
;;; with System 452.22, CLOS 439.0, RPC 443.1, Embedding Support 435.0,
;;; MacIvory Support 447.0, UX Support 443.0, Development Utilities 439.0,
;;; Old TV 436.0, Zwei 436.0, Utilities 445.0, RPC Development 438.0,
;;; MacIvory Development 434.0, UX Development 442.0, Server Utilities 442.0,
;;; Serial 435.0, Hardcopy 446.0, Zmail 442.1, SCSI 430.0, Tape 444.3, LMFS 442.1,
;;; NSage 440.0, Extended Help 441.0, CL Developer 428.0,
;;; Documentation Database 440.12, IP-TCP 452.6, IP-TCP Documentation 422.0,
;;; CLX 450.0, X Remote Screen 448.3, X Documentation 421.1, NFS Client 442.0,
;;; NFS Server 439.0, NFS Documentation 423.1, Mailer 438.0, Print Spooler 439.0,
;;; Domain Name Server 436.0, Lock Simple 435.1, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, Conversion Tools 436.0,
;;; Metering 444.0, Metering Substrate 444.1, Hacks 440.0, CLIM 72.0,
;;; Genera CLIM 72.0, PostScript CLIM 72.0, Experimental CLIM Documentation 71.27,
;;; Statice Runtime 466.1, Statice 466.0, Statice Browser 466.0,
;;; Statice Server 466.2, 8-5-Patches 2.19, MAC 414.0, HTTP Server 70.152,
;;; Showable Procedures 36.3, Binary Tree 34.0, W3 Presentation System 8.1,
;;; CL-HTTP Server Interface 53.0, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.39, Genera 8 5 Macivory Support Patches 1.0,
;;; Genera 8 5 Mailer Patches 1.1, Genera 8 5 Domain Name Server Patches 1.1,
;;; Genera 8 5 Metering Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Clim Patches 1.3, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; HTTP Client Substrate 4.20, HTTP Proxy Server 6.27, HTTP Client 51.0,
;;; Image Substrate 440.4, Essential Image Substrate 433.0,
;;; W4 Constraint-Guide Web Walker 45.10, W4 Examples 15.0,
;;; Lambda Information Retrieval System 22.4,
;;; Experimental CL-HTTP CLIM User Interface 1.0, CL-HTTP Documentation 3.0,
;;; cold load 1, Ivory Revision 4A, IFEP 328, FEP0:>I328-loaders.flod(24),
;;; FEP0:>I328-info.flod(24), FEP0:>I328-debug.flod(24), FEP0:>I328-lisp.flod(25),
;;; FEP0:>I328-kernel.fep(44), Boot ROM version 320, Device PROM version 325,
;;; Genera application 5.6.1a1, MacIvory SCSI Manager Server 4.3.2a1,
;;; Toolbox Servers 4.2, MacIvory & RPC library 6.3.4a1,
;;; MacIvory life support 4.3.8a1, Symbolics keyboard 2.1.1a1,
;;; Macintosh System Software 8.1, 1024x700 Screen with Genera fonts,
;;; Machine serial number 30376, Macintosh Quadra 800, Symbolics Keyboard,
;;; Add support for Apple's Gestalt and Speech Managers. (from SYS:MAC;MACIVORY-SPEECH-SUPPORT.LISP.1),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.11),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).


(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL)
  (SCT:REQUIRE-PATCH-LEVEL-FOR-PATCH '(HTTP-CLIENT-SUBSTRATE 4. 20.)))


(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:CLIENT;SEXP-BROWSER.LISP.90"
  "HTTP:CLIENT;SEXP-BROWSER.LISP.91"
  "HTTP:CLIENT;SEXP-BROWSER.LISP.92")



;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.90")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;;;------------------------------------------------------------------- 
;;;
;;; HANDLE AUTHENTICATION
;;;

(defparameter *prompt-user-for-passwords* t
  "Controls whether the user is prompted for passwords.")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.90")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmacro with-password-prompting (() &body body)
  `(let ((*prompt-user-for-passwords* t)) . ,body))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.91")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmacro without-password-prompting (() &body body)
  `(let ((*prompt-user-for-passwords* nil)) . ,body))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.92")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod get-user-name+pw ((url url:http-url) (realm string) method proxy-p reprompt-p &optional (stream *terminal-io*))
  (declare (ignore reprompt-p))
  (cond (*prompt-user-for-passwords*
	 (let ((string (url:name-string url)))
	   #-(or Genera MCL)			;obsolete user prompt fails to display proxy and other info 9/7/2001 -- JCMa.
	   (%get-user-name+pw string stream)
	   #+(or Genera MCL)
	   (%get-user-name+pw string realm method proxy-p stream)))
	(t (call-next-method))))		;pass control to default method, which signals

