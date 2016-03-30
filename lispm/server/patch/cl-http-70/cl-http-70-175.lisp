;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for CL-HTTP version 70.175
;;; Reason: Function HTML2::WITH-ENVIRONMENT:  recompile
;;; Function HTML:WITH-HTML-DOCUMENT:  -
;;; Written by ZiPpY, 10/07/2003 20:51:29
;;; while running on Lisp Machine Woodrow Wilson from FEP11:>Inc-Genera-8-5-from-SERVER-C-etc.ilod.1
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
;;; Statice Server 466.2, 8-5-Patches 2.19, MAC 414.0, HTTP Server 70.173,
;;; Showable Procedures 36.3, Binary Tree 34.0, W3 Presentation System 8.1,
;;; CL-HTTP Server Interface 53.0, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.41, Genera 8 5 Macivory Support Patches 1.0,
;;; Genera 8 5 Mailer Patches 1.1, Genera 8 5 Domain Name Server Patches 1.1,
;;; Genera 8 5 Metering Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Clim Patches 1.3, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; Experimental Pop3 Server NEWEST, Lambda Information Retrieval System 22.5,
;;; HTTP Client 51.5, HTTP Client Substrate 4.23, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, cold load 1, Ivory Revision 4A (FPA enabled),
;;; IFEP 333, FEP11:>I333-loaders.flod(4), FEP11:>I333-debug.flod(4),
;;; FEP11:>I333-info.flod(4), FEP11:>I333-lisp.flod(4), FEP11:>I333-kernel.fep(4),
;;; Boot ROM version 316, Device PROM version 330,
;;; 1585x1115 1-bit STATIC-GRAY X Screen RELATUS:0.0 with 224 Genera fonts (eXodus 8.0  (c) 2001 Powerlan USA,
;;; Inc. R7100), 1067x748 B&W Screen, Machine serial number 577,
;;; Add support for Apple's Gestalt and Speech Managers. (from SYS:MAC;MACIVORY-SPEECH-SUPPORT.LISP.1),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Lmfs add buffer at end patch (from W:>reti>lmfs-add-buffer-at-end-patch.lisp.1),
;;; Hostname in notifications title (from W:>reti>hostname-in-notifications-title.lisp.1),
;;; Server-Finger patch (from W:>file-server>server-finger.lisp.35),
;;; Deny some hosts access to some servers. (from W:>naha>patches>host-service-access-control.lisp.4),
;;; Mailer bandaid patch (from W:>reti>mailer-bandaid-patch.lisp.8),
;;; make sure things are flavor reachable-mailer-host (from W:>hes>fixes>yet-another-mailer-bug.lisp.2),
;;; Smtp accept reject patch (from W:>reti>wilson-smtp-accept-reject-patch.lisp.21),
;;; Nfs server patches (from W:>reti>nfs-server-patches.lisp.6),
;;; Tape spec patch (from W:>Reti>tape-spec-patch),
;;; Set dump dates on compare patch (from W:>reti>set-dump-dates-on-compare-patch.lisp.75),
;;; Domain try harder patch (from W:>reti>domain-try-harder-patch.lisp.6),
;;; Find free ephemeral space patch (from W:>reti>find-free-ephemeral-space-patch.lisp.3),
;;; Make native domain host patch (from W:>reti>make-native-domain-host-patch.lisp.2),
;;; Telnet naws patch (from W:>reti>telnet-naws-patch.lisp.9),
;;; Don't force in the mail-x host (from W:>comlink>v-6>lispm>mailer>mailbox-format),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch),
;;; Background dns refreshing (from W:>Reti>background-dns-refreshing),
;;; Cname level patch (from W:>reti>cname-level-patch),
;;; Newer xl ethernet patch (from W:>reti>newer-xl-ethernet-patch.lisp.8),
;;; Disable routing mail addresses (from W:>reti>disable-routing-mail-addresses.lisp.1),
;;; Recognize aaaa record patch (from W:>reti>recognize-aaaa-record-patch.lisp.1).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;HTML2.LISP.302"
  "HTTP:SERVER;HTML-3-2.LISP.39")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.302")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

(defmacro with-environment ((environment &key arguments close-command (fresh-line t)
                                         string-for-null-stream (stream '*output-stream*))
                            &body body)
  (let ((code `(multiple-value-prog1
                 (progn ,.(if arguments
                              `((issue-command* ,environment ,arguments ,stream ,fresh-line))
                              `(,@(when fresh-line `((fresh-line ,stream)))
                                (environment ,environment ,stream)))
                        ,@body)
                 (environment ,(or close-command environment) ,stream t)
                 ,.(when fresh-line `((fresh-line ,stream))))))
    (if string-for-null-stream
        `(with-string-for-null-stream (,stream :inline ,(eq string-for-null-stream :inline))
           ,code)
        code)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML-3-2.LISP.39")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: html3.2; -*-")

(define-macro with-html-document ((&key (stream '*output-stream*) declare-dtd-version-p) &body body)
  "Asserts the contents of BODY is an HTML document.
DECLARE-DTD-VERSION-P will declare the version of the DTD implemented by the current generation
package. This should be T whenever generation strictly conforms to the HTML version
associated with the macro WITH-HTML-DOCUMENT. When extension tags or features are used that
do not appear in the HTML DTD, DECLARE-DTD-VERSION-P should always be NIL."
  (if declare-dtd-version-p
      `(progn (declare-html-version ,stream)
              (with-environment ("HTML" :stream ,stream)
                ,@body))
      `(with-environment ("HTML" :stream ,stream)
         ,@body))) 

