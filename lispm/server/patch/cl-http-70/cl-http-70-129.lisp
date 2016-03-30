;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for CL-HTTP version 70.129
;;; Reason: Function (CLOS:METHOD HTTP:STREAM-DECODE-CRLF-UNTIL-EOF (TCP::TCP-MODAL-HTTP-STREAM SYS:LEXICAL-CLOSURE)):  specialized Lispm version for string output streams
;;; Written by Reti, 5/08/01 05:55:36
;;; while running on RAINIER-2 from RAINIER:/com/alpha/genera/sys.sct/worlds/color+clim.vlod
;;; with Open Genera 2.0, Genera 8.5, Logical Pathnames Translation Files NEWEST,
;;; Experimental Genera 8 5 Patches 1.0, Genera 8 5 System Patches 1.26,
;;; Genera 8 5 Clim Patches 1.2, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clx Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Clim Demo Patches 1.0,
;;; Genera 8 5 Color Patches 1.1, Genera 8 5 Images Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 C Patches 1.0, Color 427.1,
;;; Graphics Support 431.0, Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Color System Documentation 10.0, SGD Book Design 10.0, Images 431.2,
;;; Image Substrate 440.4, CLIM 72.0, Genera CLIM 72.0, CLX CLIM 72.0,
;;; PostScript CLIM 72.0, CLIM Demo 72.0, CLIM Documentation 72.0,
;;; HTTP Server 70.128, Showable Procedures 36.3, Binary Tree 34.0,
;;; W3 Presentation System 8.1, C 440.0, Lexer Runtime 438.0, Lexer Package 438.0,
;;; Minimal Lexer Runtime 439.0, Lalr 1 434.0, Context Free Grammar 439.0,
;;; Context Free Grammar Package 439.0, C Runtime 438.0,
;;; Compiler Tools Package 434.0, Compiler Tools Runtime 434.0, C Packages 436.0,
;;; Syntax Editor Runtime 434.0, C Library Headers 434,
;;; Compiler Tools Development 435.0, Compiler Tools Debugger 434.0,
;;; Experimental C Documentation 427.0, Syntax Editor Support 434.0,
;;; LL-1 support system 438.0, Experimental Jpeg Lib 1.0, HTTP Client 50.7,
;;; HTTP Client Substrate 4.10, Experimental I Sim 4 6, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.16, DEC OSF/1 V4.0 (Rev. 67),
;;; 1152x872 24-bit TRUE-COLOR X Screen INTERNET|128.52.39.113:0.0 with 0 Genera fonts (The XFree86 Project,
;;; Inc R40099003), Machine serial number 6292827,
;;; Test permit patch (from KRI:KRI;TEST-PERMIT-PATCH.LISP.3),
;;; Add clos to mx list methods (from KRI:KRI;ADD-CLOS-TO-MX-LIST-METHODS.LISP.1),
;;; Telnet naws patch (from KRI:KRI;TELNET-NAWS-PATCH.LISP.9),
;;; Its end of line patch (from KRI:KRI;ITS-END-OF-LINE-PATCH.LISP.3),
;;; Unix inbox from spoofing patch (from KRI:KRI;UNIX-INBOX-FROM-SPOOFING-PATCH.LISP.21),
;;; hack to treat namespace as a partial cache of domain (from W:>hes>fixes>partial-namespace-domain.lisp.5),
;;; Popup patch (from KRI:KRI;POPUP-PATCH.LISP.1),
;;; Content type in forward patch (from KRI:KRI;CONTENT-TYPE-IN-FORWARD-PATCH.LISP.4),
;;; Attempt to fix recursive block transport (from KRI:KRI;RBT-PATCH.LISP.1),
;;; Directory attributes patch (from KRI:KRI;DIRECTORY-ATTRIBUTES-PATCH.LISP.6),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Read jfif vogt (from KRI:KRI;READ-JFIF-VOGT.LISP.4),
;;; Domain try harder patch (from KRI:KRI;DOMAIN-TRY-HARDER-PATCH.LISP.6),
;;; Find free ephemeral space patch (from KRI:KRI;FIND-FREE-EPHEMERAL-SPACE-PATCH.LISP.3),
;;; Section name patch (from KRI:KRI;SECTION-NAME-PATCH.LISP.1),
;;; Tape spec patch (from KRI:KRI;TAPE-SPEC-PATCH.LISP.10),
;;; Set dump dates on compare patch (from KRI:KRI;SET-DUMP-DATES-ON-COMPARE-PATCH.LISP.75),
;;; Load file only bin (from KRI:KRI;LOAD-FILE-ONLY-BIN.LISP.1),
;;; Show jpeg pathname (from KRI:KRI;SHOW-JPEG-PATHNAME.LISP.1),
;;; Bullet proof trampoline args (from KRI:KRI;BULLET-PROOF-TRAMPOLINE-ARGS.LISP.1),
;;; More y2k patches (from KRI:KRI;MORE-Y2K-PATCHES.LISP.10),
;;; Vlm disk save patch (from KRI:KRI;VLM-DISK-SAVE-PATCH.LISP.5),
;;; Domain ad host patch (from KRI:KRI;DOMAIN-AD-HOST-PATCH.LISP.21),
;;; Background dns refreshing (from KRI:KRI;BACKGROUND-DNS-REFRESHING.LISP.11),
;;; Cname level patch (from KRI:KRI;CNAME-LEVEL-PATCH.LISP.10),
;;; Truename version in eco (from KRI:KRI;TRUENAME-VERSION-IN-ECO.LISP.1),
;;; Zmail patches (from KRI:KRI;ZMAIL-PATCHES.LISP.1),
;;; Better sectionization (from KRI:KRI;BETTER-SECTIONIZATION.LISP.5),
;;; Compile interval patch (from KRI:KRI;COMPILE-INTERVAL-PATCH.LISP.8),
;;; Stealth syn handling (from KRI:KRI;STEALTH-SYN-HANDLING.LISP.6),
;;; New show image patch (from W:>Reti>new-show-image-patch.lisp.12).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:LISPM;SERVER;LISPM.LISP.495")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.495")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

;;string output streams are lexical closures under Genera
(defmethod http:stream-decode-crlf-until-eof ((from-stream tcp::tcp-modal-http-stream) (to-stream si:lexical-closure))
  (%stream-decode-crlf-until-eof from-stream to-stream))

