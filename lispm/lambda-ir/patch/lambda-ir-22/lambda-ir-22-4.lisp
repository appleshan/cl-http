;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for LAMBDA-IR version 22.4
;;; Reason: Function (CLOS:METHOD LR::FIND-SUFFIX (LIST T)):  fix bug found by David Johnson-Davies
;;; Written by Reti, 5/01/01 08:48:30
;;; while running on Second Vesuvius VLM from VESUVIUS:/test/alpha/4.0D/genera/sys.sct/worlds/mit-full-g85p.vlod
;;; with Open Genera 2.0, Genera 8.5, Logical Pathnames Translation Files NEWEST,
;;; Experimental Genera 8 5 Patches 1.0, Genera 8 5 System Patches 1.26,
;;; Genera 8 5 Metering Patches 1.0, Genera 8 5 Joshua Patches 1.0,
;;; Genera 8 5 Jericho Patches 1.0, Genera 8 5 Joshua Doc Patches 1.0,
;;; Genera 8 5 Joshua Metering Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clx Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Clim Demo Patches 1.0, Genera 8 5 Color Patches 1.1,
;;; Genera 8 5 Images Patches 1.0, Genera 8 5 Color Demo Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; Genera 8 5 Concordia Patches 1.0, Genera 8 5 Concordia Doc Patches 1.0,
;;; Genera 8 5 C Patches 1.0, Genera 8 5 Pascal Patches 1.0,
;;; Genera 8 5 Fortran Patches 1.0, Color 427.1, Graphics Support 431.0,
;;; Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Color System Documentation 10.0, SGD Book Design 10.0, Images 431.2,
;;; Image Substrate 440.4, CLIM 72.0, Genera CLIM 72.0, CLX CLIM 72.0,
;;; PostScript CLIM 72.0, CLIM Demo 72.0, CLIM Documentation 72.0, Color Demo 422.0,
;;; Statice Runtime 466.1, Statice 466.0, Statice Browser 466.0,
;;; Statice Server 466.2, Statice Documentation 426.0, Metering 444.0,
;;; Metering Substrate 444.1, Symbolics Concordia 444.0, Graphic Editor 440.0,
;;; Graphic Editing 441.0, Bitmap Editor 441.0, Graphic Editing Documentation 432.0,
;;; Postscript 436.0, Concordia Documentation 432.0, Lock Simple 437.0, Joshua 237.4,
;;; Joshua Documentation 216.0, Joshua Metering 206.0, Jericho 237.0, C 440.0,
;;; Lexer Runtime 438.0, Lexer Package 438.0, Minimal Lexer Runtime 439.0,
;;; Lalr 1 434.0, Context Free Grammar 439.0, Context Free Grammar Package 439.0,
;;; C Runtime 438.0, Compiler Tools Package 434.0, Compiler Tools Runtime 434.0,
;;; C Packages 436.0, Syntax Editor Runtime 434.0, C Library Headers 434,
;;; Compiler Tools Development 435.0, Compiler Tools Debugger 434.0,
;;; Experimental C Documentation 427.0, Syntax Editor Support 434.0,
;;; LL-1 support system 438.0, Fortran 434.0, Fortran Runtime 434.0,
;;; Fortran Package 434.0, Experimental Fortran Doc 428.0, Pascal 433.0,
;;; Pascal Runtime 434.0, Pascal Package 434.0, Pascal Doc 427.0, HTTP Server 70.126,
;;; Showable Procedures 36.3, Binary Tree 34.0, W3 Presentation System 8.1,
;;; HTTP Client 50.7, HTTP Client Substrate 4.10, CL-HTTP Server Interface 54.0,
;;; HTTP Proxy Server 6.21, Experimental Jpeg Lib 1.0,
;;; Lambda Information Retrieval System 22.3, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.16, DEC OSF/1 V4.0 (Rev. 205),
;;; 1280x974 24-bit TRUE-COLOR X Screen INTERNET|128.52.54.15:0.0 with 224 Genera fonts (Hummingbird Communications Ltd. R6010),
;;; Machine serial number 573355060,
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
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
;;; Stealth syn handling (from KRI:KRI;STEALTH-SYN-HANDLING.LISP.5),
;;; Numeric sorted directories (from W:>reti>numeric-sorted-directories.lisp.2),
;;; Trace tcp (from W:>reti>trace-tcp.lisp.13).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:LAMBDA-IR;EXAMPLES;STEMMING.LISP.25")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LAMBDA-IR;EXAMPLES;STEMMING.LISP.25")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: lambda-ir; BASE: 10; SYNTAX: ansi-common-lisp -*-")

(defmethod find-suffix ((suffix-storage-device list) string &optional index)
  (declare (ignore index))
  (loop for (suffix result) in suffix-storage-device
        with length = (fill-pointer string)
        for suffix-length = (length suffix)
        for predicate = (and (> length suffix-length)
                             (string-equal string suffix :start1 (- length suffix-length)))
        until predicate
        finally (when predicate
                  (return (list suffix result)))))

