;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for W4 version 45.6
;;; Reason: Function (CLOS:METHOD W4::WALK (STRING W4::ACTIVITY)):  make sure *report-stream* is set up
;;; Written by HES, 9/24/2000 01:51:56
;;; while running on Harry S. Truman from FEP1:>cl-http.ilod.1
;;; with Genera 8.5, LMFS 442.1, Documentation Database 440.12,
;;; IP-TCP Documentation 422.0, X Remote Screen 448.3, NFS Server 439.0,
;;; Mailer 438.0, Print Spooler 439.0, Domain Name Server 436.0,
;;; Experimental Lock Simple 435.1, Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, Conversion Tools 436.0,
;;; Metering 444.0, Metering Substrate 444.1, Hacks 440.0, CLIM 72.0,
;;; Genera CLIM 72.0, PostScript CLIM 72.0, Experimental CLIM Documentation 71.27,
;;; Statice Runtime 466.1, Statice 466.0, Statice Browser 466.0,
;;; Statice Server 466.2, 8-5-Patches 2.19, MAC 414.0, Statice Documentation 426.0,
;;; Joshua 237.4, Joshua Documentation 216.0, Joshua Metering 206.0,
;;; address-book 7.0, HTTP Server 70.77, Showable Procedures 36.3, Binary Tree 34.0,
;;; W3 Presentation System 8.1, Working LispM Mailer 7.0, Experimental Start3 13.0,
;;; CL-HTTP Server Interface 53.0, Symbolics Common Lisp Compatibility 4.0,
;;; Experimental Comlink Packages 17.0, Experimental Comlink Utilities 21.0,
;;; Experimental COMLINK Cryptography 13.0, Experimental Routing Taxonomy 20.0,
;;; Experimental COMLINK Database 24.0, Experimental Email Servers 24.0,
;;; Experimental Comlink Customized LispM Mailer 18.0,
;;; Experimental Dynamic Forms 25.0, Experimental Communications Linker Server 50.0,
;;; Experimental Knowledge-Based Collaboration Webs System 35.0, IDEAL 2.0,
;;; The Project Planner 10.0, W4 Constraint-Guide Web Walker 45.5,
;;; HTTP Client Substrate 3.23, Lambda Information Retrieval System 22.3,
;;; HTTP Proxy Server 5.31, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Macivory Support Patches 1.0,
;;; Genera 8 5 Mailer Patches 1.1, Genera 8 5 Domain Name Server Patches 1.1,
;;; Genera 8 5 Metering Patches 1.0, Genera 8 5 Joshua Patches 1.0,
;;; Genera 8 5 Joshua Doc Patches 1.0, Genera 8 5 Joshua Metering Patches 1.0,
;;; Genera 8 5 Statice Runtime Patches 1.0, Genera 8 5 Statice Patches 1.0,
;;; Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.0,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; Ivory Revision 4A, IFEP 328, FEP0:>I328-loaders.flod(24),
;;; FEP0:>I328-info.flod(24), FEP0:>I328-debug.flod(24), FEP0:>I328-lisp.flod(25),
;;; FEP0:>I328-KERNEL.FEP(44), Boot ROM version 320, Device PROM version 325,
;;; Genera application 5.6.1a1, MacIvory SCSI Manager Server 4.3.2a1,
;;; Toolbox Servers 4.2, MacIvory & RPC library 6.3.4a1,
;;; MacIvory life support 4.3.8a1, Macintosh System Software 8.1,
;;; 1024x700 Screen with Genera fonts, Machine serial number 30372,
;;; Macintosh Quadra 800, Apple Extended Keyboard II,
;;; Add support for Apple's Gestalt and Speech Managers. (from SYS:MAC;MACIVORY-SPEECH-SUPPORT.LISP.1),
;;; zmail address book extensions (from ADDRESS-BOOK:CODE;ZMAIL-PATCHES.LISP.7),
;;; as it says (from W:>hes>zmail-hacks>sort-by-expiration-date.lisp.1),
;;; Calendar improvements (from W:>hes>zmail-hacks>calendar-improvements.lisp.32),
;;; user the login name as the password (from W:>hes>fixes>anonymous-login.lisp.1),
;;; Make update schema work on set-value attributes with accessor names (from CML:LISPM;STATICE-SET-VALUED-UPDATE.LISP.1),
;;; When you grow the array and relocate the image,
;;; you have to adjust the clipping region! (from W:>hes>fixes>bitmap-screen-growth.lisp.2),
;;; Fix up to color decoding so that draw image works better (from W:>hes>fixes>clim-color-monitor.lisp.1),
;;; Make drawing to bit-array in color work on Mac (from W:>hes>fixes>color-bitmaps.lisp.2),
;;; Return true remote depth as well (from W:>hes>fixes>console-remote-depth.lisp.1),
;;; Fix drawing circle and ellipse arcs (from W:>hes>fixes>clim-elliptical-arc.lisp.13),
;;; Allow it to specify bits-per-pixel (from W:>hes>fixes>clim-pixmaps.lisp.7),
;;; Make it work for the char bound to the key that started command (from W:>hes>fixes>com-move-over-close-paren.lisp.1),
;;; add p (from W:>hes>fixes>directory-mail.lisp.2),
;;; Prevent circles from squishing under transform (from W:>hes>fixes>macivory-circle-drawing.lisp.2),
;;; Fix meta-. so that it will find the clos methods even if there is a defgeneric hidden (from W:>hes>fixes>meta-point-for-define-generic.lisp.1),
;;; hack to treat namespace as a partial cache of domain (from W:>hes>fixes>partial-namespace-domain.lisp.5),
;;; Push our own dict at the beginning to prevent overflow (from W:>hes>fixes>postscript-prologue.lisp.1),
;;; Add a new option to function-0-q to hardcopy the frame immediately enclosing the selected window. Necessary when you have pop-up frames in an activity such as the project planner. (from W:>hes>fixes>hardcopy-selected-frame.lisp.2),
;;; The current lpq software has slightly different format than the code was based on (from W:>hes>fixes>new-lpd-queue.lisp.1),
;;; Make printing postscript files on Unix driven printers work (from W:>hes>fixes>unix-postscript-printing.lisp.4),
;;; Make the lookup of /homes/user/ go through the currently used pathways by consuting auto.home (from W:>hes>fixes>unix-homes-lookup.lisp.4),
;;; Ignore RPC pointer errors (from W:>hes>fixes>nfs-patch),
;;; make left configure go to both-mode when in a calendar mode (from W:>hes>fixes>calendar-mode-to-both.lisp.1),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Domain Fixes (from CML:MAILER;DOMAIN-FIXES.LISP.33),
;;; Don't force in the mail-x host (from W:>hes>fixes>newer-mailbox-format.lisp.4),
;;; Make Mailer More Robust (from CML:MAILER;MAILER-FIXES.LISP.15),
;;; Add CLIM presentation and text style format directives. (from FV:SCLC;FORMAT.LISP.20),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.107),
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.49),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7),
;;; Fix NFS brain damage. (from EOP:MAIL-SERVER;PATCHES;NFS-PATCH.LISP.4),
;;; Log patch (from EOP:MAIL-SERVER;PATCHES;LOG-PATCH.LISP.4),
;;; Pathname patch (from EOP:MAIL-SERVER;PATCHES;PATHNAME-PATCH.LISP.4),
;;; Pathname2 patch (from EOP:MAIL-SERVER;PATCHES;PATHNAME2-PATCH.LISP.4),
;;; Mailer bandaid patch (from W:>reti>mailer-bandaid-patch.lisp.8).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:W4;WALKER.LISP.100")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:W4;WALKER.LISP.100")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: w4; -*-")

(defmethod walk ((url string) (activity activity))
  (flet ((doit (activity url)
	   (flet ((clear-activity-caches (key value)
		    (declare (ignore key))
		    (clear-cache activity value t)))
	     (declare (dynamic-extent #'clear-activity-caches))
	     (unwind-protect
		 (when (and (satisfies-context-constraints-p url activity)
			    (satisfies-activity-p url activity))
		   (walk-using-queue activity (activity-queue activity) url)
		   (map-url-table #'clear-activity-caches)
		   (setf (activity-uri-universe-object activity) nil))))))
    (declare (inline doit))
    ;; initialize activity
    (setf (activity-aborted-p activity) nil
	  (property-list activity) nil)
    (clear-url-notes activity :all)
    ;; Allocate time for the web walk. Prevent server connection scavenger from killing us.
    (when http:*server*
      (setf (http:server-timeout http:*server*) (activity-life-time activity)))
    ;; primary method to perform the walk
    (flet ((primary-method ()
	     (let ((uri-universe (activity-uri-universe activity)))
	       (typecase uri-universe
		 (uri-universe
		   (url:with-uri-universe (uri-universe)
		     (setf (activity-uri-universe-object activity) uri-universe)
		     (doit activity (url:intern-url url))))
		 (string
		   (using-resource (uri-universe-object uri-universe uri-universe)
		     (setf (activity-uri-universe-object activity) uri-universe-object)
		     (url:with-uri-universe (uri-universe-object)
		       (doit activity (url:intern-url url)))))
		 (null (doit activity (url:intern-url url)))))))
      (cond ((activity-report-stream activity)
	     (let ((*report-stream* (report-stream activity)))
	       (primary-method)))
	    (t (with-null-stream (*report-stream*)
		 (setf (activity-report-stream activity) '*report-stream*)
		 (unwind-protect
		     (primary-method)
		   (setf (activity-report-stream activity) nil))))))
    activity))

