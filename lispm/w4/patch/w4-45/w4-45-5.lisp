;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for W4 version 45.5
;;; Reason: Function W4:WITH-ACTIVITY:  systematically use activity-var
;;; Written by HES, 9/24/2000 01:43:28
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
;;; The Project Planner 10.0, W4 Constraint-Guide Web Walker 45.0,
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

(define-macro with-activity ((name (&key (activity-var (intern "ACTIVITY" *package*))
					 (search-method :depth-first)
					 (threads 1)
					 predicate
					 (proxy '(local-proxy)) 
					 proxy-port
					 (if-does-not-exist :uninterned)
					 (if-exists :error)
					 user-agent operator connection-timeout life-time
					 (url-host-name-resolution :never)
					 uri-universe
					 documentation report-stream
					 (class ''activity))
				   &key constraints actions unsatisfied-actions)
			     &body body)
  "Top-level method for runtime definition of an activity for Web walking.
See the macros LET-CONSTRAINT-STRUCTURE and LET-ACTION-STRUCTURE
CONNECTION-TIMEOUT is the number of seconds to wait for a server to respond.
LIFE-TIME is the maximum number of seconds to allocate for the web walk.
PROXY and PROXY-PORT specify a local proxy to use when fetching web resources
to achieve local caching of resources walked.
Note that DEFINE-CLIENT-PROXY-MAPPING provides a more general interface for
specifying proxies for use by the cient."
  (let ((code `(let-constraint-structure ((constraints ,constraints))
		 (let-action-structure ((actions ,actions))
		   (let ((,activity-var (%define-activity ,name constraints actions
							  ,.(when unsatisfied-actions
							      `(:unsatisfied-actions unsatisfied-actions))
							  :if-does-not-exist ,if-does-not-exist
							  :if-exists ,if-exists
							  :documentation ,documentation
							  :user-agent ,user-agent
							  :operator ,operator
							  :connection-timeout ,connection-timeout
							  :life-time ,life-time
							  :url-host-name-resolution ,url-host-name-resolution
							  :search-method ,search-method
							  :threads ,threads
							  :predicate ,predicate
							  :proxy ,proxy
							  :proxy-port ,proxy-port
							  :uri-universe ,uri-universe
							  :report-stream ,report-stream
							  :class ,class)))
		     (unwind-protect 
			 (progn (initialize-queue ,activity-var (activity-queue-type ,activity-var))
				,@body)
		       (deinitialize-activity ,activity-var)))))))
    (if unsatisfied-actions
	`(let-action-structure ((unsatisfied-actions ,unsatisfied-actions)) ,code)
	code)))

