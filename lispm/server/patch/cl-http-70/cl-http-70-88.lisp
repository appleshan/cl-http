;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.88
;;; Reason: Function WWW-UTILS::%PATHNAME-DIRECTORY-FILE-P:  revise.
;;; Function WWW-UTILS:PATHNAME-DIRECTORY-P:  -
;;; Function (CLOS:METHOD WWW-UTILS:PATHNAME-AS-DIRECTORY (CL:PATHNAME)):  -
;;; Function HTTP::COMPRESS-DIRECTORY:  -
;;; Function HTTP::COMPRESS-DIRECTORY:  -
;;; Function (CLOS:METHOD HTTP::COMPRESS-DIRECTORY (T T)):  -
;;; Function (CLOS:METHOD HTTP::COMPRESS-DIRECTORY (STRING T)):  -
;;; Written by JCMa, 10/29/00 21:10:51
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.87,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Documentation Patches 1.0,
;;; Genera 8 5 Clim Patches 1.0, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Jcma 42, Ivory Revision 5, VLM Debugger 329,
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
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.20),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.6),
;;; Cname level patch (from W:>reti>cname-level-patch).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:LISPM;SERVER;LISPM.LISP.479"
  "HTTP:LISPM;SERVER;LISPM.LISP.478"
  "HTTP:SERVER;UTILS.LISP.472"
  "HTTP:SERVER;UTILS.LISP.473")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.479")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defun %pathname-directory-file-p (pathname)
  "Returns non-null if PATHNAME is a directory masquerading as a file.
only comes up on the lisp machine"
  (let ((path (pathname pathname)))
    (typecase path
      (fs:lmfs-pathname (equalp "directory" (pathname-type path)))
      (t #+VLM (scl:send path :get :directory)
	 #-VLM nil))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.478")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defun pathname-directory-p (pathname)
  (let ((path (pathname pathname)))
    (or (%pathname-directory-p path)
        (%pathname-directory-file-p path))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.478")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defmethod pathname-as-directory ((pathname pathname) &aux name)
  (cond ((and (setq name (pathname-name pathname))
              (%pathname-directory-file-p pathname))
	 (%pathname-as-directory pathname name))
        (t pathname)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.472")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

;;;------------------------------------------------------------------- 
;;;
;;; FILE COMPRESSION
;;;

(defgeneric compress-file (pathname mode &key delete)
  (declare (values compressed-pathname))
  (:documentation "Compresses PATHNAME according to MODE.
MODES are: COMPRESS, DECOMPRESS.
Ports should specialize this method to provide compression and decompression capabilities."))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.472")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defgeneric compress-directory (pathname mode &key delete recursive report-stream)
  (declare (values pathname))
  (:documentation "Compresses all the files in the directory, PATHNAME, according to compression MODE.
Wild-card patterns may be supplied in PATHNAME to the extent that the file system supports them.
When DELETE is non-null, input files are delete after they are compressed.
When RECURSIVE is non-null, directories are recursively descended.
When REPORT-STREAM is non-null, activity is reported on REPORT-STREAM."))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.473")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmethod compress-directory ((pathname pathname) mode &key delete recursive (report-stream *standard-output*))
  (loop for path in (directory (pathname-as-directory pathname))
	when (pathname-directory-p path)
	  do (when recursive
	       (compress-directory path mode :delete delete :recursive t))
	else do (when report-stream (format report-stream "~&Compressing ~A ...." path))
		(multiple-value-bind (compressed-pathname)
		    (compress-file path mode :delete delete)
		  (when report-stream (format report-stream "~&~A compressed into ~A." path compressed-pathname))))
  pathname)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.473")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmethod compress-directory ((pathname string) mode &key delete recursive (report-stream *standard-output*))
  (compress-directory (pathname pathname) mode :delete delete :recursive recursive :report-stream report-stream))

