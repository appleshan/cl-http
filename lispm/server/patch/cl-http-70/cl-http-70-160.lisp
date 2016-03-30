;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Patch-File: T -*-
;;; Patch file for CL-HTTP version 70.160
;;; Reason: Function HTTP::DELETE-FILE*:  Deletes FILE and optionally handles expunging the directory.
;;; Function (CLOS:METHOD HTTP::DELETE-FILE* (T)):  -
;;; Function HTTP::EXPUNGE-DELETED-FILES:  new
;;; Variable WWW-UTILS::*EXPUNGE-DIRECTORY-QUEUE*:  -
;;; Function WWW-UTILS::NOTE-DELETED-FILE:  -
;;; Function (CLOS:METHOD HTTP::DELETE-FILE* (FS:LMFS-PATHNAME) :AFTER):  -
;;; Function (CLOS:METHOD HTTP::DELETE-FILE* (FS:LOGICAL-PATHNAME) :AFTER):  -
;;; Function (CLOS:METHOD HTTP::EXPUNGE-DELETED-FILES NIL):  -
;;; Function (CLOS:METHOD HTTP::COMPRESS-FILE (CL:PATHNAME (EQL :DECOMPRESS))):  update
;;; Function (CLOS:METHOD HTTP::DELETE-FILE-CAREFULLY (T)):  ditto.
;;; Function (CLOS:METHOD HTTP::DELETE-DOCUMENT (URL:HTTP-OBJECT T)):  -
;;; Function (CLOS:METHOD HTTP::COMPRESS-FILE (CL:PATHNAME (EQL :COMPRESS))):  -
;;; Written by JCMa, 5/02/03 13:15:20
;;; while running on FUJI-3 from FUJI:/usr/lib/symbolics/vlmlmfs2.vlod
;;; with Open Genera 2.0, Genera 8.5, Logical Pathnames Translation Files NEWEST,
;;; Lock Simple 437.0, Color Demo 422.0, Color 427.1, Graphics Support 431.0,
;;; Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Color System Documentation 10.0, SGD Book Design 10.0, Images 431.2,
;;; Image Substrate 440.4, CLIM 72.0, Genera CLIM 72.0, CLX CLIM 72.0,
;;; PostScript CLIM 72.0, CLIM Demo 72.0, CLIM Documentation 72.0,
;;; Statice Runtime 466.1, Statice 466.0, Statice Browser 466.0,
;;; Statice Server 466.2, Statice Documentation 426.0, Metering 444.0,
;;; Metering Substrate 444.1, Symbolics Concordia 444.0, Graphic Editor 440.0,
;;; Graphic Editing 441.0, Bitmap Editor 441.0, Graphic Editing Documentation 432.0,
;;; Postscript 436.0, Concordia Documentation 432.0, Joshua 237.6,
;;; Joshua Documentation 216.0, Joshua Metering 206.0, Jericho 237.0, C 440.0,
;;; Lexer Runtime 438.0, Lexer Package 438.0, Minimal Lexer Runtime 439.0,
;;; Lalr 1 434.0, Context Free Grammar 439.0, Context Free Grammar Package 439.0,
;;; C Runtime 438.0, Compiler Tools Package 434.0, Compiler Tools Runtime 434.0,
;;; C Packages 436.0, Syntax Editor Runtime 434.0, C Library Headers 434,
;;; Compiler Tools Development 435.0, Compiler Tools Debugger 434.0,
;;; Experimental C Documentation 427.0, Syntax Editor Support 434.0,
;;; LL-1 support system 438.0, Fortran 434.0, Fortran Runtime 434.0,
;;; Fortran Package 434.0, Experimental Fortran Doc 428.0, Pascal 433.0,
;;; Pascal Runtime 434.0, Pascal Package 434.0, Pascal Doc 427.0,
;;; MacIvory Support 447.0, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.41, Genera 8 5 Macivory Support Patches 1.0,
;;; Genera 8 5 Metering Patches 1.0, Genera 8 5 Joshua Patches 1.0,
;;; Genera 8 5 Jericho Patches 1.0, Genera 8 5 Joshua Doc Patches 1.0,
;;; Genera 8 5 Joshua Metering Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.3,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clx Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Clim Demo Patches 1.0, Genera 8 5 Color Patches 1.1,
;;; Genera 8 5 Images Patches 1.0, Genera 8 5 Color Demo Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; Genera 8 5 Concordia Patches 1.2, Genera 8 5 Concordia Doc Patches 1.0,
;;; Genera 8 5 C Patches 1.0, Genera 8 5 Pascal Patches 1.0,
;;; Genera 8 5 Fortran Patches 1.0, Binary Tree 34.0, Showable Procedures 36.3,
;;; HTTP Server 70.159, W3 Presentation System 8.1, HTTP Client Substrate 4.21,
;;; HTTP Client 51.2, CL-HTTP Server Interface 54.0, HTTP Proxy Server 6.30,
;;; CL-HTTP Documentation 3.0, Experimental CL-HTTP CLIM User Interface 1.1,
;;; MAC 414.0, LMFS 442.1, Ivory Revision 5, VLM Debugger 329, Genera program 8.18,
;;; DEC OSF/1 V4.0 (Rev. 110),
;;; 1550x1102 24-bit TRUE-COLOR X Screen FUJI:2.0 with 224 Genera fonts (The Olivetti & Oracle Research Laboratory R3323),
;;; Machine serial number 6288682,
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add support for Apple's Gestalt and Speech Managers. (from SYS:MAC;MACIVORY-SPEECH-SUPPORT.LISP.1),
;;; Vlm lmfs patch (from W:>reti>vlm-lmfs-patch.lisp.18),
;;; Pht debugging patch (from W:>Reti>pht-debugging-patch.lisp.4),
;;; Domain ad host patch (from W:>Reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing),
;;; Cname level patch (from W:>reti>cname-level-patch),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;UTILS.LISP.506"
  "HTTP:LISPM;SERVER;LISPM.LISP.503"
  "HTTP:SERVER;UTILS.LISP.507"
  "HTTP:SERVER;SERVER.LISP.921"
  "HTTP:LISPM;SERVER;LISPM.LISP.504")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.506")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(define-generic delete-file* (pathname &key expunge &allow-other-keys)
  (:documentation "Deletes FILE and optionally handles expunging the directory."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.506")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmethod delete-file* (pathname &key &allow-other-keys)
  (delete-file pathname))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.506")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(define-generic expunge-deleted-files (&optional directory)
  (:documentation "Expunges deleted files in DIRECTORY, 
or no directory provided expunges all directories with recently deleted files."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.503")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(defvar *expunge-directory-queue* nil
  "Holds the directories queued for asynchronous expunge.")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.503")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defun note-deleted-file (pathname)
  (pushnew (http::make-directory pathname) *expunge-directory-queue* :test #'eq))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.503")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defmethod http::delete-file* :after ((pathname fs:lmfs-pathname) &key (expunge t))
  (case expunge
    (:queue  (note-deleted-file pathname))
    ((t) (fs:expunge-directory pathname))
    (t nil)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.503")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defmethod http::delete-file* :after ((pathname fs:logical-pathname) &key (expunge t))
  (http::delete-file* (http::translated-pathname pathname) :expunge expunge))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.503")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defmethod http::expunge-deleted-files (&optional directory)
  (if directory 
      (fs:expunge-directory (http::make-directory directory))
      (loop for directory = (atomic-pop *expunge-directory-queue*)
	    do (fs:expunge-directory directory))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.504")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defmethod http::compress-file ((pathname pathname) (mode (eql :decompress)) &key delete)
  (let ((compressed-file (probe-file pathname))
	file translation)
    (cond ((setq file (compression-pathname compressed-file mode))
	   (setq translation (if (url::pathname-extension-character-type-p (pathname-type file)) :text :binary))
	   (compression::compress-or-decompress-file :decompress compressed-file file
						     :preamble-type :unix
						     :translation-strategy translation
						     :preserve-dates t)
	   (values file
		   (when delete
		     (let ((new-file (probe-file file)))
		       (when (eql (pathname-version new-file) (pathname-version compressed-file))
			 (http::delete-file* compressed-file :expunge t)
			 t)))))
	  (t (error 'fs:file-not-found :pathname pathname)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.507")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmethod delete-file-carefully (pathname &key &allow-other-keys)
  (let ((probe-file (probe-file pathname)))
    (when probe-file
      (delete-file* probe-file :expunge t))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.921")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod delete-document ((url url:http-object) stream)
  (declare (ignore stream))
  (flet ((delete-document-file (pathname)
           (let ((probe-file (probe-file pathname)))
             (when probe-file
               (delete-file* probe-file :expunge t)))))
    (let ((pathname (url:translated-pathname url)))
      (when pathname
        ;; Delete CRLF files when present
        (case (url:copy-mode url)
          (:crlf (delete-document-file (crlf-pathname pathname))))
        ;; Delete the master file
        (delete-document-file pathname)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.504")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defmethod http::compress-file ((pathname pathname) (mode (eql :compress)) &key delete)
  (let ((file (probe-file pathname))
	(translation (if (url::pathname-extension-character-type-p (pathname-type pathname)) :text :binary))
	compressed-file)
    (cond (file 
	   (setq compressed-file (compression-pathname file mode))
	   (compression::compress-or-decompress-file :compress file compressed-file
						     :preamble-type :unix
						     :translation-strategy translation
						     :preserve-dates t)
	   (values compressed-file
		   (when delete
		     (let ((new-file (probe-file compressed-file)))
		       (when (eql (pathname-version new-file) (pathname-version file))
			 (http::delete-file* file :expunge t)
			 t)))))
	  (t (error 'fs:file-not-found :pathname pathname)))))

