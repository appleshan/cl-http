;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.178
;;; Reason: Function HTTP::MAP-DIRECTORY-PATHNAMES:  handle wild file type.
;;; Function HTTP::TEXT-FILE-P:  new
;;; Function (CLOS:METHOD HTTP::TEXT-FILE-P (T)):  -
;;; Function HTTP::STREAM-STANDARDIZE-LINE-BREAKS-UNTIL-EOF:  new portable function.
;;; Function (CLOS:METHOD HTTP::STREAM-STANDARDIZE-LINE-BREAKS-UNTIL-EOF (T T T)):  -
;;; Function HTTP::STANDARDIZE-LINE-BREAKS:  portable function for converting line termination
;;; Function WWW-UTILS::%DIRECTORY-LIST*:  abstract primitive driver and fix a bit.
;;; Function WWW-UTILS:DIRECTORY-LIST*:  use primitive.
;;; Shadow cl:directory in www-utils.
;;; Function WWW-UTILS::DIRECTORY:  compatible directory  function.
;;; Function WWW-UTILS::%PATHNAME-AS-DIRECTORY:  -
;;; Function (CLOS:METHOD WWW-UTILS:PATHNAME-AS-DIRECTORY (CL:PATHNAME)):  -
;;; Written by JCMa, 10/09/03 19:39:05
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
;;; HTTP Server 70.177, W3 Presentation System 8.1, HTTP Client Substrate 4.23,
;;; HTTP Client 51.4, CL-HTTP Server Interface 54.0, HTTP Proxy Server 6.32,
;;; CL-HTTP Documentation 3.0, Experimental CL-HTTP CLIM User Interface 1.1,
;;; MAC 414.0, LMFS 442.1, W4 Constraint-Guide Web Walker 45.12, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.18, DEC OSF/1 V4.0 (Rev. 110),
;;; 1152x678 24-bit TRUE-COLOR X Screen FUJI:2.0 with 224 Genera fonts (The Olivetti & Oracle Research Laboratory R3323),
;;; Machine serial number 6288682,
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add support for Apple's Gestalt and Speech Managers. (from SYS:MAC;MACIVORY-SPEECH-SUPPORT.LISP.1),
;;; Vlm lmfs patch (from W:>reti>vlm-lmfs-patch.lisp.18),
;;; Pht debugging patch (from W:>Reti>pht-debugging-patch.lisp.4),
;;; Domain ad host patch (from W:>Reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing),
;;; Cname level patch (from W:>reti>cname-level-patch),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).

;;; Patch file for CL-HTTP version 70.178
;;; Written by JCMa, 10/09/03 23:23:01
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5-inc1.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.177,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.4, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.5, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.41, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Documentation Patches 1.0,
;;; Genera 8 5 Clim Patches 1.3, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; Jcma 44, HTTP Proxy Server 6.33, HTTP Client Substrate 4.23, HTTP Client 51.5,
;;; Image Substrate 440.4, Essential Image Substrate 433.0, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.18, DEC OSF/1 V4.0 (Rev. 110),
;;; 1560x1120 24-bit TRUE-COLOR X Screen FUJI:1.0 with 224 Genera fonts (The Olivetti & Oracle Research Laboratory R3323),
;;; Machine serial number -2141194968,
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing).


(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;UTILS.LISP.530"
  "HTTP:LISPM;SERVER;LISPM.LISP.516"
  "HTTP:LISPM;SERVER;LISPM.LISP.517"
  "HTTP:LISPM;SERVER;LISPM.LISP.519"
  "HTTP:SERVER;UTILS.LISP.532"
  "HTTP:SERVER;UTILS.LISP.533")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.530")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(define map-directory-pathnames (pathname function &key (file-type :text))
  "Maps FUNCTION over the newest version pathnames of the directory, PATHNAME
according to FILE-TYPE."
  (dolist (pathname (directory (make-pathname :name :wild :type (etypecase file-type
								  (string file-type)
								  (symbol
								    (case file-type
								      (:wild file-type)
								      (t (symbol-name file-type)))))
					      :version :newest :defaults pathname)))
    (funcall function pathname))) 


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.530")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

;;;------------------------------------------------------------------- 
;;;
;;; STANDARDIZING LINE BREAKS
;;;

(defgeneric text-file-p (pathname)
  (:documentation "Returns non-null when pathname is known to be a text file.
Platforms may specialize this method for better heuristic accuracy."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.530")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmethod text-file-p (pathname)
  (let ((text-file-types `("lisp" "html" "htm" "text" "txt" "xml" "ps" "cern-map" "mak" "ncsa-map"
                                  "c" "cl" "script" "translations" "hqx")))
    (or (member (pathname-type pathname) text-file-types :test #'equalp)
        (eql :crlf (pathname-copy-mode pathname nil)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.530")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(define-generic stream-standardize-line-breaks-until-eof (from-stream to-stream line-break)
  (:documentation "Copies FROM-STREAM to TO-STREAM, canonicalizing line breaks 
to LINE-BREAK, which can be any one of :CR (carriage return), :LF (Linefeed), 
or :CRLF (return linefeed).  Ports and applications may specialize this method 
to optimize data transfer rates."))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.530")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defgeneric standardize-line-breaks (pathname &optional standard-line-break stream)
  (:documentation "Standardizes all line breaks for PATHNAME according
to STANDARD-LINE-BREAK, which can be any of:

     :LF   -- Linefeed (UNIX)
     :CR   -- Carriage Return (MacOS Classic)
     :CRLF -- Carriage Return followed by Linefeed (PC)

PATHNAME can be either an individual file or a directory.  When it is
a directory, all text files within it are standardized. If STREAM is
provided, progress is reported."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.530")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(PROGN
(defmethod standardize-line-breaks ((pathname string) &optional (standard-line-break :lf) stream)
   (standardize-line-breaks (pathname pathname) standard-line-break stream))

(defmethod standardize-line-breaks ((pathname pathname) &optional (standard-line-break :lf) stream)
  (flet ((%standardize-file-line-breaks (pathname line-break stream)
           (let ((temp (make-pathname :name (concatenate 'string (pathname-name pathname) "-temp") :defaults pathname))
                 (old (make-pathname :name (concatenate 'string (pathname-name pathname) "-old") :defaults pathname))
                 (modification-date (file-modification-date pathname))
                 #-UNIX
                 (creation-date (file-creation-date pathname))
                 (author (file-author pathname)))
             (with-open-file (file-in pathname :direction :input :element-type *standard-character-type* :if-does-not-exist :error)
               (with-open-file (file-out temp :direction :output :element-type *standard-character-type* :if-exists :supersede)
                 (stream-standardize-line-breaks-until-eof file-in file-out line-break)))
             (when modification-date
               (set-file-modification-date temp modification-date))
             #-UNIX
             (when creation-date
               (set-file-creation-date temp creation-date))
             (when author
               (set-file-author temp author nil))
             (rename-file pathname old)
             (rename-file temp pathname)
             (delete-file old)
             (when stream
               (format stream "~&Standardize Line Breaks (~A): ~A"  line-break pathname))
             pathname)))
    (declare (inline %text-file-p %standardize-file-line-breaks))
    (when (text-file-p pathname)
      (tagbody
       start
       (restart-case
           (return-from standardize-line-breaks 
             (%standardize-file-line-breaks pathname standard-line-break stream))
         (retry-load () :report (lambda (stream) (format stream "Retry standardizing ~A in ~A" standard-line-break pathname))
                     (go start))
         (skip-load () :report (lambda (stream) (format stream "Skip standardizing ~A in ~A" standard-line-break pathname))
                    (return-from standardize-line-breaks nil)))))))
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.516")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(eval-when (load eval compile)
  (shadow 'cl:directory :www-utils)
  (shadowing-import (intern "DIRECTORY" :www-utils) :http))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.517")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defun %pathname-as-directory (pathname &optional (name (pathname-name pathname)))
  (make-pathname :host (pathname-host pathname)
		 :device (pathname-device pathname)
		 :directory `(,@(pathname-directory pathname) ,name)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.517")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defmethod pathname-as-directory ((pathname pathname) &aux name)
  (cond ((and (setq name (pathname-name pathname))
              (%pathname-directory-file-p pathname))
	 (%pathname-as-directory pathname name))
        (t pathname)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.519")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defun %directory-list* (pathname predicate &optional options
				  &aux files directories)
  "Accepts the options :FILES :DIRECTORIES :SORTED :PROPERTIES."
  (flet ((collect-p (entry)
           (flet ((test-predicate (path)
                    (or (null predicate) (funcall predicate path))))
             (declare (dynamic-extent #'test-predicate))
             (when entry
               (destructuring-bind (path . plist) entry
                 (cond
                   ((null path) nil)
                   ((and files directories)
                    #+VLM (when (getf plist :directory)
                            (scl:send path :putprop t :directory))
                    (test-predicate path))
                   (files
                    (cond ((getf plist :directory) nil)
                          (t (test-predicate path))))
                   ((and directories (getf plist :directory))
                    #+VLM (scl:send path :putprop t :directory)
                    (test-predicate path))
                   (t nil))))))
         (coerce-pathname (entry)
           (when (and directories 
		      (or (getf (cdr entry) :directory)
			  (%pathname-directory-file-p (car entry))))
	     (setf (first entry) (pathname-as-directory (car entry))))
           entry))
    (declare (inline collect-p coerce-pathname))
    (let* ((path (pathname-as-directory pathname))
           (dir (make-pathname :name (or (pathname-name path) :wild)
                               :type (or (pathname-type path) :wild)
                               :version (or (pathname-version path) :wild)
			       :defaults path))
           (entries (apply #'fs:directory-list dir
                           (if (member :sorted options) '(:sorted :no-extra-info) '(:fast :no-extra-info)))))
      (setq files (member :files options)
            directories (member :directories options))
      (cond ((member :properties options)
             (loop for entry in entries
                   when (collect-p entry)
                     collect (coerce-pathname entry)))
            (t (loop for entry in entries
                     when (collect-p entry)
                       collect (car (coerce-pathname entry))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.519")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defun directory-list* (pathname predicate &rest options)
  "Accepts the options :FILES :DIRECTORIES :SORTED :PROPERTIES."
  (declare (dynamic-extent options))
  (%directory-list* (make-pathname :name :wild
				   :type :wild
				   :version :newest
				   :defaults (pathname-as-directory (pathname pathname)))
		    predicate options))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.519")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

;; this should suport wildcards sometime. -- JCMa 10/9/2003
;; Define a compatiable directory list (MCL LispWorks)
(defun directory (pathname &key test (files t) (directories t) &aux options)
  (when files
    (push :files options))
  (when directories
    (push :directories options))
  (%directory-list* (pathname pathname) test options))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.532")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmethod standardize-line-breaks :around ((pathname pathname) &optional (standard-line-break :lf) stream)
   (if (pathname-directory-p pathname)
      (dolist (path (directory (merge-pathnames "*.*" pathname) :directories t))
         (standardize-line-breaks path standard-line-break stream))
      (call-next-method pathname standard-line-break stream)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.533")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

;; ports should specialize this for high performance.
(defmethod stream-standardize-line-breaks-until-eof (from-stream to-stream line-break)
  (with-text-stream (from-stream :input)
    (with-text-stream (to-stream :output)
      (using-resource (line-buffer line-buffer *line-buffer-size*)
        (loop (multiple-value-bind (line eof delimiter length)
		  (read-delimited-line from-stream '(#\Return #\Linefeed) nil line-buffer)
		delimiter			;ignore
		(unless (zerop length)
		  (write-string line to-stream :start 0 :end length))
		(when eof (return))
		(ecase line-break
		  (:lf (write-char #\Linefeed to-stream))
		  (:cr (write-char #\Return to-stream))
		  (:crlf 
                    (write-char #\Return to-stream)
                    (write-char #\Linefeed to-stream)))))))))

