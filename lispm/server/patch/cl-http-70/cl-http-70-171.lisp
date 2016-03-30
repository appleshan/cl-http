;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.171
;;; Reason: Function HTTP::%PUT-NEW-RESOURCE:  set the server URL to the newly minted url
;;; as oon as we know it.
;;; Function (CLOS:METHOD HTTP:SERVER-RELATIVE-URL-STRING  (HTTP::BASIC-SERVER-MIXIN)):  
;;; don't lose if URL not yet set in the server (PUT digest authentication)
;;; define export tyep for .sh files (unix shell scripts).
;;; Written by JCMa, 10/01/03 16:00:24
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5-inc1.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.170,
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
;;; Jcma 44, HTTP Proxy Server 6.32, HTTP Client Substrate 4.22, HTTP Client 51.4,
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
  "HTTP:SERVER;SERVER.LISP.928"
  "HTTP:SERVER;SERVER.LISP.929")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.928")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun %put-new-resource (server stream url-string &aux (length (length url-string)))
  (labels ((url-inferior-directory (parent 1st-delim)
             (loop with start = (incf 1st-delim)
                   while (< start length)
                   for delim = (position #\/ url-string :start start :end length :test #'eql)
                   while delim
                   collect (subseq url-string start delim) into inf-path
                   do (setq start (1+ (the fixnum delim)))
                   finally (return (let ((path (translated-pathname parent)))
                                     (make-pathname :host (pathname-host path)
                                                    :device (pathname-device path)
                                                    :directory `(,@(pathname-directory path) ,.inf-path))))))
           (url-inferior-pathname (parent 1st-delim last-delim directory-level)
             (let ((name-and-extension (subseq url-string (1+ (the fixnum last-delim)) length)))
               (declare (dynamic-extent name-and-extension))
               (merge-pathnames name-and-extension (case directory-level
                                                     (0 (translated-pathname parent))
                                                     (t (url-inferior-directory parent 1st-delim))))))
           (put-inferior (parent 1st-delim last-delim directory-level)
             (let ((pathname (url-inferior-pathname parent 1st-delim last-delim directory-level)))
               (unless (or (zerop directory-level) (probe-directory pathname))
                 (create-directories-recursively pathname))
               (multiple-value-bind (url)
                   (intern-url (merge-url url-string (local-context)) :if-does-not-exist :create)
		 (setf (server-url server) url)	;tell the server about it
                 (inherit-export-parameters url parent)
                 (setf (url:translated-pathname url) pathname
                       (url:translation-method url) (export-type-for-pathname-type (pathname-type pathname) t))
		 (server-update-url-specific-timeouts server url)
                 (put-document url stream t nil)))))
    (multiple-value-bind (parent p-export-type export-type directory-level 1st-delim last-delim)
        (most-specific-exported-parent-url url-string -1 length)
      (unless parent (error 'document-not-found :url url-string :method :put))
      (with-access-control (parent :put server (or (url::write-subnets parent) *write-subnets*)
                                   :deny-subnets *disallowed-subnets* :write-method-p t)
        (cond ((not (and export-type ;;object with export type matched to directory export type.
                         (directory-type-exports-pathname-export-type-p p-export-type export-type)))
               (error 'method-not-allowed :method :put :url url-string))
              ((directory-export-type-p p-export-type)  ;single level directory export.
               (case directory-level
                 (0 (put-inferior parent 1st-delim last-delim 0))
                 (t (error 'document-not-found :url url-string :method :put))))
              ((hierarchical-directory-export-type-p p-export-type)
               (case directory-level
                 (0 (put-inferior parent 1st-delim last-delim 0))
                 ;; need to handle the export of newly created intervening directory levels.  6/11/95 -- JCMa.
                 (t (put-inferior parent 1st-delim last-delim directory-level))))
              (t (error 'method-not-allowed :method :put :url url-string)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.928")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

;; Sometimes the server URL is not interned and set untl verya late, as int he
;; case of PUT for a new document, and yet a digest authentication may require
;; the relative  URL beforehand. 10/1/2003 -- JCMa.
(defmethod server-relative-url-string ((server basic-server-mixin))
  (url:relative-name-string (or (server-url server)
				(server-url-string server))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.929")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(define-url-export-types
  (:text-file :text (:text :plain :charset :iso-8859-1) :copy-mode #.+standard-text-copy-mode+
              :alternate-extensions (:txt :lisp :c :h :sh :script)))

