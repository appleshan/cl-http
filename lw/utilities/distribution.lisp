;;;   -*- Mode: LISP; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-

;;;
;;; (c) Copyright  1995, 2003, John C. Mallery
;;;     All Rights Reserved.
;;; 

(in-package :http) 

;;;------------------------------------------------------------------- 
;;;
;;; DISTRIBUTION FACILITY FOR CL-HTTP 
;;;

#+Genera
(load #p"http:ai-lab;copyf")

(eval-when (:execute :load-toplevel :compile-toplevel)
(setf (logical-pathname-translations "http-dis")
      `(("**;*.*" #+Genera"wilson:>http>**>*.*"
                  #+MCL"wilson:http:**:*.*"
                  #+LispWorks "/Volumes/wilson.csail.mit.edu/http/**/*.*")))
) ;close eval when

(defparameter *binary-pathname-extensions* '("class" "gif" "ico" "jpeg" "jpg" "au" "mpeg" "mpg" "png" "pdf" "tar" "gz"))

(defparameter *standard-extensions* `("lisp" "html" "htm" "text" "css" "txt" "xml" "ps" "cern-map" "mak" "ncsa-map"
                                             "c" "script" "translations" "hqx"
                                             ,.*binary-pathname-extensions*))

(defparameter *distribution-directories* `("http-dis:client;"
                                           "http-dis:clim;"
                                           "http-dis:contrib;"
                                           "http-dis:examples;" 
                                           "http-dis:proxy;"
                                           "http-dis:server;"
                                           "http-dis:smtp;" 
                                           "http-dis:w3p;"
                                           "http-dis:w4;"
                                           "http-dis:www;"
                                           ;; Ports
                                           "http-dis:mcl;" "http-dis:acl;""http-dis:cmucl;" 
                                           "http-dis:lcl;" "http-dis:lispm;" "http-dis:lw;"
                                           ;; Modules
                                           "http-dis:html-parser;" "http-dis:lambda-ir;"
                                           ;; Protocol Documumentation
                                           #+ignore "http-dis:standards;"))

(defparameter *distribution-files* '("http-dis:-license-.text" 
                                     "http-dis:-release-notes-.text"
                                     "http-dis:acl-read-me.text"
                                     "http-dis:lcl-read-me.text"
                                     "http-dis:lw-read-me.text"
                                     "http-dis:mcl-read-me.text")) 

;;;------------------------------------------------------------------- 
;;;
;;;  COPYING CODE
;;;

(defun %copy-file (pathname to-directory &optional (extensions :wild) (default-version :newest) 
                            (report-stream *standard-output*))
  (labels ((copy-a-file (from-pathname to-pathname &optional (mode :text) 
                                       (create-directories t) (report-stream *standard-output*))
             #+Genera
             (zl:copyf from-pathname to-pathname :create-directories create-directories :if-exists :new-version 
                       :strip-styles t :report-stream report-stream
                       :element-type (ecase mode 
                                       (:text 'scl:string-char)
                                       (:binary  '(unsigned-byte 8))))
             #-Genera
             (copy-file from-pathname to-pathname :copy-mode mode 
                        :create-directories create-directories :report-stream report-stream))
           (extension-match-p (pathname)
             (case extensions
               (:wild t)
               (t (member (pathname-type pathname) extensions :test #'equalp))))
           (binary-p (pathname)
             (member (pathname-type pathname) *binary-pathname-extensions* :test #'equalp))
           (target-pathname (pathname to-directory)
             (make-pathname :name (pathname-name pathname) :type (pathname-type pathname) 
                            :version default-version :defaults to-directory))
           (copy-text-file (pathname)
             (copy-a-file pathname (target-pathname pathname to-directory) :text t report-stream))
           (copy-binary-file (pathname)
             (copy-a-file pathname (target-pathname pathname to-directory) :binary t report-stream)))
    (declare (inline binary-p copy-text-file copy-binary-file extension-match-p))
    (when (extension-match-p pathname)
      (if (binary-p pathname)
          (copy-binary-file pathname)
        (copy-text-file pathname))))) 

(defvar *undistributed-directories* (make-hash-table :test #'equalp))

(defun clear-undistributed-directories ()
   (clrhash *undistributed-directories*))

(declaim (notinline pathname-directory-key))

;; This key generator may need to change across lisp implementations.
;; ANSI is vague on whether physical pathname need :unspecific when
;; no device is specified, whereas it is required for logical pathanmes.
(defun pathname-directory-key (pathname)
  (let ((path (translated-pathname (pathname pathname))))
    (make-pathname :host (or (pathname-host path) :unspecific)
                   :device (or (pathname-device path) :unspecific)
                   :directory (pathname-directory path)
                   :name :unspecific
                   :type :unspecific
                   :version :newest)))

(defun distribute-directory-p (pathname)
  (let ((key (pathname-directory-key pathname)))
    (declare (dynamic-extent key))
    (not (gethash key *undistributed-directories*))))

(defun %define-undistributed-directories (pathnames)
  (loop initially (clear-undistributed-directories)
        for path in pathnames
        for key = (pathname-directory-key path)
        do (setf (gethash key *undistributed-directories*) t))
  pathnames)

(defmacro define-undistributed-directories (&rest directories)
  `(%define-undistributed-directories ',directories))

(define-undistributed-directories
 "http-dis:contrib;janderson;xml-1998-02-27;"
 "http-dis:contrib;janderson;xml-1999-05-03;"
 "http-dis:contrib;janderson;xml-2000-05-27;"
 "http-dis:contrib;janderson;xml;"
 "http-dis;examples;twistdown-tree;java;old;"
 ;; Utilities
 "http:html-parser;v9;"
 "http:html-parser;v10;"
 "http-dis:lispm;html-parser;dtd-compiler-3;"
 "http-dis:lispm;html-parser;html-parser-9;"
 "http-dis:lispm;html-parser;html-parser-10;"
 "http-dis:lispm;lambda-ir;patch;lambda-ir-21;"
  "http-dis:lispm;uri;"
 "http-dis:lispm;urn;"
 ;; Core components 
 "http-dis:clim;patch;cl-http-clim-39;"
 "http-dis:lispm;client;patch;http-client-substrate-1;"
 "http-dis:lispm;client;patch;http-client-substrate-3;"
 "http-dis:lispm;client;patch;http-base-client-49;"
 "http-dis:lispm;client;patch;http-base-client-50;"

 "http-dis:lispm;server;patch;cl-http-58;"
 "http-dis:lispm;server;patch;cl-http-63;"
 "http-dis:lispm;server;patch;cl-http-67;"
 "http-dis:lispm;proxy;patch;http-proxy-4;"
 "http-dis:lispm;proxy;patch;http-proxy-5;"
 "http-dis:lispm;clim;old-ui;patch;cl-http-clim-51;"
 "http-dis:lispm;clim;old-ui;patch;cl-http-clim-52;"
 "http-dis:lispm;clim;old-ui;patch;cl-http-clim-53;"
 "http-dis:lispm;docs;patch;cl-http-doc-2;") 

(defun %copy-directory (to-directory from-directory &optional (extensions :wild)
                                     (default-version :newest) (report-stream *standard-output*))
  (flet ((push-directory-level (directory pattern)
           (make-pathname :host (pathname-host directory)
                          :device (pathname-device directory)
                          :directory (append (pathname-directory directory)
                                             #+Genera (list (pathname-name pattern))
                                             #-Genera (last (pathname-directory pattern)))))
         (down-directory (directory)
           (make-pathname :host (pathname-host directory)
                          :device (pathname-device directory)
                          :directory #+Genera (append (pathname-directory directory)
                                                      (list (pathname-name directory)))
                          #-Genera (pathname-directory directory)))
         (directory-p (plist)
           (getf plist :directory))
         (extension-match-p (pathname)
           (case extensions
             (:wild t)
             (t (member (pathname-type pathname) extensions :test #'equalp)))))
    (when (distribute-directory-p from-directory)
      (pathname-create-directory-if-needed to-directory)
      (loop for pathname in (directory (make-pathname :name :wild :type :wild :version :newest
                                                      :version :newest :defaults from-directory)
                                       :directories t
                                       #+LispWorks :link-transparency #+LispWorks nil
                                       #+MCL :files #+MCL t #+MCL :directory-pathnames #+MCL t)
            do (cond ((pathname-directory-p pathname)
                      (%copy-directory (push-directory-level to-directory pathname)
                                       (down-directory pathname)
                                       extensions default-version report-stream))
                     ((extension-match-p pathname)
                      (%copy-file pathname to-directory :wild default-version report-stream))
                     (t nil))))))

(defun copy-directory (to-directory from-directory &key (default-version :newest)
                                    (extensions :wild)
                                    (report-stream *standard-output*))
  (flet ((directory-pathname (pathname version)
           (let ((path (translated-pathname pathname)))
             (make-pathname :host (pathname-host path)
                            :device (pathname-device path)
                            :directory (pathname-directory path)
                            :name :wild
                            :type :wild
                            :version version))))
    (declare (inline translated-pathname))
    (%copy-directory (directory-pathname to-directory :wild)
                     (directory-pathname from-directory default-version)
                     (or extensions :wild)
                     (or default-version :newest)
                     report-stream))) 

;;;------------------------------------------------------------------- 
;;;
;;; CL-HTTP DISTRIBUTIONS
;;; 

(defun copy-cl-http-distribution (target &key (extensions *standard-extensions*)
                                         (default-version :newest)
                                         (standard-line-break #+LispWorks :lf #+(or MCL Genera) :cr)
                                         (report-stream *standard-output*))
   
  (setq target (translated-pathname target))
  (loop for directory in *distribution-directories*
        for pathname = (translated-pathname directory)
        for down-target = (make-pathname :host (pathname-host target)
                                         :device (pathname-device target)
                                         :directory (append (pathname-directory target)
                                                            (last (pathname-directory pathname))))
        do (copy-directory down-target pathname
                           :extensions extensions
                           :default-version default-version
                           :report-stream report-stream))
  (loop for file in *distribution-files*
        for pathname = (translated-pathname file)
        do (%copy-file pathname target extensions default-version  report-stream))
  (when standard-line-break
    (standardize-line-breaks target standard-line-break report-stream))
  target)

#+ignore
(http::copy-cl-http-distribution "CL-HTTP-HD1:cl-http-70-167-devo:")

#+ignore
(http::copy-cl-http-distribution "/volumes/CL-HTTP-HD1/cl-http/builds/cl-http-70-209a-devo/")

#+Genera
(cp:define-command
 (com-write-cl-http-distribution
  :command-table "User"
  :provide-output-destination-keyword t)
 ((to '((fs:pathname) :default-name nil :default-type nil :default-version nil) :prompt "To Directory"))
 (copy-cl-http-distribution to
                            :extensions *standard-extensions*
                            :default-version :newest
                            :report-stream *standard-output*))

#+Genera
(cp:define-command
 (com-copy-http-directory
  :command-table "User"
  :provide-output-destination-keyword t)
 ((from '((fs:pathname) :default-name nil :default-type nil :default-version nil) :prompt "From Directory")
  (to '((fs:pathname) :default-name nil :default-type nil :default-version nil) :prompt "To Directory"))
 (copy-directory to from
                 :extensions *standard-extensions*
                 :default-version :newest
                 :report-stream *standard-output*)) 

