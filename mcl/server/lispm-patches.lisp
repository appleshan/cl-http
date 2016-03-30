;;; -*- Syntax: Ansi-Common-Lisp; Package: www-utils; Base: 10; Mode: lisp -*-

;;; (C) Copyright 1995, 2000, John C. Mallery.
;;;     All Rights Reserved.
;;; 

;;;------------------------------------------------------------------- 
;;;
;;; LOADING LISP MACHINE PATCH FILES
;;;

;;; This file allows other Common Lisps to compile and load patch files for
;;; the Lisp Machine.

(in-package :cl-user)

(add-nickname "WWW-UTILS" "SCT")

(add-nickname "WWW-UTILS" "SCL")

(in-package :www-utils)

;; export the relevant functions
(mapc #'(lambda (x) (export (intern x :www-utils) :www-utils))
      `("FILES-PATCHED-IN-THIS-PATCH-FILE" "BEGIN-PATCH-SECTION" "PATCH-SECTION-SOURCE-FILE" "PATCH-SECTION-ATTRIBUTES" "LOAD-PATCH"
         "REQUIRE-PATCH-LEVEL" "REQUIRE-PATCH-LEVEL-FOR-PATCH" "FUNDEFINE")) 

;;;------------------------------------------------------------------- 
;;;
;;;  LISPM PATCH FILE OPERATORS
;;;

(defun files-patched-in-this-patch-file (&rest pathnames)
  (declare (ignore pathnames)))

(defun begin-patch-section ())

(defun patch-section-source-file (pathname)
  pathname)

;(defun patch-section-attributes (attribute-list)
;  (flet ((get-attributes (string &optional (start 0) (end (length string)))
;	   (loop for colon fixnum = (position #\: string :start start :end end)
;		 while colon
;		 for semi fixnum = (or (position #\; string :start colon :end end)
;				       (search "-*-" string :start1 0 :end1 3 :start2 (1+ colon) :end2 end :test #'eql))
;		 while semi 
;		 for key = (http::symbolize (string-trim "-* " (subseq string start colon)) http::*keyword-package*)
;		 for value = (string-trim " " (subseq string (1+ colon) semi))
;		 collect key
;		 collect value
;		 do (setq start (1+ semi)))))
;    (let* ((plist (get-attributes attribute-list
;				  (position-if-not #'(lambda (ch) (member ch '(#\* #\-) :test #'eql)) attribute-list)))
;	   (package (getf plist :package))
;	   (pos (position-if-not #'(lambda (ch) (member ch '(#\( #\space) :test #'eql)) package))
;	   interned-package)
;      (unless (zerop pos)
;	(setq package (subseq package pos (position-if #'(lambda (ch) (member ch '(#\( #\space) :test #'eql)) package :start pos))))
;      (setq interned-package (find-package (string-upcase package)))
;      (if interned-package
;	  #+MCL (setq *package* interned-package)
;	  #-MCL (in-package package)
;	  (error "Couldn't find the package, ~S." package)))))

(defun %patch-section-attributes (attribute-list)
  (flet ((get-attributes (string &optional (start 0) (end (length string)))
	   (loop for colon fixnum = (position #\: string :start start :end end)
		 while colon
		 for semi fixnum = (or (position #\; string :start colon :end end)
				       (search "-*-" string :start1 0 :end1 3 :start2 (1+ colon) :end2 end :test #'eql))
		 while semi 
		 for key = (http::symbolize (string-trim "-* " (subseq string start colon)) http::*keyword-package*)
		 for value = (string-trim " " (subseq string (1+ colon) semi))
		 collect key
		 collect value
		 do (setq start (1+ semi)))))
    (let* ((plist (get-attributes attribute-list
				  (position-if-not #'(lambda (ch) (member ch '(#\* #\-) :test #'eql)) attribute-list)))
	   (package (getf plist :package))
	   (pos (position-if-not #'(lambda (ch) (member ch '(#\( #\space) :test #'eql)) package))
	   interned-package)
      (unless (zerop pos)
	(setq package (subseq package pos (position-if #'(lambda (ch) (member ch '(#\( #\space) :test #'eql)) package :start pos))))
      (setq interned-package (find-package (string-upcase package)))
      (if interned-package
	  #+MCL `(setq *package* ,interned-package)
	  #-MCL `(in-package ,(package-name interned-package))
	  (error "Couldn't find the package, ~S." package)))))

;; macro required for compiler to pay attention in some implementations -- 3/21/2000 -- JCMa.
(defmacro patch-section-attributes (attribute-list)
  (%patch-section-attributes attribute-list))

#|(PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: (netscape :use (future-common-lisp html2 www-utils url) :nicknames (ntsp)); BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")|# 

(define-condition require-patch-level
                           (error)
                           ((system-patch-specs :initform nil :initarg :system-patch-specs :reader system-patch-specs))
   (:report (lambda (cond stream)
                   (let ((specs (system-patch-specs cond)))
                   (format stream "Compiling or loading the current patch requires the following patch levels in the related system~P:~
                                             ~:{~&~10T~A ~D.~D ~:^~}"
                                (length specs) specs)))))

;; This function is a dummy that makes the user ensure that systems are up to the correct patch-level. -- JCMa 3/13/2000.
(defun require-patch-level-for-patch (&rest system-major-minor-specs)
   "Signals an error if the current lisp image is not patched to a certain level.
systems specs are: (SYSTEM-NAME-STRING MAJOR-VERSION MINOR-VERSION)."
   (cerror "Continue only after loading the required patches" 'require-patch-level :system-patch-specs system-major-minor-specs))

#|(REQUIRE-PATCH-LEVEL-FOR-PATCH '(CL-HTTP 70. 20.))|#

;; This function should undefine a function or method in the current lisp implementation.
(defun fundefine (function-spec)
   (etypecase function-spec
      (symbol (fmakunbound function-spec))
      #+MCL
      (ccl::method (ccl::remove-method (ccl::method-generic-function function-spec) function-spec))))

;;;------------------------------------------------------------------- 
;;;
;;;  LOADING A PATCH FILE
;;;

#+MCL
(defmacro inhibit-redefinition-warnings (&body body)
   `(let ((ccl:*warn-if-redefine* nil)) . ,body))

#+MCL
(defun compile-load-file (pathname &key recompile)
   (ccl::compile-load pathname :force-compile recompile)) 

(defun get-lispm-patch-file-directory-components (url)
   (loop for directory-components = (url:path url) then (cdr directory-components)
            while directory-components
            when (equalp (car directory-components) "lispm")
            return directory-components
            finally (error "The URL, ~S, does not denote a Lisp Machine patch file." url)))

(defmethod get-local-cl-http-patch-pathname ((url url:http-url))
  (flet ((get-lispm-patch-file-directory-components (url)
	   (loop for directory-components = (url:path url) then (cdr directory-components)
		 while directory-components
		 when (equalp (car directory-components) "lispm")
		   return directory-components
		 finally (error "The URL, ~S, does not denote a Lisp Machine patch file." url))))
    (make-pathname :host "http" 
		   :directory `(:absolute ,.(get-lispm-patch-file-directory-components url))
		   :name (url:object url)
		   :type (url:extension url)))) 

(defmethod get-patch-source ((url url:http-url) &optional request-headers pathname)
  (declare (ignore request-headers))
  (let ((path (or pathname (get-local-cl-http-patch-pathname url))))
    (loop doing (with-simple-restart (rety-fetching-patch "Retry Fetching Patch URL ~A" (url:name-string url))
		  (http:copy-file url path :copy-mode :text)
		  (return url)))))

(defmethod load-patch ((url url:http-url) &key recompile update-source request-headers)
  (let ((pathname (get-local-cl-http-patch-pathname url)))
    (when (or update-source (not (probe-file pathname)))
      (get-patch-source url pathname request-headers))
    (load-patch pathname :recompile recompile)))

(defmethod load-patch ((pathname pathname) &key recompile update-source request-headers)
  (declare (ignore update-source request-headers))
  (with-simple-restart (skip-loading-patch "Skip Loading Patch File ~A" pathname)
    (loop doing (with-simple-restart (retry-loading-patch "Retry Loading Patch File ~A" pathname)
		  (inhibit-redefinition-warnings
		    (compile-load-file pathname :recompile recompile)
		    (return pathname))))))

(defmethod load-patch ((identifier string) &key recompile update-source request-headers)
  (let ((identifier (if (url:url-string-p identifier)
			(url:intern-url identifier :if-does-not-exist :create)
			(pathname identifier))))
    (load-patch identifier :recompile recompile :update-source update-source :request-headers request-headers)))

(defgeneric load-patch (identifier &key recompile update-source request-headers)
   (:documentation "Loads a patch denoted by IDENTIFIER into the current Lisp image.
When RECOMPILE is non-null,  the patch will be compiled regardless of whether it has
already been compiled. When UPDATE-SOURCE is non-null, the patch will be refetched
over the web. REQUEST-HEADERS can be an optional header plist to use when requesting
the patch over the web."))
               
#|   
(setq u #u"http://wilson.ai.mit.edu/cl-http/sources/lispm/client/patch/http-client-substrate-3/http-client-substrate-3-3.lisp")
(get-local-cl-http-patch-pathname u) 

(url-string-p "news:foo;bar.lisp")

(load-patch "http://wilson.ai.mit.edu/cl-http/sources/lispm/client/patch/http-client-substrate-3/http-client-substrate-3-3.lisp")
|#
