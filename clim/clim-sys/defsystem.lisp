;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: CL-USER; Base: 10 -*-

(in-package "CL-USER")

;;; Self contained minimal defsystem.
;;; Copyright (C) 1994, 1995 Olivier (OBC) all rights reserved.
;;; See copyright notice in CLIM-SYS;MINIPROC file.
;;;

#||
;;; Test
(defsystem "defsys"
    (:pretty-name "Minimal defsys"
     :default-pathname *default-pathname-defaults*)
  (:serial "logidef"
	   "defsystem"))

(compile-system "defsys")
(load-system "defsys")
||#

(defun name-sys (name)
  (typecase name
    (string
     (intern (string-upcase name) (find-package "CL-USER")))
    (symbol name)
    (t nil)))

(defmacro find-sys (name)
  `(getf (symbol-plist (name-sys ,name)) 'msdef))

(defmacro defsys (name options . directives)
  `(setf (find-sys ',name) (cons ',options ',directives)))

(unless (boundp '*original-definitions*)
  (let ((ld (make-pathname :name "logidef" :defaults *load-pathname*)))
    (if (probe-file ld)
	(load ld))))

;;; There are more complete definitions in logidefs but
;;; this is sufficient to load a standalone defsystem.
;;;
(eval-when (compile load eval)
(unless (boundp '*original-definitions*)
(defvar *original-definitions* (make-hash-table))

;; CLISP as serious problems redefinition methods with names of function
;; so we need to make these methods upfront because logidef will redefine
;; them as part of bootstrapping...

#+CLISP
(defgeneric original-definition (sym &optional lambda-list method))

(#-CLISP defun #+CLISP defmethod original-definition #-CLISP (sym) #+CLISP (sym &optional lambda-list method)
  #+CLISP (declare (ignore lambda-list method))
  (multiple-value-bind (value found)
      (gethash sym *original-definitions*)
    (if found
	value
      (setf (gethash sym *original-definitions*) (or (macro-function sym)
						     ;; fboundp for clisp
						     (and (fboundp sym) (fdefinition sym)))))))

#+CLISP
(defgeneric original-function-p (sym))

(#-CLISP defun #+CLISP defmethod original-function-p (sym)
  (and (fboundp sym)
       (eql (original-definition sym) (or (macro-function sym)
					  (fdefinition sym)))))

(defmacro define-method (&rest mspec)
  `(defmethod ,@mspec))

(defvar *denotify* #+Franz-Inc nil #-Franz-Inc t)

(defmacro handling-redefinition (&rest body)
  `(#+CLISP without-package-lock #+CLISP (COMMON-LISP) #-CLISP progn
  (let ((*ENABLE-PACKAGE-LOCKED-ERRORS* nil))
    (declare (global *ENABLE-PACKAGE-LOCKED-ERRORS*))
   (handler-bind (((or simple-error error condition)
		   ;; package-locked-error?
		   #'(lambda (c)
		       (if *denotify*
			   (format t "~&;;; Redefinition: ~a.~%"
				   c))
		       (continue)))
		  ((or package-error package-locked-error)
		   #'(lambda (c)
		       (if *denotify*
			   (format t "~&;;; Package error: ~a.~%" c))
		       (continue))))
     ,@body))))

;; Note CLISP bug, unable to use #1= in both (independent) definitions
;;
(defmacro define-function (name redefine &rest mspec)
  (let ((#0=#:original (original-definition name)))
    (cond ((eql redefine :redefine)
	   `(let ((#0# (original-definition ',name)))
	      (macrolet ((original-function () '#0#))
		(if '#0# (fmakunbound ',name))
		(defun ,name ,@mspec))))
	  ((null #0#)
	   `(defun ,name ,redefine ,@mspec))
	  (*denotify*
	   (format t "~&Reusing ~a." name)))))

(defmacro define-macro (name redefine &rest mspec)
  (let ((#1=#:original (original-definition name)))
    (cond ((eql redefine :redefine)
	   `(let ((#1# (original-definition ',name)))
	      (macrolet ((original-macro () '#1#))
		(if '#1# (fmakunbound ',name))
		(defmacro ,name ,@mspec))))
	  ((null #1#)
	   `(defmacro ,name ,redefine ,@mspec))
	  (*denotify*
	   (format t "~&Reusing ~a." name)))))
)
)

(unless (fboundp 'translate-logical-pathname)
(defun translate-logical-pathname (path)
  (pathname path)))

(when (or (not (fboundp 'append-logical-pathname))
	  (original-function-p 'translate-logical-pathname))
  (fmakunbound 'append-logical-pathname)
(defmethod append-logical-pathname ((path pathname) (logical string) &rest options)
  (apply #'make-pathname :defaults (merge-pathnames logical path) options)))

(define-method transform-sys ((sym symbol) operation &rest functions)
  (let ((sysdef (find-sys sym)))
    (if sysdef
	(destructuring-bind ((&key pretty-name default-pathname &allow-other-keys) &rest directives)
	    sysdef
	  (format t "~&;~A System ~S..." operation pretty-name)
	  (if (symbolp default-pathname)
	      (setq default-pathname (eval default-pathname)))
	  (if default-pathname
	      (if (original-function-p 'translate-logical-pathname)
		  (setq default-pathname (pathname default-pathname))
		(setq default-pathname (translate-logical-pathname default-pathname))))
	  (apply #'process-directives directives default-pathname operation functions)))))

(define-method process-directives ((directives cons) (default-pathname pathname) operation &rest functions)
  (loop for directive in directives
      do (destructuring-bind (type &rest modules) directive
	   (ecase type
	     (:serial
	      (loop for module in modules
		  do (etypecase module
		       ((or string symbol)
			(loop for function in functions
			    do (funcall function module default-pathname)))
		       (cons
			(apply #'process-directives module default-pathname operation functions)))))
	     (:parallel
	      (loop for function in functions
		  do (loop for module in modules
			 do (etypecase module
			      ((or string symbol)
			       (funcall function module default-pathname))
			      (cons
			       (apply #'process-directives module default-pathname operation functions))))))
	     (:module (princ "Ignoring: ") (prin1 modules) (terpri))))))

(defvar *system-load-table* (make-hash-table :test #'equal))

(defvar *system-load-list* nil)

(defvar *system-reload* nil)

(defvar *system-recompile* nil)

(defvar *source-load-error* nil)

(defun clear-load ()
  (setq *system-load-list* nil)
  (clrhash *system-load-table*))

(defmethod binary-load-date ((binary pathname))
  (gethash binary *system-load-table*))

(defmethod (setf binary-load-date) ((date t) (binary pathname))
  (cond ((gethash binary *system-load-table*))
	(t
	 (push binary *system-load-list*)))
  (setf (gethash binary *system-load-table*) date))

;; This implements a workaround version of save-image
;; make a single binary file of all files in order loaded so far

(defmethod save-binary-load ((image string) &optional (save-list (reverse *system-load-list*)))
  (save-binary-load (translate-logical-pathname image) save-list))
  
(defmethod save-binary-load ((image pathname) &optional (save-list (reverse *system-load-list*)))
  (let ((buffer (make-string 4096)))
    (with-open-file (to-stream image :direction :output :if-exists :new-version :if-does-not-exist :create)
      (loop for binary in save-list
	    do (with-open-file (from-stream binary :direction :input)
		  (loop with buffer-size = (length buffer)
			as end = (read-sequence buffer from-stream :end buffer-size)
			do (write-sequence buffer to-stream :end end)
			when (< end buffer-size)
			do (finish-output to-stream)
			until (zerop end)))))))

(define-method show-sys ((sym symbol))
  (transform-sys sym "Show" #'(lambda (module path)
				  (format t "~&;Module: ~a ~a~%" module path))))

(define-method compile-sys ((sym symbol) &key ((:reload *system-reload*) *system-reload*) ((:recompile *system-recompile*) *system-recompile*) &allow-other-keys)
  (let (result)
    (flet ((compiler (m d)
	     (if (compile-softly m d)
		 (setq result t))))
      (transform-sys sym "Compiling" #'compiler #'load-file-only))
    result))

(define-method load-sys ((sym symbol) &key ((:reload *system-reload*) *system-reload*) ((:error *source-load-error*) *source-load-error*) &allow-other-keys)
  (let (result)
    (flet ((loader (m d)
	     (if (load-softly m d)
		 (setq result t))))
      (transform-sys sym "Loading" #'loader))
    result))

(unless (boundp '*source-type*)
(defparameter *source-type* "lisp")
(defparameter *binary-type* #+ACL5 "fasl" #+CLISP "fas" #-(or CLISP ACL5) "fsl" #+(and UNIX (not (or CLISP ACL5))) "o")
)

(defun ensure-compatible-source (module default-pathname)
  (let ((source (append-logical-pathname default-pathname module :type *source-type*)))
    (if (probe-file source)
	(values module default-pathname source)
	;; old lispm compatibility
	(let ((dmodule (string-downcase module))
	      (ddpath (pathname (string-downcase (namestring default-pathname)))))
	  (let ((dsource (append-logical-pathname ddpath dmodule :type *source-type*)))
	    (cond ((probe-file dsource)
		   (warn "Module ~a is really ~a." module dsource)
		   (values dmodule ddpath dsource))
		  (t
		   (error "Source not found, ~a." source))))))))

(define-method compile-softly ((module string) (default-pathname pathname))
  (let (source binary)
    (multiple-value-setq (module default-pathname source)
      (ensure-compatible-source module default-pathname))
    (cond (*binary-type*
	   (setq binary (make-pathname :type *binary-type* :defaults source)))
	  ((and (probe-file source)
		(setq binary (funcall (original-definition 'compile-file) source)))
	   (setq *binary-type* (pathname-type binary))))
    (when (or *system-recompile*
	      (if (and binary
		       (probe-file binary))
		  (> (file-write-date source) (file-write-date binary))
		t))
      (setq binary (funcall (original-definition 'compile-file) source :output-file binary))
      (if binary
	  (setf (binary-load-date binary) 0))
      binary)))

(define-method compile-softly ((module symbol) (default-pathname pathname))
  (compile-sys module)) 

(define-method load-softly ((module string) (default-pathname pathname))
  (let (source binary)
    (multiple-value-setq (module default-pathname source)
      (ensure-compatible-source module default-pathname))
    (if *binary-type*
	(setq binary (make-pathname :type *binary-type* :defaults source)))
    (if (not (probe-file source))
	(setq source nil))
    (cond ((if (and source binary)
	       (if (probe-file binary)
		   (> (file-write-date source) (file-write-date binary))
		 t))
	   (if *source-load-error*
	       (error "Source module needs compiling, ~a." source)
	     (warn "Source module needs compiling, ~a." source))
	   (funcall (original-definition 'load) source))
	  ((probe-file binary)
	   (let ((update (file-write-date binary)) result)
	     (when (or *system-reload*
		       (> update (or (binary-load-date binary) 0)))
	       (setq result (funcall (original-definition 'load) binary))
	       (if result
		   (setf (binary-load-date binary) update))
	       result)))
	  (t
	   (error "Source and binary not found, ~a." source)))))

(define-method load-softly ((module symbol) (default-pathname pathname))
  (load-sys module))

(define-method load-file-only ((module string) (default-pathname pathname))
  (load-softly module default-pathname))

(define-method load-file-only ((module symbol) (default-pathname pathname))
  (if *system-reload*
      (load-sys module)))

;;; Implant a mini defsystem if unavailable or shadow and simplify
;;; proprietary existing defsystem. This attempts to preserve existing
;;; defsystem and system definitions.
;;;
(handling-redefinition
(define-macro defsystem :redefine (sys &rest options)
  (if (or (find-sys sys) (stringp sys))
      `(defsys ,sys ,@options)
    (if (original-macro)
	(funcall (original-macro) `(defsystem ,sys ,@options) nil)
      `(defsys ,sys ,@options)))))

#+ignore
(defmacro defsystem (sys &rest options)
  `(defsys ,sys ,@options))

(handling-redefinition
(define-function show-system :redefine (sys &rest options)
  (if (or (find-sys sys) (stringp sys))
      (apply #'show-sys (name-sys sys) options)
    (apply (original-function) sys options)))
)

(define-function compile-system :redefine (sys &rest options)
  (if (or (find-sys sys) (stringp sys))
      (apply #'compile-sys (name-sys sys) options)
    (apply (original-function) sys options)))

(define-function load-system :redefine (sys &rest options)
  (if (or (find-sys sys) (stringp sys))
      (apply #'load-sys (name-sys sys) options)
    (apply (original-function) sys options)))

