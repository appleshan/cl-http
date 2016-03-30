;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: cl-user; Base: 10 -*-

;;; Start up file for CL-HTTP under LispWorks.

;;; Prompts for options when run interactively.

;;; For command line, syntax is: -dump-cl-http <imagename> <options...>
;;; Options are T of present on the command line, NIL is preceeded by "no-".
;;; For a list of valid options options, see the symbol-names of the keywords
;;; listed in *cl-http-options*.

(in-package "CL-USER")

;;; lispm major.minor LispWorks major.minor
(setq *cl-http-server-version* '(70 218 2 1 8))

(setq *handle-existing-defpackage* '(:add :warn))

(defvar *cl-http-options* '((:compile . :ask)
                            (:cold-compile . :ask)
			    (:debug-info . :ask)
			    (:proxy . :ask)
                            #+CLIM(:clim-monitor . :ask)
			    (:w4 . :ask)
                            (:examples . :ask)
			    (:w4-examples . :ask)
			    (:mail-archive . :ask)
			    (:lambda-ir . :ask)
			    (:configure-and-start . :ask)))

(defvar *dump-cl-http-name*
  (member "-dump-cl-http" sys:*line-arguments-list* :test 'string-equal))

(defun get-cl-http-init-script-from-command-line ()
  (second (member "-cl-http-init-script" sys:*line-arguments-list*
                  :test 'string-equal)))

(defun get-cl-http-compilation-options ()
  (let ((incremental-compile-p (cdr (assoc :compile *cl-http-options*)))
        (cold-compile-p (cdr (assoc :cold-compile *cl-http-options*))))
    (values (not (null (or incremental-compile-p cold-compile-p)))
            cold-compile-p)))

(defvar *cl-http-init-script* (or (get-cl-http-init-script-from-command-line)
                                  "http:lw;examples;cl-http-init.lisp"))

(unless (ignore-errors (logical-pathname-translations "HTTP"))
  (setf (logical-pathname-translations "HTTP")
        `(("**;*.*.*" ,(merge-pathnames "**/*.*" 
                                        (truename (pathname-location (current-pathname "../"))))))))

(load "HTTP:lw;pre-cl-http")

(defvar *cl-http-options-data*  `((:compile "Compile changed CL-HTTP files" t)
                                  (:cold-compile "Cold Compile CL-HTTP" nil) 
                                  (:debug-info "Include full debugging information" t)
                                  (:proxy "Include proxy" t)
                                  (:w4 "Include W4 Web Walker" nil)
                                  (:examples "Compile the examples" t)
                                  (:w4-examples "Include examples for W4 Web Walker" nil)
				  #+CLIM(:clim-monitor "Include CL-HTTP Monitor" nil)
                                  (:mail-archive "Include mail-archive example" nil)
                                  (:lambda-ir "Include lambda-ir" nil)
                                  (:configure-and-start "Load all examples and start HTTP server" ,(not *dump-cl-http-name*))))

(cond (*dump-cl-http-name*
       (loop for (key message default) in *cl-http-options-data*
	     do
	     (when (eq (cdr (assoc key *cl-http-options*)) :ask)
	       (setq *cl-http-options*
		     (cons (cons key (if default
					 (null (member (format nil "no-~A" key)
						       (cddr *dump-cl-http-name*)
						       :test 'string-equal))
				       (not (null (member key
							  (cddr *dump-cl-http-name*)
							  :test 'string-equal)))))
			   (delete key *cl-http-options* :key 'car)))))
       (format t "~%~SCreating CL-HTTP image ~S with options:~%"
	       *dump-cl-http-name*)
       (loop for (key message) in *cl-http-options-data*
	     do
	     (format t "~A = ~A~%" key (cdr (assoc key *cl-http-options*))))
       (terpri))
      ((and (member :CAPI *features*) #+CAPI (capi:screens))
       #+CAPI
       (let ((count -1)
	     (options '())
	     (selection '()))
	 (loop for (key message default) in *cl-http-options-data*
	       do
	       (when (eq (cdr (assoc key *cl-http-options*)) :ask)
		 (push (cons key message) options)
		 (if default
		     (push (incf count) selection)
		   (incf count))))
	 (setq options (nreverse options)
	       selection (nreverse selection))
         (when options
	   (let ((list (make-instance 'capi:check-button-panel
				      :items options
				      :selection selection
				      :print-function 'cdr
				      :layout-class 'capi:column-layout
				      :visible-border t)))
	     (if (capi:popup-confirmer list "Choose CL-HTTP options")
	         (let ((on-options (capi:choice-selected-items list)))
		   (loop for (key) in options
		         do
		         (setq *cl-http-options*
			       (cons (cons key (not (null (assoc key on-options))))
				     (delete key *cl-http-options* :key 'car)))))
	       (exit-load))))))
      (t (loop for (key message default) in *cl-http-options-data*
	       do
	       (when (eq (cdr (assoc key *cl-http-options*)) :ask)
		 (setq *cl-http-options*
		       (cons (cons key (not (null (y-or-n-p (format nil "~A (y/n)?" message)))))
			     (delete key *cl-http-options* :key 'car)))))))

(when (cdr (assoc :w4 *cl-http-options*))
  (push :w4 *features*)
  ;; don't load the examples without loading W4 -- JCMa 10/20/2006
  (when (cdr (assoc :w4-examples *cl-http-options*))
    (push :w4-examples *features*)))

(let ((startp (assoc :configure-and-start *cl-http-options*)))
  (when (and (cdr startp) (not mp::*multiprocessing*))
    (cerror "Continue without them."
	    "Must have multiprocessing to load the examples.")
    (setf (cdr startp) nil)))

;;; Load system definitions
;;;
(load "HTTP:clim;clim-sys;lw-sysdcl.lisp")

(load "HTTP:lw;server;sysdcl.lisp")

(load "HTTP:lw;translations.lisp")

(load "HTTP:lw;client;sysdcl.lisp")

(eval `(defsystem cl-http-examples (:default-type :system)
	 :members ,(append '(cl-http-base-examples)
			   #+W4-Examples '("W4-EXAMPLES"))
	 :rules ((:in-order-to :compile :all
			       (:requires (:load :serial))))))

(eval `(defsystem cl-http-build-system (:default-type :system)
	 :members ,(append '(clim-sys
			     cl-http)
			   (and (cdr (assoc :proxy *cl-http-options*))
				'("HTTP-PROXY"))
			   '(http-base-client)
                           #+CLIM
                           (and (cdr (assoc :clim-monitor *cl-http-options*))
                                '("HTTP-UI"))
			   #+W4 '("W4")
			   (and (cdr (assoc :mail-archive *cl-http-options*))
				'("MAIL-ARCHIVE"))
			   (and (cdr (assoc :lambda-ir *cl-http-options*))
                                (if (cdr (assoc :mail-archive *cl-http-options*))
                                    '("LAMBDA-IR-AND-MAIL-ARCHIVE-INDEX")
                                  '("LAMBDA-IR"))))
	 :rules ((:in-order-to :compile :all
                  (:requires (:load :serial))))))

(let* ((debug-info (cdr (assoc :debug-info *cl-http-options*)))
       (compiler::*source-file-recording* debug-info)
       (compiler::*source-level-debugging* debug-info)
       #+(or unix (not (or LispWorks3.2 LispWorks4.0)))
       (compiler::*notice-changed-definitions* debug-info)
       (compiler::*produce-xref-info* debug-info)
       (compiler::*load-xref-info*  debug-info)
       (*compile-verbose* 0) ;print file names
       (*compile-print* 0))		;print warnings)
  (when debug-info
    (proclaim '(optimize (debug 3)))
    (pushnew  :cl-http-debugging *features*)) ;remember if we are compiling with debugging -- JCMa 12/11/2003
  (multiple-value-bind (compile-p recompile-p)
      (get-cl-http-compilation-options)
    (if compile-p
        (compile-system 'cl-http-build-system :load t :force recompile-p)
      (load-system 'cl-http-build-system)))

  ;; MJS 19Oct01: Ideally cl-http-examples would be compiled
  ;; regardless of *dump-cl-http-name* and specify :load :no, but the
  ;; examples won't compile without the configuration loaded because
  ;; they need other examples that modify the HTTP package to be
  ;; loaded first.
  (when (and (cdr (assoc :examples *cl-http-options*))
             (not *dump-cl-http-name*))
    (multiple-value-bind (compile-p recompile-p)
        (get-cl-http-compilation-options)
      (declare (ignore compile-p))
      (compile-system 'cl-http-examples :load t :force recompile-p)))

  (when (cdr (assoc :configure-and-start *cl-http-options*))
    (load-system 'cl-http-examples)))

(when *dump-cl-http-name*
  (save-image (second *dump-cl-http-name*)
	      :restart-function 'www-utils:lw-cold-enable-http-service
	      #-LispWorks3.2 :remarks #-LispWorks3.2 "CL-HTTP server")
  (quit))

(when (cdr (assoc :enable *cl-http-options*))
  (http:enable-http-service))
