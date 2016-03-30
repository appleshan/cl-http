;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: Micro-CLIM-Defsystem; -*-
;;;
;;; Copyright 2003, John C. Mallery.
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;;  MICRO-DEFSYSTEM BACKEND INTO CLIM-DEFSYSTEM
;;; 

(eval-when (:execute :compile-toplevel :load-toplevel)
   (defpackage micro-clim-defsystem
      (:nicknames mcds)
      (:use #-Genera"COMMON-LISP" #+Genera"FUTURE-COMMON-LISP")
      (:export "*STANDARD-SYSTEMS*"
                    "*SYSTEMS*"
                    "*SYSTEM-DEFINE-HOOK*"
                    "*SYSTEM-UNDEFINE-HOOK*" 
                    "DEFINE-SYSTEM"
                    "EXECUTE-SYSTEM-OPERATIONS"
                    "FIND-SYSTEM-NAMED" 
                    "GET-SYSTEM-NAME"
                    "LOAD-STANDARD-SYSTEMS"
                    "NOTE-SYSTEM-DEFINITIONS"
                    "SYSTEM-FILES"
                    "REGISTER-SYSTEM"
                    "UNREGISTER-SYSTEM"))
   (do-external-symbols (sym :micro-clim-defsystem)
      (import sym :cl-user))
   (import (intern "*ALWAYS-COMPILE*" :cl-user) :micro-clim-defsystem))

(in-package :micro-clim-defsystem)

(defparameter *always-compile* (if (boundp '*always-compile*) (symbol-value '*always-compile*) nil)
  "When non-null all files are compiled, whether or not they need it.")

(defvar *systems* nil) 
(defvar *system-define-hook* nil)
(defvar *system-undefine-hook* nil) 

(defun execute-system-operations (system operations)
   (flet ((check-operation-arg (arg)
               (let ((known-operations '(:load :compile :eval-load :compile-load :compile-load-always)))
                  (unless (member arg known-operations)
                     (error "~S is not one of the known options for OPERATIONS."arg known-operations)))))
      (unless (listp operations)
         (setq operations (list operations)))
      ;; check arguments to operations
      (mapc #'check-operation-arg operations)
      ;; Perform operations
      (cond ((or *always-compile* (member :compile-load-always operations))
                 (clim-ds:compile-system system :recompile t :reload t :include-components t))
               ((member :compile-load operations)
                 (clim-ds:compile-system system :reload t :include-components t))
               ((member :compile operations)
                 (clim-ds:compile-system system :include-components t))
               ((intersection '(:eval-load :load) operations)
                 (clim-ds:load-system system :reload t :recurse t :source-if-newer t))
               (t (error "No action. Bad arg to execute-system-operations?")))
      system)) 

(defun get-system-name (system)
   (let ((system (clim-ds::find-system-named system)))
      (when system (clim-ds::system-name system))))

(defun register-system (system name &optional description)
   (declare (ignore name))
   (let ((system (find-system-named system t)))
      (prog1 (pushnew system *systems*)
	 (if description
            (setf (get (clim-ds::system-name system) :system-description) description)
            (remprop (clim-ds::system-name system):system-description))
         (mapc #'funcall *system-define-hook*))))

(defun unregister-system (system)
   (let ((sys (find-system-named system t)))
      (setq *systems* (delete sys *systems*))
      (mapc #'funcall *system-undefine-hook*)
      (remprop (clim-ds::system-name system):system-description)))

(defmacro define-system ((name &key description) (&rest operations) &body files)
   "Operations can be :load and :compile."
   (labels ((coerce-file-spec (pathname)
                   (let ((pathname (pathname pathname)))
                      `(,(pathname-name pathname)
                         :pathname ,(namestring pathname)
                         :compile-satisfies-load nil
                         :language :lisp
                         :for-compilation-only nil)))
                (coerce-module-specs (specs)
                   (loop for item in specs
                            collect (if (stringp item) 
                                          (coerce-file-spec item)
                                          (coerce-file-spec (third item))))))
      (let ((modules (coerce-module-specs files)))
         `(prog1
             (clim-ds::defsystem ,name () ,@modules)
             (register-system ',name ',name ',description)
             ,(when operations
                   ` (execute-system-operations ',name ',operations))))))

(defun find-system-named (system &optional (error-p t))
   (if error-p
      (clim-defsystem::find-system system)
      (ignore-errors (clim-defsystem::find-system system))))

(defun system-map-files (system function &key (file-type :source) (include-components t))
   (flet ((relevant-subsystems (s)
               (remove-if-not #'(lambda (s) (clim-defsystem::system-loaded-p (clim-defsystem::find-system-named s)))
                                       (clim-defsystem::system-needed-systems (clim-defsystem::find-system-named s))))
            (map-files (system function file-type)
                (dolist (module (clim-defsystem::system-module-list (clim-defsystem::find-system-named system)))
                   (let ((pathname (ecase file-type
                                              (:source (clim-defsystem::module-src-path module))
                                              (:binary (clim-defsystem::module-bin-path module)))))
                      (when pathname
                          (funcall function pathname))))))
      (declare (dynamic-extent #'relevant-subsystems #'map-files))
      (if include-components 
         (dolist (sys (clim-defsystem::expand-subsystems (list system) #'relevant-subsystems))
	    (map-files sys function file-type))
         (map-files system function file-type)))) 

(defun system-edit-files (system &key (include-components t))
   (system-map-files system #'ed :include-components include-components))

(defun system-files (system &key (file-type :source) (include-components t))
   "Returns the files of SYSTEM in compile-load order."
   (let ((files nil))
      (flet ((collect-file (p)
                  (push p files)))
         (declare (dynamic-extent #'collect-file))
         (system-map-files system #'collect-file :file-type file-type :include-components include-components)
         (return-from system-files (nreverse files)))))

(defun register-system-definition (name pathname)
   (let ((name-string (string name)))
      (setf (clim-ds:system-source-file name-string) (pathname pathname))))

(defmacro note-system-definitions (&body system-clauses)
   `(progn . ,(loop for (keyword pathname) in system-clauses
                           collect `(register-system-definition ',keyword ',pathname))))

(defparameter *standard-systems* nil)

(defun load-standard-systems (&optional (systems *standard-systems*))
   (loop for system in systems
            for pathname = (clim-ds:system-source-file system)
            do (load pathname :verbose t)
            (execute-system-operations system :compile-load)))

