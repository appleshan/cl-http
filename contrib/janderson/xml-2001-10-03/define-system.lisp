;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: cl-user; -*-

#|
this defines a micro system definition function to support the
minimal system composition spec used for cl-http

(define-system ((<name symbol> &optional <documentation string>)
                <operations (list (member :probe :compile :compile-load :compile-load-always :load))>
                &rest <files (list string)>) )

|#

(in-package "CL-USER")

#+(or ALLEGRO LispWorks)
(unless (boundp '*.lisp-pathname*)
  (defParameter *.lisp-pathname* (make-pathname :type "lisp")))

(defMacro define-system ((name &key description) (&rest operations) &body files)
  `(execute-system-operations (register-system ',name :description ,description :files ',files)
                              ',operations))

#-(or ALLEGRO LispWorks) ;; this is here for convenience, but allegro already has a definition in EXCL
(defMacro defsystem (name &key source-pathname components &aux module-names)
  `(progn
     ,@(mapcar #'(lambda (module-spec)
                   (destructuring-bind (op name &key components) module-spec
                     (push name module-names)
                     (ecase op
                       (:module `(define-system (,name) ()
                                   ,@(mapcar #'(lambda (file-spec)
                                                (destructuring-bind (&key file) file-spec
                                                  (merge-pathnames file source-pathname)))
                                            components))))))
               components)
     (define-system (,name) () ,@(reverse module-names))))

(defun call-with-src-and-bin (function filename)
  (let ((src-pathname (merge-pathnames filename *.lisp-pathname*))
        (bin-pathname (make-pathname :type "bin" :defaults filename)))
    (funcall function filename src-pathname bin-pathname
             (probe-file src-pathname) (probe-file bin-pathname))))

(defun conditional-compile-file (filename &key always)
  (call-with-src-and-bin
   #'(lambda (filename src bin probed-src probed-bin)
       (cond (probed-src
              (cond ((or always (null probed-bin) (> (file-write-date probed-src) (file-write-date probed-bin)))
                     (when *load-verbose*
                       (format *trace-output* "~%;Compiling ~s -> ~s..." src bin))
                     (compile-file probed-src :output-file (or probed-bin bin)))
                    (*load-verbose*
                     (format *trace-output* "~%;Skipped Compiling ~s." src))))
             (t
              (warn "file missing: ~s." filename))))
   filename))

(defun conditional-load-file (filename)
  (call-with-src-and-bin
   #'(lambda (filename src bin probed-src probed-bin)
       (declare (ignore src bin))
       (if (and probed-bin (or (null probed-src) (> (file-write-date probed-bin) (file-write-date probed-src))))
         (load probed-bin)
         (if probed-src
           (load probed-src)
           (error "neither source nor binary file not found: ~s." filename))))
   filename))

(defGeneric print-universal-time (stream time opt arg)
  (:method ((stream t) (time integer) (opt t) (arg t))
           (multiple-value-bind (second minute hour day month year) (decode-universal-time time)
             (format stream "~4,'0d.~2,'0d.~2,'0dT~2,'0d:~2,'0d:~2,'0d" year month day hour minute second)))
  (:method ((stream t) (time t) (opt t) (arg t))
           (write-string "****.**.**T**:**:**" stream)))

(defun probe-file-src-and-bin (filename)
  (call-with-src-and-bin
   #'(lambda (filename src bin probed-src probed-bin)
       (if probed-src
         (when *load-verbose*
           (format *trace-output*
                   "~%; ~s~@[ (@ ~/print-universal-time/)~] -> ~s~@[ (@ ~/print-universal-time/)~]..."
                   probed-src (file-write-date probed-src) bin (when probed-bin (file-write-date probed-bin))))
         (warn "file missing: ~s (= ~s)." filename src)))
   filename))
    
(defGeneric find-system-named (name &key if-does-not-exist if-exists) 
  (:method ((name symbol) &rest options)
           (unless (keywordp name) (setf name (intern (string-upcase name) "KEYWORD")))
           (destructuring-bind (&key (if-does-not-exist nil) (if-exists name)) options
             (cond ((eq name (get name :system-keyword))
                    (case if-exists
                      (:error
                       (error "Existing system named ~s found." name))
                      (t
                       if-exists)))
                   (t
                    (ecase if-does-not-exist
                      (:error
                       (error "No system named ~s found." name))
                      (:create
                       (setf (get name :system-keyword) name))
                      ((nil)
                       nil))))))
  (:method ((name string) &rest options)
           (apply #'find-system-named (intern (string-upcase name) "KEYWORD") options)))

(defun system-property (system property &rest options &key (if-does-not-exist nil))
  (when (setf system (apply #'find-system-named system :if-does-not-exist if-does-not-exist options))
    (get system property)))

(defun (setf system-property) (new-value system property &rest options
                                         &key (if-does-not-exist :error))
  (when (setf system (apply #'find-system-named system :if-does-not-exist if-does-not-exist options))
    (setf (get system property) new-value)))

(defun system-loaded-p (system)
  (system-property system :system-load-time))

(defun register-system-definition (system pathname)
  (setf (system-property system :system-pathname :if-does-not-exist :create) pathname))

(defun register-system (name &key description files)
  (let ((system-keyword (find-system-named name :if-does-not-exist :create)))
    (setf (system-property system-keyword :system-files) files)
    (setf (system-property system-keyword :system-description) description)
    (when (and (symbolp name) (not (keywordp name)))
      (setf (get name :system-keyword) system-keyword))
    system-keyword))

(defgeneric execute-system-operations (system operations)
  (:method ((system t) (operations null)) system)
  (:method ((system t) (operations cons) &aux files pathname)
           ;; if the system is not yet defined, but is registered, the load the definition
           (unless (setf files (system-property system :system-files :if-does-not-exist :error))
             (when (setf pathname (system-property system :system-pathname))
               (conditional-load-file pathname))
             (setf files (system-property system :system-files)))
           (cond (files
                  (map nil #'(lambda (name)
                               (typecase name
                                 ((or string pathname)
                                  (dolist (op operations)
                                    (when (functionp op)
                                      (funcall op name))
                                    (when (eq op :probe)
                                      (probe-file-src-and-bin name))
                                    (when (member op '(:compile :compile-load :compile-load-always))
                                      (conditional-compile-file name :always (eq op :compile-load-always)))
                                    (when (member op '(:load :compile-load :compile-load-always))
                                      (conditional-load-file name))))
                                 (symbol ; skip successive crossloads
                                  (unless (and (system-loaded-p name)
                                               (not (or (find :probe operations)
                                                        (find-if #'functionp operations))))
                                    (execute-system-operations name operations)))))
                       files)
                  (when (intersection operations '(:load :compile-load :compile-load-always))
                    (setf (system-property system :system-load-time) (get-universal-time)))
                  system)
                 (t
                  (warn "no components present in system: ~s." system))))
  (:method ((system t) (operations symbol))
           (execute-system-operations system (list operations)))
  (:method ((system t) (operations function))
           (execute-system-operations system (list operations))))


:EOF


