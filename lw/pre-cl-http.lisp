;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: CL-USER; Base: 10 -*-

(in-package "CL-USER")

(defparameter *private-patches-directory*
  (or #+LispWorks3.2
      "~/LispWorks/private-patches/"
      #+(and LispWorks4.0 Harlequin-PC-Lisp)
      "z:/pcl-release/pcl/local/images/uninstalled-patches/"
      (pathname-location (current-pathname)) ; last chance guess
      ))

(pushnew :CLIM-SYS *features*)
(setq *packages-for-warn-on-redefinition*
      (remove "CLIM-SYS" *packages-for-warn-on-redefinition*
	      :test 'equal))

#-LispWorks3.2
(pushnew :ANSI-documentation *features*) ;controls whether doc-type is optional or not

(pushnew :multi-threaded *features*)

(pushnew :http-chunking *features*)

(pushnew :cl-http-file-author *features*) ;show file authors in directory lists

#+(or LispWorks3.2 LispWorks4.0 LispWorks4.1 LispWorks4.2 LispWorks4.3 LispWorks4.4)
(when (member :win32 *features*)
  (pushnew :MSWindows *features*))

#+UNIX 
(pushnew :lispworks-unix *features*)
#-UNIX
(pushnew :lispworks-pc *features*)

(when (member :ccl *features*)
  (warn "************>>>>>>>>> removing CCL from features!")
  (setq *features* (remove :ccl *features*)))

#+LispWorks3.2
(do-demand-pre-loads :comms)

#+(and LispWorks3.2 compile-Reverse)
(defadvice (comm::create-socket-for-service-connection htons-the-port :around) (service port)
  (call-next-advice service (if (zerop port)
                                port
                              (+ (ash (logand port 255) 8)
                                 (logand (ash port -8) 255)))))

#+LispWorks3.2
(let ((symbol (intern "IGNORABLE" "CL")))
  (proclaim `(declaration ,symbol)))

#+LispWorks3.2
(defun quit ()
  (bye))

#-LispWorks3.2
(require "comm")

#|; Tell 64bit Lisp where the openssl implementation is.
;; See special 64bit compilation of openssl by LispWorks. Applies to pre-10.5 systems
;; everyone should install the library, and we'll be at 10.5 soon enough
#+(and Darwin Lispworks-64bit)
(when (member (machine-instance) '("og5.csail.mit.edu" "Relatus.local") :test #'equalp)
  ;; Tell LW where the OpenSSL library is
  ;;(comm:set-ssl-library-path "/usr/local/openssl64/lib/libssl.dylib")
  (pushnew :OpenSSL-64bit *features*))|#

#+(and Darwin Lispworks-64bit)
(pushnew :OpenSSL-64bit *features*)

;; LispWorks 4.4 provides SSL streams -- JCMa 3/23/2006
#-(or LispWorks3.2 LispWorks4.0 LispWorks4.1 LispWorks4.2 LispWorks4.3
      (and LispWorks5 Lispworks-64bit (not OpenSSL-64bit))
      (and LispWorks5 MSWindows LispWorks-Personal-Edition))
(pushnew :cl-http-ssl *features*)

#+LispWorks4.4 
(when (fboundp 'comm::x509-get-serial-number) ;supported with patch in LW 4.4
  (pushnew :cl-http-ssl-client *features*)
  (pushnew :cl-http-x509 *features*))

#+(and cl-http-ssl (or LispWorks5 LispWorks6)  (or (not Lispworks-64bit) :OpenSSL-64bit))
(progn
  (pushnew :cl-http-ssl-client *features*)
  (pushnew :cl-http-x509 *features*))

;;(lw:defsystem IMAGE-SUBSTRATE ())

(defmacro sct-defsystem (name options &rest module-list)
  (translate-to-lw-defsystem name options module-list))

(defun translate-to-lw-defsystem (name options module-list)
  (let ((new-options '())
	(required-systems '()))
    (loop while options
	  do
	  (let ((option (pop options))
		(value (pop options)))
	    (ecase option
	      (:pretty-name)
	      (:journal-directory)
	      (:initial-status)
	      (:patchable)
	      (:source-category)
	      (:default-pathname
	       (let ((pathname (pathname value)))
		 (push option new-options)
		 (push (if (typep pathname 'logical-pathname)
			   (translate-logical-pathname pathname)
			 (string-downcase (namestring pathname)))
		       new-options)))
	      (:required-systems
	       (setq required-systems
		     (append required-systems
			     (loop for system in (if (listp value)
						     value
						   (list value))
				   collect `(,system :type :system))))))))
    (let ((expanded-module-list '())
	  (submodules '()))
      (flet ((translate-module
	      (module-spec)
	      (let ((result '()))
		(loop (when (null module-spec)
			(return))
		      (let ((item (pop module-spec)))
			(cond ((symbolp item)
			       (let ((submodule (assoc item submodules)))
				 (if submodule
				     (if (eq :system (second (assoc :type (cddr submodule))))
					 (progn
					   (push `(,(car (second submodule)) :type :system) result)
					   (setq submodules
						 (delete item submodules :key 'car)))
				       (error "~S does not name a system module.")
				       #+comment
				       (setq module-spec
					     (append (second submodule) module-spec)))
				   (error "Unknown submodule ~S" item))))
			      ((or (typep item 'logical-pathname)
				   (and (stringp item)
					(typep (parse-namestring item) 'logical-pathname)))
			       (push (namestring (translate-logical-pathname item))
				     result))
			      (t (push (string-downcase item) result)))))
		result)))
	(dolist (module-desc module-list)
	  (cond ((eq (car module-desc) :serial)
		 (setq expanded-module-list
		       (append (translate-module (cdr module-desc))
			       expanded-module-list)))
		((eq (car module-desc) :module)
		 (let ((type (second (assoc :type (cdddr module-desc)))))
		   (unless (eq type :lisp-example)
		     (push (cdr module-desc) submodules)
		     (unless (member type '(nil :system))
		       (warn "Unknown module type ~S" module-desc)))
		   (push module-desc expanded-module-list)))
		(t (error "funny module-desc ~S" module-desc))))
	(let ((new-module-list '()))
	  (dolist (item expanded-module-list)
	    (if (and (consp item)
		     (eq (car item) :module))
		(when-let (submodule (assoc (second item) submodules))
		  (setq new-module-list
			(nconc (reverse (translate-module (third item)))
			       new-module-list))
		  (setq submodules
			(delete submodule submodules)))
	      (push item new-module-list)))
	  (when submodules
	    (error "Unprocecessed submodules ~S" submodules))
	  `(lw:defsystem ,name ,(nreverse new-options)
			 :members (,@required-systems ,@new-module-list)
			 :rules ((:in-order-to :compile :all
					       (:requires (:load :serial))))))))))

#+(or LispWorks3.2 LispWorks4.0 LispWorks4.1 LispWorks4.2 LispWorks4.3)
(sys::without-warning-on-redefinition
  (let ((dir (pathname-location
              (translate-logical-pathname (pathname *private-patches-directory*)))))
    #+LispWorks3.2
    (loop for (name seqname minor) in '(("symbol-macrolet-nil" sys::compiler 37)
                                        ("unspecific-pathname-device" sys::system 86)
                                        ("select-output" sys::system 87)
                                        ("select-output-extra" sys::system 173)
                                        ("listen-buffered-pipe" sys::system 175)
                                        ("lw-external-lambda-lists" sys::system 89)
                                        ("buffered-tcp-output-streams" :comms 9)
                                        ("signal-continue-search-in-same-cluster" :conditions 2))
          for seq = (and seqname (scm::find-patch-sequence seqname))
          unless (and seq minor (>= (scm::ps-minor seq) minor))
          do (load (merge-pathnames name dir)))
    #+(or LispWorks4.0 LispWorks4.1)
    (loop for (file id) in
          #+(and LispWorks4.0 Harlequin-PC-Lisp)
          '(("defpackage-disjoint-allow-duplicates"
             :system-defpackage-disjoint-allow-duplicates)
            ("result-type-values"
             :compiler-result-type-values)
            ("setf-name-uninterned"
             :system-setf-name-uninterned)
            ("dynamic-extent-multiple-value-list"
             :compiler-dynamic-extent-multiple-value-list)
            ("with-stream-buffers"
             :buffered-stream-with-stream-buffers)
            ("special-operator-p"
             :system-special-operator-p)
            ("socket-check-bytes-before-eof"
             :comm-socket-check-bytes-before-eof)
            ("host-forces-unc-pathname"
             :system-host-forces-unc-pathname)
            #+clim-2
            ("atomic-incf-decf-delta"
             :clim-standalone-atomic-incf-decf-delta))
          #-(and LispWorks4.0 Harlequin-PC-Lisp)
          '(#+clim-2
            ("atomic-incf-decf-delta"
             :clim-standalone-atomic-incf-decf-delta))
          unless (scm::patch-id-loaded-p id)
          do (load (merge-pathnames file dir)))
    #+LispWorks4.2
    (let ((missing (loop for (id module) in '((:compiler-treeify-flet-labels nil)
                                              (:system-function-typespec-result-and-arg-types nil)
                                              (:system-shadowing-import nil)
                                              (:compiler-beta-substitute-var-by-var nil)
                                              (:clim-standalone-atomic-incf-decf-delta "clim-standalone"))
                         when (and (or (null module) (member module *modules* :test 'equal))
                                   (not (scm::patch-id-loaded-p id)))
                         collect id)))
    
      (when missing
        (error "Need to install patches~{~%~S~}." missing)))
    #+LispWorks4.3
    (let ((missing (loop for (id module) in '(#+Harlequin-PC-Lisp (:system-parse-integer nil))
                         when (and (or (null module) (member module *modules* :test 'equal))
                                   (not (scm::patch-id-loaded-p id)))
                         collect id)))
      (when missing
        (error "Need to install patches~{~%~S~}." missing)))))
