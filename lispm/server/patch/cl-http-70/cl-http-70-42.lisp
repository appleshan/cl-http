;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.42
;;; Reason: Function (CLOS:METHOD CLOS:MAKE-LOAD-FORM (URL:ENTITY-TAG)):  define for dumping parsed header sets.
;;; Function (CLOS:METHOD CLOS:MAKE-LOAD-FORM (URL:MAIL-ADDRESS)):  -
;;; Function URL::URL-INTERNED-P:  new
;;; Function (CLOS:METHOD URL::URL-INTERNED-P (URL:URL)):  -
;;; Function (CLOS:METHOD CLOS:MAKE-LOAD-FORM (URL:URL)):  -
;;; Function HTTP::PARSE-LOCATION-HEADER:  never intern location urls.
;;; DEFINE-HEADER :CONTENT-LOCATION:  update.
;;; DEFINE-HEADER :LOCATION:  update.
;;; Variable HTTP::*HTTP-PACKAGE*:  -
;;; Function HTTP::MAP-DIRECTORY-PATHNAMES:  clean up.
;;; Function TQ:TASK-QUEUE-NEXT:  -
;;; Function (CLOS:METHOD TQ::TASK-QUEUE-PUSH-FRONT (TQ:TASK-QUEUE T)):  -
;;; Function (CLOS:METHOD TQ:TASK-QUEUE-EXECUTE-PENDING-TASKS (TQ:TASK-QUEUE)):  Careful not to lose entries when error occur executing tasks.
;;; Function HTTP::FAST-MAKE-INSTANCE:  new.
;;; Written by JCMa, 5/07/00 15:57:32
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.41,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42, HTTP Proxy Server 4.9,
;;; HTTP Client Substrate 3.10, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.11, DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x994 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number -2141189585,
;;; Domain Fixes (from CML:MAILER;DOMAIN-FIXES.LISP.33),
;;; Don't force in the mail-x host (from CML:MAILER;MAILBOX-FORMAT.LISP.24),
;;; Make Mailer More Robust (from CML:MAILER;MAILER-FIXES.LISP.15),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add CLIM presentation and text style format directives. (from FV:SCLC;FORMAT.LISP.20),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; Make update schema work on set-value attributes with accessor names (from CML:LISPM;STATICE-SET-VALUED-UPDATE.LISP.1),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.107),
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48).

;;; Patch file for CL-HTTP version 70.42
;;; Written by JCMa, 5/08/00 21:07:42
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.41,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.11, DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x994 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number -2141189585,
;;; Domain Fixes (from CML:MAILER;DOMAIN-FIXES.LISP.33),
;;; Don't force in the mail-x host (from CML:MAILER;MAILBOX-FORMAT.LISP.24),
;;; Make Mailer More Robust (from CML:MAILER;MAILER-FIXES.LISP.15),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add CLIM presentation and text style format directives. (from FV:SCLC;FORMAT.LISP.20),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; Make update schema work on set-value attributes with accessor names (from CML:LISPM;STATICE-SET-VALUED-UPDATE.LISP.1),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.107),
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48).




(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;HEADERS.LISP.458"
  "HTTP:SERVER;URL.LISP.391"
  "HTTP:SERVER;URL.LISP.392"
  "HTTP:SERVER;HEADERS.LISP.459"
  "HTTP:SERVER;VARIABLES.LISP.185"
  "HTTP:SERVER;UTILS.LISP.445"
  "HTTP:SERVER;TASK-QUEUE.LISP.26"
  "HTTP:SERVER;TASK-QUEUE.LISP.27"
  "HTTP:SERVER;TASK-QUEUE.LISP.30"
  "HTTP:SERVER;UTILS.LISP.446")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.458")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod make-load-form ((entity-tag entity-tag))
  `(make-instance ',(type-of entity-tag) :value ,(entity-tag-value entity-tag)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.391")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

;; Needed for header dumping
(defmethod make-load-form ((mail-address mail-address))
  (with-slots (user-id host-string personal-name original-string) mail-address
    `(create-internet-mail-address ,user-id ,host-string ,personal-name ,original-string)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.392")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define-generic url-interned-p (url)
  (:documentation "Returns non-null if url is interned."))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.392")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(export (intern "URL-INTERNED-P" :url) :url)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.392")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod make-load-form ((url url))
  (let ((url-string (name-string url)))
    `(intern-url ,url-string :if-does-not-exist ,(if (eq url (get-url url-string)) :create :uninterned))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.392")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod url-interned-p ((url url))
  (eq url (get-url (name-string url))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.459")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;; If called when (or *client* *server*), relative URLs are resolved against
;; the current url contained by these objects. 11/18/99 -- JCMa.
(defun parse-location-header (string &optional (start 0) (end (length string)))
  (with-string-trim-bounds (*white-space-chars* string start end)
    (with-bad-escaping-resignalled (string :start start :end end :reason "Bad Escaping: Ill-Formed Location Header")
      (multiple-value-bind (nstring unescaped-p new-string-p)
	  (string-unescape-special-chars string start end)	;handle broken Microsoft location headers   4/21/97 -- JCMa.
	unescaped-p				;ignore
        (flet ((relative-object ()
		 (or (and (boundp '*client*) (symbol-value '*client*))	;defined by the client not the server
		     *server*)))
	  (declare (inline relative-object))
	  (let ((relative-object (relative-object)))
	    (handler-case-if relative-object
	       (if new-string-p
		   (url:intern-url nstring :if-does-not-exist :uninterned)
		   (url:intern-url string :start start :end end :if-does-not-exist :uninterned))
	      (url::no-scheme-found ()
				    (url:intern-url (if new-string-p
							(url::%merge-relative-url nstring (name-string relative-object))
							(url::%merge-relative-url string (name-string relative-object) start end))
						    :if-does-not-exist :uninterned)))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.459")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :content-location
               (:header :entity)
  :print-string "Content-Location"
  :parse-function 'parse-location-header
  :print-function 'print-location-header)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.459")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :location (:header :response)
  :print-string "Location"
  :parse-function 'parse-location-header
  :print-function 'print-location-header)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;VARIABLES.LISP.185")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp;-*-")

(define-variable *http-package* (find-package :http))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.445")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(define map-directory-pathnames (pathname function &key (file-type :text))
  "Maps FUNCTION over the newest version pathnames of the directory, PATHNAME
according to FILE-TYPE."
  (dolist (pathname (directory (make-pathname :name :wild :type (etypecase file-type
								  (symbol (string file-type))
								  (string file-type))
					      :version :newest :defaults pathname)))
    (funcall function pathname))) 


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.26")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defgeneric task-queue-next (task-queue)
   (declare (values first-entry))
   (:documentation "Gets the first task without removing it from the queue  with inter-process locking."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.26")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defgeneric task-queue-push-front (task-queue task)
  (:documentation "Push an entry onto TASK-QUEUE with inter-process locking."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.26")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod task-queue-push-front ((task-queue task-queue) task)
  (with-slots (lock queue pointer n-pending-tasks) task-queue 
    (with-lock-held (lock :write "Task Queue Push")
      (cond (queue
	     (push task queue))
	    (t (let ((entry (list task)))
		 (setf queue entry
		       pointer entry))))
      (incf (the fixnum n-pending-tasks)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.27")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(export (intern "TASK-QUEUE-PUSH-FRONT" :tq) :tq)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.30")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod task-queue-execute-pending-tasks ((task-queue task-queue))
  (loop for task = (pop-task-queue task-queue)
        while task
	;; Careful not to lose entries when error occur executing tasks
        do (unwind-protect
	       (progn 
		 (task-queue-execute-task task-queue task)
		 (setq task nil))
	     (when task
	       (task-queue-push-front task-queue task)))
        while (task-queue-run-p task-queue)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.446")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

;;;------------------------------------------------------------------- 
;;;
;;; MACROS
;;;

(define-macro fast-make-instance (specialized-class (default-class) &body init-args)
  "Makes an instance of the class, SPECIALIZED-CLASS, using the initialziations INIT-ARGS.
When SPECIALIZED-CLASS is DEFAULT-CLASS, this macro exposes the constant DEFAULT-CLASS 
to the Lisp compiler to allow compilation into faster code. SPECIALIZED-CLASS is
evaluated at runtime whereas DEFAULT-CLASS is a compile-time class."
  `(let ((class ,specialized-class))
     (case class
       (,default-class
	(make-instance ',default-class ,@init-args))
       (t (make-instance class ,@init-args)))))

