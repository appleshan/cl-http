;;;   -*- Mode: LISP; Package: HTTP-UI; BASE: 10; Syntax: ANSI-Common-Lisp; -*-

;;;
;;; (c) Copyright  2000, Rainer Joswig, joswig@corporate-world.lisp.de
;;;     All Rights Reserved.
;;;


(eval-when (compile load eval)
  (assert (member :clim-2 *features*) ()
	  "CLIM 2 is needed to run or compile this code.
Load CLIM, then retry assertion."))


;;; This code is the a starting point for a full-blown administration
;;; interface for CL-HTTP. Part of it is to understand, test and demonstrate some
;;; CLIM capabilities like: multiple layouts (not yet), drag&drop (not yet),
;;; presentation translators, hierarchical menus, ...  well, a lot
;;; of the advanced capabilities of CLIM will be needed.
;;; In general we will use the presentation-based mechanisms
;;; of CLIM to provide a very dynamic user interface.

;;; To invoke this CL-HTTP CLIM-based user interface (after loading the software) do:
;;;
;;;  Genera:  Type the two keys "Select Square" or do Select Activity HTTP-UI.
;;;  MCL:     Call (http-ui::http-ui) from a listener.

;; Questions

;;; ------------------------------------------------------------------- 
;;;
;;; Main Application Frame
;;;

#+MCL
(cl:load "http:mcl;server;cl-http-menu")        ; for now

(in-package "HTTP-UI")

;;; Define a few command tables for the menus in the HTTP-UI frame.
;;; Each command table defines a menu. Commands in these command tables
;;; are the menu items. The command table of the application frame
;;; needs to inherit from these command tables.
;;; Where is this explained in the CLIM docs?

(clim:define-command-table window-command-table)
(clim:define-command-table server-command-table)
(clim:define-command-table client-command-table)
(clim:define-command-table proxy-command-table)
(clim:define-command-table url-command-table)
(clim:define-command-table log-command-table)
(clim:define-command-table info-command-table)
(clim:define-command-table misc-command-table)


;;; ------------------------------------------------------------------- 
;;; The application frame HTTP-UI

(clim:define-application-frame http-ui ()
    ((log-pane-lock :initform (clim-sys:make-lock "log pane lock")
		    :reader frame-log-pane-lock))
  (:panes
    (display-pane :application
		  :display-after-commands nil
		  :scroll-bars :both 
		  :end-of-line-action :allow
		  :end-of-page-action :scroll
		  :borders (:thickness 2)
		  :incremental-redisplay t)
    (command-pane :command-menu :incremental-redisplay t)
    (title-pane :title :display-string "CL-HTTP Monitor")
    (log-pane :application
	      :display-after-commands nil
	      :scroll-bars :both
	      :end-of-line-action :allow
	      :end-of-page-action :scroll
	      :incremental-redisplay t)
    (interactor :interactor))
  (:command-table (http-ui
		    :inherit-from (clim:user-command-table
				    window-command-table
				    server-command-table
				    client-command-table
				    proxy-command-table
				    url-command-table
				    log-command-table
				    info-command-table
				    misc-command-table)
		    :menu (("Window" :menu window-command-table)
			   ("Server" :menu server-command-table)
			   ("Client" :menu client-command-table)
			   ("Proxy" :menu proxy-command-table)
			   ("URL" :menu url-command-table)
			   ("Log" :menu log-command-table)
			   ("Info" :menu info-command-table)
			   ("Misc" :menu misc-command-table))))

  (:layouts
    (standard
      (clim:vertically ()
	#-MCL title-pane
	#-MCL command-pane
	(2/3 display-pane)
	(1/6 log-pane)
	(:fill interactor))))
  #+genera
  (:menu-bar nil))

; add the command-table to the command defining macro,
; so we get multiple menus with multiple menu-items.
; The predefined DEFINE-HTTP-UI-COMMAND doesn`t allow us to name
; the command table we want to use.
(defmacro define-a-http-ui-command ((name command-table &rest options) arguments &body body)
  #+genera (declare (zwei:indentation 0 3 1 3 2 1))
  `(clim:define-command (,name :command-table ,command-table ,@options)
                        ,arguments
     ,@body))

(defun http-ui ()
  "Top level function for starting the HTTP-UI user interface"
  (or (clim:find-application-frame 'http-ui :create nil)
      (let ((width  #+Genera clim:+fill+ #-Genera 750)
	    (height #+Genera clim:+fill+ #-Genera 650))
	(clim:run-frame-top-level
	  (clim:make-application-frame 'http-ui :width width :height height)))))

; Make this CLIM application known to Genera - so that "Select Square"
; will select the HTTP-UI user interface. "Select Control-Square"
; generates a new application frame.
#+genera
(clim:define-genera-application http-ui
  :select-key #\square)

; Add this application to the programs column of the System Menu
#+genera
(tv:add-to-system-menu-programs-column "CL-HTTP"
  '(clim:find-application-frame 'HTTP-UI::HTTP-UI)
  "CL-HTTP Administration Interface")

; (process-run-function "HTTP-UI" #'http-ui)

#|
#+CLIM-Env
(clim:define-command (com-start-cl-http-interface :command-table clim-env::application-menu
						  :name t :menu "CL-HTTP Interface") ()
  (let ((width  #+Genera clim:+fill+ #-Genera 750)
	(height #+Genera clim:+fill+ #-Genera 650))
    (clim-env::make-clim-environment-application clim:*application-frame* 'http-ui :width width :height height)))
|#

;;; ------------------------------------------------------------------- 
;;; Window Commands

; commands that manipulate the http-ui application frame window.

(define-a-http-ui-command (com-quit window-command-table :menu t :name t) ()
  "Quit the http-ui application frame"
  (clim:frame-exit clim:*application-frame*))

(define-a-http-ui-command (com-clear-window window-command-table :menu t) ()
  "Clears the various frames of the http-ui application frame"
  (clim:with-application-frame (frame)
    (clim:window-clear (clim:frame-standard-output frame))
    (clim:window-clear (log-standard-output frame))
    (clim:window-clear (clim:frame-standard-input frame))))


;;; ------------------------------------------------------------------- 
;;; HTTP Service

; commands for controlling the HTTP service provided by CL-HTTP

(define-a-http-ui-command (com-enable-http-service server-command-table :menu t) ()
  (http:enable-http-service))

(define-a-http-ui-command (com-disable-http-service server-command-table :menu t) ()
  (http:disable-http-service))

#+MCL
(define-a-http-ui-command (com-reset-network server-command-table :menu t) ()
  (ccl::opentransport-cleanup))


;;; ------------------------------------------------------------------- 
;;; Start Webserver Command

#+mcl
(define-a-http-ui-command (com-start-webserver server-command-table :menu t)
                         ((how '(clim:member :dns :without-dns :ip-over-appletalk)
                               :default :dns))
  (load (ecase how
          (nil "http:mcl;start-server-ip-number.lisp")
          (:dns "http:mcl;start-server.lisp")
          (:without-dns "http:mcl;start-server-ip-number.lisp")
          (:ip-over-appletalk "http:mcl;start-server-appletalk.lisp"))))


;;; ------------------------------------------------------------------- 
;;; Set Webserver Preferences Command

(defun sorted-preference-types (&optional (table http::*preference-type-table*))
  (let ((preference-types  nil))
    (maphash #'(lambda (key preference-type)
		 (declare (ignore key))
		 (pushnew preference-type preference-types))
	     table)
    (sort preference-types #'string-lessp :key #'http::preference-type-name)))

(defvar *unused-preferences*
  (list
   #+mcl :host-name-for-apple-talk-operation))

(defun accept-cl-http-preferences (stream)
  (let ((preference-to-value-table (make-hash-table)))
    (clim:accepting-values (stream :own-window t
				   :label "Webserver Preferences"
				   :align-prompts nil
				   :scroll-bars t)
      (dolist (preference-type (sorted-preference-types http::*preference-type-table*))
	(clim:with-text-face (stream :bold)
	  (princ (slot-value preference-type 'http::name) stream))
	(terpri stream)
	(loop for preference in (slot-value preference-type 'http::inferiors)
	      when (and (slot-boundp preference 'http::presentation-type)
			(slot-boundp preference 'http::value-setter)
			(slot-boundp preference 'http::value-getter)
			(slot-boundp preference 'http::prompt)
                        (not (member (http::preference-keyword preference)
                                     *unused-preferences*)))
		do (multiple-value-bind (value value-exists-p)
		       (gethash preference preference-to-value-table) 
		     (setf (gethash preference preference-to-value-table)
			   (clim:accept (convert-w3p-to-clim-presentation-type
					  (slot-value preference 'http::presentation-type))
					:prompt (string-right-trim
						  '(#\:)
						  (slot-value preference 'http::prompt))
					:stream stream
					:default (if value-exists-p
						     value
						     (funcall (http::preference-value-getter preference)))
					:query-identifier (slot-value preference 'http::keyword))))
	      and do (terpri stream))
	(terpri stream)))
    (maphash #'(lambda (preference value)
		 (funcall (http::preference-value-setter preference) value))
	     preference-to-value-table)))

(define-a-http-ui-command (com-set-webserver-preferences server-command-table :menu t)
    ()
  (clim:with-application-frame (frame)
    (let ((stream (clim:frame-standard-output frame)))
      (accept-cl-http-preferences stream))))

(define-a-http-ui-command (com-save-webserver-preferences server-command-table :menu t)
    ((pathname 'clim:pathname :default http:*standard-configuration-pathname*))
  (http::save-standard-server-configuration :pathname pathname))

(define-a-http-ui-command (com-load-webserver-preferences server-command-table :menu t)
    ((pathname 'clim:pathname :default http:*standard-configuration-pathname*))
  (http::standard-configure-server :pathname pathname))

;;; ------------------------------------------------------------------- 
;;; Proxy Service

(define-a-http-ui-command (com-setup-proxy proxy-command-table :menu t) ()
  (clim:with-application-frame (frame)
    (let* ((pane (clim:frame-standard-output frame))
	   (proxy-enabled-p (not (null (http::proxy-service-enabled-p))))
	   (proxy-is-enabled-p proxy-enabled-p)
	   (debug-proxy http::*debug-proxy*)
	   (proxy-caching-p http::*proxy-caching-p*))
      (clim:accepting-values (pane :own-window t
				   :label "Proxy Preferences"
				   :align-prompts nil
				   :scroll-bars t)
        (setf proxy-enabled-p
              (clim:accept 'clim:boolean
                           :stream pane
                           :query-identifier 1
                           :prompt "Enable Proxy"
                           :default proxy-enabled-p))
        (terpri pane)
        (setf debug-proxy
              (clim:accept 'clim:boolean
                           :stream pane
                           :query-identifier 2
                           :prompt "Debug Proxy"
                           :default debug-proxy))
        (terpri pane)
        (setf proxy-caching-p
              (clim:accept 'clim:boolean
                           :stream pane
                           :query-identifier 3
                           :prompt "Enable Proxy Caching"
                           :default proxy-caching-p)))
      (if proxy-enabled-p
	  (when (not proxy-is-enabled-p)
	    (http:enable-proxy-service))
	  (http:disable-proxy-service))
      (setf http::*debug-proxy* debug-proxy
            http::*proxy-caching-p* proxy-caching-p))))


;;; -------------------------------------------------------------------
;;; URL commands

(defun list-all-urls ()
  "Returns a sorted list of all known URLs."
  (let ((urls nil))
    (flet ((add (name object)
             (declare (ignore name))
             (push object urls)))
      (url:map-url-table #'add))
    (setf urls (sort urls #'string< :key #'url:coerce-url-string))
    urls)) 

(defun list-all-urls-by-class ()
  (let ((urls-by-class (make-hash-table))
	(classes nil))
    (url:map-url-table #'(lambda (string object)
			   (declare (ignore string))
			   (let ((class (class-of object)))
			     (push object (gethash class urls-by-class))
			     (pushnew class classes))))
    (setf classes (sort classes #'string-lessp :key #'class-name))
    (loop for class in classes
	  collect (list class (sort (gethash class urls-by-class)
				    #'string-lessp
				    :key #'url:name-string)))))

; (list-all-urls-by-class)

(define-a-http-ui-command (com-show-all-urls url-command-table :menu t) ()
  "Print all URLs by class."
  (clim:with-application-frame (frame)
    (let ((stream (clim:frame-standard-output frame)))
      (let ((*standard-output* stream))
	(loop for (class . (urls)) in (list-all-urls-by-class)
	      do (print (class-name class) stream)
	      do (loop for url in urls
		       do (terpri stream)
			  (princ " " stream)
			  (clim:present url
					(clim:presentation-type-of url)
					:stream stream)))))))


(define-a-http-ui-command (com-inspect-url url-command-table :menu t)
    ((url 'url:url :gesture nil))
  (inspect url))

(define-a-http-ui-command (com-edit-html-file-for-url url-command-table :menu t)
    ((url 'url:http-object
	  :gesture (nil :tester ((object)
				 (and (slot-boundp object 'pathname)
				      (pathnamep (slot-value object 'pathname))
				      (probe-file (slot-value object 'pathname)))))))
  (let ((file (slot-value url 'pathname)))
    (when (probe-file file)
      (ed file))))

(defmethod url-exported-p ((url url:url))
  (and (slot-exists-p url 'url:translation-method)
       (slot-boundp url 'url:translation-method)
       (slot-value url 'url:translation-method)))

(www-utils:define-generic url-exported-p (url)
   (:documentation "Returns non-null if the URL is exported."))

(defmethod url-exported-p ((url url:translation-method-mixin))
  (and (slot-boundp url 'url:translation-method)
       (slot-value url 'url:translation-method)))

(defmethod url-exported-p (url)
  (declare (ignore url))
  nil)

(define-a-http-ui-command (com-unexport-url url-command-table :menu t)
    ((url 'url:translation-method-mixin
	  :gesture (nil :tester ((object)
				 (url-exported-p object)))))
  (http:unexport-url url))

(define-a-http-ui-command (com-unintern-url url-command-table :menu t)
    ((url 'url:url
	  :gesture (nil :tester ((object)
				 (url:intern-url object :if-does-not-exist :soft)))))
  (url:unintern-url url))

(define-a-http-ui-command (com-find-url url-command-table :menu t) ((name-string 'clim:string))
  (let ((urls nil))
    (url:map-url-table #'(lambda (string object)
			   (when (search name-string string :test #'char=)
			     (push object urls))))
    (when urls
      (clim:with-application-frame (frame)
	(let ((stream (clim:frame-standard-output frame)))
	  (let ((*standard-output* stream))
	    (format stream "~%~%URLs containing ~s:" name-string)
	    (loop for url in urls
		  do (terpri stream)
		  do (clim:present url
				   (clim:presentation-type-of url)
				   :stream stream))))))))

;;; ------------------------------------------------------------------- 
;;; Info commands

(defun display-file (file)
  (when (ignore-errors (probe-file file))
    #+MCL
    (ccl::%buffer-set-read-only (ccl:fred-buffer (ed file)) t)
    #-MCL (ed file)))

(define-a-http-ui-command (com-show-license info-command-table :menu t) ()
  (display-file "http:http;-license-.text"))


;;; ------------------------------------------------------------------- 
;;; Show Release Notes Command

(define-a-http-ui-command (com-show-release-notes info-command-table :menu t) ()
  (display-file "http:http;-release-notes-.text"))


;;; ------------------------------------------------------------------- 
;;; Show Readme Command


#+(or MCL LispWorks)
(define-a-http-ui-command (com-show-readme info-command-table :menu t) ()
  (display-file #+MCL"http:mcl-read-me.text"
                #+LispWorks "http:lw;-read-me-.text"))


;;; ------------------------------------------------------------------- 
;;; About Command

(define-a-http-ui-command (com-about info-command-table :menu t) ()
  (show-about-info))

(defparameter *local-server-administration-url*
  #+ignore
  #u"/cl-http/maintenance/configure-server.html"
  nil)

(defparameter *cl-http-mailing-list-url*
  (url:url "mailto:www-cl@ai.mit.edu"))

(defparameter *cl-http-home-page-url*
  (url:url http:*cl-http-home-page-url-string*))

(defparameter *cl-http-bug-report-mailing-list-url*
  (url:url "mailto:bug-cl-http@ai.mit.edu"))

(defparameter *cl-http-port-bug-report-mailing-list-url*
  (url:url #+MCL"mailto:bug-mcl-http@ai.mit.edu"
           #+LispWorks "mailto:bug-lw-cl-http@ai.mit.edu"))

(defparameter *cl-http-ftp-archive-url*
  (url:url "ftp://ftp.ai.mit.edu/pub/users/jcma/cl-http/"))

#+Genera
(defparameter *symbolics-homepage-url*
  (url:url "http://www.symbolics.com/"))

(defun show-about-info ()
  (clim:with-application-frame (frame)
    (let ((stream (clim:frame-standard-output frame))
          (f1 (clim:make-text-style :fix :bold :small))
          (f2 (clim:make-text-style :fix :roman :small)))
      (clim:surrounding-output-with-border (stream)
        (flet ((define-it (string1 object)
                 (clim:with-text-style (stream f1)
                   (write-string string1 stream)
                   (write-string ":" stream))
		 (etypecase object
		   (string (clim:with-text-style (stream f2)
			     (write-string "  " stream)
			     (write-string object stream))
			   (terpri stream))
		   (url:url (clim:present object
					  (clim:presentation-type-of object)
					  :stream stream)
			    (terpri stream))
                   (null))))
          (terpri stream)
          (define-it "CL-HTTP is written by" "John C. Mallery")
          (define-it "This CLIM-based interface for CL-HTTP is written by" "Rainer Joswig")
          (terpri stream)
          (define-it "CL-HTTP version"
            (destructuring-bind (major minor &rest l)
                                (www-utils:%server-version-info)
	      (declare (ignore l))
              (format nil "~D.~D" major minor)))
	  #-genera
          (define-it "Macintosh port version"
            (destructuring-bind (nil nil port-major port-minor port-patch-level)
                                (www-utils:%server-version-info)
              (format nil "~D.~D.~D" port-major port-minor port-patch-level)))
          (define-it "Lisp version" (format nil "~a ~a"
                                            (lisp-implementation-type)
                                            (lisp-implementation-version)))
          (define-it "Running on a" (format nil "~a" (machine-version)))
          (terpri stream)
          (when *local-server-administration-url*
            (define-it "Local Server Administration" *local-server-administration-url*)
            (terpri stream))
          (define-it "CL-HTTP home page" *cl-http-home-page-url*)
          (define-it "CL-HTTP mailing list" *cl-http-mailing-list-url*)
          (define-it "CL-HTTP bug reports" *cl-http-bug-report-mailing-list-url*)
          (define-it "CL-HTTP port-specific bug reports"
                     *cl-http-port-bug-report-mailing-list-url*)
          (define-it "CL-HTTP FTP archive" *cl-http-ftp-archive-url*)
          (terpri stream)
	  #+Genera(define-it "Symbolics home page"  *symbolics-homepage-url*)
          (define-it "LispWorks home page" "http://www.lispworks.com/")
	  #+MCL(define-it "Digitool home page" "http://www.digitool.com/")
          #+MCL(define-it "Digitool Mail Address" "mailto:info@digitool.com")
          #+MCL(define-it "Digitool MCL mailing list" "mailto:info-mcl@digitool.com")
          #+MCL(define-it "Digitool MCL bug reports" "mailto:bug-mcl@digitool.com")
          #+MCL(define-it "Digitool MCL news group" "news:comp.lang.lisp.mcl")
          #+MCL(define-it "Digitool FTP archive" "ftp://ftp.digitool.com/")))
      (terpri stream))))


;;; -------------------------------------------------------------------
;;; Status

(define-a-http-ui-command (com-status info-command-table :menu t) ()
  (show-status))

(defun display-virtual-host-table (table stream)
  (princ "Virtual Hosts" stream)
  (terpri stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream)
	(princ "Host" stream))
      (clim:formatting-cell (stream)
	(princ "Ports" stream)))
    (maphash #'(lambda (host server-descriptions)
		 (clim:formatting-row (stream)
		   (clim:formatting-cell (stream)
		     (princ host stream))
		   (clim:formatting-cell (stream)
		     (clim:format-textual-list server-descriptions
					       #'(lambda (item stream)
						   (destructuring-bind (port . url-string) item
						     (format stream "Port ~a as " port)
						     (clim:present (url:url url-string)
								   'url:url
								   :stream stream)))
					       :stream stream))))
	     table)))

(defun show-status ()
  (clim:with-application-frame (frame)
    (let ((stream (clim:frame-standard-output frame)))
      (terpri stream)
      (let ((ports (http:http-service-enabled-p)))
	(if ports
	    (progn
	      (princ "CL-HTTP is running on following ports: " stream)
	      (princ ports))
	    (princ "CL-HTTP is not running" stream)))
      (terpri stream)
      (format stream "The IP number of the local host ~a is ~a."
	      (www-utils:local-host-domain-name)
	      (www-utils:local-host-ip-address))
      (terpri stream)
      (princ "The proxy service is enabled: " stream)
      (clim:present (http::proxy-service-enabled-p) 'clim:boolean :stream stream)
      (terpri stream)
      (when (plusp (hash-table-count http::*virtual-host-table*))
	(display-virtual-host-table http::*virtual-host-table* stream)
	(terpri stream)))))


;;; -------------------------------------------------------------------
;;; Inspect CL-HTTP data

(defparameter *cl-http-inspect-description*
  nil
  "A list (name form description) of all items to inspect with a menu.
Items can be added with ADD-ITEM-TO-INSPECT.")

(defun add-item-to-inspect (name form documentation)
  "Adds a new inspect description to *cl-http-inspect-description*.
If a description with the same name exists, it will be replaced."
  (check-type name string)
  (assert (or (null documentation)
              (stringp documentation))
          (documentation))
  (let ((item (list name form documentation))
        (previous-item (assoc name *cl-http-inspect-description*
                              :test #'string-equal)))
    (if previous-item
      (setf (rest previous-item)
            (rest item))
      (setf *cl-http-inspect-description* (merge 'list
                                                 (list item)
                                                 *cl-http-inspect-description*
                                                 #'string<
                                                 :key #'first)))))

(eval-when (:load-toplevel :execute)
  (add-item-to-inspect "List URLs" '(list-all-urls) "List all URLs in URL table.")
  (add-item-to-inspect "URL Table" 'url::*url-table* "The URL Table itself.")
  (add-item-to-inspect "List Logs" 'http::*all-logs* "List all known logs.")
  (add-item-to-inspect "Server Initialization List"
                       'http::*server-initialization-list*
                       "The cold initialization list for the CL-HTTP server.")
  (add-item-to-inspect "User Agent Capabilities"
                       'http:*user-agent-capabilities*
                       "Maps capabilities to user agents.
Entries are (capability &rest (user-agent &rest versions)).
No versions means that all versions handle the capability.")
  (add-item-to-inspect "Virtual Host Table"
                       'http::*virtual-host-table*
                       (documentation 'http::*virtual-host-table* 'variable))
  (add-item-to-inspect "Builtin Client Colors"
                       'netscape1.1:*built-in-client-colors*
                       (documentation 'netscape1.1:*built-in-client-colors* 'variable))
  (add-item-to-inspect "Embedded Script Languages"
                       'netscape2.0::*embedded-script-languages*
                       (documentation 'netscape2.0::*embedded-script-languages* 'variable))
  (add-item-to-inspect "HTML Event Handlers"
                       'html2:*event-handlers*
                       (documentation 'html2:*event-handlers* 'variable))
  #+mcl
  (add-item-to-inspect "Service Name Number Alist"
                       'ccl::*service-name-number-alist*
                       (documentation 'ccl::*service-name-number-alist* 'variable))
  #+(or mcl lispworks)
  (add-item-to-inspect "Store and Forward Mail Hosts"
                       'smtp:*store-and-forward-mail-hosts*
                       (documentation 'smtp:*store-and-forward-mail-hosts* 'variable))
  #+(or mcl lispworks)
  (add-item-to-inspect "All Resources"
                       'resources::*all-resources*
                       (documentation 'resources::*all-resources* 'variable))
  (add-item-to-inspect "Builtin Backgrounds"
                       'netscape1.1:*built-in-backgrounds*
                       (documentation 'netscape1.1:*built-in-backgrounds* 'variable))
  (add-item-to-inspect "ISO Readtable"
                       'html2::*iso-readtable*
                       (documentation 'html2::*iso-readtable* 'variable))
  (add-item-to-inspect "Server Control Alist"
                       'http::*server-control-alist*
                       "A property list of streams awaiting incoming http connections on each port.")
  (add-item-to-inspect "Server Launch Initialization List"
                       'http::*server-launch-initialization-list*
                       "The initialization list for launching the CL-HTTP server."))


(define-a-http-ui-command (com-inspect-cl-http info-command-table :menu t) ()
  (let ((form (clim:menu-choose (mapcar #'(lambda (description)
				(destructuring-bind (name form desc) description
				  (declare (ignore desc))
				  (list name :value form)))
			    *cl-http-inspect-description*))))
    (when form
      (inspect (ignore-errors (eval form))))))


;;; -------------------------------------------------------------------
;;; Browse Server command

#+mcl
(define-a-http-ui-command (com-browse-server info-command-table :menu t) ()
  (labels ((make-local-url (string)
             (when (http:local-context)
               (http:merge-url string (http:local-context))))
           (make-menu-choose-list ()
             (loop for (name string)
                   in '(("Local Server" "/")
                        ("Projects" "/cl-http/projects.html"))
                   for url = (make-local-url string)
                   when url
                   collect (list name :value url))))
    (let ((url (clim:menu-choose (make-menu-choose-list))))
      (when url
        (ccl:open-url url :activate-p t)))))


;;; -------------------------------------------------------------------
;;; Put File command

;;; Copies a file to a URL.

(define-a-http-ui-command (com-put-file client-command-table :menu t)
    ((pathname 'clim:pathname)
     (url 'clim:string))
  (http:copy-file pathname (url:url url)))


;;; -------------------------------------------------------------------
;;; Inspect Frame command

(define-a-http-ui-command (com-inspect-frame misc-command-table :menu t) ()
  (clim:with-application-frame (frame)
    (inspect frame)))


;;; -------------------------------------------------------------------
;;; Update Menubar command

#+mcl
(define-a-http-ui-command (com-update-menubar misc-command-table :menu t) ()
  (clim:with-application-frame (frame)
    (mcl-clim::setup-mcl-menubar frame)))

#|
(mcl-clim::setup-mcl-menubar
 (clim:find-application-frame 'http-ui :activate nil))
|#


;;; -------------------------------------------------------------------
;;; Window Logging

; Uses a log object of class HTTP::WINDOW-NOTIFICATION-LOG,
; %WRITE-CLIM-WINDOW-NOTIFICATION writes the log information
;  to the log pane.

(defmethod log-standard-output ((frame http-ui))
  "Returns the stream of the log pane."
  (clim:get-frame-pane frame 'log-pane))

(define-a-http-ui-command (com-start-window-logging log-command-table :menu t) ()
  (clim:with-application-frame (frame)
    (start-window-logging frame)))

(defun start-window-logging (frame)
  (let ((log (http::ensure-window-notification-log)))
    (let ((stream (log-standard-output frame)))
      (http::log-install-stream log stream)
      (http:log-notifications-on log t))))

(define-a-http-ui-command (com-stop-window-logging log-command-table :menu t) ()
  (clim:with-application-frame (frame)
    (stop-window-logging frame)))

(define-a-http-ui-command (com-change-lock-type log-command-table :menu t) ()
  (clim:with-application-frame (frame)
    (setf (slot-value frame 'log-pane-lock)
	  (clim-sys:make-lock "Log Pane Lock"))))

(defun stop-window-logging (frame)
  (let ((log (http::get-window-notification-log)))
    (when log
      (http::log-deinstall-stream log (log-standard-output frame))
      (http:log-notifications-on log nil))))

; To Do: debug this one
(defmethod clim:frame-exit :before ((frame http-ui) #-Genera &key #-Genera destroy)
  "Make sure that before closing a window, there are no more
notifications from the log object."
#-Genera (declare (ignore destroy))
  (stop-window-logging frame))


;;; -------------------------------------------------------------------
;;; Realms

; (http:realm-table)
; (http:add-realm name scheme)
; (url:map-realms function)
; (http:intern-user realm user &key :password :groups :personal-name :email-address)
; (url:map-users realm function)
; (url:map-groups realm function)
; (http:sorted-realms)
; (http:sorted-users)
; (http:sorted-groups)
; (http:sorted-access-controls)
; (http::authenticate-user realm authorization)

(clim:define-presentation-type http::user ())
(clim:define-presentation-type http:realm ())

(define-a-http-ui-command (com-list-realms info-command-table :menu t)
    ()
  (clim:with-application-frame (frame)
    (let ((*standard-output* (clim:frame-standard-output frame)))
      (terpri)
      (loop for realm in (http:sorted-realms)
	    do (clim:present realm))
      (terpri))))

(define-a-http-ui-command (com-list-users info-command-table :menu t)
                          ((realm 'http:realm))
  (when realm
    (clim:with-application-frame (frame)
      (let ((*standard-output* (clim:frame-standard-output frame)))
        (terpri)
        (loop for user in (http:sorted-users realm)
	      do (clim:present user))
        (terpri)))))

(define-a-http-ui-command (com-add-user info-command-table :menu t)
    ((realm 'http:realm)
     (user-name 'string)
     (password 'string)
     (personal-name 'string)
     (email-address 'string))
  (clim:with-application-frame (frame)
    (let ((*standard-output* (clim:frame-standard-output frame)))
      (terpri)
      (assert (and realm user-name password personal-name email-address) ())
      (clim:present (http:intern-user realm user-name
				      :password password
				      :personal-name personal-name
				      :email-address email-address
				      :if-does-not-exist :create))
      (terpri))))


;;; -------------------------------------------------------------------
;;; Start a HTTP-UI frame

#|
(eval-when (:load-toplevel)
  (clim:find-application-frame 'http-ui))
|#


;;; -------------------------------------------------------------------
;;; End Of File http-ui.lisp

