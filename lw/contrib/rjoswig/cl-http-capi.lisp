;;; -*- mode:lisp; package: cl-http-capi; -*-
;;; Copyright Rainer Joswig, 2003, joswig@lisp.de

;;; use
;;;     (cl-http-capi:control-panel)
;;;     (cl-http-capi:control-panel :create-p t)
 
;;; Changes

;;; RJ  2003-12-03    inspect allows function call values
;;; RJ  2003-12-03    browse local site button
;;; RJ  2003-12-03    added mail bug reports switch, not used


#-:capi
(cl:error "LispWorks CAPI needed!")

(cl:defpackage "CL-HTTP-CAPI"
  (:add-use-defaults t)
  (:use "CAPI")
  (:export "CONTROL-PANEL")) ;top-level interface

(cl:in-package "CL-HTTP-CAPI")

(defun start-cl-http-callback (data interface)
  (declare (ignore data interface))
  (when (yes-or-no-p "Start CL-HTTP?")
    (http:enable-http-service)))

(defun stop-cl-http-callback (data interface)
  (declare (ignore data interface))
  (when (yes-or-no-p "Stop CL-HTTP?")
    (http:disable-http-service)))

(defun generate-title ()
  (destructuring-bind (major minor &rest port-version-numbers)
      (www-utils:%server-version-info)
    (format nil "CL-HTTP ~D.~D [LW ~{~D~^.~}] (~A ~A)"
            major minor  port-version-numbers (lisp-implementation-type) (lisp-implementation-version) )))

(defun inspect-cl-http-object (d i)  
  (declare (ignore d i))
  (let ((objects '(HTTP::*ALL-LOGS*
                   RESOURCES::*ALL-RESOURCES*
                   NETSCAPE1.1:*BUILT-IN-BACKGROUNDS*
                   NETSCAPE1.1:*BUILT-IN-CLIENT-COLORS*
                   HTTP::*CGI-VARIABLE-BINDINGS*
                   HTTP::*CLIM-SYS-HTTP-PROCESSES*
                   HTTP::*CONNECTION-TABLE*
                   WWW-UTILS::*DAILY-SERVER-TASKS*
                   WWW-UTILS::*DAILY-SERVER-TIMER*
                   WWW-UTILS::*DOMAIN-NAME-LOOKUP*
                   HTTP:*ESCAPED-CHARACTERS*
                   *FEATURES*
                   HTTP::*FORM-QUERY-KEYWORD-TOKENIZER*
                   HTTP::*HEADER-TOKENIZER*
                   HTTP::*HEADER-VALUE-TOKENIZER*
                   HTTP::*LOG-CLASSES*
                   HTTP::*MIME-CONTENT-TYPE-ALIST*
                   HTTP::*MULTIPORT-ACCESS-LOGS*
                   HTTP::*PATHNAME-EXTENSION-EXPORT-TYPE-ALIST*
                   HTTP::*PREFERENCE-TYPE-TABLE*
                   HTTP::*PROXY-TABLE*
                   HTTP:*SERVER-INITIALIZATION-LIST*
                   HTTP::*SERVER-INTERFACE-ALIST*
                   HTTP:*SERVER-LAUNCH-INITIALIZATION-LIST*
                   HTTP::*STANDARD-ACCESS-LOGS*
                   HTML3.2:*STANDARD-COLOR-SCHEME*
                   HTTP::*STATUS-CODE-ALIST*
                   URL::*URI-UNIVERSE*
                   HTTP:*USER-AGENT-CAPABILITIES*
                   HTTP::*VIRTUAL-HOST-TABLE*
                   (http:all-servers))))
    (let ((object (prompt-with-list (sort (copy-list objects)
                                          #'string-lessp
                                          :key #'princ-to-string)
                                    "Select an object to inspect:")))
      (when object
        (cond ((symbolp object)
               (inspect (symbol-value object)))
              ((consp object)
               (inspect (eval object))))))))

; (scm:system-pathname-list (scm:find-system 'cl-HTTP))

(defun edit-cl-http-file (d i)  
  (declare (ignore d i))
  (let ((object (prompt-with-list (sort (scm:system-pathname-list (scm:find-system 'CL-USER::CL-HTTP))
                                        #'string-lessp
                                        :key #'file-namestring)
                                  "Files:")))
    (when object
      (ed object))))

(defun edit-configuration-file (d i)  
  (declare (ignore d i))
  (let ((file (probe-file "http:examples;configuration.lisp")))
    (when file
      (ed file))))

(defparameter *cl-http-system-names*
  (list 'cl-user::w4
        'cl-user::http-proxy
        'cl-user::cl-http-examples
        'cl-user::w4-examples
        'cl-user::cl-http
        'cl-user::clim-sys
        'cl-user::http-base-client
        'cl-user::http-client-substrate))

(defun create-system-action (action title)
  (lambda (d i)
    (declare (ignore d i))
    (let ((object (prompt-with-list (sort *cl-http-system-names* #'string-lessp)
                                    (format nil "~a:" title))))
      (when object
        (www-utils:process-run-function title action object)))))
  
(defun edit-log-file (d i)
  (declare (ignore d i))
  (let ((file (prompt-for-file "Log File"
                               :pathname (translate-logical-pathname "http:logs;*.text")
                               :filters (list "Logs" "*.text"))))
    (when file
      (ed file))))

(define-interface cl-http ()
    ()
  (:panes
   (start push-button
          :text "Start"
          :selection-callback 'start-cl-http-callback)
   (stop push-button
         :text "Stop"
         :selection-callback 'stop-cl-http-callback)
   (debug-server check-button
          :text "Debug Server"
          :selected http:*debug-server*
          :selection-callback (lambda (d i)
                                (declare (ignore d i))
                                (setf http:*debug-server* t))
          :retract-callback (lambda (d i)
                              (declare (ignore d i))
                              (setf http:*debug-server* nil)))
   (debug-client check-button
          :text "Debug Client"
          :selected http:*debug-client*
          :selection-callback (lambda (d i)
                                (declare (ignore d i))
                                (setf http:*debug-client* t))
          :retract-callback (lambda (d i)
                              (declare (ignore d i))
                              (setf http:*debug-client* nil)))
   (debug-mailer check-button
          :text "Debug Mailer"
          :selected SMTP:*DEBUG-MAILER*
          :selection-callback (lambda (d i)
                                (declare (ignore d i))
                                (setf smtp:*debug-mailer* t))
          :retract-callback (lambda (d i)
                              (declare (ignore d i))
                              (setf smtp:*debug-mailer* nil)))
#||
   (mail-bug-reports check-button
          :text "Mail Bug Reports"
          :selected http::*report-bug-via-mail*
          :selection-callback (lambda (d i)
                                (declare (ignore d i))
                                (setf http::*report-bug-via-mail* t))
          :retract-callback (lambda (d i)
                              (declare (ignore d i))
                              (setf http::*report-bug-via-mail* nil)))
||#
   (compile-system push-button
                   :text "Compile System"
                   :selection-callback (create-system-action 'scm:compile-system "Compile System"))
   (load-system push-button
                :text "Load System"
                :selection-callback (create-system-action 'scm:load-system "Load System"))
   (compile-and-load-system push-button
                            :text "Compile and Load System"
                            :selection-callback (create-system-action (lambda (system)
                                                                        (scm:compile-system system :load t))
                                                                      "load System"))
   (resolve-ip-addresses check-button
          :text "Resolve IP Addresses"
          :selected HTTP:*RESOLVE-IP-ADDRESSES*
          :selection-callback (lambda (d i)
                                (declare (ignore d i))
                                (setf HTTP:*RESOLVE-IP-ADDRESSES* t))
          :retract-callback (lambda (d i)
                              (declare (ignore d i))
                              (setf HTTP:*RESOLVE-IP-ADDRESSES* nil)))
   (log-resolve-ip-addresses check-button
          :text "Resolve IP Addresses for Logging"
          :selected HTTP:*LOG-RESOLVE-IP-ADDRESSES*
          :selection-callback (lambda (d i)
                                (declare (ignore d i))
                                (setf HTTP:*LOG-RESOLVE-IP-ADDRESSES* t))
          :retract-callback (lambda (d i)
                              (declare (ignore d i))
                              (setf HTTP:*LOG-RESOLVE-IP-ADDRESSES* nil)))
   (log-window push-button
               :text "Expose Log Window"
               :selection-callback (lambda (d i)
                                     (declare (ignore d i))
                                     (WWW-UTILS:EXPOSE-LOG-WINDOW)))
   (edit-log-file push-button
         :text "Edit Log File"
         :selection-callback 'edit-log-file)
   (edit-configuration-file push-button
         :text "Edit Configuration"
         :selection-callback 'edit-configuration-file)
   (edit-cl-http-file push-button
                      :text "Edit CL-HTTP File"
                      :selection-callback 'edit-cl-http-file )
   (inspect push-button
         :text "Inspect"
         :selection-callback 'inspect-cl-http-object)
   (browse-local-site push-button
                      :text "Browse Local Site"
                      :selection-callback (lambda (d i)
                                            (declare (ignore d i))
                                            (hqn-web:browse
                                             (concatenate 'string
                                                          "http://"
                                                          (www-utils:local-host-domain-name)
                                                          ":"
                                                          (princ-to-string http:*standard-http-port*)
                                                          "/")))))
  (:layouts
   (main-layout column-layout '(row-of-buttons log-row options resolver system browser-row))
   (row-of-buttons row-layout '(start stop inspect))
   (options row-layout '(debug-server debug-client debug-mailer ; mail-bug-reports
                                      ))
   (resolver row-layout '(resolve-ip-addresses log-resolve-ip-addresses))
   (system row-layout '(edit-cl-http-file load-system compile-system compile-and-load-system))
   (log-row row-layout '(log-window edit-log-file edit-configuration-file))
   (browser-row row-layout '(browse-local-site)))
  (:menus
   (control-menu "Control"
                 (("Start" :selection-callback 'start-cl-http-callback)
                  ("Stop"  :selection-callback 'stop-cl-http-callback))))
  (:menu-bar control-menu)
  (:default-initargs :title (generate-title)))

(defvar *control-panel* nil)

(defun control-panel (&key create-p)
  "Exposes the CL-HTTP Control panel."
  (display (or (and (not create-p) *control-panel*)
               (setq *control-panel* (make-instance 'CL-HTTP)))))

;;; End of File
