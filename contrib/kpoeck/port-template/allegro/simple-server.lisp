(in-package :http)

(defvar *main-socket*)
(defvar *main-stream*)
(defvar *terminate-server* nil)
(defvar *main-process*)
(defvar *all-sockets* nil)

;;; Tiny design problem with lispworks
;;; We have a server process, sockets (but not as an object, probably just a file descriptor and streams
;;; For now, threat the process as the socket in other lisps
(defun note-socket (socket)
  (pushnew socket *all-sockets*))

(defun kill-socket (socket)
  (close-the-socket socket)
  (setq *all-sockets* (remove socket *all-sockets*)))

(defclass console-log (synchronous-log-notification-mixin access-log)
  ()
  )

(defun start (&key 
              hostname
              (port 8000)
              (type :not-yet)
              )
  "Short version of ENABLE-HTTP-SERVICE."
  (clear-access-logs) 
  (let ((http:*log-access-log-class* 'http::console-log))
    (log-notifications-on (current-access-logs) t)
    (ensure-current-log port))
  (www-utils::note-hostname hostname)
  (reset-server-local-host-variables :standard-port port)
  (set-dispatch-macro-character #\# #\u  #'sharp-sign-u-reader-helper)
  (run-server-initializations t)
  (run-server-launch-initializations t)
  (log-notifications-on (current-access-logs) t)
  (setq html::*standard-color-scheme*
        (html:create-color-scheme :background :white
                                  :foreground :black
                                  :link :blue
                                  :visited-link :purple
                                  :active-link :red))
  ;;;don't really know, but seems to be better
  ;;; fixme,perhaps needed with chunking
  (setq http::*persistent-connection-timeout* 10.)
  (unless (eq type :not-yet)
    (start-serving hostname port :type type)
    )
  )

(defun start-serving (hostname port &key type)
  #-lispworks
  (ecase type
    (:single (start-single-threaded-server hostname port))
    (:stupid-multi (start-simple-multi-thread-server hostname port))
    )
  #+lispworks (book-sockets hostname port :type type)
  (notify-log-window "HTTP service enabled for: http://~A:~D/"
                       (www-utils:local-host-domain-name) port)
  )

(defun book-sockets (host port &key type)
  #-lispworks (declare (ignore type))
  (when (%http-servive-on-port-enabled port)
    ;;that should better not crash
    (kill-socket (%http-servive-on-port-enabled port)))
  (setq *main-socket* 
        #-lispworks (www-utils::create-socket host port)
        #+lispworks (www-utils::%make-server-socket-process host port (if (eq type :single) 'handle-socket-response-single 'handle-socket-response-multi)))
  #+lispworks (setq *main-process* *main-socket*)
  (note-socket *main-socket*)
  (%note-server-on-port *main-socket* port)
  *main-socket*
  )

#-lispworks
(defun start-single-threaded-server  (host port)
  (let ((socket (book-sockets host port)))
    (%single-listen-for-connections socket))
  )

#-lispworks
(defun start-simple-multi-thread-server (host port)
  (let ((socket (book-sockets host port)))
    (setq *main-process*
          (www-utils:process-run-function
           "Stupid cl-http-accept-thread"
           #'stupid-accept-thread
           socket)))
  )

#+lispworks
(defun handle-socket-response-single (socket-handle stream)
  (declare (ignore socket-handle))
  (setq *main-stream* stream)
  (let* ((client-address (www-utils::foreign-host stream))
         (client-domain-name (www-utils:ip-address-for-parsed-ip-address client-address)))
    (notify-log-window "~&About to launch new HTTP request...~%" nil)
    (%provide-single-service stream client-domain-name client-address)
    )
  )

#+lispworks
(defun handle-socket-response-multi (socket-handle stream)
  (declare (ignore socket-handle))
  (let* ((client-address (www-utils::foreign-host stream))
         (client-domain-name (www-utils:ip-address-for-parsed-ip-address client-address)))
    (notify-log-window "~&About to launch new HTTP request...~%" nil)
    (handler-case 
                (www-utils:process-run-function "Stupid cl-http process thread" #'%provide-single-service stream client-domain-name client-address)
              (error (e)
                (notify-log-window "Error in Stupid Accept ~s~%" e))))
  )

#-lispworks
(defun %single-listen-for-connections (server-socket)
  (setq *terminate-server* nil)
  (unwind-protect
      (loop
        (let ((stream (www-utils::wait-for-connection server-socket)))
          (www-utils::note-timeout stream 60)
          (setq *main-stream* stream)
          (let* ((client-address (www-utils::foreign-host stream))
                 (client-domain-name (www-utils:ip-address-for-parsed-ip-address client-address)))
            (notify-log-window "~&About to launch new HTTP request...~%" nil)
            (%provide-single-service stream client-domain-name client-address)
            )
          (when *terminate-server*
            (return))
          ))
    (close-the-socket server-socket)))

(defvar *last-error*)

;;; Need to add error-handling for cmu
;;; Quicktype movie --> sigpipe
;;; #<SIMPLE-ERROR {4A7A116D}> is a structure.
;;; FUNCTION-NAME: UNIX::SIGPIPE-HANDLER
;;; ACTUAL-INITARGS: (:FORMAT-CONTROL "SIGPIPE at #x~x." :FORMAT-ARGUMENTS (2455938582))
;;; ASSIGNED-SLOTS: NIL
;;; From portable-aserve:
#+cmu
(defun sigpipe-p (c)
  (and (typep c 'simple-error)
       (stringp (simple-condition-format-control c))
       (search "SIGPIPE at "
	       (simple-condition-format-control c)
	       :test #'char=)))

#|
in the original port they do:
(defun disable-sigpipe ()
  (sys:ignore-interrupt unix:sigpipe))
|#

(defun %provide-single-service (stream client-address client-host)
  (flet ((log-dropped-connection (server)
                                 (handler-case
                                     (progn
                                       (abort-http-stream stream)
                                       (set-server-status server 408)       ;client timeout status code changed from 504 -- JCMa 5/29/1995.
                                       (log-access server))
                                   #+allegro
                                   (excl:socket-error (error)
                                                      (notify-log-window "Catch Socket error in log-dropped-connection ~s ~s ~s ~s ~%"
                                                                         (stream-error-stream error)
                                                                         (excl:stream-error-action error)
                                                                         (excl:stream-error-code error)
                                                                         (excl:stream-error-identifier error)))
                                   #+clozure-common-lisp
                                   (ccl:socket-error (error)
                                                     (notify-log-window "Catch Socket error in log-dropped-connection ~s ~s ~s ~s ~%"
                                                                        "Socket Error:"
                                                                        (ccl:socket-error-situation error)
                                                                        (ccl:socket-error-code error)
                                                                        (ccl:socket-error-identifier error)))
                                   #+(or sbcl ecl)
                                   (sb-bsd-sockets:socket-error (error)
                                                                (notify-log-window "Catch Socket error in log-dropped-connection ~s ~s ~s ~s ~%"
                                                                                   "Socket Error:"
                                                                                   (sb-bsd-sockets::socket-error-errno error)
                                                                                   (sb-bsd-sockets::socket-error-symbol error)
                                                                                   (sb-bsd-sockets::socket-error-syscall error))
                                                                )
                                   #+lispworks
                                   (comm:socket-error (error)
                                     (notify-log-window "Catch Socket error in log-dropped-connection ~s ~s ~s ~s ~%"
                                                                                   "Socket Error:"
                                                                                   "Dont"
                                                                                   "know "
                                                                                   "yet")
                                     )
                                   #+clisp
                                   (SYSTEM::SIMPLE-OS-ERROR (error)
                                     (notify-log-window "Catch simple os error in log-dropped-connection ~s ~s ~s ~s ~%"
                                                        "Socket Error:"
                                                        "Dont"
                                                        "know "
                                                        "yet"))
				   #+cmu
				   (ext:socket-error (error)
                                     (notify-log-window "Catch Socket error in log-dropped-connection ~s ~s ~s ~s ~%"
                                                                                   "Socket Error:"
                                                                                   "Dont"
                                                                                   "know "
                                                                                   "yet")
                                     )
				   #+cmu
				   (simple-error (error)
				     (if (sigpipe-p error)
					    (notify-log-window "Catch sigpipe error in log-dropped-connection ~s ~s ~s ~s ~%"
							       "Sigpipe Error:"
							       (simple-condition-format-control error)
							       (simple-condition-format-arguments error)
							       "No clue")
					   (notify-log-window "Caught unknown error in log-dropped-connection ~s~%" error))
				     )
                                   (error (error)
                                     (notify-log-window "Caught unknown error in log-dropped-connection ~s~%" error))))
         (handle-unhandled-error (server error)
                                 (handler-case
                                     (progn
                                       (set-server-status server 500)
                                       (log-access server)
                                       (report-status-unhandled-error error stream (server-request server))
                                       (close stream))                  ; force output
                                   #+allegro
                                   (excl:socket-error ())
                                   #+clozure-common-lisp
                                   (ccl:socket-error ())
                                   #+(or sbcl ecl)
                                   (sb-bsd-sockets:socket-error ())
                                   #+lispworks
                                   (comm:socket-error ())
                                   #+clisp
                                   (SYSTEM::SIMPLE-OS-ERROR (error))
				   #+cmu
				   (ext:socket-error ())
                                   (error (error)
                                     (notify-log-window "Caught unknown error in handle-unhandled-error ~s~%" error)))
                                 ;; log the access after the output has been forced.
                                 (log-access server)))
    ;; make sure we know our process. 
    ;; client-host is NIL because we only parse it on demand.
    (handler-case 
        (let ((server (make-server nil  stream client-host client-address)))
          (initialize-resourced-server nil server stream client-host client-address)
          
          ;; keep other processes from tampering with our TCP state.
          (let ((*server* server))
            (handler-case
                (provide-service server)
              ;; catch aborts anywhere within server.-- JCMa 12/30/1994.
              ;; log aborts as dropped connections by client. -- JCMa 5/24/1999.
              (http-abort (e) 
                          (notify-log-window "Http-abort ~s~%"e)
                          (log-dropped-connection server))
              #+allegro
              (excl:socket-error (e)
                                 (notify-log-window "excl:socket-error ~s~%" e)
                                 (log-dropped-connection server))
               #+allegro
              (excl:stream-closed-error (e)
                                        (notify-log-window "excl:stream-closed-error ~s~%" e)
                                        (log-dropped-connection server))
              #+clozure-common-lisp
              (ccl:socket-error (error)
                                (notify-log-window "Catch Socket error after process service ~s ~s ~s ~s ~%"
                                                   "Socket Error:"
                                                   (ccl:socket-error-situation error)
                                                   (ccl:socket-error-code error)
                                                   (ccl:socket-error-identifier error))
                                (log-dropped-connection server))
              #+(or sbcl ecl)
              (sb-bsd-sockets:socket-error (error)
                                           (notify-log-window "Catch Socket error after process service ~s ~s ~s ~s ~%"
                                                              "Socket Error:"
                                                              (sb-bsd-sockets::socket-error-errno error)
                                                              (sb-bsd-sockets::socket-error-symbol error)
                                                              (sb-bsd-sockets::socket-error-syscall error))
					   (log-dropped-connection server))
              #+lispworks
              (comm:socket-error (error)
                (notify-log-window "Catch Socket error after process server ~s ~s ~s ~s ~%"
                                   "Socket Error:"
                                   "Dont"
                                   "know "
                                   "yet")
		(log-dropped-connection server)
                )
              #+clisp
              (SYSTEM::SIMPLE-OS-ERROR (error)
                (notify-log-window "Catch simple os error after process server ~s ~s ~s ~s ~%"
                                   "Socket Error:"
                                   "Dont"
                                   "know "
                                   "yet")
		(log-dropped-connection server)
                )
	      #+cmu
	      (ext:socket-error (error)
		(notify-log-window "Catch Socket error after process service ~s ~s ~s ~s ~%"
				   "Socket Error:"
				   "dont"
				   "Know"
				   "Yet")
		(log-dropped-connection server))
	      #+cmu
	      (simple-error (error)
		(cond ((sigpipe-p error)
		       (notify-log-window "Catch sigpipe error after process service ~s ~s ~s ~s ~%"
					  "Sigpipe Error:"
					  (simple-condition-format-control error)
					  (simple-condition-format-arguments error)
					  "No further clue")
		       (log-dropped-connection server))
		      (t 
		       (setq *last-error* error *terminate-server* t)
		       (notify-log-window "error ~s~%" error)
		       (handle-unhandled-error server error))))
              (error (error)
                (setq *last-error* error *terminate-server* t)
                #+no (inspect *last-error*)
                (notify-log-window "error ~s~%" error)
                (handle-unhandled-error server error)))))
      (error (error)
        (notify-log-window "Caught error in %provide-single-service ~a~%" error)))))

#+no
(defun %provide-single-service (stream client-address client-host)
  (let ((server (make-server nil  stream client-host client-address)))
          (initialize-resourced-server nil server stream client-host client-address)
          
          ;; keep other processes from tampering with our TCP state.
          (let ((*server* server))
            (provide-service server))))

(defun abort-http-stream (http-stream)
  "Closes http-stream in abort mode.  
This will push any output in the transmit buffer and catch any network errors.
Takes care to clean up any dangling pointers."
  (handler-case 
    (close http-stream :abort t)
    (file-error ())
    #+allegro
    (excl:socket-error ())
    #+clozure-common-lisp
    (ccl:socket-error ())))

#-lispworks
(defun stupid-accept-thread (server-socket)
  (unwind-protect
      (loop
        (let ((stream (www-utils::wait-for-connection server-socket)))
          (www-utils::note-timeout stream 60)
          (let* ((client-address (www-utils::foreign-host stream))
                 (client-domain-name (www-utils:ip-address-for-parsed-ip-address client-address)))
            (notify-log-window "About to launch new HTTP worker process...~%" nil)
            (handler-case 
                (www-utils:process-run-function "Stupid cl-http process thread" #'%provide-single-service stream client-domain-name client-address)
              (error (e)
                (notify-log-window "Error in Stupid Accept ~s~%" e)))
            )
          )
        )
    (close-the-socket server-socket)))
  
(defun stop ()
  (when *main-process*
    (www-utils::process-kill *main-process*)
    (setq *main-process* nil))
  (close-the-socket *main-socket*)
  (mapc #'%forget-server-on-port (copy-list (%http-get-all-active-ports)))
  (dolist (process (all-processes))
    (when (or (search "Stupid cl-http process" (process-name process))
              (search "Stupid cl-http-accept-thread" (process-name process)))
      (ignore-errors (www-utils:process-kill process))))
  )

(defun start-examples (&key (load-examples t)(type :stupid-multi)(host "localhost")(port 8000))
  (start :hostname host :port port :type :not-yet)
  (when load-examples
    (compile-examples)
    (load-examples))
  (start-serving host port :type type)
  )

(defun compile-examples ()
  (let ((files '(
                 "http:examples;find-url"
                 "http:examples;documentation"
                 "http:examples;access-control"
                 "http:w3p;documentation"
                 "http:examples;log-window"
                 "http:examples;vrml;vrml"
                 "http:examples;twistdown-tree;twistdown"
                 "http:examples;lispdoc"
                 "http:examples;examples")))
     (with-compilation-unit ()
      (dolist (file files)
        (cl-user::potentially-compile-file file)
        ;;; I hate toplevel-exports
        (when (string= "http:examples;documentation" file)
          (export (find-symbol "SUBMIT-AND-RESET-BUTTONS" (find-package :http)) (find-package :http)))))))

(defun load-examples ()
  ;;; this does not get load in the portable sources
  ;;; do it here until this is fixed in the common sources
  ;;; might be a clever idea to compile the examples 
     (#+sbcl handler-bind
	     #+sbcl ((sb-ext:defconstant-uneql #'(lambda (condition)
					       (let ((restart (find-restart 'continue condition)))
						 (when restart (invoke-restart restart))))))
	     ;;; otherwise ecl does not complete .lisp to the pathname
             ;;; but seems to work in toplevel ???
	     #+ecl let #+ecl ((*default-pathname-defaults* (make-pathname :type "lisp" :defaults *default-pathname-defaults*)))
	     #-(or sbcl ecl) progn
	     
	     (load "http:examples;find-url")
	     (load "http:examples;exports")))

#|
For ecl
(setq *default-pathname-defaults* (make-pathname :type "lisp"))

(setq http::*debug-server* t)
(setq http:*resolve-ip-addresses* nil)
(http::start-examples :load-examples t :host "192.168.1.4")
(http::start :hostname "localhost" :port 8000 :type :not-yet)
(http::start-examples :load-examples t :host "localhost" :port 8000 :type :single)
(http::start-examples :load-examples nil :host "localhost" :port 8000 :type :single)
(when t
      (load "http:examples;find-url.lisp")
  (load "http:examples;exports.lisp"))

(load "http:examples;exports.lisp" :print t)

(http::start-serving "localhost" 8000 :type :stupid-multi)
(http::start-serving "localhost" 8000 :type :single)

(start-examples nil)
(start-examples t :single)
(stop)
(setq *terminate-server* t)

(start :hostname "localhost.localdomain" :port 8000 :type :not-yet)
(load "http:examples;exports.lisp")
(start-serving "localhost.localdomain" 8000 :type :stupid-multi)

(start-serving "localhost.localdomain" 8000 :type :single)


(close *main-socket*)
(class-of *main-socket*)
(mp:process-kill *main-process*)
http::local-context

(set-standard-http-port 8000)
(stop)

(ed "http:examples;exports.lisp")
(ed "http:acl;obc;examples;exports.lisp")
|#

