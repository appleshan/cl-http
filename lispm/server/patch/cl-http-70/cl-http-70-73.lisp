;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.73
;;; Reason: Introduce multiport log objects and define an asynchronous statistics and
;;; notification log using a superior entry format that is proxy-aware.
;;; Collect some proxy stats in the log counter mixin.
;;; 
;;; Remove statistics and notification from normal port-specific file log classes.
;;; Users can resintate these if they want.
;;; 
;;; Variable HTTP::*MULTIPORT-LOG-PORT*:  -
;;; Variable HTTP::*MULTIPORT-ACCESS-LOGS*:  -
;;; Function HTTP::MULTIPORT-ACCESS-LOGS:  -
;;; Function HTTP::CLEAR-ACCESS-LOGS:  update.
;;; Function (CLOS:METHOD HTTP:FIND-ACCESS-LOG-IF (T (EQL :MULTIPORT))):  update.
;;; Function (CLOS:METHOD HTTP:ADD-ACCESS-LOG (HTTP::ACCESS-LOG (EQL :MULTIPORT))):  update.
;;; Function (CLOS:METHOD HTTP:REMOVE-ACCESS-LOG (HTTP::ACCESS-LOG (EQL :MULTIPORT))):  update.
;;; Function (CLOS:METHOD HTTP:REMOVE-ACCESS-LOG (HTTP::ACCESS-LOG (EQL :ALL))):  update.
;;; Remove function HTTP::SERVER-ACCESS-LOGS: undefine.
;;; Function HTTP::SERVER-ACCESS-LOGS:  -
;;; Function (CLOS:METHOD HTTP::SERVER-ACCESS-LOGS (HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Function HTTP:LOG-ACCESS:  -
;;; Function (CLOS:METHOD HTTP:LOG-ACCESS (HTTP::SERVER-LOGGING-MIXIN)):  add multiport logging.
;;; Function HTTP:INTERN-ACCESS-LOG:  -
;;; CLOS class HTTP::LOG-COUNTERS-MIXIN:  -
;;; Function HTTP::%NOTE-ACCESS-STATUS-CODE:  add proxy-p and proxy-cache-hit-p.
;;; Remove function HTTP::UPDATE-LOG-STATISTICS: undefine.
;;; Function HTTP::UPDATE-LOG-STATISTICS:  add proxy-p
;;; Function (CLOS:METHOD HTTP::UPDATE-LOG-STATISTICS (HTTP::LOG-COUNTERS-MIXIN T T T T T T T T)):  -
;;; Function (CLOS:METHOD HTTP::LOG-SERVER-ACCESS (HTTP::LOG-COUNTERS-MIXIN HTTP::SERVER-LOGGING-MIXIN) AND):  pass proxy-p.
;;; Remove function HTTP::NOTE-ACCESS-STATUS-CODE: undefine.
;;; Function HTTP::NOTE-ACCESS-STATUS-CODE:  add proxy-p
;;; Function (CLOS:METHOD HTTP::NOTE-ACCESS-STATUS-CODE (HTTP::LOG-COUNTERS-MIXIN T T)):  -
;;; Function HTTP::SERVER-PROXY-REQUEST-P:  -
;;; Function HTTP::SERVER-PROXY-CACHE-HIT-P:  -
;;; Function (CLOS:METHOD HTTP::SERVER-PROXY-CACHE-HIT-P (HTTP::PROXY-SERVER-MIXIN)):  -
;;; Function HTTP::SERVER-NOTE-PROXY-CACHE-HIT:  -
;;; Function (CLOS:METHOD HTTP::SERVER-NOTE-PROXY-CACHE-HIT (HTTP::PROXY-SERVER-MIXIN)):  -
;;; CLOS class HTTP::ACCESS-STATISTICS-LOG:  -
;;; Function HTTP::ENSURE-MULTIPORT-STATISTICS-LOG:  -
;;; CLOS class HTTP::SYNCHRONOUS-LOG-COUNTERS-MIXIN:  -
;;; CLOS class HTTP::ASYNCHRONOUS-LOG-COUNTERS-MIXIN:  -
;;; Remove function (CLOS:METHOD HTTP::LOG-SERVER-ACCESS (HTTP::LOG-COUNTERS-MIXIN HTTP::SERVER-LOGGING-MIXIN) AND): undefine.
;;; Function (CLOS:METHOD HTTP::LOG-SERVER-ACCESS (HTTP::SYNCHRONOUS-LOG-COUNTERS-MIXIN HTTP::SERVER-LOGGING-MIXIN) AND):  -
;;; Function (CLOS:METHOD HTTP::LOG-SERVER-ACCESS (HTTP::ASYNCHRONOUS-LOG-COUNTERS-MIXIN HTTP::SERVER-LOGGING-MIXIN) AND):  -
;;; Function (CLOS:METHOD HTTP:LOG-ENTRY-WRITER (HTTP::ASYNCHRONOUS-LOG-COUNTERS-MIXIN HTTP::SERVER-LOGGING-MIXIN)):  -
;;; CLOS class HTTP::SYNCHRONOUS-LOG-NOTIFICATION-MIXIN:  -
;;; CLOS class HTTP::ASYNCHRONOUS-LOG-NOTIFICATION-MIXIN:  -
;;; Remove function (CLOS:METHOD HTTP::LOG-SERVER-ACCESS (HTTP::LOG-NOTIFICATION-MIXIN HTTP::SERVER-LOGGING-MIXIN) AND): undefine.
;;; Function (CLOS:METHOD HTTP::LOG-SERVER-ACCESS (HTTP::SYNCHRONOUS-LOG-NOTIFICATION-MIXIN HTTP::SERVER-LOGGING-MIXIN) AND):  -
;;; Function HTTP::%WRITE-CONSOLE-WINDOW-NOTIFICATION:  introduce into the server.
;;; Function HTTP::%SERVER-WRITE-CONSOLE-WINDOW-NOTIFICATION:  -
;;; Function HTTP::%SERVER-NOTIFY-CONSOLE-WINDOW:  -
;;; Function HTTP::WRITE-CONSOLE-WINDOW-NOTIFICATION:  -
;;; Function (CLOS:METHOD HTTP::WRITE-CONSOLE-WINDOW-NOTIFICATION (HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Remove function HTTP::LOG-NOTIFY-CONSOLE: -
;;; Function HTTP::LOG-NOTIFY-CONSOLE:  -
;;; Function (CLOS:METHOD HTTP::LOG-NOTIFY-CONSOLE (HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Function (CLOS:METHOD HTTP::LOG-SERVER-ACCESS (HTTP::SYNCHRONOUS-LOG-NOTIFICATION-MIXIN HTTP::SERVER-LOGGING-MIXIN) AND):  -
;;; Function HTTP::CAREFUL-NOTIFY-LOG-WINDOW:  -
;;; Function HTTP::%SERVER-NOTIFY-CONSOLE-WINDOW:  -
;;; Function (CLOS:METHOD HTTP:LOG-ENTRY-WRITER (HTTP::ASYNCHRONOUS-LOG-COUNTERS-MIXIN HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Function (CLOS:METHOD HTTP:LOG-ENTRY-WRITER (HTTP::ASYNCHRONOUS-LOG-COUNTERS-MIXIN HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Function (CLOS:METHOD HTTP:LOG-ENTRY-WRITER (HTTP::ASYNCHRONOUS-LOG-NOTIFICATION-MIXIN HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Function (CLOS:METHOD HTTP::LOG-SERVER-ACCESS (HTTP::ASYNCHRONOUS-LOG-NOTIFICATION-MIXIN HTTP::SERVER-LOGGING-MIXIN) AND):  -
;;; Function HTTP::%WRITE-CONSOLE-WINDOW-NOTIFICATION:  -
;;; Written by JCMa, 9/16/00 17:13:42
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.72,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42, HTTP Proxy Server 5.25,
;;; HTTP Client Substrate 3.22, HTTP Client 49.10, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Documentation Patches 1.0,
;;; Genera 8 5 Clim Patches 1.0, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.11,
;;; DEC OSF/1 V4.0 (Rev. 110),
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
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing),
;;; Domain ad host patch (from W:>Reti>domain-ad-host-patch.lisp.1).

;;; Patch file for CL-HTTP version 70.73
;;; Written by JCMa, 9/16/00 19:53:44
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.72,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Documentation Patches 1.0,
;;; Genera 8 5 Clim Patches 1.0, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, HTTP Client Substrate 3.22, Jcma 42,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.11,
;;; DEC OSF/1 V4.0 (Rev. 110),
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
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.1),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.5).


(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;SERVER.LISP.858"
  "HTTP:SERVER;LOG.LISP.187"
  "HTTP:SERVER;LOG.LISP.188"
  "HTTP:SERVER;LOG.LISP.189"
  "HTTP:SERVER;SERVER.LISP.859"
  "HTTP:SERVER;LOG.LISP.190"
  "HTTP:SERVER;CLASS.LISP.39"
  "HTTP:SERVER;CLASS.LISP.40"
  "HTTP:SERVER;CLASS.LISP.41"
  "HTTP:SERVER;SERVER.LISP.860"
  "HTTP:SERVER;UTILS.LISP.463"
  "HTTP:SERVER;SERVER.LISP.861"
  "HTTP:SERVER;CLASS.LISP.42"
  "HTTP:SERVER;LOG.LISP.193")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.187")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defvar *multiport-access-logs* nil
  "An list of all the multiport access logs.")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.187")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(declaim (inline multiport-access-logs))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.187")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(define multiport-access-logs ()
  "Returns the list of all the multiport access logs."
  (declare (values logs))
  *multiport-access-logs*)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.187")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(define clear-access-logs (&aux logs)
  "Clears all access logs so that none will be known in the environment."
  (setq logs *all-logs*
        *all-logs* nil
        *standard-access-logs* nil
	*multiport-access-logs* nil)
  (mapc #'log-queue-process-kill logs)
  logs)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.187")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod find-access-log-if (predicate (port (eql :multiport)) &optional (start 0) end)
  (find-if predicate *multiport-access-logs* :start start :end end))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.187")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod add-access-log ((log access-log) (port (eql :multiport)))
  (pushnew log *multiport-access-logs*))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.187")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod remove-access-log ((log access-log) (port (eql :multiport)))
  (setq *multiport-access-logs* (delete log *multiport-access-logs*)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.187")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod remove-access-log ((log access-log) (port (eql :all)))
  (remove-access-log log :multiport)
  (loop for entry in *standard-access-logs*
        do (when (member log (cdr entry))
             (delete log entry)
             (unless (cdr entry)
               (setq *standard-access-logs* (delete entry *standard-access-logs*))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.858")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(SCL:FUNDEFINE 'SERVER-ACCESS-LOGS)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.858")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(define-generic server-access-logs (server)
  (declare (values current-port-logs multiport-logs))
  (:documentation "Returns the access log objects for SERVER."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.858")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod server-access-logs ((server server-logging-mixin))
  (values (standard-access-logs *standard-http-port*)
	  (multiport-access-logs)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.858")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defgeneric log-access (server)
  (:documentation "Logs an HTTP server access according to the request port."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.858")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod log-access ((server server-logging-mixin)) 
  (dolist (log (standard-access-logs *standard-http-port*))
    (log-server-access log server))
  (dolist (log (multiport-access-logs))
    (log-server-access log server)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.187")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(define intern-access-log (name &key (port *standard-http-port*)
                                (if-does-not-exist :error)
                                (directory *standard-log-directory*)
                                host
                                (class *log-access-log-class*))
  "Interns a server access log named, NAME, that monitors port, PORT.
If port is null, this returns any log whose name and class match.
If port is :MULTIPORT, it returns a multiport log."
  (declare (values log newly-created-p))
  (flet ((equal-log-p (x)
           (and (equalp name (log-name x))
                (typep x class)))
	 (handle-does-not-exist (name port class host)
	   (ecase if-does-not-exist
	     (:soft nil)
	     (:create
	       (let* ((*standard-log-directory* directory)
		      (log (allocate-log :name name :port port :class class :local-host (or host (local-host)))))
		 (when port (add-access-log log port))
		 (values log t)))
	     (:error (error "Unknown HTTP server access log, ~A~:[~:*, for port ~D~]." name port)))))
    (declare (dynamic-extent #'equal-log-p))
    (etypecase name
      (string
        (cond
          ((find-access-log-if #'equal-log-p port))
          (t (handle-does-not-exist name port class host))))
      (basic-log-mixin
	(if (and (or (null port) (eql port (log-port name)))
		 (typep name class))
	    name
	    (handle-does-not-exist (log-name name) port class host))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.188")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(define standard-access-logs (port)
  "Returns the standard access logs for the port, PORT."
  (declare (values logs))
  (etypecase port
    (integer
      (let ((entry (assoc port *standard-access-logs* :test #'eq)))
	(cond (entry (cdr entry))
	      (t (intern-access-log (default-log-file-name port)
				    :port port
				    :directory *standard-log-directory*
				    :class *log-access-log-class*
				    :if-does-not-exist :create)
		 (or (cdr (assoc port *standard-access-logs* :test #'eql))
		     (error "Should never happen: Access log not created!"))))))
    (keyword
      (ecase port
	(:multiport *multiport-access-logs*)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.189")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defun %note-access-status-code (log status-code proxy-p)
  (check-type status-code integer)
  ;; with-slots required for locatives to work in Genera.
  (with-slots (n-requests n-requests-served n-server-errors n-client-errors
                          n-insufficient-resource-denials n-redirects n-access-denials
			  n-gateway-errors n-proxy-requests n-proxy-cache-hits) log
    (atomic-incf n-requests)
    (when proxy-p
      (atomic-incf n-proxy-requests)
      (when  (eql proxy-p :cache-hit)
	(atomic-incf n-proxy-cache-hits)))
    (cond
      ((< 199 status-code 300) (atomic-incf n-requests-served))
      ((< 299 status-code 400) (atomic-incf n-redirects))
      ((and (< 400 status-code 416) (/= status-code 408))
       (atomic-incf n-access-denials))
      ((member status-code '(400 408) :test #'=)
       (atomic-incf n-client-errors))
      ((< 499 status-code)
       (case status-code
	 ((500 501 505) (atomic-incf n-server-errors))
	 (502 (atomic-incf n-gateway-errors))
	 (503 (atomic-incf n-insufficient-resource-denials)))))
    log))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.859")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(SCL:FUNDEFINE 'UPDATE-LOG-STATISTICS)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.859")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(define-generic update-log-statistics (log-counters-mixin status method proxy-p requests-completed
                                                          bytes-transmitted bytes-received elapsed-time cpu-time))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.859")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

;; This could use a log thread if one is available.   3/16/97 -- JCMa.
(defmethod update-log-statistics ((log log-counters-mixin) status method proxy-p requests-completed
                                  transmitted-bytes received-bytes time-elapsed time-cpu)
  (with-slots (bytes-transmitted bytes-received elapsed-time cpu-time) log
    (%note-access-status-code log status proxy-p)
    (%note-http-method log method)
    (atomic-incf bytes-transmitted transmitted-bytes)
    (atomic-incf bytes-received received-bytes)
    (atomic-incf elapsed-time time-elapsed)
    (atomic-incf cpu-time time-cpu)
    (%note-http-connections log requests-completed)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.859")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod log-server-access and ((log log-counters-mixin) (server server-logging-mixin)
                                  &aux (stream (server-stream server)))
  (let ((cpu-time (cpu-time server))
        (elapsed-time (elapsed-time server))
        (status (server-status server))
        (method (server-method server))
	(proxy-p (server-proxy-request-p server))
        (requests-completed (server-requests-completed server))
        (bytes-transmitted (www-utils:bytes-transmitted stream))
        (bytes-received (www-utils:bytes-received stream)))
    (update-log-statistics log status method proxy-p requests-completed
                           bytes-transmitted bytes-received elapsed-time cpu-time))
  t)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.189")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(SCL:FUNDEFINE 'NOTE-ACCESS-STATUS-CODE)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.189")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(define-generic note-access-status-code (log status-code proxy-p)
  (:documentation "Updates counters in LOG according to status-code."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.189")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod note-access-status-code ((log log-counters-mixin) status-code proxy-p)
  (%note-access-status-code log status-code proxy-p))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.859")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(define-generic server-proxy-request-p (server)
  (:documentation "Returns non-null when the current request is a proxy request.
When the proxy cache is hit, the value is :CACHE-HIT"))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.859")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(define-generic server-proxy-cache-hit-p (server)
  (:documentation "Returns non-null when the current proxy request was served from the proxy cache."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.859")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod server-proxy-cache-hit-p ((server proxy-server-mixin))
  (with-slots (proxy-request-p) server
    (eq :cache-hit proxy-request-p)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.859")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(define-generic server-note-proxy-cache-hit (server)
  (:documentation "Informs server that a proxy request has been served out of the proxy cache."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.859")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod server-note-proxy-cache-hit ((server proxy-server-mixin))
  (with-slots (proxy-request-p) server
    (setq proxy-request-p :cache-hit)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.190")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(define ensure-multiport-statistics-log ()
  "Ensures that a multi-port statistics log is running."
  (ensure-extended-access-log :name "Multi-Port-Statistics-Log" :port :multiport :class 'access-statistics-log))

(mapc #'(lambda (x) (export (intern x :http) :http)) '("ACCESS-STATISTICS-LOG" "ENSURE-MULTIPORT-STATISTICS-LOG"))
;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.190")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(add-initialization
  "Ensure Multiport Server Statistics"
  '(ensure-multiport-statistics-log)
  '(:normal)
  '*server-initialization-list*)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.39")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defclass extended-common-file-log
          (extended-common-file-format-mixin
            access-log
            basic-file-logging-mixin)
    ((log-file-name :initform "Ext-Common-Log" :initarg :log-file-name :allocation :class))
  (:documentation "This log class records accesses according to the Common File Format
but also records the referrer field and the user agent when they are present.
This will cons more than common-file-log because it must copy two header values."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.39")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defclass common-file-log
          (common-file-format-mixin
            access-log
            basic-file-logging-mixin)
    ((log-file-name :initform "Common-Log" :initarg :log-file-name :allocation :class))
  (:documentation "This log class records accesses according to the Common File Format."))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.40")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defclass synchronous-log-counters-mixin
	  (log-counters-mixin)
    ()
  (:documentation "A mixin that updates statistics synchonously."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.40")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defclass asynchronous-log-counters-mixin
	  (log-counters-mixin process-queued-logging-mixin)
    ()
  (:documentation "A mixin that updates statistics asynchonously."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.860")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(SCL:FUNDEFINE '(METHOD LOG-SERVER-ACCESS (LOG-COUNTERS-MIXIN SERVER-LOGGING-MIXIN) AND))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.860")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod log-server-access and ((log synchronous-log-counters-mixin) (server server-logging-mixin)
                                  &aux (stream (server-stream server)))
  (let ((cpu-time (cpu-time server))
        (elapsed-time (elapsed-time server))
        (status (server-status server))
        (method (server-method server))
	(proxy-p (server-proxy-request-p server))
        (requests-completed (server-requests-completed server))
        (bytes-transmitted (www-utils:bytes-transmitted stream))
        (bytes-received (www-utils:bytes-received stream)))
    (update-log-statistics log status method proxy-p requests-completed
                           bytes-transmitted bytes-received elapsed-time cpu-time))
  t)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.860")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod log-server-access and ((log asynchronous-log-counters-mixin) (agent server-logging-mixin))
  (let ((entry-writer (log-entry-writer log agent)))
    (when entry-writer
      (tq:push-task-queue log entry-writer))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.41")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defclass synchronous-log-notification-mixin
          (log-notification-mixin)
    ())

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.41")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defclass asynchronous-log-notification-mixin
          (log-notification-mixin process-queued-logging-mixin)
    ())


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.41")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defclass access-statistics-log
	  (asynchronous-log-notification-mixin
	    asynchronous-log-counters-mixin
	    log-locking-mixin
	    access-log)
    ()
  (:documentation "This log class maintains statistics on HTTP service."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.860")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(SCL:FUNDEFINE '(METHOD LOG-SERVER-ACCESS (LOG-NOTIFICATION-MIXIN SERVER-LOGGING-MIXIN) AND))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.861")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(declaim (inline %server-write-console-window-notification))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.861")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(define-generic write-console-window-notification (server &optional log-stream)
  (:documentation "Writes an entry in console notification format for SERVER on LOG-STREAM."))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.861")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(SCL:FUNDEFINE 'LOG-NOTIFY-CONSOLE)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.861")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(define-generic log-notify-console (server)
  (:documentation "Notifies the console with a server log entry."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.861")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod log-notify-console ((server server-logging-mixin))
  (%server-notify-console-window server))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.861")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod log-server-access and ((log synchronous-log-notification-mixin) (server server-logging-mixin))
  (when (log-notification log)
    (log-notify-console server))
  t)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.463")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defun careful-notify-log-window (continuation)
  "Notifies the console being careful with special characters."
  (using-resource (string line-buffer *line-buffer-size*)
    (setf (fill-pointer string) 0)
    (with-output-to-string (stream string)
      (funcall continuation stream)
      (if (find #\~ string :test #'eql)
	  (www-utils:notify-log-window "~A" string)
	  (www-utils:notify-log-window string)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.861")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod log-entry-writer ((log asynchronous-log-counters-mixin) (server server-logging-mixin)
			     &aux (stream (server-stream server)))
  (let ((cpu-time (cpu-time server))
        (elapsed-time (elapsed-time server))
        (status (server-status server))
        (method (server-method server))
	(proxy-p (server-proxy-request-p server))
        (requests-completed (server-requests-completed server))
        (bytes-transmitted (www-utils:bytes-transmitted stream))
        (bytes-received (www-utils:bytes-received stream)))
    (flet ((update-stats (log)
	     (update-log-statistics log status method proxy-p requests-completed
				    bytes-transmitted bytes-received elapsed-time cpu-time)))
      #'update-stats)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.861")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod log-server-access and ((log asynchronous-log-notification-mixin) (agent server-logging-mixin))
  (let ((entry-writer (log-entry-writer log agent)))
    (when entry-writer
      (tq:push-task-queue log entry-writer))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.861")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun %server-write-console-window-notification (server log-stream)
  (let* ((requests (server-requests-completed server))
	 (host-name (host-log-name server))
	 (user-name (%server-user-qualified-name server))
	 (request (server-request server t))
	 (request-time nil)
	 (status (server-status server))
	 (port *standard-http-port*)		;stream data may no longer be valid
	 (bytes (bytes-transmitted (server-stream server)))	;total bytes (not number of bytes in a document)
	 (proxy-p (server-proxy-request-p server))
	 header-set cpu-time elapsed-time user-agent-val referer-val)
    (unless proxy-p
      (when (setq header-set (server-headers server)))
      (with-header-values (user-agent referer) header-set
	(setq user-agent-val user-agent
	      referer-val referer)))
    (psetq cpu-time (cpu-time server)
	   elapsed-time (elapsed-time server))
    (%write-console-window-notification
      host-name port proxy-p requests request request-time status bytes user-name user-agent-val referer-val
      cpu-time elapsed-time log-stream)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.861")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod write-console-window-notification ((server server-logging-mixin) &optional (log-stream *standard-output*))
  (with-string-for-null-stream (log-stream)
    (%server-write-console-window-notification server log-stream)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.861")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun %server-notify-console-window (server)
  (flet ((notify-the-console (stream)
	   (%server-write-console-window-notification server stream)))
    (declare (dynamic-extent #'notify-the-console))
    (careful-notify-log-window #'notify-the-console)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.861")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod log-entry-writer ((log asynchronous-log-notification-mixin) (server server-logging-mixin)
			     &aux (stream (server-stream server)))
  (let* ((requests (server-requests-completed server))
	 (host-name (host-log-name server))
	 (user-name (%server-user-qualified-name server))
	 (request (server-request server t))
	 (status (server-status server))
	 (port *standard-http-port*)		;stream data may no longer be valid
	 (bytes (bytes-transmitted (server-stream server)))	;total bytes (not number of bytes in a document)
	 (proxy-p (server-proxy-request-p server))
	 header-set cpu-time elapsed-time user-agent-val referer-val)
    (unless proxy-p
      (when (setq header-set (server-headers server)))
      (with-header-values (user-agent referer) header-set
	(setq user-agent-val user-agent
	      referer-val referer)))
    (psetq cpu-time (cpu-time server)
	   elapsed-time (elapsed-time server))
    (flet ((notify (log)
	     (when (log-notification log)	;don't lose with a slew of these backed up in the queue
	       (flet ((notify-the-console (log-stream)
			(%write-console-window-notification
			  host-name port proxy-p requests request nil status bytes user-name user-agent-val referer-val
			  cpu-time elapsed-time log-stream)))
		 (declare (dynamic-extent #'notify-the-console))
		 (careful-notify-log-window #'notify-the-console)))))
      #'notify)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.42")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defclass log-counters-mixin
          ()
    ((n-access-denials :initform 0 :accessor log-number-of-access-denials)
     (n-gateway-errors :initform 0 :accessor log-number-of-gateway-errors)
     (n-client-errors :initform 0 :accessor log-number-of-client-errors)
     (n-insufficient-resource-denials :initform 0 :accessor log-number-of-insufficient-resource-denials)
     (n-redirects :initform 0 :accessor log-number-of-redirects)
     (n-requests :initform 0 :accessor log-total-number-of-requests)
     (n-proxy-requests :initform 0 :accessor log-number-of-proxy-requests)
     (n-proxy-cache-hits :initform 0 :accessor log-number-of-proxy-cache-hits)
     (n-requests-served :initform 0 :accessor log-number-of-requests-served)
     (n-server-errors :initform 0 :accessor log-number-of-server-errors)
     (bytes-transmitted :initform 0 :accessor log-bytes-transmitted)
     (bytes-received :initform 0 :accessor log-bytes-received)
     (elapsed-time :initform 0 :accessor elapsed-time)  ;elapsed time in INTERNAL-TIME-UNITS-PER-SECOND
     (cpu-time :initform 0 :accessor cpu-time)  ;microseconds of compute time
     (n-connections :initform 0 :accessor log-http-connections)
     (n-deletes :initform 0 :accessor log-number-of-deletes)
     (n-gets :initform 0 :accessor log-number-of-gets)
     (n-heads :initform 0 :accessor log-number-of-heads)
     (n-options :initform 0 :accessor log-number-of-options)
     (n-extension-methods :initform 0 :accessor log-number-of-extension-methods)
     (n-posts :initform 0 :accessor log-number-of-posts)
     (n-puts :initform 0 :accessor log-number-of-puts)
     (n-traces :initform 0 :accessor log-number-of-traces)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.193")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defun %write-console-window-notification (host-name port proxy-p requests-per-connection request request-time status bytes user-name user-agent referer
						     cpu-time elapsed-time stream &aux (tab #\tab))
  (flet ((write-milliseconds (milliseconds stream) ;; "Writes milliseconds in 4 characters."
	   (cond ((< milliseconds 1000)
		  (prin1 (float (/ (round milliseconds 10) 100)) stream))
		 ((< milliseconds 10000)
		  (prin1 (float (/ (round milliseconds 10) 100)) stream))
		 ((< milliseconds 100000)
		  (prin1 (float (/ (round milliseconds 100) 10)) stream))
		 (t (prin1 (round milliseconds 1000) stream))))
	 (write-microseconds (microseconds stream) ;; "Writes microseconds in 4 characters."
	   (let* ((milliseconds (round microseconds 1000)))
	     (cond ((> 10 milliseconds)
		    (write-string "   " stream))
		   ((> 100 milliseconds)
		    (write-string "  " stream))
		   ((> 1000 milliseconds)
		    (write-char #\space stream)))
	     (prin1 milliseconds stream)))
         (get-request-indices (request)
           (when request
             (let* ((end (length request))
                    (pos1 (and (not (zerop end)) (%fast-position-if white-space-char-p request :start 0 :end end)))
                    (pos2 (and pos1 (%fast-position-if white-space-char-p request :start (1+ pos1) :end end))))
               (values pos2 (and pos1 end))))))
    (declare (inline write-microseconds write-milliseconds get-request-indices))
    (multiple-value-bind (http-version-pos request-length)
        (get-request-indices request)
      ;; date and time
      (cond (request-time
	     (write-char #\[ stream)
	     (multiple-value-bind (seconds minutes hours)
		 (decode-universal-time request-time)
	       (write-24-hour-time hours minutes seconds stream)
	       ;; milliseconds of elapsed time
	       (write-char #\space stream)
	       (write-milliseconds elapsed-time stream)
	       ;; microsecond CPU time
	       (write-char #\space stream)
	       (write-microseconds cpu-time stream)
	       (write-char #\] stream)))
	    (t (setq tab #\space)
	       (write-char #\{ stream)
	       (write-milliseconds elapsed-time stream)
	       (write-char #\space stream)
	       (write-microseconds cpu-time stream)
	       (write-char #\} stream)))
      ;; host domain name or IP address.
      (write-char tab stream)
      (write-string host-name stream)
      (write-char tab stream)
      ;; Status code returned to the client.
      (prin1 status stream)
      ;; HTTP version
      (write-char #\space stream)
      (cond (http-version-pos
             (write-string request stream :start (1+ http-version-pos) :end request-length))
            (request-length
             (write-string "HTTP/0.9" stream))
            (t (write-string "HTTP/?.?" stream)))
      ;; Server access port
      (fast-format stream " ~D" port)
      (write-char #\space stream)
      ;; number of requests per connection
      (fast-format stream "{~D}" requests-per-connection)
      ;; Number of bytes transfered to the client.
      (write-char #\space stream)
      (prin1 bytes stream)
      ;; Authenticated User Name
      (write-char tab stream)
      (if user-name (write user-name :escape t :stream stream) (write-char #\- stream))
      (when proxy-p
	(write-char #\space stream)
	(write-char (if (eql :cache-hit proxy-p) #\+ #\-) stream))
            ;; Exact request received from client.
      (write-char #\space stream)
      (write-char #\" stream)
      (cond (http-version-pos
             (write-string request stream :start 0 :end http-version-pos))
            (request-length
             (write-string request stream :start 0 :end request-length)))
      (write-char #\" stream)
      (unless proxy-p
	(write-char tab stream)
	(if user-agent (write user-agent :stream stream :escape t) (write-char #\- stream))
	(write-char #\space stream)
	(if referer (write referer :stream stream :escape t) (write-char #\- stream)))
      ;; Trailing CR makes this consistently parsable.
      (terpri stream))))
