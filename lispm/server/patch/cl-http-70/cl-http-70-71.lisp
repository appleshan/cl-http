;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.71
;;; Reason: CLOS class HTTP::PROXY-SERVER-MIXIN:  add client data slot.
;;; Function HTTP::LOG-SERVER-ACCESS-LOGS-PROXY-ACCESS:  Turns on and off whether normal server logs also log proxy accesses.
;;; Function (CLOS:METHOD HTTP::LOG-SERVER-ACCESS-LOGS-PROXY-ACCESS (T T)):  default does nothing.
;;; Function (CLOS:METHOD HTTP::LOG-SERVER-ACCESS-LOGS-PROXY-ACCESS (CONS T)):  -
;;; Function (CLOS:METHOD HTTP::LOG-SERVER-ACCESS-LOGS-PROXY-ACCESS (HTTP::SERVER-COMMON-FILE-LOG T)):  -
;;; Function (CLOS:METHOD HTTP::LOG-SERVER-ACCESS-LOGS-PROXY-ACCESS (HTTP:EXTENDED-COMMON-FILE-LOG NULL)):  -
;;; Function (CLOS:METHOD HTTP::LOG-SERVER-ACCESS-LOGS-PROXY-ACCESS (HTTP::SERVER-EXTENDED-COMMON-FILE-LOG T)):  -
;;; Remove function (CLOS:METHOD HTTP::LOG-ENTRY-P (HTTP::EXCLUSIVE-SERVER-LOG-MIXIN HTTP::PROXY-SERVER-MIXIN)): undefine.
;;; Function (CLOS:METHOD HTTP::LOG-ENTRY-P (HTTP::EXCLUSIVE-SERVER-LOG-MIXIN HTTP::PROXY-SERVER-MIXIN) AND):  fix method combination bug.
;;; Remove function (CLOS:METHOD HTTP::LOG-ENTRY-P (HTTP::EXCLUSIVE-PROXY-LOG-MIXIN HTTP::PROXY-SERVER-MIXIN)): undefine.
;;; Function (CLOS:METHOD HTTP::LOG-ENTRY-P (HTTP::EXCLUSIVE-PROXY-LOG-MIXIN HTTP::PROXY-SERVER-MIXIN) AND):  ditto.
;;; Variable HTTP:*PROXY-SERVICE*:  update documentation.
;;; Function HTTP::PROXY-SERVICE-ENABLED-P:  provide optional port argument.
;;; Function HTTP::%EXECUTE-REQUEST:  use proxy-service-enabled-p to determine service.
;;; Function WWW-UTILS::ENSURE-HTTP-PROTOCOL-ON-PORT:  new platofmr-specific function to able http on a port.
;;; Function WWW-UTILS::ENABLE-HTTP-SERVICE:  respect the on-ports argument.
;;; Function WWW-UTILS::WITH-TCP-PORT-FOR-PROTOCOL:  add protocol and port when not present and stack cons.
;;; Written by JCMa, 9/14/00 17:01:45
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.70,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42, HTTP Proxy Server 5.22,
;;; HTTP Client Substrate 3.17, Ivory Revision 5, VLM Debugger 329,
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

;;; Patch file for CL-HTTP version 70.71
;;; Written by JCMa, 9/14/00 23:36:05
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.71,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42, HTTP Proxy Server 5.23,
;;; HTTP Client Substrate 3.18, HTTP Client 49.9, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, Ivory Revision 5, VLM Debugger 329,
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


;;; Patch file for CL-HTTP version 70.71
;;; Written by JCMa, 9/14/00 22:51:57
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.71,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42, HTTP Proxy Server 5.23,
;;; HTTP Client Substrate 3.18, HTTP Client 49.9, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, Ivory Revision 5, VLM Debugger 329,
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


;;; Patch file for CL-HTTP version 70.71
;;; Written by JCMa, 9/14/00 22:09:59
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.71,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42, HTTP Proxy Server 5.22,
;;; HTTP Client Substrate 3.17, HTTP Client 49.9, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, Ivory Revision 5, VLM Debugger 329,
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


;;; Patch file for CL-HTTP version 70.71
;;; Written by JCMa, 9/14/00 21:49:16
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.70,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42, HTTP Proxy Server 5.22,
;;; HTTP Client Substrate 3.17, HTTP Client 49.9, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, Ivory Revision 5, VLM Debugger 329,
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


(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;CLASS.LISP.35"
  "HTTP:SERVER;SERVER.LISP.853"
  "HTTP:SERVER;LOG.LISP.184"
  "HTTP:SERVER;LOG.LISP.185"
  "HTTP:SERVER;LOG.LISP.186"
  "HTTP:SERVER;VARIABLES.LISP.187"
  "HTTP:SERVER;SERVER.LISP.854"
  "HTTP:LISPM;SERVER;LISPM.LISP.468"
  "HTTP:LISPM;SERVER;LISPM.LISP.469"
  "HTTP:LISPM;SERVER;LISPM.LISP.470"
  "HTTP:LISPM;SERVER;LISPM.LISP.471")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CLASS.LISP.35")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defclass proxy-server-mixin
	  ()
    ((proxy-request-p :initform nil :initarg :proxy-request-p :accessor server-proxy-request-p)	;whether we're serving a proxy request
     (client-data :initform nil :initarg client-data :accessor server-proxy-client-data))	;data passed back from client
  (:documentation "Provides proxy service capabilities."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.853")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod reset-transaction-state ((server basic-server-mixin))
  (let ((stream (server-stream server))
        (process (server-process server)))
    ;; reset resourced items
    (setf (fill-pointer (%server-request server)) 0
	  (fill-pointer (server-url-buffer server)) 0)
    (clear-header-set (server-headers server) nil)
    ;; reset instance variables
    (setf (server-request-copy server) nil
	  (server-method server) nil
          (server-url-string server) nil
          (server-url server) nil
          (property-list server) nil
          (server-status server) 200
	  (server-form-data server) nil
	  (server-close-connection-p server) nil
	  (server-persistent-connection-p server) nil
          (www-utils:bytes-transmitted stream) 0
          (www-utils:bytes-received stream) 0
          (server-request-time server) nil
          (server-timeout server) *server-timeout*
          (server-life-time server) *server-life-time*
          (server-start-time server) (get-internal-real-time)
          (server-process-start-time server) (www-utils:process-run-time process)
	  (server-proxy-request-p server) nil
	  (server-proxy-client-data server) nil)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.184")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(define-generic log-server-access-logs-proxy-access (log-or-logs on-p)
  (:documentation "Turns on and off whether normal server logs also log proxy accesses."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.184")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod log-server-access-logs-proxy-access (log on-p)
  (declare (ignore log on-p)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.184")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod log-server-access-logs-proxy-access ((logs cons) on-p)
  (dolist (log logs)
    (log-server-access-logs-proxy-access log on-p))
  logs)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.184")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod log-server-access-logs-proxy-access ((log common-file-log) (on-p null))
  (change-class log 'server-common-file-log))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.184")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod log-server-access-logs-proxy-access ((log server-common-file-log) on-p)
  (when on-p
    (change-class log 'common-file-log)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.184")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod log-server-access-logs-proxy-access ((log extended-common-file-log) (on-p null))
  (change-class log 'server-extended-common-file-log))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.184")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod log-server-access-logs-proxy-access ((log server-extended-common-file-log) on-p)
  (when on-p
    (change-class log 'extended-common-file-log)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.185")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(SCL:FUNDEFINE '(METHOD LOG-ENTRY-P (EXCLUSIVE-SERVER-LOG-MIXIN PROXY-SERVER-MIXIN)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.186")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

;; Predicate that decides when we have a normal server log
(defmethod log-entry-p and ((log exclusive-server-log-mixin) (server proxy-server-mixin))
  (not (server-proxy-request-p server)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.186")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(SCL:FUNDEFINE '(METHOD LOG-ENTRY-P (EXCLUSIVE-PROXY-LOG-MIXIN PROXY-SERVER-MIXIN)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.186")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

;; Predicate that decides when we have a proxy log
(defmethod log-entry-p and ((log exclusive-proxy-log-mixin) (server proxy-server-mixin))
  (server-proxy-request-p server))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.854")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(declaim (inline proxy-service-enabled-p))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;VARIABLES.LISP.187")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp;-*-")

(define-variable *proxy-service* nil
                 "A list of proxy ports available to clients in *PROXY-SUBNETS*.")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.854")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(declaim (inline proxy-service-enabled-p))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.854")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(define proxy-service-enabled-p (&optional port)
  "Returns non-null if proxy service is enabled on port."
  (not (null (if port
		 (member port *proxy-service*)
		 *proxy-service*))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.854")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun %execute-request (server request stream)
  (declare (values persistent-connection-p)
	   (optimize (speed 3)))
  (flet ((invoke-local-service (server url-string method http-version)
           (multiple-value-bind (request-context bind-context-p)
               (request-local-context url-string http-version *headers*)
             (setf (server-url-string server) (%merge-url url-string request-context t))
             (if bind-context-p
                 (with-virtual-host-local-context (request-context)
                   (invoke-server-method server method http-version))
                 (invoke-server-method server method http-version)))))
    (declare (inline invoke-local-service))
    ;; parse the request
    (multiple-value-bind (method url-string http-version)
	(parse-request request 0 (length request) (server-url-buffer server))
      (unless (and method url-string http-version)
	(error 'bad-syntax-provided :method method :url url-string
	       :format-string "Bad HTTP Request: ~S"
	       :format-args (list request)))
      (unless (member http-version '(:http/1.1 :http/1.0 :http/0.9))
	(error 'http-version-not-supported :url url-string
	       :format-string "The server does not support ~A."
	       :format-args (list http-version)))
      (setf (server-method server) method
	    (server-http-version server) http-version
	    (server-url-string server) url-string
	    (server-status server) 200)		;anything other than 200 must reset the status value.
      (without-connection-overflows (url-string)
	(let ((*headers* (resourced-read-headers (server-headers server) stream)))
	  (cond ;; scheme prefixed URL Proxy path
	    ((url:scheme-prefixed-url-p url-string)
	     (setf (server-proxy-request-p server) t
		   (server-url-string server) url-string)
	     (let ((url:*escape-search-urls* nil)	;pass through escaping
		   (url:*search-url-signal-bad-form-encoding* nil)	;pass through broken URL- form encoded URLs.
		   (uri (intern-url url-string :if-does-not-exist *proxy-intern-url-status*)))
	       (cond  ;; Actually a local reference, start over as 
		 ((url:local-url-p uri)
		  (invoke-local-service server url-string method http-version))
		 ((proxy-service-enabled-p (server-host-local-port server))
		  (setf (server-url server) uri)
		  (invoke-proxy-service server uri method http-version))
		 (t (error 'access-forbidden 
			   :format-string "HTTP Proxy service is currently unavailable on ~A (~D)." 
			   :format-args (list (local-host-domain-name) (server-host-local-port server))
			   :method method :url uri)))))
	    ;; Standard path, call the primary server method.
	    (t (invoke-local-service server url-string method http-version))))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.468")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(export (intern "ENSURE-HTTP-PROTOCOL-ON-PORT" :www-utils) :www-utils)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.469")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

;;;------------------------------------------------------------------- 
;;;
;;; ENABLE AND DISABLE SERVICES
;;;

(define enable-http-service (&optional on-ports)
  "Top-level method for starting up HTTP servers."
  (dolist (port on-ports)
    (ensure-http-protocol-on-port port))
  (sys:enable-services :http nil))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.470")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defun ensure-http-protocol-on-port (port)
  "Ensures that the HTTP protocol is available on PORT."
  (check-type port integer)
  (flet ((add-http-to-port (port)		;required because tcp:add-tcp-port-for-protocol 
	   (nconc tcp::*tcp-protocol-alist* (list (cons port :http)))	;allows only one-to-one mapping of ports to protocols
	   ;; put them all on one page
	   (setf tcp::*tcp-protocol-alist* (copy-tree tcp::*tcp-protocol-alist*))))
    (let ((protocol (tcp:tcp-port-protocol-name port nil)))
      (case protocol
	((nil) (add-http-to-port port))
	(:http nil)
	(t (error "The protocol, ~S, is already using the port, ~D." protocol port))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.471")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(define-macro with-tcp-port-for-protocol ((protocol port) &body body)
  "Makes PROTOCOL use port PORT with the scope of BODY."
  `(case (cdr (assoc ,port tcp::*tcp-protocol-alist*))
     (,protocol ,@body)
     (t (sys:stack-let ((tcp::*tcp-protocol-alist* (list* (list* ,port ,protocol) tcp::*tcp-protocol-alist*)))
	  ,@body))))

