;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Patch-File: T -*-
;;; Patch file for HTTP-PROXY version 6.19
;;; Reason: Variable HTTP::*PROXY-PORT-ACCESS-CONTROL-ALIST*:  -
;;; Function HTTP::PROXY-ACCESS-CONTROL:  -
;;; Function HTTP::SET-PROXY-ACCESS-CONTROL:  -
;;; Function (CLOS:METHOD HTTP::SET-PROXY-ACCESS-CONTROL (LISP:INTEGER HTTP:ACCESS-CONTROL)):  -
;;; Function (CLOS:METHOD HTTP::SET-PROXY-ACCESS-CONTROL (T CONS)):  -
;;; Function HTTP::WITH-PROXY-AUTHENTICATION-ACCESS-CONTROL:  -
;;; Function (CLOS:METHOD HTTP::INVOKE-PROXY-SERVICE (HTTP::PROXY-SERVER-MIXIN T T T) :AROUND):  install user access control.
;;; Written by JCMa, 4/10/01 20:48:48
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.118,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.26, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, HTTP Proxy Server 6.18,
;;; HTTP Client Substrate 4.9, Statice Server 466.2, HTTP Client 50.6,
;;; Image Substrate 440.4, Essential Image Substrate 433.0, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x976 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
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
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.7),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:PROXY;CACHE.LISP.156"
  "HTTP:PROXY;PROXY.LISP.70")


(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL)
  (SCT:REQUIRE-PATCH-LEVEL-FOR-PATCH '(CL-HTTP 70. 119.)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;CACHE.LISP.156")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;;;------------------------------------------------------------------- 
;;;
;;; PROXY USER ACCESS CONTROL
;;;

(define-variable *proxy-port-access-control-alist* nil
		 "Holds an alist mapping proxy ports to access controls.")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;CACHE.LISP.156")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(declaim (inline proxy-access-control))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;CACHE.LISP.156")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun proxy-access-control (port)
  "Returns the access control for the proxy operating on PORT."
  (cdr (assoc port *proxy-port-access-control-alist* :test #'eql)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;CACHE.LISP.156")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod set-proxy-access-control ((port integer) (access-control access-control))
  (let ((entry (assoc port *proxy-port-access-control-alist* :test #'eql)))
    (if entry
	(setf (cdr entry) access-control)
	(push (list* port access-control) *proxy-port-access-control-alist*)))
  access-control)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;CACHE.LISP.156")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod set-proxy-access-control (port (access-control-spec cons))
  (destructuring-bind ((realm-name &rest realm-args) access-control-name &rest access-control-args)
      access-control-spec
    (declare (dynamic-extent realm-args access-control-args))
    (let* ((realm-object (apply #'intern-realm realm-name realm-args))
	   (access-control-object (apply #'intern-access-control realm-object access-control-name access-control-args)))
      (set-proxy-access-control port access-control-object))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;CACHE.LISP.156")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defgeneric set-proxy-access-control (port access-control)
  (:documentation "Sets user-based access control for the proxy running on port, PORT, to use ACCESS-CONTROL.
ACCESS-CONTROL is either a named access control or NIL. When NIL, user-based access control
is turned off on PORT. When it is a specification suitable for interning the realm and
access control, the syntax for ACCESS-CONTROL is:

     ((REALM-NAME &REST REALM-ARGS) ACCESS-CONTROL-NAME &REST ACCESS-CONTROL-ARGS)

Where REALM-ARGS are arguments to INTERN-REALM and ACCESS-CONTROL-ARGS are arguments
to INTERN-ACCESS-CONTROL, excluding the realm."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;CACHE.LISP.156")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-macro with-proxy-authentication-access-control ((proxy-access-control method &key rejection-form (server '*server*)) &body body)
  "Executes REJECTION-FORM whenever AUTHORIZATION does not qualify for capabilities under PROXY-ACCESS-CONTROL.
Otherwise executes BODY."
  `(let (.access-control.)
     (cond ((setq .access-control. ,proxy-access-control)
	    (let ((realm (access-control-realm .access-control.))
		  authorization user)
	      (handler-case
		(cond ((and (setq authorization (get-header :proxy-authorization *headers*)) 
			    (setq user (authenticate-user realm authorization ,method :proxy-access))
			    (allow-user-access-p .access-control. user ,method))
		       (setf (server-user-object ,server) user)
		       ,@body)
		      (t ,rejection-form))
		(unknown-authentication-method () ,rejection-form))))
	   (t ,@body))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY.LISP.70")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;;;------------------------------------------------------------------- 
;;;
;;; PROXY METHODS 
;;; 

(defmethod invoke-proxy-service :around ((server proxy-server-mixin) url method http-version)
  (flet ((provide-proxy-service (server url method http-version &aux (catch-error-p (not *debug-proxy*)))
	   (flet ((ensure-live-upstream-connection (condition)
		    (declare (ignore condition))
		    (abort-if-connection-inactive *server*)
		    nil))
	     (handler-case-if 
		 catch-error-p
		;; Nasty signalling ensues if the client has dropped the connection, 
		;; so intercept errors here and abort the connection if the client is gone. -- JCMa 5/24/1999.
		(handler-bind-if catch-error-p
		   ((condition #'ensure-live-upstream-connection))
		  ;; Set the life time for the connection 
		  (setf (server-life-time server) *proxy-server-life-time*)
		  ;; call the primary method
		  (call-next-method server url method http-version))
	       (unknown-host-name (err) (error 'proxy-unresolvable-domain-name :format-string (report-string err)
					       :method method :url url))
	       (protocol-timeout (err) (error 'proxy-connection-timeout :format-string (report-string err)
					      :method method :url url))
	       (connection-refused (err) (error 'proxy-connection-refused :format-string (report-string err)
						:method method :url url))
	       (local-network-error (err)  (error 'proxy-local-network-error :format-string (report-string err)
						  :method method :url url :close-connection t))
	       (remote-network-error (err) (error 'proxy-remote-network-error :format-string (report-string err)
						  :method method :url url :close-connection t))))))
    (let ((proxy-access-control (proxy-access-control 8000)))
      (typecase proxy-access-control
	(null ;; Always respect subnet security as the default
	  (with-subnet-access-control
	    ((server-address server) (or *proxy-subnets* *secure-subnets*) 
	     :deny-subnets *disallowed-subnets*
	     :rejection-form (error 'proxy-access-forbidden :method method :url url))
	    (provide-proxy-service server url method http-version)))
	(basic-access-control ;; Respect subnet security for basic access control
	  (with-subnet-access-control
	    ((server-address server) (or *proxy-subnets* *secure-subnets*) 
	     :deny-subnets *disallowed-subnets*
	     :rejection-form (error 'proxy-access-forbidden :method method :url url))
	    (with-proxy-authentication-access-control
	      (proxy-access-control
		method
		:server server
		:rejection-form (error 'recoverable-unauthorized-proxy-access :method method :url url
				       :authentication-realm realm :authentication-method (realm-scheme realm)))
	      (provide-proxy-service server url method http-version))))
	;; Require either subnet security or digest user access. Make this configurable sometime -- JCMa 04/10/2001
	(t (with-proxy-authentication-access-control
	     (proxy-access-control
	       method
	       :server server
	       :rejection-form (with-subnet-access-control
				 ((server-address server) (or *proxy-subnets* *secure-subnets*) 
				  :deny-subnets *disallowed-subnets*
				  :rejection-form (error 'recoverable-unauthorized-proxy-access :method method :url url
							 :authentication-realm realm :authentication-method (realm-scheme realm)))
				 (provide-proxy-service server url method http-version)))
	     (provide-proxy-service server url method http-version)))))))

