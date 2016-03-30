;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Patch-File: T -*-
;;; Patch file for HTTP-CLIENT-SUBSTRATE version 4.4
;;; Reason: Handle HTTP 1.1 servers that do not implement 100-continue status codes.
;;; 
;;; Function (CLOS:METHOD HTTP::SERVER-HTTP-VERSION-ON-PORT (T T)):  -
;;; Function (CLOS:METHOD HTTP::NOTE-SERVER-HTTP-VERSION-ON-PORT (T T T)):  -
;;; Function HTTP::GET-HOST-PORT-HTTP-SERVER-PROPERTY:  -
;;; Function (CLOS:METHOD HTTP::GET-HOST-PORT-HTTP-SERVER-PROPERTY (T T T)):  -
;;; Function (CLOS:METHOD (CL:SETF HTTP::GET-HOST-PORT-HTTP-SERVER-PROPERTY) (T T T T)):  -
;;; CLOS class HTTP::BASIC-CONNECTION:  add plist.
;;; Function HTTP::ALLOCATE-CONNECTION:  initialize http-plist.
;;; Function (CLOS:METHOD HTTP::NOTE-SERVER-HTTP-PROPERTY (HTTP::BASIC-CONNECTION T T)):  -
;;; Function (CLOS:METHOD HTTP:INVOKE-HTTP-SERVICE (HTTP::BASIC-CLIENT-MIXIN T T T)):  handle buggy http 1.1 100-continue servers.
;;; Function (CLOS:METHOD HTTP::CONNECTION-CONFIRMED-VERSION (HTTP::BASIC-CONNECTION)):  return http plist as well.
;;; Period Task: Client GC Server Version Cache
;;; Written by JCMa, 12/13/00 22:59:03
;;; while running on FUJI-3 from FUJI:/usr/lib/symbolics/Inc-CL-HTTP-70-90-LMFS.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, LMFS 442.1, Color 427.1,
;;; Graphics Support 431.0, Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Color System Documentation 10.0, SGD Book Design 10.0, Images 431.2,
;;; Image Substrate 440.4, CLIM 72.0, Genera CLIM 72.0, CLX CLIM 72.0,
;;; PostScript CLIM 72.0, CLIM Demo 72.0, CLIM Documentation 72.0,
;;; Statice Runtime 466.1, Statice 466.0, Statice Browser 466.0,
;;; Statice Server 466.2, Statice Documentation 426.0, Metering 444.0,
;;; Metering Substrate 444.1, Symbolics Concordia 444.0, Graphic Editor 440.0,
;;; Graphic Editing 441.0, Bitmap Editor 441.0, Graphic Editing Documentation 432.0,
;;; Postscript 436.0, Concordia Documentation 432.0, Joshua 237.4,
;;; Joshua Documentation 216.0, Joshua Metering 206.0, Jericho 237.0, C 440.0,
;;; Lexer Runtime 438.0, Lexer Package 438.0, Minimal Lexer Runtime 439.0,
;;; Lalr 1 434.0, Context Free Grammar 439.0, Context Free Grammar Package 439.0,
;;; C Runtime 438.0, Compiler Tools Package 434.0, Compiler Tools Runtime 434.0,
;;; C Packages 436.0, Syntax Editor Runtime 434.0, C Library Headers 434,
;;; Compiler Tools Development 435.0, Compiler Tools Debugger 434.0,
;;; C Documentation 426.0, Syntax Editor Support 434.0, LL-1 support system 438.0,
;;; Pascal 433.0, Pascal Runtime 434.0, Pascal Package 434.0, Pascal Doc 427.0,
;;; Fortran 434.0, Fortran Runtime 434.0, Fortran Package 434.0, Fortran Doc 427.0,
;;; HTTP Proxy Server 6.10, HTTP Server 70.97, Showable Procedures 36.3,
;;; Binary Tree 34.0, W3 Presentation System 8.1, HTTP Client Substrate 4.3,
;;; HTTP Client 50.1, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Metering Patches 1.0, Genera 8 5 Joshua Patches 1.0,
;;; Genera 8 5 Jericho Patches 1.0, Genera 8 5 Joshua Doc Patches 1.0,
;;; Genera 8 5 Joshua Metering Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.0,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clx Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Clim Demo Patches 1.0, Genera 8 5 Color Patches 1.1,
;;; Genera 8 5 Images Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Genera 8 5 Concordia Patches 1.0,
;;; Genera 8 5 Concordia Doc Patches 1.0, Genera 8 5 C Patches 1.0,
;;; Genera 8 5 Pascal Patches 1.0, Genera 8 5 Fortran Patches 1.0, Mailer 438.0,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.16,
;;; DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x976 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number -2141189522,
;;; Vlm lmfs patch (from W:>Reti>vlm-lmfs-patch.lisp.12),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.7),
;;; Pht debugging patch (from W:>Reti>pht-debugging-patch.lisp.4),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.6).

;;; Patch file for HTTP-CLIENT-SUBSTRATE version 4.4
;;; Written by JCMa, 12/14/00 17:02:28
;;; while running on FUJI-3 from FUJI:/usr/lib/symbolics/Inc-CL-HTTP-70-90-LMFS.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, LMFS 442.1, Color 427.1,
;;; Graphics Support 431.0, Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Color System Documentation 10.0, SGD Book Design 10.0, Images 431.2,
;;; Image Substrate 440.4, CLIM 72.0, Genera CLIM 72.0, CLX CLIM 72.0,
;;; PostScript CLIM 72.0, CLIM Demo 72.0, CLIM Documentation 72.0,
;;; Statice Runtime 466.1, Statice 466.0, Statice Browser 466.0,
;;; Statice Server 466.2, Statice Documentation 426.0, Metering 444.0,
;;; Metering Substrate 444.1, Symbolics Concordia 444.0, Graphic Editor 440.0,
;;; Graphic Editing 441.0, Bitmap Editor 441.0, Graphic Editing Documentation 432.0,
;;; Postscript 436.0, Concordia Documentation 432.0, Joshua 237.4,
;;; Joshua Documentation 216.0, Joshua Metering 206.0, Jericho 237.0, C 440.0,
;;; Lexer Runtime 438.0, Lexer Package 438.0, Minimal Lexer Runtime 439.0,
;;; Lalr 1 434.0, Context Free Grammar 439.0, Context Free Grammar Package 439.0,
;;; C Runtime 438.0, Compiler Tools Package 434.0, Compiler Tools Runtime 434.0,
;;; C Packages 436.0, Syntax Editor Runtime 434.0, C Library Headers 434,
;;; Compiler Tools Development 435.0, Compiler Tools Debugger 434.0,
;;; C Documentation 426.0, Syntax Editor Support 434.0, LL-1 support system 438.0,
;;; Pascal 433.0, Pascal Runtime 434.0, Pascal Package 434.0, Pascal Doc 427.0,
;;; Fortran 434.0, Fortran Runtime 434.0, Fortran Package 434.0, Fortran Doc 427.0,
;;; HTTP Proxy Server 6.10, HTTP Server 70.98, Showable Procedures 36.3,
;;; Binary Tree 34.0, W3 Presentation System 8.1, HTTP Client Substrate 4.3,
;;; HTTP Client 50.1, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Metering Patches 1.0, Genera 8 5 Joshua Patches 1.0,
;;; Genera 8 5 Jericho Patches 1.0, Genera 8 5 Joshua Doc Patches 1.0,
;;; Genera 8 5 Joshua Metering Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.0,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clx Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Clim Demo Patches 1.0, Genera 8 5 Color Patches 1.1,
;;; Genera 8 5 Images Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Genera 8 5 Concordia Patches 1.0,
;;; Genera 8 5 Concordia Doc Patches 1.0, Genera 8 5 C Patches 1.0,
;;; Genera 8 5 Pascal Patches 1.0, Genera 8 5 Fortran Patches 1.0, Mailer 438.0,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.16,
;;; DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x976 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number -2141189522,
;;; Vlm lmfs patch (from W:>Reti>vlm-lmfs-patch.lisp.12),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.7),
;;; Pht debugging patch (from W:>Reti>pht-debugging-patch.lisp.4),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.6).


(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:CLIENT;CONNECTION.LISP.77"
  "HTTP:CLIENT;CONNECTION.LISP.78"
  "HTTP:CLIENT;CLIENT.LISP.272"
  "HTTP:CLIENT;CONNECTION.LISP.79")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CONNECTION.LISP.77")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-generic get-host-port-http-server-property (host-object http-port indicator)
  (:documentation "Returns the value of indicator from the properties for the HTTP server operating on HTTP-PORT of HOST-OBJECT."))

(eval-when (:load-toplevel)
  (clrhash *server-http-version-table*))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CONNECTION.LISP.77")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(defmethod get-host-port-http-server-property (host-object http-port indicator)
  (let ((entry (gethash host-object *server-http-version-table*))
	plist value)
    (cond ((and entry
		(setq plist (cdr (getf entry http-port))))
	   (case (setq value (getf plist indicator :+no-value+))
	     (:+no-value+ nil)
	     (t (values value t))))
	  (t nil))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CONNECTION.LISP.77")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(defmethod (setf get-host-port-http-server-property) (value host-object http-port indicator)
  (let ((entry (gethash host-object *server-http-version-table*)))
    (cond (entry
	   (let ((plist (getf entry http-port)))
	     (if plist
		 (setf (getf (cdr plist) indicator) value)
		 (setf (getf entry http-port) (list nil indicator value)))))
	  (t (setf (gethash host-object *server-http-version-table*) (list http-port (list nil indicator value)))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CONNECTION.LISP.77")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(defclass basic-connection
          (connection-pool-mixin)
    ((stream :initarg :stream :accessor connection-stream)
     (close-p :initform nil :initarg :close-connection-p :accessor connection-close-p)
     (domain-name :initarg :domain-name :accessor connection-domain-name)
     (host :initarg :host :accessor connection-host)
     (port :initarg :port  :accessor connection-port)
     (state :initform :closed :initarg :state :accessor %connection-state)
     (version :initform nil :initarg :server-version :accessor connection-version)
     (version-confirmed-p :initform nil :initarg :server-version-confirmed-p :accessor connection-version-confirmed-p)
     (http-plist :initform nil :initarg :server-http-plist :accessor connection-http-plist))	;destructive modifications may be retained
  (:documentation "The basic persistent connection infrastrcture for the client side."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CONNECTION.LISP.77")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define allocate-connection (host port &optional domain-name)
  "Allocates a new connection to HOST on PORT with DOMAIN-NAME."
  (declare (values connection new-connection-p))
  (multiple-value-bind (http-version confirmed-p http-plist)
      (server-http-version-on-port host port)
    (let* ((stream (open-http-stream-to-host host port))
	   (conn (allocate-resource 'http-connection host port stream domain-name)))
      (setf (%connection-state conn) :open
	    (connection-close-p conn) nil
	    (connection-timeout conn) *client-persistent-connection-timeout*
	    (connection-requests-allowed conn) *client-persistent-connection-number*
	    (connection-version conn) http-version
	    (connection-version-confirmed-p conn) confirmed-p
	    (connection-http-plist conn) http-plist)
      (atomic-incf *connections-allocated*)
      (client-trace "~&Allocate Connection (~D): ~S" *connections-allocated* conn)
      (values conn t))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CONNECTION.LISP.77")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-generic server-http-version-on-port (host-object port)
  (declare (values http-version confirmed-p http-plist))
  (:documentation "Returns the HTTP version of the server at HOST-OBJECT operating on PORT."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CONNECTION.LISP.77")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(defmethod server-http-version-on-port (host-object port)
  (let ((entry (gethash host-object *server-http-version-table*))
	plist)
    (cond (entry
	   (if (setq plist (getf entry port))
	       (destructuring-bind (version . http-plist) plist
		 (if version
		     (values version t http-plist)
		     (values (second entry) nil http-plist)))
	       (values (second entry) nil)))
	  (t nil))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CONNECTION.LISP.77")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(defmethod note-server-http-property ((connection basic-connection) indicator value)
  (setf (get-host-port-http-server-property (connection-host connection) (connection-port connection) indicator) value))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CONNECTION.LISP.77")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-generic note-server-http-property (connection indicator value)
  (:documentation "Notes the VALUE for INDICATOR as property for the HTTP server associated with CONNECTION."))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CONNECTION.LISP.78")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(defmethod connection-confirmed-version ((connection basic-connection))
  (with-slots (version version-confirmed-p http-plist) connection
    (values version version-confirmed-p http-plist))) 


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CONNECTION.LISP.78")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(defgeneric connection-confirmed-version (connection)
  (declare (values version version-confirmed-p http-plist))
  (:documentation "Returns the HTTP version of connection-or-client and whether this version has been confirmed."))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CONNECTION.LISP.78")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(defmethod note-server-http-version-on-port (host-object http-port http-version)
  (flet ((update-entry (key entry foundp)
	   (declare (ignore key))
	   (cond (foundp
		  (loop for l = entry then more
			for (port plist . more) = l
			when (eql port http-port)
			  do (unless (eq (first plist) http-version)
			       (setf (first plist) http-version))
			     (return entry)
			while more
			finally (setf (cddr l) (list http-port (list http-version)))
				(return entry)))
		 (t (list http-port (list http-version))))))
    (declare (dynamic-extent #'update-entry))
    (check-type http-port integer)
    (check-type http-version keyword)
    (modify-hash *server-http-version-table* host-object #'update-entry)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CLIENT.LISP.272")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod invoke-http-service ((client basic-client-mixin) method header-writer response-handler
				&optional request-entity-generator (http-version *client-http-version*))
  (flet ((trace-request (url method version &aux (trace-stream *trace-output*))
	   (let ((proxy (client-proxy client)))
	     (if proxy
		 (fast-format trace-stream "~&Proxy: ~A (~D) ~%Request: ~A ~I ~A" 
			      (proxy-host proxy) (proxy-port proxy) method (url:write-name url trace-stream) version)
		 (fast-format trace-stream "~&Host: ~A~%Request: ~A ~I ~A"
			      (host-string url) method (url:write-local-name url trace-stream) version)))))
    (with-standard-client-io-syntax ()
      (with-current-connection (client :http-version http-version :connection-var connection)
	(multiple-value-bind (request-version request-version-confirmed-p http-plist)
	    (connection-confirmed-version connection)
	  (let ((url (client-url client))
		(stream (connection-stream connection)))
	    (setf (client-method client) method)
	    ;; send a request to the remote host
	    (send-request stream method url request-version header-writer (client-proxy client))
	    (when *trace-client* (trace-request url method request-version))
	    (when request-entity-generator	;Send the request body when provided
	      (case request-version
		((:http/1.0 :http/0.9)		;1.0 remote server, just send the data.
		 (funcall request-entity-generator client stream request-version)
		 (force-output stream))
		(t (let (100-continue)
		     (multiple-value-bind (reply reply-length)
			 (cond (request-version-confirmed-p
				(ecase (setq 100-continue (getf http-plist :100-continue :unknown))
				  (:implemented
				    (read-reply-line stream (client-reply-line client)))
				  (:unknown
				    (with-timeout (*client-await-continue-reply-timeout* :error-p nil)
				      (read-reply-line stream (client-reply-line client))))
				  (:not-implmented
				    (values nil nil))))
			       (t (with-timeout (*client-await-continue-reply-timeout* :error-p nil)
				      (read-reply-line stream (client-reply-line client)))))
		       (cond (reply
			      (multiple-value-bind (status server-version reason-offset)
				  (parse-reply reply reply-length)
				(unless request-version-confirmed-p
				  (note-server-http-version connection server-version)
				  (setq request-version-confirmed-p t))
				(setf (client-status client) status
				      (client-reason-offset client) reason-offset)
				(with-headers-for-client (client stream request-version)	; handles update of connection version
				  (case status
				    (100
				      (funcall request-entity-generator client stream request-version)
				      (force-output stream)
				      (unless (eq 100-continue :implemented)
					(note-server-http-property connection :100-continue :implemented))
				      (clear-header-set (client-response-headers client) t))	; clear continue headers, if any
				    (417 (signal 'expectation-failed :url url :version server-version))
				    ;; Request was rejected. Handle the rejection response
				    (t (return-from invoke-http-service
					 (multiple-value-prog1	;must return the values returned by the handler
					   (funcall response-handler client stream request-version))))))))
			     (t (funcall request-entity-generator client stream request-version)
				(force-output stream)
				(unless (eq 100-continue :not-implmented)
				  (note-server-http-property connection :100-continue :not-implmented)))))))))
	    (tagbody				;Handle the primary server response
	      read-next-reply
		 (multiple-value-bind (reply reply-length)
		     (read-reply-line stream (client-reply-line client))
		   (multiple-value-bind (status server-version reason-offset)
		       (parse-reply reply reply-length)
		     (unless (and request-version-confirmed-p (eq server-version request-version))	;detect version changes
		       (note-server-http-version connection server-version))
		     (setf (client-status client) status
			   (client-reason-offset client) reason-offset)
		     (with-headers-for-client (client stream request-version)	; handles update of connection version
		       (case status
			 (100 (go read-next-reply))
			 (417 (signal 'expectation-failed :url url :version server-version))
			 (t (return-from invoke-http-service
			      (multiple-value-prog1	;must return the values returned by the handler
				(funcall response-handler client stream request-version)))))))))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CONNECTION.LISP.79")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(add-periodic-task "Client GC Server Version Cache" :daily '(clear-server-http-version-cache))

