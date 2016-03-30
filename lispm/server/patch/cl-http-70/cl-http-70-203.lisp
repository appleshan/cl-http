;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for CL-HTTP version 70.203
;;; Reason: Function HTTP:USER-AGENT-CAPABILITY-P:  fix bug.
;;; Function HTML::VERTICAL-ALIGNMENT-VALUE:  fix format string.
;;; Function NETSCAPE4.0::%WRITE-LAYER-ARGUMENTS:  fix error args.
;;; Function (CLOS:METHOD HTTP::GET-SHTML-OPERATION ((EQL :EVAL) T)):  fix error args.
;;; Function HTTP:INTERN-ACCESS-LOG:  fix bug.
;;; Function HTTP::GET-CGI-VARIABLE-BINDING:  fix error format string.
;;; Function HTTP::GET-SERVER-INTERFACE-VARIABLE-BINDING:  ditto.
;;; Written by jcma, 8/20/05 22:02:18
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/Plus-CL-HTTP-A-CSAIL-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, Metering 444.0,
;;; Metering Substrate 444.1, Conversion Tools 436.0, Hacks 440.0, CLIM 72.0,
;;; Genera CLIM 72.0, CLX CLIM 72.0, PostScript CLIM 72.0, CLIM Demo 72.0,
;;; CLIM Documentation 72.0, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.40, Genera 8 5 Macivory Support Patches 1.0,
;;; Genera 8 5 Metering Patches 1.0, Genera 8 5 Joshua Patches 1.0,
;;; Genera 8 5 Jericho Patches 1.0, Genera 8 5 Joshua Doc Patches 1.0,
;;; Genera 8 5 Joshua Metering Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.3,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clx Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Clim Demo Patches 1.0, Genera 8 5 Color Patches 1.1,
;;; Genera 8 5 Images Patches 1.0, Genera 8 5 Color Demo Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; Genera 8 5 Concordia Patches 1.2, Genera 8 5 Concordia Doc Patches 1.0,
;;; Genera 8 5 C Patches 1.0, Genera 8 5 Pascal Patches 1.0,
;;; Genera 8 5 Fortran Patches 1.0, MAC 415.2, MacIvory Support 447.0,
;;; MacIvory Development 434.0, Color 427.1, Graphics Support 431.0,
;;; Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Color System Documentation 10.0, SGD Book Design 10.0, Color Demo 422.0,
;;; Images 431.2, Image Substrate 440.4, Statice Runtime 466.1, Statice 466.0,
;;; Statice Browser 466.0, Statice Server 466.2, Statice Documentation 426.0,
;;; Symbolics Concordia 444.0, Graphic Editor 440.0, Graphic Editing 441.0,
;;; Bitmap Editor 441.0, Graphic Editing Documentation 432.0, Postscript 436.0,
;;; Concordia Documentation 432.0, Joshua 237.6, Joshua Documentation 216.0,
;;; Joshua Metering 206.0, Jericho 237.0, C 440.0, Lexer Runtime 438.0,
;;; Lexer Package 438.0, Minimal Lexer Runtime 439.0, Lalr 1 434.0,
;;; Context Free Grammar 439.0, Context Free Grammar Package 439.0, C Runtime 438.0,
;;; Compiler Tools Package 434.0, Compiler Tools Runtime 434.0, C Packages 436.0,
;;; Syntax Editor Runtime 434.0, C Library Headers 434,
;;; Compiler Tools Development 435.0, Compiler Tools Debugger 434.0,
;;; C Documentation 426.0, Syntax Editor Support 434.0, LL-1 support system 438.0,
;;; Fortran 434.0, Fortran Runtime 434.0, Fortran Package 434.0, Fortran Doc 427.0,
;;; Pascal 433.0, Pascal Runtime 434.0, Pascal Package 434.0, Pascal Doc 427.0,
;;; HTTP Server 70.202, Showable Procedures 36.3, Binary Tree 34.0,
;;; Experimental W3 Presentation System 8.1, CL-HTTP Server Interface 54.0,
;;; HTTP Proxy Server 6.34, HTTP Client Substrate 4.23,
;;; W4 Constraint-Guide Web Walker 45.13, HTTP Client 51.9, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.18, DEC OSF/1 V4.0 (Rev. 110),
;;; 1728x985 24-bit TRUE-COLOR X Screen FUJI:1.0 with 224 Genera fonts (The Olivetti & Oracle Research Laboratory R3323),
;;; Machine serial number -2142637960,
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7),
;;; Get Xauthority pathname from user namespace object. (from W:>jcma>fixes>xauthority-pathname.lisp.2),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;UTILS.LISP.554"
  "HTTP:SERVER;HTML-3-2.LISP.40"
  "HTTP:SERVER;NETSCAPE-4-0.LISP.16"
  "HTTP:SERVER;SHTML.LISP.26"
  "HTTP:SERVER;LOG.LISP.224"
  "HTTP:SERVER;CGI.LISP.53")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.554")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(define user-agent-capability-p (capability user-agent version)
  #.(format nil "Returns non-null when VERSION of USER-AGENT has CAPABILITY. Existing capabilities are: ~S."
            (mapcar #'car *user-agent-capabilities*))
  (flet ((%user-agent-capability-p (capability user-agent version)
           (let ((entry (assoc capability *user-agent-capabilities* :test #'eq))
                 ua-entry)
             (cond (entry
                    (and (setq ua-entry (assoc user-agent (cdr entry) :test #'eq))
                         (or (null (cdr ua-entry))      ;no versions means all versions
                             (not (null (member version (cdr ua-entry) :test #'eq))))))
                   (t (error "~S is not one of the known capabilities, ~S, for user agents."
                             capability (mapcar #'car *user-agent-capabilities*)))))))
    (declare (inline %user-agent-capability-p))
    (typecase capability
      (keyword
        (%user-agent-capability-p capability user-agent version))
      (cons
        (loop for item in capability
              do (unless (%user-agent-capability-p item user-agent version)
                   (return-from user-agent-capability-p nil))
              finally (return-from user-agent-capability-p t))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML-3-2.LISP.40")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: html3.2; -*-")

(defun vertical-alignment-value (alignment)
  (unless (member alignment *vertical-alignment-values*)
    (error "~S is not one of the possible vertical alignments, ~S" alignment *vertical-alignment-values*))
  (symbol-name alignment))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;NETSCAPE-4-0.LISP.16")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: (netscape4.0 :use (future-common-lisp ns3.0 www-utils url)); BASE: 10; Syntax: ansi-common-lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun %write-layer-arguments (stream name reference visibility background background-url
                                      x-page-origin y-page-origin x-origin y-origin width height z-index parent
                                      clip-x1 clip-y1 clip-x2 clip-y2 events)
  (cond-every
    (name (%write-command-key-arg stream "NAME" name))
    (x-page-origin (%write-pixel-or-percent-argument stream "PAGEX" x-page-origin))
    (y-page-origin (%write-pixel-or-percent-argument stream "PAGEY" y-page-origin))
    (x-origin (%write-pixel-or-percent-argument stream "LEFT" x-origin))
    (y-origin (%write-pixel-or-percent-argument stream "TOP" y-origin))
    (background
      (%write-command-key-arg stream "BGCOLOR" (color-mapping background t)))
    (background-url
      (%write-command-key-arg stream "BACKGROUND" (url:coerce-url-string background-url)))
    (reference
      (%write-command-key-arg stream "SRC" (url:coerce-url-string reference t)))
    (width (%write-command-key-arg stream "WIDTH" width t))
    (height (%write-command-key-arg stream "height" height t))
    (z-index
      (typecase z-index
        (integer (%write-command-key-arg stream "Z-INDEX" z-index t))
        (t (check-type parent string)
           (unless parent
             (error "When z-index is ~S, PARENT must be specified." z-index))
           (%write-command-key-arg stream
                                   (ecase z-index
                                     (:above "ABOVE")
                                     (:below "BELOW"))
                                   parent))))
    (visibility
      (unless (member visibility '(:show :hide :inherit))
        (error "visibility is ~S, which is not one of :SHOW, :HIDE or :INHERIT." visibility))
      (%write-command-key-arg stream "VISIBILITY" visibility))
    ((or clip-x2 clip-y2)
     (cond ((and clip-x2 clip-y2 clip-x1 clip-y1)
            (multiple-value-bind (x1 y1 x2 y2)
                (%make-layer-clip-args clip-x1 clip-y1 clip-x2 clip-y2)
              (declare (dynamic-extent x1 y1 x2 y2))
              (fast-format stream " CLIP=~D,~D,~D,~D" x1 y1 x2 y2)))
           (t (error "WITH-LAYER: Incomplete arguments for clipping box:~
                      CLIP-X1: ~S~&CLIP-Y1: ~S~&CLIP-X2: ~S~&CLIP-Y2:~S"
                     clip-x1 clip-y1 clip-x2 clip-y2))))
    (events
      (dolist (event events)
        (html2::%write-input-type-event-arg stream event)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SHTML.LISP.26")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defmethod get-shtml-operation ((method (eql :eval)) parameter-plist)
  (let ((action (getf parameter-plist :action)))
    (cond (action
           (values (validate-shtml-action action 0 (length action))
                   parameter-plist))
          (t (error "SHTML ~S element contains no action." parameter-plist)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.224")
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
           (and (eq port (log-port x))
		(equalp name (log-name x))
                (typep x class)))
	 (handle-does-not-exist (name port class host)
	   (ecase if-does-not-exist
	     (:soft nil)
	     (:create
	       (let* ((*standard-log-directory* directory)
		      (log (allocate-log :name name :port port :class class :local-host (or host (local-host)))))
		 (when port (add-access-log log port))
		 (values log t)))
	     (:error (error "Unknown HTTP server access log, ~A~:[~;*, for port ~D~]." name port)))))
    (declare (dynamic-extent #'equal-log-p))
    (etypecase name
      (string
        (cond
          ((find-if #'equal-log-p *all-logs*))
          (t (handle-does-not-exist name port class host))))
      (basic-log-mixin
	(if (and (or (null port) (eql port (log-port name)))
		 (typep name class))
	    name
	    (handle-does-not-exist (log-name name) port class host))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CGI.LISP.53")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun get-cgi-variable-binding (variable &aux entry)
  (flet ((get-key (var)
           (symbolize (symbol-name var) *keyword-package*)))
    (declare (dynamic-extent #'get-key))
    (cond ((setq entry (assoc (get-key variable) *cgi-variable-bindings* :test #'eq))
           (second entry))
          ((get-cgi-header-binding variable))
          (t (error "~S is not one of the known CGI variables, ~{~A~^, ~}."
                    variable (mapcar #'car *cgi-variable-bindings*))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;CGI.LISP.53")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun get-server-interface-variable-binding (variable &optional (si-version *server-interface-version*) &aux entry)
  (flet ((get-key (var)
           (symbolize (symbol-name var) *keyword-package*)))
    (declare (dynamic-extent #'get-key))
    (let ((alist (server-interface-alist si-version t)))
      (cond ((setq entry (assoc (get-key variable) alist :test #'eq))
             (second entry))
            ((get-server-interface-header-binding variable))
            (t (error "~S is not one of the known Server Interface (~A) variables, ~{~A~^, ~}."
                      variable (symbol-name si-version) (mapcar #'car alist)))))))

