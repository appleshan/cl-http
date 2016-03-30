;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.194
;;; Reason: Function URL::%MAKE-CACHE-CONTROL-DIRECTIVE-PLIST: abstract creation of cache control plists
;;; Function URL::MAKE-CACHE-CONTROL-DIRECTIVE-PLIST:  -
;;; Function (CLOS:METHOD URL:INITIALIZE-RESPONSE-CACHE-CONTROL-DIRECTIVES (URL:HTTP-CACHE-CONTROL-MIXIN T)):  update
;;; Function HTTP::%STANDARD-URL-REDIRECT: Use the forward condition for all redirects within a post method per HTTP 1.1 spec
;;; Function (CLOS:METHOD HTTP:REDIRECT-REQUEST (HTTP::BASIC-SERVER-MIXIN URL:URI)):  -
;;; Function (CLOS:METHOD HTTP:REDIRECT-REQUEST (HTTP::BASIC-SERVER-MIXIN CONS)):  -
;;; Function HTTP:PROPERTY-LIST:  -
;;; Function HTTP:WITH-VALUE-CACHED:  -
;;; Function HTTP::REMOVE-VALUES:  -
;;; Function (CLOS:METHOD HTTP::REMOVE-VALUES (HTTP:PROPERTY-LIST-MIXIN T)):  -
;;; Function HTTP::CLEAR-CACHED-VALUES:  new
;;; Function (CLOS:METHOD HTTP::CLEAR-CACHED-VALUES (HTTP:PROPERTY-LIST-MIXIN T)):  implement
;;; Function URL:EXPLICIT-EXPIRATION-P:  -
;;; Function (CLOS:METHOD URL:EXPLICIT-EXPIRATION-P (URL:EXPIRATION-MIXIN)):  -
;;; Written by jcma, 7/11/05 13:57:15
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
;;; HTTP Server 70.193, Showable Procedures 36.3, Binary Tree 34.0,
;;; Experimental W3 Presentation System 8.1, CL-HTTP Server Interface 54.0,
;;; HTTP Proxy Server 6.34, HTTP Client Substrate 4.23,
;;; W4 Constraint-Guide Web Walker 45.13, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.18, DEC OSF/1 V4.0 (Rev. 110),
;;; 1728x1003 24-bit TRUE-COLOR X Screen FUJI:1.0 with 224 Genera fonts (The Olivetti & Oracle Research Laboratory R3323),
;;; Machine serial number -2142637960,
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7),
;;; Get Xauthority pathname from user namespace object. (from W:>jcma>fixes>xauthority-pathname.lisp.2),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing).

;;; Patch file for CL-HTTP version 70.194
;;; Written by jcma, 7/11/05 13:26:52
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
;;; HTTP Server 70.198, Showable Procedures 36.3, Binary Tree 34.0,
;;; Experimental W3 Presentation System 8.1, CL-HTTP Server Interface 54.0,
;;; HTTP Proxy Server 6.34, HTTP Client Substrate 4.23,
;;; W4 Constraint-Guide Web Walker 45.13, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.18, DEC OSF/1 V4.0 (Rev. 110),
;;; 1728x985 24-bit TRUE-COLOR X Screen FUJI:1.0 with 224 Genera fonts (The Olivetti & Oracle Research Laboratory R3323),
;;; Machine serial number -2142637960,
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7),
;;; Get Xauthority pathname from user namespace object. (from W:>jcma>fixes>xauthority-pathname.lisp.2),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing).




(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;URL.LISP.445"
  "HTTP:SERVER;URL.LISP.446"
  "HTTP:SERVER;REPORT.LISP.191"
  "HTTP:SERVER;SERVER.LISP.934"
  "HTTP:SERVER;PLIST.LISP.28")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.445")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

;;;------------------------------------------------------------------- 
;;;
;;; CACHE CONTROL
;;;

(defun %make-cache-control-directive-plist (public private max-age no-cache no-store must-revalidate proxy-revalidate
                                                   no-transform &optional directives)
  (cond-every
    ((and public (not private))
     (setf (getf directives :public) t))
    (private
      (setf (getf directives :private) (if (and (consp no-cache) (every #'keywordp no-cache)) no-cache t)))
    (no-cache
      (setf (getf directives :no-cache) (if (and (consp no-cache) (every #'keywordp no-cache)) no-cache t)))
    ((and no-store (not (eql no-cache t)))	;subsumed by no-cache
     (setf (getf directives :no-store) t))
    (must-revalidate
      (setf (getf directives :must-revalidate) t))
    ((and proxy-revalidate (not must-revalidate))	;subsumed by must-revalidate
     (setf (getf directives :proxy-revalidate) t))
    (no-transform
      (setf (getf directives :no-transform) t))
    (max-age
      (check-type max-age integer)
      (setf (getf directives :max-age) max-age)))
  directives)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.445")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(declaim (inline make-cache-control-directive-plist))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.445")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defun make-cache-control-directive-plist (&key public private no-cache no-store must-revalidate proxy-revalidate
                                                no-transform max-age directives)
  (%make-cache-control-directive-plist public private max-age no-cache no-store
                                       must-revalidate proxy-revalidate no-transform directives))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.445")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod initialize-response-cache-control-directives ((url http-cache-control-mixin) args)
  (with-slots (directives) url
    (destructuring-bind (&key public private no-cache no-store must-revalidate proxy-revalidate
			      no-transform max-age &allow-other-keys) args
      (remove-value url :proxy-revalidate-directives)	; remove any cached proxy directives see RESPONSE-CACHE-CONTROL-DIRECTIVES
      (setf directives (%make-cache-control-directive-plist public private max-age no-cache no-store
                                                            must-revalidate proxy-revalidate no-transform))
      url)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.446")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define-generic explicit-expiration-p (URL)
  (:documentation "Returns non-null if URL specifies explicit expiration information."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.446")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod explicit-expiration-p ((expiration-mixin expiration-mixin))
  (with-slots (expiration-function max-age-function) expiration-mixin
    (not (null (or expiration-function max-age-function)))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (export (intern "EXPLICIT-EXPIRATION-P" :url) :url))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;REPORT.LISP.191")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(defun %standard-url-redirect (url target-url redirection-type method target-window)
  (check-type target-window (or null string))
  (let ((alternate-urls (http::ensure-list target-url)))
    (unless alternate-urls
      (signal 'document-not-found :url url))
    (ecase redirection-type
      (:temporary-redirect
	(case method
	  (:get (signal 'document-found :url url :method method :new-urls alternate-urls :target-window target-window))
	  (t (signal 'document-moved-temporarily :url url :method method :new-urls alternate-urls :target-window target-window))))
      (:permanent-redirect
	(signal 'document-moved-permanently :url url :method method :new-urls alternate-urls :target-window target-window))
      (:forward
	(if (case method
	      (:get (not (explicit-expiration-p url)))
	      (:post t))
	    (signal 'document-forwarded :url url :method method :new-urls alternate-urls :target-window target-window)
	    (signal 'document-moved-temporarily :url url :method method :new-urls alternate-urls :target-window target-window)))
      (:proxy
	(signal 'document-access-via-proxy :url url :method method :new-urls alternate-urls :target-window target-window))
      (:multiple-choices
	(signal 'document-multiple-choices :url url :method method :new-urls alternate-urls :target-window target-window)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.934")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod redirect-request ((server basic-server-mixin) (target-uri uri) &optional target-window)
  (with-slots (url method) server
    (let ((redirection (case method (:post :forward) (t :temporary-redirect))))
      (%standard-url-redirect url target-uri redirection method target-window))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.934")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod redirect-request ((server basic-server-mixin) (target-uris cons) &optional target-window)
  (with-slots (url method) server
    (let ((redirection (case method (:post :forward) (t :multiple-choice))))
      (if (every #'url::uri-p target-uris)
          (%standard-url-redirect url target-uris redirection method target-window)
	(let ((alternate-uris (mapcar #'(lambda (uri) (intern-url uri :if-does-not-exist :create)) target-uris)))
	  (declare (dynamic-extent alternate-uris))
	  (%standard-url-redirect url alternate-uris redirection method target-window))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;PLIST.LISP.28")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(define-generic property-list  (property-list-mixin)
  (:documentation "Returns the property list for PROPERTY-LIST-MIXIN."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;PLIST.LISP.28")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(define-macro with-value-cached ((property-list-mixin key &key (recompute-p nil recompute-supplied-p)) &body value-form)
  "Caches the value returned by value-form on PROPERTY-LIST-MIXIN's property list under the indicator KEY.
When RECOMPUTE-P is non-null, the value is recomputed and recached.
The returned values are VALUE and RETRIEVE-FROM-CACHE-P.
Remove a series of cached values with CLEAR-CACHED-VALUES."
  (declare (values cached-value retrieved-from-cache-p))
  (let ((form `(let ((val (getf plist ,key :+not-found+)))
                 (case val
                   (:+not-found+ 
                     (setf (getf plist ,key) (progn  . ,value-form)))
                   (t (values val t))))))      
    (cond (recompute-supplied-p
           `(with-slots (plist) ,property-list-mixin
              (cond (,recompute-p
                     (setf (getf plist ,key) (progn . ,value-form)))
                    (t ,form))))
          (t `(with-slots (plist),property-list-mixin ,form)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;PLIST.LISP.28")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(define-generic remove-values  (property-list-mixin indicators)
  (:documentation "Removes the values for all INDICATORS from PROPERTY-LIST-MIXIN."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;PLIST.LISP.28")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defmethod remove-values ((plist  property-list-mixin) indicators)
  (with-slots (plist) plist
    (loop with properties = plist
          and removed-p
          for key in indicators
          do (when (remf properties key)
               (unless removed-p
                 (setq removed-p t)))
          finally (return (if removed-p
                              (setf plist properties)
			      properties)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;PLIST.LISP.28")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(define-generic clear-cached-values  (property-list-mixin cache-keys)
  (:documentation "Clears the values for each key in CACHE-KEYS from PROPERTY-LIST-MIXIN."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;PLIST.LISP.28")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-")

(defmethod clear-cached-values ((plist property-list-mixin) cache-keys)
  (remove-values plist cache-keys))
