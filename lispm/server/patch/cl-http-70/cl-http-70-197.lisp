;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.197
;;; Reason: CLOS class URL::HTTPS-URL: fix default port to be 443
;;; CLOS class URL::RTSP-URL:  new.
;;; CLOS class URL::RTSP-DIRECTORY:  -
;;; CLOS class URL::RTSP-PATHNAME:  -
;;; Export "HTTPS-URL" and "RTSP-URL"
;;; Function (CLOS:METHOD URL:CANONICALIZE-URL ((EQL :RTSP) T)):  -
;;; DEFINE-SCHEME-PARSER URL::RTSP:  define.
;;; Function HTTP::%EXPORT-COMPUTED-URL: Fix destructuring bind error on export of
;;; header functions for http-form, http-computed-form, http-computed-url, and
;;; http-client-script. These url classe will now hanlde the header-function
;;; correctly.
;;; Function (CLOS:METHOD HTTP:EXPORT-URL (URL:HTTP-OBJECT (EQL :HTML-FORM))):  -
;;; Function HTTP::%EXPORT-COMPUTED-FORM:  -
;;; Function (CLOS:METHOD HTTP:EXPORT-URL (URL:HTTP-MINIMUM-OBJECT (EQL :SCRIPT))):  -
;;; Function (CLOS:METHOD URL:INITIALIZE-SPECIALIZATION (URL:HTTP-FORM T T)):  -
;;; Function (CLOS:METHOD URL:INITIALIZE-SPECIALIZATION (URL:HTTP-COMPUTED-URL T T)):  -
;;; Function (CLOS:METHOD URL:INITIALIZE-SPECIALIZATION (URL:HTTP-COMPUTED-FORM T T)):  -
;;; Function (CLOS:METHOD URL:INITIALIZE-SPECIALIZATION (URL:HTTP-CLIENT-SCRIPT T T)):  -
;;; Written by jcma, 7/11/05 12:38:37
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
;;; HTTP Server 70.196, Showable Procedures 36.3, Binary Tree 34.0,
;;; Experimental W3 Presentation System 8.1, CL-HTTP Server Interface 54.0,
;;; HTTP Proxy Server 6.34, HTTP Client Substrate 4.23,
;;; W4 Constraint-Guide Web Walker 45.13, HTTP Client 51.8, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.18, DEC OSF/1 V4.0 (Rev. 110),
;;; 1728x1003 24-bit TRUE-COLOR X Screen FUJI:1.0 with 224 Genera fonts (The Olivetti & Oracle Research Laboratory R3323),
;;; Machine serial number -2142637960,
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7),
;;; Get Xauthority pathname from user namespace object. (from W:>jcma>fixes>xauthority-pathname.lisp.2),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;URL-CLASS.LISP.24"
  "HTTP:SERVER;PACKAGE.LISP.488"
  "HTTP:SERVER;SERVER.LISP.936"
  "HTTP:SERVER;URL.LISP.446")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL-CLASS.LISP.24")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: URL; Base: 10 -*-")

(defclass https-url
	  (translation-method-mixin
	    host-port-mixin
	    http-cache-control-mixin
	    expiration-mixin
	    secure-subnets-mixin
	    authentication-mixin
	    content-language-mixin
	    url)
    ((scheme :initform "https" :reader scheme :allocation :class)
     (protocol :initform :https :reader protocol :allocation :class)
     (standard-port :initform 443 :reader standard-port :allocation :class))
  (:documentation "Root class of Secure HTTP URLs."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL-CLASS.LISP.24")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: URL; Base: 10 -*-")

;;;------------------------------------------------------------------- 
;;;
;;; RTSP URL (Real Time Streaming Protocol)
;;;

(defclass rtsp-url
	  (url user-id-and-pw-mixin)
  ((scheme :initform "rtsp" :reader scheme :allocation :class)
   (protocol :initform :rtsp :reader protocol :allocation :class)
   (standard-port :initform 554 :reader standard-port :allocation :class))
  (:documentation "Real Time Streaming Protocol URL class."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL-CLASS.LISP.24")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: URL; Base: 10 -*-")

(defclass rtsp-directory
          (path-mixin host-port-mixin rtsp-url)
  ())


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL-CLASS.LISP.24")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: URL; Base: 10 -*-")

(defclass rtsp-pathname
          (object-mixin path-mixin host-port-mixin rtsp-url)
  ())


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;PACKAGE.LISP.488")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: CL-USER; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(eval-when (:load-toplevel)
(mapc #'(lambda (x) (export (intern x :url) :url))
      '("HTTPS-URL" "RTSP-URL")))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.446")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod canonicalize-url ((scheme (eql :rtsp)) url-string &optional (start 0) (end (length url-string)) destructive-p)
  (%canonicalize-host-prefixed-url "rtsp" 4 url-string start end destructive-p))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.446")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

;;;------------------------------------------------------------------- 
;;;
;;; RTSP URL PARSER
;;;

;; cloned from the FTP URL parser
(define-scheme-parser
    rtsp
    (:classes (rtsp-url))
    (url start end)
  (multiple-value-bind (user-id pw host-index host-end)
      (get-user-id-and-password url start end)
    (multiple-value-bind (host-string port path-index)
        (get-host-port-info url host-index host-end t)
      ;; extract the path components
      (multiple-value-bind (path object-index next-index)
          (get-path-info url path-index end)
        ;; The spec requires a path. Should we signal an error when there
        ;; is no path?  create the appropriate URL
        (cond ;; get the object components when present         
              (object-index
               (multiple-value-bind (object extension)
                   (get-object-info url object-index next-index)
                 (make-instance 'rtsp-pathname
                                :name-string (when *retain-original-name-string* (subseq url start end))
                                :host-string host-string
                                :port port
                                :user-id user-id
                                :password pw
                                :path path
                                :object object
                                :extension extension)))
              (t (make-instance 'rtsp-directory
                                :name-string (when *retain-original-name-string* (subseq url start end))
                                :host-string host-string
                                :port port
                                :user-id user-id
                                :password pw
                                :path path)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.936")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun %export-computed-url (url translation args)
  (destructuring-bind (&key response-function header-function pathname character-set &allow-other-keys) args
    (when pathname
      (setf (translated-pathname url) pathname))
    (unless (and response-function (good-response-function-p response-function))
      (error "RESPONSE-FUNCTION, ~S, must be a defined function when exporting the URL, ~S, with translation, ~S"
             response-function url translation))
    (setf (translation-method url) translation
          (character-set url) character-set)
    (let ((init-args `(,response-function ,header-function)))
      (declare (dynamic-extent init-args))
      (url:initialize-specialization url 'url:http-computed-url init-args))
    url))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.936")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod  export-url ((url url:http-object) (translation (eql :html-form)) &rest args)
  (destructuring-bind (&key response-function header-function pathname server character-set
			    (durable-form-values nil durable-form-values-p-supplied)
			    (maximum-upload-file-size nil maximum-upload-file-size-supplied-p)
			    &allow-other-keys) args
    (cond (pathname
           (setf (translated-pathname  url) pathname))
          (t (error "No PATHNAME was provided while exporting the URL, ~S, with translation, ~S"
                    url translation)))
    (unless (and response-function (good-response-function-p response-function))
      (error "RESPONSE-FUNCTION, ~S, must be a defined function when exporting the URL, ~S, with translation, ~S"
             response-function url translation))
    (setf (translation-method url) translation
          (character-set url) character-set)
    (if durable-form-values-p-supplied
	(setf (get-value url :durable-form-values-p) durable-form-values)
	(remove-value url :durable-form-values-p))
    (if maximum-upload-file-size-supplied-p
	(setf (get-value url :maximum-upload-file-size) maximum-upload-file-size)
	(remove-value url :maximum-upload-file-size))
    (let ((init-args `(,server ,response-function ,header-function)))
      (declare (dynamic-extent init-args))
      (url:initialize-specialization url 'url:http-form init-args))
    url))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.936")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun %export-computed-form (url translation args)
  (destructuring-bind (&key form-function response-function header-function
			    (durable-form-values nil durable-form-values-p-supplied)
			    (maximum-upload-file-size nil maximum-upload-file-size-supplied-p)
			    &allow-other-keys) args
    (unless (and response-function (good-response-function-p response-function))
      (error "RESPONSE-FUNCTION, ~S, must be a defined function when exporting the URL, ~S, with translation, ~S"
             response-function url translation))
    (unless (and form-function (good-response-function-p form-function))
      (error "FORM-FUNCTION, ~S, must be a defined function when exporting the URL, ~S, with translation, ~S"
             form-function url translation))
    (setf (translation-method url) translation)
    (if durable-form-values-p-supplied
	(setf (get-value url :durable-form-values-p) durable-form-values)
	(remove-value url :durable-form-values-p))
    (if maximum-upload-file-size-supplied-p
	(setf (get-value url :maximum-upload-file-size) maximum-upload-file-size)
	(remove-value url :maximum-upload-file-size))
    (let ((init-args `(,form-function ,response-function ,header-function)))
      (declare (dynamic-extent init-args))
      (url:initialize-specialization url 'url:http-computed-form init-args))
    url))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.936")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod export-url ((url url:http-minimum-object) (translation (eql :script)) &rest args)
  (destructuring-bind (&key script header-function &allow-other-keys) args
    (unless script
      (error "No script was provided while exporting the URL, ~S, with EXPORT-TYPE, ~S"
             url translation))
    (setf (translation-method url) translation)
    (let ((args `(,script ,header-function)))
      (declare (dynamic-extent args))
      (url:initialize-specialization url 'url:http-client-script args))
    url))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.446")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod initialize-specialization ((url http-form) class init-args)
  (with-class-change-for-initialize-specialization (url class init-args)
    (destructuring-bind (server response-function header-function) init-args
      (setf (form-server url) server
            (response-function url) response-function
            (header-function url) header-function)
      url)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.446")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod initialize-specialization ((url http-computed-url) class init-args)
  (with-class-change-for-initialize-specialization (url class init-args)
    (destructuring-bind (response-function header-function) init-args
      (setf (response-function url) response-function
            (header-function url) header-function)
      url)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.446")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod initialize-specialization ((url http-computed-form) class init-args)
  (with-class-change-for-initialize-specialization (url class init-args)
    (destructuring-bind (form-function response-function header-function) init-args
      (setf (form-function url) form-function
            (response-function url) response-function
            (header-function url) header-function)
      url)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.446")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod initialize-specialization ((url http-client-script) class init-args)
  (with-class-change-for-initialize-specialization (url class init-args)
    (destructuring-bind (script header-function) init-args
      (setf (script url) script
            (header-function url) header-function)
      url)))

