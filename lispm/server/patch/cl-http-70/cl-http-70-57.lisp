;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: url; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.57
;;; Reason: Provide URI universes.
;;; Written by JCMa, 7/17/00 15:11:25
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.56,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42, HTTP Client Substrate 3.13,
;;; HTTP Proxy Server 5.16, HTTP Client 49.8, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, W4 Constraint-Guide Web Walker 44.0,
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
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).




(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;URL-CLASS.LISP.18"
  "HTTP:SERVER;URL.LISP.400"
  "HTTP:SERVER;URL.LISP.401")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL-CLASS.LISP.18")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: URL; Base: 10 -*-")

(defstruct (uri-universe (:print-function print-uri-universe))
  (table (make-hash-table :test #'equal) :read-only t)
  (lock (make-lock "URI Lock" :type :multiple-reader-single-writer) :read-only t)
  (name "No Name" :type string))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.400")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defun print-uri-universe (uri-universe stream depth)
  (declare (ignore depth))
  (print-unreadable-object (uri-universe stream :type t :identity t)
    (when (uri-universe-name uri-universe)
      (write (uri-universe-name uri-universe) :stream stream :escape nil))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.400")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defvar *default-uri-table-size* 700
  "The default size for URL/URI tables.")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.400")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defun create-uri-universe (name &optional (size *default-uri-table-size*))
  (check-type name string)
  ;; Case-sensitive for compatibility with the universe.
  (make-uri-universe :name name :table (make-hash-table :test #'equal :size size)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.400")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defvar *uri-universe* nil
  "Holds the current URI Universe.")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.400")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defun initialize-standard-uri-universe (&optional redo-p)
  (cond ((or redo-p (null *uri-universe*))
	 (setq *uri-universe* (create-uri-universe "Standard")))
	(t *uri-universe*)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.400")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

;; revover any existing URLs when the patch is loaded
(cond ((and (boundp '*url-table*)
	    *url-table*
	    (< 0 (hash-table-count *url-table*)))
       (setq *uri-universe* (make-uri-universe :name "Standard" :table *url-table*)
	     *url-table* nil))
      (t (initialize-standard-uri-universe)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.400")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

;; Obsolete method
;;(defvar *url-table* (make-hash-table :test #'equal))

(defmacro with-uri-universe ((uri-universe) &body body)
  "Binds the URL context to use uri-universe as the repository of URLs."
  `(let ((*uri-universe* ,uri-universe))
     ,@body))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.400")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defun get-url (url-string)
  (gethash url-string (uri-universe-table *uri-universe*)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.400")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defsetf get-url (url-string) (url)
  `(setf (gethash ,url-string (uri-universe-table *uri-universe*)) ,url))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.400")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define clear-url-table ()
  "Clears all URLs, making any existing objects available for GC."
  (with-lock-held ((uri-universe-lock *uri-universe*) :write "Clear URI Context")
    (clrhash (uri-universe-table *uri-universe*))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.400")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define map-url-table (function)
  "Maps FUNCTION over all interned URLs.
FUNCTION is called with url-string url-object."
  (maphash function (uri-universe-table *uri-universe*)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.400")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod url-interned-p ((url url))
  (eq url (get-url (name-string url))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.400")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod register ((url url) &optional return-existing-interned-url-p)
  (declare (values registered-url))
  (let* ((name-string (name-string url))
         (interned-url (get-url name-string)))
    (with-lock-held ((uri-universe-lock *uri-universe*) :write "Register URI")
      (cond ((setq interned-url (get-url name-string))
	     (when (or return-existing-interned-url-p (eq url interned-url))
	       ;; return the existing URL
	       (return-from register interned-url)))
	    ;; return the newly created URL
	    (t (setf (get-url name-string) url)
	       (return-from register url))))
    ;; otherwise signal the collision
    (error 'interned-url-already-using-name :interned-url interned-url :uninterned-url url)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.400")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define remap-url-host (old-host new-host &key (old-port http:*standard-http-port*) (new-port nil new-port-supplied-p)
                                 (report-stream *standard-output*))
  "Remaps all known URLs from OLD-HOST OLD-PORT to NEW-HOST NEW-PORT.
OLD-PORT defaults to the standard HTTP port. NEW-PORT defaults to OLD-PORT.
However, if NEW-PORT is NIL, the current port value is preserved."
  (let ((n-port (if new-port-supplied-p
                    new-port
                    (or old-port http:*standard-http-port*))))
    (format report-stream "~&Remapping URLs from ~A:~:[*~;~:*~D~] to ~A:~:[*~;~:*~D~] . . . ."
            old-host old-port new-host n-port)
    (with-lock-held ((uri-universe-lock *uri-universe*) :write "Remap URL Host")
      (loop for url being each hash-value in (uri-universe-table *uri-universe*)
	    when (host-match-p url old-host old-port)
	      do (change-host url new-host (or n-port (port url)))
	      and sum 1 into count
	    finally (format report-stream "~&~D URLs remapped from ~A:~:[*~;~:*~D~] to ~A:~:[*~;~:*~D~]."
			    count old-host old-port new-host n-port)
		    (return *uri-universe*)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.400")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(PROGN
#-(or ANSI-CL Draft-ANSI-CL-2)
(defmethod make-load-form ((url url))
  (let ((url-string (name-string url)))
    `(intern-url ,url-string :if-does-not-exist ,(if (eq url (get-url url-string)) :create :uninterned))))

)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.400")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defmethod unregister ((url url))
  (let* ((uri-universe *uri-universe*)
	 (table uri-universe)
	 (url-string (name-string url)))
    (with-lock-held ((uri-universe-lock uri-universe) :write "Clear URI Context")
      (remhash url-string table))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.400")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(mapc #'(lambda (x) (export (intern x :url) :url)) '("WITH-URI-UNIVERSE" "URI-UNIVERSE" "*DEFAULT-URI-TABLE-SIZE*"))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.401")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(PROGN
(defun %make-uri-universe (resource name &optional (size *default-uri-table-size*))
  (declare (ignore resource))
  (create-uri-universe name size))

(defun %initialize-uri-universe (resource uri-universe name &optional (size *default-uri-table-size*))
  (declare (ignore resource size))
  (setf (uri-universe-name uri-universe) name)
  uri-universe)

(defun %deinitialize-uri-universe (resource uri-universe)
  (declare (ignore resource))
  (clrhash (uri-universe-table uri-universe))
  (setf (uri-universe-name uri-universe) "Unallocated")
  uri-universe)

(defun %match-uri-universe-p (resource uri-universe name &optional (size *default-uri-table-size*))
  (declare (ignore resource name))
  (< size (hash-table-size (uri-universe-table uri-universe)) (* size 1.5)))

(defresource uri-universe (name &optional (size *default-uri-table-size*))
  :matcher %match-uri-universe-p
  :constructor %make-uri-universe
  :initializer %initialize-uri-universe
  :deinitializer %deinitialize-uri-universe)

(define clear-uri-universe-resource ()
  (clear-resource 'uri-universe))


)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.401")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(PROGN
(eval-when (:compile-toplevel :execute :load-toplevel)
(defmacro define-http-scheme-parser (scheme url-class object-class path-class search-class searchable-object-class)
  `(define-scheme-parser
     ,scheme
     (:classes (,url-class))
     (url start end)
     (let (object extension search-keys search-parent)
       ;; extract the host parameters
       (multiple-value-bind (host-string port path-index)
	   (get-host-port-info url start end)
	 ;; extract the path components
	 (multiple-value-bind (path object-index next-index search-p)
	     (get-path-info url path-index end)
	   ;; get the object components when present
	   (when object-index
	     (multiple-value-setq (object extension)
	       (get-object-info url object-index next-index)))
	   ;; get the search keys where necessary
	   (when search-p
	     (let ((s-suffix (1+ (the fixnum next-index))))
	       (unless (= s-suffix end)
		 (setq search-parent (intern-url url :start start :end s-suffix)
		       search-keys (funcall (search-parser search-parent) url s-suffix end)))))
	   ;; create the appropriate URL
	   (cond
	     (search-p
	      (let ((object (if extension
				;; searchable object (used for searchable images)
				(make-instance ',searchable-object-class
					       :name-string (when *retain-original-name-string* (subseq url start end))
					       :host-string host-string
					       :port port
					       :path path
					       :object object
					       :extension extension
					       :search-keys search-keys
					       :search-parent search-parent)
				;; regular search urls
				(make-instance ',search-class
					       :name-string (when *retain-original-name-string* (subseq url start end))
					       :host-string host-string
					       :port port
					       :path path
					       :object object
					       :search-keys search-keys
					       :search-parent search-parent))))
		;; inherit the parent's security properties on creation
		(if search-keys
		    (inherit-parent-access-controls object)
		    ;; set the search parent so we know we're the root.
		    (setf (%search-parent object) object))
		object))
	     (object
	      (make-instance ',object-class
			     :name-string (when *retain-original-name-string* (subseq url start end))
			     :host-string host-string
			     :port port
			     :path path
			     :object object
			     :extension extension))
	     (t (make-instance ',path-class
			       :name-string (when *retain-original-name-string* (subseq url start end))
			       :host-string host-string
			       :port port
			       :path path))))))))
)						;close eval when

(define-http-scheme-parser http http-url http-object http-path http-search http-searchable-object)

(define-http-scheme-parser https https-url https-object https-path https-search https-searchable-object)

(initialize-http-scheme-parser)
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.401")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define-scheme-parser
  ftp
  (:classes (ftp-url))
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
             (make-instance 'ftp-pathname
                            :name-string (when *retain-original-name-string* (subseq url start end))
                            :host-string host-string
                            :port port
                            :user-id user-id
                            :password pw
                            :path path
                            :object object
                            :extension extension)))
          (t (make-instance 'ftp-directory
                            :name-string (when *retain-original-name-string* (subseq url start end))
                            :host-string host-string
                            :port port
                            :user-id user-id
                            :password pw
                            :path path)))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.401")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define-scheme-parser
  news
  (:classes (news-url))
  (url start end)
  (multiple-value-bind (group message-id)
      (get-news-info url start end)
    (cond (group
           (make-instance 'news-group
                          :name-string (when *retain-original-name-string* (subseq url start end))
                          :group group))
          (t (make-instance 'news-article
                            :name-string (when *retain-original-name-string* (subseq url start end))
                            :message-id message-id)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.401")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define-scheme-parser
  gopher
  (:classes (gopher-url))
  (url start end)
  (let (object extension search-keys search-parent)
    ;; extract the host parameters
    (multiple-value-bind (host-string port path-index)
        (get-host-port-info url start end)
      ;; extract the path components
      (multiple-value-bind (path object-index next-index search-p)
          (get-path-info url path-index end)
        ;; get the object components when present
        (when object-index
          (multiple-value-setq (object extension)
            (get-object-info url object-index next-index)))
        ;; get the search keys where necessary
        (when search-p
          (let ((s-suffix (1+ (the fixnum next-index))))
            (unless (= s-suffix end)
              (setq search-parent (intern-url url :start (- (the fixnum start) 7) :end s-suffix)
                    search-keys (parse-search-info url s-suffix end)))))
        ;; create the appropriate URL
        (let ((type (or (pop path) "1")))       ;default when type and selector omitted
          (cond
            (search-p
             (make-instance 'gopher-search
                            :name-string (when *retain-original-name-string* (subseq url start end))
                            :host-string host-string
                            :port port
                            :type type
                            :path (if object
                                      ;; ignore extension until we have
                                      ;; searchable gopher urls 7/28/94 -- JCMa.
                                      (nconc path (list object))
                                      path)
                            :search-keys search-keys
                            :search-parent search-parent))
            (object
             (make-instance 'gopher-object
                            :name-string (when *retain-original-name-string* (subseq url start end))
                            :host-string host-string
                            :type type
                            :port port
                            :path path
                            :object object
                            :extension extension))
            (t (make-instance 'gopher-path
                              :name-string (when *retain-original-name-string* (subseq url start end))
                              :host-string host-string
                              :port port
                              :type type
                              :path path))))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.401")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define-scheme-parser
  wais
  (:classes (wais-url))
  (url start end)
  (multiple-value-bind (host-string port path-index)
      (get-host-port-info url start end)
    ;; extract the path components
    (multiple-value-bind (raw-path object-index next-index search-p)
        (get-path-info url path-index end)
      (cond
        (search-p
         (when raw-path
           (error 'path-components-in-wais-database :url-string url))
         (let ((s-suffix (1+ (the fixnum next-index)))
               search-parent search-keys)
           (unless (= s-suffix end)
             (setq search-parent (intern-url url :start (- (the fixnum start) 5) :end s-suffix)
                   search-keys (parse-search-info url s-suffix end)))
           (make-instance 'wais-search
                          :name-string (when *retain-original-name-string* (subseq url start end))
                          :host-string host-string
                          :port port
                          :database (subseq url object-index next-index)
                          :search-keys search-keys
                          :search-parent search-parent)))
        ((null raw-path) (error 'no-wais-database :url-string url))
        ((cdr raw-path) ;; WAIS document
         ;; extract required fields
         (multiple-value-bind (database type size path)
             (extract-wais-doc-info url raw-path)
           ;; extract object if present
           (multiple-value-bind (object extension)
               (when object-index
                 (get-object-info url object-index next-index))
             ;; make the WAIS document
             (make-instance 'wais-doc
                            :name-string (when *retain-original-name-string* (subseq url start end))
                            :host-string host-string
                            :port port
                            :database database
                            :type type
                            :size size
                            :path path
                            :object object
                            :extension extension))))
        (t (error 'raw-path-components-in-wais-database :url-string url))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.401")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define-scheme-parser
  telnet
  (:classes (telnet-url))
  (url start end)
  (multiple-value-bind (userid host-string port)
      (get-telnet-info url start end)
    (make-instance 'telnet-url
                   :name-string (when *retain-original-name-string* (subseq url start end))
                   :host-string host-string
                   :port port
                   :user-id userid)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.401")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define-scheme-parser
  mailto
  (:classes (mailto-url))
  (url start end)
  (multiple-value-bind (userid host-string)
      (get-mailto-info url start end)
    (make-instance 'mailto-url
                   :name-string (when *retain-original-name-string* (subseq url start end))
                   :user-id userid
                   :host-string host-string)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.401")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define-scheme-parser
  file
  (:classes (file-url))
  (url start end)
  (let ((path-index (string-search= "//" url 0 2 start end))
        object extension)
    (unless path-index
      (error 'parsing-error :url-string url))
    ;; extract the path components
    (multiple-value-bind (path object-index next-index)
        (get-path-info url (+ 2 path-index) end)
      (let ((host-string (or (pop path) "localhost")))
        ;; get the object components when present
        (when object-index
          (multiple-value-setq (object extension)
            (get-object-info url object-index next-index)))
        (if object
            (make-instance 'file-pathname
                           :name-string (when *retain-original-name-string* (subseq url start end))
                           :host-string host-string
                           :path path
                           :object object
                           :extension extension)
            (make-instance 'file-directory
                           :name-string (when *retain-original-name-string* (subseq url start end))
                           :host-string host-string
                           :path path))))))
