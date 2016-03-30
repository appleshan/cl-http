;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: xml-utils; -*-
;;;

#|
<DOCUMENTATION>
 <DESCRIPTION>
  common macro definitions and utility functions
  </DESCRIPTION>
 <COPYRIGHT YEAR='2001' AUTHOR='james adam anderson' MARK='(C)'
            href='file://xml/sysdcl.lisp' />
 <CHRONOLOGY>
  <DELTA DATE='20010501' AUTHOR='JAA'> factored from XQDM model</DELTA>
  <DELTA DATE='20010605' AUTHOR='MS'> lispworks conformance</DELTA>
  <DELTA DATE='20010611'>
   with-http-stream to use location header to update the base uri</DELTA>
  <DELTA DATE='20010621'>support for file url hosts</DELTA>
  <DELTA DATE='20010602'><code>DATA-URL</code>s as per rfc2397.</DELTA>
  <DELTA DATE='20010715'>name/symbol distinction</DELTA>
  <DELTA DATE='20010716'>fix for host in file-url pathname</DELTA>
  <DELTA DATE='20010914'><code>canonical-encoding</code> as util</DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
|#

(in-package "XML-UTILS")

(defParameter *token-package* (find-package "xml")
  "binds the package which holds XML keywords and metasymbols.")

;; the xml parser reduction constructors are defined via the macros implemented below.
;; the default functional form is a generic function. this to permit the application
;; to augment the parser without redefining the constructors. the generic form adds
;; a percent or two to the parse time. should that matter, replace the DEFMETHOD
;; definition operator with DEFUN.

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;(defParameter *constructor-operator* 'defun)
  (defParameter *constructor-operator* 'defmethod)
  )

(defun make-constructor-name (name)
  (setf name (string name))
  (unless (and (> (length name) 12)
               (string= "-Constructor" name :start2 (- (length name) 12)))
    (setf name (concatenate 'string  (remove-if (complement #'alpha-char-p) name)
                            "-Constructor")))
  (intern name))
;(make-constructor-name '|asdf|) (make-constructor-name '|asdf-Constructor|)

;;
;;

(defMacro defAlternativeConstructor (name)
  `(defConstructor ,name (&rest arglist &aux value)
     (declare (dynamic-extent arglist)
              (type list arglist)
              (optimize (speed 3) (safety 0)))
     (loop (unless (consp arglist) (return))
           (when (setf value (first arglist)) (return value))
           (setf arglist (rest arglist)))))

(defMacro defConstantConstructor (name value)
  `(,*constructor-operator* ,(make-constructor-name name) (&rest structure)
     (declare (ignore structure))
    ,value))

(defMacro defConstructor (name arglist &rest body)
  "define a construction method. <code>*constructor-operator*</code> value
   decides between <code>defun</code> and <code>defMethod</code>."
  `(,*constructor-operator* ,(make-constructor-name name) ,arglist
    ,@body))

(defMacro defConstructorMethod (name arglist &rest body)
  "define a construction method to permit specialization.
   if a documentation clause is present, use defGeneric in order to install 
   generic documentation
   ."
  (let ((documentation (when (and (consp (first body)) (eq (caar body) :documentation))
                      (pop body)))
        (generic-arglist nil)
        (name (make-constructor-name name)))
    (cond (documentation
           (dolist (arg arglist)
             (case arg
               (&aux (return))
               (t (push (if (consp arg) (first arg) arg) generic-arglist))))
           `(defGeneric ,name ,(reverse generic-arglist)
              ,documentation
              (:method ,arglist ,@body)))
          (t
           `(defMethod ,name ,arglist ,@body)))))

(defMacro defIdentityConstructor (name)
  `(,*constructor-operator* ,(make-constructor-name name) (structure)
    structure))

(defMacro defLiteralConstructor (name)
  ;; copy out the string buffer and return it
  `(,*constructor-operator* ,(make-constructor-name name) (structure)
     structure))

(defMacro defNullConstructor (name)
  `(defConstantConstructor ,name nil))

;; keywords from XML syntax are carried as symbols interned in the "xml" package
;; they are present in all documents and figure in other constructors to
;; control the parse.

;; bind this in order to collect symbols to be exported from the
;; token package - used for development purposes only
(defVar *tokens-to-export*)

(defMacro defTokenConstructor (name)
  `(defConstantConstructor ,name ',(intern name *token-package*)))

(defMacro defTokenConstructors (&rest constants)
  `(progn ,@(mapcar #'(lambda (c) `(defTokenConstructor ,c)) constants)))

(defmacro defToken (string)
  (let ((token (intern string *token-package*)))
    `(progn
       (unless (boundp ',token)
         (defParameter ,token ',token))
       ;; (setf (get ',token *package*) :token)
       (when (boundp '*tokens-to-export*)
         (pushnew ,(string token) *tokens-to-export* :test #'string=))
       ',token)))

(defMacro defTokens (&rest strings)
  `(progn
     ,@(mapcar #'(lambda (string) `(defToken , string)) strings)))


(defMacro trace-constructor (&rest names)
  `(trace ,@(mapcar #'make-constructor-name names)))



#+(or ALLEGRO LispWorks)
(defMethod stream-position ((stream file-stream) &optional new)
  (if new
    (file-position stream new)
    (file-position stream)))

#+ALLEGRO
(defun stream-column (stream)
  (excl:stream-line-column stream))

#+LispWorks
(defun stream-column (stream)
  (stream:stream-line-column stream))


#+ALLEGRO
(defMethod stream-position ((stream socket::socket) &optional new)
  (if new
    (error "position setting not supported for stream: ~s." stream)
    0))

#+(and LispWorks (not LISPWORKS-PERSONAL-EDITION))
(defMethod stream-position ((stream comm:socket-stream) &optional new)
  (if new
    (error "position setting not supported for stream: ~s." stream)
    0))

#+MCL
(defMethod stream-position ((stream ccl::binary-tcp-stream) &optional new)
  (if new
    (error "position setting not supported for stream: ~s." stream)
    (ccl::ot-conn-bytes-read (ccl::stream-io-buffer stream))))



;;
;;
;; uri management

(defGeneric merge-uris (new base)
  (:method ((new string) (base pathname))
           (merge-pathnames new base))
  (:method ((new pathname) (base pathname))
           (when (find ".." (pathname-directory new) :test #'string=)
             (setf new (make-pathname :directory (substitute :up ".." (pathname-directory new) :test #'string=)
                                      :name (pathname-name new)
                                      :type (pathname-type new))))
           (merge-pathnames new base))
  (:method ((new t) (base null))
           new))

(defun parse-http-url (literal &aux (start 0) (relative nil) (host nil) )
  (when (string= "http:" literal :end2 (min (length literal) 5))
    (setf start 5))
  (cond ((char= (schar literal start) #\/)
         (setf host (and (> (length literal) (1+ start))
                          (char= (schar literal (1+ start)) #\/))))
        (t
         (setf relative t)))
  (let ((elements
         (split-string literal #'(lambda (c) (char= c #\/)) :start start)))
    (unless elements (error "http url syntax error: ~s." literal))
    (if host
      (let* ((host (first elements))
             (path (butlast (rest elements)))
             (file (first (last (rest elements))))
             (dot (position #\. file))
             (object (if dot (subseq file 0 dot) file))
             (extension (when dot (subseq file (1+ dot)))))
        (values host path object extension))
      (let* ((path (butlast elements))
             (file (first (last elements)))
             (dot (position #\. file))
             (object (if dot (subseq file 0 dot) file))
             (extension (when dot (subseq file (1+ dot)))))
        (when (and relative
                   (not (or (string= (first path) ".") (string= (first path) ".."))))
          (push "." path))
        (values nil path object extension)))))

(defun parse-file-url (literal &aux (start 0) (relative nil) hostp)
  (cond ((string= "file://" literal :end2 (min (length literal) 7))
         (setf start 7)
         (unless (char= (schar literal start) #\/)
           (setf hostp t)))
        ((char= (schar literal start) #\/))
        (t
         (setf relative t)))
  (let ((list
         (split-string literal #'(lambda (c) (char= c #\/)) :start start)))
    (let* ((host (if hostp (first list)))
           (path (butlast (if hostp (rest list) list)))
           (file (first (last list)))
           (dot (position #\. file))
           (object (if dot (subseq file 0 dot) file))
           (extension (when dot (subseq file (1+ dot)))))
      (when relative
        (unless (or (string= (first path) ".") (string= (first path) ".."))
          (push "." path)))
      (values host path object extension))))

(defun pathname-file-url (pathname &aux host)
  (setf pathname (or (probe-file pathname) pathname))
  (setf host (pathname-host pathname))
  (when (eq host :unspecific) (setf host nil))
  ;; round-about through the string to accommodate +/- cl-http
  (make-uri (format nil "file://~@[~a~]/~{~a/~}~a~@[.~a~]"
                    host
                    (rest (pathname-directory pathname))
                    (pathname-name pathname)
                    (pathname-type pathname))
            nil))

(defun parse-data-url (literal &aux (start 0))
  (cond ((string= "data:" literal :end2 (min (length literal) 5))
         (setf start 5)))
  (flet ((decode-url (string start end &aux (data (make-array 64 :adjustable t
                                                              :fill-pointer 0)))
           (do ((i start (1+ i)))
               ((>= i end))
             (case (char string i)
               (#\% (vector-push-extend (parse-integer string :start (+ i 1)
                                                       :end (+ i 3)
                                                       :radix 16)
                                        data)
                (incf i 2))
               (t (vector-push-extend (char-code (char string i)) data))))
           data))
    (let* ((comma (or (position #\, literal :start start)
                      (error "data url syntax error: ~s." literal)))
           (data (decode-url literal (1+ comma) (length literal)))
           (props (split-string literal ";," :start start :end comma))
           (base64 (member "base64" props :test #'string-equal))
           (media-type (ldiff props base64))
           (mime-type (first media-type))
           (parameters (rest media-type)))
      (when base64
        (error "data url: unsupported encoding: base64." ))
      (when (and mime-type (not (string-equal mime-type "text/plain")))
        (error "data url: unsupported mime type: ~s." mime-type))
      (dolist (parameter parameters)
        (cond ((string= "charset=" parameter :end2 (min (length parameter) 8))
               (unless (or (string-equal parameter "charset=US-ASCII")
                           (string-equal parameter "charset=USASCII"))
                 (error "data url: unsupported encoding: ~s." parameter)))
              (t
               (warn "data url: parameter ignored: ~s." parameter))))
      (values data mime-type parameters))))

;(parse-data-url "data:,asdf%20qwer")
;(parse-data-url "data:text/plain,asdfqwer")
;(parse-data-url "data:text/plain;charset=USASCII,asdfqwer")

#-CL-HTTP
(defClass uri () () )

#-CL-HTTP
(defClass http-url (uri)
  ((host :initform nil :initarg :host :accessor host-string)
   (port :initform 80 :initarg :port :accessor port)
   (path :initform nil :initarg :path :accessor path)
   (object :initform nil :initarg :object :accessor object)
   (extension :initform nil :initarg :extension :accessor extension)))

#-CL-HTTP
(defMethod print-object ((instance http-url) (stream t))
  (print-unreadable-object (instance stream :type t)
    (with-slots (host port path object extension) instance
      (format stream "http://~@[~a~]~@[:~a~]/~{~a/~}~@[~a~@[.~a~]~]"
              host (when (and port (/= port 80)) port)
              path object extension instance))))

#-CL-HTTP
(defun make-http-url (namestring &optional defaults &aux default-path default-host)
  (loop (etypecase defaults
          (file-url (setf default-host (host-string defaults)
                          default-path (path defaults))
                    (return))
          (http-url (setf default-host (host-string defaults)
                          default-path (path defaults))
                    (return))
          (string (setf defaults (make-uri defaults nil)))
          (pathname (setf defaults (pathname-file-url defaults)))
          (urn (return))
          (null (return))))
  (multiple-value-bind (host path object extension)
                       (parse-http-url namestring)
    (cond ((string= (first path) ".")
           (when default-path (setf path (nconc default-path (rest path)))))
          ((string= (first path) "..")
           (when default-path
             (loop (setf default-path (butlast default-path))
                   (pop path)
                   (unless (string= (first path) "..") (return)))
             (setf path (nconc default-path path)))))
    (unless host (setf host default-host))
    (make-instance 'http-url :host host :path path
                   :object object :extension extension)))

#-CL-HTTP
(defMethod uri-namestring ((uri http-url) &aux (port (port uri)))
  (format nil "http://~@[~a~]~@[:~a~]/~{~a/~}~@[~a~@[.~a~]~]"
          (host-string uri) (when (and port (/= port 80)) port)
          (path uri) (object uri) (extension uri)))


#-CL-HTTP
(defClass file-url (uri)
  ((host :initform nil :initarg :host :accessor host-string)
   (path :initform nil :initarg :path :accessor path)
   (object :initform nil :initarg :object :accessor object)
   (extension :initform nil :initarg :extension :accessor extension)))

#-CL-HTTP
(defMethod print-object ((instance file-url) (stream t))
  (print-unreadable-object (instance stream :type t)
    (format stream "file://~@[~a~]/~{~a/~}~a~@[.~a~]"
            (host-string instance)
            (path instance) (object instance) (extension instance))))

#-CL-HTTP
(defun make-file-url (namestring &optional defaults &aux default-host default-path)
  (loop (etypecase defaults
          ((or file-url http-url)
           (setf default-host (host-string defaults)
                 default-path (path defaults))
           (return))
          (string (setf defaults (make-uri defaults nil)))
          (pathname (setf defaults (pathname-file-url defaults)))
          (urn (return))
          (null (return))))
  (multiple-value-bind (host path object extension)
                       (parse-file-url namestring)
    (unless host (setf host default-host))
    (cond ((string= (first path) ".")
           (if default-path
             (setf path (nconc default-path (rest path)))
             ;; try to recover from anomolous '.' with root as default
             (setf path (rest path))))
          ((string= (first path) "..")
           ;; try to recover from anomolous '..' with root as default
           (loop (setf default-path (butlast default-path))
                 (pop path)
                 (unless (string= (first path) "..") (return)))
           (setf path (nconc default-path path))))
    (make-instance 'file-url :host host
                   :path path :object object :extension extension)))

#-CL-HTTP
(defMethod uri-namestring ((uri file-url))
  ;; no host spec used
  (format nil "file://~@[~a~]/~{~a/~}~a~@[.~a~]"
          (host-string uri) (path uri) (object uri) (extension uri)))


(defClass urn (uri)
  ((literal :initform nil :initarg :literal :accessor literal)))

(defMethod print-object ((instance urn) (stream t))
  (print-unreadable-object (instance stream :type t)
    (format stream "urn:~a" (literal instance) )))

(defun make-urn (literal)
  (make-instance 'urn :literal literal))

(defMethod uri-namestring ((uri urn))
  (format nil "urn:~a" (literal uri) ))


(defClass data-url (uri)
  ((data :initform #() :initarg :data :reader url-data)
   (mime-type :initform :text/plain :initarg :mime-type :reader url-mime-type)
   (parameters :initform nil :initarg :parameters :reader url-parameters))
  (:documentation
   "the form is
       dataurl    := 'data:' [ mediatype ] [ ';base64' ] ',' data
       mediatype  := [ type '/' subtype ] *( ';' parameter )
       data       := *urlchar
       parameter  := attribute '=' value
    as per rfc2397.
    'text/plain;charset=US-ASCII' is the only supported media type and encoding."))

(defMethod print-object ((instance data-url) (stream t))
  (print-unreadable-object (instance stream :type t)
    (write-string "data:," stream)
    (map 'nil #'(lambda (code) (write-char (code-char code) stream))
         (url-data instance))))

(defun make-data-url (literal)
  (multiple-value-bind (data mime-type parameters)
                       (parse-data-url literal)
    (make-instance 'data-url :data data :mime-type mime-type :parameters parameters)))

(defMethod uri-namestring ((instance data-url))
  (format nil "data:,~a" (map 'string #'code-char (url-data instance))))

;(make-uri "data:,asdf")

;;
;;
;;


#+CL-http
(defParameter *http-url-mode* :client
  "distinguish urls which are read from a foreign source from those which are
     served.
     when :SERVER, only pre-existing interned urls are permitted.
     when :CLIENT, no constraint is enforced.")

#+CL-http
(defun make-http-url (namestring &optional defaults)
  ;; reduce defaults to the directory portion in order that merge works
  (etypecase defaults
    (file-url (setf defaults (directory-name-string defaults)))
    (http-url (setf defaults (directory-name-string defaults)))
    (string )
    (urn (setf defaults ""))
    (null (setf defaults "")))
  (ecase *http-url-mode*
    (:client (parse-url (merge-url namestring defaults)))
    (:server (intern-url (merge-url namestring defaults) :if-does-not-exist :error))))

#+CL-http
(defun make-file-url (namestring &optional defaults)
  (etypecase defaults
    (file-url (setf defaults (directory-name-string defaults)))
    (http-url (setf defaults (directory-name-string defaults)))
    (string )
    (urn (setf defaults ""))
    (null (setf defaults "")))
  (ecase *http-url-mode*
    (:client (parse-url (merge-url namestring defaults)))
    (:server (intern-url (merge-url namestring defaults) :if-does-not-exist :error))))

#+CL-http
(defmethod uri-namestring ((url url))
  (name-string url))


(defMethod uri ((uri uri)) uri)

(defun make-uri (literal &optional base)
  ;; treat this as an uri, which, if unscheamed, is presumed to be of file type
  (handler-case
    (cond ((string= "file:" literal :end2 (min (length literal) 5))
           (make-file-url literal base))
          ((string= "http:" literal :end2 (min (length literal) 5))
           (make-http-url literal  base))
          ((string= "data:" literal :end2 (min (length literal) 5))
           ;; data urls have no relative form, thus no base is used
           (make-data-url literal))
          (t
           (etypecase base
             (file-url
              (make-file-url literal base))
             (null
              (make-file-url literal "file:///"))
             (pathname
              (make-file-url literal (pathname-file-url base)))
             (http-url
              (make-http-url literal base)))))
    (condition (condition)
               (cond (|rfc1738|
                      (error condition))
                     (t
                      (warn "literal uri value will be used: ~s / ~s:~%~a."
                            literal base condition)
                      (return-from  make-uri literal))))))

(defGeneric ensure-uri (datum)
  (:method ((datum string)) (make-uri datum))
  (:method ((url file-url)) url)
  (:method ((url http-url)) url)
  (:method ((url data-url)) url)
  (:method ((urn urn)) urn))

(defGeneric ensure-http-url (datum)
  (:method ((datum string)) (ensure-http-url (make-uri datum)))
  (:method ((datum http-url)) datum))

(defGeneric ensure-file-url (datum)
  (:method ((datum string)) (ensure-file-url (make-uri datum)))
  (:method ((datum file-url)) datum))

(defGeneric ensure-data-url (datum)
  (:method ((datum string)) (ensure-data-url (make-uri datum)))
  (:method ((datum data-url)) datum))

(defMethod file-url-pathname ((url string))
  (file-url-pathname (ensure-file-url url)))

(defMethod file-url-pathname ((url file-url))
  (let ((directory (path url))
        (name (object url))
        (host (host-string url))
        (type (extension url)))
    (if (or (string= ".." (first directory)) (string= "." (first directory)))
      (push :relative directory)
      (push :absolute directory))
    (if (zerop (length host))
      (make-pathname :directory directory :name name :type type)
      (make-pathname :host host :directory directory :name name :type type))))

(defun normalize-pathname (pathname &aux (directory (pathname-directory pathname))
                                    (modified nil))
  ;; this is intended to operate on absolute defaulted pathnames.
  ;; in mcl4.2 "." and ".." in pathnames are not handled correctly, so they are edited
  ;; here.
  (when (find "." directory :test #'string=)
    (setf directory (remove "." directory :test #'string=)
          modified t))
  (when (find ".." directory :test #'string=)
    (setf directory (substitute :up ".." directory :test #'string=)
          modified t))
  (if modified
    (make-pathname :directory directory :defaults pathname)
    pathname))

(defun resolve-entity-identifiers (public system defaults)
  (cond (system
         (make-uri system defaults))
        (public
         ;; for now, a notation can have just a public id, in which case an urn is used
         (make-urn public))
        (t
         nil)))

;;
;;
;; http url stream management


;; in mcl, http url retrieval without cl-http

#+(and (or ALLEGRO LispWorks MCL) (not CL-HTTP))
(defun read-http-header-line
       (stream
        &optional (line (make-array 10 :adjustable t :element-type 'character :fill-pointer 0))
        &aux (byte nil))
  (setf (fill-pointer line) 0)
  (multiple-value-bind (reader arg) (stream-reader stream)
    (loop (unless (setf byte (funcall reader arg)) (return line))
          (when (= byte #.(char-code #\return))
            (funcall reader arg)
            (return line))
          (vector-push-extend (code-char byte) line))))

#+(and ALLEGRO (not CL-HTTP))
(defMacro with-http-stream ((stream url)
                            &rest body
                            &aux (is-complete-sym (gensym "COMPLETE?-"))
                            (url-sym (gensym "URL-"))
                            (response-sym (gensym "RESPONSE-")))
  ;; this should really be used with uri instances, for which defaulting is complete,
  ;; but coerce just in case.
  ;; it connects, strips the headers and continues with the content.
  `(let* ((,url-sym (ensure-http-url ,url))
          (,is-complete-sym nil)
          (,stream nil)
          (,response-sym nil))
     (unwind-protect
       (progn
         (setf ,stream (socket::make-socket
                         :remote-host (host-string ,url-sym)
                         :remote-port (port ,url-sym)
                         :format :binary))
         ;; (setf ,stream (ccl::open-tcp-stream (host ,url-sym) (port ,url-sym) :element-type 'unsigned-byte))
         (map nil #'(lambda (char) (write-byte (char-code char) ,stream))
              (format nil "GET ~a HTTP/1.0~C~C~C~C"
                      (uri-namestring ,url-sym)
                      #\Newline #\Linefeed #\Newline #\Linefeed))
         (force-output ,stream)
         (setf ,response-sym (read-http-header-line ,stream))
         (case (ignore-errors (with-input-from-string (stream ,response-sym)
                                (read stream) (read stream)))
           (200
            (loop (when (zerop (length (read-http-header-line ,stream ,response-sym)))
                    (return))
                  ;; update the base to match the location
                  (when (string-equal "location:" ,response-sym :end2 (min (length ,response-sym) 9))
                    (setf *xml-base*
                          (make-uri (string-trim #(#\space) ,response-sym) *xml-base*))))
            (prog1 (progn ,@body)
              (setf ,is-complete-sym t)))
           ((404 410)
            (error "Document Not Found: ~s." ,url-sym))
           (t
            (error "Unknown HTTP error: ~s: ~s." ,response-sym ,url-sym))))
       (when ,stream (close ,stream :abort (not ,is-complete-sym))))))

#+(and LispWorks (not LISPWORKS-PERSONAL-EDITION) (not CL-HTTP))
(defMacro with-http-stream ((stream url)
                            &rest body
                            &aux (is-complete-sym (gensym "COMPLETE?-"))
                            (url-sym (gensym "URL-"))
                            (response-sym (gensym "RESPONSE-")))
  ;; this should really be used with uri instances, for which defaulting is complete,
  ;; but coerce just in case.
  ;; it connects, strips the headers and continues with the content.
  `(let* ((,url-sym (ensure-http-url ,url))
          (,is-complete-sym nil)
          (,stream nil)
          (,response-sym nil))
     (unwind-protect
       (progn
         (setf ,stream (comm:open-tcp-stream (host-string ,url-sym)
					     :element-type '(unsigned-byte 8)))
         ;; (setf ,stream (ccl::open-tcp-stream (host ,url-sym) (port ,url-sym) :element-type 'unsigned-byte))
         (map nil #'(lambda (char) (write-byte (char-code char) ,stream))
              (format nil "GET ~a HTTP/1.0~C~C~C~C"
                      (uri-namestring ,url-sym)
                      #\Newline #\Linefeed #\Newline #\Linefeed))
         (force-output ,stream)
         (setf ,response-sym (read-http-header-line ,stream))
         (case (ignore-errors (with-input-from-string (stream ,response-sym)
                                (read stream) (read stream)))
           (200
            (loop (when (zerop (length (read-http-header-line ,stream ,response-sym)))
                    (return))
                  ;; update the base to match the location
                  (when (string-equal "location:" ,response-sym :end2 (min (length ,response-sym) 9))
                    (setf *xml-base*
                          (make-uri (string-trim #(#\space) ,response-sym) *xml-base*))))
            (prog1 (progn ,@body)
              (setf ,is-complete-sym t)))
           ((404 410)
            (error "Document Not Found: ~s." ,url-sym))
           (t
            (error "Unknown HTTP error: ~s: ~s." ,response-sym ,url-sym))))
       (when ,stream (close ,stream :abort (not ,is-complete-sym))))))

#+(and MCL (not CL-HTTP))
(defMacro with-http-stream ((stream url)
                            &rest body
                            &aux (is-complete-sym (gensym "COMPLETE?-"))
                            (url-sym (gensym "URL-"))
                            (response-sym (gensym "RESPONSE-")))
  ;; this should really be used with uri instances, for which defaulting is complete,
  ;; but coerce just in case.
  ;; it connects, strips the headers and continues with the content.
  `(let* ((,url-sym (ensure-http-url ,url))
          (,is-complete-sym nil)
          (,stream nil)
          (,response-sym nil))
     (unwind-protect
       (progn
         (setf ,stream (make-instance 'ccl::binary-tcp-stream
                         :host (host-string ,url-sym)
                         :port (or (port ,url-sym) 80)
                         :element-type 'unsigned-byte))
         ;; (setf ,stream (ccl::open-tcp-stream (host-string ,url-sym) (port ,url-sym) :element-type 'unsigned-byte))
         (map nil #'(lambda (char) (write-char char ,stream))
              ;; 1.0 requests, no chunking
              (format nil "GET ~a HTTP/1.0~C~C~C~C"
                      (uri-namestring ,url-sym)
                      #\Newline #\Linefeed #\Newline #\Linefeed))
         (force-output ,stream)
         (setf ,response-sym (read-http-header-line ,stream))
         (case (ignore-errors (with-input-from-string (stream ,response-sym)
                                (read stream) (read stream)))
           (200
            (loop (when (zerop (length (print (read-http-header-line ,stream ,response-sym))))
                    (return))
                  
                  ;; update the base to match the location
                  (when (string-equal "location:" ,response-sym :end2 (min (length ,response-sym) 9))
                    (setf *xml-base*
                          (make-uri (string-trim #(#\space) ,response-sym) *xml-base*))))
            (prog1 (progn ,@body)
              (setf ,is-complete-sym t)))
           ((404 410)
            (error "Document Not Found: ~s." ,url-sym))
           (t
            (error "Unknown HTTP error: ~s: ~s." ,response-sym ,url-sym))))
       (when ,stream (close ,stream :abort (not ,is-complete-sym))))))

;; the default mechanism with cl-http present is to use the client facilities
;; the mcl version has to assert binary mode explicitly
#+CL-HTTP
(defMacro with-http-stream ((stream url) &rest body &aux (do-body (gensym "BODY-")))
  `(flet ((,do-body (,stream)
            #+MCL (ccl::set-input-mode ,stream :binary)
            ,@body))
     (let (; (http:*client-http-version* :http/1.1) ; http/1.0)
           (url (ensure-http-url ,url)))
     (http:with-http-request
       (url :get
            :request-headers
            (http::compute-standard-request-headers url :header-plist '( :accept ((:* :xml)))))
       (http::with-header-values (location transfer-encoding) http::*headers*
         (when location
           (setf *xml-base* (make-uri location *xml-base*)))
         (case transfer-encoding
           (:chunked
            (http::with-chunked-transfer-decoding (http::remote-stream)
              (,do-body http::remote-stream)))
           ((nil) (,do-body http::remote-stream))
           (t (error "transfer encoding not supported: ~s." transfer-encoding))))))))

#|
;; this version avoid chunked responses and appears to tell servers not to expect
;; a host header
(defMacro with-http-stream ((stream url) &rest body)
  `(let ((http:*client-http-version* :http/1.0)
         (,stream nil))
     (http:with-http-request ((ensure-http-url ,url) :get)
       (setf ,stream http::remote-stream)
       ,@body)))|#

#-(or ALLEGRO CL-HTTP LispWorks MCL)
(defMacro with-http-stream (&whole form (stream url &rest args &key element-type port)
                                   &rest body)
  (declare (ignore stream url args element-type port body))
  (cerror "continue with no HTTP support." "http stream not supported: ~s." form)
  `(error "no support for http streams: ~s." ',form))


(defMacro with-data-stream ((stream url) &rest body)
  `(let ((,stream (make-instance 'vector-input-stream
                    :vector (url-data (ensure-data-url ,url)))))
     ,@body))

#|
(uri-namestring (make-uri "http://www.adobe.com/"))
(with-http-stream (stream "http://www.adobe.com/")
  (let ((buffer (make-array 100 :element-type 'byte :adjustable t :fill-pointer 0))
        (byte nil))
    (loop (when (ccl::stream-eofp stream) (return))
          (setf byte (read-byte stream nil))
          (unless byte (return))
          (vector-push-extend byte buffer))
    buffer))

(with-http-stream (stream "http://home.nexgo.de/james_anderson/mandyswelt/index.wml")
  (let ((buffer (make-array 100 :element-type 'unsigned-byte :adjustable t :fill-pointer 0))
        (byte nil))
    (loop (when (ccl::stream-eofp stream) (return))
          (setf byte (read-byte stream nil))
          (unless byte (return))
          (vector-push-extend byte buffer))
    buffer))
    
|#

;;
;;
;; file url stream management

;; in mcl, file url retrieval without cl-http

#-CL-HTTP
(defMacro with-file-stream ((stream url) &rest body)
  ;; this should really be used with uri instances, for which defaulting is complete,
  ;; but ...
  `(with-open-file (,stream (normalize-pathname
                             (file-url-pathname (ensure-file-url ,url)))
                              :direction :input :element-type 'unsigned-byte)
       ,@body))

#+CL-HTTP
(defMacro with-file-stream ((stream url) &rest body)
  `(with-open-file (,stream (normalize-pathname
                             (translated-pathname (ensure-file-url ,url)))
                              :direction :input :element-type 'unsigned-byte)
     ,@body))


;;
;;
;; encoding utilities

(defun canonical-encoding (encoding)
  "edit the encoding to remove non-alphanumeric content;
   use the stripped form to search the known encodings."
  (setf encoding (remove-if (complement #'alphanumericp) (string encoding)))
  (first (find-if #'(lambda (alternatives)
                      (find encoding alternatives :test #'string-equal))
                  *encoding-map*)))
;(mapcar #'canonical-encoding '(:utf-8 "usascii"))




#|
(make-file-url "file:///asdf/qwer.ty" "file:///aaa/bbb/ccc.ddd")
(make-file-url "file:///../qwer.ty" "file:///aaa/bbb/ccc.ddd")
(make-file-url "file:///../../qwer.ty" "file:///aaa/bbb/ccc.ddd")
(make-file-url "file:///../../../qwer.ty" "file:///aaa/bbb/ccc.ddd")
(make-file-url "file:///./qwer.ty" "file:///aaa/bbb/ccc.ddd")
(make-file-url "qwer.ty" "file:///aaa/bbb/ccc.ddd")
(make-file-url "./qwer.ty" "file:///aaa/bbb/ccc.ddd")
(make-file-url "../qwer.ty" "file:///aaa/bbb/ccc.ddd")
(make-file-url "qwer.ty" "file://xml/aaa/bbb/ccc.ddd")
(make-file-url "./qwer.ty" "file://xml/aaa/bbb/ccc.ddd")
(make-file-url "../qwer.ty" "file://xml/aaa/bbb/ccc.ddd")
(make-file-url "qwer.ty" "file:///")
(make-file-url "./qwer.ty" "file:///")
(make-file-url "../qwer.ty" "file:///")

(trace parse-http-url)
(make-http-url "http://asdf/qwer.ty" "http://aaa/bbb/ccc.ddd")
(make-http-url "http:/../qwer.ty" "http://aaa/bbb/ccc.ddd")
(make-http-url "http:/../../qwer.ty" "http://aaa/bbb/ccc.ddd")
(make-http-url "http:/./qwer.ty" "http://aaa/bbb/ccc.ddd")
(make-http-url "qwer.ty" "http://aaa/bbb/ccc.ddd")
(make-http-url "./qwer.ty" "http://aaa/bbb/ccc.ddd")
(make-http-url "../qwer.ty" "http://aaa/bbb/ccc.ddd")

(make-uri "data:,asdf")

|#
:EOF
