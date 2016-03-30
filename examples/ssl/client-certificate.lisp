;;; -*- Mode: LISP; Package: HTTP; Syntax: ANSI-Common-Lisp -*-

;;; Copyright 2006, John C. Mallery.
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;; GENERATING CLIENT CERTIFICATES
;;;
;;; Based on: http://www.pseudonym.org/ssl/ssl_nsclient_certs.html
;;;
;;; Works with Mozilla, but fails to redisplay browser page after succeeding (fix this someone),
;;;
;;; To do:
;;;         - Add an interface for revoking client certificates
;;;

(in-package :http)

(defun write-create-client-certificate-form (url stream)
  (macrolet ((write-label (label stream)
               `(with-table-cell (:horizontal-alignment :right :stream ,stream)
                  (with-rendition (:italic :stream ,stream)
                    (fast-format ,stream "~A:" ,label)))))
    (flet ((write-row (stream query label value &optional size)
             (with-table-row (:stream stream)
               (write-label label stream)
               (with-table-cell (:stream stream)
                 (if size
                     (accept-input 'string query :default value :size size :stream stream)
                   (accept-input 'string query :default value :stream stream))))))
      (let ((title "Create Client Certificate"))
        (with-cl-http-html-document (:declare-dtd-version-p :transitional :stream stream)
          (with-document-look-preamble (:stream stream)
            (declare-title title :stream stream))
          (with-document-look-body (:heading title :header-class "header.cl-http" :body-class "body.cl-http"
                                    :footer-class "footer.cl-http" :stream stream)
            (with-paragraph (:stream stream) 
              (with-fillout-form (:post url :stream stream)
                (with-table (:cell-padding 5 :stream stream)
                  (write-row stream "commonName" "Common Name (eg, YOUR name)" "John Doe" 64)
                  (write-row stream "emailAddress" "Email" "Webmaster@nospam.cl-http.org" 40)
                  (write-row stream "organizationName" "Organization (eg, company name)" "CL-HTTP Consortium" 64)
                  (write-row stream "organizationalUnitName" "Organizational Unit (eg, section)" "Cambridge Research" 64)
                  (write-row stream "localityName" "Locality (eg, City)" "Cambridge" 40)
                  (write-row stream "stateOrProvinceName" "State or Province (full name)" "MA" 40)
                  (write-row stream "countryName" "Country (2 letter ISO Code)" "US" 2)
                  (with-table-row (:stream stream)
                    (write-label "Key Length" stream)
                    (with-table-cell (:stream stream)
                      (comment "keygen is Netscape specific and will be ignored by internet explorer" :stream stream)
                      ;; See: http://wp.netscape.com/eng/security/comm4-keygen.html
                      (fast-format stream "<KEYGEN NAME=\"SPKAC\" CHALLENGE=\"challengePassword\" keytype=\"RSA\">")))
                  (with-table-row (:stream stream)
                    (with-table-cell (:stream stream)
                      (with-rendition (:bold :stream stream)
                        (fast-format stream "Action:" label)))
                    (with-table-cell (:stream stream)
                      (accept-input 'submit-button "Submit" :stream stream))))))))))))

(defvar *certificate-authority-configuration* "[ ca ]
default_ca              = CA_own
[ CA_own ]
dir                     = .
certs                   = .
new_certs_dir           = ./ca.db.certs
database                = ./ca.db.index
serial                  = ./ca.db.serial
RANDFILE                = ./ca.db.rand
certificate             = ./ca.crt
private_key             = ./ca.key
default_days            = 365
default_crl_days        = 30
default_md              = md5
preserve                = no
policy                  = policy_anything
[ policy_anything ]
countryName             = optional
stateOrProvinceName     = optional
localityName            = optional
organizationName        = optional
organizationalUnitName  = optional
commonName              = supplied
emailAddress            = optional")

(defun write-certificate-authority-configuration-file (pathname)
  (with-open-file (stream pathname :direction :output :element-type *standard-character-type*)
    (format stream "~A~&" *certificate-authority-configuration*)))

(defclass certificate-authority
          ()
  ((name :initarg :name :reader ca-name :type string)
   (directory :initarg :directory :reader ca-directory :type string)
   (config-file :initarg :config-file :reader ca-config-file :type string)
   (certificate-file :initarg :certificate-file :reader ca-certificate-file :type string)
   (private-key-file :initarg :private-key-file :reader ca-private-key-file :type string)
   (password :initarg :password :reader ca-password :type string)
   (client-certificate-directory :initarg :client-certificate-directory :reader ca-client-certificate-directory :type string)
   (client-certificate-expiration :initarg :client-certificate-expiration :accessor ca-client-certificate-expiration :type (integer 0))
   (lock :initform (make-lock "Certificate Authority Lock") :reader ca-lock) ;; Shell command OpenSSL CA is not thread safe
   (uid-series :initarg :uid-series :reader uid-series)))

(defmethod print-object ((certificate-authority certificate-authority) stream)
  (with-slots (name) certificate-authority
    (print-unreadable-object (certificate-authority stream :type t :identity t)
      (write-string (or name "No name") stream))))

(defmethod ca-allocate-uid ((certificate-authority certificate-authority))
  (with-slots (uid-series) certificate-authority
    (uid-series-allocate uid-series)))

(defun make-certificate-authority (name &key directory config-file certificate-file private-key-file password
                                        client-certificate-directory client-certificate-expiration
                                        (uid-width 8) (uid-interval 10))
  (flet ((path-namestring (pathname)
           (namestring (translated-pathname pathname))))
    (let ((client-certificate-directory (path-namestring client-certificate-directory))
          (ca-config-path (merge-pathnames config-file directory)))
      ;; Make sure a default CA config file is in place
      (unless (probe-file ca-config-path)
        (write-certificate-authority-configuration-file ca-config-path))
      ;; make the object
      (make-instance 'certificate-authority
                     :name name
                     :directory (path-namestring directory)
                     :config-file (path-namestring config-file)
                     :certificate-file (path-namestring certificate-file)
                     :private-key-file (path-namestring private-key-file)
                     :password password
                     :client-certificate-directory client-certificate-directory
                     :client-certificate-expiration client-certificate-expiration
                     :uid-series (create-uid-series (concatenate 'string name "-certificate-authority-uid")
                                                    uid-interval uid-width client-certificate-directory
                                                    #'(lambda (event format-string &rest format-args)
                                                        (declare (ignore event)
                                                                 (dynamic-extent format-args))
                                                        (apply #'notify-log-window format-string format-args)))))))

;; If you create a certificate authority following the procedure in /cl-http/ssl/certificate.html
;; then you can plug in your parameter here and mint your own licent certificates using netscape,
;; mozilla, or Firefox.

(defvar *certificate-authority* (make-certificate-authority
                                 "CL-HTTP"
                                 :directory "/Users/jcma/desktop/ssl-ca/"
                                 :config-file "caweb.config"
                                 :certificate-file "ca.crt"
                                 :private-key-file "ca.key"
                                 :password ""
                                 :client-certificate-directory "/Users/jcma/desktop/ssl-ca/clientcerts/"
                                 :client-certificate-expiration 365))

;; switches for shell invocation
(defparameter *ca-shell-switches* "--norc")

#+(and unix LispWorks)
(defmethod openssl-create-client-certificate ((certificate-authority certificate-authority) spkac-file certificate-file)
  (declare (values file error-p error-description))
  (flet ((filename (type default)
           (namestring (make-pathname :type type :defaults default))))
    (let ((command (format nil "sh ~A -c 'cd ~A ; openssl ca -config ~A -cert ~A -keyfile ~A -key ~A -spkac ~A -out ~A -days ~D;'"
                           *ca-shell-switches* 
                           (ca-directory certificate-authority)
                           (ca-config-file certificate-authority)
                           (ca-certificate-file certificate-authority)
                           (ca-private-key-file certificate-authority)
                           (ca-password certificate-authority) 
                           spkac-file 
                           certificate-file 
                           (ca-client-certificate-expiration certificate-authority))))
      (declare (dynamic-extent command))
      (with-lock-held ((ca-lock certificate-authority) :write "Certificate Authority Wait")
        (case (sys:run-shell-command command :wait t :output (filename "out" spkac-file) :error-output (filename "err" spkac-file))
          (0 
           (if (probe-file certificate-file)
               (values certificate-file nil) ;return the certificate file
             (values (filename "err" spkac-file) t "Certificate Request Failure: No certificate was produced!")))            
          (t (values (filename "err" spkac-file) t "Certificate Request Failure: Call to OpenSSL CA failed")))))))

(defmethod create-client-certificate ((certificate-authority certificate-authority) spkac common-name email-address organization-name organizational-unit-name 
                                      locality-name state-or-province-name country-name)
  (declare (values file error-p error-description))
  (labels ((write-spkac (stream) ;clean up spkac
             (loop for ch across spkac
                   unless (member ch '(#\Return #\Linefeed))
                   do (write-char ch stream)))
           (write-request-file (pathname &rest values)
             (declare (dynamic-extent values))
             (pathname-create-directory-if-needed pathname)
             (with-open-file (stream pathname :direction :output :if-does-not-exist :create :element-type *standard-character-type*)
               (loop for label in '("commonName" "emailAddress" "organizationName" "organizationalUnitName"
                                                 "localityName" "stateOrProvinceName" "countryName" "SPKAC")
                     for value in values
                     do (cond (value
                               (fast-format stream "~&~A = ~I~%" label (etypecase value
                                                                         (string (write-string value stream))
                                                                         (function (funcall value stream)))))
                              (t (error "Client Certificate Error: ~S has the value, ~S." label value)))))))
    (declare (dynamic-extent #'write-spkac))
    (let* ((cert-uid (ca-allocate-uid certificate-authority))
           (cert-dir (ca-client-certificate-directory certificate-authority))
           (request-pathname (make-pathname :name (format nil "cert~D" cert-uid) :type "spkac" :defaults cert-dir))
           (cert-pathname (make-pathname :name (format nil "cert~D" cert-uid) :type "pem" :defaults cert-dir)))
      ;; Write out a request file
      (write-request-file request-pathname common-name email-address organization-name organizational-unit-name 
                          locality-name state-or-province-name country-name #'write-spkac)
      ;; Create the certificate or return error values
      (openssl-create-client-certificate certificate-authority request-pathname cert-pathname))))

(defun create-client-certificate-from-form-values (url query-alist)
  (declare (values file error-p error-description)
           (ignore url))
  (bind-query-values (commonname emailaddress organizationname organizationalunitname localityname stateorprovincename countryname spkac)
      (url query-alist)
    (create-client-certificate *certificate-authority* spkac commonname emailaddress organizationname organizationalunitname 
                               localityname stateorprovincename countryname)))

(defun respond-to-create-client-certificate (url stream query-alist)
  (flet ((report-error (stream type &rest args &aux title)
           (declare (dynamic-extent args))
           (ecase type
             (:format (setq title "Certificate Request Failure"))
             (:error (setq title (second args))))
           (with-successful-response (stream :html 
                                             :cache-control (url:response-cache-control-directives url)
                                             :content-language (languages url))
             (with-cl-http-html-document (:declare-dtd-version-p :transitional :stream stream)
               (with-document-look-preamble (:stream stream)
                 (declare-title title :stream stream))
               (with-document-look-body (:heading title :header-class "header.cl-http" :body-class "body.cl-http"
                                         :footer-class "footer.cl-http" :stream stream)
                 (with-paragraph (:stream stream)
                   (ecase type
                     (:format (apply #'format stream (first args) (rest args)))
                     (:error
                      (cond ((probe-file (first args))
                             (with-open-file (file (first args) :direction :input :element-type *standard-character-type*)
                               (with-emphasis (:quotation :class "shell-output" :stream stream)
                                 (with-verbatim-text (:stream stream)
                                   (stream-copy-until-eof file stream :text)))))
                            (t (fast-format stream "~&No error file was produced by OpenSSL."))))))))))
         (return-certificate (stream certificate-pathname)
           (with-successful-response (stream :x509
                                             :status :created
                                             :cache-control (url:response-cache-control-directives url)
                                             #| :location (merge-url "/cl-http/")
                                             :additional-headers `(:refresh ,(concatenate 'string "content=\"1; URL=" (merge-url "/cl-http/") "\""))|#)
             (with-open-file (file certificate-pathname :direction :input :element-type *standard-character-type* :if-does-not-exist :error)
               (stream-copy-until-eof file stream :text)))))
    (bind-query-values (spkac)
        (url query-alist)
      (cond (spkac
             (multiple-value-bind (certificate-pathname error-p error-description)
                 (create-client-certificate-from-form-values url query-alist)
               (if error-p
                   (report-error stream :error certificate-pathname error-description)
                 (return-certificate stream certificate-pathname ))))
            (t (report-error stream "No client public key generated by your browser. Mozilla, or Netscape are required."))))))

(export-url #u("/cl-http/create-client-certificate.html" :port 8443 :protocol :https)
            :html-computed-form
            :form-function 'write-create-client-certificate-form
            :response-function 'respond-to-create-client-certificate
            :private t
            :no-cache t
            :no-store t
            :language :en
            :keywords '(:cl-http :ssl :certificates)
            :documentation "A form interface for creating client certificates with CL-HTTP.")

#|
#+ignore (create-client-certificate-write-html-description url stream query-alist)

(defun create-client-certificate-write-html-description (url stream query-alist)
  (flet ((write-row (stream label value)
           (with-table-row (:stream stream)
             (with-table-cell (:stream stream)
               (with-rendition (:italic :stream stream)
                 (fast-format stream "~A:" label)))
             (with-table-cell (:stream stream)
               (fast-format stream "~S" value)))))
    (bind-query-values (commonname emailaddress organizationname organizationalunitname localityname stateorprovincename countryname spkac)
        (url query-alist)
      (let ((title "Client Certificate Created"))
        (with-successful-response (stream :html :expires (url:expiration-universal-time url)
                                          :cache-control (url:response-cache-control-directives url)
                                          :content-language (languages url)
                                          :additional-headers '((:content-type (:application :x-x509-user-cert))))
          (with-cl-http-html-document (:declare-dtd-version-p :transitional :stream stream)
            (with-document-look-preamble (:stream stream)
              (declare-title title :stream stream))
            (with-document-look-body (:heading title :header-class "header.cl-http" :body-class "body.cl-http"
                                      :footer-class "footer.cl-http" :stream stream)
              (with-paragraph (:stream stream) 
                (with-table (:stream stream)
                  (write-row stream "Common Name" commonName)
                  (write-row stream "Email" emailAddress)
                  (write-row stream "Organization" organizationName)
                  (write-row stream "Organizational Unit" organizationalUnitName)
                  (write-row stream "Locality (City)" localityName)
                  (write-row stream "State" stateOrProvinceName)
                  (write-row stream "Country" countryName)
                  (write-row stream "Certificate Request (spkac)" spkac))))))))))
|#
