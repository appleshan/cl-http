;;;   -*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-

;;;
;;; (c) Copyright  1995, 2005-2006, John C. Mallery
;;;     All Rights Reserved.
;;;

;;;------------------------------------------------------------------- 
;;;
;;; FIND URL FOR LOCATING A URL ON WEB SITE
;;;
;;; Includes condition handling for document-not found to offer to search for the correct URL

(in-package :http)

(defun find-url (&optional search-spec sorted-p (protocol *standard-protocol*) (port *standard-http-port*)
                           &aux start2 end2)
  "Returns URLs matching SUBSTRING, optionally sorting them when SORTED-P is non-null.
Matching is retricted to PROTOCOL and PROTOCOL to avoid leakage across server applications.
When SEARCH-SPEC, PROTOCOL, or PORT are null, they are intrepreted as wild cards."
  (flet ((wild-p  (search-spec)
           (or (null search-spec)
               (and (stringp search-spec) (null-string-p search-spec))))
         (scheme-prefix-length (protocol)
           (multiple-value-bind (prefix prefix-length)
               (url::%scheme-prefix-for-protocol protocol)
             (declare (ignore prefix))
             prefix-length))
         (make-constraint (substring)
           (let ((start1 0)
                 (end1 (length substring)))
             #'(lambda (name)
                 (string-search substring name start1 end1 start2 end2)))))
    (declare (inline wild-p scheme-prefix-length)
             (dynamic-extent #'make-constraint))
    ;; Set up the constraints
    (let* ((wild-p (wild-p search-spec))
           (constraints (unless wild-p (mapcar #'make-constraint (ensure-list search-spec))))
           urls scheme-prefix-length)
      (declare (dynamic-extent constraints))
      ;; define the collector
      (flet ((collect (name url)
               (handler-case-if (not *debug-server*)
                   (when (and (url::exported-url-p url)
                              (local-url-p url)	;only match local URLs -- doesn't handle multiple interfaces -- JCMa 4/17/2006
                              (or (null port) (eql (url:port url) (the fixnum port))) ;match port
                              (or (null protocol) (eql (url:protocol url) protocol)) ;match protocol
                              (or wild-p
                                  (loop initially (setq end2 (length name)
                                                        start2 (char-position #\/ name 
                                                                              (or scheme-prefix-length (scheme-prefix-length (url:port url))) end2))
                                        for constraint in constraints
                                        always (funcall constraint name))))
                     (push url urls))
                 (error () nil))))
        ;; Prepare to match
        (when protocol
          (setq scheme-prefix-length (scheme-prefix-length protocol)))
        ;; Collect matching URLs
        (url:map-url-table #'collect))
      ;; Optionally sort them -- better to use the push-ordered macro from CML
      (cond (sorted-p
             (stable-sort urls #'string< :key #'url:name-string))
            (t urls)))))

(defun find-url-and-respond (url substrings stream)
  (let* ((urls (find-url substrings t))
         (title (if (and substrings (not (null-string-p (first substrings))))
                    (format nil "~D URL~:*~P Matching ~{~A~^, ~}" (length urls) substrings)
                  (format nil "~D URL~:*~P Exported by Server" (length urls)))))
    (with-successful-response (stream :html :expires (url:expiration-universal-time url)
                                      :cache-control (url:response-cache-control-directives url)
                                      :content-language (languages url)) 
      (with-html-document (:declare-dtd-version-p t :stream stream)
        (with-document-preamble (:stream stream)
          (declare-title title :stream stream)
          (declare-base-reference url :stream stream)
          (declare-link :reference "/cl-http/css/base.css" 
                        :relation "stylesheet" :media-type "text/css" :stream stream))
        (with-document-body (:stream stream)
          (with-section-heading (title :stream stream)
            (horizontal-line :stream stream)
            (with-enumeration (stream :enumerate)
              (loop for url in urls
                    for url-string = (url:relative-name-string url)
                    do (enumerating-item (stream)
                         (with-rendition (:bold :stream stream)
                           (flet ((write-describe-url (stream)
                                    (write-string "/cl-http/describe-url?" stream)
                                    (write-string-escaping-special-chars url-string stream)))
                             (declare (dynamic-extent #'write-describe-url))
                             (note-anchor "[?] " :reference #'write-describe-url :stream stream)
                             (note-anchor url-string :reference url-string :stream stream))))))
            (horizontal-line :stream stream)
            (cl-http-signature stream)))))))

(defmethod respond-to-find-url  ((url url:http-search) stream)
  (with-slots (url:search-keys) url
    (find-url-and-respond url url:search-keys stream)))

(defmethod compute-find-url-form ((url url:http-form) stream)
  (with-conditional-get-response (stream :html :expires (url:expiration-universal-time url)
                                         :cache-control (url:response-cache-control-directives url)
                                         :content-language (languages url))
    (with-html-document (:declare-dtd-version-p t :stream stream)
      (with-document-preamble (:stream stream)
        (declare-title "Find Exported URLs" :stream stream)
        (declare-base-reference url :stream stream)
        (declare-link :reference "/cl-http/css/base.css" 
                      :relation "stylesheet" :media-type "text/css" :stream stream))
      (with-document-body (:stream stream)
        (with-section-heading ("Find Exported URLs" :stream stream)
          (horizontal-line :stream stream)
          (with-paragraph (:stream stream)
            (write-string "This form will list all exported URLS on the server that match a substring.
You may provide multiple substrings if you separate the with a space." stream))
          (with-fillout-form (:post url :stream stream)
            (with-paragraph (:stream stream)
              (with-rendition (:bold :stream stream)
                (fresh-line stream)
                (write-string "Match (Substring): " stream))
              (accept-input 'string "URL-SUBSTRINGS" :size 30 :stream stream))
            (submit-and-reset-buttons stream))
          (horizontal-line :stream stream)
          (cl-http-signature stream))))))

(defmethod respond-to-find-url-form ((url url:http-form) stream query-alist)
  (labels ((delimiter-p (char)
             (member char '(#\space #\tab #\return #\Linefeed) :test #'eql))
           (breakup-pattern (string)
             (loop with length = (length string)
                   for start = (or (position-if-not #'delimiter-p string :start 0 :end length) 0)
                             then (or (position-if-not #'delimiter-p string :start end :end length) length)
                   for end = (or (position-if #'delimiter-p string :start start :end length) length)
                   while (< start end)
                   for item = (subseq string start end)
                   unless (null-string-p item)
                     collect item)))
    (bind-query-values (url-substrings)
                       (url query-alist)
      (find-url-and-respond url (breakup-pattern url-substrings) stream))))


;;;------------------------------------------------------------------- 
;;;
;;; SPECIALIZE URL NOT FOUND CONDITION
;;;

(defmethod report-status-message :around ((condition document-not-found) stream &optional format-string format-args)
  (flet ((inside-w4-p () ;;  Kludgey. 4/20/97 -- JCMa.
           (when (member :w4 *features*)
             (let ((url-stack (intern "*URL-STACK*" :w4)))
               (and (boundp url-stack) (symbol-value url-stack))))))
    (cond ((or (null *server*) 
               (server-proxy-request-p *server*) ;don't lose inside the proxy -- JCMa 4/25/2006
               (inside-w4-p));Don't lose on the web walker.
           (call-next-method))
          (t (let* ((status-code (status-code condition))
                    (reason (or (http-reason condition)
                                (get-string-for-status-code status-code)))
                    (url (http-url condition)))
               (unless format-string
                 (setq format-string (format-string condition)))
               (unless format-args
                 (setq format-args (format-args condition)))
               (with-cl-http-html-document (:declare-dtd-version-p :strict :stream stream)
                 (with-document-look-preamble (:stream stream)
                   (declare-title reason :stream stream)
                   (declare-base-reference url :stream stream))
                 (with-document-look-body (:heading reason :header-class "header.cl-http" :body-class "body.cl-http"
                                           :footer-class "footer.cl-http" :stream stream)
                   (with-paragraph (:stream stream)
                     (cond (format-string
                            (apply #'format stream format-string format-args))
                           (url (format stream "~A for URL ~A" reason (url:relative-name-string url)))
                           (t (format stream "~&Error ~D not properly reported.~&Please advise the server maintainer at ~A"
                                      (server-status *server*)
                                      (server-mail-address)))))
                   (etypecase url		;don't lose on URNs   8/19/98 -- JCMa.
                     ((or string url)
                      (horizontal-line :stream stream)
                      (with-section-heading ("Find URL" :stream stream)
                        (with-fillout-form (:post (url:relative-name-string #u"/cl-http/find-url.html") :stream stream)
                          (with-paragraph (:stream stream)
                            (with-rendition (:bold :stream stream)
                              (fresh-line stream)
                              (write-string "Match (Substring): " stream))
                            (accept-input 'string "URL-SUBSTRINGS" :size 72 :default "" :stream stream))
                          (submit-and-reset-buttons stream))))))))))))
