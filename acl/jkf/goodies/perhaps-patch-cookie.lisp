(in-package :http-user)

;;; If domain is localhost, consider it legal and don't send the domain part of the cookie

(defmethod respond-to-compute-cookie-form ((url url:http-form) stream query-alist)
  (flet ((clean-up (item)
                   (and item                            ; don't let NIL through
                        (not (null-string-p (setq item (string-trim '(#\space #\tab #\return #\Linefeed) item))))
                        item))
         (local-domain ()
                       (let ((host-name (local-host-domain-name)))
                         (let ((pos (position #\. (local-host-domain-name))))
                           (if pos
                               (subseq host-name (1+ pos))
                             ;;; this name will not be valid, but at least the server is not crashing 
                             host-name))))
         (expires (expires delete-p)
                  (cond ((equalp delete-p "yes")
                         (- (get-universal-time) (* 60 60)))
                        (expires
                         (parse-gmt-time expires))
                        (t (+ (get-universal-time) (* 60 60))))))
    (bind-query-values (name value domain path expires delete-p) (url query-alist)
                       (let* ((name (clean-up name))
                              (value (clean-up value))
                              (domain (clean-up domain))
                              (path (clean-up path))
                              (expires (clean-up expires))
                              (delete-p (clean-up delete-p))
                              (headers (when (and name value)
                                         ;; construct the cookie setting header  using the defined interface.
                                         (http:set-cookie-http-headers ((http::intern-keyword name) value
                                                                        :expires (expires expires delete-p)
                                                                        :domain (or domain (local-domain))
                                                                        :path path)))))
                         (declare (dynamic-extent headers))
                         (setq *der* headers)
                         (with-successful-response (stream :html :content-location url :expires (url:expiration-universal-time url)
                                                           :cache-control (url:response-cache-control-directives url)
                                                           :content-language (languages url)
                                                           :additional-headers headers)
                           ;; generate another version of the form with the new values.
                           (write-compute-cookie-form url stream))))))

(defun http::valid-domain-name-string-p (hostname &optional (start 0) (end (length hostname)) )
  "Returns non-null if HOSTNAME is a valid internet domain name."
  (flet ((illegal-char-p (char)
           (member char '(#\% #\space #\tab #\return) :test #'eql)))
    (declare (inline illegal-char-p))
    (or
     (and (http::char-position #\. hostname start end t)
          (not (find-if #'illegal-char-p hostname :start start :end end)))
     (string-equal hostname "localhost"))))

(define make-set-cookie-header-value (name value &key expires domain path secure)
  "Creates a header value for use with the :SET-COOKIE header
  that will store a cookie named NAME with value VALUE on a client.  This value
  created with this function should be passed as the value of :SET-COOKIE using
  the ADDITIONAL-HEADERS argument to WITH-SUCCESSFUL-RESPONSE, and related
  macros.

  EXPIRES is a universal time when the cookie expires. DOMAIN is the server
  domain name for which the cookie is valid, defaults to the server host name.
  PATH is a relative URL denoting the range of URLs for DOMAIN for which the
  cookie is valid. The client tests to see if the current URL is spanned by
  PATH. PATH defaults to /. SECURE is a boolean value indicating whether the
  cookie should sent over insecure connections (i.e., non-SSL).

  For each cookie, the name and the value must not exceed 4k bytes. Each domain
  name is limited to 20 cookies. When the 300 total cookies per client or 20
  cookies per domain name limit are exceeded, cookies are deleted by clients
  according to least recent usage. Servers may force cookies to be deleted by
  providing an expiration that is in the past.

  Applications may wish to use WRITE-TO-ARMOR-PLATED-STRING and
  READ-FROM-ARMOR-PLATED-STRING to protect lisp forms stored in the client.
  However, this encoding reduces the amount of data that can be store in a
  cookie by approximately 25 percent. Alternatively,
  STRING-ESCAPE-SPECIAL-CHARS and STRING-UNESCAPE-SPECIAL-CHARS may be used as a
  transfer encoding. "
  (check-type name keyword)
  (check-type domain (or null string))
  (check-type path (or null string (satisfies url-p)))
  (unless (> 4001 (+ (the fixnum (length (symbol-name name))) (the fixnum (length value))))
    (error "The combined size of NAME and VALUE exceed 4k bytes."))
  (let ((args nil))
    (cond-every
      (secure
        (push t args)
        (push :secure args))
      (path
        (check-type path (or null string (satisfies url-p)))
        (push path args)
        (push :path args))
      (domain
        (let ((string (etypecase domain
                        (symbol (symbol-name domain))
                        (string domain))))
          (unless (valid-domain-name-string-p string)
            (error "The domain name, ~A, is not valid." string))
          (unless (string-equal "localhost" string)
          (push string args)
          (push :domain args))))
      (expires
        (check-type expires integer)
        (push expires args)
        (push :expires args)))
    `(,name ,value ,.args)))