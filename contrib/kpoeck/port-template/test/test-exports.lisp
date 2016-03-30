(in-package :http-user)

(export-url #u"/license"
            :text-file
            :pathname "http:-license-.text" ;"http:port;translations.lisp"
            :expiration `(:interval 1)
            :public t
            :language :en
            :keywords '(:cl-http :documentation :license))

(export-url #u"/translations.lisp"
            :text-file
            :pathname "http:port;translations.lisp"
            :expiration `(:interval 1)
            :public t
            :language :en
            :keywords '(:cl-http :documentation :license))

(defun test-cl-http (url stream)
  (generate-test-cl-http url stream))

(defun generate-test-cl-http (url stream)
  (declare (ignore url))
  (with-successful-response (stream :html)
    (let ((title "foo"))
      (with-html-document (:stream stream)
        (with-document-preamble (:stream stream)
          (declare-title title :stream stream))
        (with-standard-document-body (:stream stream)
          (format stream "bar"))))))
                                 
(export-url #u"/generated"
            :computed
            :response-function #'test-cl-http
            :expiration '(:no-expiration-header)
            :public t
            :no-cache t
            :language :en
            :keywords '(:cl-http :demo)
                 :documentation "test")