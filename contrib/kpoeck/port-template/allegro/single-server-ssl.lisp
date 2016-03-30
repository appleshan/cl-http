(in-package :http)

(defun start-examples-ssl ()
  (start :hostname "localhost.localdomain" :port 443 :type :not-yet)
  (start-serving "localhost.localdomain" 443 :type :stupid-multi)
  )

(start-examples-ssl)

(http::set-local-context "https://localhost.localdomain:443")

(defmacro http::with-successful-response-https ((stream content-type &key (status :success) bytes last-modification 
                                                character-set entity-tag expires location content-location
                                                content-language allow cache-control
                                                additional-headers additional-mime-headers
                                                (termination-line-p t) chunk-function footer-plist version) &body body)
  
  `(progn (http1::send-response ,stream ,content-type
                           :status ,status
                           :bytes ,bytes
                           :last-modification ,last-modification
                           :character-set ,character-set
                           :entity-tag ,(or entity-tag version)
                           :expires ,expires
                           :location ,location
                           :content-location ,content-location
                           :content-language ,content-language
                           :allow ,allow
                           :cache-control ,cache-control
                           :additional-headers ,additional-headers
                           :additional-mime-headers ,additional-mime-headers
                           :termination-line-p ,termination-line-p)
     ,@body))

(defun generate-test-cl-http (url stream)
  (declare (ignore url))
  (with-successful-response-https (stream :html)
    (let ((title "foo"))
      (with-html-document (:stream stream)
        (with-document-preamble (:stream stream)
          (declare-title title :stream stream))
        (with-standard-document-body (:stream stream)
          (format stream "bar"))))))

(defun generate-test-cl-http (url stream)
  (declare (ignore url))
  (with-successful-response (stream :html)
    (format stream "bar")))

(defun generate-test-cl-http (url stream)
  (declare (ignore url))
  (format stream "bar"))

(defun test-cl-http (url stream)
  (generate-test-cl-http url stream))
                                 
(export-url #U"/cl-http/generated"
            :computed
            :response-function #'test-cl-http
            :expiration '(:no-expiration-header)
            :public t
            :no-cache t
            :language :en
            :keywords '(:cl-http :demo)
            :documentation "test")

(export-url #U"/cl-http/license"
            :text-file
            :pathname "http:-license-.text" ;"http:port;translations.lisp"
            :expiration `(:interval 1)
            :public t
            :language :en
            :keywords '(:cl-http :documentation :license))

(export-url #u"/cl-http/yosemite-large.gif"
            :gif-image
            :pathname "http:www;cl-http;examples;yosemite-valley-l.gif"
            :expiration `(:interval ,(* 24. 60. 60.))
            :public t
            :keywords '(:cl-http :demo))

(export-url #u"/cl-http/video.mpeg"
            :mpeg-video
            :pathname "http:www;cl-http;examples;us-radar.mpg"
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :keywords '(:cl-http :demo))

(defun www-utils::wait-for-connection (socket)
  (www-utils::wait-for-ssl-connection socket))

(defun www-utils::local-protocol (http-stream)
  (declare (ignore http-stream))
  :https)