(in-package :cl-user)
(net.aserve:start :listeners 2 :port 8080 :DEBUG-STREAM nil :debug :all)

(defun blah (a b)
  (generate-blah a b))

(net.aserve:publish :path "/generate" 
                      :content-type "text/html"
                      :function
                    #'blah)

(defun generate-blah (req ent)
  (net.aserve:with-http-response (req ent)
    (net.aserve:with-http-body (req ent)
      (net.html.generator:html
       (:html
        :newline
        (:head 
         :newline
         (:title "foo")
         :newline)
        :newline
        (:body
         (:princ "bar"))
        :newline))))
  )

#|
HTTP/1.1 200  OK
Date: Wed, 04 Jun 2003 22:00:24 GMT
Connection: Close
Server: AllegroServe/1.2.27
Content-Type: text/html
Transfer-Encoding: chunked

41
<html>
<head>
<title>foo</title>
</head>
<body>bar</body>
</html>
0

|#

(defun blah-clhttp (url stream)
  (generate-blah-cl-http url stream))

(defun generate-blah-cl-http (url stream)
  (declare (ignore url))
  (http:with-successful-response (stream :html)
    (let ((title "foo"))
      (http::with-html-document (:stream stream)
        (html:with-document-preamble (:stream stream)
          (html:declare-title title :stream stream))
                               (html:with-standard-document-body (:stream stream)
                                 (format stream "bar"))))))
                                 
(http:export-url #u"/generate2"
            :computed
            :response-function #'blah-cl-http
            :expiration '(:no-expiration-header)
            :public t
            :no-cache t
            :language :en
            :keywords '(:cl-http :demo)
                 :documentation "test")

#|
HTTP/1.1 200 OK
Date: Wed, 04 Jun 2003 21:59:45 GMT
Server: CL-HTTP/70.159 (International Allegro CL Enterprise Edition; 1.6.3)
Content-type: text/html; charset=ISO-8859-1
Transfer-Encoding: chunked

42
<HTML>
<HEAD>
<TITLE>foo</TITLE>
</HEAD>
<BODY>bar</BODY>
</HTML>

0
#|

#|
TTP/1.1 200 OK
Date: Thu, 05 Jun 2003 20:36:21 GMT
Server: CL-HTTP/70.159 (LispWorks Personal Edition; 1.7.0)
Content-type: text/html; charset=ISO-8859-1
Transfer-Encoding: chunked

42
<HTML>
<HEAD>
<TITLE>foo</TITLE>
</HEAD>
<BODY>bar</BODY>
</HTML>

0
#|

(net.aserve:shutdown)

(http::stop)
