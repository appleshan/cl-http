;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-user -*-

;;; Copyright John C. Mallery, 1995-1997, 2005-2006.
;;; All rights reserved.

;;;------------------------------------------------------------------- 
;;;
;;; EXPORT URLS COMPRISING THE CL-HTTP EXAMPLE WEB SITE
;;;
(in-package :http-user)

;;;------------------------------------------------------------------- 
;;;
;;; SET UP SOME SECURITY
;;;

(defparameter *trusted-subnets* #-CL-HTTP.Org`(,(local-host-ip-address)) #+CL-HTTP.Org nil
  "Subnets that we trust.  
A list of host names or IP addresses.
For IP addresses, zero is a wildcard for the component.
When null, access not controlled by subnet.")

#+CL-HTTP.Org
(defvar *ssl-port* 8444)

(defun secure-export-url (relative-url export-type &rest args)
  (declare (dynamic-extent args))
  #+CL-HTTP.Org
  (let* ((spec `(,relative-url :port ,*ssl-port* :protocol :https))
         (ssl-url (intern-url (merge-url spec (local-context)) :if-does-not-exist :create)))
    (declare (dynamic-extent spec))
    (apply #'export-url ssl-url export-type
           :authentication-realm :cl-http :capabilities :cl-http-users 
           args)
    ;; Replace this with redirect directory
    (export-url (intern-url (merge-url relative-url (local-context)) :if-does-not-exist :create)
                :redirect
                :alternate-urls (with-output-to-string (string) ;best to have a method which does this directly
                                 (http::write-url-remapping-context ssl-url string))
                :keywords '(:cl-http :distribution)))
  #-CL-HTTP.Org
  (apply #'export-url (intern-url (merge-url relative-url (local-context)) :if-does-not-exist :create)  export-type args))

;; Set up password control for source browsing
#+CL-HTTP.Org
(unless (intern-realm :cl-http :if-does-not-exist :soft)
  (add-realm :cl-http :digest)
  (add-group :cl-http :users)
  (add-access-control-group :cl-http-users
                            :cl-http
                            :capabilities '((:get :users)
                                            (:head :users)
                                            (:post :users)))
  #+ignore
  (add-user "JQHacker" 
            :cl-http
            :password ""
            :groups '(:users)
            :personal-name "John Q. Hacker"
            :email-address "JQHackert@myhost.com"))

;;;------------------------------------------------------------------- 
;;;
;;; EXPORT DOCUMENTATION URLS
;;;

;; Note that #U is a reader macro that completes a partial URL according
;; to the local context in which is it evaluated, see HTTP:MERGE-URL

(export-url #u"/cl-http/find-documentation.html"
            :html-computed-form
            :form-function #'http::compute-find-documentation-form
            :response-function #'http::respond-to-find-documentation
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation)
            :documentation "A form interface for looking up documentation for CL-HTTP.")

(export-url #u"/cl-http/find-documentation?"
            :search
            :response-function #'http::respond-to-find-documentation-search
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation)
            :documentation "A search URL interfaced into the same functions that respond to the find-documentation form.
To use, provide a symbol with a package prefix after the ?, e.g., [package]:[substring].
The search for [substring] will be performed in the package, [package]. 
A single colon will cause only external symbols to be returned, whereas a double colon
will retrieve both internal and external symbols.")

(export-url #u"/cl-http/show-documentation?"
            :search
            :response-function #'http::respond-to-show-documentation
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation)
            :documentation "Shows the documentation for a Lisp symbol in CL-HTTP.
To use, provide a package-prefixed name of a symbol in uppercase (unless slashified versions are desired).")

(export-url #u"/cl-http/find-url?"
            :search 
            :response-function #'http::respond-to-find-url
            :public t
            :language :en
            :keywords '(:cl-http :documentation)
            :documentation "Search for URLs containing a particular substring.")

(export-url #u"/cl-http/find-url.html"
            :html-computed-form
            :form-function #'http::compute-find-url-form
            :response-function #'http::respond-to-find-url-form
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation)
            :documentation "A form interface to search for URLs containing a particular substring.")

(export-url #u"/cl-http/describe-url?"
            :search
            :response-function #'http::respond-to-describe-url
            :keywords '(:cl-http :documentation)
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :documentation "Describe for a specific URL on the server.")

;;;------------------------------------------------------------------- 
;;;
;;; EXPORT LISPDOC REFERENCE MANUAL FOR CL-HTTP
;;;

(lispdoc:export-lispdoc-interface "CL-HTTP")

;;;------------------------------------------------------------------- 
;;;
;;; TWISTDOWN TREE EXPORTS
;;;

;; Export all the java binary .class files
(http:export-url #u"/cl-http/twistdown-tree/"
		 :directory
		 :pathname "http:examples;twistdown-tree;java;"
                 :immediate-export t
                 :expiration `(:interval ,(* 15. 60.))
                 :public t
                 :language :en
                 :keywords '(:cl-http :twistdown-tree))

(http:export-url #u"/cl-http/twistdown-tree/hdir/"
		 :directory
                 :recursive-p t
		 :pathname "http:examples;twistdown-tree;hdir;"
                 :expiration `(:interval ,(* 15. 60.))
                 :public t
                 :language :en
                 :keywords '(:cl-http :twistdown-tree :documentation))

(http:export-url #u"/cl-http/twistdown-tree/twistdown.html"
		 :computed
		 :response-function #'html4.0::compute-twistdown-tree-example
                 :expiration `(:interval ,(* 15. 60.))
                 :public t
                 :language :en
                 :keywords '(:cl-http :twistdown-tree))

;;;------------------------------------------------------------------- 
;;;
;;; Export CL-HTTP Example site
;;;

(export-url #u"/cl-http/"
            :directory
            :recursive-p t
            :pathname "http:www;cl-http;"
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation))

(export-url #u"/cl-http/release-notes.text"
            :text-file
            :pathname "http:http;-release-notes-.text"
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation :release-notes))

(export-url #u"/cl-http/license.text"
            :text-file
            :pathname "http:http;-license-.text"
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation :license)) 

;; obsolete here for backward compatibility
(export-url #u"/cl-http.html"
            :redirect
            :alternate-urls #u"/cl-http/cl-http.html"
            :keywords '(:cl-http :demo))

(export-url #u"/cl-http/frame-index.html"
            :computed
            :response-function #'write-cl-http-index-pane
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords `(:cl-http :documentation))

(export-url #u"/cl-http/frame-title.html"
            :computed
            :response-function #'write-cl-http-title-pane
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords `(:cl-http :documentation))

(export-url #u"/cl-http/frame.html"
            :computed
            :response-function #'write-cl-http-frame-set
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords `(:cl-http :documentation))

;; redirect top-level requests to the documentation.
(export-url #u"/"
            :computed
            :response-function #'redirect-to-documentation-root
	    :expiration '(:interval #.(* 60 15))	;cacheable for 15 minutes
            :keywords '(:cl-http :demo))

;; Provide the Website URL icon for use in bookmarks etc.
;; ICO is the windows icon format, which you can create using GraphicConverter from http://www.lemkesoft.com
(export-url #u"favicon.ico"
            :ico-image
            :pathname #p"http:www;cl-http;icons;lambda.ico"
            :cache-file-data t
            :file-data-revalidation (* 60 60 60) ;revalidate in-memory cache every hour
            :file-data-wired t
            :max-age (* 60 60 24) ;http clients recache every day
            :public t 
            :keywords '(:cl-http :demo)
            :documentation "The Website URL icon.")

;; For details of the robot exclusion standard, see: http://www.robotstxt.org/wc/robots.html
(export-url #u"robots.txt"
            :text-file
            :pathname #p"http:www;robots.text"
            :cache-file-data t
            :file-data-revalidation (* 60 60 60) ;revalidate in-memory cache every hour
            :max-age (* 60 60 24) ;http clients recache every day
            :public t 
            :keywords '(:cl-http :demo :robots)
            :documentation "Tell the robots how to navigate the site.")

;; provide a URL in the documentation directory in case the server root is
;; otherwise engaged.
(export-url #u"/cl-http/docs.html"
            :computed
            :response-function #'redirect-to-documentation-root
	    	    :expiration '(:interval #.(* 60 15))	;cacheable for 15 minutes
            :keywords '(:cl-http :demo))

;;;------------------------------------------------------------------- 
;;;
;;; MULTIMEDIA EXPORTS 
;;;

(export-url #u"/cl-http/yosemite.gif"
            :gif-image
            :pathname "http:www;cl-http;examples;yosemite-valley.gif"
            :expiration `(:interval ,(* 24. 60. 60.))
            :public t
            :keywords '(:cl-http :demo))

(export-url #u"/cl-http/yosemite-large.gif"
            :gif-image
            :pathname "http:www;cl-http;examples;yosemite-valley-l.gif"
            :expiration `(:interval ,(* 24. 60. 60.))
            :public t
            :keywords '(:cl-http :demo))

(export-url #u"/cl-http/audio.au"
            :basic-audio
            :pathname "http:www;cl-http;examples;us-vp2.au"
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :demo))

(export-url #u"/cl-http/video.mpeg"
            :mpeg-video
            :pathname "http:www;cl-http;examples;us-radar.mpg"
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :keywords '(:cl-http :demo)) 

(export-url #u"/cl-http/headers.html"
            :computed
            :response-function #'compute-headers-page
            :expiration '(:no-expiration-header)
            :public t
            :no-cache t
            :language :en
            :keywords '(:cl-http :demo)
            :documentation "Shows the headers sent by the client to the server.")

(export-url #u"/cl-http/computed-form.html"
            :html-computed-form
            :form-function #'compute-form
            :expiration '(:no-expiration-header)
            :response-function #'respond-to-computed-form
            :public t
            :language :en
            :keywords '(:cl-http :demo)
            :documentation "An example of copmuting the form html on the fly and responding to the resulting submissions.")

(export-url #u"/cl-http/icons/"
            :image-directory
            :pathname "http:www;cl-http;icons;"
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :keywords '(:cl-http :demo)
            :documentation "A directory of icons distributed with the server.")

(export-url #u"/cl-http/icons/index.html"
            :computed
            :response-function #'compute-icons-index
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :keywords '(:cl-http :demo)
            :documentation "An index of the icon directory which is computed on the fly.")
 
;;;------------------------------------------------------------------- 
;;;
;;; IMAGE MAPS
;;;

(export-url #u"/cl-http/image-maps/cern-shapes.gif?"
            :image-map
            :export-type :gif-image
            :pathname "http:www;cl-http;image-maps;cern-shapes.gif"
            :map-format :cern
            :map-pathname (pathname "http:www;cl-http;image-maps;cern-shapes.cern-map")
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :demo)
            :documentation "An example of a cern image-map.")

(export-url #u"/cl-http/image-maps/ncsa-shapes.gif?"
            :image-map
            :export-type :gif-image
            :pathname "http:www;cl-http;image-maps;ncsa-shapes.gif"
            :map-format :ncsa
            :map-pathname "http:www;cl-http;image-maps;ncsa-shapes.ncsa-map"
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :demo)
            :documentation "An example of an ncsa image-map.")

(export-url #u"/cl-http/image-maps/cern-shapes.cern-map"
            :text-file
            :pathname "http:www;cl-http;image-maps;cern-shapes.cern-map"
            :public t)

(export-url #u"/cl-http/image-maps/ncsa-shapes.ncsa-map"
            :text-file
            :pathname "http:www;cl-http;image-maps;ncsa-shapes.ncsa-map"
            :public t)

(export-url #u"/cl-http/server-structure/inspect-node?"
            :search
            :response-function #'respond-to-inspect-node
            :keywords '(:cl-http :documentation)
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :documentation "Describe a node on the basic-classes diagram.")

(export-url #u"/cl-http/server-structure/basic-classes.gif?"
            :image-map
            :export-type :gif-image
            :pathname "http:www;cl-http;server-structure;basic-classes.gif"
            :map-format :cern
            :map-pathname "http:www;cl-http;server-structure;basic-classes.cern-map"
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation)
            :documentation "Shows basic cl-http class structure.")

(export-url #u"/cl-http/yosemite-image-map.gif?"
            :gif-image
            :pathname "http:www;cl-http;examples;yosemite-valley.gif"
            :response-function #'respond-to-image-search
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :keywords '(:cl-http :demo)
            :documentation "An example of image search returning the coordinate where the user clicks.")

(export-url #u"/cl-http/server-interface-environment.html"
            :computed
            :response-function #'compute-server-interface-page
            :expiration '(:no-expiration-header)
            :public t
            :language :en
            :keywords '(:cl-http :demo)
            :documentation "Shows the Server Interface available within the macro HTTP:WITH-SERVER-INTERFACE-ENVIRONMENT.")

(export-url #u"/cl-http/cgi-environment.html"
            :computed
            :response-function #'compute-cgi-variables-page
            :expiration '(:no-expiration-header)
            :public t
            :language :en
            :keywords '(:cl-http :demo)
            :documentation "Shows the CGI Variables available within the macro HTTP:WITH-CGI-ENVIRONMENT.")

;;;------------------------------------------------------------------- 
;;;
;;;  DIRECTORY EXPORTS
;;;

(export-url #u"/cl-http/sources/examples/"
            :lisp-directory
            :pathname "http:examples;*.lisp"
            :recursive-p t
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation)
            :documentation "Example Lisp files showing configuration of the server and export of URLs.")

(export-url #u"/cl-http/sources/mcl/examples/"
            :lisp-directory
            :pathname "http:mcl;examples;*.lisp"
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation)
            :documentation "Example Lisp files for the MAC that show configuration of the server and export of URLs.")

;; export the directory containing standards relevant to the world wide web.
(export-url #u"/cl-http/standards/"
            :text-directory
            :pathname "http:standards;*.text"
            :recursive-p t
            :expiration `(:interval ,(* 182. 24. 60. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation)
            :documentation "The set of standards documents on which the World Wide Web is based.
Read the ones on HTTP and HTML if you want to learn more about this server and be able to extend it.")

;; export portable common lisp sources.
(secure-export-url "/cl-http/sources/common-lisp/"
                   :lisp-directory
                   :pathname "http:server;*.lisp"
                   :expiration `(:interval ,(* 24. 60. 60.))
                   :private t
                   :proxy-revalidate t
                   :secure-subnets *trusted-subnets*
                   :language :en
                   :keywords '(:cl-http :documentation)
                   :documentation "The portable Lisp source for CL-HTTP.")

;; Export portable CLIM interface code
(secure-export-url "/cl-http/sources/clim/"
                   :lisp-directory
                   :pathname "http:clim;*.lisp"
                   :recursive-p t
                   :expiration `(:interval ,(* 24. 60. 60.))
                   :private t
                   :proxy-revalidate t
                   :secure-subnets *trusted-subnets*
                   :language :en
                   :keywords '(:w3p :cl-http :documentation)
                   :documentation "The CLIM interface source for CL-HTTP.")

;; Export the client code.
(secure-export-url "/cl-http/sources/client/"
                   :lisp-directory
                   :pathname "http:client;*.lisp"
                   :expiration `(:interval ,(* 24. 60. 60.))
                   :private t
                   :proxy-revalidate t
                   :secure-subnets *trusted-subnets*
                   :language :en
                   :keywords '(:cl-http :documentation)
                   :documentation "The portable Lisp source for the basic HTTP client.")

(secure-export-url "/cl-http/sources/proxy/"
                   :lisp-directory
                   :pathname "http:proxy;*.lisp"
                   :recursive-p t
                   :expiration `(:interval ,(* 24. 60. 60.))
                   :private t
                   :proxy-revalidate t
                   :secure-subnets *trusted-subnets*
                   :language :en
                   :keywords '(:cl-http :documentation)
                   :documentation "The portable Lisp source for the HTTP proxy.")

(secure-export-url "/cl-http/sources/smtp/"
                   :lisp-directory
                   :pathname "http:smtp;*.lisp"
                   :expiration `(:interval ,(* 24. 60. 60.))
                   :private t
                   :proxy-revalidate t
                   :secure-subnets *trusted-subnets*
                   :language :en
                   :keywords '(:cl-http :documentation)
                   :documentation "The portable Lisp source for sending mail via SMTP.")

(secure-export-url "/cl-http/sources/ftp/"
                   :lisp-directory
                   :pathname "http:ftp;*.lisp"
                   :expiration `(:interval ,(* 24. 60. 60.))
                   :private t
                   :proxy-revalidate t
                   :secure-subnets *trusted-subnets*
                   :language :en
                   :keywords '(:cl-http :documentation)
                   :documentation "The portable Lisp source for transfering files via FTP.")

;; Export W4 Constraint Guided Web Walker
(secure-export-url "/cl-http/sources/w4/"
                   :lisp-directory
                   :pathname "http:w4;*.lisp"
                   :expiration `(:interval ,(* 24. 60. 60.))
                   :private t
                   :proxy-revalidate t
                   :secure-subnets *trusted-subnets*
                   :language :en
                   :keywords '(:w4 :cl-http :documentation)
                   :documentation "The W4 Constraint-Guided Web Walker for CL-HTTP.")

;; Export W3 Presentation System
(secure-export-url "/cl-http/sources/w3p/"
                   :lisp-directory
                   :pathname "http:w3p;*.lisp"
                   :expiration `(:interval ,(* 24. 60. 60.))
                   :private t
                   :proxy-revalidate t
                   :secure-subnets *trusted-subnets*
                   :language :en
                   :keywords '(:cl-http :documentation)
                   :documentation "The W3p Presentation System for CL-HTTP.")

;; Export HTML Parser
(secure-export-url "/cl-http/sources/html-parser/"
                   :directory
                   :pathname "http:html-parser;v11;*.lisp"
                   :expiration `(:interval ,(* 24. 60. 60.))
                   :private t
                   :proxy-revalidate t
                   :secure-subnets *trusted-subnets*
                   :language :en
                   :keywords '(:html-parser :cl-http :documentation)
                   :documentation "An HTML parser for W4.")

;; Export the LambdaVista System
(secure-export-url "/cl-http/sources/lambda-ir/"
                   :lisp-directory
                   :pathname "http:lambda-ir;*.lisp"
                   :recursive-p t
                   :expiration `(:interval ,(* 24. 60. 60.))
                   :private t
                   :proxy-revalidate t
                   :secure-subnets *trusted-subnets*
                   :language :en
                   :keywords '(:cl-http :documentation)
                   :documentation "The LambdaVista automatic site indexing for CL-HTTP.") 

;;;------------------------------------------------------------------- 
;;;
;;; PORT SOURCES
;;;

;; export lw-specific sources.
(secure-export-url "/cl-http/sources/lw/"
                   :lisp-directory
                   :recursive-p t
                   :pathname (pathname "http:lw;*.lisp")
                   :expiration `(:interval ,(* 15. 60.))
                   :private t
                   :proxy-revalidate t
                   :secure-subnets *trusted-subnets*
                   :keywords '(:cl-http :documentation :lispworks))

;; export MAC-specific sources.
(secure-export-url "/cl-http/sources/mcl/"
                   :lisp-directory
                   :pathname "http:mcl;*.lisp"
                   :expiration `(:interval ,(* 24. 60. 60.))
                   :private t
                   :proxy-revalidate t
                   :secure-subnets *trusted-subnets*
                   :recursive-p t
                   :language :en
                   :keywords '(:cl-http :documentation)
                   :documentation "The platform-specific sources that allow CL-HTTP to run on the MAC.")

;; export Lisp Machine specific sources.
(secure-export-url "/cl-http/sources/lispm/"
                   :lisp-directory
                   :pathname "http:lispm;*.lisp"
                   :recursive-p t
                   :expiration `(:interval ,(* 24. 60. 60.))
                   :private t
                   :proxy-revalidate t
                   :secure-subnets *trusted-subnets*
                   :language :en
                   :keywords '(:cl-http :documentation)
                   :documentation "The platform-specific sources that allow CL-HTTP to run on the Lisp Machine.")

;; export cmucl-specific sources.
(secure-export-url "/cl-http/sources/cmucl/"
                   :lisp-directory
                   :recursive-p t
                   :pathname (pathname "http:cmucl;*.lisp")
                   :expiration `(:interval ,(* 15. 60.))
                   :private t
                   :proxy-revalidate t
                   :secure-subnets *trusted-subnets*
                   :keywords '(:cl-http :documentation :cmu-cl))

;; export ACL-specific sources.
(secure-export-url "/cl-http/sources/acl/obc/"
                   :lisp-directory
                   :recursive-p t
                   :pathname (pathname "http:acl;obc;*.lisp")
                   :expiration `(:interval ,(* 15. 60.))
                   :private t
                   :proxy-revalidate t
                   :secure-subnets *trusted-subnets*
                   :keywords '(:cl-http :documentation :allegro))

;; export ACL-specific sources.
(secure-export-url "/cl-http/sources/clim/clim-sys/"
                   :lisp-directory
                   :recursive-p t
                   :pathname (pathname "http:clim;clim-sys;*.lisp")
                   :expiration `(:interval ,(* 15. 60.))
                   :private t
                   :proxy-revalidate t
                   :secure-subnets *trusted-subnets*
                   :keywords '(:cl-http :documentation :allegro))

;; export ACL-specific sources.
(secure-export-url "/cl-http/sources/acl/jkf/"
                   :lisp-directory
                   :recursive-p t
                   :pathname (pathname "http:acl;jkf;*.lisp")
                   :expiration `(:interval ,(* 15. 60.))
                   :private t
                   :proxy-revalidate t
                   :secure-subnets *trusted-subnets*
                   :keywords '(:cl-http :documentation :allegro))

;; Export SCL specific sources.
(secure-export-url "/cl-http/sources/scl/"
                   :lisp-directory
                   :recursive-p t
                   :pathname (pathname "http:scl;*.lisp")
                   :expiration `(:interval ,(* 15. 60.))
                   :private t
                   :proxy-revalidate t
                   :secure-subnets *trusted-subnets*
                   :keywords '(:cl-http :documentation :scl))

;; export lcl-specific sources.
(secure-export-url "/cl-http/sources/lcl/"
                   :lisp-directory
                   :recursive-p t
                   :pathname (pathname "http:lcl;*.lisp")
                   :expiration `(:interval ,(* 15. 60.))
                   :private t
                   :proxy-revalidate t
                   :secure-subnets *trusted-subnets*
                   :keywords '(:cl-http :documentation :lucid))



;;;------------------------------------------------------------------- 
;;;
;;; CONTRIBUTIONS
;;;

(secure-export-url "/cl-http/contributions/"
                   :directory
                   :pathname "http:contrib;"
                   :recursive-p t
                   :expiration `(:interval ,(* 15. 60.))
                   :private t
                   :proxy-revalidate t
                   :secure-subnets *trusted-subnets*
                   :language :en
                   :keywords '(:cl-http :contributions)
                   :documentation "Contributions containing code deemed generally useful for CL-HTTP hackers.")

(secure-export-url "/cl-http/contributions/acl/"
                   :directory
                   :pathname "http:acl;contrib;"
                   :recursive-p t
                   :expiration `(:interval ,(* 15. 60.))
                   :private t
                   :proxy-revalidate t
                   :secure-subnets *trusted-subnets*
                   :language :en
                   :keywords '(:cl-http :acl :contributions)
                   :documentation "Contributions containing code deemed generally useful for Franz Allegro CL-HTTP hackers.")

(secure-export-url "/cl-http/contributions/lw/"
                   :directory
                   :pathname "http:lw;contrib;"
                   :recursive-p t
                   :expiration `(:interval ,(* 15. 60.))
                   :private t
                   :proxy-revalidate t
                   :secure-subnets *trusted-subnets*
                   :language :en
                   :keywords '(:cl-http :lw :contributions)
                   :documentation "Contributions containing code deemed generally useful for LispWorks CL-HTTP hackers.")

(secure-export-url "/cl-http/contributions/mcl/"
                   :directory
                   :pathname "http:mcl;contrib;"
                   :recursive-p t
                   :expiration `(:interval ,(* 15. 60.))
                   :private t
                   :proxy-revalidate t
                   :secure-subnets *trusted-subnets*
                   :language :en
                   :keywords '(:cl-http :mac :contributions)
                   :documentation "Contributions containing code deemed generally useful for Apple Macintosh CL-HTTP hackers.")

(secure-export-url "/cl-http/contributions/scl/"
                   :directory
                   :pathname "http:scl;contrib;"
                   :recursive-p t
                   :expiration `(:interval ,(* 15. 60.))
                   :private t
                   :proxy-revalidate t
                   :secure-subnets *trusted-subnets*
                   :language :en
                   :keywords '(:cl-http :scl :contributions)
                   :documentation "Contributions containing code deemed generally useful for the Scieneer CL CL-HTTP hackers.")

;;;------------------------------------------------------------------- 
;;;
;;; EXPORTS FOR AUTHENTICATION EXAMPLES USING BASIC METHOD
;;;

;; Call DEFINE-ACCESS-CONTROL-REALMS in your configuration.lisp file.
;; ADD-REALM is used here to avoid clobbering existing authentication realms

(add-realm :minimum-security  :basic)
(add-realm :system-admin  :basic)

;; Some groups within realms must be created before references are made to
;; them in user objects.

(add-groups :minimum-security :users :developers)
(add-group :system-admin :system-hackers)

;; Set up a number of users, assigning realms and groups
(add-user "ernest" :minimum-security
          :password "ernest-foo"
          :groups '(:users)
          :personal-name "Ernest Jones"
          :email-address "ernest@foo.com")

(add-user "jim" :minimum-security
          :password "jim-foo"
          :groups '(:developers :users)
          :personal-name "Jim Thompson"
          :email-address "jim@foo.com")

(add-user "frank" :minimum-security
          :password "frank-foo"
          :groups '(:developers :users)
          :personal-name "Frank Smith"
          :email-address "frank@foo.com")

(add-user "luke" :system-admin
          :password "luke-foo"
          :groups '(:system-hackers)
          :personal-name "Luke Miller"
          :email-address "luke@foo.com")

;; Define some capabilities

;; A set of capabilities giving the :developers group basic access
(add-access-control-group :minimum-security-developers
                          :minimum-security
                          :capabilities '((:get :developers)
                                          (:head :developers)
                                          (:post :developers)))
  

;; A set of capabilities allowing anyone named "frank" to do everything but delete or unlink. 
(add-access-control-group :minimum-security-frank-capabilities
                          :minimum-security
                          :capabilities '((:delete)     ;frank can't delete URLs
                                          (:default "frank")))

;; Export some access-controlled urls
(export-url #u"/cl-http/authentication/minimum.html"
            :computed
            :response-function #'http:display-url-authentication-status
            :authentication-realm :minimum-security
            :capabilities nil   ;;no capabilities means anyone in the realm has access
            :private t
            :proxy-revalidate t
            :language :en
            :expiration '(:no-expiration-header)
            :keywords '(:cl-http :authentication :demo))

(export-url #u"/cl-http/authentication/minimum-developers.html"
            :computed
            :response-function #'http:display-url-authentication-status
            :authentication-realm :minimum-security
            ;; the keyword denotes a access-control group created earlier
            :capabilities :minimum-security-developers
            :expiration '(:no-expiration-header)
            :private t
            :proxy-revalidate t
            :language :en
            :keywords '(:cl-http :authentication :demo))

(export-url #u"/cl-http/authentication/minimum-frank.html"
            :computed
            :response-function #'http:display-url-authentication-status
            :authentication-realm :minimum-security
            ;; the keyword denotes a access-control group created earlier
            :capabilities :minimum-security-frank-capabilities
            :expiration '(:no-expiration-header)
            :private t
            :proxy-revalidate t
            :language :en
            :keywords '(:cl-http :authentication :demo))

(export-url #u"/cl-http/authentication/system-hackers.html"
            :computed
            :response-function #'http:display-url-authentication-status
            :authentication-realm :system-admin
            ;; Method capabilties suitable for add-access-control-group are
            ;; passed in directly.
            :capabilities '((:default :system-hackers))  ;;system-hackers do everything
            :expiration '(:no-expiration-header)
            :private t
            :proxy-revalidate t
            :language :en
            :keywords '(:cl-http :authentication :demo))


;;;------------------------------------------------------------------- 
;;;
;;; EXPORTS FOR AUTHENTICATION EXAMPLES USING MD5 DIGEST METHOD
;;;

;; ADD-REALM a new authentication realm using the digest authentication scheme.
(add-realm :digest-realm :digest)

;; The realm must contain groups before user objects refer to them.
(add-groups :digest-realm :members :elite-members)

;; Set up two users, assigning realms and groups.
(add-user "mike" :digest-realm
          :password "mike-foo"
          :groups '(:members)
          :personal-name "Mike Smith"
          :email-address "mike@foo.com")

(add-user "joe" :digest-realm
          :password "joe-foo"
          :groups '(:members :elite-members)
          :personal-name "Joe Doe"
          :email-address "joe@foo.com")

;; Define a set of capabilities giving the :elite-members group basic access
(add-access-control-group :elite-members-access
                          :digest-realm
                          :capabilities '((:get :elite-members)
                                          (:head :elite-members)))

;; Export some access-controlled urls
(export-url #u"/cl-http/authentication/members.html"
            :computed
            :response-function #'http:display-url-authentication-status
            :authentication-realm :digest-realm
            :capabilities nil ;;no capabilities means anyone in the realm has access
            :expiration '(:no-expiration-header)
            :private t
            :proxy-revalidate t
            :language :en
            :keywords '(:cl-http :authentication :demo))

(export-url #u"/cl-http/authentication/elite-members.html"
            :computed
            :response-function #'http:display-url-authentication-status
            :authentication-realm :digest-realm
            :capabilities :elite-members-access
            :expiration '(:no-expiration-header)
            :private t
            :proxy-revalidate t
            :language :en
            :keywords '(:cl-http :authentication :demo)) 

;;;------------------------------------------------------------------- 
;;;
;;; EXAMPLE OF EXPORTING A DIRECTORY ALLOWING THE HTTP PUT METHOD
;;;

#|(defun local-host-subnet-ip-address ()
   (let* ((ip (local-host-ip-address))
             (mask (position #\. ip :from-end t)))
      (concatenate 'string (subseq ip 0 mask) ".0")))

;; See the documentation on security policies in the authentication documentation.
;; The PUT method requires access control to protect you from undesirables.
(export-url #u"/cl-http/users/"
            :directory
            :recursive-p t
            :pathname "http:www;cl-http;users;"
            :expiration `(:interval ,(* 15. 60.))
            :secure-subnets (list (local-host-subnet-ip-address))
            :keywords '(:cl-http :documentation))|# 

;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(export-url #u"/cl-http/choose-color-scheme.html"
            :html-computed-form
            :form-function #'write-form-for-choose-color-scheme
            :response-function #'respond-to-choose-color-scheme
            :public t
            :no-cache t
            :language :en
            :documentation "Choose color schemes for Netscape compatible clients."
            :keywords '(:cl-http :demo))

#+W3P
(export-url #u"/cl-http/mix-colors.html"
            :html-computed-form
            :form-function #'write-form-for-mix-colors
            :response-function #'respond-to-mix-colors
            :public t
            :no-cache t
            :language :en
            :documentation "Mix a new color using RGB."
            :keywords '(:cl-http :demo))

(http:export-url #u"/cl-http/image-maps/show-client-side-image-map.html"
                 :computed
                 :response-function #'respond-to-show-client-side-image-map
                 :expiration `(:interval ,(* 15. 60.))
                 :public t
                 :language :en
                 :keywords '(:cl-http :demo)
                 :documentation "An example of client side image maps.")

(http:export-url #u"/cl-http/image-maps/show-client-side-image-map-frame.html"
                 :computed
                 :response-function #'respond-to-show-client-side-image-map-frame
                 :expiration `(:interval ,(* 15. 60.))
                 :public t
                 :language :en
                 :keywords '(:cl-http :demo)
                 :documentation "An example of client side image maps.")

(http:export-url #u"/cl-http/show-frame-layout.html"
                 :computed
                 :response-function #'respond-to-show-frame-layout
                 :expiration `(:interval ,(* 15. 60.))
                 :public t
                 :language :en
                 :keywords '(:cl-http :demo)
                 :documentation "An example of frames using client side image maps.")

(export-url #u"/cl-http/computed-cookie-form.html"
            :html-computed-form
            :form-function #'compute-cookie-form
            :response-function #'respond-to-compute-cookie-form
            :expiration '(:no-expiration-header)
            :public t
            :no-cache t
            :language :en
            :keywords '(:cl-http :demo)
            :documentation "An example of Netscape cookies.")

(export-url #u"/cl-http/welcome.talk"
            :computed
            :response-function #'computed-welcome-message
            :public t
            :language :en
            :keywords '(:cl-http :demo :plug-in)
            :documentation "An example of generating inline speech for a client-side plug-in.")

(export-url #u"/cl-http/marquee.html"
            :computed
            :response-function #'compute-marquee-headers-page
            :expiration '(:no-expiration-header)
            :public t
            :language :en
            :keywords '(:cl-http :demo)
            :documentation "CL-HTTP rules ... This Marquee implemented with JavaScript ... 
by emitting appropriate code and calls from ... Common Lisp ...")

(export-url #u "/cl-http/examples/layers/images/flowers/"
            :image-directory
            :pathname "http:examples;layers;images;"
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :keywords '(:cl-http :example :layers))

(http:export-url #u"/cl-http/layers.html"
                 :computed
                 :response-function #'compute-layered-images
                 :public t
                 :no-cache t
                 :language :en
                 :keywords '(:cl-http :demo)
                 :documentation "Shows an examples of Netscape 4.0 layers and JavaScript 1.2 emission.")

;; Be careful to set the access control correctly to avoid unwanted security
;; breaches. We recommend that you specify secure subnets for safest
;; operation.
#+Web-Lisp-Listener
(export-url #u"/cl-http/listener.html"
            :html-computed-form
            :form-function #'write-listener-form
            :expiration '(:no-expiration-header)
            :response-function #'read-eval-print-form
	    :secure-subnets #+CSAIL-Site '("128.52.0.0") #-CSAIL-Site (list (local-host-ip-address))
            :private t
            :language :en
            :keywords '(:cl-http :demo)
            :documentation "A CL-HTTP Lisp listener that runs over the Web.")
;;;------------------------------------------------------------------- 
;;;
;;; EXPORT THE VRML 1.0 EXAMPLES
;;;

#+VRML-1.0-Examples
(vrml-test:export-vrml-examples)

;;;------------------------------------------------------------------- 
;;;
;;; EXPORT THE URLS NEEDED FOR PORT-SPECIFIC DOCUMENTATION
;;;

;; Bullet proofing for the user who deletes the ACL directory inadvertently.
(http:load-files-safely '("http:acl;obc;examples;exports.lisp"
                          "http:acl;jkf;examples;exports.lisp"
                          "http:lw;examples;exports.lisp"
                          "http:lcl;examples;exports.lisp"
                          "http:cmucl;examples;exports.lisp"
                          "http:scl;examples;exports.lisp"))

;;;------------------------------------------------------------------- 
;;;
;;;  Export the urls for the CL-HTTP paper from WWW-94
;;;

(export-url #u"/projects/iiip/doc/cl-http/"
            :directory
            :pathname "http:www;cl-http;www94;code-example.html"
            :public t
            :language :en
            :keywords '(:cl-http :demo :www94)
            :documentation "The paper describing CL-HTTP at the First International Conference on the World Wide Web.") 

;;;------------------------------------------------------------------- 
;;;
;;; EXPORT MAINTENANCE URLs
;;;

;; Log Window and Web Configuration URL
;; For a the defined activities, see define-log-window-activities in http:examples;log-window.lisp
#-CSAIL-Site
(export-log-window
 :keywords '(:cl-http :maintenance)
 ;; Webmaster access only on server
 :authentication-realm :server
 :authentication :server
 :capabilities :webmasters)

;; Export access control management interface
#+ignore
(export-access-control-interface 
 :authentication-realm :server
 :capabilities :webmasters
 :secure-subnets `(,(local-host-ip-address)))

;; export the log files for the server.
#-(or CSAIL-Site CL-HTTP.Org)
(export-url #u"/cl-http/logs/"
            :text-directory
            :pathname "http:logs;*.text"
            :recursive-p t
            :expiration '(:no-expiration-header)
            :keywords '(:cl-http :demo)
            :documentation (format nil "The log files for CL-HTTP running on ~A." (www-utils:local-host-domain-name))
            ;; Webmaster access only on server
            :authentication-realm :server
            :capabilities :webmasters
            :secure-subnets `(,(local-host-ip-address))
            :private t
            :proxy-revalidate t
            :language :en)

#-(or CSAIL-Site CL-HTTP.Org)
(export-url #u"/cl-http/pw/"
            :lisp-directory
            :pathname "http:pw;"
            :recursive-p t
            :recache t
            :keywords '(:cl-http :maintenance)
            ;; Webmaster access only on server
            :authentication-realm :server
            :capabilities :webmasters
            :secure-subnets `(,(local-host-ip-address))
            :private t
            :proxy-revalidate t
            :language :en)

;;Export the entire distribution directory hierarchy
#-(or CSAIL-Site CL-HTTP.Org)
(export-url #u"/cl-http/distribution/log/pw/"
            :directory
            :pathname "http:pw;"
            :recursive-p t
            :recache t
            :keywords '(:cl-http :distribution)
            ;; Webmaster access only on server
            :authentication-realm :server
            :capabilities :webmasters
            :secure-subnets `(,(local-host-ip-address))
            :private t
            :proxy-revalidate t
            :language :en)

#-(or CSAIL-Site CL-HTTP.Org)
(secure-export-url"/cl-http/distribution/"
                  :directory
                  :pathname "http:http;"
                  :recursive-p t
                  :recache nil
                  :private t
                  :proxy-revalidate t
                  :secure-subnets *trusted-subnets*
                  :language :en
                  :keywords '(:cl-http :distribution))

;;;------------------------------------------------------------------- 
;;;
;;; W3P DOCUMENTATION
;;;

;; The supporting code is loaded at the top of this file.
;; Export documentation for w3p
#+W3P
(http:export-url #u"/cl-http/w3p/basic-function?"
                 :search
                 :response-function #'w3p::respond-to-basic-functions
                 :expiration `(:interval ,(* 15. 60.))
                 :public t
                 :language :en
                 :keywords '(:cl-http :w3p :documentation)
                 :documentation "describe a basic w3p function")

#+W3P
(http:export-url #u"/cl-http/w3p/presentation-function?"
                 :search
                 :response-function #'w3p::respond-to-presentation-functions
                 :expiration `(:interval ,(* 15. 60.))
                 :public t
                 :language :en
                 :keywords '(:cl-http :w3p :documentation)
                 :documentation "describe a w3p presentation function")

#+W3P
(http:export-url #u"/cl-http/w3p/presentation-type?"
                 :search
                 :response-function #'w3p::respond-to-presentation-types
                 :expiration `(:interval ,(* 15. 60.))
                 :public t
                 :language :en
                 :keywords '(:cl-http :w3p :documentation)
                 :documentation "describe a w3p presentation type")


;;;------------------------------------------------------------------- 
;;;
;;; PROXY INTERFACE
;;;

#+CL-HTTP-PROXY
(when (member :cl-http-proxy *features*)
  (http:export-proxy-interfaces :secure-subnets `(,(local-host-ip-address))))

#+Lambda-IR
(when (member :lambda-ir *features*)
  (http:export-lambdavista-search-page))

;; Start the server by evaluating the form: (enable-http-service)

;; Enable services here
#-CSAIL-Site
(enable-http-service)
#+(and CL-HTTP-PROXY (not CSAIL-Site))
(enable-proxy-service)