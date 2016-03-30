;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: cl-user; Base: 10 -*-
;;
;; start.lisp
;;
;; this shows one way to load and start cl-http on acl.  
;; It always starts the server on port 8000 since this will work regardless
;; of whether you're on Windows or Unix.
;;
;; See the -read-me-.text file for alternative ways.
;; 

(in-package :cl-user)

;;; KAP 2003-05-03
;;; Allow the file to be loaded without :cd
;;; You need to load the file, otherwise *load-pathname* will not be set
(load (make-pathname :name "load" :type "lisp" :defaults *load-pathname*))

(let ((comp:*cltl1-compile-file-toplevel-compatibility-p* t)
      (*compile-print* nil)
      (*compile-verbose* nil)
      (*load-verbose* t)
      (*load-print* nil))
  (build-cl-http)
  (compile-exports))

#|
Without dns, try 
(pushnew :really-local *features*)
|#

;;; to let the variables might have funny effects
;;; better set them globally


(let ((host #-really-local "localhost" #+really-local "127.0.0.1")
      (port 8000))
  (progn
    (setq 
     http:*log-access-log-class* 'http:console-log
      html::*standard-color-scheme* (html:create-color-scheme :background :white
                                                              :foreground :black
                                                              :link :blue
                                                              :visited-link :purple
                                                              :active-link :red)
      http:*resolve-ip-addresses* #-really-local t #+really-local nil
      http:*log-resolve-ip-addresses* nil
      url:*url-host-name-resolution* :never
      )
    (ipc:compute-host-domain-name host)  
    (http:ensure-current-log port)
    (http:start 
     :port port
     :host host
     :log t
     :listeners 5)
    )
  )

; to start with proxy on Unix...
; (http:start :port 8000 :proxy-port 8080 :proxy t)
; Not sure whether the proxy works in this port

(setq *features* (remove :CL-HTTP-PROXY *features*))

(load-test)
:done
#|

With IE try http://localhost:8000/cl-http/frame.html
with Netscape try http://localhost:8000/cl-http/cl-http.html

(http:disable-proxy-service) ; still needs to be debugged
(http::stop)


;;; lambdavista loading

(let ((.files-loaded. nil)) 
  (compile-and-load-files 
   '(("http:lambda-ir;examples;" "lambdavista" "stemming" "lambdavista-exports"))))

(http:export-lambdavista-search-page)
|#








