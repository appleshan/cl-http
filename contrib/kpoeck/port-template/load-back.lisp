;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: cl-user; Base: 10 -*-

(in-package "CL-USER")


(eval-when (:load-toplevel :compile-toplevel :execute)
;;; Comment to test with string-streams
  (push :no-stubs *features*)
;;; Better for debugging, change for delivery
  (declaim (OPTIMIZE (DEBUG 3) (SPEED 0) (SAFETY 3) (SPACE 0))))

#|
(pushnew :cl-http-ssl *features*)
|#
 
(defparameter *force-compile* t)
(setq *force-compile* t)

#|
Karsten Poeck

This is an intent to produce clearer ports of  cl-http.
- The first port to MCL was done by us sort of blindly adapting the symbolics sources, 
  but we did not clearly separate portable and non portable stuff
- Later ports did copy the mcl specific stuff and did not separate it either
- Most of the codebase is portable and can be loaded in any valid ansi cl 
 (but beware of the non ansi use of loop breaking clisp and ecl)
- In addition to the portable codebase some multiprocessing, dairectory and tcp stuff is needed
- Dummies for these are defined in "stubs-to-port-www-utils" "stubs-to-port-http"
- To try the server in a new lisp just define mode as :stubs and load
- The html generation can be tested without sockets and multitasking as in "http:port;test;load"
- Try (test-all) 
- If this is running, tcpip and multithreading can be tested
- I'd propose to first try single threaded then multithreaded
- See the files in allegro for examples
- Once multithreading is working fine, tune the server with resourcing of threads and objects
- Since I borrowed the stubs from the mcl and the allegro ports, no specific copyright applies.

I sort of tested the core server with allegro, clisp, ecl, lispworks, openmcl, sbcl, cmu (all ok) and cormanlisp (fails)
Depending on the c-compiler, ecl has problems with the size of strings (miscrosoft only)
In Allegro (windows and macosx), sbcl and openmcl (just macosx) the simple server also works
ECL and clisp are sort of tested single threaded (at least the test pages come up)
In clisp the main redirect does not work and errors are not really handled, start from frame page
ecl could probably also worked threaded, but have problems with the process-wait and friends
probably will simply enable process-run-function and dummy out all the rest (a bit as in sbcl)
lispworks also works fine
cmu done single-threaded

SBCL complains about same declaration errors
- variables declared fixnum that could be nil
- #\nul declared a standard-char
sbcl is probably right in all the points
clozure compiles fine, but mcl needs to be deleted from *features*
ecl: the mpeg examples crashes the lisp immediately
     the parsing of get parameters does not seem to work

In some places in the server, definitions were just done for the known lisp, and new one breaks
I fixed all places I found.
- There are ansi issues with the use of defgeneric/values values and loop
- I have created definitions of values,loop and defgeneric compatible with the use in cl-http
- These are shadow-imported into the corresponding packages
- Only fails in Task-Queue since the defpackage is inside the same file where it is used

Places to fix for a new lisp
- add-package in package.lisp
- w3p-system.lisp
- variables.lisp
- bin-dump-utils.lisp
- url.lisp
- lispdoc.lisp (or assure that the clos-symbols are imported into www-utils in fix-symbols
|#

#|
*mode* describes how the server is loaded
:stubs is a testbed for new ports, just works on string-streams without threads
                                                                
:simple-server
loads a very simple server, both single and multithreaded
so far tested in allegro, openmcl and sbcl, lispworks, ecl & clisp
 
needs to add resourcing for a production quality server
Still have problems with chunking and netscape browsers
might borrow code for chunking from chunga or the lw;server;tcpstream from jcma
|#

;;; Platform specific setup
#+ecl
(require :sockets)

;; (declaim (optimize (debug 3) (speed 0)(safety 3)(space 0)))

#+sbcl
(require :sb-bsd-sockets)

#+sbcl
(require :sb-introspect)

#+(and lispworks unix)
(pushnew :LispWorks-UNIX *features*)

#+CLOZURE-COMMON-LISP
(setq *features* (remove :mcl *features*))

#+cormanlisp
(progn
  (load "c:/cygwin/home/Karsten/cl-http-svn/contrib/kpoeck/port-template/corman/corman.lisp")
  (load "c:/cygwin/home/Karsten/cl-http-svn/contrib/kpoeck/port-template/corman/corman-clos.lisp")
  #+no (load "c:/cygwin/home/Karsten/cl-http-svn/contrib/kpoeck/port-template/corman/corman-special.lisp")
  (load "c:/cygwin/home/Karsten/cl-http-svn/contrib/kpoeck/port-template/corman/corman-defpackage.lisp")
  (load "c:/cygwin/home/Karsten/cl-http-svn/contrib/kpoeck/port-template/corman/corman-readtable.lisp")
  )

(defparameter *mode* 
  #-no-stubs :stubs
  #+no-stubs :simple-server
  )

;;; setup the logical pathnames
#-(or cormanlisp)
(load (make-pathname
       :name "translations" 
       :type "lisp" 
       :defaults (or *load-pathname* "c:/data/cvs/cl-http-svn/contrib/kpoeck/port-template/translations.lisp")))
(pushnew :multi-threaded *features*) ; need before compiling the code

;;; This is not really respected in the code
;;; Chunking is assumed allover
;;; Perhaps http::use-chunked-transfer-encoding-p may return nil to avoid chunking
;;;;Plus all the methods in report.lisp
(pushnew :http-chunking *features*)

;;; Conclusion put new feature to disable chunking
#-allegro
(pushnew :disable-chunking *features*)
(pushnew :ANSI-documentation *features*)


(defparameter *interface-files*
  (ecase *mode*
    (:stubs '(("http:port;stubs;" "stubs-to-port-www-utils" "stubs-to-port-http" "network-conditions")))
    (:simple-server
     '(("http:port;allegro;"
        "files"
        "misc"
        #+allegro "processes"
        #+clozure-common-lisp "processes-clozure"
        #+sbcl "processes-sbcl"
        #+clisp "processes-dummies"
	#+ecl "processes-dummies"
        #+lispworks "processes-lispworks"
	#+cmu "processes-dummies" ;;; has mp, but for easier tests
        "server-tasks"
        #+allegro "acl-socket-low"
        #+clozure-common-lisp "clozure-socket-low"
        #+(or sbcl ecl) "sbcl-socket-low"
        #+clisp "clisp-socket-low"
        #+lispworks "lw-socket-low"
	#+cmu "cmu-socket-low"
        "tcp-macros"
        "bivalent-macros"
        "tcpip"
        #+allegro "tcp-stream"
        #+clozure-common-lisp "tcp-stream-clozure"
        #+sbcl "tcp-stream-sbcl"
        #+clisp "tcp-stream-clisp"
	#+ecl "tcp-stream-ecl"
        #+lispworks "tcp-stream-lw"
	#+cmu "tcp-stream-cmu"
        "tcp-conditions"
        #+allegro  "chunking"
        ;the following don't do something usefull
        #+clozure-common-lisp "chunking-clozure"
        #+sbcl "chunking-sbcl"
        #+clisp "chunking-clisp"
	#+ecl "chunking-ecl"
        #+lispworks "chunking-lw"
	#+cmu "chunking-cmu"
        ;these 2 are just dummies, not yet done
        "http"
        "mail")))))

(defparameter *post-interface-files*
  (ecase *mode*
    (:stubs nil)
    (:simple-server '(("http:port;allegro;" "post-http" "simple-server" "obc-stub" #+clozure-common-lisp "debug")))))

(defparameter *files-cl-http*
  `("http:port;declaration" ;;; to allow (declare (values ...))
    #+(or clisp ecl) ("http:port;" "mit-loop-package" "mit-loop" "mit-defgeneric-package" "mit-defgeneric")
    #+(or sbcl cmu) ("http:port;" "mit-values-package" "mit-values-setup")
    ("http:server;" "package" "preliminary")
    #+sbcl ("http:port;" "defconstant-sbcl") 
    ("http:server;" "variables")
    #+(or clisp ecl) ("http:port;" "fix-non-ansi" "fix-define-generic")
    #+(or sbcl cmu) ("http:port;" "fix-values")
    "http:mcl;server;resources"
    "http:port;fix-symbols"
    "http:port;import-mop"
    "http:mcl;server;www-utils"
    ;;;some definitions are not done for lispworks, do now
    "http:port;fix-www-utils-lw"
    "http:port;version"
    ,@*interface-files*
    ("http:server;" "base64-encoding" "md5" "sha" "task-queue")
    #+lispworks
    "http:mcl;server;md5"
    #+nil
    ("http:smtp;"   "package" "smtp" "mail")
    ("http:server;"
     "class"			; Server class definitions
     "url-class"		; URL class definitions 
     "http-conditions" 
     "plist"			; property list mixin for CLOS
     "utils"			; utility functions
     "tokenizer"		; Simple tokenizer
     "url"                    ; URL implementation ;;;
     "headers"                ; Header hacking, including MIME
     ;;; headers uses url::no-scheme-found used to be defined later
     ;;; Lispworks complains, Allegro, clisp are fine.
     ;;; Symbol should be exported anyhow
     "host"                   ; Host objects and operations
     "html2"                  ; HTML authoring functions
     "netscape-1-1"		; Netscape 1.1 html generation
     "vrml-1-0"		; HTML 1.0 Generation
     "image-maps"		; Image maps
     "netscape-2-0"		; Netscape 2.0 HTML generation
     "netscape-3-0"		; Netscape 3.0 HTML generation
     "html-3-2"		; HTML 3.2 Generation	
     "html4"			; HTML 4 Generation	
     "scripts"	        	; Client-Side Scripts
     "netscape-4-0"		;  Netscape 4.0 HTML generation
     "shtml")			; Server-side includes
    ,@*post-interface-files*
    ("http:w3p;"
     "package")
     #+(or sbcl cmu)("http:port;" "fix-values-w3p")
     ("http:w3p;"
    "class"
     "w3p-system"
     "functions"
     "standard-types"
     "standard-methods"
     "html-definitions")
    ("http:server;"
     "report"			; Reporting for HTTP Conditions
     "log"                    ; Logging 
     "authentication"		; User authentication
     "data-cache"		; Data caching
     "server"                 ; HTTP Server
     "cgi"			; Common Gateway Interface
     "preferences"
     "web-configuration") 
    ;;; this should be really in the portable sources
    "http:acl;jkf;server;pathname-get-value"
    )
  )

(defun convert-file (file)
  (when (find (pathname-type file)
              '("lisp" "lsp" "cl")
              :test #'string-equal)
    (format t "Converting ~a~%" file)
    (let ((tmp-file (merge-pathnames 
                     (make-pathname :type "tmp")
                     file)))
      (when (probe-file tmp-file)
        (delete-file tmp-file))
      (with-open-file (input file :direction :input)
        (with-open-file (output tmp-file :direction :output :if-does-not-exist :create)
          (loop
            (let ((char (read-char input nil :eof nil)))
              (when (eql char :eof)
                (return))
              (cond ((char= char #\Return)
                     (cond ((char= (peek-char nil input nil :eof nil) #\Newline)
                            (write-char char output))
                           (t
                            (write-char #\Return output)
                            (write-char #\Newline output))))
                    (t (write-char char output)))))))
      (delete-file file)
      (rename-file tmp-file file)
      )
    )
  )

(defun file-sexp-empty-p (file)
  (with-open-file (stream file :direction :input)
    (let ((empty t))
      (loop
        (let ((exp (read stream nil :end)))
          (if (eq :end exp)
              (return empty)
            (return (setq empty nil))))))))

(defun load-the-thing (value)
  (multiple-value-bind 
        (fasl warnings failure)
      #+cormanlisp 
    (values (simple-translate-logical-pathname value) nil nil)
    #-cormanlisp
    (potentially-compile-file value)
    (cond (failure 
           (when warnings
             (format t "~%Failure Warnings compiling ~a: ~a ~%" value warnings))
           (if fasl
               (load fasl :verbose t :print nil)
             (error "~%Compiling ~a failed~%" value))
           )
          (warnings 
           (format t "~%Warnings compiling ~a: ~a ~%" value warnings)
           (load fasl :verbose t :print nil))
          (t (load fasl :verbose t :print nil))))
  #+LISPWORKS-PERSONAL-EDITION
  (hcl:mark-and-sweep 3)
  #+cormanlisp
  (force-output)
  )

(defparameter *do-not-compile*
  #+ecl nil
  #-ecl nil
  )

(defun potentially-compile-file (lisp-file &key (force *force-compile*))
  (let (#+allegro (comp:*cltl1-compile-file-toplevel-compatibility-p* t))
    (let ((lisp (merge-pathnames lisp-file (make-pathname :type "lisp")))
          (fasl (compile-file-pathname lisp-file)))
      (when (file-sexp-empty-p lisp)
        (error "File seems to be empty, beware mac encoding ~a~%" lisp))     
      (if (member (pathname-name lisp) *do-not-compile* :test #'string-equal)
          (values lisp nil nil)
        (if (and (not force) 
                 (probe-file fasl)
                 (> (file-write-date fasl) 
                    (file-write-date lisp)))
            fasl
          (compile-file lisp :verbose t :print nil))))))
#|
To debug ecl
 #+(and msvc ecl) :data-file #+(and msvc ecl) "c:/data/lisp/ecl.data"
 #+(and msvc ecl) :c-file #+(and msvc ecl) "c:/data/lisp/ecl.c"
 #+(and msvc ecl) :h-file #+(and msvc ecl) "c:/data/lisp/ecl.h"
|#

(defun load-cl-http-files ()
  (set-logical)
  (with-compilation-unit ()
    (load-cl-http-files-internal *files-cl-http*)
    #-LISPWORKS-PERSONAL-EDITION
    (load-cl-http-files-internal
     '(("http:client;"
        "variables"			;variables controlling the client
        "connection"			;symbols
        "proxy"			; Upstream Proxies
        "authentication"		; Client authentication
        "client")			; WWW Client substrate
       #+nil "http:port;client;unix"))
    #-LISPWORKS-PERSONAL-EDITION
    (load-cl-http-files-internal
     '(("http:proxy;"
        "uid"
        "package"
        "variables"
        "class"
        "utils"
        "database"
        "cache"
        "resource"
        "representation"
        "proxy-cache"
        "proxy"
        "documentation")))
    #-LISPWORKS-PERSONAL-EDITION
    (load-cl-http-files-internal
     '(("http:client;" "sexp-browser" "flogger")))
    #-LISPWORKS-PERSONAL-EDITION
    (load-cl-http-files-internal
     '("http:client;w4-client"
       ("http:w4;"
        "package"			; Package Definition
        )
       #+(or clisp ecl)
       "http:port;fix-non-ansi-w4"
       #+(or sbcl cmu) "http:port;fix-values-w4"
       ("http:w4;"
        "variables"                  ; Variables
        "utils"				; Utility functions and macros
        "class"				; Class definitions and Print methods
        "walker"				; Main Walker code
        "constraints"                ; Constraint Definitions
        "actions"			; Action definitions
        "activity"                   ; Activity definitionse
        )))
    #-LISPWORKS-PERSONAL-EDITION
    (load-cl-http-files-internal
     '(("http:lambda-ir;" 
        "package") 
       #+(or clisp ecl sbcl cmu)
       "http:port;fix-non-ansi-lambda-ir"
       ("http:lambda-ir;"
        "ir-utils"
        "variables"
        "class"
        "bit-vectors"
        "data-structures"
        "computations"
        "ir-base"
        "contexts"
        "constraints"
        "bin-dump-utils"
        "examples;lambdavista" 
        "examples;stemming" 
        "examples;lambdavista-exports")
       )
     )
    )
  
  ;;; Postsystem patches or extensions
  #+clisp
  (load-cl-http-files-internal
   '("http:port;clisp;fix-clisp"))
  #+lispworks
  (load-cl-http-files-internal
   '(("http:port;allegro;" "lw-read-headers" "lw-http-patches")))
  #+allegro
  (load-cl-http-files-internal
   '(("http:port;allegro;" "add-http-acl")))
  )

#+no
(defun loop-tester ()
  (loop with idx = 0 and end = 3
      while (< idx end)
      for char = (aref "012" idx)
      do (print char)(incf idx)))

(defun load-cl-http-files-internal (files)
  (dolist (value files)
    (cond ((stringp value)
           (load-the-thing value))
          ((listp value)
           (let ((root (first value)))
             (dolist (file (rest value))
               (load-the-thing (concatenate 'string root file))))))))

(defun load-cl-http-tester ()
  (load "http:port;test;load.lisp"))

(defun load-all ()
  (load-cl-http-files)
  (load-cl-http-tester))


#-:sbcl
(defun compile-all ()
  (time
   (load-cl-http-files)))

#+:sbcl
(declaim (sb-ext:muffle-conditions sb-ext:compiler-note sb-ext:code-deletion-note))

#+:sbcl
(defun compile-all ()
  (handler-bind ((sb-ext:defconstant-uneql #'(lambda (c)
					       (let ((r (find-restart 'continue c)))
						 (when r (invoke-restart r))))))
    (sb-ext:with-unlocked-packages 
	(:cl)
      (time (load-cl-http-files)))
    )
  )

(format t "~%Excecute ~A to compile the system. ~%" '(compile-all))
(format t "~%Excecute ~A to test the system without sockets. ~%" '(load-cl-http-tester))

#+(and :no-stubs (or allegro clozure-common-lisp sbcl clisp ecl lispworks))
(format t "~%To start the simple-server do (http::start-examples) .~%")


#|
(CONVERT-FILE "http:mcl;server;resources.lisp")
(CONVERT-FILE "http:mcl;server;www-utils.lisp")
(CONVERT-FILE "http:proxy;proxy.lisp")
(when (file-sexp-empty-p "http:examples;exports.lisp")
  (CONVERT-FILE "http:examples;exports.lisp"))

(URL:MAP-URL-TABLE #'(LAMBDA (A B) (PRINT (LIST A B))))

http::standardize-line-breaks 

(compile-all)
|#


#|
Current state of affairs in sbcl:
; 
; compilation unit finished
;   caught 28 WARNING conditions
;   caught 145 STYLE-WARNING conditions

* (http::test "generated")
And the result is:
"HTTP/1.0 200 OK
Date: Thu, 26 Oct 2006 21:53:36 GMT
Server: CL-HTTP/70.216 (SBCL; 0.9.0)
Content-Type: text/html; charset=ISO-8859-1

<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0.1 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">
<html>
<head>
<title>foo</title>
</head>
<body bgcolor=\"#FFFFFF\" text=\"#000000\" link=\"#0000FF\" vlink=\"#800080\" alink=\"#FF0000\">bar</body>
</html>
"
NIL
|#
