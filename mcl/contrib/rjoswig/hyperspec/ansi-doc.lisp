;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: ccl -*-
;;;
;;; Hack to access the ANSI CL HyperSpec documentation via your favorite
;;; web browser from within MCL. Kent Pitman has converted the ANSI CL
;;; documentation into hypertext. Xanalys has made it
;;; available on the web.
;;;
;;; This File: http:mac;contrib;rjoswig;hyperspec;ansi-doc.lisp
;;;
;;;
;;; type "c-x f" while the cursor is on a symbol to look up the documentation.
;;;
;;;
;;; Needs the code in http:mcl;server;control.lisp and the www-utils package.
;;;
;;; puts property :EXTERNAL-HTML-DOCUMENTATION on CL symbols
;;;
;;;
;;; Author: Rainer Joswig, joswig@lispmachine.de
;;;
;;; Copyright, 1996-2002, All rights reserved.
;;;
;;;
;;; Thanks to Mike Travers, Kent Pitman, Xanalys and Digitool.
;;;
;;; Instructions:
;;;
;;; 1) Get the ANSI CL HyperSpec from Xanalys.
;;;    See: http://www.xanalys.com/software_tools/reference/HyperSpec/FrontMatter/index.html
;;;    For conditions see:
;;;    http://www.xanalys.com/software_tools/reference/HyperSpec/FrontMatter/About-HyperSpec.html#Legal
;;;    The complete ANSI CL HyperSpec can be got from:
;;;    http://www.xanalys.com/software_tools/reference/HyperSpec/HyperSpec-4-0.tar.gz
;;;    You may need tools like Stuffit Expander 4.0 and/or SunTar
;;;    to unpack this file.
;;;
;;; 2) Change the parameters *USER-ANSI-CL-URL-BASE* and *SYMBOL->URL-FILE* below.
;;; 
;;; 3) Load this file.
;;;
;;; Some functionality is accessible by keys:
;;;
;;; - You then can locate the ANSI documentation for a current symbol when
;;;   you have a Netscape web browser running, by pressing "c-x f".
;;; - You can insert a link to your own documentation for the current symbol
;;;   by pressing "c-x c-f".
;;; - You can insert a link to Xanalys' documentation for the current symbol
;;;   by pressing "c-x c-m-f".
;;;
;;; Hints:
;;;
;;; - You may change the keybindings. See below.
;;; - It works with CLIM 2, too. See: http:mac;contrib;rjoswig;hyperspec;extract-clim-doc-refs.lisp .
;;; 

;;; Changes:
;;; 30. June 1996, RJ  -  Add parameter *USER-ANSI-CL-URL-BASE*.
;;; 28. Sep  2000, RJ  -  Xanalys is now the home of the Hyperspec
;;;                    -  There is a version 4.0 HyperSpec
;;;                    -  More logic to locate files.
;;; 01. Feb  2001, RJ  -  Make it a general and reusable mechanism.
;;; 23. Sep  2002, RJ  -  There is a version 6.0 HyperSpec. Get it.
;;; 25. Oct  2002, RJ  -  We are back in the game. Now runs in MCL 4.4 under MacOS 10.2.1. Needs CL-HTTP utils.
;;; 28. Oct  2002, RJ  -  Support different line ends in mapping files.


;;; To Do:
;;;  Update the documentation about the HyperSpec 6.0 distribution.


(in-package "CCL")

(export '(*ANSI-CL-URL-BASE*
          *USER-ANSI-CL-URL-BASE*
          GET-DOCUMENTATION-URL
          SHOW-EXTERNAL-DOCUMENTATION))


;;; ================================================================
;;; General Machinery for accessing external HTML-based documentation


(defun read-a-line (stream &optional (line-end :return))
  "If we run under OSX, files usually have a different line end."
  #+ccl-4.4(if (osx-p)
             (ecase line-end
               (:linefeed (www-utils:read-delimited-line stream '(#\linefeed)))
               (:return (read-line stream nil nil)))
             (read-line stream nil nil))
  #-ccl-4.4(read-line stream nil nil))


(defvar *external-documentations* (make-hash-table)
  "Maps the keyword for the documentation object to the
documentation object")

(defclass external-documentation ()
  ((mapping-file :initarg :mapping-file
                 :accessor doc-mapping-file)
   (url-fragment-start :initarg :url-fragment-start
                       :accessor doc-url-fragment-start
                       :initform nil)
   (default-package-name :initarg :default-package-name
                         :accessor doc-default-package-name)
   (keyword :initarg :keyword
            :accessor doc-keyword)
   (name :initarg :name
         :accessor doc-name)
   (local-url :initarg :local-url
              :accessor doc-local-url)
   (original-url :initarg :original-url
                 :accessor doc-original-url)
   (line-end :initarg :line-end
             :accessor doc-line-end
             :initform :return)
   (add-extension :initform nil
                  :initarg :add-extension
                  :accessor doc-add-extension)))

(defmethod all-symbol-mappings ((doc external-documentation))
  "Returns a list of (symbol url-fragment). Reads from the external file."
  (let ((start (doc-url-fragment-start doc)))
    (with-open-file (stream (doc-mapping-file doc) :direction :input)
      (loop for symbol =  (read-a-line stream (doc-line-end doc))
            while symbol
            for url-fragment = (read-a-line stream (doc-line-end doc))
            while url-fragment
            collect (list symbol (if start
                                   (subseq url-fragment start)
                                   url-fragment))))))

(defmethod find-symbol-for-doc ((doc external-documentation) symbol-string)
  "Tries to create a symbol from a string. Merges against the
default documentation package, if necessary.
Returns the values of FIND-SYMBOL."
  (assert (and (stringp symbol-string) (> (length symbol-string) 0))
          (symbol-string))
  (let ((package (find-package (doc-default-package-name doc)))
        (colon-position (position #\: symbol-string :test #'char=)))
    (cond ((and (numberp colon-position) (zerop colon-position))
           (setf package (find-package :keyword)))
          ((numberp colon-position)
           (setf package (find-package (subseq symbol-string 0 colon-position)))))
    (assert (packagep package) (package))
    (find-symbol (if colon-position
                   (subseq symbol-string (1+ colon-position))
                   symbol-string)
                 package)))
                 
(defparameter *scan-documentation-index-file-verbose* t
  "Generates warnings when symbols are not found")

(defmethod init-external-documentation ((doc external-documentation))
  "Puts the :EXTERNAL-HTML-DOCUMENTATION keyword on the symbols'
property list and registers the documentation object on
*EXTERNAL-DOCUMENTATIONS*"
  (loop for (symbol-name url-fragment) in (all-symbol-mappings doc)
        when (and symbol-name url-fragment
                  (> (length symbol-name) 0)
                  (> (length url-fragment) 0))
        do (multiple-value-bind (symbol status)
                                (find-symbol-for-doc doc symbol-name)
             (if status
               (setf (get symbol :external-html-documentation) (doc-keyword doc))
               (when *scan-documentation-index-file-verbose*
                 (warn "Unknown symbol ~a" symbol-name)))))
  (setf (gethash (doc-keyword doc) *external-documentations*) doc))

(defmethod doc-url-for-symbol ((symbol symbol))
  "Returns two values: the URL and a documentation object."
  (let ((doc-keyword (get symbol :external-html-documentation)))
    (when doc-keyword
      (let ((documentation (gethash doc-keyword *external-documentations*)))
        (with-open-file (stream (doc-mapping-file documentation) :direction :input)
          (loop for symbol-string = (read-a-line stream (doc-line-end documentation))
                while symbol-string
                when (eq symbol (find-symbol-for-doc documentation symbol-string))
                do (return (let ((url-fragment (read-a-line stream (doc-line-end documentation))))
                             (when url-fragment
                               (values (let ((start (doc-url-fragment-start documentation)))
                                         (let ((url (if start
                                                      (subseq url-fragment start)
                                                      url-fragment)))
                                           (if (doc-add-extension documentation)
                                             (concatenate 'string url "." (doc-add-extension documentation))
                                             url)))
                                       documentation))))))))))

(defmethod initialize-instance :after ((doc external-documentation) &key &allow-other-keys)
  (init-external-documentation doc))

(defun directory-to-file-url (directory)
  "Converts a pathname to a FILE URL. Well, almost. Enough for our purposes now."
  (concatenate 'string
               "file:///"
               (substitute #\/ #\: (namestring (truename directory)) :test #'char=)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; The external interface

(defun get-documentation-url (symbol &optional (mode :local))
  "Return the URL for the ANSI Common Lisp documentation for the symbol."
  (multiple-value-bind (url documentation)
                       (doc-url-for-symbol symbol)
    (when url
      (ecase mode
        (:local (concatenate 'string (doc-local-url documentation) url))
        (:remote (concatenate 'string (doc-original-url documentation) url))))))

(defun show-external-documentation (symbol &key (mode :local) activate-p)
  "Shows the ANSI Common Lisp documentation for the symbol.
Uses a call to CCL::OPEN-URL to control the web browser."
  (let ((url (get-documentation-url symbol mode)))
    (if url
      (ccl::open-url url :activate-p activate-p)
      (ccl:ed-beep))))

(defmethod ed-get-html-documentation ((w ccl:fred-mixin))
  "Show the external documentation for the current symbol (in the
fred editor) using the web browser."
  (multiple-value-bind (sym endp) (ccl:ed-current-sexp w)
    (if (null endp)
      (ccl::edit-anything-dialog :documentation)
      (progn
        (when (consp sym) (setq sym (car sym)))
        (if (symbolp sym)
          (show-external-documentation sym :activate-p t)      
          (ccl:ed-beep))))))

(defmethod ed-insert-url-for-current-symbol ((view fred-mixin) &optional (mode :local))
  "Insert URL for current symbol into the buffer."
  (let ((buffer (ccl:fred-buffer view))
        (*package* (or (ccl:fred-package view) *package*)))
    (multiple-value-bind (start end)
                         (ccl:buffer-current-sexp-bounds buffer)  ; well, it works
      (when (and start end)
        (multiple-value-bind (sym endp)
                             (ccl:ed-current-sexp view)
          (if (or (null endp) (not (symbolp sym)))
            (ccl:ed-beep)
            (let ((url (get-documentation-url sym mode)))
              (if url
                (progn
                  (ccl:collapse-selection view t)
                  (ccl:ed-replace-with-undo view start end
                                            (format nil "<a href=\"~a\">~a</a>" url sym)))
                (ccl:ed-beep)))))))))

(defmethod ed-insert-remote-url-for-current-symbol ((view fred-mixin))
  "Insert remote documentation URL for the current symbol into the buffer."
  (ed-insert-url-for-current-symbol view :remote))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; you might want to choose different key bindings

(ccl:comtab-set-key ccl:*control-x-comtab*
                    '(#\f)
                    'ed-get-html-documentation
                    "Locate the documentation for the current symbol,
using a running web browser.")

(ccl:comtab-set-key ccl:*control-x-comtab*
                    '(:control #\f)
                    'ed-insert-url-for-current-symbol
                    "Insert documentation URL for current symbol.")

(ccl:comtab-set-key ccl:*control-x-comtab*
                    '(:control :meta #\f)
                    'ed-insert-remote-url-for-current-symbol
                    "Insert remote documentation for current symbol.")


;;; ================================================================
;;; Create the documentation objects

(defparameter *ansi-cl-url-base*
  "http://www.xanalys.com/software_tools/reference/HyperSpec/Body/"
  "The URL where the Xanalys' ANSI CL HyperSpec body resides.
To customize this you should change the variable *USER-ANSI-CL-URL-BASE* .")

(defparameter *hyperspec-version*
  (cond ((or (probe-file "ccl:HyperSpec;Data;Map_Sym.txt")
             (probe-file "ccl:HyperSpec-6-0;HyperSpec;Data;Map_Sym.txt"))
         :|6.0|)
        ((probe-file "ccl:HyperSpec-4-0;HyperSpec;Data;Symbol-Table.text")
         :|4.0|)
        ((probe-file "ccl:HyperSpec-3-0;HyperSpec;Data;Symbol-Table.text")
         :|3.0|)
        (t (warn "Unknown Hyperspec version")
           :unknown)))

(defparameter *ansi-cl-symbol->url-file*
  (or (probe-file "ccl:HyperSpec;Data;Map_Sym.txt")
      (probe-file "ccl:HyperSpec-6-0;HyperSpec;Data;Map_Sym.txt")
      (probe-file "ccl:HyperSpec-4-0;HyperSpec;Data;Symbol-Table.text")
      (probe-file "ccl:HyperSpec-3-0;HyperSpec;Data;Symbol-Table.text")
      (choose-file-dialog :prompt "Locate the HyperSpec File Symbol-Table.text or Map_Sym.txt"
                          :window-title "Locate the HyperSpec symbol table file"
                          :button-string "Choose")
      (error "can't find file Symbol-Table.text or Map_Sym.txt"))
  "This file is from the HyperSpec package. It defines the mappings
between ANSI Common Lisp symbols and the URLs for the documentation.
You need a local copy for this file.")

(defparameter *user-ansi-cl-url-base*
  (or (let ((hyperspec-body-directory "ccl:HyperSpec;Body;"))
        (and (probe-file hyperspec-body-directory)
             (directory-to-file-url hyperspec-body-directory)))
      (let ((hyperspec-body-directory "ccl:HyperSpec-6-0;HyperSpec;Body;"))
        (and (probe-file hyperspec-body-directory)
             (directory-to-file-url hyperspec-body-directory)))
      (let ((hyperspec-body-directory "ccl:HyperSpec-4-0;HyperSpec;Body;"))
        (and (probe-file hyperspec-body-directory)
             (directory-to-file-url hyperspec-body-directory)))
      (let ((hyperspec-body-directory "ccl:HyperSpec-3-0;HyperSpec;Body;"))
        (and (probe-file hyperspec-body-directory)
             (directory-to-file-url hyperspec-body-directory)))
      *ansi-cl-url-base*)
  "The URL where the ANSI CL HyperSpec body files reside. This can
be on your local machine or on the network. Default is
a local version or the one at Xanalys,
but you can override this.")

(defparameter *ansi-cl-external-documentation*
  (make-instance 'external-documentation
    :mapping-file *ansi-cl-symbol->url-file*
    :keyword :ansi-cl-hyperspec
    :name "ANSI CL Documentation"
    :local-url *user-ansi-cl-url-base*
    :url-fragment-start 8
    :line-end :linefeed
    :default-package-name "COMMON-LISP"
    :original-url *ansi-cl-url-base*))


#+cl-http-menu
(cl-http-menu:add-item-to-open-url
 "CL: ANSI CL HyperSpec"
 (concatenate 'string
              (subseq *user-ansi-cl-url-base* 0 (- (length *user-ansi-cl-url-base*) 5)) ; get rid of "Body/"
              (case *hyperspec-version*
                ((:|4.0| :|3.0|) "FrontMatter/index.html")
                (otherwise "Front/index.htm")))
 "The Home of the ANSI Common Lisp HyperSpec."
 :update-menu-item t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of File - Have Fun


