;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: cl-user; Base: 10 -*-
;;
;; convert.lisp
;;
;; text files are based in OS-X Newline syntax, must be converted to Windows 

(in-package :cl-user)

(defun map-all-files (directory action)
  (let ((files-or-directories (directory (concatenate 'string directory "/*.*") :directories t)))
    (dolist (file files-or-directories)
      (if #+clozure (ccl:directory-pathname-p file)
          #+allegro (excl:file-directory-p file)
          #+lispworks (lw:file-directory-p file)
          (map-all-files (namestring file) action)
        (funcall action file)))))




#|
(defparameter *root* "/Users/karstenpoeck/Documents/workspace/trunk")
(setq *root* "C:/data/cvs/cl-http-svn")
(map-all-files *root* #'print)

(map-all-files 
 *root*
 #'(lambda(file)
     (when (member (pathname-type file)
                   (list  "fasl" "fas" "fsl" "lib" "o" "exp" "ilk"  "ofasl" "bak" "xfasl"
                          "lisp-crlf" "lisp~" "dx64fsl" "64xfasl"
                         (pathname-type (compile-file-pathname "test.lisp")))
                   :test #'string-equal)
       (format t "Deleting fasl ~a~%" file)
       (delete-file file))
     :fin))

(let ((types nil))
  (map-all-files 
   *root*
   #'(lambda(file)
       (pushnew (pathname-type file) types :test #'string=)))
  types)

(map-all-files 
 *root* 
 #'(lambda(file)
     (when (and (>= (length (pathname-name file)) 2)
                (string= (subseq (pathname-name file) 0 2) "._"))
       (format t "Deleting mac ~a~%" file)
       (delete-file file)
       )))

(let ((newest nil))
  (map-all-files 
   *root* 
   #'(lambda(file)
       (when (string-equal "lisp" (pathname-type file))
         (let ((date (file-write-date file)))
           (if (null newest)
               (setq newest (list file date))
             (if (< date (second newest))
                 (setq newest (list file date))))))))
  newest)

(map-all-files "c:" #'(lambda(was)
                                (when (string= "com" (pathname-type was))
                                  (print was))))
|#


