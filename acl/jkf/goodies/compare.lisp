(defun file-identical-p (file1 file2)
  (with-open-file (stream1 file1)
    (with-open-file (stream2 file2)
      (loop
        (let ((line1 (read-line stream1 nil :end))
              (line2 (read-line stream2 nil :end)))
          (when (or (eq :end line1)(eq :end line2))
            (return (eq line1 line2)))
          (unless (string= line1 line2)
            (return (values nil line1 line2))))))))

#|
(file-identical-p 
 "C:/data/lisp/clhttp/cl-http-70-190a/acl/jkf/translations.lisp"
 "C:/data/lisp/clhttp/cl-http-70-201d-devo/acl/jkf/translations.lisp")
|#

(defun compare (directory1 directory2)
  (dolist (file (directory (concatenate 'string directory1 "*")))
    (cond ((file-directory-p file)
           ;;;
           (let ((new-dir (first (last (pathname-directory (concatenate 'string (namestring file) "/blah.txt"))))))
             ;(format t "Dir ~a;~a~%" new-dir file)
             (compare
              (concatenate 'string (namestring file) "/")
              (concatenate 'string directory2 new-dir "/"))
             ))
          (t 
           (let ((type (pathname-type file))
                 (name (pathname-name file)))
             (cond ((string= "fasl" type)
                    #+no (format t "Ignore ~a~%" file)
                    )
                   ((string= "." (subseq name 0 1))
                    #+no (format t "Ignore ~a~%" file))
                   (t
                    (let ((file-2 (make-pathname
                                   :type type
                                   :name name
                                   :defaults directory2)))
                      (cond ((probe-file file-2)
                             (multiple-value-bind (same line1 line2)
                                 (file-identical-p file file-2)                  
                               (unless same
                                 (format t "Diff ~a ~a ~a ~%" file-2 line1 line2))))
                            (t (format t "Missing ~a~%" file)))))))))))

#|
(compare "C:/data/lisp/clhttp/cl-http-70-201d-devo/acl/jkf/"
         "C:/data/lisp/clhttp/cl-http-70-190a/acl/jkf/")

(compare 
 "C:/data/lisp/clhttp/cl-http-70-190a/acl/jkf"
 "C:/data/lisp/clhttp/cl-http-70-201d-devo/acl/jkf/blah.lisp")
|#