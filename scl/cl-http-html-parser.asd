(in-package :cl-user)

(unless (ignore-errors (logical-pathname-translations "html-parser"))
  (setf (logical-pathname-translations "html-parser")
	`(("*.*.*" ,(namestring (merge-pathnames "html-parser/v10/*.*.~*~"
						 *cl-http-directory*))))))

(asdf:defsystem cl-http-html-parser
  :pathname "html-parser:"
  :depends-on (cl-http-server)
  :serial t
  :components
  ((:file "packages")
   #+nil (:file "tokenizer")
   #+nil (:file "plist")
   (:file "defs")
   (:file "patmatch")
   (:file "rewrite-engine")
   (:file "rewrite-rules")
   (:file "html-tags")
   (:file "html-reader")
   (:file "html-parser")
   (:file "html-utilities")))

