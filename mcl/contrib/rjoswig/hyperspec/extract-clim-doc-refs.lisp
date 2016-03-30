;;;-*- Mode: Lisp; Package: CCL -*-
;;; Copyright: Rainer Joswig, joswig@lisp.de, All Rights Reserved, 1998-2002
;;;

;;;; Purpose
;;;  Let's you refer to the CLIM User Guide (HTML version) from within Macintosh Common Lisp.
;;;  Place you cursor on a CLIM symbol and press c-x f. Your browser should
;;;  open the documentation.

;;;; Instructions
;;; Set *CLIM-EXTERNAL-DOCUMENTATION* to where your CLIM 2 guide is located.
;;;  set the :LOCAL-URL parameter to MAKE-INSTANCE 'EXTERNAL-DOCUMENTATION

;;;  Changes
;;;  03 Mar 2001   use the new scheme to locate external HTML documentation
;;;  28 Oct 2002   some adjustments for making it work with MCL 4.4


(in-package :ccl)

(defparameter *clim-external-documentation*
  (make-instance 'external-documentation
    :mapping-file (make-pathname :defaults *load-pathname*
                                 :name "clim-defs"
                                 :type "data")
    :keyword :clim
    :name "CLIM 2"
    :local-url "file:///PopLife/Lisp/MCL/MCL 4.4b5/clim-docs/html/"
    :original-url "http://www.xanalys.com/software_tools/reference/lwu41/climuser/"
    :url-fragment-start 0
    :default-package-name "CLIM"
    :add-extension "htm"))

; (init-external-documentation *clim-external-documentation*)
; (all-symbol-mappings *clim-external-documentation*)

#|

(defparameter *clim-docs-path*
  "PopLife:Lisp:MCL:MCL 4.4b5:clim-docs:html:")
(defparameter *CLIM-DOCS-URL-PATH*
  "file:///PopLife/Lisp/MCL 4.4b5/clim-docs/html/")
(defparameter *clim-defs-path* "http:mcl;contrib;rjoswig;hyperspec;clim-defs.data")
(defparameter *harlequin-clim-link*
  "http://www.xanalys.com/software_tools/reference/lwu41/climuser/")

(defun clim-def-on-line (line)
  (when (and (stringp line)
             (> (length line) 35)
             (string-equal "<b>"
                           line
                           :end1 3 :end2 3))
    (let ((pos1 (position #\< line :test #'char= :start 2)))
      (if (and pos1
               (not (= pos1 3))
               (> (length line) (+ pos1 (length "<A NAME=MARKER")))
               (string-equal "<A NAME=MARKER"
                             line
                             :start1 0
                             :start2 pos1
                             :end1 (length "<A NAME=MARKER")
                             :end2 (+ pos1 (length "<A NAME=MARKER"))))
        (let ((pos1a (position #\= line :test #'char= :start pos1)))
          (when pos1a
            (let ((pos1b (position #\- line :test #'char= :start pos1a)))
              (when pos1b
                (let ((pos1c (position #\- line :test #'char= :start (1+ pos1b))))
                  (when pos1c
                    (let ((pos1d (position #\> line :test #'char= :start pos1c)))
                      (when pos1d
                        (let ((pos2 (position #\[ line :test #'char= :start pos1d)))
                          (when pos2 pos1d
                                (let ((pos3 (position #\] line :test #'char= :start pos2)))
                                  (when (and pos3 (< pos1 pos1a pos1b pos1c pos1d pos2 pos3))
                                    (values (string-upcase (string-right-trim '(#\space) (subseq line 3 pos1)))
                                            (parse-integer line
                                                           :start (1+ pos1b)
                                                           :end pos1c)
                                            (parse-integer line
                                                           :start (1+ pos1c)
                                                           :end pos1d)
                                            (intern (string-upcase (subseq line (1+ pos2) pos3))))))))))))))))
        (when (string-equal "<B><A NAME=MARKER-"
                            line
                            :start1 0
                            :start2 0
                            :end1 (length "<B><A NAME=MARKER-")
                            :end2 (length "<B><A NAME=MARKER-"))
          (let ((pos1 (position #\- line :test #'char= :start 2)))
            (when pos1
              (let ((pos2 (position #\- line :test #'char= :start (1+ pos1))))
                (when pos2
                  (let ((pos3 (search "></A>" line :test #'char= :start2 pos2)))
                    (when pos3
                      (let ((pos4 (position #\< line :test #'char= :start (+ pos3 6))))
                        (let ((pos5 (position #\[ line :test #'char= :start pos4)))
                          (when pos5
                            (let ((pos6 (position #\] line :test #'char= :start pos5)))
                              (when (and pos3 (< pos1 pos2 pos3 pos4 pos5 pos6))
                                (values (string-upcase (string-right-trim '(#\space) (subseq line (+ 5 pos3) pos4)))
                                        (parse-integer line
                                                       :start (1+ pos1)
                                                       :end pos2)
                                        (parse-integer line
                                                       :start (1+ pos2)
                                                       :end pos3)
                                        (intern (string-upcase (subseq line (1+ pos5) pos6))))))))))))))))))))
(defun find-clim-name (string)
  (when (and (stringp string) (not (zerop (length string))))
    (if (char= (aref string 0) #\:)
      (read-from-string string)
      (or (find-symbol string (find-package :clim))
          (find-symbol string (find-package :clim-silica))))))

(defun extract-clim-defs (&optional (files (concatenate 'string *clim-docs-path* "*.htm")))
  (loop with defs = nil
        for file in (directory files)
        do (with-open-file (stream file)
             (loop for line = (read-line stream nil nil)
                   while line
                   do (multiple-value-bind (name m1 m2 type)
                                           (clim-def-on-line line)
                        (when (and name m1 m2 type)
                          (let ((symbol (find-clim-name name)))
                            (if symbol
                              (push (list symbol m1 m2 type (pathname-name file))
                                    defs)
                              (print (list name type
                                           (concatenate 'string
                                                        *harlequin-clim-link*
                                                        (string-upcase (pathname-name file))
                                                        ".HTM#MARKER-"
                                                        (princ-to-string m1)
                                                        "-"
                                                        (princ-to-string m2))))))))))
        finally (return defs)))

(defun create-clim-defs-file (defs clim-doc-dir &optional (file *clim-defs-path*))
  (when defs
    (with-open-file (stream file :direction :output :if-exists :supersede)
;      (format stream "~a~%" clim-doc-dir)
      (loop for (symbol m1 m2 nil file) in defs
            do (format stream "~s~%~a~%" symbol file)))))


(defun get-clim-guide-url (symbol)
  (unless (probe-file *clim-defs-path*)
    (create-clim-defs-file (extract-clim-defs) *clim-docs-path*))
  (let ((where (let ((s1 (format nil "~a:~a"
                                 (package-name (symbol-package symbol))
                                 (symbol-name symbol))))
                 (with-open-file (stream "ccl:clim-defs")
                   (read-line stream nil nil)
                   (loop for line = (read-line stream nil nil)
                         while line
                         for pos1 = (position #\space line :test #'char=)
                         while pos1
                         when (string-equal s1 (subseq line 0 pos1))
                         do (return (read-from-string line nil nil :start pos1)))))))
    (when where
      (format nil
              "~a~a.HTM#MARKER-~a-~a"
              *clim-docs-url-path*
              (third where)
              (first where)
              (second where)))))

(defun open-url-for-clim-symbol (symbol)
  (let ((url (get-clim-guide-url symbol)))
    (when url
      (open-url url :activate-p t))))


(defun show-ansi-cl-documentation (sym)
  "Show the ANSI Common Lisp documentation for the symbol."
  (let ((url (or (get-ansi-cl-url sym) (get-clim-guide-url sym))))
    (if url
      (ccl::open-url url)
      (ccl:ed-beep))))
|#


#||

;;; Create a Definitions file
(create-clim-defs-file (extract-clim-defs) *clim-docs-path*)


(with-open-file (stream "ccl:clim-defs-1" :direction :input)
  (let ((*standard-output* (ed)))
    (loop for line = (read-line stream nil nil)
          while (and line (> (length line) 0))
          do (let* ((space-position (position #\space line :test #'char=))
                    (string (subseq line 0 space-position))
                    (l1 (read-from-string line nil nil :start (1+ space-position))))
               (princ string)
               (terpri)
               (format *standard-output*
                       "~a.HTM#MARKER-~a-~a"
                       (third l1)
                       (first l1)
                       (second l1))
               (terpri)))
    (force-output)))
               


;;; Try to find one
(open-url-for-clim-symbol 'clim:present)


(inspect (sort (extract-defs) #'string-lessp :key #'(lambda (item)
                                                      (symbol-name (first item)))))

(let ((defs (extract-defs)))
  (values (length defs)
          (length (remove-duplicates defs :key #'first))))

(time (progn (extract-defs) nil))



<B>panep <A NAME=MARKER-2-140></A></B> [Function] <BR>
<B><A NAME=MARKER-2-38></A>draw-rectangle</B> [Function] <BR>

<B>panep <A NAME=MARKER-2-140></A></B> [Function] <BR>

(length "<B> <A NAME=MARKER></A></B> [] <BR>")

(string-equal "<b>" "<B>panep <A NAME=MARKER-2-140></A></B> [Function] <BR>"
              :end1 3 :end2 3)

(clim-def-on-line "<B>panep <A NAME=MARKER-2-140></A></B> [Function] <BR>")
(clim-def-on-line "<B>draw-circle<A NAME=MARKER-2-44></A> </B> [Function] <BR>")
(clim-def-on-line "<B><A NAME=MARKER-2-38></A>draw-rectangle</B> [Function] <BR>")
(step (clim-def-on-line "<B>draw-circle<A NAME=MARKER-2-44></A> </B> [Function] <BR>"))
(step (clim-def-on-line "<B><A NAME=MARKER-2-38></A>draw-rectangle</B> [Function] <BR>"))

||#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of File

