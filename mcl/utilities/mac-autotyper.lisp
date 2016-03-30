;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: ccl -*-

;;; (C) Copyright 2000, John C. Mallery.
;;;     All Rights Reserved.
;;;------------------------------------------------------------------- 
;;;
;;;  AUTOMATICALLY SET FILE TYPE AND CREATOR BASED ON FILE EXTENSION
;;; 
;;; rewritten 3/20/2000 -- JCMa.

(in-package :ccl) 

(defvar *file-type-extension-table* (make-hash-table :test #'equalp))

(defun %define-file-type-extentions (specs)
   (loop with table = *file-type-extension-table*
            initially (clrhash table)
            for spec in specs
            do (dolist (key (car spec))
                    (check-type key string)
                    (setf (gethash key table) spec))))

(defmacro define-file-type-extentions (&body body)
   "Associates file extensions with (FILE-TYPE FILE-CREATOR)"
   `(%define-file-type-extentions ',body))

(define-file-type-extentions
   (("text" "txt")  "TEXT")
   (("lisp" "lsp")  "TEXT" "CCL2")
   (("ncsa-map")  "TEXT")
   (("cern-map")  "TEXT")
   (("class") "clss" "java")
   (("gif")  "GIFf" "JVWR")
   (("gzip") "Gzip" "Gzip")
   (("jpeg" "jpg")  "JPEG" "JVWR")
   (("pict")  "PICT")
   (("pdf") "PDF " "CARO")            ; file types are 4 chars so we need a space for PDF -- JCMa 3/20/2000.
   (("tiff" "tff")  "TIFF")
   (("xbm")  "XBM")
   (("au")  "ULAW")
   (("mov")  "MooV")
   (("mpeg" "mpg")  "MPEG" "mMPG")) 

(declaim (inline get-file-type-pathname-extension-entry))

(defun get-file-type-pathname-extension-entry (pathname)
   (gethash (pathname-type pathname) *file-type-extension-table* nil))

(defun set-mac-file-type&creator (pathname &optional report-stream)
   (let ((entry  (get-file-type-pathname-extension-entry pathname)))
      (when entry
          (destructuring-bind (type &optional creator) (cdr entry)
             (when type
                 (ccl:set-mac-file-type pathname type))
             (when creator
                 (ccl:set-mac-file-creator pathname creator)))
          (when report-stream
              (format report-stream "~&Autotype: ~A" pathname))
          pathname))) 

(defmethod autotype-file ((pathname pathname) &optional report-stream)
   (if (ccl:directoryp pathname)
      (dolist (path (directory (merge-pathnames pathname "*.*") :directories t :files t :resolve-aliases t))
         (autotype-file path report-stream))
      (set-mac-file-type&creator pathname report-stream)))

(defmethod autotype-file ((pathname string) &optional report-stream)
   (autotype-file (pathname pathname) report-stream)) 

(defun autotype-folder (&optional report-stream)
   (let ((directory (ccl:choose-directory-dialog)))
      (when directory
          (autotype-file directory report-stream))))

(defun standardize-file-pathname-type (pathname &optional stream)
   (let* ((type (pathname-type pathname))
             (entry (get-file-type-pathname-extension-entry pathname)))
      (when entry
          (unless (equal type (caar entry))
             (let* ((new-type (caar entry))          ; conses
                       (new-path (make-pathname :name (pathname-name pathname) :type new-type :defaults pathname)))
                (rename-file pathname new-path)
                (when stream
                    (format stream "~&Standardize: ~A" new-path))
                new-path)))))

(defmethod standardize-pathname-type ((pathname pathname) &optional report-stream)
   (if (ccl:directoryp pathname)
      (dolist (path (directory (merge-pathnames pathname "*.*") :directories t :files t :resolve-aliases t))
         (standardize-pathname-type path report-stream))
      (standardize-file-pathname-type pathname report-stream)))

(defmethod standardize-pathname-type ((pathname string) &optional report-stream)
   (standardize-pathname-type (pathname pathname) report-stream))
