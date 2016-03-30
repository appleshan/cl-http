(in-package :www-utils)

(defparameter %wild-indicator% 
  #+(or clisp ecl) "*"
  #-(or clisp ecl) "*.*")

(defun %directify-pathname (name indicator)
  #-ecl (merge-pathnames name indicator)
  #+ecl (make-pathname 
	 :name :wild 
	 :type :wild
	 :version nil
	 :defaults (translate-logical-pathname name)))
;;; FIXME Don't know how to get the directory listing to work in ecl
;;; Does not work as advertised in macosx

(defun directory-list* (pathname predicate &rest options)
  "Accepts the options :FILES :DIRECTORIES :SORTED :PROPERTIES."
  (let ((pathnames (apply #'unix-directory-list* (%directify-pathname pathname %wild-indicator%) predicate options)))
    (when (member :sorted options)
      (setq pathnames (sort pathnames
                            #'directory-name-type<)))
    (cond ((member :properties options)
           (loop for path in pathnames
               collect  (if (pathname-directory-p path)
                            (list path)
                          (multiple-value-bind (length creation-date)
                              (file-properties path) 
                            `(,path 
                              ,.(when length `(:length-in-bytes ,length))
                              ,.(when creation-date `(:creation-date ,creation-date)))))))
          (t pathnames))))

#|
(make-pathname 
 :name :wild 
 :type :wild
 :version nil
 :defaults (translate-logical-pathname #P"http:examples;twistdown-tree;java;" ))
(directory-list* "http:" nil :files :directories :properties)
(remove-if-not #'pathname-directory-p (directory-list* "http:" nil :files :directories))
(directory-list* "/Users/karstenpoeck/lisp/cl-http-svn/contrib/" nil :files :directories :properties)
(unix-directory-list* (merge-pathnames "/Users/karstenpoeck/Documents/workspace/trunk/contrib/kpoeck/port-template/" "*.*") 
		      nil :files :directories)

(directory-list* "/Users/karstenpoeck/Documents/workspace/trunk/contrib/kpoeck/port-template/" nil :files :directories :properties)

(directory "/Users/karstenpoeck/Documents/workspace/trunk/contrib/kpoeck/port-template/*.*")
(directory "/Users/karstenpoeck/Documents/workspace/trunk/contrib/kpoeck/port-template/*")
(directory "/Users/karstenpoeck/Documents/workspace/trunk/contrib/kpoeck/port-template/*/")

|#

(defmethod file-length-in-bytes ((pathname pathname) &optional new-length)
   (declare (ignore new-length))
   (with-open-file (file-stream pathname)
     (file-stream-length-in-bytes file-stream)))

(defun file-stream-length-in-bytes (file-stream)
  "Returns the length in bytes for FILE-STREAM's source file."
  (file-length file-stream))

(defun file-version (pathname)
  (when (probe-file pathname)
    (file-stream-creation-date pathname)))

(defun file-stream-creation-date (file-stream)
  "Returns the creation date in universal time for FILE-STREAM's source file."
  (file-write-date file-stream))

(defun file-stream-version (file-stream)
  (file-stream-creation-date file-stream))

(defun file-stream-modification-date (file-stream)
  (file-write-date file-stream))

(defun file-creation-date (pathname)
  #+no
  (with-open-stream (stream pathname)
    (file-write-date pathname))
  (file-write-date pathname))

;;; Taken from cl-fad
;;; Copyright (c) 2004, Peter Seibel.  All rights reserved.
;;; Copyright (c) 2004-2007, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
(defun %component-present-p (value)
  "Helper function for DIRECTORY-PATHNAME-P which checks whether VALUE
is neither NIL nor the keyword :UNSPECIFIC."
  (and value (not (eql value :unspecific))))

(defun %directory-pathname-p (pathspec)
  "Returns NIL if PATHSPEC \(a pathname designator) does not designate
a directory, PATHSPEC otherwise.  It is irrelevant whether file or
directory designated by PATHSPEC does actually exist."
  (and 
    (not (%component-present-p (pathname-name pathspec)))
    (not (%component-present-p (pathname-type pathspec)))
    pathspec))

(defun %pathname-as-directory (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to directory
form."
  (let ((pathname (pathname pathspec)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (cond ((not (%directory-pathname-p pathspec))
           (make-pathname :directory (append (or (pathname-directory pathname)
                                                 (list :relative))
                                             (list (file-namestring pathname)))
                          :name nil
                          :type nil
                          :defaults pathname))
          (t pathname))))

(defun %pathname-as-file (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to file form."
  (let ((pathname (pathname pathspec)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (cond ((%directory-pathname-p pathspec)
           (let* ((directory (pathname-directory pathname))
                  (name-and-type (pathname (first (last directory)))))
             (make-pathname :directory (butlast directory)
                            :name (pathname-name name-and-type)
                            :type (pathname-type name-and-type)
                            :defaults pathname)))
          (t pathname))))

(defun %file-exists-p (pathspec)
  "Checks whether the file named by the pathname designator PATHSPEC
exists and returns its truename if this is the case, NIL otherwise.
The truename is returned in `canonical' form, i.e. the truename of a
directory is returned as if by PATHNAME-AS-DIRECTORY."
  #+(or :sbcl :lispworks :openmcl :ecl :digitool) (probe-file pathspec)
  #+:allegro (or (excl:probe-directory (%pathname-as-directory pathspec))
                 (probe-file pathspec))
  #+(or :cmu :scl :abcl) (or (probe-file (%pathname-as-directory pathspec))
                             (probe-file pathspec))
  #+:cormanlisp (or (and (ccl:directory-p pathspec)
                         (%pathname-as-directory pathspec))
                    (probe-file pathspec))
  #+:clisp (or (ignore-errors
                 (let ((directory-form (%pathname-as-directory pathspec)))
                   (when (ext:probe-directory directory-form)
                     directory-form)))
               (ignore-errors
                 (probe-file (pathname-as-file pathspec))))
  #-(or :sbcl :cmu :scl :lispworks :openmcl :allegro :clisp :cormanlisp :ecl :abcl :digitool)
  (error "FILE-EXISTS-P not implemented"))

(defun %directory-exists-p (pathspec)
  "Checks whether the file named by the pathname designator PATHSPEC
exists and if it is a directory.  Returns its truename if this is the
case, NIL otherwise.  The truename is returned in directory form as if
by PATHNAME-AS-DIRECTORY."
  #+:allegro
  (and (excl:probe-directory pathspec)
       (%pathname-as-directory (truename pathspec)))
  #+:lispworks
  (and (lw:file-directory-p pathspec)
       (%pathname-as-directory (truename pathspec)))
  #-(or :allegro :lispworks)
  (let ((result (%file-exists-p pathspec)))
    (and result
         (%directory-pathname-p result)
         result)))
;;; End code from cl-fad

(defun pathname-directory-p (pathname)
  "Returns non-null if PATHNAME denotes a directory."
  #+allegro
  (excl:file-directory-p pathname)
  #+clozure-common-lisp
  (ccl:directory-pathname-p pathname)
  #+(and no lispworks)
  (lw:file-directory-p pathname)
  #-(or allegro clozure-common-lisp (and no lispworks))
  (%directory-pathname-p pathname))

(defun file-properties (pathname)
  "Returns the length in bytes  and the creation in in universal time 
for FILE-STREAM's source file."
  (with-open-file (stream pathname)
    (values 
     (file-length stream)
     (file-write-date stream))))

(defun create-directories-recursively (pathname)
  "Recursively create directories according to the directories present in PATHNAME."
  (ensure-directories-exist pathname))

(defun directory-info (pathname &key (name :wild) (type :wild) (version :newest) (sort-pathnames t)
                                 directories)
  "Returns a poperty list of information for every file in the directory PATHNAME
that matches pathnames wildcards. Directories are included when directories is non-null."
  (declare (notinline))
  (flet ((get-directory-listing (p &optional (sort-p sort-pathnames))
           (let ((args nil))
             (declare (dynamic-extent args))
             (when sort-p (push :sorted args))
             (when directories (push :directories args))
             (apply #'directory-list p :no-extra-info args)))
         (pattern (path type)
           (make-pathname :host (pathname-host path)
			  :device (pathname-device path)
                          :directory (pathname-directory path)
                          :name (etypecase name
                                  (keyword
                                    (ecase name
                                      (:wild #-sbcl "*" #+sbcl :wild)))
                                  (string name))
                          :type (etypecase type
                                  (keyword
                                    (case type
                                      (:wild #-sbcl "*" #+sbcl :wild)
                                      (t (symbol-name type))))
                                  #-sbcl (string type)
				  #+sbcl (string (if (string= "*" type)
					     :wild
					     type))
				  )
                          :version (etypecase version
                                     (keyword
                                       (ecase version
                                         (:wild nil)
                                         (:newest :newest))))))
         (sorter (e1 e2)
           (let ((p1 (car e1))
                 (p2 (car e2)))
             (and (string<  (pathname-name p1) (pathname-name p2))
                  (let ((t1 (pathname-type p1))
                        (t2 (pathname-type p2)))
                    (cond ((and t1 t2)
                           (string< t1 t2))
                          (t1 nil)
                          (t t)))))))
    (let ((p (pathname pathname)))
      (typecase type
        (keyword
          (ecase type
            (:wild (get-directory-listing (pattern p "*")))))
        (string
          (get-directory-listing (pattern p type)))
        (cons
          (loop for type in type
                nconc (get-directory-listing (pattern p type)) into paths
                finally (return (if sort-pathnames
                                    (sort paths #'sorter)
                                  paths))))))))

;;;; Internal stuff

(defun directory-list (pathname &rest options)
  "Returns a lisp Machine style directory listing."
  (let ((pathnames (apply #'unix-directory-list* (merge-pathnames pathname "*.*") nil options)
                   ))
    (when (member :sorted options)
      (setq pathnames (sort pathnames
			    #'directory-name-type<
			    )))
			    
    (loop with length and creation-date
          for path in pathnames
          do (multiple-value-setq (length creation-date)
               (file-properties path))
          collect `(,path 
                    ,.(when length `(:length-in-bytes ,length))
                    ,.(when creation-date `(:creation-date ,creation-date))))))

;;; in clisp to get the subdirectories a "/" has to be added
(defun unix-directory-list* (pathname predicate &rest options)
  (multiple-value-bind (dirs error)
      #+allegro
    (ignore-errors (directory pathname
                                ;; This fixes directory problem on UNIX for Allegro
                                :directories-are-files t))
    #+Clozure-common-lisp
    (directory pathname :directories t)
    #+(or clisp ecl)
    (if (member :directories options)
        (append (directory pathname)
                (directory (concatenate 'string (namestring pathname) "/")))
      (directory pathname))
    #-(or allegro clozure-common-lisp clisp)
    (directory pathname)

    (when error
      (warn "Error reading directory ~s." pathname)
      (return-from unix-directory-list* nil))
    
    (if (not (member :directories options))
        (setq dirs (loop for file in dirs
                       unless (pathname-directory-p file)
                       collect file))
      (setq dirs (loop for file in dirs
                     when (pathname-directory-p file)
                     collect (unix-directory-pathname file)
                     else collect file)))
    (if predicate
        (setq dirs (loop for file in dirs
                       when (funcall predicate file)
                       collect file)))
    dirs))

(defun directory-name-type< (x y)
  (block nil
    (let (c nx ny)
      (flet ((pname (path)
                    (let ((n (and (null (pathname-name path))
                                  (null (pathname-type path))
                                  (first (last (pathname-directory path))))))
                      (if (stringp n)
                          n))))
        (setq nx (pname x) ny (pname y))
        (setq c (string< nx ny)))
      (if nx
          (if ny
              (return c)
            (return 0))
        (if ny
            (return nil))))
    (let ((a (string< (pathname-name x)
                      (pathname-name y))))
      (if a
          (let ((b (string< (pathname-type x)
                            (pathname-type y))))
            (if b
                (+ a b)
              a))))))

(defun unix-directory-pathname (pathname)
  (let ((lastdir (pathname-name pathname)))
    (if (and lastdir (not (keywordp lastdir)))
        (make-pathname 
         :directory (append (pathname-directory pathname)
                            (list lastdir))
         :name nil
         :type nil
         :version nil
         :defaults pathname)
      pathname)))

(defun %pathname-location (pathname)
  (make-pathname :name nil :type nil :defaults pathname))

(defgeneric probe-directory (pathname)
  (:documentation "Returns non-null if the directory pathname exists."))

(defmethod probe-directory ((pathname pathname))
  (setq pathname (translate-logical-pathname pathname))
  (or (%directory-exists-p pathname)
      (if (%directory-pathname-p pathname)
	  nil
	  (%directory-exists-p (%pathname-location pathname)))))

(defmethod probe-directory ((pathname string))
  (probe-directory (pathname pathname)))

;;; Fix-me, shouldnt be better take the current timestamp?
;;; Dont crash when pathname not existing
(defmethod file-modification-date ((me pathname))
  (if (or (probe-directory me)(%directory-pathname-p me))
      (encode-universal-time 0 0 0 1 1 1990)
  (with-open-file (stream me)
    (file-stream-modification-date stream))))
