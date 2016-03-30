(in-package :www-utils)

;;;; http:port;server;unix.lisp
;;;; use in all version of allegro, not only until acl5.0

(defun directory-list* (pathname predicate &rest options)
  "Accepts the options :FILES :DIRECTORIES :SORTED :PROPERTIES."
  (let ((pathnames #+allegro (apply #'unix-directory-list* (merge-pathnames pathname "*.*") predicate options)))
    (when (member :sorted options)
      (setq pathnames (sort pathnames
			     #'directory-name-type<)))
    (cond ((member :properties options)
           (loop for path in pathnames
                 collect  (multiple-value-bind (length creation-date)
                              (file-properties path) 
                            `(,path 
                              ,.(when length `(:length-in-bytes ,length))
                              ,.(when creation-date `(:creation-date ,creation-date))))))
          (t pathnames))))