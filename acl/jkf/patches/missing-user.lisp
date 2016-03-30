(in-package :cl-user)

#+:common-graphics
(defun user::prompt-user-string (string)
  (multiple-value-bind
        (result ignore1 ignore2 result-okp)
      (cg:ask-user-for-string string "" "Ok" "Cancel")
    (declare (ignore ignore1 ignore2))
    (if result-okp
        result
      nil)))

#-:common-graphics
(defun user::prompt-user-string (string)
  (format t "~a~%" string)
  (read-line))

;;; LOAD-LOGICAL-PATHNAME-TRANSLATIONS-PATCH
;;; found in logidef.lisp

(defun user::load-logical-pathname-translations-patch (host &key all)
  (loop with hostspath = (mapcar #'merge-pathnames (mapcar #'translate-logical-pathname (logical-pathname-translations-database-pathnames)))
      with translations
      for path in hostspath
      do (if (probe-file path)
             (with-open-file (stream path :direction :input)
               (loop with eof = '#:eof
                   as read = (read stream nil eof)
                   until (eq read eof)
                   if (equalp read host)
                   do (flet ((coltrans ()
                                       (loop do (setq read (read stream nil eof))
                                           while (consp read)
                                           collect (eval read))))
                        (when (setq translations (coltrans))
                          (setf (logical-pathname-translations host) translations)
                          (when all
                            (loop with translations
                                as host = read
                                do (cond ((stringp host)
                                          (if (setq translations (coltrans))
                                              (setf (logical-pathname-translations host) translations)
                                            (error "Host translations for ~s not found in ~s." host hostspath)))
                                         ((eq read eof)
                                          (return))
                                         (t
                                          (error "Unexpected host ~s in ~s." host hostspath))))))
                        (return)))))
      if translations
      do (return translations)
      finally (error "Host ~s not found in ~s." host hostspath)))