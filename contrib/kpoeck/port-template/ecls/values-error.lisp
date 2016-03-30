(in-package :cl-user)

(defvar *debug-server* nil)

(defmacro handler-case-if (condition form &body clauses)
  "Sets up condition handlers when condition returns non-null."
  `(flet ((execute-form () ,form))
     (declare (inline execute-form))
     (cond (,condition
            (handler-case (execute-form) ,@clauses))
           (t (execute-form)))))

(defmacro with-bad-escaping-resignalled ((string &key reason start end (error-class 'error)) &body body)
  `(handler-case-if (not *debug-server*)
      (progn ,@body)
     (,error-class (err)
      (error err
	     :url ,(if (and start end)
		       `(subseq ,string (max ,start 0) (min ,end (length ,string)))
		       `(progn ,string))
	     ,@(when reason `(:reason ,reason))
	     :format-string "hugo"))))

(defun %unescape-url (url-string start end destructive-p)
  (declare (fixnum start end)
           (values canonical-url new-start new-end))
  (declare (ignore destructive-p))
  (with-bad-escaping-resignalled (url-string :start start :end end
                                             :reason "Bad Escaping: Ill-escaped URL")
    (multiple-value-bind (unescaped-string chars-unescaped-p new-url-string-p)
        (string-unescape-special-chars url-string start end t)
      (declare (ignore chars-unescaped-p))
      (unless new-url-string-p
        (setq unescaped-string (subseq url-string start end)))
      (values unescaped-string 0 (length unescaped-string)))))

(defun string-unescape-special-chars (string &optional (start 0) (end (length string) end-supplied-p) (escape-fragments-p t))
  (declare (ignore start end end-supplied-p escape-fragments-p))
  (let ((new-string (make-string 10 :initial-element #\a)))
    new-string))

(defun test ()
  (let ((url "http://localhost/foo.html"))
    (%unescape-url url 0 (length url) nil)))

(defun %unescape-url-2 (url-string start end destructive-p)
  (declare (fixnum start end)
           (values canonical-url new-start new-end))
  (declare (ignore destructive-p))
  (progn
    (multiple-value-bind (unescaped-string chars-unescaped-p new-url-string-p)
        (string-unescape-special-chars url-string start end t)
      (declare (ignore chars-unescaped-p))
      (unless new-url-string-p
        (setq unescaped-string (subseq url-string start end)))
      (values unescaped-string 0 (length unescaped-string)))))

(defun test2 ()
  (let ((url "http://localhost/foo.html"))
    (%unescape-url-2 url 0 (length url) nil)))