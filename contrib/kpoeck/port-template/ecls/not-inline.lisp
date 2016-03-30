(defun generate-noinline (file package)
  (let ((*package* package))
  (with-open-file (stream file)
    (let ((result nil))
      (loop
        (let ((sexp (read stream nil :end)))
        (when (eq :end sexp)
          (return))
          (when (listp sexp)
            (case (first sexp)
              ((define defun defmethod) 
               (let ((fun (second sexp)))
                 (when (and (fboundp fun)(not (listp fun))(eq (symbol-package fun) package))
                   (push fun result))))
              (t nil)))))
      `(eval-when (:compile-toplevel :load-toplevel)
         (declaim (notinline ,@result)))))))


#|
(generate-noinline "c:/data/lisp/clhttp/cl-http-70-201d-devo/server/url.lisp" (find-package :url))

|#