(in-package mit-defgeneric)

(defun %filter-options-methods (options-methods)
  (mapcar #'(lambda(option)
              (cond ((member (first option)
                             '(:argument-precedence-order :documentation :method-combination :generic-function-class :method-class))
                     option)
                    ((eq (first option) 'declare)
                     (cons 'declare (%fix-declare (rest option))))
                    (t (warn "Don't know how to parse ~a~%" option))))
    options-methods))

(defun %fix-declare (decls)
  (let ((res nil))
    (dolist (decl decls)
      (if (eq 'optimize (first decl))
          (push decl res)
	  #-no nil
	  #+no (warn "Non ansi declaration in defgeneric ~a~%" decl)))
    (nreverse res)))

(defmacro defgeneric (function-name lambda-list &rest options-methods)
  `(cl:defgeneric ,function-name ,lambda-list ,@(%filter-options-methods options-methods)))


#|
(defgeneric otto (me string))

(defgeneric md5-encode-stream (stream element-type &optional start end)
  (declare (values md5-digest))
  (:documentation "Produces an MD5 digest for stream from start upto end according to element-type."))

(defgeneric get-value (property-list-mixin indicator &optional default)
  (declare (values value found-p))
  (:documentation "Gets the value stored under INDICATOR from PROPERTY-LIST-MIXIN or returns DEFAULT.
This returns a second value indicating whether the value was found or not."))

(defgeneric character-stream-get-byte-reader (character-stream)
  (declare (values byte))
  (:documentation "Returns a function that will pop the next byte off a character stream."))

(defgeneric binary-stream-get-byte-reader (stream)
  (declare (values byte))
  (:documentation "Returns a function that will pop the next byte off a binary stream."))

define-generic
|#
  