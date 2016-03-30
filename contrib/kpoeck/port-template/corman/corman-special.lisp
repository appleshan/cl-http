(in-package :common-lisp)

(defun add-package-nickname (package new-name)
    (let ((p (find-package package)))
        (if p
            (setf (uref p package-nicknames-offset)
                 (cons (canonicalize-string-designator new-name) (uref package package-nicknames-offset)))
        (Error "Not a package ~a~%"))))