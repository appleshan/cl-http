(in-package :cl)

;;; KAP 2006-10-14
;;;into-pkg-name has to be quoted
(defun build-import-forms (into-pkg-name specification-list shadowing)
    (let (forms)
      (do* ((still-to-process specification-list (cdr still-to-process))
            (spec (car still-to-process) (car still-to-process)))
            ((null still-to-process) nil)
        (let* ((package (canonical-package-name (car spec)))
               (symbol-names (mapcan 
                               #'(lambda (name)
                                   (let ((sym (find-symbol (string name) package)))
                                     (if sym
                                       (list sym)
                                       (progn (warn
                                                "Symbol ~A was not found in the package named \"~A\" and will not be imported."
                                                name package) nil))))
                               (cdr spec))))
			(push `(,(if shadowing 'shadowing-import 'import) ',symbol-names ',into-pkg-name) forms)))
      `(progn ,@forms)))