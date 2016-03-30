(in-package :http)

;;; Stolen from lw/server/unix.lisp
;;; I honestly don't understand why this code is not in the portable sources

#|
(defvar %plist-table (make-hash-table :test 'equal))

(defmethod get-value ((pathname pathname) indicator &optional default)
  (let ((was (gethash pathname %plist-table )))
    (cond (was (getf was indicator default))
          (t default))))

(defmethod (setf get-value) (value (pathname pathname) indicator &optional default)
  (declare (ignore default))
  (let ((was (gethash pathname %plist-table )))
    (cond ((null was) (setf (gethash pathname %plist-table)
                        (list indicator value)))
          (t (setf (getf was indicator) value)))))
|#

;;;------------------------------------------------------------------- 
;;;
;;; AN INTERFACE CONSISTENT WITH PROPERTY LIST MIXIN FOR PATHNAMES
;;;
;;; This facility compensate for Lisps that can't implement pathname property lists
;;; directly on pathnames. Lack of EQ pathnames means that a plist slot on a pathname 
;;; is useless.
(defvar *pathname-property-list-table* nil
  "Holds the property lists for pathnames in the file system.")

(declaim (inline pathname-property-list-table))

;; pathnames are not eq in LW 4.3!!! -- JCMa 9/4/2003
;; change the hashtable test to EQ when they are fixed.

(defun pathname-property-list-table ()
  (or *pathname-property-list-table*
      (setf *pathname-property-list-table* (make-hash-table :test #'equal))))

(defun clear-pathname-property-list-table ()
   (when *pathname-property-list-table*
       (clrhash *pathname-property-list-table*)))

(declaim (inline %pathname-property-list))

(defun %pathname-property-list (pathname)
  (gethash pathname (pathname-property-list-table)))

(declaim (inline %set-pathname-property-list))

(defun %set-pathname-property-list (pathname plist)
  (setf (gethash pathname (pathname-property-list-table)) plist))

(defsetf %pathname-property-list %set-pathname-property-list)

(declaim (inline %pathname-property-list-put-value))

(defun %pathname-property-list-put-value (pathname indicator value)
   (let ((plist (%pathname-property-list pathname)))
      (prog1 (setf (getf plist indicator) value)
         (setf (%pathname-property-list pathname) plist))))

(declaim (inline %remove-property-list))

(defun %remove-property-list (pathname)
  (remhash pathname (pathname-property-list-table))) 

(defmethod get-value ((pathname pathname) indicator &optional default)
  (let ((value (getf (%pathname-property-list pathname) indicator :+not-found+)))
    (case value
      (:+not-found+
        (values default nil))
      (t (values value t))))) 

(defmethod %put-value ((pathname pathname) indicator value)
   (%pathname-property-list-put-value pathname indicator value))

(defmethod (setf get-value) (value (pathname pathname) indicator &optional default)
   (declare (ignore default))
   (%pathname-property-list-put-value pathname indicator value)) 

(defmethod remove-value ((pathname pathname) indicator)
  (let ((plist (%pathname-property-list pathname)))
    (when plist
      (prog1 (remf plist indicator)
             (if plist
                 (setf (%pathname-property-list pathname) plist)
                 (%remove-property-list pathname))))))

(defmethod property-list ((pathname pathname))
  (%pathname-property-list pathname))

(defmethod map-indicators ((pathname pathname) function)
  (loop for item in (%pathname-property-list pathname) by #'cddr
        do (funcall function item)))

(defmethod map-values ((pathname pathname) function)
  (loop for item in (cdr (%pathname-property-list pathname)) by #'cddr
        do (funcall function item))) 

