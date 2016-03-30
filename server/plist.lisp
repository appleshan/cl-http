;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-

;;; Copyright 1995, 1997, 2005, John C. Mallery.
;;; All rights reserved.

;;;------------------------------------------------------------------- 
;;;
;;;  PROPERTY LIST MIXIN FOR OBJECTS
;;;

(in-package :http) 

(define-generic get-value (property-list-mixin indicator &optional default)
  (declare (values value found-p))
  (:documentation "Gets the value stored under INDICATOR from PROPERTY-LIST-MIXIN or returns DEFAULT.
This returns a second value indicating whether the value was found or not.")) 

(defmethod get-value ((plist property-list-mixin) indicator &optional default)
  (with-slots (plist) plist
    (let ((value (getf plist indicator :+not-found+)))
      (case value
        (:+not-found+
          (values default nil))
        (t (values value t))))))

(defmethod (setf get-value) (value (plist property-list-mixin) indicator &optional default)
  (declare (ignore default))
  (with-slots (plist) plist
    (setf (getf plist indicator) value)))

#+Genera
(defmethod www-utils:property-list-location ((plist property-list-mixin))
  (with-slots (plist) plist
    (scl:locf plist)))

#+Genera
(defmethod www-utils:property-list-value-location ((plist property-list-mixin) indicator &optional default)
  (with-slots (plist) plist
    (scl:locf (scl:getf plist indicator default))))

(define-generic remove-value (property-list-mixin indicator)
  (:documentation "Removes the value stored under INDICATOR from PROPERTY-LIST-MIXIN."))

(defmethod remove-value ((plist property-list-mixin) indicator)
  (with-slots (plist) plist
    (remf plist indicator)))

(define-generic remove-values  (property-list-mixin indicators)
  (:documentation "Removes the values for all INDICATORS from PROPERTY-LIST-MIXIN."))

(defmethod remove-values ((plist  property-list-mixin) indicators)
  (with-slots (plist) plist
    (loop with properties = plist
          and removed-p
          for key in indicators
          do (when (remf properties key)
               (unless removed-p
                 (setq removed-p t)))
          finally (return (if removed-p
                              (setf plist properties)
			      properties)))))

(define-generic map-indicators (property-list-mixin function)
  (:documentation "Maps FUNCTION over all the indicators of PROPERTY-LIST-MIXIN."))

(defmethod map-indicators ((plist property-list-mixin) function)
  (with-slots (plist) plist
    (loop for item in plist by #'cddr
          do (funcall function item))))

(define-generic map-values  (property-list-mixin function)
  (:documentation "Maps FUNCTION over all the values of PROPERTY-LIST-MIXIN."))

(defmethod map-values ((plist property-list-mixin) function)
  (with-slots (plist) plist
    (loop for item in (cdr plist) by #'cddr
          do (funcall function item))))

(define-generic property-list  (property-list-mixin)
  (:documentation "Returns the property list for PROPERTY-LIST-MIXIN."))

(define-macro with-value-cached ((property-list-mixin key &key (recompute-p nil recompute-supplied-p)) &body value-form)
  "Caches the value returned by value-form on PROPERTY-LIST-MIXIN's property list under the indicator KEY.
When RECOMPUTE-P is non-null, the value is recomputed and recached.
The returned values are VALUE and RETRIEVE-FROM-CACHE-P.
Remove a series of cached values with CLEAR-CACHED-VALUES."
  (declare (values cached-value retrieved-from-cache-p))
  (let ((form `(let ((val (getf plist ,key :+not-found+)))
                 (case val
                   (:+not-found+ 
                     (setf (getf plist ,key) (progn  . ,value-form)))
                   (t (values val t))))))      
    (cond (recompute-supplied-p
           `(with-slots (plist) ,property-list-mixin
              (cond (,recompute-p
                     (setf (getf plist ,key) (progn . ,value-form)))
                    (t ,form))))
          (t `(with-slots (plist),property-list-mixin ,form)))))

(define-generic clear-cached-values  (property-list-mixin cache-keys)
  (:documentation "Clears the values for each key in CACHE-KEYS from PROPERTY-LIST-MIXIN."))

(defmethod clear-cached-values ((plist property-list-mixin) cache-keys)
  (remove-values plist cache-keys))