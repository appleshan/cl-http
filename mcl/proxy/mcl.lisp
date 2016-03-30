;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-

;;; (C) Copyright 2000, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;; (C) Copyright 1996-97, Christopher R. Vincent
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; CL-HTTP PROXY SERVER
;;;

(in-package :http)

(defgeneric handle-set-file-type (handle file-type)
   (:documentation "Sets the MAC file type of HANDLE to be FILE-TYPE."))

(defmethod handle-set-file-type (handle file-type)
   (declare (ignore handle file-type))
   nil)

(defmethod handle-set-file-type ((handle filesystem-handle) file-type)
   (ccl:set-mac-file-type (filesystem-handle-pathname handle) file-type))

(declaim (special *proxy-cache*))

(defun make-cache-mcl-readable (&optional (cache *proxy-cache*))
   (flet ((make-files-readable (rep)
                (handle-set-file-type  (representation-metadata-handle rep) :text)
                (handle-set-file-type  (representation-entity-handle rep) :text)))
      (map-representations cache #'make-files-readable)
      cache))


#+cl-http-menu
(cl-http-menu:add-item-to-inspect "Proxy Cache"
                                  '*proxy-cache*
                                  (documentation '*proxy-cache* 'variable))
