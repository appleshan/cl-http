;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: HTML-PARSER -*-
;;
;; This is a simple rule based list rewriting system that relies on a
;; pattern matcher capable of matching segment variables. This is a one time
;; operation that shall be done at compile time, so I have not bothered to
;; do any kind of optimization of this code.
;;
;; Copyright (c) 1996-97 Sunil Mishra <smishra@everest.com>,
;; all rights reserved.
;;
;; $Id: //depot/cl-http/html-parser-11/rewrite-engine.lisp#1 $
;;
;; This library is free software; you can redistribute it and/or modify it
;; under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or (at
;; your option) any later version.
;;
;; This library is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
;; General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this library; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(in-package :html-parser)

;;; Parameters

(defparameter *rewrite-rules* nil)
(defparameter *processing-instructions* nil)

;;; Data structures

(defstruct (rewrite-rule (:conc-name "REWRITE-"))
  (pattern nil)
  (result nil)
  (context nil))

;;; Processing instructions

(defun fetch-processing-instruction (sym)
  (cdr (assoc sym *processing-instructions*)))

(defun execute-processing-instructions (tree)
  (etypecase tree
    (cons
     (let ((instruction (fetch-processing-instruction (car tree))))
       (if instruction
	   (funcall instruction (cdr tree))
	 (let ((processed-car (execute-processing-instructions (car tree)))
	       (processed-cdr (execute-processing-instructions (cdr tree))))
	   (if (and (eq processed-car (car tree))
		    (eq processed-cdr (cdr tree)))
	       tree
	     (cons processed-car processed-cdr))))))
    (atom tree)))

(defun define-processing-instruction (instruction fn)
  (push (cons instruction fn) *processing-instructions*))

(defun execute-prefix-instruction (args)
  (let ((item (car args)))
    (nconc (mapcar #'(lambda (list-item)
		       (list item list-item))
		   (execute-processing-instructions (cadr args)))
	   (execute-processing-instructions (cddr args)))))

(define-processing-instruction '?prefix #'execute-prefix-instruction)

(defun execute-append-instruction (args)
  (append (execute-processing-instructions (car args))
	  (execute-processing-instructions (cdr args))))

(define-processing-instruction '?append #'execute-append-instruction)

(defun delete-duplicates-recursive (list)
  (etypecase list
    (cons
     (etypecase (car list)
       (cons
	(setf (car list) (delete-duplicates-recursive (car list))))
       (atom nil))
     (delete-duplicates list :test #'equal))
    (atom list)))

;;; Top level forms

(defun fetch-matching-rule (sexp context bindings)
  (loop 
      for rule in *rewrite-rules*
      for new-bindings = (when (eq (rewrite-context rule) context)
			   (pat-match (rewrite-pattern rule) sexp bindings))
      when new-bindings return (values rule new-bindings)))

(defun rewrite-sexp-non-recursive (sexp context duplicates bindings)
  (loop
      with rule and new-bindings
      for result-sexp = sexp then (construct-rewrite 
				   (rewrite-result rule) new-bindings)
      do (multiple-value-setq (rule new-bindings)
	   (fetch-matching-rule result-sexp context bindings))
      while rule
      finally (return (if duplicates
                          result-sexp
			(delete-duplicates-recursive result-sexp)))))

(defun rewrite-sexp-recursive (sexp context duplicates bindings 
			       &optional (start-p t))
  (when (consp sexp)
    (setf (car sexp)
      (rewrite-sexp-recursive (car sexp) context duplicates bindings))
    (rewrite-sexp-recursive (cdr sexp) context duplicates bindings nil))
  (when start-p
    (rewrite-sexp-non-recursive sexp context duplicates bindings)))

(defun rewrite-sexp (sexp context 
		     &key recursive (duplicates t) (bindings no-bindings))
  (if recursive
      (rewrite-sexp-recursive sexp context duplicates bindings)
    (rewrite-sexp-non-recursive sexp context duplicates bindings)))

(defun construct-rewrite (rewrite bindings)
  (execute-processing-instructions (sublis bindings rewrite)))

(defmacro define-rewrite (context pattern result)
  `(push (make-rewrite-rule :pattern ',pattern :result ',result
                            :context ',context)
         *rewrite-rules*))

;;; EOF
