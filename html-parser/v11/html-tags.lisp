;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: HTML-PARSER -*-
;;
;; Contains code and definitions for handling tags and the DTD.
;;
;; Copyright (c) 1996-98,2000 Sunil Mishra <smishra@everest.com>,
;; portions copyright (c) 1999 Kaelin Colclasure <kaelin@acm.org>,
;; all rights reserved.
;;
;; $Id: //depot/cl-http/html-parser-11/html-tags.lisp#1 $
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

;;; Constants & Variables

(eval-when (:compile-toplevel :load-toplevel)
  (defconstant *combination-operators* '(:or :set :sequence :and))
  (defconstant *unary-operators* '(+ * ?)))

(defconstant *character-data-classes* '(#t"CDATA" #t"PCDATA"))
(defvar *pcdata-default-container* #t"p")

(defvar *dtd-tags*)
(defvar *dtd-toplevel-tags*)
(defvar *html-characters* nil)

;;; Parameter entity management

#-cl-http
(defun clear-parameter-entities ()
  (map-entity-tokens #'(lambda (token-name token)
			 (when (parameter-entity-token-p token)
			   (remove-entity-token token-name)))))

#-cl-http
(defun clear-pero-cache ()
  (map-entity-tokens #'(lambda (token-name token)
			 (declare (ignore token-name))
			 (remove-value token :entity-translation-cache))))

#+cl-http
(defun clear-parameter-entities ()
  nil)

#+cl-http
(defun clear-pero-cache ()
  nil)

;;; Expression tokenization and simplification

(declaim (type function rewrite-sexp))

(defun resolve-sgml-operators (content)
  (etypecase content
    (cons (rewrite-sexp content :dtd-parser :recursive t :duplicates nil))
    (atom content)))

(defun operationp (item)
  (and (atom item)
       (or (keywordp item)
	   (case item
	     (#.*unary-operators* t)))))

(defun tokenize-names-for-definition (item force-list-p)
  (let ((result
	 (typecase item
	   (html-entity-token
	    (let* ((cached-value (get-value item :entity-translation-cache))
		   (result (or cached-value
			       (%tokenize-names-for-definition
				(entity-definition item)))))
	      (when (and (typep item 'html-entity-token) (null cached-value))
		(setf (get-value item :entity-translation-cache) result))
	      result))
	   (t (%tokenize-names-for-definition item)))))
    (if (or (not force-list-p) (listp result))
	result
      (list result))))

(defun %tokenize-names-for-definition (item)
  (etypecase item
    (null nil)
    (symbol (if (operationp item)
		item
	      (intern-name-token (symbol-name item))))
    (string (intern-name-token item))
    (number item)
    (html-entity-token
     (assert (parameter-entity-token-p item))
     (or (get-value item :entity-translation-cache)
	 (tokenize-names-for-definition item nil)))
    (html-name-token item)
    (cons (cons (%tokenize-names-for-definition (car item))
		(%tokenize-names-for-definition (cdr item))))))

(defun remove-leading-or (exp)
  (assert (eq (car exp) :or))
  (cdr exp))

;;; Attribute construction

(defun attribute-default-value-form (val)
  (typecase val
    (keyword val)
    (symbol (assert val)
	    (intern-name-token (symbol-name val)))
    (html-name-token val)
    (html-entity-token
     (let ((edef (entity-definition val)))
       (assert (atom edef))
       (attribute-default-value-form edef)))
    (t val)))

(defun parse-attr-value-type (value-type)
  (etypecase value-type
    (html-entity-token (tokenize-names-for-definition value-type nil))
    (html-name-token value-type)
    (symbol (intern-name-token (symbol-name value-type)))
    (cons (let ((result (resolve-sgml-operators
			 (tokenize-names-for-definition value-type nil))))
	    (if (atom result)
		(list result)
	      (remove-leading-or result))))))

(defun %make-attribute-def (exp)
  (multiple-value-bind (attr-name value-type default-type)
      (values-list exp)
    `(make-instance 'html-attribute
       :name ,(tokenize-names-for-definition attr-name nil)
       :values ',(parse-attr-value-type value-type)
       ;; Are mime types case insensitive?
       :default-value ,(attribute-default-value-form default-type))))

(defun make-html-attribute-forms (attrs-list)
  (when attrs-list
    (let ((exp (car attrs-list)))
      (etypecase exp
	(cons (cons (%make-attribute-def exp)
		    (make-html-attribute-forms (cdr attrs-list))))
	(html-entity-token
	 (make-html-attribute-forms (append (entity-definition exp)
					    (cdr attrs-list))))))))

;;; Element construction

(defun make-html-element-form (name start-optional end-optional attribute-forms
			       content inclusions exclusions)
  `((pushnew ,name *dtd-tags*)
    (setf (get-value ,name :tag-definition)
      (make-instance 'html-tag
	:name ,name
	:attributes ,(when attribute-forms (cons 'list attribute-forms))
	:start-optional ,start-optional
	:end-optional ,end-optional
	:contains ',content
	:inclusions ',inclusions
	:exclusions ',exclusions))))

;;; Definition macros

(defmacro define-html-element (names &key start-optional end-optional 
					  attributes content 
					  inclusions exclusions)
  (let ((names (tokenize-names-for-definition names t))
	(attribute-forms (make-html-attribute-forms attributes))
	(content (resolve-sgml-operators
		  (tokenize-names-for-definition content t)))
	(inclusions (resolve-sgml-operators 
		     (tokenize-names-for-definition inclusions t)))
	(exclusions (resolve-sgml-operators 
		     (tokenize-names-for-definition exclusions t))))
    (cons 'progn
	  (if (cdr names)
	      (loop for name in (remove-leading-or
				 (resolve-sgml-operators names))
		  nconc (make-html-element-form name start-optional
						end-optional attribute-forms
						content inclusions exclusions))
	    (make-html-element-form (car names) start-optional end-optional
				    attribute-forms content
				    inclusions exclusions)))))
	
(defmacro define-html-entity (token value)
  (let ((token (etypecase token
		 (string (intern-entity-token token))
		 (html-entity-token token))))
    (if (parameter-entity-token-p token)
	`(eval-when (:compile-toplevel)
	   (setf (get-value ,token :entity-definition) ',value))
      `(eval-when (:load-toplevel)
	 (setf (get-value ,token :entity-definition) ',value)))))

;;; Tag operations

(defun initialize-tags ()
  (mapc #'(lambda (dtd-tag)
	    #-debug (declare (ignore dtd-tag))
	    #+debug (setf (instances dtd-tag) nil))
	(mapcar #'tag-definition *dtd-tags*)))

(defmethod tag-attribute-definition ((attribute html-name-token) 
				     (tag-defn html-tag))
  (find attribute (attributes tag-defn) :key #'name))

(defmethod tag-attribute-definition ((attribute string) tag-defn)
  (tag-attribute-definition (intern-name-token attribute) tag-defn))

(defmethod tag-attribute-definition (attribute (tag-name html-name-token))
  (tag-attribute-definition attribute (tag-definition tag-name)))

(defmethod tag-attribute-definition (attribute (tag-name string))
  (tag-attribute-definition attribute
			    (tag-definition (intern-name-token tag-name))))

(defun define-unknown-tag ()
  (push #t"UNKNOWN" *dtd-tags*)
  (setf (get-value #t"UNKNOWN" :tag-definition)
    (make-instance 'html-tag
      :name #t"UNKNOWN"
      :contains nil
      :inclusions nil
      :exclusions nil
      :default-container #t"BODY"
      :attributes nil)))

(defun set-html-tag-backpointers ()
  (loop
      with character-data-classes = *character-data-classes*
      for tag-token in *dtd-tags*
      for exclusions = (exclusions tag-token)
      for inclusions = (inclusions tag-token)
      for contains = (contains tag-token)
      do (loop
	     for item in (flatten (list contains inclusions))
	     for content-tag = (if (or (symbolp item)
				       (member item character-data-classes))
				   nil
				 (tag-definition item))
	     when (and content-tag (not (member content-tag exclusions)))
	     do (push tag-token (containers content-tag))))
  (setq *dtd-toplevel-tags*
    (loop
	for tag-token in *dtd-tags*
	if (null (containers tag-token)) collect tag-token))
  #+debug (assert *dtd-toplevel-tags*))

(defun do-breadth-search (nodes child-fn do-fn &aux visited-nodes)
  ;; A generalized BFS algorithm that calls do-fn on a node, then
  ;; pushes the result of calling child-fn on node on the queue.
  (loop
      with q = (copy-list nodes)
      with q-end = (last q)
      for node = (pop q)
      until (or (null node) (funcall do-fn node))
      unless (member node visited-nodes)
        do (push node visited-nodes)
	   (let ((children (funcall child-fn node)))
	     (when children
	       (cond (q (setf (cdr q-end) (copy-list children))
			(setf q-end (last q-end)))
		     (t (setf q (copy-list children))
			(setf q-end (last q))))))))

(defun most-general-tag (tags &aux general-tag)
  (cond ((cdr tags)
	 (do-breadth-search
	     *dtd-toplevel-tags*
	   #'(lambda (tag)
	       (unless (member tag *character-data-classes*)
		 (flatten (contains (tag-definition tag)) :key #'cdr)))
	   #'(lambda (tag)
	       (when (and (member tag tags)
			  (start-optional-p (tag-definition tag)))
		 (setq general-tag tag) t)))
	 general-tag)
	(t (car tags))))

(defun set-html-tag-default-containers ()
  (dolist (tag *dtd-tags*)
    (unless (member tag *dtd-toplevel-tags*)
      (let ((tag-definition (tag-definition tag)))
	(setf (default-container tag-definition)
	  (most-general-tag (containers tag-definition)))))))

;;; EOF
