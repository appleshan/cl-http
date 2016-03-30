;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: HTML-PARSER -*-
;;
;; Utilities for the HTML parser package.
;;
;; Copyright (c) 1996-98,2000 Sunil Mishra <smishra@everest.com>,
;; portions copyright (c) 1999 Kaelin Colclasure <kaelin@acm.org>,
;; all rights reserved.
;;
;; $Id: //depot/cl-http/html-parser-11/html-utilities.lisp#1 $
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

;;; Attr-val accessor

(defun attr-val (attr tag-data &optional default-value error-if-not-found)
  (let ((attr-val
	 (find (if (typep attr 'html-name-token)
		   attr 
		 (intern-name-token (string attr)))
	       (attr-values tag-data)
	       :key #'(lambda (val-pair)
			(name (car val-pair))))))
    (cond (attr-val (cdr attr-val))
          (error-if-not-found
           (error "Attribute ~A unbound in ~A" attr tag-data))
          (t default-value))))

(defsetf attr-val (name tag) (value)
  `(let ((tag-attr-def (tag-attribute-definition ,name (instance-of ,tag)))
	 (new-value ,value))
     (setf (attr-values ,tag)
       (if new-value
	   (acons tag-attr-def new-value
		  (remove tag-attr-def (attr-values ,tag) :key #'car))
	 (remove tag-attr-def (attr-values ,tag) :key #'car)))
     new-value))

;;; Simple parser

;; The parser definition below shall take a string as argument and return
;; a parsed structure.

(define-html-parser-context simple-parse-context ()
  (:on-open-tag (:any (save it)))
  (:on-pcdata (save it))
  (:on-close-tag (html (exit-context it))))

(define-html-parser simple-parser ()
  (:transitions (:start (simple-parse-context))
		(simple-parse-context t :end)))

(defun file->string (path)
  (with-open-file (stream path
		   :direction :input
		   :element-type *string-char-type*)
    (stream->string stream)))

(defun stream->string (stream)
  (declare (optimize (speed 3) (debug 0) (space 1)))
  (with-output-to-string (string-stream)
    (loop 
	for char = (read-char stream nil *eof*)
	until (eq char *eof*)
	do (write-char char string-stream))))

;;; EOF
