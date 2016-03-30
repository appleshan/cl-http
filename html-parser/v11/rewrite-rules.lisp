;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: HTML-PARSER -*-
;;
;; This file contains some simple rules that shall be used in reorganizing
;; and processing the HTML element containment rules. I do not guarantee
;; their completeness or accuracy, they are simply heuristics that I think
;; ought to make program processing simpler.
;;
;; Copyright (c) 1996-97 Sunil Mishra <smishra@everest.com>,
;; all rights reserved.
;;
;; $Id: //depot/cl-http/html-parser-11/rewrite-rules.lisp#1 $
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

(define-rewrite :dtd-parser
  (:or ?x)
  ?x)
(define-rewrite :dtd-parser
  (:and ?x)
  ?x)
(define-rewrite :dtd-parser
  (:set ?x)
  ?x)
(define-rewrite :dtd-parser
  (:sequence ?x)
  ?x)

(define-rewrite :dtd-parser
  (? (* ?x))
  (* ?x))
(define-rewrite :dtd-parser
  (? (+ ?x))
  (* ?x))

(define-rewrite :dtd-parser
  (* (:set (?* ?x)))
  (* (:or ?append ?x)))
(define-rewrite :dtd-parser
  (+ (:set (?* ?x)))
  (+ (:or ?append ?x)))

(define-rewrite :dtd-parser
  (* (:or (?* ?x) (* ?y) (?* ?z)))
  (* (:or ?append ?x ?y ?append ?z)))
(define-rewrite :dtd-parser
  (+ (:or (?* ?x) (+ ?y) (?* ?z)))
  (+ (:or ?append ?x ?y ?append ?z)))
(define-rewrite :dtd-parser
  (* (:or (?* ?x) (+ ?y) (?* ?z)))
  (* (:or ?append ?x ?y ?append ?z)))
(define-rewrite :dtd-parser
  (+ (:or (?* ?x) (* ?y) (?* ?z)))
  (* (:or ?append ?x ?y ?append ?z)))

(define-rewrite :dtd-parser
  (?op (?* ?x) (?op (?* ?y)) (?* ?z))
  (?op ?append ?x ?append ?y ?append ?z))

(define-rewrite :dtd-parser
  (:set (?* ?x) (:and (?* ?y)) (?* ?z))
  (:and ?append ?y
	?prefix ? ?x
	?prefix ? ?z))

(define-rewrite :parser-defn
  ((?* ?pre-exit) (?exit-context (?* ?x)) (?* ?post-exit))
  (?append ?pre-exit
   (unless *ignore-exits*
     (setq ?exitp-var t
	   ?exit-value-var (progn . ?x)))))

(define-rewrite :parser-defn
  (?save ?it)
  (?save ?it ?its-pd))

;;; EOF
