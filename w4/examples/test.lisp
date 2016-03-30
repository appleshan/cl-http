;;;   -*- Mode: LISP; Package: w4; BASE: 10; Syntax: ansi-common-lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-
;;;
;;; (c) Copyright  2000, John C. Mallery
;;;     All Rights Reserved.
;;;

(in-package :w4)

(defun alpha-enumerate-p (entry1 entry2)
  (string< (url:name-string (qe-url entry1)) (url:name-string (qe-url entry2))))

(defun test (url &key domain constraints (operator (http-user-email-address)) (depth 3) (search-method :depth-first)
		 (threads 1) (predicate #'alpha-enumerate-p) (life-time (* 60 5)) (report-stream *standard-output*))
  (flet ((sort-inferiors (x y)
           (string< (url:name-string x) (url:name-string y))))
    (declare (dynamic-extent #'sort-inferiors))
    (with-activity 
      ("Test-Enumeration"
       (:operator operator
	:search-method search-method
	:threads threads
	:predicate predicate
	:url-host-name-resolution :preferred
	:if-does-not-exist :uninterned
	:life-time life-time
	:report-stream report-stream)
       :constraints `(,.(when depth
			  `((depth ,depth)))
		      (no-cycles)
		      ,.(when domain
			  `((url-host-domain ,domain)))
		      ,.(when constraints constraints))
       :actions `((trace)
		  (generate-sorted-inferiors ,#'sort-inferiors)))
      (walk (url:intern-url url :if-does-not-exist :uninterned) activity))))