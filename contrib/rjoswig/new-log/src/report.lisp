;;; -*- Package: NEW-LOG; Syntax: ANSI-Common-Lisp -*-

(in-package "NEW-LOG")

;;; ================================================================
;;; Simple Log Reports
;;;
;;; Copyright Rainer Joswig, joswig@lisp.de, 2006-2008


;;; ================================================================
;;; Globals

(defparameter *report-format* :text)

;;; ================================================================
;;; Printing Text Tables

(defun print-list-as-text-table (header lines stream)
  "Takes a list of headers and a list of lines
Each line is a list of objects. Calculates
max column widths."
  (let* ((items (loop for line in lines
                      collect (loop for cell in line
                                    collect (princ-to-string cell))))
         (header (loop for cell in header
                       collect (princ-to-string cell)))
         (lengths (max-cell-lengths (cons header items)))
         (column-number (length header)))
    (print-table-line header column-number lengths stream)
    (print-table-border-line lengths stream)
    (loop for line in lines
          do (print-table-line line column-number lengths stream))))

(defun print-table-border-line (lengths stream)
  (print-table-line (mapcar #'(lambda (length)
                                (make-string length :initial-element #\-))
                            lengths)
                    (length lengths)
                    lengths stream :space nil))

(defun print-table-line (line line-length lengths stream &key (space t))
  (loop for i from 1
        for cell in line
        for max-length in lengths
        for nil = nil then (if space
                             (write-string " | " stream)
                             (write-string "-+-" stream))
        do (princ cell stream)
        unless (= i line-length)
        do (print-repeated-char #\space
                                (- max-length (length cell))
                                stream))
  (terpri stream))

(defun print-repeated-char (character count stream)
  "Writes character count times to stream."
  (loop repeat count do (write-char character stream)))


(defun max-cell-lengths (lines)
  "Returns a list of maximum column widths."
  (reduce #'(lambda (l1 l2)
              (mapcar #'max l1 l2))
          (mapcar #'(lambda (line)
                      (mapcar #'length line))
                  lines)))

;;; ================================================================
;;; HTML Table

(defun print-list-as-html-table (header lines stream)
  "Takes a list of headers and a list of lines
Each line is a list of objects."
  (html4.0:with-table (:stream stream)
    (html4.0:with-table-row (:stream stream)
      (dolist (item header)
        (html4.0:with-table-cell (:header-p t :stream stream)
          (princ item stream))))
    (loop for line in lines
          do (html4.0:with-table-row (:stream stream)
               (dolist (item line)
                 (html4.0:with-table-cell (:stream stream)
                   (princ item stream)))))))


(defun print-list-as-table (header lines stream)
  "Takes a list of headers and a list of lines
Each line is a list of objects. Calculates
max column widths."
  (ecase *report-format*
    (:html (print-list-as-html-table header lines stream))
    (:text (print-list-as-text-table header lines stream))))


;;; ================================================================
;;; Compute Report Values


(defmethod number-of-successful-requests ((log new-log) &key
					  (start (get-log-start-time log))
					  (end (get-log-end-time log)))
  (range-count (new-log-transactions log)
	       (make-transaction-area :start-date start :end-date end :status-start 200 :status-end 399)))

(defmethod number-of-request-errors ((log new-log) &key
					  (start (get-log-start-time log))
					  (end (get-log-end-time log)))
  (range-count (new-log-transactions log)
	       (make-transaction-area :start-date start :end-date end :status-start 400 :status-end 599)))


(defmethod number-of-different-files-requested ((log new-log) &key
						(start (get-log-start-time log))
						(end (get-log-end-time log)))
  (range-count-different-items (new-log-transactions log)
			       (make-transaction-area :start-date start :end-date end)
			       #'transaction-url))


(defmethod number-of-different-requesting-hosts ((log new-log) &key
						 (start (get-log-start-time log))
						 (end (get-log-end-time log)))
  (range-count-different-items (new-log-transactions log)
			       (make-transaction-area :start-date start :end-date end)
			       #'transaction-host))

(defmethod bytes-served ((log new-log) &key
			 (start (get-log-start-time log))
			 (end (get-log-end-time log)))
  (let ((amount 0))
    (range-map (new-log-transactions log) (make-transaction-area :start-date start :end-date end :start-bytes 1)
               #'(lambda (transaction)
                   (incf amount (transaction-bytes transaction))))
    amount))


;;; ================================================================
;;; Report methods

(defgeneric print-report-item (log type stream &key start end))

;;; Report Interval

(defmethod print-report-item ((log new-log) (type (eql :report-interval)) stream &key
			      (start (get-log-start-time log))
			      (end (get-log-end-time log)))
  (when (and start end)
    (format stream "~%The log starts at ")
    (print-gmt-time stream start)
    (princ " and ends at " stream)
    (print-gmt-time stream end)))

; (print-report-item *test-log-instance* :report-interval t)

;;; Number of Successful Requests

(defmethod print-report-item ((log new-log) (type (eql :number-of-successful-requests)) stream &key
			      (start (get-log-start-time log))
			      (end (get-log-end-time log)))
    (format stream "~%Number of successful requests: ~a"
	    (number-of-successful-requests log :start start :end end)))

;;; Number of Successful HTML Requests
;;; Average Page Requests per day
;;; Request Errors

(defmethod print-report-item ((log new-log) (type (eql :number-of-request-errors)) stream &key
			      (start (get-log-start-time log))
			      (end (get-log-end-time log)))
  (format stream "~%Number of request errors: ~a"
	  (number-of-request-errors log :start start :end end)))

;;; Redirected Requests
;;; Number of different files requested

(defmethod print-report-item ((log new-log) (type (eql :number-of-different-files-requested)) stream &key
			      (start (get-log-start-time log))
			      (end (get-log-end-time log)))
  (format stream "~%Number of different URLs requested: ~a"
	  (number-of-different-files-requested log :start start :end end)))


;;; Number of different hosts

(defmethod print-report-item ((log new-log) (type (eql :number-of-different-hosts)) stream &key
			      (start (get-log-start-time log))
			      (end (get-log-end-time log)))
  (format stream "~%Number of different hosts: ~a"
	  (number-of-different-requesting-hosts log :start start :end end)))

;;; Unreadable lines in log file
;;; Unused log file entries
;;; Data size of answers

(defmethod print-report-item ((log new-log) (type (eql :number-of-bytes-served)) stream &key
			      (start (get-log-start-time log))
			      (end (get-log-end-time log)))
  (format stream "~%Number Bytes Served: ~a"
	  (bytes-served log :start start :end end)))

;;; Average size of answers per day

;;; Hourly Request Overview
;;; H, #Req, %Req, Pag, %Pag, kbytes, %kbytes

(defmethod print-report-item ((log new-log) (type (eql :hourly-request-overview)) stream &key
			      (start (get-log-start-time log))
			      (end (get-log-end-time log)))
  (format stream "~%Hourly report from ")
  (print-gmt-time stream start)
  (princ " to " stream)
  (print-gmt-time stream end)
  (loop for time from start upto end by (* 60 60)
	do (let ((requests 0) (bytes 0))
	     (range-map (new-log-transactions log)
                        (make-transaction-area :start-date time :end-date (+ time (* 60 60) -1))
                        #'(lambda (transaction)
                            (incf requests)
                            (incf bytes (transaction-bytes transaction))))
	     (format stream "~%  ")
	     (print-gmt-time stream time)
	     (format stream " ~a ~a" requests bytes))))


;;; Visit Overview, by default hourly
;;; H, #Visit, kybtes, #Requests

(defmethod print-report-item ((log new-log) (type (eql :visit-overview)) stream &key
			      (start (get-log-start-time log))
			      (end (get-log-end-time log))
			      (interval (* 60 60)))
  (format stream "~%~%Visit report from ")
  (print-gmt-time stream start)
  (princ " to " stream)
  (print-gmt-time stream end)
  (terpri stream)
  (print-list-as-table
   (list "Date" "Visits" "Bytes" "Requests" "Client Errors" "Server Errors")
   (loop for time from start upto end by interval
	 collect (list (with-output-to-string (stream) (print-gmt-time stream time))
                       (princ-to-string (range-count (new-log-visits log)
			                             (make-visit-area :start-date time :end-date (+ time interval -1))))
                       (princ-to-string (range-sum (new-log-visits log)
                                                   (make-visit-area :start-date time :end-date (+ time interval -1))
                                                   #'visit-bytes))
                       (princ-to-string (range-count (new-log-transactions log)
			                             (make-transaction-area :start-date time :end-date (+ time interval -1))))
                       (princ-to-string (range-count (new-log-transactions log)
			                             (make-transaction-area :start-date time
                                                                            :end-date (+ time interval -1)
                                                                            :status-start 400
                                                                            :status-end 499)))
                       (princ-to-string (range-count (new-log-transactions log)
			                             (make-transaction-area :start-date time
                                                                            :end-date (+ time interval -1)
                                                                            :status-start 500
                                                                            :status-end 599)))))
   stream))

(defmethod print-report-item ((log new-log) (type (eql :daily-visit-overview)) stream &key
                              (start (get-log-start-time log))
                              (end (get-log-end-time log))
                              (interval (* 24 60 60)))
  (print-report-item log :visit-overview stream :start (www-utils::day-start start) :end end :interval interval))


;;; ================================================================
;;; Visit table

(defmethod print-report-item ((log new-log) (type (eql :visit-table)) stream &key
			      (start (get-log-start-time log))
			      (end (get-log-end-time log))
			      (interval (* 60 60)))
  (format stream "~%~%Visit table report from ")
  (print-gmt-time stream start)
  (princ " to " stream)
  (print-gmt-time stream end)
  (terpri stream)
  (print-list-as-table
   (list "Start" "Duration" "Transactions" "Visitor" "Referrer")
   (loop for visit in (range-list (new-log-visits log)
                                  (make-visit-area :start-date start :end-date end))
         when (> (range-count (visit-transactions visit) (make-transaction-area)) 15)
         collect (list (with-output-to-string (stream) (print-gmt-time stream (visit-start visit)))
                       (princ-to-string (visit-duration visit))
                       (princ-to-string (range-count (visit-transactions visit) (make-transaction-area)))
                       (princ-to-string (visit-host visit))
                       (if (visit-referrer visit)
                           (with-output-to-string (stream)
                             (html4.0:note-anchor (visit-referrer visit)
                                                  :reference (visit-referrer visit)
                                                  :stream stream))
                         "")))
   stream))

;;; ================================================================
;;; Visit table: 10 visits with the most bytes transferred

(defmethod print-report-item ((log new-log) (type (eql :visit-table-10-most-bytes)) stream &key
			      (start (get-log-start-time log))
			      (end (get-log-end-time log))
			      (interval (* 60 60)))
  (format stream "~%~%Visit table (10 visits with most bytes) report from ")
  (print-gmt-time stream start)
  (princ " to " stream)
  (print-gmt-time stream end)
  (terpri stream)
  (print-list-as-table
   (list "Start" "Bytes" "Duration" "Transactions" "Visitor" "Referrer")
   (loop for visit in (let ((list (sort (range-list (new-log-visits log)
                                                    (make-visit-area :start-date start :end-date end))
                                        #'>
                                        :key #'visit-bytes)))
                        (subseq list 0 (min (length list) 10)))
         collect (list (with-output-to-string (stream) (print-gmt-time stream (visit-start visit)))
                       (princ-to-string (visit-bytes visit))
                       (princ-to-string (visit-duration visit))
                       (princ-to-string (range-count (visit-transactions visit) (make-transaction-area)))
                       (princ-to-string (visit-host visit))
                       (if (visit-referrer visit)
                           (with-output-to-string (stream)
                             (let ((url:*escape-search-urls* nil))
                               (html4.0:note-anchor (visit-referrer visit)
                                                    :reference (visit-referrer visit)
                                                    :stream stream)))
                         "")))
   stream))

;;; ================================================================
;;; Transaction table: 100 transactions with the most bytes transferred

(defmethod print-report-item ((log new-log) (type (eql :transactions-table-100-most-bytes)) stream &key
			      (start (get-log-start-time log))
			      (end (get-log-end-time log))
			      (interval (* 60 60)))
  (format stream "~%~%Transaction table (100 transactions with most bytes) report from ")
  (print-gmt-time stream start)
  (princ " to " stream)
  (print-gmt-time stream end)
  (terpri stream)
  (print-list-as-table
   (list "Date" "Bytes" "Host" "Request" "Referrer")
   (loop for transaction in (let ((list (sort (range-list (new-log-transactions log)
                                                          (make-transaction-area :start-date start :end-date end))
                                              #'>
                                              :key #'transaction-bytes)))
                              (subseq list 0 (min (length list) 500)))
         collect (list (with-output-to-string (stream) (print-gmt-time stream (transaction-date transaction)))
                       (princ-to-string (transaction-bytes transaction))
                       (princ-to-string (transaction-host transaction))
                       (princ-to-string (transaction-url transaction))
                       (if (transaction-referrer transaction)
                           (with-output-to-string (stream)
                             (let ((url:*escape-search-urls* nil))
                               (html4.0:note-anchor (transaction-referrer transaction)
                                                    :reference (transaction-referrer transaction)
                                                    :stream stream)))
                         "")))
   stream))

;;; ================================================================
;;; Robot usage table

(defmethod print-report-item ((log new-log) (type (eql :robot-usage-table)) stream &key
			      (start (get-log-start-time log))
			      (end (get-log-end-time log))
			      (interval (* 60 60)))
  (format stream "~%~%Robot access table report from ")
  (print-gmt-time stream start)
  (princ " to " stream)
  (print-gmt-time stream end)
  (terpri stream)
  (let ((distribution (distribution (range-list (new-log-transactions log)
                                                (make-transaction-area :url "/robots.txt"))
                                    #'transaction-host
                                    #'equal)))
    (print-list-as-table
     (list "Number" "Robot Host")
     (loop for (host . number) in distribution
           collect (list (princ-to-string number)
                         (princ-to-string host)))
     stream)))

;;; ================================================================
;;; HTML entry point table

(defmethod print-report-item ((log new-log) (type (eql :html-entry-point-table)) stream &key
			      (start (get-log-start-time log))
			      (end (get-log-end-time log))
			      (interval (* 60 60)))
  (format stream "~%~%HTML entry point table ")
  (print-gmt-time stream start)
  (princ " to " stream)
  (print-gmt-time stream end)
  (terpri stream)
  (let ((distribution (distribution (range-list (new-log-visits log)
                                                (make-visit-area))
                                    #'visit-start-html-url
                                    #'equalp)))
    (print-list-as-table
     (list "Number" "Entry Point")
     (loop for (entry-point . number) in distribution
           collect (list (princ-to-string number)
                         (princ-to-string entry-point)))
     stream)))


;;; ================================================================
;;; Referrer distribution table

(defmethod print-report-item ((log new-log) (type (eql :referrer-distribution-table)) stream &key
			      (start (get-log-start-time log))
			      (end (get-log-end-time log))
			      (interval (* 60 60)))
  (format stream "~%~%Referrer distribution table report from ")
  (print-gmt-time stream start)
  (princ " to " stream)
  (print-gmt-time stream end)
  (terpri stream)
  (let ((distribution (distribution (range-list (new-log-transactions log)
                                                (make-transaction-area :start-date start :end-date end))
                                    #'transaction-referrer
                                    #'equalp))
        (site-url-string (new-log-site-url-string log)))
    (flet ((site-p (site referrer)
             (and (> (length referrer) (length site))
                  (string-equal referrer site
                                :start1 0
                                :start2 0
                                :end1 (length site)
                                :end2 (length site)))))
      (setf distribution
            (loop for (referrer . number) in distribution
                  unless (or (site-p "http://www.bloglines.com" referrer)
                             (site-p "http://bloglines.com" referrer)
                             (and site-url-string
                                  (site-p site-url-string referrer))
                             (site-p "http://mail.google." referrer)
                             (site-p "http://www.google." referrer))
                  collect (cons referrer number)))
      (print-list-as-table
       (list "Number" "Referrer")
       (loop for (referrer . number) in distribution
             collect (list (princ-to-string number)
                           (if referrer
                               (or (handler-case (with-output-to-string (stream)
                                                    (let ((url:*escape-search-urls* nil))
                                                      (html4.0:note-anchor referrer
                                                                           :reference referrer
                                                                           :stream stream)))
                                     (t nil))
                                   "error")
                             "none")))
       stream))))

;;; ================================================================
;;; Search distribution table

(defmethod print-report-item ((log new-log) (type (eql :search-distribution-table)) stream &key
			      (start (get-log-start-time log))
			      (end (get-log-end-time log))
			      (interval (* 60 60)))
  (format stream "~%~%Search distribution table report from ")
  (print-gmt-time stream start)
  (princ " to " stream)
  (print-gmt-time stream end)
  (terpri stream)
  (let ((distribution (referrer-search-key-string-distribution log nil start end)))
    (print-list-as-table
     (list "Number" "Search")
     (loop for (search . number) in distribution
           collect (list (princ-to-string number)
                         (princ-to-string search)))
     stream)))

;;; ================================================================
;;; Error distribution table

(defmethod print-report-item ((log new-log) (type (eql :error-distribution-table)) stream &key
			      (start (get-log-start-time log))
			      (end (get-log-end-time log))
			      (interval (* 60 60)))
  (format stream "~%~%Error distribution table report from ")
  (print-gmt-time stream start)
  (princ " to " stream)
  (print-gmt-time stream end)
  (terpri stream)
  (let ((distribution (distribution (range-list (new-log-transactions log)
                                                (make-transaction-area :status-start 400 :status-end 499
                                                                       :start-date start
                                                                       :end-date end))
                                    #'transaction-url
                                    #'equalp)))
    (print-list-as-table
     (list "Number" "Error")
     (loop for (error . number) in distribution
           collect (list (princ-to-string number)
                         (princ-to-string Error)))
     stream)))


;;; Toplevel Domains
;;; #Req, %kbytes, toplevel-domain

;;; Hosts
;;; #Req, %Req, Pag, %Pag, kbytes, %kbytes, Host

;;; Directories
;;; #Req, %Req, Pag, %Pag, kbytes, %kbytes, Directory

;;; Requests
;;; #Req, %Req, Pag, %Pag, kbytes, %kbytes, File


;;; ================================================================
;;; Request distribution table

(defmethod print-report-item ((log new-log) (type (eql :request-distribution-table)) stream &key
			      (start (get-log-start-time log))
			      (end (get-log-end-time log))
			      (interval (* 60 60)))
  (format stream "~%~%Request distribution table report from ")
  (print-gmt-time stream start)
  (princ " to " stream)
  (print-gmt-time stream end)
  (terpri stream)
  (let ((distribution (distribution (range-list (new-log-transactions log)
                                                (make-transaction-area :status-start 200 :status-end 399
                                                                       :start-date start
                                                                       :end-date end))
                                    #'transaction-url
                                    #'equalp)))
    (print-list-as-table
     (list "Number" "Request")
     (loop for (error . number) in distribution
           collect (list (princ-to-string number)
                         (princ-to-string Error)))
     stream)))


;;; Status
;;; #N, Code, Description

;;; 

;;; ================================================================
;;; Reports

(defparameter *report-item-types*
  '(:report-interval
    :number-of-successful-requests
    :number-of-request-errors
    :number-of-different-files-requested
    :number-of-different-hosts
    :number-of-bytes-served
    :daily-visit-overview
;                 :visit-overview
    :visit-table
    :referrer-distribution-table
    :search-distribution-table
    :error-distribution-table
    :visit-table-10-most-bytes
    :robot-usage-table
    :transactions-table-100-most-bytes
    :request-distribution-table
    :html-entry-point-table
              ;   :hourly-request-overview
    ))

(defmethod print-report ((log new-log) (type (eql :basic)) stream &key
			 (start (get-log-start-time log))
			 (end (get-log-end-time log)))
  (loop for item-type in *report-item-types*
	do (print-report-item log item-type stream :start start :end end)))

(defmethod print-report ((log new-log) (type (eql :html-basic)) stream &key
			 (start (get-log-start-time log))
			 (end (get-log-end-time log)))
  (let ((*report-format* :html))
    (loop for item-type in *report-item-types*
          do (html4.0:with-paragraph (:stream stream)
               (print-report-item log item-type stream :start start :end end)))))

;;; ================================================================
;;; Examples

; (print-report *test-log-instance* :basic *standard-output*)
; (print-report-item *test-log-instance* :hourly-request-overview *standard-output*)
; (print-report-item *test-log-instance* :visit-overview *standard-output*)

#|
(let ((stream (ed)))
  (print-report *test-log-instance* :basic stream)
  (force-output stream))
|#

;;; ================================================================
;;; End of File
