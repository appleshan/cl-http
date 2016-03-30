;;; -*- Package: WWW-UTILS; Syntax: ANSI-Common-Lisp -*-

;;; ================================================================

(in-package "WWW-UTILS")

(define-constant *month-alist*
  '((1 "Jan" "January")
    (2 "Feb" "February")
    (3 "Mar" "March")
    (4 "Apr" "April")
    (5 "May" "May")
    (6 "Jun" "June")
    (7 "Jul" "July")
    (8 "Aug" "August")
    (9 "Sep" "September")
    (10 "Oct" "October")
    (11 "Nov" "November")
    (12 "Dec" "December")))

(defun translated-month (string &optional (start 0) (end (length string)))
   "Returns a number from 1 to 12, corresponding to the month passed in."
   (flet ((some-digit-char-p (string start end)
               (with-fast-array-references ((string string string))
                   (loop for idx upfrom start below end
                            when (digit-char-p (aref string idx))
                            return t
                            else  return nil
                            finally (return t)))))
      (declare (inline some-digit-char-p))
      (cond ((some-digit-char-p string start end)
                 (let ((month-num (parse-integer string :start start :end end)))
                    (if (< 0 month-num 13)
                       month-num
                       (error 'HTTP::bad-syntax-provided :format-string "~A in ~S is a bad month." :format-args (list month-num (subseq string start end))))))
               (t (let ((month (subseq string start end)))
                     (cond ((gethash month *month-table*))
                              (t (error 'HTTP::bad-syntax-provided :format-string"~A is a bad month." :format-args(list (subseq string start end))))))))))

(define-constant *weekday-alist*
  '((0 "Mon" "Monday")
    (1 "Tue" "Tuesday")
    (2 "Wed" "Wednesday")
    (3 "Thu" "Thursday")
    (4 "Fri" "Friday")
    (5 "Sat" "Saturday")
    (6 "Sun" "Sunday")))

(defun translated-timezone (timezone)
  (when timezone   ;; null timezones are OK.
    (etypecase timezone 
      (integer
       (cond ((< -23 timezone 23) timezone)    ; between -24 and 24
             (t (error "~A is an invalid timezone" timezone))))
      (string 
       (cond ((and (= (length timezone) 5)
                   (or (char= #\+ (aref timezone 0))
                       (char= #\- (aref timezone 0))))
              (let* ((hours (parse-integer timezone :start 1 :end 3))
                     (minutes (parse-integer timezone :start 3 :end 5))
                     (timezone-value (+ hours (/ minutes 60))))
                (- (if (char= #\- (aref timezone 0))
                     (- timezone-value)
                     timezone-value))))
             ((string-equal "GMT" timezone) 0)
             (t (error  "~A is an invalid timezone" timezone)))))))

(defconstant *cern-time-separators* '(#\space #\: #\/))

(defun tokenize-cern-string (string &aux temp-string result)
  (flet ((index-temp-string ()
           (unless (null temp-string)
             (push (concatenate 'string temp-string) result)
             (setq temp-string nil))))
    (declare (inline index-temp-string))
    (loop for index downfrom (1- (the fixnum (length string))) to 0
          for char = (aref string index)
          when (member char *cern-time-separators* :test #'char=)
            do (index-temp-string)
          else do (push char temp-string)
          finally (index-temp-string) 
                  (return (values-list result)))))

; (tokenize-cern-string "24/Aug/1996:00:30:42 +0200")

(defun parsed-encode-universal-time (seconds minute hour day-of-month month year timezone)
  (encode-universal-time
    (parse-integer seconds)
    (parse-integer minute)
    (parse-integer hour)
    (parse-integer day-of-month)
    (translated-month month)
    (parse-integer year)
    (if timezone
	(translated-timezone timezone)
	0)))

(defun parse-cern-log-time-string (time-string)
  (multiple-value-bind (day-of-month month year hour minute seconds timezone)
                       (tokenize-cern-string time-string)
    (declare (ignore x-weekday))
    (parsed-encode-universal-time seconds minute hour day-of-month
                                             month year timezone)))

(define parse-cern-log-time (time-string &optional (start 0) end (futurep t)
                                          base-time must-have-time date-must-have-year
                                          time-must-have-second (day-must-be-valid t))
  (declare (ignore futurep base-time must-have-time date-must-have-year
                   time-must-have-second day-must-be-valid))
  (parse-cern-log-time-string (subseq time-string start end)))

; (parse-cern-log-time-string "24/Aug/1996:00:30:42 +0200")

(defun day-start (day)
  (multiple-value-bind (secs mins hours day month year)
      (decode-universal-time day)
    (declare (ignore secs mins hours))
    (encode-universal-time 0 0 0 day month year)))

;;; ================================================================
;;; End of File
