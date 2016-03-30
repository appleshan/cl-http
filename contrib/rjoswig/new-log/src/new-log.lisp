;;; -*- Package: NEW-LOG; Syntax: ANSI-Common-Lisp -*-

;;; Copyright Rainer Joswig, joswig@lisp.de, 2006

(in-package "NEW-LOG")

;;; ================================================================
;;; parsing logs:

#+ignore
(defun parse-time (time-string)
  (or (ignore-errors (www-utils::parse-cern-log-time time-string))
      (ignore-errors (www-utils::parse-iso-time time-string))))

(defun parse-time (time-string)
  (or (www-utils::parse-cern-log-time time-string)
      (www-utils::parse-iso-time time-string)))

; See also
; http::%parse-extended-common-log-format-log-entry  in http:server;log.lisp


(defun %parse-common-log-format-log-entry (line &optional (time-parser #'parse-time)
                                                &aux (l (length line)))
  (declare (values url host method date server-version status bytes user-name)
           (fixnum l))
  (labels ((white-space-p (x)
             (char= x #\space))
           (parse-exact-request (exact-request &aux (l (length exact-request)))
             (declare (fixnum l))
             (let* ((r1 (position-if #'white-space-p exact-request :start 0 :end l))
                    (r2 (position-if #'white-space-p exact-request :start (the fixnum (1+ r1)) :end l))
                    (method (subseq exact-request 0 r1))
                    (url (subseq exact-request (the fixnum (1+ r1)) r2))
                    (server-version (if r2
                                      (subseq exact-request (the fixnum (1+ r2)) l)
                                      "HTTP/0.9")))		; http 0.9 or telnet hackers don't send version
               (declare (fixnum r1 r2))
               (values method url server-version)))
           (parse-user (line start end)
             (declare (fixnum start))
             (if (and (char= #\- (aref line start)) (char= #\space (aref line (1+ start))))
               nil
               (subseq line start (1+ (the fixnum (position-if-not #'white-space-p line :start start :end end :from-end t)))))))
    (declare (inline white-space-p parse-exact-request parse-user))
    (let (p1 p2 p3 p4 p5 p6 p7 p7a p8 p9 p10)
      p3
      (cond
       ((and (setq p1 (position-if #'white-space-p line :start 0 :end l)) ; host end
             (setq p2 (position-if #'white-space-p line :start (the fixnum (1+ p1)) :end l)) ; user start
             (setq p3 (position-if #'white-space-p line :start (the fixnum (1+ p2)) :end l))
             (setq p4 (position #\[ line :start p1 :end l)) ; user end , date start
             (setq p5 (position #\] line :start p2 :end l)) ; date end
             (setq p6 (position #\" line :start 0 :end l))  ; request start
             (setq p7a (search " HTTP" line :start2 p6 :end2 l))
             (setq p7 (position #\" line :start (the fixnum (1+ p7a)) :end l)) ; request end
             (< (1+ (the fixnum p6)) p7)			;empty request string is a bad entry  7/20/95 -- JCMa.
             (setq p8 (position-if #'white-space-p line
                                   :start (the fixnum (+ 2 p7))
                                   :end l)))
        (setq p9 (position #\" line
                              :start (the fixnum (+ 1 p8))
                              :end l)) ; referrer start
        (when p9
          (setq p10 (position #\" line :start (the fixnum (+ 1 p9)) :end l))) ; referrer end, user agent start
        (locally
          (declare (fixnum p1 p2 p3 p4 p5 p6 p7 p8 p9 p10))
          ;          (when p10 (incf p10))
          (multiple-value-bind (method url server-version)
                               (parse-exact-request (subseq line (the fixnum (1+ p6)) p7))
            (let ((url url)
                  (host (subseq line 0 p1))
                  (method (intern (nstring-upcase method) http:*keyword-package*))
                  (date (funcall time-parser (subseq line (the fixnum (1+ p4)) p5)))
                  (server-version server-version)
                  (status (parse-integer (subseq line (the fixnum (+ 2 p7)) p8)))
                  (bytes (ignore-errors
                          (parse-integer (subseq line (the fixnum (1+ p8)) p9))))
                  (user (parse-user line (1+ (the fixnum p2)) p4))
                  (referrer (and p9 p10 (subseq line (the fixnum (1+ p9)) p10)))
                  (user-agent (and p9 p10 (subseq line (the fixnum (+ p10 3)) (1- l)))))
              (values url host method date server-version status bytes user
                      referrer user-agent)))))
       (t (values nil nil :bad-record))))))



; Latest Proxy Log Format:
; <proxy-extended-common-log-format>::
;
; <common log format> <server-bytes-received> <server-requests-this-connection> <server-cpu-time>
; <server-elapsed-time> <cache-hit-p> [<client-http-version> <client-status-code> <client-requests-this-connection> <client-cpu-time> <client-elapsed-time>]
;
; <cache-hit-p>:: "-" | "+"


;;; ================================================================
;;; Transaction

(defclass transaction ()
  ((url :accessor transaction-url :initarg :url)
   (host :accessor transaction-host :initarg :host)
   (method :accessor transaction-method :initarg :method)
   (date :accessor transaction-date :initarg :date)
   (server-version :accessor transaction-server-version :initarg :server-version)
   (status :accessor transaction-status :initarg :status)
   (bytes :accessor transaction-bytes :initarg :bytes)
   (user :accessor transaction-user :initarg :user)
   (referrer :accessor transaction-referrer :initarg :referrer)
   (user-agent :accessor transaction-user-agent :initarg :user-agent)
   (visit :accessor transaction-visit)))

(defmethod print-object ((transaction transaction) stream)
  (print-unreadable-object (transaction stream :type t)
    (www-utils:print-gmt-time stream (transaction-date transaction))
    (format stream ", host ~a with user agent ~a for ~a"
            (transaction-host transaction)
            (transaction-user-agent transaction)
	    (transaction-url transaction))))

(defun make-transaction-kdtree ()
  (make-kdtree (vector #'transaction-url
		       #'transaction-host
		       #'transaction-date
		       #'transaction-status
		       #'transaction-bytes
		       #'transaction-referrer
		       #'transaction-user-agent)
	       (vector #'string-not-greaterp
		       #'string-not-greaterp
		       #'<=
		       #'<=
		       #'<=
		       #'string-not-greaterp
		       #'string-not-greaterp)
	       #'eq))

(defparameter +transaction-url-dimension+ 0)
(defparameter +transaction-host-dimension+ 1)
(defparameter +transaction-date-dimension+ 2)
(defparameter +transaction-status-dimension+ 3)
(defparameter +transaction-bytes-dimension+ 4)
(defparameter +transaction-referrer-dimension+ 5)
(defparameter +transaction-user-agent-dimension+ 6)

(defun make-transaction-area (&key url start-url end-url host start-date end-date status-start status-end start-bytes end-bytes referrer user-agent)
  (list (cons (or start-url url) (or end-url url))
	(cons host host)
	(cons start-date end-date)
	(cons status-start status-end)
	(cons start-bytes end-bytes)
	(cons referrer referrer)
	(cons user-agent user-agent)))

;;; ================================================================
;;; Visit

(defparameter *visit-timeout* (* 3 60)
  "Number of seconds when a visit times out")

(defclass visit ()
  ((start :accessor visit-start :initarg :start)
   (host :accessor visit-host :initarg :host)
   (user :accessor visit-user :initarg :user)
   (user-agent :accessor visit-user-agent :initarg :user-agent)
   (start-url :accessor visit-start-url :initarg :start-url)
   (start-html-url :accessor visit-start-html-url :initarg :start-html-url :initform "")
   (referrer :accessor visit-referrer :initarg :referrer)
   (transactions :accessor visit-transactions
                 :initform (make-transaction-kdtree))))

(defun make-visit-kdtree ()
  (make-kdtree (vector #'visit-start
		       #'visit-host
		       #'visit-user-agent
		       #'visit-start-url
		       #'visit-referrer
		       #'visit-start-html-url)
	       (vector #'<=
		       #'string-not-greaterp
		       #'string-not-greaterp
		       #'string-not-greaterp
		       #'string-not-greaterp
		       #'string-not-greaterp)
	       #'eq))

(defun make-visit-area (&key start-date end-date host user-agent start-url referrer start-html-url)
  (list (cons start-date end-date)
	(cons host host)
	(cons user-agent user-agent)
	(cons start-url start-url)
	(cons referrer referrer)
	(cons start-html-url start-html-url)))

(defparameter +visit-start-dimension+ 0)
(defparameter +visit-host-dimension+ 1)
(defparameter +visit-user-agent-dimension+ 2)
(defparameter +visit-start-url-dimension+ 3)
(defparameter +visit-referrer-dimension+ 4)
(defparameter +visit-start-html-url-dimension+ 5)

(defmethod print-object ((visit visit) stream)
  (print-unreadable-object (visit stream :type t)
    (www-utils:print-gmt-time stream (visit-start visit))
    (format stream " actions: ~a, from host ~a with user agent ~a"
            (range-count (visit-transactions visit) '(nil nil nil nil nil nil nil))
            (visit-host visit)
            (visit-user-agent visit))))

(defmethod visit-end ((visit visit))
  (transaction-date (first (dimension-extrema (visit-transactions visit)
                                              +transaction-date-dimension+
                                              :right))))

(defmethod visit-duration ((visit visit))
  (- (visit-end visit)
     (visit-start visit)))

(defmethod visit-bytes ((visit visit))
  (let ((sum 0))
    (range-map (visit-transactions visit)
               (make-transaction-area)
               #'(lambda (transaction)
                   (incf sum (transaction-bytes transaction))))
    sum))

(defmethod visit-number-of-transactions ((visit visit))
  (range-count (visit-transactions visit) (make-transaction-area)))

(defmethod visit-touching-url-p ((visit visit) url)
  (range-list (visit-transactions visit)
	      (make-transaction-area :url url)))


(defmethod list-visit-transactions ((visit visit) &key
				    start
				    end)
  (sort (range-list (visit-transactions visit)
		    (make-transaction-area :start-date start :end-date end))
	#'<
	:key #'transaction-date))

;;; ================================================================
;;; New Log

(defclass new-log ()
  ((site-name :accessor new-log-site-name :type string :initarg :site-name)
   (site-url-string :accessor new-log-site-url-string :type string :initarg :site-url-string)
   (timezone)
   (urls :accessor new-log-urls :initform (make-hash-table :test #'equalp))
   (hosts :accessor new-log-hosts :initform (make-hash-table :test #'equalp))
   (protocols :accessor new-log-protocols :initform (make-hash-table :test #'equalp))
   (user-agents :accessor new-log-user-agents :initform (make-hash-table :test #'equalp))
   (referrers :accessor new-log-referrers :initform (make-hash-table :test #'equalp))
   (transactions :accessor new-log-transactions
                 :initform (make-transaction-kdtree))
   (visits :accessor new-log-visits :initform (make-visit-kdtree))
   (current-visits :accessor new-log-current-visits :initform nil)))


(defmethod get-log-start-time ((log new-log))
  (let ((start (first (dimension-extrema (new-log-transactions log) +transaction-date-dimension+ :left))))
    (when start
      (transaction-date start))))

(defmethod get-log-end-time ((log new-log))
  (let ((end (first (dimension-extrema (new-log-transactions log) +transaction-date-dimension+ :right))))
    (when end
      (transaction-date end))))

(defun new-log-transactions-list (log  &key url host start-date end-date status-start status-end start-bytes end-bytes referrer user-agent)
  (range-list (new-log-transactions log)
	      (make-transaction-area :url url :host host :start-date start-date :end-date end-date
				     :status-start status-start :status-end status-end
				     :start-bytes start-bytes :end-bytes end-bytes :referrer referrer :user-agent user-agent)))


(defun new-log-visits-list (log &key start-date end-date host user-agent)
  (range-list (new-log-visits log)
              (make-visit-area :start-date start-date
                               :end-date end-date
                               :host host
                               :user-agent user-agent)))

(defmethod list-touching-visits ((log new-log) url &key
				 (start (get-log-start-time log))
				 (end (get-log-end-time log)))
  (let ((visits nil))
    (range-map (new-log-visits log)
               (make-visit-area :start-date start :end-date end)
               #'(lambda (visit)
                   (when (visit-touching-url-p visit url)
                     (push visit visits))))
    visits))

(defmethod host-visits ((log new-log) host &key
                        (start (get-log-start-time log))
                        (end (get-log-end-time log)))
  (new-log-visits-list log
                       :host host
                       :start-date start
                       :end-date end))

(defmethod host-visit-count ((log new-log) host &key
                             (start (get-log-start-time log))
                             (end (get-log-end-time log)))
  (range-count (new-log-visits log)
               (make-visit-area :start-date start
                                :end-date end
                                :host host)))

(defmethod host-transactions ((log new-log) host &key
                              (start (get-log-start-time log))
                              (end (get-log-end-time log)))
  (new-log-transactions-list log
                             :host host
                             :start-date start
                             :end-date end))

(defmethod host-transaction-count ((log new-log) host &key
                                   (start (get-log-start-time log))
                                   (end (get-log-end-time log)))
  (range-count (new-log-transactions log)
	       (make-transaction-area :host host :start-date start :end-date end)))


;;; ================================================================
;;; Log File Parsing

(defmacro make-unique (table item)
  `(let ((item1 (gethash ,item ,table)))
     (if item1
       (setf ,item item1)
       (setf (gethash ,item ,table) ,item))))

; (setf *noisy* nil)

(defun parse-new-log-file (log file &optional continuously)
  (with-open-file (stream file)
    (parse-log-stream log stream continuously)))

(defun parse-log-line (log line urls hosts protocols user-agents referrers &key resolve-host-names host-name-table)
  (when line
    (multiple-value-bind (url host method date server-version status bytes user
			      user-agent referrer)
	                 (http::%parse-extended-common-log-format-log-entry line)
      (when (and resolve-host-names host-name-table)
        (setf host (resolve-host-name host host-name-table)))
      (make-unique urls url)
      (make-unique hosts host)
      (make-unique protocols server-version)
      (make-unique user-agents user-agent)
      (make-unique referrers referrer)
      (let ((transaction (make-instance 'transaction :url url :host host :method method
					:date date :server-version server-version
					:status status :bytes (or bytes 0) :user (or user "")
					:referrer referrer :user-agent user-agent)))
	(unless (eq method :bad-record)
	  (tree-insert (new-log-transactions log) transaction))
	transaction))))

(defun parse-log-stream (log stream &key continuously resolve-host-names host-name-table (parser #'parse-log-line))
  (unless host-name-table
    (setf host-name-table (make-hash-table :test #'equalp)))
  (let  ((urls (new-log-urls log))
         (hosts (new-log-hosts log))
         (protocols (new-log-protocols log))
         (user-agents (new-log-user-agents log))
         (referrers (new-log-referrers log)))
    (read-delimited-line stream '(#\linefeed #\newline) nil)
    (loop for line = (read-delimited-line stream '(#\linefeed #\newline) nil)
          do (cond ((stringp line)
                    (let ((transaction (funcall parser log line urls hosts protocols user-agents referrers
                                                :resolve-host-names resolve-host-names
                                                :host-name-table host-name-table)))
                      ;(when (or (not transaction) (eq (transaction-method transaction) :bad-record))
                      ;  (warn "Bad Record (~a) for line |~a|" stream line))
                      (unless (eq (transaction-method transaction) :bad-record)
                        (compute-visit-for-transaction log transaction))))
                   ((null line)
                    (if continuously
                        (sleep 2)
                      (return)))))
    log))

(defun resolve-host-name (name host-name-table)
  (let ((host (gethash name host-name-table)))
    (if host
      host
      (setf (gethash name host-name-table) (www-utils:host-http-name name)))))


;;; ================================================================
;;; User Agents


(defparameter *robot-user-agents*
  '("KIT-Fireball/2.0" "ArchitextSpider"  "Slurp/2.0 (slurp@inktomi.com; http://www.inktomi.com/slurp.html)"
    "WiseWire-Spider2" "Scooter/1.0 scooter@pa.dec.com" "Mozilla/4.0 (compatible; MSIE 4.0; Windows NT; Site Server 3.0 Robot) Encyclopedia Britannica "
    "Harvest/1.5.17" "WebNerd/0.1" "ia_archiver" "Lycos_Spider_(T-Rex)" "Mozilla/4.0 (compatible; MSIE 4.01; MSIECrawler; Windows NT)"
    "Scooter/1.1 (custom)" "Scooter/2.0 G.R.A.B. X2.0" "WiseWire-Spider1" "WiseWire-Spider" "NetMechanic" "WiseWire-DeadLinkDetector"))


;;; ================================================================
;;; Compute Visits

(defparameter *noisy* nil)

(defun remove-timed-out-visits (visits date max-timeout)
  (flet ((visit-in-time-range-p (visit)
	   (range-map (visit-transactions visit)
                      (list nil nil (cons (- date max-timeout) nil) nil nil nil nil)
                      #'(lambda (transaction)
                          (declare (ignore transaction))
                          (return-from visit-in-time-range-p t)))
	   nil))
    (loop for visit in visits
	  unless (visit-in-time-range-p visit)
          do (when *noisy* (format t "~%End of Visit: ~a" visit))
	  else collect visit)))

(defun visit-match (host user user-agent visit)
  (and (equal host (visit-host visit))
       (equal user (visit-user visit))
       (equal user-agent (visit-user-agent visit))))

(defun find-visit (host user user-agent current-visits)
  (loop for visit in current-visits
        when (visit-match host user user-agent visit)
        do (return visit)))

(defun html-url-string-p (url-string)
  (let ((pos (search ".html" url-string :from-end t)))
    (and pos (= pos (- (length url-string) 5)))))

(defun compute-visit-for-transaction (log transaction &optional (max-timeout *visit-timeout*))
  (when transaction
    (unless (member (transaction-user-agent transaction) *robot-user-agents* :test #'equalp)
      (when (transaction-date transaction)
	(setf (new-log-current-visits log)
	      (remove-timed-out-visits (new-log-current-visits log)
				       (transaction-date transaction)
				       max-timeout)))
      (let ((visit (find-visit (transaction-host transaction)
			       (transaction-user transaction)
			       (transaction-user-agent transaction)
			       (new-log-current-visits log))))
	(if visit
          (progn
            (setf (transaction-visit transaction) visit)
            (when (and (equal (visit-start-html-url visit) "")
                       (html-url-string-p (transaction-url transaction)))
              (setf (visit-start-html-url visit) (transaction-url transaction)))
            (tree-insert (visit-transactions visit) transaction))
          (let ((visit (make-instance 'visit
                         :start-url (transaction-url transaction)
                         :referrer (transaction-referrer transaction)
                         :host (transaction-host transaction)
                         :user (transaction-user transaction)
                         :user-agent (transaction-user-agent transaction)
                         :start (transaction-date transaction))))
            (when (html-url-string-p (transaction-url transaction))
              (setf (visit-start-html-url visit) (transaction-url transaction)))
            (tree-insert (visit-transactions visit) transaction)
            (setf (transaction-visit transaction) visit)
            (when *noisy* (format t "~%New Visit: ~a" visit))
            (tree-insert (new-log-visits log) visit)
            (push visit (new-log-current-visits log))))))))


;; I need to apply the function in date sorted order. Works??
(defun compute-visits (log &optional (max-timeout *visit-timeout*))
  (setf (new-log-visits log) (make-visit-kdtree))
  (range-map (new-log-transactions log) (make-transaction-area)
             #'(lambda (transaction)
                 (compute-visit-for-transaction log transaction max-timeout))))


(defmethod show-visits-and-transactions ((log new-log) (url string) start end &optional (stream *standard-output*))
  (let ((visits nil))
    (range-map (new-log-transactions log)
               (make-transaction-area :start-url url
                                      :end-url (concatenate 'string url (string (code-char 255)))
                                      :start-date start
                                      :end-date end)
               #'(lambda (transaction)
                   (when (slot-boundp transaction 'visit)
                     (pushnew (transaction-visit transaction) visits))))
    (loop for visit in (sort visits #'< :key #'visit-start)
	  do (format stream "~%~a" visit)
	  do (loop for transaction in (sort (list-visit-transactions visit) #'< :key #'transaction-date)
		   do (format stream "~%  ~a  from  ~a "
			      (transaction-url transaction)
			      (transaction-referrer transaction))))))

;;; ================================================================
;;; Statistics

#||
(defun distribution (items key test)
  (let ((items-list nil))
    (loop for item in items
          do (let* ((length (funcall key item))
                    (found (assoc length items-list :test test)))
               (if found
                 (incf (cdr found))
                 (progn
                   (push (cons length 1) items-list)
                   (setf items-list (sort items-list #'> :key #'cdr))))))
    (sort items-list #'> :key #'cdr)))
||#

(defun distribution (items key test)
  (let ((table (make-hash-table :test test)))
    (loop for item in items
          do (incf (gethash (funcall key item) table 0)))
    (let ((items-list nil))
      (maphash (lambda (value count)
                 (push (cons value count) items-list))
               table)
      (sort items-list #'> :key #'cdr))))

(defun find-referrers-for-url (log url &key (unique t)
				   (start (get-log-start-time log))
				   (end (get-log-end-time log)))
  (let ((referrers nil))
    (range-map (new-log-transactions log)
               (make-transaction-area :url url :start-date start :end-date end)
               #'(lambda (transaction)
                   (if unique
                       (pushnew (transaction-referrer transaction) referrers :test #'equalp)
                     (push (transaction-referrer transaction) referrers))))
    (sort referrers #'string-greaterp)))


;;; ================================================================
;;; Searches


(defmethod extract-search-words ((string string))
  (let* ((host (url::get-host-port-info string))
         (pos1 (position #\? string)))
    (when (and host pos1)
        (extract-search-words-1 string host pos1))))

(defun split-string (string item)
  #+mcl(ccl::split-string string :item item)
  #+lispworks(lispworks:split-sequence (list item) string))

(defmethod extract-search-words-1 ((string string) (host string) (pos1 number))
  (let ((search (and pos1 (mapcar (lambda (item)
                                    (split-string item #\=))
                                  (split-string (subseq string (1+ pos1))
                                                #\&)))))
    (when search
      (cond ((or (equalp host "www.altavista.com")
                 (equalp host "www.digital.altavista.com")
                 (equalp host "www.altavista.digital.com")
                 (equalp host "altavista.digital.com")
                 (equalp host "www.bing.com")
                 (equalp host "www.fireball.de")
                 (equalp host "www.google.com")
                 (equalp host "search.msn.de")
                 (equalp host "search.msn.com")
                 (equalp host "programming.reddit.com")
                 (equalp host "www.reddit.com")
                 (equalp host "reddit.com")
                 (and (> (length host) #.(length "www.google."))
                      (string-equal "www.google." host :end1 #.(length "www.google.") :end2 #.(length "www.google."))))
             (second (assoc "q" search :test #'equal)))
            ((or (equalp host "image.altavista.com"))
             (second (assoc "query" search :test #'equal)))
            ((or (equalp host "ink.yahoo.com")
                 (equalp host "de.search.yahoo.com")
                 (equalp host "search.yahoo.com"))
             (second (assoc "p" search :test #'equal)))
            ((or (equalp host "www.lycos.de")
                 (equalp host "www.lycos.com")
                 (equalp host "lycospro.lycos.com"))
             (second (assoc "query" search :test #'equal)))
            ((or (equalp host "guide-p.infoseek.com"))
             (second (assoc "oq" search :test #'equal)))
            ((or (equalp host "www.infoseek.com"))
             (second (assoc "qt" search :test #'equal)))
            ((or (equalp host "www.metacrawler.com"))
             (second (assoc "general" search :test #'equal)))
            ((or (equalp host "www.northernlight.com"))
             (second (assoc "qr" search :test #'equal)))
            ((or (equalp host "del.icio.us"))
             (second (assoc "all" search :test #'equal)))
            ((or (equalp host "www.hotbot.com")
                 (equalp host "www.goo.ne.jp"))
             (second (assoc "MT" search :test #'equal)))
            (t (list host search))))))

(defmethod extract-search-words ((url url:url))
  (let* ((host (url:host-string url))
         (string (url:name-string url))
         (pos1 (position #\? string)))
    (when host
      (setf host (string-trim '(#\") host)))
    (when string
      (setf string (string-trim '(#\") string)))
    (when pos1
      (extract-search-words-1 string host pos1))))

; (extract-search-words (url:url "http://www.google.com/search?q=using+lisp+on+mac&btnG=Google+Search&hl=en&lr=&ie=UTF-8&oe=UTF-8"))


;;; ================================================================
;;; Referrer

(defun referrer-search-key-string-distribution (log url start end)
  (distribution
   (mapcar
    #'http:string-unescape-special-chars
    (remove-if
     (lambda (item)
       (not (stringp item)))
     (mapcar #'(lambda (item) (handler-case (extract-search-words item)
                                (t nil)))
             (remove-if-not
              (lambda (url-string)
                (and (> (length url-string) 7)
                     (string-equal "http://" url-string :start1 0 :start2 0 :end1 7 :end2 7)
                     (position #\? url-string)))
              (mapcar #'transaction-referrer
                      (range-list (new-log-transactions log)
                                  (make-transaction-area :url url :start-date start :end-date end)))))))
   #'identity
   #'equalp))


;;; ================================================================
;;; End of File
