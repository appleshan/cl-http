;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: xml-parser; -*-

(in-package "XML-PARSER")

(with-xml-writer ((make-instance 'fred-window))
  (xml "users"
       (mapcar #'(lambda (user)
                   (encode-char #\return)
                   (destructuring-bind (id name rating) user
                     (xml "user_tuple"
                          (xml "userid" id)
                          (xml "name" name)
                          (xml "rating" rating))))
               '(("U01" "Tom Jones" "B")
                 ("U02" "Mary Doe" "A")
                 ("U03" "Dee Linquent" "D")
                 ("U04" "Roger Smith" "C")
                 ("U05" "Jack Sprat" "B")
                 ("U06" "Rip Van Winkle" "B")))))

(with-xml-writer ((make-instance 'fred-window))
  (xml "items"
       (mapcar #'(lambda (item)
                   (encode-char #\return)
                   (destructuring-bind (itemno desc offered start end reserve)
                                       item
                     (xml "item_tuple"
                          (xml "itemno" itemno)
                          (xml "description" desc)
                          (xml "offered_by" offered)
                          (xml "start_date" start)
                          (xml "end_date" end)
                          (xml "reserve_price" reserve))))
               '(("1001"  "Red Bicycle"  "U01"  "99-01-05"  "99-01-20"  "40")
                 ("1002"  "Motorcycle"  "U02"  "99-02-11"  "99-03-15"  "500")
                 ("1003"  "Old Bicycle"  "U02"  "99-01-10"  "99-02-20"  "25")
                 ("1004"  "Tricycle"  "U01"  "99-02-25"  "99-03-08"  "15")
                 ("1005"  "Tennis Racket"  "U03"  "99-03-19"  "99-04-30"  "20")
                 ("1006"  "Helicopter"  "U03"  "99-05-05"  "99-05-25"  "50000")
                 ("1007"  "Racing Bicycle"  "U04"  "99-01-20"  "99-02-20"  "200")
                 ("1008"  "Broken Bicycle"  "U01"  "99-02-05"  "99-03-06"  "25")))))


(with-xml-writer ((make-instance 'fred-window))
  (xml "bids"
       (mapcar #'(lambda (bid)
                   (encode-char #\return)
                   (destructuring-bind (id item bid date) bid
                     (xml "bid_tuple"
                          (xml "userid" id)
                          (xml "itemno" item)
                          (xml "bid" bid)
                          (xml "bid_date" date))))
               '(("U02"  "1001"  "35"  "99-01-07")
                 ("U04"  "1001"  "40"  "99-01-08")
                 ("U02"  "1001"  "45"  "99-01-11")
                 ("U04"  "1001"  "50"  "99-01-13")
                 ("U02"  "1001"  "55"  "99-01-15")
                 ("U01"  "1002"  "400"  "99-02-14")
                 ("U02"  "1002"  "600"  "99-02-16")
                 ("U03"  "1002"  "800"  "99-02-17")
                 ("U04"  "1002"  "1000"  "99-02-25")
                 ("U02"  "1002"  "1200"  "99-03-02")
                 ("U04"  "1003"  "15"  "99-01-22")
                 ("U05"  "1003"  "20"  "99-02-03")
                 ("U01"  "1004"  "40"  "99-03-05")
                 ("U03"  "1007"  "175"  "99-01-25")
                 ("U05"  "1007"  "200"  "99-02-08")
                 ("U04"  "1007"  "225"  "99-02-12")))))