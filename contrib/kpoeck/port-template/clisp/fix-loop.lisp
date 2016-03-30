(in-package :http)

(define string-unescape-special-chars (string &optional (start 0) (end (length string) end-supplied-p)
                                              (only-safe-characters-p nil) &aux new-string)
  "When any escaped characters are present, this returns a string with these characters unescaped.
A new string is consed only when escaped characters are present.
ONLY-SAFE-CHARACTERS-P set to non-nill skips reserved or unsafe URL characters."
  (declare (values unescaped-string chars-unescaped-p new-string-returned-p)
           (fixnum start end))
  (with-fast-array-references ((string string string)
                               ;; Can't declare unless an array is provided on LispM.
                               #-Genera (new-string new-string (or null string)))
    (loop with idx fixnum = start
        and last-idx fixnum = start
        and new-idx fixnum = start
        and new-char
        while (< idx end)
        when (escape-character-p (aref string idx))
        do 
          (setf new-char (unescape-character string (1+ idx) (+ idx 3)))
          (cond  ;; Skip unescaping a char, just incf idx to skip the hex pair.
           ((and only-safe-characters-p (uri-reserved-or-unsafe-char-p new-char))
            (incf idx 3))
           ;; Escape a char, we have already started a new string.
           (new-string 
            (let ((new-idx2 (+ new-idx (- idx last-idx))))
              (setf new-string (replace new-string string :start1 new-idx :end1 new-idx2 :start2 last-idx :end2 idx)
                (aref new-string new-idx2) new-char
                new-idx (1+ (the fixnum new-idx2))
                last-idx (incf idx 3))))
           ;; Escape a char, need to start a new string.
           (t (setf new-idx (- idx start)
                new-string (replace (make-array (- end start 2) :fill-pointer t :element-type *standard-character-type*)
                                    string :start1 0 :end1 new-idx :start2 start :end2 idx)
                (aref new-string new-idx) new-char
                last-idx (incf idx 3))
              (incf new-idx)))
        else
        do (incf idx)
        finally (return (cond ;; We've started a new string, now finish up
                         (new-string 
                          (let ((new-end (+ (the fixnum new-idx) (- end last-idx))))
                            (setf new-string (replace new-string string :start1 new-idx :end1 new-end :start2 last-idx :end2 end)
                              (fill-pointer new-string) new-end))
                          (values new-string t t))
                         ;; No escaping was performed
                         ((and (zerop start) (or (not end-supplied-p) (= end (length string))))
                          (values string nil nil))
                         ;; Trim original as necessary
                         (t (values (subseq string start end) nil t)))))))


(defun most-specific-exported-parent-url (string &optional (backoff-level 1) (end (length string)) &aux first-dir-delim)
  "Returns the most specific exported parent URL.
This is intended for static content types only, because the export type is computed from the pathname extension."
  (declare (values parent-url translation export-type directory-levels first-delim last-delim)
           (fixnum backoff-level))
  (flet ((get-export-type (string start end &aux period-pos)
           ;; there is no extension and we aren't going to grovel files to figure it out right now
           (when (setq period-pos (position #\. string :start start :end end :from-end t :test #'eql))
             (let ((extension (subseq string (1+ (the fixnum period-pos)) end)))
               (declare (dynamic-extent extension))
               (export-type-for-pathname-type extension nil)))))
    (declare (inline get-export-type))
    (when (setq first-dir-delim (position #\/ string :start 7 :end end :test #'eql))
      (loop with last-delim = (position #\/ string :start first-dir-delim :end end :from-end t :test #'eql)
            for directory-backoff fixnum downfrom backoff-level
            until (zerop directory-backoff)
            for delim = last-delim then (position #\/ string :start first-dir-delim :end delim :from-end t :test #'eql)
            while delim
          as url = (if delim 
                       (intern-url string :start 0 :end (1+ (the fixnum delim)) :if-does-not-exist :soft)
                     nil)
            when url
              do (let ((translation (translation-method url)))
                   (if translation
                       (return (values url translation (get-export-type string last-delim end)
                                       (- backoff-level directory-backoff) delim last-delim))
                       (return nil)))
            finally (return nil)))))