;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: xml-utils; -*-
;;;

#|
<DOCUMENTATION>
 <DESCRIPTION>
  this code originates in the CLOCC / CLLIB - thus the distinct file.
  see http://clocc.sourceforge.net/ for more.
  </DESCRIPTION>
 <COPYRIGHT YEAR='2001' AUTHOR='james adam anderson' MARK='(C)'
            href='file://xml/sysdcl.lisp' />
 <CHRONOLOGY>
  <DELTA DATE='20010605' AUTHOR='MS'>split-string</DELTA>
  <DELTA DATE='20010702'>end limit enforced in split-seq</DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
|#

(in-package "XML-UTILS")

(defun split-seq (seq pred &key (start 0) end key strict)
  "Return a list of subseq's of SEQ, split on predicate PRED.
Start from START, end with END.  If STRICT is non-nil, collect
zero-length subsequences too.
  (split-seq SEQ PRED &key (start 0) end key strict)"
  (declare (sequence seq) (type (function (t t) t) pred) (fixnum start))
  (loop :for st0 = (if strict start
                       (position-if-not pred seq :start start
                                        :end end :key key))
        :then (if strict (if st1 (1+ st1))
                  (position-if-not pred seq :start (or st1 st0)
                                   :end end :key key))
        :with st1 = 0 :while (and st0 st1) :do
        (setq st1 (position-if pred seq :start st0 :end end :key key))
        ;; 20010702.jaa added end conditional, as the original was ignoring end limit
        :collect (subseq seq st0 (if st1 st1 end))))

(defun split-string (str chars &rest opts)
  "Split the string on chars."
  (declare (string str))
  (apply #'split-seq str
         (etypecase chars
           (sequence #'(lambda (ch) (declare (character ch)) (find ch chars)))
           (function chars))
         opts))

;(split-string "asdf,qwer" "," :end 3) (subseq "asdf" 1 nil)