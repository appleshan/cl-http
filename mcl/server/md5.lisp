;;;-*- Mode: Lisp; Package: MD5; -*- 
;;;
;;; Copyright 1999, Digitool, Inc.
;;; All rights reserved.
;;;
;;; Enhancements Copyright 2003, Xanalys, Inc.
;;;
;;; Enhancements Copyright 2003, John C. Mallery.
;;;
;;;------------------------------------------------------------------- 
;;;
;;;  FAST, CONS-FREE 16-BIT MD5 CODE
;;;
;;; This code needs to load after http:server;md5.lisp
;;;
;;;                    SLH wrote code
;;;  04/02/99 JCMa integrated into CL-HTTP
;;;  09/11/03 JCMa conditionalized for LispWorks
;;;  09/11/03 Martin whole series of LispWorks optimizations
;;;  09/11/03 JCMa tuned for LispWorks
;;;
(in-package :md5)

;;;------------------------------------------------------------------- 
;;;
;;; PRIMITIVES
;;;

#|
(pushnew :MD5-Debugging *features*)
(setq *features* (delete :MD5-Debugging *features*))
|#

#+MD5-Debugging
(defun okp (n)
  (and #+MCL(ccl:fixnump n) 
       #+LispWorks(lw:fixnump n)
       (<= 0 n #xFFFF)))

#+MD5-Debugging
(declaim (notinline %hi16 %lo16 %lsl %lsr))

;; this inline declaration doesn't improve performance in LW 4.3 -- JCMa 9/11/03
;;#-MD5-Debugging
(declaim (inline %hi16 %lo16 %lsl %lsr)) 

(defun %hi16 (n)
  #-MD5-Debugging (declare (optimize (debug 0) (safety 0) (speed 3)
                                     #+LispWorks(hcl:fixnum-safety 0)))
  #+MD5-Debugging (unless (okp n) (break "hi ~S" n))
  (let ((n n)) ;LW 4.3 needs the let for the compiler to optimize -- 9/15/03 JCMa
    (declare (fixnum n))
    (ash n -16)))

(defun %lo16 (n)
  #-MD5-Debugging (declare (optimize (debug 0) (safety 0) (speed 3)
                                     #+LispWorks(hcl:fixnum-safety 0)))
  #+MD5-Debugging (unless (okp n) (break "lo ~S" n))
  (let ((n n)) ;LW 4.3 needs the let for the compiler to optimize -- 9/15/03 JCMa
    (declare (fixnum n))
    (logand #xFFFF n)))

(defun %lsl (n count)
  #-MD5-Debugging (declare (optimize (debug 0) (safety 0) (speed 3)
                                     #+LispWorks(hcl:fixnum-safety 0)))
  #+MD5-Debugging (unless (and (okp n) (< count 25)) (break "lsl ~S ~S" n count))
  (let ((n n) ;LW 4.3 needs the let for the compiler to optimize -- 9/15/03 JCMa
        (count count))
    (declare (fixnum n)
             (type (integer 0 25) count))   ; shouldn't 25 be 16?
    #+MCL (ccl::%ilsl count n)
    #-MCL (ash n count)))

(defun %lsr (n count)
  #-MD5-Debugging (declare (optimize (debug 0) (safety 0) (speed 3) 
                                     #+LispWorks(hcl:fixnum-safety 0)))
  #+MD5-Debugging (unless (and (okp n) (< count 25)) (break "lsr"))
  (let ((n n) ;LW 4.3 needs the let for the compiler to optimize -- 9/15/03 JCMa
        (count count))
    (declare (fixnum n)
             (type (integer 0 25) count))   ; shouldn't 25 be 16?
    #+MCL (ccl::%ilsr count n)
    #-MCL (ash n (the fixnum (- count)))))

(defun %32-add (ah al bh bl)
  (declare (fixnum ah al bh bl))
  #+MD5-Debugging (unless (and (okp ah)(okp al)(okp bh)(okp bl)) (break "add"))
  (let* ((lo (+ al bl))
         (hi (+ ah bh)))
    (declare (fixnum lo hi))
    (when (> lo #xFFFF)
      (incf hi))
    (values (%lo16 hi) (%lo16 lo))))

;;; not num
(defun %32-not (nh nl)
  (declare (fixnum nh nl))
  #+MD5-Debugging (unless (and (okp nh)(okp nl)) (break "not"))
  (values (%lo16 (lognot nh))
          (%lo16 (lognot nl))))

;;; a v b
(defun %32-or (ah al bh bl)
  (declare (fixnum ah al bh bl))
  #+MD5-Debugging (unless (and (okp ah)(okp al)(okp bh)(okp bl)) (break "or"))
  (values (logior ah bh)
          (logior al bl)))

;;; a XOR b
(defun %32-xor (ah al bh bl)
  (declare (fixnum ah al bh bl))
  #+MD5-Debugging (unless (and (okp ah)(okp al)(okp bh)(okp bl)) (break "xor"))
  (values (logxor ah bh)
          (logxor al bl)))

;;; ab
(defun %32-and (ah al bh bl)
  (declare (fixnum ah al bh bl))
  #+MD5-Debugging (unless (and (okp ah)(okp al)(okp bh)(okp bl)) (break "and"))
  (values (logand ah bh)
          (logand al bl)))

;;;a.~b
(defun %32-and-not (ah al bh bl) ;martin
  (declare (fixnum ah al bh bl))
  #+MD5-Debugging (unless (and (okp ah)(okp al)(okp bh)(okp bl)) (break "and"))
  (values (logandc2 ah bh)
          (logandc2 al bl)))

;;;a v ~b
(defun %32-or-not (ah al bh bl) ;martin
  (declare (fixnum ah al bh bl))
  #+MD5-Debugging (unless (and (okp ah)(okp al)(okp bh)(okp bl)) (break "and"))
  (values (logior ah (logxor bh #xFFFF))
          (logior al (logxor bl #xFFFF))))

;;;; ; assumes by >= 0 (conses if by near 32) --slh
#|
(defun %32-left-rot (nh nl left)
  (declare (fixnum nh nl left))
  #+MD5-Debugging  (unless (and (okp nh)(okp nl)(< left 29)) (break "not"))
  (when (> left 16)
    (psetq nh nl nl nh)
    (decf left 16))
  (let ((right (- 16 left)))
    (declare (fixnum right))
    (values (logior (the fixnum (%lo16 (%lsl nh left)))
                    (the fixnum (%lsr nl right)))
            (logior (the fixnum (%lo16 (%lsl nl left)))
                    (the fixnum (%lsr nh right))))))
|#
;; version of %32-LEFT-ROT that never conses, which makes the
;; allocation O(1) instead of O(n) -- Martin

(defun %32-left-rot (nh nl left)
  (declare (fixnum nh nl left))
   #+MD5-Debugging  (unless (and (okp nh)(okp nl)(< left 29)) (break "not"))
  (when (> left 16)
    (psetq nh nl nl nh)
    (decf left 16))
  (let* ((right (- 16 left))
         (left-mask (1- (the fixnum (%lsl 1 right)))))
    (declare (fixnum right left-mask))
    (values (logior (the fixnum (%lsl (logand nh left-mask) left))
                    (the fixnum (%lsr nl right)))
            (logior (the fixnum (%lsl (logand nl left-mask) left))
                    (the fixnum (%lsr nh right))))))

;;;; Orginal SLH versions
;;;; (defun %md5-function-f (xh xl yh yl zh zl)
;;;;   (multiple-value-bind (h1 l1) (%32-and xh xl yh yl)
;;;;     (multiple-value-bind (h l) (%32-not xh xl)
;;;;       (multiple-value-bind (h l) (%32-and h l zh zl)
;;;;         (%32-or h1 l1 h l)))))

;;;; (defun %md5-function-g (xh xl yh yl zh zl)
;;;;   (multiple-value-bind (h1 l1) (%32-and xh xl zh zl)
;;;;     (multiple-value-bind (h l) (%32-not zh zl)
;;;;       (multiple-value-bind (h l) (%32-and yh yl h l)
;;;;         (%32-or h1 l1 h l)))))

;;;; (defun %md5-function-h (xh xl yh yl zh zl)
;;;;   (multiple-value-bind (h l) (%32-xor xh xl yh yl)
;;;;     (%32-xor h l zh zl)))

;;;; (defun %md5-function-i (xh xl yh yl zh zl)
;;;;   (multiple-value-bind (h l) (%32-not zh zl)
;;;;     (multiple-value-bind (h l) (%32-or xh xl h l)
;;;;       (%32-xor yh yl h l))))

;; martin 
(defun %md5-function-f (xh xl yh yl zh zl)
  (multiple-value-bind (h1 l1) (%32-and xh xl yh yl)
    (multiple-value-bind (h l) (%32-and-not zh zl xh xl)
      (%32-or h1 l1 h l))))

(defun %md5-function-g (xh xl yh yl zh zl)
  (multiple-value-bind (h1 l1) (%32-and xh xl zh zl)
    (multiple-value-bind (h l) (%32-and-not yh yl zh zl)
      (%32-or h1 l1 h l))))

(defun %md5-function-h (xh xl yh yl zh zl)
  (multiple-value-bind (h l) (%32-xor xh xl yh yl)
    (%32-xor h l zh zl)))

(defun %md5-function-i (xh xl yh yl zh zl)
  (multiple-value-bind (h l) (%32-or-not xh xl zh zl)
    (%32-xor yh yl h l)))

;;;------------------------------------------------------------------- 
;;;
;;;  SINE TABLES
;;;

; (loop for n across *random-sine-table* collect (ash n -16))
(defconstant *%h-sine-table* #(55146 59591  9248 49597 62844 18311 43056 64838
                                     27008 35652 65535 35164 27536 64920 42617 18868
                                     63006 49216  9822 59830 54831   580 55457 59347
                                     8673 49975 62677 17754 43491 64751 26479 36138
                                     65530 34673 28061 64997 42174 19422 63163 48831
                                     10395 60065 54511  1160 55764 59099  8098 50348
                                     62505 17194 43924 64659 25947 36620 65519 34180
                                     28584 65068 41729 19976 63315 48442 10967 60294))
  
; (loop for n across *random-sine-table* collect (logand #xFFFF n))
(defconstant *%l-sine-table* #(42104 46934 28891 52974  4015 50730 17939 38145
                                     39128 63407 23473 55230  4386 29075 17294  2081
                                     9570 45888 23121 51114  4189  5203 59009 64456
                                     52710  2006  3463  5357 59653 41976   729 19594
                                     14658 63105 24866 14348 59972 53161 19296 48240
                                     32454 10234 12421  7429 53305 39397 31992 22117
                                     8772 65431  9127 41017 22979 52370 62589 24017
                                     32335 59104 17172  4513 32386 62005 53947 54161))

;;;------------------------------------------------------------------- 
;;;
;;; MD5 STATE  
;;;

; out-of-line to avoid humongous %md5-encode or %md5-encode-string
(defun ffgghhii (ah al bh bl ch cl dh dl xh xl k s i fn)
  (declare (fixnum k i s))
  (decf i)
  (multiple-value-bind (h l) 
      (funcall fn bh bl ch cl dh dl)
    (multiple-value-bind (h1 l1) 
        (%32-add ah al h l)
      (multiple-value-bind (h2 l2) 
          (%32-add (aref xh k)
                   (aref xl k)
                   (svref *%h-sine-table* i)
                   (svref *%l-sine-table* i))
        (multiple-value-bind (h l) 
            (%32-add h1 l1 h2 l2)
          (multiple-value-bind (h l) 
              (%32-left-rot h l s)
            (%32-add bh bl h l)))))))

(defmacro %with-md5-state ((ah al bh bl ch cl dh dl) &body body)
  `(let (,ah ,al ,bh ,bl ,ch ,cl ,dh ,dl)
     ,@body))

(defmacro %initialize-md5-state (ah al bh bl ch cl dh dl)
  `(setf ,ah #x6745 ,al #x2301
         ,bh #xefcd ,bl #xab89
         ,ch #x98ba ,cl #xdcfe
         ,dh #x1032 ,dl #x5476))

(defmacro %md5-function-ffgghhii (ah al bh bl ch cl dh dl xh xl k s i fghi)
  `(multiple-value-setq (,ah ,al)
       (ffgghhii ,ah ,al ,bh ,bl ,ch ,cl ,dh ,dl ,xh ,xl ,k ,s ,i #',fghi)))

(defmacro %md5-generate-code-for-one-round (fghi (ah al bh bl ch cl dh dl) xh xl
                                                 (k-initial k-inc)
                                                 (s0 s1 s2 s3) i-initial)
  (do* ((k k-initial (mod (+ k k-inc) 16))
        (which 0 (mod (1+ which) 4))
        (i i-initial (1+ i))
        (forms nil))
       ((>= i (+ i-initial 16))
        (cons 'progn (nreverse forms)))
    (multiple-value-bind (s abcd)
        (ecase which
          (0 (values s0 (list ah al bh bl ch cl dh dl)))
          (1 (values s1 (list dh dl ah al bh bl ch cl)))
          (2 (values s2 (list ch cl dh dl ah al bh bl)))
          (3 (values s3 (list bh bl ch cl dh dl ah al))))
      (push `(%md5-function-ffgghhii ,@abcd ,xh ,xl ,k ,s ,i ,fghi)
            forms))))

(defmacro %md5-digest-byte-vector-little-endian (ah al bh bl ch cl dh dl &optional (unitizer 'vector))
  (flet ((bytes (num16)
           `((ldb (byte 8 0) ,num16)
             (ldb (byte 8 8) ,num16))))
    `(let ((ah ,ah) (al ,al)
           (bh ,bh) (bl ,bl)
           (ch ,ch) (cl ,cl)
           (dh ,dh) (dl ,dl))
       (,unitizer
        ,@(bytes 'al) ,@(bytes 'ah)
        ,@(bytes 'bl) ,@(bytes 'bh)
        ,@(bytes 'cl) ,@(bytes 'ch)
        ,@(bytes 'dl) ,@(bytes 'dh)))))

(defmacro %md5-digest-byte-vector (ah al bh bl ch cl dh dl)
  "Constructs a byte vector from the 4 values returned by %MD5-ENCODE."
  `(%md5-digest-byte-vector-little-endian ,ah ,al ,bh ,bl ,ch ,cl ,dh ,dl vector))

(defmacro %md5-digest-byte-list (ah al bh bl ch cl dh dl)
  "Constructs a byte list from the 4 values returned by %MD5-ENCODE."
  `(%md5-digest-byte-vector-little-endian ,ah ,al ,bh ,bl ,ch ,cl ,dh ,dl list))

(defmacro %md5-digest-byte-hexadecimal-string (ah al bh bl ch cl dh dl)
  "Constructs a byte hexadecimal string from the 8 values returned by %MD5-ENCODE."
  `(%md5-digest-byte-vector-little-endian ,ah ,al ,bh ,bl ,ch ,cl ,dh ,dl md5-hexadecimal-encode))

(defmacro %md5-digest-byte-integer (ah al bh bl ch cl dh dl)
  "Constructs a byte integer from the 5 values returned by SHA-ENCODE."
  `(dpb ,ah (byte 16 112)
        (dpb ,al (byte 16 96)
             (dpb ,bh (byte 16 80)
                  (dpb ,bl (byte 16 64)
                       (dpb ,ch (byte 16 48)
                            (dpb ,cl (byte 16 32)
                                 (dpb ,dh (byte 16 16) ,dl))))))))

;;;------------------------------------------------------------------- 
;;;
;;;  
;;;

; don't cons if fixnum
(defun %md5-length64 (message-length-in-bits)
  (declare (values hih hil loh lol))
  (typecase message-length-in-bits
    (fixnum
     (values 0 
             0
             (%hi16 message-length-in-bits)
             (%lo16 message-length-in-bits)))
    (t (values (ldb (byte 16 48) message-length-in-bits)
               (ldb (byte 16 32) message-length-in-bits)
               (ldb (byte 16 16) message-length-in-bits)
               (ldb (byte 16  0) message-length-in-bits)))))

; avoid closure consing
(defun %md5-encode-string (string &optional (start 0) end)
  #-MD5-Debugging (declare  (optimize (speed 3) (debug 0)))
  (let ((index start)
        (end (or end (length string)))
        (state :data)
        (count 0)
        (flag13 nil))
    (labels ((string-reader-function ()
               (if (< index end)
                   (prog1 (character-to-byte (aref string index))
                     (incf index))
                 nil))
             (fill-md5-buffers (hbuffer lbuffer)
               (dotimes (i 16 t)
                 (flet ((gb ()
                          (ecase state
                            (:done (return-from fill-md5-buffers nil))
                            (:data
                             (let ((byte (string-reader-function)))
                               (if byte
                                   (progn (incf count)
                                     byte)
                                 (progn (setq state :must-pad)
                                   (when (= i 13)
                                     (setq flag13 1))
                                   #x80))))
                            (:must-pad
                             (when (and (= i 14) flag13)
                               (setq state :pad))
                             (unless (= i 14)
                               (setq state :pad)
                               (setq flag13 nil))
                             0)
                            (:pad
                             (if (= i 14)
                                 (multiple-value-bind (hih hil loh lol)
                                     (%md5-length64 (* 8 count))
                                   (setf (aref hbuffer 14) loh
                                         (aref lbuffer 14) lol
                                         (aref hbuffer 15) hih
                                         (aref lbuffer 15) hil)
                                   (setq state :done)
                                   (return-from fill-md5-buffers t))
                               0)))))
                   (declare (dynamic-extent #'gb))
                   (let ((b0 (gb)) (b1 (gb)) (b2 (gb)) (b3 (gb)))
                     (declare (fixnum b0 b1 b2 b3))
                     (setf (aref hbuffer i) (logior (the fixnum (%lsl b3 8)) b2)
                           (aref lbuffer i) (logior (the fixnum (%lsl b1 8)) b0)))))))
      (declare (dynamic-extent #'string-reader-function #'fill-md5-buffers))
      ;; Removed :element-type '(unsigned-byte 32)) because SIMPLE-VECTOR is likely to be faster
      ;; on stock hardware because the fixnums are stored directly rather than as binary values.
      (with-stack-array (xh 16)
        (with-stack-array (xl 16)
          (%with-md5-state (ah al bh bl ch cl dh dl)
            (%initialize-md5-state ah al bh bl ch cl dh dl)
            (loop 
             (if (fill-md5-buffers xh xl)
                 (%with-md5-state (aah aal bbh bbl cch ccl ddh ddl)
                   (setq aah ah aal al bbh bh bbl bl cch ch ccl cl ddh dh ddl dl)
                   (%md5-generate-code-for-one-round
                    %md5-function-f
                    (ah al bh bl ch cl dh dl) xh xl (0 1) (7 12 17 22) 1)
                   (%md5-generate-code-for-one-round
                    %md5-function-g
                    (ah al bh bl ch cl dh dl) xh xl (1 5) (5 9 14 20) 17)
                   (%md5-generate-code-for-one-round
                    %md5-function-h
                    (ah al bh bl ch cl dh dl) xh xl (5 3) (4 11 16 23) 33)
                   (%md5-generate-code-for-one-round
                    %md5-function-i
                    (ah al bh bl ch cl dh dl) xh xl (0 7) (6 10 15 21) 49)
                   (multiple-value-setq (ah al) (%32-add ah al aah aal))
                   (multiple-value-setq (bh bl) (%32-add bh bl bbh bbl))
                   (multiple-value-setq (ch cl) (%32-add ch cl cch ccl))
                   (multiple-value-setq (dh dl) (%32-add dh dl ddh ddl)))
               (return (values ah al bh bl ch cl dh dl))))))))))

;;;------------------------------------------------------------------- 
;;;
;;; REDEFINITIONS
;;; 

(defun md5-digest-vector (string &optional (start 0) end)
  "Returns a MD5 digest vector for STRING."
  (declare (values md5-digest-vector))
  (multiple-value-bind (ah al bh bl ch cl dh dl)
      (%md5-encode-string string start end)
    (%md5-digest-byte-vector ah al bh bl ch cl dh dl)))

(defun md5-digest-list (string &optional (start 0) end)
  "Returns a MD5 digest list for STRING."
  (declare (values md5-digest-list))
  (multiple-value-bind (ah al bh bl ch cl dh dl)
      (%md5-encode-string string start end)
    (%md5-digest-byte-list  ah al bh bl ch cl dh dl)))

(defun md5-digest-integer (string &optional (start 0) end)
  "Returns a MD5 digest integer for STRING."
  (declare (values md5-digest-list))
  (multiple-value-bind (ah al bh bl ch cl dh dl)
      (%md5-encode-string string start end)
    (%md5-digest-byte-integer ah al bh bl ch cl dh dl)))

; MD5 entry function
(defun md5-digest-hexadecimal-string (string &optional (start 0) end)
  "Returns a MD5 digest hexadecimal string for STRING."
  (multiple-value-bind (ah al bh bl ch cl dh dl)
      (%md5-encode-string string start end)
    (%md5-digest-byte-hexadecimal-string ah al bh bl ch cl dh dl))) 

#|

(defun 32bit-md5-digest-vector (string &optional (start 0) end)
  "Returns a MD5 digest vector for STRING."
  (declare (values md5-digest-vector))
  (multiple-value-bind (a b c d)
      (md5-encode-string string start end)
    (md5-digest-byte-vector a b c d)))

(defun 32bit-md5-digest-list (string &optional (start 0) end)
  "Returns a MD5 digest list for STRING."
  (declare (values md5-digest-vector))
  (multiple-value-bind (a b c d)
      (md5-encode-string string start end)
    (md5-digest-byte-list a b c d)))

(defun 32bit-md5-digest-hexadecimal-string (string &optional (start 0) end)
  "Returns a MD5 digest hexadecimal string for STRING."
  (multiple-value-bind (a b c d)
      (md5-encode-string string start end)
    (md5-digest-byte-hexadecimal-string a b c d)))

(defparameter +md5-test-vector+
              '(("" #xd41d8cd98f00b204e9800998ecf8427e)
                ("a" #x0cc175b9c0f1b6a831c399e269772661)
                ("abc" #x900150983cd24fb0d6963f7d28e17f72)
                ("message digest" #xf96b697d7cb7938d525a2f31aaf161d0)
                ("abcdefghijklmnopqrstuvwxyz" #xc3fcd3d76192e4007dfb496cca67e13b)
                ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
                 #xd174ab98d277d9f5a5611c2c9f419d9f)
                ("12345678901234567890123456789012345678901234567890123456789012345678901234567890"
                 #x57edf4a22be3c955ac49da2e2107b67a)))

(defun test-md5-strings ()
  (loop initially  (format t "~&Testing MD5 digest hex string")
        for (string) in +md5-test-vector+
        for old-md5 = (32bit-md5-digest-hexadecimal-string string)
        for new-md5 = (md5-digest-hexadecimal-string string)
        when (equal old-md5 new-md5)
        do (format t "~&Passed: ~S" string)
        else do (format t "~&Failed: ~S~&Old: ~S~&New: ~S" string old-md5 new-md5)))

(defun test-md5-vectors ()
  (loop initially  (format t "~&Testing MD5 digest vector")
        for (string) in +md5-test-vector+
        for old-md5 = (32bit-md5-digest-vector string)
        for new-md5 = (md5-digest-vector string)
        when (md5-digest-equal old-md5 new-md5)
        do (format t "~&Passed: ~S" string)
        else do (format t "~&Failed: ~S~&Old: ~S~&New: ~S" string old-md5 new-md5)))

(defun test-md5-lists ()
  (loop initially  (format t "~&Testing MD5 digest list")
        for (string) in +md5-test-vector+
        for old-md5 = (32bit-md5-digest-list string)
        for new-md5 = (md5-digest-list string)
        when (equal old-md5 new-md5)
        do (format t "~&Passed: ~S" string)
        else do (format t "~&Failed: ~S~&Old: ~S~&New: ~S" string old-md5 new-md5)))

(md5::test-md5-strings)

(defun make-circular-list (list)
  (let ((n-list (copy-list list)))
    (nconc n-list n-list)))

(setq *print-circle* t) ;don't lose

(defun test-performance (n &key (new-function #'md5-digest-hexadecimal-string)
                           (old-function #'32bit-md5-digest-hexadecimal-string))
  (let ((test (make-circular-list +md5-test-vector+)))
    (declare (dynamic-extent test))
    (format t "~&Statistics for 16-bit MD5 on ~D iterations: " n)
    (time
     (loop repeat n
           for ((string) . ptr) = test then ptr
           do (funcall new-function string)))
    (format t "~&Statistics for 32-bit MD5 on ~D iterations: " n)
    (time
     (loop repeat n
           for ((string) . ptr) = test then ptr
           do (funcall old-function string)))
    (format t "~&Done.")))

(without-interrupts
  (md5::test-performance 5000
                         :new-function #'md5::md5-digest-vector
                         :old-function #'md5::32bit-md5-digest-vector))

|#


