;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: tk1; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.130
;;; Reason: Patch in Fast Tokenizer
;;; 
;;; Redefine all tokenizers and reinsert any standard tokens.
;;; 
;;; DEFINE-TOKENIZER HTTP::HEADER:  -
;;; DEFINE-TOKENIZER HTTP:HEADER-VALUE:  -
;;; DEFINE-TOKENIZER HTTP::FORM-QUERY-KEYWORD:  -
;;; Written by JCMa, 5/10/01 17:03:31
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.129,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 54.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.4, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.26, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Images Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; HTTP Proxy Server 6.20, HTTP Client Substrate 4.9, Statice Server 466.2,
;;; Images 431.2, Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Image Substrate 440.4, Jcma 43, HTTP Client 50.6,
;;; W4 Constraint-Guide Web Walker 45.10, W4 Examples 15.0,
;;; Experimental CL-HTTP CLIM User Interface 1.0, CL-HTTP Documentation 3.0,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.16,
;;; DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x994 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number 6294063,
;;; Domain Fixes (from CML:MAILER;DOMAIN-FIXES.LISP.33),
;;; Don't force in the mail-x host (from CML:MAILER;MAILBOX-FORMAT.LISP.24),
;;; Make Mailer More Robust (from CML:MAILER;MAILER-FIXES.LISP.15),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add CLIM presentation and text style format directives. (from FV:SCLC;FORMAT.LISP.20),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; Make update schema work on set-value attributes with accessor names (from CML:LISPM;STATICE-SET-VALUED-UPDATE.LISP.1),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.107),
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.11),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7),
;;; Compute serial from network addresses (from W:>reti>compute-serial-from-network-addresses.lisp.2).

;;; Patch file for CL-HTTP version 70.130
;;; Written by JCMa, 5/10/01 19:02:17
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.129,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.4, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.26, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Documentation Patches 1.0,
;;; Genera 8 5 Clim Patches 1.2, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Jcma 43, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x994 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number -2141189585,
;;; Domain Fixes (from CML:MAILER;DOMAIN-FIXES.LISP.33),
;;; Don't force in the mail-x host (from CML:MAILER;MAILBOX-FORMAT.LISP.24),
;;; Make Mailer More Robust (from CML:MAILER;MAILER-FIXES.LISP.15),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add CLIM presentation and text style format directives. (from FV:SCLC;FORMAT.LISP.20),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; Make update schema work on set-value attributes with accessor names (from CML:LISPM;STATICE-SET-VALUED-UPDATE.LISP.1),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.107),
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.11).


;;; Patch file for CL-HTTP version 70.130
;;; Written by JCMa, 5/10/01 18:49:34
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.129,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.4, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.26, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Documentation Patches 1.0,
;;; Genera 8 5 Clim Patches 1.2, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Jcma 43, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x994 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number -2141189585,
;;; Domain Fixes (from CML:MAILER;DOMAIN-FIXES.LISP.33),
;;; Don't force in the mail-x host (from CML:MAILER;MAILBOX-FORMAT.LISP.24),
;;; Make Mailer More Robust (from CML:MAILER;MAILER-FIXES.LISP.15),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add CLIM presentation and text style format directives. (from FV:SCLC;FORMAT.LISP.20),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; Make update schema work on set-value attributes with accessor names (from CML:LISPM;STATICE-SET-VALUED-UPDATE.LISP.1),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.107),
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.11).


;;; Patch file for CL-HTTP version 70.130
;;; Written by JCMa, 5/10/01 17:58:53
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.129,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 54.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.4, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.26, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Images Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; HTTP Proxy Server 6.20, HTTP Client Substrate 4.9, Statice Server 466.2,
;;; Images 431.2, Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Image Substrate 440.4, Jcma 43, HTTP Client 50.6,
;;; W4 Constraint-Guide Web Walker 45.10, W4 Examples 15.0,
;;; Experimental CL-HTTP CLIM User Interface 1.0, CL-HTTP Documentation 3.0,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.16,
;;; DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x994 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number 6294063,
;;; Domain Fixes (from CML:MAILER;DOMAIN-FIXES.LISP.33),
;;; Don't force in the mail-x host (from CML:MAILER;MAILBOX-FORMAT.LISP.24),
;;; Make Mailer More Robust (from CML:MAILER;MAILER-FIXES.LISP.15),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add CLIM presentation and text style format directives. (from FV:SCLC;FORMAT.LISP.20),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; Make update schema work on set-value attributes with accessor names (from CML:LISPM;STATICE-SET-VALUED-UPDATE.LISP.1),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.107),
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.11),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7),
;;; Compute serial from network addresses (from W:>reti>compute-serial-from-network-addresses.lisp.2).





(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;TOKENIZER.LISP.38"
  "HTTP:SERVER;SERVER.LISP.908"
  "HTTP:SERVER;HEADERS.LISP.501")


(eval-when (:execute :compile-toplevel :load-toplevel)
(dolist (x '("MAKE-LOCK" "WITH-LOCK-HELD" "TOKENIZER" "ENTRY" "VALUE-ENTRY"))
  (let ((sym (scl:intern-local-soft x :tk1)))
    (unintern sym :tk1)))

(tk1:undefine-tokenizer "HEADER" :http)
(tk1:undefine-tokenizer "HEADER-VALUE" :http)
(tk1:undefine-tokenizer "HEADER-KEYWORD" :http)
(tk1:undefine-tokenizer "FORM-QUERY-KEYWORD" :http)
)
;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TOKENIZER.LISP.38")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ansi-common-lisp; Base: 10; Package: (tk1 :use (future-common-lisp)); -*-")

;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Base: 10; Package: (tk1 :use (future-common-lisp)); -*-

;;; Copyright John C. Mallery,  1999-2000.
;;; All rights reserved.

;;; Inspired by a kernel provided by Rusty Johnson. Revised, polished,
;;; debugged, and integrated by John C. Mallery. Improved by comments from
;;; Martin Simmons, Kalman Reti.

;;;------------------------------------------------------------------- 
;;;
;;; FAST TOKEN HASHING
;;; 

(eval-when (:load-toplevel :compile-toplevel :execute)
   (defpackage tk1
      (:use future-common-lisp)
      (:import-from "WWW-UTILS" "WITH-FAST-ARRAY-REFERENCES" "MAKE-LOCK" "WITH-LOCK-HELD")
      (:export
    "CREATE-TOKENIZER"
        "DEFINE-TOKENIZER"
        "DESCRIBE-TOKENIZER"
        "FIND-TOKENIZER-NAMED"
        "GET-TOKEN"
        "MAP-TOKENS"
        "REHASH"
        "TOKENIZE"
        "UNDEFINE-TOKENIZER"))
)

(in-package :tk1) 

(eval-when (:load-toplevel :compile-toplevel :execute)

;;;------------------------------------------------------------------- 
;;;
;;; STRING HASHING FUNCTIONS 
;;; 

#+Genera
(declaim (inline non-negative-fixnum))

#+Genera
(defun non-negative-fixnum (n)
  #+Genera 
  (logxor (ldb (byte 31. 0) n) (ldb (byte 1. 31.) n))
  #-Genera
  (multiple-value-bind (top bottom)
                       (floor n most-positive-fixnum)
    (loop until (or (zerop top) (eql -1 top)) ;(< -2 top 1) might be faster on some architectures
          do (multiple-value-setq (top bottom)
               (floor (logxor top bottom) most-positive-fixnum))
          finally (return bottom)))) 

#|
(defun rotate (integer n)
  ;; 32-bit rotate in genera
  #+Genera
  (sys:rot integer n)
  #-Genera
  (let* ((window (- (integer-length most-positive-fixnum) 1 n))         ; remove 1 for sign bit
         (source (ldb (byte window 0) integer)))
    (declare (dynamic-extent window source))
    (dpb source (byte window n) (ldb (byte n window) integer))))
|# 

#+Genera
(defmacro %rotate (integer n)
  ;; 32-bit rotate in genera
  #+Genera 
  `(sys:rot ,integer ,n)
  ;; 29-bit rotate everywhere else
  #-Genera 
  ; Wrong if ever compiled on a cross compiler for a different platform architecture.
  ; Remove 1 bit to avoid creating a bignum
  (let ((window (- (integer-length most-positive-fixnum) 1 n)))         ; remove 1 for sign bit
    `(let ((source (ldb ,(byte window 0) ,integer)))
       (declare (dynamic-extent source))
       (dpb source ,(byte window n) (ldb ,(byte n window) ,integer))))) 

#+Genera
(declaim (inline HASH-CHAR))

#+Genera
(defun hash-char (hash char hash-mask)
  (let* ((mask-bits (logand (char-code char) hash-mask))
         (merge-bits (logxor mask-bits hash)))
    (declare (dynamic-extent mask-bits merge-bits))
    (%rotate merge-bits 7)))

;; This is the same as (sxhash (subseq string start end))
;; Same speed as (sxhash <string>) on VLM (~ 1.55 secs / 10 million iterations)
;;
;; Hash-Masks: Case-Insensitive #b11011111
;;               Case-Sensitive #b11111111 
#+Genera
(defun hash-string (string &optional (start 0) (end (length string)) (hash-mask #b11011111))
  (declare (fixnum start end))
  (with-fast-array-references ((string string string))
    (loop for idx fixnum from start below end
          for hash fixnum = (hash-char most-positive-fixnum (aref string idx) hash-mask)
		   then (hash-char hash (aref string idx) hash-mask)
          finally (return-from hash-string (non-negative-fixnum hash)))))

;;;------------------------------------------------------------------- 
;;;
;;; FAST STRING EQUALITY FUNCTIONS 
;;;

;; TOKEN-EQUAL is either STRING-EQUAL or STRING= depending on the value of mask
;;   It also has no keyword options, and ignores character style (font) and control-bits.
;;   It is approximately 3 times faster.
;; char-equal (zerop (logand #b11011111 (logxor (char-code #\a) (char-code #\a) )))
;; char=      (zerop (logand #b11111111 (logxor (char-code #\a) (char-code #\a) )))
;; char=      (zerop                    (logxor (char-code #\a) (char-code #\a) )) 

(defmacro %%token-equal (string1 start1 end1 string2 start2 end2 mask)
  (case mask
    (#b11011111
     #-Genera
     `(string-equal ,string1 ,string2 :start1 ,start1 :end1 ,end1 :start2 ,start2 :end2 ,end2)
     #+Genera
     `(with-fast-array-references ((string1 string1 string)
				   (string2 string2 string))
	(loop for idx1 fixnum upfrom ,start1 below ,end1
	      for idx2 fixnum upfrom ,start2
	      do (let* ((xor (logxor (char-code (aref ,string1 idx1))
				     (char-code (aref ,string2 idx2)) ))
			(mask-and (logand mask xor)))
		   (declare (dynamic-extent xor mask-and))
		   (unless (eql 0 mask-and)
		     (return nil)))
	      finally (return t))))
    (#b11111111
     #-Genera
     `(string= ,string1 ,string2 :start1 ,start1 :end1 ,end1 :start2 ,start2 :end2 ,end2)
     #+Genera ;; EQL is faster than all the bit operations -- Reti 4/16/2000.
     `(with-fast-array-references ((string1 string1 string)
				   (string2 string2 string))
	(loop for idx1 fixnum upfrom ,start1 below ,end1
	      for idx2 fixnum upfrom ,start2
	      unless (eql (aref ,string1 idx1) (aref ,string2 idx2))
		do (return nil)
	      finally (return t))))
    (t `(ecase ,mask
	  (#b11011111
	   (%%token-equal ,string1 ,start1 ,end1 ,string2 ,start2 ,end2 #b11011111))
	  (#b11111111
	   (%%token-equal ,string1 ,start1 ,end1 ,string2 ,start2 ,end2 #b11111111))))))

(declaim (inline %token-equal))

(defun %token-equal (string1 start1 end1 string2 start2 end2 mask)
  (declare (fixnum start1 end1 start2 end2))
  (cond ((eql (the fixnum (- end1 start1)) (the fixnum (- end2 start2)))
         (%%token-equal string1 start1 end1 string2 start2 end2 mask))
        (t nil)))

(defun token-equal (string1 string2 &key (start1 0) (end1 (length string1)) 
                              (start2 0) (end2 (length string2)) (test #'char-equal))
  "Returns non-null if string1 is equal to string2 within the specified ranges.
test operators can be CHAR=, CHAR-EQUAL and EQL."
  (check-type string1 string)
  (check-type string2 string)
  (let ((mask (cond ((or (eq test #'char-equal) (eq test 'char-equal))
                     #b11011111)
                    ((or (eq test #'eql) (eq test 'eql))
                     #b11111111)
                    ((or (eq test #'char=) (eq test #'char=))
                     #b11111111)
                    (t (error "Unknown function for TEST: ~S" test)))))
    (%token-equal string1 start1 end1 string2 start2 end2 mask))) 

)
;;;------------------------------------------------------------------- 
;;;
;;; GENERATING GOOD ARRAY SIZES 
;;;

;;; Generate a sequence of primes at compile time to use as array sizes.

(eval-when (:load-toplevel :execute :compile-toplevel)

;; grow by 2.5 percent
(defvar *tokenizer-table-growth-factor* 1.025) 

;; use primes distributed logarithmically between 3 and MAX-SIZE as sizes for tokenizer tables. 
(defun prime-numbers-for-tokenizer-system (max-size)
  (flet ((next-prime (start)
           (declare (values prime prime+2))
           (loop with prime
                 for prime+2 fixnum from (logior start 1) by 2
                 when (loop for factor fixnum from 3 by 2 to (isqrt prime+2)
                                 never (zerop (mod prime+2 factor)))
                 do (if (eql prime (- prime+2 2))
                      (return-from next-prime prime)
                      (setq prime prime+2)))))
    (declare (inline next-prime))
    (loop with max-tokenizer-elements = (min most-positive-fixnum max-size)
          and growth-factor float = *tokenizer-table-growth-factor*
          for i fixnum = 3 then (floor (* (1+ prime) growth-factor))
          while (< i max-tokenizer-elements)
          as prime fixnum = (next-prime i)
          collect prime)))

(defvar *tokenizer-maximum-size* 1000000)       ; 1 million entries max
)                                       ; close eval when 

(eval-when (:load-toplevel :execute :compile-toplevel)

(defvar *tokenizer-table-sizes* '#.(prime-numbers-for-tokenizer-system *tokenizer-maximum-size*))

(defun select-table-size (filled-size)
  "Returns a new size for a tokenizer table with filled-size elements
as well as a threshold when the tokenizer table should be rehashed."
  (declare (values size rehash-size)
           (fixnum filled-size))
  (let* ((min-size (ceiling (* filled-size 5) 4))	; allow hash array to become 80% full.
	 (modulus (find min-size *tokenizer-table-sizes* :test #'<)))
    (declare (fixnum modulus))
    (unless modulus
      (error "Attempt to create a tokenizer table larger than the maximum implementation size, ~D." *tokenizer-maximum-size*))
    (values modulus (floor (* modulus 4) 5)))) 

;;;------------------------------------------------------------------- 
;;;
;;; DEFINE TOKENIZER
;;;

(defstruct (tokenizer (:print-function print-tokenizer))
  (lock nil)                            ; lock for multithreaded operation
  (table nil :type vector)                           ; array containing entries 
  (modulus 0 :type fixnum) 
  (mask 0 :type fixnum) 
  (function nil :type (or null function))      ; tokenizing function
  (count 0 :type fixnum)               ; number of entries
  (rehash-size 0 :type fixnum)          ; when to grow the table
  (name "tokenizer" :type string)
  (documentation nil :type (or null string))) 

(defun print-tokenizer (tokenizer stream depth)
   (declare (ignore depth))
   (print-unreadable-object (tokenizer stream :type t :identity t)
      (when (tokenizer-function tokenizer)
          (format stream "~A ~D (~D)" 
                       (tokenizer-name tokenizer) (tokenizer-count tokenizer) (tokenizer-rehash-size tokenizer)))))

(defstruct (entry  (:type list) #+ignore(:print-function print-entry))
  (hash 0 :type fixnum)                ; hash value 
  (key nil :type (or null string) :read-only t)        ; token string
  (key-size 0 :type fixnum :read-only t)                 ; token string size
  (value nil :read-only t))                          ; token value

(defun print-entry (entry stream depth)
   (declare (ignore depth))
   (print-unreadable-object (entry stream :type t :identity t)
      (when (entry-key entry)
          (write (entry-key entry) :stream stream :escape nil)
          (write-string "=>" stream)
          (write (entry-value entry) :stream stream :escape nil)))) 

(defparameter *default-tokenizer-size* 20
   "The default initial size for tokenizer tables.")

(defun allocate-table (size)
  (make-array size :element-type t :initial-element nil))

(defun compute-mask (test)
  (cond ((or (eql test 'string-equal) (eql test #'string-equal)
             (eq test #'char-equal) (eq test 'char-equal)
             (eq test #'equalp) (eq test 'equalp))
         #b11011111)
        ((or (eql test 'string=) (eql test #'string=)
             (eq test #'eql) (eq test 'eql)
             (eq test #'char=) (eq test #'char=)
             (eq test #'equal) (eq test 'equal))
         #b11111111)
        (t (error "Unknown string equality comparitor, ~S." test))))

(defun create-tokenizer (name &key (function #'identity) (size *default-tokenizer-size*) (test #'string-equal)
			      documentation)
  (check-type name string)
  (check-type size integer)
  (check-type function function)
  (multiple-value-bind (modulus rehash-size) 
      (select-table-size  size)
    (make-tokenizer :table (allocate-table modulus)
		    :modulus modulus
		    :mask (compute-mask test)
		    :function function
		    :rehash-size rehash-size
		    :count 0
		    :name name
		    :lock (make-lock name :type :multiple-reader-single-writer)
		    :documentation documentation))) 

;;;------------------------------------------------------------------- 
;;;
;;; MAPPING TOKEN TABLES 
;;;

(defun map-tokens (tokenizer function)
  "Maps FUNCTION over all the tokens of TOKENIZER.
FUNCTION is called with (KEY VALUE), where KEY is
a string and VALUE is the tokenized value."
  (with-lock-held ((tokenizer-lock tokenizer) :read "Map Tokens")
    (loop for entry across (tokenizer-table tokenizer)
          when entry 
          do (funcall function (entry-key entry) (entry-value entry)))
    tokenizer)) 

;;;------------------------------------------------------------------- 
;;;
;;; REHASHING TOKEN TABLES 
;;; 

;; Computes the table index given the hash value and the modulus.
(defmacro compute-index (hash-value modulus)
  `(rem (the fixnum ,hash-value) (the fixnum ,modulus)))

(defun %rehash-tokenizer (tokenizer &optional size)
  (check-type tokenizer tokenizer)
  (flet ((rebucket (entry tokenizer new-table modulus)
           (declare (fixnum modulus))
           (with-fast-array-references ((new-table new-table vector))
             (loop with index fixnum = (compute-index (entry-hash entry) modulus)
                   as temp = (aref new-table index)
                   repeat modulus
                   do (unless temp
                        (setf (aref new-table index) entry)
                        (incf (tokenizer-count tokenizer))
                        (return-from rebucket t))
                   do (unless (< (incf index) modulus)
                        (decf index modulus) )
                   finally (error "Missing hash bucket: This should never happen.")))))
    (declaim (inline rebucket))
    (multiple-value-bind (modulus rehash-size)
                         (select-table-size (or size (* 2 (tokenizer-count tokenizer))))
      (let ((table (tokenizer-table tokenizer))
            (new-table (allocate-table modulus)))
        (with-fast-array-references ((table table vector))
          (loop with count fixnum = 0
                for entry across table
                when entry
                do (rebucket entry tokenizer new-table modulus)
                (incf count)
                finally (setf (tokenizer-table tokenizer) new-table
                              (tokenizer-modulus tokenizer) modulus
                              (tokenizer-rehash-size tokenizer) rehash-size
                              (tokenizer-count tokenizer) count))))
      tokenizer)))

(defun rehash-tokenizer (tokenizer &optional size)
  "Rehashes the tokenizer, TOKENIZER, according to SIZE while holding a write lock."
  (with-lock-held ((tokenizer-lock tokenizer) :write "Rehash Tokenizer")
    (%rehash-tokenizer tokenizer size)))

(defun optimize-tokenizer (tokenizer)
  (rehash-tokenizer tokenizer (tokenizer-count tokenizer)))

;;;------------------------------------------------------------------- 
;;;
;;; TOKENIZE 
;;; 

(defmacro with-tokenizing ((tokenizer block hash-mask hash string start end
                                      &key (table-var 'table) (index-var 'index)) 
                           create-entry-form)
  `(let* ((,table-var (tokenizer-table ,tokenizer))
          (modulus (tokenizer-modulus ,tokenizer))
          (,index-var (compute-index ,hash modulus)))
     (declare (fixnum ,index-var hash modulus))
     (with-fast-array-references ((,table-var ,table-var array))
       (loop as entry = (aref ,table-var ,index-var)
             repeat modulus
             unless entry
             do ,create-entry-form
             when (and (eql (entry-hash entry) ,hash)
                       (%token-equal ,string ,start ,end (entry-key entry) 0 (entry-key-size entry) ,hash-mask))
             do (return-from ,block (values (entry-value entry) nil))
             unless (< (incf ,index-var) modulus)
             do (decf ,index-var modulus)
             finally (error "TOKENIZE Lost: This should never happen.")))))

(defun %intern-token (tokenizer table index hash string start end)
  (declare (fixnum index start end))
  (let* ((key (subseq string start end))
	 (key-size (- end start))
	 (token (funcall (tokenizer-function tokenizer) key 0 key-size)))
    (setf (aref table index) (make-entry :hash hash :key key :key-size key-size :value token))
    (when (> (incf (the fixnum (tokenizer-count tokenizer))) (tokenizer-rehash-size tokenizer))
      (%rehash-tokenizer tokenizer))
    token))

(declaim (notinline %tokenize))

(defun %tokenize (tokenizer hash-mask string start end)
  "Tokenize STRING using TOKENIZER while ensuring thread safety."
  (declare (values token newly-interned-p))
  (let ((lock (tokenizer-lock tokenizer))
	(hash (hash-string string start end hash-mask)))
    (with-lock-held (lock :read "Tokenize")
      (with-tokenizing (tokenizer %tokenize hash-mask hash string start end)
		       (return nil)))		; fall through to capture write lock
    ;; must recheck in case someone else has interned the entry or the tokenizer has been rehashed.
    (with-lock-held (lock :write "Intern Token")
      (with-tokenizing (tokenizer %tokenize hash-mask hash string start end)
		       (return-from %tokenize (values (%intern-token tokenizer table index hash string start end) t))))))

(defun tokenize (tokenizer string &optional (start 0) (end (length string)))
  "Tokenize STRING using TOKENIZER while ensuring thread safety."
  (declare (values token newly-interned-p))
  (%tokenize tokenizer (tokenizer-mask tokenizer) string start end))

(declaim (inline %get-token))

(defun %get-token (tokenizer hash-mask string start end)
  (let ((hash (hash-string string start end hash-mask)))
    (with-lock-held ((tokenizer-lock tokenizer) :read "Tokenize")
      (with-tokenizing (tokenizer %get-token hash-mask hash string start end)
		       (return nil)))))

(defun get-token (tokenizer string &optional (start 0) (end (length string)))
  (%get-token tokenizer (tokenizer-mask tokenizer) string start end))

(defun update-tokenizer (tokenizer name function test documentation)
   (check-type tokenizer tokenizer)
   (check-type name string)
   (check-type function function)
   (check-type test function)
   (check-type documentation (or null string))
   (setf (tokenizer-name tokenizer) name
            (tokenizer-function tokenizer) function
            (tokenizer-mask tokenizer) (compute-mask  test)
            (tokenizer-documentation tokenizer) documentation)
   tokenizer) 

#|
(setq t1 (create-tokenizer "test-1" :tokenizer #'http::intern-keyword :size 100 :test #'string=))

(dolist (x '("foo" "bar" "baz"))
  (tokenize t1 x))

(map-tokens t1 #'(lambda (key val) (format t "~&~A: ~S" key val)))

(dolist (x '("foo" "bar" "baz" "Foo" "Bar" "Baz" "Joe"))
  (multiple-value-bind (token new-p)
                       (tokenize t1 x)
    (format t "~&~A => ~S ~:[~;!~]" x token new-p)))
|# 


;;;------------------------------------------------------------------- 
;;;
;;; DESCRIBING TOKENIZERS 
;;; 

(defun collision-map (tokenizer &aux collision-alist)
  "Returns an alist of the hash collisions in tokenizer."
  (with-lock-held ((tokenizer-lock tokenizer) :read "Count Collisions")
    (let* ((table (tokenizer-table tokenizer))
           (modulus (tokenizer-modulus tokenizer))
           (hash-mask (tokenizer-mask tokenizer)))
      (labels ((note-collision (num-collisions key colliding-keys)
                 (let ((entry (assoc num-collisions collision-alist)))
                   (if entry
                     (push (list* key colliding-keys) (cdr entry))
                     (push (list num-collisions (list* key colliding-keys)) collision-alist))))
               (check-entry (key value)
                 (declare (ignore value))
                 (let* ((key-size (length key))
                        (hash (hash-string key 0 key-size hash-mask)) 
                        (index (compute-index hash modulus)))
                   (declare (fixnum index-var hash modulus))
                   (with-fast-array-references ((table table vector))
                     (loop with num-collisions fixnum = 0
                           as entry = (aref table index)
                           repeat modulus
                          ;; do (format t "~&Checking ~D: ~S against ~S" num-collisions key (entry-key entry))
                           when (and (eql (entry-hash entry) hash)
                                     (%token-equal key 0 key-size (entry-key entry) 0 (entry-key-size entry) hash-mask))
                           do (unless (zerop num-collisions)
                                (note-collision num-collisions key colliding-keys))
                           (return-from check-entry)
                           else do (incf num-collisions)
                           collect (entry-key entry) into colliding-keys
                           unless (< (incf index) modulus)
                           do (decf index modulus)
                           finally (error "COUNT-COLLISIONS Lost: This should never happen."))))))
        (declare (dynamic-extent #'note-collision #'check-entry))
        (map-tokens tokenizer #'check-entry)
        collision-alist))))

(defun show-collisions (tokenizer &optional (stream *standard-output*))
  "Shows the hash collisions in TOKENIZER on STREAM."
  (let ((collision-alist (collision-map tokenizer)))
    (format stream "~&~:[No ~;~]Hash Collisions for ~S:" collision-alist tokenizer)
    (when collision-alist
      (loop for (num-collisions . entries) in (sort collision-alist #'> :key #'car)
            do (format stream "~&~D Collision~:*~P for ~D Entr~:*~@P:" num-collisions (length entries))
            sum (loop for (key . colliding-keys) in entries
                      do (format stream "~&~30T~A:~{ ~A~^,~}" key colliding-keys)
                      sum num-collisions)
            into collisions
            finally (format stream "~&Hash Secondary Buckets: ~D" collisions))
      collision-alist)))

(defun describe-tokenizer (tokenizer &optional (stream *standard-output*))
   "Describes TOKENIZER on STREAM."
   (flet ((describe-entry (key value)
               (format stream "~12T~A => ~S~&" key value)))
      (format stream "~&Tokenizer: ~A~&Filled Elements: ~D~&Rehash Size: ~D~&Modulus: ~D~&Function: ~S~&Documentation: ~A" 
                   (tokenizer-name tokenizer)
                   (tokenizer-count tokenizer)
                   (tokenizer-rehash-size tokenizer)
                   (tokenizer-modulus tokenizer)
                   (tokenizer-function tokenizer)
                   (tokenizer-documentation tokenizer))
      (show-collisions tokenizer stream)
      (format stream "~&Contents:" )
      (map-tokens tokenizer #'describe-entry)
      tokenizer))

;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

#+ignore
(defun map-over-tokens (tokenizer continuation string delimiters &optional (start 0) (end (length string))) 
  (flet ((charset-position (charset string start end)
           (declare (fixnum start end))
           (with-fast-array-references ((string string string))
             (loop for idx fixnum upfrom start below end
                   when (member (aref string idx) charset)
                   return idx
                   finally (return nil))))
         (not-charset-position (charset string start end)
           (declare (fixnum start end))
           (with-fast-array-references ((string string string))
             (loop for idx fixnum upfrom start below end
                   unless (member (aref string idx) charset)
                   return idx
                   finally (return nil)))))
    (declare (inline charset-position not-charset-position))
    (loop with temp = start
	  do (setq start (or (not-charset-position delimiters string temp end) end))
	  while (< start end)
	  do (setq temp (or (charset-position delimiters string start end) end))
          (funcall continuation (tokenize token-table string start temp))
	  until (= temp end))))

;;;------------------------------------------------------------------- 
;;;
;;; PROGRAMMER INTERFACE
;;;

(defun %tokenizer-variable (name &optional (package *package*))
   (intern (concatenate 'string "*" (string-upcase name) "-TOKENIZER*") package))

(defun %tokenizer-function (name &optional (package *package*))
  (intern (format nil "TOKENIZE-~A" name) package))

(defun %tokenizer-fast-function (name &optional (package *package*))
  (intern (format nil "%TOKENIZE-~A" name) package)) 

(defun find-tokenizer-named (name &optional (error-p t) (package *package*))
   "Finds the tokenizer named NAME.
Unless ERROR-P is non-null, it returns NIL when no tokenizer is found.
Otherwise an error is signalled."
   (let ((variable (%tokenizer-variable name package)))
      (cond ((boundp variable) (symbol-value variable))
               (error-p (error "No tokenizer named ~S was found." name))
               (t nil))))

(defun %define-tokenizer (name tokenizer test size documentation definer)
  (labels ((frob-test (test)
	     (etypecase test
	       (symbol
		 (case test
		   ((equalp char-equal) 'string-equal)
		   ((eql char=) 'string=)
		   (t test)))
	       (cons
		 (case (car test)
		   ((quote function) (frob-test (second test)))
		   (t test))))))
    (let* ((name (string name))
	   (package *package*)
	   (variable (%tokenizer-variable name package))
	   (function-name (%tokenizer-function name package))
	   (fast-function-name (%tokenizer-fast-function name package))
	   (test (frob-test test))
	   (mask (compute-mask test)))
      (check-type definer symbol)
      `(progn
	 (defvar ,variable)
	 (if (boundp ',variable)
	     (update-tokenizer ,variable ,name (fdefinition ,tokenizer) (fdefinition ',test) ,documentation)
	     (setq ,variable (create-tokenizer ,name
					       :test (fdefinition ',test)
					       :function (fdefinition ,tokenizer)
					       :size ,(or size *default-tokenizer-size*)
					       :documentation ,documentation)))
	 ;; define a fast tokenizer
	 (declaim (inline ,fast-function-name))
	 (,definer ,fast-function-name (string &optional (start 0) (end (length string)))
	  (declare (values token newly-interned-p))
	  (%tokenize ,variable ,mask  string start end))
	 ;; define a general-purpose interface function.
	 (,definer ,function-name (string &optional (start 0) (end (length string)) (if-does-not-exist :create))
	  (declare (values token newly-interned-p))
	  (ecase if-does-not-exist
	    (:create
	      (%tokenize ,variable ,mask string start end))
	    (:soft (%get-token ,variable ,mask string start end))
	    (:error
	      (or (%get-token ,variable ,mask string start end)
		  (error "Unknown token: ~S." (subseq  string start end))))))
	 ,variable))))

(defmacro define-tokenizer (name &key tokenizer test size documentation (definer 'defun))
  (%define-tokenizer name tokenizer test size documentation definer))

(defun undefine-tokenizer (name &optional (package *package*))
   (declare (values tokenizer-undefined-p))
  (let ((var (%tokenizer-variable name package)))
     (when (boundp var)
         (makunbound var)
         (unintern var package))))

)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.501")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(tk1:define-tokenizer header
                      :tokenizer '%intern-new-header
                      :test 'char-equal
                      :definer define
                      :documentation "Tokenizes HTTP headers without consing.")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.501")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(tk1:define-tokenizer header-value
                      :tokenizer '%intern-new-header-value
                      :test 'char-equal
                      :definer define
                      :documentation "Tokenizes HTTP header values without consing.")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.501")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(tk1:define-tokenizer header-keyword
                      :tokenizer '%intern-header-keyword-value
                      :test 'char-equal
                      :definer define
                      :documentation "Tokenizes HTTP header values that parse to keywords without consing.")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.908")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(tk1:define-tokenizer form-query-keyword
                      :tokenizer '%intern-new-form-query-keyword
                      :test 'char-equal
                      :definer define
                      :documentation "Tokenizes form query keywords without consing.")

