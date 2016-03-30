;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: tk1; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.138
;;; Reason: Function TK1::WITH-TOKENIZING:  more parameterization
;;; Function TK1::%TOKENIZE:  update.
;;; Function TK1::%GET-TOKEN:  -
;;; Function TK1:GET-TOKEN:  -
;;; DEFINE-TOKENIZER HTTP:HEADER-VALUE:  -
;;; DEFINE-TOKENIZER HTTP::HEADER-KEYWORD:  -
;;; Structure TK1::ENTRY:  -
;;; Written by JCMa, 7/13/01 18:33:36
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.137,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.4, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.30, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Lock Simple Patches 1.0, Jcma 43,
;;; HTTP Proxy Server 6.23, HTTP Client Substrate 4.15, Statice Server 466.2,
;;; BNF Parser 1.0, Ivory Revision 5, VLM Debugger 329, Genera program 8.16,
;;; DEC OSF/1 V4.0 (Rev. 110),
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.11),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1).

;;; Patch file for CL-HTTP version 70.138
;;; Written by JCMa, 7/29/01 02:53:21
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.6,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.139,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.4, Jcma 44,
;;; Experimental Genera 8 5 Patches 1.0, Genera 8 5 System Patches 1.34,
;;; Genera 8 5 Mailer Patches 1.1, Genera 8 5 Joshua Patches 1.0,
;;; Genera 8 5 Statice Runtime Patches 1.0, Genera 8 5 Statice Patches 1.0,
;;; Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, HTTP Proxy Server 6.23,
;;; HTTP Client Substrate 4.15, Statice Server 466.2, Wilbur 1.2, HTTP Client 50.9,
;;; Image Substrate 440.4, Essential Image Substrate 433.0, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.11),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1).


;;; Patch file for CL-HTTP version 70.138
;;; Written by JCMa, 7/27/01 21:22:09
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.6,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.137,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.4, Jcma 44,
;;; Experimental Genera 8 5 Patches 1.0, Genera 8 5 System Patches 1.34,
;;; Genera 8 5 Mailer Patches 1.1, Genera 8 5 Joshua Patches 1.0,
;;; Genera 8 5 Statice Runtime Patches 1.0, Genera 8 5 Statice Patches 1.0,
;;; Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, HTTP Proxy Server 6.23,
;;; HTTP Client Substrate 4.15, Statice Server 466.2, Wilbur 1.0, HTTP Client 50.9,
;;; Image Substrate 440.4, Essential Image Substrate 433.0, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.11),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1).


;;; Patch file for CL-HTTP version 70.138
;;; Written by JCMa, 7/16/01 13:50:40
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.137,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.4, Jcma 44,
;;; Experimental Genera 8 5 Patches 1.0, Genera 8 5 System Patches 1.30,
;;; Genera 8 5 Mailer Patches 1.1, Genera 8 5 Joshua Patches 1.0,
;;; Genera 8 5 Statice Runtime Patches 1.0, Genera 8 5 Statice Patches 1.0,
;;; Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; HTTP Proxy Server 6.23, HTTP Client Substrate 4.15, Statice Server 466.2,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.16,
;;; DEC OSF/1 V4.0 (Rev. 110),
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.11),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1).


(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;TOKENIZER.LISP.54"
  "HTTP:SERVER;TOKENIZER.LISP.44"
  "HTTP:SERVER;HEADERS.LISP.507"
  "HTTP:SERVER;SERVER.LISP.908"
  "HTTP:SERVER;TOKENIZER.LISP.46"
  "HTTP:SERVER;TOKENIZER.LISP.47"
  "HTTP:SERVER;TOKENIZER.LISP.48"
  "HTTP:SERVER;SERVER.LISP.909")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TOKENIZER.LISP.48")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ansi-common-lisp; Base: 10; Package: (tk1 :use (future-common-lisp)); -*-")

(eval-when (:compile-toplevel :load-toplevel :execute)

(defmacro %%token-equal (string1 start1 end1 string2 start2 end2 mask)
  (case mask
    (#b11011111
     #-Genera
     `(string-equal ,string1 ,string2 :start1 ,start1 :end1 ,end1 :start2 ,start2 :end2 ,end2)
     #+Genera
     `(with-fast-array-references ((,string1 ,string1 string)
                                   (,string2 ,string2 string))
        (loop for idx1 fixnum upfrom ,start1 below ,end1
              for idx2 fixnum upfrom ,start2
              do (let* ((xor (logxor (char-code (aref ,string1 idx1))
                                     (char-code (aref ,string2 idx2)) ))
                        (mask-and (logand ,mask xor)))
                   (declare (dynamic-extent xor mask-and))
                   (unless (eql 0 mask-and)
                     (return nil)))
              finally (return t))))
    (#b11111111
     #-Genera
     `(string= ,string1 ,string2 :start1 ,start1 :end1 ,end1 :start2 ,start2 :end2 ,end2)
     #+ Genera ;; EQL is faster than all the bit operations -- Reti 4/16/2000.
     `(with-fast-array-references ((,string1 ,string1 string)
                                   (,string2 ,string2 string))
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


(defmacro with-tokenizing ((tokenizer hash-mask hash string start end
                                      &key (table-var 'table) (index-var 'index)) 
                           &key entry-not-found  (entry-found '(values (entry-value entry) nil) ))
  `(let* ((,table-var (tokenizer-table ,tokenizer))
          (modulus (tokenizer-modulus ,tokenizer))
          (,index-var (compute-index ,hash modulus))
	  (size (- (the fixnum ,end) (the fixnum ,start))))
     (declare (fixnum ,index-var hash modulus size))
     (with-fast-array-references ((,table-var ,table-var array))
       (loop as entry = (aref ,table-var ,index-var)
             repeat modulus fixnum
             unless entry
               do (return ,entry-not-found)
             when (and (eql (entry-hash entry) ,hash)
		       (eql (entry-key-size entry) size)
		       (let ((key (entry-key entry)))
			 (%%token-equal key 0 size ,string ,start ,end ,hash-mask)))
               do (return ,entry-found)
             unless (< (incf ,index-var) modulus)
               do (decf ,index-var modulus)
             finally (error "TOKENIZE Lost: This should never happen.")))))

)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TOKENIZER.LISP.44")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ansi-common-lisp; Base: 10; Package: (tk1 :use (future-common-lisp)); -*-")

(PROGN
(declaim (inline %insert-new-token))

(defun %insert-new-token (tokenizer table index hash token key key-size)
  (setf (aref table index) (make-entry :hash hash :key key :key-size key-size :value token))
  (when (> (incf (the fixnum (tokenizer-count tokenizer))) (tokenizer-rehash-size tokenizer))
    (%rehash-tokenizer tokenizer))
  token)
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TOKENIZER.LISP.44")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ansi-common-lisp; Base: 10; Package: (tk1 :use (future-common-lisp)); -*-")

(defun %create-and-insert-new-token (tokenizer table index hash string start end)
  (declare (fixnum index start end))
  (let* ((key (subseq string start end))
         (key-size (- end start))
         (token (funcall (tokenizer-function tokenizer) key 0 key-size)))
    (%insert-new-token tokenizer table index hash token key key-size)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TOKENIZER.LISP.46")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ansi-common-lisp; Base: 10; Package: (tk1 :use (future-common-lisp)); -*-")

(defun %tokenize (tokenizer hash-mask string start end)
  "Tokenize STRING using TOKENIZER while ensuring thread safety."
  (declare (values token newly-interned-p))
  (let ((lock (tokenizer-lock tokenizer))
        (hash (hash-string string start end hash-mask)))
    (with-lock-held (lock :read "Tokenize")
      (with-tokenizing (tokenizer hash-mask hash string start end)
		       :entry-found (values (entry-value entry) nil)
                       :entry-not-found nil))	; fall through to capture write lock
    ;; must recheck in case someone else has interned the entry or the tokenizer has been rehashed.
    (with-lock-held (lock :write "Intern Token")
      (with-tokenizing (tokenizer hash-mask hash string start end)
                       :entry-found (values (entry-value entry) nil)
		       :entry-not-found (values (%create-and-insert-new-token tokenizer table index hash string start end) t)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TOKENIZER.LISP.46")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ansi-common-lisp; Base: 10; Package: (tk1 :use (future-common-lisp)); -*-")

(defun %get-token (tokenizer hash-mask string start end)
  (let ((hash (hash-string string start end hash-mask)))
    (with-lock-held ((tokenizer-lock tokenizer) :read "Tokenize")
      (with-tokenizing (tokenizer hash-mask hash string start end)
                       :entry-found (values (entry-value entry) nil)
		       :entry-not-found nil))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TOKENIZER.LISP.46")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ansi-common-lisp; Base: 10; Package: (tk1 :use (future-common-lisp)); -*-")

(defun get-token (tokenizer string &optional (start 0) (end (length string)))
  (%get-token tokenizer (tokenizer-mask tokenizer) string start end))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TOKENIZER.LISP.54")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ansi-common-lisp; Base: 10; Package: (tk1 :use (future-common-lisp)); -*-")

(defstruct (entry  (:type list) #+ignore(:print-function print-entry))
  (hash 0 :type fixnum)                         ; hash value 
  (key nil :type (or null string) :read-only t) ; token string
  (key-size 0 :type fixnum :read-only t)        ; token string size
  (value nil))					; token value

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TOKENIZER.LISP.46")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ansi-common-lisp; Base: 10; Package: (tk1 :use (future-common-lisp)); -*-")

(defun insert-token (tokenizer token string &optional (start 0) (end (length string)))
  (declare (fixnum start end)
	   (values token new-entry-p))
  (let* ((lock (tokenizer-lock tokenizer))
	 (hash-mask (tokenizer-mask tokenizer))
	 (hash (hash-string string start end hash-mask)))
    (with-lock-held (lock :write "Intern Token")
      (with-tokenizing (tokenizer hash-mask hash string start end)
		       :entry-found (setf (entry-value entry) token)
		       :entry-not-found (values (%insert-new-token tokenizer table index hash token
								   (subseq string start end) (- end start)) t)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TOKENIZER.LISP.44")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ansi-common-lisp; Base: 10; Package: (tk1 :use (future-common-lisp)); -*-")


(eval-when (:load-toplevel :compile-toplevel :execute)

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
            (:soft (get-token ,variable string start end))
            (:error
              (or (get-token ,variable string start end)
                  (error "Unknown token: ~S." (subseq  string start end))))))
         ,variable))))
)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.507")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(tk1:define-tokenizer header
                      :tokenizer '%intern-new-header
                      :test 'char-equal
                      :definer define
                      :documentation "Tokenizes HTTP headers without consing.")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.507")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(tk1:define-tokenizer header-value
                      :tokenizer '%intern-new-header-value
                      :test 'char-equal
                      :definer define
                      :documentation "Tokenizes HTTP header values without consing.")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.507")
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


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TOKENIZER.LISP.46")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ansi-common-lisp; Base: 10; Package: (tk1 :use (future-common-lisp)); -*-")

(defun tokenize (tokenizer string &optional (start 0) (end (length string)))
  "Tokenize STRING using TOKENIZER while ensuring thread safety."
  (declare (values token newly-interned-p))
  (locally
    (declare (inline %tokenize))
    (%tokenize tokenizer (tokenizer-mask tokenizer) string start end)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TOKENIZER.LISP.47")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ansi-common-lisp; Base: 10; Package: (tk1 :use (future-common-lisp)); -*-")

(defun clear-tokenizer (tokenizer)
  (let ((lock (tokenizer-lock tokenizer)))
    (with-lock-held (lock :write "Clear Tokenizer")
      (loop with table = (tokenizer-table tokenizer)
	    for idx upfrom 0 below (length table)
	    do (setf (aref table idx) nil)
	    finally (setf (tokenizer-count tokenizer) 0))
      tokenizer)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TOKENIZER.LISP.48")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ansi-common-lisp; Base: 10; Package: (tk1 :use (future-common-lisp)); -*-")

(defun %token-equal (string1 start1 end1 string2 start2 end2 mask)
  (declare (fixnum start1 end1 start2 end2))
  (cond ((eql (the fixnum (- end1 start1)) (the fixnum (- end2 start2)))
         (%%token-equal string1 start1 end1 string2 start2 end2 mask))
        (t nil)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TOKENIZER.LISP.48")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ansi-common-lisp; Base: 10; Package: (tk1 :use (future-common-lisp)); -*-")

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

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.909")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(define-url-export-types
  (:xml-file :xml (:text :xml) :copy-mode #.+standard-text-copy-mode+ :data-type :html
	     :alternate-extensions (:rdf :daml)))

