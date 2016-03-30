;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.134
;;; Reason: Remove function HTTP::SIGNAL-MIME-MULTIPART-BLOCK-MAXIMUM-SIZE-EXCEEDED: -
;;; Function HTTP::SIGNAL-MIME-MULTIPART-BLOCK-MAXIMUM-SIZE-EXCEEDED:  redefine as function.
;;; Function (CLOS:METHOD HTTP::MIME-STREAM-DECODE-UNTIL-BOUNDARY (T T T (EQL :TEXT))):  update.
;;; Function (CLOS:METHOD HTTP::MIME-STREAM-DECODE-UNTIL-BOUNDARY (T T T (EQL :BINARY))):  update.
;;; Function WWW-UTILS::%MIME-STREAM-BINARY-DECODE-UNTIL-BOUNDARY:  specalized decoder.
;;; Function (CLOS:METHOD WWW-UTILS::MIME-STREAM-DECODE-UNTIL-BOUNDARY (TCP::TCP-MODAL-HTTP-STREAM SI:BINARY-STREAM T (EQL :BINARY))):  interface.
;;; Function WWW-UTILS::%MIME-STREAM-ASCII-DECODE-UNTIL-BOUNDARY:  specialized decoder.
;;; Function (CLOS:METHOD WWW-UTILS::MIME-STREAM-DECODE-UNTIL-BOUNDARY (TCP::TCP-MODAL-HTTP-STREAM SI:CHARACTER-STREAM T (EQL :TEXT))):  interface
;;; Written by JCMa, 5/12/01 19:04:47
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.133,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
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
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Lock Simple Patches 1.0, Jcma 43,
;;; HTTP Proxy Server 6.22, HTTP Client Substrate 4.13, Statice Server 466.2,
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
  "HTTP:SERVER;UTILS.LISP.494"
  "HTTP:SERVER;UTILS.LISP.495"
  "HTTP:LISPM;SERVER;LISPM.LISP.496")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.494")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(SCL:FUNDEFINE 'SIGNAL-MIME-MULTIPART-BLOCK-MAXIMUM-SIZE-EXCEEDED)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.495")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defun signal-mime-multipart-block-maximum-size-exceeded (bytes maximum-bytes)
  (funcall *mime-multipart-block-maximum-size-exceeded-error-function* bytes maximum-bytes))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.495")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmethod mime-stream-decode-until-boundary (mime-stream to-stream boundary (mode (eql :text)) &optional buffer
							  &aux (max-bytes *mime-multipart-block-maximum-size*) (total-bytes 0))
  (declare (values last-block-p)
	   (integer total-bytes))
  (macrolet ((check-size-limit (n-chars total-bytes max-bytes &optional crlf-p)
	       `(when (and ,max-bytes (> (setq ,total-bytes (+ ,total-bytes (the fixnum ,n-chars)
							       ,@(when crlf-p `((if ,crlf-p 2 0)))))
					 (the integer ,max-bytes)))
		  (signal-mime-multipart-block-maximum-size-exceeded ,total-bytes ,max-bytes))))
    (with-text-stream (mime-stream :input)
      (loop with subsequent-line-p and  boundary-length = (length boundary)
	    doing (multiple-value-bind (line error-p delmiter length)
		      (read-delimited-line mime-stream '(#\Return #\Linefeed) nil buffer)
		    (when error-p
		      (return nil))
		    (multiple-value-bind (finish-p last-block-p)
			(mime-boundary-line-p line boundary length boundary-length)
		      (cond (finish-p (return (values last-block-p)))
			    ((member delmiter '(#\Return #\Linefeed))
			     (check-size-limit length total-bytes max-bytes subsequent-line-p)
			     (if subsequent-line-p (terpri to-stream) (setq subsequent-line-p t))
			     (write-string line to-stream :start 0 :end length))
			    (t (check-size-limit length total-bytes max-bytes subsequent-line-p)			     
			       (write-string line to-stream :start 0 :end length)))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.495")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmethod mime-stream-decode-until-boundary (mime-stream to-stream boundary (mode (eql :binary)) &optional buffer
							  &aux (max-bytes *mime-multipart-block-maximum-size*) (total-bytes 0))
  (declare (values last-block-p)
	   (ignore buffer)
	   (integer total-bytes))
  (macrolet ((binary-char-code (char)
	       (typecase char
		 (#.*standard-character-type* #+Genera (si:ascii-code char) #-Genera (char-code char))
		 (t #+Genera `(si:ascii-code ,char) #-Genera `(char-code ,char)))))
    (labels ((fill-boundary-array (array boundary boundary-length)
	       (with-fast-array-references ((array array vector)
					    (boundary boundary string))
		 (loop initially (setf (aref array 0) (binary-char-code #\Return)
				       (aref array 1) (binary-char-code #\Linefeed) )
		       for i fixnum below boundary-length
		       do (setf (aref array (+ 4 i)) (char-code (aref boundary i))))))
	     (protected-write-byte (byte stream)
	       (and max-bytes
		    (> (incf total-bytes) (the integer max-bytes))
		    (signal-mime-multipart-block-maximum-size-exceeded total-bytes max-bytes))
	       (write-byte byte stream))
	     (write-array-portion (to-stream array n &rest extra-bytes)
	       (declare (dynamic-extent extra-bytes))
	       (with-fast-array-references ((array array vector))
		 (loop for idx fixnum upfrom 0 below n
		       do (protected-write-byte (aref array idx) to-stream)))
	       (dolist (byte extra-bytes)
		 (protected-write-byte byte to-stream)))
	     (check-for-mime-boundry (mime-stream to-stream boundary-array boundary-array-length)
	       (let ((last 1))
		 ;; start with 1 because the CR matched to get us here!
		 (with-fast-array-references ((boundary-array boundary-array vector))
		   (loop for idx fixnum upfrom 1 below boundary-array-length
			 for next-byte = (read-byte mime-stream)
			 while (eql next-byte (aref boundary-array idx))
			 do (setq last idx)
			 finally (when (< last (1- boundary-array-length))
				   (write-array-portion to-stream boundary-array last next-byte)
				   (return-from check-for-mime-boundry nil))))
		 (let ((byte1 (read-byte mime-stream))
		       (byte2 (read-byte mime-stream)) )
		   (cond ((and (= byte1 (binary-char-code #\-)) (= byte2 (binary-char-code #\-)))
			  (setq byte1 (read-byte mime-stream)
				byte2 (read-byte mime-stream))
			  (cond ((and (eql byte1 (binary-char-code #\Return)) (eql byte2 (binary-char-code #\Linefeed)))
				 (return-from mime-stream-decode-until-boundary t))
				(t (write-array-portion to-stream boundary-array boundary-array-length
							(binary-char-code #\-) (binary-char-code #\-) byte1 byte2))))
			 ((and (eql byte1 (binary-char-code #\Return)) (eql byte2 (binary-char-code #\Linefeed)))
			  (return-from mime-stream-decode-until-boundary nil))
			 (t (write-array-portion to-stream boundary-array boundary-array-length byte1 byte2)))))))
      (declare (inline fill-boundary-array)
	       (dynamic-extent #'protected-write-byte))
      (let* ((boundary-length (length boundary) )
	     (boundary-array-length (+ 4 boundary-length)) ;; len + CR + LF + 2 #\-'s
	     (boundary-array  (make-array boundary-array-length :initial-element #.(char-code #\-))))
	(declare (dynamic-extent boundary-array)
		 (fixnum boundary-length boundary-array-length))
	(with-binary-stream (mime-stream :input)
	  (loop initially (fill-boundary-array boundary-array boundary boundary-length)
		for byte = (read-byte mime-stream)
		do (if (eql byte (binary-char-code #\Return))
		       (check-for-mime-boundry mime-stream to-stream boundary-array boundary-array-length)
		       (protected-write-byte byte to-stream))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.496")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defun %mime-stream-binary-decode-until-boundary (mime-stream to-stream boundary
							      &aux (max-bytes http::*mime-multipart-block-maximum-size*) (total-bytes 0))
  (declare (values last-block-p))
  (labels ((fill-boundary-array (array array-length boundary boundary-length)
	     (loop initially (setf (aref array 0) #.(si:ascii-code #\Return)
				   (aref array 1) #.(si:ascii-code #\Linefeed) )
		   for idx upfrom 0 below boundary-length
		   do (setf (aref array (+ 4 idx)) (si:ascii-code (aref boundary idx)))))	; 4 = CR + LF + - + -
	   (send-buffer (stream buffer start end)
	     (unless (= start end)
	       (and max-bytes
		    (> (incf total-bytes) max-bytes)
		    (http::signal-mime-multipart-block-maximum-size-exceeded total-bytes max-bytes))
	       (scl:send stream :string-out buffer start end)))
	   (check-boundary (input-buffer start end boundary-array boundary-start boundary-end &optional last-byte &aux index)
	     (declare (values boundary-match-p last-block-p partial-match next-index residual))
	     (macrolet ((check-byte (absolute-index char boundary-index input-index &optional residual)
			  `(let ((boundary-offset (- ,boundary-index boundary-end)))
			     (cond ((< -1 boundary-offset ,absolute-index)	;true until we get to the current clause 
				    ,(if (zerop absolute-index) `(= ,(si:ascii-code char) ,residual) t))	;first byte distinguishes cases
				   ((= ,absolute-index boundary-offset)
				    (cond ((< ,input-index end)
					   (cond ((= (aref input-buffer ,input-index) ,(si:ascii-code char))
						  (incf ,input-index)
						  (incf ,boundary-index)
						  t)
						 (t ,(if (zerop absolute-index)
							 nil
							 `(return-from check-boundary
							    (values nil nil nil ,boundary-index ',(mapcar #'si:ascii-code residual)))))))
					  (t (return-from check-boundary (values nil nil ,boundary-index ,input-index)))))
				   (t (error "MIME boundary index was ~D, which is oustide the range,~D to ~D"
					     ,boundary-index boundary-end (+ boundary-end 4)))))))
	       (with-fast-array-references ((boundary-array boundary-array vector) (input-buffer input-buffer vector))
		 (cond ((< -1 boundary-start boundary-end)	;match the boundary array
			(loop for idx1 upfrom boundary-start
			      for idx2 upfrom start
			      while (and (< idx1 boundary-end) (< idx2 end) (= (aref boundary-array idx1) (aref input-buffer idx2)))
			      finally (cond ((< idx1 boundary-end)
					     (return-from check-boundary (if (= idx2 end) (values nil nil idx1 idx2) (values nil nil nil start))))
					    (t (setq boundary-start idx1	;prepare for post boundary matching
						     index idx2)))))	;maintain buffer index
		       (t (setq index start)))	;partial match after the boundary array
		 ;; Match the trailing characters
		 (cond ((check-byte 0 #\- boundary-start index last-byte)
			(if (and (check-byte 1 #\- boundary-start index (#\-))
				 (check-byte 2 #\Return boundary-start index (#\- #\-))
				 (check-byte 3 #\Linefeed boundary-start index (#\- #\- #\Return)))
			    (values t t nil index)
			    (error "This should never happen.")))
		       ((and (check-byte 0 #\Return boundary-start index last-byte)
			     (check-byte 1 #\Linefeed boundary-start index (#\Return)))
			(values t nil nil index))
		       (t (values nil nil nil index)))))))
    (declare (inline fill-boundary-array)
	     (dynamic-extent #'send-buffer))
    (let* ((boundary-length (length boundary) )
	   (boundary-end (+ 4 boundary-length)) ;; len + CR + LF + 2 #\-'s
	   (boundary-array  (make-array boundary-end :initial-element #.(char-code #\-))))
      (declare (dynamic-extent boundary-array))
      (with-fast-array-references ((boundary boundary string) (boundary-array boundary-array vector))
	(loop initially (fill-boundary-array boundary-array boundary-end boundary boundary-length)
	      with boundary-start = 0 and boundary-last-byte
	      doing (multiple-value-bind (input-buffer offset limit)
			(scl:send mime-stream :read-input-buffer t)
		      (unless (zerop boundary-start)	;Partial match case
			(multiple-value-bind (boundary-match-p last-block-p partial-match next-index residual)
			    (check-boundary input-buffer offset limit boundary-array boundary-start boundary-end boundary-last-byte)
			  (cond (boundary-match-p
				 (scl:send mime-stream :advance-input-buffer next-index)
				 (return-from %mime-stream-binary-decode-until-boundary last-block-p))
				(partial-match ;; Handle small buffer case
				 (setq boundary-start partial-match
				       boundary-last-byte (car residual)))
				(t (send-buffer to-stream boundary-array 0 boundary-start)
				   (dolist (byte residual)
				     (scl:send to-stream :tyo byte))
				   (setq boundary-start 0
					 boundary-last-byte nil)))))
		      (when (zerop boundary-start)	;Normal scan case
			(with-fast-array-references ((input-buffer input-buffer vector))
			  (loop for idx upfrom offset below limit
				for byte = (aref input-buffer idx)
				when (= byte #.(si:ascii-code #\Return))
				  do (multiple-value-bind (boundary-match-p last-block-p partial-match next-index residual)
					 (check-boundary input-buffer (1+ idx) limit boundary-array 1 boundary-end)	;set for next character
				       (cond (boundary-match-p
					      (send-buffer to-stream input-buffer offset idx)	;send any pending data
					      (scl:send mime-stream :advance-input-buffer next-index)
					      (return-from %mime-stream-binary-decode-until-boundary last-block-p))
					     (partial-match
					      (setq boundary-start partial-match
						    boundary-last-byte (car residual))
					      (loop-finish))))	;send any pending data
				finally (send-buffer to-stream input-buffer offset idx)
					(scl:send mime-stream :advance-input-buffer))))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.496")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defmethod mime-stream-decode-until-boundary ((mime-stream tcp::tcp-modal-http-stream) (to-stream si:binary-stream)
					      boundary (mode (eql :binary)) &optional buffer)
  (declare (values last-block-p)
	   (ignore buffer))
  (with-binary-stream (mime-stream :input)
    (%mime-stream-binary-decode-until-boundary mime-stream to-stream boundary)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.496")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defun %mime-stream-ascii-decode-until-boundary (mime-stream to-stream boundary
							     &aux (max-bytes http::*mime-multipart-block-maximum-size*) (total-bytes 0))
  (declare (values last-block-p))
  (macrolet ((checking-headroom ((index limit headroom) &body body)
	       `(cond ((< (+ ,index ,headroom) ,limit) ,@body)
		      (t (error "MIME Suffix wraps around more than one buffer.")))))
    (labels ((fill-boundary-string (string boundary boundary-length)
	       (loop initially (setf (aref string 0) #\-
				     (aref string 1) #\-)
		     for idx upfrom 0 below boundary-length
		     do (setf (aref string (+ 2 idx)) (aref boundary idx))))	; 2 = - + -)
	     (send-buffer (stream buffer start end)
	       (unless (= start end)
		 (and max-bytes
		      (> (incf total-bytes) max-bytes)
		      (http::signal-mime-multipart-block-maximum-size-exceeded total-bytes max-bytes))
		 (scl:send stream :string-out buffer start end))))
      (declare (inline fill-boundary-string send-buffer))
      (let* ((last-lock-pattern #.(coerce '(#\- #\- #\Return) 'string))
	     (boundary-length (length boundary))
	     (boundary-string-end (+ 2 boundary-length)) ;; len + 2 #\-'s
	     (boundary-string  (make-string boundary-string-end)))
	(declare (dynamic-extent boundary-string))
	(loop initially (fill-boundary-string boundary-string boundary boundary-length)
	      with match-index = 0 and dash-index = 0
	      doing (multiple-value-bind (input-buffer offset limit)
			(scl:send mime-stream :read-input-buffer t)
		      (with-fast-array-references ((input-buffer input-buffer string)
						   (boundary-string boundary-string string)
						   (last-lock-pattern last-lock-pattern string))
			(loop for idx upfrom offset below limit
			      for char = (aref input-buffer idx)
			      when match-index
				do (cond ((= match-index boundary-string-end)	;patterned matched, check suffix
					  (ecase dash-index	;currently assumes that the new buffer will have room for suffix testing, ie at least 4 chars
					    (0
					      (cond ((eql #\Return char)
						     (scl:send mime-stream :advance-input-buffer (1+ idx))
						     (return-from %mime-stream-ascii-decode-until-boundary nil))
						    ((and (eql #\- char)
							  (checking-headroom (idx limit 2)
									     (sys:%string-equal last-lock-pattern 1 input-buffer (1+ idx) 2)))
						     (scl:send mime-stream :advance-input-buffer (+ 3 idx))
						     (return-from %mime-stream-ascii-decode-until-boundary t))
						    (t (send-buffer to-stream boundary-string 0 match-index)
						       (setq match-index nil))))
					    (1 (cond ((and (eql #\- char) (checking-headroom (idx limit 1)
											     (eql #\Return (aref input-buffer (1+ idx)))))
						      (scl:send mime-stream :advance-input-buffer (+ 2 idx))
						      (return-from %mime-stream-ascii-decode-until-boundary t))
						     (t (send-buffer to-stream boundary-string 0 match-index)
							(send-buffer to-stream last-lock-pattern 0 dash-index)
							(setq match-index nil))))
					    (2 (cond ((eql #\Return char)
						      (scl:send mime-stream :advance-input-buffer (1+ idx))
						      (return-from %mime-stream-ascii-decode-until-boundary t))
						     (t (send-buffer to-stream boundary-string 0 match-index)
							(send-buffer to-stream last-lock-pattern 0 dash-index)
							(setq match-index nil))))))
					 ((eql char (aref boundary-string match-index))
					  (incf match-index)
					  (when (= match-index boundary-string-end)
					    (cond ((< (1+ idx) limit)
						   (cond ((eql #\Return (aref input-buffer (1+ idx)))
							  (scl:send mime-stream :advance-input-buffer (+ 2 idx))
							  (return-from %mime-stream-ascii-decode-until-boundary nil))
							 ((eql #\- (aref input-buffer (1+ idx)))
							  (cond ((< (+ 3 idx) limit)
								 (cond ((sys:%string-equal last-lock-pattern 1 input-buffer (+ 2 idx) 2)
									(scl:send mime-stream :advance-input-buffer (+ 4 idx))
									(return-from %mime-stream-ascii-decode-until-boundary t))
								       (t (setq match-index nil))))
								((< (+ 2 idx) limit)
								 (cond ((eql #\- (aref input-buffer (+ 2 idx)))
									(setq dash-index 2)
									(scl:send mime-stream :advance-input-buffer (+ 3 idx))
									(return))
								       (t (setq match-index nil))))
								(t (setq dash-index 1)
								   (scl:send mime-stream :advance-input-buffer (+ 2 idx))
								   (return))))
							 (t (setq match-index nil))))
						  (t (setq dash-index 0)
						     (return)))))
					 (t (setq match-index nil)))
			      until (member char '(#\Return #\Linefeed) :test #'eql)
			      finally (unless match-index	;always send buffer unless we're matching
					(send-buffer to-stream input-buffer offset limit)
					(setq match-index 0))
				      (cond ((= idx limit)
					     (scl:send mime-stream :advance-input-buffer))
					    (t (scl:send mime-stream :advance-input-buffer (1+ idx))))))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.496")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defmethod mime-stream-decode-until-boundary ((mime-stream tcp::tcp-modal-http-stream) (to-stream si:character-stream)
					      boundary (mode (eql :text)) &optional buffer)
  (declare (values last-block-p)
	   (ignore buffer))
  (with-text-stream (mime-stream :input)
    (%mime-stream-ascii-decode-until-boundary mime-stream to-stream boundary)))
