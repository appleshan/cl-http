;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: tcp; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.62
;;; Reason: Fix chunk encoding error where insufficient headroom for chunking overhead may
;;; occur when genera returns buffers with insufficient free space. Detect the
;;; error should it somehow manage to occur again before cascading effects occur.
;;; 
;;; Function (FLAVOR:NCWHOPPER :SETUP-NEW-OUTPUT-BUFFER TCP::CHUNK-TRANSFER-ENCODING-OUTPUT-STREAM-MIXIN):  
;;; Fix insufficient headroom error.
;;; Variable TCP::*CHUNK-ENCODING-MINIMUM-BUFFER-FREE-SPACE*:  
;;; New constant to control minimum chunk body size sent in a packet.
;;; Function (FLAVOR:METHOD TCP::NOTE-FIRST-CHUNK TCP::CHUNK-TRANSFER-ENCODING-OUTPUT-STREAM-MIXIN):  
;;; Ensure that there is sufficient buffer headroom.
;;; Function (FLAVOR:NCWHOPPER :SEND-OUTPUT-BUFFER TCP::CHUNK-TRANSFER-ENCODING-OUTPUT-STREAM-MIXIN):  
;;; Error on zero length chunks, which should never occur.
;;; Function (FLAVOR:METHOD TCP::NOTE-LAST-CHUNK TCP::CHUNK-TRANSFER-ENCODING-OUTPUT-STREAM-MIXIN):  
;;; Ensure headroom for last chunk overhead.
;;; Written by JCMa, 8/17/00 16:18:26
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.61,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 42, HTTP Proxy Server 5.18,
;;; HTTP Client Substrate 3.15, W4 Constraint-Guide Web Walker 45.2,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.11,
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
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7),
;;; Ansi common lisp as synonym patch (from W:>reti>ansi-common-lisp-as-synonym-patch.lisp.9).

(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.165"
  "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.166"
  "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.167"
  "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.169")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.165")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

(defconstant *chunk-encoding-minimum-buffer-free-space* 16
  "Controls the minimum free space in a buffer for a chunk body.")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.165")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

(proclaim '(inline chunk-encoding-sufficient-headroom-p))

(defun chunk-encoding-sufficient-headroom-p (output-index output-limit)
  (> output-limit
     (+ output-index
	#.(+ *default-chunk-size-vector-length*	;chunk size header
	      2					;CRLF
	      *chunk-encoding-minimum-buffer-free-space*	;chunk body
	      2))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.165")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

(defwhopper (:setup-new-output-buffer chunk-transfer-encoding-output-stream-mixin) ()
  (cond (chunk-output
	 ;;Force new packets to be gotten when chunking to avoid headroom errors 8/17/2000 -- JCMa.
	 (let ((*tcp-segment-combination-disabled* t))	
	   (prog1 (continue-whopper)
		  (setq chunk-start si:stream-output-index)
		  (%note-chunk-body-start))))
	(t (continue-whopper))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.166")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

(defun-in-flavor (%note-chunk-body-start chunk-transfer-encoding-output-stream-mixin) ()
  #+ignore (format *trace-output* "~&~'bChunk-Start:~ ~'bBuffer~ ~S ~'bIndex~ ~4D ~'bLimit~ ~D "
		   si:stream-output-buffer si:stream-output-index si:stream-output-limit)
  (unless (chunk-encoding-sufficient-headroom-p si:stream-output-index si:stream-output-limit)
    (error "Chunk Encoding Error: Insufficient Headroom in buffer (Index: D Limit: ~D)"
	   si:stream-output-index si:stream-output-limit))
  (setq content-length-start si:stream-output-index)
  (write-chunk-size-header si:stream-output-buffer
                           si:stream-output-index (incf si:stream-output-index  *default-chunk-size-vector-length*))
  (write-8-bit-crlf si:stream-output-buffer si:stream-output-index (incf si:stream-output-index 2))
  (setq chunk-end si:stream-output-limit
        si:stream-output-limit (- si:stream-output-limit 2)
        chunk-body-start si:stream-output-index))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.166")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

(defmethod (note-first-chunk chunk-transfer-encoding-output-stream-mixin) ()
  (unless (chunk-encoding-sufficient-headroom-p si:stream-output-index si:stream-output-limit)
    (send self :setup-new-output-buffer))	;already in chunk encoding mode when called
  (%note-chunk-body-start))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.167")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

(defmethod (note-last-chunk chunk-transfer-encoding-output-stream-mixin) (&optional footers-plist)
  (flet ((ensure-chunk-headroom (amount)
	   (unless (and si:stream-output-buffer (< amount (- si:stream-output-limit si:stream-output-index)))
	     ;;Force new packets to be gotten when chunking to avoid headroom
	     ;;errors. Because chunking has been turned off, we can't rely on
	     ;;:setup-new-output-buffer to bind this variable.  8/17/2000 -- JCMa.
	     (let ((*tcp-segment-combination-disabled* t))	
	       (send self :setup-new-output-buffer)))))
    (declare (inline ensure-chunk-headroom))
    ;; Prevent chunked output buffer setup from repeating and prevent send output from reinserting chunk args
    (setq chunk-output nil)
    (cond ;; if no data written to body yet, convert to end header
      ((= chunk-body-start si:stream-output-index)
       (ensure-chunk-headroom 3)
       (setf (aref si:stream-output-buffer si:stream-output-index) #.(si:ascii-code #\0))
       (write-8-bit-crlf si:stream-output-buffer (incf si:stream-output-index 1) (incf si:stream-output-index 2)))
      (t (multiple-value-setq (si:stream-output-index)
	   (%note-body-end si:stream-output-buffer si:stream-output-index))
	 (ensure-chunk-headroom 3)		;make sure we have room for termination chunk
	 (setf (aref si:stream-output-buffer si:stream-output-index) #.(si:ascii-code #\0))
	 (incf si:stream-output-index)
	 (write-8-bit-crlf si:stream-output-buffer si:stream-output-index (incf si:stream-output-index 2))))
    ;; ensure that we're in ascii mode before writing footers
    (ascii-output-mode self)
    ;; write the footers
    (http::write-headers self footers-plist t)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.169")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

(defwhopper (:send-output-buffer chunk-transfer-encoding-output-stream-mixin) (seg limit explicit)
  (cond ((null chunk-output)
         (continue-whopper seg si:stream-output-index explicit))
	((zerop (- limit chunk-body-start))     ;don't send zero sized chunks
	 ;; Must error because zero-sized chunks denote the end of chunked
	 ;; encoding and continue whopper must be called to avoid Lispm buffer
	 ;; synchronization error.
	 (error "Chunk Encoding Error: Attempt to send a zero length HTTP chunk."))
        (t (multiple-value-setq (si:stream-output-index)
             (%note-body-end seg limit))
	   (continue-whopper seg si:stream-output-index explicit))))

