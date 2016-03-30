;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.66
;;; Reason: Fixes to low frequency bugs reading ascii translated chunked input.
;;; 
;;; Function HTTP::%PROCESS-REQUEST:  clean up error throwing through a tag that has already been thrown through.
;;; Flavor TCP::CHUNK-TRANSFER-DECODING-INPUT-STREAM-MIXIN:  require basic-tcp-stream
;;; Function (FLAVOR:NCWHOPPER :NEXT-INPUT-BUFFER TCP::MODAL-ASCII-TRANSLATING-BUFFERED-INPUT-STREAM-MIXIN):  -
;;; Function (DEFUN-IN-FLAVOR TCP::%SUSPEND-ASCII-INPUT-TRANSLATION TCP::MODAL-ASCII-TRANSLATING-BUFFERED-INPUT-STREAM-MIXIN):  -
;;; Function (DEFUN-IN-FLAVOR TCP::%RESUME-ASCII-INPUT-TRANSLATION TCP::MODAL-ASCII-TRANSLATING-BUFFERED-INPUT-STREAM-MIXIN):  -
;;; Function (DEFUN-IN-FLAVOR TCP::%INHIBITING-ASCII-INPUT-TRANSLATION TCP::MODAL-ASCII-TRANSLATING-BUFFERED-INPUT-STREAM-MIXIN):  -
;;; Function (FLAVOR:NCWHOPPER :NEXT-INPUT-BUFFER TCP::CHUNK-TRANSFER-DECODING-INPUT-STREAM-MIXIN):  update
;;; Function (FLAVOR:NCWHOPPER :SETUP-NEXT-INPUT-BUFFER TCP::CHUNK-TRANSFER-DECODING-INPUT-STREAM-MIXIN):  update.
;;; Function (FLAVOR:METHOD TCP::CHUNK-TRANSFER-DECODING-MODE TCP::CHUNK-TRANSFER-DECODING-INPUT-STREAM-MIXIN):  update.
;;; Function (FLAVOR:NCWHOPPER :DISCARD-CURRENT-INPUT-BUFFER TCP::MODAL-ASCII-TRANSLATING-BUFFERED-INPUT-STREAM-MIXIN):  
;;; Make sure LF following CR at end of ascii translation window in order to assure proper reset of at-cr-flag.
;;; Function (FLAVOR:METHOD TCP::NOTE-LAST-CHUNK TCP::CHUNK-TRANSFER-ENCODING-OUTPUT-STREAM-MIXIN):  don't error on null si:stream-output-buffer
;;; Remove function (FLAVOR:NCWHOPPER :READ-INPUT-BUFFER TCP::CHUNK-TRANSFER-DECODING-INPUT-STREAM-MIXIN): undefine.
;;; 
;;; Function WWW-UTILS:PARSE-HOST:  catch neti:object-not-found-in-namespace during calls to net:parse-host 
;;; 				and resignal as unknown-host-name.
;;; DEFINE-CONDITION HTTP::PROXY-UNRESOLVABLE-DOMAIN-NAME:  provide a more descriptive reason.
;;; Function URL:COERCE-URL-STRING:  fix bug.
;;; 
;;; Fix handling of persistent connections for http 1.0 clients and ensure that
;;; content-length is sent with CRLF variants of text files.
;;; 
;;; Remove function HTTP::WRITE-SERVER-PERSISTENT-CONNECTION-HEADERS: undefine.
;;; Function HTTP::WRITE-SERVER-PERSISTENT-CONNECTION-HEADERS:  -
;;; Function (CLOS:METHOD HTTP::WRITE-SERVER-PERSISTENT-CONNECTION-HEADERS (HTTP::BASIC-SERVER-MIXIN T T)):  remove content-type arg from consideration.
;;; Function HTTP::WRITING-FROM-CRLF-DATA-SOURCE:  new.
;;; Function HTTP::%WRITE-DOCUMENT-CRLF-FROM-PATHNAME:  use writing-from-crlf-data-source.
;;; Function HTTP::%WRITE-DOCUMENT-CRLF-RANGE-FROM-PATHNAME:  ditto.
;;; Function HTTP::%WRITE-DOCUMENT-MIME-HEADERS:  use the new regime.
;;; Function (CLOS:METHOD HTTP::PERSISTENT-CONNECTION-P (HTTP::BASIC-SERVER-MIXIN)):  handle :proxy-connection.
;;; Written by JCMa, 8/30/00 21:21:18
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.65,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, HTTP Proxy Server 5.19,
;;; HTTP Client Substrate 3.15, HTTP Client 49.8, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.11, DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x976 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number -2141189585,
;;; Domain Fixes (from CML:MAILER;DOMAIN-FIXES.LISP.33),
;;; Don't force in the mail-x host (from CML:MAILER;MAILBOX-FORMAT.LISP.24),
;;; Make Mailer More Robust (from CML:MAILER;MAILER-FIXES.LISP.15),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add CLIM presentation and text style format directives. (from FV:SCLC;FORMAT.LISP.20),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; Make update schema work on set-value attributes with accessor names (from CML:LISPM;STATICE-SET-VALUED-UPDATE.LISP.1),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.107),
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48).

;;; Patch file for CL-HTTP version 70.66
;;; Written by JCMa, 9/01/00 13:20:10
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.66,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, HTTP Proxy Server 5.19,
;;; HTTP Client Substrate 3.15, HTTP Client 49.8, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, Jcma 42, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.11, DEC OSF/1 V4.0 (Rev. 110),
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
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48).


(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.177"
  "HTTP:SERVER;SERVER.LISP.845"
  "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.183"
  "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.180"
  "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.182"
  "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.186"
  "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.187"
  "HTTP:SERVER;SERVER.LISP.847"
  "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.188"
  "HTTP:LISPM;SERVER;LISPM.LISP.467"
  "HTTP:SERVER;HTTP-CONDITIONS.LISP.177"
  "HTTP:SERVER;URL.LISP.407"
  "HTTP:SERVER;UTILS.LISP.461"
  "HTTP:SERVER;SERVER.LISP.849"
  "HTTP:SERVER;SERVER.LISP.850"
  "HTTP:SERVER;HEADERS.LISP.466")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.177")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

;;;------------------------------------------------------------------- 
;;;
;;; CHUNKED TRANSFER DECODING
;;;

(defflavor chunk-transfer-decoding-input-stream-mixin
        ((chunked-input nil)                    ;whether output input-chunking is on or off
         (old-mode nil)                         ;previous input mode
         (input-content-length 0)               ;total content length of input
         (input-chunk-size 0)                   ;total length of chunk
         (input-chunk-size-vector nil)          ;resource for reading chunk size
         (input-chunk-args-vector nil)          ;resource for reading chunk size args
         (input-scan-start 0)                   ;position where buffer scan starts
         (input-scan-end 0)                     ;position where buffer scan ends
         (input-scan-length 0)                  ;length of buffer scan
         (input-chunk-content-length 0)         ;current content length
         (input-chunk-crosses-buffer-p nil)     ;whether chunk crosses packet boundaries
         (input-buffer-limit 0)                 ;limit position for input buffer
         (input-chunks-received 0))		;number of chunks received
        ()
  :abstract-flavor
  (:required-flavors si:buffered-input-stream basic-tcp-stream tcp-output-stream-mixin modal-ascii-translating-buffered-input-stream-mixin)
  (:required-methods :tyo)
  (:documentation :mixin "An input stream that provides chunked transfer decoding input on an HTTP stream."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.177")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

;;; Because ASCII translation occurs in a :next-input-buffer whopper, chunking
;;; must provide the correct window before that happens lest translation
;;; change a CR delimiting the end of the chunk. This pathology occurs only
;;; when chunks cross packet boundaries, and is especially frequent when text
;;; is shipped with only LF boundaries.   4/22/97 -- JCMa.
(defwhopper (:next-input-buffer chunk-transfer-decoding-input-stream-mixin) (&optional no-wait-p &aux at-eof)
  (cond ((null chunked-input)
         (continue-whopper no-wait-p))
        (input-chunk-crosses-buffer-p
         (multiple-value-setq (si:stream-input-buffer si:stream-input-index si:stream-input-limit at-eof)
           (continue-whopper nil))		;always wait for data, or synchronization will be lost.   5/28/99 -- JCMa.
	 (when at-eof
	   (if tcb
	       (error 'tcp-connection-no-more-data :connection tcb :reason "closing during an HTTP chunked read")
	       (error 'tcp-stream-closed :attempt "HTTP chunked read from" :stream self)))
	 (unless si:stream-input-buffer
	   (error "No data available while reading HTTP chunked input."))
         (%note-chunk-continue)
         ;; Update by withheld buffer window
         (incf (bytes-received self) (- input-buffer-limit input-scan-end))
         ;; Always return the binary buffer because :SETUP-NEXT-INPUT-BUFFER binds it.
         (values si:stream-input-buffer si:stream-input-index input-scan-end at-eof))
        (t (continue-whopper no-wait-p))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.845")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(define-url-export-types
  (:mp3-audio :mp3 (:audio :mp3) :copy-mode :binary)
  )

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.180")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

(defun-in-flavor (%suspend-ascii-input-translation modal-ascii-translating-buffered-input-stream-mixin) ()
  (when (and si:stream-input-index at-end (< si:stream-input-index at-end))
    (untranslate-window si:stream-input-buffer at-string si:stream-input-index at-end))
  (setq input-mode :ascii-suspended))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.188")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

(defun-in-flavor (%resume-ascii-input-translation modal-ascii-translating-buffered-input-stream-mixin) (old-mode)
  (setq input-mode old-mode)
  (when si:stream-input-buffer
    (do-ascii-translation si:stream-input-index si:stream-input-limit)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.183")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

(defmacro-in-flavor (%inhibiting-ascii-input-translation modal-ascii-translating-buffered-input-stream-mixin) (&body body)
  `(let ((old-mode input-mode))
     (case old-mode
       ((:ascii :ascii-crlf)
	(unwind-protect
	    (progn (%suspend-ascii-input-translation)
		   ,@body)
	  (%resume-ascii-input-translation old-mode)))
       (t ,@body))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.180")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

(defwhopper (:setup-next-input-buffer chunk-transfer-decoding-input-stream-mixin) (no-hang-p eof)
  (cond ((null chunked-input)
         (continue-whopper no-hang-p eof))
        (input-chunk-crosses-buffer-p
         (with-chunk-transfer-decoding-traced
           (format *trace-output* "~&~'bCross Packet Boundary~"))
         (continue-whopper no-hang-p eof))
        (t (%inhibiting-ascii-input-translation
	     (%note-chunk-end)
	     (%note-chunk-start))
           (values si:stream-input-buffer nil))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.180")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

(defmethod (chunk-transfer-decoding-mode chunk-transfer-decoding-input-stream-mixin) ()
  (when (and at-cr-flag si:stream-input-buffer (< si:stream-input-index si:stream-input-limit))
    (clear-at-cr-flag si:stream-input-index))
  ;; the chunk may start in the next TCP packet after the headers.
  (unless (and si:stream-input-buffer (< si:stream-input-index si:stream-input-limit))
    (send self :setup-next-input-buffer nil t)
    ;; clear any in the next buffer
    (when at-cr-flag
      (clear-at-cr-flag si:stream-input-index)))
  (setq chunked-input t
        input-content-length 0
        input-scan-length 0)
  (%inhibiting-ascii-input-translation
    (%note-chunk-start)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.180")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

;; inline when debugged.
;; Some overlap in translation windows between :next-input and read-input-buffer.  4/22/97 -- JCMa.
(defun-in-flavor (do-ascii-translation modal-ascii-translating-buffered-input-stream-mixin) (start end)
  (check-type start integer)
  (check-type end integer)
  (let* ((ascii-input-mode-p (eql input-mode :ascii))
         (ascii-translation-p (or ascii-input-mode-p (eql input-mode :ascii-crlf))))
    (cond (ascii-translation-p
           ;; could be changed to happen once only in :next-input-buffer, if safe.
           (si:change-indirect-array at-string (si:array-type at-string) (list end) si:stream-input-buffer 0)
           (let ((buffer si:stream-input-buffer)
                 (string at-string))
             (declare (sys:array-register buffer string))
             (when at-cr-flag (clear-at-cr-flag start))
             (setq at-start start               ;move position forward
                   si:stream-input-index start)
             (loop for idx upfrom start below end
                   for ch = (aref buffer idx)
                   do (when (< ch #o040)        ; ascii space
                        (setf (aref string idx) (ascii-to-char ch))
                        (when (= ch #.(si:char-to-ascii #\Return))      ;ascii CR
                          (setq at-end (incf idx))
                          (when ascii-input-mode-p      ;will translate whole buffer in CRLF mode.
                            (setq at-cr-flag t)
                            (return)))
                        ;; UNIX and Windows systems often don't send CRLF.
                        (when (= ch #.(si:char-to-ascii #\Line-Feed))   ;ascii LF
                          (setq at-end (incf idx))
                          (when ascii-input-mode-p      ;will translate whole buffer in CRLF mode.
                            (return))))
                   finally (setq at-end end))
             #+ignore
             (format *trace-output* "~&~'bDo-ASCII-Translation:~ ~D ~D (~:C)~&" at-start at-end (aref string (1- at-end)))
             ))
          (at-cr-flag
           (clear-at-cr-flag si:stream-input-index))
          (t (setq si:stream-input-index start)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.180")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

(defwhopper (:next-input-buffer modal-ascii-translating-buffered-input-stream-mixin)
            (&optional no-wait-p &aux new-input-buffer-p)
  (unless si:stream-input-buffer
    (setq new-input-buffer-p t)
    (multiple-value-setq (si:stream-input-buffer si:stream-input-index si:stream-input-limit)
      (continue-whopper no-wait-p))
    (when si:stream-input-buffer
      (incf bytes-received (- si:stream-input-limit si:stream-input-index))))   ;one stop counting after chunking before translation
  ;; handle ASCII translations
  (when si:stream-input-buffer
    (case input-mode
      ((:ascii :ascii-crlf :ascii-suspended)
       (cond (new-input-buffer-p
              (setq at-start si:stream-input-index
                    at-end si:stream-input-limit)
              (unless at-string
                (unless (= 8 (si:array-element-byte-size si:stream-input-buffer))
                  (error "Underlying buffer has bad element size."))
                (setq at-string (make-array 2048. :type 'si:art-string :displaced-to si:stream-input-buffer)))
              ;; Set up the indirect array only once when new input buffers arrive.
              #+ignore(si:change-indirect-array at-string (si:array-type at-string)
					(list (array-total-size si:stream-input-buffer)) si:stream-input-buffer 0))
             (t (setq at-start (or at-end si:stream-input-index)
                      at-end si:stream-input-limit)))
       (case input-mode
	 (:ascii-suspended)			;Skip ASCII translation when suspended
	 ;; Perform ASCII translations
	 (t (do-ascii-translation at-start at-end))))))
  ;; always return the binary buffer because :SETUP-NEXT-INPUT-BUFFER binds it.
  (values si:stream-input-buffer si:stream-input-index si:stream-input-limit))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.180")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

(defwhopper (:read-input-buffer modal-ascii-translating-buffered-input-stream-mixin) (&optional eof no-hang-p)
  (macrolet ((handle-buffer-return (&body body)
               `(multiple-value-bind (buff index limit)
                    ,@body
                  (if buff
                      (values at-string si:stream-input-index at-end)
                      (values buff index limit)))))
    (ecase input-mode
      ((:ascii :ascii-crlf)
       (cond ((null si:stream-input-buffer)
              (handle-buffer-return
                (continue-whopper eof no-hang-p)))
             ((and at-end (< si:stream-input-index at-end))     ;more chars to read in current window
              (values (and si:stream-input-buffer at-string) si:stream-input-index at-end))
             ((and at-end (< at-end si:stream-input-limit))     ;get next window
              (do-ascii-translation at-end si:stream-input-limit)
              (values (and si:stream-input-buffer at-string) si:stream-input-index at-end))
             (t (handle-buffer-return
                  (continue-whopper eof no-hang-p)))))
      (:ascii-suspended
	(handle-buffer-return
	  (continue-whopper eof no-hang-p)))
      ((:binary :crlf) (continue-whopper eof no-hang-p)))))

		;return whether to continue reading requests from the stream
;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.182")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

;;; When closing, we want to really discard buffer all the way, advance state
;;; past anything intermediate, lose packets on close.  This really doesn't
;;; mean that the user hasn't read all the input.  There may be one character
;;; (#o012) and at-cr-flag so that :next-input-buffer wouldn't return it.
(defmethod (:close modal-ascii-translating-buffered-input-stream-mixin :before) (&optional ignore)
  (case input-mode
    ((:ascii :ascii-crlf)
     (when (and (variable-boundp si:stream-input-limit) si:stream-input-limit)
       (setq si:stream-input-index si:stream-input-limit
             at-end si:stream-input-limit
             at-cr-flag nil)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.186")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

;; Make sure that :CLOSE method remains synchronized with the logic here in
;; order to avoid packet leakage.   2/3/97 -- JCMa.
(defwhopper (:discard-current-input-buffer modal-ascii-translating-buffered-input-stream-mixin) ()
  ;; prevents ASCII translation windows from being thrown away by
  ;; advance-input buffer and set-up-new-input buffer.  1/29/97 -- JCMa.
  (cond ((and si:stream-input-buffer (member input-mode '(:ascii :ascii-crlf)) (< si:stream-input-index si:stream-input-limit))
         (cond ((and (< si:stream-input-index at-end)
                     (not (< (setq si:stream-input-index at-end) si:stream-input-limit)))
                (continue-whopper))
	       ;; If the last char is an LF, we can just continue discarding the buffer 8/30/2000 -- JCMa.
               ((and at-cr-flag
		     (= (1+ at-end) si:stream-input-limit)
		     (eq (aref si:stream-input-buffer at-end) #.(si:char-to-ascii #\linefeed)))
                (setq at-cr-flag nil)
                (continue-whopper))))
        (t (continue-whopper))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.187")
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
    (cond
      ((null si:stream-output-buffer)		;there is no stream buffer
       (ascii-output-mode self)			;clean up a little and return 8/31/2000 -- JCMa.
       (return-from note-last-chunk nil))
      ;; if no data written to body yet, convert to end header
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
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.847")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun %process-request (server stream)
  (declare (values persistent-connection-p)
	   (optimize (speed 3)))
  (labels ((preview-condition (condition)
	     (when *debug-server*
               (break (report-string condition)))
	     nil)
	   (handle-http-condition (condition stream server status-code)
             (case *debug-server*
               (:conditions (break (report-string condition))))
	     (setf (server-status server) status-code)
	     (when (close-connection-p condition) 
	       (setf (server-close-connection-p server) t))
	     (typecase condition
	       ((or network-error request-timeout))
	       (t (handler-case
		    (report-status condition stream)
		    (network-error () nil)
		    (error (err) (bug-report-error err) nil))))
	     (throw 'exit-http-server nil))	;closes connection
	   (handle-reportable-condition (condition)
	     (handle-http-condition condition stream server (status-code condition)))
	   (handle-parsing-error (error)
	     (handle-http-condition error stream server 400))
	   (report-error (error)
	     (typecase error
	       ((or http-condition network-error condition))
	       (t (bug-report-error error)))
	     nil))
    (declare (dynamic-extent #'handle-reportable-condition #'handle-parsing-error))

      (handler-bind
	((error #'preview-condition)		; MCL 4.1 loses when testing CONDITION here. -- JCMa 7/24/1997.
	 (reportable-condition #'handle-reportable-condition)
	 (url:parsing-error #'handle-parsing-error)
	 (error #'report-error))
	(catch 'exit-http-server
	  (multiple-value-bind (request eof delimiter)
	      (read-delimited-line stream '(#\Linefeed #\Return) t (%server-request server))
	    delimiter				;ignore
	    ;; http-version not set, but will default via server-http-version
	    (when eof
	      (error 'request-timeout :format-string "Client dropped connection while reading request line."))
	    (setf (%server-request server) request	;capture in case of growth
		  (server-request-time server) (get-universal-time))	;set the request time
	    (%execute-request server request stream))))
    ;; epilogue
    (let ((persistent-connection-p (and (server-persistent-connection-p server)	;set by positive predicate
					(not (server-close-connection-p server)))))	;errors may set this
      ;; Don't force output if there is incoming data: pipeline responses 
      (unless (and persistent-connection-p (http-input-data-available-p stream nil))
	(force-output stream))			;force output while deciding what to do next
      (incf (server-requests-completed server))	;count completed requests
      persistent-connection-p)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.188")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

(FUNDEFINE '(FLAVOR:NCWHOPPER :READ-INPUT-BUFFER CHUNK-TRANSFER-DECODING-INPUT-STREAM-MIXIN))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.467")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(define parse-host (host &optional no-error-p (inhibit-validity-checking-p neti:*inhibit-validity-checking*))
  "Parses HOST and returns a host object."
  (typecase host
    (net:host host)
    (string 
      (cond ((equalp host "localhost") (local-host))    ; RFC 1738 says defines this string   5/3/96 -- JCMa.
            ((url:numeric-hostname-p host)
             (let ((host-spec (concatenate 'string "INTERNET|" host)))
               (declare (dynamic-extent host-spec))
               (net:parse-host host-spec no-error-p)))
            (t (scl:condition-case-if (not no-error-p) (err)
		    (net:parse-host host no-error-p)
		  (neti:object-not-found-in-namespace (error 'unknown-host-name :name host))))))
    ;; feed it to parse-host to signal the error as necessary
    (t (unless no-error-p
         (net:parse-host host no-error-p (not inhibit-validity-checking-p))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.177")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition proxy-unresolvable-domain-name
		  (proxy-bad-gateway)
  ((reason :initform "Domain Name Unresolvable" :initarg :reason :reader http-reason)
   (close-connection-p :initform nil :initarg :close-connection :reader http-close-connection-p)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.407")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(define coerce-url-string (url &optional (escape-search-url-p *escape-search-urls*)
                               (downcase-p *downcase-url-strings*)
                               &aux s)
  "Returns a string for URL."
  (declare (notinline))
  (setq s (cond (escape-search-url-p
                 (etypecase url
                   (string (ensure-escaped-search-url url))
                   (url (name-string url))))
                (t (etypecase url
                     (string url)
		     (search-mixin (name-string-with-unescaped-search-suffix url))
                     (url (name-string url))))))
  (cond-every
    (downcase-p
      (setq s (string-downcase s))))
  s)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.461")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(eval-when (:compile-toplevel :execute :load-toplevel)
  #+(or Genera MCL LispWorks-UNIX)
  (pushnew :character-translation-skews-file-size *features*))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.461")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

#+character-translation-skews-file-size
(defvar *crlf-document-data* nil)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.461")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmacro writing-from-crlf-data-source (&body body)
  "Wrap this macro around CRLF data sources when serving text
in order to assure optimal use of content-length."
  #+character-translation-skews-file-size
  `(let ((*crlf-document-data* t)) . ,body)
  #-character-translation-skews-file-size
  `(progn . ,body))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.849")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun %write-document-crlf-from-pathname (url pathname content-type stream &optional charset last-modification version)
  (writing-from-crlf-data-source
    (with-data-cache (pathname)
		     (%%caching-write-binary-file data-cache url content-type stream charset last-modification version)
      (let ((crlf-pathname (ensure-crlf-canonical-file pathname)))
	(%%write-binary-file crlf-pathname url content-type stream charset last-modification version)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.849")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun %write-document-crlf-range-from-pathname (url pathname content-type stream start end 
                                                     &optional charset last-modification version)
  (writing-from-crlf-data-source
    (with-data-cache (pathname)
		     (%%caching-write-binary-range data-cache url content-type stream start end charset last-modification version)
      (let ((crlf-pathname (ensure-crlf-canonical-file pathname)))
	(%%write-binary-range url crlf-pathname content-type stream start end charset last-modification version)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.849")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod persistent-connection-p ((server basic-server-mixin) &optional inside-transaction-p)
  (with-slots (requests-completed) server
    (cond ((server-close-connection-p server) nil)
          (t (case (server-http-version server)
	       (:http/0.9 nil)
	       (:http/1.0
		 (let* ((headers (server-headers server))
			(connection (if (server-proxy-request-p server)
					;; :proxy-connection deprecated 1.0 extension supported by Netscape & IE 4
					(or (get-header :proxy-connection headers)	
					    (get-header :connection headers))
					(get-header :connection headers))))
		   (and connection 
			(member :keep-alive connection)
			(> *persistent-connection-maximum-requests*
			   (if inside-transaction-p (1+ (the fixnum requests-completed)) requests-completed)))))
	       (t (let ((connection (get-header :connection (server-headers server))))
		    (if (member :close connection)
                        nil
                        (> *persistent-connection-maximum-requests*
                           (if inside-transaction-p (1+ (the fixnum requests-completed)) requests-completed))))))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.849")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(SCL:FUNDEFINE 'WRITE-SERVER-PERSISTENT-CONNECTION-HEADERS)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.850")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(define-generic write-server-persistent-connection-headers (server stream content-length)
  (declare ( values header-plist))
  (:documentation "Writes persistent connection headers to stream.
These handle negotiations concerning persistent connections with clients."))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.850")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod write-server-persistent-connection-headers ((server basic-server-mixin) stream content-length)
  (case (server-http-version server)
    (:http/1.0
      (cond ((and (case (server-method server)
		    (:get (case (server-status server)
			    ((204 304) t)
			    (t content-length)))
		    (:head t)
		    (t nil))
		  (persistent-connection-p server t))
	     (setf (server-persistent-connection-p server) t)
	     (write-header :connection '(:keep-alive) stream)
	     (multiple-value-bind (timeout max-requests)
		 (%server-persistent-connection-parameters server t)
	       (let ((value `(:timeout ,timeout :max ,max-requests)))
		 (declare (dynamic-extent value))
		 (write-header :keep-alive value stream))
	       t))
	    (t (setf (server-close-connection-p server) t)
	       nil)))
    (:http/0.9 nil)
    ;; Default in HTTP 1.1 is to always use persistent connections.
    (t (cond ((persistent-connection-p server t)
	      (setf (server-persistent-connection-p server) t)
	      nil)
	     (t (setf (server-close-connection-p server) t)
		(write-header :connection '(:close) stream)
		t)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.466")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun %write-document-mime-headers (stream &rest args)
  ;; content-length can be :bytes when one wants an :accept-ranges without a content-length
  (declare (dynamic-extent args)
           #+Genera(scl:arglist stream content-type charset 
                                length last-modification entity-tag expires location content-location content-language
                                public allow cache-control &optional (termination-line-p t) mime-header-plist header-plist))
  (destructuring-bind
    (content-type charset length last-modification entity-tag expires location content-location content-language
                  public allow cache-control &optional (termination-line-p t) header-plist mime-header-plist)
      args
    (let* ((server *server*)
           (http-version (server-http-version server))
           (1-0-protocol-p (member http-version '(:http/1.0 :http/0.9)))
           (content-type (cond ((null content-type) nil)
                               (charset (mime-content-type-spec-merging-character-set content-type charset))
                               (t (%mime-content-type-spec content-type))))
           ;; Lispm and MCL do ascii translations skewing the content-length downwards.
           (content-length (etypecase length
			     (null nil)
			     (integer
			       #+character-translation-skews-file-size
			       (case (server-method server)
				 (:get (if (and (url::content-type-copy-mode-is-text-p content-type)
						(not *crlf-document-data*))
					   nil
					   length))
				 (t length))
			       #-character-translation-skews-file-size
			       length)))
           (accept-bytes-p (or content-length (eql length :bytes))))
      (declare (dynamic-extent content-type))
      (write-header :date (server-request-time server) stream)
      (write-header :server *server-version* stream)
      (write-server-persistent-connection-headers server stream content-length)
      (cond-every
	(last-modification (write-header :last-modified last-modification stream))
	(expires (write-header :expires expires stream))
	(location (write-header :location location stream))
	(accept-bytes-p (write-header :accept-ranges :bytes stream))
	(public (write-header :public public stream))
	(allow (write-header :allow allow stream))
	((and cache-control (not 1-0-protocol-p)) (write-header :cache-control cache-control stream))
	(header-plist (write-headers stream header-plist nil)))
      (cond-every
	(content-type (write-header :content-type content-type stream))
	(content-length (write-header :content-length content-length stream))
	(content-location (write-header :content-location content-location stream))
	(content-language (write-header :content-language content-language stream))
	(entity-tag
	  (if 1-0-protocol-p
	      (write-header :content-version (entity-tag-value entity-tag) stream)
	      (write-header :etag entity-tag stream)))
	(mime-header-plist (write-headers stream mime-header-plist nil)))
      (when termination-line-p			;send header termination line.
	(send-cr-line-feed stream)))))
