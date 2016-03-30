;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.64
;;; Reason: Function TCP::STREAM-COPY-BYTES:  signal eof error 
;;; if input buffer does not contain enough bytes during buffer-level copies.
;;; Written by JCMa, 8/25/00 14:25:09
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.18, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.63,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Domain Name Server 436.0,
;;; HTTP Proxy Server 5.18, HTTP Client Substrate 3.15,
;;; W4 Constraint-Guide Web Walker 45.3, Ivory Revision 5, VLM Debugger 329,
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
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).

(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.171")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.171")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

;;;------------------------------------------------------------------- 
;;;
;;; FAST STREAM COPYING
;;;

(defun stream-copy-bytes (from-stream to-stream n-bytes &optional start)
  (check-type n-bytes integer)
  (when start
    (check-type start integer)
    (send from-stream :set-pointer start))
  (cond ;; buffer level copying
    ((and (operation-handled-p from-stream :read-input-buffer)
          (operation-handled-p to-stream :string-out))
     ;; If it can go, this mode is the most efficient by far.
     (loop with remaining = n-bytes and length 
           doing (multiple-value-bind (buffer offset limit)
                     (send from-stream :read-input-buffer t)
                   (cond ((null buffer) (return nil))
                         ((< (setq length (- limit offset)) remaining)
                          (send to-stream :string-out buffer offset limit)
                          (decf remaining length)
                          (send from-stream :advance-input-buffer))
                         (t (send to-stream :string-out buffer offset (setq length (+ remaining offset)))
                            (send from-stream :advance-input-buffer length)     ;leave buffer pointer in correct position
                            (return nil))))))
    ;; resourced line level copying
    ((and (operation-handled-p from-stream :string-line-in)
          (operation-handled-p to-stream :line-out)
          ;; copying from an interval is faster using :line-in
          (not (send-if-handles from-stream :line-in-more-efficient-than-string-line-in-p)))
     ;; Not as good, but better than :line-in/:line-out
     (loop with remaining = n-bytes
           with line = (or (without-interrupts
                             (prog1 si:*stream-copy-until-eof-temp-buffer*
                                    (setq si:*stream-copy-until-eof-temp-buffer* nil)))
                           (make-array 128. :type 'art-fat-string :fill-pointer 0))
           when (multiple-value-bind (length eof neol)
                    (send from-stream :string-line-in nil line 0 (min 128. remaining))
                  (cond ((< length remaining)
                         (send to-stream (if (or neol eof) :string-out :line-out) line 0 length)
                         (decf remaining length))
                        (t (send to-stream (if (or neol eof) :string-out :line-out) line 0 remaining)
                           (setq si:*stream-copy-until-eof-temp-buffer* line)
                           (return nil)))
                  eof)
             do (signal 'sys:end-of-file :stream from-stream)))
    ((and (operation-handled-p from-stream :line-in)
          (operation-handled-p to-stream :line-out))
     ;; Not as good, but better than :tyi/:tyo
     (loop with remaining = n-bytes
           for line = (send from-stream :line-in (min 128 remaining))
           for length = (length line)
           when (< length remaining)
             do (send to-stream :line-out line)
                (decf remaining length)
           else
             do (send to-stream :string-out line 0 remaining)
                (return nil)))
    ;; This always wins, but is incredibly slow.
    (t (loop for idx upfrom 0 below n-bytes
             for char = (send from-stream :tyi)
             when char
               do (send to-stream :tyo char)
             do (ignore idx)
                (signal 'sys:end-of-file :stream from-stream)))))

