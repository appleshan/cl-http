;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for CL-HTTP version 70.128
;;; Reason: Function (DEFUN-IN-FLAVOR TCP::DO-ASCII-TRANSLATION TCP::MODAL-ASCII-TRANSLATING-BUFFERED-INPUT-STREAM-MIXIN):  fix binary mode translation bug
;;; Function (DEFUN-IN-FLAVOR TCP::UNTRANSLATE-WINDOW TCP::MODAL-ASCII-TRANSLATING-BUFFERED-INPUT-STREAM-MIXIN):  ditto
;;; Written by Reti, 5/03/01 15:17:00
;;; while running on RAINIER-2 from RAINIER:/com/alpha/genera/sys.sct/worlds/color+clim.vlod
;;; with Open Genera 2.0, Genera 8.5, Logical Pathnames Translation Files NEWEST,
;;; Experimental Genera 8 5 Patches 1.0, Genera 8 5 System Patches 1.26,
;;; Genera 8 5 Clim Patches 1.2, Genera 8 5 Genera Clim Patches 1.0,
;;; Genera 8 5 Postscript Clim Patches 1.0, Genera 8 5 Clx Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Clim Demo Patches 1.0,
;;; Genera 8 5 Color Patches 1.1, Genera 8 5 Images Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Color 427.1, Graphics Support 431.0,
;;; Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Color System Documentation 10.0, SGD Book Design 10.0, Images 431.2,
;;; Image Substrate 440.4, CLIM 72.0, Genera CLIM 72.0, CLX CLIM 72.0,
;;; PostScript CLIM 72.0, CLIM Demo 72.0, CLIM Documentation 72.0,
;;; W4 Constraint-Guide Web Walker 45.10, HTTP Client Substrate 4.10,
;;; HTTP Server 70.126, Showable Procedures 36.3, Binary Tree 34.0,
;;; W3 Presentation System 8.1, HTTP Client 50.7, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.16, DEC OSF/1 V4.0 (Rev. 67),
;;; 1152x872 24-bit TRUE-COLOR X Screen INTERNET|128.52.39.113:0.0 with 0 Genera fonts (The XFree86 Project,
;;; Inc R40099003), Machine serial number 6292827,
;;; Real 32b image patch (from W:>reti>real-32b-image-patch.lisp.13),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.198")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.198")
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
           ;; could be changed to happen once only in :next-input-buffer, but users would not get bounds checking on the high end
           (si:change-indirect-array at-string (si:array-type at-string) (list end) si:stream-input-buffer 0)
           (let ((buffer si:stream-input-buffer)
                 (string at-string))
             (declare (sys:array-register buffer string))
             (when at-cr-flag (clear-at-cr-flag start))
             (setq at-start start               ;move position forward
                   si:stream-input-index start)
             (loop for idx upfrom start below end
                   for ch = (aref buffer idx)
                   do ;;this when clause made it impossible to untranslated when
                      ;;switching to binary mode -- Kalman
		      ;(when (< ch #o040)        ; ascii space
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
                            (return)))
			;)  corresponded to commented out when
			finally (setq at-end end))
             #+ignore
             (format *trace-output* "~&~'bDo-ASCII-Translation:~ ~D ~D (~:C)~&" at-start at-end (aref string (1- at-end)))
             ))
          (at-cr-flag
           (clear-at-cr-flag si:stream-input-index))
          (t (setq si:stream-input-index start)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.198")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

(defun-in-flavor (untranslate-window modal-ascii-translating-buffered-input-stream-mixin)
                 (at-buffer at-string start end)
  (let ((buffer at-buffer)
        (string at-string))
    (declare (sys:array-register buffer string))
    (loop for idx downfrom (1- end) to start
          for char = (aref string idx)
          for byte = (si:char-to-ascii char)
;;the when made it impossible to untranslate correctly when switching to
;;binary mode -- Kalman
;         when (< byte #o040)
            do (setf (aref buffer idx) byte))))

