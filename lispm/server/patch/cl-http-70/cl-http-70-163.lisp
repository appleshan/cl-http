;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for CL-HTTP version 70.163
;;; Reason: Function HTTP::JPEG-IMAGE-SIZE:  Fix error message on incorrect first mark.
;;; DEFINE-MIME-CONTENT-TYPE :IMAGE:  update some image types.
;;; Define export type :ico-image
;;; Variable HTTP:*USER-AGENT-CAPABILITIES*:  update browser capabilities.
;;; Written by JCMa, 9/01/03 23:45:59
;;; while running on FUJI-3 from FUJI:/usr/lib/symbolics/vlmlmfs2.vlod
;;; with Open Genera 2.0, Genera 8.5, Logical Pathnames Translation Files NEWEST,
;;; Lock Simple 437.0, Color Demo 422.0, Color 427.1, Graphics Support 431.0,
;;; Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Color System Documentation 10.0, SGD Book Design 10.0, Images 431.2,
;;; Image Substrate 440.4, CLIM 72.0, Genera CLIM 72.0, CLX CLIM 72.0,
;;; PostScript CLIM 72.0, CLIM Demo 72.0, CLIM Documentation 72.0,
;;; Statice Runtime 466.1, Statice 466.0, Statice Browser 466.0,
;;; Statice Server 466.2, Statice Documentation 426.0, Metering 444.0,
;;; Metering Substrate 444.1, Symbolics Concordia 444.0, Graphic Editor 440.0,
;;; Graphic Editing 441.0, Bitmap Editor 441.0, Graphic Editing Documentation 432.0,
;;; Postscript 436.0, Concordia Documentation 432.0, Joshua 237.6,
;;; Joshua Documentation 216.0, Joshua Metering 206.0, Jericho 237.0, C 440.0,
;;; Lexer Runtime 438.0, Lexer Package 438.0, Minimal Lexer Runtime 439.0,
;;; Lalr 1 434.0, Context Free Grammar 439.0, Context Free Grammar Package 439.0,
;;; C Runtime 438.0, Compiler Tools Package 434.0, Compiler Tools Runtime 434.0,
;;; C Packages 436.0, Syntax Editor Runtime 434.0, C Library Headers 434,
;;; Compiler Tools Development 435.0, Compiler Tools Debugger 434.0,
;;; Experimental C Documentation 427.0, Syntax Editor Support 434.0,
;;; LL-1 support system 438.0, Fortran 434.0, Fortran Runtime 434.0,
;;; Fortran Package 434.0, Experimental Fortran Doc 428.0, Pascal 433.0,
;;; Pascal Runtime 434.0, Pascal Package 434.0, Pascal Doc 427.0,
;;; MacIvory Support 447.0, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.41, Genera 8 5 Macivory Support Patches 1.0,
;;; Genera 8 5 Metering Patches 1.0, Genera 8 5 Joshua Patches 1.0,
;;; Genera 8 5 Jericho Patches 1.0, Genera 8 5 Joshua Doc Patches 1.0,
;;; Genera 8 5 Joshua Metering Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.3,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clx Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Clim Demo Patches 1.0, Genera 8 5 Color Patches 1.1,
;;; Genera 8 5 Images Patches 1.0, Genera 8 5 Color Demo Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; Genera 8 5 Concordia Patches 1.2, Genera 8 5 Concordia Doc Patches 1.0,
;;; Genera 8 5 C Patches 1.0, Genera 8 5 Pascal Patches 1.0,
;;; Genera 8 5 Fortran Patches 1.0, Binary Tree 34.0, Showable Procedures 36.3,
;;; HTTP Server 70.162, W3 Presentation System 8.1, HTTP Client Substrate 4.22,
;;; HTTP Client 51.3, CL-HTTP Server Interface 54.0, HTTP Proxy Server 6.32,
;;; CL-HTTP Documentation 3.0, Experimental CL-HTTP CLIM User Interface 1.1,
;;; MAC 414.0, LMFS 442.1, Ivory Revision 5, VLM Debugger 329, Genera program 8.18,
;;; DEC OSF/1 V4.0 (Rev. 110),
;;; 1152x678 24-bit TRUE-COLOR X Screen FUJI:2.0 with 224 Genera fonts (The Olivetti & Oracle Research Laboratory R3323),
;;; Machine serial number 6288682,
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add support for Apple's Gestalt and Speech Managers. (from SYS:MAC;MACIVORY-SPEECH-SUPPORT.LISP.1),
;;; Vlm lmfs patch (from W:>reti>vlm-lmfs-patch.lisp.18),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing),
;;; Pht debugging patch (from W:>Reti>pht-debugging-patch.lisp.4),
;;; Cname level patch (from W:>reti>cname-level-patch),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;UTILS.LISP.517"
  "HTTP:SERVER;HEADERS.LISP.520"
  "HTTP:SERVER;SERVER.LISP.924"
  "HTTP:SERVER;VARIABLES.LISP.199")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.517")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(define jpeg-image-size (stream &optional (error-p t) &aux (file-length (file-length stream)))
  "Returns WIDTH and HEIGHT for JPEG encountered in data stream. 
   Assumes stream is positioned at beginning of JPEG data."
  (declare (values height width jpeg-image-p))
  (labels ((read-two-bytes (stream)
             (+ (ash (read-byte stream nil nil) 8)
                (read-byte stream nil nil)))
           (get-next-type (stream error-p)
             (let ((lead (read-byte stream nil nil))
                   (next (read-byte stream nil nil)))
               (cond ((and lead (= lead #xff)) next)
                     (error-p (error "~X is not a mark for JPEG stream ~A" lead stream))
                     (t (return-from jpeg-image-size (values nil nil t))))))
           (image-block-p (type)
             (and type (<= #xc0 type #xcf) (/= type #xc4) (/= type #xcc)))
           (get-size-from-image-block (stream)
             (file-position stream (+ (file-position stream) 3))
             (let ((height (read-two-bytes stream))
                   (width (read-two-bytes stream)))
               (values width height t)))
           (skip-block (stream type error-p)
             (unless (= type #xd9)
               (let* ((length (read-two-bytes stream))
                      (new-length (+ length -2 (file-position stream))))
                 (file-position stream (+ length -20 (file-position stream)))
                 (cond ((<= new-length file-length)
                        (file-position stream new-length))
                       (error-p (error "Wrong format in JPEG stream: ~S" stream))
                       (t (return-from jpeg-image-size (values nil nil t))))))))
    (let ((first-mark (get-next-type stream error-p)))
      (cond ((and first-mark (= first-mark #xd8))
             (loop for type = (get-next-type stream error-p)
                   while type
                   when (image-block-p type)
                     do (return (get-size-from-image-block stream))
                   else do (skip-block stream type error-p)))
            (error-p (error "First mark in JPEG stream ~S is ~X and not #xd8." stream first-mark))
            (t (values nil nil nil))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.520")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-mime-content-type
  :image
  :minor-types (:gif :jpeg :png :png :pict :tiff :ico))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.924")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(http:define-url-export-type :ico-image :ico (:image :x-icon) :copy-mode :binary)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;VARIABLES.LISP.199")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp;-*-")

;; There are many more interesting user agents out there.  Typical user
;; agents are now Mozilla (and related), MSIE, Opera and Safari (Apple).
;; Predicates on universally-implemented features should just return t
;; for unknown browsers -- JCMa 9/1/2003
(define-parameter *user-agent-capabilities*
  '((:tables 
      (:msie)
      (:mozilla)
      (:ncsa-mosaic)
      (:safari)
      (:opera)
      (:spyglass_mosaic :|2.1| :|2.1F9|)
      (:macweb :|2.0| :|2.0A3E| :|2.0B1E|)
      (:icab))
    (:frames
      (:msie)
      (:mozilla)
      (:safari)
      (:opera)
      (:icab))
    (:client-side-image-maps
      (:msie)
      (:mozilla)
      (:safari)
      (:opera)
      (:spyglass_mosaic :|2.1| :|2.1F9|)
      (:icab))
    (:cookies
      (:msie)
      (:mozilla)
      (:safari)
      (:opera)
      (:icab))
    (:java-script
      (:msie)
      (:mozilla)
      (:safari)
      (:opera)
      (:icab))
    (:java
      (:msie)
      (:mozilla)
      (:safari)
      (:opera)
      (:HotJava)))
  "Maps capabilities to user agents.
Entries are (capability &rest (user-agent &rest versions)).
No versions means that all versions handle the capability.")

