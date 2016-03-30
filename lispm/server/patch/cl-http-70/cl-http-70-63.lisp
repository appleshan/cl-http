;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.63
;;; Reason: Variable HTTP:*USER-AGENT-CAPABILITIES*:  add icab client capabilities.
;;; 
;;; Correct bug in note-anchor and related functions whereby the href would be
;;; erroneously formatted when both a url reference and local reference were
;;; present. Applications will need to recompile functions calling any variant of
;;; %write-anchor-command-arguments inline, normally via html generation macros.
;;; 
;;; Function HTTP::WITH-STRING-TRIM-BOUNDS:  fix inline declaration
;;; Function HTML2::%WRITE-HREF-WITH-LOCAL-REFERENCE:  specialized function.
;;; Function HTML2::%WRITE-ANCHOR-COMMAND-ARGUMENTS:  use it.
;;; Function HTML2::%DECLARE-LINK:  -
;;; Function HTML2::%NOTE-ANCHOR:  -
;;; Function HTML2::%NOTE-IMAGE:  -
;;; Function HTML2:WRITE-STRING-ANCHORING-URLS:  -
;;; Function NS11::%NOTE-IMAGE:  -
;;; Function NS2.0::%WRITE-ANCHOR-COMMAND-ARGUMENTS:  -
;;; Function HTML::%WRITE-ANCHOR-COMMAND-ARGUMENTS:  -
;;; Function HTML::%DECLARE-LINK:  -
;;; Function HTML::%NOTE-ANCHOR:  -
;;; Function NS2.0::%NOTE-ANCHOR:  -
;;; Function NS2.0::%NOTE-IMAGE:  -
;;; Written by JCMa, 8/17/00 19:50:02
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.62,
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
  "HTTP:SERVER;VARIABLES.LISP.186"
  "HTTP:SERVER;UTILS.LISP.460"
  "HTTP:SERVER;HTML2.LISP.292"
  "HTTP:SERVER;NETSCAPE-1-1.LISP.128"
  "HTTP:SERVER;HTML-3-2.LISP.36"
  "HTTP:SERVER;NETSCAPE-2-0.LISP.123"
  "HTTP:SERVER;NETSCAPE-3-0.LISP.39")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;VARIABLES.LISP.186")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp;-*-")

(define-parameter *user-agent-capabilities* '((:tables 
                                                (:mozilla)
                                                (:msie)
                                                (:ncsa-mosaic)
                                                (:spyglass_mosaic :|2.1| :|2.1F9|)
                                                (:macweb :|2.0| :|2.0A3E| :|2.0B1E|)
						(icab))
                                              (:frames
                                                (:mozilla)
						(icab))
                                              (:client-side-image-maps
                                                (:mozilla)
                                                (:msie)
                                                (:spyglass_mosaic :|2.1| :|2.1F9|)
						(icab))
                                              (:cookies
                                                (:mozilla)
                                                (:msie)
						(icab))
                                              (:java-script
                                                (:mozilla)
						(icab))
                                              (:java
                                                (:HotJava)
                                                (:mozilla)))
                  "Maps capabilities to user agents.
Entries are (capability &rest (user-agent &rest versions)).
No versions means that all versions handle the capability.")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.460")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(define-macro with-string-trim-bounds ((charset string start end) &body body)
  "Rebinds START and END to ignore leading and trailing characters in CHARSET."
  `(locally
     (declare (inline %string-trim-bounds))
     (multiple-value-bind (,start ,end)
	 (%string-trim-bounds ,charset ,string ,start ,end)
       ,@body)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.292")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

;;;------------------------------------------------------------------- 
;;;
;;; HYPERTEXT LINKS
;;;

(defun %write-href-with-local-reference (stream reference local-reference)
  (flet ((write-functional-reference (reference stream)
	   (let* ((string (with-output-to-string (string)
			    (funcall reference string)))
		  (start 0)
		  (end (length string)))
	     (declare (dynamic-extent string))
	     (http::with-string-trim-bounds ('(#\") string start end)
	       (write-string string stream :start start :end end)))))
    (declare (inline write-functional-reference))
    (fast-format stream "HREF=\"~I#~A\""
		 (typecase reference
		   (function (write-functional-reference reference stream))
		   (t (write-string (coerce-url-string reference url:*escape-search-urls*) stream)))
		 local-reference)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.292")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

(defun %write-anchor-command-arguments
       (stream reference local-reference tag relation inverse urn title methods)
  ;; write anchor arguments
  (cond ((and local-reference reference)
	 (%write-href-with-local-reference stream reference local-reference))
        (reference ;; URL for a anchored document retrieved by the anchor.
         ;; ensure generated URLs are escaped.
         (%write-command-key-arg stream "HREF" (coerce-url-string reference url:*escape-search-urls*)))
        (local-reference ;; tags for a position in the current document.
         (fast-format stream " HREF=#~A" local-reference)))
  (cond-every 
    (tag ;; "A tag anchor."
      (%write-command-key-arg stream "NAME" tag))
    (relation ;; "The relationship the anchored document has to this one."
      (%write-command-key-arg stream "REL" relation)) 
    (inverse ;; "The reverse relationship type, and the inverse of REL."
      (%write-command-key-arg stream "REV" inverse))
    (urn ;;  "URN for a anchored document retrieved by the anchor."
      (%write-command-key-arg stream "URN" urn))
    (title ;;  "Title to use for otherwise untitled anchored document."
      (%write-command-key-arg stream"TITLE" title))
    (methods ;;  "Comma separated list of HTTP methods supported by the anchored  object."
      (%write-command-key-arg stream"METHODS" methods))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.292")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

(defun %declare-link (stream reference local-reference tag relation inverse urn title methods)
  (declare (notinline %write-anchor-command-arguments)) ; speed not essential so conserve working set  12/4/95 -- JCMa.
  (%issue-command ("LINK" stream :fresh-line t :trailing-line t)
    (%write-anchor-command-arguments stream reference local-reference
                                     tag relation inverse urn title methods)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.292")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

(defun %note-anchor (stream text reference local-reference tag relation inverse urn title methods)
  (%with-environment ("A" :string-for-null-stream :inline :stream stream :fresh-line nil)
                     (%write-anchor-command-arguments stream reference local-reference
                                                      tag relation inverse urn title methods)
    (write-string text stream)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.292")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

;; html spec says to always provide alternative text
;; "~&<IMG SRC=\"~A\" ALIGN=\"~A\" ALT=\"~A\">~&"
(defun %note-image (stream image-url alternative-text alignment accept-coordinates-at-url)
  (flet ((write-element (stream image-url alignment alternative-text accept-coordinates-at-url)
           (flet ((alignment-value (alignment)
                    (unless (member alignment *image-alignment-values*)
                      (error "Unknown alignment, ~S, for an image." alignment))
                    (symbol-name alignment)))
             (declare (inline alignment-value))
             (%issue-command ("IMG" stream)
               (cond-every
                 (image-url
                   (%write-command-key-arg stream "SRC" image-url))
                 (alignment
                   (%write-command-key-arg stream "ALIGN" (alignment-value alignment)))
                 (alternative-text
                   (check-type alternative-text string)
                   (%write-command-key-arg stream "ALT" alternative-text))
                 (accept-coordinates-at-url
                   (%write-command-key-arg stream "ISMAP")))))))
    (let ((url-string (url:name-string-without-search-suffix image-url nil)))
      (declare (dynamic-extent url-string))
      (case accept-coordinates-at-url
        ((nil :no-url)
         (write-element stream url-string alignment alternative-text accept-coordinates-at-url))
        (t (with-anchor-noted (:reference (if (eq accept-coordinates-at-url t)
                                              url-string
                                              (url:name-string-without-search-suffix accept-coordinates-at-url nil))
                               :stream stream)
             (write-element stream url-string alignment alternative-text accept-coordinates-at-url)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.292")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

(defun write-string-anchoring-urls (string stream &optional (start 0) (end (length string)))
  "Writes STRING to STREAM spotting any URLS and anchoring them
while being careful to translated any special characters for HTML."
  (loop with s = start
        doing (multiple-value-bind (url-start url-end)
                  (url::url-position string s end)
                (cond ((and url-start url-end)
                       (unless (= s url-start)
                         (write-string-quoting-specials string stream s url-start))
                       (let ((url (subseq string url-start url-end)))
                         (declare (dynamic-extent url))
                         (with-anchor-noted (:reference url :stream stream)
                           (write-string-quoting-specials string stream url-start url-end)))
                       (if (< url-end end)
                           (setq s url-end)
                           (return-from write-string-anchoring-urls)))
                      (t (write-string-quoting-specials string stream s end)
                         (return-from write-string-anchoring-urls))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;NETSCAPE-1-1.LISP.128")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: netscape1.1; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;; html spec says to always provide alternative text
(defun %note-image (stream image-url alternative-text alignment accept-coordinates-at-url
                           border vertical-space horizontal-space width height)
  (flet ((write-element (stream image-url image-url-string alignment alternative-text accept-coordinates-at-url)
           (flet ((alignment-value (alignment)
                    (unless (member alignment *image-alignment-values*)
                      (error "Unknown alignment, ~S, for an image." alignment))
                    (symbol-name alignment))
                  (write-integer-arg (stream option value)
                    (check-type value integer)
                    (%write-command-key-arg stream option value t)))
             (declare (inline alignment-value write-integer-arg))
             (%issue-command ("IMG" stream)
               ;; Automagically insert image sizes when algorithms available.
               (when (and image-url (not (or width height)) http:*image-sizes-default-automatically*)
                 (multiple-value-setq (width height)
                   (url:image-size image-url)))
               (cond-every
                 (image-url-string
                   (%write-command-key-arg stream "SRC" image-url-string))
                 (alignment
                   (%write-command-key-arg stream "ALIGN" (alignment-value alignment)))
                 (alternative-text
                   (check-type alternative-text string)
                   (%write-command-key-arg stream "ALT" alternative-text))
                 (accept-coordinates-at-url (%write-command-key-arg stream "ISMAP"))
                 (border (write-integer-arg stream "BORDER" border))
                 (vertical-space (write-integer-arg stream "VSPACE" vertical-space))
                 (horizontal-space (write-integer-arg stream "HSPACE" horizontal-space))
                 (width (write-integer-arg stream "WIDTH" width))
                 (height (write-integer-arg stream "HEIGHT" height)))))))
    (declare (dynamic-extent #'write-element))
    (let* ((url-string (url:name-string-without-search-suffix image-url nil))
           (real-image-url (typecase image-url
                             (string nil)
                             (t (intern-url url-string)))))
      (declare (dynamic-extent url-string))
      (case accept-coordinates-at-url
        ((nil :no-url)
         (write-element stream real-image-url url-string alignment alternative-text accept-coordinates-at-url))
        (t (with-anchor-noted (:reference (if (eq accept-coordinates-at-url t)
                                              url-string
                                              (url:name-string-without-search-suffix accept-coordinates-at-url nil))
                               :stream stream)
             (write-element stream real-image-url url-string alignment alternative-text accept-coordinates-at-url)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;NETSCAPE-2-0.LISP.123")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: netscape2.0; BASE: 10; Syntax: ansi-common-lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun %write-anchor-command-arguments
       (stream reference local-reference tag target events relation inverse urn title methods)
  (declare (notinline html2::%write-command-key-arg))   ;don't bloat the working set
  ;; write anchor arguments
  (cond ((and local-reference reference)
	 (html2::%write-href-with-local-reference stream reference local-reference))
        (reference ;; URL for a anchored document retrieved by the anchor.
         ;; ensure generated URLs are escaped.
         (%write-command-key-arg stream "HREF" (coerce-url-string reference url:*escape-search-urls*)))
        (local-reference ;; tags for a position in the current document.
         (fast-format stream " HREF=#~A" local-reference)))
  (cond-every 
    (tag ;; "A tag anchor."
      (html2::%write-command-key-arg stream "NAME" tag))
    (target ;;target window Netscape 2.0
      (%write-target-window-command-key-arg stream target))
    (events ;; Netscape's Javascript events
      (dolist (event events)
        (funcall event stream)))
    (relation ;; "The relationship the anchored document has to this one."
      (html2::%write-command-key-arg stream "REL" relation)) 
    (inverse ;; "The reverse relationship type, and the inverse of REL."
      (html2::%write-command-key-arg stream "REV" inverse))
    (urn ;;  "URN for a anchored document retrieved by the anchor."
      (html2::%write-command-key-arg stream "URN" urn))
    (title ;;  "Title to use for otherwise untitled anchored document."
      (html2::%write-command-key-arg stream"TITLE" title))
    (methods ;;  "Comma separated list of HTTP methods supported by the anchored  object."
      (html2::%write-command-key-arg stream"METHODS" methods))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML-3-2.LISP.36")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: html3.2; -*-")

(defun %write-anchor-command-arguments (stream reference local-reference tag relation inverse title)
  ;; write anchor arguments
  (cond ((and local-reference reference)
	 (html2::%write-href-with-local-reference stream reference local-reference))
        (reference ;; URL for a anchored document retrieved by the anchor.
         ;; ensure generated URLs are escaped.
         (%write-command-key-arg stream "HREF" (typecase reference
                                                 (function reference)
                                                 (t (coerce-url-string reference url:*escape-search-urls*)))))
        (local-reference ;; tags for a position in the current document.
         (fast-format stream " HREF=#~A" local-reference)))
  (cond-every 
    (tag ;; "A tag anchor."
      (%write-command-key-arg stream "NAME" tag))
    (relation ;; "The relationship the anchored document has to this one."
      (%write-command-key-arg stream "REL" relation)) 
    (inverse ;; "The reverse relationship type, and the inverse of REL."
      (%write-command-key-arg stream "REV" inverse))
    (title ;;  "Title to use for otherwise untitled anchored document."
      (%write-command-key-arg stream"TITLE" title))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML-3-2.LISP.36")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: html3.2; -*-")

(defun %declare-link (stream reference local-reference tag relation inverse title)
  (declare (inline %write-anchor-command-arguments))
  (%issue-command ("LINK" stream :fresh-line t :trailing-line t)
    (%write-anchor-command-arguments stream reference local-reference tag relation inverse title)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML-3-2.LISP.36")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: html3.2; -*-")

(defun %note-anchor (stream text reference local-reference tag relation inverse title)
  (%with-environment ("A" :string-for-null-stream :inline :stream stream :fresh-line nil)
                     (%write-anchor-command-arguments stream reference local-reference tag relation inverse title)
    (write-string text stream)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;NETSCAPE-2-0.LISP.123")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: netscape2.0; BASE: 10; Syntax: ansi-common-lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun %note-anchor (stream text reference local-reference tag target events relation inverse urn title methods)
  (%with-environment ("A" :string-for-null-stream :inline :fresh-line nil :stream stream)
                     (%write-anchor-command-arguments stream reference local-reference tag
                                                      target events relation inverse urn title methods)
    (write-string text stream)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;NETSCAPE-2-0.LISP.123")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: netscape2.0; BASE: 10; Syntax: ansi-common-lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;;;------------------------------------------------------------------- 
;;;
;;; IMAGE EXTENSIONS
;;;

;; html spec says to always provide alternative text
(defun %note-image (stream image-url alternative-text alignment accept-coordinates-at-url
                           client-side-image-map
                           border vertical-space horizontal-space width height)
  (flet ((write-element (stream image-url alignment alternative-text accept-coordinates-at-url)
           (flet ((alignment-value (alignment)
                    (unless (member alignment *image-alignment-values*)
                      (error "Unknown alignment, ~S, for an image." alignment))
                    (symbol-name alignment))
                  (write-integer-arg (stream option value)
                    (check-type value integer)
                    (%write-command-key-arg stream option value t)))
             (declare (inline alignment-value write-integer-arg))
             ;; Automagically insert image sizes when algorithms available.
             (when (and image-url (not (or width height)) http:*image-sizes-default-automatically*)
	       (multiple-value-setq (width height)
		 (url:image-size image-url)))
             (%issue-command ("IMG" stream)
               (cond-every
                 (image-url
                   (%write-command-key-arg stream "SRC" image-url))
                 (alignment
                   (%write-command-key-arg stream "ALIGN" (alignment-value alignment)))
                 (alternative-text
                   (check-type alternative-text string)
                   (%write-command-key-arg stream "ALT" alternative-text))
                 (accept-coordinates-at-url (%write-command-key-arg stream "ISMAP"))
                 (client-side-image-map
                   (%write-command-key-arg
                     stream "USEMAP" (url:name-string-without-search-suffix client-side-image-map nil)))
                 (border (write-integer-arg stream "BORDER" border))
                 (vertical-space (write-integer-arg stream "VSPACE" vertical-space))
                 (horizontal-space (write-integer-arg stream "HSPACE" horizontal-space))
                 (width (write-integer-arg stream "WIDTH" width))
                 (height (write-integer-arg stream "HEIGHT" height)))))))
    (declare (inline write-element))
    (let ((url-string (url:name-string-without-search-suffix image-url nil)))
      (declare (dynamic-extent url-string))
      (case accept-coordinates-at-url
        ((nil :no-url)
         (write-element stream url-string alignment alternative-text accept-coordinates-at-url))
        (t (with-anchor-noted (:reference (if (eq accept-coordinates-at-url t)
                                              url-string
                                              (url:name-string-without-search-suffix accept-coordinates-at-url nil))
                               :stream stream)
             (write-element stream url-string alignment alternative-text accept-coordinates-at-url)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;NETSCAPE-3-0.LISP.39")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: netscape3.0; BASE: 10; Syntax: ansi-common-lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;;;------------------------------------------------------------------- 
;;;
;;; IMAGE EXTENSIONS
;;;

;; html spec says to always provide alternative text
(defun %note-image (stream image-url alternative-text alignment accept-coordinates-at-url
                           client-side-image-map
                           border vertical-space horizontal-space width height events)
  (flet ((write-element (stream image-url alignment alternative-text accept-coordinates-at-url events)
           (flet ((alignment-value (alignment)
                    (unless (member alignment *image-alignment-values*)
                      (error "Unknown alignment, ~S, for an image." alignment))
                    (symbol-name alignment))
                  (write-integer-arg (stream option value)
                    (check-type value integer)
                    (%write-command-key-arg stream option value t)))
             (declare (inline alignment-value write-integer-arg))
	     ;; Automagically insert image sizes when algorithms available.
             (when (and image-url (not (or width height)) http:*image-sizes-default-automatically*)
	       (multiple-value-setq (width height)
		 (url:image-size image-url)))
             (%issue-command ("IMG" stream)
               (cond-every
                 (image-url
                   (%write-command-key-arg stream "SRC" image-url))
                 (alignment
                   (%write-command-key-arg stream "ALIGN" (alignment-value alignment)))
                 (alternative-text
                   (check-type alternative-text string)
                   (%write-command-key-arg stream "ALT" alternative-text))
                 (accept-coordinates-at-url (%write-command-key-arg stream "ISMAP"))
                 (client-side-image-map
                   (%write-command-key-arg
                     stream "USEMAP" (url:name-string-without-search-suffix client-side-image-map nil)))
                 (border (write-integer-arg stream "BORDER" border))
                 (vertical-space (write-integer-arg stream "VSPACE" vertical-space))
                 (horizontal-space (write-integer-arg stream "HSPACE" horizontal-space))
                 (width (write-integer-arg stream "WIDTH" width))
                 (height (write-integer-arg stream "HEIGHT" height))
                 (events
                   (dolist (event events)
                     (html2::%write-input-type-event-arg stream event))))))))
    (let ((url-string (url:name-string-without-search-suffix image-url nil)))
      (declare (dynamic-extent url-string))
      (case accept-coordinates-at-url
        ((nil :no-url)
         (write-element stream url-string alignment alternative-text accept-coordinates-at-url events))
        (t (with-anchor-noted (:reference (if (eq accept-coordinates-at-url t)
                                              url-string
                                              (url:name-string-without-search-suffix accept-coordinates-at-url nil))
                               :stream stream)
             (write-element stream url-string alignment alternative-text accept-coordinates-at-url events)))))))

