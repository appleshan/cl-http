;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for HTTP-PROXY version 6.4
;;; Reason: Function (CLOS:METHOD HTTP::SWEEP-CACHE-REPRESENTATIONS-FOR-GARBAGE (HTTP::PROXY-CACHE T)):  fix deadlock when deleting invalid representations.
;;; Function (CLOS:METHOD HTTP::SWEEP-CACHE-RESOURCES-FOR-GARBAGE (HTTP::PROXY-CACHE T)):  remove deadlock case
;;; Function (CLOS:METHOD HTTP::GARBAGE-COLLECT-INVALID-REPRESENTATIONS (HTTP::PROXY-CACHE)):  also GC for invalid entities.
;;; Function (CLOS:METHOD HTTP::HANDLE-VALID-P (HTTP::FILESYSTEM-HANDLE)):  -
;;; Function HTTP::HANDLE-VALID-P:  -
;;; Function HTTP::REPRESENTATION-ENTITY-HANDLE-VALID-P:  -
;;; Function (CLOS:METHOD HTTP::REPRESENTATION-ENTITY-HANDLE-VALID-P (HTTP::ENTITY-PERSISTENCE-MIXIN)):  -
;;; Function HTTP::PROXY-CACHE-VERIFY-ENTITY-VALIDITY:  -
;;; Function (CLOS:METHOD HTTP::PROXY-CACHE-VERIFY-ENTITY-VALIDITY (HTTP::PROXY-CACHE)):  -
;;; Function HTTP::HANDLE-VALID-P:  -
;;; Function (CLOS:METHOD HTTP::HANDLE-INVALIDATE (HTTP::FILESYSTEM-HANDLE)):  -
;;; Function (CLOS:METHOD HTTP::HANDLE-VALID-P (HTTP::FILESYSTEM-HANDLE)):  -
;;; Function (CLOS:METHOD HTTP::DATABASE-CLOSE (T HTTP::FILESYSTEM-HANDLE (EQL :OUTPUT)) :AFTER):  -
;;; Function HTTP::PROXY-RESPOND-WHILE-CACHING-ENTITY:  careful about setting representation-entity-invalid-p.
;;; Function (CLOS:METHOD HTTP::PROXY-REVALIDATE-REPRESENTATION (HTTP::REPRESENTATION)):  careful about setting entity validity.
;;; Function (CLOS:METHOD HTTP::HANDLE-OBJECT-DELETE (HTTP::FILESYSTEM-HANDLE)):  lose deleted pathname.
;;; Function (CLOS:METHOD HTTP::HANDLE-INITIALIZE (HTTP::FILESYSTEM-HANDLE)):  -
;;; Written by JCMa, 12/03/00 13:05:03
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.88,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.0,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; HTTP Proxy Server 6.3, HTTP Client Substrate 4.0, Statice Server 466.2,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.16,
;;; DEC OSF/1 V4.0 (Rev. 110),
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
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.6),
;;; Cname level patch (from W:>reti>cname-level-patch).

;;; Patch file for HTTP-PROXY version 6.4
;;; Written by JCMa, 12/05/00 11:52:47
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.90,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.0,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Lock Simple Patches 1.0, Jcma 42,
;;; HTTP Proxy Server 6.4, HTTP Client Substrate 4.1, Statice Server 466.2,
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.6),
;;; Cname level patch (from W:>reti>cname-level-patch).


;;; Patch file for HTTP-PROXY version 6.4
;;; Written by JCMa, 12/05/00 11:38:14
;;; while running on FUJI-3 from FUJI:/usr/lib/symbolics/Inc-CL-HTTP-70-90-LMFS.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, LMFS 442.1, Color 427.1,
;;; Graphics Support 431.0, Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Color System Documentation 10.0, SGD Book Design 10.0, Images 431.2,
;;; Image Substrate 440.4, CLIM 72.0, Genera CLIM 72.0, CLX CLIM 72.0,
;;; PostScript CLIM 72.0, CLIM Demo 72.0, CLIM Documentation 72.0,
;;; Statice Runtime 466.1, Statice 466.0, Statice Browser 466.0,
;;; Statice Server 466.2, Statice Documentation 426.0, Metering 444.0,
;;; Metering Substrate 444.1, Symbolics Concordia 444.0, Graphic Editor 440.0,
;;; Graphic Editing 441.0, Bitmap Editor 441.0, Graphic Editing Documentation 432.0,
;;; Postscript 436.0, Concordia Documentation 432.0, Joshua 237.4,
;;; Joshua Documentation 216.0, Joshua Metering 206.0, Jericho 237.0, C 440.0,
;;; Lexer Runtime 438.0, Lexer Package 438.0, Minimal Lexer Runtime 439.0,
;;; Lalr 1 434.0, Context Free Grammar 439.0, Context Free Grammar Package 439.0,
;;; C Runtime 438.0, Compiler Tools Package 434.0, Compiler Tools Runtime 434.0,
;;; C Packages 436.0, Syntax Editor Runtime 434.0, C Library Headers 434,
;;; Compiler Tools Development 435.0, Compiler Tools Debugger 434.0,
;;; C Documentation 426.0, Syntax Editor Support 434.0, LL-1 support system 438.0,
;;; Pascal 433.0, Pascal Runtime 434.0, Pascal Package 434.0, Pascal Doc 427.0,
;;; Fortran 434.0, Fortran Runtime 434.0, Fortran Package 434.0, Fortran Doc 427.0,
;;; HTTP Proxy Server 6.4, HTTP Server 70.90, Showable Procedures 36.3,
;;; Binary Tree 34.0, W3 Presentation System 8.1, HTTP Client Substrate 4.1,
;;; HTTP Client 50.0, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.21, Genera 8 5 Metering Patches 1.0,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Jericho Patches 1.0,
;;; Genera 8 5 Joshua Doc Patches 1.0, Genera 8 5 Joshua Metering Patches 1.0,
;;; Genera 8 5 Statice Runtime Patches 1.0, Genera 8 5 Statice Patches 1.0,
;;; Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.0,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clx Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Clim Demo Patches 1.0, Genera 8 5 Color Patches 1.1,
;;; Genera 8 5 Images Patches 1.0, Genera 8 5 Image Substrate Patches 1.0,
;;; Genera 8 5 Lock Simple Patches 1.0, Genera 8 5 Concordia Patches 1.0,
;;; Genera 8 5 Concordia Doc Patches 1.0, Genera 8 5 C Patches 1.0,
;;; Genera 8 5 Pascal Patches 1.0, Genera 8 5 Fortran Patches 1.0, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x976 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number -2141189522,
;;; Vlm lmfs patch (from W:>Reti>vlm-lmfs-patch.lisp.12),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.6),
;;; Pht debugging patch (from W:>Reti>pht-debugging-patch.lisp.2),
;;; Cname level patch (from W:>reti>cname-level-patch).


(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:PROXY;CACHE.LISP.149"
  "HTTP:PROXY;CACHE.LISP.150"
  "HTTP:PROXY;CACHE.LISP.151"
  "HTTP:PROXY;DATABASE.LISP.81"
  "HTTP:PROXY;REPRESENTATION.LISP.60"
  "HTTP:PROXY;PROXY-CACHE.LISP.84"
  "HTTP:PROXY;CLASS.LISP.31"
  "HTTP:PROXY;DATABASE.LISP.83"
  "HTTP:PROXY;DATABASE.LISP.84")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;CACHE.LISP.149")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod sweep-cache-resources-for-garbage ((cache proxy-cache) gc-predicate &key (enumeration :oldest-reference)
					      reclaim-bytes reclaim-objects
					      &aux (bytes-reclaimed 0) (resources-decached 0) fctn resource-deleter)
  (declare (integer bytes-reclaimed resources-decached))
  (labels ((conditional-gc (resource)
	     (when (funcall gc-predicate resource)
	       (let ((size (cache-object-size resource)))
		 (funcall resource-deleter cache resource)
		 (incf resources-decached)
		 (incf bytes-reclaimed size)
		 t)))
	   (bounded-gc (resource)
	     (when (conditional-gc resource)
	       (when (or (and reclaim-bytes (>= bytes-reclaimed reclaim-bytes))
			 (and reclaim-objects (= resources-decached reclaim-objects)))
		 (return-from sweep-cache-resources-for-garbage (values resources-decached bytes-reclaimed)))))
	   (map-variant (uri resource)
	     (declare (ignore uri))
	     (funcall fctn resource)))
    (declare (dynamic-extent #'conditional-gc #'bounded-gc #'map-variant)
	     (inline delete-resource))
    ;; preamble
    (cond ((or reclaim-bytes reclaim-objects)
	   (unless (or (and reclaim-bytes (< 0 reclaim-bytes))
		       (and reclaim-objects (< 0 reclaim-objects)))
	     (return-from sweep-cache-resources-for-garbage (values 0 0)))
	   (setq fctn #'bounded-gc))
	  (t (setq fctn #'conditional-gc)))
    ;; Apply the sweep method
    (ecase enumeration
      (:oldest-reference
	(setq resource-deleter #'remove-cache-object)	;delete grabbing table lock
	(cache-recency-map-resources cache fctn))
      (:random
	(setq resource-deleter #'%remove-cache-object)	;delete without grabbing table lock
	(map-resources cache #'map-variant :write)))
    ;; report the results
    (values resources-decached bytes-reclaimed)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;CACHE.LISP.150")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod sweep-cache-representations-for-garbage ((cache proxy-cache) gc-predicate &key (enumeration :oldest-reference)
						    reclaim-bytes reclaim-objects
						    &aux (bytes-reclaimed 0) (representations-decached 0) (resources-decached 0) fctn
						    resource-deleter)
  (declare (integer bytes-reclaimed representations-decached resources-decached))
  (labels ((delete-resource-without-lock (cache resource)
	     (%unintern-resource cache resource))
	   (delete-resource-with-lock (cache resource)
	     (with-lock-held ((cache-resource-table-lock cache) :write "Cache")
	       (%unintern-resource cache resource)))
	   (conditional-gc (resource)
	     (loop with representations-decached-p
		   for representation in (resource-representations resource)
		   do (when (funcall gc-predicate representation)
			(let ((size (cache-object-size representation)))
			  (remove-representation resource representation)
			  (incf representations-decached)
			  (incf bytes-reclaimed size)
			  (setq representations-decached-p t)))
		   finally (unless (resource-representations resource)
			     (with-lock-held ((cache-object-lock resource) :write "Resource")
			       (unless (resource-representations resource)
				 (funcall resource-deleter cache resource)
				 (incf resources-decached))))
			   (return representations-decached-p)))
	   (bounded-gc (resource)
	     (when (conditional-gc resource)
	       (when (or (and reclaim-bytes (>= bytes-reclaimed reclaim-bytes))
			 (and reclaim-objects (= resources-decached reclaim-objects)))
		 (return-from sweep-cache-representations-for-garbage (values representations-decached resources-decached bytes-reclaimed)))))
	   (map-variant (uri resource)
	     (declare (ignore uri))
	     (funcall fctn resource)))
    (declare (dynamic-extent #'conditional-gc #'bounded-gc #'map-variant))
    ;; preamble
    (cond ((or reclaim-bytes reclaim-objects)
	   (unless (or (and reclaim-bytes (< 0 reclaim-bytes))
		       (and reclaim-objects (< 0 reclaim-objects)))
	     (return-from sweep-cache-representations-for-garbage (values 0 0 0)))
	   (setq fctn #'bounded-gc))
	  (t (setq fctn #'conditional-gc)))
    ;; Apply the sweep method
    (ecase enumeration
      (:oldest-reference
	(setq resource-deleter #'delete-resource-with-lock)
	(cache-recency-map-resources cache fctn))
      (:random
	(setq resource-deleter #'delete-resource-without-lock)
	(map-resources cache #'map-variant :write)))
    ;; report the results
    (values representations-decached resources-decached bytes-reclaimed)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;CACHE.LISP.151")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;; Grabs a write lock on the resource table. Be prepared to have http threads block
(defmethod garbage-collect-invalid-representations ((cache proxy-cache) &key gc-type)
  (flet ((invalid-representation-p (representation)
	   (or (representation-invalid-p representation)
	       (representation-entity-invalid-p representation))))
    (let ((total-count (resources-count cache))
	  (gc-type-string (case gc-type (:full "Full GC") (:incremental "Incremental GC") (t gc-type))))
      (log-event :normal "Proxy Cache~:[~;~:* ~A~]: Beginning invalid representation garbage collection of ~D resources..." gc-type-string total-count)
      (multiple-value-bind (representations-decached resources-decached bytes-reclaimed)
	  (sweep-cache-representations-for-garbage cache #'invalid-representation-p :enumeration :random)
	(log-event :normal "Proxy Cache~:[~;~:* ~A~]: Finished invalid representation garbage collection.~
                            ~&~D representations expunded, ~D out of ~D resources expunged, and ~D bytes reclaimed."
		   gc-type-string representations-decached resources-decached total-count bytes-reclaimed))
      cache)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;CACHE.LISP.151")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defgeneric proxy-cache-verify-entity-validity (proxy-cache)
  (:documentation "Verifies that all putatively valid representations point to valid entity data.
Representations with invalid entity data are marked to have invalid data."))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;CACHE.LISP.151")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod proxy-cache-verify-entity-validity ((cache proxy-cache) &aux (count 0))
  (flet ((verify-entity-validity (representation)
	   (with-lock-held ((cache-object-lock representation) :write "Verify Entity Validity")
	     (when (and (representation-valid-p representation)
			(not (representation-entity-invalid-p representation))
			(not (representation-entity-handle-valid-p representation)))
	       (setf (representation-entity-invalid-p representation) t)
	       (incf count)))))
    (let ((total-count (resources-count cache)))
      (log-event :normal "Proxy Cache: Scanning ~D resources to verify entity validity..." total-count)
      (map-representations cache #'verify-entity-validity :read)
      (if (zerop count)
	  (log-event :normal "Proxy Cache: No representations found with invalid entities.")
	  (log-event :normal "Proxy Cache: ~D representations found with invalid entities and invalidated." count))))
  cache)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;DATABASE.LISP.81")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defgeneric handle-invalidate (handle)
  (:documentation "Invalidates HANDLE so that it cannot be used to access invalid data."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;DATABASE.LISP.81")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defgeneric handle-valid-p (handle &optional verify-p)
  (:documentation "Returns non-null if HANDLE points to valid data.
When VERIFY-P is non-null, this actively checks to make sure the handle is valid."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;DATABASE.LISP.81")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(PROGN
;;file-stream-length-in-bytes on a stream doesn't work in Genera   5/9/2000 -- JCMa.
#+Genera
(defmethod database-close :after (stream (handle filesystem-handle) (direction (eql :output)) &optional abort-p)
  (declare (ignore stream))
  (if abort-p
      (handle-valid-p handle t)
      (let ((pathname (filesystem-handle-pathname handle)))
	(setf (%handle-object-size handle) (file-length-in-bytes pathname)))))

)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;REPRESENTATION.LISP.60")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defgeneric representation-entity-handle-valid-p (representation &optional verify-p)
  (:documentation "Returns non-null if the entity handle for REPRESENTATION points to valid data.
When VERIFY-p is non-null, this Actively verifies the entity handle validity."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;REPRESENTATION.LISP.60")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod representation-entity-handle-valid-p ((representation entity-persistence-mixin) &optional verify-p)
  (handle-valid-p (representation-entity-handle representation) verify-p))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY-CACHE.LISP.84")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun proxy-respond-while-caching-entity (resource url request-headers response-headers http-status http-version
						    remote-stream request-stream transfer-encoding)
  (flet ((handle-eof-error-on-optional-entity (representation response-headers)
	   (setf (server-close-connection-p *server*) t)	;close connection to maintain syncrhonization
	   (%representation-clean-up-incomplete-cache-of-entity representation response-headers)))
    (let* ((server *server*)
	   (resource (or resource (intern-resource server (name-string url) :if-does-not-exist :create)))
	   (current-access-time (server-request-time server)))
      (multiple-value-bind (representation)
	  (intern-representation resource request-headers :if-does-not-exist :create)
	;; if someone else gets this write lock first we just refetch 5/5/2000 -- JCMa.
	(with-lock-held ((cache-object-lock representation) :write "Cache Entity")
	  (proxy-trace "~&;Caching data for ~S" representation)
	  (setf (representation-valid-p representation) nil
		(representation-entity-invalid-p representation) t)
	  (handling-optional-entity-for-status-code
	    (http-status response-headers :clean-up-form (handle-eof-error-on-optional-entity representation response-headers))
	    (with-binary-stream (request-stream :output)
	      (with-binary-stream (remote-stream :input)
		(with-transfer-encoding (request-stream transfer-encoding)
		  (with-entity-data-stream (entity-stream representation :output)
		    (let ((broadcast (make-broadcast-stream request-stream entity-stream)))
		      (declare (dynamic-extent broadcast))
		      (with-transfer-decoding* (remote-stream url http-version :headers response-headers :copy-mode :binary)
			(stream-copy-until-eof remote-stream broadcast :binary)))))
		(setf (representation-entity-invalid-p representation) (not (representation-entity-handle-valid-p representation))))))
	  ;; update date after successful cache.
	  (setf (representation-response-headers representation) (proxy-header-plist response-headers))	;set response headers
	  (representation-note-transaction representation http-status http-version current-access-time request-headers)
	  (setf (representation-last-reference representation) current-access-time
		(representation-last-update representation) current-access-time
		(representation-unsaved-metadata representation) current-access-time)
	  ;; the last form must set the validity of representation
	  (setf (representation-valid-p representation) t))
	;; Arrange for persistent storage of new metadata outside of write lock
	(note-metadata-update representation current-access-time)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY-CACHE.LISP.84")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod proxy-revalidate-representation ((representation representation) &optional (revalidate-time (get-universal-time)))
  (let ((url (intern-url (uri-string representation) :if-does-not-exist :uninterned))
	(request-headers (representation-request-headers representation)))
    (flet ((%write-request-headers (stream method http-version)
	     (declare (ignore method http-version))
	     (let* ((origin-server-date (and (representation-valid-p representation) (representation-origin-server-date representation)))
		    (modification-plist (when origin-server-date `(:if-modified-since ,origin-server-date))))
	       (declare (dynamic-extent modification-plist))
	       (write-modified-headers request-headers stream modification-plist *hop-by-hop-headers* nil)
	       (when *trace-proxy*
		 (format *trace-output* "~&Request Headers:~&")
		 (write-modified-headers request-headers *trace-output* modification-plist *hop-by-hop-headers* nil)))))
      (declare (dynamic-extent #'%write-request-headers))
      (proxy-trace "~&;--------------------~&;Revalidate: ~A" (name-string url))
      (with-http-request  (url :get :request-headers #'%write-request-headers)
	(let* ((client-status (client-status client))
	       (client-http-version (client-connection-version client))
	       (response-headers (client-response-headers client)))
	  (when *trace-proxy*
	    (format *trace-output* "~&;Protocol: ~A~&;Status: ~D~&;Response Headers:~&"
		    client-http-version client-status)
	    (write-header-buffer response-headers *trace-output*))
	  (cond
	    ((member client-status *cacheable-response-status-codes*)
	     ;; uncacheable client requests should never get into the cache. 5/12/2000 -- JCMa.
	     (cond ((proxy-cacheable-server-response-p client-status http-version response-headers)
		    ;; if someone else gets this write lock first we just refetch 5/5/2000 -- JCMa.
		    (with-lock-held ((cache-object-lock representation) :write "Cache Entity")	
		      (proxy-trace "~&;Caching data for ~S" representation)
		      (setf (representation-valid-p representation) nil
			    (representation-entity-invalid-p representation) t)
		      (handling-optional-entity-for-status-code
			(client-status response-headers
				       :clean-up-form (%representation-clean-up-incomplete-cache-of-entity representation response-headers))
			(with-binary-stream (remote-stream :input)
			  (with-entity-data-stream (entity-stream representation :output)
			    (with-transfer-decoding* (remote-stream url http-version :headers response-headers :copy-mode :binary)
			      (stream-copy-until-eof remote-stream entity-stream :binary)))
			  (setf (representation-entity-invalid-p representation) (not (representation-entity-handle-valid-p representation)))))
		      ;; update date after successful cache.
		      (setf (representation-response-headers representation) (proxy-header-plist response-headers))	;set response headers
		      (representation-note-transaction representation client-status http-version revalidate-time)
		      (setf (representation-last-update representation) revalidate-time)
		      ;; If new cache entry, set the last reference to the creation date
		      (unless (representation-last-reference representation)
			(setf (representation-last-reference representation) (cache-object-creation-date representation)))
		      (setf (representation-unsaved-metadata representation) revalidate-time)
		      ;; the last form must set the validity of representation
		      (setf (representation-valid-p representation) t))
		    ;; Arrange for persistent storage of new metadata outside of write lock
		    (note-metadata-update representation revalidate-time))
		   (t (proxy-trace "~&;Revalidate: Ignoring and decaching uncacheable entity for status ~S response." client-status)
		      (with-transfer-decoding* (remote-stream url http-version :headers *headers*)
			(advance-input-buffer remote-stream (get-header :content-length response-headers)))
		      (unintern-representation representation))))	;remove uncacheable representation
	    ((eql client-status 304)
	      (proxy-trace "~&;Revalidate: Caching metadata for status ~S response." client-status)
	      (let (verified-date)
		(with-lock-held ((cache-object-lock representation) :write "Cache Metadata")
		  ;; Bail out if more recent verification
		  (unless (and (setq verified-date (representation-verified-date representation))
			       (> revalidate-time verified-date))
		    (return-from proxy-revalidate-representation))
		  (setf (representation-valid-p representation) nil)
		  (representation-update-response-headers representation response-headers)	;update response headers
		  (representation-note-transaction representation client-status client-http-version revalidate-time)
		  (setf (representation-unsaved-metadata representation) revalidate-time)
		  (setf (representation-valid-p representation) t))
		(note-metadata-update representation revalidate-time)))	;arrange for persistent storage of new metadata
	    (t (when (response-status-code-implies-entity-p client-status)
		 (with-transfer-decoding* (remote-stream url http-version :headers *headers*)
		   (advance-input-buffer remote-stream (get-header :content-length response-headers))))
	       (case client-status
		 ((401 402 403 404)
		  (proxy-trace "~&;Revalidate: Ignoring metadata for status ~S response." client-status))
		 ((400)
		  (proxy-trace "~&;Revalidate: Broken request syntax ~S response.~&URL: ~S"
			       client-status (name-string url)))
		 (t (client-signal-http-code url client-status :get :headers response-headers
					     :reason (client-reason client) :version client-http-version))))))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;CLASS.LISP.31")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defclass filesystem-handle
	  (database-handle)
    ((type :reader handle-type :allocation :class)
     (pathname :initarg :pathname :accessor filesystem-handle-pathname)
     (valid-p :initform nil :initarg valid-p :accessor filesystem-handle-valid-p))
  (:documentation "Handle for a filesystem-database."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;DATABASE.LISP.83")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod handle-initialize ((handle filesystem-handle))
  (let ((pathname (filesystem-database-cache-directory (handle-database handle)))
	(uid (handle-uid handle))
	(type (handle-type handle)))
    ;; (setf (filesystem-handle-pathname handle) (make-pathname :name (proxy-uid-string uid) :type type :defaults pathname))
    (setf (filesystem-handle-pathname handle) (proxy-make-cache-pathname uid type pathname)
	  (filesystem-handle-valid-p handle) t)
    handle))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;DATABASE.LISP.83")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;; Simple-minded immediate expunge for LMFS. 
;; More efficient alternatives should be used when serious. 10/30/2000 -- JCMa.
(defmethod handle-object-delete ((handle filesystem-handle))
  (let ((pathname (filesystem-handle-pathname handle)))
    (when pathname
      (prog1 (handler-case			;speculative delete for better performance
	       #-Genera
	       (delete-file pathname)
	       #+Genera
	       (typecase pathname
		 (fs:lmfs-pathname
		   (delete-file pathname)
		   (fs:expunge-directory pathname))
		 (t (delete-file pathname)))
	       (file-error () nil))
	     (setf (%handle-object-size handle) 0
		   (filesystem-handle-valid-p handle) nil)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;DATABASE.LISP.83")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod handle-invalidate ((handle filesystem-handle))
  (setf (filesystem-handle-valid-p handle) nil))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;DATABASE.LISP.84")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod handle-valid-p ((handle filesystem-handle) &optional verify-p)
  (let ((valid-p (filesystem-handle-valid-p handle)))
    (cond ((not valid-p)
	   (if (and verify-p (probe-file (filesystem-handle-pathname handle)))
	       (setf (filesystem-handle-valid-p handle) t)
	       nil))
	  (verify-p
	   (or (probe-file (filesystem-handle-pathname handle))
	       (setf (filesystem-handle-valid-p handle) nil)))
	  (t valid-p))))

