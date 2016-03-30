;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Patch-File: T -*-
;;; Patch file for HTTP-CLIENT-SUBSTRATE version 4.2
;;; Reason: Move some FTP Lispm interface code into the client substrate from the server.
;;; 
;;; Function WWW-UTILS::HOST-ACCESS-PATH-FOR-HOST-P:  -
;;; Variable WWW-UTILS::*STANDARD-GET-USER-ID-AND-PASSWORD*:  -
;;; Variable WWW-UTILS::*NFS-AUTHENTICATION-FUNCTION*:  -
;;; Function WWW-UTILS::WITH-AUTOMATIC-LOGIN:  -
;;; Function WWW-UTILS:OPEN-WWW-UTILS-STREAM-TO-HOST:  use ensure-services.
;;; Function WWW-UTILS:FTP-DIRECTORY-INFO:  -
;;; Function WWW-UTILS:FTP-COPY-FILE:  -
;;; Function WWW-UTILS:FTP-COPY-FILE-TO-HTTP-STREAM:  -
;;; DEFINE-CONDITION HTTP::CLIENT-UNAUTHORIZED-FTP-ACCESS:  new.
;;; Function (CLOS:METHOD HTTP::AUTHENTICATION-HEADER-SPEC (HTTP::CLIENT-UNAUTHORIZED-FTP-ACCESS)):  -
;;; Written by JCMa, 12/06/00 16:34:59
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
;;; HTTP Proxy Server 6.7, HTTP Server 70.93, Showable Procedures 36.3,
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing),
;;; Pht debugging patch (from W:>reti>pht-debugging-patch.lisp.4),
;;; Cname level patch (from W:>reti>cname-level-patch).

;;; Patch file for HTTP-CLIENT-SUBSTRATE version 4.2
;;; Written by JCMa, 12/07/00 18:51:07
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
;;; HTTP Proxy Server 6.9, HTTP Server 70.95, Showable Procedures 36.3,
;;; Binary Tree 34.0, W3 Presentation System 8.1, HTTP Client Substrate 4.3,
;;; HTTP Client 50.1, Experimental Genera 8 5 Patches 1.0,
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.7),
;;; Pht debugging patch (from W:>Reti>pht-debugging-patch.lisp.4),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1).


;;; Patch file for HTTP-CLIENT-SUBSTRATE version 4.2
;;; Written by JCMa, 12/07/00 16:18:54
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
;;; HTTP Proxy Server 6.9, HTTP Server 70.94, Showable Procedures 36.3,
;;; Binary Tree 34.0, W3 Presentation System 8.1, HTTP Client Substrate 4.2,
;;; HTTP Client 50.1, Experimental Genera 8 5 Patches 1.0,
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.7),
;;; Pht debugging patch (from W:>Reti>pht-debugging-patch.lisp.4),
;;; Cname level patch (from W:>reti>cname-level-patch),
;;; Change random host default (from W:>reti>change-random-host-default.lisp.3),
;;; Fix FTP Directory List for Default Hosts (from W:>Reti>fix-ftp-directory-list.lisp.5).


;;; Patch file for HTTP-CLIENT-SUBSTRATE version 4.2
;;; Written by JCMa, 12/06/00 18:43:00
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
;;; HTTP Proxy Server 6.8, HTTP Server 70.94, Showable Procedures 36.3,
;;; Binary Tree 34.0, W3 Presentation System 8.1, HTTP Client Substrate 4.2,
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
;;; Background dns refreshing (from W:>reti>background-dns-refreshing),
;;; Pht debugging patch (from W:>reti>pht-debugging-patch.lisp.4),
;;; Cname level patch (from W:>reti>cname-level-patch).




(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:LISPM;CLIENT;LISPM.LISP.38"
  "HTTP:CLIENT;CLIENT.LISP.268"
  "HTTP:CLIENT;CLIENT.LISP.267"
  "HTTP:LISPM;CLIENT;LISPM.LISP.41")

(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL)
  (SCT:REQUIRE-PATCH-LEVEL-FOR-PATCH '(CL-HTTP 70. 94.)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;CLIENT;LISPM.LISP.38")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: ansi-common-lisp; Base: 10; Package: www-utils; Mode: LISP -*-")
(eval-when (load eval)
  (mapcar #'(lambda (x)
	      (let ((sym (scl:intern-local-soft x :http)))
		(when sym
		  (unintern sym :http))))
	  '("OPEN-HTTP-STREAM-TO-HOST" "FTP-DIRECTORY-INFO" "FTP-COPY-FILE" "FTP-COPY-FILE-TO-HTTP-STREAM"))

  (mapcar #'(lambda (x)
	      (export (intern x :www-utils) :www-utils))
	  '("OPEN-HTTP-STREAM-TO-HOST" "FTP-DIRECTORY-INFO" "FTP-COPY-FILE" "FTP-COPY-FILE-TO-HTTP-STREAM"))
  )

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;CLIENT;LISPM.LISP.38")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: ansi-common-lisp; Base: 10; Package: www-utils; Mode: LISP -*-")

(PROGN

(defun ensure-services (host &rest services)
  (let* ((host-object (neti:parse-host host))
	 (host-services (scl:send host-object :get :service))
	 (new-services (loop for service in services
			     unless (member service host-services :test #'equal)
			       collect service)))
    (when new-services
      (let* ((uninterned (scl:send host-object :uninterned-p))
	     (primary-name (scl:send host-object :primary-name))
	     (primary-namespace (and (not uninterned) (scl:send primary-name :namespace))))
	(scl:send host-object :putprop (append host-services new-services) :service)
	(unless uninterned
	  ;; Don't use update-object-permanently on uninterned hosts as it will
	  ;; intern the host which can cause all sorts of problems later on ...
	  (neti:update-object-permanently :host primary-namespace primary-name
					  (scl:send host-object :property-list)
					  t))))
    host-object))

(net:define-protocol :http (:http :tcp)
  (:invoke (access-path)
    (net:get-connection-for-service access-path :translation :modal :characters t)))

(declaim (inline deallocate-client-http-stream))

;; a no-op on the lisp machine but required for platforms such as the mac.
;; 8/13/96 -- JCMa.
(defun deallocate-client-http-stream (stream)
  (declare (ignore stream)))

(declaim (special http::*client-timeout*))

(defun open-http-stream-to-host (host port)
  (declare (values stream))
  (let ((host-object (ensure-services host '(:http :tcp :http))))
    (with-tcp-port-for-protocol (:http port)
      (let ((tcp:*tcp-connect-timeout* http::*client-timeout*))
	(neti::invoke-service-on-host :http host-object)))))

;;;------------------------------------------------------------------- 
;;;
;;; LISPM CLIENT FTP INTERFACE
;;;

(defun host-access-path-for-host-p (access-path host)
  (let ((ahost (scl:send access-path :host))
        (rhost (parse-host host t)))
    (host-eq ahost rhost)))

(defvar *standard-get-user-id-and-password* #'fs:get-user-id-and-password)
(defvar *nfs-authentication-function* #'rpc:authentication-initialize)

(define-macro with-automatic-login ((host user-id user-pw) &body body)
  "Supplies userid and PW to ensure successul FTP login to host with BODY."
  `(labels ((standard-get-user-id-and-password (access-path host-user-id host-password condition)
              (funcall *standard-get-user-id-and-password* access-path host-user-id host-password condition))
            (nfs-authenticate (authentication-mixin host-user-id host-password)
              (funcall *nfs-authentication-function* authentication-mixin
                       (or host-user-id ,user-id)
                       (or host-password ,user-pw)))
            (automatic-get-user-id-and-password
               (access-path host-user-id host-password condition)
              (cond ((and (null host-user-id)
                          (null host-password)
                          (host-access-path-for-host-p access-path ,host))
                     (values ,user-id ,user-pw))
                    (t (standard-get-user-id-and-password access-path host-user-id host-password condition)))))
     (scl:letf ((#'fs:get-user-id-and-password #'automatic-get-user-id-and-password)
                (#'rpc:authentication-initialize #'nfs-authenticate))
       ,@body)))

(define ftp-copy-file (from-pathname to-stream &key (element-type 'character) (port 21)
                                     (user-id "anonymous") (user-pw (server-mail-address)))
  "Copies the content of FROM-PATHNAME to TO-STREAM. 
If a network error is encountered, this returns NIL, otherwise T.
ELEMENT-TYPE is the ANSI file openning argument."
  (declare (values success-p))
  (let ((host (pathname-host from-pathname)))
    (ensure-services host '(:file :tcp :tcp-ftp))
    (with-tcp-port-for-protocol (:ftp port)
      (let ((tcp:*tcp-connect-timeout* http::http::*client-timeout*))
	(with-automatic-login (host user-id user-pw)
	  (with-open-file (ftp-stream from-pathname :direction :input :element-type element-type)
	    (http::stream-copy-until-eof ftp-stream to-stream (case element-type
								(character :text)
								(t :binary)))
	    (values t)))))))

)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CLIENT.LISP.268")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(define-condition client-unauthorized-ftp-access
                  (recoverable-unauthorized-client-access)
  ((reason :initform "Unauthorized FTP Access")))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CLIENT.LISP.267")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod authentication-header-spec ((condition client-unauthorized-ftp-access))
  `(:www-authenticate (,(http-authentication-method condition) ,(http-authentication-realm condition))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;CLIENT;LISPM.LISP.41")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: ansi-common-lisp; Base: 10; Package: www-utils; Mode: LISP -*-")

(define ftp-copy-file-to-http-stream (from-pathname http-stream &key (port 21) data-type
						    url additional-headers
						    (user-id "anonymous") (user-pw (server-mail-address)))
  "Copies the content of FROM-PATHNAME to HTTP-STREAM."
  (declare (values success-p))
  (flet ((handle-invalid-ftp-user-id-and-password (&rest ignore)
	   (declare (ignore ignore))
	   (signal 'http::client-unauthorized-ftp-access :url url :method :get
		   :authentication-realm "FTP Server" :authentication-method :basic)))
    (declare (dynamic-extent #'handle-invalid-ftp-user-id-and-password))
    (let* ((host (pathname-host from-pathname))
	   (copy-mode (or (url::%content-type-copy-mode data-type nil) :binary))
	   (element-type (ecase copy-mode
			   (:text 'character)
			   ((:binary :crlf) '(unsigned-byte 8))))
	   (*standard-get-user-id-and-password* #'handle-invalid-ftp-user-id-and-password))
      (ensure-services host '(:file :tcp :tcp-ftp))
      (with-tcp-port-for-protocol (:ftp port)
	(let ((tcp:*tcp-connect-timeout* http::http::*client-timeout*))
	  (with-automatic-login (host user-id user-pw)
	    (with-open-file (ftp-stream from-pathname :direction :input :element-type element-type)
	      (http:with-successful-response (http-stream (case data-type ((:unknown nil) :text) (t data-type))
							  :location url :additional-headers additional-headers)
		(case copy-mode
		  (:text
		    (with-text-stream (http-stream :output)
		      (http::stream-copy-until-eof ftp-stream http-stream :text)))
		  ((:binary :crlf)
		   (with-binary-stream (http-stream :output)
		     (http::stream-copy-until-eof ftp-stream http-stream :binary))))
		(values t)))))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;CLIENT;LISPM.LISP.41")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: ansi-common-lisp; Base: 10; Package: www-utils; Mode: LISP -*-")

(define ftp-directory-info (directory &optional (port 21) url (user-id "anonymous") (user-pw (server-mail-address)))
  "Returns a list of pathname spec for directory just like DIRECTORY-INFO.
If a network error is encountered, this returns NIL."
  (declare (values directory-listing directory-exists-p))
  (flet ((handle-invalid-ftp-user-id-and-password (&rest ignore)
	   (declare (ignore ignore))
	   (signal 'http::client-unauthorized-ftp-access :url url :method :get
		   :authentication-realm "FTP Server" :authentication-method :basic)))
    (declare (dynamic-extent #'handle-invalid-ftp-user-id-and-password))
    (let* ((path (pathname directory))
	   (host (pathname-host path))
	   (*standard-get-user-id-and-password* #'handle-invalid-ftp-user-id-and-password))
      (ensure-services host '(:file :tcp :tcp-ftp))
      (with-tcp-port-for-protocol (:ftp port)
	(let ((tcp:*tcp-connect-timeout* http::http::*client-timeout*))
	  (with-automatic-login (host user-id user-pw)
	    ;; ansi CL directory fails due to :fast option  3/13/94 -- JCMa.
	    (let ((path (make-pathname :defaults directory))
		  (listing (cdr (fs:directory-list (make-pathname :defaults directory) :sorted))))
	      (if listing
		  (values listing t)
		  (values nil (ignore-errors (open path :direction :probe-directory :if-does-not-exist nil)))))))))))

