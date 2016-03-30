;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.30
;;; Reason: Function (CLOS:METHOD HTTP::PUT-DOCUMENT (URL:HTTP-OBJECT T)):  delete the CRLF files after decoding in PUT.
;;; Written by JCMa, 3/20/00 02:38:59
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.29,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, HTTP Proxy Server 4.5,
;;; HTTP Client Substrate 3.3, Jcma 41, HTTP Client 49.4, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, Ivory Revision 5, VLM Debugger 329,
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
  "HTTP:SERVER;SERVER.LISP.829")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.829")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod put-document ((url url:http-object) stream &optional newly-created-url-p (check-versions-p t))
  (declare (values url exported))
  (macrolet ((with-crlf-environment ((url pathname) &body body)
               `(flet ((delete-document-file (pathname)
			 (let ((probe-file (probe-file pathname)))
			   (when probe-file
			     (delete-file probe-file)))))
		  (let* ((content-type (get-header :content-type *headers*))
			 (copy-mode (if content-type
					(mime-content-type-copy-mode content-type)
					(url:copy-mode ,url))))
		    (case copy-mode
		      (:crlf
			(let ((crlf-pathname (crlf-pathname ,pathname))
			      modification-date)
			  ,.(subst 'crlf-pathname pathname body)
			  (decode-crlf-file crlf-pathname)
			  (set-file-author ,pathname server nil)
			  (setq modification-date (www-utils:file-modification-date crlf-pathname))
			  #+Genera (delete-document-file crlf-pathname)
			  (values url (if newly-created-url-p :created :modified) modification-date)))
		      (t ,@body
			 (set-file-author ,pathname server nil)
			 (values url (if newly-created-url-p :created :modified) (www-utils:file-modification-date ,pathname))))))))
    (let ((pathname (url:translated-pathname url))
          (server *server*)
          (headers *headers*)
          bytes transfer-encoding)
      ;; throw out of http transaction if a conflict is detected
      (when check-versions-p
        (let ((version (document-version pathname)))
          (case (server-http-version server)
            ((:http/0.9 :http/1.0)
             (check-derived-from-version url version))
            (t (check-if-match-precondition version t :put headers) 
               (check-if-unmodified-since-precondition version :put headers)))))
      ;; check for byte ranges in 1.1
      (when (get-header :content-range headers)
        (error 'server-not-implemented :url url :method :put
               :format-string "Putting byte ranges is not implemented."))
      (cond ((setq transfer-encoding (get-header :transfer-encoding headers))
             (case transfer-encoding
               #+(or Genera MCL LispWorks)
               (:chunked
                 (with-successful-put-response (server stream)
                   (with-crlf-environment (url pathname)
                                          (with-chunked-transfer-decoding (stream :headers headers)
                                            (stream-copy-until-eof stream pathname copy-mode)))))
               (t (error 'server-not-implemented :close-connection t :url url :method :put
                         :format-string "The HTTP transfer encoding, ~A, is not implemented."
                         :format-args (list transfer-encoding)))))
            ((and (setq bytes (get-header :content-length headers)))
             (handler-case-if (not *debug-server*) 
                (with-successful-put-response (server stream)
                  (with-crlf-environment (url pathname)
                                         (stream-copy-bytes stream pathname bytes copy-mode)))
               (error (err)
                      (error 'error-handling-put-method :url url :method :put :server-error err :headers (header-plist)
                             :format-string "Error executing PUT method for ~A."
                             :format-args (list (url:name-string url))))))
            (t (error 'content-length-required :url url :method :put
                      :format-string "no content-length header provided for ~A."
                      :format-args (list (url:name-string url))))))))

