;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.33
;;; Reason: Bring the preferences upto date with the changes in subnet security regimes.
;;; 
;;; DEFINE-PREFERENCE :READ-SUBNETS:  -
;;; DEFINE-PREFERENCE :WRITE-SUBNETS:  -
;;; DEFINE-PREFERENCE :ACCEPT-WRITE-METHODS:  -
;;; Written by JCMa, 3/21/00 05:21:35
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.17, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.32,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, HTTP Proxy Server 4.5,
;;; HTTP Client Substrate 3.5, Jcma 41, HTTP Client 49.6, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, W4 Constraint-Guide Web Walker 41.2,
;;; W4 Examples 13.0, Ivory Revision 5, VLM Debugger 329, Genera program 8.11,
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
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;PREFERENCES.LISP.35")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;PREFERENCES.LISP.35")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;; needs a subnet presentation type.   -cvince 1/6/97
;(define-preference :secure-subnets
;                   :name "Secure Subnets"
;                   :presentation-type '(w3p:null-or-type (w3p:sequence (w3p:bounded-string 15)))
;                   :prompt "Secure Subnets:"
;                   :default-value-form nil
;                   :get-value-form (mapcar #'ip-address-for-parsed-ip-address *secure-subnets*)
;                   :set-value-form (parse-secure-subnets value)
;                   :description "Defines default subnet security for all URLs exported by this server.  Should be a comma-separated
;list of IP addresses or None.")

(define-preference :read-subnets
   :name "Read Subnets"
   :presentation-type '(w3p:null-or-type (w3p:sequence (w3p:bounded-string 15)))
   :prompt "Read Subnets:"
   :default-value-form nil
   :get-value-form (mapcar #'ip-address-for-parsed-ip-address *read-subnets*)
   :set-value-form (parse-read-subnets value)
   :description "Defines default read subnet security for all URLs exported by this server.  Should be a comma-separated
list of IP addresses or None.")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;PREFERENCES.LISP.35")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(define-preference :write-subnets
		   :name "Write Subnets"
		   :presentation-type '(w3p:null-or-type (w3p:sequence (w3p:bounded-string 15)))
		   :prompt "Write Subnets:"
		   :default-value-form nil
		   :get-value-form (mapcar #'ip-address-for-parsed-ip-address *write-subnets*)
		   :set-value-form (parse-write-subnets value)
		   :description "Defines default write subnet security for all URLs exported by this server.  Should be a comma-separated
list of IP addresses or None.")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;PREFERENCES.LISP.35")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;;;------------------------------------------------------------------- 
;;;
;;; DEFINE THE CONFIGURATION PARAMETERS FOR THE SERVER
;;;

(define-preference :accept-write-methods
                   :name "Accept Write Methods"
                   :presentation-type `(w3p:member-sequence (:access-controlled :authenticated-users
                                                             :authenticated-users-on-write-subnets
                                                             :local-host :none :remote-host :write-subnets))
                   :prompt "Write Methods Accepted From Hosts:"
                   :default-value-form '(:local-host)
                   :get-value-form *accept-write-methods*
                   :set-value-form (setq *accept-write-methods* value)
                   :description "Controls the security policy for side-effecting methods such
as PUT or DELETE.  Each security policy imposes minimum requirements to invoke these methods. 
The following security policies are available.
ACCESS-CONTROLLED: Requires URLs to restrict users via either user authentication or subnet security.
AUTHENTICATED-USERS: Requires URLs to restrict users only via user authentication.
AUTHENTICATED-USERS-ON-WRITE-SUBNETS: Requires URLs to restrict users via both user authentication and write subnet security.
LOCAL-HOST: Requires users to be on the local host running the server.
NONE: No users can invoke side-effecting methods.
REMOTE-HOST: Does require URLs to control access, but respects any global or URL level access controls.
WRITE-SUBNETS: Requires URLs to restrict access to hosts trusted for write operations.")

