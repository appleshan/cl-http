;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.195
;;; Reason: Function (CLOS:METHOD HTTP:INTERN-USER (HTTP::STANDARD-REALM-USER-MIXIN STRING)):  Ensure thread safety on creation of users
;;; Variable HTTP::*USER-NAME-CACHE-KEYS*:  -
;;; Function HTTP::USER-CLEAR-NAME-CACHES:  new
;;; Function (CLOS:METHOD HTTP::USER-CLEAR-NAME-CACHES (HTTP::USER)):  implement
;;; Function HTTP::NOTE-USER-NAME-CACHE-KEY:  new
;;; Function (CLOS:METHOD HTTP::%RENAME-USER (HTTP::STANDARD-REALM-USER-MIXIN HTTP::USER STRING STRING)):  new.
;;; Function HTTP::RENAME-USER:  new method
;;; Function (CLOS:METHOD HTTP::RENAME-USER (HTTP::STANDARD-REALM-USER-MIXIN HTTP::USER STRING)):  implement.
;;; Written by jcma, 7/11/05 14:23:02
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/Plus-CL-HTTP-A-CSAIL-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Lock Simple 437.0, Version Control 405.0,
;;; Compare Merge 404.0, VC Documentation 401.0,
;;; Logical Pathnames Translation Files NEWEST, Metering 444.0,
;;; Metering Substrate 444.1, Conversion Tools 436.0, Hacks 440.0, CLIM 72.0,
;;; Genera CLIM 72.0, CLX CLIM 72.0, PostScript CLIM 72.0, CLIM Demo 72.0,
;;; CLIM Documentation 72.0, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.40, Genera 8 5 Macivory Support Patches 1.0,
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
;;; Genera 8 5 Fortran Patches 1.0, MAC 415.2, MacIvory Support 447.0,
;;; MacIvory Development 434.0, Color 427.1, Graphics Support 431.0,
;;; Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Color System Documentation 10.0, SGD Book Design 10.0, Color Demo 422.0,
;;; Images 431.2, Image Substrate 440.4, Statice Runtime 466.1, Statice 466.0,
;;; Statice Browser 466.0, Statice Server 466.2, Statice Documentation 426.0,
;;; Symbolics Concordia 444.0, Graphic Editor 440.0, Graphic Editing 441.0,
;;; Bitmap Editor 441.0, Graphic Editing Documentation 432.0, Postscript 436.0,
;;; Concordia Documentation 432.0, Joshua 237.6, Joshua Documentation 216.0,
;;; Joshua Metering 206.0, Jericho 237.0, C 440.0, Lexer Runtime 438.0,
;;; Lexer Package 438.0, Minimal Lexer Runtime 439.0, Lalr 1 434.0,
;;; Context Free Grammar 439.0, Context Free Grammar Package 439.0, C Runtime 438.0,
;;; Compiler Tools Package 434.0, Compiler Tools Runtime 434.0, C Packages 436.0,
;;; Syntax Editor Runtime 434.0, C Library Headers 434,
;;; Compiler Tools Development 435.0, Compiler Tools Debugger 434.0,
;;; C Documentation 426.0, Syntax Editor Support 434.0, LL-1 support system 438.0,
;;; Fortran 434.0, Fortran Runtime 434.0, Fortran Package 434.0, Fortran Doc 427.0,
;;; Pascal 433.0, Pascal Runtime 434.0, Pascal Package 434.0, Pascal Doc 427.0,
;;; HTTP Server 70.194, Showable Procedures 36.3, Binary Tree 34.0,
;;; Experimental W3 Presentation System 8.1, CL-HTTP Server Interface 54.0,
;;; HTTP Proxy Server 6.34, HTTP Client Substrate 4.23,
;;; W4 Constraint-Guide Web Walker 45.13, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.18, DEC OSF/1 V4.0 (Rev. 110),
;;; 1728x1003 24-bit TRUE-COLOR X Screen FUJI:1.0 with 224 Genera fonts (The Olivetti & Oracle Research Laboratory R3323),
;;; Machine serial number -2142637960,
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7),
;;; Get Xauthority pathname from user namespace object. (from W:>jcma>fixes>xauthority-pathname.lisp.2),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;AUTHENTICATION.LISP.159")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;AUTHENTICATION.LISP.159")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod intern-user ((realm standard-realm-user-mixin) (user string) 
                        &key (if-does-not-exist :error) password groups personal-name email-address 
                        &allow-other-keys
                        &aux user-object)
  (declare (values interned-user newly-created-p))
  (with-slots (user-table) realm
    (tagbody
      start
	 (cond ((setq user-object (gethash user user-table))
		(cond-every
		  (password (setf (user-password-digest user-object) (make-user-password-digest realm user password)))
		  (groups
		    (dolist (group groups)
		      (group-add-user (intern-group realm group :if-does-not-exist :create) user-object)))
		  (email-address (setf (user-email-address user-object) email-address))
		  (personal-name (setf (user-personal-name user-object) personal-name)))
		(return-from intern-user user-object))
	       (t (ecase if-does-not-exist
		    (:soft (return-from intern-user nil))
		    (:create 
		      (setq user-object (make-user realm user password email-address personal-name))
		      (with-realm-write-lock (realm)
			(cond ((gethash user user-table) (go start))	;someone else created the user
			      (t (setf (gethash user user-table) user-object))))
		      (dolist (group groups)
			(group-add-user (intern-group realm group :if-does-not-exist :create) user-object))
		      (return-from intern-user (values user-object t)))
		    (:error (error "There is no user, ~S, in the realm, ~S." user (realm-name realm)))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;AUTHENTICATION.LISP.159")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defvar *user-name-cache-keys* '(:qualified-name)
  "Holds the cache keys for any user caches containing the user name.")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;AUTHENTICATION.LISP.159")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(define-generic user-clear-name-caches  (user)
  (:documentation "Clears all caches containing the user name for  USER."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;AUTHENTICATION.LISP.159")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod user-clear-name-caches ((user user))
  (clear-cached-values user *user-name-cache-keys*))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;AUTHENTICATION.LISP.159")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(define note-user-name-cache-key (key)
  "Adds a new key to the known keys for user name caches."
  (pushnew key *user-name-cache-keys*))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;AUTHENTICATION.LISP.159")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;; must be called inside a with-realm-write-lock
(defmethod %rename-user ((realm standard-realm-user-mixin) (user user) (old-name string) (new-name string))
  (let ((user-table (realm-user-table realm))
        existing-user)
    (cond ((and (setq existing-user (gethash new-name user-table))
                (not (eq user existing-user)))
           (values existing-user nil))
          (t (setf (gethash new-name user-table) user)
             (setf (user-name user) new-name)
             (remhash old-name user-table)
             (user-clear-name-caches user)
             (values user t)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;AUTHENTICATION.LISP.159")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(define-generic rename-user (realm user new-name &optional if-exists)
  (declare (values user-object user-renamed-p))
  (:documentation "Renames USER to NEW-NAME in REALM.
If a user named new-name already exists in realm, IF-EXISTS controls
the action. IF-EXISTS can be:

   :SOFT Return the values (USER-OBJECT-WITH-NEW-NAME NIL)
   :ERROR  Signal an error."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;AUTHENTICATION.LISP.159")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod rename-user ((realm standard-realm-user-mixin) (user user) (new-name string) &optional (if-exists :soft))
  (multiple-value-bind (existing-user renamed-p)
      (with-realm-write-lock (realm)
        (%rename-user realm user (user-name user) new-name))
    (cond (existing-user
           (ecase if-exists
             (:soft (values existing-user renamed-p))
             (:error (error "A user named, ~S, already exists, in the realm, ~S." new-name (realm-name realm)))))
          (t (values user renamed-p)))))

