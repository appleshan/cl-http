;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: www-utils -*-
;;;
;;; (C) Copyright 2006, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; SSL UTILITIES
;;;

(in-package :www-utils)

;; Initialize the SSL libraries when this file is loaded.

(eval-when (:load-toplevel) 
  ;; This was a special version of libssl that we build for 64-bit on 10.4,
  ;; because Apple only supplied a 32-bit version at that time.  From 10.5
  ;; onwards, Apple ship their own 64-bit version.
  #+(and OpenSSL-64bit lispworks5.0)
  (comm:set-ssl-library-path "/usr/local/openssl64/lib/libssl.dylib")
  ;; Ensure the ssl libraries are loaded
  (comm:ensure-ssl))

(add-initialization "Initialize SSL Random Seed" '(comm:do-rand-seed) 
                    '(:normal) 'http:*server-launch-initialization-list*)

(defconstant *ssl-versions* '(:ssl-default :ssl-2-or-3 :ssl-3 :ssl-2 :tls-1)
  "The known SSL versions available as configuration parameters.")

(defun %convert-ssl-versions-for-lw (ssl-version)
  (ecase ssl-version
    (:ssl-default :default)
    (:ssl-2-or-3 :v23)
    (:ssl-3 :v3)
    (:ssl-2 :v2)
    (:tls-1 :tls-v1)))

(defconstant *ssl-cipher-sets* '(:all :high :medium :low :56-bit-export :export :40-bit-export)
  "The set of known keywords denoting cipher sets for SSL.")

(defun ssl-cipher-string (cipher-keyword-or-string)
  (etypecase cipher-keyword-or-string
    (keyword
     (ecase cipher-keyword-or-string
       (:all "ALL") ;all ciphers suites except the eNULL ciphers which must be explicitly enabled.
       (:high "HIGH") ;Currently means key lengths larger than 128 bits.
       (:medium "MEDIUM") ;Currently means ciphers using 128 bit encryption.
       (:low "LOW") ;Currently means ciphers using 64 or 56 bit encryption algorithms but excluding export cipher suites.
       (:56-bit-export "EXPORT56") ;56 bit export encryption algorithms
       (:export "EXPORT") ;export encryption algorithms. Including 40 and 56 bits algorithms.
       (:40-bit-export "EXPORT40"))) ;40 bit export encryption algorithms
    ;; should be validate OpenSSL cipher string. See: http://www.openssl.org/docs/apps/ciphers.html
    (string cipher-keyword-or-string)))

#+CAPI
(capi:define-interface gui-ask-user-ssl-server-key-pw-dialog ()
  ()
  (:panes
   (local-context-pane capi:title-pane)
   (password-pane capi:password-pane)
   (confirmation-pane capi:password-pane))
  (:layouts
   (main-layout capi:grid-layout
                '("Server:" local-context-pane 
                  "Password:" password-pane
                  "Confirm:" confirmation-pane)
                :columns 2))
  (:default-initargs
   :layout 'main-layout))

#+CAPI
(defmethod initialize-instance :after ((self gui-ask-user-ssl-server-key-pw-dialog)
                                       &key host port password)
  (with-slots (local-context-pane password-pane confirmation-pane) self
    (setf (capi:title-pane-text local-context-pane) (format nil "https://~A:~D" host port))
    (when password
      (setf (capi:text-input-pane-text password-pane) password
            (capi:text-input-pane-text confirmation-pane) password))))

#+CAPI
(defun gui-ask-user-ssl-server-key-pw-dialog-password (self)
  (with-slots (password-pane confirmation-pane) self
    (let ((password (capi:text-input-pane-text password-pane))
          (confirmation (capi:text-input-pane-text confirmation-pane)))
      (when (equal password confirmation)
        password))))

(defconstant *gui-ask-user-ssl-server-key-title* "SSL Launch: Provide Server Key Password")

#+CAPI
(defun gui-ask-user-ssl-server-key-pw (host port pwd)
  (loop with dialog = (make-instance 'gui-ask-user-ssl-server-key-pw-dialog
                                     :title *gui-ask-user-ssl-server-key-title*
                                     :host host :port port :password pwd)
        while (capi:popup-confirmer dialog nil)
        do (let ((pwd (gui-ask-user-ssl-server-key-pw-dialog-password dialog)))
             (when pwd
               (return-from gui-ask-user-ssl-server-key-pw (values pwd nil))))
        finally (return (values nil t))))

(defun ask-user-ssl-server-key-pw (host port pwd stream)
  #+CAPI
  (when (capi:screens)
    (return-from ask-user-ssl-server-key-pw
      (gui-ask-user-ssl-server-key-pw host port pwd)))
  (let (npwd)
    (fresh-line stream)
    (write-string *gui-ask-user-ssl-server-key-title* stream)
    (format stream "~&https://~A:~D" host port)
    (if pwd
        (format stream "~&Password (default: ~a): " (make-string (length pwd) :initial-element #\*) pwd)
      (format stream "~&Password: "))
    (setq npwd (read-line stream nil nil))
    (if (equal npwd "")
        (setq npwd pwd))
    (values (or npwd pwd) (not npwd))))

(defun http::get-ssl-server-private-key-password (host port &optional (stream *query-io*))
  (declare (values password abort-p))
  (ask-user-ssl-server-key-pw host port nil stream))

(declaim (special http::*ssl-implementation-version*))

(defun ssl-implementation (&optional recache-p)
  "Returns the SSL implementation and version as strings."
  (declare (values implementation version))
  (cond ((and (not recache-p) http::*ssl-implementation-version*)
         (values-list http::*ssl-implementation-version*))
        (t (let* ((string (comm:openssl-version))
                  (end (length string))
                  (pos1 (position #\space string :end end))
                  (type (subseq string 0 pos1))
                  (pos2 (position-if-not #'http::white-space-char-p string :start (1+ pos1) :end end))
                  (pos3 (position #\space string :start pos2 :end end))
                  (version (subseq string pos2 pos3)))
             ;; cache values
             (setq http::*ssl-implementation-version* (list type version))
             ;; return result
             (values type version)))))