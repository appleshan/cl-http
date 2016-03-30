;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: www-utils -*-

;;; Copyright (c) 1987--2003 Xanalys LLC. All rights reserved.
;;;


;;;------------------------------------------------------------------- 
;;;
;;; CODE TO SET FILE TIMES AND AUTHORS
;;;
;;;  SET-FILE-DATES file &key creation modification access
;;;  SET-FILE-OWNERS file &key author group

(in-package :www-utils)

#+unix
(fli:define-c-typedef time_t :int)

#+unix
(fli:define-c-struct utimbuf
  (actime time_t)
  (modtime time_t))

#+unix
(fli:define-foreign-function (c-utime utime)
    ((name (:reference-pass :lisp-string-array))
     (times (:pointer utimbuf)))
  :result-type :integer)

#+unix
(defconstant *time-til-70* 2208988800)

#+unix
(defun set-file-dates (file &key creation modification access)
  (declare (ignore creation)) ; makes no sense on UNIX
  (let* ((pathname (truename file))
         (filename (sys::os-namestring pathname))
         (stat (unless (and access modification)
                 (sys:get-file-stat pathname))))
    (fli:with-dynamic-foreign-objects ((buffer utimbuf))
      (setf (fli:foreign-slot-value buffer 'actime)
            (if access
                (- access *time-til-70*)
              (sys:file-stat-last-access stat)))
      (setf (fli:foreign-slot-value buffer 'modtime)
            (if modification
                (- modification *time-til-70*)
              (sys:file-stat-last-modify stat)))
      (unless (zerop (c-utime filename buffer))
        (report-unix-error 'set-file-dates (lw:errno-value) pathname)))))

#+unix
(defun report-unix-error (function errno pathname)
  (error "Failed to ~A file ~A: ~A(~A)."
         function
         pathname
         (lw:get-unix-error errno)
         errno))


#+unix
(fli:define-foreign-function (c-chown chown)
    ((name (:reference-pass :lisp-string-array))
     (uid :integer)
     (gid :integer))
  :result-type :fixnum)

#+unix
(defun set-file-owners (file &key author group)
  (let* ((pathname (truename file))
         (filename (sys::os-namestring pathname))
         (uid (cond ((null author) -1)
                    ((integerp author) author)
                    (t (or (and (stringp author)
                                (sys::get-user-entry author :user-id))
                           (error "User name ~S not known." author)))))
         (gid (cond ((null group) -1)
                    ((integerp group) group)
                    (t (error "Non integer group name ~S not supported."
                              group)))))
    (unless (zerop (c-chown filename uid gid))
      (report-unix-error 'set-file-owners (lw:errno-value) pathname))))

#+link-load (link-load:read-foreign-modules "-lc")

;; WINDOWS IMPLEMENATION

#+WIN32
(fli:define-c-struct _FILETIME
  (dw-low-date-time (:UNSIGNED :LONG))
  (dw-high-date-time (:UNSIGNED :LONG)))

#+Win32
(fli:define-c-typedef FILETIME
  (:struct _FILETIME))

#+Win32
(fli:define-foreign-function (set-file-time "SetFileTime")
    ((h-file win32:handle)
     (lp-creation-time (:pointer filetime))
     (lp-last-access-time (:pointer filetime))
     (lp-last-write-time (:pointer filetime)))
  :result-type win32:bool)

#+Win32
(defconstant +filetime-1900+ 94354848000000000)

#+Win32
(defun set-filetime-from-universal-time (utime filetime)
  (let ((filetime-value (+ (* utime 10000000) +filetime-1900+)))
    (setf (fli:foreign-slot-value filetime 'dw-high-date-time)
          (ash filetime-value -32))
    (setf (fli:foreign-slot-value filetime 'dw-low-date-time)
          (logand filetime-value #xffffffff))))

#+Win32
(defun set-file-dates (file &key creation modification access)
  (let ((pathname (truename file)))
    (fli:with-dynamic-foreign-objects ((creation-filetime filetime)
                                       (modification-filetime filetime)
                                       (access-filetime filetime))
      (when creation
        (set-filetime-from-universal-time creation creation-filetime))
      (when modification
        (set-filetime-from-universal-time modification modification-filetime))
      (when access
        (set-filetime-from-universal-time access access-filetime))
      (unless (with-open-file (stream pathname
                                      :direction :output
                                      :if-exists :overwrite
                                      :element-type '(unsigned-byte 8))
                (set-file-time (io:file-stream-file-handle
                                stream)
                               (and creation creation-filetime)
                               (and access access-filetime)
                               (and modification modification-filetime)))
        (error "Failed to set file dates on ~A:~A."
               file
               (win32:get-last-error-string))))))

#+Win32
(defun set-file-owners (file &key author group)
  (declare (ignore file author group))
  ;; authors not implemented
  nil)

