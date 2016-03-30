;;; -*- Mode: Lisp; Package: FTP; Syntax: Ansi-Common-Lisp; -*-
;;;
;;; Copyright 2006, John C. Mallery. 
;;; All rights reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; BASIC FTP CLIENT
;;;

(in-package :ftp)

(defmethod ftp-local-directory ((connection ftp-connection))
  (or (http:get-value connection :local-directory)
      (setf (http:get-value connection :local-directory) (user-homedir-pathname))))

(defmethod (setf ftp-local-directory) ((directory pathname) (connection ftp-connection))
  (setf (http:get-value connection :local-directory) (make-pathname :name nil :type nil :version nil :defaults directory)))

(defparameter *command-table*
  '((:help ftp-help "Print help")
    (:ascii ftp-ascii "ASCII Transfer Type: ascii")
    (:binary ftp-binary "Binary Transfer Type: binary [byte-size]")
    (:cd ftp-cd "Change Directory: cd [dir]")
    (:connect ftp-connect "Connect To Host: connect [host port user-id password]")
    (:delete ftp-dele "Delete File:delete remote-file")
    (:dir ftp-dir "Directory Full List: dir [directory-name]")
    (:exit ftp-exit "Exit the client")
    (:feat ftp-feat "Display FTP extensions.")
    (:get ftp-get "Get File: get remote-file [local-name]")
    (:lcd ftp-lcd "Local Change Directory: lcd [dir]")
    (:lpwd ftp-lpwd "Local Print Working Directory")
    (:ls ftp-list "Directory List: ls [directory-name]")
    (:mls ftp-mlist "Machine List: mls [file-or-directory-name]")
    (:mkdir ftp-mkdir "Make Directory: mkdir remote-directory")
    (:opt ftp-opt "Command Options: opt ftp-command-name")
    (:ping ftp-ping "Ping FTP Server:ping")
    (:put ftp-put "Put File: put local-file [remote-file]")
    (:pwd ftp-pwd "Print Working Directory: pwd")
    (:rmdir ftp-rmd "Delete Directory: rmdir remote-directory")
    (:rn ftp-rn "Rename Pathname: rm from-pathname to-pathname")
    (:size ftp-size "File Size: size remote-file.")
    (:status ftp-status "Status: stat [file-or-directory-name]")
    (:type ftp-type "Set Transfer Type: type [binary-or-text]")
    (:view ftp-view "View File: view remote-file")))

(mapc #'(lambda (x) (tokenize-keyword (symbol-name (car x)))) *command-table*)

(mapc #'(lambda (x) (tokenize-keyword (symbol-name x))) '(:binary :text))

(defun ftp-help (connection args &optional (stream *standard-output*))
  (declare (ignore connection args))
  (loop initially (format stream "~&CL-FTP Client Commands:")
        for (cmd function docs) in *command-table*
        do (format stream "~&~A: ~A (~S)~%" cmd docs function)))

(defun ftp-ascii (connection args &optional (stream *standard-output*))
  (declare (ignore args))
  (send-transfer-type-command connection :text)
  (format stream "~&Transfer type set to ASCII." ))

(defun ftp-binary (connection args &optional (stream *standard-output*))
  (declare (ignore args))
  (send-transfer-type-command connection :binary)
  (format stream "~&Transfer type set to binary." ))

(defun ftp-type (connection args &optional (stream *standard-output*))
  (destructuring-bind (&optional mode) args
    (cond ((null mode)
           (let ((mode (ftp-transfer-type connection)))
             (format stream "~&Using ~A mode to transfer files." 
                     (case mode
                       (:text :ascii)
                       (t mode)))))
          (t (case (tokenize-keyword mode 0 (length mode) :soft)
               ((:text :ascii) (ftp-ascii connection args stream))
               (:binary (ftp-binary connection args stream))
               (t (format stream "~&Unkown Transfer type: ~A" mode )))))))

(defun ftp-cd (connection args &optional (stream *standard-output*))
  (flet ((report-directory (code line start end)
           (declare (ignore code))
           (fast-format stream "~&Remote directory is now: ~I" 
                        (write-string line stream :start start :end end))))
    (declare (dynamic-extent #'report-directory))
    (destructuring-bind (&optional (directory "/")) args
      (if (string-equal ".." directory)
          (send-change-to-parent-directory-command connection)
        (send-change-directory-command connection directory))
      (send-current-directory-command connection #'report-directory))))

(defun ftp-connect (connection args &optional (stream *standard-output*))
  (destructuring-bind (&optional host port user-id password) args
    (cond-every
     (host (setf (ftp-host connection) host))
     (port (setf (ftp-port connection) port))
     (user-id (setf (ftp-user-id connection) user-id))
     (password (setf (ftp-password connection) password)))
    (connect connection stream)))

(defun ftp-exit (connection args &optional (stream *standard-output*))
  (declare (ignore args))
  (flet ((report-quit (code line start end)
           (declare (ignore code))
           (fresh-line stream)
           (write-string line stream :start start :end end)))
    (declare (dynamic-extent #'report-quit))
    (send-logout-command connection (and stream #'report-quit))
    (throw 'ftp-exit t)))

(defun ftp-dele (connection args &optional (stream *standard-output*))
  (destructuring-bind (file) args
    (cond ((yes-or-no-p "~&Delete the file, ~A? " file)
           (send-delete-file-command connection file)
           (format stream "~&File deleted: ~A" file))
          (t (format stream "~&File not deleted: ~A" file)))))

(defun ftp-get (connection args &optional (stream *standard-output*))
  (destructuring-bind (remote-file &optional local-file) args
    (if (ftp-get-file connection 
                  remote-file
                  (merge-pathnames (or local-file remote-file) (ftp-local-directory connection))
                  :mode (ftp-transfer-type connection))
        (format stream "~&File transfer completed.")
      (format stream "~&File transfer failed."))))

(defun ftp-feat (connection args &optional (stream *standard-output*))
  (declare (ignore args))
  (flet ((write-features (code line start end)
           (declare (ignore code))
           (fresh-line stream)
           (write-string line stream :start start :end end)))
    (declare (dynamic-extent #'write-features))
    (handler-case
        (send-features-command connection #'write-features)
      (ftp-command-not-implemented () (format stream "~&The FTP server does not support the FEAT command.")))))

(defun ftp-lcd (connection args &optional (stream *standard-output*))
  (destructuring-bind (&optional (directory #-MCL "/" #+MCL "")) args
    (let* ((path (pathname directory))
           (defaults (ftp-local-directory connection)))
      (setf (ftp-local-directory connection) (if (pathname-directory path)
                                               (merge-pathnames path nil)
                                               (make-pathname :directory (append (pathname-directory defaults) (list (pathname-name path)))
                                                              :defaults defaults)))
      (format stream "~&Local directory is now: ~A" (ftp-local-directory connection)))))

(defun ftp-list (connection args &optional (stream *standard-output*))
  (destructuring-bind (&optional (directory :current-directory)) args
    (send-directory-brief-list-command connection stream directory)))

(defun ftp-dir (connection args &optional (stream *standard-output*))
  (destructuring-bind (&optional (directory :current-directory)) args
    (send-directory-list-command connection stream directory)))

(defun ftp-lpwd (connection args &optional (stream *standard-output*))
  (declare (ignore args))
  (format stream "~&Local directory: ~A" (ftp-local-directory connection)))

(defun ftp-mlist (connection args &optional (stream *standard-output*))
  (flet ((write-mlst-info (code line start end)
           (declare (ignore code))
           (fresh-line stream)
           (write-string line stream :start start :end end)))
    (declare (dynamic-extent #'write-mlst-info))
    (destructuring-bind (&optional (directory :current-directory)) args
      (send-machine-list-command connection directory #'write-mlst-info))))

(defun ftp-mkdir (connection args &optional (stream *standard-output*))
  (flet ((report-directory (code line start end)
           (declare (ignore code))
           (fast-format stream "~&Directory created: ~I"
                        (write-string line stream :start start :end end))))
    (declare (dynamic-extent #'report-directory))
    (destructuring-bind (directory) args
      (send-create-directory-command connection directory #'report-directory))))

(defun ftp-opt (connection args &optional (stream *standard-output*))
  (flet ((write-options (code line start end)
           (declare (ignore code))
           (fresh-line stream)
           (write-string line stream :start start :end end)))
    (declare (dynamic-extent #'write-options))
    (destructuring-bind (&optional command) args
      (if command
          (handler-case
              (send-options-command connection command #'write-options)
            (ftp-command-not-implemented () (format stream "~&The FTP server does not support the OPT command.")))
        (format stream "~&A command name must be supplied.")))))

(defun ftp-ping (connection args &optional (stream *standard-output*))
  (declare (ignore args))
  (handler-case
      (prog1 
          (send-no-op-command connection)
        (format stream "~&FTP service available: server ready for commands."))
    (ftp-error (cond) (format stream "~&FTP service unavailable: ~A" (report-string cond)))
    (error (cond)  (format stream "~&~FTP service down: ~A" (report-string cond)))))

(defun ftp-passwd (connection args &optional (stream *standard-output*))
  (flet ((report-password (code line start end)
           (declare (ignore code))
           (fresh-line stream)
           (write-string line stream :start start :end end)))
    (declare (dynamic-extent #'report-password))
    (destructuring-bind (&optional (password (ftp-password connection) password-supplied-p)) args
      (send-password-command connection password #'report-password)
      (when password-supplied-p
        (setf (ftp-password connection) password)))))

(defun ftp-put (connection args &optional (stream *standard-output*))
  (destructuring-bind (local-file &optional remote-file) args
    (if (ftp-put-file connection 
                  (merge-pathnames local-file (ftp-local-directory connection))
                  (or remote-file local-file)
                  :mode (ftp-transfer-type connection))
        (format stream "~&File transfer completed.")
      (format stream "~&File transfer failed."))))

(defun ftp-pwd (connection args &optional (stream *standard-output*))
  (declare (ignore args))
  (flet ((report-directory (code line start end)
           (declare (ignore code))
           (fast-format stream "~&Remote directory: ~I"
                        (write-string line stream :start start :end end))))
    (declare (dynamic-extent #'report-directory))
    (send-current-directory-command connection #'report-directory)))

(defun ftp-rmd (connection args &optional (stream *standard-output*))
  (destructuring-bind (directory) args
    (cond ((yes-or-no-p "~&Delete the directory, ~A? " directory)
           (send-delete-directory-command connection directory)
           (format stream "~&Directory deleted: ~A" directory))
          (t (format stream "~&Directory not deleted: ~A" directory)))))

(defun ftp-rn (connection args &optional (stream *standard-output*))
  (flet ((report-user (code line start end)
           (declare (ignore code))
           (fresh-line stream)
           (write-string line stream :start start :end end)))
    (declare (dynamic-extent #'report-user))
    (destructuring-bind (from-pathname to-pathname) args
      (send-rename-file-command connection from-pathname to-pathname #'report-user))))

(defun ftp-size (connection args &optional (stream *standard-output*))
  (destructuring-bind (filename) args
    (let ((size (send-file-size-command connection filename)))
      (format stream "~&~A ~D" filename size))))

(defun ftp-status (connection args &optional (stream *standard-output*))
  (flet ((report (code line start end)
           (declare (ignore code))
           (fresh-line stream)
           (write-string line stream :start start :end end)))
    (declare (dynamic-extent #'report))
    (destructuring-bind (&optional filename) args
      (send-status-command connection filename #'report))))

(defun ftp-user (connection args &optional (stream *standard-output*))
  (flet ((report-user (code line start end)
           (declare (ignore code))
           (fresh-line stream)
           (write-string line stream :start start :end end)))
    (declare (dynamic-extent #'report-user))
    (destructuring-bind (&optional (user-id (ftp-user-id connection) user-id-supplied-p)) args
      (send-user-command connection user-id #'report-user)
      (when user-id-supplied-p
        (setf (ftp-user-id connection) user-id)))))

(defun ftp-view (connection args &optional (stream *standard-output*))
  (destructuring-bind (remote-file) args
    (format stream "~&View file: ~A~%" remote-file)
    (ftp-get-file connection remote-file stream :mode :text)))

(defvar *shell-readtable* nil)

(defun shell-readtable ()
  (or *shell-readtable*
      (prog1 (setq *shell-readtable* (copy-readtable))
        (setf (readtable-case *shell-readtable*) :preserve))))

(defun ftp-shell-loop (connection &optional (stream *query-io*))
  (labels ((parse-command (command &optional (start 0) (end (length command)))
             (declare (fixnum start end))
             (loop with s1 fixnum = start
                   for idx fixnum upfrom start below end
                   for ch = (aref command idx)
                   when (and (member ch '(#\space #\newline #\tab))
                             (not (and (= idx s1) (setq s1 (1+ idx)))))
                   collect (subseq command s1 idx) into result
                   and do (setq s1 (1+ idx))
                   finally (destructuring-bind (&optional cmd &rest args)
                               (if (= s1 end)
                                   result
                                 (nconc result (list (subseq command s1 idx))))
                             (let* ((command (and (tk1:get-token *keyword-tokenizer* cmd)))
                                    (entry (and command (assoc command *command-table*)))
                                    (function (second entry)))
                               (unless function
                                 (format stream "~&Unknown FTP command: ~A!~%" cmd))
                               (return (values function args))))))
           #+ignore ;; Cons-free. but fails to recognize rubout
           (read-command-line (stream buffer)
             (format stream "~&CL-FTP > ")
             (loop initially (setf (fill-pointer buffer) 0)
                   for ch = (read-char stream nil)
                   while ch
                   do (case ch
                        (#\newline (loop-finish))
                        ((#\rubout #\delete #\backspace)
                         (unless (zerop (fill-pointer buffer))
                           (decf (fill-pointer buffer))))
                        (t (unless (vector-push ch buffer)
                             (error "Command line too long"))))
                   finally (return (values buffer (fill-pointer buffer)))))
           (read-command-line (stream buffer)
             (format stream "~&CL-FTP > ")
             (let* ((line (read-line stream nil nil nil))
                    (end (length line)))
               (declare (dynamic-extent line))
               (copy-vector-portion line 0 end buffer  0 end)
               (setf (fill-pointer buffer) end)
               (values buffer end)))
           (call-shell-function (connection stream &optional buffer)
             (setf (fill-pointer buffer) 0)
             (multiple-value-bind (line end)
                 (read-command-line stream buffer)
               (multiple-value-bind (function arguments)
                   (and line (parse-command line 0 end))
                 (when function
                   (handler-case 
                       (funcall function connection arguments)
                     (ftp-error (cond) (format stream "~&~A: ~A~%" (ftp-status-code cond) (ftp-reason cond)))))))))
    (using-resource (buffer http::line-buffer http::*line-buffer-size*)
      (let ((*readtable* (shell-readtable)))
        (loop doing (call-shell-function connection stream buffer))))))

(defun ftp-shell (host &key (port *ftp-standard-port*) (user-id *ftp-default-user-id*) (password (ftp-default-password)) (query-stream *query-io*))
  (catch 'ftp-exit
    (handler-case
        (with-ftp-connection (connection host port :user-id user-id :password password :report-stream query-stream)
          (ftp-shell-loop connection query-stream))
      (ftp-service-unavailable
       () (format query-stream "~&Service Unavailable: Connection closed by FTP server (~A)." host)))))
