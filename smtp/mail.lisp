;;;-*- Syntax: ansi-common-lisp; Base: 8; Mode: lisp; Package: www-utils -*-

;;; Copyright John C. Mallery,  1995.
;;; All rights reserved.
;;;

;;;------------------------------------------------------------------- 
;;;
;;;  interfaces for CL-HTTP callers
;;;

(in-package :www-utils)

#|
(setq smtp:*network-mail-host* "ai.mit.edu")

(setq http:*server-mail-address* "jcma@wilson.ai.mit.edu")

(smtp:debug-mailer t)

|# 

(defun send-mail-from (from to subject mail-writer
			    &key keywords comments file-references reply-to additional-headers
                            (user (smtp:get-smtp-authentication-user smtp:*network-mail-host*))
                            (password (smtp:get-smtp-authentication-password smtp:*network-mail-host*)))
  "Send an email message from FROM to TO with subject SUBJECT.
MESSAGE-WRITER is either a string or a function accepting a stream argument
that is used to generate the message body. KEYWORDS is a list of keywords for a keyword
header. COMMENTS is a string for a comment header. FILE-REFERENCES is a list of pathnames.
REPLY-TO is automatically added unless supplied here."
  (labels ((keyword-string (keywords)
             (when keywords
               (format nil "~{~:(~A~)~^, ~}" keywords)))
           (file-references-string (file-references)
             (when file-references
               (format nil "~{~A~^, ~}" file-references)))
           (send-it ()
             (smtp:%send-email-message from to subject mail-writer reply-to
                                       (keyword-string keywords) comments
                                       (file-references-string file-references)
                                       additional-headers
                                       user
                                       password))
           (write-message (mail-writer stream)
             (etypecase mail-writer
               (string (write-string mail-writer stream))
               (function (funcall mail-writer stream))))
           (log-message (case from to subject mail-writer)
             (notify-log-window
              "~A"				;prevents error when message contains a tilde.
              (with-output-to-string (stream)
                (format stream "~&~A~&From: ~A~&To: ~A~&Subject: ~A~2%"
                        case from to subject)
                (write-message mail-writer stream))))
           (handle-mailer-error ()
             (loop for path in (smtp:store-and-forward-mailer-hosts)
                   for host = (parse-host path t)
                   unless (http::host-eq host (local-host))
                   do (handler-case
                          (smtp:with-network-mail-host (host)
                            (send-it)
                            (return))
                        (network-error () nil))
                   finally (progn (log-message "No Accessible Mailer Found:" from to subject mail-writer)
                             (return t)))))
    (declare (dynamic-extent #'send-it))
    (cond ((null smtp:*network-mail-host*)
           (log-message "No network mail host specified:" from to subject mail-writer))
          (smtp:*debug-mailer* (send-it))
          (t (handler-case
                 (send-it)
               (network-error () (handle-mailer-error)))))))

(define report-bug  (to subject format-string &rest format-args)
  "Reports a bug to TO with SUBJECT and message body
produced by applying format to FORMAT-STRING and FORMAT-ARGS.
If FORMAT-STRING is a function, it is called with stream to produce
the bug report FORMAT-ARGS should be null in this case.
The from field is http:*server-mail-address*.
If http::*bug-report-destination* is :mail, then
the bug report will be send by mail.
If http::*bug-report-destination*  is :log-window,
then the bug-report is only displayed in the log-window.
If http::*bug-report-destination* is NIL, then
no bug report will happen."
  (flet ((write-message (stream)
	   (apply #'format stream format-string format-args)))
    (case http::*bug-report-destination*
      (:mail
       (process-run-function
        "Report HTTP Server Bug"
        #'send-mail-from http:*server-mail-address* to subject (etypecase format-string
                                                                 (string #'write-message)
                                                                 (function format-string))))
      (:log-window
       (apply #'notify-log-window format-string format-args)))))