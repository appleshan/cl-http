(in-package :www-utils)

;;;implemented in smtp;mail.lisp
;;;can't load that since acl internal classes are used, grrr

(defun send-mail-from (from to subject mail-writer
                            &key keywords comments file-references reply-to additional-headers
                            user 
                            password)
  "Send an email message from FROM to TO with subject SUBJECT.
MESSAGE-WRITER is either a string or a function accepting a stream argument
that is used to generate the message body. KEYWORDS is a list of keywords for a keyword
header. COMMENTS is a string for a comment header. FILE-REFERENCES is a list of pathnames.
REPLY-TO is automatically added unless supplied here."
  (values from to subject mail-writer keywords comments file-references reply-to additional-headers
          user 
          password)
  )


(defun report-bug  (to subject format-string &rest format-args)
  "Reports a bug to TO with SUBJECT and message body
produced by applying format to format-string and format-args.
The from field is http:*server-mail-address*."
  (values to subject format-string format-args))

