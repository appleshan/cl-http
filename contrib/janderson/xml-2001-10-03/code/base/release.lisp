;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: xml-utils; -*-
;;;

#|
<DOCUMENTATION>
 <DESCRIPTION>
  <p>utilities to extract and format version/changes and to copy sources for release.
  requires the loaded system for system description, packages, and for the
  documentation parser.
   </p>
  </DESCRIPTION>
 <CHRONOLOGY>
  <DELTA DATE='20010910'>
   <ul><li>adpated to new sysdcl form</li>
       <li>now distinguishing pc and mac release only</li></ul></DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
|#
(in-package "XUTILS")


(defMethod to-crlf ((from stream) (to stream) &aux byte)
  (multiple-value-bind (reader reader-arg) (stream-reader from)
    (multiple-value-bind (writer writer-arg) (stream-writer to)
      (loop (unless (fixnump (setf byte (funcall reader reader-arg))) (return))
            (funcall writer writer-arg byte)
            (if (= byte #.(char-code #\return))
              (funcall writer writer-arg #.(char-code #\linefeed))))))
  (set-mac-file-type (pathname to) :TEXT))

(defMethod to-crlf ((from pathname) (to t))
  (format *trace-output* "~%~a ->" from)
  (with-open-file (from from :direction :input :element-type 'unsigned-byte)
    (to-crlf from to))
  (format *trace-output* " ."))

(defMethod to-crlf ((from t) (to pathname))
  (format *trace-output* "~a" to)
  (with-open-file (to to :direction :output
                      :if-does-not-exist :create :if-exists :supersede
                      :element-type 'unsigned-byte)
    (to-crlf from to)))

(defun copy-release (target version source
                            &key
                            (translations (cond ((string-equal target "CRLF") '(("html" . "htm")))
                                                ((string-equal target "MACOS") nil)
                                                (t (error "unknown target type: ~s." target)))))
  (setf source (translate-logical-pathname source))
  (print source)
  (let ((destination (make-pathname :directory
                                    (append (butlast (pathname-directory (probe-file #4p"xml:")))
                                            (list (concatenate 'string  "XML-" version "-" target)
                                                  ;; don't add the extra level
                                                  ;; (first (last (butlast (pathname-directory source))))
                                                  :wild-inferiors))
                                    :name :wild :type :wild))
        (destination-pathname nil)
        (translation)
        (excluded-directories '("inprogress"))
        (excluded-names '("icon" "xml-grammar.lisp" "xpath-grammar.lisp" "xql-grammar.lisp"))
        (excluded-types '("bin" "fasl" "pfsl")))
    (dolist (source-pathname (directory source :directories t :files t))
      (cond ((pathname-name source-pathname)
             ;; copy file
             (unless (or (find (pathname-type source-pathname) excluded-types :test #'string-equal)
                         (find-if #'(lambda (ex) (search ex (namestring source-pathname) :test #'char-equal))
                                  excluded-names)
                         (intersection (pathname-directory source-pathname) excluded-directories :test #'string-equal))
               (setf destination-pathname (translate-pathname source-pathname source destination))
               (when (setf translation (rest (assoc (pathname-type destination-pathname) translations :test #'string-equal)))
                 (setf destination-pathname (make-pathname :type translation :defaults destination-pathname)))
               (cond ((string-equal target "mcl")
                      (copy-file source-pathname destination-pathname))
                     (t
                      (to-crlf source-pathname destination-pathname)))))
            (t
             ;; create empty directory
             (unless (intersection (pathname-directory source-pathname) excluded-directories :test #'string-equal)
               (setf destination-pathname (translate-pathname source-pathname source destination))
               (format *trace-output* "~%creating ~a." destination-pathname)
               (create-directory destination-pathname :if-exists nil)))))))
      


;(copy-release "CRLF" "0-915" #4p"xml:**;*.*")
;(copy-release "CRLF" "0-915" #4p"xml:code;xparser;xml-stream-coding.lisp")
;(copy-release "MACOS" 0-915")
;edit version markers
;(mapc #'ed '("xml:code;xparser;xml-parameters.lisp" "xml:code;xquery;xq-parameters.lisp" "xml:code;xpath;xpath-parameters.lisp" "xml:code;xquerydatamodel;xqdm-parameters.lisp" "xml:sysdcl.lisp"))


;;
;;
;;

(defun print-package-defs (package-name)
  (let ((library nil)
        (forms nil)
        (args nil)
        (w (make-instance 'fred-window :scratch-p t)))
    (do-external-symbols (sym (find-package package-name))
      (when (fboundp sym)
        (let ((fun (symbol-function sym)))
          (when (functionp fun)
            (if (CCL::STANDARD-GENERIC-FUNCTION-P fun)
              (setf args (nthcdr 2 (second (function-lambda-expression
                                            (method-function (elt (generic-function-methods fun) 0))))))
              (setf args (second (function-lambda-expression fun))))
            (if (find-if #'upper-case-p (string sym))
              (push (cons sym args) forms)
              (push (cons sym args) library))))))
    (write-string "<table border='1'>" w)
    (terpri w)
    (write-string "<tr><th colspan='2'>XPath forms</td></tr>" w)
    (dolist (sym-args (sort forms #'string-lessp :key #'first))
      (destructuring-bind (sym . args) sym-args
        (format w "~%<tr><td  width='160 pixels'>~a</td><td>~{~a~^ ~}</td></tr>"
                sym args)))
    (format w "~%</table>")
    
    (write-string "<table border='1'>" w)
    (terpri w)
    (write-string "<tr><th colspan='2'>XPath library functions</td></tr>" w)
    (dolist (sym-args (sort library #'string-lessp :key #'first))
      (destructuring-bind (sym . args) sym-args
        (format w "~%<tr><td  width='160 pixels'>~a</td><td>~{~a~^ ~}</td></tr>"
                sym args)))
    (format w "~%</table>")))

;(print-package-defs "XPA")
;(print-package-defs "XQL")


;;
;;
;;

(defGeneric read-documentation (source)
  (:method ((pathname pathname))
           (with-open-file (stream (merge-pathnames *.lisp-pathname* pathname) :direction :input)
             (read-documentation stream)))
  (:method ((pathname string))
           (read-documentation (pathname pathname)))
  (:method ((stream stream) &aux (buffer (make-array 256 :fill-pointer 0
                                                     :adjustable t
                                                     :element-type 'character)))
           (flet ((read-docstring (stream &aux char)
                    (loop (unless (setf char (read-char stream nil nil))
                            (return-from read-docstring nil))
                          (when (and (char= char #\#) (eql #\| (peek-char nil stream nil nil)))
                            (read-char stream) (return)))
                    (loop (unless (setf char (read-char stream nil nil))
                            (return-from read-docstring nil))
                          (when (and (char= char #\|) (eql #\# (peek-char nil stream nil nil)))
                            (read-char stream) (return))
                          (vector-push-extend char buffer))
                    (setf buffer (string-trim #(#\space #\return #\linefeed #\tab) buffer))))
             (when (and (plusp (length (read-docstring stream))) (eql (char buffer 0) #\<))
               (handler-case
                 (let ((doc-node
                        (xmlp::document-parser (string-trim #(#\space #\return #\linefeed) buffer))))
                   (when doc-node
                     (setf (uri doc-node) (pathname-file-url (pathname stream))))
                   doc-node)
                 (error (condition)
                        (warn "error parsing documentation: ~s: ~a."
                              stream condition)
                        condition))))))

(defun get-deltas (doc-node)
  (typecase doc-node
    (xqdm:doc-node
     (dolist (e (xqdm:children (xqdm:root doc-node)))
       (typecase e
         (xqdm:elem-node
          (when (string-equal (xqdm:name e) "CHRONOLOGY")
            (return
             (remove-if-not #'(lambda (n) (string-equal n "DELTA")) (xqdm:children e)
                            :key #'xqdm:name)))))))
    (null
     nil)
    (t
     (warn "no deltas from: ~s." doc-node)
     nil)))

(defun collect-system-files (system)
  (let ((files nil))
    (cl-user::execute-system-operations system #'(lambda (name) (pushnew name files :test #'equalp)))
    (sort files #'string-lessp :key #'namestring)))

(defGeneric print-version-chronology (destination &key key)
  (:method ((pathname pathname) &rest args)
           (with-open-file (stream pathname :direction :output
                                   :if-exists :supersede :if-does-not-exist :create)
             (apply #'print-version-chronology stream args)))
  (:method ((pathname string) &rest args)
           (apply #'print-version-chronology (pathname pathname) args))
  (:method ((stream stream) &key (key :date) &aux (files (collect-system-files :xquery)))
           (labels ((select-files (directory)
                      (remove-if-not #'(lambda (pathname) (find directory (pathname-directory pathname)
                                                                :test #'string-equal))
                                     files))
                    (collect-deltas (paths)
                      (apply #'append (mapcar #'get-deltas
                                              (mapcar #'read-documentation paths))))
                    (delta-date (delta &aux (date@ (find "date" (xqdm:attributes delta)
                                                         :key #'xqdm:name :test #'string-equal))
                                       date)
                      (cond ((and date@ (setf date (xqdm:value date@))
                                  (= (length date) 8))
                             date)
                            (t
                             (error "bad date: ~a: ~s." date@ date)
                             "")))
                    (delta-version (delta &aux (version@ (find "version" (xqdm:attributes delta)
                                                               :key #'xqdm:name :test #'string-equal)))
                      (when version@ (xqdm:value version@)))
                    (date-string (date)
                      (multiple-value-bind (second minute hour day month year)
                                           (decode-universal-time date)
                        (declare (ignore second))
                        (format nil "~2,'0d.~2,'0d.~4,'0d ~2,'0d:~2,'0d"
                                day month year hour minute))))
             (let* ((xqdm-deltas (collect-deltas (select-files "xquerydatamodel")))
                    (xml-deltas (collect-deltas (select-files "xparser")))
                    (xp-deltas (collect-deltas (select-files "xpath")))
                    (xq-deltas (collect-deltas (select-files "xquery")))
                    (base-deltas (collect-deltas (cons "xml:sysdcl" (select-files "base")))))
               (flet ((extract-versions (deltas)
                        (sort (remove nil
                                      (remove-duplicates (mapcar #'(lambda (d &aux v)
                                                                     (when (setf v (delta-version d))
                                                                       (list v (delta-date d))))
                                                                 deltas)
                                                         :test #'equalp))
                              #'string-lessp :key #'first))
                      (assign-deltas (v-infos deltas &aux (previous-date "") version-alist)
                        ;; collect everything up to the first version
                        (setf version-alist
                              (nconc (mapcar #'(lambda (v-info)
                                                 (destructuring-bind (version date) v-info
                                                   (prog1 (list (list version previous-date date))
                                                     (setf previous-date date))))
                                             v-infos)
                                     ;; add an ultimate entry for canges since the last version
                                     (list (list (list "?" previous-date "?")))))
                        
                        (map nil #'(lambda (delta &aux (date (delta-date delta)) entry)
                                     (unless (setf entry (find-if
                                                          #'(lambda (v-info)
                                                              (and (string< (second v-info)
                                                                            date)
                                                                   (string<= date
                                                                             (third v-info))))
                                                          version-alist :key #'first))
                                       (error "missing v-info."))
                                     (push delta (rest entry)))
                             deltas)
                        (map nil #'(lambda (v-info)
                                     (setf (rest v-info)
                                           (sort (rest v-info)
                                                 (ecase key
                                                   (:file
                                                    #'(lambda (di1 di2)
                                                        (if (string= (first di1) (first di2))
                                                          (string< (rest di1) (rest di2))
                                                          (string< (first di1) (first di2)))))
                                                   (:date
                                                    #'(lambda (di1 di2)
                                                        (if (string= (rest di1) (rest di2))
                                                          (string< (first di1) (first di2))
                                                          (string< (rest di1) (rest di2))))))
                                                 :key #'(lambda (delta)
                                                          (cons (object (xqdm:uri (xqdm:document delta)))
                                                                (delta-date delta))))))
                             version-alist)
                        version-alist)
                      (format-deltas (module versions)
                        (xmlp:xml "hr")
                        (xmlp:xml "h2" module)
                        (xmlp:xml "hr")
                        (xmlp:xml "dl"
                                  (dolist (v-info versions)
                                    (destructuring-bind ((name from to) &rest deltas &aux prev-header) v-info
                                      (if (string= name "?")
                                        (setf name "Open")
                                        (setf name (format nil "Version ~a" name)))
                                      (if (string= from "")
                                        (setf from "........"))
                                      (if (string= to "?")
                                        (setf to "present"))
                                      (xmlp:xml "dt"
                                                name " (covers " from " - " to "):")
                                      (xmlp:xml "dd"
                                                (dolist (delta deltas)
                                                  (let ((name (object (xqdm:uri (xqdm:document delta))))
                                                        (date (delta-date delta)))
                                                    (ecase key
                                                      (:file
                                                       (unless (equalp name prev-header)
                                                         (when prev-header (xmlp:encode-string "</p>"))
                                                         (xmlp:encode-string "<p>")
                                                         (xmlp:xml "b" name)
                                                         (xmlp:encode-string "<br />")
                                                         (setf prev-header name))
                                                       (xmlp:encode-string date)
                                                       (xmlp:encode-string "<br />")
                                                       (xmlp:encode-string (xqdm:value-string delta)))
                                                      (:date
                                                       (unless (equalp date prev-header)
                                                         (when prev-header (xmlp:encode-string "</p>"))
                                                         (xmlp:encode-string "<p>")
                                                         (xmlp:xml "b" date)
                                                         (xmlp:encode-string "<br />")
                                                         (setf prev-header date))
                                                       (xmlp:encode-string name)
                                                       (xmlp:encode-string "<br />")
                                                       (xmlp:encode-string (xqdm:value-string delta))))
                                                    (xmlp:xml "p"))))))
                                  (xmlp:xml "hr"))))
                 (xmlp:with-xml-writer (stream)
                   (xmlp:xml "html"
                             (xmlp:xml ("head" ("title" (date-string (get-universal-time)))))
                             (xmlp:xml "body"
                                       (xmlp:xml "hr")
                                       (xmlp:encode-string (date-string (get-universal-time)))
                                       (xmlp:encode-string "<br />")
                                       "[" (xmlp:xml ("a" ("href" "#base")) "Base") "] ["
                                       (xmlp:xml ("a" ("href" "#xqdm")) "XQDM") "] ["
                                       (xmlp:xml ("a" ("href" "#xparser")) "XParser") "] ["
                                       (xmlp:xml ("a" ("href" "#xpointer")) "XPointer") "] ["
                                       (xmlp:xml ("a" ("href" "#xquery")) "XQuery") "]"
                                       (xmlp:xml ("a" ("name" "base")))
                                       (format-deltas "Base"
                                                      (assign-deltas (extract-versions base-deltas)
                                                                     base-deltas))
                                       (xmlp:xml ("a" ("name" "xqdm")))
                                       (format-deltas "XML Query Data Model"
                                                      (assign-deltas (extract-versions xqdm-deltas)
                                                                     xqdm-deltas))
                                       (xmlp:xml ("a" ("name" "xparser")))
                                       (format-deltas "XML Parser"
                                                      (assign-deltas (extract-versions xml-deltas)
                                                                     xml-deltas))
                                       (xmlp:xml ("a" ("name" "xpointer")))
                                       (format-deltas "X Pointer"
                                                      (assign-deltas (extract-versions xp-deltas)
                                                                     xp-deltas))
                                       (xmlp:xml ("a" ("name" "xquery")))
                                       (format-deltas "X Query"
                                                      (assign-deltas (extract-versions xq-deltas)
                                                                     xq-deltas))
                                       (xmlp:xml "hr")))))))))

#|
;; this produces the complete version documentation
(print-version-chronology "xml:documentation;version-0-912.html")

;; these test aspects of the process
(read-documentation "entwicklung@bataille:Source:LISP:xml:xpath:XPath-parameters.lisp")

(get-deltas (read-documentation "entwicklung@bataille:Source:LISP:xml:xpath:XPath-parameters.lisp"))
|#

:EOF
