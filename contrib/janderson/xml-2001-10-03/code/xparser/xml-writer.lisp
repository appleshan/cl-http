;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: xml-parser; -*-

#|
<DOCUMENTATION>
 <DESCRIPTION>
  immediate serialization macros.
  <p>
  adapted from html-gen.slik. the resemblance remains, but is remote.</p>
  </DESCRIPTION>
 <COPYRIGHT YEAR='2001' AUTHOR='james adam anderson' MARK='(C)'
            href='file://xml/sysdcl.lisp' />
 <CHRONOLOGY>
  <DELTA DATE='20010621' AUTHOR='JAA'>
   new. adapted html-gen to the encoding serialization functions.</DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
|#

(in-package "XML-PARSER")

(defmacro xml (tag &rest content)
  "Write a tag with contents, xs.  form can be:
  tag -> name
  tag -> (name . attribute ...)
  name -> a symbol naming an HTML element, like h1. (nb. unquoted is normal)
  attribute -> a symbol naming an attribute with a declare value, 
               like compact.
  attribute -> (name value)
  value -> will be turned into a string and escaped properly.
  nb. intended to be used within an an <code>WITH-XML-WRITER</code>."
  (let* ((tag-name (etypecase tag
                     (string tag)
                     (symbol tag)
                     (cons (if (and (= (length tag) 2) (eq (car tag) 'quote))
                             (second tag)
                             (if (consp (first tag))
                               (second (first tag))
                               (first tag))))))
	 (attributes (etypecase tag
                       (string nil)
                       (symbol nil)
                       (cons (if (and (= (length tag) 2) (eq (car tag) 'quote))
                               nil (cdr tag)))))
         (namespace-bindings
          (remove nil
                  (mapcar #'(lambda (attr)
                              (destructuring-bind (name value) attr
                                ;; check if a new namespace binding is implied
                                (when (and (not (stringp name))
                                           (eq (namespace name) *xmlns-namespace*))
                                  (unless (find-package value)
                                    (warn "namespace package not defined: '~a'" value))
                                  `(setf *namespace-bindings* (acons ',name (find-package ,value)
                                                                     *namespace-bindings*)))))
                          attributes)))
         (need-namespaces (not (and (stringp tag-name) (null attributes)))))
    (flet ((needs-quoting? (v)
             ;; this test applies only to static strings in element content...
             (not (find-if #'(lambda (c) (find c "<>&")) v))))
      (when (symbolp tag-name) (setf tag-name `(quote ,tag-name)))
      `(let (,@(when need-namespaces
                 `((*namespace-bindings* *namespace-bindings*)
                   (generated-ns-bindings nil)))
             (*node-level* (1+ *node-level*)))
         ,@namespace-bindings
         ,(if need-namespaces
            `(handler-bind ((|NSC: Prefix Declared|
                             #'(lambda (condition &aux (prefix (next-prefix))
                                                  node)
                                 (setf node (cons prefix (name condition)))
                                 (push node *namespace-bindings*)
                                 (push node generated-ns-bindings)
                                 (use-value prefix))))
               (when *print-pretty*
                 (encode-char #\newline)
                 (dotimes (i *node-level*) (encode-char #\space)))
               (encode-char #\<)
               (encode-node ,tag-name)
               ,@(mapcar #'(lambda (attr &aux name value)
                             (if (consp attr)
                               (setf name (first attr)
                                     value (second attr))
                               (setf name attr value (string attr)))
                             `(progn (encode-char #\space)
                                     (encode-node ',name)
                                     (encode-char #\=)
                                     (encode-char #\')
                                     (encode-node ,value)
                                     (encode-char #\')))
                         attributes)
               (when generated-ns-bindings
                 (encode-generated-ns-bindings generated-ns-bindings)))
            `(progn (when *print-pretty*
                      (encode-char #\newline)
                      (dotimes (i *node-level*) (encode-char #\space)))
                    (encode-char #\<)
                    (encode-node ,tag-name)))
         ,(if content '(encode-char #\>) '(encode-string " />"))
         ;; if the form is an expression, then presume it is to be included for the side effect
         ;; a string or a symbol is written directly
         ,@(mapcar #'(lambda (form)
                       (if (consp form)
                         form
                         (if (and (stringp form) (not (needs-quoting? form)))
                           `(encode-string ,form)
                           `(encode-node ,form))))
                   content)
         (when *print-pretty*
           (encode-char #\newline)
           (dotimes (i (1+ *node-level*)) (encode-char #\space)))
         ,@(when content
             `((encode-string "</")
               (encode-node ,tag-name)
               (encode-string ">")))))))

#|
(with-xml-writer (*trace-output*)
  (xml tag "test 1 2 3"))

(with-xml-writer (*trace-output*)
  (xml "tag" "test 1 2 3"))


(with-xml-writer (*trace-output*)
  (xml (|xhtml|::tag (|xmlns|:|| "http://www.w3.org/1999/xhtml")) "test 1 2 3"))

(with-xml-writer (*trace-output*)
  (xml ("tag" (|xmlns|:|| "http://www.w3.org/1999/xhtml")) "test 1 2 3"))

|#
:EOF
