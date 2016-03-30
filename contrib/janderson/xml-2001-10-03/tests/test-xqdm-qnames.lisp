;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: abstract-query-data-model; -*-

(defpackage "ABSTRACT-QUERY-DATA-MODEL"
  (:nicknames "AQDM")
  (:import-from "XML-PARSER"
                "ENCODE-NODE"
                "ENCODE-STRING"
                "ENCODE-CHAR"
                "ENCODE-NEWLINE"
                "XML")
  (:import-from "XML-QUERY-DATA-MODEL"
                "NAME"))
(in-package "AQDM")

#|
 qname -> uname resolution tests demonstrate the correct interpretation of three
 expressions:

   literal identity
     ([p_0->ns_0]_0 nse_0)(p_0:lp_0) == ([p_0->ns_0]_1 nse_1)(p_0:lp_0)
   synonymity
     ([p_0->ns_0]_0 nse_0)(p_0:lp_0) == ([p_1->ns_0]_1 nse_1)(p_1:lp_0)
   homography
     ([p_0->ns_0]_0 nse_0)(p_0:lp_0) != ([p_0->ns_1]_1 nse_1)(p_0:lp_0)

 whereby the abstract expressions assume one of various forms. the abstract expression
 form is:

 prefix       ::= 'p1' | 'p2' | ...
 local-part   ::= 'l1' | 'l2' | ...
 namespace    ::= 'ns1' | 'ns2' | ...
 null         ::= ''
 qn  .E. QualifiedName        ::= (:qn (Prefix + null) X LocalPart )
 un  .E. UniversalName        ::= (:un (Namespace + null) X LocalPart )
 n   .E. Name                 ::= UniversalName + QualifiedName
 nsb .E. NamespaceBinding     ::= (:nsb . ((Prefix X Namespace) + (null X (Namespace + null))) )
 a   .E. Attribute            ::= (:a Name )
 e   .E. Element              ::= (:e Name X NamespaceBinding* X Attribute* X Element* )
 ed  .E. ElementDeclaration   ::= (:ed Name X (Name* + '*') )
 ad  .E. AttributeDeclaration ::= (:ad Name X NamespaceBinding* X Attribute* )
 dcl .E. DeclarationContext   ::= (:dcl (ElementDeclaration + AttributeDeclaration + DeclarationContext)* )
 doc .E. Document             ::= (:doc Name DeclarationContext X Element )
 from which the following variations are extrapolated:
|#

(defParameter *counter* 0
  "counter used to generate entity ids and attribute values")
(defParameter *left-margin* 0)

(defun write-node (form &optional (*standard-output* *standard-output*))
  "serialize abstract query document model components as the equivalent xml."
  (encode-node (first form) (rest form)))

(defClass aqdm-node () ())

(defMacro defAQDMNode (name supers slots &rest options)
  (let ((encode-node (assoc :encode-node options))
        (print-node-markup (assoc :print-node-markup options))
        (print-object (assoc :print-object options))
        (macro (assoc :macro options)))
    `(prog1 (defClass ,name ,(or supers '(aqdm-node))
              ,(mapcar #'(lambda (slot)
                           (if (getf (rest slot) :initarg) slot
                               (cons (first slot)
                                     (list* :initarg
                                            (intern (string (first slot)) "KEYWORD")
                                            (rest slot)))))
                       slots)
              ,@(nremove print-object
                         (nremove encode-node
                                  (nremove print-node-markup
                                           (nremove macro options)))))
       ,@(when macro
          `((defmacro ,(intern (string name) "KEYWORD")
                      ,@(rest (second macro)))))
       ,@(when encode-node
           `((defMethod xmlp::encode-node ((node ,name))
               (funcall (function ,(second encode-node)) node))))
       ,@(when print-node-markup
           `((defMethod print-node-markup ((stream t) (node ,name) &optional colon at)
               (funcall (function ,(second print-node-markup)) stream node colon at))))
       ,@(when print-object
           `((defMethod print-object ((node ,name) stream)
               (if *print-escape* (call-next-method)
                   (funcall (function ,(second print-object)) node stream))))))))
                                                
(defGeneric print-node-markup (stream node &optional colon atsign)
  (:documentation
   "write an abstract qdm node to standard output as a math expression."))
            
(defAQDMNode doc ()
  ((name)
   (declaration-context)
   (element))
  (:macro
   (lambda (name dcl element)
     `(make-instance 'doc :name ,name :declaration-context ,dcl :element ,element)))
  (:encode-node
   (lambda (node &aux (*counter* 0) (*left-margin* 0))
     (with-slots (name declaration-context element) node
       (encode-string "<!DOCTYPE ")
       (encode-node name)
       (encode-string " [")
       ;; top-level context is written as internal dtd
       (with-slots (declarations) declaration-context
         (dolist (dcl declarations)
           (encode-newline)
           (encode-string "  ")
           (encode-node dcl))
         (encode-string " ] >")
         (encode-node element)))))
  (:print-node-markup 
   (lambda (stream node colon at &aux (*left-margin* 1))
     (declare (ignore colon at))
     (with-slots (name declaration-context element) node
       ;; (write-string "<pre xmlns='http://www.w3.org/1999/xhtml'><code>" stream)
       (format stream "(:doc ~/AQDM::PRINT-NODE-MARKUP/~% ~/AQDM::PRINT-NODE-MARKUP/~% ~/AQDM::PRINT-NODE-MARKUP/)"
                     name declaration-context element)
       ;; (write-string "</code></pre>" stream)
       )))
  (:print-object
   (lambda (node stream)
     (with-slots (name declaration-context element) node
       (format stream "(:doc ~a~% ~a~% ~a)"
               name declaration-context element)))))
                

(defAQDMNode dcl ()
  ((declarations))
  (:macro
   (lambda (declarations)
     `(make-instance 'dcl :declarations (list ,@declarations))))
  (:encode-node
   (lambda (node &aux (name (format nil "e-~d" (incf *counter*))) value)
     "sub-dcls are written as dtd uri's !don't try to nest them"
     (with-slots (declarations) node
       (encode-string "<!ENTITY % ")
       (encode-string name)
       (encode-string " SYSTEM 'data:,")
       (setf value (with-output-to-string (*standard-output*)
                     (xmlp:with-xml-writer (*standard-output*)
                       (dolist (dcl declarations)
                         (encode-node dcl)
                         (encode-char #\space)))))
       (if (find #\' value)  ;; escaping for one level only
         (map nil #'(lambda (c)
                      (if (char= c #\') (encode-string "\"") (encode-char c)))
              value)
         (encode-string value))
       (encode-string "'> %")
       (encode-string name)
       (encode-char #\;))))
  (:print-node-markup 
   (lambda (stream node colon at &aux (*left-margin* (+ *left-margin* 7)))
     (declare (ignore colon at))
     (with-slots (declarations) node
       (write-string "(:dcl (" stream)
       (mapl #'(lambda (dcls)
                 (print-node-markup stream (first dcls))
                 (when (rest dcls)
                   (terpri) (dotimes (i *left-margin*) (write-char #\space))))
               declarations)
       (write-string "))" stream))))
  (:print-object
   (lambda (node stream)
     (with-slots (declarations) node
       (format stream "(:dcl ~a)" declarations )))))

(defAQDMNode ad ()
  ((name)
   (namespace-bindings)
   (attributes))
  (:macro
   (lambda (name namespace-bindings attributes)
     `(make-instance 'ad :name ,name :namespace-bindings (list ,@namespace-bindings)
                     :attributes (list ,@attributes))))
  (:encode-node
   (lambda (node)
     (with-slots (name namespace-bindings attributes) node
       (encode-string "<!ATTLIST ")
       (encode-node name)
       (dolist (ns namespace-bindings)
         (with-slots (prefix namespace) ns
           (encode-string " xmlns")
           (when prefix (encode-char #\:) (encode-string (string prefix)))
           (encode-string " CDATA '")
           (when namespace (encode-string (string-downcase namespace)))
           (encode-string "' ")))
       (dolist (attr attributes)
         (with-slots (name) attr
           (encode-char #\space)
           (encode-node name)
           (encode-string " CDATA 'attValue-")
           (encode-string (write-to-string (incf *counter*)))
           (encode-string "' ")))
       (encode-char #\>))))
  (:print-node-markup
   (lambda (stream node colon atsign)
     (declare (ignore colon atsign))
     (with-slots (name namespace-bindings attributes) node
       (format stream "(:ad ~/AQDM::PRINT-NODE-MARKUP/ ~{~/AQDM::PRINT-NODE-MARKUP/~^ ~} ~{~/AQDM::PRINT-NODE-MARKUP/~^ ~})"
               name namespace-bindings attributes))))
  (:print-object
   (lambda (node stream)
     (with-slots (name namespace-bindings attributes) node
       (format stream "(:ad ~a ~a ~a)" name namespace-bindings attributes)))))


(defAQDMNode ed ()
  ((name)
   (model))
  (:macro
   (lambda (name model)
     `(make-instance 'ed :name ,name
                     :model ,(if (listp model) `(list ,@model) `(quote ,model)))))
  (:encode-node
   (lambda (node)
     (with-slots (name model) node
       (encode-string "<!ELEMENT ")
       (encode-node name)
       (cond ((and (symbolp model) (string= model "*"))
              (encode-string " ANY >"))
             ((null model)
              (encode-string " EMPTY >"))
             (t
              (encode-string " ( ")
              (mapl #'(lambda (names)
                        (encode-node (first names))
                        (when (rest names) (encode-string ", ")))
                    model)
              (encode-string " ) >"))))))
  (:print-node-markup
   (lambda (stream node colon at)
     (declare (ignore colon at))
     (with-slots (name model) node
       (format stream "(:ed ~/AQDM::PRINT-NODE-MARKUP/ ~:[~a~;~{~/AQDM::PRINT-NODE-MARKUP/~^ ~}~])"
                      name (listp model) model))))
  (:print-object
   (lambda (node stream)
     (with-slots (name model) node
       (format stream "(:ed ~a ~:a)" name model)))))

(defAQDMNode e ()
  ((name)
   (namespace-bindings)
   (attributes)
   (elements))
  (:macro
   (lambda (name &optional namespace-bindings attributes elements)
     `(make-instance 'e :name ,name :namespace-bindings (list ,@namespace-bindings)
                     :attributes (list ,@attributes)
                     :elements (list ,@elements))))
  (:encode-node
   (lambda (node)
     (with-slots (name namespace-bindings attributes elements) node
       (encode-newline)
       (encode-char #\<)
       (encode-node name)
       (dolist (ns namespace-bindings)
         (with-slots (prefix namespace) ns
           (encode-string " xmlns")
           (when prefix (encode-char #\:) (encode-string (string prefix)))
           (encode-string "='")
           (when namespace
             (encode-string (string-downcase namespace)))
           (encode-string "' ")))
       (dolist (attr attributes)
         (with-slots (name) attr
           (encode-char #\space)
           (encode-node name)
           (encode-string "='")
           (encode-string (write-to-string (incf *counter*)))
           (encode-char #\')))
       (cond (elements
              (encode-char #\>)
              (dolist (element elements)
                (encode-node element))
              (encode-string "</")
              (encode-node name)
              (encode-char #\>))
             (t
              (encode-string "/>"))))))
  (:print-node-markup
   (lambda (stream node colon at &aux (*left-margin* (+ *left-margin* 2)))
     (declare (ignore colon at))
     (with-slots (name namespace-bindings attributes elements) node
       (format stream "(:e ~/AQDM::PRINT-NODE-MARKUP/ (~{~/AQDM::PRINT-NODE-MARKUP/~^ ~}) (~{~/AQDM::PRINT-NODE-MARKUP/~^ ~})"
               name namespace-bindings attributes)
       (cond (elements
              (format stream "~%~vT(" (1- *left-margin*))
              (mapl #'(lambda (elements)
                        (print-node-markup stream (first elements))
                        (when (rest elements)
                          (format stream "~%~vT" *left-margin*)))
                    elements)
              (write-char #\) stream))
             (t
              (write-string " ())" stream))))))
  (:print-object
   (lambda (node stream)
     (with-slots (name namespace-bindings attributes elements) node
       (format stream "(:e ~a ~:a ~:a ~:a)"
               name namespace-bindings attributes elements)))))

(defAQDMNode a ()
  ((name))
  (:macro
   (lambda (name)
     `(make-instance 'a :name ,name)))
  ;; no encode method is necessary, as they are done differently depending on context
  (:print-node-markup
   (lambda (stream node colon at)
     (declare (ignore colon at))
     (with-slots (name) node
       (format stream "(:ad ~/AQDM::PRINT-NODE-MARKUP/)"
               name))))
  (:print-object
   (lambda (node stream)
     (with-slots (name) node
       (format stream "(:a ~a)" name)))))

(defAQDMNode nsb ()
  ((prefix)
   (namespace))
  (:macro
   (lambda (prefix namespace)
     `(make-instance 'nsb :prefix ',prefix :namespace ',namespace)))
  ;; no encode method is necessary, as they are done differently depending on context
  (:print-node-markup
   (lambda (stream node colon at)
     (declare (ignore colon at))
     (with-slots (prefix namespace) node
       (format stream "(:nsb ")
       (let* ((name (string (string-downcase prefix)))
              (pos (position-if #'digit-char-p name)))
         (if pos
           (format stream "~a<sub>~a</sub>"
                   (subseq name 0 pos) (subseq name pos))
           (write-string name stream)))
       (let* ((name (string (string-downcase namespace)))
              (pos (position-if #'digit-char-p name)))
         (if pos
           (format stream "~a<sub>~a</sub>"
                   (subseq name 0 pos) (subseq name pos))
           (write-string name stream))))))
  (:print-object
   (lambda (node stream)
     (with-slots (prefix namespace) node
       (format stream "(:nsb ~a ~a)" prefix namespace)))))
  

(defAQDMNode un ()
  ((namespace)
   (local-part))
  (:macro
   (lambda (namespace local-part)
     `(make-instance 'un :namespace ',namespace :local-part ',local-part)))
  (:encode-node
   (lambda (node)
     (with-slots (namespace local-part) node
       (when namespace (encode-char #\{)
             (encode-string (string namespace))
             (encode-char #\}))
       (encode-string (string local-part)))))
  (:print-node-markup
   (lambda (stream node colon at)
     (declare (ignore colon at))
     (with-slots (namespace local-part) node
       (let* ((name (string (string-downcase namespace)))
              (pos (position-if #'digit-char-p name)))
         (if pos
           (format stream "~a<sub>~a</sub>"
                   (subseq name 0 pos) (subseq name pos))
           (write-string name stream)))
       (write-char #\space)
       (let* ((name (string (string-downcase local-part)))
              (pos (position-if #'digit-char-p name)))
         (if pos
           (format stream "~a<sub>~a</sub>"
                   (subseq name 0 pos) (subseq name pos))
           (write-string name stream)))
       (write-char #\) stream))))
  (:print-object
   (lambda (node stream)
     (with-slots (namespace local-part) node
       (format stream "(:un ~a ~a)" namespace local-part)))))
     

(defAQDMNode qn ()
  ((prefix)
   (local-part))
  (:macro
   (lambda (prefix local-part)
     `(make-instance 'qn :prefix ',prefix :local-part ',local-part)))
  (:encode-node
   (lambda (node)
     (with-slots (prefix local-part) node
       (when prefix (encode-string (string prefix))
             (encode-char #\:))
       (encode-string (string local-part)))))
  (:print-node-markup
   (lambda (stream node colon at)
     (declare (ignore colon at))
     (with-slots (prefix local-part) node
       (format stream "(:qn ")
       (let* ((name (string (string-downcase prefix)))
              (pos (position-if #'digit-char-p name)))
         (if pos
           (format stream "~a<sub>~a</sub>"
                   (subseq name 0 pos) (subseq name pos))
           (write-string name stream)))
       (let* ((name (string (string-downcase local-part)))
              (pos (position-if #'digit-char-p name)))
         (if pos
           (format stream "~a<sub>~a</sub>"
                   (subseq name 0 pos) (subseq name pos))
           (write-string name stream)))
       (write-char #\) stream))))
  (:print-object
   (lambda (node stream)
     (with-slots (prefix local-part) node
       (format stream "(:qn ~a ~a)" prefix local-part)))))

(defun write-node-to-string (node)
  (with-output-to-string (stream)
    (xmlp::write-node node stream)))

(defMethod xmlp::write-node
           ((*document* doc) stream)
  (xmlp::with-xml-writer (stream)
    (encode-node *document*)
    *document*))


(defun document-qname-contexts (doc-node)
  (let ((contexts nil))
    (maphash #'(lambda (key def-type)
                 (declare (ignore key))
                 (push (qname-context def-type) contexts))
             (types doc-node))
    (sort contexts #'string< :key #'name)))


(defGeneric aqdm-node (xqdm-node)
  (:method ((node xqdm:doc-node))
           (make-instance 'doc
             :name (aqdm-node (xqdm:name node))
             :declaration-context
             (make-instance 'dcl
               :declarations
               (let ((dcls nil)
                     (types nil))
                 (maphash #'(lambda (name def) (declare (ignore name)) (push def types))
                          (xqdm:types node))
                 (sort types #'string-lessp :key #'xqdm:name)
                 (map nil #'(lambda (def
                                      &aux (name (xqdm:name def))
                                      (names (xqdm:collect-model-names
                                                   (xqdm:model def)))
                                      (props (mapcar #'xqdm:prototype
                                                     (xqdm:properties def))))
                              (when props
                                (push (make-instance 'ad
                                        :name (aqdm-node name)
                                        :namespace-bindings
                                        (mapcar #'aqdm-node
                                                (xqdm:collect-namespaces props))
                                        :attributes
                                        (mapcar #'aqdm-node
                                                (xqdm:collect-attributes props)))
                                      dcls))
                              (push (make-instance 'ed
                                      :name (aqdm-node name)
                                      :model (cond ((eq (first names) '|xml|::|()|)
                                                    nil)
                                                   ((eq (first names) '|xml|::|*|)
                                                    '*)
                                                   (t
                                                    (mapcar #'aqdm-node names))))
                                    dcls))
                      types)
                 (nreverse dcls)))
             :element
             (aqdm-node (xqdm:root node))))
  (:method ((node xqdm:elem-node))
           (make-instance 'e
             :name (aqdm-node (xqdm:name node))
             :namespace-bindings (mapcar #'aqdm-node (xqdm:namespaces node))
             :attributes (mapcar #'aqdm-node (xqdm:attributes node))
             :elements (mapcar #'aqdm-node (xqdm:collect-elements (xqdm:children node)))))
  (:method ((node xqdm:ns-node)
            &aux (ns-name (xqdm:namespace-name node)) (name (xqdm:name node)))
           (make-instance 'nsb
             :prefix (unless (eq name xqdm::*default-namespace-attribute-name*)
                       (xqdm:local-part name))
             :namespace (unless (string-equal ns-name "") ns-name)))
  (:method ((node xqdm:attr-node))
           (make-instance 'a :name (aqdm-node (xqdm:name node))))
  (:method ((node #+xml-symbols symbol #-xml-symbols name)
            &aux (namespace (xqdm:namespace node)))
           (if namespace
             (make-instance 'un
               :namespace (unless (eq namespace xqdm:*null-namespace*)
                            (xqdm:namespace-name namespace))
               :local-part (xqdm:local-part node))
             (make-instance 'qn
               :prefix (xqdm:prefix node) :local-part (xqdm:local-part node)))))

(defun format-example (description doc dom)
  (flet ((escape-string (string &aux (buffer (make-array 10 :element-type 'character
                                                         :fill-pointer 0 :adjustable t)))
           (map nil #'(lambda (c)
                        (case c
                          (#\< (map nil #'(lambda (c) (vector-push-extend c buffer))
                                    "&lt;"))
                          (#\> (map nil #'(lambda (c) (vector-push-extend c buffer))
                                    "&gt;"))
                          (#\linefeed (vector-push-extend #\newline buffer))
                          (t (vector-push-extend c buffer))))
                string)
           buffer))
    (format t "<p>~a</p>
<p>
<pre><code>~a

~/aqdm::print-node-markup/
==&gt;
~/aqdm::print-node-markup/</code></pre></p>
<hr />
"
          description (escape-string (write-node-to-string doc)) doc  (aqdm-node dom))))

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;;
;; tests
;;
;;
;; literal identity

(defParameter *doc-li-1* (:doc (:qn p0 l0)
                               (:dcl ((:ed (:qn p0 l0) ())
                                      (:ad (:qn p0 l0) ((:nsb p0 ns0)) ())))
                               (:e (:qn p0 l0) () () ())))
(defParameter *dom-li-1*
  (xmlp::document-parser (write-node-to-string *doc-li-1*) :validate t :verbose-qnames t)
  "the scope of the p1->ns0 binding comprises the element declaration. by virtue of
   attribute defaults it extends to the document element instance")

(format-example (documentation '*dom-li-1* 'variable) *doc-li-1* *dom-li-1*)


(defParameter *doc-li-2* (:doc (:qn p0 l0)
                           (:dcl ((:ed (:qn p0 l0) ((:qn p0 l1) (:qn p0 l2)))
                                  (:ad (:qn p0 l0) ((:nsb p0 "ns0"))
                                       ((:a (:qn p0 la1)) (:a (:qn p0 la2))))
                                  (:ed (:qn p0 l1) ())
                                  (:ed (:qn p0 l2) ())
                                  (:ad (:qn p0 l0) () ((:a (:qn p0 att3))))))
                           (:e (:qn p0 l0) () ()
                               ((:e (:qn p0 l1) () () ())
                                (:e (:qn p0 l2) () () ())))))
(defParameter *dom-li-2*
  (xmlp::document-parser (write-node-to-string *doc-li-2*) :validate t :verbose-qnames t)
  "the scope of the p1->ns1 binding comprises the element declaration and to content
   elements. by virtue of attribute defaults it extends to the document element
   instance.")

(format-example (documentation '*dom-li-2* 'variable) *doc-li-2* *dom-li-2*)


(defParameter *doc-li-3* (:doc (:qn p1 l1)
                            (:dcl ((:ed (:qn p1 l1) ((:qn p1 l2)))
                                   (:ed (:qn p1 l2) ())
                                   (:ad (:qn p1 l1) ((:nsb p1 "ns1")) ())))
                            (:e (:qn p1 l1) () () ((:e (:qn p1 l2))))))
(defParameter *dom-li-3*
  (xmlp::document-parser (write-node-to-string  *doc-li-3*) :validate t :verbose-qnames t)
  "the scope of the p1->ns1 binding comprises the element declaration and declared
   content elements. by virtue of attribute defaults it extends to the document element
   instance.")

(format-example (documentation '*dom-li-3* 'variable) *doc-li-3* *dom-li-3*)



(defParameter *doc-li-4* (:doc (:qn p1 l1)
                           (:dcl ((:ed (:qn p1 l1) *) (:ed (:qn p1 l2) ())
                                  (:ad (:qn p1 l1) ((:nsb p1 "ns1")) ())))
                           (:e (:qn p1 l1) () () ((:e (:qn p1 l2))))))
(defParameter *dom-li-4*
  (xmlp::document-parser (write-node-to-string *doc-li-4*) :validate t :verbose-qnames t)
  "the scope of the p1->ns1 binding comprises the element declaration and ANY
   content elements. by virtue of attribute defaults it extends to the document element
   instance.")

(format-example (documentation '*dom-li-4* 'variable) *doc-li-4* *dom-li-4*)


;;
;;
;; synonomy

(defParameter *doc-s-5* (:doc (:qn p1 l1)
                           (:dcl ((:ad (:qn p1 l1) ((:nsb p1 "ns1")) ())
                                  (:dcl ((:ed (:qn p1 l1) *)))
                                  (:dcl ((:ed (:qn p1 l2) ())))))
                           (:e (:qn p1 l1) () () ((:e (:qn p1 l2))))))
(defParameter *dom-s-5*
  (xmlp::document-parser (write-node-to-string *doc-s-5*) :validate t :verbose-qnames t)
  "the scope of the p1->ns1 binding encompasses declarations of
    elements which appear in declaration entities dependent on the entity within which
    the binding appears. it extends as well over distinct branch entities and over
    children of the directly declared element by virtue of the ANY content model.")

(format-example (documentation '*dom-s-5* 'variable) *doc-s-5* *dom-s-5*)


(defParameter *doc-s-6* (:doc (:qn p1 l1)
                           (:dcl ((:ad (:qn p1 l1) ((:nsb p1 "ns1") (:nsb p2 "ns1")) ())
                                  (:dcl ((:ed (:qn p1 l1) ((:qn p1 l2)))))
                                  (:dcl ((:ed (:qn p2 l2) ())))))
                           (:e (:qn p1 l1) () () ((:e (:qn p1 l2))))))
(defParameter *dom-s-6*
  (xmlp::document-parser (write-node-to-string *doc-s-5*) :validate t :verbose-qnames t)
  "the scope of the p1->ns1, and p2->ns1 binding encompasses declarations of
    elements which appear in declaration entities dependent on the entity within which
    the binding appears. it extends as well over distinct child entities and over
    children of the directly declared element to effect the identity od names with
    distinct literal expressions.")

(format-example (documentation '*dom-s-6* 'variable) *doc-s-6* *dom-s-6*)


;;
;;
;; homography

(defParameter *doc-h-1* (:doc (:qn p1 l1)
                           (:dcl ((:ad (:qn p1 l1) ((:nsb p1 ns1)) ())
                                  (:dcl ((:ed (:qn p1 l1) *)))
                                  (:dcl ((:ed (:qn p1 l2) ())))))
                           (:e (:qn p1 l1) () () ((:e (:qn p1 l2))))))
(defParameter *dom-h-1*
  (xmlp::document-parser (write-node-to-string *doc-h-1*) :validate t :verbose-qnames t)
  "the scope of the p1->ns1 binding encompasses declarations of
    elements which appear in declaration entities dependent on the entity within which
    the binding appears. it extends as well over distinct child entities and over
    children of the directly declared element.")
(format-example (documentation '*dom-h-1* 'variable) *doc-h-1* *dom-h-1*)


(defParameter *doc-h-2* (:doc (:qn p1 l1)
                              (:dcl ((:ad (:qn p1 l1) ((:nsb p1 ns1) (:nsb p2 ns2)) ())
                                     (:ed (:qn p1 l1) ((:qn p1 l2) (:qn p2 l2)))
                                     (:dcl ((:ed (:qn p1 l2) ())
                                            (:ad (:qn p1 l2) ((:nsb p1 ns1)) ())))
                                     (:dcl ((:ed (:qn p1 l2) ())
                                            (:ad (:qn p1 l2) ((:nsb p1 ns2)) ())))))
                              (:e (:qn p1 l1) () () ((:e (:qn p1 l2)) (:e (:qn p2 l2))))))
(defParameter *dom-h-2*
  (xmlp::document-parser (write-node-to-string *doc-h-2*) :validate t :verbose-qnames t)
  "the scope of the p1->ns1 binding does not encompass declarations of
    elements which appear in declaration entities in parallel branches.
   it extends as well over distinct child entities and over
    children of the directly declared element.")

(format-example (documentation '*dom-h-2* 'variable) *doc-h-2* *dom-h-2*)


#|
(inspect (qname-contexts :all))
(inspect (qname-contexts :by-name))
(inspect *def-attr-qname-contexts*)
(map nil 'print (top-inspect-form))

|#



:EOF
