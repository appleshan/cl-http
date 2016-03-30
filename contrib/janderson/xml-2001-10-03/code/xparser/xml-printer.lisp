;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: xml-parser; -*-

#|
<DOCUMENTATION>
 <DESCRIPTION>
  print methods for xml data models
  </DESCRIPTION>
 <COPYRIGHT YEAR='2001' AUTHOR='james adam anderson' MARK='(C)'
            href='file://xml/sysdcl.lisp' />
 <CHRONOLOGY>
  <DELTA DATE='20010305' AUTHOR='JAA'/>
  <DELTA DATE='20010511' AUTHOR='JAA'>
   separate <code>WRITE-NODE</code> and <code>ENCODE-NODE</code> functions.
   the first acts as the interface and takes a stream arg. the second is
   internal and expects bindings for the writer function.
   </DELTA>
  <DELTA DATE='20010603'>
   corrected (encode-node symbol) to permit uninterned names -> no prefix;
   bind *document* in (write-node doc-node) for consistency parse/serialize.
   </DELTA>
  <DELTA DATE='20010621'>
   fixed spacing in attribute declarations<br />
   fixed encoding printing for doc-node</DELTA>
  <DELTA DATE='20010621'>WITH-XML-WRITER macro</DELTA>
  <DELTA DATE='20010623'>encode-node for doc-node takes a default doctype
   name from the root element</DELTA>
  <DELTA DATE='20010625'>
   adjustments to encode-node for <code>*-model</code> for PCDATA
   <br />
   *node-cache* to support recursive <code>DEF-NODE</code> printing, which
   makes it possible to effect prefixes analogous to those used in the
   document entity.</DELTA>
  <DELTA DATE='20010626'>
   changed respective <code>WRITE-NODE</code> and <code>ENCODE-NODE</code>
   to specialize on <code>ABSTRACT-ELEM-NODE</code> rather than on
   <code>ELEM-NODE</code> in order to support specialized instantiation.
   <code>ENCODE-NODE</code> now relies on the interface rather than the
   presence of slots.
   </DELTA>
  <DELTA DATE='20010707'>reified <code>ENCODE-NEWLINE</code></DELTA>
  <DELTA DATE='20010803'>
   introduced uniform -interface specializers document, element, and property
   nodes</DELTA>
  <DELTA DATE='20010906'>modified <code>ENCODE-NODE</code> for name instances</DELTA>
  <DELTA DATE='20010910'>fix to encode node for default namespace names</DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
|#

(in-package "XML-PARSER")


;;
;;
;; interface functions

(defMacro with-xml-writer ((stream &optional (encoding :utf-8)) &rest body)
  "execute the body in a context appropriate to encode xml to the
   provided stream."
  `(multiple-value-bind (*writer-function *writer-arg)
                        (encoding-stream-writer ,stream ,encoding)
     ,@body))

(defGeneric write-node (datum stream)
  (:documentation
   "encode the node as xml to the provided stream.")
  (:method ((*document* doc-node) *output-destination*
            &aux (*namespace-bindings* *default-namespaces*))
           (with-xml-writer (*output-destination* (encoding *document*))
             (encode-node *document*)
             *document*))
  (:method ((node abstract-elem-node) *output-destination*
            &aux (*namespace-bindings* *default-namespaces*))
           (with-xml-writer (*output-destination* (encoding node))
             (encode-node node)
             node)))


;;
;;
;; primitives

(defun encode-char (char)
  "encode the provided character to the current output destination."
  (funcall *writer-function *writer-arg char))

(defun encode-string (string)
  "encode the provided string to the output destination.
   used when it is known that no escapes are necessary."
  (dotimes (i (length string))
    (funcall *writer-function *writer-arg (char string i))))

(defun encode-newline ()
  "emit a newline to the output destination."
  (encode-char #.(code-char #x0a)))

(defGeneric encode-node (node)
  (:documentation
   "encode the provided node to the current output destination.
    perform a tree walk for linked nodes.
    escape character data in content and attribute values as appropriate.
    introduce namespace bindings as required.")) 

(defMethod encode-node
           ((string string) &aux char)
  (dotimes (x (length string))
    (setf char (char string x))
    (case char
      (#\< (encode-string "&lt;"))
      (#\> (encode-string "&gt;"))
      (#\&
       ; here an attempt to both escape and not
       (if (position #\; string :start x)
         (funcall *writer-function *writer-arg char)
         (encode-string "&amp;")))
      (t (funcall *writer-function *writer-arg char)))))

(defMethod encode-node
           ((node list))
  (mapc #'encode-node node))

;;
;;
;; node methods

(defMethod encode-node
           ((node symbol)
            &aux (name (local-part node))
            (namespace (namespace node))
            prefix)
  (cond ((eq namespace *xmlns-namespace*)
         (encode-string *xmlns-prefix-namestring*)
         (unless (eq node *default-namespace-attribute-name*)
           (encode-char #\:)
           (encode-node name)))
        (namespace
         (setf prefix (local-part (namespace-prefix namespace)))
         (unless (string= prefix "")
           (encode-node (local-part prefix))
           (encode-char #\:))
         (encode-node name))
        (t ;; allow uninterned names
         (encode-node name))))

(defMethod encode-node
           ((node abstract-name)
            &aux (name (local-part node))
            (namespace (namespace node))
            prefix)
  (cond ((eq namespace *xmlns-namespace*)
         (encode-string *xmlns-prefix-namestring*)
         (unless (eq node *default-namespace-attribute-name*)
           (encode-char #\:)
           (encode-node name)))
        (namespace
         (setf prefix (local-part (namespace-prefix namespace)))
         (unless (string= prefix "")
           (encode-node (local-part prefix))
           (encode-char #\:))
         (encode-node name))
        (t ;; allow uninterned names
         (encode-node name))))

(defMethod encode-node
           ((node doc-node-interface)
            &aux
            (*prefix-count* *prefix-count*)
            (*namespace-bindings* *namespace-bindings*)
            (name nil)
            (encoding (encoding node))
            (standalone (standalone node))
            (version (version node)))
  (encode-string "<?xml")
  (when (zerop (length version))
    (setf version *xml-version*))
  (encode-string " version='")
  (encode-string version)
  (encode-char #\')
  (when encoding
    (encode-string " encoding='")
    (encode-string (string encoding))
    (encode-char #\'))
  (encode-string " standalone='")
  (encode-string (if standalone "yes" "no"))
  (encode-string "' ?>")
  (encode-char #\newline)
  (setf name (name (entity-info node)))
  (when (or (eq name *null-name*) (null name))
    (setf name (name (root node))))
  (encode-string "<!DOCTYPE ")
  (encode-node (local-part name))  ;; force the literal name w/o ns|prefix
  (encode-string " [")
  (flet ((encode-def (key def)
           (declare (ignore key))
           (encode-newline)
           (encode-node def)))
    (maphash #'encode-def (notations node))
    (maphash #'encode-def (parameter-entities node))
    (maphash #'encode-def (general-entities node)))
  (let* ((root (root node))
         (root-type (when root (def root)))
         (*node-cache* nil))
    (flet ((encode-def (key def)
             (declare (ignore key))
             (unless (find def *node-cache*)
               (push def *node-cache*)
               (encode-newline)
               (encode-node def))))
      ;; encode the root definition first. the method proceeds recursively,
      ;; which generates sensible namespace prefixes. any remaining
      ;; definitions are done subsequently
      (if root-type
        (encode-def nil root-type)
        (encode-string " <!-- no root element definition present -->"))
      (maphash #'encode-def (types node))))
  (encode-newline)
  (encode-string " ]>")
  (dolist (child (children node))
    (encode-newline)
    (encode-node child)))

(defun encode-generated-ns-bindings (generated-ns-bindings)
  (dolist (binding generated-ns-bindings)
    (encode-char #\space)
    (encode-node (first binding))
    (encode-char #\=)
    (encode-char #\')
    (encode-node (namespace-name (rest binding)))
    (encode-char #\')))

(defMethod encode-node
           ((node elem-node-interface))
  (let ((*namespace-bindings* *namespace-bindings*)
        (*node-level* (1+ *node-level*))
        (*prefix-count* *prefix-count*)
        (generated-ns-bindings nil))
    (declare (dynamic-extent *namespace-bindings*))
    (with-accessors ((name name)
                     (children children)
                     (attributes attributes)
                     (namespaces namespaces)) node
      (dolist (ns-node namespaces)
        (push ns-node *namespace-bindings*))
      ; (break "namespaces: ~s/~s." *namespace-bindings* namespaces)
      (encode-char #\<)
      (handler-bind ((|NSC: Prefix Declared|
                      #'(lambda (condition &aux (prefix (next-prefix))
                                           node)
                          (setf node (cons prefix (name condition)))
                          (push node *namespace-bindings*)
                          (push node generated-ns-bindings)
                          (use-value prefix))))
        (encode-node name)
        (dolist (node namespaces)
          (encode-char #\space)
          (encode-node node))
        (dolist (node attributes)
          (encode-char #\space)
          (encode-node node)))
      (when generated-ns-bindings
        (encode-generated-ns-bindings generated-ns-bindings))
      (cond (children
             (encode-char #\>)
             (dolist (node children)
               (when *print-pretty*
                 (encode-newline)
                 (dotimes (x *node-level*) (encode-char #\space)))
               (encode-node node))
             (encode-string "</")
             (encode-node name)
             (encode-char #\>))
            (t
             (encode-string " />"))))))

(defMethod encode-node
           ((node elem-property-node-interface))
  (with-slots (name children) node
    (encode-node name)
    (encode-char #\=)
    (encode-char #\')
    (dolist (node children)
      (encode-node node))
    (encode-char #\')))

(defmethod encode-node
           ((node pi-node))
  (encode-string "<?")
  (encode-node (name node))
  (encode-char #\space)
  (dolist (node (children node))
    (encode-node node))
  (encode-string "?>"))

(defMethod encode-node
           ((node comment-node))
  (encode-string "<!-- ")
  (dolist (node (children node))
    (encode-node node))
  (encode-string " -->"))

;;
;;
;; declarations

(defMethod encode-node ((node def-elem-type)
                        &aux
                        generated-ns-bindings
                        (*namespace-bindings* *namespace-bindings*)
                        c-def)
  (with-slots (name children properties) node
    ;; first do the definition itself, that is, the content model and the
    ;; attribute declarations. these all in the same namespace context
    (dolist (node properties)
      (when (is-ns-node (prototype node))
        (push (prototype node) *namespace-bindings*)))
    (handler-bind ((|NSC: Prefix Declared|
                    #'(lambda (condition &aux (prefix (next-prefix))
                                         node)
                        (setf node (cons prefix (name condition)))
                        (push node *namespace-bindings*)
                        (push node generated-ns-bindings)
                        (use-value prefix))))
      ;; write the element declaration
      (encode-string " <!ELEMENT ")
      (encode-node name)
      (encode-char #\space)
      (encode-node children)
      (encode-string " >")
      ;; write the attribute declarations
      (when properties
        (encode-newline)
        (encode-string " <!ATTLIST ")
        (encode-node name)
        (encode-char #\space)
        (mapc #'(lambda (n)
                  (encode-newline)
                  (encode-string "   ")
                  (encode-node n))
              properties)
        (encode-string " >"))
      (when generated-ns-bindings
        (encode-newline)
        (encode-string " <!ATTLIST ")
        (encode-node name)
        (encode-char #\space)
        (dolist (binding generated-ns-bindings)
          (encode-newline)
          (encode-string "   ")
          (encode-node (first binding))
          (encode-string " CDATA '")
          (encode-string (package-name (rest binding)))
          (encode-string "'"))
        (encode-string " >")))
    ;; then do type definitions referenced by virtue of their presence in the
    ;; content model
    (dolist (c-name (collect-model-names (first (bnfp::bnf-rhs (model node)))))
      (cond ((eq c-name *empty-name*) nil)
            ((eq c-name *wild-name*) nil)
            ((setf c-def (find-def-elem-type c-name (document node)))
             (unless (find c-def *node-cache*)
               (push c-def *node-cache*)
               (encode-newline)
               (encode-node c-def)))
            (t
             (encode-string
              (format nil " <!-- definition not found: ~a: ~a."
                      (name node) c-name)))))))

(defMethod encode-node ((node |content-model|))
  (encode-node (first (bnfp::bnf-rhs node))))
(defMethod encode-node ((node |\|-content|)
                        &aux (expressions (bnfp::bnf-expressions node)))
  (encode-char #\()
  (loop (encode-node (pop expressions))
        (if expressions (encode-string " | ") (return)))
  (encode-char #\)))
(defMethod encode-node ((node |?-content|))
  (encode-node (bnfp::bnf-expression node))
  (encode-char #\?))
(defMethod encode-node ((node |*-content|)
                        &aux (expression (bnfp::bnf-expression node))
                        name)
  (setf name
        (when (typep expression '|content-name|) (bnfp::bnf-name expression)))
  (cond ((eq name *wild-name*) (encode-string "ANY"))
        ((eq name *empty-name*) (encode-string "EMPTY"))
        ((eq name *mixed-name*) (encode-string "(#PCDATA)"))
        (t (encode-node expression)
           (encode-char #\*))))
(defMethod encode-node ((node |+-content|))
  (encode-node (bnfp::bnf-expression node))
  (encode-char #\+))
(defMethod encode-node ((node |content|))
  (encode-node (bnfp::bnf-expression node)))
(defMethod encode-node ((node |,-content|)
                        &aux (expressions (bnfp::bnf-expressions node)))
  (encode-char #\()
  (loop (encode-node (pop expressions))
        (if expressions (encode-string ", ") (return)))
  (encode-char #\)))
(defMethod encode-node ((node |content-name|)
                        &aux (name (bnfp::bnf-name node)))
  (cond ((eq name *mixed-name*) (encode-string "(#PCDATA)"))
        (t (encode-node (bnfp::bnf-name node)))))
(defMethod encode-node ((node |type-name|))
  (encode-node (bnfp::bnf-name node)))


;; this isn't complete, but it's a start

(defMethod encode-node ((node def-elem-property-type)
                        &aux (prototype (prototype node))
                        (default (children prototype)))
  (flet ((encode-enumeration (enumeration)
           (cond (enumeration
                  (encode-char #\()
                  (loop (encode-node (pop enumeration))
                        (if enumeration (encode-char #\|) (return)))
                  (encode-char #\)))
                 (t
                  (encode-string "()")))))
    (encode-node (name node))
    (typecase prototype
      (entity-attr-node (encode-string " ENTITY"))
      (entities-attr-node (encode-string " ENTITIES"))
      (enumeration-attr-node
       (encode-char #\space)
       (encode-enumeration (children node)))
      (id-attr-node (encode-string " ID"))
      (id-ref-attr-node (encode-string " IDREF"))
      (id-refs-attr-node (encode-string " IDREFS"))
      (nmtoken-attr-node (encode-string " NMTOKEN"))
      (nmtokens-attr-node (encode-string " NMTOKENS"))
      (notation-attr-node
       (encode-string " NOTATION")
       (encode-enumeration (children node)))
      (ns-node (encode-string " CDATA"))
      (attr-node (encode-string " CDATA"))
      )
    (encode-char #\space)
    (ecase (stipulation node)
      ((nil) )
      (:required (encode-string "#REQUIRED"))
      (:implied (encode-string "#IMPLIED"))
      (:fixed (encode-string "#FIXED")))
    (when default
      (encode-char #\space)
      (encode-char #\')
      (encode-node default)
      (encode-char #\'))))
  
  
(defMethod encode-node ((node def-notation))
  (with-slots (name system-id public-id) node
    (encode-string " <!NOTATION " )
    (encode-node name)
    (encode-char #\space)
    (cond (public-id
           (encode-string "PUBLIC '")
           (encode-string public-id)
           (when system-id
             (encode-string "' '")
             (encode-string system-id)))
          (t
           (encode-string "SYSTEM '")
           (encode-string system-id)))
    (encode-string "' >")))

(defMethod encode-node ((node ref-entity))
  (encode-char #\&)
  (encode-node (name node))
  (encode-char #\;))

(defMethod encode-node ((node def-internal-entity))
  (encode-string " <!ENTITY ")
  (when (is-def-parameter-entity node)
    (encode-string "% "))
  (encode-node (name node))
  (encode-string " '")
  (mapc #'(lambda (byte) (funcall *writer-function *writer-arg
                                  (code-char byte)))
        (children node))
  (encode-string "' >"))

(defMethod encode-node ((node def-external-entity))
  (encode-string " <!ENTITY ")
  (when (is-def-parameter-entity node)
    (encode-string "% "))
  (encode-node (name node))
  (let ((system (system-id node))
        (public (public-id node)))
    (cond (public (encode-string " PUBLIC '")
                  (encode-node public)
                  (encode-string "' '")
                  (encode-node system)
                  (encode-node "'"))
          (system (encode-string " SYSTEM '")
                  (encode-node system)
                  (encode-node "'"))))
  (encode-string " >"))


:EOF
