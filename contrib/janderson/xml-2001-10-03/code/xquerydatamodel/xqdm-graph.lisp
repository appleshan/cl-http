;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: xml-query-data-model; -*-

#|
<DOCUMENTATION>
 <DESCRIPTION>
  type and instance graphs via serialization to dot format.
  </DESCRIPTION>
 <COPYRIGHT YEAR='2001' AUTHOR='james adam anderson' MARK='(C)'
            href='file://xml/sysdcl.lisp' />
 <CHRONOLOGY>
  <DELTA DATE='20010324' AUTHOR='JAA'>new</DELTA>
  <DELTA DATE='20010906'>changes for name instances</DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
|#

(in-package "XML-QUERY-DATA-MODEL")


(defParameter *node-graph-cache* nil)

(defParameter *eol*
  (make-array 2 :element-type 'character
              :initial-contents '(#\return #\linefeed))
  "network-standard eol sequence for use with .dot files. otherwise windows
   interpreters can't parse them.")

(defGeneric write-node-graph (node stream)
  (:method (node (pathname pathname))
           (with-open-file (stream pathname :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
             (write-node-graph node stream)))
  (:method (node (pathname string))
           (write-node-graph node (pathname pathname)))
  (:method (node (*standard-output* stream))
           "the method for a named node expects to be applied to the root of the graph"
           (let ((*node-graph-cache* (make-hash-table))
                 (*gensym-counter* 0))
             (format *standard-output* "~a~a \"NODE-0\" {"
                     *eol* "digraph")
             (encode-node-graph node)
             (format *standard-output* "~a }~a" *eol* *eol*))))

(defGeneric encode-node-graph (node)
  (:documentation
   "write .dot form for a node to <code>*standard-output*</code>.
    where the node is a type definition, the content models are
    graphed recursively. where the node is an instance, the instance
    graph is generated using generated names."))

(defGeneric node-graph-properties (node)
  (:documentation
   "where properties are to be ascribed to a specific node,
    specialize the function to generate them as an p-list.")
  (:method ((node t)) nil))

(defGeneric node-link-properties (node1 node2)
  (:documentation
   "where properties are to be ascribed to links between specific nodes,
    specialize the function to generate them as an p-list.")
  (:method ((node1 t) (node2 t)) nil))

(defun encode-node-string (string &aux char)
  ;; write out the string with returns and quotes escaped
  (dotimes (i (length string))
    (case (setf char (char string i))
      (#.(code-char #x0A) (write-string "\\n" *standard-output*))
      (#\" (write-string "\\\"" *standard-output*))
      (t (write-char char *standard-output*)))))


(defMethod encode-node-graph ((node doc-node) &aux c-id)
  (format *standard-output* "~a  \"~a\" ~@[[~{~a=\"~a\"~^,~}]~];"
          *eol* (name node) (node-graph-properties node))
  (dolist (c (children node))
    (when (setf c-id (encode-node-graph c))
      (format *standard-output*
                   "~a  \"~a\" -> \"~a\" ~@[[~{~a=\"~a\"~^,~}]~];"
                   *eol* (name node) c-id (node-link-properties node c)))))

(defMethod node-graph-id ((node elem-node) &aux cache-entry)
  (cond ((setf cache-entry (gethash node *node-graph-cache*))
         (values (second cache-entry) (third cache-entry) nil))
        (t
         (let ((id (gensym (concatenate 'string (string (name node)) "-")))
               (props (node-graph-properties node)))
           (setf (gethash node *node-graph-cache*) (list node id props))
           (values id props t)))))

(defMethod encode-node-graph ((node elem-node) &aux c-id)
  (flet ((format-node (name properties)
           (format *standard-output* "~a  \"~a\" ~@[[~{~a=\"~a\"~^,~}]~];"
                   *eol* name properties))
         (format-link (from to properties)
           (format *standard-output*
                   "~a  \"~a\" -> \"~a\" ~@[[~{~a=\"~a\"~^, ~}]~];"
                   *eol* from to properties)))
    ;; instance graphs should, of themselves, be acyclic, but prepare
    ;; for tracing id links.
    (multiple-value-bind (id props new?)
                         (node-graph-id node)
      (cond (new?
             (format-node id props)
             (dolist (c (namespaces node))
               (when (setf c-id (encode-node-graph c))
                 (format-link id c-id (node-link-properties node c))))
             (dolist (c (attributes node))
               (when (setf c-id (encode-node-graph c))
                 (format-link id c-id (node-link-properties node c))))
             (dolist (c (children node))
               (when (setf c-id (encode-node-graph c))
                 (format-link id c-id (node-link-properties node c)))))
            (t
             (format-node id props)))
      id)))
           
(defMethod encode-node-graph ((node elem-property-node) &aux name id)
  (setf name (name node))
  (when (eq name *default-namespace-attribute-name*)
    (setf name "xmlns"))
  (setf id (gensym (concatenate 'string (string name) "-")))
  (format *standard-output* "~a  \"~a\" [label=\"~a='" *eol* id name)
  (map nil #'encode-node-string (children node))
  (format *standard-output* "'\"~@[~{,~a=\"~a\"~}~]];" (node-graph-properties node))
  id)

(defMethod encode-node-graph ((node ns-node) &aux name id)
  (setf name (name node))
  (when (eq name *default-namespace-attribute-name*)
    (setf name "xmlns"))
  (setf id (gensym (concatenate 'string (string name) "-")))
  (format *standard-output* "~a  \"~a\" [label=\"ns: ~a='" *eol* id name)
  (map nil #'encode-node-string (children node))
  (format *standard-output* "'\"~@[~{,~a=\"~a\"~}~]];" (node-graph-properties node))
  id)

(defMethod encode-node-graph ((node string) &aux id)
  (setf id (gensym "STRING-"))
  (format *standard-output* "~a  \"~a\" [label=\"'" *eol* id)
  (encode-node-string node)
  (format *standard-output* "'\"~@[~{,~a=\"~a\"~}~]];" (node-graph-properties node))
  id)

(defMethod encode-node-graph ((node comment-node) &aux id)
  (setf id (gensym "COMMENT-"))
  (format *standard-output* "~a  \"~a\" [label=\"'" *eol* id)
  (map nil #'encode-node-string (children node))
  (format *standard-output* "'\"~@[~{,~a=\"~a\"~}~]];" (node-graph-properties node))
  id)

(defMethod encode-node-graph ((node pi-node) &aux id)
  (setf id (gensym "PI-"))
  (format *standard-output* "~a  \"~a\" [label=\"~a: '" *eol* id (target node))
  (map nil #'encode-node-string (children node))
  (format *standard-output* "'\"~@[~{,~a=\"~a\"~}~]];" (node-graph-properties node))
  id)

(defMethod node-graph-id ((node def-elem-type) &aux cache-entry)
  (cond ((setf cache-entry (gethash node *node-graph-cache*))
         (values (second cache-entry) (third cache-entry) nil))
        (t
         (let ((id (name node))
               (props (node-graph-properties node)))
           (setf (gethash node *node-graph-cache*) (list node id props))
           (values id props t)))))

(defMethod encode-node-graph ((node def-elem-type) &aux c-def)
  (flet ((format-node (name properties)
           (format *standard-output* "~a  \"~a\" ~@[[~{~a=\"~a\"~^,~}]~];"
                   *eol* name properties))
         (format-link (from to properties)
           (format *standard-output*
                   "~a  \"~a\" -> \"~a\" ~@[[~{~a=\"~a\"~^,~}]~];"
                   *eol* from to properties)))
    (multiple-value-bind (id props new?)
                         (node-graph-id node)
      (cond (new?
             ; graph new node and recurse
             (format-node id props)
             ;; the q-name contexts has already resolved the successors, but that
             ;; is the closure. this expression required the immediate successors
             ;; only.
             (dolist (c-id
                      (collect-model-names (first (bnfp::bnf-rhs (model node)))))
               (cond ((eq c-id *empty-name*)
                      c-id)
                     ((eq c-id *wild-name*)
                      (unless (gethash *wild-name* *node-graph-cache*)
                        (setf props (node-graph-properties *wild-name*))
                        (setf (gethash node *node-graph-cache*)
                              (list c-id c-id props))
                        (format-node c-id props))
                      (format-link id c-id (node-link-properties node c-id)))
                     ((setf c-def (find-def-elem-type c-id (document node)))
                      (encode-node-graph c-def)
                      (format-link id c-id (node-link-properties node c-def)))
                     
                     (t
                      (warn "definition not found: ~a: ~a." node c-id)))))
            (t
             ;; a node for a given type is defined in the graph once only.
             ;; the other times, the id is serialized as the target of a link.
             ))
      id)))

;;
;; graphs for components used to resolve qualified names


(defMethod node-graph-id ((node qname-context) &aux cache-entry)
  (cond ((setf cache-entry (gethash node *node-graph-cache*))
         (values (second cache-entry) (third cache-entry) nil))
        (t
         (let ((id (gensym "NODE-"))
               (props (node-graph-properties node)))
           ; (break)
           (setf (gethash node *node-graph-cache*) (list node id props))
           (values id props t)))))

(defMethod node-graph-properties ((node qname-context))
  (list :|label|
        #|(make-symbol (with-output-to-string (stream) (print-qname (name node) stream)))|#
        (with-output-to-string (stream) (print-qname (name node) stream))))

(defMethod encode-node-graph ((node def-type-qname-context) &aux target-id)
  (flet ((format-node (name properties)
           (format *standard-output* "~a  \"~a\" ~@[[~{~a=\"~a\"~^,~}]~];"
                   *eol* name properties))
         (format-attr-link (from to properties)
           (format *standard-output*
                   "~a  \"~a\" -> \"~a\" ~@[[~{~a=\"~a\"~^, ~}]~];"
                   *eol* from to properties))
         (format-model-link (from to properties)
           (format *standard-output*
                   "~a  \"~a\" -> \"~a\" ~@[[~{~a=\"~a\"~^, ~}]~];"
                   *eol* from to properties))
         (format-ns-link (from to properties)
           (format *standard-output*
                   "~a  \"~a\" -> \"~a\" ~@[[~{~a=\"~a\"~^, ~}]~];"
                   *eol* from to properties))
         (format-parent-link (from to properties)
           (format *standard-output*
                   "~a  \"~a\" -> \"~a\" ~@[[~{~a=\"~a\"~^, ~}]~];"
                   *eol* from to properties)))
    (multiple-value-bind (id props new?)
                         (node-graph-id node)
      (cond (new?
             (with-slots (name parents children content-names attr-nodes ns-nodes) node
               (format-node id props)
               (when (slot-boundp node 'parents)
                 (dolist (p-context parents)
                   (when (setf target-id (encode-node-graph p-context))
                     (format-parent-link id target-id (node-link-properties node p-context)))))
               (dolist (c ns-nodes)
                 (when (setf target-id (encode-node-graph c))
                   (format-ns-link id target-id (node-link-properties node c))))
               (dolist (c attr-nodes)
                 (when (setf target-id (encode-node-graph c))
                   (format-attr-link id target-id (node-link-properties node c))))
               (dolist (c children)
                 (when (setf target-id (encode-node-graph c))
                   (format-model-link id target-id (node-link-properties node c))))
               (dolist (c-name content-names)
                 ;; if the content model name was never resolved to the name of another
                 ;; element, generate the reference node 'inline'
                 (unless (content-name-type-name c-name)
                   (format-node (gensym (concatenate 'string
                                                     (with-output-to-string (stream)
                                                       (print-qname c-name stream))
                                                     "-"))
                                nil)))))
            (t
             ;; no need to repeat
             ))
      id)))

(defMethod encode-node-graph ((node def-attr-qname-context) &aux target-id)
  (flet ((format-node (name properties)
           (format *standard-output* "~a  \"~a\" ~@[[~{~a=\"~a\"~^,~}]~];"
                   *eol* name properties))
         (format-attr-link (from to properties)
           (format *standard-output*
                   "~a  \"~a\" -> \"~a\" ~@[[~{~a=\"~a\"~^, ~}]~];"
                   *eol* from to properties))
         (format-ns-link (from to properties)
           (format *standard-output*
                   "~a  \"~a\" -> \"~a\" ~@[[~{~a=\"~a\"~^, ~}]~];"
                   *eol* from to properties))
         (format-parent-link (from to properties)
           (format *standard-output*
                   "~a  \"~a\" -> \"~a\" ~@[[~{~a=\"~a\"~^, ~}]~];"
                   *eol* from to properties)))
    (multiple-value-bind (id props new?)
                         (node-graph-id node)
      (cond (new?
             (with-slots (name  attr-nodes ns-nodes assignment) node
               (format-node id props)
               (when assignment
                 (when (setf target-id (encode-node-graph assignment))
                   (format-parent-link id target-id (node-link-properties node assignment))))
               (dolist (c ns-nodes)
                 (when (setf target-id (encode-node-graph c))
                   (format-ns-link id target-id (node-link-properties node c))))
               (dolist (c attr-nodes)
                 (when (setf target-id (encode-node-graph c))
                   (format-attr-link id target-id (node-link-properties node c))))))
            (t
             ;; no need to repeat
             ))
      id)))


           
:EOF
