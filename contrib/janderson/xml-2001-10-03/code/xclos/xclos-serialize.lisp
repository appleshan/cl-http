;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: xml-parser; -*-


#|
<DOCUMENTATION>
 <DESCRIPTION>
  a serialization mechanism for clos instances.
  this extends <code>WRITE-NODE</code> with a generic model-driven method for
  clos instances
 
  </DESCRIPTION>
 <COPYRIGHT YEAR='2001' AUTHOR='james adam anderson' MARK='(C)'
            href='file://xml/sysdcl.lisp' />
 <CHRONOLOGY>
  <DELTA DATE='20010601' AUTHOR='JAA'/>
  </CHRONOLOGY>
 </DOCUMENTATION>
|#

(in-package "XML-PARSER")

(defParameter *generic-type-definitions* (make-hash-table))
(defParameter *serializer-definitions* nil
  "binds a set of serializer definitions for use during code generation.")
(defParameter *instance-identifier* nil
  "binds the generic identifier to be used to encode a given instance.
   where null, the class name is used.")
(defParameter *parent* nil)

(defClass serializer-definition ()
  ((name :initarg :name :accessor serializer-definition-name)
   (type :initarg :type :accessor serializer-definition-type)
   (reader :initarg :reader :accessor serializer-definition-reader)
   (writer :initarg :writer :accessor serializer-definition-writer)
   (serializer :initform 'encode-node-as :initarg :serializer :accessor serializer-definition-serializer)))

(defMethod initialize-instance ((instance serializer-definition) &rest initargs
                                &key accessor (reader accessor)
                                (writer (when accessor `(setf ,accessor))))
  (apply #'call-next-method instance :reader reader :writer writer
         initargs))

;;
;;
;; an abstract class for clos elements which can act as nodes

(defClass clos-elem-node (abstract-node)
  ())

(defClass clos-elem-child (named-node)
  ())

(defMethod children ((instance clos-elem-node))
  (with-slots (children) instance
    (if (slot-boundp instance 'children)
      children
      (flet ((generate-child (slot-definition &aux (name (slot-definition-name slot-definition)))
               (make-instance 'clos-elem-child :name name :parent instance)))
        (setf children (mapcar #'generate-child (class-instance-slots (class-of instance))))))))

(defMethod children ((instance clos-elem-child))
  (with-slots (name parent) instance
    (let ((value (slot-value parent name)))
      (if (listp value) value (list value)))))


;;
;;
;; extend the standard interface

(defGeneric encode-node-as (node as)
  (:method ((node t) (as t)) (encode-node node))
  (:documentation
   "encode a node in a specific form. by default defer to the generic method."))

(defMethod write-node ((node standard-object) stream)
  "encode a node as its 'type'. lacking a speciflaized method, this will defer to the generic
   method, which encodes all slots. bind the identifier to be used by respective encoding method."
  (multiple-value-bind (*writer-function *writer-arg)
                       (encoding-stream-writer stream (encoding node))
    (let ((*instance-identifier* (type-of node)))
      (encode-node-as node (type-of node)))
    node))

(defMethod encoding ((node standard-object))
  (if *document*
    (encoding *document*)
    :iso-8859-1))

;;
;;
;; serializer generation

(defClass clos-def-elem-type (def-elem-type)
  ((serializer-definitions :initform nil :initarg :serializer-definitions
                           :writer (setf serializer-definitions))))

(defMethod serializer-definitions ((def-type clos-def-elem-type))
  (with-slots (name serializer-definitions) def-type
    (if serializer-definitions
      serializer-definitions
      (setf serializer-definitions
            (mapcar #'compute-default-serializer-definition
                    (class-instance-slots (find-class name)))))))


(defMethod clos-def ((instance standard-object))
  (or (gethash (type-of instance) *generic-type-definitions*)
      (setf (gethash (type-of instance) *generic-type-definitions*)
            (make-instance 'clos-def-elem-type :name (type-of instance)))))


(defMethod compute-default-serializer-definition ((definition cons))
  (let ((name (slot-definition-name definition))
        (type (slot-definition-type definition)))
    (make-instance 'serializer-definition
      :name name :type type :reader name :writer `(setf ,name))))



;;
;; the generic method serializes all slots as content

(defMethod encode-node ((instance standard-object))
  (let* ((def (clos-def instance))
         (serializer-defs (serializer-definitions def))
         (datum nil)
         (*namespace-bindings* *namespace-bindings*)
         (*node-level* (1+ *node-level*))
         (*prefix-count* *prefix-count*)
         (generated-ns-bindings nil))
    (handler-bind ((|NSC: Prefix Declared|
                    #'(lambda (condition &aux (prefix (next-prefix))
                                         node)
                        (setf node (cons prefix (name condition)))
                        (push node *namespace-bindings*)
                        (push node generated-ns-bindings)
                        ;; push a default prefix as well.
                        (setf node (cons *default-namespace-attribute-name* (name condition)))
                        (push node *namespace-bindings*)
                        (push node generated-ns-bindings)
                        (use-value prefix))))
      (encode-char #\<)
      (encode-node *instance-identifier*)
      (encode-string " type='")
      (encode-node (name def))
      (encode-char #\'))
    (when generated-ns-bindings
      (encode-generated-ns-bindings generated-ns-bindings))
    (encode-char #\>)
    (flet ((encode-content (datum name)
             (typecase datum
               (abstract-elem-node (encode-node datum))
               (null nil)
               (standard-object
                (let ((*instance-identifier* name))
                  (encode-node datum)))
               (t
                (encode-char #\<)
                (encode-node name)
                (encode-char #\>)
                (encode-node datum)
                (encode-string "</")
                (encode-node name)
                (encode-char #\>)))))
      (dolist (csd serializer-defs)
        (setf datum (funcall (serializer-definition-reader csd) instance))
        (if (listp datum)
          (map nil #'(lambda (datum) (encode-content datum (serializer-definition-name csd))) datum)
          (encode-content datum (serializer-definition-name csd)))))
    (encode-string "</")
    (encode-node *instance-identifier*)
    (encode-char #\>)))

;;
;;
;; generate a compiled encode method from an element declaration and slot/serializer definitions

(defMacro defEncodeNode (class-name type-definition &rest serializers)
  (let ((encode-node-as-method (compute-encode-node-as-method class-name type-definition serializers)))
    `(progn (defMethod encode-node ((instance ,class-name))
              (encode-node-as instance ',class-name))
            ,encode-node-as-method)))

(defMacro defEncodeNodeAs (class-name type-definition &rest serializers)
  (compute-encode-node-as-method class-name type-definition serializers))

(defMethod compute-encode-node-as-method (class-name (type-definition string) serializers)
  (let* ((*class.def-elem-type* 'clos-def-elem-type)
         (declarations (children (document-parser type-definition :start-name '|ExtSubset|)))
         (def-type (find-if #'is-def-elem-type declarations)))
    (unless def-type
      (error "element declaration missing:~%~s" type-definition))
    
    ;; intern the parsed declarations, augmented with a declaration for the default namespace
    (intern-qnames (nconc (remove nil (mapcar #'qname-context declarations))
                          (list
                           (qname-context
                            (make-def-attr :name (name def-type)
                                           :attributes (list (make-def-elem-property-type
                                                              :name *default-namespace-attribute-name*
                                                              :prototype (make-ns-node :name *default-namespace-attribute-name*
                                                                                       :namespace *package*
                                                                                       :value "" :children '("")))))))))
    (assign-universal-names declarations)
    (compute-encode-node-as-method class-name def-type
                                (append (mapcar #'(lambda (properties) (apply #'make-instance 'def-serializer :name properties))
                                                serializers)
                                        (serializer-definitions def-type)))))



(defMethod compute-encode-node-as-method (class-name def-type *serializer-definitions*)
  (let ((as (name def-type))
        (properties (remove *xmlns-namespace* (properties def-type)
                            :key #'(lambda (def) (namespace (name def))))))
    ;; use the model to generate the encoding function
    `(defmethod encode-node-as ((instance ,class-name) (form (eql ',as))
                                &aux (*namespace-bindings* *namespace-bindings*)
                                (*node-level* (1+ *node-level*))
                                (*prefix-count* *prefix-count*) (generated-ns-bindings nil))
       (encode-char #\<)
       ,@(when properties
           `((handler-bind ((|NSC: Prefix Declared|
                             #'(lambda (condition &aux (prefix (next-prefix))
                                                  node)
                                 (setf node (cons prefix (name condition)))
                                 (push node *namespace-bindings*)
                                 (push node generated-ns-bindings)
                                 ;; push a default prefix as well.
                                 (setf node (cons *default-namespace-attribute-name* (name condition)))
                                 (push node *namespace-bindings*)
                                 (push node generated-ns-bindings)
                                 (use-value prefix))))
               (encode-node *instance-identifier*)
               (encode-string " type='")
               (encode-node (type-of ,*instance-identifier*))
               (encode-char #\')
               ,@(mapcar #'compute-attribute-serializer properties))))
       (encode-char #\>)
       (flet ((encode-content (datum name &optional (serializer #'encode-node))
                (typecase datum
                  (abstract-elem-node (encode-node datum))
                  (null nil)
                  (standard-object
                   (let ((*instance-identifier* name))
                     (encode-node datum)))
                  (t
                   (encode-char #\<)
                   (encode-node name)
                   (encode-char #\>)
                   (funcall serializer datum)
                   (encode-string "</")
                   (encode-node name)
                   (encode-char #\>)))))
         ,(compute-content-serializer (model def-type)))
       (encode-string "</")
       (encode-node *instance-identifier*)
       (encode-char #\>))))

(defun find-def-serializer (name)
  (or (find name *serializer-definitions* :key #'serializer-definition-name)
      (error "missing serializer definition: ~s." name )))
           
(defGeneric compute-attribute-serializer (def)
  (:method ((attr-def def-elem-property-type) &aux (name (name attr-def)))
           (let ((def-serializer (find-def-serializer name)))
             `(progn (encode-char #\space)
                     (encode-node ',(name attr-def))
                     (encode-string "='")
                     (,(serializer-definition-serializer def-serializer)
                      (value-string (,(serializer-definition-reader def-serializer) instance)))
                     (encode-char #\')))))

(defGeneric compute-content-serializer (model-particle)
  (:method ((particle t))
           (warn "no compute-content-serializer for particle: ~s." particle))
  (:method ((particles list))
           `(progn ,@(mapcar #'compute-content-serializer particles)))
  (:method ((particle |content-model|)) 
           (compute-content-serializer (bnfp::bnf-rhs particle)))
  (:method ((particle |\|-content|))
           "use the first element in the respectve models to decide which path to take"
           (flet ((compute-accessor-form (name)
                    `(,(serializer-definition-reader (find-def-serializer name)) ,*instance-identifier*)))
             `(cond ,@(mapcar #'(lambda (particle &aux (name (first-model-name particle)))
                                  `(,(compute-accessor-form name) ,(compute-content-serializer particle)))
                              (bnfp::bnf-expressions particle)))))
  (:method ((particle |?-content|) &aux (names (collect-model-names particle)) (name (first names)))
           "serialize a non-null value as a single element"
           (when (> (length names) 1)
             (error "repeated groups not supported: ~a" particle))
           (let ((def-serializer (find-def-serializer name)))
             `(when (,(serializer-definition-reader (find-def-serializer name)) *instance-identifier*)
                (encode-content (,(serializer-definition-reader def-serializer) ,*instance-identifier*)
                                ',name
                                (function ,(serializer-definition-serializer def-serializer))))))
  (:method ((particle |*-content|) &aux (names (collect-model-names particle)) (name (first names)))
           (case (length names)
             (0 (error "repeated content missing: ~a" particle))
             (1 t)
             (t (error "repeated groups not supported: ~a" particle)))
           (let ((def-serializer (find-def-serializer name)))
             `(dolist (datum (,(serializer-definition-reader def-serializer) ,*instance-identifier*))
                (encode-content datum ',name (function ,(serializer-definition-serializer def-serializer))))))
  (:method ((particle |+-content|) &aux (names (collect-model-names particle)) (name (first names)))
           (case (length names)
             (0 (error "repeated content missing: ~a" particle))
             (1 t)
             (t (error "repeated groups not supported: ~a" particle)))
           (let ((def-serializer (find-def-serializer name)))
             `(dolist (datum (,(serializer-definition-reader def-serializer) ,*instance-identifier*))
                (encode-content datum ',name (function ,(serializer-definition-serializer def-serializer))))))
  (:method ((particle |content|))
           (compute-content-serializer (bnfp::bnf-expression particle)))
  (:method ((context |,-content|))
           (compute-content-serializer (bnfp::bnf-expressions context)))
  (:method ((context |content-name|) &aux (name (bnfp::bnf-name context)))
           (let ((def-serializer (find-def-serializer name)))
             `(encode-content (,(serializer-definition-reader def-serializer) ,*instance-identifier*)
                              ',name
                              (function ,(serializer-definition-serializer def-serializer))))))



  

#|

;; interpretive serialization
(defclass c1 ()
  ((s1 :initform "one" :accessor s1)
   (s2 :initarg :s2 :initform "two" :accessor s2)))
(defClass c2 ()
  ((s3 :initform "three" :accessor s3)))

(write-node (make-instance 'c1 :s2 (list (make-instance 'c2) (make-instance 'c2))) *standard-output*)
(inspect (compute-def (make-instance 'c1 :s2 (list (make-instance 'c2) (make-instance 'c2)))))

;; model-based serialization

(defclass mc1 ()
  ((s1 :initarg :s1 :initform "one" :accessor s1)
   (s2 :initarg :s2 :initform '("two" "two two") :accessor s2)))

(defEncodeNode ((instance mc1))
  "<!ELEMENT MC1 (S2*)> <!ATTLIST MC1 S1 CDATA #IMPLIED> <!ELEMENT S2 (PCDATA) >")
(write-node (make-instance 'mc1 :s2 (list (make-instance 'c2) (make-instance 'c2))) *standard-output*)
  
|#
:EOF
