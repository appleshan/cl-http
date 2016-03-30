;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: xml-query-data-model; -*-

#|
<DOCUMENTATION>
 <DESCRIPTION>
  this file implements namespaces and defines the constructors to be used for the
  name form specified by the xml-symbols conditionalizer. name instance and property-access
  definitions are defined for both symbols and name instances - the programming interface
  only is optionally generated.</DESCRIPTION>
 <COPYRIGHT YEAR='2001' AUTHOR='james adam anderson' MARK='(C)'
            href='file://xml/sysdcl.lisp' />
 <CHRONOLOGY>
  <DELTA DATE='20010714'>namespace/package distinction</DELTA>
  <DELTA DATE='20010723'>escape syntax for name reader</DELTA>
  <DELTA DATE='20010810'>broadened to include limited parallel instance and symbol definitions.
   </DELTA>
  <DELTA DATE='20010917' AUTHOR='SK'>#. removed from reader macro for '{' for acl-6.0.1</DELTA>
  <DELTA DATE='20011203' AUTHOR='JAA'>added <code>make-load-form</code> for <code>uname</code>
   instances.</DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
|#

(in-package "XML-QUERY-DATA-MODEL")

(defParameter *namespace-dictionary* nil
  "binds additional, document-specific, aliases for namespaces as an a-list.
   where an alternative name/ alias is registered, it will be used in addition to
   the packages proper name / nicknames and preferred when serializing.")



(defParameter *qname-extent* nil
  "binds a generated identifier for each physical entity.
   used to partition qualified name extents.")

(defun new-qname-extent (&aux (context (or *qname-extent* "QE")))
  (gensym (concatenate 'string (string context) ".")))

(defun qname-extent-equal (e1 e2 &aux length)
  (setf e1 (string e1)
        e2 (string e2)
        length (min (length e1) (length e2)))
  (string= e1 e2 :end1 length :end2 length))


;; definitions for names specific to non-symbol names

(defClass nameset ()
  ((name :initarg :name :reader name)
   (names :initform (make-hash-table :test #'equalp) :reader namespace.names)
   (nicknames :initform nil :initarg :nicknames :reader namespace.nicknames)
   (use :initform nil :initarg :use :reader namespace.use)
   (export :initform nil :initarg :export :reader namespace.export)
   (static :initform t :initarg :static :accessor namespace.is-static)))

(defStruct (nameset-tokenizer (:include tk1::tokenizer) (:conc-name tokenizer-)
                              (:constructor %make-nameset-tokenizer))
  (nicknames nil)
  (use nil)
  (export nil)
  (is-static t :type (member t nil)))

(defun make-nameset-tokenizer (&key name nicknames use export intern (static t) (is-static static)
                                     &aux tokenizer)
  (multiple-value-bind (modulus rehash-size) 
                       (tk1::select-table-size  tk1::*default-tokenizer-size*)
    (setf tokenizer
          (%make-nameset-tokenizer
           :name name :nicknames nicknames
           :modulus modulus
           :table (tk1::allocate-table modulus)
           :mask (tk1::compute-mask #'string=)
           :function #'subseq
           :rehash-size rehash-size
           :is-static is-static
           :lock (www-utils::make-lock name :type :multiple-reader-single-writer)))
    
    (dolist (local-part intern)
      (intern-name local-part tokenizer))
    (setf (tokenizer-export tokenizer)
          (mapcar #'(lambda (local-part) (intern-name local-part tokenizer))
                  export))
    (use-namespace use tokenizer)
    tokenizer))

    


(defClass abstract-name ()
  ((prefix :initform nil :type (or null string)
           :documentation
           "binds the prefix used for the first qname which resolved to a given universal name.
            the reader is an explicit method which implements an optional lookup for u-names
            which bind no explicit prefix.")
   (local-part :initform nil :initarg :local-part :reader local-part :type (or null string))
   (namespace :initform nil :initarg :namespace :reader namespace))
  (:documentation
   "the class of universal names."))

(defMethod initialize-instance :after ((instance abstract-name) &key prefix)
  (setf (prefix instance) prefix))

(defClass uname (abstract-name) ())

(defClass qname (abstract-name)
  ((qname-extent :initform nil :initarg :qname-extent :accessor qname-extent)
   (content-name-type-name :initform nil :accessor content-name-type-name)
   (type-name-content-names :initform nil :accessor type-name-content-names))
  (:documentation
   "the class of qualified names. note that these specialize abstract name, which affords them
    a namespace property. this is permits assignment during the qname->uname resolution process
    prior to replacement with the ultimate uname."))

(defMethod print-object ((object abstract-name) stream)
  (if *print-pretty*
    (print-qname stream object)
    (print-unreadable-object (object stream :type t)
      (print-qname stream object))))

(defMethod make-load-form ((instance uname))
  (with-slots (prefix local-part namespace) instance
    (values `(intern-name ,local-part ,(namespace-name namespace))
            `(setf (prefix ,instance) ,prefix))))

(defMethod bnfp::bnf-namestring ((datum abstract-name)) (local-part datum))
(defMethod bnfp::name= ((name1 abstract-name) (name2 t)) (bnfp::name= (local-part name1) name2))
(defMethod bnfp::name= ((name1 t) (name2 abstract-name)) (bnfp::name= name1 (local-part name2)))
(defMethod bnfp::name-lessp ((name1 abstract-name) (name2 t)) (bnfp::name-lessp (local-part name1) name2))
(defMethod bnfp::name-lessp ((name1 t) (name2 abstract-name)) (bnfp::name-lessp name1 (local-part name2)))

(defParameter *class.uname* 'uname)
(defParameter *class.qname* 'qname)
(defParameter *class.namespace* 'nameset)
(defParameter *namespace-mode* :use
  "specify how to treat an existing namespace. the default for operations outseide of a document
   is to use the static instance. within a document the mode is rebound to :copy, which
   creates local versions of any static space.")

(defType namespace () '(or package nameset nameset-tokenizer))
(defun is-namespace (x) (typep x 'namespace))
(defType name () '(or (and symbol (not null)) abstract-name))
(defun is-name (x) (typep x 'name))



;; generic definitions for both symbol and name instance implementations

(defGeneric namespace (datum)
  (:method ((value symbol)) (symbol-package value)))

(defGeneric namespace-name (datum)
  (:method  ((value symbol) &aux (package (symbol-package value)))
    (if package (package-name package) (get value :namespace)))
  (:method ((value package))
    (declare (special *namespace-dictionary*))
    (or (first (rassoc value *namespace-dictionary*))
        (package-name value)))
  (:method ((value abstract-name) &aux (namespace (namespace value)))
    (if namespace (namespace-name namespace)))
  (:method ((namespace nameset))
    (declare (special *namespace-dictionary*))
    (or (first (rassoc namespace *namespace-dictionary*))
        (name namespace)))
  (:method ((namespace nameset-tokenizer))
           (declare (special *namespace-dictionary*))
           (or (first (rassoc namespace *namespace-dictionary*))
               (tokenizer-name namespace)))
  (:documentation
   "this method is from the infoset spec. it is suspect. for elements
    and attributes it returns the name of the namespace as it
    suggests. for an ns-node it returns the value of the node and nothing
    which has to do with its name as the name 'has' no namespace."))

(defGeneric (setf namespace-name) (namespace-name datum)
  (:documentation
   "method used during namespace propagation to assert the inferred namespace name.
    checks for conflicting constraints.")
  (:method  ((namespace-name string) (value symbol))
            (if (symbol-package value)
              (unless (eq (find-package namespace-name) (symbol-package value))
                (error "namespace already fixed: ~s." value))
              (setf (get value :namespace) namespace-name)))
  (:method ((namespace-name string) (value abstract-name))
           (if (namespace value)
             (unless (string= namespace-name (namespace-name value))
               (error "namespace already fixed: ~s." value))
             (setf (slot-value value 'namespace) (find-namespace namespace-name)))))

(defGeneric (setf namespace) (namespace datum)
  (:documentation
   "method used ti cache the namespace instance in name instances.
    does not applied to symbol form of names.")
  (:method ((namespace nameset) (name abstract-name))
           (setf (slot-value name 'namespace) namespace))
  (:method ((namespace nameset-tokenizer) (name abstract-name))
           (setf (slot-value name 'namespace) namespace))
  (:method ((namespace-name string) (value abstract-name))
           (if (namespace value)
             (unless (string= namespace-name (namespace-name value))
               (error "namespace already fixed: ~s." value))
             (setf (slot-value value 'namespace) (find-namespace namespace-name)))))
  
(defGeneric qname-extent (name)
  (:documentation
   "retrieve the noted extent at the point a name parsed.")
  (:method ((name symbol)) (get name :qname-extent))
  (:method ((name abstract-name)) nil))

(defGeneric (setf qname-extent) (extent name)
  (:documentation
   "note the extent active at the point when a name is parsed. this serves for homograph/synonym
    processing.")
  (:method (extent (name symbol))
           (setf (get name :qname-extent) extent))
  (:method (extent (name abstract-name))
           (warn "attempt to set qname extent of an abstract name: ~s: ~s."
                 name extent)))
  
(defGeneric content-name-type-name (name)
  (:method ((name symbol))
           (get name :type-name)))

(defGeneric (setf content-name-type-name) (type-name name)
  (:method ((type-name symbol) (name symbol))
           (setf (get name :type-name) type-name))
  (:method (type-name (name abstract-name))
           (warn "attempt to set type-name of an abstract name: ~s: ~s."
                 name type-name)))

(defGeneric type-name-content-names (name)
  (:method ((name symbol))
           (get name :content-names))
  (:method ((name abstract-name)) nil))

(defGeneric (setf type-name-content-names) (content-name name)
  (:method ((content-names list) (name symbol))
           (setf (get name :content-names) content-names))
  (:method (content-name (name abstract-name))
           (warn "attempt to set content-name of an abstract name: ~s: ~s."
                 name content-name)))

(defGeneric prefix (name)
  (:method ((name symbol) &aux package)
           (declare (special *default-namespace-attribute-name*))
           (or (get name :prefix)
               (if (setf package (symbol-package name))
                 (ignore-errors (namespace-prefix package))
                 *default-namespace-attribute-name*)))
  (:method ((name abstract-name) &aux namespace)
    (declare (special *default-namespace-attribute-name*))
    (with-slots (prefix) name
      (or (and (slot-boundp name 'prefix) prefix)
          (if (setf namespace (namespace name))
            (ignore-errors (namespace-prefix namespace))
            *default-namespace-attribute-name*)))))


(defGeneric (setf prefix) (prefix name)
  (:method ((prefix string) (name symbol))
           (setf (get name :prefix) prefix))
  (:method ((prefix null) (name symbol))
           (setf (get name :prefix) prefix))
  (:method ((prefix string) (name abstract-name))
           (setf (slot-value name 'prefix) prefix))
  (:method ((prefix null) (name abstract-name))
           (setf (slot-value name 'prefix) prefix))
  (:method ((prefix symbol) (name t))
           (setf (prefix name) (symbol-name prefix)))
  (:method ((prefix abstract-name) (name t))
           (setf (prefix name) (local-part prefix))))

(defGeneric local-part (x) 
  (:method ((name symbol))
           (or (get name :local-part) (symbol-name name))))

(defGeneric uname-equal (n1 n2)
  (:documentation
   "compare universal names: the interned values should be eq.
    default methods are provided for alternative types.")
  ;;(defMethod uname-equal ((q1 qname-value) (q2 t)) (uname-equal (value q1) q2))
  ;;(defMethod uname-equal ((q1 t) (q2 qname-value)) (uname-equal q1 (value q2)))
  ;;(defMethod uname-equal ((q1 qname-value) (q2 qname-value)) (eq (value q1) (value q2)))
  (:method ((q1 string) (q2 string)) (string= q1 q2))
  (:method ((q1 t) (q2 t)) nil)
  (:method ((q1 null) (q2 null)) nil)
  (:method ((q1 symbol) (q2 symbol)) (eq q1 q2))
  (:method ((u1 abstract-name) (u2 abstract-name)) (eq u1 u2)))
  
(defGeneric qname-equal (q1 q2)
  (:documentation
   "compare qualified names: values are equal if eq or, if not yet interned, they can be
    literally equal and in the same qualified name context.
    default methods are provided for alternative types.")
  (:method ((q1 t) (q2 t)) nil)
  (:method ((q1 string) (q2 string)) (string= q1 q2))
  (:method ((q1 symbol) (q2 symbol))
    (declare (special *distinguish-qname-homographs*))
    (declare (optimize (speed 3) (safety 0))
             (type symbol q1 q2))
    ;; true if either they are eq, or at least one is not yet interned
    ;; and the qualified names (prefix:local-part) match
    ;; and they are in the same qname extent
    (if (eq q1 q2)
      t
      (if (and (symbol-package q1) (symbol-package q2))
        nil
        (and (string= (symbol-name q1) (symbol-name q2))
             (eq (prefix q1) (prefix q2))
             ;; when distinguishing homographs, constrain equality by adjoining extents
             ;; otherwise homographs are identical.
             (if *distinguish-qname-homographs*
               (qname-extent-equal (qname-extent q1) (qname-extent q2))
               t)))))
  (:method ((q1 qname) (q2 qname))
    (declare (special *distinguish-qname-homographs*))
    (declare (optimize (speed 3) (safety 0)))
    ;; true if either they are eq, or at least one is not yet interned
    ;; and the qualified names (prefix:local-part) match
    ;; and they are in the same qname extent
    (if (eq q1 q2)
      t
      (if (and (namespace q1) (namespace q2))
        (or (eq (namespace q1) (namespace q2))
            (string= (namespace-name q1) (namespace-name q2)))
        (and (string= (local-part q1) (local-part q2))
             (eq (prefix q1) (prefix q2))
             ;; when distinguishing homographs, constrain equality by adjoining extents
             ;; otherwise homographs are identical.
             (if *distinguish-qname-homographs*
               (qname-extent-equal (qname-extent q1) (qname-extent q2))
               t))))))

(defGeneric find-name (namestring namespace)
  (:method (namestring (namespace package)) (find-symbol namestring namespace))
  (:method (namestring (namespace nameset)) (gethash namestring (namespace.names namespace)))
  (:method (namestring (namespace nameset-tokenizer)) (tk1::get-token namespace namestring)))

(defGeneric export-names (names namespace)
  (:method (names (namespace package))
           (export names namespace))
  (:method (names (namespace nameset))
           (dolist (name names)
             (unless (eq name (find-name (local-part name) namespace))
               (error "name not present in namespace: ~s: ~s." name namespace)))
           (with-slots (export) namespace
             (dolist (name names)
               (pushnew name export )))
           names)
  (:method (names (namespace nameset-tokenizer))
           (dolist (name names)
             (unless (eq name (find-name (local-part name) namespace))
               (error "name not present in namespace: ~s: ~s." name namespace)))
           (dolist (name names)
             (pushnew name (tokenizer-export namespace)))
           names))

(defMethod namespace.is-static ((tokenizer nameset-tokenizer)) (tokenizer-is-static tokenizer))
(defMethod (setf namespace.is-static) (value (tokenizer nameset-tokenizer))
  (setf (tokenizer-is-static tokenizer) value))
(defMethod namespace.nicknames ((tokenizer nameset-tokenizer)) (tokenizer-nicknames tokenizer))

(defGeneric use-namespace (namespaces-to-use namespace)
  (:method ((use list) (namespace string))
           (use-namespace use (find-namespace namespace :if-does-not-exist :error)))
  (:method ((use list) (namespace package))
           (use-package use namespace))
  (:method ((use list) (namespace nameset))
           (mapc #'(lambda (ns-name &aux ns old-name)
                     (etypecase ns-name
                       (string (setf ns (find-namespace ns-name :if-does-not-exist :error)))
                       (namespace (setf ns ns-name)))
                     (dolist (name (namespace.export ns))
                       (setf old-name (gethash (local-part name) (namespace.names ns)))
                       (when (and old-name (not (eq old-name name)))
                         (error "name already present in namespace: ~s: ~s." (local-part name) namespace))
                       (setf (gethash (local-part name) (namespace.names namespace)) name)))
                 use))
  (:method ((use list) (namespace nameset-tokenizer))
           (mapc #'(lambda (ns-name &aux ns old-name)
                     (etypecase ns-name
                       (string (setf ns (find-namespace ns-name :if-does-not-exist :error)))
                       (namespace (setf ns ns-name)))
                     (dolist (name (tokenizer-export ns))
                       (setf old-name (find-name (local-part name) ns))
                       (when (and old-name (not (eq old-name name)))
                         (error "name already present in namespace: ~s: ~s."
                                (local-part name) namespace))
                       (intern-name (local-part name) namespace)))
                 use)))

(defGeneric import-from-namespace (source-namespace namestrings namespace)
  (:method ((source-namespace string) (namestrings t) (namespace t))
           (import-from-namespace (find-namespace source-namespace :if-does-not-exist :error)
                                  namestrings namespace))
  (:method ((source-namespace t) (namestrings list) (namespace package))
           (import (mapcar #'(lambda (local-part)
                                 (or (find-name local-part source-namespace)
                                     (error "name not present in namespace: ~s: ~s."
                                            local-part source-namespace)))
                           namestrings)
                   namespace))
  (:method ((source-namespace t) (namestrings list) (namespace nameset) &aux name)
           (dolist (local-part namestrings)
             (unless (setf name (find-name local-part source-namespace))
               (error "name not present in namespace: ~s: ~s." local-part source-namespace))
             (when (gethash local-part (namespace.names namespace))
               (error "name already present in namespace: ~s: ~s." local-part namespace))
             (setf (gethash local-part (namespace.names namespace)) name)))
  (:method ((source-namespace t) (namestrings list) (namespace nameset-tokenizer) &aux name)
           (dolist (local-part namestrings)
             (unless (setf name (find-name local-part source-namespace))
               (error "name not present in namespace: ~s: ~s." local-part source-namespace))
             (when (tk1::get-token namespace local-part)
               (error "name already present in namespace: ~s: ~s." local-part namespace))
             (tk1::insert-token namespace name local-part))))


;; depending on conditionalization, generate name/namespace operators to use symbols and packages
;; or name instances and nameset or nameset-tokenizer instances.
;; as most look-up functions are generic (see above), the conditionalization applies to the
;; instantiation functions intended to be used by the parser.

#+xml-symbols
(progn
  (defMacro defNamespace (name &rest options)
    (pushnew '(:use) options :test #'eq :key #'first)
    `(defPackage ,name ,@options))
  
  (defun intern-name (namestring namespace)
    (intern namestring namespace))
  (defMacro make-name (namestring)
    `(make-symbol ,namestring))
  (defMacro make-qname (namestring)
    `(make-symbol ,namestring))
  
  (defMacro find-prefix (prefix namespace)
    `(find-symbol ,prefix ,namespace))
  (defMacro intern-prefix (prefix namespace)
    `(intern ,prefix ,namespace))
  
  (defMacro intern-type (type-name namespace)
    `(intern ,type-name ,namespace))
  
  (defun find-namespace (name &key (if-does-not-exist :error) (if-exists t) &aux space)
    (declare (special *namespace-dictionary* *null-namespace*))
    (setf space
          (etypecase name
            (namespace name)
            (null *null-namespace*)
            (string (or (rest (assoc name *namespace-dictionary* :test #'string=))
                        (find-package name)))))
    (if space
      (case if-exists
        (:error (error 'internal-xml-error
                       :format-string "Attempt to create a previously defined namespace: ~s"
                       :format-arguments (list name)))
        ((nil) nil)
        (t space))
      (case if-does-not-exist
        ((nil) nil)
        (:create (make-package name :use nil))
        (:error
         ;; an attempt to find the prefix for a namespace which was never bound constitues
         ;; an internal error: the parser should not be able to generate such names absent
         ;; the respective binding.
         (error 'internal-xml-error
                :format-string "Attempt to locate an undefined namespace: ~s"
                :format-arguments (list name))))))
  )


;; nb. this combination remains untested for 0.912 
#+(and (not xml-symbols) (not nameset-tokenizer))
(progn
  (defMacro defNamespace (name &rest options)
    (let ((export (rest (assoc :export options)))
          (nicknames (rest (assoc :nicknames options)))
          (use (rest (assoc :use options)))
          (import-from (mapcar #'rest (remove-if-not #'(lambda (key) (eq key :import-from))
                                                     options))))
      `(define-namespace ,name
         ,@(when export `(:export (list ,@export)))
         ,@(when nicknames `(:nicknames (list ,@nicknames)))
         ,@(when import-from `(:import-from ',import-from))
         ,@(when use `(:use (list ,@use))))))
  
  (defun define-namespace (name &key use nicknames intern import-from export &aux
                                namespace old-namespace)
    (dolist (n nicknames) (assert (stringp n)))
    (mapc #'(lambda (ns-name &aux ns)
              (when (setf ns (find-namespace ns-name :if-does-not-exist nil))
                (if old-namespace
                  (unless (eq old-namespace ns)
                    (error "namespace exists: ~s." ns-name))
                  (setf old-namespace ns))))
           (cons name nicknames))
    (setf namespace (make-instance *class.namespace* :name name :nicknames nicknames))
    (flet ((register-space (ns-name &aux ns-binding)
             (cond ((setf ns-binding (assoc ns-name *namespace-dictionary* :test #'string=))
                    (setf (rest ns-binding) namespace))
                   (t
                    (setf *namespace-dictionary*
                          (acons ns-name namespace *namespace-dictionary*))))))
      (mapc #'register-space nicknames)
      (register-space name))
    (dolist (local-part intern)
      (intern-name local-part namespace))
    (setf (slot-value namespace 'export)
          (mapcar #'(lambda (local-part) (intern-name local-part namespace))
                  export))
    (use-namespace use namespace)
    (mapc #'(lambda (import-spec &aux (ns-name (pop import-spec)) ns name)
              (import-from-namespace ns-name import-spec namespace))
            import-from)
    namespace)

  (defun intern-name (local-part namespace)
    (etypecase namespace
      (namespace nil)
      (string (setf namespace (find-namespace namespace :if-does-not-exist :error))))
    (or (find-name local-part namespace)
        (setf (gethash local-part (namespace.names namespace))
              (make-instance *class.uname* :local-part local-part :namespace namespace))))
  (defMacro make-name (local-part)
    `(make-instance *class.uname* :local-part ,local-part))
  (defMacro make-qname (local-part)
    `(make-instance *class.qname* :local-part ,local-part))
  
  (defMacro find-prefix (prefix namespace)
    `(find-name ,prefix ,namespace))
  (defMacro intern-prefix (prefix namespace)
    `(intern-name ,prefix ,namespace))
  
  (defMacro intern-type (type-name namespace)
    `(intern-name ,type-name ,namespace))
  

  (defun find-namespace (name &key (if-does-not-exist :error) (if-exists *namespace-node*)
                              &aux space)
    (declare (special *namespace-dictionary* *null-namespace*))
    (setf space
          (etypecase name
            (namespace name)
            (null *null-namespace*)
            (string (rest (assoc name *namespace-dictionary* :test #'string=)))))
    (if space
      (case if-exists
        (:error (error 'internal-xml-error
                       :format-string "Attempt to create a previously defined namespace: ~s"
                       :format-arguments (list name)))
        ((nil) nil)
        (:copy
         (when (and *document* (namespace.is-static space))
           (setf space (clone-namespace space))
           (setf (namespace.is-static space) nil)
           (dolist (name (namespace.nicknames space))
             (setf *namespace-dictionary* (acons name space *namespace-dictionary*)))
           (setf *namespace-dictionary* (acons name space *namespace-dictionary*))
           (push space (namespaces *document*)))
         space)
        (:use
         space))
      (case if-does-not-exist
        ((nil) nil)
        (:create
         (setf space (make-instance *class.namespace* :name name :use nil))
         (setf *namespace-dictionary* (acons name space *namespace-dictionary*))
         (when *document* (push space (namespaces *document*)))
         space)
        (:error
         ;; an attempt to find the prefix for a namespace which was never bound constitues
         ;; an internal error: the parser should not be able to generate such names absent
         ;; the respective binding.
         (error 'internal-xml-error
                :format-string "Attempt to locate an undefined namespace: ~s"
                :format-arguments (list name))))))
  (defun clone-namespace (namespace &aux new)
    (setf new (make-instance *class.namespace*))
    (setf (slot-value new 'name) (slot-value namespace 'name))
    (setf (slot-value new 'nicknames) (copy-list (slot-value namespace 'nicknames)))
    (setf (slot-value new 'use) (copy-list (slot-value namespace 'use)))
    (setf (slot-value new 'export) (copy-list (slot-value namespace 'export)))
    (with-slots (names) new
      (maphash #'(lambda (local-part name) (setf (gethash local-part names) name))
               (slot-value namespace 'names))))
  )

#+(and (not xml-symbols) nameset-tokenizer)
(progn
  ;; this implements namespaces based on tokenizers.
  ;; the tokenizers exist in two forms: either as static namespaces, within which program code
  ;; can define names, or as dynaimcally generated spaces, which are generated by the parser
  ;; at the point of first reference within a document and which exist only so long as the document
  ;; itself.
  (defMacro defNamespace (name &rest options)
    (let ((export (rest (assoc :export options)))
          (nicknames (rest (assoc :nicknames options)))
          (use (rest (assoc :use options)))
          (import-from (mapcar #'rest (remove-if-not #'(lambda (option) (eq (first option)
                                                                            :import-from))
                                                     options))))
      `(define-namespace ,name
                       ,@(when export `(:export (list ,@export)))
                       ,@(when nicknames `(:nicknames (list ,@nicknames)))
                       ,@(when import-from `(:import-from ',import-from))
                       ,@(when use `(:use (list ,@use))))))

  (defun define-namespace (name &key use nicknames intern import-from export &aux
                                namespace old-namespace)
    "create a static namespace and register it in the global namespace dictionary."
    (dolist (n nicknames) (assert (stringp n)))
    (mapc #'(lambda (ns-name &aux ns)
              (when (setf ns (find-namespace ns-name :if-does-not-exist nil ))
                (if old-namespace
                  (unless (eq old-namespace ns)
                    (error "namespace exists: ~s." ns-name))
                  (setf old-namespace ns))))
          (cons name nicknames))
    (setf namespace (make-nameset-tokenizer :name name :nicknames nicknames
                                            :intern intern
                                            :use use
                                            :export export))
    (flet ((register-space (ns-name &aux ns-binding)
             (cond ((setf ns-binding (assoc ns-name *namespace-dictionary* :test #'string=))
                    (setf (rest ns-binding) namespace))
                   (t
                    (setf *namespace-dictionary*
                          (acons ns-name namespace *namespace-dictionary*))))))
      (mapc #'register-space nicknames)
      (register-space name))

    (mapc #'(lambda (import-spec &aux (ns-name (pop import-spec)))
              (import-from-namespace ns-name import-spec namespace))
            import-from)
    namespace)

  (defun intern-name (local-part namespace)
    (etypecase namespace
      (namespace nil)
      (string (setf namespace (find-namespace namespace :if-does-not-exist :error))))
    (or (tk1::get-token namespace local-part)
        (tk1::insert-token namespace
                           (make-instance *class.uname* :local-part local-part
                                          :namespace namespace)
                            local-part)))
  (defMacro make-name (local-part)
    `(make-instance *class.uname* :local-part ,local-part))
  (defMacro make-qname (local-part)
    `(make-instance *class.qname* :local-part ,local-part))
  
  (defMacro find-prefix (prefix namespace)
    `(find-name ,prefix ,namespace))
  (defMacro intern-prefix (prefix namespace)
    `(intern-name ,prefix ,namespace))
  
  (defMacro intern-type (type-name namespace)
    `(intern-name ,type-name ,namespace))


  (defun find-namespace (name &key (if-does-not-exist :error) (if-exists *namespace-mode*)
                              &aux space)
    (declare (special *namespace-dictionary* *null-namespace* *document*))
    (setf space
          (etypecase name
            (namespace (let ((space name))
                         (setf name (tokenizer-name name))
                         (cond ((rest (assoc name *namespace-dictionary* :test #'string=)))
                               (t
                                (warn "unregistered namespace specified: ~s." space)
                                space))))
            (null *null-namespace*)
            (string (rest (assoc name *namespace-dictionary* :test #'string=)))))
    (if space
      (ecase if-exists
        (:error (error 'internal-xml-error
                       :format-string "Attempt to create a previously defined namespace: ~s"
                       :format-arguments (list name)))
        ((nil) nil)
        (:copy
         (when (and *document* (namespace.is-static space))
           (setf space (clone-namespace space))
           (setf (namespace.is-static space) nil)
           (dolist (name (namespace.nicknames space))
             (setf *namespace-dictionary* (acons name space *namespace-dictionary*)))
           (setf *namespace-dictionary* (acons name space *namespace-dictionary*))
           (push space (namespaces *document*)))
         (unless (stringp (caar *namespace-dictionary*)) (break))
         space)
        (:use
         space))
      (case if-does-not-exist
        ((nil) nil)
        (:create
         (setf space (make-nameset-tokenizer :name name :static nil))
         (dolist (name (namespace.nicknames space))
             (setf *namespace-dictionary* (acons name space *namespace-dictionary*)))
         (setf *namespace-dictionary* (acons name space *namespace-dictionary*))
         (when *document* (push space (namespaces *document*)))
         space)
        (:error
         ;; an attempt to find the prefix for a namespace which was never bound constitues
         ;; an internal error: the parser should not be able to generate such names absent
         ;; the respective binding.
         (error 'internal-xml-error
                :format-string "Attempt to locate an undefined namespace: ~s"
                :format-arguments (list name))))))

  (defun clone-namespace (tokenizer &aux (new (copy-nameset-tokenizer tokenizer)))
    (setf (tk1::tokenizer-table new) (tk1::allocate-table (tk1::tokenizer-modulus tokenizer)))
    (setf (tokenizer-nicknames new) (copy-list (tokenizer-nicknames new)))
    (setf (tokenizer-use new) (copy-list (tokenizer-use new)))
    (setf (tokenizer-export new) (copy-list (tokenizer-export new)))
    (setf (tk1::tokenizer-lock new)
          (www-utils::make-lock (tk1::tokenizer-name new) :type :multiple-reader-single-writer))
    (tk1::map-tokens tokenizer
                     #'(lambda (local-part name) (tk1::insert-token new name local-part)))
    new)

  (defGeneric describe-namespace (namespace &optional stream)
    (:method ((name string) &optional (stream *standard-output*)
              &aux (namespace (find-namespace name :if-does-not-exist nil)))
             (cond (namespace (describe-namespace namespace stream))
                   (t (format stream "~&Tokenizer: ~A not found." name))))
    (:method ((namespace nameset-tokenizer)  &optional (stream *standard-output*))
             (tk1::describe-tokenizer namespace stream)))
  )

(defNamespace ""
  (:export "xmlns" "document" "lt" "gt" "amp" "quot" "apos"))

(defNamespace "*"
  (:export "*"))

;; esp for lispworks, be careful not to bash the existing package
#+xml-symbols
(import (mapcar #'(lambda (string) (find-symbol string ""))
                '("" "lt" "gt" "amp" "quot" "apos"))
        "xml")
#-xml-symbols
(defNamespace "xml"
  (:nicknames "http://www.w3.org/XML/1998/namespace")
  (:import-from "" "lt" "gt" "amp" "quot" "apos"))
;(describe-namespace "xml")

(defNamespace "xmlns"
  ;; don't do this, it would say that {}xmlns == {xmlns}xmlns" which is not true and
  ;; conflicts with {}xmlns == {xmlns}||
  ;; (:import-from "" "xmlns")
  (:nicknames "http://www.w3.org/XML/1998/namespace/xmlns")
  (:export "xmlns" "xml" "" "*"))

(defNamespace "xhtml"
  (:nicknames "http://www.w3.org/1999/xhtml"))

(defNamespace "xsd"
  (:export "anyComplexType" "anySimpleType" "anyTree" "anyType" "anyURI"
           "attribute" "boolean" "comment" "decimal" "element" "float"
           "integer" "Name" "pi" "string"))

(defun |{-reader|
       (stream char
               &aux
               (namespace (make-array 32 :adjustable t :fill-pointer 0 :element-type 'character))
               (local-part (make-array 32 :adjustable t :fill-pointer 0 :element-type 'character)))
  (declare (ftype (function ((or character integer)) t) xml-namechar?))
  ;; read and intern a qname value
  (setf (fill-pointer namespace) 0
        (fill-pointer local-part) 0)
  (loop (setf char (read-char stream))
        (when (char= char #\}) (return))
        (vector-push-extend char namespace))
  (cond ((eql #\| (peek-char t stream))
         ;; where explicitly escaped allow non-name characters
         (read-char stream)
         (loop (setf char (read-char stream nil nil))
               (unless char (return))
               (when (eql #\| char)
                 (return))
               (vector-push-extend char local-part)))
        (t
         (loop (setf char (read-char stream nil nil))
               (unless char (return))
               (unless (xml-namechar? char)
                 (unread-char char stream)
                 (return))
               (vector-push-extend char local-part))))
  (intern-name (subseq local-part 0) (subseq namespace 0)))

(set-macro-character #\{ #'|{-reader| t)

; '{xml}qwe  (quote {xml}qwe) '{xml}|| '{xml}|aa aaa|
; (let ((x '{xml}qwe)) (case x (a 'a) ({xml}qwe x) (t nil)))
;(mapc #'describe-namespace '("" "*" "xml" "xmlns" "xhtml" "xsd"))

:EOF

