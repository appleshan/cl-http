;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: xml-query-data-model; -*-

#|
<DOCUMENTATION>
 <DESCRIPTION>
  this file declares the conditions which denote deviation from the
  well-formedness, validity, and namespace constraints descirbed in the
  respective specification.
  <p>
  the conditions are defined here together with check-methods. the checks are
  triggered during class instantiation via :after methods which are also
  defined here. checks during model operations and parsing are triggered
  elsewhere.</p>
  </DESCRIPTION>
 <COPYRIGHT YEAR='2001' AUTHOR='james adam anderson' MARK='(C)'
            href='file://xml/sysdcl.lisp' />
 <CHRONOLOGY>
  <DELTA DATE='20010318' AUTHOR='JAA'>
   factoer out as a distinct file.</DELTA>
  <DELTA DATE='20010623'>WFC: Version Match</DELTA>
  <DELTA DATE='20010918'>report and properties corrected for |WFC: Parsed Entity|
   and |VC: Name Token|.</DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
|#

(in-package "XQDM")
               
;;
;;
;; abstract conditions

(define-condition xml-error (error) ()
  (:report (lambda (condition stream)
             (call-next-method)
             (let ((string (condition-format-string condition)))
               (when  (boundp 'bnfp::*atn-input)
                 (fresh-line stream)
                 (format stream "while processing xml stream ~a."
                         (parse-state bnfp::*atn-input)))
               (when string
                 (fresh-line stream)
                 (apply #'format stream string
                        (condition-format-arguments condition)))))))

(define-condition simple-xml-error (xml-error terminal-parse-error)
  ((format-string :initform nil :initarg :format-string :reader condition-format-string)
   (format-arguments :initform nil :initarg :format-arguments :reader condition-format-arguments)))

(defClass wellformedness-error (xml-error terminal-parse-error) () )
(defClass wellformedness-cerror (xml-error continuable-parse-error) () )
(defClass namespace-error (xml-error continuable-parse-error)
  ((name :initform nil :initarg :name :reader name)) )
(defClass validity-error (xml-error continuable-parse-error) ())
(defClass internal-xml-error (simple-xml-error) ())
(defClass document-model-error (simple-xml-error) ())
(defClass xml-eof-error (simple-xml-error parse-eof-error) ())


(define-condition production-error (wellformedness-error)
  ((name))
  (:report (lambda (condition stream)
             (with-slots (name) condition
               (let ((production (get name :production)))
                 (call-next-method)
                 (fresh-line stream)
                 (print-xml-error-message condition stream
                                          "production failed~@[: ~s~]."
                                          production name))))))

(define-condition incomplete-parse (wellformedness-error)
  ((result :initarg :result :initform nil :accessor condition-result)
   (document :initarg :document :initform nil :accessor condition-document))
  (:report (lambda (condition stream)
             (with-slots (result) condition
               (call-next-method)
               (print-xml-error-message condition stream
                                        "incomplete parse~@[: result ~s~]."
                                        result)))))

(defMethod print-xml-error-message ((condition xml-error) stream format &rest args)
  (fresh-line stream)
  (format stream "[~a] : " (type-of condition))
  (apply #'format stream format args))

(defGeneric xml-error (condition &rest args)
  (:documentation
   "distinguish error handling for terminal/continuable/validity error.
    allow as specification<ul>
    <li>a symbol with args, which generates the respective condition instance,</li>
    <li>a condition with args, which initializes the condition</li>
    <li>a condition, which uses it as is</li>
    </ul>")
  (:method ((condition null) &key &allow-other-keys)
           ; error is ignored
           nil)
  (:method ((condition symbol) &rest args)
           (declare (dynamic-extent args))
           (apply #'xml-error (allocate-instance (find-class condition)) args))
  (:method ((condition error)  &rest args)
           (when args (setf condition (apply #'initialize-instance condition args)))
           (error condition))
  (:method ((condition terminal-parse-error) &rest args)
           (when args (setf condition (apply #'initialize-instance condition args)))
           (error condition))
  (:method ((condition continuable-parse-error) &rest args)
           (when args (setf condition (apply #'initialize-instance condition args)))
           (apply #'cerror (condition-continue-format-string condition)
                  condition (condition-continue-format-arguments condition)))
  (:method ((condition validity-error) &rest args)
           (when (validate? *document*) (apply #'call-next-method condition args)))
  (:method ((message string) &rest args)
           (xml-error 'internal-xml-error
                      :format-string message :format-arguments args)))

(defun xml-eof-error (&rest args)
  (apply #'xml-error 'xml-eof-error args))

(defGeneric xml-warn (datum &rest args)
  (:method ((format-string string) &rest args)
           (apply #'warn format-string args)
           (when (boundp 'bnfp::*atn-input)
             (fresh-line *error-output*)
             (format *error-output* "while processing xml stream ~a."
                     (parse-state bnfp::*atn-input)))))

;;
;;
;; condition definition macros

(defMacro defException
          ((name kind) super slots &rest options
           &aux (checks (remove :check options :test-not #'eq :key #'first))
           (c-name (intern (concatenate 'string (string kind) ": " (string name))))
           (f-name (intern (concatenate 'string "XML-ERROR-" (string c-name)))))
  `(progn (eval-when (:compile-toplevel :load-toplevel :execute)
            (export '(,c-name ,f-name)))
          (define-condition ,c-name ,super ,slots
            ,@(set-difference options checks))
          (defParameter ,c-name ',c-name)
          (defun ,f-name (&rest args)
            (apply #'xml-error ,c-name args))
          ,@(mapcar #'(lambda (check)
                        (destructuring-bind ((condition instance
                                                        &rest parameter-list)
                                             &rest body)
                                            (rest (second check))
                          `(defMethod check-constraint
                                      (,(if (symbolp condition)
                                          `(,condition ,c-name)
                                          condition)
                                       ,instance
                                       ,@parameter-list)
                             ,@body)))
                    checks)))

(defMacro defWFC (name &optional super slots &rest options)
  `(defException (,name "WFC") ,(or super '(wellformedness-error))
     ,slots
     ,@options))

(defMacro defWFCProduction (name &optional super slots &rest options)
  `(defException (,(format nil "[~a]" name) "WFC") ,(or super '(production-error))
     ((name :initform ',(intern name) :allocation :class)
      ,@slots)
     ,@options))

(defMacro defNSC (name &optional super slots &rest options)
  `(defException (,name "NSC") ,(or super '(namespace-error))
     ,slots
     ,@options))
  
(defMacro defVC (name &optional super slots &rest options)
  `(defException (,name "VC") ,(or super '(validity-error))
     ,slots
     ,@options))

(defMacro defDMC (name &optional super slots &rest options)
  `(defException (,name "DMC") ,(or super '(document-model-error))
     ,slots
     ,@options))


#+CCL
(progn
  (pushnew '(defWFC . 2) *FRED-SPECIAL-INDENT-ALIST* :key #'first)
  (pushnew '(defNSC . 2) *FRED-SPECIAL-INDENT-ALIST* :key #'first)
  (pushnew '(defVC . 2) *FRED-SPECIAL-INDENT-ALIST* :key #'first)
  nil)
;(setf (rest (assoc 'define-condition  *FRED-SPECIAL-INDENT-ALIST*)) 2)


;;
;;
;; 

(defGeneric check-constraint (constraint-condition instance)
  (:method ((constraint null) (instance t)) nil)
  (:method ((constraint symbol) (instance t))
           ;; generate an instance for a constraint name
           (check-constraint (allocate-instance (find-class constraint)) instance))
  (:method ((constraint t) (instance t))
           (warn "constraint does not apply to instance: ~s: ~s."
                 constraint instance))
  (:documentation
   "collects the condition/instance related constraints in a single function.
    implementations are provided for the initial values for the respective conditions.
    a method for the <code>NULL</code> case serves, where a condition has been rebound
    to <code>NIL</code>, to disable the constraint."))

;;
;;
;; concrete conditions

(defClass parse-context-error (wellformedness-error)
  ((context :initarg :context :initform nil)))

(defVC "Attribute Default Legal" )

(defVC "Attribute Value Type" )

(defVC "Attribute Declared" )

(defWFC "Byte Order Mark" ()
  ((data :initarg :data :initform nil))
  (:report (lambda (condition stream)
             (with-slots (data) condition
               (call-next-method)
               (print-xml-error-message
                condition stream
                "illegal byte order mark: ~s." data)))))

(defNSC "CDATA Required for Namespace Node" )

(defWFC "Character Reference" ()
  ((format-string :initform "invalid character reference syntax: ~s.")
   (format-arguments :initform '(nil))))

(defWFC "Character Context" (parse-context-error)
  ((character :initarg :character :initform nil))
  (:report (lambda (condition stream)
             (with-slots (context character) condition
               (call-next-method)
               (print-xml-error-message
                condition stream
                "character not permitted in this context: ~s."
                character)))))

(defWFCProduction "CharRef" ())

(defWFCProduction "Comment" ())

(defWFC "Element Type Match" ()
  ((start-name :initform nil :initarg :start-name)
   (end-name :initform nil :initarg :end-name))
  (:report (lambda (condition stream)
             (with-slots (start-name end-name) condition
               (call-next-method)
               (print-xml-error-message
                condition stream "start tag identifier does not match end: ~s: ~s."
                start-name end-name)))))

(defNSC "Element Type Match" ()
  ((start-name :initform nil :initarg :start-name)
   (end-name :initform nil :initarg :end-name))
  (:report (lambda (condition stream)
             (with-slots (start-name end-name) condition
               (call-next-method)
               (print-xml-error-message
                condition stream "start tag identifier does not match end: ~s: ~s."
                start-name end-name)))))

(defVC "Element Valid" ()
  ((name :initform nil :initarg :name))
  (:report (lambda (condition stream)
             (with-slots (name) condition
               (call-next-method)
               (print-xml-error-message
                condition stream "there must be a definition where the name matches: ~s."
                name)))))

(defVC "Element Content" ()
  ((model :initform nil :initarg :model)
   (content :initform nil :initarg :content))
  (:report (lambda (condition stream &aux (*print-length* 7))
             (with-slots (content model) condition
               (call-next-method)
               (print-xml-error-message
                condition stream "content must match the element model: ~s: ~a."
                content (with-output-to-string (stream) (pprint model stream)))))))

(defVC "Entity Declared" ()
  ((name :initarg :name :initform nil))
  (:report (lambda (condition stream)
             (with-slots (name) condition
               (call-next-method)
               (print-xml-error-message
                condition stream "entity not defined prior to use: ~a." name)))))

(defWFC "Entity Declared" ()
  ((name :initarg :name :initform nil))
  (:report (lambda (condition stream)
             (with-slots (name) condition
               (call-next-method)
               (print-xml-error-message
                condition stream "entity not defined prior to use: ~a." name)))))

(defVC "Entity Name" ()
  ((name :initarg :name :initform nil)
   (context :initarg :context :initform nil)
   (detail :initarg :detail :initform nil))
  (:report (lambda (condition stream)
             (with-slots (context name detail) condition
               (call-next-method)
               (print-xml-error-message
                condition stream "erroneous entity name: ~s : ~s~@[ (~a)~]."
                context name detail)))))

(defWFCProduction "EntityRef" ())

(defVC "Enumeration" ()
  ((name :initarg :name :initform nil))
  (:report (lambda (condition stream)
             (with-slots (name) condition
               (call-next-method)
               (print-xml-error-message
                condition stream "all enumeration values must be be declared: ~s." name)))))

(defWFC "External Subset" ()
  ((system-id :initarg :system-id :initform nil)
   (public-id :initarg :public-id :initform nil)
   (datum :initarg :datum :initform nil))
  (:report (lambda (condition stream)
             (with-slots (system-id public-id datum) condition
               (call-next-method)
               (print-xml-error-message
                condition stream
                "entity identified as (SYSTEM ~s PUBLIC ~s) does not have the form of an external subset:~%~a"
                system-id public-id datum)))))

(defWFC "External Parsed Entity" ()
  ((system-id :initarg :system-id :initform nil)
   (public-id :initarg :public-id :initform nil)
   (datum :initarg :datum :initform nil))
  (:report (lambda (condition stream)
             (with-slots (system-id public-id datum) condition
               (call-next-method)
               (print-xml-error-message
                condition stream
                "entity identified as (SYSTEM ~s PUBLIC ~s) does not have the form of an external parsed entity:~%~a"
                system-id public-id datum)))))

        

(defVC "Fixed Attribute Default" ()
  ((name :initarg :name :initform nil)
   (default :initarg :default :initform nil)
   (encoded :initarg :encoded :initform nil))
  (:report (lambda (condition stream) (break)
             (with-slots (name default encoded) condition
               (call-next-method)
               (print-xml-error-message
                condition stream "attribute value does not match default: ~a: ~s: ~s:"
                name default encoded)))))

(defVC "ID" ()
  ((name :initarg :name :initform nil)
   (value :initarg :value :initform nil)
   (detail :initarg :detail :initform nil))
  (:report (lambda (condition stream)
             (with-slots (name value detail) condition
               (call-next-method)
               (print-xml-error-message
                condition stream
                "ID values must match the Name production and be unique: ~s: ~s~@[ (~a)~]."
                name value detail)))))

(defVC "ID Attribute Default" ()
  ((name :initform nil :initarg :name))
  (:report (lambda (condition stream)
             (with-slots (name) condition
               (call-next-method)
               (print-xml-error-message
                condition stream "ID attributes must have a declared default of #IMPLIED or #REQUIRED: ~s."
                name)))))

(defVC "IDRef" ()
  ((name :initform nil :initarg :name))
  (:report (lambda (condition stream)
             (with-slots (name) condition
               (call-next-method)
               (print-xml-error-message
                condition stream "IDREF values must match the Name production: ~s."
                name)))))

(defVC "IDRef Target" ()
  ((name :initform nil :initarg :name))
  (:report (lambda (condition stream)
             (with-slots (name) condition
               (call-next-method)
               (print-xml-error-message
                condition stream
               "IDREF values must match the value of an ID attribute on some element in the document: ~s."
                name)))))
           

(defWFC "In DTD" )

(defNSC "Leading XML" ()
  ()
  (:report (lambda (condition stream)
             (with-slots (name) condition
               (call-next-method)
               (print-xml-error-message
                condition stream "'xml' prefix precluded in attribute name: ~s." name)))))

(defWFC "Legal Character" ()
  ((code :initarg :code :initform nil)
   (name :initarg :name :initform nil))
  (:report (lambda (condition stream)
             (with-slots (code name) condition
               (call-next-method)
               (print-xml-error-message
                condition stream
                "character name/code error: ~s/#x~x (~a)."
                name code 
                (if (<= code char-code-limit) "not permitted" "not supported"))))))

(defWFC "Name" () )

(defVC "Name Token" ()
  ((name :initarg :name :initform nil)
   (value :initarg :value :initform nil)
   (detail :initarg :detail :initform nil))
  (:report (lambda (condition stream)
             (with-slots (name value detail) condition
               (call-next-method)
               (print-xml-error-message
                condition stream
                "NMTOKEN values must match the Nmtoken production: ~s: ~s~@[ (~a)~]."
                name value detail)))))

(defDMC "NCName Required" ()
  ((node :initform nil :initarg :node))
  (:report (lambda (condition stream)
             (with-slots (node) condition
               (call-next-method)
               (print-xml-error-message
                condition stream
                "nc-name required for node: ~s." node))))
  (:check (lambda (constraint (instance ncnamed-node))
            (with-slots (name) instance
              (unless (or (typep name 'name) (typep name 'string))
                (xml-error |DMC: NCName Required| :node instance))))))

(defMethod initialize-instance :after
           ((instance ncnamed-node) &key)
  (check-constraint |DMC: NCName Required| instance))

(defVC "No Duplicate Tokens" ()
  ((name :initarg :name :initform nil)
   (context :initarg :context :initform nil))
  (:report (lambda (condition stream)
             (with-slots (name context) condition
               (call-next-method)
               (print-xml-error-message
                condition stream
                "The names in a single declaration must all be distinct."
                context name )))))

(defVC "No Duplicate Types" ()
  ((name :initarg :name :initform nil)
   (context :initarg :context :initform nil))
  (:report (lambda (condition stream)
             (with-slots (name context) condition
               (call-next-method)
               (print-xml-error-message
                condition stream
                "The same name must not appear more than once in a single mixed-content declaration: ~s: ~s."
                context name )))))

(defWFC "No External Entity References" )

(defWFC "No Recursion" ()
  ((entity :initarg :entity :initform nil))
  (:report (lambda (condition stream)
             (with-slots (entity) condition
               (call-next-method)
               (print-xml-error-message
                condition stream "no entity recursion permitted: ~s" entity)))))

(defWFC "No < in Attribute Values" )

(defNSC "No Null Namespace Bindings" ()
  ((continue-format-string :initform "permit the binding." :allocation :class))
  (:report (lambda (condition stream)
             (with-slots (name) condition
               (call-next-method)
               (print-xml-error-message
                condition stream "no null namespace binding permitted: ~s." name)))))

(defVC "Notation Attributes" ()
  ((name :initarg :name :initform nil))
  (:report (lambda (condition stream)
             (with-slots (name) condition
               (call-next-method)
               (print-xml-error-message
                condition stream "all notation names must be declared: ~s." name)))))

(defVC "One ID per Element Type" ()
  ((name :initform nil :initarg :name)
   (attribute-names :initform nil :initarg :attribute-names))
  (:report (lambda (condition stream)
             (with-slots (name attribute-names) condition
               (call-next-method)
               (print-xml-error-message
                condition stream "no element type may have more than one ID attribute specified: ~s: ~s."
                name attribute-names))))
   (:check (lambda (condition (node def-elem-type))
             (check-constraint condition (properties node))))
   (:check (lambda (condition (declarations list))
             (when (> (count-if #'is-id-attr-node declarations :key #'prototype) 1)
               (xml-error condition
                          :name (name (parent (first declarations)))
                          :attribute-names (mapcar #'name declarations))))))

(defMethod initialize-instance :after ((instance def-elem-type) &key)
  (dolist (def (properties instance)) (setf (parent def) instance))
  (check-constraint |VC: One ID per Element Type| instance))

(defMethod (setf properties) :before (new-value (instance def-elem-type))
  (dolist (def new-value) (setf (parent def) instance))
  (check-constraint |VC: One ID per Element Type| new-value))
  

(defWFC "Parsed Entity" (wellformedness-cerror)
  ((name :initform nil :initarg :name))
  (:report (lambda (condition stream)
             (with-slots (name) condition
               (call-next-method)
               (print-xml-error-message
                condition stream
                "An entity reference must not contain the name of an unparsed entity: ~s."
                name)))))

(defWFC "PE Between Declarations" )

(defWFC "PEs in Internal Subset" (wellformedness-cerror)
  ((name :initform nil :initarg :name)
   (continue-format-string :initform "permit the reference" :allocation :class))
  (:report (lambda (condition stream)
             (with-slots (name) condition
               (call-next-method)
               (print-xml-error-message
                condition stream
                "In the internal DTD subset, parameter-entity references can occur only where markup declarations can occur: ~a."
                name)))))
        

(defNSC "Prefix Declared" ()
  ((continue-format-string :initform "ascribe name to null namespace." :allocation :class))
  (:report (lambda (condition stream)
             (with-slots (name) condition
               (call-next-method)
               (print-xml-error-message
                condition stream "no visible binding for prefix: ~s." name)))))

(defWFC "Predefined Entity" (wellformedness-cerror)
  ((name :initform nil :initarg :name)
   (type :initform nil :initarg :type)
   (continue-format-string :initform "ignore new definition" :allocation :class))
  (:report (lambda (condition stream)
             (with-slots (name type) condition
               (call-next-method)
               (print-xml-error-message
                condition stream "duplicate ~@[~a ~]entity definition precluded: ~a."
                type name))))
  (:check (lambda (condition (instance def-parameter-entity)
                             &aux old-def (name (name instance)))
            (cond ((and (setf old-def
                              (find-def-parameter-entity name (document instance)
                                                         :if-does-not-exist nil))
                        (eq (extent old-def) :static)
                        (not (equalp (children old-def) (children instance))))
                   (xml-error |WFC: Predefined Entity|
                              :name name :type (type-of instance))
                   nil)
                  (t t))))
  (:check (lambda (condition (instance def-general-entity)
                             &aux old-def (name (name instance)))
            (cond ((and (setf old-def (find-def-general-entity name (document instance)
                                                               :if-does-not-exist nil))
                        (eq (extent old-def) :static)
                        (not (equalp (children old-def) (children instance)))
                        #| 20010506 no longer necessary, as entity value is normalized upon instantiation
                        ;; special case for &amp;  &lt, ... which have varying definitions
                        ;; permit redefinition where a single byte is replaced with "#&..."
                        (or (not (eql (first (children instance)) 38))
                            (not (eql (second (children instance)) 35))
                            (not (= (length (children old-def)) 1)))|#
                        )
                   (xml-error |WFC: Predefined Entity|
                              :name name :type (type-of instance))
                   nil)
                  (t t)))))

(defWFC "Proper Declaration/PE Nesting" )

(defVC "Proper Group/PE Nesting" )

(defWFC "Replacement Text Integrity" (wellformedness-error)
  ((name :initform nil :initarg :name)
   (text :initform nil :initarg :text))
  (:report (lambda (condition stream)
             (with-slots (name type text) condition
               (call-next-method)
               (print-xml-error-message
                condition stream "incomplete entity reference(s) in replacement text: ~a: ~s."
                name text)))))

(defDMC "QName Required" ()
  ((node :initform nil :initarg :node))
  (:report (lambda (condition stream)
             (with-slots (node) condition
               (call-next-method)
               (typecase node
                 (named-node
                  (print-xml-error-message
                   condition stream
                   "q-name required for node: ~s." node))
                 (t
                  (print-xml-error-message
                   condition stream
                   "cannot generate q-name: ~a." node))))))
  (:check (lambda (constraint (instance unamed-node))
            (with-slots (name) instance
              (unless (or (typep name 'name) (typep name 'string))
                (xml-error |DMC: QName Required| :node instance))))))

(defMethod initialize-instance :after
           ((instance unamed-node) &key)
  (check-constraint |DMC: QName Required| instance))

(defVC "Required Attribute" ()
  ((names :initarg :names :initform nil))
  (:report (lambda (condition stream)
             (with-slots (names) condition
               (call-next-method)
               (print-xml-error-message
                condition stream "~r attribute~:P missing: ~s."
                (length names) names)))))

(defVC "Root Element Type" ()
  ((root-name :initarg :root-name)
   (document-name :initarg :document-name))
  (:report (lambda (condition stream)
             (with-slots (root-name document-name) condition
               (call-next-method)
               (print-xml-error-message
                condition stream "document and root element names do not match: ~s: ~s."
                document-name root-name)))))

(defVC "Standalone Document Declaration" () ()
       (:documentation "this is here, but senseless to enforce."))

(defWFCProduction "SystemLiteral" ()
  ((datum :initarg :datum :initform nil))
  (:report (lambda (condition stream)
             (with-slots (datum) condition
               (call-next-method)
               (print-xml-error-message
                condition stream "no fragment identifier permitted: ~s." datum)))))

(defWFC "Unique Att Spec" (wellformedness-cerror)
  ((attributes :initform nil :initarg :attributes)
   (continue-format-string :initform "permit duplication." :allocation :class))
  (:report (lambda (condition stream)
             (with-slots (attributes) condition
               (call-next-method)
               (print-xml-error-message
                condition stream "duplicate attribute~p precluded: ~{~s~^ ~}."
                (length attributes) (mapcar #'name attributes)))))
  (:check (lambda (condition (node elem-node) &aux duplicates)
            (with-slots (attributes namespaces) node
              (flet ((check-node (node list &aux (name (name node)))
                       (map nil #'(lambda (test)
                                    (when (and (not (eq test node))
                                               (uname-equal (name test) name))
                                      (push test duplicates)))
                            list)))
                (mapl #'(lambda (nodes) (check-node (first nodes) (rest nodes)))
                      attributes)
                (mapl #'(lambda (nodes) (check-node (first nodes) (rest nodes)))
                     namespaces))
              (when duplicates
                (xml-error |WFC: Unique Att Spec| :attributes duplicates))))))

#|
20010907.jaa removed as the parser sets the properties rather than providing them for
initialization.

(defMethod initialize-instance :after ((instance elem-node) &key)
  (inspect instance)
  (break)
  (check-constraint |WFC: Unique Att Spec| instance))
|#

(defVC "Unique Element Type Declaration" ()
  ((name :initarg :name :initform nil))
  (:report (lambda (condition stream)
             (with-slots (name) condition
               (call-next-method)
               (print-xml-error-message
                condition stream
                "No element type may be declared more than once: ~s."
                name)))))
       

(defDMC "Unique Root Element Required" ()
  ()
  (:check (lambda (constraint (instance doc-node))
            (with-slots (children root) instance
              (unless (and root
                           (find root children)
                           (= (count-if #'is-abstract-elem-node children) 1))
                (xml-error |DMC: Unique Root Element Required|
                           :format-string "root node required: ~s."
                           :format-arguments (list instance)))))))

(defMethod (setf children) :after
           ((new-value t) (doc doc-node))
  (check-constraint |DMC: Unique Root Element Required| doc))

(defWFC "Version Match" ()
  ((name :initarg :name :initform nil))
  (:report (lambda (condition stream)
             (with-slots (name) condition
               (call-next-method)
               (print-xml-error-message
                condition stream
                "XML version not supported: ~s."
                name)))))

#|
 don't enforce the constraint during instantiation, that would prevent
 instantiating provisional documents
(defMethod initialize-instance :after
           ((instance doc-node) &key)
  (check-constraint |DMC: Unique Root Element Required| instance))
|#

:EOF