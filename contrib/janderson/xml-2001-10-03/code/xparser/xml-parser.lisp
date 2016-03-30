;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: xml-parser; -*-

#|
<DOCUMENTATION>
 <DESCRIPTION>
  bnf-driven parser for XML.
  </DESCRIPTION>
 <COPYRIGHT YEAR='2001' AUTHOR='james adam anderson' MARK='(C)'
            href='file://xml/sysdcl.lisp' />
 <CHRONOLOGY>
  <DELTA DATE='20010618'>xutils:*parsetable*</DELTA>
  <DELTA DATE='20010621'>
   corrected *XML-BASE* bindings to follow 4.2.2 - that is to always rebind.
   </DELTA>
  <DELTA DATE='20010627'>eliminated <code>WITH-DOCUMENT-ENVIRONMENT</code>,
   as it was used once only.</DELTA>
  <DELTA DATE='20010629'>augmented parse context with a qname extent id
   </DELTA>
  <DELTA DATE='20010704'><code>*VERBOSE-QNAME*</code> keyword for
   <code>DOCUMENT-PARSER</code></DELTA>
  <DELTA DATE='20010716'>name/symbol distinction;
   *construction-context* bound for |Element| parse function</DELTA>
  <DELTA DATE='20010902'>manage *in-name-token* to support name tokenizer</DELTA>
  <DELTA DATE='20010906'>trim string argument to <code>document-parser</code></DELTA>
  <DELTA DATE='20010913'>extended <code>parse-external-subset-toplevel</code> to include
   qname-to-uname interning</DELTA>
  <DELTA DATE='20010917'>added :compile-toplevel to eval-when for classes</DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
|#

(in-package "XML-PARSER")

;; terminal primitives apply for many of the non-terminal productions in the
;; original bnf. in cases where the tokenizer implements the distinction,
;; the nonterminal is replaced by a category predicate, the various data tokens
;; are simply strings, and the tokenizer ensures the correct content.

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun |IS-AttlistDeclOp| (x) (and (stringp x) (string= x "ATTLIST")))
(defun |IS-AttCharData| (x) (stringp x))
(defun |IS-DefaultAttCharData| (x) (stringp x))
(defun |IS-CDataCharData| (x) (stringp x))
(defun |IS-CharData| (x) (stringp x))
(defun |IS-CommentCharData| (x) (stringp x))
(defun |IS-EncNameCharData| (x)
  (and (stringp x) (plusp (length x))
       (xml-latinalphachar? (schar x 0)) 
       (not (find-if (complement #'xml-latinchar?) x))))
(defun |IS-EntityDeclOp| (x) (string= x "ENTITY"))
(defun |IS-EntityCharData| (x) (stringp x))
(defun |IS-EntityData| (x) (listp x))
(defun |IS-HexNumber| (x) (and (stringp x)
                               (not (find-if #'(lambda (c)
                                                 (not (digit-char-p (code-char c) 16)))
                                             x))))
(defun |IS-IgnoreCData| (x)
  (stringp x))
(defun |IS-NameCharData| (x) (is-namechardata x))
(defun |IS-NCName| (x) (is-ncname x))
(defun |IS-Number| (x) (numberp x))
(defun |IS-ParsedExtSubset| (x) (is-ext-subset-node x))
(defun |IS-ParsedReference| (x) (is-ref-general-entity x))
(defun |IS-PiTarget| (x)
  ;; (when (symbolp x) (setf x (string x)))
  (and (stringp x)
       (unless (and (= (length x) 3) (char-equal (schar x 0) #\X)
                    (char-equal (schar x 1) #\M) (char-equal (schar x 2) #\L))
         (|IS-NCName| x))))
(defun |IS-PiCharData| (x) (stringp x))
(defun |IS-PubidCharData| (x)
  (and (stringp x) (not (find-if (complement #'xml-pubidchar?) x))))
#|
(defun |IS-QNameCharData| (x &aux colon? last)
  (declare (type simple-string x) (type character last)
           (optimize (speed 3) (safety 0)))
  (when (stringp x)
    (case (length x)
      (0 nil)
      (1 (xml-initial-namechar? (schar x 0)))
      (t (and (xml-initial-namechar? (schar x 0))
              (not (find-if (complement #'(lambda (c)
                                            (setf last c)
                                            (or (xml-namechar? c)
                                                (when (char= c #\:)
                                                  (setf colon? (not colon?))))))
                            x :start 1))
              (not (char= #\: last)))))))
|#

(defun |IS-QNameCharData| (x &aux colon? length)
  (declare (type simple-string x) (type character last)
           (type fixnum index length)
           (optimize (speed 3) (safety 0)))
  (when (stringp x)
    (unless (zerop (setf length (length x)))
      (and (xml-initial-namechar? (schar x 0))
           (/= (the fixnum (char-code (schar x (decf length))))
               #.(char-code #\:))
           (loop (when (<= length 0) (return t))
                 (unless (or (xml-namechar? (schar x length))
                             (when (= (the fixnum (char-code (schar x length)))
                                      #.(char-code #\:))
                               (setf colon? (not colon?))))
                   (return nil))
                 (decf length))))))

;(time (dotimes (x 10000) (|IS-QNameCharData| "asdf@a")))

(defun |IS-S| (x)
  ;; must allow for string tokens - that is those which have been reduced
  ;; during reading
  (or (eq x '|xml|:| |)
      (and (stringp x)
           (not (find-if (complement #'xml-space?) x)))))
(defun |IS-SystemCharData| (x) (stringp x))
(defun |IS-VersionNumCharData| (x)
  (and (stringp x) (plusp (length x))
       (not (find-if (complement #'xml-versionnumchar?) x))))

;; category tests are used to effect string tests for literals in order to avoid
;; having the tokenizer check every string for interning. 

(defMacro defLiteralPredicate (name &aux (function 'string=) pattern)
  (if (consp name)
    (setf pattern (second name) name (first name))
    (setf pattern name))
  `(defun ,(intern (concatenate 'string "IS-"
                                (remove-if (complement #'alpha-char-p) name)
                                "Token"))
          (value)
     ,(format nil "? = '~a'" name)
     (declare (type string value) (optimize (speed 3) (safety 0)))
     (,function value ,pattern)))

(defMacro defLiteralPredicates (parameter-name &rest names)
  `(progn ,@(mapcar #'(lambda (name) `(defLiteralPredicate ,name)) names)
          (defParameter ,parameter-name ',names)))

(defLiteralPredicates  *literal-strings*
  "ANY" "CDATA" "encoding" "EMPTY" "ENTITY" "ENTITIES"
  "ID" "IDREF" "IDREFS" "INCLUDE" "IGNORE" "NDATA" "NMTOKEN" "NMTOKENS"
  "no"  "NOTATION" "PUBLIC" "standalone" "SYSTEM"
  "version" "xml" "yes" 
  "#FIXED" "#IMPLIED" "#PCDATA" "#REQUIRED")

#| this is the original list of disjoint tokens. it is no onger pertinent
   as the grammar is represented with many tokens merged with markup literals.
   for example '<!' ELEMENT, which would require a predicate for an "ELEMENT" token
   is now specified as '<!ELEMENT' for which the respective reader produces a static
   symbol. only those predicates are generated above which are required by the bnf...
(defLiteralPredicates  *literal-strings*
  "ANY" "ATTLIST" "CDATA" "DOCTYPE" "ELEMENT" "EMPTY" "encoding"
  "ENTITY" "ENTITIES"
  "ID" "IDREF" "IDREFS" "INCLUDE" "IGNORE" "NDATA" "NMTOKEN" "NMTOKENS"
  "no"  "NOTATION" "PUBLIC" "standalone" "SYSTEM"
  "version" "xml" "yes" 
  "#FIXED" "#IMPLIED" "#PCDATA" "#REQUIRED")
|#
) ;; eval-when

;(time (dotimes (x 10000) (|IS-REQUIREDToken| "XXXXXXXXXXXXX")))

(defParameter *xml-nonterminals*
  '(|Document| |Prologue| |Element| |Misc|
    |Names| |NCName| |Nmtoken| |Nmtokens|
    |EntityValue| |EntityElement| |EntityLiteral|
    |AttValue| |AttValueElement| |AttValueLiteral|
    |Comment| |CommentLiteral|
    |Pi| |PiTarget| |PiLiteral|
    |CDSect| |CData|
    |XMLDecl| |DoctypeDecl| |XMLPITarget| |VersionInfo| |EncodingDecl| |SDDecl|
    |VersionNum|
    |DoctypeDecl| |MarkupDecl|
    |ElementDecl| |AttlistDecl| |EntityDecl| |NotationDecl|
    |ExtSubset| |TextDecl| |ExtSubsetDecl|
    )
  "the grammar symbols appear in an unreduced parse result tree.
   this list comprises those present in the normative grammar.
   the implemented grammar has additional non-terminals to permit
   context-dependent token-generation.")


(eval-when (:compile-toplevel :load-toplevel :execute )

(defClass xqdm-lexical-unit () ())

(defClass xqdm-atn (xqdm-lexical-unit atn)
  ((ignored-terms :initform (append '(|S| |Eq| |PCDATA| |MixedCardinality|)
                                    (set-difference *literal-strings*
                                                    '("EMPTY" "ANY")
                                                    :test #'string=))))
  (:documentation
   "an augmented atn class serves to extend the compiled bindings to include
    the namespace stack and to bind parse tables."))

(defClass xqdm-atn-node (xqdm-lexical-unit  atn-node) ()
  (:documentation
   "an augmented atn class serves to extend the compiled bindings to include
    the namespace stack and to bind parse tables."))

(defClass xqdm-edge () ())

(defClass xqdm-test-atn-edge (xqdm-edge xqdm-lexical-unit test-atn-edge) ()
  (:documentation
   "an augmented atn class serves to extend the compiled bindings to include
    the namespace stack and to bind parse tables."))
(defClass xqdm-pop-atn-edge (xqdm-edge pop-atn-edge) ()
  ;; pop performs no thest, thus can't take input
  (:documentation
   "an augmented atn class serves to extend the compiled bindings to include
    the namespace stack and to bind parse tables."))
(defClass xqdm-push-atn-edge (xqdm-edge push-atn-edge) ()
  ;; push is not a lexical unit, as the respective net handles that.
  (:documentation
   "an augmented atn class serves to extend the compiled bindings to include
    the namespace stack and to bind parse tables."))
(defClass xqdm-cat-atn-edge (xqdm-edge xqdm-lexical-unit cat-atn-edge) ()
  (:documentation
   "an augmented atn class serves to extend the compiled bindings to include
    the namespace stack and to bind parse tables."))


(defMethod atn-term-names ((node xqdm-atn))
  (with-slots (ignored-terms) node
    (remove-if #'(lambda (term) (member term ignored-terms :test #'string=))
               (call-next-method))))

(defMethod atn-term-cardinality ((node xqdm-atn) name)
  (with-slots (ignored-terms) node
    (unless (member name ignored-terms :test #'string=)
      (call-next-method))))


(defGeneric lexical-context (node)
  (:method ((unit atn)) (bnfp::atn-name unit))
  (:method ((unit atn-node)) (bnfp::atn-name unit))
  (:method ((unit cat-atn-edge)) (bnfp::category-name (bnfp::atn-cat unit)))
  (:method ((unit test-atn-edge)) (bnfp::atn-test unit))
  (:method ((unit pop-atn-edge)) (bnfp::atn-register unit))
  (:method ((unit word-atn-edge)) (bnfp::atn-word unit)))


(defMethod bnfp:atn-bindings ((node xqdm-atn)
                              &aux (name (lexical-context node)))
  (append (when (eq name '|Element|)
            '((*namespace-bindings* *namespace-bindings*)
              (*construction-context* *construction-context*)))
          (when (eq name '|Root|)
            ;; bind these for resolving qualified names - if there is a root definition, use its
            ;; namespaces. if not, make sure the null namespace is bound
            `((*namespace-bindings* (make-document-namespace-bindings))))
          (when (member name '(|DoctypeDecl| |ExtSubset|))
            '((*in-dtd* t)))
          (when (eq name '|DeclSep|)
            '((*in-decl-sep* t)))
          (when (eq name '|IntSubsetDecl|)
            '((*in-internal-subset* t)))
          (when (member name '(|EntityValue| |ExtEntityData|))
            '((*in-entity-value* t)))
          (when (member name '(|NamedConditionalSect| |IgnoreSectContents|))
                ;; if in a conditional section where the INCLUDE/IGNORE was not
                ;; resolved, then don't bind any definitions and don't try to
                ;; expand any entities.
                '((bnfp::*atn-reduce* nil)))
          (call-next-method)))

(defMethod bnfp:atn-bindings ((node xqdm-lexical-unit)
                             &aux 
                             (name (lexical-context node))
                             (parsetable (parsetable name)))
  (append (when parsetable
            `((*parsetable*
               ,(intern (concatenate 'string (string name) "-Parsetable")))))
          (when (eq name '|QName|)
            '((*in-name-token* t)))
          (call-next-method)))


(defMethod bnfp:atn-initial-actions ((node xqdm-lexical-unit)
                                      &aux 
                                      (name (lexical-context node))
                                      (parsetable (parsetable name)))
  (append (when (and parsetable
                         (member 'quote-context (table.properties parsetable)))
                  `((setf *quote-token* (or *quote-token* t))))
          (call-next-method)))

(defMethod bnfp:atn-constructor-specializer ((node xqdm-edge))
  (let ((context (lexical-context node)))
    (when (member context '(|AttCharData|
                            |Attribute|
                            |CharData|
                            |CDataCharData|
                            |Comment|
                            |Content|
                            |ContentSequence|
                            |DefaultAttCharData|
                            |Document|
                            |Element|
                            |ExtParsedEnt|
                            |Pi|
                            |PiCharData|
                            |STag|)
                  :test #'equalp)
      '*construction-context*)))

#|
(defMethod bnfp:atn-continue-actions ((node xqdm-lexical-unit)
                                      &aux 
                                      (name (lexical-context node)))
  (append (unless (or (find #\/ (string name)) (find #\. (string name)))
            '((setf *xml-lexical-context* saved-*xml-lexical-context*)))
          (call-next-method)))
|#

) ;; end of compiler additions 
;;
;;
;;


(defConstant *token-cache-size* 32)
(defParameter *token-cache-mask* (1- *token-cache-size*))
(defun make-token-cache () (make-array *token-cache-size* :initial-element nil))
(defParameter *input-token-cache* (make-token-cache))
(defParameter *input-index* -1)
(defParameter *input-last* nil)

(defun input-length () (1+ *input-index*))

(defClass xml-input (atn-input) ())

(defMethod parse-state ((input xml-input) &aux (datum (get-input)))
  (list* :inputs (cons (list :source *input-source*
                              :position (stream-position *input-source*)
                              :column *input-column*
                              :line *input-line*)
                        (input-stack))
          :input (typecase datum
                   (fixnum (code-char datum))
                   (t datum))
          :token (peek-token-string)
          :context (when *parsetable* (table.name *parsetable*))
          (call-next-method)))

(defMethod input.item ((input xml-input) position)
  (input-reference position))

(defMethod input.peek-item ((input xml-input) position &aux token)
  (setf token (when (<= position *input-index*)
                (svref *input-token-cache* (logand position *token-cache-mask*))))
  (when *atn-trace*
    (bnfp::%atn-format " [peek (~@[~a~]/~a) @(~s/~s) == ~s]"
                       (when (boundp '*atn-stack) (first *atn-stack))
                       (when *parsetable* (table.name *parsetable*))
                       position *input-index* token))
  token)



#|
;; an alternative with tracing code
(defun input-reference (position &aux token)
  (declare (ignore input)
           (type fixnum position *input-index*)
           (type simple-vector *input-token-cache*)
           (optimize (speed 3) (safety 0)))
  (when *atn-trace*
    (bnfp::%atn-format " [input (~@[~a~]/~a) @(~s/~s)"
                       (when (boundp '*atn-stack) (first *atn-stack))
                       (when *parsetable* (table.name *parsetable*))
                       position *input-index*))
  (cond ((= position *input-index*) *input-last*)
        ((and (= position (1+ *input-index*)) (setf token (token-reader)))
         (incf *input-index*)
         (setf *input-last*
               (setf (svref *input-token-cache* (logand *input-index* *token-cache-mask*)) token))
         (when *atn-trace*
           (format *trace-output* " ->new/@(~s) ~s]"
                   position token))
         token)
        ((< position *input-index*)
         (if (> (- *input-index* position) *token-cache-size*)
           (xml-error "token position wrapped: ~s/~s."
                      position *input-index*))
         (setf token (svref *input-token-cache* (logand position *token-cache-mask*)))
         (when *atn-trace*
           (format *trace-output* " -> ~s]" token))
         token)
        ((> position *input-index*)
         (when *atn-trace* (format *trace-output* "*EOF*"))
         nil)))
|#

(defun input-reference (position &aux token)
  (declare (ignore input)
           (type fixnum position *input-index*)
           (type simple-vector *input-token-cache*)
           (optimize (speed 3) (safety 0)))
  (cond ((= position *input-index*) *input-last*)
        ((and (= position (1+ *input-index*)) (setf token (token-reader)))
         (incf *input-index*)
         (setf *input-last*
               (setf (svref *input-token-cache* (logand *input-index* *token-cache-mask*)) token))
         token)
        ((< position *input-index*)
         (if (> (- *input-index* position) *token-cache-size*)
           (xml-error "token position wrapped: ~s/~s."
                      position *input-index*))
         (svref *input-token-cache* (logand position *token-cache-mask*)))
        ((> position *input-index*)
         nil)))

(defun input-eof? (position)
  ; (break)
  (and (> position *input-index*)
       *input-eof*))

;;
;;
;;

#|
at one point mcll4.2 was causing type 2 errors at this point.
after a system restart they "disappeared". !!
(eval-when (:compile-toplevel :load-toplevel :execute)
  (break))
|#

(eval-when (:compile-toplevel)
  (let* ((*class.atn* 'xqdm-atn)
         (*class.atn-node* 'xqdm-atn-node)
         (*class.cat-atn-edge* 'xqdm-cat-atn-edge)
         (*class.pop-atn-edge* 'xqdm-pop-atn-edge)
         (*class.push-atn-edge* 'xqdm-push-atn-edge)
         (*class.test-atn-edge* 'xqdm-test-atn-edge))
    (bnfp:compile-atn-system #p"xml:xml-grammar.bnf"
                             :compile t
                             ;; don't load directly - leave that to the app
                             ;; :load t
                             :package *token-package*
                             :fasl-pathname "xml:code;atn-lib;xml-grammar.bin"
                             :source-pathname "xml:code;atn-lib;xml-grammar.lisp"
                             :input-function 'input-reference
                             :input-eof-function 'input-eof?
                             :trace nil))
  )


(eval-when  (:load-toplevel)
  (cl-user::conditional-load-file "xml:code;atn-lib;xml-grammar"))

(eval-when (:execute)
  (let ((*class.atn* 'xqdm-atn)
        (*class.atn-node* 'xqdm-atn-node)
        (*class.cat-atn-edge* 'xqdm-cat-atn-edge)
        (*class.pop-atn-edge* 'xqdm-pop-atn-edge)
        (*class.push-atn-edge* 'xqdm-push-atn-edge)
        (*class.test-atn-edge* 'xqdm-test-atn-edge))
    (bnfp:compile-atn-system #p"xml:xml-grammar.bnf"
                             :execute t :compile nil
                             :package *token-package*
                             :source-pathname "xml:code;atn-lib;xml-grammar.lisp"
                             :input-function 'input-reference
                             :input-eof-function 'input-eof?
                             :trace nil))
  )


;;
;;           
;;

(defMacro with-parse-environment
          ((stream &key encoding) &rest body)
  `(let* ((*input-source-stack*
           (if *input-source-stack* *input-source-stack*
               (make-array 4 :adjustable t :fill-pointer 0)))
          (*input-column* 1)
          (*input-line* 1)
          (*element-count* 0)
          (*input-token-cache* (make-token-cache))
          (*input-index* -1)
          (*input-last* nil)
          (*token-fill* (make-list 32))
          (*token-start* *token-fill*)
          (*token-end* nil)
          (*token-length* 0)
          (*input-datum* nil)
          (*input-eof* nil)
          (*reinput-datum* nil)
          (*input-reader* nil)
          (*input-reader-arg* nil)
          (*name-fill* (make-list 32))
          (*name-start* *name-fill*)
          (*name-length* 0)
          ;; always make a new tokenizer per document to reduce contention
          ;; also the global one serves as the static default.
          (*name-tokenizer* (make-xml-tokenizer))
          (*input-encoding* ,encoding)
          (*root-def-type* nil)
          (*quote-token* nil)
          (*namespace-dictionary* *namespace-dictionary*)
          (*namespace-mode* :copy)
          (*in-entity-value* nil)
          (*in-name-token* nil)
          (*input-source* ,stream)
          (*qname-extent* (new-qname-extent))
          (*def-type-id-qname-contexts* (or *def-type-id-qname-contexts*
                                            (make-hash-table :test #'equalp)))
          (*def-type-model-qname-contexts* (or *def-type-model-qname-contexts*
                                               (make-hash-table :test #'equalp)))
          (*def-attr-qname-contexts* (or *def-attr-qname-contexts*
                                         (make-hash-table :test #'equalp)))
          (bnfp::*atn-input (make-instance 'xml-input)))
     (multiple-value-bind (function arg detected-encoding to-reread)
                          (decoding-stream-reader ,stream *input-encoding*)
       (unless (eq detected-encoding *input-encoding*)
         (when (and *input-encoding*
                    (not (search (string detected-encoding) (string *input-encoding*)
                                 :test #'char-equal)))
           (warn "detected encoding differs from that specified: ~s: ~s."
                 detected-encoding *input-encoding*))
         (setf *input-encoding* detected-encoding))
       (setf *input-reader* function
             #|#'(lambda (arg &aux byte)
                                (setf byte (funcall function arg))
                                (when (eql byte #.(char-code #\')) (Break))
                                byte)|#
             *input-reader-arg* arg)
       (when to-reread
         ;; wrap the sequence in a stream in case there is an error
         (push-input-source (make-instance 'vector-input-stream
                              :vector to-reread))))
     (advance-input)
     ,@body))


;;
;;
;;

(defMethod document-parser ((input string) &rest args)
  (when (and (plusp (length input)) (xml-space? (char input 0)))
    (setf input (string-trim #(#\return #\linefeed #\tab #\space) input)))
  (cond ((and (plusp (length input)) (char= (char input 0) #\<))
         (let ((stream (make-instance 'vector-input-stream :vector input)))
           (apply #'document-parser stream args)))
        (t
         (apply #'document-parser (make-uri input *xml-base*) args))))

(defMethod document-parser ((input vector) &rest args)
  (let ((stream (make-instance 'vector-input-stream :vector input)))
    (apply #'document-parser stream args)))

#| 20010621 according to the passage 4.2.2,
"relative URIs are relative to the location of the resource within which the
entity declaration occurs." the base uri must _always_ be rebound.

(defMethod document-parser ((source file-url) &rest args)
  (let ((*xml-base* (if *xml-base* *xml-base* source)))
    (with-file-stream (stream source)
      (apply #'document-parser stream args))))

(defMethod document-parser ((source http-url) &rest args)
  (let ((*xml-base* (if *xml-base* *xml-base* source)))
    (with-http-stream (stream source)
      (apply #'document-parser stream args))))

(defMethod document-parser ((source pathname) &rest args)
  (let ((*xml-base* (if *xml-base* *xml-base* (pathname-file-url source))))
    (with-open-file (stream source :direction :input :element-type 'unsigned-byte)
      (apply #'document-parser stream args))))
|#

(defMethod document-parser ((*xml-base* file-url) &rest args)
  (with-file-stream (stream *xml-base*)
    (apply #'document-parser stream args)))

(defMethod document-parser ((*xml-base* http-url) &rest args)
  (with-http-stream (stream *xml-base*)
    (apply #'document-parser stream args)))

(defMethod document-parser ((*xml-base* data-url) &rest args)
  (with-data-stream (stream *xml-base*)
    (apply #'document-parser stream args)))

(defMethod document-parser ((source pathname) &rest args)
  (let ((*xml-base* (pathname-file-url source)))
    (with-open-file (stream source :direction :input
                            :element-type 'unsigned-byte)
      (apply #'document-parser stream args))))

(defMethod document-parser ((stream stream) &rest args
                            &key
                            ((:ignore-whitespace *ignore-whitespace*) *ignore-whitespace*)
                            ((:verbose-qnames *verbose-qnames*) *verbose-qnames*)
                            ((:verbose *xml-verbose*) *xml-verbose*)
                            encoding
                            &allow-other-keys
                            &aux
                            ;; isolate the declarations for each physical entity branch
                            ;; higher-level definitions contribute to lower ones, but
                            ;; separate branches are isolated.
                            (*entity-declarations* *entity-declarations*))
  ;; this had to be nested in order to permit the reference to args
  ;; note that the document container is not generated dependant on the construction context.
  ;; the present implementation expects to be able to expand entities and uses the document to
  ;; bind the definitions. the document itself can be specialized via its class parameter.
  (destructuring-bind (&key
                       ;; if this is the outermost invocation, instantiate a context document
                       ((:document *document*) (or *document* (apply #'make-parse-document args)))
                       ((:construction-context *construction-context*)
                        (construct-construction-context
                         *construction-context* *document*))
                       &allow-other-keys)
                      args
    ;; check settings for name handling
    (with-parse-environment (stream :encoding encoding)
      ;; bind the document parsetable, as a default to ensuse a minimal lexical context
      (with-parsetable |Document|
        (multiple-value-bind (result index complete)
                             (apply #'|Document-Parser|
                                    bnfp::*atn-input
                                    args)
          ;; (break)
          (if complete
            (values result index)
            (xml-error 'incomplete-parse :result result)))))))

;;
;;
;;

(defMethod parse-external-source ((source pathname) production)
  (document-parser source :start-name production))

(defMethod parse-external-source ((system-literal string) production)
  (parse-external-source (make-uri system-literal *xml-base*) production))

(defMethod parse-external-source ((source file-url) production)
  (document-parser source :start-name production))

(defMethod parse-external-source ((source http-url) production)
  (document-parser source :start-name production))

(defMethod parse-external-source ((source data-url) production)
  (document-parser source :start-name production))

(defMethod parse-external-source ((stream stream) production)
  (document-parser stream :start-name production))

(defMethod parse-external-source ((source def-external-entity) production)
  (declare (special *entity-stack))
  (let ((*entity-stack (when (boundp '*entity-stack) *entity-stack)))
    (declare (special *entity-stack) (dynamic-extent *entity-stack))
    (cond ((find source *entity-stack)
           (xml-error-|WFC: No Recursion| :entity (name source)))
          (t
           (setf *entity-stack (cons source *entity-stack))
           (parse-external-source (uri source) production)))))


(defun parse-external-subset (def-entity)
  "parse an external source as an external document type definition subset."
  (parse-external-source def-entity '|ExtSubset|))

;; for convenience
(defun parse-external-subset-toplevel
       (source &rest args &key (start-name '|ExtSubset|)
               (intern-names t intern-names-p)
               (bind-definitions nil bind-definitions-p)
               &allow-other-keys)
  "define the cache context for qname resolution and (by default) intern names"
  (let ((*def-type-id-qname-contexts* (or *def-type-id-qname-contexts*
                                          (make-hash-table :test #'equalp)))
        (*def-type-model-qname-contexts* (or *def-type-model-qname-contexts*
                                             (make-hash-table :test #'equalp)))
        (*def-attr-qname-contexts* (or *def-attr-qname-contexts*
                                       (make-hash-table :test #'equalp)))
        (*document* (make-parse-document))
        (ext-subset nil))
    (when intern-names-p (remf args :intern-names))
    (when bind-definitions-p (remf args :bind-definitions))
    (setf ext-subset (apply #'document-parser source :start-name start-name args))
    (cond ((and intern-names bind-definitions)
           (assign-universal-names)
           (map-node-by-type #'is-def-elem-type
                      #'(lambda (def) (bind-definition def *document*))
                      (children ext-subset))
           (values ext-subset *document*))
          (intern-names
           (assign-universal-names)
           ext-subset)
          (t
           ext-subset))))


(defun parse-external-general-entity (def-entity &aux result)
  "parse an external source as an general entity."
  (setf result (parse-external-source def-entity '|ExtParsedEnt|)) 
  (typecase result
    ;; cache the actual encoding
    (external-parsed-entity (setf (encoding def-entity)
                                  (encoding result))))
  result)


;; load a parameter entity

(defGeneric read-external-entity-data (source)
  (:documentation
   "read and cache the data from an external entity in a byte array"))

(defMethod read-external-entity-data ((source pathname))
  (with-open-file (stream source :direction :input :element-type 'unsigned-byte)
    (read-external-entity-data stream)))

(defMethod read-external-entity-data ((system-literal string))
  (read-external-entity-data (make-uri system-literal *xml-base*)))

(defMethod read-external-entity-data ((source file-url))
  (with-file-stream (stream source)
    (read-external-entity-data stream)))

(defMethod read-external-entity-data ((source http-url))
  (with-http-stream (stream source)
    (read-external-entity-data stream)))

(defMethod read-external-entity-data ((source data-url))
  (with-data-stream (stream source)
    (read-external-entity-data stream)))

(defMethod read-external-entity-data ((source stream))
  (multiple-value-bind (reader arg) (decoding-stream-reader source *input-encoding*)
    (let ((buffer (make-array 32 :adjustable t :fill-pointer 0))
          (byte 0))
      ;; no padding, this is for use within an entity
      (loop (unless (setf byte (funcall reader arg)) (return))
            (vector-push-extend byte buffer))
      buffer)))

(defMethod read-external-entity-data ((source def-external-parameter-entity))
  (read-external-entity-data (uri source)))


;;
;;
;; nb. this works only if the context does not change - a rare situation...

(defun |Document-Tokenizer| (input-str)
  (let ((tokens (make-array 0 :fill-pointer 0 :adjustable t))
        (expr nil))
    (with-input-from-string (string-stream input-str)
      (with-parse-environment (string-stream)
        (loop (setf expr (token-reader))
              (unless expr (return tokens))
              (vector-push-extend expr tokens))))))

(defun document-tokenizer (x)
  (with-parsetable |Document|
    (|Document-Tokenizer| x)))


(defGeneric |!-reader| (stream char dispatch)
  (:documentation
   "this is the generic framework for xml-related reader macros.
   a direct reader macro function peeks at the next character and
   dispatches on that.")
  (:method ((stream t) (dispatch-char t) (sub-arg t))
           (error "no reader for form: ~s." dispatch-char)))

(defMethod |!-reader|
           ((stream stream) (char (eql #\<)) (sub t)
            &aux input parse)
  (setf input (|Document-Tokenizer| stream))
  (setf parse (caar (|Document-Parser| input)))
  (unless (is-doc-node (cdr parse))
    (error "document not parsed: ~s: ~s." input (cdr parse)))
  (cdr parse))

(defMethod |!-reader|
           ((stream stream) (char (eql #\{)) (sub t))
  (xqdm::|{-reader| stream char))

(set-dispatch-macro-character #\# #\!
                              #'(lambda (stream char sub)
                                  (declare (ignore char))
                                  (|!-reader| stream (read-char stream) sub)))



:EOF

