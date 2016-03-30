;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: xml-parser; -*-

;; tests for qname interpretation in context of xml-parsing

(in-package "XML-PARSER")

#|
<DOCUMENTATION>
 <DESCRIPTION>
  this demonstrates the effect of conflicting prefix bindings.
  the homographic relation among the "a:x" names within
  the same declaration path is distinguished in the second example based ont he
  presence of an explicit declaration in the scope of the doc element declaration,
  which causes the document to be invalid.
  </DESCRIPTION>
 </DOCUMENTATION>
|#

;; 20010914 0.913

(write-node
 (document-parser
  "<!DOCTYPE doc [
    <!ELEMENT doc (a:x)* >
    <!ATTLIST doc xmlns CDATA 'data:,ns-top'>
    <!ELEMENT a:x EMPTY>
    <!ATTLIST a:x xmlns:a CDATA 'data:,ns-a'>
    ]>

   <doc xmlns='data:,ns-top'
        xmlns:a='data:,ns-a'>
     <x xmlns='data:,ns-a'></x>
     </doc>"
  :validate t)
 *trace-output*)

;; this will signal a (continuable) validation error
(write-node
 (document-parser
  "<!DOCTYPE doc [
    <!ELEMENT doc (a:x)* >
    <!ATTLIST doc xmlns CDATA 'data:,ns-top'
                  xmlns:a CDATA 'data:,ns-b'>
    <!ELEMENT a:x EMPTY>
    <!ATTLIST a:x xmlns:a CDATA 'data:,ns-a'>
    ]>

   <doc xmlns='data:,ns-top'
        xmlns:a='data:,ns-a'>
     <x xmlns='data:,ns-a'></x>
     </doc>"
  :validate t)
 *trace-output*)


:EOF
