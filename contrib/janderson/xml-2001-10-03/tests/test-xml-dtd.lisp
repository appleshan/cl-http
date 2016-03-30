;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: xml-parser; -*-

(in-package "XML-PARSER")

#|
 <DOCUMENTATION>
  <DESCRIPTION>
   examples for operations on a complete DTD.
   they include several in-line examples, invocations to parse several included dtd files
   and an invocation for the somewhat larger dtd from docbook.
   </DESCRIPTION>
  </DOCUMENTATION>
|#


;; 20010914 0.913
;;
;; simple test of qualified name resolution

(defParameter *a-dom* nil)
(defParameter *doc-with-definition-string* nil)

(setq *doc-with-definition-string*
"<?xml version=\"1.0\"?>
<!DOCTYPE doc [
 <!ENTITY % one 'CDATA'>
 <!ENTITY % two 'pre:A'>
 <!ENTITY % three '<!ELEMENT test ANY >' >
 <!ELEMENT doc (pre:A*)>
 <!ELEMENT pre:A EMPTY>
 %three;
 <!ATTLIST pre:A
           not        NOTATION  (linespecific) 'linespecific'
           xmlns:pre  %one;     'pre space'
           att1       NMTOKENS  'pre:asdf'
           att2       CDATA     #REQUIRED
           att3       IDREFS    #FIXED 'id1 id2'
           att4       CDATA     #FIXED 'value att4'
           att5       CDATA     'value att5' >
 ]> <doc> <pre:A att5='abc / def' />
 </doc>
")

(let ((xutils::|REC-xml-19980210.PEs in Internal Subset| nil))
  (setq *a-dom* (document-parser *doc-with-definition-string* :reduce t :trace nil)))
(inspect *a-dom*)

(write-node *a-dom* *trace-output*)

(setq *doc-with-definition-string*
"<?xml version='1.0' standalone='no' ?>
  <!DOCTYPE LISP SYSTEM 'lisp.dtd'>
  <LISP xmlns='-//mecomnet.de//DTD Lisp (sort of)//en'>
   <DEFPARAMETER> <NAME>testing</NAME><!-- a comment -->
    <VALUE>one two three</VALUE> </DEFPARAMETER>
   </LISP>")

(let ((*xml-base* #p"xml:tests;xml;"))
  (setq *a-dom* (document-parser *doc-with-definition-string* :reduce t :trace nil)))


(parse-external-subset-toplevel #p"xml:tests;xml;lisp.dtd")
(parse-external-subset-toplevel #p"xml:tests;xml;shakespeare.dtd")
(parse-external-subset-toplevel #p"xml:tests;xml;old-testament.dtd")
(parse-external-subset-toplevel #p"xml:tests;xml;aramaic.dtd")

;; a test for conditional sections

(defParameter *extsubset*
"<!ENTITY % ISOamsa.module 'INCLUDE'>  <!-- 'IGNORE' -->
 <![%ISOamsa.module;[
 <!ENTITY % ISOamsa PUBLIC
 'ISO 8879:1986//ENTITIES Added Math Symbols: Arrow Relations//EN'
 'data:,<!ENTITY % dummy \"\">'>
 %ISOamsa;
 <!--end of ISOamsa.module-->]]>")

(parse-external-subset-toplevel *extsubset* )


;; the docbook dtd itself.

(time
(inspect
(multiple-value-list
 (parse-external-subset-toplevel
  #p"www:dokumente:standards:xml:Altheim:docbook:0.7:db3xml07.dtd"
  :bind-definitions t))))

(time
(inspect
(parse-external-subset-toplevel
 #p"www:dokumente:standards:xml:Altheim:docbook:0.7:db3xml07.dtd"
 :intern-names nil)))

#|
result of docbook:

(INSPECT (PARSE-EXTERNAL-SUBSET-TOPLEVEL #P"www:dokumente:standards:xml:Altheim:docbook:0.7:db3xml07.dtd")) took 255,854 milliseconds (255.854 seconds) to run.
Of that, 7,731 milliseconds (7.731 seconds) were spent in The Cooperative Multitasking Experience.
34,688 milliseconds (34.688 seconds) was spent in GC.
 1,479,510,080 bytes of memory allocated.
#<INSPECTOR-WINDOW "(|ExtSubsetDeclSequence| (#<COMMENT-NODE #x7É" #x7D3BBD6>
? (gc)
NIL
? (room)
There are at least 50,688,472 bytes of available RAM.

                  Total Size             Free                 Used
Mac Heap:      3423096 (3342K)      1111600 (1085K)      2311496 (2258K)
Lisp Heap:    61883768 (60433K)     49576872 (48415K)     12306896 (12018K)
Stacks:         215808 (211K)        210816 (206K)          4992 (5K)
Static:         283040 (276K)             0 (0K)          283040 (276K)

can't really be read separately as it depends on entities from the main
(document-parser #p"www:dokumente:standards:xml:Altheim:docbook:0.7:dbpoolx.mod"
                   :start-name '|ExtSubset|))
|#

;; rlf example

(setq *doc-with-definition-string*
"<?xml version='1.0'?>
<!DOCTYPE tar [
  <!ELEMENT tar EMPTY>
  <!ATTLIST tar
       attributeFormDefault  (qualified | unqualified) 'unqualified'
       blockDefault CDATA ''
       lang CDATA #IMPLIED
       xml:lang CDATA #IMPLIED
       finalDefault CDATA ''
       id ID #IMPLIED
       ID IDREF #IMPLIED
       version CDATA #IMPLIED
       xml:version CDATA #REQUIRED
       xmlns:fred CDATA #FIXED 'http://www.whatever/fred'
       xmlns CDATA #FIXED 'http://www.whatever/xx'>
 ]> <tar/>
")
(setq *a-dom* (document-parser *doc-with-definition-string*))
(inspect *a-dom*)
(write-node *a-dom* *trace-output*)

:EOF
