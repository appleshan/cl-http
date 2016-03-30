;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: xml-parser; -*-

(in-package "XML-PARSER")


(time (document-parser "<Envelope/>" :trace nil :reduce t))
(document-parser "data:,<Envelope/>")
(time (document-parser "<E att='qwer'><?pi?></E><?pi &#63;&#62;too?>" :trace nil :reduce t))
(inspect *)

;; an example from "electric xml":
;; the first version of the parser, working with strings, tracing, and unoptimized
;; managed at best 100 milliseconds on a 604/200 under mcl4.2 - two orders of magnitude
;; over the quoted java times.
;; the first released version, binary-based, optimised and untraced required 40 for the
;; first call and around 21 for successive calls.

(defParameter *document-string*
  "<SOAP-ENV:Envelope
     xmlns:SOAP-ENV='http://schemas.xmlsoap.org/soap/envelope/'
     xmlns:xsi='http://www.w3.org/1999/XMLSchema-instance'
     xmlns:xsd='http://www.w3.org/1999/XMLSchema'>
     <SOAP-ENV:Body>
        <ns1:getRate xmlns:ns1='urn:demo1:exchange' 
           SOAP-ENV:encodingStyle='http://schemas.xmlsoap.org/soap/encoding/'>
           <country1 xsi:type='xsd:string'>USA</country1>
           <country2 xsi:type='xsd:string'>japan</country2>
        </ns1:getRate>
     </SOAP-ENV:Body>
  </SOAP-ENV:Envelope>")

(defParameter *document-string*
  "<!DOCTYPE ns:doc [
     <!ELEMENT ns:doc (a, b)>
     <!ATTLIST ns:doc xmlns:ns CDATA 'a namespace' >
     <!ELEMENT a ANY>
     <!ATTLIST a ns:att1 CDATA ' a default' >
     <!ELEMENT b ANY>
     <!ELEMENT c ANY>
     ]>
   <ns:doc>
     <a xmlns:qwerty='a namespace' qwerty:att1='explicitly'><b><c/></b></a>
     </ns:doc>")

(time (dotimes (x 10) (document-parser "<?xml version='1.0' encoding='UTF-8' ?>
<library>
  <!-- the isbn could have been an element -->
  <book isbn='0-312-85172-3'>
    <title>Aristoi</title>
    <author>Walter Jon Williams</author>
  </book>
  <book isbn='0-812-51349-5'>
    <title>Enders Game</title>
    <author>Orson Scott Card</author>
 </book>
</library>")))

(let ((*specialize-elem-node* t))
  (time (document-parser *document-string* :trace nil :reduce t)))
(time (document-parser *document-string* :trace nil :reduce nil))
(list *document-start-time* *document-in-time* *document-end-time*)
(length *document-string*)
(inspect *)

(defParameter *a-dom* nil)


;; a file url permits a logical host
(setf *a-dom* (document-parser #P"xml:Tests;xml;channel.xml" :trace nil))
(setf *a-dom* (document-parser "file://xml/Tests/xml/channel.xml" :trace nil))

(setf *a-dom* (document-parser #P"xml:Tests;xml;email.xml" :trace nil))
(setf *a-dom* (document-parser #P"xml:Tests;xml;lisp.xml" :trace nil))
(setf *a-dom* (document-parser #P"xml:Tests;xml;real-mini-aleph-het.xml" :trace nil))
;; http retrieval needs a network connection either remote or local
;; (setf *a-dom* (document-parser "http://www.w3.org/TR/2000/REC-xml-20001006.xml" :trace nil))
;; (setf *a-dom* (document-parser "http://192.168.1.2/standards/www/XML/REC-xml-20001006.xml" :trace nil))

(setf *a-dom* (document-parser #P"www:Dokumente:Standards:XML:REC-xml-20001006.xml"))
(setf *a-dom* (document-parser "file:///www/Dokumente/Standards/XML/REC-xml-20001006.xml"))
(setf *a-dom* (document-parser #P"www:Dokumente:Standards:XML:XMLSchema:XMLSchema.xsd"))
; "www:Dokumente:Standards:XML:XMLSchema:XMLSchema.dtd"
; "www:Dokumente:Standards:XML:XMLSchema:datatypes.dtd"

(inspect *a-dom*)
(write-node *a-dom* *trace-output*)
(write-node (root *a-dom*) *trace-output*)

(inspect (stream->vector #P"xml:Tests;xml;real-mini-aleph-het.xml"))


(setf *a-dom* nil)

;; 2,162 milliseconds 604/200 mcl4.2
(let ((xutils::|REC-xml-names-19990114| nil))
  (time (setf *a-dom* (document-parser "file:///www/Dokumente/Standards/XML/REC-xml-20001006.xml"
                               :trace nil :reduce nil
                               ;; where reduction is disabled, the declared encoding must be correct
                               :encoding :ISO-8859-1))))

;; 8,015 milliseconds
(let ((xutils::|REC-xml-names-19990114| nil))
  (time (setf *a-dom* (document-parser "file://www/Dokumente/Standards/XML/REC-xml-20001006.xml"
                               :trace nil :reduce t))))

;; 17,519 milliseconds 604/200 mcl4.2
(time (setf *a-dom* (document-parser "file://www/Dokumente/Standards/XML/REC-xml-20001006.xml"
                               :trace nil :reduce t)))

;; 12,959 milliseconds 604/200 mcl4.2
(time (document-parser #P"www:Dokumente:Standards:XML:XMLConf:xmlbench Folder:Benchmark:Data:chrbig.xml"
                       :trace nil :reduce nil))

;; 14,365 milliseconds 604/200 mcl4.2
(time (setf *a-dom* (document-parser #P"www:Dokumente:Standards:XML:XMLConf:xmlbench Folder:Benchmark:Data:chrbig.xml"
                               :trace nil :reduce t)))

;; 20010913 0.912 
;; 190,737 - 21,178 milliseconds 604/200 mcl4.2
;; based on before/after gc 14664K for the 4.8M file
(time (setf *a-dom* (document-parser #P"www:Dokumente:Standards:XML:XMLConf:xmlbench Folder:Benchmark:Data:big.xml"
                               :trace nil :reduce t)))

(time (setf *a-dom* (document-parser #P"www:Dokumente:Standards:XML:XMLConf:xmlconf:xmlconf.xml"
                               :trace nil :reduce t)))

(setf *a-dom* (document-parser #P"www:dokumente:standards:xml:XHTML1+DTD:REC-xhtml1-20000126.html" :trace nil))
(inspect *a-dom*)
(write-node *a-dom* *TOP-LISTENER*)


(setf *a-dom* (document-parser #P"www:Dokumente:Standards:XML:XHTMLMOD+DTD:examples:inventory.xml" :trace nil))

(document-parser #P"www:Dokumente:Standards:XML:XHTMLMOD+DTD:examples:inventory-1.mod" :reduce t :trace nil
                 :start-name '|ExtSubset|)

;; macbeth.xml in the 1999 version, approximately 160Kbytes
;; 8.031, 7.644, 7.521 seconds 604/200 mcl4.2
(time (document-parser #p"www:Dokumente:Standards:XML:XMLConf:macbeth.xml"))

;; without reduction ( at the moment entites are still consed)
;; 3.45, 2.626 seconds
(time (document-parser #p"www:Dokumente:Standards:XML:XMLConf:macbeth.xml" :reduce nil))

;; reducint to a consed structure
;; 3,057 milliseconds 
(time (progn (document-parser #p"www:Dokumente:Standards:XML:XMLConf:macbeth.xml" :reduce 'cons) nil))

; (inspect (document-parser #p"www:Dokumente:Standards:XML:XMLConf:macbeth.xml" :reduce 'cons))

;;
;;
;; if there is an appropriate class declared for the tag name, it will be used to
;; instantiate

(defClass ||::|Envelope| (abstract-elem-node)
  ((plist)))
(defMethod initialize-instance :after ((instance ||::|Envelope|) &rest initargs &key &allow-other-keys)
  (setf (slot-value instance 'plist) initargs))

(defMethod name ((instance ||::|Envelope|)) (type-of instance))
(defMethod def ((instance ||::|Envelope|)) nil)
(defMethod parent ((instance ||::|Envelope|))
  (with-slots (plist) instance (getf plist :parent)))
(defMethod children ((instance ||::|Envelope|))
  (with-slots (plist) instance (getf plist :children)))
(defMethod (setf children) (children (instance ||::|Envelope|))
  (with-slots (plist) instance (setf (getf plist :children) children)))
(defMethod attributes ((instance ||::|Envelope|))
  (with-slots (plist) instance (getf plist :attributes)))
(defMethod namespaces ((instance ||::|Envelope|))
  (with-slots (plist) instance (getf plist :namespaces)))

(let ((*specialize-elem-node* t))
  (inspect (document-parser "<Envelope>asd<f/></Envelope>")))
(write-node (top-inspect-form) *trace-output*)

:EOF
