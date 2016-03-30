;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: xml-query-data-model; -*-

#|
<DOCUMENTATION>
 <DESCRIPTION>
  demonstrates a simple model definition and its serialization.
  note the effect of undeclared namespaces: the requisite nodes are generated on-demand
  first define the static namespaces
  </DESCRIPTION
 <CHRONOLOGY>
  <DELTA DATE='20010906'>names recoded for instance names v/s symbols</DELTA>
  <CHRONOLOGY>
 <DOCUMENTATION>
|#

;; 20010914 0.913

(in-package "XML-QUERY-DATA-MODEL")


(defNamespace "xmldata0")
(defNamespace "xmldata1")


;; now generate and serialize a simple model

(write-node (make-doc-node
             :root (make-elem-node
                    :name '{xmldata0}e1
                    :namespaces (list (make-ns-node :name '{xmlns}x
                                                    :children (list "xmldata0")))
                    :children (list (make-elem-node
                                     :name '{xmldata1}e1
                                     :attributes (list (make-string-attr-node
                                                        :name '{xmldata1}x
                                                        :children (list "data")))
                                     :children (list (make-elem-node
                                                      :name '{xmldata0}e1)))
                                    (make-comment-node
                                     :children '("a comment text"))
                                    (make-pi-node
                                     :target '{}aPiNode
                                     :children '("with some text")))))
            *trace-output*)

:EOF
