;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: cl-user; -*-

#|
<DOCUMENTATION>
 <DESCRIPTION>
 implementation packages for
  <ul>
   <li> XQDM: an xml document model which follows the xml-query document
    model.</li>
   <li> XQ: a serializer (parser/reader and writer functions) for the document
    aspects of xml-query algebra expressions based on the XQDM.</li>
   <li> XPATH: a parser/reader and interpreter for xpath expressions.</li>
   <li> XMLP: a parser for xml expressions based onthe XQDM.</li>
   <!-- <li> XMLP: a compatibility package downwards compatible to the older XMLP
    package.</li> was considered, but not implemented -->
   </ul>
  </DESCRIPTION>
 <COPYRIGHT YEAR='2001' AUTHOR='james adam anderson' MARK='(C)'
            mailto='james.anderson@setf.de' />
 <LICENSE href='file://xml/LGPL.txt' >
    This library is free software;
    With the exceptions noted below, you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version,
    as ammended below.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    The terms of the GNU Lesser General Public License are ammended to include
    the stipulation, as paragraph 2.e, that any modifications to the library for
    the purpose of correcting bugs must be returned to the copyright holder
    for inclusion in future versions of the library.
  </LICENSE>
 <LICENSE href='file://xml/base/tokenizer.lisp'>
  the license terms for tokenizer implementation are described in the
  respective source file.
  </LICENSE>
 <LICENSE href='file://xml/bnf/xml-grammar.bnf'>
  the www-consortium retains license rights to the respective bnf.
  it is distributed herewith under the terms of their software license.</LICENSE>
 <LICENSE href='file://xml/bnf/xpath-grammar.bnf'>
  the www-consortium retains license rights to the respective bnf.
  it is distributed herewith under the terms of their software license.</LICENSE>
 <LICENSE href='file://xml/bnf/xql-grammar.bnf'>
  the www-consortium retains license rights to the respective bnf.
  it is distributed herewith under the terms of their software license.</LICENSE>
 <LICENSE href='file://xml/demos/saxandsoap/*.lisp'>
  <COPYRIGHT YEAR='2001' AUTHOR='Stanley Knutson' MARK='(C)'
            mailto='knewt@alum.mit.edu' />
  for information on this module, please contact the author.</LICENSE>
 <CHRONOLOGY>
  <!-- nb. this version number tracks that of the 'parser' module.
       the others vary independently. -->
  <DELTA DATE='20010605' AUTHOR='MS' VERSION='0.907'>
   lispworks/ansi conformance</DELTA>
  <DELTA DATE='20010608' VERSION='0.908'>
   CL-HTTP in ALLEGRO</DELTA>
  <DELTA DATE='20010621'>xml-writer</DELTA>
  <DELTA DATE='20010702'>vector-stream moved to utils for data-url support</DELTA>
  <DELTA DATE='20010910' VERSION='0.912'>xparser tokenizers</DELTA>
  <DELTA DATE='20010910' VERSION='0.914'>0.912+lw/allegro tests</DELTA>
  <DELTA DATE='20010918' VERSION='0.915'>first-level tokenization</DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
|#

(in-package "CL-USER")

(eval-when (:load-toplevel :compile-toplevel :execute)
  (let* ((defaults
           #+allegro (or *load-truename* *load-pathname* *compile-file-truename*)
           #+ccl (or *load-truename* *load-pathname*  *compile-file-truename*
                     (pathname (ccl:front-window)))
           #+LispWorks (truename (lw:current-pathname)))
         (bin-target nil)
         (root-target (make-pathname :name :wild :type :wild
                                     :directory (append (pathname-directory defaults)
                                                        '(:wild-inferiors))
                                     :defaults defaults)))
    ;; w/o bootstrap lispworks collapsed the bin target to host and type
    (setf (logical-pathname-translations "xml")
          `(("root;**;*.*" ,root-target)))
    (setf bin-target
          (merge-pathnames (compile-file-pathname "*.lisp")
                           #+(and MCL (not M68K)) "xml:root;bin;ppcFASL;*.*"
                           #+(and MCL M68K) "xml:root;bin;m68kFASL;*.*"
                           #+ALLEGRO "xml:root;bin;aclFASL;*.*"
                           #+LISPWORKS "xml:root;bin;lwFASL;*.*"))
    (setf (logical-pathname-translations "xml")
          `(("bnfp;**;*.*" "xml:root;atn-parser;**;*.*")
            ("code;atn-lib;*.bin" ,bin-target)
            ("code;base;*.bin" ,bin-target)
            ("code;patch;*.bin" ,bin-target)
            ("code;xclos;*.bin" ,bin-target)
            ("code;xparser;*.bin" ,bin-target)
            ("code;xquery;*.bin" ,bin-target)
            ("code;xquerydatamodel;*.bin" ,bin-target)
            ("code;**;*.*" "xml:root;code;**;*.*")
            ("root;**;*.*" ,root-target)
            ("**;*.bnf" "xml:root;bnf;*.*")
            ("**;*.*" "xml:root;**;*.*")))))

#+CL-HTTP  ;; nb. this check is effective in the fasl only
(unless (find :cl-http *features*)
  (warn "CL-HTTP not present."))

;;
;; network access 
#+(and MCL (not CL-HTTP))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-declaration values nil)
  (define-declaration arglist nil)
  (require "OPENTRANSPORT"))

#+(and ALLEGRO (not CL-HTTP))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "SOCK"))


;; the atn-bnf parser must be loaded in order to translate grammars,
;; for regular expressions, and for validation. this is reflected in it presence
;; in the :xparser definition below.
;; nb. this should be refined to load only the minimum for runtime use

(eval-when (:compile-toplevel :execute :load-toplevel)
  (register-system-definition :bnfp "xml:bnfp;sysdcl")

  (define-system
    (xutil :description "xml base utilities")
    ()
    "xml:code;base;package"
    "xml:code;base;parameters"
    "xml:code;base;cllib"
    #+CL-HTTP "xml:code;base;cl-http-utils"
    "xml:code;base;utils"
    "xml:code;base;parsetable"
    "xml:code;base;vector-stream"
    #-CL-HTTP "xml:code;base;www-utils-ersatz"
    #-CL-HTTP "xml:code;base;tokenizer")

  (define-system
    (xqdm :description "model from the x-query data model")
    ()
    "xml:code;xquerydatamodel;xqdm-namespaces"
    "xml:code;xquerydatamodel;xqdm-parameters"
    "xml:code;xquerydatamodel;xqdm-character-classes"
    "xml:code;xquerydatamodel;xqdm-classes"
    "xml:code;xquerydatamodel;xsd-types"
    "xml:code;xquerydatamodel;xqdm-conditions"
    "xml:code;xquerydatamodel;xqdm-operators"
    "xml:code;xquerydatamodel;xqdm-qnames"
    "xml:code;xquerydatamodel;xqdm-validation"
    "xml:code;xquerydatamodel;xqdm-graph")

  (define-system
    (xparser :description "xml processor: parsing/serialization for xqdm with standard encoding")
    ()
    :bnfp :xutil :xqdm
    "xml:code;xparser;xml-parameters"
    "xml:code;xparser;xml-stream-coding"
    "xml:code;xparser;xml-operators"
    "xml:code;xparser;xml-readers"
    "xml:code;xparser;xml-tokenizer"
    "xml:code;xparser;xml-processing-instruction"
    "xml:code;xparser;xml-constructors"
    "xml:code;xparser;xml-parser"
    "xml:code;xparser;xml-printer"
    "xml:code;xparser;xml-writer")
  
  (define-system
    (xpath :description "xml path model and encoding to extend the processor")
    ()
    :xparser
    "xml:code;xpath;xpath-parameters"
    "xml:code;xpath;xpath-tokenizer"
    "xml:code;xpath;xpath-classes"
    "xml:code;xpath;xpath-operators"
    "xml:code;xpath;xpath-constructors"
    "xml:code;xpath;xpath-parser"
    "xml:code;xpath;xpath-printer"
    "xml:code;xpath;xpath-library")

  (define-system
    (xquery :description "xml query processor based on xml processor and xml path")
    ()
    :xparser :xpath
    "xml:code;xquery;xq-parameters"
    "xml:code;xquery;xqa-classes"
    "xml:code;xquery;xqa-operators"
    "xml:code;xquery;xql-operators"
    "xml:code;xquery;xqa-library"
    "xml:code;xquery;xql-library"
    "xml:code;xquery;xql-tokenizer"
    "xml:code;xquery;xql-constructors"
    "xml:code;xquery;xql-parser"
    "xml:code;xquery;xq-printer"
    
    ))


;; see also xml:tests;test.lisp
;; (load "entwicklung@paz:sourceServer:lisp:xml:define-system.lisp")
;; (register-system-definition :xparser "entwicklung@paz:sourceServer:lisp:xml:sysdcl.lisp")
;; (execute-system-operations :xutil '(:load))
;; (execute-system-operations :xparser '(:load))

;; to test if the pathnames are ok.
;; (execute-system-operations :xparser '(:probe))

;; to load
;; (execute-system-operations :xparser '(:load))
;; (execute-system-operations :xquery '(:load))

;; to compile / load
;; (execute-system-operations :xparser '(:compile))
;; (execute-system-operations :xparser '(:compile :load))

;; (execute-system-operations :xquery #'print)

;; (translate-logical-pathname "xml:xxx;yyy;xml-grammar.bnf")
;; (translate-logical-pathname "xml:bnfp;sysdcl")
;; (translate-logical-pathname "xml:code;xquery;xq-printer.lisp")
;; (translate-logical-pathname "xml:code;xquery;xq-printer.bin")
;; (translate-logical-pathname "xml:demo;xq-printer.lisp")
;; (translate-logical-pathname "xml:demo;xq-printer.bin")

:EOF
