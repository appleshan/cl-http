;;;   -*- Mode: LISP; Package: CL-USER; BASE: 10; Syntax: ANSI-Common-Lisp;-*-

#|
<DOCUMENTATION>
 <DESCRIPTION>
  this module implement a bnf->lisp parser generator.
  </DESCRIPTION>
 <COPYRIGHT YEAR='2001' AUTHOR='james adam anderson' MARK='(C)'
            mailto='james.anderson@setf.de' />
 <LICENSE href='file://bnfp/LGPL.txt' >
    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
  </LICENSE>
 <HISTORY>
  <DELTA DATE='1999'>
   an initial version of the parser generator was used to generate java parsers </DELTA>
  <DELTA DATE='20010605' AUTHOR='MS'>
   lispworks/ansi conformance</DELTA>
  <DELTA DATE='20010620'>string regular expressions</DELTA>
  <DELTA DATE='20010712'AUTHOR='JCMA'>Genera compatibility</DELTA>
  <DELTA DATE='20010719'>loading controlled by host application</DELTA>
  </HISTORY>
 </DOCUMENTATION>
|#

;;;------------------------------------------------------------------- 
;;;
;;; ATN-BASED BNF PARSER GENERATOR
;;;

(in-package "CL-USER")

#+Genera
(unless (fs:get-logical-pathname-host "BNFP" t)
  (fs:make-logical-pathname-host "BNFP"))

#+Genera 
(sct:defsystem bnfp
    (:pretty-name "BNF Parser"
     :default-pathname "BNFP:CODE;"
   ;;  :default-destination-pathname "BNFP:BIN;" ;; doesn't work 
     :journal-directory "BNFP:PATCH;"
     :initial-status :experimental
     :patchable t
     :source-category :basic
     :before-patches-initializations (pushnew :atn-compiler *features*))
  (:module pointers
   ("SYS:SITE;BNFP.TRANSLATIONS"
    "SYS:SITE;BNFP.SYSTEM"
    "BNFP:CODE;TESTS")
   (:type :lisp-example))
  (:serial
    "BNFP:CODE;ATN-PACKAGE"
    "BNFP:CLIFS;INFERENCE-SYSTEM-CLASSES"
    "BNFP:CLIFS;INFERENCE-UNITS"
    "BNFP:CODE;ATN-PARAMETERS"
    "BNFP:CODE;ATN-CLASSES"
    "BNFP:CODE;ATN-MACROS"
    "BNFP:CODE;EBNF-TOKENIZER"
    "BNFP:CODE;EBNF-TO-ATN-TRANSLATOR"
    "BNFP:CODE;ATN-MACRO-TO-CANONIC-FORM"
    ;; The BNF grammar is coded in a specially arranged lisp file.
    "BNFP:CODE;EBNF-GRAMMAR.ATN"
    "BNFP:CODE;ATN-RUNTIME"
    "BNFP:CODE;ATN-LISP-COMPILER"
    "BNFP:CODE;ATN-REGEX.LISP"
    ;; the java translator is present here as documentation, as of 20010208 it has
    ;; not been reintegrated with the changes to compile to lisp.
    #+bnfp-java "BNFP:CODE;ATN-JAVA-COMPILER"))


#-Genera
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
    (setf (logical-pathname-translations "bnfp")
          `(("root;**;*.*" ,root-target)))
    (setf bin-target
          (merge-pathnames (compile-file-pathname "*.lisp")
                           #+(and MCL (not M68K)) "bnfp:root;bin;ppcFASL;*.*"
                           #+(and MCL M68K) "bnfp:root;bin;m68kFASL;*.*"
                           #+ALLEGRO "bnfp:root;bin;aclFASL;*.*"
                           #+LISPWORKS "bnfp:root;bin;lwFASL;*.*"))
    (setf (logical-pathname-translations "bnfp")
          `(("code;*.bin" ,bin-target)
            ("code;clifs;*.bin" ,bin-target)
            ("code;**;*.*" "bnfp:root;code;**;*.*")
            ("root;**;*.*" ,root-target)
            ("**;*.*" "bnfp:root;**;*.*")))))

#-Genera
(define-system 
  (bnfp)
  ()
  "bnfp:code;atn-package"
  "bnfp:code;clifs;inference-system-classes"
  "bnfp:code;clifs;inference-units"
  "bnfp:code;atn-parameters"
  "bnfp:code;atn-classes"
  "bnfp:code;atn-macros"
  "bnfp:code;ebnf-tokenizer"
  "bnfp:code;ebnf-to-atn-translator"
  "bnfp:code;atn-macro-to-canonic-form"
  "bnfp:code;ebnf-grammar.atn" ;; the bnf grammar is coded in a "atn" syntax lisp file.
  "bnfp:code;atn-runtime"
  "bnfp:code;atn-lisp-compiler"
  "bnfp:code;atn-regex"
  ;; the java translator is present here as documentation, as of 20010208 it has
  ;; not been reintegrated with the changes to compile to lisp.
  #+bnfp-java "bnfp:code;atn-java-compiler")

;; (execute-system-operations :bnfp '(:probe))
;; (execute-system-operations :bnfp '(:compile))


:EOF
