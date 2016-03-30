;;;   -*- Mode: LISP; Package: xhtml1.1; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-
;;;
;;; (c) Copyright 2011, Chun Tian (binghe)
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; XHTML 1.1 (Module-based XHTML) GENERATION
;;;
;;; Specification available at: http://www.w3.org/TR/xhtml11/
;;;
;;; This facility is a largely complete and accurate XHTML 1.0 implementation.
;;; Please be on the lookout for any missing or incorrect functionality
;;; and report it right away. 9/26/2005 -- JCMa.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; PACKAGE DEFINITION
;;;

(defpackage xhtml1.1
  (:use future-common-lisp xhtml1.0 www-utils url)
  (:shadow
   "DECLARE-HTML-VERSION"
   "WITH-HTML-DOCUMENT")
  (:export
   "DECLARE-HTML-VERSION"
   "WITH-HTML-DOCUMENT"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (mapc #'(lambda (x) (import (intern x :html4.0) :xhtml1.1))
        '("%%WRITE-STANDARD-ARGUMENTS"
          "%WITH-ENVIRONMENT"
          "%WRITE-COMMAND-KEY-ARG"
          "%WRITE-ID-ARGUMENT"
          "%WRITE-ID-ARGUMENT-HANDLING-XHTML-BACKWARD-COMPATIBLY"
          "%WRITE-CLASS-ARGUMENT"
          "%WRITE-TITLE-ARGUMENT"
          "%WRITE-STYLE-ARGUMENT"
          "%WRITE-WIDTH-ARGUMENT"
          "%MAKE-WITH-HTML-DOCUMENT-ENV"
          "WITH-DEPRECATION-CHECKING")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (mapc #'(lambda (x) (import (intern x :xhtml1.0) :xhtml1.1))
        '("%WRITE-XML-NAMESPACE-ARG")))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (mapc #'(lambda (x)
            (let ((sym (intern x :xhtml1.0)))
              (import sym :xhtml1.1)
              (export sym :xhtml1.1)))
        '("%ISSUE-COMMAND"
          "DECLARE-XML-VERSION"
          "*XHTML-BACKWARD-COMPATIBLE-IDENTIFIERS*"
          "XHTML-CHECK-BACKWARD-COMPATIBLE-IDENTIFIER-STRING")))

;; Make sure we're exporting a compete set of XML1.0 symbols.
(eval-when (:compile-toplevel :execute :load-toplevel)
  (let* ((ancestor-pkg (find-package :xhtml1.0))
         (pkg (find-package :xhtml1.1))
         (shadowed-symbols (mapcar #'symbol-name (package-shadowing-symbols pkg))))
    (do-external-symbols (sym ancestor-pkg)
      (let ((name (symbol-name sym)))
        (unless (member name shadowed-symbols :test #'equalp)   ; don't export shadowed symbols
          (import sym pkg)
          (export sym pkg))))))

(in-package :xhtml1.1)

(define declare-html-version (&optional (stream *output-stream*) (dtd-version :frameset))
  "Declares the document type as the current HTML generation DTD.
All XHTML 1.1 document must declare the document type definition version.

   DTD-VERSION can be any of:

      :STRICT       - includes all elements that have not been deprecated and do not appear in
                      frameset documents.

      :TRANSITIONAL - includes everything is the STRICT DTD plus deprecated elements and attributes.

      :FRAMESET     - includes everything in TRANSITIONAL plus frames."
  (when dtd-version
    (%issue-command ("!DOCTYPE html PUBLIC" stream :fresh-line t :trailing-line t)
      (ecase dtd-version
        ((:frameset t)
         (fast-format stream " ~S ~S" "-//W3C//DTD XHTML 1.1 Frameset//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11-frameset.dtd"))
        (:strict
	  (fast-format stream " ~S ~S" "-//W3C//DTD XHTML 1.1 Strict//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11-strict.dtd"))
        (:transitional
	  (fast-format stream " ~S ~S" "-//W3C//DTD XHTML 1.1 Transitional//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11-transitional.dtd"))))))

(define-macro with-html-document ((&key (declare-dtd-version-p :transitional) (character-encoding :utf-8)
					(language nil language-supplied-p) (direction nil direction-supplied-p)
                                        (stream '*output-stream*)) &body body)
  "Asserts the contents of BODY is an HTML document.

CHARACTER-ENCODING in XML defaults to :UTF-8 or :UTF-16. For character encoding other than these,
applications MUST supply CHARACTER-ENCODING.

DECLARE-DTD-VERSION-P will declare the version of the DTD implemented by the current generation
package. This should be :STRICT whenever generation strictly conforms to the HTML version associated
with the macro WITH-HTML-DOCUMENT. XHTML 1.0 offers three DTD versions.  Consequently,
DECLARE-DTD-VERSION-P can be any of :FRAMESET, :TRANSITIONAL, or :STRICT.  A value of T is
interpreted as :FRAMESET. DECLARE-DTD-VERSION-P should always be NIL, whenever extension tags or
features outside these specifications are used.

LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)

DIRECTION is the base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT."
  (let ((xml-declaration `((declare-xml-version ,.(when character-encoding `(:character-encoding ,character-encoding)) :stream ,stream)))
        (xml-args `((%write-xml-namespace-arg ,stream "http://www.w3.org/1999/xhtml"))))
    (%make-with-html-document-env stream 'declare-html-version xml-declaration xml-args declare-dtd-version-p
                                  language language-supplied-p direction direction-supplied-p
                                  body)))

