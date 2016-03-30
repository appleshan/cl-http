
;; it is not possible to use two #1= in the same expression
;; for two independent functions!

;; terminating a file with an extra ) causes the following error:
;;*** - READ from 
;;#<INPUT BUFFERED FILE-STREAM CHARACTER
;;  #P"/path/to/file/name.lisp" @313>: an object cannot start with #\)

;; Have unclosed paren give the same type of weird error as above

;; Defining methods with same name as functions cause trouble
;; better define functions as methods with same lambda list upfront

;; The following was trying to identify a bug that disappeared
;; by simply quitting clisp. Previous botched compilation of files
;; can prevent future compilations. Beware.

;; The following type of error would indicate that a defpackage
;; used to extend an existing package is causing problems.
;;** - Continuable Error
;;RENAME-PACKAGE("COMMON-LISP"): #<PACKAGE COMMON-LISP> is locked
;;If you continue (by typing 'continue'): Ignore the lock and proceed


#||
[15]> (apropos "-ERROR" "COMMON-LISP")
;; sorted by type

ARITHMETIC-ERROR                           class
CELL-ERROR                                 class
CONTROL-ERROR                              class
FILE-ERROR                                 class
PACKAGE-ERROR                              class
PARSE-ERROR                                class
PROGRAM-ERROR                              class
READER-ERROR                               class
SIMPLE-ERROR                               class
SIMPLE-TYPE-ERROR                          class
STREAM-ERROR                               class
TYPE-ERROR                                 class

ARITHMETIC-ERROR-OPERANDS                  function
ARITHMETIC-ERROR-OPERATION                 function
CELL-ERROR-NAME                            function
FILE-ERROR-PATHNAME                        function
PACKAGE-ERROR-PACKAGE                      function
STREAM-ERROR-STREAM                        function
TYPE-ERROR-DATUM                           function
TYPE-ERROR-EXPECTED-TYPE                   function

IGNORE-ERRORS                              macro
||#

(in-package "CL-USER")

(eval-when (compile eval load)
(DEFVAR *ORIGINAL-DEFINITIONS* (MAKE-HASH-TABLE))

(DEFUN ORIGINAL-DEFINITION (SYM)
  (MULTIPLE-VALUE-BIND (VALUE FOUND)
      (GETHASH SYM *ORIGINAL-DEFINITIONS*)
    (IF FOUND
	VALUE
      (SETF (GETHASH SYM *ORIGINAL-DEFINITIONS*) (OR (MACRO-FUNCTION SYM)
						     ;; FUNCTIONP FOR CLISP
						     (AND (FUNCTIONP SYM) (FDEFINITION SYM)))))))
)

(DEFUN NOPE (&REST ARGS) ARGS)

#+ignore
(LET ((#0=#:ORIGINAL (ORIGINAL-DEFINITION 'SHOWNOBUG)))
 (MACROLET ((ORIGINAL-FUNCTION NIL '#0#)) (FMAKUNBOUND 'SHOWNOBUG)
  (DEFUN SHOWNOBUG (X &REST OPTIONS)
   (IF (STRINGP X)
    (APPLY #'NOPE X OPTIONS)
    (APPLY (ORIGINAL-FUNCTION) X OPTIONS)))))

(defvar *denotify* #+Franz-Inc nil #-Franz-Inc t)

(defmacro define-function (name redefine &rest mspec)
  (let ((#0=#:original (original-definition name)))
    (cond ((eql redefine :redefine)
	   `(let ((#0# (original-definition ',name)))
	      (macrolet ((original-function () '#0#))
		(if '#0# (fmakunbound ',name))
		(defun ,name ,@mspec))))
	  ((null #0#)
	   `(defun ,name ,redefine ,@mspec))
	  (*denotify*
	   (format t "~&Reusing ~a." name)))))

(defun name-sys (name)
  (typecase name
    (string
     (intern (string-upcase name) (find-package "CL-USER")))
    (symbol name)
    (t nil)))

(define-function showbug :redefine (sys &rest options)
  (if (stringp sys)
      (apply #'nope (name-sys sys) options)
    (apply (original-function) sys options)))
