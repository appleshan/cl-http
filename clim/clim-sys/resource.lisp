;;;   -*- Mode: LISP; Package: MINP; BASE: 10; Syntax: ANSI-Common-Lisp;-*-

(in-package "MINP")

;;; Moved package definition to CLIM-SYS/PACKAGE
;;; See also CLIM-SYS/PROCESS file.

#+ignore
(defpackage "MINP"
  (:use)
  (:import-from "RESOURCES" 
   "ALLOCATE-RESOURCE" "CLEAR-RESOURCE" "DEALLOCATE-RESOURCE"
   #+workaround-this-incompatibility "DEFRESOURCE"
   #+workaround-this-incompatibility "MAP-RESOURCE" "USING-RESOURCE")
  (:export
   "ALLOCATE-RESOURCE" "CLEAR-RESOURCE" "DEALLOCATE-RESOURCE" "DEFRESOURCE"
   "MAP-RESOURCE" "USING-RESOURCE"))

;; Customized version to match clim-sys spec
(defmacro defresource (name parameters &key constructor initializer deinitializer
                       (initial-copies 0) matcher)
  "Name, an unevaluated symbol, will become the name of the new resource.
   PARAMETERS, a lambda list, are used to initialize (create) instances of the 
   resource, and come from allocate-resource (so it can be used to supply, 
   e.g. default arguments)

   CONSTRUCTOR is a function to call to make a new object when the resource
   is empty, and accepts the PARAMETERS as arguments. Note this is required.

   Options are:

        :INITIAL-COPIES (used to set up the pool to begin with).

        :INITIALIZER (called on a newly allocated object, and the other 
        parameters). Note the constructor isn't called on objects that
        are already in the pool.

        :DEINITIALIZER (called on a newly freed object) Useful to allow gc
        of objects the resource refers to.

        :MATCHER Args are like initializer, but is expected to be a predicate
        that succeeds if the unallocated pool object is appropriate for the 
        call. The default one assumes only pool objects created with the same
        parameters are appropriate.
        This is useful if you are going to put different size objects in the
        pool, but don't want to have to create a new object when a (bigger)
        one already exists."
  ;;
  ;; Change to insure compilation of lambda forms - OBC
  ;;
  (flet ((compquotify (x)
	   (if (symbolp x)
	       (list 'quote x)
	     x)))
    `(resources::initialize-resource (resources::intern-resource ',name :if-does-not-exist :create
					   :class ',(if matcher 'matching-resource 'resource))
			  :set-name ',name
			  :set-parameters ',parameters
			  :set-constructor ,(compquotify constructor)
			  :set-initializer ,(compquotify initializer)
			  :set-deinitializer ,(compquotify deinitializer)
			  :set-matcher ,(compquotify matcher)
			  :set-initial-copies ,initial-copies
			  :from-clim-sys t)))

;;; Notice FUNCTION is first argument in CLIM-SYS specification - OBC
;;;
(defmethod map-resource (function (resource symbol) &rest args)
  (apply #'map-resource function (resources::%get-resource resource t) args))

(defmethod map-resource (function (resource resource) &rest args)
  (apply #'resources:map-resource resource function args))

;;; Great idea to include some tests.
;;; Keeping them up to date - OBC
#||
(defun make-test (resource foo bar baz)
  (print (list 'make-test resource foo bar baz))
  (list 'make-test foo bar baz))

;;; Notice the resource argument is not needed anymore
(defun test-initializer (#+ignore resource object foo bar baz)
  (print (list 'initializer 'test object foo bar baz))
  object)

(defun test-deinitializer (#+ignore resource object)
  (print (list 'deinitialize 'test object))
  object)

(defresource test
  (foo bar baz)
  ;; Unquote these symbols
  :constructor make-test
  :initializer test-initializer
  :deinitializer test-deinitializer)

(defresource test1
  (a b c)
  :constructor #'(lambda (resource a b c)
		   (print (list 'construct resource a b c)))
  :initializer #'(lambda (object a b c)
		   (print (list 'initialize object a b c))
		   object)
  :deinitializer #'(lambda (object)
		     (print (list 'deinitialize object))
		     object))
||#

;;; Trace
#||
;;; First test
MINP(64): (setq x (allocate-resource 'test 1 2 3))

(MAKE-TEST #<RESOURCE TEST @ #x145de02> 1 2 3) 
(INITIALIZER TEST (MAKE-TEST 1 2 3) 1 2 3) 
(MAKE-TEST 1 2 3)
MINP(65): (map-resource #'(lambda (&rest xx) (print xx)) 'test)

((MAKE-TEST 1 2 3) T #<RESOURCE TEST @ #x145de02>) 
NIL
MINP(66): (describe x)
(MAKE-TEST 1 2 3) is a CONS
MINP(67): (deallocate-resource 'test x)

(DEINITIALIZE TEST (MAKE-TEST 1 2 3)) 
((MAKE-TEST 1 2 3))
MINP(68): (map-resource #'(lambda (&rest xx) (print xx)) 'test)

((MAKE-TEST 1 2 3) NIL #<RESOURCE TEST @ #x145de02>) 
NIL
MINP(69):
;;; Second test - works here when defresource expr is compiled.
;;; Actually this extension to support lambdas does not seem
;;; portable so don't use it until CLIM-SYS implementations fix it...
MINP(77): (setq x (allocate-resource 'test1 1 2 3))

(CONSTRUCT #<RESOURCE TEST1 @ #xe106e2> 1 2 3) 
(INITIALIZE (CONSTRUCT #<RESOURCE TEST1 @ #xe106e2> 1 2 3) 1 2 3) 
(CONSTRUCT #<RESOURCE TEST1 @ #xe106e2> 1 2 3)
MINP(78): (map-resource #'(lambda (&rest xx) (print xx)) 'test1)

((CONSTRUCT #<RESOURCE TEST1 @ #xe106e2> 1 2 3) T #<RESOURCE TEST1 @ #xe106e2>) 
NIL
MINP(79): (deallocate-resource 'test1 x)

(DEINITIALIZE (CONSTRUCT #<RESOURCE TEST1 @ #xe106e2> 1 2 3)) 
((CONSTRUCT #<RESOURCE TEST1 @ #xe106e2> 1 2 3))
MINP(80): (map-resource #'(lambda (&rest xx) (print xx)) 'test1)

((CONSTRUCT #<RESOURCE TEST1 @ #xe106e2> 1 2 3) NIL
						#<RESOURCE TEST1 @ #xe106e2>) 
NIL
MINP(81): 
||#
