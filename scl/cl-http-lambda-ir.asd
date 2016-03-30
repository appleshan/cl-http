
;;;
(mk:defsystem lambda-ir
    :source-pathname "http:"
    :components
    ((:module lambda-ir :source-pathname "http:lambda-ir;"
	      :components
	      ("package"
	       "ir-utils"
	       "variables"
	       "class"))
     (:module scl :source-pathname "http:scl;lambda-ir;"
	      :components
	      ("bit-vectors"))
     (:module lambda-ir :source-pathname "http:lambda-ir;"
	      :components
	      ("data-structures"
	       "computations"
	       "ir-base"
	       "contexts"
	       "constraints"
	       "bin-dump-utils"))
     (:module examples :source-pathname "http:lambda-ir;examples;"
	      :components
	      ("lambdavista"
	       "stemming"
	       "lambdavista-exports"))))
