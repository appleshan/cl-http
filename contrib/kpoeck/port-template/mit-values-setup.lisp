(in-package :mit-values)

(declaim (declaration values))

(defmacro values (&body body)
  `(cl:values ,@body))

#+sbclold
(pushnew 'mit-values::values sb-pcl::*var-declarations-without-arg*)

#+cmu
(pushnew 'mit-values::values PCL::*VARIABLE-DECLARATIONS-WITHOUT-ARGUMENT*)