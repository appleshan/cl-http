(in-package :cl-user)

#-(or genera)
(eval-when 
    (:load-toplevel :execute :compile-toplevel)
  (declaim (declaration #-sbcl values arglist)))