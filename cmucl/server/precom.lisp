;;; Some make-load-form related error can be ignored. --binghe

(in-package :cl-user)

(ignore-errors
  (pcl::precompile-random-code-segments http))
