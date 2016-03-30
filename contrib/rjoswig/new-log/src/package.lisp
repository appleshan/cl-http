;;; -*- Package: CL-USER; Syntax: ANSI-Common-Lisp -*-

(in-package "CL-USER")

(defpackage "NEW-LOG"
  (:use "COMMON-LISP" "WWW-UTILS")
  (:export
    "MAKE-KDTREE"
    "TREE-INSERT"
    "RANGE-SEARCH" "RANGE-LIST" "RANGE-LIST-SORTED" "RANGE-COUNT" "DIMENSION-EXTREMA"
    "WALK-TREE" "TREE-DEPTHS"))

;;; ================================================================

