;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: NEW-LOG -*-
;;; ================================================================
;;;; Simple Unbalanced KD-Trees
;;;  Copyright Rainer Joswig, joswig@lisp.de, 1998, 2009
;;;  All Rights Reserved

;;; Changes
;;;  2009 Rainer Joswig  Replaced recursive algorithms with iterative versions
;;;  2009 Rainer Joswig  Tweaked the arglists to follow the (range-foo tree area ...) pattern


;;; Create a KD-Tree
;;;  make-kdtree (keys predicates eq-pred) -> tree
;;;   keys is a list of accessor functions for a point, one for each dimension
;;;   predicates is a list of comparison functions, one for each dimension
;;;     it compares point values in a certain dimension
;;;   eq-pred is a function with two arguments, the arguments are tree elements, aka points
;;;
;;;   the number of keys and predicates has to be the same

;;; Insertion of objects
;;;  tree-insert (tree point)
;;;   point is an arbitrary tree element, which has values in various dimensions

;;; Retrieval of objects
;;; Area is a list of intervals. Each interval is either nil or a cons of start and end value.
;;; Start or end value can also be nil. Nil means "unbounded".
;;;  range-map                   (tree area function)
;;;  range-list                  (tree area) -> list of points
;;;  range-list-sorted           (tree area predicate &key key) -> list of points
;;;  range-list-sorted-n         (tree area predicate n &key key) -> list of max n points
;;;  range-list-different-items  (tree area key) -> list of items
;;;  range-count                 (tree area) -> number
;;;  range-average               (tree area) -> number
;;;  range-sum                   (tree area key) -> number
;;;  range-count-different-items (tree area key) -> number
;;;  dimension-extrema           (tree dimension side) -> list of points
;;;    side is one of :left or :right, dimension is the number of the dimension
;;; walk-tree                    (tree function)
;;; tree-depths                  (tree) -> list of pairs (depth . count)



;;; ================================================================
;;; Implementation of KD-Trees

(in-package "NEW-LOG")

(defclass kdtree ()
  ((top        :accessor kdtree-top        :initarg :top :initform nil)
   (dimensions :accessor kdtree-dimensions :initarg :dimensions :type fixnum)
   (accessors  :accessor kdtree-accessors  :initarg :accessors)
   (predicates :accessor kdtree-predicates :initarg :predicates)
   (eq-pred    :accessor kdtree-eq-pred    :initarg :eq-pred)))

(defclass node ()
  ((points :accessor node-points :initarg :points)
   (l      :accessor node-l      :initarg :l :initform nil)
   (r      :accessor node-r      :initarg :r :initform nil)))

(declaim (inline next-dimension))

(defun next-dimension (dimension max-dimension)
  (declare (fixnum dimension max-dimension))
  (the fixnum (mod (the fixnum (1+ dimension)) max-dimension)))



(defun make-kdtree (keys predicates eq-pred)
  "make-kdtree creates a tree.
keys is a list of accessor functions, one for each dimension
predicates is a list of comparison functions, one for each dimension
eq-pred is a function with two arguments, the arguments are tree elements"
  (assert (= (length keys) (length predicates)) (keys predicates))
  (make-instance 'kdtree
                 :dimensions (length keys)
                 :accessors  keys
                 :predicates predicates
                 :eq-pred    eq-pred))


(defmethod tree-insert ((tree kdtree) point)
  "Inserts a point into tree. Point is an arbitrary tree element."
  (let ((top (kdtree-top tree)))
    (if top
        (let ((td nil)
              (f nil)
              (is-point-member-of-points nil)
              (is-point-similar-to-points nil)
              (accessors (kdtree-accessors tree))
              (dimensions (kdtree-dimensions tree))
              (predicates (kdtree-predicates tree))
              (eq-pred (kdtree-eq-pred tree)))
          (declare (simple-vector accessors predicates)
                   (fixnum dimensions)
                   (function eq-pred))
          (loop for dimension fixnum = 0 then (next-dimension dimension dimensions)
                for points of-type cons = (node-points top)
                do (setf is-point-member-of-points (member point points)
                         is-point-similar-to-points (or is-point-member-of-points
                                                        (funcall eq-pred point (first points)))
                         td (let ((accessor (aref accessors dimension)))
                              (declare (function accessor))
                              (funcall (the function (aref predicates dimension))
                                       (funcall accessor point)
                                       (funcall accessor (first points))))
                         f top
                         top (if td (node-l top) (node-r top)))
                while (and top (not is-point-member-of-points) (not is-point-similar-to-points)))
          (if (and is-point-similar-to-points (not is-point-member-of-points))
              (push point (node-points f))
            (let ((node (make-instance 'node :points (list point))))
              (if td
                  (setf (node-l f) node)
                (setf (node-r f) node)))))
      (setf (kdtree-top tree) (make-instance 'node :points (list point))))))

(defun inside-area-p (point area keys predicates)
  (every #'(lambda (key predicate interval)
             (or (not interval)
		 (and (or (not (first interval))
                          (funcall predicate (first interval) (funcall key point)))
		      (or (not (rest interval))
                          (funcall predicate (funcall key point) (rest interval))))))
         keys predicates area))

(declaim (inline inside-area-p))

; to do: rewrite and remove recursion
(defmethod range-map ((tree kdtree) area function)
  "Applies the function over the points in the area of the tree.
Area is a list of intervals. Each interval is either nil or a cons of start and end value.
Start or end value can also be nil. Nil means unbounded."
  (let ((keys       (kdtree-accessors tree))
        (predicates (kdtree-predicates tree))
        (dimensions (kdtree-dimensions tree)))
    (assert (= (length area) dimensions (length predicates)) (area))
    (labels ((range (top dimension)
	       (when top
                 (let ((nodes (list (cons dimension top))))
                   (loop while nodes
                         do (destructuring-bind (current-dimension . current-node)
                                (pop nodes)
                              (let ((interval (nth current-dimension area)))
                                ;; left side
                                (when (or (null interval)
                                          (null (car interval))
                                          (funcall (elt predicates current-dimension)
                                                   (first interval)
                                                   (funcall (elt keys current-dimension)
                                                            (first (node-points current-node)))))
                                  (when (node-l current-node)
                                    (push (cons (next-dimension dimension dimensions) (node-l current-node))
                                          nodes)))
                                ;; inside area
                                (when (inside-area-p (first (node-points current-node))
                                                     area keys predicates)
                                  (map nil function (node-points current-node)))
                                ;; right side
                                (when (or (null interval)
                                          (null (cdr interval))
                                          (funcall (elt predicates current-dimension)
                                                   (funcall (elt keys current-dimension)
                                                            (first (node-points current-node)))
                                                   (rest interval)))
                                  (when (node-r current-node)
                                    (push (cons (next-dimension dimension dimensions) (node-r current-node))
                                          nodes))))))))))
      (range (kdtree-top tree) 0))))

(defmethod range-list ((tree kdtree) area)
  "Returns the points in the area of the tree as a list.
Area is a list of intervals. Each interval is either nil or a cons of start and end value.
Start or end value can also be nil. Nil means unbounded."
  (let ((points nil))
    (range-map tree area #'(lambda (point)
                             (push point points)))
    points))

(defgeneric range-list-sorted (tree area predicate &key key))

(defmethod range-list-sorted ((tree kdtree) area predicate &key (key #'identity))
  "Returns the points in the area of the tree as a sorted list.
Area is a list of intervals. Each interval is either nil or a cons of start and end value.
Start or end value can also be nil. Nil means unbounded."
  (let ((points nil))
    (range-map tree area #'(lambda (point)
                             (setf points (merge 'list (list point) points predicate :key key))))
    points))

(defun cut-list (list n)
  (if (plusp n)
      (let ((cell (nthcdr (1- n) list)))
        (when cell
          (setf (cdr cell) nil))
        list)
    nil))

(defmethod range-list-sorted-n ((tree kdtree) area predicate n &key (key #'identity))
  "Returns N points in the area of the tree as a sorted list.
Area is a list of intervals. Each interval is either nil or a cons of start and end value.
Start or end value can also be nil. Nil means unbounded."
  (let ((points nil))
    (range-map tree area
               #'(lambda (point)
                   (setf points (cut-list (merge 'list (list point) points predicate :key key)
                                          n))))
    points))

(defmethod range-count ((tree kdtree) area)
  "Counts the points in the area of the tree.
Area is a list of intervals. Each interval is either nil or a cons of start and end value.
Start or end value can also be nil. Nil means unbounded."
  (let ((count 0))
    (range-map tree area #'(lambda (point)
                             (declare (ignore point))
                             (incf count)))
    count))

(defmethod range-sum ((tree kdtree) area key)
  "Sums the points in the area of the tree.
Area is a list of intervals. Each interval is either nil or a cons of start and end value.
Start or end value can also be nil. Nil means unbounded."
  (let ((sum 0))
    (range-map tree area #'(lambda (point)
                             (incf sum (funcall key point))))
    sum))

(defmethod range-average ((tree kdtree) area key)
  "Returns the average of the points in the area of the tree.
Area is a list of intervals. Each interval is either nil or a cons of start and end value.
Start or end value can also be nil. Nil means unbounded."
  (let ((sum 0)
        (count 0))
    (range-map tree area #'(lambda (point)
                             (incf count)
                             (incf sum (funcall key point))))
    (/ sum count)))

(defmethod range-count-different-items ((tree kdtree) area key)
  (let ((table (make-hash-table :test #'equalp)))
    (range-map tree area
               #'(lambda (point)
                   (let ((item (funcall key point)))
                     (setf (gethash item table) item))))
    (hash-table-count table)))

(defmethod range-list-different-items ((tree kdtree) area key)
  (let ((table (make-hash-table :test #'equalp)))
    (range-map tree area
               #'(lambda (point)
                   (let ((item (funcall key point)))
                     (setf (gethash item table) item))))
    (loop for value being the hash-values of table
	  collect value)))

(defmethod dimension-extrema ((tree kdtree) dimension-n side)
  "Returns the extreme points in one dimension on a given side.
Dimension-n is a number.
Side is one of :left or :right"
  (let ((keys (kdtree-accessors tree))
	(predicates (kdtree-predicates tree))
	(dimensions (kdtree-dimensions tree)))
    (assert (and (<= dimension-n dimensions)
		 (<= dimension-n (length predicates)))
        (dimension-n))
    (assert (member side '(:left :right)) (side))
    (let ((predicate (elt predicates dimension-n))
	  (key (elt keys dimension-n))
	  (extreme-points nil)
	  (extreme-value nil))
      (labels ((set-extrema (value node)
		 (setf extreme-value value
		       extreme-points (node-points node)))
	       (check-node (node)
                 "Check if we have a new extrema."
		 (let* ((value (funcall key (first (node-points node))) )
			(pred-value (funcall predicate value extreme-value)))
		   (ecase side
		     (:left (when pred-value (set-extrema value node)))
		     (:right (unless pred-value (set-extrema value node))))
		   pred-value))
	       (extrema (top dimension)
                 "walks down the tree"
		 (when top
                   (let ((nodes (list (cons dimension top))))
                     (loop while nodes
                           do (destructuring-bind (current-dimension . current-node)
                                  (pop nodes)
                                (check-node current-node)
                                (let ((next-dimension (next-dimension current-dimension dimensions)))
                                  (if (= current-dimension dimension-n)
                                      (let ((subnode (ecase side
                                                       (:left (node-l current-node))
                                                       (:right (node-r current-node)))))
                                        (when subnode
                                          (push (cons next-dimension subnode) nodes)))
                                    (progn
                                      (when (node-l current-node)
                                        (push (cons next-dimension (node-l current-node))
                                              nodes))
                                      (when (node-r current-node)
                                        (push (cons next-dimension (node-r current-node))
                                              nodes)))))))))))
	(let ((top (kdtree-top tree)))
	  (when top
	    (set-extrema (funcall key (first (node-points top))) top)
	    (extrema top 0))
	  extreme-points)))))

(defmethod walk-tree ((tree kdtree) function)
  "Applies the function over all the nodes in the tree.
The argument function should expect the node and its depth in the tree
as its arguments."
  (let ((nodes (list tree))
        (depth 0))
    (loop while nodes
          do (setf nodes (loop for node in nodes
                               for l = (node-l node) and r = (node-r node)
                               do (funcall function node depth)
                               when l collect l
                               when r collect r))
          do (incf depth))))

(defmethod tree-depths ((tree kdtree))
  "Returns a list of (depth . count) items.
Gives you the number of nodes at each depth."
  (let ((depths nil))
    (flet ((node-depth (node depth)
             (declare (ignore node))
             (let ((item (assoc depth depths)))
               (if item
                   (incf (rest item))
                 (push (cons depth 1) depths)))))
      (walk-tree tree #'node-depth)
      (sort depths #'< :key #'first))))
    

;;; ================================================================
;;; End of File
