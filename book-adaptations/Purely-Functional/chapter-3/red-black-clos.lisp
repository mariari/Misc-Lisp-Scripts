(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :trivia)
  (use-package 'trivia))

;;;; Red and Black tree********************************************************************************************
;;; invariant 1: no red node has a red child.
;;; invariant 2: Every path from the root to an empty node contains the same number of black nodes

(defconstant +red+ :red)
(defconstant +black+ :black)

(defconstant +empty+ :rb-Empty)



(deftype color ()
  "colors for a red-black tree"
  `(member ,+red+ ,+black+))

(deftype red-black ()
  `(or (eq :rb-Empty)
      (satisfies rb-tree-p)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (rb-tree (:conc-name t-))
    (col +black+ :type color)
    elem
    (left +empty+ :type red-black)
    (right +empty+ :type red-black)))


(defclass empty () ())

(defmethod red-black-p ((tree rb-tree)) t)
(defmethod red-black-p ((tree t))       nil)
(defmethod red-black-p ((tree symbol))  (typep tree 'red-black))


(defmethod rb-memebr (ele (tree symbol))
  nil)
(defmethod rb-member (ele (tree rb-tree))
  (match tree
    ((guard (rb-tree left elem)  (< ele elem)) (rb-member ele left))
    ((guard (rb-tree right elem) (> ele elem)) (rb-member ele right))
    (_ t)))