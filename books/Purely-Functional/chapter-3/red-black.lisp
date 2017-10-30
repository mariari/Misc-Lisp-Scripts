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
  `(or (eql :rb-Empty)
      (satisfies rb-tree-p)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (rb-tree (:conc-name t-))
    (col +black+ :type color)
    elem
    (left +empty+ :type red-black)
    (right +empty+ :type red-black)))

(defun red-black-p (tree)
  (typep tree 'red-black))

;;;; Actual code***************************************************************************************************


(defun rb-member (ele tree)
  (match tree
    (:rb-empty nil)
    ((guard (rb-tree left elem)  (< ele elem)) (rb-member ele left))
    ((guard (rb-tree right elem) (> ele elem)) (rb-member ele right))
    (_ t)))


;; a b c d are trees
;; x y and z are values
;; more precisely a = left-left
;;                b = left-right
;;                c = right-left
;;                d = right-right
;;                x = left-ele
;;                y = main-ele
;;                z = right-ele
(declaim (ftype (FUNCTION (color red-black T red-black) (VALUES RB-TREE &OPTIONAL)) balance))
(defun balance (color left val right)
  (match (list color left val right)
    ((cons :black
           (or (list (rb-tree :col :red
                             :left (rb-tree :col :red :left a :elem x :right b)
                             :elem y
                             :right c)
                    z d)
              (list (rb-tree :col   :red
                             :left  a
                             :elem  x
                             :right (rb-tree :col :red :left b :elem y :right c))
                    z d)
              (list a x
                    (rb-tree :col   :red
                             :left  (rb-tree :col :red :left b :elem y :right c)
                             :elem  z
                             :right d))
              (list a x
                    (rb-tree :col   :red
                             :left  b
                             :elem  y
                             :right (rb-tree :col :red :left c :elem z :right d)))))
     (make-rb-tree :col :red
                   :left (make-rb-tree :col :black :left a :elem x :right b)
                   :elem y
                   :right (make-rb-tree :col :black :left c :elem z :right d)))
    (_
     (make-rb-tree :col color :left left :elem val :right right))))


(defun rb-insert (val tree)
  (labels ((ins (tree)
             (match tree
               (:rb-empty                    (make-rb-tree :col +red+ :elem val))
               ((rb-tree col left elem right) (cond ((< val elem) (balance col (ins left) elem right))
                                                    ((> val elem) (balance col left       elem (ins right)))
                                                    (t             tree))))))
    (match (ins tree)
      ((rb-tree left elem right)
       (make-rb-tree :left left :elem elem :right right))))) ; make the same tree, but have the first node be black


(defun rb-insert-many (tree &rest list)
  (reduce (lambda (acc x) (rb-insert x acc)) list :initial-value tree))

(defun rb-insert-seq (seq tree)
  (apply #'rb-insert-many tree seq))

;; Testing functions***********************************************************************************************
(defparameter *manual-tree-test* (make-rb-tree :elem 7
                                               :left (make-rb-tree :col +red+
                                                                   :elem 4
                                                                   :right (make-rb-tree :col +red+ :elem 5))))

(defparameter *tree-bal* (balance (t-col *manual-tree-test*)
                                  (t-left *manual-tree-test*)
                                  (t-elem *manual-tree-test*)
                                  (t-right *manual-tree-test*)))

(defparameter *tree* (rb-insert 1 (rb-insert 2 (rb-insert 3 +empty+))))

;; (time (rb-insert-seq (loop for i from 0 to 1000 collect (random 1000)) +empty+))
