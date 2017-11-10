(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :trivia)
  (use-package 'trivia))

(SETF *ARITY-CHECK-BY-TEST-CALL* NIL)

;; here we are going to emulate the following Haskell Data Structure
;;  data FingerTree a  = Empty
;;                     | Single a
;;                     | Deep (Digit a) (FingerTree (Node a)) (Digit a)
;; data Digit a = One a | Two a a | Three a a a | Four a a a a
;; data Node a = Node2 a a | Node3 a a a

;;;; creating the data structure========================================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +empty-t+ :empty
    "Used for the empty type of finger tree")

  (defstruct single
    "single will be of type FingerTree"
    ele)

  (deftype finger-tree ()
    "the data structure finger tree is composed of empty, single, and deep"
    `(or (eql :empty)
        (satisfies single-p)
        (satisfies deep-p)))


  (defstruct deep
    "this is the spine of the finger tree, along with some digits on the sides"
    (left (make-digit) :type digit) ; the default values are a hack and should thus never be used!
    (spine :empty :type finger-tree)
    (right (make-digit) :type digit))


  (defstruct digit
    "a digit can hold either 1 or 2 or 3 or 4 things in it"
    one two three four)

  (defstruct node
    "a node either has two or three things in it"
    one two three))

(defun finger-tree-p (tree)
  (typep tree 'finger-tree))

(defun node-3p (node)
  "returns true -> this is a node three
   returns false -> this is a node two"
  (node-three node))


;;;; Functions==========================================================================================================
;;; time to convert some functions!

(defun cons-l (x tree)
  "cons on an element to the front of a finger-tree"
  (match tree
    (:empty       (make-single :ele x))
    ;; upgrade the single element into a deep structure with an empty spine
    ((Single ele) (make-deep :left (make-digit :one x)
                             :right (make-digit :one ele)))
    ;; this case we must recurse, as our left node is at max capacity!
    ((guard (deep :left  (digit one two three four)
                  :spine deeper
                  :right right)
            (not (null four)))
     (let ((node (make-node :one two :two three :three four)))
       (make-deep :left  (make-digit :one x :two one)
                  :spine (cons-t node deeper)
                  :right right)))
    ;; our left isn't at max capacity, so we add things to our digit
    ((deep left spine right)
     (make-deep :left (cons-l-dig x left)
                :spine spine
                :right right))))

(defun cons-r (x tree)
  "cons an element onto the back of a finger-tree"
  (match tree
    (:empty (make-single :ele x))
    ((Single ele) (make-deep :left  (make-digit :one ele)
                             :right (make-digit :one x)))
    ((guard (deep :left  left
                  :spine deeper
                  :right (digit one two three four))
            (not (null four)))
     (let ((node (make-node :one one :two two :three three))) ; want to send the elements furthest from the
       (make-deep :left  left                                 ; end down
                  :spine (cons-t node deeper)
                  :right (make-digit :one four :two x))))
    ((deep left spine right)
     (make-deep :left  left
                :spine spine
                :right (cons-r-dig x right)))))



;;; generic functions===================================================================================================

(defmethod to-list ((node node))
  (if (node-3p node)
      (list (node-one node) (node-two node) (node-three node))
      (list (node-one node) (node-two node))))

(defmethod to-list ((digit digit))
  (match digit
    ((digit one two three four) (cond (four  (list one two three four))
                                      (three (list one two three))
                                      (two   (list one two))
                                      (one   (list one))))))

;;; Helper Functions====================================================================================================

(defun to-digit (lis)
  (make-digit :one (car lis) :two (cadr lis) :three (caddr lis) :four (cadddr lis)))

(defun cons-l-dig (x dig)
  (match dig ((digit one two three four)
              (when four
                (error "can't append a node onto a digit of four"))
              (make-digit :one x :two one :three two :four three))))

(defun cons-r-dig (x dig)
  (match dig ((digit one two three four)
              (when four
                (error "can't append a node onto a digit of four"))
              (cond (three (make-digit :one one :two two :three three :four x))
                    (two   (make-digit :one one :two two :three x))
                    (one   (make-digit :one one :two x))
                    (t     (make-digit :one x))))))