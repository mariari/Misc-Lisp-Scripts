(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :trivia)
  (use-package 'trivia))

;; here we are going to emulate the following Haskell Data Structure
;; data FingerTree a  = Empty
;;                    | Single a
;;                    | Deep (Digit a) (FingerTree (Node a)) (Digit a)
;; data Digit a = One a | Two a a | Three a a a | Four a a a a
;; data Node a = Node2 a a | Node3 a a a

;;;; creating the data structure========================================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (SETF *ARITY-CHECK-BY-TEST-CALL* NIL)
  (defconstant +empty-t+ :empty
    "Used for the empty type of finger tree")

  (defstruct-l single
      "single will be of type FingerTree"
    ele)

  (deftype finger-tree ()
    "the data structure finger tree is composed of empty, single, and deep"
    `(or (eql :empty)
        (satisfies single-p)
        (satisfies deep-p)))

  (defstruct-l deep
      "this is the spine of the finger tree, along with some digits on the sides"
    (left (make-digit) :type digit) ; the default values are a hack and should thus never be used!
    (spine :empty :type finger-tree)
    (right (make-digit) :type digit))


  (defstruct-l digit
      "a digit can hold either 1 or 2 or 3 or 4 things in it"
    one two three four)

  (defstruct-l node
      "a node either has two or three things in it"
    one two three)

  (defstruct-l view
      "a view of a finger-tree, gives back an element and the rest of the tree"
    (ele nil)
    (tree :empty :type finger-tree)))



(defun finger-tree-p (tree)
  (typep tree 'finger-tree))

(defun node-3p (node)
  "returns true -> this is a node three
   returns false -> this is a node two"
  (node-three node))

(defun empty-viewp (view)
  "we can simulate an empty view like this"
  (or (equalp view (make-view))))


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
                  :right right)
            (not (null four)))
     (let ((node (make-node :one two :two three :three four)))
       (make-deep :left  (make-digit :one x :two one)
                  :spine (cons-l node (deep-spine-l tree))
                  :right right)))
    ;; our left isn't at max capacity, so we add things to our digit
    ((deep left spine right)
     (make-deep :left (cons-l-dig x left)
                :spine spine
                :right right))))

(defun cons-r (x tree)
  "cons an element onto the back of a finger-tree"
  (match tree
    (:empty       (make-single :ele x))
    ((Single ele) (make-deep :left  (make-digit :one ele)
                             :right (make-digit :one x)))
    ((guard (deep :left  left
                  :right (digit one two three four))
            (not (null four)))
     (let ((node (make-node :one one :two two :three three))) ; want to send the elements furthest from the
       (make-deep :left  left                                 ; end down
                  :spine (cons-r node (deep-spine-l tree))
                  :right (make-digit :one four :two x))))
    ((deep left spine right)
     (make-deep :left  left
                :spine spine
                :right (cons-r-dig x right)))))


;; see deprecated code, if you really want to abstract out the patterns vs left and right views!
(defun view-l (tree)
  (match tree
    (:empty       (make-view))
    ((Single ele) (make-view :ele ele))
    ((guard (deep :left (digit one two) :right (digit :one a :two b :three c :four d))
            (null two))                                            ; checks for the case were we remove the only digit
     (make-view-l :ele one
                  :tree
                  (let ((view-spine (view-l (deep-spine-l tree)))) ; if the spine is not empty, recurse on the spine
                    (cond ((not (empty-viewp view-spine))
                           (make-deep :left  (to-digit (to-list (view-ele-l view-spine))) ; convert a node into a digit
                                      :spine (view-tree view-spine)
                                      :right (deep-right tree)))
                          ;; spine is empty, so match against the right and give it to the left!
                          (c (make-deep :left (make-digit :one a :two b) :right (make-digit :one c :two d)))
                          (b (make-deep :left (make-digit :one a)        :right (make-digit :one b)))
                          (t (make-single :ele a))))))
    ((deep left spine right)
     (let ((dig-list (to-list left)))
       (make-view-l :ele (car dig-list)
                    :tree (make-deep :left (to-digit (cdr dig-list)) :spine spine :right right))))))

;; left version has the comments, since it's the same structure!
(defun view-r (tree)
  "the right and left cases are almost the same, so just swap left for right and call it done!"
  (match tree
    (:empty       (make-view))
    ((Single ele) (make-view :ele ele))
    ((guard (deep :left (digit :one a :two b :three c :four d)
                  :right (digit one two))
            (null two))
     (make-view-l :ele one
                  :tree
                  (let ((view-spine (view-r (deep-spine-l tree))))
                    (cond ((not (empty-viewp view-spine))
                           (make-deep :left  (deep-left tree)
                                      :spine (view-tree view-spine)
                                      :right (to-digit (to-list (view-ele-l view-spine)))))
                          (c (make-deep :left (make-digit :one a :two b) :right (make-digit :one c :two d)))
                          (b (make-deep :left (make-digit :one a)        :right (make-digit :one b)))
                          (t (make-single :ele a))))))
    ((deep left spine right)
     (let* ((dig-list (to-list right))
            (last     (car (last dig-list)))
            (rest     (butlast dig-list)))
       (make-view-l :ele last
                    :Tree (make-deep :left left :spine spine :right (to-digit rest)))))))

;;; generic functions===================================================================================================
(defgeneric cat (s1 s2)
  (:documentation "concatenates two structures of the same type"))

(defmethod cat ((tree1 deep) tree2)   (finger-cat tree1 tree2))
(defmethod cat ((tree1 single) tree2) (finger-cat tree1 tree2))

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

(defmethod to-list ((tree deep))   (finger-to-list tree))
(defmethod to-list ((tree single)) (finger-to-list tree))


(defmethod to-stream ((tree deep))   (finger-to-stream tree))
(defmethod to-stream ((tree single)) (finger-to-stream tree))

(defmethod foldr (f x (single single)) (tree-foldr f x single))
(defmethod foldr (f x (deep deep))     (tree-foldr f x deep))

(defmethod foldl (f x (single single)) (tree-foldl f x single))
(defmethod foldl (f x (deep deep))     (tree-foldl f x deep))

(defmethod foldr (f x (node node))   (reduce f (to-list node) :initial-value x :from-end t))
(defmethod foldr (f x (digit digit)) (reduce f (to-list digit) :initial-value x :from-end t))

(defmethod foldl (f x (node node))   (reduce f (to-list node) :initial-value x))
(defmethod foldl (f x (digit digit)) (reduce f (to-list digit) :initial-value x))

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

(defun flip (f)
  (lambda (x y &rest args) (apply f y x args)))

(declaim (inline flip flip-2))

(defun flip-2 (f)
  (lambda (x y) (funcall f y x)))

;; we prefer Node3's as much as possible here!
;; [a] -> [Node a]
(defun nodes (xs &optional acc)
  "converts a list of elements to a list of Node of elements"
  (match xs
    (nil             (error "it requires at least 2 elements to be a node, not 0"))
    ((list _)        (error "it requires at least 2 elements to be a node, not 1"))
    ((list a b)      (reverse (cons (make-node :one a :two b)          acc)))
    ((list a b c)    (reverse (cons (make-node :one a :two b :three c) acc)))
    ((list a b c d)  (reverse (list*(make-node :one c :two d) (make-node :one a :two b) acc)))
    ((list* a b c d) (nodes d (cons (make-node :one a :two b :three c) acc)))))

(defun app3 (tree1 xs tree2)
  "concatenate two finger trees using a middle list to store elements that will go between the branches"
  (match (list tree1 tree2)
    ((list :empty _)       (lift-cons-l xs tree2))
    ((list _ :empty)       (lift-cons-r xs tree1))
    ((list (single ele) _) (cons-l ele (lift-cons-l xs tree2)))
    ((list _ (single ele)) (cons-r ele (lift-cons-r xs tree1)))
    ((list (deep :left 1l :right 1r)
           (deep :left 2l :right 2r))
     (make-deep :left 1l
                :spine (app3 (deep-spine-l tree1)
                             (nodes (append (to-list 1r) xs (to-list 2l)))
                             (deep-spine-l tree2))
                :right 2r))))

;;;; None generic versions of generic functions=========================================================================
;; note the empty type requires these calls instead of the more generic version, since a symbol is ambiguous!

(defun finger-cat (tree1 tree2)
  "concatenates two fingertrees"
  (app3 tree1 '() tree2))


(defun tree-foldr (f z tree)
  (match tree
    (:empty z)
    ((Single ele) (funcall f ele z))
    ((deep left spine right)
     (labels ((-<.  (acc xs) (foldr f acc xs))
              (-<.. (acc xs) (tree-foldr (flip-2 #'-<.) acc xs))) ; here foldr swaps the order of args, so realign it
       (declare (inline -<. -<..))                                ; we use flip-2 to make -<. inlineable!
       (-<. (-<.. (-<. z right) spine) left)))))

(defun tree-foldl (f z tree)
  (match tree
    (:empty z)
    ((Single ele) (funcall f z ele))
    ((deep left spine right)
     (labels ((>-.  (acc xs) (foldl f acc xs))
              (>-.. (acc xs) (tree-foldl #'>-. acc xs))) ; foldl does not swap args, so we are safe!
       (declare (inline >-. >-..))
       (>-. (>-.. (>-. z right) spine) left)))))

(defun lift-cons-gen (f seq tree)
  (reduce f seq :initial-value tree :from-end t))

(defun lift-cons-r (seq tree)
  (lift-cons-gen #'cons-r seq tree))

(defun lift-cons-l (seq tree)
  (lift-cons-gen #'cons-l seq tree))

(defun to-finger (seq)
  (lift-cons-l seq :empty))

(defun to-finger-r (seq)
  (to-finger-gen #'cons-r seq))

(defun finger-to-list (tree &optional acc)
  (let ((view (view-l tree)))
    (if (view-ele-l view)
        (finger-to-list (view-tree-l view) (cons (view-ele-l view) acc))
        (reverse acc))))

(defun finger-to-stream (tree)
  (let ((view (view-l tree)))
    (if (view-ele-l view)
        (scons (view-ele-l view) (finger-to-stream (view-tree-l view)))
        nil)))


;;;; Unused Ideas=======================================================================================================

(defun nodes-l (xs)
  "a lazy conversion of [a] |-> [Node a] "
  (scdr (scdr (scdr (scdr xs))))
  (scar xs)
  (scar (scdr xs))
  (scar (scdr (scdr xs)))
  (match xs
    (nil           (error "it requires at least 2 elements to be a node, not 0"))
    ((list _)      (error "it requires at least 2 elements to be a node, not 1"))
    ((list a b)    (slist (make-node :one a :two b)))
    ((list a b c)  (slist (make-node :one a :two b :three c)))
    ((list* a b c) (scons (make-node :one a :two b)
                          (nodes-l c)))))

(defun app3-l (tree1 xs tree2)
  "concatenate two finger trees using a middle list to store elements that will go between the branches"
  (match (list tree1 tree2)
    ((list :empty _)       (lift-cons-l (make-strict xs) tree2))
    ((list _ :empty)       (lift-cons-r (make-strict xs) tree1))
    ((list (single ele) _) (cons-l ele (lift-cons-l (make-strict xs) tree2)))
    ((list _ (single ele)) (cons-r ele (lift-cons-r (make-strict xs) tree1)))
    ((list (deep :left 1l :right 1r)
           (deep :left 2l :right 2r))
     (make-deep :left 1l
                :spine (app3% (deep-spine-l tree1)
                             (nodes-l (sappend (sappend (to-list 1r) xs) (to-list 2l)))
                             (deep-spine-l tree2))
                :right 2r))))

;;;; Deprecated Ideas===================================================================================================

;; instead of defining two seperate instances for right and left, and making them one-liners, Ι decdied to go for just
;; a copy and paste to the structure, but this structure can be replicated with a macro like the following, this can be
;; done for all left and right operations, very simple macro really!
;; here it only saves 10 lines, but it's wonderful if you change it a lot!


(defmacro tree-view-gen (left?)
  "the right and left cases are almost the same, so just swap left for right and call it done!"
  `(match tree
     (:empty       (make-view))
     ((Single ele) (make-view :ele ele))
     ((guard (deep :left ,(if left?
                              `(digit one two)
                              `(digit :one a :two b :three c :four d))
                   spine
                   :right ,(if left?
                               `(digit :one a :two b :three c :four d)
                               `(digit one two)))
             (null two)) ; checks for the case were we remove the only digit
      (make-view :ele one
                 :tree
                 (let ((view-spine (view-l spine))) ; if the spine is not empty, recurse on the spine
                   (cond ((not (empty-viewp view-spine)) ; the :left part converts a node into a digit
                          (make-deep :left  ,(if left?
                                                 `(to-digit (to-list (view-ele view-spine)))
                                                 `(deep-left tree))
                                     :spine (view-tree view-spine)
                                     :right ,(if left?
                                                 `(deep-right tree)
                                                 `(to-digit (to-list (view-ele view-spine))))))
                         ;; spine is empty, so match against the right and give it to the left!
                         (d (make-deep :left (make-digit :one a :two b) :spine :empty :right (make-digit :one c :two d)))
                         (c (make-deep :left (make-digit :one a :two b) :spine :empty :right (make-digit :one c)))
                         (b (make-deep :left (make-digit :one a)        :spine :empty :right (make-digit :one b)))
                         (t (make-single :ele a))))))
     ((deep left spine right)
      ,(if left?
           `(let ((dig-list (to-list left)))
              (make-view :ele (car dig-list)
                         :tree (make-deep :left (to-digit (cdr dig-list)) :spine spine :right right)))
           `(let* ((dig-list (to-list right))
                   (last     (car (last dig-list)))
                   (rest     (butlast dig-list)))
              (make-view :ele (car dig list)
                         :Tree (make-deep :left left :spine spine :right (to-digit rest))))))))