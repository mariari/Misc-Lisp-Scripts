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
    measure
    (left (make-digit) :type digit) ; the default values are a hack and should thus never be used!
    (spine :empty :type finger-tree)
    (right (make-digit) :type digit))


  (defstruct digit "a digit can hold either 1 or 2 or 3 or 4 things in it")

  (defstruct (digit-1 (:include digit))
      "a digit can hold either 1 or 2 or 3 or 4 things in it"
    one)

  (defstruct (digit-2 (:include digit-1))
      "a digit can hold either 1 or 2 or 3 or 4 things in it"
    two)

  (defstruct (digit-3 (:include digit-2))
      "a digit can hold either 1 or 2 or 3 or 4 things in it"
    three)

  (defstruct (digit-4 (:include digit-3))
      "a digit can hold either 1 or 2 or 3 or 4 things in it"
    four)

  (defstruct-l node
      "a node either has two or three things in it"
    (measure 0))

  (defstruct-l (node-2 (:include node))
      one two)

  (defstruct-l (node-3 (:include node))
      one two three)

  (defstruct-l view
      "a view of a finger-tree, gives back an element and the rest of the tree"
    (ele nil)
    (tree :empty :type finger-tree)))

(defun finger-tree-p (tree)
  (typep tree 'finger-tree))


(defun empty-viewp (view)
  "we can simulate an empty view like this"
  (or (equalp view (make-view))))


;;;; Monoidic Constructors==============================================================================================

(defmacro f (&rest args)
  `(funcall ,@args))

(defgeneric <> (xs ys)
  (:documentation "the monoidic operation between two values"))

(defgeneric mempty (_)
  (:documentation "the empty value for a type, takes a fake variable"))

(defgeneric bar (x &optional mempty-value)
  (:documentation "converts a structure into a monoidic structure, the mempty-value is value of the type we want, so
                   I can generically have this work on any type"))

;; These three variables allow the user to use their own measure by letting over <> and mempty before using!
(defparameter <> #'<>
  "the monidic function, by default, it goes to the defmethod, but let over this to change the behavior of functions")

(defparameter mempty #'mempty
  "the identity value by default, if users want to use their own measure, just let over this")

(defparameter bar #'bar
  "the value that gets called onto a structure to make it monoidic, by default it converts things to numbers")

(defun make-s-node (&key measure one two three (bar bar) (<> <>))
  (let ((measure? (if measure
                      measure
                      (f <> (f bar one) (f bar two)))))
    (if three
        (make-node-3 :measure (f <> measure? (f bar three))
                     :one one :two two :three three)
        (make-node-2 :measure measure?
                     :one one :two two))))

(defun make-s-deep (&key measure (left (make-digit)) (spine :empty) (right (make-digit)))
  (let ((deep (make-deep :left left :spine spine :right right)))
    (setf (deep-measure deep)
          (delay (if measure measure
                     (f <> (f <> (f bar left)
                                 (f bar (deep-spine-l deep)))
                           (f bar right)))))
    deep))


;;;; Functions==========================================================================================================
;;; time to convert some functions!

(defun cons-l (x tree)
  "cons on an element to the front of a finger-tree"
  (match tree
    (:empty       (make-single :ele x))
    ;; upgrade the single element into a deep structure with an empty spine
    ((Single ele) (make-s-deep :left (make-digit-1 :one x)
                               :right (make-digit-1 :one ele)))
    ;; this case we must recurse, as our left node is at max capacity!
    ((deep :left  (digit-4 one two three four)
           :right right)
     (let ((node (make-s-node :one two :two three :three four)))
       (make-s-deep :left  (make-digit-2 :one x :two one)
                    :spine (cons-l node (deep-spine-l tree))
                    :right right)))
    ;; our left isn't at max capacity, so we add things to our digit
    ((deep left spine right)
     (make-s-deep :left (cons-l-dig x left)
                  :spine spine
                  :right right))))

(defun cons-r (x tree)
  "cons an element onto the back of a finger-tree"
  (match tree
    (:empty       (make-single :ele x))
    ((Single ele) (make-s-deep :left  (make-digit-1 :one ele)
                               :right (make-digit-1 :one x)))
    ((deep :left  left
           :right (digit-4 one two three four))
     (let ((node (make-s-node :one one :two two :three three))) ; want to send the elements furthest from the
       (make-s-deep :left  left           ; end down
                    :spine (cons-r node (deep-spine-l tree))
                    :right (make-digit-2 :one four :two x))))
    ((deep left spine right)
     (make-s-deep :left  left
                  :spine spine
                  :right (cons-r-dig x right)))))


;; see deprecated code, if you really want to abstract out the patterns vs left and right views!
(defun view-l (tree)
  (match tree
    (:empty       (make-view))
    ((Single ele) (make-view :ele ele))
    ((deep :left (digit-1 one) right) ; checks for the case were we remove the only digit
     (make-view-l :ele one
                  :tree
                  (let ((view-spine (view-l (deep-spine-l tree)))) ; if the spine is not empty, recurse on the spine
                    (if (not (empty-viewp view-spine))
                        (make-s-deep :left  (to-digit (to-list (view-ele-l view-spine))) ; convert a node into a digit
                                     :spine (view-tree view-spine)
                                     :right (deep-right tree))
                          ;; spine is empty, so match against the right and give it to the left!
                        (match right
                          ((digit-4 :one a :two b :three c :four d) (make-s-deep :left (make-digit-2 :one a :two b)
                                                                                 :right (make-digit-2 :one c :two d)))
                          ((digit-3 :one a :two b :three c)         (make-s-deep :left (make-digit-2 :one a :two b)
                                                                                 :right (make-digit-1 :one c)))
                          ((digit-2 :one a :two b)                  (make-s-deep :left (make-digit-1 :one a)
                                                                                 :right (make-digit-1 :one b)))
                          ((digit-1 :one a)                         (make-single :ele a)))))))
    ((deep left spine right)
     (let ((dig-list (to-list left)))
       (make-view-l :ele (car dig-list)
                    :tree (make-s-deep :left (to-digit (cdr dig-list)) :spine spine :right right))))))

;; left version has the comments, since it's the same structure!
(defun view-r (tree)
  "the right and left cases are almost the same, so just swap left for right and call it done!"
  (match tree
    (:empty       (make-view))
    ((Single ele) (make-view :ele ele))
    ((deep left :right (digit-1 one))  ;; (digit :one a :two b :three c :four d)
     (make-view-l :ele one
                  :tree
                  (let ((view-spine (view-r (deep-spine-l tree))))
                    (if (not (empty-viewp view-spine))
                        (make-s-deep :left  (deep-left tree)
                                     :spine (view-tree view-spine)
                                     :right (to-digit (to-list (view-ele-l view-spine))))
                        (match left
                          ((digit-4 :one a :two b :three c :four d) (make-s-deep :left (make-digit-2 :one a :two b)
                                                                                 :right (make-digit-2 :one c :two d)))
                          ((digit-3 :one a :two b :three c)         (make-s-deep :left (make-digit-2 :one a :two b)
                                                                                 :right (make-digit-1 :one c)))
                          ((digit-2 :one a :two b)                  (make-s-deep :left (make-digit-1 :one a)
                                                                                 :right (make-digit-1 :one b)))
                          ((digit-1 :one a)                         (make-single :ele a)))))))
    ((deep left spine right)
     (let* ((dig-list (to-list right))
            (last     (car (last dig-list)))
            (rest     (butlast dig-list)))
       (make-view-l :ele last
                    :Tree (make-s-deep :left left :spine spine :right (to-digit rest)))))))

;;; generic functions===================================================================================================
(defgeneric cat (s1 s2)
  (:documentation "concatenates two structures of the same type"))

(defmethod cat ((tree1 deep) tree2)   (finger-cat tree1 tree2))
(defmethod cat ((tree1 single) tree2) (finger-cat tree1 tree2))

(defmethod to-list ((node node-2))
  (list (node-one node) (node-two node)))

(defmethod to-list ((node node-3))
  (list (node-one node) (node-two node) (node-three node)))

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
  (match lis
    ((list a)       (make-digit-1 :one a))
    ((list a b)     (make-digit-2 :one a :two b))
    ((list a b c)   (make-digit-3 :one a :two b :three c))
    ((list a b c d) (make-digit-4 :one a :two b :three c :four d))
    (t              (error "the list must be of size 4 or lesser to become a digit"))))

(defmethod cons-l-dig (x (dig digit-1))
  (make-digit-2 :one x :two (digit-1-one dig)))

(defmethod cons-l-dig (x (dig digit-2))
  (make-digit-3 :one x :two (digit-2-one dig) :three (digit-2-two dig)))

(defmethod cons-l-dig (x (dig digit-3))
  (make-digit-4 :one x :two (digit-3-one dig) :three (digit-3-two dig) :four (digit-3-three dig)))

(defmethod cons-l-dig (x (dig digit-4))
  (error "can't append a node onto a digit of four"))



(defmethod cons-r-dig (x (dig digit-1))
  (make-digit-2 :one (digit-1-one dig) :two x))
(defmethod cons-l-dig (x (dig digit-2))
  (make-digit-3 :one (digit-2-one dig) :two (digit-2-two dig) :three x))

(defmethod cons-l-dig (x (dig digit-3))
  (make-digit-4 :one (digit-3-one dig) :two (digit-3-two dig) :three (digit-3-three dig) :four x))

(defmethod cons-l-dig (x (dig digit-4))
  (error "can't append a node onto a digit of four"))

(defun flip (f)
  (lambda (x y &rest args) (apply f y x args)))

(declaim (inline flip flip-2))

(defun flip-2 (f)
  (lambda (x y) (f f y x)))

;; we prefer Node3's as much as possible here!
;; [a] -> [Node a]
(defun nodes (xs &optional acc)
  "converts a list of elements to a list of Node of elements"
  (match xs
    (nil             (error "it requires at least 2 elements to be a node, not 0"))
    ((list _)        (error "it requires at least 2 elements to be a node, not 1"))
    ((list a b)      (reverse (cons (make-s-node :one a :two b)          acc)))
    ((list a b c)    (reverse (cons (make-s-node :one a :two b :three c) acc)))
    ((list a b c d)  (reverse (list*(make-s-node :one c :two d) (make-s-node :one a :two b) acc)))
    ((list* a b c d) (nodes d (cons (make-s-node :one a :two b :three c) acc)))))

(defun app3 (tree1 xs tree2)
  "concatenate two finger trees using a middle list to store elements that will go between the branches"
  (match (list tree1 tree2)
    ((list :empty _)       (lift-cons-l xs tree2))
    ((list _ :empty)       (lift-cons-r xs tree1))
    ((list (single ele) _) (cons-l ele (lift-cons-l xs tree2)))
    ((list _ (single ele)) (cons-r ele (lift-cons-r xs tree1)))
    ((list (deep :left 1l :right 1r)
           (deep :left 2l :right 2r))
     (make-s-deep :left 1l
                  :spine (app3 (deep-spine-l tree1)
                               (nodes (append (to-list 1r) xs (to-list 2l))) ; at most there are 4 things in this list
                               (deep-spine-l tree2))
                  :right 2r))))


(defmethod node-one ((node node-2))   (node-2-one node))
(defmethod node-one-l ((node node-2)) (node-2-one-l node))
(defmethod node-two ((node node-2))   (node-2-two node))
(defmethod node-two-l ((node node-2)) (node-2-two-l node))

(defmethod node-one ((node node-3))     (node-3-one node))
(defmethod node-one-l ((node node-3))   (node-3-one-l node))
(defmethod node-two ((node node-3))     (node-3-two node))
(defmethod node-two-l ((node node-3))   (node-3-two-l node))
(defmethod node-three ((node node-3))   (node-3-three node))
(defmethod node-three-l ((node node-3)) (node-3-three-l node))


;;;; None generic versions of generic functions=========================================================================
;; note the empty type requires these calls instead of the more generic version, since a symbol is ambiguous!

(defun finger-cat (tree1 tree2)
  "concatenates two fingertrees"
  (app3 tree1 '() tree2))


(defun tree-foldr (f z tree)
  (match tree
    (:empty z)
    ((Single ele) (f f ele z))
    ((deep left spine right)
     (labels ((-<.  (acc xs) (foldr f acc xs))
              (-<.. (acc xs) (tree-foldr (flip-2 #'-<.) acc xs))) ; here foldr swaps the order of args, so realign it
       (declare (inline -<. -<..)) ; we use flip-2 to make -<. inlineable!
       (-<. (-<.. (-<. z right) spine) left)))))

(defun tree-foldl (f z tree)
  (match tree
    (:empty z)
    ((Single ele) (f f z ele))
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
  (lift-cons-r seq :empty))

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

;;;; Monoidic Instances of types========================================================================================

(defmethod bar ((x number) &optional mempty-value)
  (declare (ignore mempty-value))
  x)

(defmethod bar ((xs string) &optional mempty-value)
  (declare (ignore mempty-value))
  (length xs))

(defmethod mempty ((x number))
  (declare (ignore x))
  0)

(defmethod mempty ((x string))
  (declare (ignore x))
  "")

(defmethod <> ((x number) (y number))
  (+ x y))

(defmethod <> ((x string) (y string))
  (concatenate 'string x y))

;; node bar/norm================================
(defmethod bar ((xs node) &optional mempty-value)
  (declare (ignore mempty-value))
  (node-measure-l xs))

;; digit bar/norm==========================================================
(defmethod bar ((xs digit)
                &optional (mempty-value (f mempty (digit-one-l xs))))
  (foldl (lambda (acc a) (<> acc (bar a))) mempty-value xs))

;; Finger Tree Bar/Norm===============================
(defmethod bar ((x symbol) &optional (mempty-value 0))
  (f mempty mempty-value))

(defmethod bar ((x single) &optional mempty-value)
  (declare (ignore mempty-value))
  (bar (single-ele x)))

(defmethod bar ((x deep) &optional mempty-value)
  (declare (ignore mempty-value))
  (deep-measure-l x))
;;====================================================

;; (let ((bar (lambda (x) (declare (ignore x)) 2)))
;;   (print (make-s-node :one 3 :two 4 :bar (lambda (x) (declare (ignore x)) 8))))

(defmethod mempty ((x node))
  (mempty (node-measure-l x)))


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
    ((list a b)    (slist (make-s-node :one a :two b)))
    ((list a b c)  (slist (make-s-node :one a :two b :three c)))
    ((list* a b c) (scons (make-s-node :one a :two b)
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
     (make-s-deep :left 1l
                :spine (app3-l (deep-spine-l tree1)
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
                          (make-s-deep :left  ,(if left?
                                                 `(to-digit (to-list (view-ele view-spine)))
                                                 `(deep-left tree))
                                     :spine (view-tree view-spine)
                                     :right ,(if left?
                                                 `(deep-right tree)
                                                 `(to-digit (to-list (view-ele view-spine))))))
                         ;; spine is empty, so match against the right and give it to the left!
                         (d (make-s-deep :left (make-digit :one a :two b) :spine :empty :right (make-digit :one c :two d)))
                         (c (make-s-deep :left (make-digit :one a :two b) :spine :empty :right (make-digit :one c)))
                         (b (make-s-deep :left (make-digit :one a)        :spine :empty :right (make-digit :one b)))
                         (t (make-single :ele a))))))
     ((deep left spine right)
      ,(if left?
           `(let ((dig-list (to-list left)))
              (make-view :ele (car dig-list)
                         :tree (make-s-deep :left (to-digit (cdr dig-list)) :spine spine :right right)))
           `(let* ((dig-list (to-list right))
                   (last     (car (last dig-list)))
                   (rest     (butlast dig-list)))
              (make-view :ele (car dig list)
                         :Tree (make-s-deep :left left :spine spine :right (to-digit rest))))))))