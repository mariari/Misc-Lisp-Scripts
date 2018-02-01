(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "let-over-lambda"))
(defstruct (tree! (:type list)) val left right)
(defstruct (tree+ (:type list)) len tree) ; like tree! expect we keep track of the length so we can get the last element easily

;; Violates TCO, we should continuation style pass to fix this
;; does not over flow even on 100,000+ nodes
;; overflows after 200,000~ nodes
;; NOTE the impure version also has this issue
(defun make-tree!-nodes (&rest values)
  (let* ((root        (make-tree! :val (car values)))
         (values-arry (coerce values 'vector))
         (length      (length values-arry)))
    (labels ((child (fn pos)
               (make-tree! :val (elt values-arry (funcall fn (* 2 pos)))))
             ;; pos will always be 1+ the actual place in the array
             (rec (pos root)
               (flet ((create-tree (right)
                        (make-tree! :val   (tree!-val root)
                                    :left  (rec (* 2 pos) (child #'1- pos))
                                    :right right)))
                 (cond ((> pos (floor length 2)) (make-tree! :val (tree!-val root)))
                       ((<= length (* 2 pos))    (create-tree (tree!-right root)))
                       (t                        (create-tree (rec (1+ (* 2 pos)) (child #'identity pos))))))))
      (rec 1 root))))

;; (create-tree (<*> (° rec 1+ (* 2)) (child #'identity) pos))  ; no applicative :((((

(defun make-tree+-nodes (&rest values)
  (make-tree+ :len (length values) :tree (apply #'make-tree!-nodes values)))

(defparameter *x* (make-tree+-nodes 1 2 3 4 5 6 7 8 9 10 11 12))


(defun tree+-insert (root ele)
  (let* ((root            (make-tree+ :len  (1+ (tree+-len root)) :tree (tree+-tree root)))
         (last-spot       (path-to-last-element root))
         (spot-to-place   (car (last last-spot)))
         (path-last-ele   (butlast last-spot))) ; can use nbutlast instead of butlast here to make the code slightly faster
    (flet ((update-tree (f)
             (tree+-traverse-update root path-last-ele f)))
      (if (= 0 spot-to-place)
          (update-tree (lambda (node)
                         (make-tree! :left (make-tree! :val ele) :right (tree!-right node)   :val (tree!-val node))))
          (update-tree (lambda (node)
                         (make-tree! :left (tree!-left node)     :right (make-tree! :val ele) :val (tree!-val node))))))))


;; Last function left to replace
(defun tree!-insert% (tree val)
  (macrolet ((set-val (x)
               `(setf ,x (make-tree! :val val))))
    (labels ((rec (root)
               (cond ((null #1=(tree!-left  root)) (set-val #1#))
                     ((null #2=(tree!-right root)) (set-val #2#))
                     ((= (random 2) 1)             (rec #1#))
                     (t                            (rec #2#)))))
      (rec tree) tree)))

(defun tree!-insert (tree val)
  (cond ((null (tree!-left tree))
         (make-tree! :val   (tree!-val tree)
                     :left  (make-tree! :val val)))
        ((null (tree!-right tree))
         (make-tree! :val   (tree!-val tree)
                     :left  (tree!-left tree)
                     :right (make-tree! :val val)))
        ((= (random 2) 1)
         (make-tree! :val   (tree!-val tree)
                     :left  (tree!-insert (tree!-left tree) val)
                     :right (tree!-right tree)))
        (t
         (make-tree! :val   (tree!-val tree)
                     :left  (tree!-left tree)
                     :right (tree!-insert (tree!-right tree) val)))))

(time (reduce #'tree!-insert '(2 3 4 5 6) :initial-value (make-tree! :val 1)))

(time (reduce #'tree+-insert '(2 3 4 5 6) :initial-value (make-tree+-nodes 1)))

(defun tree!-to-list (tree)
  (reverse (breadthp-in-list tree (lambda (x) (not (null x))))))


;;HELPER FUNCTIONS----------------------------------------------------------------------------------
(defun number-to-redix-list (num &optional (base 10) acc)
  (if (zerop num)
      acc
      (multiple-value-bind (val rem) (floor num base)
        (number-to-redix-list val base (cons rem acc)))))

(defun num-to-base2-list (num)
  (number-to-redix-list num 2))

(defun path-to-last-element (root)
  (cdr (num-to-base2-list (tree+-len root))))

(defun tree+-traverse (root path)
  "Given a binary path, finds the node in the tree+ (0 left, 1 right) where the location lies"
  (make-tree+ :len  (tree+-len root)
              :tree (tree!-traverse (tree+-tree root) path)))

(defun tree+-traverse-update (root path f)
  "Given a binary path, finds the node in the tree+ (0 left, 1 right) where the location lies"
  (make-tree+ :len  (tree+-len root)
              :tree (tree!-traverse-update (tree+-tree root) path f)))

(defun tree!-traverse (root path)
  "Given a binary path, finds the node in the tree! (0 left, 1 right) where the location lies"
  (reduce (lambda (tree x)
            (if (= 0 x)
                (tree!-left tree)
                (tree!-right tree)))
          path :initial-value root))

;; replace with a reduce
;; the reduce will also make this so it doesn't violate TCO
(defun tree!-traverse-update (root path f)
  "Given a binary path, updates the node in the tree! (0 left, 1 right) with the function given"
  (cond ((null path)      (funcall f root))
        ((= 0 (car path)) (make-tree! :val   (tree!-val root)
                                      :left  (tree!-traverse-update (tree!-left root) (cdr path) f)
                                      :right (tree!-right root)))
        (t                (make-tree! :val   (tree!-val root)
                                      :left  (tree!-left root)
                                      :right (tree!-traverse-update (tree!-right root) (cdr path) f)))))

(defun tree+-last-element (root)
  (tree+-traverse root (path-to-last-element root)))

(defun breadthp-in-list (l pred &optional (acc nil))
  "takes a predicate and returns a list of all elements that match"
  (flet ((f (l acc)
           (breadthp-in-list l pred acc)))
    (cond ((null l)               (reverse acc))
          ((listp (car l))        (f (append (cdr l) (car l)) acc))
          ((funcall pred (car l)) (f (cdr l)                  (cons (car l) acc)))
          (t                      (f (cdr l)                  acc)))))

;; note that this function goes in depth first order
(defun depthp-in-list (l pred &optional (acc nil))
  "takes a predicate and returns a list of all elements that match"
  (reduce (lambda (y xs)
            (cond ((listp y)        (dethp-in-list y pred xs))
                  ((funcall pred y) (cons y xs))
                  (t                xs)))
          l :initial-value acc :from-end t))
;;--------------------------------------------------------------------------------------------------

;; From PAIP
(defun breadth-search (l predicate)
  "Returns the first atom found in l that satisfies
   the predicate, nil if one ain't there."
  (format t "~%input list: ~a" l)
  (cond ((null l) nil)
        ((atom (car l))
         (if (funcall predicate (car l))
           (car l)
           (breadth-search (cdr l) predicate)))
        (t (breadth-search (append (cdr l)
                                   (car l))
                           predicate))))

;; From PAIP
(defun depth-search (l predicate)
  "Returns the first atom found in l that satisfies
   the predicate, nil if one ain't there."
  (format t "~%input list: ~a" l)
  (cond ((null l) nil)
        ((atom (car l))
         (if (funcall predicate (car l))
           (car l)
           (depth-search (cdr l) predicate)))
        (t (depth-search (append (car l)
                                 (cdr l))
                         predicate))))
