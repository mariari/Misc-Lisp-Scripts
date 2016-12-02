(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "let-over-lambda"))
(defstruct (tree! (:type list)) val left right)
(defstruct (tree+ (:type list)) len tree) ; like tree! expect we keep track of the length so we can get the last element easily


(defun make-tree!-nodes (&rest values)
  (let* ((root (make-tree! :val (car values)))
         (values-arry (coerce values 'vector))
         (length (length values-arry)))
    (labels ((child (fn pos)
               (make-tree! :val (elt values-arry (funcall fn (* 2 pos)))))
             ;; pos will always be 1+ the actual place in the array
             (rec (pos root)            
               (unless (> pos (floor length 2))
                 (rec (* 2 pos) (setf (tree!-left root) (child #'1- pos)))
                 (when (< (* 2 pos) length)
                   (rec (1+ (* 2 pos)) (setf (tree!-right root) (child #'identity pos)))))))
      (rec 1 root) root)))

(defun make-tree+-nodes (&rest values)
  (make-tree+ :len (length values) :tree (apply #'make-tree!-nodes values)))

(defparameter x (make-tree+-nodes 1 2 3 4 5 6 7 8 9 10 11 12))


(defun tree+-insert (root ele)
  (incf (tree+-len root))
  (let* ((last-spot       (path-to-last-element root))
         (spot-to-place   (car (last last-spot)))
         (path-last-ele   (nbutlast last-spot)) ; can't flip order of this and spot- since this has side effects 
         (child-to-append (tree+-traverse root path-last-ele)))
    (macrolet ((place-child (fn)
                 `(setf (,fn child-to-append) (make-tree! :val ele))))
      (if (= 0 spot-to-place)
          (place-child tree!-left)
          (place-child tree!-right))))
  root)

(defun tree!-insert (tree val)
  (let ((placed))
    (macrolet ((set-val (x)
                 `(setf ,x (make-tree! :val val) placed t)))
      (labels ((rec (root)
                 (cond (placed root)
                       ((null #1=(tree!-left  root)) (set-val #1#))
                       ((null #2=(tree!-right root)) (set-val #2#))
                       (t (if (= (random 2) 1)
                              (progn (rec #1#) (rec #2#))
                              (progn (rec #2#) (rec #1#)))))))
        (rec tree) tree))))

(time (reduce #'tree!-insert '(2 3 4 5 6) :initial-value (make-tree! :val 1)))

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
  (let ((c-child (tree+-tree root)))
    (mapc (lambda (x)
            (if (= 0 x)
                (setf c-child (tree!-left  c-child))
                (setf c-child (tree!-right c-child))))
          path)
    c-child))

(defun tree+-last-element (root)
  (tree+-traverse root (path-to-last-element root)))

(defun breadthp-in-list (l predicate &optional (acc nil))
  "takes a predicate and returns a list of all elements that match"
  (cond ((null l) acc)
        ((atom (car l))
         (if (funcall predicate (car l))
             (breadthp-in-list (cdr l) predicate (cons (car l) acc))
             (breadthp-in-list (cdr l) predicate acc)))
        (t (breadthp-in-list (append (cdr l)
                                    (car l))
                             predicate acc))))
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


;;HISTORY-------------------------------------------------------------------------------------------
(defun make-tree!-nodes% (&rest values)
  (let ((tree        (make-tree! :val (car values)))
        (values-arry (coerce (cdr values) 'vector)))
    (labels ((rec (root vals)
               (cond ((= (length vals) 0) root)
                     ((null #1=(tree!-left root)) ; use #1= instead of let because we want to save this
                        (setf #1# (make-tree! :val (elt vals 0)))
                        (rec root (subseq vals 1)))
                     ((null #2=(tree!-right root)) ; structure for later and let only saves the let
                      (let* ((split (ceiling (1- (length vals)) 2))
                             (f (lambda (x &optional y) (subseq (subseq vals 1) x y)))
                             (left-v  (funcall f 0 split))
                             (right-v (funcall f split)))
                          (setf #2# (make-tree! :val (elt vals 0))) 
                          (rec  #1# left-v)
                          (rec  #2# right-v))))))
      (rec tree values-arry) tree)))
