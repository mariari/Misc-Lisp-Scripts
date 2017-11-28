(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:trivia
                  :fset))
  (rename-package 'fset 'fset '(:fs))
  (use-package 'trivia))

(setf *ARITY-CHECK-BY-TEST-CALL* nil)

;; Binomial Heaps *************************************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct Node (rank 0 :type Integer)
                  val
                  (tre-list '() :type list)))

(defun create-node (rank val tre-list)
  (make-node :rank rank :val val :tre-list tre-list))

(defun link (t1 t2 &optional (compare #'<=))
  "Link trees of equal rank"
  (if (funcall compare (node-val t1) (node-val t2))
      (create-node (1+ (node-rank t1)) (node-val t1) (cons t2 (node-tre-list t1)))
      (create-node (1+ (node-rank t1)) (node-val t2) (cons t1 (node-tre-list t2)))))

(defun link% (t1 t2 &optional (compare #'<=))
  "Link trees of equal rank"
  (match (list t1 t2)
         ((list (node :val v1 :tre-list c1 :node-rank r)
                (node :val v2 :tre-list c2))
          (if (funcall compare v1 v2)
              (create-node (1+ r) v1 (cons t2 c1))
              (create-node (1+ r) v2 (cons t1 c2))))))

(defstruct binomial (tree '() :type list))

(defun ins-tree (t1 ts)
  (cond ((null ts)
         (list t1))
        ((< (node-rank t1) (node-rank (car ts)))
         (cons t1 ts))
        (t
         (ins-tree (link (car ts) t1) (cdr ts)))))


(defun insert (x ts)
  (ins-tree (create-node 0 x '()) ts))


(defun bi-merge-gen (ts1 ts2 comp1 comp2)
  (cond ((null ts1) ts2)
        ((null ts2) ts1)
        (t (let ((t1 (car ts1))
                 (t2 (car ts2)))
             (cond ((funcall comp1 (node-rank t1) (node-rank t2)) (cons t1 (bi-merge-gen (cdr ts1) ts2 comp1 comp2)))
                   ((funcall comp2 (node-rank t1) (node-rank t2)) (cons t2 (bi-merge-gen ts1 (cdr ts2) comp1 comp2)))
                   (t
                    (ins-tree (link t1 t2) (bi-merge-gen (cdr ts1) (cdr ts2) comp1 comp2))))))))

(defun bi-merge (ts1 ts2)
  "the standard merge function that is used to merge nodes in the heap"
  (bi-merge-gen ts1 ts2 #'< #'>))

(defun bi-merge-rev (ts1 ts2)
  "A merge function that is used for node merging, not for heap merging"
  (bi-merge-gen ts1 ts2 #'> #'<))

(defun remove-min-tree (ts &optional (compare #'<=))
  (if (null (cdr ts))
      (list (car ts) '())
      (match (remove-min-tree (cdr ts))
        ((list min? ts2)
         (if (funcall compare (node-val (car ts)) (node-val min?))
             (list (car ts) (cdr ts))
             (list min? (cons (car ts) ts2)))))))

(defun bi-find-min (ts)
  (node-val (car (remove-min-tree ts))))

(defun delete-min (ts)
  (match (remove-min-tree ts)
    ((list min rest) (bi-merge (reverse (node-tre-list min)) rest))))

(defun bubble-up (node number)
  "bubbles the value of the node at number (counting from 0) to the parent value and shifts the parent value down"
  (labels ((list-rec (xs number)
             (cond ((null xs)      (error "the number inputted is too high for the rank of the xs"))
                   ((zerop number) (bubble-first-child (make-node :val (node-val node) :tre-list xs)))
                   (t              (let ((bub (list-rec (cdr xs) (1- number))))
                                     (make-node :rank     (node-rank node)
                                                :val      (node-val bub)
                                                :tre-list (cons (car xs) (node-tre-list bub))))))))
    (list-rec (node-tre-list node) number)))

;; use this
(defun bubble-first-child (node)
  "swaps the value of the first child and the parent node"
  (let ((child (car (node-tre-list node))))
    (make-node :rank     (node-rank node)
               :val      (node-val child)
               :tre-list (cons (make-node :rank (node-rank child)
                                          :val  (node-val node)
                                          :tre-list (node-tre-list child))
                               (cdr (node-tre-list node))))))

(defparameter *linked* (link% (link% (create-node 0 1 '())
                                     (create-node 0 2 '()))
                              (link% (create-node 0 3 '())
                                     (create-node 0 5 '()))))

(defparameter *nodes* (insert 4 (insert 1 (insert 2 (insert 3 '())))))

(defparameter *nodes15* (reduce #'insert (f:range 14) :initial-value '() :from-end t))
(defparameter *8th* (cadddr *nodes15*))
;; (defparameter *new* #S(NODE
;;                        :RANK 1
;;                        :VAL 2
;;                        :TRE-LIST (#S(NODE :RANK 0 :VAL 10 :TRE-LIST NIL))))

;; (bubble-up *new* #S(NODE
;;                    :RANK 3
;;                    :VAL 7
;;                    :TRE-LIST (#S(NODE
;;                                  :RANK 2
;;                                  :VAL 11
;;                                  :TRE-LIST (#S(NODE
;;                                                :RANK 1
;;                                                :VAL 13
;;                                                :TRE-LIST (#S(NODE
;;                                                              :RANK 0
;;                                                              :VAL 14
;;                                                              :TRE-LIST NIL)))
;;                                               #S(NODE :RANK 0 :VAL 12 :TRE-LIST NIL)))
;;                                 #S(NODE :RANK 0 :VAL 8 :TRE-LIST NIL))))