(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "bordeaux-threads")
  (ql:quickload "bt-semaphore")
  (ql:quickload "inferior-shell")
  (ql:quickload "alexandria")
  (ql:quickload '(:fare-quasiquote-readtable
                  :fare-quasiquote
                  :trivia))
  (asdf:load-system :uiop))

(defpackage #:pure-data
  (:use #:uiop
        #:trivia
        #:common-lisp))


(in-package :pure-data)
;; Leftist heaps **************************************************************************************************

(defparameter empty-heap 'empty)

(defstruct heap (rank 1) value (left empty-heap) (right empty-heap))


(defun heap-empty-p (heap)
  (equalp heap empty-heap))

(defun heap-merge (x-heap y-heap &key (compare #'<))
  (cond ((heap-empty-p x-heap) y-heap)
        ((heap-empty-p y-heap) x-heap)
        ((funcall compare (heap-value x-heap) (heap-value y-heap))
         (make-t (heap-value x-heap)
                 (heap-left  x-heap)
                 (heap-merge (heap-right x-heap) y-heap)))
        (t
         (make-t (heap-value y-heap)
                 (heap-left y-heap)
                 (heap-merge x-heap (heap-right y-heap))))))

(defun rank (heap)
  (if (heap-empty-p heap)
      0
      (heap-rank heap)))

(defun make-t (val xs ys)
  (if (>= (rank xs) (rank ys))
      (make-heap :rank (1+ (rank ys)) :value val :left xs :right ys)
      (make-heap :rank (1+ (rank xs)) :value val :left ys :right xs)))

(defun heap-insert (x heap)
  (heap-merge (make-heap :value x) heap))

(defun find-min (heap)
  (heap-value heap))

(defun remove-min (heap)
  (heap-merge (heap-left heap) (heap-right heap)))


;; Example Usage

(defparameter *x* (make-heap :rank 2 :value 1 :left  (make-heap :value 2)
                                              :right (make-heap :value 3)))


(defparameter *y* (heap-merge (heap-merge (make-heap :value 4)
                                          (make-heap :value 3))
                              (heap-merge (make-heap :value 1)
                                          (make-heap :value 2))))

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


(defun bi-merge (ts1 ts2)
  (cond ((null ts1) ts2)
        ((null ts2) ts1)
        (t (let ((t1 (car ts1))
                 (t2 (car ts2)))
             (cond ((< (node-rank t1) (node-rank t2)) (cons t1 (bi-merge (cdr ts1) ts2)))
                   ((> (node-rank t1) (node-rank t2)) (cons t2 (bi-merge ts1 (cdr ts2))))
                   (t
                    (ins-tree (link t1 t2) (bi-merge (cdr ts1) (cdr ts2)))))))))

(defun bi-merge% (ts1 ts2)
  (match (list ts1 ts2)
    ((list '() ts2) ts2)
    ((list ts1 '()) ts1)
    ((list (list* (node (node-rank r1)) tr1)
           (list* (node (node-rank r2)) tr2))
     (cond ((< r1 r2) (cons (car ts1) (bi-merge% tr1 ts2)))
           ((> r1 r2) (cons (car ts2) (bi-merge% ts1 tr2)))
           (t (ins-tree (link (car ts1) (car ts2)) (bi-merge% tr1 tr2)))))))

(defun remove-min-tree (ts &optional (compare #'<=))
  (if (null (cdr ts))
      (list (car ts) '())
      (match (list (remove-min-tree (cdr ts)) ts)
        ((list (list* min? ts2)
               (list* t1 ts1))
         (if (funcall compare (node-rank t1) (node-rank min?))
             (list t1 ts1)
             (list min? (cons t1 ts2)))))))

(defun bi-find-min (ts)
  (node-rank (car (remove-min-tree ts))))

(defun delete-min (ts)
  (match (remove-min-tree ts)
    ((list min rest) (bi-merge (reverse (node-tre-list min)) rest))))

(defparameter *linked* (link% (link% (create-node 0 1 '())
                                     (create-node 0 2 '()))
                              (link% (create-node 0 3 '())
                                     (create-node 0 5 '()))))

(defparameter *nodes* (insert 4 (insert 1 (insert 2 (insert 3 '())))))
