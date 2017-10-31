(setf *ARITY-CHECK-BY-TEST-CALL* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:trivia))
  (use-package 'trivia))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct queue (q '() :type list) (d '() :type list)))

(defun enqueue (val queue)
  (make-queue :q (cons val (queue-q queue)) :d (queue-d queue)))

;; return a nil value if the queue is empty because that's the CL way, any other language throw an error
(defun-match dequeue (queue)
  ((Queue :q '()    :d '())          (list nil queue))
  ((Queue :q queued :d '())          (let ((rev (reverse queued)))
                                       (list (car rev) (make-queue :d (cdr rev)))))
  ((Queue :q queued :d (list* x xs)) (list x (make-queue :q queued :d xs))))

(defun empty-p (queue)
  (equalp (make-queue) queue))

(defun enqueue-many (queue &rest enq)
  (enqueue-seq enq queue))

(defun enqueue-seq (seq queue)
  (reduce (lambda (acc-queue x) (enqueue x acc-queue)) seq :initial-value queue))

(defparameter *queue-test* (dequeue
                            (enqueue-seq '(3 34 4 5 6 7 8)
                                         (cadr (dequeue (enqueue-many (make-queue) 2 3 4 5 6))))))

;;; A queue with 3 lists***********************************************************************************************
;; we will first have to implement lazyness in CL.. IΕ after I read more of okasaki's book

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (defstruct t-queue (q '() :type list) (d '() :type list) (d '() :type list)))