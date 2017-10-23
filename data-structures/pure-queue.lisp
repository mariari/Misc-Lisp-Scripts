(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:trivia))
  (use-package 'trivia))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct queue (q '() :type list) (d '() :type list)))

(defun enqueue (val queue)
  (make-queue :q (cons val (queue-q queue)) :d (queue-d queue)))

;; return a nil value if the queue is empty because that's the CL way, any other language throw an error
(defun dequeue (queue)
  (match queue
    ((Queue :q '()    :d '())          (list nil queue))
    ((Queue :q queued :d '())          (let ((rev (reverse queued)))
                                         (list (car rev) (make-queue :d (cdr rev)))))
    ((Queue :q queued :d (list* x xs)) (list x (make-queue :q queued :d xs)))))

(defun empty-p (queue)
  (equalp (make-queue) queue))

(defun enqueue-many (queue &rest enq)
  (reduce (lambda (acc-queue x) (enqueue x acc-queue)) enq :initial-value queue))

(defun enqueue-seq (seq queue)
  (apply #'enqueue-many queue seq))

(defparameter *queue-test* (dequeue
                            (enqueue-seq '(3 34 4 5 6 7 8)
                                         (cadr (dequeue (enqueue-many (make-queue) 2 3 4 5 6))))))