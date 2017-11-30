(setf *ARITY-CHECK-BY-TEST-CALL* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:trivia))
  (use-package 'trivia))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct queue (q '() :type list) (d '() :type list)))


(defun enqueue (val queue)
  (match queue
    ((queue :d '() :q '()) (make-queue :d (list val)))
    ((queue d q)           (make-queue :q (cons val q) :d  d))))

(defun-match dequeue (queue)
  ((queue :q '() :d '())      queue)
  ((queue :q '() :d (list _)) (make-queue))
  ((queue q :d (list _))      (make-queue :d (reverse q)))
  ((queue q d)                (make-queue :d (cdr d) :q q)))

(defun queue-head (queue)
  (car (queue-d queue)))

(defun empty-p (queue)
  (equalp (make-queue) queue))

(defun enqueue-many (queue &rest enq)
  (enqueue-seq enq queue))

(defun enqueue-seq (seq queue)
  (reduce (lambda (acc-queue x) (enqueue x acc-queue)) seq :initial-value queue))

(defparameter *queue-test* (dequeue
                            (enqueue-seq '(3 34 4 5 6 7 8)
                                         (dequeue (enqueue-many (make-queue) 2 3 4 5 6)))))

;;; A queue with 3 lists***********************************************************************************************
;; we will first have to implement lazyness in CL.. IΕ after I read more of okasaki's book

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (defstruct t-queue (q '() :type list) (d '() :type list) (d '() :type list)))