(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:trivia
                  :lil))
  (use-package 'trivia)
  (use-package 'interface)
  (use-package :lil/core/metaclass)
  (use-package :lil/interface/collection))

;; (lil/interface/run:run)
(setf *ARITY-CHECK-BY-TEST-CALL* nil)

(define-interface <queue> (<type>)
    ()
  (:abstract)
  (:generic enqueue (val queue) (:values queue))
  (:generic> dequeue (queue) (:in 1))
  (:generic> empty-p (queue) (:in 1))
  (:generic> test (queue val1 val2))
  ((key-interface )))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct que (q '() :type list) (d '() :type list))
  (define-interface <que> (<queue> <finite-collection>) ()
    (:parametric () (make-instance '<que> :empty-interface (make-que))) ; allows me to instantiate with <que>
    ))


(defmethod enqueue (val (queue que))
  (make-que :q (cons val (que-q queue)) :d (que-d queue)))

(defmethod test ((i <que>) que val1 val2)
  (order<= (key-interface i) val1 val2))
;;; A queue with 3 lists***********************************************************************************************