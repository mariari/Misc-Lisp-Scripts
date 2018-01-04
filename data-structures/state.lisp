(defpackage #:state
  (:nicknames #:s)
  (:documentation "provides the state monad")
  (:use #:common-lisp
        #:tuple
        #:trivia)
  (:export :make-state
           :state-p
           :state-run
           :sget
           :put
           :modify
           :spure
           :<*> :fmap :>>= :=<<))

(in-package :state)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (SETF *ARITY-CHECK-BY-TEST-CALL* NIL))


;; newtype State s a = State {runState :: s -> (a, s)}
(defstruct (state (:constructor make-state (run)))
  (run #'identity :type function))


(defparameter sget
  (make-state (lambda (x) (tup x x))))


(defun put (s)
  (make-state (constantly (tup nil s))))

(defun modify (f)
  (make-state (lambda (s) (tup nil (funcall f s)))))


(defmethod spure (a)
  (make-state (lambda (s) (tup a s))))

(defun >>=% (f g)
  (make-state (lambda (s)
                (let-match1 (tup :fst a :snd s%) (funcall (state-run f) s)
                  (funcall (state-run (funcall g a)) s%)))))

(defmethod fmap (f (m State))
  (make-state (lambda (s)
                (let-match1 (tup :fst a :snd s%) (funcall (state-run m) s)
                  (tup (funcall f a) s%)))))

(defmethod <*> ((k state) (m state))
  (make-state (lambda (s)
                (let-match* (((tup :fst f :snd s%)  (funcall (state-run k) s))
                             ((tup :fst a :snd s%%) (funcall (state-run m) s%)))
                  (tup (funcall f a) s%%)))))

(defmethod >>= ((f state) g)
  (>>=% f g))

(defmethod =<< (g (f state))
  (>>=% f g))
