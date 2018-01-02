
(use-package 'trivia)

(defstruct state
  (run #'identity :type function))


(defparameter sget
  (make-state :run (lambda (x) (list x x))))


(defun put (s)
  (make-state :run (constantly (list nil s))))

(defun modify (f)
  (make-state :run (lambda (s) (list nil (funcall f s)))))


(defmethod spure (a)
  (make-state :run (lambda (s) (list a s))))

(defun >>=% (f g)
  (make-state :run (lambda (s)
                     (let-match1 (list a s%) (funcall (state-run f) s)
                       (funcall (state-run (funcall g a)) s%)))))

(defmethod fmap (f (m State))
  (make-state :run (lambda (s)
                     (let-match1 (list a s%) (funcall (state-run m) s)
                       (list (funcall f a) s%)))))

(defmethod <*> ((k state) (m state))
  (make-state :run (lambda (s)
                     (let-match* (('(f s%)  (funcall (state-run k) s))
                                  ('(a s%%) (funcall (state-run m) s%)))
                       (list (funcall f a) s%%)))))

(defmethod >>= ((f state) g)
  (>>=% f g))

(defmethod =<< (g (f state))
  (>>=% f g))
