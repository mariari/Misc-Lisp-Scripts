;; (load "./data-structures/tuple.lisp")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package 'tuple))

(defun **** (&rest fns)
  (flet ((func (f g)
           (lambda (x)
             (tup (funcall f (fst x))
                  (funcall g (snd x))))))
    (reduce #'func fns :from-end t)))

(defun &&& (&rest fns)
  (flet ((func (f g)
           (lambda (x)
             (tup (funcall f x)
                  (funcall g x)))))
    (reduce #'func fns :from-end t)))


(defun ***** (&rest functions)
  (lambda (xs) (mapcar (lambda (f x) (funcall f x)) functions xs)))
