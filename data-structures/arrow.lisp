;; (load "./data-structures/tuple.lisp")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package 'tuple))

(defun **** (f g x)
  (tup (funcall f (fst x))
       (funcall g (snd x))))

(defun &&& (f g x)
  (tup (funcall f x)
       (funcall g x)))
