(defun increasing-order? (list)
  (unless (eq (cadr list) nil)
      (if (> (car list) (cadr list))
          nil
          (increasing-order? (cdr list)))
      t))

(increasing-order? '(1 2 3 3 4 5))
(eq '(1 2 3) '(1 2 3))

