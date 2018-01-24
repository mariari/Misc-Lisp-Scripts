(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload 'trivia)
  (use-package 'g)
  (use-package 'trivia))


(defmacro do! (&body args)
  (match args
    ((cons (list '<- var bind) rest) `(>>= ,bind (lambda (,var) (do! ,@rest))))
    ((cons x 'nil)                     x)
    ((cons x rest)                   `(>> ,x (do! ,@rest)))))


(funcall
 (state-run
  (do! (state-3-add-x 10)
       (<- x sget)
       (spure x)))
 3)
