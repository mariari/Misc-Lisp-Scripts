(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload 'trivia)
  (use-package 'g)
  (use-package 'trivia))


(defmacro do! (&body args)
  (match args
    ((cons (list '<- var bind) rest) `(>>= ,bind (lambda (,var) (do! ,@rest))))
    ((cons x 'nil) x)
    ((cons x rest) `(>> ,x (do! ,@rest)))))

(time (funcall
       (state-run
        (do! ;; (state-3-add-x 10)
          (<- y (state-3-add-x 10))
          (<- x sget)
          (spure (+ x y))))
       3))


(do! ;; (state-3-add-x 10)
  (<- y (state-3-add-x 10))
  (<- x sget)
  (spure (+ x y)))


(time (funcall (state-run (fmap (lambda (x) (+ x 50)) sget)) 10))
(time (funcall (state-run (fmap (lambda (x) (+ x 50)) sget)) 10))

(time (funcall (state-run (do! (<- x sget) (spure (+ x 50)))) 10))
(time (funcall (state-run (do! (<- x sget) (spure (+ x 50)))) 10))