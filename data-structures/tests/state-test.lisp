(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package 'state)
  (use-package 'g))

(funcall (state-run
          (fmap #'1+ (>> (>>= sget (f:compose #'put #'1+))
                         (spure 8))))
;; [9 3]
         2)


(defun state-3-add-x (x)
  (>>= sget
       (lambda (orig)
         (>> (modify (f:curry + 3))
             (spure (+ x orig))))))

(time
 (funcall (state-run
          (>>* (state-3-add-x 10)
               (fmap #'1+ sget)
               (state-3-add-x 10)))
          2))
(time
 (funcall (state-run
           (*>* (state-3-add-x 10)
                (fmap #'1+ sget)
                (state-3-add-x 10)))
          2))


(>>= (state-3-add-x 10)
   (spure (lambda (x) (+ 3 x))))

(defun run-add-3 (x y)
  (funcall (state-run (state-3-add-x x)) y))