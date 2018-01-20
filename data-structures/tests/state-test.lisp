(use-package 'state)
(use-package 'g)

(funcall (state-run
          (fmap #'1+ (>> (>>= sget (f:compose #'put #'1+))
                         (spure 8))))
         2)
;; [9 3]