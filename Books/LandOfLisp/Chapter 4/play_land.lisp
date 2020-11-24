;; flet doesn't work in this scenario since g can't see f 
(labels ((f (n)
           (+ n 10))
         (g (n)
           (- n (f 2))))
  (g (f 5)))

(cdadar '( (ant (mouse bat) nat) (cat dog) (ant nii)))
