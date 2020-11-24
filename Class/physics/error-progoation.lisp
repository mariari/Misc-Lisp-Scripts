(defun error-prop (v i &optional  (error-const-v 0.0001) (error-const-i 0.001))
  (sqrt
   (+ (* (expt error-const-v 2)  (expt (/ v (expt i 2)) 2))
      (* (expt error-const-i 2)   (expt (/ 1 i) 2)))))
