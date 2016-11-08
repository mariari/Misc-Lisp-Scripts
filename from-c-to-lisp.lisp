;; Translated from my K&R c 
(defun binsearch% (num arr &optional (length (length arr)))
  "Finds num in arr, and returns the location where it is found, the array has to be sorted"
  (let* ((high (1- length))
         (low 0)
         (mid (floor (/ high 2))))
    (loop while (<= low high)
       do (progn (if (< num (elt arr mid))
                     (setf high (1- mid))
                     (setf low  (1+ mid)))
                 (setf mid (floor (/ (+ low high) 2)))))

    (if (= (elt arr mid) num) mid -1)))

;; Done the lisp way 
(defun binsearch (num arr &optional (length (length arr)))
  "Finds num in arr, and returns the location where it is found, the array has to be sorted"  
  (labels ((f (&optional (high (1- length)) (low 0) (mid (floor (/ (+ low high) 2))))
             (if (> low high)
                 mid
                 (if (< num (elt arr mid))
                     (f (1- mid) low)
                     (f high (1+ mid)))))) 
    (let ((mid (f)))
      (if (= (elt arr mid) num) mid -1))))
