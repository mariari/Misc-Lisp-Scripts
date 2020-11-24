(defun binomial-random-varariable (p &optional (n 1))
  (let ((acc 0))
    (dotimes (i n acc)
      (if (=  (random (/ 1 p)) 0)
          (incf acc)
          0))))

(defun prob-binomial-random-variable-past-x (p n x)
  (reduce #'+ (mapcar (lambda (x) (* (choose n x) (expt p x) (expt (- 1 p) (- n x)))) (range x 0))))

(defun prob-binomial-random-variable-at-x% (p n x)
  (- (prob-binomial-random-variable-past-x p n x) (prob-binomial-random-variable-past-x p n (1- x))))

(defun prob-binomial-random-variable-at-x (p n x)
  (* (choose n x) (expt p x) (expt (- 1 p) (- n x))))


(defun likelyhood-to-reach-z (n m z p q)
  (let* ((max-loop (cond ((and (< z (max n m)) (< z (min n m))) (1+ z))
                         ((< z (max n m)) z)
                         (t (- z (max m n)))))
         (i (min n z))
         (j (- z i))
         (acc 0))
    (dotimes (k max-loop acc)
      (setf acc (+ acc (* (prob-binomial-random-variable-at-x p n i)
                          (prob-binomial-random-variable-at-x q m j))))
      (decf i)
      (incf j))))

;; (mapcar (lambda (x) (likelyhood-to-reach-z 10 5 x 1/4 1/3)) (range 10 1))
