

(/ (random 100000000000) 100000000000)

(defun v ()
  (case (random 4)
    (0 1/5)
    (1 2/5)
    (2 3/5)
    (3 4/5)))
(let ((acc 0))
  (dotimes (i 1000)
    (incf acc (/ (reduce #'+ (mapcar (lambda (x) (max (v) (/ (random 100000000000) 100000000000))) (range 1e4 1))) 1e4)))
  (/ acc 1000))
