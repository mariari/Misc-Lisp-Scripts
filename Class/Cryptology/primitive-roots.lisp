

(defun primitive-roots-of (prime)
  (labels ((primitive? (num &optional (new-num num) (times 1))
             (cond ((= times (1- prime))
                    t)
                   ((= new-num 1)
                    nil)
                   (t
                    (primitive? num (mod (* new-num num) prime) (1+ times))))))
    (remove-if-not #'primitive?
                   (list:range 1 (1- prime)))))


(defun roots-less-than-100 (n)
  (remove-if-not (lambda (prime) (member n (primitive-roots-of prime)))
                 (list 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)))
