
(defun egcd (x y)
  (labels
      ((update-points (prev-value curr-value num-div)
         (mapcar (lambda (curr prev) (+ (* -1 curr num-div) prev))
                 curr-value prev-value))

       (recurse (x y prev-value curr-value)
         (multiple-value-bind (mult remainder) (floor x y)
           (if (zerop remainder)
               (list curr-value
                     (update-points prev-value curr-value mult))
               (recurse y
                        remainder
                        curr-value
                        (update-points prev-value curr-value mult))))))
    (recurse (max x y) (min x y) '(1 0) '(0 1))))

;; (egcd 47 11)
;; ((4 -17) (-11 47))

;; since (-11 47) is equal to the input
;; 11^-1 ≡ 30 mod 47

(defun dec->bin (n &optional (cps #'identity))
  "Converts a number to a list contaning its binary representation,
   LSB comes first"
  (multiple-value-bind (r m) (floor n 2)
    (if (zerop n)
        (funcall cps nil)
        (dec->bin r (lambda (x) (funcall cps (cons m x)))))))

(defun inverse-fast-power-algo (a p)
  (labels ((recurse (acc bs)
             (cond ((null bs)        acc)
                   ((zerop (car bs)) (recurse (mod (expt acc 2) p) (cdr bs)))
                   (t                (recurse (mod (* a (mod (expt acc 2) p)) p) (cdr bs))))))
    (recurse 1 (reverse (dec->bin (- p 2))))))
