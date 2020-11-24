
(defun zip (&rest lists)
  (apply #'mapcar #'list lists))

(defun dec->bin (n &optional (cps #'identity))
  "Converts a number to a list contaning its binary representation,
   LSB comes first"
  (multiple-value-bind (r m) (floor n 2)
    (if (= n 0)
        (funcall cps nil)
        (dec->bin r (lambda (x) (funcall cps (cons m x)))))))

(defun dec->bin% (n)
  (LOOP FOR B = n THEN (ash B -1) UNTIL (zerop B) COLLECT (if (evenp B) 0 1)))


(defun iterate-n-acc (n fun start &optional acc)
  (reverse (if (< n 1)
               acc
               (iterate-n-acc (- n 1) fun
                              (funcall fun start)
                              (cons start acc)))))

(defun iterate-n-cps (n fun elem &optional (cps #'identity))
  (if (= n 0)
      (funcall cps nil)
      (iterate-n-cps (- n 1) fun
                     (funcall fun elem)
                     (f:compose cps (f:curry cons elem)))))


(defun iterate-n (n fun elem)
  (iterate-n-cps n fun elem))


(defun powmod (b e p)
  "Solves the congruence x ≡ b^e mod p"
  (reduce (lambda (acc x)
            (if (= 0 (car x)) acc (*mod (cadr x) acc p)))
          (zip (dec->bin e)
               (iterate-n (1+ (ceiling (log e 2))) (lambda (x) (*mod x x p)) b))
          :initial-value 1))

(defun *mod (a b p)
  "Multiply a and b mod p."
  (mod (* a b) p))
