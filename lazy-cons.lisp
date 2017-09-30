;; I don't have the environment, so I'll just use lambda to delay evaluation!
;; lambda the ultimate lazy


(defmacro delay (n) `(lambda () ,n))

(defun force (n) (funcall n))

(defun terms (n)
  (cons (/ 1 (expt n 2))
        (lambda () (terms (1+ n)))))

(defun scons (x xs)
  (cons (delay x)
        (delay xs)))


(defun sterms (n)
  (scons (/ 1 (expt n 2))
         (terms (1+ n))))


(defun scar (lis) (force (car lis)))

(defun scdr (lis) (force (cdr lis)))


;;  this model is inferior as we would have to recompute the values
;; lets try again

(defmacro scons% (x xs)
  `(cons ',x ,xs))


(scons% (+ 3 4) (scons% (+ 1 3) nil))

(defun scar% (n) (eval (car n)))


;;  failure again lets go back to scar and scdr
