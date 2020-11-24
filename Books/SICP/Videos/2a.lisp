(defun sum-int% (a b &optional (acc 0))
  (if (> a b)
      acc
      (sum-int% (1+ a) b (+ a acc))))

(defun sum-sq% (a b &optional (acc 0))
  (if (> a b)
      0
      (sum-sq% (1+ a) b (+ acc (expt a 2)))))


(defun sum (term a next b &optional (acc 0))
  (if (> a b)
      acc
      (sum term (funcall next a)
           next b (+ (funcall term a) acc))))

(defun sum-int (a b)
  (sum #'identity a #'1+ b))

(defun sum-sq (a b)
  (sum (lambda (x) (expt x 2)) a #'1+ b))

(defun pi-sum (a b)
  (sum (lambda (i) (/ 1 (* i (+ 1 2))))
       a
       (lambda (i) (+ i 4))
       b))

(defun average (x y)
  (/ (+ x y) 2))


(defun our-sqrt (x)
  (coerce (fixed-point
           (average-damp (lambda (y) (/ x y)))
           1) 'float))

(defun average-damp (f)
  (lambda (x) (average (funcall f x) x)))

(defun average-damp% (f)
  (lambda (x) (hl:=<< (average) (funcall f) x)))

(defun fixed-point (f start)
  (let ((tolerance 0.00001))
    (labels ((close-enuf? (u v)
               (> tolerance (abs (- u v))))
             (iter (old new)
               (if (close-enuf? old new)
                   new
                   (iter new (funcall f new)))))
      (iter start (funcall f start)))))

(defparameter *dx* .00001)

(defun deriv (f)
  (lambda (x) (/ (- (funcall f (+ x *dx*))
               (funcall f x))
            *dx*)))
