(defmacro macro-map-set (val list)
  `(cons (setf ,(cadr list) ,val) (if (cdr ,list)
                                      (macro-map-set ,val (list ,@(cddr list)))
                                      nil)))

(defparameter *a* (make-hash-table))
(setf (gethash 1 *a*) 3)
(setf (gethash 2 *a*) 4)
(setf (gethash 3 *a*) 5)

(defparameter *y* *a*)
(defparameter *z* nil)
(defparameter *l* nil)

(mapc (lambda (x) (setf x *a*)) (list *z* *l*))
(mapc (lambda (x y)  (setf (gethash x *a*) y)) '(1 2 3) '(1 2 3))

(defun set-val (hash-old hash-new)
  (let ((n hash-old)
        (f hash-new))
    (setf n f)))

(let ((b 2))
  (defun incfb (b)
    (setf b (1+ b)))
  (defun getb () b))


(defmacro macro-map (lam list)
  `(cons (,lam (car ,list)) (if (cdr ,list)
                                (macro-map ,lam (cdr ,list))
                                nil)))
