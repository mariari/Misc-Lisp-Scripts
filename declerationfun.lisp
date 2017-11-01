;; (declaim (optimize (speed 3) (space 2) (safety 0) (debug 0) (compilation-speed 0)))

(locally (declare (optimize (speed 3) (space 2) (safety 0) (debug 0) (compilation-speed 0)))

  (defun the-test (x)
    (declare (type fixnum x))
    (let ((y (+ x 10)))
      (declare (type fixnum y))
      (the fixnum y)))

  (defun the-not-test (x)
    (declare (type fixnum x))
    (let ((y (+ x 10)))
      (declare (type fixnum y))
      y))

  ;; the two above are =
  
  (defun the-not-no-y-test (x)
    (declare (type fixnum x))
    (+ 10 x))

  (defun the-not-no-y-test-no-declare (x)
    (+ 10 x))


  ;; (ftype (function (fixnum) fixnum) the-no-ftype-test)
  (defun the-no-ftype-test (x)
    (the fixnum (+ 10 x)))
  
  (declaim (ftype (function (fixnum) fixnum) test-ftype test-ftype-the))
  (defun test-ftype (x)
    (+ x 10))
  
  (defun test-ftype-the (x)
    (the fixnum (+ x 10)))

  )

(locally (declare (optimize (speed 3) (space 2) (safety 0) (debug 0) (compilation-speed 0)))

  (defun reverse% (lis)
    (labels ((rec (lis1 lis2)
               (if (null lis1)
                   lis2
                   (rec (cdr lis1)
                        (cons (car lis1) lis2)))))
      (rec lis '())))


  (defun reverse%%% (lis &optional (acc nil))
    (if (null lis)
        acc
        (reverse%%% (cdr lis) (cons (car lis) acc))))


  (defun reverse%% (lis)
    (reduce (lambda (x y) (cons y x)) lis :initial-value '()))
  
  (defun list-reverse (list)
    (do ((new-list ()))
        ((endp list) new-list)
      (push (pop list) new-list)))

  ;; not my range, this was made by lisp
  (defun range (x &optional (y 0) (z 1) &aux (a (make-array (ceiling (1+ (abs (- x y))) z)
                                                            :element-type (if (and (>= most-positive-fixnum x) (>= most-positive-fixnum y)) 'fixnum 'integer))))
    (declare (type unsigned-byte x y z))
    (setf (aref a 0) x)
    (prog* ((n 1) (z (if (< x y) z (- z))) (s (+ x z))) :begin
           (if (or (<= x s y)
                  (>= x s y))
               (setf (aref a n) s)
               (return-from range a))
           (incf n)
           (setf s (+ s z))
           (go :begin)))

  (declaim (ftype (function (fixnum &optional (or fixnum null) fixnum) (simple-array fixnum (*))) range-v))
  (defun range-v (first &optional (second) (step 1))
    "returns a range in a vector, much faster than range, but only supports fixnums"
    (flet ((compute (first second)
             (declare (type fixnum first second))
             (let ((vec (make-array (1+ (floor (abs (- second first)) step))
                                    :element-type 'fixnum)))
               (dotimes (i (length vec) vec)
                 (declare (type fixnum i))
                 (setf (aref vec i) (if (< first second)
                                        (+ first (the fixnum (* step i)))
                                        (- first (the fixnum (* step i)))))))))
      (declare (inline compute))
      (if second
          (compute first second)
          (compute 0     first)))))

(locally (declare (optimize (speed 3) (space 2) (safety 0) (debug 0) (compilation-speed 0)))
  (time (defparameter *x* (range 0 100000 1))))

;; (print (range-v 10))


(declaim (inline range range-v))





(let ((var 2))
  (defun fun1 (x)
    (+ x var))
    
  (defun fun2 (x)
    (+ x var)))


(defun reverse* (lis)
  
  
  (labels ((_reverse* (lis lis2)
             (if (null lis)
                 lis2
                 (_reverse* (cdr lis)
                            (cons (if (listp (car lis))
                                      (_reverse* (car lis) '())
                                      (car lis))
                                  lis2)))))
    (_reverse* lis '())))
