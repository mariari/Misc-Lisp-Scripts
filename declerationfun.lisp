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
