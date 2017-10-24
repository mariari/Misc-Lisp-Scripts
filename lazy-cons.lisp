;; I don't have the environment, so I'll just use lambda to delay evaluation!
;; lambda the ultimate lazy

(defmacro delay (n) `(lambda () ,n))

(defun force (n) (funcall n))


(defmacro scons (x xs)
  `(cons ,x (delay ,xs)))

(defun sterms (n)
  (scons (/ 1 (expt n 2))
         (sterms (1+ n))))

(defun scar (lis) (car lis))

(defun scdr (lis)
  (if (functionp (cdr lis))
      (setf (cdr lis) (force (cdr lis)))
      (cdr lis)))


(defparameter *test2* (cons-stream 1 (cons-stream 2 nil)))

(scdr (scdr *test2*))

(defparameter *sterms* (sterms 1))

;; time to add a match pattern to this




;; First attempt at lazyness********************************************************************************************
(defun terms (n)
  (cons (/ 1 (expt n 2))
        (lambda () (terms (1+ n)))))

;; A better way to define it!,
(defun scons% (x xs)
  (cons (delay x) (delay xs)))

(defun sterms% (n)
  (scons (/ 1 (expt n 2))
         (sterms% (1+ n))))

(defun scar% (lis) (force (car lis)))

(defun scdr% (lis) (force (cdr lis)))


;;; Second Attempt at lazyness that failed******************************************************************************

(defmacro scons%% (x xs)
  `(cons ',x ,xs))


(defun scar%% (n) (eval (car n)))

;; (scons% (+ 3 4) (scons% (+ 1 3) nil))