
;; lambda the ultimate lazy

;; This struct allows me to tell whether the structure is lazy or an actual function the user wants!
(defstruct lazy (expr (lambda () #'identity) :type function))

(defmacro delay (n) `(make-lazy :expr (lambda () ,n)))

(defun force (n) (funcall (lazy-expr n)))

(defmacro scons (x xs)
  `(cons (delay ,x) (delay ,xs)))

(defun scar (lis)
  (let ((first (car lis)))
    (if (lazy-p first)
        (setf (car lis) (force first))
        first)))

(defun scdr (lis)
  (let ((rest (cdr lis)))
    (if (lazy-p rest)
        (setf (cdr lis) (force rest))
        rest)))

(defun sterms (n)
  (scons (/ 1 (expt n 2))
         (sterms (1+ n))))

(defparameter *test2* (scons 1 (scons 2 nil)))

(scdr (scdr *test2*))

(defparameter *sterms* (sterms 1))

;; the first 100 values of sterm are evaluated
;; this function is just iterate in disguise and an end point
(reduce (lambda (acc _)
          (declare (ignore _))
          (scar acc)
          (scdr acc))
        (loop for i from 0 to 100 collect i) ; should just put range into my auto load file :(
        :initial-value *sterms*)

;; time to add a match pattern to this

;; My third way to do this, the problem is that it forces the head every time*******************************************
(defmacro delay% (n) `(lambda () ,n))

(defun force% (n) (funcall n))

(defmacro scons%%% (x xs)
  `(cons ,x (delay ,xs)))

(defun scar%%% (lis)
  (car lis))

(defun scdr%%% (lis)
  (if (functionp (cdr lis))
      (setf (cdr lis) (force (cdr lis)))
      (cdr lis)))

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