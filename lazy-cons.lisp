
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

(defun sreverse (lis &optional acc)
  (let ((first (scar lis)))
    (if first
        (sreverse (scdr lis) (scons first acc))
        acc)))

(defun make-strict (lis)
  (if lis
      (cons (scar lis) (make-strict (scdr lis)))
      '()))

(defun make-strict% (lis)
  (if (scar lis)
      (cons (scar lis) (make-strict% (scdr lis)))
      '()))

(defun sappend (xs ys)
  (if (null xs)
      ys
      (scons (scar xs) (sappend (scdr xs) ys))))

(defun slist (&rest arguments)
  (if arguments
      (scons (car arguments) (apply #'slist (cdr arguments)))
      '()))

(declaim (ftype (function (function list &optional list) *) sfilter-bad))
(defun sfilter-bad (pred lis &optional acc)
  (cond ((null lis)                (sreverse acc))
        ((funcall pred (scar lis)) (sfilter-bad pred (scdr lis) (scons (scar lis) acc)))
        (t                         (sfilter-bad pred (scdr lis) acc))))

(defun sfilter (pred lis)
  (cond ((null lis)                '())
        ((funcall pred (scar lis)) (scons (scar lis) (sfilter pred (scdr lis))))
        (t                         (sfilter pred (scdr lis))))) ; not lazy

(defun sfoldl (f acc lis)
  "a strict version of foldl that does all its arguments first"
  (if lis
      (sfoldl f (funcall f acc (scar lis)) (scdr lis))
      acc))

;; fix by making macros being lazy
(defun sfoldr (f acc lis)
  "this can't be lazy since scons is a macro, thus we need to make things strict!"
  (labels ((rec (f acc lis)
             (if lis
                 (delay (funcall f (scar lis) (rec f acc (scdr lis))))
                 acc)))
    (scdr (cons nil (rec f acc lis)))))

(defun smap (f lis)
  "note since CL is strict, the f function will be strict"
  (if (null lis)
      '()
      (scons (funcall f (scar lis)) (smap f (scdr lis)))))

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
        (make-list 101) ; should just put range into my auto load file :(
        :initial-value *sterms*)

;; time to add a match pattern to this

(defun insert (x lis)
  (cond ((null lis)        (scons x '()))
        ((<= x (scar lis)) (scons x lis))
        (t                 (scons (scar lis) (insert x (scdr lis))))))


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