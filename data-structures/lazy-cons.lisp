;; lambda the ultimate lazy

;; This struct allows me to tell whether the structure is lazy or an actual function the user wants!

;;; Core Function********************************************************************************************************
(defstruct lazy (expr (lambda () #'identity) :type function))

(defmacro delay (n) `(make-lazy :expr (lambda () ,n)))

(defun force (n) (funcall (lazy-expr n)))

(defmacro force-and-update (x)
  `(setf ,x (force ,x)))

(defmacro force-eval (x)
  `(if (lazy-p ,x)
       (setf ,x (force ,x))
       ,x))

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

;;; Conversion functions*************************************************************************************************

(defun make-strict% (lis &optional (orig lis))
  "makes a lazy list strict"
  (if lis
      (progn (scar lis)
             (make-strict% (scdr lis) orig))
      orig))

(defun make-strict (lis)
  "makes a lazy list strict"
  (do ((i lis (scdr i))
       (j lis (scar i)))
      ((null i) lis)))

;; Works kinda... primes still take up needless amount of memory
(defun make-strict-gc (lis)
  "manually calls the gc, which is needed
   for some lazy streams to be properly evaluated"
  (do ((i lis (scdr i))
       (j lis (scar i)))
      ((null i) lis)
    (when (> (sb-kernel::dynamic-usage) 512000000) ; ensures no error will be signaled
      (gc :full t))))                              ; but it might not GC enough and the function will take forever


(defun lazyfy (lis)
  "makes a strict list lazy"
  (if lis
      (scons (car lis) (apply #'lazyfy (cdr lis)))
      '()))

;;; Useful Functions*****************************************************************************************************

(defmacro slist (&body arguments)
  "creates a lazy list from an argument list"
  (if arguments
      `(scons ,(car arguments) (slist ,@(cdr arguments)))
      '()))

(defun sreverse (lis &optional acc)
  "reverses a lazy list, note this is O(n)"
  (let ((first (scar lis)))
    (if first
        (sreverse (scdr lis) (scons first acc))
        acc)))

(defun sappend (xs ys)
  "appends one list (lazy or otherwise) to another."
  (if (null xs)
      ys
      (scons (scar xs) (sappend (scdr xs) ys))))

(defun sfilter (pred lis)
  "filters a stream lazily"
  (cond ((null lis)                '())
        ((funcall pred (scar lis)) (scons (scar lis) (sfilter pred (scdr lis))))
        (t                         (sfilter pred (scdr lis))))) ; not lazy

(defun smap (f lis)
  "maps f onto a lazy list, note that f is a function, and is thus strict"
  (if (null lis)
      '()
      (scons (funcall f (scar lis)) (smap f (scdr lis)))))

(defun sfoldl (f acc lis)
  "a strict version of foldl that does all its arguments first"
  (if lis
      (sfoldl f (funcall f acc (scar lis)) (scdr lis))
      acc))

(defun sfoldr-bad (f acc lis)
  "a version of that fold that tries to be lazy, but fails since macros aren't first class"
  (labels ((rec (f acc lis)
             (if lis
                 (delay (funcall f (scar lis) (rec f acc (scdr lis))))
                 acc)))
    (scdr (cons nil (rec f acc lis)))))

;; fix by making macros being lazy
;; this is a macro
;; fix this macro later
;; (defmacro sfoldr (f acc lis)
;;   "a macro version of foldr that can take macros and functions as f, and can thus be lazy or strict"
;;   (if lis
;;       `(,f (scar ,lis)
;;            (sfoldr ,f ,acc (scdr ,lis)))
;;       acc))


(defun stake (num lis)
  "take NUM number of things from a lazy stream"
  (if (zerop num)
      '()
      (scons (scar lis) (stake (1- num) (scdr lis)))))

(defun sdrop (num lis)
  "drops NUM number of things form the lazy stream"
  (if (zerop num)
      lis
      (sdrop (1- num) (scdr lis))))

;;; functions built ontop of laziness************************************************************************************

(defun sterms (n)
  (scons (/ 1 (expt n 2))
         (sterms (1+ n))))

(defun sieve (lis)
  (scons (scar lis)
         (sieve (sfilter (lambda (x) (/= 0 (mod x (scar lis))))
                         (scdr lis)))))

(defun primes ()
  (labels ((naturals-from (n)
             (scons n (naturals-from (1+ n)))))
    (sieve (naturals-from 2))))

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


;;; Deprecated**********************************************************************************************************
(declaim (ftype (function (function list &optional list) *) sfilter-bad))
(defun sfilter-bad (pred lis &optional acc)
  "don't use this version of filter, it is strict"
  (cond ((null lis)                (sreverse acc))
        ((funcall pred (scar lis)) (sfilter-bad pred (scdr lis) (scons (scar lis) acc)))
        (t                         (sfilter-bad pred (scdr lis) acc))))

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
