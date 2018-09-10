(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package 'lcons))


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

