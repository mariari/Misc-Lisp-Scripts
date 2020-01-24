(defpackage #:list
  (:documentation "Provides extra functionality functions for lists")
  (:use #:common-lisp)
  (:export :range
           :split-on
           :ncircular
           :circular
           :foldr-b
           :foldl-b
           :scanl
           :scanl1
           :scanr
           :scanr1
           :nrotate-left))

(in-package :list)

(defun range (first &optional (second nil) (step 1))
  (macrolet ((for (second word first)
               `(loop :for x :from ,second ,word ,first :by step
                   collect x)))
    (cond ((and second (> second first)) (for first to     second))
          (second                        (for first downto second))
          (t                             (for 0     to     first)))))


(defun is-prefix? (xs ys)
  "checks if the first list is a prefix of the second list"
  (cond ((null xs)                 t)
        ((equal (car xs) (car ys)) (is-prefix? (cdr xs) (cdr ys)))
        (t                         nil)))

(defun split-on (delim seq)
  "splits a sequence based on the deliminator
   The deliminator can either be a list or a single element"
  (let* ((delim     (if (listp delim) delim (list delim)))
         (len-delim (length delim)))
    (labels ((rec (seq not-matched acc-list)
               (cond
                 ((null seq)             (reverse (cons (reverse not-matched) acc-list)))
                 ((is-prefix? delim seq) (rec (subseq seq len-delim)
                                              '()
                                              (list* (reverse not-matched) delim acc-list)))
                 (t                      (rec (cdr seq)
                                              (cons (car seq) not-matched)
                                              acc-list)))))
      (rec seq '() '()))))

;; From Lisp
(defun nrotate-left (list &aux (last (last list)))
  "Rotate elements of list to the left, circularly."
  (declare (type list list))
  (if list
      (setf (cdr last) list
            list (cddr last)
            (cddr last) nil))
  list)


(defun ncircular (list)
  (when list
    (setf (cdr (last list)) list)))

(defun circular (list)
  (ncircular (copy-list list)))

(defstruct break-cps run)

(defun foldr-b (f z xs &optional (cps #'identity) (break (lambda (x) (make-break-cps :run x))))
  (if (null xs)
      (funcall cps z)
      (foldr-b f z
               (cdr xs)
               (lambda (acc)
                 (let ((curr (funcall f (car xs) acc break)))
                   (if (break-cps-p curr)
                       (break-cps-run curr)
                       (funcall cps curr))))
               break)))

(defun foldl-b (f z xs &optional (break (lambda (x) (make-break-cps :run x))))
  (cond ((break-cps-p z) (break-cps-run z))
        ((null xs)       z)
        (t               (foldl-b f (funcall f z (car xs) break) (cdr xs) break))))


(defun scanl (f acc xs)
  "scanl is similar to reduce, but returns a list of successive reduced values from the left

   scanl f z (x1 x2 ...) == (z (z `f` x1) (f (z `f` x1) x2) ...)
   Note that

   (last (scanl f z xs)) == (reduce f xs :initial-value z)"
  (labels ((rec (acc xs)
             (if (null xs)
                 acc
                 (rec (cons (funcall f (car acc) (car xs)) acc) (cdr xs)))))
    (reverse (rec (list acc) xs))))

(defun scanl1 (f xs)
  "scanl1 is a variant of scanl that has no starting value argument so uses the first element"
  (and xs (scanl f (car xs) (cdr xs))))

(defun scanr (f acc xs)
  "scanr is the right-to-left dual of scanl. Note that

   (car (scanr f z xs)) == (reduce f xs :initial-value z :from-end t)"
  (labels ((rec (xs cps)
             (if (null xs)
                 (funcall cps (list acc))
                 (rec (cdr xs)
                      (lambda (acc)
                        (funcall cps (cons (funcall f (car xs) (car acc))
                                           acc)))))))
    (rec xs #'identity)))

(defun scanr1 (f xs)
  "scanr1 is a variant of scanr that has no starting value argument so uses the last value"
  (labels ((rec (xs cps)
             (if (cdr xs)
                 (rec (cdr xs)
                      (lambda (acc)
                        (funcall cps (cons (funcall f (car xs) (car acc))
                                           acc))))
                 (funcall cps (list (car xs))))))
    (and xs (rec xs #'identity))))

(declaim (ftype (function (fixnum list) list) group))
(defun group (n xs)
  "groups a list into lists of size n"
  (labels ((rec (i xs acc)
             (cond ((null xs) (reverse (on-car #'reverse acc)))
                   ((zerop i) (rec (1- n)
                                   (cdr xs)
                                   (cons (list (car xs))
                                         (on-car #'reverse acc))))
                   (t         (rec (1- i)
                                   (cdr xs)
                                   (on-car (lambda (as) (cons (car xs) as)) acc))))))
    (rec n xs '())))

(declaim (ftype (function (function list) list) on-car))
(defun on-car (f xs)
  "applies a function f onto the car of a list"
  (cons (funcall f (car xs)) (cdr xs)))
