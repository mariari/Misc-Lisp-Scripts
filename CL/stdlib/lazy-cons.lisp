(defpackage #:lcons
  (:documentation "Provides the lazy cons data structure")
  (:use #:cl #:lazy)
  (:export :scons
           :scar
           :scdr
           :make-strict
           :lazyfy
           :slist
           :sreverse
           :sappend
           :sfilter
           :smap
           :sfoldl
           :stake
           :sdrop))

(in-package :lcons)

;; lambda the ultimate lazy

;;; Core Function********************************************************************************************************

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

(defun make-strict (lis)
  "makes a lazy list strict"
  (do ((i lis (scdr i))
       (j lis (scar i)))
      ((null i) lis)))


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
