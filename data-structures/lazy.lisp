(defpackage #:lazy
  (:documentation "Creates lazy primitives in CL")
  (:use #:cl)
  (:export :lazy
           :lazy-p
           :delay
           :force
           :force-and-update
           :force-eval))

(in-package :lazy)

(defstruct lazy (expr (lambda () #'identity) :type function))

(defmacro delay (n) `(make-lazy :expr (lambda () ,n)))

(defun force (n) (funcall (lazy-expr n)))

(defmacro force-and-update (x)
  `(setf ,x (force ,x)))

(defmacro force-eval (x)
  `(if (lazy-p ,x)
       (setf ,x (force ,x))
       ,x))
