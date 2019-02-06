(defpackage #:vector
  (:documentation "Provides extra functionality functions for vectors")
  (:use #:common-lisp)
  (:export :range))

(in-package :vector)

(declaim (ftype (function (fixnum &optional (or fixnum null) fixnum)
                          (simple-array fixnum (*)))
                range))
(defun range (first &optional (second) (step 1))
  "returns a range in a vector, much faster than range, but only supports fixnums"
  (flet ((compute (first second)
           (let ((vec      (make-array (1+ (floor (abs (- second first)) step))
                                       :element-type 'fixnum))
                 (new-step (if (> first second) (- step) step)))
             (dotimes (i (length vec) vec)
               (setf (aref vec i)
                     (+ first (the fixnum (* new-step i))))))))
    (declare (inline compute))
    (if second
        (compute first second)
        (compute 0     first))))

(declaim (inline range))
