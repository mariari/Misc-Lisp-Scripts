(defpackage #:string
  (:documentation "Provides extra functionality functions for strings")
  (:use #:common-lisp)
  (:export :split-on))

(in-package :string)


(defun split-on (delim seq)
  (let ((length (length delim)))
    (labels ((rec (seq cps)
               (let ((num (search delim seq)))
                 (if num
                     (rec (subseq seq (+ length num))
                          (lambda (x)
                            (funcall cps (list* (subseq seq 0 num)
                                                (subseq seq num (+ length num))
                                                x))))
                     (funcall cps (list seq))))))
      (rec seq #'identity))))
