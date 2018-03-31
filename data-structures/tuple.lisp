

(defpackage #:tuple
  (:nicknames #:t)
  (:documentation "provides the tuple data structure and some functions")
  (:use #:common-lisp)
  (:export :tup
           :fst :snd
           :tup-p))

(in-package :tuple)

(defstruct (tup (:conc-name nil)
                (:constructor tup (fst snd))
                (:print-function
                 (lambda (p s k)
                   (declare (ignore k))
                   (format s "[~A ~A]"
                           (fst p)
                           (snd p)))))
  fst snd)
