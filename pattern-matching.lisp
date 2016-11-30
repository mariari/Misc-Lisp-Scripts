(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:fare-quasiquote-readtable
                  :trivia
                  :let-over-lambda)))

(defpackage #:match
  ;; (:nicknames)
  (:use #:trivia)
  (:use #:let-over-lambda)
  (:use #:common-lisp))
(in-package :match)

(named-readtables:in-readtable :fare-quasiquote)

(match '(1 . 2)
  ((cons a b) (+ a b)))



(match '(1 2 3 4 5 6 7)
  ((list* 1 2 _ a b) (reduce #'+ (cons a b))))

(match '(1 2 3 4 5 6 7)
  (`(1 2 _ ,a ,@b) (reduce #'+ (cons a b))))


(match '(a b c d) 
  (`(a b ,@x) x))

(match '((a . 1) (b . 2) (c . 3))
  ((assoc b x) x))
