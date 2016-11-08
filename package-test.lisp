(ql:quickload "optima")
(ql:quickload "let-over-lambda")

(defpackage #:fun-times
  (:nicknames #:fun :times)
  (:use #:optima)
  (:use #:let-over-lambda)
  (:use #:common-lisp))

(in-package :fun-times)
(match 1 (1 1))

;; (define-macro (wrap . expr)
;;     (let ((arg (gens)))
;;       `(lambda (,arg) (,@expr ,arg))))

(defmacro curry (fn . args)
  "Creates a partially applied function that takes 1 argument"
  (let ((arg (gensym)))
    `(lambda (,arg) (,fn ,@args ,arg))))

(funcall (wrap + 3 4) 2)
(mapcar (wrap + 3) '(1 2 3 4))
