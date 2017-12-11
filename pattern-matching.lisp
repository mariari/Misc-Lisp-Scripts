(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:fare-quasiquote-readtable
                  :fare-quasiquote
                  :trivia.quasiquote
                  :trivia
                  :let-over-lambda)))

(defpackage #:match
  ;; (:nicknames)
  (:use #:trivia)
  (:use #:common-lisp))
(in-package :match)


(named-readtables:in-readtable :fare-quasiquote)

(match '(1 . 2) ((cons a b) (+ a b)))

(match '(a b c d) (`(a b ,@x) x))


(match (list 1 2 3 4 5)
  ((guard (list* a _)
          (= a 1)) a))


(print (equal (match '(1 2 3 4 5 6 7)
                ((list* 1 2 _ a b) (reduce #'+ (cons a b))))

              (match '(1 2 3 4 5 6 7)
                (`(1 2 ,_ ,a ,@b) (reduce #'+ (cons a b))))))

(match '(1 2 3 4)
  (`(1 ,x ,@y) (list x y)))(match '(1 2 3 4)
  (`(1 ,x ,@y) (list x y)))
;; (in-optimizer :trivia)
;; (in-optimizer :balland2006)
(match '(a b c d) 
  ((list* _ _ x) x))
;; broken?
(match '((a . 1) (b . 2) (c . 3))
  (`(assoc b ,x) x))

;; works
(match '((a . 1) (b . 2) (c . 3))
  ((assoc 'b x) x))

(defpattern string-type-specifier (length)
   `(or (list 'string ,length)
        (and 'string (<> ,length '*))))

;; (match (list 1 2 3 4 5)
;;   ((guard (list* a b)
;;           (evenp a)
;;           a)))

(match (list 2 5)
  ((guard (list x y)     ; subpattern
          (= 10 (* x y)) ; test-form
          (- x y) (satisfies evenp)) ; generator1, subpattern1
   t))

(match (list 2 5)
  ((guard (list x y)     ; subpattern
          (= 10 (* x y)) ; test-form
          (oddp (- x y)) (satisfies null)) ; generator1, subpattern1
   t))

(match1 '(1 2)
  ((guard1 temp (consp temp)
           (car temp) (guard1 a (oddp a))
           (cadr temp) (guard1 b (evenp b)))
   (vector a b)))


(match (list 1 (list 1 2) 2)
           ((cons _ (list (or (list a 3) (list 1 a)) 2)) a))