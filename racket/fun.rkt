#lang racket

(require racket/match)


(define (m-value-int xs)
  (let ((ops (hash '+ + '- - '* * '/ quotient '% remainder)))
    (match xs
      ((list a b op) ((hash-ref ops op) (m-value-int a) (m-value-int b)))
      ((list* _ _)   (error "unvalid operation"))
      (x             x))))
