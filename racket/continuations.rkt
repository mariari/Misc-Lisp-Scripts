#lang racket

(define (fib n k)
  (if (< n 2)
      (k 1)
      (fib (- n 1)
           (λ (fib-of-n-1)
             (fib (- n 2)
                  (λ (fib-of-n-2)
                    (k (+ fib-of-n-1 fib-of-n-2))))))))