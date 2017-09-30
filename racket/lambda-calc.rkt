#lang racket

(((λ (x) (x x))
  (λ (y) y))
 '((1 2 3 4)
   (1 2 3 4)))


(((λ (x) (x x))
  (λ (y)
     (if (equal? 'zero y)
         0
       y)))
 'zero)




;;  Defined the normal way
(define (factorial x)
  (if (= 1 x)
      1
      (* x (factorial (- x 1)))))

;; define the fun way
(define fact-y-comb
  ((λ (x) (x x))
   (λ (f)
      (λ (x)
         (if (= 1 x)
             1
             (* x ((f f) (- x 1))))))))

(((λ (x) (x x))
  (λ (f)
     (λ (x)
        (if (= 1 x)
            1
            (* x ((f f) (- x 1)))))))
 4)


;; TCO style
(define (factorial-tco x (tco 1))
  (if (= 1 x)
      tco
      (factorial-tco (- x 1) (* x tco))))


;; TCO y-comb
(define fact-tco-y-comb
  ((λ (x) (x x))
   (λ (f)
      (λ (x)
         (λ (t)
            (if (= 1 x)
                t
                (((f f) (- x 1)) (* t x))))))))

((fact-tco-y-comb 4) 1)

((((λ (x) (x x))
   (λ (f)
     (λ (x)
         (λ (t)
            (if (= 1 x)
                t
                (((f f) (- x 1)) (* t x)))))))
  4)
 1)


;; cps style
(define ∘ compose)

(define (factorial-cps x (cps identity))
  (if (= 1 x)
      (cps x)
      (factorial-cps (- x 1) (∘ cps (curry * x)))))


((∘ (∘ identity (curry * 3)) (curry * 2)) 1)

(define fact-cps-y-comb
  ((λ (x) (x x))
   (λ (f)
     (λ (x)
       (λ (k)
         (if (= 1 x)
             (k x)
             (((f f) (- x 1)) (λ (y) (k (* x y))))))))))


((fact-cps-y-comb 4) (λ (x) x))


((((λ (x) (x x))
   (λ (f)
     (λ (x)
       (λ (k)
         (if (= 1 x)
             (k x)
             (((f f) (- x 1)) (λ (y) (k (* x y)))))))))
  4)
 (λ (x) x))

;; evaluation of this function does not fault :(
(define Y
  (λ (f)
    ((λ (x) (f (x x)))
     (λ (x) (f (x x))))))



;; (Y (λ (f)
;;       (λ (x)
;;          (if (= 1 x)
;;              1
;;            (* x (f (- x 1)))))))


;; goes on forever
;; ((λ (x) (x x)) (λ (x) (x x)))
