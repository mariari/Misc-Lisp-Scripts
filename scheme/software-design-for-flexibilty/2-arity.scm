(define (construct-arity min max) (cons min max))

;; in-range checks if ele member [min ... max]
(define (in-range ele min max)
  (>= max ele min))

(define (procedure-arity-max% proc)
  (cdr proc))

(define (procedure-arity-min% proc)
  (car proc))

(define (get-arity-max proc)
  (or
   (procedure-arity-max% (or (hash-table-ref/default arity-table proc #f)
                             (procedure-arity proc))))
  +inf.0)

(define (get-arity-min proc)
  (or
   (procedure-arity-min% (or (hash-table-ref/default arity-table proc #f)
                             (procedure-arity proc)))
   0))

(define (arity proc)
  (let ((arr (or (hash-table-ref/default arity-table proc #f)
                 (procedure-arity proc))))
    (construct-arity (or (car arr) 0) (or (cdr arr) +inf.0))))

(define (restrict-arity proc nargs #!optional max)
  (if (eq? max #!default)
      (hash-table-set! arity-table proc (cons nargs nargs))
      (hash-table-set! arity-table proc (cons nargs max)))
  proc)

;; redefining just to keep it backward comparable
(define (get-arity proc)
  (let ((a (or (hash-table-ref/default arity-table proc #f)
               (procedure-arity proc))))
    (assert (eqv? (procedure-arity-min a)
                  (procedure-arity-max a)))
    (procedure-arity-min a)))


;; personal function
(define (add-arity i range)
  (construct-arity (+ i (min-arity range))
                   (+ i (max-arity range))))

(define (bump-arity-min i arity)
  (assert (>= (max-arity arity) i))
  (construct-arity (max (min-arity arity) i)
                   (max-arity arity)))
