
;;;; ------------------------------------------------------------
;;;; 2.0 Domain Specific Languages
;;;; ------------------------------------------------------------

(define (compose f g)
  (lambda args
    (f (apply g args))))

((compose (lambda (x) `(foo ,x))
          (lambda (x) `(bar ,x)))
 'z)

;; redefine it with a name will come in handy
(define (compose f g)
  (define (the-composition . args)
    (f (apply g args)))
  the-composition)

(define ((iterate n) f)
  (if (zero? n)
      identity
      (compose f ((iterate (-1+ n)) f))))

(define (identity x) x)

(define (parallel-combine h f g)
  (define (the-combination . args)
    (h (apply f args) (apply g args)))
  the-combination)

((parallel-combine list
                   (lambda (x y z) (list 'foo x y z))
                   (lambda (u v w) (list 'bar u v w)))
 'a 'b 'c)

;; more natural in factor
(define (spread-combine h f g)
  (let ((n (get-arity f)))
    (define (the-combination . args)
      (h (apply f (list-head args n))
         (apply g (list-tail args n))))
    the-combination))

(define (spread-combine h f g)
  (let* ((n (get-arity f))
         (m (get-arity g))
         (t (+ n m)))
    (define (the-combination . args)
      (assert (= (length args) t))
      (h (apply f (list-head args n))
         (apply g (list-tail args n))))
    (restrict-arity the-combination t)))

;; ahh stack based calling conventions
((spread-combine list
                 (lambda (x y)   (list 'foo x y))
                 (lambda (u v w) (list 'bar u v w)))
 'a 'b 'c 'd 'e)


(define (restrict-arity proc nargs)
  (hash-table-set! arity-table proc nargs)
  proc)

(define (get-arity proc)
  (or (hash-table-ref/default arity-table proc #f)
      (let ((a (procedure-arity proc))) ; arity not in table
        (assert (eqv? (procedure-arity-min a)
                      (procedure-arity-max a)))
        (procedure-arity-min a))))

(define arity-table (make-key-weak-eqv-hash-table))

;; it seems I can screw with the calling conventions, as if I combine
;; them which effect goes off first?

;; Answer ∷ no, as scheme stores each as compound procedures and not symbols
((spread-combine list
                 (spread-combine cons
                                 (lambda () 'foo)
                                 (lambda (x y) '(x y)))
                 (spread-combine cons
                                 (lambda () 'bar)
                                 (lambda (u v w) '(u v w))))
 'a 'b 'c 'd 'e)

;;; ------------------------------------------------------------
;;; 2.1 Arity repair
;;; ------------------------------------------------------------

;; book is a bit inconsistent due to the h。(f,g) syntax showing h taking 2
;; Reading on it seems they fix that, so I'll just make f take 1 for now!
;; seems they don't bother, so we'll just not check for f being 1
(define (compose f g)
  (let* ((m (get-arity g)))
    (define (the-composition . args)
      (assert (= (length args) m))
      (f (apply g args)))
    (restrict-arity the-composition m)))

((compose 1+ (lambda (x y z) (* x y z))) 3 4 5)

(define (parallel-combine h f g)
  (let ((n (get-arity f)) (m (get-arity g)))
    (assert (= n m))
    (define (the-combination . args)
      (assert (= (length args) m))
      (h (apply f args) (apply g args)))
    (restrict-arity the-combination n)))

((parallel-combine list
                   (lambda (x y z) (list 'foo x y z))
                   (lambda (u v w) (list 'bar u v w)))
 'a 'b 'c)

;;; ------------------------------------------------------------
;;; 2.2 Arity Extension
;;; ------------------------------------------------------------

;; Probably easiest way to handle this is to either take the smallest
;; or most amount of arguments. The issue with taking the longest is
;; that it'll leave the second function with no arguments. So you
;; could have it be like

;; the first function that goes off eats the smallest, the last eats
;; between the smallest and biggest it can, and we try to ascertain
;; that is correct. or we could just eat the smallest for both.


(define (get-arity-max proc)
  (or
   (procedure-arity-max (or (hash-table-ref/default arity-table proc #f)
                            (procedure-arity proc))))
  +inf.0)

(define (get-arity-min proc)
  (procedure-arity-min (or (hash-table-ref/default arity-table proc #f)
                           (procedure-arity proc))))

(define (arity proc)
  (or (hash-table-ref/default arity-table proc #f)
      (procedure-arity proc)))

(define (restrict-arity proc nargs #!optional max)
  (if (eq? max #!default)
      (hash-table-set! arity-table proc (cons nargs nargs))
      (hash-table-set! arity-table proc (cons nargs max)))
  proc)

;; redefining just to keep it backward compatable
(define (get-arity proc)
  (let ((a (or (hash-table-ref/default arity-table proc #f)
               (procedure-arity proc))))
    (assert (eqv? (procedure-arity-min a)
                  (procedure-arity-max a)))
    (procedure-arity-min a)))

(define (get-arity-list proc)
  (or (hash-table-ref/default arity-table proc #f)
      (procedure-arity proc)))

(define (spread-combine h f g)
  (let* ((f-min (get-arity-min f))
         (g-max (get-arity-max g))
         (g-min (get-arity-min g))
         (t-min (+ f-min g-min))
         (t-max (+ f-min g-max)))
    (define (the-combination . args)
      (assert (>= (length args) t-min))
      (assert (<= (length args) t-max))
      (h (apply f (list-head args f-min))
         (apply g (list-tail args f-min))))
    (restrict-arity the-combination t-min t-max)))

(define (compose f g)
  (let* ((m (get-arity g)))
    (define (the-composition . args)
      (assert (= (length args) m))
      (f (apply g args)))
    (restrict-arity the-composition m)))

((spread-combine list
                 (lambda (x y) (list 'foo x y))
                 list)
 'a 'b 'c 'd 'e)

(get-arity-list
 (spread-combine list
                 (lambda (x y) (list 'foo x y))
                 list))
