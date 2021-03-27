
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
        (assert (eqv? (procedure-arity-min% a)
                      (procedure-arity-max% a)))
        (procedure-arity-min% a))))

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


(define (spread-combine h f g)
  (let* ((f-min (get-arity-min f))
         (g-max (get-arity-max g))
         (g-min (get-arity-min g))
         (t-min (+ f-min g-min))
         (t-max (+ f-min g-max)))
    (define (the-combination . args)
      (assert (in-range (length args) t-min t-max))
      (h (apply f (list-head args f-min))
         (apply g (list-tail args f-min))))
    (restrict-arity the-combination t-min t-max)))

((spread-combine list
                 (lambda (x y) (list 'foo x y))
                 list)
 'a 'b 'c 'd 'e)

(get-arity-list
 (spread-combine list
                 (lambda (x y) (list 'foo x y))
                 list))


(define (compose f g)
  (let* ((g-min (get-arity-min g))
         (g-max (get-arity-max g)))
    (define (the-composition . args)
      (assert (in-range (length args) g-min g-max))
      (f (apply g args)))
    (restrict-arity the-composition g-min g-max)))


;; ----------------------------------------------
;; Helpers
;; ----------------------------------------------

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
  (or (hash-table-ref/default arity-table proc #f)
      (procedure-arity proc)))

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

(define (get-arity-list proc)
  (or (hash-table-ref/default arity-table proc #f)
      (procedure-arity proc)))

;;; ------------------------------------------------------------
;;; On wards
;;; ------------------------------------------------------------


;; for this section all my changes for the better checked size
;; applies, so code is different from the book

;; since we redefine functions a lot, we could generalize the checking
;; functions, but Ι have decided not to out of laziness, and to try to
;; stay close to the book

;; all this min max stuff is annoying I'd rather just define
;; arithmetic on it, to be less annoying
(define (spread-apply f g)
  (let* ((f-min (get-arity-min f))
         (g-max (get-arity-max g))
         (g-min (get-arity-min g))
         (t-min (+ f-min g-min))
         (t-max (+ f-min g-max)))
    (define (the-combination . args)
      (assert (assert (in-range (length args) t-min t-max)))
      (values (apply f (list-head args f-min))
              (apply g (list-tail args f-min))))
    (restrict-arity the-combination t-min t-max)))

(define (spread-combine h f g)
  (compose h (spread-apply f g)))

(define (compose f g)
  (let* ((g-min (get-arity-min g))
         (g-max (get-arity-max g)))
    (define (the-composition . args)
      (assert (in-range (length args) g-min g-max))
      (call-with-values (lambda () (apply g args))
        f))
    (restrict-arity the-composition g-min g-max)))

;; same behavior as before
((spread-combine list
                 (lambda (x y) (list 'foo x y))
                 list)
 'a 'b 'c 'd 'e)

((compose (lambda (a b)
            (list 'foo a b))
          (lambda (x)
            (values (list 'bar x) (list 'baz x))))
 'z)

(define (spread-apply f g)
  (let* ((f-min (get-arity-min f))
         (g-max (get-arity-max g))
         (g-min (get-arity-min g))
         (t-min (+ f-min g-min))
         (t-max (+ f-min g-max)))
    (define (the-combination . args)
      (assert (in-range (length args) t-min t-max))
      (let-values ((fv (apply f (list-head args f-min)))
                   (gv (apply g (list-tail args f-min))))
        (apply values (append fv gv))))
    (restrict-arity the-combination t-min t-max)))

((spread-combine list
                 (lambda (x y #!optional opt) (values x y))
                 (lambda (u v w #!optional z) (values w v u z)))
 'a 'b 'c 'd 'e 'f)

;;; ------------------------------------------------------------
;;; 2.3 a quickie
;;; ------------------------------------------------------------

;; in the spirit of the book we'll make our own combinators
(define (construct-arity min max) (cons min max))
(define (min-arity arity) (car arity))
(define (max-arity arity) (cdr arity))

;; sadly assert-arity can't be made itno a composition
(define (assert-arity range f)
  (let ((min (min-arity range)) (max (max-arity range)))
    (define (the-combination . args)
      (assert (in-range (length args) min max))
      (apply f args))
    (restrict-arity the-combination min max)))

(define (parallel-check f g)
  (let* ((f-min (get-arity-min f))
         (f-max (get-arity-max f))
         (g-min (get-arity-min g))
         (g-max (get-arity-max g))
         (t-min (max f-min g-min))
         (t-max (min f-max g-max)))
    (assert (and (in-range t-min g-min g-max)
                 (in-range t-max g-min g-max)
                 (in-range t-min f-min f-max)
                 (in-range t-max f-min f-max)))
    (construct-arity t-min t-max)))

(define (parallel-apply f g)
  (let* ((arity (parallel-check f g)))
    (define (the-combination . args)
      (let-values ((fv (apply f args))
                   (gv (apply g args)))
        (apply values (append fv gv))))
    (assert-arity arity the-combination)))

(define (parallel-combine h f g)
  (compose h (parallel-apply f g)))

(arity (parallel-apply list list))

((parallel-combine list
                   (lambda args (values (cons 'foo args)))
                   (lambda (a b c d) (values (list 'bar a b c d))))
 'a 'b 'c 'd)

;;; ------------------------------------------------------------
;;; 2.4 a small library
;;; ------------------------------------------------------------
