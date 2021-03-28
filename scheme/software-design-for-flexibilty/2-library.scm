(load "2-arity.scm")

;;; ------------------------------------------------------------
;;; Main Functions
;;; ------------------------------------------------------------

(define (compose . funs)
  (let* ((g (last (cons identity funs)))
         (g-min (get-arity-min g))
         (g-max (get-arity-max g)))
    (restrict-arity
     (fold-right (lambda (f calls)
                   (lambda args
                     (call-with-values (lambda () (apply calls args))
                       f)))
                 values
                 funs)
     g-min g-max)))

(define (parallel-combine h f g)
  (compose h (parallel-apply f g)))

(define (spread-combine h f g)
  (compose h (spread-apply f g)))

(define ((iterate n) f)
  (if (zero? n)
      identity
      (compose f ((iterate (-1+ n)) f))))

(define (list->alist xs)
  (let rec ((lst xs) (acc '()))
    (if (null? lst)
        (reverse acc)
        (rec (cddr lst)
             (cons (cons (car lst) (cadr lst)) acc)))))

(define (foldl-with-index fun initial first)
  (car
   (fold-left (lambda (acc x)
                (cons (fun (car acc) (cdr acc) x) (1+ (cdr acc))))
              (cons initial 0)
              first)))

(define (list-remove lst index)
  (let lp ((lst lst) (index index))
    (if (zero? index)
        (cdr lst)
        (cons (car lst) (lp (cdr lst) (-1+ index))))))

;; -----------------------------------
;; Exported Permutation functions    -
;; -----------------------------------

(define ((discard-argument . is) f)
  (let ((m ((bump-arity-min (apply max is))
            (add-arity 1 (arity f)))))
    (restrict-arity (compose f (discard-manipulation is))
                    (min-arity m)
                    (max-arity m))))

(define (((curry-argument . is) . args) f)
  (let ((manipulated (apply (curry-manipulation is) args)))
    (lambda arguments-left
      (apply (compose f manipulated)
             arguments-left))))

(define (permute-arguments . permspec)
  (let ((permute (permute-manipulation permspec)))
    (lambda (f)
      (let ((n           (arity f))
            (perm-length (length permspec)))
        (assert (in-range perm-length (min-arity n) (max-arity n)))
        (restrict-arity (compose f permute)
                        perm-length)))))

;;; ------------------------------------------------------------
;;; Helpers
;;; ------------------------------------------------------------

;; -----------------------------
;; Updated manipulation functions
;; -----------------------------

(define (discard-manipulation is)
  (map (lambda (i) (assert (exact-nonnegative-integer? i))) is)
  (let ((discard (make-discard is)))
    (define (the-combination . args)
      (apply values (discard args)))
    (restrict-arity the-combination (1+ (apply max is)) +inf.0)))

(define (curry-manipulation spec)
  (map (lambda (i) (assert (exact-nonnegative-integer? i))) spec)

  (define (the-combination . args)
    (define (realize-term . arguments-left)
      (let ((curry ((make-curry spec) arguments-left)))
        (apply values (curry args))))

    (restrict-arity realize-term (length spec)))
  (restrict-arity the-combination (1+ (apply max spec)) +inf.0))


(define (discard-argument i)
  (assert (exact-nonnegative-integer? i))
  (lambda (f)
    (let ((m ((bump-arity-min i) (add-arity 1 (arity f)))))
      (define (the-combination . args)
        (apply f (list-remove args i)))
      (assert-arity the-combination m))))

; --------------------------------------
;; Various apply and permutation applies
;; -------------------------------------

(define (parallel-apply f g)
  (let* ((arity (parallel-arity f g)))
    (define (the-combination . args)
      (let-values ((fv (apply f args))
                   (gv (apply g args)))
        (apply values (append fv gv))))
    (assert-arity the-combination arity)))

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

;; Creates the multiple discard
;; just realized if I sort then I can remove arguments in reverse order
;; however that would be slower ☹
(define (make-discard discard-spec)
  (let ((tree (alist->rb-tree (map (lambda (x) (cons x #t)) discard-spec) = <)))
    (define (remove-index acc index x)
      (if (rb-tree/lookup tree index #f)
          acc
          (cons x acc)))
    (define (the-discard lst)
      (reverse! (foldl-with-index remove-index '() lst)))
    the-discard))

;; the curried arguments are what is left
;; the left fold was oddly worse, right fold would have been fine though

;; make-curry takes a spec indexed by 0 of where to placed the
;; args-not-curried in the arguments-already-curried.
;; Example
;; 1 (user) => (((make-curry (list 1 2)) (list 10 20)) (list 0 3 4 5))
;; ;Value: (0 10 20 3 4 5)
(define ((make-curry curry-spec) args-not-curried)
  (assert (= (length args-not-curried) (length curry-spec)))
  (let ((tree (alist->rb-tree
               (map (lambda (x y) (cons x y)) curry-spec args-not-curried)
               = <)))
    (define (the-added-curry arguments-already-curried)
      (let rec ((lst arguments-already-curried) (acc '()) (index 0))
        (let ((ele (rb-tree/lookup tree index #f)))
          (cond (ele         (rec lst (cons ele acc) (1+ index)))
                ((null? lst) (reverse! acc))
                (else        (rec (cdr lst) (cons (car lst) acc) (1+ index)))))))
    the-added-curry))

;; -----------------------------------
;; Arity Checker helpers             -
;; -----------------------------------

(define (parallel-arity f g)
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
