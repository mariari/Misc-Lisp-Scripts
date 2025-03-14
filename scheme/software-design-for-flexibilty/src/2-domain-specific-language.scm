;;;; ------------------------------------------------------------
;;;; 2.0 Domain Specific Languages
;;;; ------------------------------------------------------------

;;;;; ------------------------------------------------------------
;;;;; 2.1 Function Combinators
;;;;; ------------------------------------------------------------

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

(define (restrict-arity proc nargs)
  (hash-table-set! arity-table proc nargs)
  proc)

(define (procedure-arity-max% proc)
  (cdr proc))

(define (procedure-arity-min% proc)
  (car proc))

(define (get-arity proc)
  (or (hash-table-ref/default arity-table proc #f)
      (let ((a (procedure-arity proc))) ; arity not in table
        (assert (eqv? (procedure-arity-min% a)
                      (procedure-arity-max% a)))
        (procedure-arity-min% a))))

(define arity-table (make-key-weak-eqv-hash-table))

;; ahh stack based calling conventions
((spread-combine list
                 (lambda (x y)   (list 'foo x y))
                 (lambda (u v w) (list 'bar u v w)))
 'a 'b 'c 'd 'e)

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


;; ----------------------------------------------
;; Helpers
;; ----------------------------------------------

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

;; ----------------------------------------------
;; main functionality
;; ----------------------------------------------


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
 'a 'b 'c 'd)

(arity
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

;; sadly assert-arity can't be made into a composition
(define (assert-arity f range)
  (let ((min (min-arity range)) (max (max-arity range)))
    (define (the-combination . args)
      (assert (in-range (length args) min max))
      (apply f args))
    (restrict-arity the-combination min max)))

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

(define (parallel-apply f g)
  (let* ((arity (parallel-arity f g)))
    (define (the-combination . args)
      (let-values ((fv (apply f args))
                   (gv (apply g args)))
        (apply values (append fv gv))))
    (assert-arity the-combination arity)))

(define (parallel-combine h f g)
  (compose h (parallel-apply f g)))

(arity (parallel-apply list list))

((parallel-combine list
                   (lambda args (values (cons 'foo args)))
                   (lambda (a b c d) (values (list 'bar a b c d))))
 'a 'b 'c 'd)

;;; ------------------------------------------------------------
;;; a small library
;;; ------------------------------------------------------------

;; personal function
(define (add-arity i range)
  (construct-arity (+ i (min-arity range))
                   (+ i (max-arity range))))

(define ((bump-arity-min i) arity)
  (assert (>= (max-arity arity) i))
  (construct-arity (max (min-arity arity) i)
                   (max-arity arity)))

;; further for discard argument we'll want to bump the min, if we can
(define (bump-arity-min i)
  (compose (parallel-combine construct-arity
                             (lambda (a) (max (min-arity a) i))
                             max-arity)
           (lambda (arity) (assert (>= (max-arity arity) i)) arity)))

(define (discard-argument i)
  (assert (exact-nonnegative-integer? i))
  (lambda (f)
    (let ((m ((bump-arity-min i) (add-arity 1 (arity f)))))
      (define (the-combination . args)
        (apply f (list-remove args i)))
      (assert-arity the-combination m))))

(define (list-remove lst index)
  (let lp ((lst lst) (index index))
    (if (zero? index)
        (cdr lst)
        (cons (car lst) (lp (cdr lst) (-1+ index))))))

(((discard-argument 2)
  (lambda (x y z) (list 'foo x y z)))
 'a 'b 'c 'd)

;; our assertion tries to keep it 1 below the range itself
;; however it's not clear per say, unlike the books
(define (((curry-argument i) . args) f)
  (let ((arity (add-arity -1 (arity f))))
    (assert (in-range (length args) (min-arity arity) (max-arity arity)))
    (lambda (x)
      (apply f (list-insert args i x)))))

(define (list-insert lst index value)
  (let lp ((lst lst) (index index))
    (if (zero? index)
        (cons value lst)
        (cons (car lst) (lp (cdr lst) (-1+ index))))))

((((curry-argument 2) 'a 'b 'c)
  (lambda (x y z w) (list 'foo x y z w)))
 'd)

(define (permute-arguments . permspec)
  (let ((permute (make-permutation permspec)))
    (lambda (f)
      (define (the-combination . args)
        (apply f (permute args)))
      (let ((n           (arity f))
            (perm-length (length permspec)))
        (assert (in-range perm-length (min-arity n) (max-arity n)))
        (restrict-arity the-combination perm-length)))))

(define (make-permutation permspec)
  (define (the-permuter lst)
    (let ((vec (list->vector lst)))
      (map (lambda (p) (vector-ref vec p))
           permspec)))
  the-permuter)

(((permute-arguments 1 2 0 3)
  (lambda args (cons 'foo args)))
 'a 'b 'c 'd)

;;;; ------------------------------------------------------------
;;;; 2.1.2 Combinators and body plans
;;;; ------------------------------------------------------------

;;; ------------------------------------------------------------
;;; 2.4 As compositions?
;;; ------------------------------------------------------------

;; Make them into a composition between argument manipulations and the
;; procedure

(define (discard-manipulation i)
  (assert (exact-nonnegative-integer? i))
  (define (the-combination . args)
    (apply values (list-remove args i)))
  (restrict-arity the-combination (1+ i) +inf.0))

(define (curry-manipulation i)
  (assert (exact-nonnegative-integer? i))
  (define (the-combination . args)
    (lambda (x)
      (apply values (list-insert args i x))))
  (restrict-arity the-combination (1+ i) +inf.0))

(define (permute-manipulation permspec)
  (let* ((permute (make-permutation permspec))
         (len     (length permspec)))
    (define (the-combination . args)
      (apply values (permute args)))
    (restrict-arity the-combination len)))

(define ((discard-argument i) f)
  (let ((m ((bump-arity-min i)
            (add-arity 1 (arity f)))))
    (restrict-arity (compose f (discard-manipulation i))
                    (min-arity m)
                    (max-arity m))))

(define (((curry-argument i) . args) f)
  (compose f (apply (curry-manipulation i) args)))

(define (permute-arguments . permspec)
  (let ((permute (permute-manipulation permspec)))
    (lambda (f)
      (let ((n           (arity f))
            (perm-length (length permspec)))
        (assert (in-range perm-length (min-arity n) (max-arity n)))
        (restrict-arity (compose f permute)
                        perm-length)))))

;;; ------------------------------------------------------------
;;; 2.4 Useful combinators
;;; ------------------------------------------------------------

(define (foldl-with-index fun initial first)
  (car
   (fold-left (lambda (acc x)
                (cons (fun (car acc) (cdr acc) x) (1+ (cdr acc))))
              (cons initial 0)
              first)))

;; ---------------------------------
;; makes the various permutation lists
;; ---------------------------------

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

;; ---------------------
;; The Main functionality
;; ---------------------

(define (((curry-argument . is) . args) f)
  (let ((manipulated (apply (curry-manipulation is) args)))
    (lambda arguments-left
      (apply (compose f manipulated)
             arguments-left))))

(define ((discard-argument . is) f)
  (let ((m ((bump-arity-min (apply max is))
            (add-arity 1 (arity f)))))
    (restrict-arity (compose f (discard-manipulation is))
                    (min-arity m)
                    (max-arity m))))

(((make-curry (list 1 2)) (list 2 3)) (list 0 3 4 5))

(((discard-argument 2 1)
  (lambda (x y) (list 'foo x y)))
 'a 'b 'c 'd)

((((curry-argument 2) 'a 'b 'c)
  (lambda (x y z w) (list 'foo x y z w)))
 'd)

((((curry-argument 2 1) 'a 'b 'c)
  (lambda (a b c d e) (list 'foo a b c d e)))
 2 1)


(define (list->alist xs)
  (let rec ((lst xs) (acc '()))
    (if (null? lst)
        (reverse acc)
        (rec (cddr lst)
             (cons (cons (car lst) (cadr lst)) acc)))))

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

((compose (lambda args
            (cons 'foo args))
          (lambda (x y)
            (values (list 'faz x) (list 'fat y)))
          (lambda (x)
            (values (list 'bar x) (list 'baz x))))
 'z)

;;;;; ------------------------------------------------------------
;;;;; 2.2 Regular expressions
;;;;; ------------------------------------------------------------

;; wait... wait does he just make parser combinators

;; matches any character expect newline
(define (r:dot) ".")

;; matches only the beginning of a line
(define (r:bol) "^")

;; matches only the end of a line
(define (r:eol) "$")

;; matches the string
(define (r:quote string)
  (r:seq
   (list->string
    (append-map (lambda (char)
                  (if (memv char chars-needing-quoting)
                      (list #\\ char)
                      (list char)))
                (string->list string)))))

;; matches one character that is in the string
(define (r:char-from string)
  (case (string-length string)
    ((0) (r:seq))
    ((1) (r:quote string))
    (else
     (bracket string
              (lambda (members)
                (if (lset= eqv? '(#\- #\^) members)
                    '(#\- #\^)
                    (quote-bracketed-contents members)))))))

;; matches one character that is not in the string
(define (r:char-not-from string)
  (bracket string
           (lambda (members)
             (cons #\^ (quote-bracketed-contents members)))))

;; compounds

;; >>= in parser combinators
;; this pattern matches each argument pattern in sequence from left to
;; right
(define (r:seq . exprs)
  ;; ( ) here are made to isolate the environment, but we must escape
  ;; them, as regex treats them oddly
  (string-append "\\(" (apply string-append exprs) "\\)"))

;; <|>/choice in parser combinators
;; this pattern tries each argument pattern from left to right, until
;; one of these alternative matches, if none match then this pattern
;; does not match
(define (r:alt . expers)
  (if (pair? expers)
      (apply r:seq
             (cons (car expers)
                   (append-map (lambda (expr)
                                 (list "\\|" expr))
                               (cdr expers))))
      (r:seq)))

;; many in parser combinators
;; this pattern tries to match the argument pattern a minimum of min
;; times but no more than maximum of max times. if max is given as #f
;; then no maximum is specified. If max equals min, the given pattern
;; must match exactly that many times
(define (r:repeat min max expr)
  (apply r:seq
         (append (make-list min expr)
                 (cond ((not max)     (list expr "*"))
                       ((= max min) '())
                       ; entails the last check but is faster
                       (else
                        (make-list (- max min)
                                   (r:alt expr "")))))))

(define (write-bourne-shell-grep-command expr filename)
  (display (bourne-shell-grep-command-string expr filename)))

(define (bourne-shell-grep-command-string expr filename)
  (string-append "grep -e "
                 (bourne-shell-quote-string expr)
                 " "
                 filename))

(define (bourne-shell-quote-string string)
  (list->string
   (append (list #\')
           (append-map (lambda (char)
                         (if (char=? char #\')
                             (list #\' #\\ char #\')
                             (list char)))
                       (string->list string))
           (list #\'))))

;; ---------------------------------
;; Regex helper
;; ---------------------------------

(define chars-needing-quoting
  '(#\. #\[ #\\ #\^ #\$ #\*))


(define (bracket string procedure)
  (list->string
   (append '(#\[)
           (procedure (string->list string))
           '(#\]))))

(define (quote-bracketed-contents members)
  (define (optional char)
    (if (memv char members) (list char) '()))
  (append (optional #\])
          (remove
           (lambda (c)
             (memv c chars-needing-quoting-in-brackets))
           members)
          (optional #\^)
          (optional #\-)))

(define chars-needing-quoting-in-brackets
  '(#\] #\^ #\-))

(r:seq (r:quote "a") (r:dot) (r:quote "c"))

(write-bourne-shell-grep-command (r:alt (r:quote "cat") (r:quote "dog")) "fi")

;; regex a hack, I understand the lesson here is how to deal with
;; poorly designed systems, however Ι will only do 2 exercises

;;; ------------------------------------------------------------
;;; 2.6 Adding * and + to regex
;;; ------------------------------------------------------------

(define r:+
  (((curry-argument 2) 1 #f) r:repeat))

(define r:*
  (((curry-argument 2) 0 #f) r:repeat))

;;; ------------------------------------------------------------
;;; 2.7 Adding * and + to regex
;;; ------------------------------------------------------------

;; many in parser combinators
;; this pattern tries to match the argument pattern a minimum of min
;; times but no more than maximum of max times. if max is given as #f
;; then no maximum is specified. If max equals min, the given pattern
;; must match exactly that many times

;; Uses the proper {} extension
(define (r:repeat min max expr)
  (apply r:seq
         (if (and (= 0 min) (not max))
             (list expr "*")
             (list expr
                   "\\{"
                   (number->string min)
                   ","
                   (if (not max) "" (number->string max))
                   "\\}"))))

;;;; ------------------------------------------------------------
;;;; 2.3 Wrappers
;;;; ------------------------------------------------------------

(define (gas-law-volume pressure temperature amount)
  (/ (* amount gas-constant temperature) pressure))

(define gas-constant 8.3144621)         ; J/(k*mol)

(define (sphere-radius volume)
  (expt (/ volume (* 4/3 pi)) 1/3))

(define pi (* 4 (atan 1 1)))

(define fahrenheit-to-celsius
  (make-unit-conversion (lambda (f) (* 5/9 (- f 32)))
                        (lambda (c) (+ (* c 9/5) 32))))

(define celsius-to-kelvin
  (let ((zero-celsius 273.15)) ; kelvins
    (make-unit-conversion (lambda (c) (+ c zero-celsius))
                          (lambda (k) (- k zero-celsius)))))

;; conversion numbers gotten online
(define inch-to-meter
  (make-unit-conversion (lambda (i) (* i 0.0254))
                        (lambda (m) (* m 39.3701))))

;; conversion numbers gotten online
(define pound-to-newton
  (make-unit-conversion (lambda (p) (* p 4.44822))
                        (lambda (n) (* n 0.224809))))


;; never given the actual definition for make-unit-conversion but seems obvious

(define (make-unit-conversion forward backwards)
  (lambda (x #!optional go-backwards)
    (if (eq? go-backwards #!default)
        (forward x)
        (backwards x))))

(define (unit:invert f)
  (define (flip x)
    (if (eq? x #!default) #t #!default))
  (define (new-f x #!optional flipped)
    (f x (flip flipped)))
  new-f)


;; Defining this later so we have a chance to compile

(define psi-to-nsm
  (compose pound-to-newton
           (unit:invert inch-to-meter)
           (unit:invert inch-to-meter)))

;; need to know radius of a sphere by 1 more of an ideal gas at
;; 68F and 14.7 psi

((unit:invert inch-to-meter)
 (sphere-radius
  (gas-law-volume (psi-to-nsm 14.7)
                  ((compose celsius-to-kelvin fahrenheit-to-celsius) 68)
                  1)))
;; 7.049626759112956

;; unit conversion is hard to read, the physics is separated and so is the geometry


;;; ------------------------------------------------------------
;;; 2.3.1 Specialization wrappers
;;; ------------------------------------------------------------

(define make-specialized-gas-law-volume
  (unit-specializer
   gas-law-volume
   '(expt meter 3)                      ; output (volume)
   '(/ newton (expt meter 2))           ; pressure
   'kelvin                              ; temperature
   'mole))                              ; amount

(define conventional-gas-law-volume
  (make-specialized-gas-law-volume
   '(expt inch 3)                       ; output (volume)
   '(/ pound (expt inch 2))             ; pressure
   'fahrenheit                          ; temperature
   'mole))                              ; amount

(sphere-radius (conventional-gas-law-volume 14.7 68 1))
