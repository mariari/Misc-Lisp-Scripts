#lang racket

(require compatibility/defmacro)

(require macro-debugger/stepper)

(require macro-debugger/expand)

(require racket/match)

(require racket/splicing)

(module a racket
        (define-syntax (aλ exp)
          (syntax-case exp ()
                       [(aλ args body ...)
                        (with-syntax ([self (datum->syntax exp 'self)])
                                     #'(letrec ([self (λ args body ...)])
                                         self))]))
        (provide aλ))

(require (for-syntax racket/match 'a))

(begin-for-syntax
  (define/match (fn-name fn-list)
    [((list* (list* a _) _)) (fn-name a)]
    [((list* a _))           a]
    [(a)                     a]))

(begin-for-syntax
 (define/match (args-list fn-list)
   [((list* (list* a _) _)) (args-list (car fn-list))]
   [((list* _ b))           (map (match-lambda ((list* a _) a)
                                               (a           a)) b)] ; removes the given optional arg
   [(_)                     null]))


;; Note we should also make a version that does not save the hash-table between calls
;; as this can eventually eat our memory!
(define-macro (defmemo-old fn-list . body)
  "defines a memoized function, NOTE: it does support ((f a) b)
   syntax and only memorizes the answer for a and NOΤ b"
  (let* ((name     (fn-name fn-list))
         (table    (gensym))
         (memo-b   (gensym))
         (eq-name? (λ (x) (equal? name x))))
    `(splicing-let ((,table (make-hash)))
       (define ,fn-list
         ,@(map (aλ (x)
                    (match x                                                               ; we evaluate b
                      [(list* (? eq-name? fn) b) `(let ((,memo-b (list ,@(map self b))))   ; because we want the
                                                    (unless (hash-has-key? ,table ,memo-b) ; answers of the expr
                                                      (hash-set! ,table                    ; not the uneval form
                                                                 ,memo-b                   ; also let to remove
                                                                 (apply ,name ,memo-b)))   ; multiple eval
                                                    (hash-ref ,table
                                                              ,memo-b))]
                      [(list* a b) (map self x)]
                      [a a]))
                body)))))

;; Better expansion and allows for curry to work
;; this does not work with optional arguments yet!
(define-macro (defmemo fn-list . body)
  "defines a memoized function, NOTE: it does support ((f a) b)
   syntax and only memorizes the answer for a and NOΤ b"
  (let ((table (gensym))
        (memo-b (gensym)))
    `(splicing-let ((,table (make-hash)))
       (define ,fn-list
         (hash-ref! ,table
                    (list ,@(args-list fn-list))
                    (λ () ,@body))))))

;; would just call defmmeo inside, but racket has a weird macro system that makes me
;; have to define a package to call
;; this does not work with optional arguments yet!
(define-macro (defmemol fn-list . body)
  "defines a memoized function, NOTE: it does support ((f a) b)
   syntax and only memorizes the answer for a and NOΤ b
   does not save hash between computation calls"
  `(define ,fn-list
     (defmemo ,fn-list ,@body)
     ,fn-list))

(defmemol (mfib n)
  (if (< n 1)
      1
      (+ (mfib (- n 1)) (mfib (- n 2)))))

;; note that the CPS here is quite slow
;; 3132 cpu time for 1000, while the normal is 3
;; 1000x times slower at 1000 elements
(defmemo (mfib-cps n [cps identity])
  (if (< n 1)
      (cps 1)
      (mfib-cps (- n 1) (compose cps (curry + (mfib-cps (- n 2)))))))

;; 1680 cpu time
(defmemo (mfib-tco n [tco 1])
  (if (< n 1)
      tco
      (mfib-tco (- n 1) (+ tco (mfib-tco (- n 2))))))


(defmemo-old (mfib% n)
  (if (< n 1)
      1
      (+ (mfib% (- n 1)) (mfib% (- n 2)))))

(define (fib n)
  (if (< n 1)
      1
      (+ (fib (- n 1)) (fib (- n 2)))))


;; HYGENIC VERSION BY ITTALLICS

(require racket/match)

(define-syntax-rule (def-memo (fn arg ...) body ...)
  (define fn
    (let ([memo (make-hash)])
      (λ (arg ...)
        (let ([args (list arg ...)])
          (hash-ref! memo
                     args
                     (λ () body ...)))))))

(define-syntax-rule (def-memo-local (fn arg ...) body ...)
  (define (fn arg ...)
    (def-memo (fn arg ...) body ...)
    (fn arg ...)))

;; (def-memo-local (fib n)
;;   (if (< n 2) n
;;       (+ (fib (- n 1))
;;          (fib (- n 2)))))

(map mfib (range 15))


(defmemo (x n)
  (cond ((= n 1) 1)
        ((= n 2) 2)
        ((< n 0) 0)
        (else (/ (+ (x (- n 1))
                    (x (- n 2)))
                 2))))
(defmemol (s n)
  (if (= n 1)
      1
      (sqrt (+ 1 (s (- n 1))))))
;;  converges to phi
