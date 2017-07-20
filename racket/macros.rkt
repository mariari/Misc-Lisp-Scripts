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
   [((list* a b))           b]
   [(a)                     '()]))


;; Note we should also make a version that does not save the hash-table between calls
;; as this can eventually eat our memory!
(define-macro (defmemo-old fn-list . body)
  "defines a memoized function, NOTE: it does support ((f a) b)
   syntax and only memorizes the answer for a and NOΤ b"
  (let ((name  (fn-name fn-list))
        (table (gensym))
        (memo-b (gensym)))
    `(splicing-let ((,table (make-hash)))
       (define ,fn-list
         ,@(map (aλ (x)
                    (match x
                      [(list* (? (λ (x) (equal? name x)) fn) b)
                       `(let ((,memo-b (list ,@(map self b))))   ; we evaluate b because we
                          (unless (hash-has-key? ,table ,memo-b) ; want the answers and not
                            (hash-set! ,table                    ; unevaluated form 
                                       ,memo-b                   ; we also let to remove multiple eval
                                       (apply ,name ,memo-b)))
                          (hash-ref ,table
                                    ,memo-b))]
                      [(list* a b) (map self x)]
                      [a a]))
                body)))))

;; Better expansion and allows for curry to work
(define-macro (defmemo fn-list . body)
  "defines a memoized function, NOTE: it does support ((f a) b)
   syntax and only memorizes the answer for a and NOΤ b"
  (let ((name  (fn-name fn-list))
        (table (gensym))
        (memo-b (gensym)))
    `(splicing-let ((,table (make-hash)))
       (define ,fn-list
         (let ((,memo-b (list ,@(args-list fn-list))))
           (unless (hash-has-key? ,table ,memo-b)
               (hash-set! ,table
                          ,memo-b
                          ,@body))
           (hash-ref ,table
                     ,memo-b))))))

(defmemo (mfib n)
  (if (< n 1)
      1
      (+ (mfib (- n 1)) (mfib (- n 2)))))

(defmemo-old (mfib% n)
  (if (< n 1)
      1
      (+ (mfib% (- n 1)) (mfib% (- n 2)))))

(defmemo (fact n)
  (if (>= 0 n)
      1
      (* n (fact (- n 1)))))
(define (fib n)
  (if (< n 1)
      1
      (+ (fib (- n 1)) (fib (- n 2)))))
