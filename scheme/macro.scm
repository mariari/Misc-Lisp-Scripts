(define-syntax our-let
  (syntax-rules ()
    ;; _ stand in for our-let
    ((_ ((x e)) body)
     ((lambda (x) body) e))))

(define-syntax expand
  (syntax-rules ()
    ((_ form)
     (unsyntax
      (syntax (quote form)
              (nearest-repl/environment))))))

(our-let ((x (+ 2 3)))
  (* x x))

(define-syntax our-let
  (syntax-rules ()
    ;; ... is + in bnf (0 or more)
    ;; 1 or more is one expression and then another one with ...
    ((_ ((x e) ...) body body* ...)
     ((lambda (x ...) body body* ...) e ...))))


(define-syntax our-let*
  (syntax-rules ()
    ((_ () body body* ...)
     (let () body body* ...))
    ((_ ((x e) (x* e*) ...) body body* ...)
     (let ((x e))
       (our-let* ((x* e*) ...)
         body body* ...)))))

(define-syntax our-and
  (syntax-rules ()
    ((_)          #t)
    ((_ e)        e)
    ((_ e e* ...) (if e (our-and e* ...) #f))))

(define-syntax our-or
  (syntax-rules ()
    ((_)          #t)
    ;; important for optimziation on non chez implementations
    ((_ e)        e)
    ((_ e e* ...) (let ((x e))
                    (if x x (our-and e* ...))))))

;; with middle case
;; (expand (if  (zero? 5) 3 (our-or (foo 3))))
;; ;Value: (if (zero? 5) 3 (foo 3))

;; without middle case
;; (expand (if  (zero? 5) 3 (our-or (foo 3))))
;; ;Value: (if (zero? 5) 3 (let ((.x.1-0 (foo 3))) (if .x.1-0 .x.1-0 #t)))

 (define-syntax our-cond
   (rsc-macro-transformer
    (lambda (exp env)
      (fold-right
       (lambda (x ys)
         `(if ,(car x)
              (begin ,@(cdr x))
              ,ys))
       '()
       (cdr exp)))))

(pp (expand
     (our-cond ((even? 3) 4)
               (#t 3))))
