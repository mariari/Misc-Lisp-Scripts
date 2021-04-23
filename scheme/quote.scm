;; taken from
;; http://okmij.org/ftp/Scheme/quote-as-macro.txt


;; First we need unhygenic macros
(define-syntax define-macro
   (syntax-rules ()
     ((define-macro (name . args) body ...)
      (define-syntax name
        (rsc-macro-transformer
          (let ((transformer (lambda args body ...)))
            (lambda (exp env)
              (apply transformer (cdr exp)))))))))

(define-macro (my-quote body)
  (let ((s-list (string->symbol "list"))
	(s-cons (string->symbol "cons"))
	(s-vector (string->symbol "vector"))
	(s-my-quote (string->symbol "my-quote"))
	(s-string->symbol (string->symbol "string->symbol")))
    (cond
     ((null? body) (list s-list)) ; Expansion: (list)
     ((list? body) (cons s-list   ; expansion: (list (my-quote el1) ...)
			 (map (lambda (el) (list s-my-quote el))
			      body)))
     ((pair? body) (list s-cons
			 (list s-my-quote (car body))
			 (list s-my-quote (cdr body))))
     ((vector? body) (cons s-vector ; expansion: (vector (my-quote el1) ...)
			   (map (lambda (el) (list s-my-quote el))
				(vector->list body))))
     ((symbol? body) (list s-string->symbol (symbol->string body)))
     (else body))))


; The following statements print the expected results:

(display (append (my-quote (1 2 3)) (my-quote ()) (my-quote (4))))
(newline)

(write (my-quote "string"))
(newline)

(write (my-quote (1 . 2)))
(newline)

(write (my-quote (1 2 a 4 . 5)))
(newline)

(write (my-quote #(1 #\2 xyz)))
(newline)

(write (vector-length (my-quote #(1 2 3))))
(newline)

; the quote inside my-quote loses its meaning, as it should
(write (my-quote (1 #(1 2) '4 xyz #t)))
(newline)

(write (symbol? (my-quote abcdef)))
(newline)
