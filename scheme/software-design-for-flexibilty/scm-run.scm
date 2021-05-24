;; loading

(define (load-source)
  (load "src/2-arity.scm")
  ;; library relies on arity
  (load "src/2-library.scm")
  (load "src/2-regex.scm"))

(define (load-test)
  (load-source)
  (load "test/2-library.scm"))

(define (disassemble proc)
  (fluid-let ((load-debugging-info-on-demand? #t))
    (let ((compiled (compile-procedure proc)))
      (compiler:disassemble compiled))))
