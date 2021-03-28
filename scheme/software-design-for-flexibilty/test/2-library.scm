

((spread-combine list
                 (lambda (x y) (list 'foo x y))
                 list)
 'a 'b 'c 'd)

(arity
 (spread-combine list
                 (lambda (x y) (list 'foo x y))
                 list))

((spread-combine list
                 (lambda (x y) (list 'foo x y))
                 list)
 'a 'b 'c 'd 'e)

((compose (lambda (a b)
            (list 'foo a b))
          (lambda (x)
            (values (list 'bar x) (list 'baz x))))
 'z)

((spread-combine list
                 (lambda (x y #!optional opt) (values x y))
                 (lambda (u v w #!optional z) (values w v u z)))
 'a 'b 'c 'd 'e 'f)

((spread-combine list
                 (lambda (x y) (list 'foo x y))
                 list)
 'a 'b 'c 'd 'e)

((compose (lambda (a b)
            (list 'foo a b))
          (lambda (x)
            (values (list 'bar x) (list 'baz x))))
 'z)

((spread-combine list
                 (lambda (x y #!optional opt) (values x y))
                 (lambda (u v w #!optional z) (values w v u z)))
 'a 'b 'c 'd 'e 'f)

(arity (parallel-apply list list))

((parallel-combine list
                   (lambda args (values (cons 'foo args)))
                   (lambda (a b c d) (values (list 'bar a b c d))))
 'a 'b 'c 'd)

(((discard-argument 2)
  (lambda (x y z) (list 'foo x y z)))
 'a 'b 'c 'd)

((((curry-argument 2) 'a 'b 'c)
  (lambda (x y z w) (list 'foo x y z w)))
 'd)

(((permute-arguments 1 2 0 3)
  (lambda args (cons 'foo args)))
 'a 'b 'c 'd)

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

((compose (lambda args
            (cons 'foo args))
          (lambda (x y)
            (values (list 'faz x) (list 'fat y)))
          (lambda (x)
            (values (list 'bar x) (list 'baz x))))
 'z)
