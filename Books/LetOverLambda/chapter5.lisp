(load "chapter1.lisp")
(load "chapter3.lisp")

(defmacro! defunits% (quantity base-unit &rest units)
  `(defmacro ,(symb 'unit-of- quantity) (,g!val ,g!un)
     `(* ,,g!val
         ,(case ,g!un
            ((,base-unit) 1)
            ,@(mapcar (lambda (x)
                        `((,(car x)) ,(cadr x)))
                      (group units 2))))))

(defun defunits-chaining% (u units)
  (let ((spec (find u units :key #'car)))
    (if (null spec)
        (error "Unknown unit ~a" u)
        (let ((chain (cadr spec)))
          (if (listp chain)
              (* (car chain)
                 (defunits-chaining% 
                     (cadr chain)
                     units))
              chain)))))

(defmacro! defunits%% (quantity base-unit &rest units)
  `(defmacro ,(symb 'unit-of- quantity) (,g!val ,g!un)
     `(* ,,g!val
         ,(case ,g!un
            ((,base-unit) 1)
            ,@(mapcar (lambda (x)
                        `((,(car x))
                          ,(defunits-chaining%
                               (car x)
                               (cons `(,base-unit 1)
                                     (group units 2)))))
                      (group units 2))))))

(defunits-chaining%
    (car '(H (60 M)))
    (cons '(S 1) (group ' (m 60
                             h (60 m)
                             d (24 h)
                             ms (1/1000 s)
                             us (1/1000 ms)) 2)))


(defunits% time s
  m 60
  h 3600
  d 86400
 ms 1/1000
 us 1/1000000)
(defunits%% time s
  m 60
  h (60 m)
  d (24 h)
 ms (1/1000 s)
 us (1/1000 ms))




(defmacro cxr% (x tree)
  (if (null x)
      tree
      `(,(cond
           ((eq 'a (cadr x)) 'car)
           ((eq 'd (cadr x)) 'cdr)
           (t (error "Non A/D symbol")))
         ,(if (= 1 (car x))
              `(cxr% ,(cddr x) ,tree)
              `(cxr% ,(cons (- (car x) 1) (cdr x))
                     ,tree)))))

(defun eleventh (x)
  (cxr% (1 a 10 d) x))

;; Has issues with accepting a variable

(defvar cxr-inline-thresh 10)

(lol:defmacro! cxr (x tree)
  (if (null x)
      tree
      (let ((op (cond
                  ((eq 'a (cadr x)) 'car)
                  ((eq 'd (cadr x)) 'cdr)
                  (t (error "Non A/D symbol")))))
        (if (and (integerp (car x))
               (<= 1 (car x) cxr-inline-thresh))
            (if (= 1 (car x))
                `(,op (cxr ,(cddr x) ,tree))
                `(,op (cxr ,(cons (- (car x) 1) (cdr x)) ,tree)))
            `(lol:nlet-tail
                 ,g!name ((,g!count ,(car x))
                          (,g!val (cxr ,(cddr x) ,tree)))
               (if (>= 0 ,g!count)
                   ,g!val
                   (,g!name (- ,g!count 1)
                            (,op ,g!val))))))))
