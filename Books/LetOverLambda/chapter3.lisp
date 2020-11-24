;; Macros
(ql:quickload "let-over-lambda")

(defun sleep-units% (value unit)
  (sleep
   (* value
      (case unit
        ((s) 1)
        ((m) 60)
        ((h) 360)
        ((d) 86400)
        ((ms) 1/1000)
        ((us) 1/1000000)
        (t 1)))))

(defmacro sleep-units (value unit)
  `(sleep
   (* ,value
      ,(case unit
        ((s) 1)
        ((m) 60)
        ((h) 360)
        ((d) 86400)
        ((ms) 1/1000)
        ((us) 1/1000000)
        (t 1)))))

;;; A better macro because we can tie it with the problem a lot easier
(defmacro unit-of-time (value unit)
  `(* ,value
     ,(case unit
        ((s) 1)
        ((m) 60)
        ((h) 360)
        ((d) 86400)
        ((ms) 1/1000)
        ((us) 1/1000000)
        (t 1))))


;; (sleep-units% 5 'us)
;; (sleep-units 1/720 h)
;; (unit-of-time 1/720 h)

;;; Something scheme has
(defmacro nlet (n letargs &rest body)
  `(labels ((,n ,(mapcar #'car letargs)
              ,@body))
     (,n ,@(mapcar #'cadr letargs))))

(defun nlet-fact (n)
  (nlet fact ((n n) (acc 1))
        (if (zerop n)
            acc
            (fact (- n 1) (* n acc)))))


(macroexpand-1 '(nlet fact ((n n) (acc 1))
        (if (zerop n)
            acc
            (fact (- n 1) (* n acc)))))


(defmacro nif-buggy (expr pos zero neg)
  `(let ((obscure-name ,expr))
     (cond ((plusp obscure-name) ,pos)
           ((zerop obscure-name) ,zero)
           (t ,neg))))


;; Works fine by the rules ... no unwanted variable capture 
(nif-buggy 5
           (let ((obscure-name 'pos))
             obscure-name) 'zero 'neg)

;; Unwanted Variable capture, since ,obscure-name is bound to 5 because of the let 
(let ((obscure-name 'pos))
  (nif-buggy 5 obscure-name 'zero 'neg))


(defmacro nif (expr pos zero neg)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ((plusp ,g) ,pos)
             ((zerop ,g) ,zero)
             (t ,neg)))))


;; Going to be used to test our G symbols
(defun g!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "G!"
                :start1 0
                :end1 2)))

(defun flatten-old (x)
  "Completely flattens a given list"
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   #+(and sbcl (not lol::old-sbcl))
                   ((typep x 'sb-impl::comma) (rec (sb-impl::comma-expr x) acc))
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p
                              (flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar
              (lambda (s) `(,s (gensym ,(subseq (symbol-name s) 2))))
              syms)
         ,@body))))

(defmacro test-flatten (&rest body) (lol:flatten body))
(defmacro test-flatten! (&rest body) (flatten-old body))

(defmacro/g! nif-g! (expr pos zero neg)
  `(let ((,g!result ,expr))
     (cond ((plusp ,g!result) ,pos)
           ((zerop ,g!result) ,zero)
           (t ,neg))))

;; (nif-g! 5
;;            (let ((obscure-name 'pos))
;;              obscure-name) 'zero 'neg)
;;* 
(defmacro/g! junk-outer ()
  `(defmacro/g! junk-inter ()
     `(let ((,g!abc))
        ,g!abc)))


(defun o!-symbol-p (s)
  (and (symbolp s)
     (> (length (symbol-name s)) 2)
     (string= (symbol-name s)
              "O!"
              :start1 0
              :end1 2)))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun o!-symbol-to-g!-symbol (s)
  (symb "G!"
        (subseq (symbol-name s) 2)))

(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p args))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(mapcar #'list (list ,@gs) (list ,@os))
          ,(progn ,@body)))))


(defmacro! square (o!x)
  `(* ,g!x ,g!x))

(defmacro! nif-o! (o!expr pos zero neg)
  `(cond ((plusp ,g!expr) ,pos)
         ((zerop ,g!expr) ,zero)
         (t ,neg)))
