;; Macros----------------------------------------------------------------

;; 7.1 How Macros work-------------------------------------

(defmacro nil! (var)
  (list 'setq var nil))
;; My version
(defmacro nil! (var)
  `(setq ,var nil))
;; Seems like macros make a list that get evaluated
;;                            Macro expand       Evaluation 
;; (list 'setq var nil)) --> (setq var nil) ---> var == nil

;; Happens in 2 steps
;; 1. Macro expand
;; 2. Evaluation 
(defmacro nif (expr pos zero neg)
  `(case (truncate (signum ,expr))
     (1 ,pos)
     (0 ,zero)
     (-1 ,neg)))

;; 1.0 != 1 in case... uses eq

;; ,@ varient of a comma... it splices the ()s away

(defparameter b '(1 2 3))
;; (A (1 2 3) C)
`(a ,b c)

;; (A 1 2 3 C)
`(a ,@b c)

;; A . 1)
`(a ,@1)

;; (2 3) by default member uses eql 
(member 2.0 '(1 2 3) :test #'=)

(defmacro memq (obj lst)
  `(member ,obj ,lst :test #'eq))


(pprint (macroexpand-1 '(memq 2.0 '(1 2 3 4))))
;; (MEMBER 2.0 '(1 2 3 4) :TEST #'EQ)

(defparameter *x* (macroexpand-1 '(memq 2.0 '(1 2 3 4))))
(eval *x*)

(destructuring-bind (x (y) . z) '(a (b) c d) (list x y z))
;; (A B (C D))

(dolist (x '(a b c))
  (princ x))
;; ABC

(macroexpand-1 '(dolist (x '(a b c))
                 (princ x)))

(defmacro out-dolist ((var list &optional result) &body body)
  `(progn
     (mapc #'(lambda (,var) ,@body)
           ,list)
     (let ((,var nil))
       ,result)))


(defmacro ora (&rest args)
  (or-expand args))

(defun or-expand (args)
  (if (null args)
      nil
      (let ((sym (gensym)))
        `(let ((,sym ,(car args)))
          (if ,sym
               ,sym
               ,(or-expand (cdr args)))))))
