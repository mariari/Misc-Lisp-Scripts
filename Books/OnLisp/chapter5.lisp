(defun joiner (obj)
  (typecase obj
    (cons
     #'append)
    (number #'+)))

(defun join (&rest args)
  (apply (joiner (car args)) args))

;; Weird closure thingy that lexical scoping allows to work 
(defun make-adder (n)
  #'(lambda (x) (+ x n)))

;; (setq add3 (make-adder 3))
;; (funcall add3 2)

;; Used to getting the destructive version of a function 
(defvar *!equivs* (make-hash-table))

(defun ! (fn)
  (or (gethash fn *!equivs*) fn))

(defun def! (fn fn!)
  (setf (gethash fn *!equivs*) fn!))
;; In action
(def! #'remove-if  #'delete-if)

(defparameter *x* '(1 2 3 4 5 6 7))

;; A bit awkward in cl- since we need the funcall and #' 
(funcall (! #'remove-if) #'oddp *x*)
;; (2 4 6) destructively removes the odd numbers 

;;; 5.3 Memorize-------------------------------------------------------
(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
              val
              (setf (gethash args cache)
                    (apply fn args)))))))
;; Testing out memory efficiency of this all
;; (setq slowid (memoize #'(lambda (x) (sleep 5) x)))

;; (time (funcall slowid 1))
;;   5.0000 seconds of real time


;; (time (funcall slowid 1))
;;   0.000 seconds of real time

;; (setq fastid (memoize #'(lambda (&rest args) (apply #'+ args))))
;; (funcall fastid 1 2 3 4 5 )

;;5.4 Composing Functions----------------------------------------------
(defun compose (&rest fns)
  "Returns a function like F(G(x)).... the functions,
   when applied happen in reverse order of how they were inputted"
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        (lambda (&rest args)
          (reduce #'funcall fns
                  :from-end t
                  :initial-value (apply fn1 args))))
      #'identity))

;; Testing compusing F(g(x))
(compose #'list #'1+)
(funcall (compose #'list #'1+ ) 2 )

(funcall (compose #'list #'+) 2 3 4)

;; We might as well just use apply and list outside of this 
(funcall (compose #'list #'apply) #'+ '(2 3 4))

;; = (9) = (list (apply #'+ '(2 3 4)))



(funcall (compose #'1+ #'find-if) #'oddp '(2 3 4 5))
;; above is eq --> (1+ (find-if #'oddp '(2 3 4 5)))



(defun fint (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
        #'(lambda (x)
            (and (funcall fn x) (funcall chain x))))))

;; This is a nice enclosure which allows for testing
(defun fif (if then &optional else)
  "Checks for if if t then it checks then else if it fails if, it goes to else (if there is one)
   This has an enclosure of funcall"
  #'(lambda (x)
      (if (funcall if x)
          (funcall then x)
          (if else (funcall else x)))))

(mapcar (fif #'oddp (lambda (x) (< 10 x)) #'evenp)
        '(1 2 3 4 5 6 ))
(mapcar (fif #'oddp #'- #'evenp)
        '(1 2 3 4 5 6 ))

;;5.5 Recursion on Crds------------------------------------------------

;; Basically this chapter describes that even our basic recursion can be abstracted even higher

;;5.6 Recursion on Subtress--------------------------------------------
(defparameter x '(a b))

(defparameter listx (list x 1))

(eq x (car (copy-list listx)))
;; Returns T
(eq x (car (copy-tree listx)))
;; Returns nil
;; (our-copy-tree listx)

(defun our-copy-tree (tree)
  (if (atom tree)
      tree
      (cons (our-copy-tree (car tree))
            (if (cdr tree) (our-copy-tree (cdr tree))))))
;; Counts the leaves on a tree
(defun count-leaves (tree)
  (if (atom tree)
      1
      (+ (count-leaves (car tree))
         (if (cdr tree)
             (count-leaves (cdr tree))
             1))))

(defun count-leaves (tree)
  (cond ((atom tree) 1)
        ((cdr tree) (+ (count-leaves (car tree)) (count-leaves (cdr tree))))
        (t          (+ (count-leaves (car tree)) 1))))

(count-leaves '((a b (c d)) (e) f))
;; a . b . (c . (d . nil) . nil) . (e . nil) . nil . f . nil

;;  I skipped the rest of the section


