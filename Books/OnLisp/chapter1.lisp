(defun our-remove-if (fn lst)
  (if (null lst)
      nil
      (if (funcall fn (car lst))
          (our-remove-if fn (cdr lst))
          (cons (car lst) (our-remove-if fn (cdr lst))))))

(defun list+ (lst n)
  (mapcar (lambda (x) (+ x n))
          lst))


(let ((counter 0))
  (defun new-id ()
    (incf counter))
  (defun reset-id () (setq counter 0)))

(defun make-adder (n)
  #'(lambda (x) (+ x n)))

;; The 2 = the n
(defparameter add2 (make-adder 2))
(defparameter add10 (make-adder 10))
;; The 5 here is the x 
(funcall add2 5)

;; Time to make closures with malleable states
(defun make-adderb (n)
  #'(lambda (x &optional change)
      (if change
          (setq n x)
          (+ x n))))
;; Now time to initialize it
(defparameter addx (make-adderb 1))
;; Normal behavior
(funcall addx 3)
;; overloaded
(funcall addx 100 t)
;; New behavior
(funcall addx 3)


;; Sharing data types
;; (setq cities (make-dbms ’((boston . us) (paris . france))))


;; Using labels to recursively find stuff with mapcar
(defun count-instances (obj lsts)
  (labels ((instances-in (lst)
             (if (consp lst)
                 (+ (if (eq (car lst) obj) 1 0)
                    (instances-in (cdr lst)))
                 0)))
    (mapcar #'instances-in lsts)))
;; Testing out our new function 
(count-instances 'a '((a  b c) (d a r p a) (dar) (a a a a)))




;; Time to go fast
(defun triangle (n)
  (labels ((tri (c n)
             (declare (type fixnum n c))
             (if (zerop n)
                 c
                 (tri (the fixnum (+ n c))
                      (the fixnum (- n 1))))))
    (tri 0 n)))

;; Cheating how compiles work 
(defun 50th (lst) (nth 49 lst))

(proclaim '(inline 50th))

;; (defun foo (lst)
;; (+ (50th lst) 1))
;; Works the same as
(defun foo (lst)
  (+ (nth 49 lst) 1))
