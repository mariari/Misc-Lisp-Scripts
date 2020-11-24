;; Lets make a let function that removes the (())---------------

; &body is a keyword that tells lisp
; to put the rest of the arguments to the value right of it
(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

;; Old let 
(let ((foo (+ 2 3)))
  (* foo foo))

;; New let1 
(let1 foo (+ 2 3)
  (* foo foo))

;; With this look at how much cleaner a function like add gets

; (defun add (a b)
;   (let ((x (+ a b)))
;     x))

  (defun add (a b)
    (let1 x (+ a b)
      x))

(add 2 3)

;; Macro expand
(macroexpand '(let1 foo (+ 2 3)
	       (princ "Lisp is awesome!")
	       (* foo foo)))


;; Broken
(defmacro split (val yes no)
  `(let1 x ,val
     (if x
	 (let ((head (car x))
	       (tail (cdr x)))
	   ,yes)
	 ,no)))

(split (progn (princ "Lisp rocks!")
	      '(2 3))
       (format t "this list can be split ")
       (format t "this list can't be split"))

(macroexpand '(split (progn (princ "Lisp rocks!")
			    '(2 3))
	       (format t "this list can be split ")
	       (format t "this list can't be split")))
