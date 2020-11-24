(defun plot (fn min max step)
  (loop for i from min to max by step
     do (loop repeat (funcall fn i) do (format t "*"))
       (format t "~%")))

(plot #'exp 0 4 1/2)

(plot (lambda (x) (* 2 x)) 0 10 1)

(defvar *x* 1)
(defun foo (x)
  (let ((*x* 10))
    (print (setf *x* (1+ *x*)))
    (bar *x*))
  (bar *x*)
  *x*)


(defun bar (*x*)
  (print *x*))


;;; Testing out let bindings
(defun baz (x) (setf x 10))

(let ((y 20))
  (baz y)
  (print y))

;; Does x eval to *my-db-title*, when calling it directly would evaluate to #<HASHTABLE>
(mapcar (lambda (x) (symbol-value x)) *my-db-list*)
;; These two s
(symbol-value '*my-db-album*)

(let ((a 4))
   (declare (special a))
   (let ((b (symbol-value 'a)))
     (setf (symbol-value 'a) 5)
     (values a b)))

(let ((a 2))
  (declare (special a))
  (setf (symbol-value 'a) 3)
  a)
;; 3


(let ((a 2))
  (setf (symbol-value 'a) 3)
  a)
;; 2

(symbol-value *my-db-title*)
20 
