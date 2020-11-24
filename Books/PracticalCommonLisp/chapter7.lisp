(defmacro when% (condition &rest body)
  `(if ,condition (progn ,@body)))

(defun test (&rest stuff)
  (print `(,@stuff))
  (print `(,stuff))
  (print `(,+ ,@stuff))
  (print stuff)
  (apply #'+ stuff))


(do ((n 0 (1+ n))
     (curr 0 next)
     (next 1 (+ curr next)))
    ;; End test form 
    ((= 10 n) curr))
;; Notice, no body 


;; This one does have a body 
(do ((i 0 (1+ i)))
    ((>= i 4))
  (print i))
