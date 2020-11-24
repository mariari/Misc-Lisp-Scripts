(loop for i
   below 5
   sum i)

(loop for i from 5 to 10
   sum i)

(loop for i in '(100 20 3)
     sum i)

(loop for i
   below 5
   do (print i))
(loop for i below 10
   when (oddp i)
   sum i)

;; Multiple iterations 
(loop
   for i below 10
   for j below 10
   collect (+ x y))
;; Nested iterations
(loop for i below 10
   collect (loop for j below 10
	      collect (+ i j)))
;; Cool stuff
(loop for i
   from 0
   for day in '(monday tuesday wednesday thursday friday saturday sunday)
   collect (cons i day))
